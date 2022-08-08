module Implementation
open Domain
open NonEmptyList
open NonEmptyMap
open EventStack
open PowerMaps
open CardTypes
open Card
open ImplementationTypes
open PointFree

// Give very large lists if passed a zero length
let private createIDs start lst =
    [for i in 0u..uint (List.length lst - 1) -> start + LanguagePrimitives.UInt32WithMeasure i]
let private createIDsToLength start len =
    [for i in 0u..(len - 1u) -> start + LanguagePrimitives.UInt32WithMeasure i]
let private zipIDs (start: uint<'a>) lst =
    let IDs = [for i in 0u..uint (List.length lst - 1) -> start + LanguagePrimitives.UInt32WithMeasure i]
    List.zip IDs lst
let private createIDMap start lst =
    zipIDs start lst
    |> Map.ofList

let private transferAttackerIDs attackerIDs =
    match attackerIDs with
    | SingleAttackerID (ActiveUnitID id) -> SingleCardID id
    | PairAttackerIDs (PairedUnitID id1, PairedUnitID id2) -> PairIDs (id1, id2)

let private emptyLane = {
    InactiveUnits = List.empty
    ActiveUnits = List.empty
    Pairs = List.empty
}

let private laneSolePresence (lane: Lane) =
    let cardOwners =
        (lane.InactiveUnits |> List.map (fun card -> card.Owner))
        @ (lane.ActiveUnits |> List.map (fun card -> card.Owner))
        @ (lane.Pairs |> List.collect (fun {Owner = owner} -> [owner; owner]))
    match List.distinct cardOwners with
    | [] -> Empty
    | [controller] -> Won controller
    | _ -> Contested

let private filterLane predicate lane =
    (lane.InactiveUnits |> List.map InactiveUnit |> List.filter predicate)
    @ (lane.ActiveUnits |> List.map ActiveUnit |> List.filter predicate)
    @ (
        lane.Pairs
        |> List.collect (pairToFullPairedUnits >> pairToList)
        |> List.map PairedUnit
        |> List.filter predicate
        )

let private findDeadCardsInLane laneID board =
    Map.find laneID board.Lanes
    |> filterLane isDead
let private findOwnedDeadCardsInLane laneID currentPlayer board =
    Map.find laneID board.Lanes
    |> filterLane (fun card -> isDead card && ownedBy currentPlayer card)
let private findNonownedDeadCardsInLane laneID currentPlayer board =
    Map.find laneID board.Lanes
    |> filterLane (fun card -> isDead card && not (ownedBy currentPlayer card))
let private findDeadCardIDsInLane laneID board =
    findDeadCardsInLane laneID board
    |> List.map unitID
let private findOwnedDeadCardIDsInLane laneID currentPlayer board =
    findOwnedDeadCardsInLane laneID currentPlayer board
    |> List.map unitID
let private findNonownedDeadCardIDsInLane laneID currentPlayer board =
    findNonownedDeadCardsInLane laneID currentPlayer board
    |> List.map unitID

let private removeFromLaneInCardsState f laneID cardsState =
    let board = cardsState.Board
    let lane = Map.find laneID board.Lanes
    let removed, newLane = f lane
    removed,
    {cardsState with Board = {board with Lanes = board.Lanes |> Map.add laneID newLane}}
let private addToLaneInCardsState f added laneID cardsState =
    let board = cardsState.Board
    let lane = Map.find laneID board.Lanes
    let newLane = f added lane
    {cardsState with Board = {board with Lanes = board.Lanes |> Map.add laneID newLane}}

let private removeCardFromInactiveUnits: Remover<InactiveUnit, InactiveUnitID> = fun (InactiveUnitID cardID) ->
    let removeIn lane =
        lane.InactiveUnits
        |> List.partition (fun {InactiveUnitID = InactiveUnitID id} -> id = cardID)
        |> opPair List.exactlyOne (fun iu -> {lane with InactiveUnits = iu})
    removeFromLaneInCardsState removeIn
let private removeCardsFromInactiveUnits: ListRemover<InactiveUnit, InactiveUnitID> = fun cardIDs ->
    let removeIn lane =
        lane.InactiveUnits
        |> List.partition (fun {InactiveUnitID = id} -> List.contains id cardIDs)
        |> opRight (fun iu -> {lane with InactiveUnits = iu})
    removeFromLaneInCardsState removeIn
let private removeCardPairFromActiveUnits: PairRemover<ActiveUnit, ActiveUnitID> = fun (cardID1, cardID2) ->
    let removeIn lane =
        lane.ActiveUnits
        |> List.partition (fun {ActiveUnitID = id} -> id = cardID1)
        |> opPair
            List.exactlyOne
            (
            List.partition (fun {ActiveUnitID = id} -> id = cardID2)
            >> opPair List.exactlyOne (fun au -> {lane with ActiveUnits = au})
                )
        |> flattenRight
        |> unflattenLeft
    removeFromLaneInCardsState removeIn
let private addCardToInactiveUnits: Adder<InactiveUnit> =
    let addTo card lane = {lane with InactiveUnits = lane.InactiveUnits @ [card]}
    addToLaneInCardsState addTo
let private addCardsToInactiveUnits: ListAdder<InactiveUnit> =
    let addTo cards lane = {lane with InactiveUnits = lane.InactiveUnits @ cards}
    addToLaneInCardsState addTo
let private addCardToActiveUnits: Adder<ActiveUnit> =
    let addTo card lane = {lane with ActiveUnits = lane.ActiveUnits @ [card]}
    addToLaneInCardsState addTo
let private addCardsToActiveUnits: ListAdder<ActiveUnit> =
    let addTo cards lane = {lane with ActiveUnits = lane.ActiveUnits @ cards}
    addToLaneInCardsState addTo
let private addCardsToPairs: Adder<Pair> =
    let addTo pair lane = {lane with Pairs = lane.Pairs @ [pair]}
    addToLaneInCardsState addTo

let private removeFromPairedStructure = fun toTwople singleTransform f ->
    List.map (
        toTwople
        >> dup
        >> opLeft (opBoth f)
        >> function
        | (false, false), p -> [], [], Some p
        | (true, false), (l, r) -> [singleTransform l], [singleTransform r], None
        | (false, true), (l, r) -> [singleTransform r], [singleTransform l], None
        | (true, true), p -> (pairToList p |> List.map singleTransform), [], None
        )
    >> List.unzip3
    >> (fun (lst1, lst2, opts) -> List.collect id lst1, List.collect id lst2, List.choose id opts)
let private removeFromPairs = removeFromPairedStructure pairToFullPairedUnits fullPairedToActiveUnit
let private removeFromLane = fun (fIn, fAct, fPair) lane ->
    let removedInactives, inactive = List.partition fIn lane.InactiveUnits
    let removedActives, active = List.partition fAct lane.ActiveUnits
    let removedPairMembers, isolatedPartners, pairs = removeFromPairs fPair lane.Pairs
    (List.map InactiveUnit removedInactives) @ (List.map ActiveUnit removedActives) @ (List.map ActiveUnit removedPairMembers),
    isolatedPartners,
    {lane with
        InactiveUnits = inactive
        ActiveUnits = active
        Pairs = List.map fullPairedUnitsToPair pairs
        }

let private findCardInLane = fun predicate lane ->
    let inactive =
        lane.InactiveUnits
        |> List.map InactiveUnit
    let active =
        lane.ActiveUnits
        |> List.map ActiveUnit
    let paired =
        lane.Pairs
        |> List.collect (pairToFullPairedUnits >> pairToList)
        |> List.map PairedUnit
    List.find predicate (inactive @ active @ paired)

let private findActiveCardInLane = fun predicate lane ->
    let active =
        lane.ActiveUnits
        |> List.map Solo
    let paired =
        lane.Pairs
        |> List.collect (pairToFullPairedUnits >> pairToList)
        |> List.map Paired
    List.find predicate (active @ paired)

let private findInactiveCardInLane = fun predicate lane ->
    List.find predicate lane.InactiveUnits

let private removeCardsInLane = fun cardIDs lane ->
    let fIn {InactiveUnitID = InactiveUnitID cardID} = List.contains (UnitID cardID) cardIDs
    let fAct {ActiveUnitID = ActiveUnitID cardID} = List.contains (UnitID cardID) cardIDs
    let fPair {FullPairedUnitID = FullPairedUnitID cardID} = List.contains (UnitID cardID) cardIDs
    removeFromLane (fIn, fAct, fPair) lane
let private removeCardsFromLane = fun cardIDs laneID cardsState ->
    let lane = Map.find laneID cardsState.Board.Lanes
    let removed, partners, newLane = removeCardsInLane cardIDs lane
    removed, partners,
    {cardsState with Board = {cardsState.Board with Lanes = Map.add laneID newLane cardsState.Board.Lanes}}
let private changeCardLane cardID fromLaneID toLaneID cardsState =
    let moved, isolatedPartners, reducedCardsState =
        removeCardsFromLane [cardID] fromLaneID cardsState
    let movedInactive, movedActive =
        moved
        |> List.map (function
            | InactiveUnit c -> Some c, None
            | ActiveUnit c -> None, Some c
            | PairedUnit c -> None, Some (fullPairedToActiveUnit c)
            )
        |> List.unzip
        |> opPair (List.choose id) (List.choose id)
    reducedCardsState
    |> addCardsToActiveUnits isolatedPartners fromLaneID
    |> addCardsToInactiveUnits movedInactive toLaneID
    |> addCardsToActiveUnits movedActive toLaneID

let private flipAndActivateInactiveDeathPowersInLane laneID zeroHealthInactiveDeathPowerUnits cardsState =
    let inactiveIDs =
        zeroHealthInactiveDeathPowerUnits
        |> List.choose (function
            | InactiveUnit {InactiveUnitID = cid} -> Some cid
            | ActiveUnit _
            | PairedUnit _ -> None
            )
    let newCards, cs1 =
        cardsState
        |> removeCardsFromInactiveUnits inactiveIDs laneID
        |> opLeft (List.map (inactiveToActiveUnit >> fullyHealActiveCard))
    addCardsToActiveUnits newCards laneID cs1

let private changeInPlace f laneID cardsState =
    let lane = Map.find laneID cardsState.Board.Lanes
    let newLane = f lane
    {cardsState with Board = {cardsState.Board with Lanes = Map.add laneID newLane cardsState.Board.Lanes}}
let private changeCardInLane = fun (fIn, fAct, fPair) cardID lane -> {
    lane with
        InactiveUnits =
            lane.InactiveUnits
            |> List.map (fun card ->
                let {InactiveUnitID = InactiveUnitID id} = card
                if id = cardID then fIn card else card
                )
        ActiveUnits =
            lane.ActiveUnits
            |> List.map (fun card ->
                let {ActiveUnitID = ActiveUnitID id} = card
                if id = cardID then fAct card else card
                )
        Pairs =
            lane.Pairs
            |> List.map (fun pair ->
                let {Cards = card1, card2} = pair
                let {PairedUnitID = PairedUnitID id1} = card1
                let {PairedUnitID = PairedUnitID id2} = card2
                let newCards =
                    (if id1 = cardID then fPair card1 else card1),
                    (if id2 = cardID then fPair card2 else card2)
                {pair with Cards = newCards}
                )
    }

let private damageCard (UnitID cardID) damage =
    let damageIn card = ({card with Damage = card.Damage + damage}: InactiveUnit)
    let damageAct card = ({card with Damage = card.Damage + damage}: ActiveUnit)
    let damagePair card = ({card with Damage = card.Damage + damage}: PairedUnit)
    let changeLane = changeCardInLane (damageIn, damageAct, damagePair)
    changeInPlace (changeLane cardID)

let private incrementCardActionsUsed cardID =
    let incAct card = ({card with ActionsSpent = card.ActionsSpent + 1u<action>}: ActiveUnit)
    let incPair card = ({card with ActionsSpent = card.ActionsSpent + 1u<action>}: PairedUnit)
    let changeLane = changeCardInLane (id, incAct, incPair)
    changeInPlace (changeLane cardID)

let private setMaxCardActions (cardID: CardID) left laneID cardsState =
    let setAct card = ({card with MaxActions = left}: ActiveUnit)
    let setPair card = ({card with MaxActions = left}: PairedUnit)
    let lane = Map.find laneID cardsState.Board.Lanes
    let matchesID = function
    | Solo {ActiveUnitID = ActiveUnitID cid}
    | Paired {FullPairedUnitID = FullPairedUnitID cid} ->
        cid = cardID
    let (card: ActiveCard) = findActiveCardInLane matchesID lane
    let playerID =
        match card with
        | Solo {Owner = pid}
        | Paired {Owner = pid} ->
            pid
    let changeLane = changeCardInLane (id, setAct, setPair)
    [AttacksSet (playerID, card, left)],
    changeInPlace (changeLane (cardID)) laneID cardsState

let private makeCardKnown cardID playerID cardsState =
    let setIn card = ({card with KnownBy = card.KnownBy |> Set.add playerID}: InactiveUnit)
    let changeLane = changeCardInLane (setIn, id, id)
    let lanesUpdated = {
        cardsState with
            Board = {
                cardsState.Board with
                    Lanes = cardsState.Board.Lanes |> Map.map (fun _ lane -> changeLane cardID lane)
            }
        }
    match lanesUpdated.GameStage with
    | Early stage ->
        let newBases =
            stage.Bases
            |> Map.map (fun _ cards ->
                cards
                |> List.map (fun card ->
                    let {BaseCardID = BaseCardID id} = card
                    if id = cardID then
                        {card with KnownBy = card.KnownBy |> Set.add playerID}
                    else
                        card
                    )
                )
        let newStage = Early {stage with Bases = newBases}
        {lanesUpdated with GameStage = newStage}
    | DrawPileEmpty _
    | HandsEmpty _ ->
        lanesUpdated

let private removeCardFromHand (HandCardID cardID) playerID (cardsState: CardsState) =
    match cardsState.GameStage with
    | Early gs ->
        let hands = gs.HandCards
        let removedCard, newHand =
            match Map.tryFind playerID hands with
            | Some lst -> lst |> List.partition (fun {HandCardID = HandCardID id} -> id = cardID)
            | None -> failwithf "Can't remove card from non-existant player's hand"
        let newHands =
            hands
            |> Map.change playerID (fun oldHand -> Some newHand)
        List.head removedCard,
        {cardsState with
            GameStage = Early {gs with HandCards = newHands}
            }
    | DrawPileEmpty gs ->
        let hands = gs.HandCards
        let removedCard, newHand =
            match Map.tryFind playerID hands with
            | Some lst -> lst |> List.partition (fun {HandCardID = HandCardID id} -> id = cardID)
            | None -> failwithf "Can't remove card from non-existant player's hand"
        let newHands =
            hands
            |> Map.change playerID (fun oldHand -> Some newHand)
        List.head removedCard,
        {cardsState with
            GameStage = DrawPileEmpty {gs with HandCards = newHands}
            }
    | HandsEmpty _ ->
        failwithf "Can't remove card when hands are empty"
let private removeHandsIfAllEmpty cardsState =
    match cardsState.GameStage with
    | DrawPileEmpty {HandCards = handCards; LaneWins = laneWins} ->
        if Map.forall (fun _ cards -> List.isEmpty cards) handCards then
            {cardsState with GameStage = HandsEmpty {LaneWins = laneWins}}
        else
            cardsState
    | Early _
    | HandsEmpty _ ->
        cardsState
let private flipBasesOnLane (bases, lane) =
    {lane with InactiveUnits = lane.InactiveUnits @ (List.map baseToInactiveUnit bases)}
let private joinMaps map1 map2 = // left join
    let findIn = swapIn Map.find
    map1
    |> Map.map (fun key state -> pairRev key state |> opRight(findIn map2))
let private flipBasesOnBoard bases ({Lanes = lanes; Discard = discard}: Board) =
    {
        Lanes =
            joinMaps bases lanes
            |> Map.map (fun _ -> flipBasesOnLane)
        Discard = discard
        }

let private incrementActionsLeft (gameState: GameStateDuringTurn) =
    {gameState with
        TurnState = {
            gameState.TurnState with
                ActionsLeft = gameState.TurnState.ActionsLeft + 1u<action>
        }
    }

let private changeCardsState (gameState: GameStateDuringTurn) newCardsState =
    {gameState with CardsState = newCardsState}

let private flipInactiveCardsInLaneAndAddActivationPowersToStack playerID laneID cardsState turnState =
    let lane = Map.find laneID cardsState.Board.Lanes
    let flippingIDs =
        lane.InactiveUnits
        |> List.choose (fun card -> if card.Owner = playerID then Some card.InactiveUnitID else None)
    cardsState
    |> removeCardsFromInactiveUnits flippingIDs laneID
    |> opLeft (
        List.map inactiveToActiveUnit
        >> dup
        >> opLeft dup
        >> flattenLeft
        >> (fun (a, b, c) ->
            List.map CardActivated a,
            b
            |> List.choose (fun {ActiveUnitID = ActiveUnitID id; Abilities = abilities} ->
                match abilities.OnActivation with
                | [] -> None
                | _ -> Some (PowerContext (laneID, id, abilities.Name))
                )
            |> tryFromList,
            c
            )
        )
    |> (fun ((a, b, c), d) -> a, addCardsToActiveUnits c laneID d, Option.map OrderChoiceEpoch b)
    |> unflattenRight
    |> opRight (toMiddle turnState)

let private toActionChoice gameState =
    {
        CardsState = gameState.CardsState
        TurnState = gameState.TurnState
        TurnStage = ActionChoice
    }
let private triggerTargetInactiveDeathPowers gameState =
    let cardsState = gameState.CardsState
    let board = cardsState.Board
    let laneIDs =
        board.Lanes
        |> Map.toList
        |> List.map (fun (id, _) -> id)
    let zeroHealthInactiveDeathPowerUnits =
        laneIDs
        |> List.collect (fun id -> findDeadCardsInLane id board)
        |> List.filter (fun card ->
            match card with
            | InactiveUnit {Abilities = a} ->
                not (List.isEmpty a.OnInactiveDying)
            | ActiveUnit _
            | PairedUnit _ ->
                false
            )
    laneIDs
    |> List.fold (swapIn (fun laneID ->
        flipAndActivateInactiveDeathPowersInLane laneID zeroHealthInactiveDeathPowerUnits
        )) cardsState
    |> changeCardsState gameState
let private healAttackersIfDefenderDying (attackerIDs: AttackerIDs) (UnitID attackedID) laneID gameState =
    let board = gameState.CardsState.Board
    let lane = Map.find laneID board.Lanes
    let dyingCardIDs = findDeadCardIDsInLane laneID board
    if List.contains (UnitID attackedID) dyingCardIDs then
        match attackerIDs with
        | SingleAttackerID id ->
            let activeUnits =
                lane.ActiveUnits
                |> List.map (fun card ->
                    if card.ActiveUnitID = id then
                        decrementActiveCardDamage card
                    else
                        card
                    )
            let newLane = {lane with ActiveUnits = activeUnits}
            let newLanes = Map.add laneID newLane board.Lanes
            let newBoard = {board with Lanes = newLanes}
            {gameState.CardsState with Board = newBoard}
            |> changeCardsState gameState
        | PairAttackerIDs (id1, id2) ->
            let pairs =
                lane.Pairs
                |> List.map (fun pair ->
                    let (card1, card2) = pair.Cards
                    let newCards =
                        if List.contains card1.PairedUnitID [id1; id2] then
                            decrementPairedCardDamage card1,
                            decrementPairedCardDamage card2
                        else
                            card1, card2
                    {pair with Cards = newCards}
                    )
            let newLane = {lane with Pairs = pairs}
            let newLanes = Map.add laneID newLane board.Lanes
            let newBoard = {board with Lanes = newLanes}
            {gameState.CardsState with Board = newBoard}
            |> changeCardsState gameState
    else
        gameState
let private moveNonownedDeadCardsToDiscard gameState =
    let cardsState = gameState.CardsState
    let board = cardsState.Board
    let currentPlayer = gameState.TurnState.CurrentPlayer
    let laneIDs =
        board.Lanes
        |> Map.toList
        |> List.map fst
    let removed, newCardsState =
        laneIDs
        |> List.fold (fun (previousRemoved, cs) laneID ->
            let deadCardIDs = findNonownedDeadCardIDsInLane laneID currentPlayer cs.Board
            removeCardsFromLane deadCardIDs laneID cs
            |> unflattenRight
            |> opPair ((@) previousRemoved) (uncurry (swapIn addCardsToActiveUnits laneID))
            ) ([], cardsState)
    let discardCards = List.map unitToDiscardedCard removed
    let newDiscard = {newCardsState.Board with Discard = (newCardsState.Board.Discard @ discardCards)}
    {cardsState with Board = newDiscard}
    |> changeCardsState gameState
let private moveOwnedDeadCardsToDiscard gameState =
    let cardsState = gameState.CardsState
    let board = cardsState.Board
    let currentPlayer = gameState.TurnState.CurrentPlayer
    let laneIDs =
        board.Lanes
        |> Map.toList
        |> List.map fst
    let removed, newCardsState =
        laneIDs
        |> List.fold (fun (previousRemoved, cs) laneID ->
            let deadCardIDs = findOwnedDeadCardIDsInLane laneID currentPlayer cs.Board
            removeCardsFromLane deadCardIDs laneID cs
            |> unflattenRight
            |> opPair ((@) previousRemoved) (uncurry (swapIn addCardsToActiveUnits laneID))
            ) ([], cardsState)
    let discardCards = List.map unitToDiscardedCard removed
    let newDiscard = {newCardsState.Board with Discard = (newCardsState.Board.Discard @ discardCards)}
    {cardsState with Board = newDiscard}
    |> changeCardsState gameState
let private updateLaneWins gameState =
    match gameState.CardsState.GameStage with
    | Early _ ->
        gameState
    | DrawPileEmpty gs ->
        let lanes = gameState.CardsState.Board.Lanes
        let currentLaneWins =
            lanes
            |> Map.toList
            |> List.choose (fun (laneID, lane) ->
                match laneSolePresence lane with
                | Contested
                | Empty -> None
                | Won controller -> Some (laneID, controller)
                )
            |> Map.ofList
        let newGameStage = DrawPileEmpty {gs with LaneWins = currentLaneWins}
        {gameState.CardsState with GameStage = newGameStage}
        |> changeCardsState gameState
    | HandsEmpty gs ->
        let lanes = gameState.CardsState.Board.Lanes
        let currentLaneWins =
            lanes
            |> Map.toList
            |> List.choose (fun (laneID, lane) ->
                match laneSolePresence lane with
                | Contested
                | Empty -> None
                | Won controller -> Some (laneID, controller)
                )
            |> Map.ofList
        let newGameStage = HandsEmpty {gs with LaneWins = currentLaneWins}
        {gameState.CardsState with GameStage = newGameStage}
        |> changeCardsState gameState
let private checkForGameEnd gameState =
    match gameState.TurnStage with
    | AbilityChoice _
    | StackChoice _ ->
        GameStateDuringTurn gameState
    | ActionChoice ->
        let cs = gameState.CardsState
        match cs.GameStage with
        | Early _
        | DrawPileEmpty _ ->
            GameStateDuringTurn gameState
        | HandsEmpty {LaneWins = laneWins} ->
            let lanes = cs.Board.Lanes
            let wonLaneCounts =
                laneWins
                |> Map.toList
                |> List.countBy snd
            match wonLaneCounts with
            | [] -> GameStateDuringTurn gameState
            | lst ->
                let (leadingPlayer, leadingWins) =
                    lst
                    |> List.maxBy snd
                if leadingWins >= 2 then
                    GameStateWon {Winner = leadingPlayer; EndLanes = lanes; LaneWins = laneWins}
                else
                    let contestedLanes =
                        lanes
                        |> Map.filter (fun laneID lane ->
                            let laneIsEmpty =
                                List.isEmpty lane.InactiveUnits
                                && List.isEmpty lane.ActiveUnits
                                && List.isEmpty lane.Pairs
                            match Map.containsKey laneID laneWins, laneIsEmpty with
                            | false, false ->
                                true
                            | false, true
                            | true, false // shouldn't happen?
                            | true, true ->
                                false
                            )
                    if Map.isEmpty contestedLanes then
                        GameStateTied {EndLanes = lanes; LaneWins = laneWins}
                    else
                        GameStateDuringTurn gameState

let rec private shuffleRec unshuffled shuffled (sampler: System.Random) =
    match unshuffled with
    | [] -> shuffled
    | _ ->
        let index = sampler.Next(List.length unshuffled)
        let newUnshuffled =
            unshuffled
            |> List.indexed
            |> List.choose (function
                | n, _ when n = index -> None
                | _, el -> Some el
            )
        shuffleRec newUnshuffled (unshuffled.[index] :: shuffled) sampler

let private shuffle lst =
    let sampler = System.Random()
    shuffleRec lst [] sampler

let private createUnshuffledDeck () : (Rank * Suit) list =
    let toRank (caseInfo: Reflection.UnionCaseInfo) =
        Reflection.FSharpValue.MakeUnion(caseInfo, [||]) :?> Rank
    let toSuit (caseInfo: Reflection.UnionCaseInfo) =
        Reflection.FSharpValue.MakeUnion(caseInfo, [||]) :?> Suit
    let ranks =
        Reflection.FSharpType.GetUnionCases typeof<Rank>
        |> Array.toList
        |> List.map toRank
    let suits =
        Reflection.FSharpType.GetUnionCases typeof<Suit>
        |> Array.toList
        |> List.map toSuit
    List.allPairs ranks suits

let private getLanePlayerTroopKnowledges viewerID ownerID (lane: Lane) : InactiveUnitKnowledge list * ActiveUnitKnowledge list * PairKnowledge list =
    let pairsKnowledge =
        lane.Pairs
        |> List.filter (fun {Owner = owner} -> owner = ownerID)
        |> List.map getPairKnowledge
    let nonPairedActiveUnitKnowledge =
        lane.ActiveUnits
        |> List.filter (fun card -> card.Owner = ownerID)
        |> List.map (fun card ->
            let {ActiveUnitID = ActiveUnitID id} = card
            let actionability = getActionability (ActiveUnit card)
            ((id, card.Rank, card.Suit, card.Abilities.Name, card.Damage, card.MaxActions - card.ActionsSpent, actionability): ActiveUnitKnowledge)
            )
    let inactiveUnitKnowledge =
        lane.InactiveUnits
        |> List.filter (fun card -> card.Owner = ownerID)
        |> List.map (fun card ->
            let {InactiveUnitID = InactiveUnitID id} = card
            let actionability = if card.FreezeStatus = NotFrozen then Normal else Frozen
            if Set.contains viewerID card.KnownBy then
                KnownInactiveCardKnowledge (id, card.Rank, card.Suit, card.Abilities.Name, card.Damage, actionability)
            else
                UnknownInactiveCardKnowledge (id, card.Damage, actionability)
            )
    inactiveUnitKnowledge,
    nonPairedActiveUnitKnowledge,
    pairsKnowledge

let private getPlayerLaneWins =
    Map.toList
    >> List.groupBy snd
    >> List.map (opRight (List.map fst))

let private getBoardKnowledge viewerID cardsState turnInProgress =
    let ({Lanes = l; Discard = d}: Board) = cardsState.Board
    let getBase = getBaseKnowledge viewerID
    let getDeadCard = getDeadCardKnowledge viewerID
    let getLanePlayerTroops = getLanePlayerTroopKnowledges viewerID
    let (NPlayers nPlayers) = turnInProgress.NPlayers
    match cardsState.GameStage with
    | Early {Bases = b; DrawPile = dp} ->
        let lanesKnowledge =
            joinMaps b l
            |> Map.map (fun _ (bases, lane) ->
                let troopKnowledge =
                    createIDsToLength 1u<PID> nPlayers
                    |> List.map (fun playerID ->
                        playerID, getLanePlayerTroops playerID lane
                        )
                    |> Map.ofList
                {
                    Bases = List.map getBase bases
                    Troops = troopKnowledge
                    } : PreBaseFlipLaneKnowledge
                )
        let drawPileSize = NonEmptyList.length dp
        PreBaseFlipBoardKnowledge {
            Lanes = lanesKnowledge
            DrawPileSize = drawPileSize
            Discard = List.map getDeadCard d
            }
    | DrawPileEmpty {LaneWins = laneWins} ->
        let lanesKnowledge =
            l
            |> Map.map (fun laneID lane ->
                let troopKnowledge =
                    createIDsToLength 1u<PID> nPlayers
                    |> List.map (fun playerID ->
                        playerID, getLanePlayerTroops playerID lane
                        )
                    |> Map.ofList
                match Map.tryFind laneID laneWins with
                | None ->
                    ContestedLaneKnowledge {
                        Troops = troopKnowledge
                        }
                | Some c ->
                    WonLaneKnowledge {
                        Controller = c
                        Troops = troopKnowledge
                        }
                )
        PostBaseFlipBoardKnowledge {
            Lanes = lanesKnowledge
            Discard = List.map getDeadCard d
            }
    | HandsEmpty {LaneWins = laneWins} ->
        let lanesKnowledge =
            l
            |> Map.map (fun laneID lane ->
                let troopKnowledge =
                    createIDsToLength 1u<PID> nPlayers
                    |> List.map (fun playerID ->
                        playerID, getLanePlayerTroops playerID lane
                        )
                    |> Map.ofList
                match Map.tryFind laneID laneWins with
                | None ->
                    ContestedLaneKnowledge {
                        Troops = troopKnowledge
                        }
                | Some c ->
                    WonLaneKnowledge {
                        Controller = c
                        Troops = troopKnowledge
                        }
                )
        PostBaseFlipBoardKnowledge {
            Lanes = lanesKnowledge
            Discard = List.map getDeadCard d
            }

let private getHandInfos viewerID gameStage =
    match gameStage with
    | Early {HandCards = hco}
    | DrawPileEmpty {HandCards = hco} ->
        hco
        |> Map.partition (fun owner _ -> owner = viewerID)
    | HandsEmpty _ ->
        Map.empty, Map.empty
    |> opLeft (Map.toList >> List.collect (fun (_, cards) -> cards |> List.map getHandCardInfo))
    |> opRight (Map.map (fun _ cards -> List.length cards) >> Map.toList)

let private getPlayActionsInfo gameState =
    let playerID = gameState.TurnState.CurrentPlayer
    match gameState.CardsState.GameStage with
    | Early {HandCards = hc}
    | DrawPileEmpty {HandCards = hc} ->
        Map.find playerID hc
    | HandsEmpty _ ->
        List.empty
    |> List.allPairs (createIDsToLength 1u<LID> (gameState.CardsState.Board.Lanes |> Map.count |> uint))
    |> List.map (fun (laneID, {HandCardID = HandCardID cardID}) ->
        Play (laneID, cardID)
        )

let private getActivateActionsInfo gameState =
    let playerID = gameState.TurnState.CurrentPlayer
    let lanes = gameState.CardsState.Board.Lanes
    lanes
    |> Map.toList
    |> List.collect (fun (laneID, lane) ->
        lane.InactiveUnits
        |> List.filter (fun card -> card.Owner = playerID && card.FreezeStatus = NotFrozen)
        |> List.map (fun {InactiveUnitID = InactiveUnitID id} -> Activate (laneID, id))
        )

let private getPairActionsInfoFromUnits laneID =
    let rec distPairs lst =
        match lst with
        | [] -> []
        | [_] -> []
        | h::t -> List.allPairs [h] t @ distPairs t
    distPairs
    >> List.choose (fun (card1: ActiveUnit, card2: ActiveUnit) ->
        if card1.Rank = card2.Rank then
            let {ActiveUnitID = ActiveUnitID id1} = card1
            let {ActiveUnitID = ActiveUnitID id2} = card2
            CreatePair (laneID, id1, id2)
            |> Some
        else
            None
        )

let private getAttackActionsInfoInLane playerID (laneID, lane) =
    let findOwner toList =
        toList
        >> List.head
        >> function
        | InactiveUnit {Owner = owner}
        | ActiveUnit {Owner = owner}
        | PairedUnit {Owner = owner} -> owner

    let possibleAttackers toList =
        List.filter (
            toList
            >> List.forall (
                dup
                >> opLeft (function
                    | Solo {Owner = o}
                    | Paired {Owner = o} -> o = playerID
                    )
                >> opRight (
                    dup
                    >> function
                    | Ready, Solo {FreezeStatus = fs}
                    | Ready, Paired {FreezeStatus = fs} ->
                        fs = NotFrozen
                    | Exhausted, _ ->
                        false
                    )
                >> (fun (a, b) -> a && b)
                )
            )
    let possibleUnitAttackers = possibleAttackers (Solo >> List.singleton) lane.ActiveUnits
    let possiblePairAttackers = possibleAttackers (pairToFullPairedUnits >> pairToList >> List.map Paired) lane.Pairs

    let addTypeAndOwner T owner =
        function
        | InactiveUnit {InactiveUnitID = InactiveUnitID cid}
        | ActiveUnit {ActiveUnitID = ActiveUnitID cid}
        | PairedUnit {FullPairedUnitID = FullPairedUnitID cid} -> cid
        >> pair owner
        >> T
    let possibleTypeTargets toList T =
        let transform owner = toList >> List.map (addTypeAndOwner T owner)
        List.groupBy (findOwner toList)
        >> List.filter (fst >> (<>) playerID)
        >> List.collect (
            opLeft transform
            >> (uncurry List.collect)
            )
    let possibleSingleTypeTargets = possibleTypeTargets List.singleton
    let possiblePairTypeTargets = possibleTypeTargets pairToList
    let possibleInactiveUnitTargets = possibleSingleTypeTargets InactiveTarget (lane.InactiveUnits |> List.map InactiveUnit)
    let possibleActiveUnitTargets = possibleSingleTypeTargets ActiveSingleTarget (lane.ActiveUnits |> List.map ActiveUnit)
    let possiblePairTargets = possiblePairTypeTargets ActivePairMemberTarget (lane.Pairs |> List.map (pairToFullPairedUnits >> fun (c1, c2) -> PairedUnit c1, PairedUnit c2))
    let allTargets = possibleInactiveUnitTargets @ possibleActiveUnitTargets @ possiblePairTargets

    let tauntTargets =
        allTargets
        |> List.filter (function
            | InactiveTarget _ ->
                false
            | ActiveSingleTarget (_, cardID) ->
                let card =
                    lane.ActiveUnits
                    |> List.find (fun {ActiveUnitID = ActiveUnitID id} -> id = cardID)
                List.contains ProtectsNonTauntAlliesInLane card.Abilities.WhileActive
            | ActivePairMemberTarget (_, cardID) ->
                let card =
                    lane.Pairs
                    |> List.map pairToFullPairedUnits
                    |> List.collect pairToList
                    |> List.find (fun {FullPairedUnitID = FullPairedUnitID id} -> id = cardID)
                List.contains ProtectsNonTauntAlliesInLane card.Abilities.WhileActive
        )

    let availableTargets =
        if List.isEmpty tauntTargets then
            allTargets
        else
            tauntTargets

    let singleAttacks =
        possibleUnitAttackers
        |> List.collect (fun {ActiveUnitID = ActiveUnitID attackerID} ->
            availableTargets
            |> List.map (fun target ->
                SingleAttack (laneID, attackerID, target)
                )
        )
    let pairAttacks =
        possiblePairAttackers
        |> List.collect (fun {Cards = attacker1, attacker2; Abilities = abilities} ->
            let {PairedUnitID = PairedUnitID attackerID1} = attacker1
            let {PairedUnitID = PairedUnitID attackerID2} = attacker2
            availableTargets
            |> List.map (fun target ->
                PairAttack (laneID, (attackerID1, attackerID2), target)
            )
        )
    singleAttacks @ pairAttacks

let private getAttackActionsInfo gameState =
    let playerID = gameState.TurnState.CurrentPlayer
    let lanes = gameState.CardsState.Board.Lanes
    lanes
    |> Map.toList
    |> List.collect (getAttackActionsInfoInLane playerID)

let private getPairActionsInfo gameState =
    let playerID = gameState.TurnState.CurrentPlayer
    let lanes = gameState.CardsState.Board.Lanes
    lanes
    |> Map.toList
    |> List.collect (fun (laneID, {ActiveUnits = activeUnits}) ->
        activeUnits
        |> List.filter (fun card -> card.Owner = playerID)
        |> getPairActionsInfoFromUnits laneID
        )

let private resolveReturnDamage laneID attackerIDs targetCardID cardsState turnState resolutionStack =
    let gameState = {CardsState = cardsState; TurnState = turnState; TurnStage = ActionChoice}
    let newState =
        gameState.CardsState
        |> damageCard (UnitID targetCardID) 1u<health> laneID
    match attackerIDs with
    | SingleCardID id ->
        newState
        |> damageCard (UnitID id) 1u<health> laneID
        |> changeCardsState gameState
        |> toActionChoice
        |> triggerTargetInactiveDeathPowers
        |> moveNonownedDeadCardsToDiscard
        |> moveOwnedDeadCardsToDiscard
        |> toActionChoice
    | PairIDs (id1, id2) ->
        {
            CardsState = newState
            TurnState = gameState.TurnState
            TurnStage = AbilityChoice {
                ChoiceContext = ReturnDamagePairChoiceContext (laneID, (id1, id2), targetCardID)
                ResolutionStack = resolutionStack
            }
        }

let private executePlayAction: ExecutePlayAction = fun laneID cardID cardsState turnState ->
    let playerID = turnState.CurrentPlayer
    let playedCard, newCardsState = removeCardFromHand cardID playerID cardsState
    let newCard = handToInactiveUnit playedCard
    [CardPlayed (newCard, laneID)],
    {
        CardsState =
            newCardsState
            |> addCardToInactiveUnits newCard laneID
            |> removeHandsIfAllEmpty
        TurnState = turnState
        TurnStage = ActionChoice
    }

let private tryDrawCard playerID cardsState =
    match cardsState.GameStage with
    | Early preInfo ->
        let boardInfo = cardsState.Board
        let hands = preInfo.HandCards
        let drawPile = preInfo.DrawPile
        match drawPile.Tail with
        | [] ->
            let newHandCards =
                hands
                |> Map.change playerID (function
                    | Some hc ->
                        Some (hc @ [deckToHandCard cardsState.GetAbilities playerID drawPile.Head])
                    | None ->
                        failwithf "non-existent players can't draw cards"
                    )
            let newCards = {
                cardsState with
                    Board = flipBasesOnBoard preInfo.Bases boardInfo
                    GameStage = DrawPileEmpty {HandCards = newHandCards; LaneWins = Map.empty}
                }
            [CardDrawn playerID; DrawPileExhausted], newCards
        | newTopCard :: newRest ->
            let newHandCards =
                hands
                |> Map.change playerID (function
                    | Some hc ->
                        Some (hc @ [deckToHandCard cardsState.GetAbilities playerID drawPile.Head])
                    | None ->
                        failwithf "non-existent players can't draw cards"
                    )
            let newCards =
                {cardsState with
                    GameStage =
                        Early {
                            preInfo with
                                DrawPile = NonEmptyList.create newTopCard newRest
                                HandCards = newHandCards
                            }
                }
            [CardDrawn playerID], newCards
    | DrawPileEmpty _
    | HandsEmpty _ ->
        [CannotDraw playerID], cardsState

let private healOwnUnitsInLane playerID amount (lane: Lane) =
    let healedInactives, newInactives =
        lane.InactiveUnits
        |> List.map (fun card ->
            if card.Owner = playerID && card.Damage > 0u<health> then
                let healed = healInactiveCard amount card
                Some healed, healed
            else
                None, card
            )
        |> List.unzip
        |> opLeft (List.choose id >> List.map InactiveUnit)
    let healedActives, newActives =
        lane.ActiveUnits
        |> List.map (fun card ->
            if card.Owner = playerID && card.Damage > 0u<health> then
                let healed = healActiveCard amount card
                Some healed, healed
            else
                None, card
            )
        |> List.unzip
        |> opLeft (List.choose id >> List.map ActiveUnit)
    let healedPairedUnits, newPairs =
        lane.Pairs
        |> List.map (fun pair ->
            let {Cards = (card1, card2); Owner = owner} = pair
            if owner = playerID then
                match card1.Damage > 0u<health>, card2.Damage > 0u<health> with
                | false, false ->
                    None, pair
                | true, false ->
                    let newPair = {pair with Cards = healPairedCard amount card1, card2}
                    newPair
                    |> dup
                    |> opLeft (pairToFullPairedUnits >> fst >> List.singleton >> Some)
                | false, true ->
                    let newPair = {pair with Cards = card1, healPairedCard amount card2}
                    newPair
                    |> dup
                    |> opLeft (pairToFullPairedUnits >> snd >> List.singleton >> Some)
                | true, true ->
                    let newPair = {pair with Cards = healPairedCard amount card1, healPairedCard amount card2}
                    newPair
                    |> dup
                    |> opLeft (pairToFullPairedUnits >> pairToList  >> Some)
            else
                None, pair
            )
        |> List.unzip
        |> opLeft (List.choose id >> List.collect (List.map PairedUnit))
    (healedInactives @ healedActives @ healedPairedUnits)
    |> List.map CardHealed,
    {lane with
        InactiveUnits = newInactives
        ActiveUnits = newActives
        Pairs = newPairs
        }
let private healOwnUnits playerID amount cardsState =
    let board = cardsState.Board
    let events, newLanes =
        board.Lanes
        |> Map.toList
        |> List.map (fun (laneID, lane) ->
            let (events, newLane) = healOwnUnitsInLane playerID amount lane
            events, (laneID, newLane)
            )
        |> List.unzip
        |> opPair (List.collect id) (Map.ofList)
    events, {cardsState with Board = {board with Lanes = newLanes}}

let private streamEvents previousEvents (newEvents, state) =
    (previousEvents @ newEvents), state

let private freezeEnemyNonActiveNimbleCardsInLane playerID laneID cardsState =
    let board = cardsState.Board
    let gameStage = cardsState.GameStage
    let newGameStage =
        match gameStage with
        | Early gs ->
            let newBases =
                gs.Bases
                |> Map.add laneID (
                    gs.Bases
                    |> Map.find laneID
                    |> List.map (fun baseCard ->
                        if baseCard.Owner = playerID then
                            baseCard
                        else
                            {baseCard with FreezeStatus = FrozenBy playerID}
                        )
                    )
            Early {gs with Bases = newBases}
        | DrawPileEmpty _
        | HandsEmpty _ ->
            gameStage
    let lane = Map.find laneID board.Lanes
    let frozenInactives, newInactiveUnits =
        lane.InactiveUnits
        |> List.map (fun card ->
            if card.Owner <> playerID then
                let newCard = {card with FreezeStatus = FrozenBy playerID}
                Some newCard, newCard
            else
                None, card
            )
        |> List.unzip
        |> opLeft (List.choose id >> List.map InactiveUnit)
    let frozenActives, newActiveUnits =
        lane.ActiveUnits
        |> List.map (fun card ->
            if card.Owner <> playerID && not (List.contains (InstantNonTargetAbility FreezeEnemiesInLane) card.Abilities.Ignores) then
                let newCard = {card with FreezeStatus = FrozenBy playerID}
                Some newCard, newCard
            else
                None, card
            )
        |> List.unzip
        |> opLeft (List.choose id >> List.map ActiveUnit)
    let frozenPairedUnits, newPairs =
        lane.Pairs
        |> List.map (fun pair ->
            let {Cards = c1, c2; Owner = owner; Abilities = {Ignores = ignores}} = pair
            if owner <> playerID && not (List.contains (InstantNonTargetAbility FreezeEnemiesInLane) ignores) then
                let newCards = {c1 with FreezeStatus = FrozenBy playerID}, {c2 with FreezeStatus = FrozenBy playerID}
                let newPair = {pair with Cards = newCards}
                Some (pairToFullPairedUnits newPair), newPair
            else
                None, pair
            )
        |> List.unzip
        |> opLeft (List.choose id >> List.collect pairToList >> List.map PairedUnit)
    let events =
        (frozenInactives @ frozenActives @ frozenPairedUnits)
        |> List.map CardFrozen
    let newLane = {lane with InactiveUnits = newInactiveUnits; ActiveUnits = newActiveUnits; Pairs = newPairs}
    events,
    {
        cardsState with
            Board = {board with Lanes = board.Lanes |> Map.add laneID newLane}
            GameStage = newGameStage
        }

let private addActiveNonEmpowerActivationAbilitiesInLaneToStack laneID (ActiveUnitID empowererCardID) cardsState turnState =
    let playerID = turnState.CurrentPlayer
    let lane =
        cardsState.Board.Lanes
        |> Map.find laneID
    let powerChoices =
        List.map Solo lane.ActiveUnits @ (lane.Pairs |> List.collect (pairToFullPairedUnits >> pairToList >> List.map Paired))
        |> List.filter (function
            | Solo {ActiveUnitID = ActiveUnitID id; Owner = owner} -> owner = playerID && id <> empowererCardID
            | Paired {FullPairedUnitID = FullPairedUnitID id; Owner = owner} -> owner = playerID && id <> empowererCardID
            )
        |> List.choose (function
            | Solo card ->
                let {ActiveUnitID = ActiveUnitID id; Abilities = abilities} = card
                match abilities.OnActivation with
                | [] -> None
                | oa ->
                    if List.contains ReactivateNonEmpowerActivationPowersInLane oa then
                        None
                    else
                        Some (Solo card, PowerContext (laneID, id, abilities.Name))
            | Paired card ->
                let {FullPairedUnitID = FullPairedUnitID id; Abilities = abilities} = card
                match abilities.OnActivation with
                | [] -> None
                | oa ->
                    if List.contains ReactivateNonEmpowerActivationPowersInLane oa then
                        None
                    else
                        Some (Paired card, PowerContext (laneID, id, abilities.Name))
            )
    match powerChoices with
    | [] -> [], (cardsState, turnState, None)
    | lst ->
        List.map (fst >> CardReactivated) lst,
        (cardsState, turnState, lst |> List.map snd |> fromList |> OrderChoiceEpoch |> Some)

let private flipCard eventType laneID (InactiveUnitID cardID) cardsState =
    removeCardFromInactiveUnits (InactiveUnitID cardID) laneID cardsState
    |> opLeft (inactiveToActiveUnit >> dup >> opLeft (eventType >> List.singleton))
    |> flattenLeft

let private resolveInstantNonTargetAbility: ResolveInstantNonTargetAbility =
    fun event cardsState turnState ->
    let playerID = turnState.CurrentPlayer
    let ability, laneID, cardID = event
    match ability with
    | Draw ->
        tryDrawCard playerID cardsState
        |> opRight (fun cs -> (cs, turnState, None))
    | Discard ->
        [],
        (cardsState, turnState, Some (AbilityChoiceEpoch (DiscardChoiceContext cardID)))
    | ViewInactive ->
        [],
        (cardsState, turnState, Some (AbilityChoiceEpoch (ViewInactiveChoiceContext cardID)))
    | FreezeEnemiesInLane ->
        freezeEnemyNonActiveNimbleCardsInLane playerID laneID cardsState
        |> opRight (fun cs -> cs, turnState, None)
        |> streamEvents [LaneFrozen laneID]
    | HealAllAllies n ->
        healOwnUnits playerID (n*1u<health>) cardsState
        |> opRight (fun cs -> cs, turnState, None)
    | MayMoveAllyToOwnLane ->
        [],
        (cardsState, turnState, Some (AbilityChoiceEpoch (MayMoveAllyToOwnLaneChoiceContext (laneID, cardID))))
    | ExtraActions n ->
        let newTurnState = {turnState with ActionsLeft = turnState.ActionsLeft + n*1u<action>}
        [ActionsGained (playerID, 1u<action>)], (cardsState, newTurnState, None)
    | ChangeMaxAttacksThisTurn n ->
        setMaxCardActions cardID (n*1u<action>) laneID cardsState
        |> opRight (fun cs -> cs, turnState, None)
    | ActivateAlliesInLane ->
        flipInactiveCardsInLaneAndAddActivationPowersToStack playerID laneID cardsState turnState
    | ReactivateNonEmpowerActivationPowersInLane ->
        addActiveNonEmpowerActivationAbilitiesInLaneToStack laneID (ActiveUnitID cardID) cardsState turnState
    | FullyHealSelf ->
        let lane = Map.find laneID cardsState.Board.Lanes
        let fIn (card: InactiveUnit) = {card with Damage = 0u<health>}
        let fAct (card: ActiveUnit) = {card with Damage = 0u<health>}
        let fPair (card: PairedUnit) = {card with Damage = 0u<health>}
        let cs = changeInPlace (changeCardInLane (fIn, fAct, fPair) cardID) laneID cardsState
        let matchID = function
        | InactiveUnit {InactiveUnitID = InactiveUnitID cid} -> cid = cardID
        | ActiveUnit {ActiveUnitID = ActiveUnitID cid} -> cid = cardID
        | PairedUnit {FullPairedUnitID = FullPairedUnitID cid} -> cid = cardID
        let card = findCardInLane matchID lane
        [CardFullyHealedSelf card], (cs, turnState, None)
    | HealSelf n ->
        let lane = Map.find laneID cardsState.Board.Lanes
        let fIn = healInactiveCard (n*1u<health>)
        let fAct = healActiveCard (n*1u<health>)
        let fPair = healPairedCard (n*1u<health>)
        let cs = changeInPlace (changeCardInLane (fIn, fAct, fPair) cardID) laneID cardsState
        let matchID = function
        | InactiveUnit {InactiveUnitID = InactiveUnitID cid} -> cid = cardID
        | ActiveUnit {ActiveUnitID = ActiveUnitID cid} -> cid = cardID
        | PairedUnit {FullPairedUnitID = FullPairedUnitID cid} -> cid = cardID
        let card = findCardInLane matchID lane
        [CardHealedSelf card], (cs, turnState, None)
    | ActivateSelf ->
        let (events, card, cs) = flipCard CardActivatedSelf laneID (InactiveUnitID cardID) cardsState
        let epoch =
            card.Abilities.OnActivation
            |> List.map (swapIn pair (laneID, cardID) >> flattenRight)
            |> fromList
            |> OrderedAbilityEpoch
        events, (cs, turnState, Some epoch)

let private addCardActivationAbilitiesToStack laneID (ActiveUnitID cardID) cardsState maybeStack =
    let {Board = board} = cardsState
    let lane = Map.find laneID board.Lanes
    let card = List.find (fun {ActiveUnitID = ActiveUnitID id} -> id = cardID) lane.ActiveUnits
    let maybeAbilitiesEpoch =
        card.Abilities.OnActivation
        |> List.map (fun ability -> ability, laneID, cardID)
        |> NonEmptyList.tryFromList
        |> Option.map OrderedAbilityEpoch
    maybeStack
    |> NonEmptyList.consOptions maybeAbilitiesEpoch

let rec private processResolutionStack: ProcessResolutionStack =
    fun cardsState turnState (maybeResolutionStack: ResolutionStack option) ->
    match maybeResolutionStack with
    | None ->
        [],
        {
            CardsState = cardsState
            TurnState = turnState
            TurnStage = ActionChoice
        }
    | Some {Head = h; Tail = t} ->
        match h with
        | OrderChoiceEpoch activationPowerContexts ->
            [],
            {
                CardsState = cardsState
                TurnState = turnState
                TurnStage = StackChoice {
                    EpochEvents =
                        activationPowerContexts
                        |> NonEmptyList.toList
                        |> createIDMap 1u<EID>
                        |> NonEmptyMap.fromMap
                    ResolutionStack = NonEmptyList.tryFromList t
                    }
            }
        | AbilityChoiceEpoch choiceContext ->
            [],
            {
                CardsState = cardsState
                TurnState = turnState
                TurnStage = AbilityChoice {
                    ChoiceContext = choiceContext
                    ResolutionStack = NonEmptyList.tryFromList t
                    }
            }
        | OrderedAbilityEpoch triggerEvents ->
            let ({Head = first; Tail = rest}: AbilityEvent epoch) = triggerEvents
            let remainingHead =
                NonEmptyList.tryFromList rest
                |> Option.map OrderedAbilityEpoch
            let remainingStack =
                NonEmptyList.tryFromList t
                |> NonEmptyList.consOptions remainingHead
            let events, (cs, ts, maybeNewEpoch) = resolveInstantNonTargetAbility first cardsState turnState
            let newStack = NonEmptyList.consOptions maybeNewEpoch remainingStack
            streamEvents events (processResolutionStack cs ts newStack)

let private resolveActivationPower: ResolveActivationPower =
    fun laneID (ActiveUnitID cardID) cardsState turnState stack ->
        let {Board = board} = cardsState
        let lane = Map.find laneID board.Lanes
        let card = List.find (fun {ActiveUnitID = ActiveUnitID id} -> id = cardID) lane.ActiveUnits
        let newEpoch =
            card.Abilities.OnActivation
            |> List.map (fun ability -> ability, laneID, cardID)
            |> NonEmptyList.tryFromList
            |> Option.map OrderedAbilityEpoch
        processResolutionStack cardsState turnState (NonEmptyList.consOptions newEpoch stack)

let private resolveAttackerPassivePower laneID attackerIDs (UnitID attackedCardID) turnState cardsState =
    let lane = Map.find laneID cardsState.Board.Lanes
    let card =
        match attackerIDs with
        | SingleAttackerID (ActiveUnitID id) ->
            lane.ActiveUnits
            |> List.find (fun {ActiveUnitID = ActiveUnitID thisID} -> thisID = id)
            |> Solo
        | PairAttackerIDs ((PairedUnitID id), _) ->
            lane.Pairs
            |> List.collect (pairToFullPairedUnits >> pairToList)
            |> List.find (fun {FullPairedUnitID = FullPairedUnitID thisID} -> thisID = id)
            |> Paired
    let abilities =
        match card with
        | Solo {Abilities = a} -> a
        | Paired {Abilities = a} -> a
    let attackingAbilities = (List.map AttackAbility abilities.OnAttack) @ (List.map InstantNonTargetAbility abilities.OnKill)
    match attackingAbilities with
    | [] ->
        {CardsState = cardsState; TurnState = turnState; TurnStage = ActionChoice}
        |> triggerTargetInactiveDeathPowers
        |> moveNonownedDeadCardsToDiscard
        |> moveOwnedDeadCardsToDiscard
    | [AttackAbility (ExtraDamageAgainstExtraMaxHealth 1u)] ->
        let newCardsState = damageCard (UnitID attackedCardID) 1u<health> laneID cardsState
        {CardsState = newCardsState; TurnState = turnState; TurnStage = ActionChoice}
        |> triggerTargetInactiveDeathPowers
        |> moveNonownedDeadCardsToDiscard
        |> moveOwnedDeadCardsToDiscard
    | [AttackAbility DamageExtraTarget] ->
        let activeLaneCards =
            (List.map Solo lane.ActiveUnits) @ List.collect (pairToFullPairedUnits >> pairToList >> List.map Paired) lane.Pairs
        let activeTargetCard =
            activeLaneCards
            |> List.tryFind (function
                | Solo {ActiveUnitID = (ActiveUnitID id)} -> id = attackedCardID
                | Paired {FullPairedUnitID = FullPairedUnitID id} -> id = attackedCardID
                )
        match activeTargetCard with
        | Some (Solo {Abilities = abilities})
        | Some (Paired {Abilities = abilities}) ->
            if List.contains (AttackAbility DamageExtraTarget) abilities.Ignores then
                {CardsState = cardsState; TurnState = turnState; TurnStage = ActionChoice}
                |> triggerTargetInactiveDeathPowers
                |> moveNonownedDeadCardsToDiscard
                |> moveOwnedDeadCardsToDiscard
            else
                let choice = {
                    ChoiceContext = DamageExtraTargetChoiceContext (laneID, (transferAttackerIDs attackerIDs), attackedCardID)
                    ResolutionStack = None
                }
                {
                    CardsState = cardsState
                    TurnState = turnState
                    TurnStage = AbilityChoice choice
                }
        | None ->
            let choice = {
                ChoiceContext = DamageExtraTargetChoiceContext (laneID, (transferAttackerIDs attackerIDs), attackedCardID)
                ResolutionStack = None
            }
            {
                CardsState = cardsState
                TurnState = turnState
                TurnStage = AbilityChoice choice
            }
    | [InstantNonTargetAbility (HealSelf 1u)] ->
        {CardsState = cardsState; TurnState = turnState; TurnStage = ActionChoice}
        |> triggerTargetInactiveDeathPowers
        |> healAttackersIfDefenderDying attackerIDs (UnitID attackedCardID) laneID
        |> moveNonownedDeadCardsToDiscard
        |> moveOwnedDeadCardsToDiscard
    | _ ->
        failwithf "Unrecognised OnAttack + OnKill contents: %A" attackingAbilities

let private executeActivateAction: ExecuteActivateAction = fun laneID (InactiveUnitID cardID) cardsState turnState ->
    let (events, newCard, cs1) =
        flipCard CardActivated laneID (InactiveUnitID cardID) cardsState
    streamEvents
        events
        (resolveActivationPower laneID (ActiveUnitID cardID) (addCardToActiveUnits newCard laneID cs1) turnState None)

let private getTargetIDFromTargetInfo targetInfo =
    match targetInfo with
    | InactiveTarget (_, id)
    | ActiveSingleTarget (_, id)
    | ActivePairMemberTarget (_, id) ->
        UnitID id

let private getAttackerSelfDamage attackerAbilities targetActive targetAbilities =
    if targetActive
        && not (List.contains (DefendAbility ReturnDamage) attackerAbilities.Ignores)
        && (List.contains ReturnDamage targetAbilities.OnDamaged)
    then
        1u<health>
    else
        0u<health>

let private laneCards lane =
    (lane.InactiveUnits |> List.map InactiveUnit)
    @ (lane.ActiveUnits |> List.map ActiveUnit)
    @ (lane.Pairs |> List.collect (pairToFullPairedUnits >> pairToList >> List.map PairedUnit))

let private getAttackInfo findAttackerCards getAbilities baseDamage attackerIDs targetInfo lane =
    let attacker = findAttackerCards attackerIDs lane
    let attackerAbilities = getAbilities attacker
    let targetID = getTargetIDFromTargetInfo targetInfo
    let (targetCard, targetAbilities, targetActive) =
        laneCards lane
        |> List.choose (fun card ->
            match card with
            | InactiveUnit {InactiveUnitID = InactiveUnitID cid} ->
                if UnitID cid = targetID then
                    Some (card, false)
                else
                    None
            | ActiveUnit {ActiveUnitID = ActiveUnitID cid}
            | PairedUnit {FullPairedUnitID = FullPairedUnitID cid} ->
                if UnitID cid = targetID then
                    Some (card, true)
                else
                    None
            )
        |> List.exactlyOne
        |> opLeft (
            dup
            >> opRight (function
                | InactiveUnit {Abilities = a}
                | ActiveUnit {Abilities = a}
                | PairedUnit {Abilities = a} ->
                    a
                )
            )
        |> flattenLeft
    let selfDamage = getAttackerSelfDamage attackerAbilities targetActive targetAbilities
    attacker, targetCard, targetID, baseDamage, selfDamage

let private getSingleAttackInfo =
    let findAttackerCards (ActiveUnitID attackerIDs) lane =
        lane.ActiveUnits
        |> List.find (fun {ActiveUnitID = ActiveUnitID id} -> id = attackerIDs)
    let getAbilities (card: ActiveUnit) = card.Abilities
    getAttackInfo findAttackerCards getAbilities 1u<health>

let private getPairAttackInfo =
    let findAttackerCards (PairedUnitID pid1, PairedUnitID pid2) lane =
        lane.Pairs
        |> List.find (fun {Cards = {PairedUnitID = PairedUnitID id1}, {PairedUnitID = PairedUnitID id2}} ->
            (id1, id2) = (pid1, pid2)
        )
    let getAbilities (pair: Pair) = pair.Abilities
    getAttackInfo findAttackerCards getAbilities 2u<health>

let private executeAttackAction getAttackInfo getAttackerIDList getAttackerList attackerType attackerUnitType attackerIDType laneID (attackerIDs, targetInfo) cardsState turnState =
    let board = cardsState.Board
    let attackerIDList = getAttackerIDList attackerIDs
    let attacker, target, targetID, damage, selfDamage =
        getAttackInfo attackerIDs targetInfo (Map.find laneID board.Lanes)
    let attackerList = getAttackerList attacker
    let damagedState =
        cardsState
        |> (fun cs -> List.fold (fun state cid -> incrementCardActionsUsed cid laneID state) cs attackerIDList)
        |> damageCard targetID damage laneID
        |> (fun cs -> List.fold (fun state cid -> damageCard (UnitID cid) selfDamage laneID state) cs attackerIDList)
    [
        CardAttacked (attackerType attacker, target)
        CardDamaged (target, damage)
    ]
    @ List.map (fun attacker -> CardDamaged (attackerUnitType attacker, selfDamage)) attackerList
    ,
    resolveAttackerPassivePower laneID (attackerIDType attackerIDs) targetID turnState damagedState

let private executeSingleAttackAction: ExecuteSingleAttackAction =
    let getAttackerIDList (ActiveUnitID attackerID) = [attackerID]
    let getAttackerList = List.singleton
    executeAttackAction
        getSingleAttackInfo getAttackerIDList getAttackerList SingleAttacker ActiveUnit SingleAttackerID

let private executePairAttackAction: ExecutePairAttackAction =
    let getAttackerIDList (PairedUnitID cid1, PairedUnitID cid2) = [cid1; cid2]
    let getAttackerList = pairToFullPairedUnits >> pairToList
    executeAttackAction
        getPairAttackInfo getAttackerIDList getAttackerList PairAttacker PairedUnit PairAttackerIDs

let private executeCreatePairAction: ExecuteCreatePairAction = fun laneID (cardID1, cardID2) cardsState turnState ->
    let cards, newCardsState =
        cardsState
        |> removeCardPairFromActiveUnits (cardID1, cardID2) laneID
    let pair = activeUnitsToPair cards
    let pairedState = addCardsToPairs pair laneID newCardsState
    [CardsPaired pair],
    {
        CardsState = pairedState
        TurnState = turnState
        TurnStage = ActionChoice
    }

let private decrementActionsLeft (turnState: TurnInProgress) =
    {turnState with ActionsLeft = turnState.ActionsLeft - 1u<action>}

let private timeoutOwnedFreezeStatesInLane playerID lane =
    {
        lane with
            InactiveUnits =
                lane.InactiveUnits
                |> List.map (fun (card: InactiveUnit) ->
                    if card.FreezeStatus = FrozenBy playerID then
                        {card with FreezeStatus = NotFrozen}
                    else
                        card
                )
            ActiveUnits =
                lane.ActiveUnits
                |> List.map (fun card ->
                    if card.FreezeStatus = FrozenBy playerID then
                        {card with FreezeStatus = NotFrozen}
                    else
                        card
                )
            Pairs =
                lane.Pairs
                |> List.map (fun pair ->
                    let {Cards = c1, c2} = pair
                    let newC1 =
                        if c1.FreezeStatus = FrozenBy playerID then
                                {c1 with FreezeStatus = NotFrozen}
                        else
                            c1
                    let newC2 =
                        if c2.FreezeStatus = FrozenBy playerID then
                            {c2 with FreezeStatus = NotFrozen}
                        else
                            c2
                    {pair with Cards = newC1, newC2}
                )
    }
let private timeoutOwnedFreezeStates playerID cardsState =
    let board = cardsState.Board
    let gameStage = cardsState.GameStage
    let newGameStage =
        match gameStage with
        | Early gs ->
            let newBases =
                gs.Bases
                |> Map.map (fun _ bases ->
                    bases
                    |> List.map (fun baseCard ->
                        match baseCard.FreezeStatus with
                        | FrozenBy freezer ->
                            if freezer = playerID then
                                {baseCard with FreezeStatus = NotFrozen}
                            else
                                baseCard
                        | NotFrozen -> baseCard
                        )
                    )
            Early {gs with Bases = newBases}
        | DrawPileEmpty _
        | HandsEmpty _ ->
            gameStage
    let newLanes =
        board.Lanes
        |> Map.map (fun _ lane -> timeoutOwnedFreezeStatesInLane playerID lane)
    {cardsState with Board = {board with Lanes = newLanes}; GameStage = newGameStage}

let private resetAllCardActionsUsedInLane lane =
    {lane with
        ActiveUnits = lane.ActiveUnits |> List.map (fun card -> {card with ActionsSpent = 0u<action>})
        Pairs =
            lane.Pairs
            |> List.map (fun pair ->
                let {Cards = card1, card2} = pair
                let newCards = {card1 with ActionsSpent = 0u<action>}, {card2 with ActionsSpent = 0u<action>}
                {pair with Cards = newCards}
                )
        }
let private resetAllCardActionsUsed cardsState =
    let board = cardsState.Board
    let newLanes =
        board.Lanes
        |> Map.map (fun _ lane -> resetAllCardActionsUsedInLane lane)
    {cardsState with Board = {board with Lanes = newLanes}}

let private resetMaxActiveActions (activeCard: ActiveUnit) =
    {activeCard with MaxActions = 1u<action>}
let private resetMaxPairedActions (pairedCard: PairedUnit) =
    {pairedCard with MaxActions = 1u<action>}
let private resetMaxActionsInLane lane =
    {lane with
        ActiveUnits = lane.ActiveUnits |> List.map resetMaxActiveActions
        Pairs =
            lane.Pairs
            |> List.map (fun pair ->
                let {Cards = card1, card2} = pair
                let newCards = resetMaxPairedActions card1, resetMaxPairedActions card2
                {pair with Cards = newCards}
                )
        }
let private resetAllMaxCardActions cardsState =
    let board = cardsState.Board
    let newLanes =
        board.Lanes
        |> Map.map (fun _ lane -> resetMaxActionsInLane lane)
    {cardsState with Board = {board with Lanes = newLanes}}

let private displayGameEvent: GameEventToDisplayGameEvent = function
| GameStarted -> DisplayGameStarted
| TurnStarted pid -> DisplayTurnStarted pid
| TurnEnded pid -> DisplayTurnEnded pid
| AbilityChoiceMade (pid, choiceInfo) -> DisplayAbilityChoiceMade (pid, choiceInfo)
| ActionChosen (pid, choiceInfo) -> DisplayActionChosen (pid, choiceInfo)
| StackChoiceMade (pid, choiceInfo) -> DisplayStackChoiceMade (pid, choiceInfo)
| CardPlayed (inactiveCard, laneID) ->
    let {InactiveUnitID = InactiveUnitID cid; Rank = rank; Suit = suit; Abilities = {Name = powerName}; Owner=  owner} = inactiveCard
    DisplayCardPlayed (owner, cid, laneID)
| CardActivated activeCard ->
    let {ActiveUnitID = ActiveUnitID cid; Rank = rank; Suit = suit; Abilities = {Name = powerName}; Owner = owner} = activeCard
    DisplayCardActivated (owner, cid, rank, suit, powerName)
| CardAttacked (attackers, target) ->
    let (attackOwner, attackerIDs) =
        match attackers with
        | SingleAttacker {ActiveUnitID = ActiveUnitID attackerID; Owner = attackOwner} ->
            attackOwner, SingleCardID attackerID
        | PairAttacker {Cards = ({PairedUnitID = PairedUnitID cid1}, {PairedUnitID = PairedUnitID cid2}); Owner = attackOwner} ->
            attackOwner, PairIDs (cid1, cid2)
    let (defenceOwner, targetID) =
        match target with
        | InactiveUnit {InactiveUnitID = InactiveUnitID cid; Owner = pid}
        | ActiveUnit {ActiveUnitID = ActiveUnitID cid; Owner = pid}
        | PairedUnit {FullPairedUnitID = FullPairedUnitID cid; Owner = pid} -> pid, cid
    DisplayCardAttacked (attackOwner, attackerIDs, defenceOwner, targetID)
| CardDamaged (damaged, damage) ->
    let (damagedOwner, damagedID) =
        match damaged with
        | InactiveUnit {InactiveUnitID = InactiveUnitID cid; Owner = pid}
        | ActiveUnit {ActiveUnitID = ActiveUnitID cid; Owner = pid}
        | PairedUnit {FullPairedUnitID = FullPairedUnitID cid; Owner = pid} -> pid, cid
    DisplayCardDamaged (damagedOwner, damagedID, damage)
| CardsPaired pair ->
    let {Owner = pid; Cards = (card1, card2); Rank = rank; Abilities = {Name = powerName}} = pair
    let {PairedUnitID = PairedUnitID cardID1; Suit = suit1} = card1
    let {PairedUnitID = PairedUnitID cardID2; Suit = suit2} = card2
    DisplayCardsPaired (pid, cardID1, cardID2, suit1, suit2, rank, powerName)
| ActionsGained (pid, n) ->
    DisplayActionsGained (pid, n)
| AttacksSet (pid, (card: ActiveCard), n) ->
    let (cardID, rank, suit, powerName) =
        match card with
        | Solo {ActiveUnitID = ActiveUnitID cid; Rank = rank; Suit = suit; Abilities = {Name = pn}}
        | Paired {FullPairedUnitID = FullPairedUnitID cid; Rank = rank; Suit = suit; Abilities = {Name = pn}} ->
            cid, rank, suit, pn
    DisplayAttacksSet (pid, cardID, rank, suit, powerName, n)
| CannotDraw pid -> DisplayCannotDraw pid
| CardDrawn pid -> DisplayCardDrawn pid
| DrawPileExhausted -> DisplayDrawPileExhausted
| LaneFrozen laneID -> DisplayLaneFrozen laneID
| CardFrozen unitCard ->
    match unitCard with
    | InactiveUnit {Rank = rank; Suit = suit; Abilities = {Name = powerName}; Owner = owner}
    | ActiveUnit {Rank = rank; Suit = suit; Abilities = {Name = powerName}; Owner = owner}
    | PairedUnit {Rank = rank; Suit = suit; Abilities = {Name = powerName}; Owner = owner} ->
        DisplayCardFrozen (rank, suit, powerName, owner)
| CardHealed unitCard ->
    match unitCard with
    | InactiveUnit {Rank = rank; Suit = suit; Abilities = {Name = powerName}; Damage = damage}
    | ActiveUnit {Rank = rank; Suit = suit; Abilities = {Name = powerName}; Damage = damage}
    | PairedUnit {Rank = rank; Suit = suit; Abilities = {Name = powerName}; Damage = damage} ->
        DisplayCardHealed (rank, suit, powerName, damage)
| CardReactivated activeCard ->
    match activeCard with
    | Solo {Rank = rank; Suit = suit; Abilities = {Name = powerName}}
    | Paired {Rank = rank; Suit = suit; Abilities = {Name = powerName}} ->
        DisplayCardReactivated (rank, suit, powerName)
| CardFullyHealedSelf unitCard ->
    match unitCard with
    | InactiveUnit {Rank = rank; Suit = suit; Abilities = {Name = powerName}}
    | ActiveUnit {Rank = rank; Suit = suit; Abilities = {Name = powerName}}
    | PairedUnit {Rank = rank; Suit = suit; Abilities = {Name = powerName}} ->
        DisplayCardFullyHealedSelf (rank, suit, powerName)
| CardHealedSelf unitCard ->
    match unitCard with
    | InactiveUnit {Rank = rank; Suit = suit; Abilities = {Name = powerName}; Damage = damage}
    | ActiveUnit {Rank = rank; Suit = suit; Abilities = {Name = powerName}; Damage = damage}
    | PairedUnit {Rank = rank; Suit = suit; Abilities = {Name = powerName}; Damage = damage} ->
        DisplayCardHealedSelf (rank, suit, powerName, damage)
| CardActivatedSelf activeUnit ->
    let {ActiveUnitID = ActiveUnitID cid; Rank = rank; Suit = suit; Abilities = {Name = powerName}} = activeUnit
    DisplayCardActivatedSelf (cid, rank, suit, powerName)

let private getDisplayInfo: GameStateToDisplayInfo = function
| GameStateDuringTurn {CardsState = cs; TurnState = ts; TurnStage = AbilityChoice tg} ->
    let id = ts.CurrentPlayer
    let (playerHandInfo, opponentHandSizes) = getHandInfos id cs.GameStage
    let boardKnowledge = getBoardKnowledge id cs ts
    AbilityChoiceDisplayInfo {
        CurrentPlayer = id
        ActionsLeft = ts.ActionsLeft
        BoardKnowledge = boardKnowledge
        PlayerHand = playerHandInfo
        OpponentHandSizes = opponentHandSizes
        ChoiceContext = tg.ChoiceContext
    }
| GameStateDuringTurn {CardsState = cs; TurnState = ts; TurnStage = StackChoice tg} ->
    let id = ts.CurrentPlayer
    let (playerHandInfo, opponentHandSizes) = getHandInfos id cs.GameStage
    let boardKnowledge = getBoardKnowledge id cs ts
    StackChoiceDisplayInfo {
        CurrentPlayer = id
        ActionsLeft = ts.ActionsLeft
        BoardKnowledge = boardKnowledge
        PlayerHand = playerHandInfo
        OpponentHandSizes = opponentHandSizes
        Stack = {
            Head = tg.EpochEvents |> NonEmptyMap.toMap |> Map.toList |> List.map snd |> NonEmptyList.fromList
            Tail =
                match tg.ResolutionStack with
                | None -> []
                | Some rs ->
                    rs
                    |> NonEmptyList.toList
                    |> List.choose (function
                        | OrderChoiceEpoch e -> Some e
                        | _ -> None
                        )
            }
    }
| GameStateDuringTurn {CardsState = cs; TurnState = ts; TurnStage = ActionChoice} ->
    let id = ts.CurrentPlayer
    let (playerHandInfo, opponentHandSizes) = getHandInfos id cs.GameStage
    let boardKnowledge = getBoardKnowledge id cs ts
    TurnDisplayInfo {
        CurrentPlayer = id
        ActionsLeft = ts.ActionsLeft
        BoardKnowledge = boardKnowledge
        PlayerHand = playerHandInfo
        OpponentHandSizes = opponentHandSizes
    }
| GameStateBetweenTurns {TurnState = ts} ->
    SwitchDisplayInfo ts.Player
| GameStateWon {Winner = winner; LaneWins = laneWins} ->
    WonGameDisplayInfo {
        Winner = winner
        LaneWins = getPlayerLaneWins laneWins
    }
| GameStateTied {LaneWins = laneWins} ->
    TiedGameDisplayInfo {
        LaneWins = getPlayerLaneWins laneWins
    }

let private getPossibleActionChoiceActionPairs cs ts =
    let gs = {CardsState = cs; TurnState = ts; TurnStage = ActionChoice}
    if ts.ActionsLeft = 0u<action> then
        TurnActionChoicePair (gs, EndTurn)
        |> List.singleton
    else
        let actions =
            getPlayActionsInfo gs
            @ getActivateActionsInfo gs
            @ getAttackActionsInfo gs
            @ getPairActionsInfo gs
        if List.isEmpty actions then
            TurnActionChoicePair (gs, EndTurn)
            |> List.singleton
        else
            actions
            |> List.map (fun action -> TurnActionChoicePair (gs, ActionChoiceInfo action))

let private getPossibleStackChoiceActionPairs cs ts sc =
    sc.EpochEvents
    |> NonEmptyMap.toMap
    |> Map.toList
    |> List.map (fun (eventID, event) ->
        StackChoicePair ((cs, ts, sc), (eventID, event))
    )

let private getDiscardChoiceActionPairs cs ts rs powerCardID =
    let playerID = ts.CurrentPlayer
    match cs.GameStage with
    | Early {HandCards = hc}
    | DrawPileEmpty {HandCards = hc} ->
        hc
        |> Map.find playerID
        |> List.map (fun {HandCardID = HandCardID id} ->
            AbilityChoicePair ((cs, ts, rs), DiscardChoice (powerCardID, id))
            )
    | HandsEmpty _ ->
        failwithf "Can't discard from an empty hand"

let private getViewInactiveChoiceActionPairs cs ts rs powerCardID =
    let playerID = ts.CurrentPlayer
    let unknownInactiveUnitIDs =
        cs.Board.Lanes
        |> Map.toList
        |> List.collect (fun (_, lane) -> lane.InactiveUnits)
        |> List.filter (fun card -> not (Set.contains playerID card.KnownBy))
        |> List.map (fun {InactiveUnitID = InactiveUnitID id} -> ForesightTargetID id)
    let unknownFaceDownCardIDs =
        match cs.GameStage with
        | Early {Bases = bases} ->
            let unknownBaseIDs =
                bases
                |> Map.toList
                |> List.collect (
                    snd
                    >> List.filter (fun card -> not (Set.contains playerID card.KnownBy))
                    >> List.map (fun {BaseCardID = BaseCardID id} -> ForesightTargetID id)
                    )
            unknownBaseIDs @ unknownInactiveUnitIDs
        | DrawPileEmpty _
        | HandsEmpty _ ->
            unknownInactiveUnitIDs
    unknownFaceDownCardIDs
    |> List.map (fun (ForesightTargetID id) ->
        AbilityChoicePair ((cs, ts, rs), ViewInactiveChoice (powerCardID, id))
        )

let private getMayMoveAllyChoiceActionPairs cs ts rs laneID powerCardID =
    let playerID = ts.CurrentPlayer
    let pairs =
        cs.Board.Lanes
            |> Map.filter (fun targetLaneID _ -> targetLaneID <> laneID)
        |> Map.toList
        |> List.collect (fun (targetLaneID, lane) ->
            (lane.InactiveUnits |> List.choose (fun {InactiveUnitID = InactiveUnitID id; Owner = ownerID}-> if ownerID = playerID then Some id else None))
            @ (lane.ActiveUnits |> List.choose (fun {ActiveUnitID = ActiveUnitID id; Owner = ownerID} -> if ownerID = playerID then Some id else None))
            @ (lane.Pairs |> List.collect (fun {Cards = {PairedUnitID = PairedUnitID id1}, {PairedUnitID = PairedUnitID id2}; Owner = ownerID} -> if ownerID = playerID then [id1; id2] else []))
            |> List.map (fun id -> targetLaneID, id)
        )
        |> List.map (fun (targetLaneID, targetCardID) ->
            AbilityChoicePair ((cs, ts, rs), MayMoveAllyToOwnLaneChoice (Some (laneID, powerCardID, targetLaneID, targetCardID)))
        )
    if List.isEmpty pairs then
        pairs
    else
        AbilityChoicePair ((cs, ts, rs), MayMoveAllyToOwnLaneChoice (None)) :: pairs

let private getDamageExtraTargetChoiceActionPairs cs ts rs laneID powerCardID originalTargetCardID =
    let playerID = ts.CurrentPlayer
    let lane = Map.find laneID cs.Board.Lanes
    let originalTargetCard =
        match List.tryFind (fun (card: InactiveUnit) -> card.InactiveUnitID = InactiveUnitID originalTargetCardID) lane.InactiveUnits with
        | Some card -> InactiveUnit card
        | None ->
            match List.tryFind (fun (card: ActiveUnit) -> card.ActiveUnitID = ActiveUnitID originalTargetCardID) lane.ActiveUnits with
            | Some card -> ActiveUnit card
            | None ->
                lane.Pairs
                |> List.collect (pairToFullPairedUnits >> pairToList)
                |> List.find (fun card -> card.FullPairedUnitID = FullPairedUnitID originalTargetCardID)
                |> PairedUnit
    let activeTauntCheckTargets, nonActiveTauntCheckTargets =
        let inactiveTargets =
            lane.InactiveUnits
            |> List.filter (fun card -> card.Owner <> playerID && card.InactiveUnitID <> InactiveUnitID originalTargetCardID)
        let activeTauntTargets, activeNonTauntTargets =
            lane.ActiveUnits
            @ (List.collect (pairToFullPairedUnits >> opBoth fullPairedToActiveUnit >> pairToList) lane.Pairs)
            |> List.filter (fun card ->
                card.Owner <> playerID
                && card.ActiveUnitID <> ActiveUnitID originalTargetCardID
                && not (List.contains (AttackAbility DamageExtraTarget) card.Abilities.Ignores)
                )
            |> List.partition (fun card -> List.contains ProtectsNonTauntAlliesInLane card.Abilities.WhileActive)
        List.map (fun {ActiveUnitID = id} -> id) activeTauntTargets,
        ((List.map (fun {InactiveUnitID = InactiveUnitID id} -> UnitID id) inactiveTargets)
        @ (List.map (fun {ActiveUnitID = ActiveUnitID id} -> UnitID id) activeNonTauntTargets))
    let originalTargetIsActiveTaunt =
        match originalTargetCard with
        | InactiveUnit _ -> false
        | ActiveUnit {Abilities = a}
        | PairedUnit {Abilities = a} -> List.contains ProtectsNonTauntAlliesInLane a.WhileActive
    let legalTargets =
        if List.isEmpty activeTauntCheckTargets && not (originalTargetIsActiveTaunt)
        then
            nonActiveTauntCheckTargets
        else
            activeTauntCheckTargets |> List.map (fun (ActiveUnitID id) -> UnitID id)
    legalTargets
    |> List.map (fun (UnitID cardID) ->
        AbilityChoicePair ((cs, ts, rs), DamageExtraTargetChoice (laneID, powerCardID, cardID))
    )

let private getReturnDamagePairChoiceActionPairs cs ts rs laneID powerCardIDs originalTargetCardID =
    pairToList powerCardIDs
    |> List.map (fun id ->
        AbilityChoicePair (
            (cs, ts, rs),
            ReturnDamagePairChoice (laneID, PairIDs powerCardIDs, originalTargetCardID, id)
        )
    )

let private getPossibleAbilityChoiceActionPairs cs ts ac =
    match ac.ChoiceContext with
    | DiscardChoiceContext powerCardID ->
        getDiscardChoiceActionPairs cs ts ac.ResolutionStack powerCardID
    | ViewInactiveChoiceContext powerCardID ->
        getViewInactiveChoiceActionPairs cs ts ac.ResolutionStack powerCardID
    | MayMoveAllyToOwnLaneChoiceContext (laneID, powerCardID) ->
        getMayMoveAllyChoiceActionPairs cs ts ac.ResolutionStack laneID powerCardID
    | DamageExtraTargetChoiceContext (laneID, powerCardID, originalTargetCardID) ->
        getDamageExtraTargetChoiceActionPairs cs ts ac.ResolutionStack laneID powerCardID originalTargetCardID
    | ReturnDamagePairChoiceContext (laneID, powerCardIDs, originalTargetCardID) ->
        getReturnDamagePairChoiceActionPairs cs ts ac.ResolutionStack laneID powerCardIDs originalTargetCardID

let private getPossibleTurnActionPairs gs =
    match gs with
    | {CardsState = cs; TurnState = ts; TurnStage = ActionChoice} ->
        getPossibleActionChoiceActionPairs cs ts
    | {CardsState = cs; TurnState = ts; TurnStage = StackChoice sc} ->
        getPossibleStackChoiceActionPairs cs ts sc
    | {CardsState = cs; TurnState = ts; TurnStage = AbilityChoice ac} ->
        getPossibleAbilityChoiceActionPairs cs ts ac

let private getPossibleActionPairs: GetPossibleActionPairs = function
| GameStateBetweenTurns gs ->
    StartTurnPair gs
    |> List.singleton
| GameStateDuringTurn gs ->
    getPossibleTurnActionPairs gs
| GameStateWon _
| GameStateTied _ ->
    List.empty

let private cancelPowerChoiceIfNoChoices state =
    let actionPairs = getPossibleActionPairs state
    match state, actionPairs with
    | GameStateDuringTurn gs, [] ->
        match gs.TurnStage with
        | AbilityChoice _ ->
            toActionChoice gs
            |> triggerTargetInactiveDeathPowers
            |> moveNonownedDeadCardsToDiscard
            |> moveOwnedDeadCardsToDiscard
            |> GameStateDuringTurn
        | _ ->
            state
    | _ ->
        state

let private executeDiscardChoice gameState playerID discardeeCardID  =
    let (discardeeCard, cs) = removeCardFromHand (HandCardID discardeeCardID) playerID gameState.CardsState
    let convertedCard = handToDiscardedCard discardeeCard
    let newCardsState = {
        cs with
            Board = {cs.Board with Discard = cs.Board.Discard @ [convertedCard]}
        }
    changeCardsState gameState newCardsState

let private executeViewInactiveChoice gameState playerID targetCardID =
    gameState.CardsState
    |> makeCardKnown targetCardID playerID
    |> changeCardsState gameState

let private executeMayMoveAllyChoice gameState maybeMove =
    match maybeMove with
    | Some (laneID, powerCardID, targetLaneID, targetCardID) ->
        gameState.CardsState
        |> changeCardLane (UnitID targetCardID) targetLaneID laneID
        |> changeCardsState gameState
    | None ->
        gameState

let private executeDamageExtraTargetChoice gameState resolutionStack laneID powerCardIDs targetCardID =
    let lane = Map.find laneID gameState.CardsState.Board.Lanes
    let activeTarget =
        List.map Solo lane.ActiveUnits @ (List.collect (pairToFullPairedUnits >> pairToList >> List.map Paired) lane.Pairs)
        |> List.tryFind (function
            | Solo {ActiveUnitID = ActiveUnitID cardID}
            | Paired {FullPairedUnitID = FullPairedUnitID cardID} -> cardID = targetCardID
            )
    match activeTarget with
    | Some (Solo {Abilities = {OnDamaged = od}})
    | Some (Paired {Abilities = {OnDamaged = od}}) ->
        match od with
        | [] ->
            gameState.CardsState
            |> damageCard (UnitID targetCardID) 1u<health> laneID
            |> changeCardsState gameState
            |> toActionChoice
            |> triggerTargetInactiveDeathPowers
            |> moveNonownedDeadCardsToDiscard
            |> moveOwnedDeadCardsToDiscard
        | [ReturnDamage] ->
            resolveReturnDamage laneID powerCardIDs targetCardID gameState.CardsState gameState.TurnState resolutionStack
        | _ ->
            failwithf "Unrecognised OnDamaged contents: %A" od
    | None ->
        gameState.CardsState
        |> damageCard (UnitID targetCardID) 1u<health> laneID
        |> changeCardsState gameState
        |> toActionChoice
        |> triggerTargetInactiveDeathPowers
        |> moveNonownedDeadCardsToDiscard
        |> moveOwnedDeadCardsToDiscard

let private executeReturnDamagePairChoice gameState laneID whichID =
    gameState.CardsState
    |> damageCard (UnitID whichID) 1u<health> laneID
    |> changeCardsState gameState
    |> toActionChoice
    |> triggerTargetInactiveDeathPowers
    |> moveNonownedDeadCardsToDiscard
    |> moveOwnedDeadCardsToDiscard

let private executeAbilityChoice: ExecuteAbilityChoice = fun abilityChoiceInfo (cardsState, turnState, resolutionStack) ->
    let gameState = {
        CardsState = cardsState
        TurnState = turnState
        TurnStage = ActionChoice
    }
    let playerID = turnState.CurrentPlayer
    [AbilityChoiceMade (playerID, abilityChoiceInfo)],
    match abilityChoiceInfo with
    | DiscardChoice (_, discardeeCardID) ->
        executeDiscardChoice gameState playerID discardeeCardID
    | ViewInactiveChoice (_, targetCardID) ->
        executeViewInactiveChoice gameState playerID targetCardID
    | MayMoveAllyToOwnLaneChoice maybeMove ->
        executeMayMoveAllyChoice gameState maybeMove
    | DamageExtraTargetChoice (laneID, powerCardIDs, targetCardID) ->
        executeDamageExtraTargetChoice gameState resolutionStack laneID powerCardIDs targetCardID
    | ReturnDamagePairChoice (laneID, powerCardID, targetCardID, whichID) ->
        executeReturnDamagePairChoice gameState laneID whichID
    |> toActionChoice

let private executeTurnAction: ExecuteTurnAction = fun action (cardsState, turnState) ->
    let ts = decrementActionsLeft turnState
    let playerID = ts.CurrentPlayer
    let (actionEvents, postAction) =
        match action with
        | Play (laneID, cardID) ->
            executePlayAction laneID (HandCardID cardID) cardsState ts
        | Activate (laneID, cardID) ->
            executeActivateAction laneID (InactiveUnitID cardID) cardsState ts
        | SingleAttack (laneID, attackerID, targetInfo) ->
            executeSingleAttackAction laneID (ActiveUnitID attackerID, targetInfo) cardsState ts
        | PairAttack (laneID, (attackerID1, attackerID2), targetInfo) ->
            executePairAttackAction laneID ((PairedUnitID attackerID1, PairedUnitID attackerID2), targetInfo) cardsState ts
        | CreatePair (laneID, cardID1, cardID2) ->
            executeCreatePairAction laneID (ActiveUnitID cardID1, ActiveUnitID cardID2) cardsState ts
    ActionChosen (playerID, action) :: actionEvents,
    match postAction.TurnStage with
    | AbilityChoice _
    | StackChoice _ ->
        postAction
    | ActionChoice ->
        updateLaneWins postAction

let private startPlayerTurn: StartTurn = fun (cs, ts) ->
    let playerID = ts.Player
    let events, cs2 =
        cs
        |> tryDrawCard playerID
        |> opRight (timeoutOwnedFreezeStates playerID)
    events,
    {
        CardsState = cs2
        TurnState = {
            CurrentPlayer = playerID
            NPlayers = ts.NPlayers
            ActionsLeft = ts.Actions
            FutureActionCounts = ts.FutureActionCounts
        }
        TurnStage = ActionChoice
    }

let private executeEndingTurnAction: EndTurn = fun (cardsState, turnState) ->
    let (NPlayers nPlayers) = turnState.NPlayers
    let nextPlayer =
        if turnState.CurrentPlayer = nPlayers*1u<PID> then
            1u<PID>
        else
            turnState.CurrentPlayer + 1u<PID>
    let actions, nextFutureActionCounts =
        match turnState.FutureActionCounts with
        | [] -> 3u<action>, []
        | h :: t -> h, t
    {
        CardsState =
            cardsState
            |> resetAllCardActionsUsed
            |> resetAllMaxCardActions
        TurnState = {
            Player = nextPlayer
            NPlayers = turnState.NPlayers
            Actions = actions
            FutureActionCounts = nextFutureActionCounts
            }
        }

let private executeStackChoice: ExecuteStackChoice = fun eventID (cardsState, turnState, stackChoice) ->
    let chosen, remaining =
        stackChoice.EpochEvents
        |> NonEmptyMap.toMap
        |> (fun m -> Map.find eventID m, Map.remove eventID m)
    let (laneID, cardID, _) = chosen
    let getMaybeRemainingHead =
        Map.toList
        >> List.map snd
        >> NonEmptyList.tryFromList
        >> Option.map OrderChoiceEpoch
    stackChoice.ResolutionStack
    |> NonEmptyList.consOptions (getMaybeRemainingHead remaining)
    |> resolveActivationPower laneID (ActiveUnitID cardID) cardsState turnState
    |> streamEvents [StackChoiceMade (turnState.CurrentPlayer, chosen)]

let private executeTurnChange = fun eventType -> splitFun (snd >> eventType >> List.singleton)
let private executeStartTurn (startPlayerTurn: StartTurn) =
    startPlayerTurn >> opRight GameStateDuringTurn
let private executeEndTurn: ExecuteEndTurn = fun endPlayerTurn ->
    let getCurrentPlayer ts = ts.CurrentPlayer
    executeTurnChange (getCurrentPlayer >> TurnEnded) (endPlayerTurn >> GameStateBetweenTurns)

let private addWithChoice executeChoice stateFn choiceFn fromState =
    pairRev fromState
    >> splitFun (uncurry executeChoice >> opRight stateFn) (fst >> choiceFn)
    >> flattenLeft
let private executeAction: ExecuteAction = function
// Want to do some checking that we have the right player
| AbilityChoicePair (state, choiceInfo) ->
    addWithChoice executeAbilityChoice checkForGameEnd AbilityChoiceInfo state choiceInfo
| StackChoicePair (state, choice) ->
    let executeChoice = fun choice -> executeStackChoice (fst choice)
    addWithChoice executeChoice GameStateDuringTurn StackChoiceInfo state choice
| TurnActionChoicePair (gameState, ActionChoiceInfo choiceInfo) ->
    addWithChoice executeTurnAction checkForGameEnd (ActionChoiceInfo >> TurnActionInfo) (gameState.CardsState, gameState.TurnState) choiceInfo
| TurnActionChoicePair (gameState, EndTurn) ->
    let parts (gs: GameStateDuringTurn) = gs.CardsState, gs.TurnState
    gameState
    |> pairRev EndTurn
    |> opPair (parts >> executeEndTurn executeEndingTurnAction) TurnActionInfo
    |> flattenLeft
| StartTurnPair gameState ->
    let parts (gs: GameStateBetweenTurns) = gs.CardsState, gs.TurnState
    gameState
    |> parts
    |> executeStartTurn startPlayerTurn
    |> pairRev StartTurn
    |> flattenLeft

let rec private makeNextActionInfo: CreateUIOutput = fun inProgress actionPair ->
    let gameEvents, newState, action = executeAction actionPair
    let checkedGameState = cancelPowerChoiceIfNoChoices newState
    // Generation of next action's resulting capabilities is part of the
    // generated capability's body, since assigning them here requires
    // generating the entire game space, blowing the stack
    let capability() = inProgress gameEvents checkedGameState
    {
        Action = action
        Capability = capability
        }
let rec private inProgress: GetInProgress = fun gameEvents gameState ->
    let nextActions =
        getPossibleActionPairs gameState
        |> List.map (makeNextActionInfo inProgress)
    InProgress (List.map displayGameEvent gameEvents, getDisplayInfo gameState, nextActions)

let private newGameState getAbilities (NPlayers nPlayers) (NLanes nLanes) bases handCards removed drawPile =
    {
        CardsState = {
            Board = {
                Lanes =
                    List.replicate (int nLanes) emptyLane
                    |> createIDMap 1u<LID>
                Discard = List.empty
                }
            GetAbilities = getAbilities
            GameStage = Early {
                Bases = bases
                DrawPile = drawPile
                HandCards = handCards
            }
            Removed = removed
        }
        TurnState = {
            Player = 1u<PID>
            NPlayers = NPlayers nPlayers
            Actions = 2u<action>
            FutureActionCounts = List.empty
        }
    }
    |> GameStateBetweenTurns

let private prepareBases getAbilities (NLanes nLanes) =
    List.splitInto (int nLanes)
    >> createIDMap 1u<LID>
    >> Map.map (fun _ ->
        zipIDs 1u<PID>
        >> List.map (fun (ownerID, (cardID, rank, suit)) ->
            {
                BaseCardID = BaseCardID cardID
                Rank = rank
                Suit = suit
                Abilities = getAbilities rank
                Owner = ownerID
                KnownBy = Set.empty
                FreezeStatus = NotFrozen
            }
        )
    )
let private prepareHands getAbilities (NPlayers nPlayers) =
    let toHandCard = swap >> uncurry (handCard getAbilities)
    let playerIDs =
        createIDsToLength 1u<PID> nPlayers
        |> List.collect (List.replicate 5)
    List.zip playerIDs
    >> List.groupBy fst
    >> List.map (opRight (List.map toHandCard))
    >> Map.ofList
let private prepareRemoved = List.map removedCard >> Set.ofList
let private prepareHead n fn = List.splitAt (int n) >> opLeft fn
let private prepareTop n fn = prepare (prepareHead n fn)

let private createGame: CreateGame = fun (NPlayers nPlayers) (NLanes nLanes) ->
    let getAbilities = basePowers
    newGameState getAbilities (NPlayers nPlayers) (NLanes nLanes)
    |> pairRev (createUnshuffledDeck())
    |> opRight (shuffle >> zipIDs 1u<CID> >> List.map flattenRight)
    |> prepareTop (nPlayers*nLanes) (prepareBases getAbilities (NLanes nLanes))
    |> prepareTop (5u*nPlayers) (prepareHands getAbilities (NPlayers nPlayers))
    |> prepareTop 10u prepareRemoved
    |> opRight (List.map deckCard >> fromList)
    |> call
    |> inProgress [GameStarted]

let api: API = {
    NewGame = fun () -> createGame (NPlayers 2u) (NLanes 3u)
}
