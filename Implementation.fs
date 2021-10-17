module Implementation
open Domain
open NonEmptyList
open EventStack

let private createIDs start lst =
    [for i in 0..List.length lst - 1 -> start + LanguagePrimitives.Int32WithMeasure i]

let private createIDsToLength start len =
    [for i in 0..len - 1 -> start + LanguagePrimitives.Int32WithMeasure i]
let private zipIDs start lst =
    let IDs = [for i in 0..List.length lst - 1 -> start + LanguagePrimitives.Int32WithMeasure i]
    List.zip IDs lst
let private createIDMap start lst =
    zipIDs start lst
    |> Map.ofList

type private RemovedCard = {
    CardID: CardID
    Power: Power
}

type private HandCard = {
    CardID: CardID
    Power: Power
}

type private BaseCard = {
    CardID: CardID
    Power: Power
    Owner: PlayerID
    KnownBy: PlayerID Set
}

type private HandCards = HandCard list
type private Bases = BaseCard list

type private FreezeStatus =
| FrozenBy of PlayerID
| NotFrozen

type private InactiveUnit = {
    CardID: CardID
    Power: Power
    Owner: PlayerID
    KnownBy: PlayerID Set
    Damage: Damage
    FreezeStatus: FreezeStatus
}

type private ActiveUnit = {
    CardID: CardID
    Power: Power
    Owner: PlayerID
    Damage: Damage
    ActionsSpent: Actions
    MaxActions: Actions
    FreezeStatus: FreezeStatus
}

type private UnitCard =
| InactiveUnit of InactiveUnit
| ActiveUnit of ActiveUnit

type private Pairs = (ActiveUnit * ActiveUnit) list

type private FaceDownDiscardedCard = {
    CardID: CardID
    Power: Power
    KnownBy: PlayerID Set
}

type private FaceUpDiscardedCard = {
    CardID: CardID
    Power: Power
}

type private DiscardedCard =
| FaceDownDiscardedCard of FaceDownDiscardedCard
| FaceUpDiscardedCard of FaceUpDiscardedCard

let private (|Exhausted|Ready|) (card: ActiveUnit) =
    if card.ActionsSpent >= card.MaxActions then
        Exhausted
    else
        Ready

type private Discard = DiscardedCard list
type private RemovedCards = RemovedCard Set
type private InactiveUnits = InactiveUnit list
type private ActiveUnits = ActiveUnit list

type private DrawPile = HandCard nonEmptyList

type private Lane = {
    InactiveUnits: InactiveUnits
    ActiveUnits: ActiveUnits
    Pairs: Pairs
}

let private emptyLane = {
    InactiveUnits = List.empty
    ActiveUnits = List.empty
    Pairs = List.empty
}

type private LaneControl =
| Contested
| Empty
| Won of PlayerID

let private laneSolePresence (lane: Lane) =
    let cardOwners =
        (lane.InactiveUnits |> List.map (fun card -> card.Owner))
        @ (lane.ActiveUnits |> List.map (fun card -> card.Owner))
        @ (lane.Pairs |> List.collect (fun (card1, card2) -> [card1.Owner; card2.Owner]))
    let playerCounts =
        cardOwners
        |> List.countBy id
    match playerCounts with
    | [] -> Empty
    | [(controller, _)] -> Won controller
    | _ -> Contested

type private CardRemover<'T> = CardID -> Lane -> 'T * Lane
type private CardsRemover<'T> = CardID list -> Lane -> 'T list * Lane
type private CardAdder<'T> = 'T -> Lane -> Lane
type private CardsAdder<'T> = 'T list -> Lane -> Lane

let private removeCardFromInactiveUnits: CardRemover<InactiveUnit> = fun cardID lane ->
    let removedCards, newInactiveUnits = List.partition (fun (c: InactiveUnit) -> c.CardID = cardID) lane.InactiveUnits
    removedCards.Head, {lane with InactiveUnits = newInactiveUnits}
let private addCardToActiveUnits: CardAdder<ActiveUnit> = fun cardID lane ->
    {lane with ActiveUnits = lane.ActiveUnits @ [cardID]}
let private addCardsToActiveUnits: CardsAdder<ActiveUnit> = fun cards lane ->
    {lane with ActiveUnits = lane.ActiveUnits @ cards}
let private removeCardsFromInactiveUnits: CardsRemover<InactiveUnit> = fun cardIDs lane ->
    let removed, newInactiveUnits = List.partition (fun (card: InactiveUnit) -> not (List.contains card.CardID cardIDs)) lane.InactiveUnits
    removed, {lane with InactiveUnits = newInactiveUnits}
let private removeCardsFromActiveUnits: CardsRemover<ActiveUnit> = fun cardIDs lane ->
    let removed, newActiveUnits = List.partition (fun (card: ActiveUnit) -> not (List.contains card.CardID cardIDs)) lane.ActiveUnits
    removed, {lane with ActiveUnits = newActiveUnits}
let private addCardPairPartnersToActiveUnits: CardsAdder<CardID> = fun cardIDs lane ->
    let partners =
        lane.Pairs
        |> List.collect (fun (card1, card2) ->
            match List.contains card1.CardID cardIDs, List.contains card2.CardID cardIDs with
            | true, true ->
                []
            | true, false ->
                [card2]
            | false, true ->
                [card1]
            | false, false ->
                []
            )
    partners
    |> List.fold (fun ln card -> addCardToActiveUnits card ln) lane
let private removeCardsFromPairs: CardsRemover<ActiveUnit> = fun cardIDs lane ->
    let removed, newPairs =
        lane.Pairs
        |> List.partition (fun (card1, card2) -> List.contains card1.CardID cardIDs || List.contains card2.CardID cardIDs)
    List.collect (fun (c1, c2) -> [c1; c2]) removed, {lane with Pairs = newPairs}
let private addCardsToPairs: CardAdder<ActiveUnit*ActiveUnit> = fun (card1, card2) lane ->
    {lane with
        Pairs = lane.Pairs @ [card1, card2]
        }

let private incrementCardActionsUsed cardID (lane: Lane) =
    {lane with
        ActiveUnits =
            lane.ActiveUnits
            |> List.map (fun card ->
                if card.CardID = cardID then
                    {card with ActionsSpent = card.ActionsSpent + 1<action>}
                else
                    card
                )
        Pairs =
            lane.Pairs
            |> List.map (fun (card1, card2) ->
                if card1.CardID = cardID then
                    {card1 with ActionsSpent = card1.ActionsSpent + 1<action>}, card2
                elif card2.CardID = cardID then
                    card1, {card2 with ActionsSpent = card2.ActionsSpent + 1<action>}
                else
                    card1, card2
                )
        }
let private setMaxCardActions cardID left lane =
    {lane with
        ActiveUnits =
            lane.ActiveUnits
            |> List.map (fun card ->
                if card.CardID = cardID then
                    {card with MaxActions = left}
                else
                    card
                )
        Pairs =
            lane.Pairs
            |> List.map (fun (card1, card2) ->
                if card1.CardID = cardID then
                    {card1 with MaxActions = left}, card2
                elif card2.CardID = cardID then
                    card1, {card2 with MaxActions = left}
                else
                    card1, card2
                )
        }
let private damageCard cardID damage lane =
    {
        lane with
            InactiveUnits =
                lane.InactiveUnits
                |> List.map (fun card -> if card.CardID = cardID then {card with Damage = card.Damage + damage} else card)
            ActiveUnits =
                lane.ActiveUnits
                |> List.map (fun card -> if card.CardID = cardID then {card with Damage = card.Damage + damage} else card)
            Pairs =
                lane.Pairs
                |> List.map (fun (card1, card2) ->
                    (if card1.CardID = cardID then {card1 with Damage = card1.Damage + damage} else card1),
                    (if card2.CardID = cardID then {card2 with Damage = card2.Damage + damage} else card2)
                    )
        }

type private Board = {
    Lanes: Map<LaneID, Lane>
    Discard: Discard
}

let private changeLanesWithFn (fn: Lane -> Lane) (board: Board) =
    {board with Lanes = Map.map (fun _ l -> fn l) board.Lanes}
let private changeLaneWithFn laneID (fn: Lane -> Lane) (board: Board) =
    {board with
        Lanes =
            board.Lanes
            |> Map.map (fun n l ->
                if n = laneID then
                    fn l
                else
                    l
            )
        }
let private changeDiscard discard (board: Board) =
    {board with Discard = discard}
let private maxHealth card =
    match card with
    | InactiveUnit _ -> 2<health>
    | ActiveUnit {Power = power} ->
        match power with
        | PassivePower Taunt -> 3<health>
        | _ -> 2<health>
let private tryAddFromInactiveUnitList cardID (fromList: InactiveUnit list) toList : InactiveUnit list =
    match List.tryFind (fun (card: InactiveUnit) -> card.CardID = cardID) fromList with
    | None -> toList
    | Some c -> toList @ [c]
let private tryAddFromActiveUnitList cardID (fromList: ActiveUnit list) toList : ActiveUnit list =
    match List.tryFind (fun (card: ActiveUnit) -> card.CardID = cardID) fromList with
    | None -> toList
    | Some c -> toList @ [c]
let private removeCardsFromLane: CardsRemover<UnitCard> = fun cardIDs lane ->
    let removedInactive, b1 =
        lane
        |> removeCardsFromInactiveUnits cardIDs
    let removedActive, b2 =
        b1
        |> removeCardsFromActiveUnits cardIDs
    let b3 =
        b2
        |> addCardPairPartnersToActiveUnits cardIDs
    let removedPaired, b4 =
        b3
        |> removeCardsFromPairs cardIDs
    let removed =
        (List.map InactiveUnit removedInactive)
        @ (List.map ActiveUnit removedActive)
        @ (List.map ActiveUnit removedPaired)
    removed, b4
let private changeCardLane cardID fromLaneID toLaneID (board: Board) =
    let fromLane = Map.find fromLaneID board.Lanes
    board
    |> changeLaneWithFn toLaneID (fun toLane -> {
        toLane with
            InactiveUnits = tryAddFromInactiveUnitList cardID fromLane.InactiveUnits toLane.InactiveUnits
            ActiveUnits = tryAddFromActiveUnitList cardID fromLane.ActiveUnits toLane.ActiveUnits
        })
    |> changeLaneWithFn fromLaneID (removeCardsFromLane [cardID] >> snd)
let private findDeadCardsInLane laneID board =
    let lane = Map.find laneID board.Lanes
    (lane.InactiveUnits |> List.choose (fun card -> if card.Damage >= maxHealth (InactiveUnit card) then Some (InactiveUnit card) else None))
    @ (lane.ActiveUnits |> List.choose (fun card -> if card.Damage >= maxHealth (ActiveUnit card) then Some (ActiveUnit card) else None))
    @ (lane.Pairs |> List.collect (fun (card1, card2) ->
        match (card1.Damage >= maxHealth (ActiveUnit card1)), (card2.Damage >= maxHealth (ActiveUnit card2)) with
        | true, true -> [ActiveUnit card1; ActiveUnit card2]
        | true, false -> [ActiveUnit card1]
        | false, true -> [ActiveUnit card2]
        | false, false -> []
        ))
let private findDeadCards board =
    board.Lanes
    |> Map.toList
    |> List.collect (fun (id, _) -> findDeadCardsInLane id board)
let private moveDeadCardsToDiscard board =
    let deadCards = findDeadCards board
    let discardCards =
        board.Lanes
        |> Map.toList
        |> List.collect(fun (_, lane) ->
            (lane.InactiveUnits |> List.map InactiveUnit)
            @ (lane.ActiveUnits |> List.map ActiveUnit)
            @ (lane.Pairs |> List.collect (fun (c1, c2) -> [ActiveUnit c1; ActiveUnit c2]))
            )
        |> List.filter (fun card -> List.contains card deadCards)
        |> List.map (function
            | InactiveUnit {CardID = cid; Power = p; KnownBy = kb} ->
                FaceDownDiscardedCard {CardID = cid; Power = p; KnownBy = kb}
            | ActiveUnit {CardID = cid; Power = p} ->
                FaceUpDiscardedCard {CardID = cid; Power = p}
            )
    board
    |> changeLanesWithFn (removeCardsFromLane (deadCards |> List.map (function | InactiveUnit {CardID = cid} | ActiveUnit {CardID = cid} -> cid)) >> snd)
    |> changeDiscard (List.fold (fun discard card -> discard @ [card]) board.Discard discardCards)
let private flipAndActivateInactiveDeathPowersInLane laneID zeroHealthInactiveDeathPowerUnits (board: Board) =
    let ids =
        zeroHealthInactiveDeathPowerUnits
        |> List.map (fun card ->
            match card with
            | InactiveUnit {CardID = cid}
            | ActiveUnit {CardID = cid} -> cid
            )
    board
    |> changeLaneWithFn laneID (fun lane ->
        let removedCards, newLane =
            lane
            |> removeCardsFromInactiveUnits ids
        let newCards =
            removedCards
            |> List.map (fun (c: InactiveUnit) -> {
                CardID = c.CardID
                Power = c.Power
                Owner = c.Owner
                Damage = c.Damage
                ActionsSpent = 0<action>
                MaxActions = 1<action>
                FreezeStatus = c.FreezeStatus
                }
            )
        newLane
        |> addCardsToActiveUnits newCards
        )
let private triggerTargetInactiveDeathPowers laneID (board: Board) =
    let inactiveUnits = (Map.find laneID board.Lanes).InactiveUnits
    let zeroHealthInactiveDeathPowerUnits =
        findDeadCardsInLane laneID board
        |> List.filter (fun card ->
            let power =
                match card with
                | InactiveUnit {Power = p}
                | ActiveUnit {Power = p} -> p
            match power, List.contains card (inactiveUnits |> List.map (fun c -> InactiveUnit c)) with
            | InactiveDeathPower p, true -> true
            | _ -> false
            )
    board
    |> flipAndActivateInactiveDeathPowersInLane laneID zeroHealthInactiveDeathPowerUnits

type private EarlyGameInfo = {
    Bases: Map<LaneID, Bases>
    DrawPile: DrawPile
    HandCards: Map<PlayerID, HandCards>
}

type private LaneWins = Map<LaneID, PlayerID>

type private PostDrawGameInfo = {
    HandCards: Map<PlayerID, HandCards>
    LaneWins: LaneWins
}

type private PostHandGameInfo = {
    LaneWins: LaneWins
}

type private GameStage =
| Early of EarlyGameInfo
| DrawPileEmpty of PostDrawGameInfo
| HandsEmpty of PostHandGameInfo

type private CardsState = {
    Board: Board
    GameStage: GameStage
    Removed: RemovedCards
}

let private changeBoard cardsState newBoard =
    {cardsState with Board = newBoard}
let private addCardToBoard (card: InactiveUnit) playerID laneID (cardsState: CardsState) =
    cardsState.Board
    |> changeLaneWithFn laneID (fun lane ->
        {lane with InactiveUnits = lane.InactiveUnits @ [card]}
        )
    |> changeBoard cardsState
let private removeCardFromHand cardID playerID (cardsState: CardsState) =
    match cardsState.GameStage with
    | Early gs ->
        let hands = gs.HandCards
        let removedCard, newHand =
            match Map.tryFind playerID hands with
            | Some lst -> lst |> List.partition (fun c -> c.CardID = cardID)
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
            | Some lst -> lst |> List.partition (fun c -> c.CardID = cardID)
            | None -> failwithf "Can't remove card from non-existant player's hand"
        let newHands =
            hands
            |> Map.change playerID (fun oldHand -> Some newHand)
        List.head removedCard,
        {cardsState with
            GameStage = DrawPileEmpty {gs with HandCards = newHands}
            }
    | HandsEmpty _ ->
        failwithf "Shouldn't be here!"
let private removeHandsIfAllEmpty (cardsState: CardsState) =
    match cardsState.GameStage with
    | DrawPileEmpty {HandCards = handCards; LaneWins = laneWins} ->
        if Map.forall (fun _ cards -> List.isEmpty cards) handCards then
            {cardsState with GameStage = HandsEmpty {LaneWins = laneWins}}
        else
            cardsState
    | Early _
    | HandsEmpty _ ->
        cardsState
let private flipBasesOnLane (bases: Bases, lane: Lane) =
    let newUnits =
        bases
        |> List.map (fun baseCard -> {
            CardID = baseCard.CardID
            Power = baseCard.Power
            Owner = baseCard.Owner
            KnownBy = baseCard.KnownBy
            Damage = 0<health>
            FreezeStatus = NotFrozen
            }
            )
    {lane with InactiveUnits = lane.InactiveUnits @ newUnits}
let private joinMaps map1 map2 = // left join
    map1
    |> Map.map (fun key value -> (value, Map.find key map2))
let private flipBasesOnBoard bases ({Lanes = lanes; Discard = discard}: Board) =
    {
        Lanes =
            joinMaps bases lanes
            |> Map.map (fun _ bl -> flipBasesOnLane bl)
        Discard = discard
        }

type private PlayerReady = {
    Player: PlayerID
    NPlayers: int
    Actions: int
    FutureActionCounts: int list
}

type private TurnInProgress = {
    CurrentPlayer: PlayerID
    NPlayers: int
    ActionsLeft: int
    FutureActionCounts: int list
}

type private GameStateBetweenTurns = {
    CardsState: CardsState
    TurnState: PlayerReady
}

type private GameStateDuringMidPassivePowerChoice = {
    CardsState: CardsState
    TurnState: TurnInProgress
    ChoiceContext: MidPassivePowerChoiceContext
}

type private GameStateDuringMidActivationPowerChoice = {
    CardsState: CardsState
    TurnState: TurnInProgress
    ChoiceContext: MidActivationPowerChoiceContext
    FutureStack: ActivationPowerContext stack option
}

type private GameStateDuringStackChoice = {
    CardsState: CardsState
    TurnState: TurnInProgress
    EpochEvents: Map<EventID, ActivationPowerContext>
    FutureEpochs: ActivationPowerContext epoch list
}

type private GameStateDuringTurn = {
    CardsState: CardsState
    TurnState: TurnInProgress
}

type private GameStateWon = {
    Lanes: Map<LaneID, Lane>
    Winner: PlayerID
    LaneWins: LaneWins
}

type private GameStateTied = {
    Lanes: Map<LaneID, Lane>
    LaneWins: LaneWins
}

type private GameState =
| GameStateDuringMidActivationPowerChoice of GameStateDuringMidActivationPowerChoice
| GameStateDuringMidPassivePowerChoice of GameStateDuringMidPassivePowerChoice
| GameStateDuringStackChoice of GameStateDuringStackChoice
| GameStateBetweenTurns of GameStateBetweenTurns
| GameStateDuringTurn of GameStateDuringTurn
| GameStateWon of GameStateWon
| GameStateTied of GameStateTied

let private incrementActionsLeft (gameState: GameStateDuringTurn) =
    {gameState with
        TurnState = {
            gameState.TurnState with
                ActionsLeft = gameState.TurnState.ActionsLeft + 1
        }
    }

let private changeCardsState (gameState: GameStateDuringTurn) newCardsState =
    {gameState with CardsState = newCardsState}
let private changeMidActivationPowerCardsState (gameState: GameStateDuringMidActivationPowerChoice) newCardsState =
    {gameState with CardsState = newCardsState}
let private changeMidPassivePowerCardsState (gameState: GameStateDuringMidPassivePowerChoice) newCardsState =
    {gameState with CardsState = newCardsState}
let private addMidPowerChoiceContext context (gameState: GameStateDuringTurn) =
    {
        CardsState = gameState.CardsState
        TurnState = gameState.TurnState
        ChoiceContext = context
        FutureStack = None
    }

let private removeMidActivationPowerChoiceContext (gameState: GameStateDuringMidActivationPowerChoice) =
    {
        CardsState = gameState.CardsState
        TurnState = gameState.TurnState
    }
let private removeMidPassivePowerChoiceContext (gameState: GameStateDuringMidPassivePowerChoice) =
    {
        CardsState = gameState.CardsState
        TurnState = gameState.TurnState
    }
let private updateLaneWins (gameState: GameStateDuringTurn) =
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
        let newGameState = DrawPileEmpty {gs with LaneWins = currentLaneWins}
        let newCardsState = {gameState.CardsState with GameStage = newGameState}
        newCardsState
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
        let newGameState = HandsEmpty {gs with LaneWins = currentLaneWins}
        let newCardsState = {gameState.CardsState with GameStage = newGameState}
        newCardsState
        |> changeCardsState gameState
let private checkForGameEnd gameState =
    match gameState with
    | GameStateDuringTurn {CardsState = cs} ->
        match cs.GameStage with
        | Early _
        | DrawPileEmpty _ ->
            gameState
        | HandsEmpty {LaneWins = laneWins} ->
            let lanes = cs.Board.Lanes
            let wonLaneCounts =
                laneWins
                |> Map.toList
                |> List.countBy (fun (laneID, playerID) -> playerID)
            match wonLaneCounts with
            | [] -> gameState
            | lst ->
                let (leadingPlayer, leadingWins) =
                    lst
                    |> List.maxBy (fun (_, n) -> n)
                if leadingWins >= 2 then
                    GameStateWon {Winner = leadingPlayer; Lanes = lanes; LaneWins = laneWins}
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
                        GameStateTied {Lanes = lanes; LaneWins = laneWins}
                    else
                        gameState
    | GameStateDuringMidActivationPowerChoice _
    | GameStateDuringMidPassivePowerChoice _
    | GameStateDuringStackChoice _
    | GameStateBetweenTurns _
    | GameStateWon _
    | GameStateTied _ ->
        gameState

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

let private createUnshuffledPowerDeck () =
    [
        ActivationPower View
        InactiveDeathPower Trap
        ActivationPower Foresight
        ActivationPower Flip
        ActivationPower Freeze
        ActivationPower Heal
        PassivePower Retaliate
        PassivePower Nimble
        PassivePower TwinStrike
        PassivePower Taunt
        PassivePower Vampiric
        ActivationPower Move
        ActivationPower Empower
        ActivationPower Action
    ]
    |> List.filter (fun power -> power <> PassivePower Vampiric)
    |> List.collect (List.replicate 4)

let private prepareHead fn n lst =
    let h, t = List.splitAt n lst
    fn h, t

let private prepareBases nLanes lst =
    lst
    |> List.splitInto nLanes
    |> createIDMap 1<LID>

let private prepareHands nPlayers (lst: CardID list) =
    let playerIDs =
        createIDsToLength 1<PID> nPlayers
        |> List.collect (List.replicate 5)
    List.zip lst playerIDs
    |> List.groupBy (fun (_, playerID) -> playerID)
    |> List.map (fun (playerID, lst) -> playerID, List.map (fun (cardID, _) -> cardID) lst)
    |> Map.ofList

let private prepareRemoved lst =
    lst
    |> Set.ofList

let private getBaseKnowledge (playerID: PlayerID) (baseCard: PlayerID * Power * PlayerID Set) =
    let (ownerID, power, knownBy) = baseCard
    if Set.contains playerID knownBy then
        KnownBaseCard (ownerID, power)
    else
        UnknownBaseCard ownerID

let private getTroops viewerID ownerID (lane: Lane) : InactiveUnitKnowledge list * ActiveUnitKnowledge list * PairKnowledge list =
    let pairPowers =
        lane.Pairs
        |> List.map (fun (x, _) -> x.Power)
    let pairDamages1, pairDamages2 =
        lane.Pairs
        |> List.map (fun (x, y) -> x.Damage, y.Damage)
        |> List.unzip
    let pairActionsLeft =
        lane.Pairs
        |> List.map (fun (x, y) -> min (x.MaxActions - x.ActionsSpent) (y.MaxActions - y.ActionsSpent))
    let pairActionabilities =
        lane.Pairs
        |> List.map (fun (x, y) ->
            (if x.FreezeStatus = NotFrozen then Normal else Frozen),
            (if y.FreezeStatus = NotFrozen then Normal else Frozen)
            )
    let pairOwners =
        lane.Pairs
        |> List.map (fun (x, _) -> x.Owner)
    let pairKnowledge =
        pairActionsLeft
        |> List.zip3 pairDamages1 pairDamages2
        |> List.zip3 pairPowers pairActionabilities
        |> List.zip lane.Pairs
        |> List.map (fun ((card1, card2), (p, (a1, a2: Actionability), (d1, d2, al))) ->
            (card1.CardID, card2.CardID, p, d1, d2, al, a1, a2): PairKnowledge
            )
        |> List.zip pairOwners
        |> List.filter (fun (owner, _) -> owner = ownerID)
    let nonPairedActiveUnitKnowledge =
        lane.ActiveUnits
        |> List.filter (fun card -> card.Owner = ownerID)
        |> List.map (fun card ->
            let actionability = if card.FreezeStatus = NotFrozen then Normal else Frozen
            card.Owner, ((card.CardID, card.Power, card.Damage, card.MaxActions - card.ActionsSpent, actionability): ActiveUnitKnowledge)
            )
    let inactiveUnitKnowledge =
        lane.InactiveUnits
        |> List.filter (fun card -> card.Owner = ownerID)
        |> List.map (fun card ->
            let actionability = if card.FreezeStatus = NotFrozen then Normal else Frozen
            if Set.contains viewerID card.KnownBy then
                card.Owner, KnownInactiveCardKnowledge (card.CardID, card.Power, card.Damage, actionability)
            else
                card.Owner, UnknownInactiveCardKnowledge (card.CardID, card.Damage, actionability)
            )
    inactiveUnitKnowledge
    |> List.map (fun (_, knowledge) -> knowledge),
    nonPairedActiveUnitKnowledge
    |> List.map (fun (_, knowledge) -> knowledge),
    pairKnowledge
    |> List.map (fun (_, knowledge) -> knowledge)

let private getDeadCardKnowledge (playerID: PlayerID) (card: DiscardedCard) =
    match card with
    | FaceDownDiscardedCard {Power = p; KnownBy = kb} ->
        if Set.contains playerID kb then
            KnownFaceDownDeadCard p
            |> KnownDeadCard
        else
            UnknownDeadCard
    | FaceUpDiscardedCard {Power = p} ->
        KnownFaceUpDeadCard p
        |> KnownDeadCard

let private getPlayerLaneWins (laneWins: LaneWins) =
    laneWins
    |> Map.toList
    |> List.groupBy (fun (_, pid) -> pid)
    |> List.map (fun (key, pairs) ->
        key,
        pairs
        |> List.map (fun (lid, _) -> lid)
        )

let private getDisplayInfo gameState =
    match gameState with
    | GameStateDuringMidPassivePowerChoice gs ->
        let id = gs.TurnState.CurrentPlayer
        let (playerHandInfo, opponentHandsInfo) =
            match gs.CardsState.GameStage with
            | Early {HandCards = hco} ->
                hco
                |> Map.partition (fun owner _ -> owner = id)
            | DrawPileEmpty {HandCards= hco} ->
                hco
                |> Map.partition (fun owner _ -> owner = id)
            | HandsEmpty _ ->
                Map.empty, Map.empty
        let playerHand =
            playerHandInfo
            |> Map.toList
            |> List.collect (fun (_, cards) -> cards)
        let getBase = getBaseKnowledge id
        let boardKnowledge =
            let ({Lanes = l; Discard = d}: Board) = gs.CardsState.Board
            let getDeadCard = getDeadCardKnowledge id
            match gs.CardsState.GameStage with
            | Early {Bases = b; DrawPile = dp} ->
                let lanesKnowledge =
                    joinMaps b l
                    |> Map.map (fun _ (bases, lane) ->
                        let troopKnowledge =
                            createIDsToLength 1<PID> gs.TurnState.NPlayers
                            |> List.map (fun playerID ->
                                playerID, getTroops id playerID lane
                                )
                            |> Map.ofList
                        {
                            Bases =
                                bases
                                |> List.map (fun {Owner = owner; KnownBy = kb; Power = power} ->
                                    owner, power, kb
                                    )
                                |> List.map getBase
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
                            createIDsToLength 1<PID> gs.TurnState.NPlayers
                            |> List.map (fun playerID ->
                                playerID, getTroops id playerID lane
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
                            createIDsToLength 1<PID> gs.TurnState.NPlayers
                            |> List.map (fun playerID ->
                                playerID, getTroops id playerID lane
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
        MidPassivePowerChoiceDisplayInfo {
            CurrentPlayer = id
            ActionsLeft = gs.TurnState.ActionsLeft
            BoardKnowledge = boardKnowledge
            PlayerHand =
                playerHand
                |> List.map (fun card ->
                    HandCardInfo (card.CardID, card.Power)
                    )
            OpponentHandSizes =
                opponentHandsInfo
                |> Map.map (fun _ cards -> List.length cards)
                |> Map.toList
            ChoiceContext = gs.ChoiceContext
        }
    | GameStateDuringMidActivationPowerChoice gs ->
        let id = gs.TurnState.CurrentPlayer
        let (playerHandInfo, opponentHandsInfo) =
            match gs.CardsState.GameStage with
            | Early {HandCards = hco} ->
                hco
                |> Map.partition (fun owner _ -> owner = id)
            | DrawPileEmpty {HandCards= hco} ->
                hco
                |> Map.partition (fun owner _ -> owner = id)
            | HandsEmpty _ ->
                Map.empty, Map.empty
        let playerHand =
            playerHandInfo
            |> Map.toList
            |> List.collect (fun (_, cards) -> cards)
        let getBase = getBaseKnowledge id
        let boardKnowledge =
            let ({Lanes = l; Discard = d}: Board) = gs.CardsState.Board
            let getDeadCard = getDeadCardKnowledge id
            match gs.CardsState.GameStage with
            | Early {Bases = b; DrawPile = dp} ->
                let lanesKnowledge =
                    joinMaps b l
                    |> Map.map (fun _ (bases, lane) ->
                        let troopKnowledge =
                            createIDsToLength 1<PID> gs.TurnState.NPlayers
                            |> List.map (fun playerID ->
                                playerID, getTroops id playerID lane
                                )
                            |> Map.ofList
                        {
                            Bases =
                                bases
                                |> List.map (fun {Owner = owner; KnownBy = kb; Power = power} ->
                                    owner,
                                    power,
                                    kb
                                    )
                                |> List.map getBase
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
                            createIDsToLength 1<PID> gs.TurnState.NPlayers
                            |> List.map (fun playerID ->
                                playerID, getTroops id playerID lane
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
                            createIDsToLength 1<PID> gs.TurnState.NPlayers
                            |> List.map (fun playerID ->
                                playerID, getTroops id playerID lane
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
        MidActivationPowerChoiceDisplayInfo {
            CurrentPlayer = id
            ActionsLeft = gs.TurnState.ActionsLeft
            BoardKnowledge = boardKnowledge
            PlayerHand =
                playerHand
                |> List.map (fun card ->
                    HandCardInfo (card.CardID, card.Power)
                    )
            OpponentHandSizes =
                opponentHandsInfo
                |> Map.map (fun _ cards -> List.length cards)
                |> Map.toList
            ChoiceContext = gs.ChoiceContext
        }
    | GameStateDuringStackChoice gs ->
        let id = gs.TurnState.CurrentPlayer
        let (playerHandInfo, opponentHandsInfo) =
            match gs.CardsState.GameStage with
            | Early {HandCards = hco} ->
                hco
                |> Map.partition (fun owner _ -> owner = id)
            | DrawPileEmpty {HandCards= hco} ->
                hco
                |> Map.partition (fun owner _ -> owner = id)
            | HandsEmpty _ ->
                Map.empty, Map.empty
        let playerHand =
            playerHandInfo
            |> Map.toList
            |> List.collect (fun (_, cards) -> cards)
        let getBase = getBaseKnowledge id
        let boardKnowledge =
            let ({Lanes = l; Discard = d}: Board) = gs.CardsState.Board
            let getDeadCard = getDeadCardKnowledge id
            match gs.CardsState.GameStage with
            | Early {Bases = b; DrawPile = dp} ->
                let lanesKnowledge =
                    joinMaps b l
                    |> Map.map (fun _ (bases, lane) ->
                        let troopKnowledge =
                            createIDsToLength 1<PID> gs.TurnState.NPlayers
                            |> List.map (fun playerID ->
                                playerID, getTroops id playerID lane
                                )
                            |> Map.ofList
                        {
                            Bases =
                                bases
                                |> List.map (fun {Owner = owner; Power = power; KnownBy = kb} ->
                                    owner, power, kb
                                    )
                                |> List.map getBase
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
                            createIDsToLength 1<PID> gs.TurnState.NPlayers
                            |> List.map (fun playerID ->
                                playerID, getTroops id playerID lane
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
                            createIDsToLength 1<PID> gs.TurnState.NPlayers
                            |> List.map (fun playerID ->
                                playerID, getTroops id playerID lane
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
        StackChoiceDisplayInfo {
            CurrentPlayer = id
            ActionsLeft = gs.TurnState.ActionsLeft
            BoardKnowledge = boardKnowledge
            PlayerHand =
                playerHand
                |> List.map (fun card ->
                    HandCardInfo (card.CardID, card.Power)
                    )
            OpponentHandSizes =
                opponentHandsInfo
                |> Map.map (fun _ cards -> List.length cards)
                |> Map.toList
            Stack = {
                Head = Map.toList gs.EpochEvents |> List.map snd |> NonEmptyList.fromList
                Tail = gs.FutureEpochs
            }
        }
    | GameStateDuringTurn gs ->
        let id = gs.TurnState.CurrentPlayer
        let (playerHandInfo, opponentHandsInfo) =
            match gs.CardsState.GameStage with
            | Early {HandCards = hco} ->
                hco
                |> Map.partition (fun owner _ -> owner = id)
            | DrawPileEmpty {HandCards= hco} ->
                hco
                |> Map.partition (fun owner _ -> owner = id)
            | HandsEmpty _ ->
                Map.empty, Map.empty
        let playerHand =
            playerHandInfo
            |> Map.toList
            |> List.collect (fun (_, cards) -> cards)
        let getBase = getBaseKnowledge id
        let boardKnowledge =
            let ({Lanes = l; Discard = d}: Board) = gs.CardsState.Board
            let getDeadCard = getDeadCardKnowledge id
            match gs.CardsState.GameStage with
            | Early {Bases = b; DrawPile = dp} ->
                let lanesKnowledge =
                    joinMaps b l
                    |> Map.map (fun _ (bases, lane) ->
                        let troopKnowledge =
                            createIDsToLength 1<PID> gs.TurnState.NPlayers
                            |> List.map (fun playerID ->
                                playerID, getTroops id playerID lane
                                )
                            |> Map.ofList
                        {
                            Bases =
                                bases
                                |> List.map (fun {Owner = owner; Power = power; KnownBy = kb} ->
                                    owner, power, kb
                                    )
                                |> List.map getBase
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
                    |> Map.map (fun laneID lane  ->
                        let troopKnowledge =
                            createIDsToLength 1<PID> gs.TurnState.NPlayers
                            |> List.map (fun playerID ->
                                playerID, getTroops id playerID lane
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
                            createIDsToLength 1<PID> gs.TurnState.NPlayers
                            |> List.map (fun playerID ->
                                playerID, getTroops id playerID lane
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
        TurnDisplayInfo {
            CurrentPlayer = id
            ActionsLeft = gs.TurnState.ActionsLeft
            BoardKnowledge = boardKnowledge
            PlayerHand =
                playerHand
                |> List.map (fun card ->
                    HandCardInfo (card.CardID, card.Power)
                    )
            OpponentHandSizes =
                opponentHandsInfo
                |> Map.map (fun _ cards -> List.length cards)
                |> Map.toList
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

let private getPlayActionsInfo (gameState: GameStateDuringTurn) =
    let playerID = gameState.TurnState.CurrentPlayer
    match gameState.CardsState.GameStage with
    | Early {HandCards = hc}
    | DrawPileEmpty {HandCards = hc} ->
        Map.find playerID hc
    | HandsEmpty _ ->
        List.empty
    |> List.allPairs (createIDsToLength 1<LID> (Map.count gameState.CardsState.Board.Lanes))
    |> List.map (fun (laneID, card) ->
        Play (playerID, card.CardID, laneID)
        |> TurnActionInfo
        )

let private getActivateActionsInfo (gameState: GameStateDuringTurn) =
    let playerID = gameState.TurnState.CurrentPlayer
    let lanes = gameState.CardsState.Board.Lanes
    lanes
    |> Map.toList
    |> List.collect (fun (laneID, lane) ->
        let ownTroops =
            lane.InactiveUnits
            |> List.filter (fun card -> card.Owner = playerID)
        ownTroops
        |> List.filter (fun card -> card.FreezeStatus = NotFrozen)
        |> List.map (fun card -> Activate (playerID, laneID, card.CardID) |> TurnActionInfo)
        )

let private getPairActionsInfoFromUnits playerID laneID (ownActiveUnits: ActiveUnit list) =
    let rec distPairs lst =
        match lst with
        | [] -> []
        | [_] -> []
        | h::t -> List.allPairs [h] t @ distPairs t
    ownActiveUnits
    |> distPairs
    |> List.choose (fun (card1, card2) ->
        if card1.Power = card2.Power then
            CreatePair (playerID, laneID, card1.CardID, card2.CardID)
            |> TurnActionInfo
            |> Some
        else
            None
        )

let private getAttackActionsInfo (gameState: GameStateDuringTurn) =
    let playerID = gameState.TurnState.CurrentPlayer
    let lanes = gameState.CardsState.Board.Lanes
    lanes
    |> Map.toList
    |> List.collect (fun (laneID, lane) ->
        let pairToList (x, y) = [x; y]
        let findOwner (toList: 'a -> UnitCard list) x =
            let first = toList >> List.head
            match (first x) with
            | InactiveUnit {Owner = owner} -> owner
            | ActiveUnit {Owner = owner} -> owner

        let possibleAttackers toList lst =
            lst
            |> List.filter (fun attacker ->
                attacker
                |> toList
                |> List.forall (fun (attackerCard: ActiveUnit) ->
                    attackerCard.Owner = playerID
                    && match attackerCard with
                        | Ready ->
                            attackerCard.FreezeStatus = NotFrozen
                        | Exhausted ->
                            false
                        )
                )
        let possibleUnitAttackers = possibleAttackers List.singleton lane.ActiveUnits
        let possiblePairAttackers = possibleAttackers pairToList lane.Pairs

        let addTypeAndOwner T owner (x: UnitCard) =
            let id =
                match x with
                | InactiveUnit {CardID = cid}
                | ActiveUnit {CardID = cid} -> cid
            T (owner, id)
        let possibleTypeTargets toList T ids =
            let transform owner = toList >> List.map (addTypeAndOwner T owner)
            ids
            |> List.groupBy (findOwner toList)
            |> List.filter (fun (owner, _) -> owner <> playerID)
            |> List.collect (fun (owner, units) ->
                units
                |> List.collect (transform owner)
                )
        let possibleSingleTypeTargets T cardIDs = possibleTypeTargets List.singleton T cardIDs
        let possiblePairTypeTargets T pairIDs = possibleTypeTargets pairToList T pairIDs
        let possibleInactiveUnitTargets = possibleSingleTypeTargets InactiveTarget (lane.InactiveUnits |> List.map InactiveUnit)
        let possibleActiveUnitTargets = possibleSingleTypeTargets ActiveSingleTarget (lane.ActiveUnits |> List.map ActiveUnit)
        let possiblePairTargets = possiblePairTypeTargets ActivePairMemberTarget (lane.Pairs |> List.map (fun (c1, c2) -> ActiveUnit c1, ActiveUnit c2))
        let allTargets = possibleInactiveUnitTargets @ possibleActiveUnitTargets @ possiblePairTargets

        let tauntTargets =
            allTargets
            |> List.filter (fun target ->
                match target with
                | InactiveTarget _ ->
                    false
                | ActiveSingleTarget (_, cardID)
                | ActivePairMemberTarget (_, cardID) ->
                    let card = List.find (fun (card: ActiveUnit) -> card.CardID = cardID) lane.ActiveUnits
                    card.Power = PassivePower Taunt
            )

        let singleAttacks =
            possibleUnitAttackers
            |> List.collect (fun attacker ->
                let power = attacker.Power
                if power = PassivePower Nimble || List.isEmpty tauntTargets then
                    allTargets
                    |> List.map (fun target ->
                        SingleAttack (playerID, laneID, attacker.CardID, target)
                        |> TurnActionInfo
                    )
                else
                    tauntTargets
                    |> List.map (fun target ->
                        SingleAttack (playerID, laneID, attacker.CardID, target)
                        |> TurnActionInfo
                    )
            )
        let pairAttacks =
            possiblePairAttackers
            |> List.collect (fun (attacker1, attacker2) ->
                let power = attacker1.Power
                if power = PassivePower Nimble || List.isEmpty tauntTargets then
                    allTargets
                    |> List.map (fun target ->
                        PairAttack (playerID, laneID, (attacker1.CardID, attacker2.CardID), target)
                        |> TurnActionInfo
                    )
                else
                    tauntTargets
                    |> List.map (fun target ->
                        PairAttack (playerID, laneID, (attacker1.CardID, attacker2.CardID), target)
                        |> TurnActionInfo
                    )
            )
        singleAttacks @ pairAttacks
        )

let private getPairActionsInfo (gameState: GameStateDuringTurn) =
    let playerID = gameState.TurnState.CurrentPlayer
    let lanes = gameState.CardsState.Board.Lanes
    lanes
    |> Map.toList
    |> List.collect (fun (laneID, {ActiveUnits = activeUnits}) ->
        activeUnits
        |> List.filter (fun card -> card.Owner = playerID)
        |> getPairActionsInfoFromUnits playerID laneID
        )

let private getPossibleActionsInfo (gameState: GameState) =
    match gameState with
    | GameStateBetweenTurns gs ->
        StartTurn gs.TurnState.Player
        |> List.singleton
    | GameStateDuringTurn gs ->
        if gs.TurnState.ActionsLeft = 0 then
            EndTurn gs.TurnState.CurrentPlayer
            |> List.singleton
        else
            let currentPlayer = gs.TurnState.CurrentPlayer
            if gs.TurnState.ActionsLeft = 0 then
                EndTurn currentPlayer
                |> List.singleton
            else
                let actions =
                    getPlayActionsInfo gs
                    @ getActivateActionsInfo gs
                    @ getAttackActionsInfo gs
                    @ getPairActionsInfo gs
                if List.isEmpty actions then
                    EndTurn currentPlayer
                    |> List.singleton
                else
                    actions
    | GameStateDuringStackChoice gs ->
        gs.EpochEvents
        |> Map.toList
        |> List.map (fun (eventID, event) ->
            StackChoiceInfo (gs.TurnState.CurrentPlayer, eventID, event)
        )
    | GameStateDuringMidActivationPowerChoice gs ->
        match gs.ChoiceContext with
        | DiscardChoiceContext (playerID, powerCardID) ->
            match gs.CardsState.GameStage with
            | Early {HandCards = hc}
            | DrawPileEmpty {HandCards = hc} ->
                hc
                |> Map.find playerID
                |> List.map (fun card -> DiscardChoice (playerID, powerCardID, card.CardID) |> MidActivationPowerChoiceInfo)
            | HandsEmpty _ ->
                failwithf "Can't discard from an empty hand"
        | ForesightChoiceContext (playerID, powerCardID) ->
            let inactiveUnits =
                gs.CardsState.Board.Lanes
                |> Map.toList
                |> List.collect (fun (laneID, lane) -> lane.InactiveUnits)
            let faceDownCards =
                match gs.CardsState.GameStage with
                | Early {Bases = bases} ->
                    let baseIDs =
                        bases
                        |> Map.toList
                        |> List.collect (fun (laneID, b) ->
                            b
                            |> List.map (fun card -> {
                                CardID = card.CardID
                                Power = card.Power
                                Owner = card.Owner
                                KnownBy = card.KnownBy
                                Damage = 0<health>
                                FreezeStatus = NotFrozen
                                }
                                )
                            )
                    baseIDs @ inactiveUnits
                | DrawPileEmpty _
                | HandsEmpty _ ->
                    inactiveUnits
            faceDownCards
            |> List.filter (fun card -> not (Set.contains playerID card.KnownBy))
            |> List.map (fun card -> ForesightChoice (playerID, powerCardID, card.CardID) |> MidActivationPowerChoiceInfo)
        | MoveChoiceContext (playerID, laneID, powerCardID) ->
            let moves =
                gs.CardsState.Board.Lanes
                    |> Map.filter (fun targetLaneID _ -> targetLaneID <> laneID)
                |> Map.toList
                |> List.collect (fun (targetLaneID, lane) ->
                    (lane.InactiveUnits |> List.choose (fun card -> if card.Owner = playerID then Some card.CardID else None))
                    @ (lane.ActiveUnits |> List.choose (fun card -> if card.Owner = playerID then Some card.CardID else None))
                    @ (lane.Pairs |> List.collect (fun (c1, c2) -> if c1.Owner = playerID then [c1.CardID; c2.CardID] else []))
                    |> List.map (fun id -> targetLaneID, id)
                )
                |> List.map (fun (targetLaneID, targetCardID) ->
                    MoveChoice (Some (playerID, laneID, powerCardID, targetLaneID, targetCardID))
                    |> MidActivationPowerChoiceInfo
                )
            if List.isEmpty moves then
                moves
            else
                MidActivationPowerChoiceInfo (MoveChoice (None)) :: moves
    | GameStateDuringMidPassivePowerChoice gs ->
        match gs.ChoiceContext with
        | TwinStrikeChoiceContext (playerID, laneID, powerCardID, originalTargetCardID) ->
            let lane = Map.find laneID gs.CardsState.Board.Lanes
            let originalTargetCard =
                match List.tryFind (fun (card: InactiveUnit) -> card.CardID = originalTargetCardID) lane.InactiveUnits with
                | Some card -> InactiveUnit card
                | None ->
                    match List.tryFind (fun (card: ActiveUnit) -> card.CardID = originalTargetCardID) lane.ActiveUnits with
                    | Some card -> ActiveUnit card
                    | None ->
                        lane.Pairs
                        |> List.collect (fun (c1, c2) -> [c1; c2])
                        |> List.find (fun (card: ActiveUnit) -> card.CardID = originalTargetCardID)
                        |> ActiveUnit
            let activeTauntCheckTargets, nonActiveTauntCheckTargets =
                let inactiveTargets =
                    lane.InactiveUnits
                    |> List.filter (fun card -> card.Owner <> playerID && card.CardID <> originalTargetCardID)
                let activeTauntTargets, activeNonTauntTargets =
                    lane.ActiveUnits
                    |> List.filter (fun card -> card.Owner <> playerID && card.CardID <> originalTargetCardID && card.Power <> PassivePower Nimble)
                    |> List.partition (fun card -> card.Power = PassivePower Taunt)
                List.map (fun (card: ActiveUnit) -> card.CardID) activeTauntTargets,
                ((List.map (fun (card: InactiveUnit) -> card.CardID) inactiveTargets) @ (List.map (fun (card: ActiveUnit) -> card.CardID) activeNonTauntTargets))
            let originalTargetIsActiveTaunt =
                match originalTargetCard with
                | InactiveUnit _ -> false
                | ActiveUnit {Power = p} -> p = PassivePower Taunt
            let legalTargets =
                if List.isEmpty activeTauntCheckTargets && not (originalTargetIsActiveTaunt)
                then
                    nonActiveTauntCheckTargets
                else
                    activeTauntCheckTargets
            legalTargets
            |> List.map (fun cardID ->
                TwinStrikeChoice (playerID, laneID, powerCardID, cardID)
                |> MidPassivePowerChoiceInfo
            )
    | GameStateWon _
    | GameStateTied _ ->
        List.empty

let private executeMidActivationPowerChoice midPowerChoice (gameState: GameStateDuringMidActivationPowerChoice) =
    match midPowerChoice with
    | DiscardChoice (playerID, _, discardeeCardID) ->
        let cs = gameState.CardsState
        match cs.GameStage with
        | Early gs ->
            let currentHand = Map.find playerID gs.HandCards
            let discardeeCard = List.find (fun (card: HandCard) -> card.CardID = discardeeCardID) currentHand
            let convertedCard = FaceDownDiscardedCard {
                CardID = discardeeCardID
                Power = discardeeCard.Power
                KnownBy = Set.singleton playerID
            }
            let newStage = Early {
                gs with
                    HandCards =
                        gs.HandCards
                        |> Map.add playerID (currentHand |> List.filter (fun card -> card.CardID <> discardeeCardID))
                }
            let newCardsState = {
                cs with
                    GameStage = newStage
                    Board = {cs.Board with Discard = cs.Board.Discard @ [convertedCard]}
                }
            changeMidActivationPowerCardsState gameState newCardsState
        | DrawPileEmpty gs ->
            let currentHand = Map.find playerID gs.HandCards
            let discardeeCard = List.find (fun (card: HandCard) -> card.CardID = discardeeCardID) currentHand
            let convertedCard = FaceDownDiscardedCard {
                CardID = discardeeCardID
                Power = discardeeCard.Power
                KnownBy = Set.singleton playerID
            }
            let newStage = DrawPileEmpty {
                gs with
                    HandCards =
                        gs.HandCards
                        |> Map.add playerID (currentHand |> List.filter (fun card -> card.CardID <> discardeeCardID))
                }
            let newCardsState = {
                cs with
                    GameStage = newStage
                    Board = {cs.Board with Discard = cs.Board.Discard @ [convertedCard]}
                }
            changeMidActivationPowerCardsState gameState newCardsState
        | HandsEmpty _ ->
            failwithf "Can't discard from an empty hand"
    | ForesightChoice (playerID, powerCardID, targetCardID) ->
        gameState.CardsState.Board
        |> changeBoard gameState.CardsState
        |> changeMidActivationPowerCardsState gameState
    | MoveChoice maybeMove ->
        match maybeMove with
        | Some (playerID, laneID, powerCardID, targetLaneID, targetCardID) ->
            gameState.CardsState.Board
            |> changeCardLane targetCardID targetLaneID laneID
            |> changeBoard gameState.CardsState
            |> changeMidActivationPowerCardsState gameState
        | None ->
            gameState
    |> removeMidActivationPowerChoiceContext

let private executeMidPassivePowerChoice midPowerChoice (gameState: GameStateDuringMidPassivePowerChoice) =
    match midPowerChoice with
    | TwinStrikeChoice (playerID, laneID, powerCardID, targetCardID) ->
        gameState.CardsState.Board
        |> changeLaneWithFn laneID (damageCard targetCardID 1<health>)
        |> moveDeadCardsToDiscard
        |> changeBoard gameState.CardsState
        |> changeMidPassivePowerCardsState gameState
    |> removeMidPassivePowerChoiceContext

let private executePlayAction cardID laneID (gameState: GameStateDuringTurn) =
    let playerID = gameState.TurnState.CurrentPlayer
    let cardsState = gameState.CardsState
    let playedCard, newCardsState = removeCardFromHand cardID playerID cardsState
    let newCard = {
        CardID = playedCard.CardID
        Power =  playedCard.Power
        Owner = playerID
        KnownBy = Set.singleton playerID
        Damage = 0<health>
        FreezeStatus = NotFrozen
    }
    newCardsState
    |> addCardToBoard newCard playerID laneID
    |> removeHandsIfAllEmpty
    |> changeCardsState gameState

let private tryDrawCard playerID (gameState: GameStateDuringTurn) =
    let cardsState = gameState.CardsState
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
                        Some (hc @ [drawPile.Head])
                    | None ->
                        failwithf "non-existent players can't draw cards"
                    )
            let newCards = {
                cardsState with
                    Board = flipBasesOnBoard preInfo.Bases boardInfo
                    GameStage = DrawPileEmpty {HandCards = newHandCards; LaneWins = Map.empty}
                }
            newCards
            |> changeCardsState gameState
        | newTopCard :: newRest ->
            let newHandCards =
                hands
                |> Map.change playerID (function
                    | Some hc ->
                        Some (hc @ [drawPile.Head])
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
            newCards
            |> changeCardsState gameState
    | DrawPileEmpty _
    | HandsEmpty _ ->
        gameState

let private healOwnUnitsInLane playerID amount (lane: Lane) =
    {lane with
        InactiveUnits =
            lane.InactiveUnits
            |> List.map (fun card ->
                if card.Owner = playerID then
                    {card with Damage = max 0<health> (card.Damage - 2<health>)}
                else
                    card
                )
        ActiveUnits =
            lane.ActiveUnits
            |> List.map (fun card ->
                if card.Owner = playerID then
                    {card with Damage = max 0<health> (card.Damage - 2<health>)}
                else
                    card
                )
        Pairs =
            lane.Pairs
            |> List.map (fun (card1, card2) ->
                if card1.Owner = playerID then
                    {card1 with Damage = max 0<health> (card1.Damage - 2<health>)},
                    {card2 with Damage = max 0<health> (card2.Damage - 2<health>)}
                else
                    card1, card2
                )
        }

let private freezeEnemyNonActiveNimbleUnitsInLane playerID lane =
    let newInactiveUnits =
        lane.InactiveUnits
        |> List.map (fun card ->
            if card.Owner <> playerID then
                {card with FreezeStatus = FrozenBy playerID}
            else
                card
            )
    let newActiveUnits =
        lane.ActiveUnits
        |> List.map (fun card ->
            if card.Owner <> playerID && card.Power <> PassivePower Nimble then
                {card with FreezeStatus = FrozenBy playerID}
            else
                card
            )
    let newPairs =
        lane.Pairs
        |> List.map (fun (c1, c2) ->
            if c1.Owner <> playerID && c1.Power <> PassivePower Nimble then
                {c1 with FreezeStatus = FrozenBy playerID}, {c2 with FreezeStatus = FrozenBy playerID}
            else
                c1, c2
            )
    {lane with InactiveUnits = newInactiveUnits; ActiveUnits = newActiveUnits; Pairs = newPairs}

let private resolveActivationPower playerID laneID cardID (gameState: GameStateDuringTurn) =
    let cardsState = gameState.CardsState
    let {Board = board} = cardsState
    let lane = Map.find laneID board.Lanes
    let card = List.find (fun (card: ActiveUnit) -> card.CardID = cardID) lane.ActiveUnits
    match card.Power with
    | ActivationPower View ->
        gameState
        |> tryDrawCard playerID
        |> addMidPowerChoiceContext (DiscardChoiceContext (playerID, cardID))
        |> GameStateDuringMidActivationPowerChoice
    | ActivationPower Foresight ->
        gameState
        |> addMidPowerChoiceContext (ForesightChoiceContext (playerID, cardID))
        |> GameStateDuringMidActivationPowerChoice
    | ActivationPower Flip ->
        gameState
        |> GameStateDuringTurn
    | ActivationPower Freeze ->
        board
        |> changeLaneWithFn laneID (freezeEnemyNonActiveNimbleUnitsInLane playerID)
        |> changeBoard cardsState
        |> changeCardsState gameState
        |> GameStateDuringTurn
    | ActivationPower Heal ->
        board
        |> changeLanesWithFn (healOwnUnitsInLane playerID 2<health>)
        |> changeBoard cardsState
        |> changeCardsState gameState
        |> GameStateDuringTurn
    | ActivationPower Move ->
        gameState
        |> addMidPowerChoiceContext (MoveChoiceContext (playerID, laneID, cardID))
        |> GameStateDuringMidActivationPowerChoice
    | ActivationPower Empower ->
        gameState
        |> GameStateDuringTurn
    | ActivationPower Action ->
        gameState.CardsState.Board
        |> changeLaneWithFn laneID (setMaxCardActions cardID 2<action>)
        |> changeBoard gameState.CardsState
        |> changeCardsState gameState
        |> incrementActionsLeft
        |> GameStateDuringTurn
    | InactiveDeathPower _
    | PassivePower _ ->
        gameState
        |> GameStateDuringTurn

let private resolveAttackerPassivePower playerID laneID unitCardIDs attackedCardID (gameState: GameStateDuringTurn) =
    let cardsState = gameState.CardsState
    let lane = Map.find laneID cardsState.Board.Lanes
    let cardID =
        match unitCardIDs with
        | SingleCardID id -> id
        | PairIDs (id, _) -> id
    let card =
        match unitCardIDs with
        | SingleCardID id -> List.find (fun (card: ActiveUnit) -> card.CardID = id) lane.ActiveUnits
        | PairIDs (id, _) -> List.find (fun (card: ActiveUnit) -> card.CardID = id) (lane.Pairs |> List.collect (fun (c1, c2) -> [c1; c2]))
    match card.Power with
    | PassivePower Retaliate
    | PassivePower Nimble
    | PassivePower Taunt ->
       gameState
       |> GameStateDuringTurn
    | PassivePower TwinStrike ->
        let context = TwinStrikeChoiceContext (playerID, laneID, unitCardIDs, attackedCardID)
        {
            CardsState = cardsState
            TurnState = gameState.TurnState
            ChoiceContext = context
        }
        |> GameStateDuringMidPassivePowerChoice
    | PassivePower Vampiric ->
        gameState
        |> GameStateDuringTurn
    | ActivationPower _
    | InactiveDeathPower _ ->
        gameState
        |> GameStateDuringTurn

let private executeActivateAction playerID laneID cardID (gameState: GameStateDuringTurn) =
    let cardsState = gameState.CardsState
    cardsState.Board
    |> changeLaneWithFn laneID (fun lane ->
        let removedCard, newLane = removeCardFromInactiveUnits cardID lane
        let newCard = {
            CardID = removedCard.CardID
            Power = removedCard.Power
            Owner = removedCard.Owner
            Damage = removedCard.Damage
            ActionsSpent = 0<action>
            MaxActions = 1<action>
            FreezeStatus = removedCard.FreezeStatus
        }
        newLane
        |> addCardToActiveUnits newCard
        )
    |> changeBoard cardsState
    |> changeCardsState gameState
    |> resolveActivationPower playerID laneID cardID

let private getBonusDefenderDamage attackerPower targetPower targetInfo =
    match attackerPower, targetPower, targetInfo with
    | PassivePower Nimble, PassivePower Taunt, ActiveSingleTarget _
    | PassivePower Nimble, PassivePower Taunt, ActivePairMemberTarget _ ->
        1<health>
    | _ ->
        0<health>

let private getTargetIDFromTargetInfo targetInfo =
    match targetInfo with
    | InactiveTarget (owner, id) -> id
    | ActiveSingleTarget (owner, id) -> id
    | ActivePairMemberTarget (owner, id) -> id

let private getAttackerSelfDamage attackerPower targetPower targetInfo =
    match attackerPower, targetPower, targetInfo with
    | PassivePower Nimble, _, _ ->
        0<health>
    | _, PassivePower Retaliate, ActiveSingleTarget _
    | _, PassivePower Retaliate, ActivePairMemberTarget _ ->
        1<health>
    | _ ->
        0<health>

let private getSingleAttackInfo attackerID targetInfo lane =
    let targetID = getTargetIDFromTargetInfo targetInfo
    let attackerPower = List.find (fun (card: ActiveUnit) -> card.CardID = attackerID) lane.ActiveUnits |> (fun card -> card.Power)
    let targetPower = List.find (fun (card: ActiveUnit) -> card.CardID = targetID) lane.ActiveUnits |> (fun card -> card.Power)
    let baseDefenderDamage = 1<health>
    let bonusDefenderDamage = getBonusDefenderDamage attackerPower targetPower targetInfo
    let selfDamage = getAttackerSelfDamage attackerPower targetPower targetInfo
    targetID, baseDefenderDamage + bonusDefenderDamage, selfDamage

let private getPairAttackInfo pairMemberID targetInfo lane =
    let targetID = getTargetIDFromTargetInfo targetInfo
    let attackerPower = List.find (fun (card: ActiveUnit) -> card.CardID = pairMemberID) lane.ActiveUnits |> (fun card -> card.Power)
    let targetPower = List.find (fun (card: ActiveUnit) -> card.CardID = targetID) lane.ActiveUnits |> (fun card -> card.Power)
    let baseDefenderDamage = 2<health>
    let bonusDefenderDamage = getBonusDefenderDamage attackerPower targetPower targetInfo
    let selfDamage = getAttackerSelfDamage attackerPower targetPower targetInfo
    targetID, baseDefenderDamage + bonusDefenderDamage, selfDamage

let private executeSingleAttackAction playerID laneID attackerID targetInfo (gameState: GameStateDuringTurn) =
    let cardsState = gameState.CardsState
    let board = cardsState.Board
    let targetID, damage, selfDamage =
        getSingleAttackInfo attackerID targetInfo (Map.find laneID board.Lanes)
    board
    |> changeLaneWithFn laneID (
        incrementCardActionsUsed attackerID
        >> damageCard targetID damage
        >> damageCard attackerID selfDamage
        )
    |> triggerTargetInactiveDeathPowers laneID
    |> moveDeadCardsToDiscard
    |> changeBoard cardsState
    |> changeCardsState gameState
    |> resolveAttackerPassivePower playerID laneID (SingleCardID attackerID) targetID

let private executePairAttackAction playerID laneID (attackerID1, attackerID2) targetInfo (gameState: GameStateDuringTurn) =
    let cardsState = gameState.CardsState
    let board = cardsState.Board
    let targetID, damage, selfDamage =
        getPairAttackInfo attackerID1 targetInfo (Map.find laneID board.Lanes)
    board
    |> changeLaneWithFn laneID (
        incrementCardActionsUsed attackerID1
        >> incrementCardActionsUsed attackerID2
        >> damageCard targetID damage
        >> damageCard attackerID1 selfDamage
        >> damageCard attackerID2 selfDamage
        )
    |> triggerTargetInactiveDeathPowers laneID
    |> moveDeadCardsToDiscard
    |> changeBoard cardsState
    |> changeCardsState gameState
    |> resolveAttackerPassivePower playerID laneID (PairIDs (attackerID1, attackerID2)) targetID

let private executeCreatePairAction laneID cardID1 cardID2 (gameState: GameStateDuringTurn) =
    let lane = Map.find laneID gameState.CardsState.Board.Lanes
    let removedUnits, newLane =
        lane
        |> removeCardsFromActiveUnits [cardID1; cardID2]
    let card1 = removedUnits.[0]
    let card2 = removedUnits.[1]
    gameState.CardsState.Board
    |> changeLaneWithFn laneID (
        (fun _ -> newLane)
        >> addCardsToPairs (card1, card2)
        )
    |> changeBoard gameState.CardsState
    |> changeCardsState gameState

let private decrementActionsLeft (gameState: GameStateDuringTurn) =
    {gameState with
        TurnState = {
            gameState.TurnState with
                ActionsLeft = gameState.TurnState.ActionsLeft - 1
            }
        }

let private executeTurnAction action gameState =
    let gs = decrementActionsLeft gameState
    let postAction =
        match action with
        | Play (playerID, cardID, laneID) ->
            executePlayAction cardID laneID gs
            |> GameStateDuringTurn
        | Activate (playerID, laneID, cardID) ->
            executeActivateAction playerID laneID cardID gs
        | SingleAttack (playerID, laneID, attackerID, targetInfo) ->
            executeSingleAttackAction playerID laneID attackerID targetInfo gs
        | PairAttack (playerID, laneID, (attackerID1, attackerID2), targetInfo) ->
            executePairAttackAction playerID laneID (attackerID1, attackerID2) targetInfo gs
        | CreatePair (playerID, laneID, cardID1, cardID2) ->
            executeCreatePairAction laneID cardID1 cardID2 gs
            |> GameStateDuringTurn
    match postAction with
    | GameStateDuringMidActivationPowerChoice _
    | GameStateDuringMidPassivePowerChoice _
    | GameStateDuringStackChoice _ ->
        postAction
    | GameStateDuringTurn pa ->
        updateLaneWins pa
        |> GameStateDuringTurn
    | GameStateBetweenTurns _
    | GameStateWon _
    | GameStateTied _ ->
        failwithf "Shouldn't be here!"

let private startPlayerTurn playerID (gameState: GameStateBetweenTurns) : GameStateDuringTurn =
    let ts = gameState.TurnState
    {
        CardsState = gameState.CardsState
        TurnState = {
            CurrentPlayer = playerID
            NPlayers = ts.NPlayers
            ActionsLeft = ts.Actions
            FutureActionCounts = ts.FutureActionCounts
            }
        }

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
                |> List.map (fun (c1, c2) ->
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
                    newC1, newC2
                )
    }

let private timeoutOwnedFreezeStates playerID (cardsState: CardsState): CardsState =
    cardsState.Board
    |> changeLanesWithFn (timeoutOwnedFreezeStatesInLane playerID)
    |> changeBoard cardsState

let private resetActionCardActionsUsed activeUnits =
    activeUnits
    |> Map.map (fun _ _ -> 0)

let private resetAllActiveCardActionsUsed cardsState =
    cardsState.Board
    |> changeLanesWithFn (fun lane ->
     {lane with ActiveUnits = List.map (fun card -> {card with ActionsSpent = 0<action>}) lane.ActiveUnits}
    )
    |> changeBoard cardsState

let private resetActionMaxCardActions activeUnits =
    activeUnits
    |> List.map (fun card -> {card with MaxActions = 1<action>})

let private resetAllActiveMaxCardActions cardsState =
    cardsState.Board
    |> changeLanesWithFn (fun lane ->
     {lane with ActiveUnits = resetActionMaxCardActions lane.ActiveUnits}
    )
    |> changeBoard cardsState

let rec private makeNextActionInfo gameState action =
    let newGameState =
        match gameState, action with
        | GameStateDuringMidActivationPowerChoice gs, MidActivationPowerChoiceInfo mapci ->
            executeMidActivationPowerChoice mapci gs
            |> GameStateDuringTurn
            |> checkForGameEnd
        | GameStateDuringMidPassivePowerChoice gs, MidPassivePowerChoiceInfo mppci ->
            executeMidPassivePowerChoice mppci gs
            |> GameStateDuringTurn
            |> checkForGameEnd
        | GameStateDuringStackChoice gs, StackChoiceInfo sci ->
            let _, index, event = sci
            let newHead =
                gs.EpochEvents
                |> Map.remove index
                |> Map.toList
                |> List.map (fun (_, ev) -> ev)
                |> NonEmptyList.tryFromList
            let newStack =
                match newHead with
                | Some h -> Some (NonEmptyList.create h gs.FutureEpochs)
                | None -> NonEmptyList.tryFromList gs.FutureEpochs
            let choiceContext =
                match event with
                | ViewPowerContext (p, c) -> DiscardChoiceContext (p, c)
                | ForesightPowerContext (p, c) -> ForesightChoiceContext (p, c)
                | MovePowerContext (p, l, c) -> MoveChoiceContext (p, l, c)
            GameStateDuringMidActivationPowerChoice {
                CardsState = gs.CardsState
                TurnState = gs.TurnState
                ChoiceContext = choiceContext
                FutureStack = newStack
            }
        | GameStateDuringTurn gs, TurnActionInfo tai ->
            executeTurnAction tai gs
            |> checkForGameEnd
        | GameStateDuringTurn gs, EndTurn _ ->
            let tip = gs.TurnState
            let nextPlayer =
                if int tip.CurrentPlayer = tip.NPlayers then
                    1<PID>
                else
                    tip.CurrentPlayer + 1<PID>
            let actions, nextFutureActionCounts =
                match tip.FutureActionCounts with
                | [] -> 3, []
                | h :: t -> h, t
            GameStateBetweenTurns {
                CardsState =
                    gs.CardsState
                    |> resetAllActiveCardActionsUsed
                    |> resetAllActiveMaxCardActions
                    |> timeoutOwnedFreezeStates tip.CurrentPlayer
                TurnState = {
                    Player = nextPlayer
                    NPlayers = tip.NPlayers
                    Actions = actions
                    FutureActionCounts = nextFutureActionCounts
                    }
                }
        | GameStateBetweenTurns gs, StartTurn id ->
            gs
            |> startPlayerTurn id
            |> tryDrawCard id
            |> GameStateDuringTurn
        | GameStateDuringMidActivationPowerChoice _, MidPassivePowerChoiceInfo _
        | GameStateDuringMidActivationPowerChoice _, StackChoiceInfo _
        | GameStateDuringMidActivationPowerChoice _, TurnActionInfo _
        | GameStateDuringMidActivationPowerChoice _, StartTurn _
        | GameStateDuringMidActivationPowerChoice _, EndTurn _
        | GameStateDuringMidPassivePowerChoice _, MidActivationPowerChoiceInfo _
        | GameStateDuringMidPassivePowerChoice _, StackChoiceInfo _
        | GameStateDuringMidPassivePowerChoice _, TurnActionInfo _
        | GameStateDuringMidPassivePowerChoice _, StartTurn _
        | GameStateDuringMidPassivePowerChoice _, EndTurn _
        | GameStateDuringStackChoice _, MidActivationPowerChoiceInfo _
        | GameStateDuringStackChoice _, MidPassivePowerChoiceInfo _
        | GameStateDuringStackChoice _, TurnActionInfo _
        | GameStateDuringStackChoice _, StartTurn _
        | GameStateDuringStackChoice _, EndTurn _
        | GameStateDuringTurn _, StackChoiceInfo _
        | GameStateDuringTurn _, MidActivationPowerChoiceInfo _
        | GameStateDuringTurn _, MidPassivePowerChoiceInfo _
        | GameStateDuringTurn _, StartTurn _
        | GameStateDuringTurn _, EndTurn _
        | GameStateBetweenTurns _, StackChoiceInfo _
        | GameStateBetweenTurns _, MidActivationPowerChoiceInfo _
        | GameStateBetweenTurns _, MidPassivePowerChoiceInfo _
        | GameStateBetweenTurns _, TurnActionInfo _
        | GameStateBetweenTurns _, EndTurn _
        | GameStateWon _, _
        | GameStateTied _, _ ->
            failwithf "action incompatible with game state"
    let possibleActionsInfo = getPossibleActionsInfo newGameState
    let checkedGameState, checkedPossibleActionsInfo =
        match newGameState, possibleActionsInfo with
        | GameStateDuringMidActivationPowerChoice gs, [] ->
            let state = removeMidActivationPowerChoiceContext gs |> GameStateDuringTurn
            state, getPossibleActionsInfo state
        | GameStateDuringMidPassivePowerChoice gs, [] ->
            let state = removeMidPassivePowerChoiceContext gs |> GameStateDuringTurn
            state, getPossibleActionsInfo state
        | _ ->
            newGameState, possibleActionsInfo
    let newDisplayInfo = getDisplayInfo checkedGameState
    // Generation of next action's resulting capabilities is part of the
    // generated capability's body, since assigning them here requires
    // generating the entire game space, blowing the stack
    let capability() =
        InProgress (
            newDisplayInfo,
            checkedPossibleActionsInfo
            |> List.map (makeNextActionInfo checkedGameState)
            )
    {
        Action = action
        Capability = capability
        }

let private createGame nPlayers nLanes =
    let shuffledPowerDeck =
        createUnshuffledPowerDeck()
        |> shuffle
    let cardIndices =
        createIDs 1<CID> shuffledPowerDeck
    let cardPowers =
        shuffledPowerDeck
        |> List.zip cardIndices
        |> Map.ofList
    let bases, notBaseCards =
        cardIndices
        |> prepareHead (prepareBases nLanes) (nPlayers*nLanes)
    let handCards, notDeckCards =
        notBaseCards
        |> prepareHead (prepareHands nPlayers) (5*nPlayers)
    let removed, notRemoved =
        notDeckCards
        |> prepareHead prepareRemoved 10
    let drawPile =
        notRemoved
        |> List.map (fun id -> {CardID = id; Power = Map.find id cardPowers} : HandCard)
        |> NonEmptyList.fromList
    let hiddenCardKnownBys =
        handCards
        |> Map.toList
        |> List.collect (fun (playerID, cards) -> List.map (fun card -> card, playerID) cards)
        |> Set.ofList
    let gameState = GameStateBetweenTurns {
        CardsState = {
            Board = {
                Lanes =
                    List.replicate nLanes emptyLane
                    |> createIDMap 1<LID>
                Discard = List.empty
                }
            GameStage = Early {
                Bases =
                    bases
                    |> Map.map (fun _ cardIDs ->
                        cardIDs
                        |> createIDMap 1<PID>
                        |> Map.toList
                        |> List.map (fun (ownerID, cardID) -> {
                            CardID = cardID
                            Power = Map.find cardID cardPowers
                            Owner = ownerID
                            KnownBy = Set.empty
                        }
                        )
                    )
                DrawPile = drawPile
                HandCards =
                    handCards
                    |> Map.map (fun _ cardIDs ->
                        cardIDs
                        |> List.map (fun cardID -> {CardID = cardID; Power = Map.find cardID cardPowers})
                        )
            }
            Removed = Set.map (fun id -> {CardID = id; Power = Map.find id cardPowers}) removed
            }
        TurnState = {
            Player = 1<PID>
            NPlayers = nPlayers
            Actions = 2
            FutureActionCounts = List.empty
            }
        }
    let displayInfo = getDisplayInfo gameState
    let nextActions =
        getPossibleActionsInfo gameState
        |> List.map (makeNextActionInfo gameState)
    InProgress (displayInfo, nextActions)

let api = {
    NewGame = fun () -> createGame 2 3
}
