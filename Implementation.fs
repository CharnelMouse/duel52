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

type private RemovedCardID = RemovedCardID of CardID
type private DeckCardID = DeckCardID of CardID
type private HandCardID = HandCardID of CardID
type private BaseCardID = BaseCardID of CardID
type private InactiveUnitID = InactiveUnitID of CardID
type private ActiveUnitID = ActiveUnitID of CardID
type private UnitID = UnitID of CardID
type private DiscardedCardID = DiscardedCardID of CardID

type private ForesightTargetID = ForesightTargetID of CardID
type private AttackerIDs =
| SingleAttackerID of ActiveUnitID
| PairAttackerIDs of ActiveUnitID * ActiveUnitID
let private transferAttackerIDs attackerIDs =
    match attackerIDs with
    | SingleAttackerID (ActiveUnitID id) -> SingleCardID id
    | PairAttackerIDs (ActiveUnitID id1, ActiveUnitID id2) -> PairIDs (id1, id2)

type private RemovedCard = {
    RemovedCardID: RemovedCardID
    Power: Power
}

type private DeckCard = {
    DeckCardID: DeckCardID
    Power: Power
}

type private HandCard = {
    HandCardID: HandCardID
    Power: Power
    Owner: PlayerID
}
let private getHandCardInfo {HandCardID = HandCardID id; Power = p} =
    HandCardInfo (id, p)

type private BaseCard = {
    BaseCardID: BaseCardID
    Power: Power
    Owner: PlayerID
    KnownBy: PlayerID Set
}
let private getBaseKnowledge playerID (baseCard: BaseCard) =
    if Set.contains playerID baseCard.KnownBy then
        KnownBaseCard (baseCard.Owner, baseCard.Power)
    else
        UnknownBaseCard baseCard.Owner

type private FreezeStatus =
| FrozenBy of PlayerID
| NotFrozen

type private InactiveUnit = {
    InactiveUnitID: InactiveUnitID
    Power: Power
    Owner: PlayerID
    KnownBy: PlayerID Set
    Damage: Damage
    FreezeStatus: FreezeStatus
}

type private ActiveUnit = {
    ActiveUnitID: ActiveUnitID
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

type private Pair = ActiveUnit * ActiveUnit

type private FaceDownDiscardedCard = {
    DiscardedCardID: DiscardedCardID
    Power: Power
    KnownBy: PlayerID Set
}

type private FaceUpDiscardedCard = {
    DiscardedCardID: DiscardedCardID
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

let private maxHealth card =
    match card with
    | InactiveUnit _ -> 2<health>
    | ActiveUnit {Power = power} ->
        match power with
        | PassivePower Taunt -> 3<health>
        | _ -> 2<health>
let private (|Dead|Alive|) (card: UnitCard) =
    let damage =
        match card with
        | InactiveUnit {Damage = damage}
        | ActiveUnit {Damage = damage} -> damage
    if damage < maxHealth card then
        Alive
    else
        Dead
let private isDead (card: UnitCard) =
    match card with
    | Alive -> false
    | Dead -> true

type private CardConverter<'From, 'To> = 'From -> 'To

let private deckToHandCard playerID : CardConverter<DeckCard, HandCard> = fun deckCard ->
    let {DeckCardID = DeckCardID id} = deckCard
    {
        HandCardID = HandCardID id
        Power = deckCard.Power
        Owner = playerID
    }
let private handToDiscardedCard: CardConverter<HandCard, DiscardedCard> = fun handCard ->
    let {HandCardID = HandCardID id} = handCard
    FaceDownDiscardedCard {
        DiscardedCardID = DiscardedCardID id
        Power = handCard.Power
        KnownBy = Set.singleton handCard.Owner
    }
let private handToInactiveUnit: CardConverter<HandCard, InactiveUnit> = fun handCard ->
    let {HandCardID = HandCardID id} = handCard
    {
        InactiveUnitID = InactiveUnitID id
        Power = handCard.Power
        Owner = handCard.Owner
        KnownBy = Set.singleton handCard.Owner
        Damage = 0<health>
        FreezeStatus = NotFrozen
    }
let private inactiveToActiveUnit: CardConverter<InactiveUnit, ActiveUnit> = fun inactiveUnit ->
    let {InactiveUnitID = InactiveUnitID id} = inactiveUnit
    {
        ActiveUnitID = ActiveUnitID id
        Power = inactiveUnit.Power
        Owner = inactiveUnit.Owner
        Damage = inactiveUnit.Damage
        ActionsSpent = 0<action>
        MaxActions = 1<action>
        FreezeStatus = inactiveUnit.FreezeStatus
    }
let private unitToDiscardedCard: CardConverter<UnitCard, DiscardedCard> = fun unitCard ->
    match unitCard with
    | InactiveUnit {InactiveUnitID = InactiveUnitID id; Power = power; KnownBy = knownBy} ->
        FaceDownDiscardedCard {
            DiscardedCardID = DiscardedCardID id
            Power = power
            KnownBy = knownBy
        }
    | ActiveUnit {ActiveUnitID = ActiveUnitID id; Power = power} ->
        FaceUpDiscardedCard {
            DiscardedCardID = DiscardedCardID id
            Power = power
        }

type private Lane = {
    InactiveUnits: InactiveUnit list
    ActiveUnits: ActiveUnit list
    Pairs: Pair list
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

type private Board = {
    Lanes: Map<LaneID, Lane>
    Discard: DiscardedCard list
}

let private findDeadCardsInLane laneID board =
    let lane = Map.find laneID board.Lanes
    (lane.InactiveUnits |> List.map InactiveUnit |> List.filter isDead)
    @ (lane.ActiveUnits |> List.map ActiveUnit |> List.filter isDead)
    @ (lane.Pairs |> List.collect (fun (card1, card2) -> [ActiveUnit card1; ActiveUnit card2]) |> List.filter isDead)
let private findDeadCardIDsInLane laneID board =
    let lane = Map.find laneID board.Lanes
    (lane.InactiveUnits |> List.choose (fun card ->
        let {InactiveUnitID = InactiveUnitID id} = card
        if isDead (InactiveUnit card) then Some (UnitID id) else None
        ))
    @ (lane.ActiveUnits |> List.choose (fun card ->
        let {ActiveUnitID = ActiveUnitID id} = card
        if isDead (ActiveUnit card) then Some (UnitID id) else None
        ))
    @ (lane.Pairs |> List.collect (fun (card1, card2) -> [card1; card2]) |> List.choose (fun card ->
        let {ActiveUnitID = ActiveUnitID id} = card
        if isDead (ActiveUnit card) then Some (UnitID id) else None
        ))

type private EarlyGameInfo = {
    Bases: Map<LaneID, BaseCard list>
    DrawPile: DeckCard NonEmptyList
    HandCards: Map<PlayerID, HandCard list>
}

type private PostDrawGameInfo = {
    HandCards: Map<PlayerID, HandCard list>
    LaneWins: Map<LaneID, PlayerID>
}

type private PostHandGameInfo = {
    LaneWins: Map<LaneID, PlayerID>
}

type private GameStage =
| Early of EarlyGameInfo
| DrawPileEmpty of PostDrawGameInfo
| HandsEmpty of PostHandGameInfo

type private CardsState = {
    Board: Board
    GameStage: GameStage
    Removed: RemovedCard Set
}

type private CardRemover<'T, 'TID> = 'TID -> LaneID -> CardsState -> 'T * CardsState
type private CardsRemover<'T, 'TID> = 'TID list -> LaneID -> CardsState -> 'T list * CardsState
type private CardAdder<'T> = 'T -> LaneID -> CardsState -> CardsState
type private CardsAdder<'T> = 'T list -> LaneID -> CardsState -> CardsState

let private removeCardFromInactiveUnits: CardRemover<InactiveUnit, InactiveUnitID> = fun (InactiveUnitID cardID) laneID cardsState ->
    let board = cardsState.Board
    let lane = Map.find laneID board.Lanes
    let removedCards, newInactiveUnits =
        lane.InactiveUnits
        |> List.partition (fun {InactiveUnitID = InactiveUnitID id} -> id = cardID)
    let newLane = {lane with InactiveUnits = newInactiveUnits}
    removedCards.Head,
    {cardsState with Board = {board with Lanes = board.Lanes |> Map.add laneID newLane}}
let private removeCardsFromInactiveUnits: CardsRemover<InactiveUnit, InactiveUnitID> = fun cardIDs laneID cardsState ->
    let board = cardsState.Board
    let lane = Map.find laneID board.Lanes
    let removed, newInactiveUnits =
        lane.InactiveUnits
        |> List.partition (fun {InactiveUnitID = id} -> List.contains id cardIDs)
    let newLane = {lane with InactiveUnits = newInactiveUnits}
    removed,
    {cardsState with Board = {board with Lanes = board.Lanes |> Map.add laneID newLane}}
let private removeCardsFromActiveUnits: CardsRemover<ActiveUnit, ActiveUnitID> = fun cardIDs laneID cardsState ->
    let board = cardsState.Board
    let lane = Map.find laneID board.Lanes
    let removed, newActiveUnits =
        lane.ActiveUnits
        |> List.partition (fun {ActiveUnitID = id} -> List.contains id cardIDs)
    let newLane = {lane with ActiveUnits = newActiveUnits}
    removed,
    {cardsState with Board = {board with Lanes = board.Lanes |> Map.add laneID newLane}}
let private removeCardsFromPairs: CardsRemover<ActiveUnit, ActiveUnitID> = fun cardIDs laneID cardsState ->
    let board = cardsState.Board
    let lane = Map.find laneID board.Lanes
    let removed, newPairs =
        lane.Pairs
        |> List.partition (fun ({ActiveUnitID = id1}, {ActiveUnitID = id2}) ->
            List.contains id1 cardIDs || List.contains id2 cardIDs
            )
    let flatRemoved = List.collect (fun (c1, c2) -> [c1; c2]) removed
    let newLane = {lane with Pairs = newPairs}
    flatRemoved,
    {cardsState with Board = {board with Lanes = board.Lanes |> Map.add laneID newLane}}
let private addCardToInactiveUnits: CardAdder<InactiveUnit> = fun card laneID cardsState ->
    let board = cardsState.Board
    let lane = Map.find laneID board.Lanes
    let newLane = {lane with InactiveUnits = lane.InactiveUnits @ [card]}
    {cardsState with Board = {board with Lanes = board.Lanes |> Map.add laneID newLane}}
let private addCardsToInactiveUnits: CardsAdder<InactiveUnit> = fun cards laneID cardsState ->
    let board = cardsState.Board
    let lane = Map.find laneID board.Lanes
    let newLane = {lane with InactiveUnits = lane.InactiveUnits @ cards}
    {cardsState with Board = {board with Lanes = board.Lanes |> Map.add laneID newLane}}
let private addCardToActiveUnits: CardAdder<ActiveUnit> = fun cardID laneID cardsState ->
    let board = cardsState.Board
    let lane = Map.find laneID board.Lanes
    let newLane = {lane with ActiveUnits = lane.ActiveUnits @ [cardID]}
    {cardsState with Board = {board with Lanes = board.Lanes |> Map.add laneID newLane}}
let private addCardsToActiveUnits: CardsAdder<ActiveUnit> = fun cards laneID cardsState ->
    let board = cardsState.Board
    let lane = Map.find laneID board.Lanes
    let newLane = {lane with ActiveUnits = lane.ActiveUnits @ cards}
    {cardsState with Board = {board with Lanes = board.Lanes |> Map.add laneID newLane}}
let private addCardsToPairs: CardAdder<ActiveUnit*ActiveUnit> = fun (card1, card2) laneID cardsState ->
    let board = cardsState.Board
    let lane = Map.find laneID board.Lanes
    let newLane = {lane with Pairs = lane.Pairs @ [card1, card2]}
    {cardsState with Board = {board with Lanes = board.Lanes |> Map.add laneID newLane}}
let private addCardPairPartnersToActiveUnits: CardsAdder<ActiveUnitID> = fun cardIDs laneID cardsState ->
    let board = cardsState.Board
    let lane = Map.find laneID board.Lanes
    let partners =
        lane.Pairs
        |> List.collect (fun (card1, card2) ->
            let {ActiveUnitID = id1} = card1
            let {ActiveUnitID = id2} = card1
            match List.contains id1 cardIDs, List.contains id2 cardIDs with
            | true, true ->
                []
            | true, false ->
                [card2]
            | false, true ->
                [card1]
            | false, false ->
                []
            )
    addCardsToActiveUnits partners laneID cardsState

let private removeCardsFromLane: CardsRemover<UnitCard, UnitID> = fun cardIDs laneID cardsState ->
    let removedInactive, cs1 =
        cardsState
        |> removeCardsFromInactiveUnits (cardIDs |> List.map (fun (UnitID id) -> InactiveUnitID id)) laneID
    let removedActive, cs2 =
        cs1
        |> removeCardsFromActiveUnits (cardIDs |> List.map (fun (UnitID id) -> ActiveUnitID id)) laneID
    let cs3 =
        cs2
        |> addCardPairPartnersToActiveUnits (cardIDs |> List.map (fun (UnitID id) -> ActiveUnitID id)) laneID
    let removedPaired, cs4 =
        cs3
        |> removeCardsFromPairs (cardIDs |> List.map (fun (UnitID id) -> ActiveUnitID id)) laneID
    let removed =
        (List.map InactiveUnit removedInactive)
        @ (List.map ActiveUnit removedActive)
        @ (List.map ActiveUnit removedPaired)
    removed, cs4
let private changeCardLane cardID fromLaneID toLaneID cardsState =
    let moved, cs1 =
        cardsState
        |> removeCardsFromLane [cardID] fromLaneID
    let movedInactive =
        moved
        |> List.choose (function
            | InactiveUnit c -> Some c
            | ActiveUnit _ -> None
            )
    let movedActive =
        moved
        |> List.choose (function
            | InactiveUnit _ -> None
            | ActiveUnit c -> Some c
            )
    cs1
    |> addCardsToInactiveUnits movedInactive toLaneID
    |> addCardsToActiveUnits movedActive toLaneID

let private healActiveCard (activeCard: ActiveUnit) =
    {activeCard with Damage = 0<health>}

let private flipAndActivateInactiveDeathPowersInLane laneID zeroHealthInactiveDeathPowerUnits cardsState =
    let inactiveIDs =
        zeroHealthInactiveDeathPowerUnits
        |> List.choose (fun card ->
            match card with
            | InactiveUnit {InactiveUnitID = cid} -> Some cid
            | ActiveUnit _ -> None
            )
    let removedCards, cs1 =
        cardsState
        |> removeCardsFromInactiveUnits inactiveIDs laneID
    let newCards =
        removedCards
        |> List.map (inactiveToActiveUnit >> healActiveCard)
    addCardsToActiveUnits newCards laneID cs1

let private damageCard (UnitID cardID) damage laneID cardsState =
    let board = cardsState.Board
    let lane = Map.find laneID board.Lanes
    let newLane = {
        lane with
            InactiveUnits =
                lane.InactiveUnits
                |> List.map (fun card ->
                    let {InactiveUnitID = InactiveUnitID id} = card
                    if id = cardID then {card with Damage = card.Damage + damage} else card
                    )
            ActiveUnits =
                lane.ActiveUnits
                |> List.map (fun card ->
                    let {ActiveUnitID = ActiveUnitID id} = card
                    if id = cardID then {card with Damage = card.Damage + damage} else card
                    )
            Pairs =
                lane.Pairs
                |> List.map (fun (card1, card2) ->
                    let {ActiveUnitID = ActiveUnitID id1} = card1
                    let {ActiveUnitID = ActiveUnitID id2} = card2
                    (if id1 = cardID then {card1 with Damage = card1.Damage + damage} else card1),
                    (if id2 = cardID then {card2 with Damage = card2.Damage + damage} else card2)
                    )
        }
    {cardsState with Board = {board with Lanes = board.Lanes |> Map.add laneID newLane}}

let private incrementCardActionsUsed cardID laneID cardsState =
    let board = cardsState.Board
    let lane = Map.find laneID board.Lanes
    let newLane = {
        lane with
            ActiveUnits =
                lane.ActiveUnits
                |> List.map (fun card ->
                    let {ActiveUnitID = ActiveUnitID id} = card
                    if id = cardID then
                        {card with ActionsSpent = card.ActionsSpent + 1<action>}
                    else
                        card
                    )
            Pairs =
                lane.Pairs
                |> List.map (fun (card1, card2) ->
                    let {ActiveUnitID = ActiveUnitID id1} = card1
                    let {ActiveUnitID = ActiveUnitID id2} = card2
                    if id1 = cardID then
                        {card1 with ActionsSpent = card1.ActionsSpent + 1<action>}, card2
                    elif id2 = cardID then
                        card1, {card2 with ActionsSpent = card2.ActionsSpent + 1<action>}
                    else
                        card1, card2
                    )
        }
    {cardsState with Board = {board with Lanes = board.Lanes |> Map.add laneID newLane}}

let private setMaxCardActions cardID left laneID cardsState =
    let board = cardsState.Board
    let lane = Map.find laneID board.Lanes
    let newLane = {
        lane with
            ActiveUnits =
                lane.ActiveUnits
                |> List.map (fun card ->
                    let {ActiveUnitID = ActiveUnitID id} = card
                    if id = cardID then
                        {card with MaxActions = left}
                    else
                        card
                    )
            Pairs =
                lane.Pairs
                |> List.map (fun (card1, card2) ->
                    let {ActiveUnitID = ActiveUnitID id1} = card1
                    let {ActiveUnitID = ActiveUnitID id2} = card2
                    if id1 = cardID then
                        {card1 with MaxActions = left}, card2
                    elif id2 = cardID then
                        card1, {card2 with MaxActions = left}
                    else
                        card1, card2
                    )
            }
    {cardsState with Board = {board with Lanes = board.Lanes |> Map.add laneID newLane}}

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
let private flipBasesOnLane (bases: BaseCard list, lane: Lane) =
    let newUnits =
        bases
        |> List.map (fun baseCard ->
            let {BaseCardID = BaseCardID id} = baseCard
            {
                InactiveUnitID = InactiveUnitID id
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
    Actions: Actions
    FutureActionCounts: Actions list
}

type private TurnInProgress = {
    CurrentPlayer: PlayerID
    NPlayers: int
    ActionsLeft: Actions
    FutureActionCounts: Actions list
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
    LaneWins: Map<LaneID, PlayerID>
}

type private GameStateTied = {
    Lanes: Map<LaneID, Lane>
    LaneWins: Map<LaneID, PlayerID>
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
                ActionsLeft = gameState.TurnState.ActionsLeft + 1<action>
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
let private triggerTargetInactiveDeathPowers (gameState: GameStateDuringTurn) =
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
            | InactiveUnit {Power = p} ->
                match p with
                | InactiveDeathPower _ ->
                    true
                | ActivationPower _
                | PassivePower _ ->
                    false
            | ActiveUnit _ ->
                false
            )
    laneIDs
    |> List.fold (fun cs laneID -> flipAndActivateInactiveDeathPowersInLane laneID zeroHealthInactiveDeathPowerUnits cs) cardsState
    |> changeCardsState gameState
let private moveDeadCardsToDiscard (gameState: GameStateDuringTurn) =
    let cardsState = gameState.CardsState
    let board = cardsState.Board
    let laneIDs =
        board.Lanes
        |> Map.toList
        |> List.map (fun (id, _) -> id)
    let removed, newCardsState =
        laneIDs
        |> List.fold (fun (removed, cs) id ->
            let deadCardIDs = findDeadCardIDsInLane id cs.Board
            let newRemoved, newCS = removeCardsFromLane deadCardIDs id cs
            (removed @ newRemoved), newCS
            ) ([], cardsState)
    let discardCards = List.map unitToDiscardedCard removed
    let newDiscard = {newCardsState.Board with Discard = (newCardsState.Board.Discard @ discardCards)}
    {cardsState with Board = newDiscard}
    |> changeCardsState gameState
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
        let newGameStage = DrawPileEmpty {gs with LaneWins = currentLaneWins}
        let newCardsState = {gameState.CardsState with GameStage = newGameStage}
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
        let newGameStage = HandsEmpty {gs with LaneWins = currentLaneWins}
        let newCardsState = {gameState.CardsState with GameStage = newGameStage}
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

let private getActionability ({FreezeStatus = fs}: ActiveUnit) =
    match fs with
    | FrozenBy _ -> Frozen
    | NotFrozen -> Normal

let private getPairKnowledge (card1, card2) : PairKnowledge =
    let {ActiveUnitID = ActiveUnitID id1} = card1
    let {ActiveUnitID = ActiveUnitID id2} = card2
    let actionability1 = getActionability card1
    let actionability2 = getActionability card2
    id1, id2,
    card1.Power,
    card1.Damage, card2.Damage,
    min (card1.MaxActions - card1.ActionsSpent) (card2.MaxActions - card2.ActionsSpent),
    actionability1, actionability2

let private getTroops viewerID ownerID (lane: Lane) : InactiveUnitKnowledge list * ActiveUnitKnowledge list * PairKnowledge list =
    let pairsKnowledge =
        lane.Pairs
        |> List.filter (fun (card1, card2) -> card1.Owner = ownerID)
        |> List.map getPairKnowledge
    let nonPairedActiveUnitKnowledge =
        lane.ActiveUnits
        |> List.filter (fun card -> card.Owner = ownerID)
        |> List.map (fun card ->
            let {ActiveUnitID = ActiveUnitID id} = card
            let actionability = getActionability card
            ((id, card.Power, card.Damage, card.MaxActions - card.ActionsSpent, actionability): ActiveUnitKnowledge)
            )
    let inactiveUnitKnowledge =
        lane.InactiveUnits
        |> List.filter (fun card -> card.Owner = ownerID)
        |> List.map (fun card ->
            let {InactiveUnitID = InactiveUnitID id} = card
            let actionability = if card.FreezeStatus = NotFrozen then Normal else Frozen
            if Set.contains viewerID card.KnownBy then
                KnownInactiveCardKnowledge (id, card.Power, card.Damage, actionability)
            else
                UnknownInactiveCardKnowledge (id, card.Damage, actionability)
            )
    inactiveUnitKnowledge,
    nonPairedActiveUnitKnowledge,
    pairsKnowledge

let private getDeadCardKnowledge (playerID: PlayerID) (card: DiscardedCard) =
    match card with
    | FaceDownDiscardedCard {Power = p; KnownBy = kb} ->
        if Set.contains playerID kb then
            KnownFaceDownDeadCard p
        else
            UnknownDeadCard
    | FaceUpDiscardedCard {Power = p} ->
        KnownFaceUpDeadCard p

let private getPlayerLaneWins (laneWins: Map<LaneID, PlayerID>) =
    laneWins
    |> Map.toList
    |> List.groupBy (fun (_, pid) -> pid)
    |> List.map (fun (key, pairs) ->
        key,
        pairs
        |> List.map (fun (lid, _) -> lid)
        )

let private getBoardKnowledge viewerID cardsState turnInProgress =
    let ({Lanes = l; Discard = d}: Board) = cardsState.Board
    let getBase = getBaseKnowledge viewerID
    let getDeadCard = getDeadCardKnowledge viewerID
    match cardsState.GameStage with
    | Early {Bases = b; DrawPile = dp} ->
        let lanesKnowledge =
            joinMaps b l
            |> Map.map (fun _ (bases, lane) ->
                let troopKnowledge =
                    createIDsToLength 1<PID> turnInProgress.NPlayers
                    |> List.map (fun playerID ->
                        playerID, getTroops viewerID playerID lane
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
                    createIDsToLength 1<PID> turnInProgress.NPlayers
                    |> List.map (fun playerID ->
                        playerID, getTroops viewerID playerID lane
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
                    createIDsToLength 1<PID> turnInProgress.NPlayers
                    |> List.map (fun playerID ->
                        playerID, getTroops viewerID playerID lane
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
    let (playerHand, opponentHands) =
        match gameStage with
        | Early {HandCards = hco}
        | DrawPileEmpty {HandCards = hco} ->
            hco
            |> Map.partition (fun owner _ -> owner = viewerID)
        | HandsEmpty _ ->
            Map.empty, Map.empty
    let playerHandInfo =
        playerHand
        |> Map.toList
        |> List.collect (fun (_, cards) -> cards |> List.map getHandCardInfo)
    let opponentHandSizes =
        opponentHands
        |> Map.map (fun _ cards -> List.length cards)
        |> Map.toList
    playerHandInfo, opponentHandSizes

let private getDisplayInfo gameState =
    match gameState with
    | GameStateDuringMidPassivePowerChoice gs ->
        let id = gs.TurnState.CurrentPlayer
        let (playerHandInfo, opponentHandSizes) = getHandInfos id gs.CardsState.GameStage
        let boardKnowledge = getBoardKnowledge id gs.CardsState gs.TurnState
        MidPassivePowerChoiceDisplayInfo {
            CurrentPlayer = id
            ActionsLeft = gs.TurnState.ActionsLeft
            BoardKnowledge = boardKnowledge
            PlayerHand = playerHandInfo
            OpponentHandSizes = opponentHandSizes
            ChoiceContext = gs.ChoiceContext
        }
    | GameStateDuringMidActivationPowerChoice gs ->
        let id = gs.TurnState.CurrentPlayer
        let (playerHandInfo, opponentHandSizes) = getHandInfos id gs.CardsState.GameStage
        let boardKnowledge = getBoardKnowledge id gs.CardsState gs.TurnState
        MidActivationPowerChoiceDisplayInfo {
            CurrentPlayer = id
            ActionsLeft = gs.TurnState.ActionsLeft
            BoardKnowledge = boardKnowledge
            PlayerHand = playerHandInfo
            OpponentHandSizes = opponentHandSizes
            ChoiceContext = gs.ChoiceContext
        }
    | GameStateDuringStackChoice gs ->
        let id = gs.TurnState.CurrentPlayer
        let (playerHandInfo, opponentHandSizes) = getHandInfos id gs.CardsState.GameStage
        let boardKnowledge = getBoardKnowledge id gs.CardsState gs.TurnState
        StackChoiceDisplayInfo {
            CurrentPlayer = id
            ActionsLeft = gs.TurnState.ActionsLeft
            BoardKnowledge = boardKnowledge
            PlayerHand = playerHandInfo
            OpponentHandSizes = opponentHandSizes
            Stack = {
                Head = Map.toList gs.EpochEvents |> List.map snd |> NonEmptyList.fromList
                Tail = gs.FutureEpochs
            }
        }
    | GameStateDuringTurn gs ->
        let id = gs.TurnState.CurrentPlayer
        let (playerHandInfo, opponentHandSizes) = getHandInfos id gs.CardsState.GameStage
        let boardKnowledge = getBoardKnowledge id gs.CardsState gs.TurnState
        TurnDisplayInfo {
            CurrentPlayer = id
            ActionsLeft = gs.TurnState.ActionsLeft
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

let private getPlayActionsInfo (gameState: GameStateDuringTurn) =
    let playerID = gameState.TurnState.CurrentPlayer
    match gameState.CardsState.GameStage with
    | Early {HandCards = hc}
    | DrawPileEmpty {HandCards = hc} ->
        Map.find playerID hc
    | HandsEmpty _ ->
        List.empty
    |> List.allPairs (createIDsToLength 1<LID> (Map.count gameState.CardsState.Board.Lanes))
    |> List.map (fun (laneID, {HandCardID = HandCardID id}) ->
        Play (playerID, id, laneID)
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
        |> List.map (fun card ->
            let {InactiveUnitID = InactiveUnitID id} = card
            Activate (playerID, laneID, id)
            )
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
            let {ActiveUnitID = ActiveUnitID id1} = card1
            let {ActiveUnitID = ActiveUnitID id2} = card2
            CreatePair (playerID, laneID, id1, id2)
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
                | InactiveUnit {InactiveUnitID = InactiveUnitID cid}
                | ActiveUnit {ActiveUnitID = ActiveUnitID cid} -> cid
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
                    let card =
                        lane.ActiveUnits
                        |> List.find (fun {ActiveUnitID = ActiveUnitID id} -> id = cardID)
                    card.Power = PassivePower Taunt
            )

        let singleAttacks =
            possibleUnitAttackers
            |> List.collect (fun attacker ->
                let {ActiveUnitID = ActiveUnitID attackerID; Power = power} = attacker
                if power = PassivePower Nimble || List.isEmpty tauntTargets then
                    allTargets
                    |> List.map (fun target ->
                        SingleAttack (playerID, laneID, attackerID, target)
                    )
                else
                    tauntTargets
                    |> List.map (fun target ->
                        SingleAttack (playerID, laneID, attackerID, target)
                    )
            )
        let pairAttacks =
            possiblePairAttackers
            |> List.collect (fun (attacker1, attacker2) ->
                let {ActiveUnitID = ActiveUnitID attackerID1;  Power = power} = attacker1
                let {ActiveUnitID = ActiveUnitID attackerID2} = attacker2
                if power = PassivePower Nimble || List.isEmpty tauntTargets then
                    allTargets
                    |> List.map (fun target ->
                        PairAttack (playerID, laneID, (attackerID1, attackerID2), target)
                    )
                else
                    tauntTargets
                    |> List.map (fun target ->
                        PairAttack (playerID, laneID, (attackerID1, attackerID2), target)
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

type private ActionPair =
| MidActivationPowerChoicePair of GameStateDuringMidActivationPowerChoice * MidActivationPowerChoiceInfo
| MidPassivePowerChoicePair of GameStateDuringMidPassivePowerChoice * MidPassivePowerChoiceInfo
| StackChoicePair of GameStateDuringStackChoice * StackChoiceInfo
| TurnActionChoicePair of GameStateDuringTurn * TurnActionInfo
| StartTurnPair of GameStateBetweenTurns * PlayerID
    
let private getPossibleActionPairs (gameState: GameState) =
    match gameState with
    | GameStateBetweenTurns gs ->
        StartTurnPair (gs, gs.TurnState.Player)
        |> List.singleton
    | GameStateDuringTurn gs ->
        let currentPlayer = gs.TurnState.CurrentPlayer
        if gs.TurnState.ActionsLeft = 0<action> then
            TurnActionChoicePair (gs, EndTurn currentPlayer)
            |> List.singleton
        else
            let actions =
                getPlayActionsInfo gs
                @ getActivateActionsInfo gs
                @ getAttackActionsInfo gs
                @ getPairActionsInfo gs
            if List.isEmpty actions then
                TurnActionChoicePair (gs, EndTurn currentPlayer)
                |> List.singleton
            else
               actions
               |> List.map (fun action -> TurnActionChoicePair (gs, ActionChoiceInfo action))
    | GameStateDuringStackChoice gs ->
        gs.EpochEvents
        |> Map.toList
        |> List.map (fun (eventID, event) ->
            StackChoicePair (gs, (gs.TurnState.CurrentPlayer, eventID, event))
        )
    | GameStateDuringMidActivationPowerChoice gs ->
        match gs.ChoiceContext with
        | DiscardChoiceContext (playerID, powerCardID) ->
            match gs.CardsState.GameStage with
            | Early {HandCards = hc}
            | DrawPileEmpty {HandCards = hc} ->
                hc
                |> Map.find playerID
                |> List.map (fun {HandCardID = HandCardID id} ->
                    MidActivationPowerChoicePair (gs, DiscardChoice (playerID, powerCardID, id))
                    )
            | HandsEmpty _ ->
                failwithf "Can't discard from an empty hand"
        | ForesightChoiceContext (playerID, powerCardID) ->
            let unknownInactiveUnitIDs =
                gs.CardsState.Board.Lanes
                |> Map.toList
                |> List.collect (fun (_, lane) -> lane.InactiveUnits)
                |> List.filter (fun card -> not (Set.contains playerID card.KnownBy))
                |> List.map (fun {InactiveUnitID = InactiveUnitID id} -> ForesightTargetID id)
            let unknownFaceDownCardIDs =
                match gs.CardsState.GameStage with
                | Early {Bases = bases} ->
                    let unknownBaseIDs =
                        bases
                        |> Map.toList
                        |> List.collect (fun (laneID, b) ->
                            b
                            |> List.filter (fun card -> not (Set.contains playerID card.KnownBy))
                            |> List.map (fun {BaseCardID = BaseCardID id} -> ForesightTargetID id)
                            )
                    unknownBaseIDs @ unknownInactiveUnitIDs
                | DrawPileEmpty _
                | HandsEmpty _ ->
                    unknownInactiveUnitIDs
            unknownFaceDownCardIDs
            |> List.map (fun (ForesightTargetID id) ->
                MidActivationPowerChoicePair (gs, ForesightChoice (playerID, powerCardID, id))
                )
        | MoveChoiceContext (playerID, laneID, powerCardID) ->
            let pairs =
                gs.CardsState.Board.Lanes
                    |> Map.filter (fun targetLaneID _ -> targetLaneID <> laneID)
                |> Map.toList
                |> List.collect (fun (targetLaneID, lane) ->
                    (lane.InactiveUnits |> List.choose (fun {InactiveUnitID = InactiveUnitID id; Owner = ownerID}-> if ownerID = playerID then Some id else None))
                    @ (lane.ActiveUnits |> List.choose (fun {ActiveUnitID = ActiveUnitID id; Owner = ownerID} -> if ownerID = playerID then Some id else None))
                    @ (lane.Pairs |> List.collect (fun ({ActiveUnitID = ActiveUnitID id1; Owner = ownerID}, {ActiveUnitID = ActiveUnitID id2}) -> if ownerID = playerID then [id1; id2] else []))
                    |> List.map (fun id -> targetLaneID, id)
                )
                |> List.map (fun (targetLaneID, targetCardID) ->
                    MidActivationPowerChoicePair (gs, MoveChoice (Some (playerID, laneID, powerCardID, targetLaneID, targetCardID)))
                )
            if List.isEmpty pairs then
                pairs
            else
                MidActivationPowerChoicePair (gs, MoveChoice (None)) :: pairs
    | GameStateDuringMidPassivePowerChoice gs ->
        match gs.ChoiceContext with
        | TwinStrikeChoiceContext (playerID, laneID, powerCardID, originalTargetCardID) ->
            let lane = Map.find laneID gs.CardsState.Board.Lanes
            let originalTargetCard =
                match List.tryFind (fun (card: InactiveUnit) -> card.InactiveUnitID = InactiveUnitID originalTargetCardID) lane.InactiveUnits with
                | Some card -> InactiveUnit card
                | None ->
                    match List.tryFind (fun (card: ActiveUnit) -> card.ActiveUnitID = ActiveUnitID originalTargetCardID) lane.ActiveUnits with
                    | Some card -> ActiveUnit card
                    | None ->
                        lane.Pairs
                        |> List.collect (fun (c1, c2) -> [c1; c2])
                        |> List.find (fun (card: ActiveUnit) -> card.ActiveUnitID = ActiveUnitID originalTargetCardID)
                        |> ActiveUnit
            let activeTauntCheckTargets, nonActiveTauntCheckTargets =
                let inactiveTargets =
                    lane.InactiveUnits
                    |> List.filter (fun card -> card.Owner <> playerID && card.InactiveUnitID <> InactiveUnitID originalTargetCardID)
                let activeTauntTargets, activeNonTauntTargets =
                    lane.ActiveUnits
                    |> List.filter (fun card -> card.Owner <> playerID && card.ActiveUnitID <> ActiveUnitID originalTargetCardID && card.Power <> PassivePower Nimble)
                    |> List.partition (fun card -> card.Power = PassivePower Taunt)
                List.map (fun {ActiveUnitID = id} -> id) activeTauntTargets,
                ((List.map (fun {InactiveUnitID = InactiveUnitID id} -> UnitID id) inactiveTargets)
                @ (List.map (fun {ActiveUnitID = ActiveUnitID id} -> UnitID id) activeNonTauntTargets))
            let originalTargetIsActiveTaunt =
                match originalTargetCard with
                | InactiveUnit _ -> false
                | ActiveUnit {Power = p} -> p = PassivePower Taunt
            let legalTargets =
                if List.isEmpty activeTauntCheckTargets && not (originalTargetIsActiveTaunt)
                then
                    nonActiveTauntCheckTargets
                else
                    activeTauntCheckTargets |> List.map (fun (ActiveUnitID id) -> UnitID id)
            legalTargets
            |> List.map (fun (UnitID cardID) ->
                MidPassivePowerChoicePair (gs, TwinStrikeChoice (playerID, laneID, powerCardID, cardID))
            )
    | GameStateWon _
    | GameStateTied _ ->
        List.empty

let private executeMidActivationPowerChoice midPowerChoice (gameState: GameStateDuringMidActivationPowerChoice) =
    match midPowerChoice with
    | DiscardChoice (playerID, _, discardeeCardID) ->
        let cs = gameState.CardsState
        let (discardeeCard, cs) = removeCardFromHand (HandCardID discardeeCardID) playerID cs
        let convertedCard = handToDiscardedCard discardeeCard
        let newCardsState = {
            cs with
                Board = {cs.Board with Discard = cs.Board.Discard @ [convertedCard]}
            }
        changeMidActivationPowerCardsState gameState newCardsState
    | ForesightChoice (playerID, powerCardID, targetCardID) ->
        gameState.CardsState
        |> changeMidActivationPowerCardsState gameState
    | MoveChoice maybeMove ->
        match maybeMove with
        | Some (playerID, laneID, powerCardID, targetLaneID, targetCardID) ->
            gameState.CardsState
            |> changeCardLane (UnitID targetCardID) targetLaneID laneID
            |> changeMidActivationPowerCardsState gameState
        | None ->
            gameState
    |> removeMidActivationPowerChoiceContext

let private executeMidPassivePowerChoice midPowerChoice (gameState: GameStateDuringMidPassivePowerChoice) =
    match midPowerChoice with
    | TwinStrikeChoice (playerID, laneID, powerCardID, targetCardID) ->
        gameState.CardsState
        |> damageCard (UnitID targetCardID) 1<health> laneID
        |> changeMidPassivePowerCardsState gameState
        |> removeMidPassivePowerChoiceContext
        |> triggerTargetInactiveDeathPowers
        |> moveDeadCardsToDiscard

let private executePlayAction cardID laneID (gameState: GameStateDuringTurn) =
    let playerID = gameState.TurnState.CurrentPlayer
    let cardsState = gameState.CardsState
    let playedCard, newCardsState = removeCardFromHand cardID playerID cardsState
    let newCard = handToInactiveUnit playedCard
    newCardsState
    |> addCardToInactiveUnits newCard laneID
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
                        Some (hc @ [deckToHandCard playerID drawPile.Head])
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
                        Some (hc @ [deckToHandCard playerID drawPile.Head])
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
                    {card with Damage = max 0<health> (card.Damage - amount)}
                else
                    card
                )
        ActiveUnits =
            lane.ActiveUnits
            |> List.map (fun card ->
                if card.Owner = playerID then
                    {card with Damage = max 0<health> (card.Damage - amount)}
                else
                    card
                )
        Pairs =
            lane.Pairs
            |> List.map (fun (card1, card2) ->
                if card1.Owner = playerID then
                    {card1 with Damage = max 0<health> (card1.Damage - amount)},
                    {card2 with Damage = max 0<health> (card2.Damage - amount)}
                else
                    card1, card2
                )
        }
let private healOwnUnits playerID amount cardsState =
    let board = cardsState.Board
    let newLanes =
        board.Lanes
        |> Map.map (fun _ lane -> healOwnUnitsInLane playerID amount lane)
    {cardsState with Board = {board with Lanes = newLanes}}

let private freezeEnemyNonActiveNimbleUnitsInLane playerID laneID cardsState =
    let board = cardsState.Board
    let lane = Map.find laneID board.Lanes
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
    let newLane = {lane with InactiveUnits = newInactiveUnits; ActiveUnits = newActiveUnits; Pairs = newPairs}
    {cardsState with Board = {board with Lanes = board.Lanes |> Map.add laneID newLane}}

let private resolveActivationPower playerID laneID (ActiveUnitID cardID) (gameState: GameStateDuringTurn) =
    let cardsState = gameState.CardsState
    let {Board = board} = cardsState
    let lane = Map.find laneID board.Lanes
    let card = List.find (fun {ActiveUnitID = ActiveUnitID id} -> id = cardID) lane.ActiveUnits
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
        cardsState
        |> freezeEnemyNonActiveNimbleUnitsInLane playerID laneID
        |> changeCardsState gameState
        |> GameStateDuringTurn
    | ActivationPower Heal ->
        cardsState
        |> healOwnUnits playerID 2<health>
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
        gameState.CardsState
        |> setMaxCardActions cardID 2<action> laneID
        |> changeCardsState gameState
        |> incrementActionsLeft
        |> GameStateDuringTurn
    | InactiveDeathPower _
    | PassivePower _ ->
        gameState
        |> GameStateDuringTurn

let private resolveAttackerPassivePower playerID laneID attackerIDs (UnitID attackedCardID) (gameState: GameStateDuringTurn) =
    let cardsState = gameState.CardsState
    let lane = Map.find laneID cardsState.Board.Lanes
    let card =
        match attackerIDs with
        | SingleAttackerID (ActiveUnitID id) -> List.find (fun {ActiveUnitID = ActiveUnitID thisID} -> thisID = id) lane.ActiveUnits
        | PairAttackerIDs ((ActiveUnitID id), _) -> List.find (fun {ActiveUnitID = ActiveUnitID thisID} -> thisID = id) (lane.Pairs |> List.collect (fun (c1, c2) -> [c1; c2]))
    match card.Power with
    | PassivePower Retaliate
    | PassivePower Nimble
    | PassivePower Taunt ->
       gameState
       |> triggerTargetInactiveDeathPowers
       |> moveDeadCardsToDiscard
       |> GameStateDuringTurn
    | PassivePower TwinStrike ->
        let context = TwinStrikeChoiceContext (playerID, laneID, (transferAttackerIDs attackerIDs), attackedCardID)
        {
            CardsState = cardsState
            TurnState = gameState.TurnState
            ChoiceContext = context
        }
        |> GameStateDuringMidPassivePowerChoice
    | PassivePower Vampiric ->
        gameState
        |> triggerTargetInactiveDeathPowers
        |> moveDeadCardsToDiscard
        |> GameStateDuringTurn
    | ActivationPower _
    | InactiveDeathPower _ ->
        gameState
        |> triggerTargetInactiveDeathPowers
        |> moveDeadCardsToDiscard
        |> GameStateDuringTurn

let private executeActivateAction playerID laneID (InactiveUnitID cardID) (gameState: GameStateDuringTurn) =
    let cardsState = gameState.CardsState
    let removedCard, cs1 = removeCardFromInactiveUnits (InactiveUnitID cardID) laneID cardsState
    let newCard = inactiveToActiveUnit removedCard
    addCardToActiveUnits newCard laneID cs1
    |> changeCardsState gameState
    |> resolveActivationPower playerID laneID (ActiveUnitID cardID)

let private getBonusDefenderDamage attackerPower targetPower targetInfo =
    match attackerPower, targetPower, targetInfo with
    | PassivePower Nimble, PassivePower Taunt, ActiveSingleTarget _
    | PassivePower Nimble, PassivePower Taunt, ActivePairMemberTarget _ ->
        1<health>
    | _ ->
        0<health>

let private getTargetIDFromTargetInfo targetInfo =
    match targetInfo with
    | InactiveTarget (owner, id)
    | ActiveSingleTarget (owner, id)
    | ActivePairMemberTarget (owner, id) ->
        UnitID id

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
    let attackerPower =
        lane.ActiveUnits
        |> List.find (fun {ActiveUnitID = ActiveUnitID id} -> id = attackerID)
        |> (fun {Power = power} -> power)
    let targetList =
        (lane.InactiveUnits |> List.map InactiveUnit)
        @ (lane.ActiveUnits |> List.map ActiveUnit)
        @ (lane.Pairs |> List.collect (fun (card1, card2) -> [ActiveUnit card1; ActiveUnit card2]))
    let targetPower =
        targetList
        |> List.find (fun card ->
            match card with
            | InactiveUnit {InactiveUnitID = InactiveUnitID cid}
            | ActiveUnit {ActiveUnitID = ActiveUnitID cid} ->
                UnitID cid = targetID
            )
        |> (fun card ->
            match card with
            | InactiveUnit {Power = power}
            | ActiveUnit {Power = power} ->
                power
            )
    let baseDefenderDamage = 1<health>
    let bonusDefenderDamage = getBonusDefenderDamage attackerPower targetPower targetInfo
    let selfDamage = getAttackerSelfDamage attackerPower targetPower targetInfo
    targetID, baseDefenderDamage + bonusDefenderDamage, selfDamage

let private getPairAttackInfo pairMemberID targetInfo lane =
    let targetID = getTargetIDFromTargetInfo targetInfo
    let attackerPower =
        lane.ActiveUnits
        |> List.find (fun {ActiveUnitID = ActiveUnitID id} -> id = pairMemberID)
        |> (fun {Power = power} -> power)
    let targetList =
        (lane.InactiveUnits |> List.map InactiveUnit)
        @ (lane.ActiveUnits |> List.map ActiveUnit)
        @ (lane.Pairs |> List.collect (fun (card1, card2) -> [ActiveUnit card1; ActiveUnit card2]))
    let targetPower =
        targetList
        |> List.find (fun card ->
            match card with
            | InactiveUnit {InactiveUnitID = InactiveUnitID cid}
            | ActiveUnit {ActiveUnitID = ActiveUnitID cid} ->
                UnitID cid = targetID
            )
        |> (fun card ->
            match card with
            | InactiveUnit {Power = power}
            | ActiveUnit {Power = power} ->
                power
            )
    let baseDefenderDamage = 2<health>
    let bonusDefenderDamage = getBonusDefenderDamage attackerPower targetPower targetInfo
    let selfDamage = getAttackerSelfDamage attackerPower targetPower targetInfo
    targetID, baseDefenderDamage + bonusDefenderDamage, selfDamage

let private executeSingleAttackAction playerID laneID (ActiveUnitID attackerID) targetInfo (gameState: GameStateDuringTurn) =
    let cardsState = gameState.CardsState
    let board = cardsState.Board
    let targetID, damage, selfDamage =
        getSingleAttackInfo attackerID targetInfo (Map.find laneID board.Lanes)
    cardsState
    |> incrementCardActionsUsed attackerID laneID
    |> damageCard targetID damage laneID
    |> damageCard (UnitID attackerID) selfDamage laneID
    |> changeCardsState gameState
    |> resolveAttackerPassivePower playerID laneID (SingleAttackerID (ActiveUnitID attackerID)) targetID

let private executePairAttackAction playerID laneID (ActiveUnitID attackerID1, ActiveUnitID attackerID2) targetInfo (gameState: GameStateDuringTurn) =
    let cardsState = gameState.CardsState
    let board = cardsState.Board
    let targetID, damage, selfDamage =
        getPairAttackInfo attackerID1 targetInfo (Map.find laneID board.Lanes)
    cardsState
    |> incrementCardActionsUsed attackerID1 laneID
    |> incrementCardActionsUsed attackerID2 laneID
    |> damageCard targetID damage laneID
    |> damageCard (UnitID attackerID1) selfDamage laneID
    |> damageCard (UnitID attackerID2) selfDamage laneID
    |> changeCardsState gameState
    |> resolveAttackerPassivePower playerID laneID (PairAttackerIDs (ActiveUnitID attackerID1, ActiveUnitID attackerID2)) targetID

let private executeCreatePairAction laneID cardID1 cardID2 (gameState: GameStateDuringTurn) =
    let removedUnits, newCardsState =
        gameState.CardsState
        |> removeCardsFromActiveUnits [cardID1; cardID2] laneID
    let card1 = removedUnits.[0]
    let card2 = removedUnits.[1]
    newCardsState
    |> addCardsToPairs (card1, card2) laneID
    |> changeCardsState gameState

let private decrementActionsLeft (gameState: GameStateDuringTurn) =
    {gameState with
        TurnState = {
            gameState.TurnState with
                ActionsLeft = gameState.TurnState.ActionsLeft - 1<action>
            }
        }

let private executeTurnAction action gameState =
    let gs = decrementActionsLeft gameState
    let postAction =
        match action with
        | Play (playerID, cardID, laneID) ->
            executePlayAction (HandCardID cardID) laneID gs
            |> GameStateDuringTurn
        | Activate (playerID, laneID, cardID) ->
            executeActivateAction playerID laneID (InactiveUnitID cardID) gs
        | SingleAttack (playerID, laneID, attackerID, targetInfo) ->
            executeSingleAttackAction playerID laneID (ActiveUnitID attackerID) targetInfo gs
        | PairAttack (playerID, laneID, (attackerID1, attackerID2), targetInfo) ->
            executePairAttackAction playerID laneID (ActiveUnitID attackerID1, ActiveUnitID attackerID2) targetInfo gs
        | CreatePair (playerID, laneID, cardID1, cardID2) ->
            executeCreatePairAction laneID (ActiveUnitID cardID1) (ActiveUnitID cardID2) gs
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
let private timeoutOwnedFreezeStates playerID cardsState =
    let board = cardsState.Board
    let newLanes =
        board.Lanes
        |> Map.map (fun _ lane -> timeoutOwnedFreezeStatesInLane playerID lane)
    {cardsState with Board = {board with Lanes = newLanes}}

let private resetAllActiveCardActionsUsedInLane lane =
    {lane with ActiveUnits = lane.ActiveUnits |> List.map (fun card -> {card with ActionsSpent = 0<action>})}
let private resetAllActiveCardActionsUsed cardsState =
    let board = cardsState.Board
    let newLanes =
        board.Lanes
        |> Map.map (fun _ lane -> resetAllActiveCardActionsUsedInLane lane)
    {cardsState with Board = {board with Lanes = newLanes}}

let private resetMaxActions activeUnit =
    {activeUnit with MaxActions = 1<action>}
let private resetMaxActionsInLane lane =
    {lane with ActiveUnits = lane.ActiveUnits |> List.map resetMaxActions}
let private resetAllActiveMaxCardActions cardsState =
    let board = cardsState.Board
    let newLanes =
        board.Lanes
        |> Map.map (fun _ lane -> resetMaxActionsInLane lane)
    {cardsState with Board = {board with Lanes = newLanes}}

let rec private makeNextActionInfo actionPair =
    let newGameState, action =
        match actionPair with
        | MidActivationPowerChoicePair (gs, mapci) ->
            executeMidActivationPowerChoice mapci gs
            |> GameStateDuringTurn
            |> checkForGameEnd,
            MidActivationPowerChoiceInfo mapci
        | MidPassivePowerChoicePair (gs, mppci) ->
            executeMidPassivePowerChoice mppci gs
            |> GameStateDuringTurn
            |> checkForGameEnd,
            MidPassivePowerChoiceInfo mppci
        | StackChoicePair (gs, sci) ->
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
            },
            StackChoiceInfo sci
        | TurnActionChoicePair (gs, ActionChoiceInfo aci) ->
            executeTurnAction aci gs
            |> checkForGameEnd,
            TurnActionInfo (ActionChoiceInfo aci)
        | TurnActionChoicePair (gs, EndTurn et) ->
            let tip = gs.TurnState
            let nextPlayer =
                if int tip.CurrentPlayer = tip.NPlayers then
                    1<PID>
                else
                    tip.CurrentPlayer + 1<PID>
            let actions, nextFutureActionCounts =
                match tip.FutureActionCounts with
                | [] -> 3<action>, []
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
                },
            TurnActionInfo (EndTurn et)
        | StartTurnPair (gs, id) ->
            gs
            |> startPlayerTurn id
            |> tryDrawCard id
            |> GameStateDuringTurn,
            StartTurn id
    let possibleActionPairs = getPossibleActionPairs newGameState
    let checkedGameState, checkedPossibleActionPairs =
        match newGameState, possibleActionPairs with
        | GameStateDuringMidActivationPowerChoice gs, [] ->
            let state =
                removeMidActivationPowerChoiceContext gs
                |> triggerTargetInactiveDeathPowers
                |> moveDeadCardsToDiscard
                |> GameStateDuringTurn
            state, getPossibleActionPairs state
        | GameStateDuringMidPassivePowerChoice gs, [] ->
            let state =
                removeMidPassivePowerChoiceContext gs
                |> triggerTargetInactiveDeathPowers
                |> moveDeadCardsToDiscard
                |> GameStateDuringTurn
            state, getPossibleActionPairs state
        | _ ->
            newGameState, possibleActionPairs
    let newDisplayInfo = getDisplayInfo checkedGameState
    // Generation of next action's resulting capabilities is part of the
    // generated capability's body, since assigning them here requires
    // generating the entire game space, blowing the stack
    let capability() =
        InProgress (
            newDisplayInfo,
            checkedPossibleActionPairs
            |> List.map makeNextActionInfo
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
        |> List.map (fun id -> {DeckCardID = DeckCardID id; Power = Map.find id cardPowers})
        |> NonEmptyList.fromList
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
                            BaseCardID = BaseCardID cardID
                            Power = Map.find cardID cardPowers
                            Owner = ownerID
                            KnownBy = Set.empty
                        }
                        )
                    )
                DrawPile = drawPile
                HandCards =
                    handCards
                    |> Map.map (fun playerID cardIDs ->
                        cardIDs
                        |> List.map (fun cardID -> {HandCardID = HandCardID cardID; Power = Map.find cardID cardPowers; Owner = playerID})
                        )
            }
            Removed = Set.map (fun id -> ({RemovedCardID = RemovedCardID id; Power = Map.find id cardPowers}: RemovedCard)) removed
            }
        TurnState = {
            Player = 1<PID>
            NPlayers = nPlayers
            Actions = 2<action>
            FutureActionCounts = List.empty
            }
        }
    let displayInfo = getDisplayInfo gameState
    let nextActions =
        getPossibleActionPairs gameState
        |> List.map makeNextActionInfo
    InProgress (displayInfo, nextActions)

let api = {
    NewGame = fun () -> createGame 2 3
}
