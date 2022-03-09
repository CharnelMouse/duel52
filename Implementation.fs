module Implementation
open Domain
open NonEmptyList
open NonEmptyMap
open EventStack
open PowerMaps

type private NPlayers = NPlayers of uint
type private NLanes = NLanes of uint

type private RemovedCardID = RemovedCardID of CardID
type private DeckCardID = DeckCardID of CardID
type private HandCardID = HandCardID of CardID
type private BaseCardID = BaseCardID of CardID
type private InactiveUnitID = InactiveUnitID of CardID
type private ActiveUnitID = ActiveUnitID of CardID
type private PairedUnitID = PairedUnitID of CardID
type private FullPairedUnitID = FullPairedUnitID of CardID
type private UnitID = UnitID of CardID
type private DiscardedCardID = DiscardedCardID of CardID
type private ForesightTargetID = ForesightTargetID of CardID
type private AttackerIDs =
| SingleAttackerID of ActiveUnitID
| PairAttackerIDs of PairedUnitID * PairedUnitID

type private RemovedCard = {
    RemovedCardID: RemovedCardID
    Rank: Rank
    Suit: Suit
}
type private DeckCard = {
    DeckCardID: DeckCardID
    Rank: Rank
    Suit: Suit
}
type private HandCard = {
    HandCardID: HandCardID
    Rank: Rank
    Suit: Suit
    Abilities: Abilities
    Owner: PlayerID
}
type private BaseCard = {
    BaseCardID: BaseCardID
    Rank: Rank
    Suit: Suit
    Abilities: Abilities
    Owner: PlayerID
    KnownBy: PlayerID Set
}

type private FreezeStatus =
| FrozenBy of PlayerID
| NotFrozen

type private InactiveUnit = {
    InactiveUnitID: InactiveUnitID
    Rank: Rank
    Suit: Suit
    Abilities: Abilities
    Owner: PlayerID
    KnownBy: PlayerID Set
    Damage: Damage
    FreezeStatus: FreezeStatus
}
type private ActiveUnit = {
    ActiveUnitID: ActiveUnitID
    Rank: Rank
    Suit: Suit
    Abilities: Abilities
    Owner: PlayerID
    Damage: Damage
    ActionsSpent: Actions
    MaxActions: Actions
    FreezeStatus: FreezeStatus
}
type private PairedUnit = {
    PairedUnitID: PairedUnitID
    Suit: Suit
    Damage: Damage
    ActionsSpent: Actions
    MaxActions: Actions
    FreezeStatus: FreezeStatus
}
type private FullPairedUnit = {
    FullPairedUnitID: FullPairedUnitID
    Rank: Rank
    Suit: Suit
    Abilities: Abilities
    Owner: PlayerID
    Damage: Damage
    ActionsSpent: Actions
    MaxActions: Actions
    FreezeStatus: FreezeStatus
}
type private ActiveCard =
| Solo of ActiveUnit
| Paired of FullPairedUnit
type private UnitCard =
| InactiveUnit of InactiveUnit
| ActiveUnit of ActiveUnit
| PairedUnit of FullPairedUnit
type private Pair = {
    Cards: PairedUnit * PairedUnit
    Rank: Rank
    Abilities: Abilities
    Owner: PlayerID
}
type private FaceDownDiscardedCard = {
    DiscardedCardID: DiscardedCardID
    Rank: Rank
    Suit: Suit
    KnownBy: PlayerID Set
}
type private FaceUpDiscardedCard = {
    DiscardedCardID: DiscardedCardID
    Rank: Rank
    Suit: Suit
}
type private DiscardedCard =
| FaceDownDiscardedCard of FaceDownDiscardedCard
| FaceUpDiscardedCard of FaceUpDiscardedCard
type private CardConverter<'From, 'To> = 'From -> 'To

type private Lane = {
    InactiveUnits: InactiveUnit list
    ActiveUnits: ActiveUnit list
    Pairs: Pair list
}
type private LaneControl =
| Contested
| Empty
| Won of PlayerID

type private Board = {
    Lanes: Map<LaneID, Lane>
    Discard: DiscardedCard list
}

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
    GetAbilities: GetAbilities
    GameStage: GameStage
    Removed: RemovedCard Set
}

type private CardRemover<'T, 'TID> = 'TID -> LaneID -> CardsState -> 'T * CardsState
type private CardPairRemover<'T, 'TID> = 'TID -> 'TID -> LaneID -> CardsState -> 'T * 'T * CardsState
type private CardsRemover<'T, 'TID> = 'TID list -> LaneID -> CardsState -> 'T list * CardsState
type private CardAdder<'T> = 'T -> LaneID -> CardsState -> CardsState
type private CardsAdder<'T> = 'T list -> LaneID -> CardsState -> CardsState

type private PlayerReady = {
    Player: PlayerID
    NPlayers: NPlayers
    Actions: Actions
    FutureActionCounts: Actions list
}
type private TurnInProgress = {
    CurrentPlayer: PlayerID
    NPlayers: NPlayers
    ActionsLeft: Actions
    FutureActionCounts: Actions list
}

type private AbilityEvent = InstantNonTargetAbility * LaneID * CardID
type private ResolutionEpoch =
| OrderChoiceEpoch of PowerContext epoch
| OrderedAbilityEpoch of AbilityEvent epoch
| AbilityChoiceEpoch of AbilityChoiceContext
type private ResolutionStack = ResolutionEpoch nonEmptyList

type private AbilityChoice = {
    ChoiceContext: AbilityChoiceContext
    ResolutionStack: ResolutionStack option
}
type private StackChoice = {
    EpochEvents: NonEmptyMap<EventID, PowerContext>
    ResolutionStack: ResolutionStack option
}
type private TurnStage =
| AbilityChoice of AbilityChoice
| StackChoice of StackChoice
| ActionChoice

type private GameStateBetweenTurns = {
    CardsState: CardsState
    TurnState: PlayerReady
}
type private GameStateDuringTurn = {
    CardsState: CardsState
    TurnState: TurnInProgress
    TurnStage: TurnStage
}
type private GameStateWon = {
    EndLanes: Map<LaneID, Lane>
    Winner: PlayerID
    LaneWins: Map<LaneID, PlayerID>
}
type private GameStateTied = {
    EndLanes: Map<LaneID, Lane>
    LaneWins: Map<LaneID, PlayerID>
}
type private GameState =
| GameStateDuringTurn of GameStateDuringTurn
| GameStateBetweenTurns of GameStateBetweenTurns
| GameStateWon of GameStateWon
| GameStateTied of GameStateTied

type private ActionPair =
| AbilityChoicePair of CardsState * TurnInProgress * ResolutionStack option * AbilityChoiceInfo
| StackChoicePair of CardsState * TurnInProgress * StackChoice * StackChoiceInfo
| TurnActionChoicePair of CardsState * TurnInProgress * TurnActionInfo
| StartTurnPair of CardsState * PlayerReady

type private ExecuteStartTurn = CardsState -> PlayerReady -> GameStateDuringTurn
type private ExecuteEndTurn = CardsState -> TurnInProgress -> GameStateBetweenTurns
type private ExecuteTurnAction = ActionChoiceInfo -> CardsState -> TurnInProgress -> GameStateDuringTurn
type private ExecuteAbilityChoice = AbilityChoiceInfo -> CardsState -> TurnInProgress -> ResolutionStack option -> GameStateDuringTurn
type private ExecuteStackChoice = CardsState -> TurnInProgress -> StackChoice -> EventID -> GameStateDuringTurn
// Game state and ActionInfo go into action pair, just for ActionInfo to come out again at execution, seems silly
type private CreateGame = NPlayers -> NLanes -> ActionResult
type private GetPossibleActionPairs = GameState -> ActionPair list
type private ExecuteAction = ActionPair -> GameState * ActionInfo
type private GetInProgress = GameState -> ActionResult
type private GameStateToDisplayInfo = GameState -> DisplayInfo
type private CreateUIOutput = GetInProgress -> ActionPair -> CapabilityInfo<ActionInfo, ActionResult>

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

let private uSubtract amount n =
    n - min n amount
let private twoToList (x, y) = [x; y]

let private transferAttackerIDs attackerIDs =
    match attackerIDs with
    | SingleAttackerID (ActiveUnitID id) -> SingleCardID id
    | PairAttackerIDs (PairedUnitID id1, PairedUnitID id2) -> PairIDs (id1, id2)

let private getHandCardInfo {HandCardID = HandCardID id; Rank = r; Suit = s; Abilities = a} =
    HandCardInfo (id, r, s, a.Name)

let private getBaseKnowledge playerID (baseCard: BaseCard) =
    if Set.contains playerID baseCard.KnownBy then
        KnownBaseCard (baseCard.Owner, baseCard.Rank, baseCard.Suit, baseCard.Abilities.Name)
    else
        UnknownBaseCard baseCard.Owner

let private (|Exhausted|Ready|) (card: ActiveCard) =
    match card with
    | Solo {ActionsSpent = spent; MaxActions = m}
    | Paired {ActionsSpent = spent; MaxActions = m} ->
        if spent >= m then
            Exhausted
        else
            Ready

let private maxHealth card =
    match card with
    | InactiveUnit _ -> 2u<health>
    | ActiveUnit {Abilities = {WhileActive = passiveAbilities}}
    | PairedUnit {Abilities = {WhileActive = passiveAbilities}} ->
        passiveAbilities
        |> List.fold (fun state passiveAbility ->
            match passiveAbility with
            | MaxHealthIncrease increase ->
                state + (uint increase)*1u<health>
            | _ -> state
        ) 2u<health>
let private (|Dead|Alive|) (card: UnitCard) =
    let damage =
        match card with
        | InactiveUnit {Damage = damage}
        | ActiveUnit {Damage = damage}
        | PairedUnit {Damage = damage} -> damage
    if damage < maxHealth card then
        Alive
    else
        Dead
let private isDead (card: UnitCard) =
    match card with
    | Alive -> false
    | Dead -> true

let private deckToHandCard (getAbilities: GetAbilities) playerID : CardConverter<DeckCard, HandCard> = fun deckCard ->
    let {DeckCardID = DeckCardID id} = deckCard
    {
        HandCardID = HandCardID id
        Rank = deckCard.Rank
        Suit = deckCard.Suit
        Abilities = getAbilities deckCard.Rank
        Owner = playerID
    }
let private handToDiscardedCard: CardConverter<HandCard, DiscardedCard> = fun handCard ->
    let {HandCardID = HandCardID id} = handCard
    FaceDownDiscardedCard {
        DiscardedCardID = DiscardedCardID id
        Rank = handCard.Rank
        Suit = handCard.Suit
        KnownBy = Set.singleton handCard.Owner
    }
let private handToInactiveUnit: CardConverter<HandCard, InactiveUnit> = fun handCard ->
    let {HandCardID = HandCardID id} = handCard
    {
        InactiveUnitID = InactiveUnitID id
        Rank = handCard.Rank
        Suit = handCard.Suit
        Abilities = handCard.Abilities
        Owner = handCard.Owner
        KnownBy = Set.singleton handCard.Owner
        Damage = 0u<health>
        FreezeStatus = NotFrozen
    }
let private inactiveToActiveUnit: CardConverter<InactiveUnit, ActiveUnit> = fun inactiveUnit ->
    let {InactiveUnitID = InactiveUnitID id} = inactiveUnit
    {
        ActiveUnitID = ActiveUnitID id
        Rank = inactiveUnit.Rank
        Suit = inactiveUnit.Suit
        Abilities = inactiveUnit.Abilities
        Owner = inactiveUnit.Owner
        Damage = inactiveUnit.Damage
        ActionsSpent = 0u<action>
        MaxActions = 1u<action>
        FreezeStatus = inactiveUnit.FreezeStatus
    }
let private unitToDiscardedCard: CardConverter<UnitCard, DiscardedCard> = fun unitCard ->
    match unitCard with
    | InactiveUnit {InactiveUnitID = InactiveUnitID id; Owner = pid; Rank = rank; Suit = suit; KnownBy = knownBy} ->
        FaceDownDiscardedCard {
            DiscardedCardID = DiscardedCardID id
            Rank = rank
            Suit = suit
            KnownBy = Set.add pid knownBy // player checks unknown base in case it's a Trap
        }
    | ActiveUnit {ActiveUnitID = ActiveUnitID id; Rank = rank; Suit = suit} ->
        FaceUpDiscardedCard {
            DiscardedCardID = DiscardedCardID id
            Rank = rank
            Suit = suit
        }
    | PairedUnit {FullPairedUnitID = FullPairedUnitID id; Suit = suit; Rank = rank} ->
        FaceUpDiscardedCard {
            DiscardedCardID = DiscardedCardID id
            Rank = rank
            Suit = suit
        }
let private pairToFullPairedUnits: CardConverter<Pair, FullPairedUnit * FullPairedUnit> = fun {Cards = card1, card2; Rank = rank; Abilities = abilities; Owner = owner} ->
    let {PairedUnitID = PairedUnitID cardID1} = card1
    let {PairedUnitID = PairedUnitID cardID2} = card2
    {
        FullPairedUnitID = FullPairedUnitID cardID1
        Suit = card1.Suit
        Damage = card1.Damage
        ActionsSpent = card1.ActionsSpent
        MaxActions = card1.MaxActions
        FreezeStatus = card1.FreezeStatus
        Rank = rank
        Abilities = abilities
        Owner = owner
    },
    {
        FullPairedUnitID = FullPairedUnitID cardID2
        Suit = card2.Suit
        Damage = card2.Damage
        ActionsSpent = card2.ActionsSpent
        MaxActions = card2.MaxActions
        FreezeStatus = card2.FreezeStatus
        Rank = rank
        Abilities = abilities
        Owner = owner
    }
let private activeToPairedUnit: CardConverter<ActiveUnit, PairedUnit> = fun card ->
    let {ActiveUnitID = ActiveUnitID cardID} = card
    {
        PairedUnitID = PairedUnitID cardID
        Suit = card.Suit
        Damage = card.Damage
        ActionsSpent = card.ActionsSpent
        MaxActions = card.MaxActions
        FreezeStatus = card.FreezeStatus
    }
let private activeToFullPairedUnit: CardConverter<ActiveUnit, FullPairedUnit> = fun card ->
    let {ActiveUnitID = ActiveUnitID cardID} = card
    {
        FullPairedUnitID = FullPairedUnitID cardID
        Suit = card.Suit
        Damage = card.Damage
        ActionsSpent = card.ActionsSpent
        MaxActions = card.MaxActions
        FreezeStatus = card.FreezeStatus
        Rank = card.Rank
        Abilities = card.Abilities
        Owner = card.Owner
    }
let private fullPairedToActiveUnit: CardConverter<FullPairedUnit, ActiveUnit> = fun card ->
    let {FullPairedUnitID = FullPairedUnitID cardID} = card
    {
        ActiveUnitID = ActiveUnitID cardID
        Rank = card.Rank
        Suit = card.Suit
        Abilities = card.Abilities
        Owner = card.Owner
        Damage = card.Damage
        ActionsSpent = card.ActionsSpent
        MaxActions = card.MaxActions
        FreezeStatus = card.FreezeStatus
    }
let private activeUnitsToPair: CardConverter<ActiveUnit * ActiveUnit, Pair> = fun (card1, card2) ->
    if (card1.Rank, card1.Abilities, card1.Owner) <> (card2.Rank, card2.Abilities, card2.Owner) then
        failwith "Paired cards don't share rank / abilities / owner"
    {
        Cards = activeToPairedUnit card1, activeToPairedUnit card2
        Rank = card1.Rank
        Abilities = card1.Abilities
        Owner = card1.Owner
    }
let private pairedToActiveUnit: CardConverter<Rank * Abilities * PlayerID * PairedUnit, ActiveUnit> = fun (rank, abilities, owner, card) ->
    let {PairedUnitID = PairedUnitID cardID} = card
    {
        ActiveUnitID = ActiveUnitID cardID
        Rank = rank
        Suit = card.Suit
        Abilities = abilities
        Owner = owner
        Damage = card.Damage
        ActionsSpent = card.ActionsSpent
        MaxActions = card.MaxActions
        FreezeStatus = card.FreezeStatus
    }

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
    let playerCounts =
        cardOwners
        |> List.countBy id
    match playerCounts with
    | [] -> Empty
    | [(controller, _)] -> Won controller
    | _ -> Contested

let private findDeadCardsInLane laneID board =
    let lane = Map.find laneID board.Lanes
    (lane.InactiveUnits |> List.map InactiveUnit |> List.filter isDead)
    @ (lane.ActiveUnits |> List.map ActiveUnit |> List.filter isDead)
    @ (lane.Pairs |> List.collect (pairToFullPairedUnits >> twoToList) |> List.map PairedUnit |> List.filter isDead)
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
    @ (lane.Pairs |> List.collect (pairToFullPairedUnits >> twoToList) |> List.choose (fun card ->
        let {FullPairedUnitID = FullPairedUnitID id} = card
        if isDead (PairedUnit card) then Some (UnitID id) else None
        ))

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
let private removeCardPairFromActiveUnits: CardPairRemover<ActiveUnit, ActiveUnitID> = fun cardID1 cardID2 laneID cardsState ->
    let board = cardsState.Board
    let lane = Map.find laneID board.Lanes
    let card1, inter =
        lane.ActiveUnits
        |> List.partition (fun {ActiveUnitID = id} -> id = cardID1)
    let card2, newActiveUnits =
        inter
        |> List.partition (fun {ActiveUnitID = id} -> id = cardID2)
    let newLane = {lane with ActiveUnits = newActiveUnits}
    card1.Head, card2.Head,
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
let private removeCardsFromPairs: CardsRemover<FullPairedUnit, PairedUnitID> = fun cardIDs laneID cardsState ->
    let board = cardsState.Board
    let lane = Map.find laneID board.Lanes
    let removed, newPairs =
        lane.Pairs
        |> List.partition (fun {Cards = {PairedUnitID = id1}, {PairedUnitID = id2}} ->
            List.contains id1 cardIDs || List.contains id2 cardIDs
            )
    let flatRemoved = List.collect (pairToFullPairedUnits >> twoToList) removed
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
    let newLane = {lane with Pairs = lane.Pairs @ [activeUnitsToPair (card1, card2)]}
    {cardsState with Board = {board with Lanes = board.Lanes |> Map.add laneID newLane}}
let private addCardPairPartnersToActiveUnits: CardsAdder<PairedUnitID> = fun cardIDs laneID cardsState ->
    let board = cardsState.Board
    let lane = Map.find laneID board.Lanes
    let partners =
        lane.Pairs
        |> List.collect (fun {Cards = card1, card2; Rank = rank; Abilities = abilities; Owner = owner} ->
            let {PairedUnitID = id1} = card1
            let {PairedUnitID = id2} = card2
            match List.contains id1 cardIDs, List.contains id2 cardIDs with
            | true, true ->
                []
            | true, false ->
                [pairedToActiveUnit (rank, abilities, owner, card2)]
            | false, true ->
                [pairedToActiveUnit (rank, abilities, owner, card1)]
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
        |> addCardPairPartnersToActiveUnits (cardIDs |> List.map (fun (UnitID id) -> PairedUnitID id)) laneID
    let removedPaired, cs4 =
        cs3
        |> removeCardsFromPairs (cardIDs |> List.map (fun (UnitID id) -> PairedUnitID id)) laneID
    let removed =
        (List.map InactiveUnit removedInactive)
        @ (List.map ActiveUnit removedActive)
        @ (List.map PairedUnit removedPaired)
    removed, cs4
let private changeCardLane cardID fromLaneID toLaneID cardsState =
    let moved, cs1 =
        cardsState
        |> removeCardsFromLane [cardID] fromLaneID
    let movedInactive =
        moved
        |> List.choose (function
            | InactiveUnit c -> Some c
            | ActiveUnit _
            | PairedUnit _ -> None
            )
    let movedActive =
        moved
        |> List.choose (function
            | InactiveUnit _ -> None
            | ActiveUnit c -> Some c
            | PairedUnit c -> Some (fullPairedToActiveUnit c)
            )
    cs1
    |> addCardsToInactiveUnits movedInactive toLaneID
    |> addCardsToActiveUnits movedActive toLaneID

let private healInactiveCard amount (inactiveCard: InactiveUnit) =
    {inactiveCard with Damage = uSubtract amount inactiveCard.Damage}
let private healActiveCard amount (activeCard: ActiveUnit) =
    {activeCard with Damage = uSubtract amount activeCard.Damage}
let private healPairedCard amount (pairedCard: PairedUnit) =
    {pairedCard with Damage = uSubtract amount pairedCard.Damage}
let private fullyHealActiveCard (activeCard: ActiveUnit) =
    {activeCard with Damage = 0u<health>}

let private flipAndActivateInactiveDeathPowersInLane laneID zeroHealthInactiveDeathPowerUnits cardsState =
    let inactiveIDs =
        zeroHealthInactiveDeathPowerUnits
        |> List.choose (fun card ->
            match card with
            | InactiveUnit {InactiveUnitID = cid} -> Some cid
            | ActiveUnit _
            | PairedUnit _ -> None
            )
    let removedCards, cs1 =
        cardsState
        |> removeCardsFromInactiveUnits inactiveIDs laneID
    let newCards =
        removedCards
        |> List.map (inactiveToActiveUnit >> fullyHealActiveCard)
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
                |> List.map (fun pair ->
                    let {Cards = card1, card2} = pair
                    let {PairedUnitID = PairedUnitID id1} = card1
                    let {PairedUnitID = PairedUnitID id2} = card2
                    let newCards =
                        (if id1 = cardID then {card1 with Damage = card1.Damage + damage} else card1),
                        (if id2 = cardID then {card2 with Damage = card2.Damage + damage} else card2)
                    {pair with Cards = newCards}
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
                        {card with ActionsSpent = card.ActionsSpent + 1u<action>}
                    else
                        card
                    )
            Pairs =
                lane.Pairs
                |> List.map (fun pair ->
                    let (card1, card2) = pair.Cards
                    let {PairedUnitID = PairedUnitID id1} = card1
                    let {PairedUnitID = PairedUnitID id2} = card2
                    let newCards =
                        if id1 = cardID then
                            {card1 with ActionsSpent = card1.ActionsSpent + 1u<action>}, card2
                        elif id2 = cardID then
                            card1, {card2 with ActionsSpent = card2.ActionsSpent + 1u<action>}
                        else
                            card1, card2
                    {pair with Cards = newCards}
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
                |> List.map (fun pair ->
                    let (card1, card2) = pair.Cards
                    let {PairedUnitID = PairedUnitID id1} = card1
                    let {PairedUnitID = PairedUnitID id2} = card2
                    let newCards =
                        if id1 = cardID then
                            {card1 with MaxActions = left}, card2
                        elif id2 = cardID then
                            card1, {card2 with MaxActions = left}
                        else
                            card1, card2
                    {pair with Cards = newCards}
                    )
            }
    {cardsState with Board = {board with Lanes = board.Lanes |> Map.add laneID newLane}}

let private makeCardKnown cardID playerID cardsState =
    let board = cardsState.Board
    let newLane _ lane = {
        lane with
            InactiveUnits =
                lane.InactiveUnits
                |> List.map (fun card ->
                    let {InactiveUnitID = InactiveUnitID id} = card
                    if id = cardID then
                        {card with KnownBy = card.KnownBy |> Set.add playerID}
                    else
                        card
                    )
            }
    let lanesUpdated = {cardsState with Board = {board with Lanes = board.Lanes |> Map.map newLane}}
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
                Rank = baseCard.Rank
                Suit = baseCard.Suit
                Abilities = baseCard.Abilities
                Owner = baseCard.Owner
                KnownBy = baseCard.KnownBy
                Damage = 0u<health>
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

let private incrementActionsLeft (gameState: GameStateDuringTurn) =
    {gameState with
        TurnState = {
            gameState.TurnState with
                ActionsLeft = gameState.TurnState.ActionsLeft + 1u<action>
        }
    }

let private changeCardsState (gameState: GameStateDuringTurn) newCardsState =
    {gameState with CardsState = newCardsState}
let private addMidPowerChoiceContext context gameState =
    {
        CardsState = gameState.CardsState
        TurnState = gameState.TurnState
        TurnStage = AbilityChoice {
            ChoiceContext = context
            ResolutionStack = None
        }
    }

let private flipInactiveCardsInLaneAndAddActivationPowersToStack playerID laneID cardsState turnState =
    let lane = Map.find laneID cardsState.Board.Lanes
    let flippingIDs =
        lane.InactiveUnits
        |> List.choose (fun card -> if card.Owner = playerID then Some card.InactiveUnitID else None)
    let removedCards, cs1 =
        cardsState
        |> removeCardsFromInactiveUnits flippingIDs laneID
    let flippedCards =
        removedCards
        |> List.map inactiveToActiveUnit
    let cs2 =
        cs1
        |> addCardsToActiveUnits flippedCards laneID
    let powerChoices =
        flippedCards
        |> List.choose (fun {ActiveUnitID = ActiveUnitID id; Abilities = abilities} ->
            match abilities.OnActivation with
            | [] -> None
            | _ -> Some (PowerContext (laneID, id, abilities.Name))
        )
        |> tryFromList
    match powerChoices with
    | None ->
        cs2, turnState, None
    | Some pc -> cs2, turnState, Some (OrderChoiceEpoch pc)

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
    |> List.fold (fun cs laneID -> flipAndActivateInactiveDeathPowersInLane laneID zeroHealthInactiveDeathPowerUnits cs) cardsState
    |> changeCardsState gameState
let private decrementInactiveCardDamage (card: InactiveUnit) =
    {card with Damage = uSubtract 1u<health> card.Damage}
let private decrementActiveCardDamage (card: ActiveUnit) =
    {card with Damage = uSubtract 1u<health> card.Damage}
let private decrementPairedCardDamage (card: PairedUnit) =
    {card with Damage = uSubtract 1u<health> card.Damage}
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
let private moveDeadCardsToDiscard gameState =
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
                |> List.countBy (fun (laneID, playerID) -> playerID)
            match wonLaneCounts with
            | [] -> GameStateDuringTurn gameState
            | lst ->
                let (leadingPlayer, leadingWins) =
                    lst
                    |> List.maxBy (fun (_, n) -> n)
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

let private prepareHead fn n lst =
    let h, t = List.splitAt (int n) lst
    fn h, t

let private prepareBases getAbilities (NLanes nLanes) lst =
    lst
    |> List.splitInto (int nLanes)
    |> createIDMap 1u<LID>
    |> Map.map (fun _ cardInfo ->
        cardInfo
        |> zipIDs 1u<PID>
        |> List.map (fun (ownerID, (cardID, rank, suit)) ->
            {
                BaseCardID = BaseCardID cardID
                Rank = rank
                Suit = suit
                Abilities = getAbilities rank
                Owner = ownerID
                KnownBy = Set.empty
            }
        )
    )

let private prepareHands getAbilities (NPlayers nPlayers) lst =
    let playerIDs =
        createIDsToLength 1u<PID> nPlayers
        |> List.collect (List.replicate 5)
    List.zip playerIDs lst
    |> List.groupBy (fun (playerID, _) -> playerID)
    |> List.map (fun (playerID, lst) ->
        playerID,
        lst
        |> List.map (fun (playerID, (cardID, rank, suit)) -> {
                HandCardID = HandCardID cardID
                Owner = playerID
                Rank = rank
                Suit = suit
                Abilities = getAbilities rank
            })
        )
    |> Map.ofList

let private prepareRemoved lst =
    lst
    |> List.map (fun (cardID, rank, suit) -> {
        RemovedCardID = (RemovedCardID cardID)
        Rank = rank
        Suit = suit
        })
    |> Set.ofList

let private getActionability card =
    match card with
    | InactiveUnit {FreezeStatus = fs}
    | ActiveUnit {FreezeStatus = fs}
    | PairedUnit {FreezeStatus = fs} ->
        match fs with
        | FrozenBy _ -> Frozen
        | NotFrozen -> Normal

let private getPairKnowledge pair : PairKnowledge =
    let (card1, card2) = pair.Cards
    let {PairedUnitID = PairedUnitID id1} = card1
    let {PairedUnitID = PairedUnitID id2} = card2
    let (fullCard1, fullCard2) = pairToFullPairedUnits pair
    let actionability1 = getActionability (PairedUnit fullCard1)
    let actionability2 = getActionability (PairedUnit fullCard2)
    id1, id2,
    pair.Rank,
    card1.Suit, card2.Suit,
    pair.Abilities.Name,
    card1.Damage, card2.Damage,
    min (card1.MaxActions - card1.ActionsSpent) (card2.MaxActions - card2.ActionsSpent),
    actionability1, actionability2

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

let private getDeadCardKnowledge (playerID: PlayerID) (card: DiscardedCard) =
    match card with
    | FaceDownDiscardedCard {Rank = r; Suit = s; KnownBy = kb} ->
        if Set.contains playerID kb then
            KnownFaceDownDeadCard (r, s)
        else
            UnknownDeadCard
    | FaceUpDiscardedCard {Rank = r; Suit = s} ->
        KnownFaceUpDeadCard (r, s)

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
        let ownTroops =
            lane.InactiveUnits
            |> List.filter (fun card -> card.Owner = playerID)
        ownTroops
        |> List.filter (fun card -> card.FreezeStatus = NotFrozen)
        |> List.map (fun card ->
            let {InactiveUnitID = InactiveUnitID id} = card
            Activate (laneID, id)
            )
        )

let private getPairActionsInfoFromUnits laneID (ownActiveUnits: ActiveUnit list) =
    let rec distPairs lst =
        match lst with
        | [] -> []
        | [_] -> []
        | h::t -> List.allPairs [h] t @ distPairs t
    ownActiveUnits
    |> distPairs
    |> List.choose (fun (card1, card2) ->
        if card1.Rank = card2.Rank then
            let {ActiveUnitID = ActiveUnitID id1} = card1
            let {ActiveUnitID = ActiveUnitID id2} = card2
            CreatePair (laneID, id1, id2)
            |> Some
        else
            None
        )

let private getAttackActionsInfoInLane playerID (laneID, lane) =
    let findOwner toList x =
        let first = toList >> List.head
        match (first x) with
        | InactiveUnit {Owner = owner}
        | ActiveUnit {Owner = owner}
        | PairedUnit {Owner = owner} -> owner

    let possibleAttackers toList lst =
        lst
        |> List.filter (fun attacker ->
            attacker
            |> toList
            |> List.forall (fun (attackerCard: ActiveCard) ->
                let owner =
                    match attackerCard with
                    | Solo {Owner = o} -> o
                    | Paired {Owner = o} -> o
                owner = playerID
                && match attackerCard with
                    | Ready ->
                        match attackerCard with
                        | Solo {FreezeStatus = fs}
                        | Paired {FreezeStatus = fs} ->
                            fs = NotFrozen
                    | Exhausted ->
                        false
                    )
            )
    let possibleUnitAttackers = possibleAttackers (Solo >> List.singleton) lane.ActiveUnits
    let possiblePairAttackers = possibleAttackers (pairToFullPairedUnits >> twoToList >> List.map Paired) lane.Pairs

    let addTypeAndOwner T owner (x: UnitCard) =
        let id =
            match x with
            | InactiveUnit {InactiveUnitID = InactiveUnitID cid}
            | ActiveUnit {ActiveUnitID = ActiveUnitID cid}
            | PairedUnit {FullPairedUnitID = FullPairedUnitID cid} -> cid
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
    let possiblePairTypeTargets T pairIDs = possibleTypeTargets twoToList T pairIDs
    let possibleInactiveUnitTargets = possibleSingleTypeTargets InactiveTarget (lane.InactiveUnits |> List.map InactiveUnit)
    let possibleActiveUnitTargets = possibleSingleTypeTargets ActiveSingleTarget (lane.ActiveUnits |> List.map ActiveUnit)
    let possiblePairTargets = possiblePairTypeTargets ActivePairMemberTarget (lane.Pairs |> List.map (pairToFullPairedUnits >> fun (c1, c2) -> PairedUnit c1, PairedUnit c2))
    let allTargets = possibleInactiveUnitTargets @ possibleActiveUnitTargets @ possiblePairTargets

    let tauntTargets =
        allTargets
        |> List.filter (fun target ->
            match target with
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
                    |> List.unzip
                    |> (fun (lst1, lst2) -> lst1 @ lst2)
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
        |> List.collect (fun attacker ->
            let {ActiveUnitID = ActiveUnitID attackerID} = attacker
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
        |> moveDeadCardsToDiscard
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
 
let private executePlayAction cardID laneID cardsState turnState =
    let gameState = {CardsState = cardsState; TurnState = turnState; TurnStage = ActionChoice}
    let playerID = gameState.TurnState.CurrentPlayer
    let cardsState = gameState.CardsState
    let playedCard, newCardsState = removeCardFromHand cardID playerID cardsState
    let newCard = handToInactiveUnit playedCard
    newCardsState
    |> addCardToInactiveUnits newCard laneID
    |> removeHandsIfAllEmpty
    |> changeCardsState gameState

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
            newCards
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
            newCards
    | DrawPileEmpty _
    | HandsEmpty _ ->
        cardsState

let private healOwnUnitsInLane playerID amount (lane: Lane) =
    {lane with
        InactiveUnits =
            lane.InactiveUnits
            |> List.map (fun card ->
                if card.Owner = playerID then
                    healInactiveCard amount card
                else
                    card
                )
        ActiveUnits =
            lane.ActiveUnits
            |> List.map (fun card ->
                if card.Owner = playerID then
                    healActiveCard amount card
                else
                    card
                )
        Pairs =
            lane.Pairs
            |> List.map (fun pair ->
                let {Cards = (card1, card2); Owner = owner} = pair
                if owner = playerID then
                    {pair with Cards = healPairedCard amount card1, healPairedCard amount card2}
                else
                    pair
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
            if card.Owner <> playerID && not (List.contains (InstantNonTargetAbility FreezeEnemiesInLane) card.Abilities.Ignores) then
                {card with FreezeStatus = FrozenBy playerID}
            else
                card
            )
    let newPairs =
        lane.Pairs
        |> List.map (fun pair ->
            let {Cards = c1, c2; Owner = owner; Abilities = {Ignores = ignores}} = pair
            if owner <> playerID && not (List.contains (InstantNonTargetAbility FreezeEnemiesInLane) ignores) then
                let newCards = {c1 with FreezeStatus = FrozenBy playerID}, {c2 with FreezeStatus = FrozenBy playerID}
                {pair with Cards = newCards}
            else
                pair
            )
    let newLane = {lane with InactiveUnits = newInactiveUnits; ActiveUnits = newActiveUnits; Pairs = newPairs}
    {cardsState with Board = {board with Lanes = board.Lanes |> Map.add laneID newLane}}

let private addActiveNonEmpowerActivationAbilitiesInLaneToStack laneID (ActiveUnitID empowererCardID) cardsState turnState =
    let playerID = turnState.CurrentPlayer
    let lane =
        cardsState.Board.Lanes
        |> Map.find laneID
    let powerChoices =
        List.map Solo lane.ActiveUnits @ (lane.Pairs |> List.collect (pairToFullPairedUnits >> twoToList >> List.map Paired))
        |> List.filter (function
            | Solo {ActiveUnitID = ActiveUnitID id; Owner = owner} -> owner = playerID && id <> empowererCardID
            | Paired {FullPairedUnitID = FullPairedUnitID id; Owner = owner} -> owner = playerID && id <> empowererCardID
            )
        |> List.choose (function
            | Solo {ActiveUnitID = ActiveUnitID id; Abilities = abilities} ->
                match abilities.OnActivation with
                | [] -> None
                | oa ->
                    if List.contains ReactivateNonEmpowerActivationPowersInLane oa then
                        None
                    else
                        Some (PowerContext (laneID, id, abilities.Name))
            | Paired {FullPairedUnitID = FullPairedUnitID id; Abilities = abilities} ->
                match abilities.OnActivation with
                | [] -> None
                | oa ->
                    if List.contains ReactivateNonEmpowerActivationPowersInLane oa then
                        None
                    else
                        Some (PowerContext (laneID, id, abilities.Name))
            )
        |> NonEmptyList.tryFromList
    match powerChoices with
    | None -> cardsState, turnState, None
    | Some pc -> cardsState, turnState, Some (OrderChoiceEpoch pc)

let private resolveInstantNonTargetAbility event cardsState turnState =
    let playerID = turnState.CurrentPlayer
    let ability, laneID, cardID = event
    match ability with
    | Draw -> tryDrawCard playerID cardsState, turnState, None
    | Discard -> cardsState, turnState, Some (AbilityChoiceEpoch (DiscardChoiceContext cardID))
    | ViewInactive -> cardsState, turnState, Some (AbilityChoiceEpoch (ViewInactiveChoiceContext cardID))
    | FreezeEnemiesInLane -> freezeEnemyNonActiveNimbleUnitsInLane playerID laneID cardsState, turnState, None
    | HealAllAllies n -> healOwnUnits playerID (n*1u<health>) cardsState, turnState, None
    | MayMoveAllyToOwnLane -> cardsState, turnState, Some (AbilityChoiceEpoch (MayMoveAllyToOwnLaneChoiceContext (laneID, cardID)))
    | ExtraActions n ->
        let (newTurnState: TurnInProgress) = {turnState with ActionsLeft = turnState.ActionsLeft + n*1u<action>}
        cardsState, newTurnState, None
    | ChangeMaxAttacksThisTurn n -> setMaxCardActions cardID (n*1u<action>) laneID cardsState, turnState, None
    | ActivateAlliesInLane -> flipInactiveCardsInLaneAndAddActivationPowersToStack playerID laneID cardsState turnState
    | ReactivateNonEmpowerActivationPowersInLane ->
        addActiveNonEmpowerActivationAbilitiesInLaneToStack laneID (ActiveUnitID cardID) cardsState turnState
    | FullyHealSelf
    | HealSelf _
    | ActivateSelf -> cardsState, turnState, None

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

let rec private processResolutionStack cardsState turnState (maybeResolutionStack: ResolutionStack option) =
    match maybeResolutionStack with
    | None -> {
        CardsState = cardsState
        TurnState = turnState
        TurnStage = ActionChoice
        }
    | Some {Head = h; Tail = t} ->
        match h with
        | OrderChoiceEpoch activationPowerContexts -> {
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
        | AbilityChoiceEpoch choiceContext -> {
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
            let cs, ts, maybeNewEpoch = resolveInstantNonTargetAbility first cardsState turnState
            let newStack = NonEmptyList.consOptions maybeNewEpoch remainingStack
            processResolutionStack cs ts newStack

let private resolveActivationPower laneID (ActiveUnitID cardID) cardsState turnState stack =
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
            |> List.collect (pairToFullPairedUnits >> twoToList)
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
        |> moveDeadCardsToDiscard
    | [AttackAbility DamageExtraTarget] ->
        let activeLaneCards =
            (List.map Solo lane.ActiveUnits) @ List.collect (pairToFullPairedUnits >> twoToList >> List.map Paired) lane.Pairs
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
               |> moveDeadCardsToDiscard
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
        |> moveDeadCardsToDiscard
    | _ ->
        failwithf "Unrecognised OnAttack + OnKill contents: %A" attackingAbilities

let private executeActivateAction laneID (InactiveUnitID cardID) cardsState turnState =
    let removedCard, cs1 = removeCardFromInactiveUnits (InactiveUnitID cardID) laneID cardsState
    let newCard = inactiveToActiveUnit removedCard
    resolveActivationPower laneID (ActiveUnitID cardID) (addCardToActiveUnits newCard laneID cs1) turnState None

let private getBonusDefenderDamage attackerAbilities targetAbilities =
    let hasBonusHealth =
        targetAbilities.WhileActive
        |> List.exists (function
            | MaxHealthIncrease _ -> true
            | _ -> false
        )
    if hasBonusHealth then
        attackerAbilities.OnAttack
        |> List.fold (fun acc ability ->
            match ability with
            | ExtraDamageAgainstExtraMaxHealth n -> acc + n*1u<health>
            | _ -> acc
        ) 0u<health>
    else
        0u<health>

let private getTargetIDFromTargetInfo targetInfo =
    match targetInfo with
    | InactiveTarget (owner, id)
    | ActiveSingleTarget (owner, id)
    | ActivePairMemberTarget (owner, id) ->
        UnitID id

let private getAttackerSelfDamage attackerAbilities targetAbilities =
    if not (List.contains (DefendAbility ReturnDamage) attackerAbilities.Ignores) && (List.contains ReturnDamage targetAbilities.OnDamaged) then
        1u<health>
    else
        0u<health>

let private getSingleAttackInfo attackerID targetInfo lane =
    let targetID = getTargetIDFromTargetInfo targetInfo
    let attackerAbilities =
        lane.ActiveUnits
        |> List.find (fun {ActiveUnitID = ActiveUnitID id} -> id = attackerID)
        |> (fun {Abilities = abilities} -> abilities)
    let targetList =
        (lane.InactiveUnits |> List.map InactiveUnit)
        @ (lane.ActiveUnits |> List.map ActiveUnit)
        @ (lane.Pairs |> List.collect (pairToFullPairedUnits >> twoToList >> List.map PairedUnit))
    let targetAbilities =
        targetList
        |> List.find (fun card ->
            match card with
            | InactiveUnit {InactiveUnitID = InactiveUnitID cid}
            | ActiveUnit {ActiveUnitID = ActiveUnitID cid}
            | PairedUnit {FullPairedUnitID = FullPairedUnitID cid} ->
                UnitID cid = targetID
            )
        |> (function
            | InactiveUnit {Abilities = abilities}
            | ActiveUnit {Abilities = abilities}
            | PairedUnit {Abilities = abilities} ->
                abilities
            )
    let baseDefenderDamage = 1u<health>
    let bonusDefenderDamage = getBonusDefenderDamage attackerAbilities targetAbilities
    let selfDamage = getAttackerSelfDamage attackerAbilities targetAbilities
    targetID, baseDefenderDamage + bonusDefenderDamage, selfDamage

let private getPairAttackInfo pairMemberID1 pairMemberID2 targetInfo lane =
    let targetID = getTargetIDFromTargetInfo targetInfo
    let attackerAbilities =
        lane.Pairs
        |> List.find (fun {Cards = {PairedUnitID = PairedUnitID id1}, {PairedUnitID = PairedUnitID id2}} ->
            (id1, id2) = (pairMemberID1, pairMemberID2)
        )
        |> (fun {Abilities = abilities} -> abilities)
    let targetList =
        (lane.InactiveUnits |> List.map InactiveUnit)
        @ (lane.ActiveUnits |> List.map ActiveUnit)
        @ (lane.Pairs |> List.collect (pairToFullPairedUnits >> twoToList >> List.map PairedUnit))
    let targetAbilities =
        targetList
        |> List.find (fun card ->
            match card with
            | InactiveUnit {InactiveUnitID = InactiveUnitID cid}
            | ActiveUnit {ActiveUnitID = ActiveUnitID cid}
            | PairedUnit {FullPairedUnitID = FullPairedUnitID cid} ->
                UnitID cid = targetID
            )
        |> (fun card ->
            match card with
            | InactiveUnit {Abilities = abilities}
            | ActiveUnit {Abilities = abilities}
            | PairedUnit {Abilities = abilities} ->
                abilities
            )
    let baseDefenderDamage = 2u<health>
    let bonusDefenderDamage = getBonusDefenderDamage attackerAbilities targetAbilities
    let selfDamage = getAttackerSelfDamage attackerAbilities targetAbilities
    targetID, baseDefenderDamage + bonusDefenderDamage, selfDamage

let private executeSingleAttackAction laneID (ActiveUnitID attackerID) targetInfo cardsState turnState =
    let board = cardsState.Board
    let targetID, damage, selfDamage =
        getSingleAttackInfo attackerID targetInfo (Map.find laneID board.Lanes)
    cardsState
    |> incrementCardActionsUsed attackerID laneID
    |> damageCard targetID damage laneID
    |> damageCard (UnitID attackerID) selfDamage laneID
    |> resolveAttackerPassivePower laneID (SingleAttackerID (ActiveUnitID attackerID)) targetID turnState

let private executePairAttackAction laneID (ActiveUnitID attackerID1, ActiveUnitID attackerID2) targetInfo cardsState turnState =
    let board = cardsState.Board
    let targetID, damage, selfDamage =
        getPairAttackInfo attackerID1 attackerID2 targetInfo (Map.find laneID board.Lanes)
    cardsState
    |> incrementCardActionsUsed attackerID1 laneID
    |> incrementCardActionsUsed attackerID2 laneID
    |> damageCard targetID damage laneID
    |> damageCard (UnitID attackerID1) selfDamage laneID
    |> damageCard (UnitID attackerID2) selfDamage laneID
    |> resolveAttackerPassivePower laneID (PairAttackerIDs (PairedUnitID attackerID1, PairedUnitID attackerID2)) targetID turnState

let private executeCreatePairAction laneID cardID1 cardID2 cardsState turnState =
    let card1, card2, newCardsState =
        cardsState
        |> removeCardPairFromActiveUnits cardID1 cardID2 laneID
    {
        CardsState = addCardsToPairs (card1, card2) laneID newCardsState
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
    let newLanes =
        board.Lanes
        |> Map.map (fun _ lane -> timeoutOwnedFreezeStatesInLane playerID lane)
    {cardsState with Board = {board with Lanes = newLanes}}

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

let private getPossibleActionPairs: GetPossibleActionPairs = function
| GameStateBetweenTurns {CardsState = cs; TurnState = ts} ->
    StartTurnPair (cs, ts)
    |> List.singleton
| GameStateDuringTurn gs ->
    match gs with
    | {CardsState = cs; TurnState = ts; TurnStage = ActionChoice} ->
        if ts.ActionsLeft = 0u<action> then
            TurnActionChoicePair (cs, ts, EndTurn)
            |> List.singleton
        else
            let actions =
                getPlayActionsInfo gs
                @ getActivateActionsInfo gs
                @ getAttackActionsInfo gs
                @ getPairActionsInfo gs
            if List.isEmpty actions then
                TurnActionChoicePair (cs, ts, EndTurn)
                |> List.singleton
            else
               actions
               |> List.map (fun action -> TurnActionChoicePair (cs, ts, ActionChoiceInfo action))
    | {CardsState = cs; TurnState = ts; TurnStage = StackChoice sc} ->
        sc.EpochEvents
        |> NonEmptyMap.toMap
        |> Map.toList
        |> List.map (fun (eventID, event) ->
            StackChoicePair (cs, ts, sc, (eventID, event))
        )
    | {CardsState = cs; TurnState = ts; TurnStage = AbilityChoice ac} ->
        let playerID = ts.CurrentPlayer
        match ac.ChoiceContext with
        | DiscardChoiceContext powerCardID ->
            match gs.CardsState.GameStage with
            | Early {HandCards = hc}
            | DrawPileEmpty {HandCards = hc} ->
                hc
                |> Map.find playerID
                |> List.map (fun {HandCardID = HandCardID id} ->
                    AbilityChoicePair (cs, ts, ac.ResolutionStack, DiscardChoice (powerCardID, id))
                    )
            | HandsEmpty _ ->
                failwithf "Can't discard from an empty hand"
        | ViewInactiveChoiceContext powerCardID ->
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
                AbilityChoicePair (cs, ts, ac.ResolutionStack, ViewInactiveChoice (powerCardID, id))
                )
        | MayMoveAllyToOwnLaneChoiceContext (laneID, powerCardID) ->
            let pairs =
                gs.CardsState.Board.Lanes
                    |> Map.filter (fun targetLaneID _ -> targetLaneID <> laneID)
                |> Map.toList
                |> List.collect (fun (targetLaneID, lane) ->
                    (lane.InactiveUnits |> List.choose (fun {InactiveUnitID = InactiveUnitID id; Owner = ownerID}-> if ownerID = playerID then Some id else None))
                    @ (lane.ActiveUnits |> List.choose (fun {ActiveUnitID = ActiveUnitID id; Owner = ownerID} -> if ownerID = playerID then Some id else None))
                    @ (lane.Pairs |> List.collect (fun {Cards = {PairedUnitID = PairedUnitID id1}, {PairedUnitID = PairedUnitID id2}; Owner = ownerID} -> if ownerID = playerID then [id1; id2] else []))
                    |> List.map (fun id -> targetLaneID, id)
                )
                |> List.map (fun (targetLaneID, targetCardID) ->
                    AbilityChoicePair (cs, ts, ac.ResolutionStack, MayMoveAllyToOwnLaneChoice (Some (laneID, powerCardID, targetLaneID, targetCardID)))
                )
            if List.isEmpty pairs then
                pairs
            else
                AbilityChoicePair (cs, ts, ac.ResolutionStack, MayMoveAllyToOwnLaneChoice (None)) :: pairs
        | DamageExtraTargetChoiceContext (laneID, powerCardID, originalTargetCardID) ->
            let lane = Map.find laneID gs.CardsState.Board.Lanes
            let originalTargetCard =
                match List.tryFind (fun (card: InactiveUnit) -> card.InactiveUnitID = InactiveUnitID originalTargetCardID) lane.InactiveUnits with
                | Some card -> InactiveUnit card
                | None ->
                    match List.tryFind (fun (card: ActiveUnit) -> card.ActiveUnitID = ActiveUnitID originalTargetCardID) lane.ActiveUnits with
                    | Some card -> ActiveUnit card
                    | None ->
                        lane.Pairs
                        |> List.collect (pairToFullPairedUnits >> twoToList)
                        |> List.find (fun card -> card.FullPairedUnitID = FullPairedUnitID originalTargetCardID)
                        |> PairedUnit
            let activeTauntCheckTargets, nonActiveTauntCheckTargets =
                let inactiveTargets =
                    lane.InactiveUnits
                    |> List.filter (fun card -> card.Owner <> playerID && card.InactiveUnitID <> InactiveUnitID originalTargetCardID)
                let activeTauntTargets, activeNonTauntTargets =
                    lane.ActiveUnits
                    |> List.filter (fun card -> card.Owner <> playerID && card.ActiveUnitID <> ActiveUnitID originalTargetCardID && not (List.contains (AttackAbility DamageExtraTarget) card.Abilities.Ignores))
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
                AbilityChoicePair (cs, ts, ac.ResolutionStack, DamageExtraTargetChoice (laneID, powerCardID, cardID))
            )
        | ReturnDamagePairChoiceContext (laneID, powerCardIDs, originalTargetCardID) ->
            let (id1, id2) = powerCardIDs
            [id1; id2]
            |> List.map (fun id ->
                AbilityChoicePair (
                    cs, ts, ac.ResolutionStack,
                    ReturnDamagePairChoice (laneID, PairIDs (id1, id2), originalTargetCardID, id)
                )
            )
| GameStateWon _
| GameStateTied _ ->
    List.empty

let private cancelPowerChoiceIfNoChoices state =
    let actionPairs = getPossibleActionPairs state
    match state, actionPairs with
    | GameStateDuringTurn gs, [] ->
        match gs.TurnStage with
        | AbilityChoice ac ->
            toActionChoice gs
            |> triggerTargetInactiveDeathPowers
            |> moveDeadCardsToDiscard
            |> GameStateDuringTurn
        | _ ->
            state
    | _ ->
        state

let private executeAbilityChoice: ExecuteAbilityChoice = fun abilityChoiceInfo cardsState turnState resolutionStack ->
    let gameState = {
        CardsState = cardsState
        TurnState = turnState
        TurnStage = ActionChoice
    }
    let playerID = turnState.CurrentPlayer
    match abilityChoiceInfo with
    | DiscardChoice (_, discardeeCardID) ->
        let (discardeeCard, cs) = removeCardFromHand (HandCardID discardeeCardID) playerID cardsState
        let convertedCard = handToDiscardedCard discardeeCard
        let newCardsState = {
            cs with
                Board = {cs.Board with Discard = cs.Board.Discard @ [convertedCard]}
            }
        changeCardsState gameState newCardsState
    | ViewInactiveChoice (powerCardID, targetCardID) ->
        gameState.CardsState
        |> makeCardKnown targetCardID playerID
        |> changeCardsState gameState
    | MayMoveAllyToOwnLaneChoice maybeMove ->
        match maybeMove with
        | Some (laneID, powerCardID, targetLaneID, targetCardID) ->
            gameState.CardsState
            |> changeCardLane (UnitID targetCardID) targetLaneID laneID
            |> changeCardsState gameState
        | None ->
            gameState
    | DamageExtraTargetChoice (laneID, powerCardIDs, targetCardID) ->
        let lane = Map.find laneID gameState.CardsState.Board.Lanes
        let activeTarget =
            List.map Solo lane.ActiveUnits @ (List.collect (pairToFullPairedUnits >> twoToList >> List.map Paired) lane.Pairs)
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
                |> moveDeadCardsToDiscard
            | [ReturnDamage] ->
                resolveReturnDamage laneID powerCardIDs targetCardID cardsState turnState resolutionStack
            | _ ->
                failwithf "Unrecognised OnDamaged contents: %A" od
        | None ->
            gameState.CardsState
            |> damageCard (UnitID targetCardID) 1u<health> laneID
            |> changeCardsState gameState
            |> toActionChoice
            |> triggerTargetInactiveDeathPowers
            |> moveDeadCardsToDiscard
    | ReturnDamagePairChoice (laneID, powerCardID, targetCardID, whichID) ->
        gameState.CardsState
        |> damageCard (UnitID whichID) 1u<health> laneID
        |> changeCardsState gameState
        |> toActionChoice
        |> triggerTargetInactiveDeathPowers
        |> moveDeadCardsToDiscard
    |> toActionChoice

let private executeTurnAction: ExecuteTurnAction = fun action cardsState turnState ->
    let ts = decrementActionsLeft turnState
    let playerID = ts.CurrentPlayer
    let postAction =
        match action with
        | Play (laneID, cardID) ->
            executePlayAction (HandCardID cardID) laneID cardsState ts
        | Activate (laneID, cardID) ->
            executeActivateAction laneID (InactiveUnitID cardID) cardsState ts
        | SingleAttack (laneID, attackerID, targetInfo) ->
            executeSingleAttackAction laneID (ActiveUnitID attackerID) targetInfo cardsState ts
        | PairAttack (laneID, (attackerID1, attackerID2), targetInfo) ->
            executePairAttackAction laneID (ActiveUnitID attackerID1, ActiveUnitID attackerID2) targetInfo cardsState ts
        | CreatePair (laneID, cardID1, cardID2) ->
            executeCreatePairAction laneID (ActiveUnitID cardID1) (ActiveUnitID cardID2) cardsState ts
    match postAction.TurnStage with
    | AbilityChoice _
    | StackChoice _ ->
        postAction
    | ActionChoice ->
        updateLaneWins postAction

let private startPlayerTurn: ExecuteStartTurn = fun cs ts ->
    {
        CardsState = cs
        TurnState = {
            CurrentPlayer = ts.Player
            NPlayers = ts.NPlayers
            ActionsLeft = ts.Actions
            FutureActionCounts = ts.FutureActionCounts
        }
        TurnStage = ActionChoice
    }

let private executeEndingTurnAction: ExecuteEndTurn = fun cardsState turnState ->
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
            |> timeoutOwnedFreezeStates turnState.CurrentPlayer
        TurnState = {
            Player = nextPlayer
            NPlayers = turnState.NPlayers
            Actions = actions
            FutureActionCounts = nextFutureActionCounts
            }
        }    

let private executeStackChoice: ExecuteStackChoice = fun cardsState turnState stackChoice eventID ->
    let chosen, remaining =
        stackChoice.EpochEvents
        |> NonEmptyMap.toMap
        |> (fun m -> Map.find eventID m, Map.remove eventID m)
    let (laneID, cardID, powerName) = chosen
    let maybeRemainingHead =
        remaining
        |> Map.toList
        |> List.map snd
        |> NonEmptyList.tryFromList
        |> Option.map OrderChoiceEpoch
    let remainingStack = NonEmptyList.consOptions maybeRemainingHead stackChoice.ResolutionStack
    let newState = resolveActivationPower laneID (ActiveUnitID cardID) cardsState turnState remainingStack
    match newState with
    | {CardsState = cs; TurnState = ts; TurnStage = ActionChoice} ->
        {
            CardsState = cs
            TurnState = ts
            TurnStage = ActionChoice
        }
    | {CardsState = cs; TurnState = ts; TurnStage = AbilityChoice c} ->
        {
            CardsState = cs
            TurnState = ts
            TurnStage = AbilityChoice c
        }
    | {CardsState = cs; TurnState = ts; TurnStage = StackChoice sc} ->
        {
            CardsState = cs
            TurnState = ts
            TurnStage = StackChoice sc
        }

let private executeAction: ExecuteAction = function
// Want to do some checking that we have the right player
| AbilityChoicePair (cardsState, turnState, resolutionStack, choiceInfo) ->
    executeAbilityChoice choiceInfo cardsState turnState resolutionStack
    |> checkForGameEnd,
    AbilityChoiceInfo choiceInfo
| StackChoicePair (cardsState, turnState, stackChoice, (eventID, powerContext)) ->
    executeStackChoice cardsState turnState stackChoice eventID
    |> GameStateDuringTurn,
    StackChoiceInfo (eventID, powerContext)
| TurnActionChoicePair (gameState, turnState, ActionChoiceInfo choiceInfo) ->
    executeTurnAction choiceInfo gameState turnState
    |> checkForGameEnd,
    TurnActionInfo (ActionChoiceInfo choiceInfo)
| TurnActionChoicePair (cardsState, turnState, EndTurn) ->
    executeEndingTurnAction cardsState turnState
    |> GameStateBetweenTurns,
    TurnActionInfo EndTurn
| StartTurnPair (cardsState, turnState) ->
    let startTurn = startPlayerTurn cardsState turnState
    {startTurn with CardsState = tryDrawCard turnState.Player cardsState}
    |> GameStateDuringTurn,
    StartTurn

let rec private makeNextActionInfo: CreateUIOutput = fun inProgress actionPair ->
    let newState, action = executeAction actionPair
    let checkedGameState = cancelPowerChoiceIfNoChoices newState
    // Generation of next action's resulting capabilities is part of the
    // generated capability's body, since assigning them here requires
    // generating the entire game space, blowing the stack
    let capability() = inProgress checkedGameState
    {
        Action = action
        Capability = capability
        }
let rec private inProgress: GetInProgress = fun gameState ->
    let nextActions =
        getPossibleActionPairs gameState
        |> List.map (makeNextActionInfo inProgress)
    InProgress (getDisplayInfo gameState, nextActions)

let private createGame: CreateGame = fun (NPlayers nPlayers) (NLanes nLanes) ->
    let getAbilities = basePowers
    let shuffledDeck =
        createUnshuffledDeck()
        |> shuffle
    let cardIDs = createIDs 1u<CID> shuffledDeck
    let cardInfos =
        shuffledDeck
        |> List.zip cardIDs
        |> List.map (fun (cardID, (rank, suit)) -> cardID, rank, suit)
    let bases, notBaseCards =
        cardInfos
        |> prepareHead (prepareBases getAbilities (NLanes nLanes)) (nPlayers*nLanes)
    let handCards, notDeckCards =
        notBaseCards
        |> prepareHead (prepareHands getAbilities (NPlayers nPlayers)) (5u*nPlayers)
    let removed, notRemoved =
        notDeckCards
        |> prepareHead prepareRemoved 10u
    let drawPile =
        notRemoved
        |> List.map (fun (id, rank, suit) ->
            {DeckCardID = DeckCardID id; Rank = rank; Suit = suit}
            )
        |> NonEmptyList.fromList
    let gameState = GameStateBetweenTurns {
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
    inProgress gameState

let api: API = {
    NewGame = fun () -> createGame (NPlayers 2u) (NLanes 3u)
}
