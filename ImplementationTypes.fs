namespace ImplementationTypes
open Domain
open NonEmptyList
open NonEmptyMap
open EventStack
open PowerMaps

type NPlayers = NPlayers of uint
type NLanes = NLanes of uint

type RemovedCardID = RemovedCardID of CardID
type DeckCardID = DeckCardID of CardID
type HandCardID = HandCardID of CardID
type BaseCardID = BaseCardID of CardID
type InactiveUnitID = InactiveUnitID of CardID
type ActiveUnitID = ActiveUnitID of CardID
type PairedUnitID = PairedUnitID of CardID
type FullPairedUnitID = FullPairedUnitID of CardID
type UnitID = UnitID of CardID
type DiscardedCardID = DiscardedCardID of CardID
type ForesightTargetID = ForesightTargetID of CardID
type AttackerIDs =
| SingleAttackerID of ActiveUnitID
| PairAttackerIDs of PairedUnitID * PairedUnitID

type RemovedCard = {
    RemovedCardID: RemovedCardID
    Rank: Rank
    Suit: Suit
}
type DeckCard = {
    DeckCardID: DeckCardID
    Rank: Rank
    Suit: Suit
}
type HandCard = {
    HandCardID: HandCardID
    Rank: Rank
    Suit: Suit
    Abilities: Abilities
    Owner: PlayerID
}
type BaseCard = {
    BaseCardID: BaseCardID
    Rank: Rank
    Suit: Suit
    Abilities: Abilities
    Owner: PlayerID
    KnownBy: PlayerID Set
}

type FreezeStatus =
| FrozenBy of PlayerID
| NotFrozen

type InactiveUnit = {
    InactiveUnitID: InactiveUnitID
    Rank: Rank
    Suit: Suit
    Abilities: Abilities
    Owner: PlayerID
    KnownBy: PlayerID Set
    Damage: Damage
    FreezeStatus: FreezeStatus
}
type ActiveUnit = {
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
type PairedUnit = {
    PairedUnitID: PairedUnitID
    Suit: Suit
    Damage: Damage
    ActionsSpent: Actions
    MaxActions: Actions
    FreezeStatus: FreezeStatus
}
type FullPairedUnit = {
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
type ActiveCard =
| Solo of ActiveUnit
| Paired of FullPairedUnit
type UnitCard =
| InactiveUnit of InactiveUnit
| ActiveUnit of ActiveUnit
| PairedUnit of FullPairedUnit
type Pair = {
    Cards: PairedUnit * PairedUnit
    Rank: Rank
    Abilities: Abilities
    Owner: PlayerID
}
type FaceDownDiscardedCard = {
    DiscardedCardID: DiscardedCardID
    Rank: Rank
    Suit: Suit
    KnownBy: PlayerID Set
}
type FaceUpDiscardedCard = {
    DiscardedCardID: DiscardedCardID
    Rank: Rank
    Suit: Suit
}
type DiscardedCard =
| FaceDownDiscardedCard of FaceDownDiscardedCard
| FaceUpDiscardedCard of FaceUpDiscardedCard
type CardConverter<'From, 'To> = 'From -> 'To

type Attacker =
| SingleAttacker of ActiveUnit
| PairAttacker of Pair

type Lane = {
    InactiveUnits: InactiveUnit list
    ActiveUnits: ActiveUnit list
    Pairs: Pair list
}
type LaneControl =
| Contested
| Empty
| Won of PlayerID

type Board = {
    Lanes: Map<LaneID, Lane>
    Discard: DiscardedCard list
}

type EarlyGameInfo = {
    Bases: Map<LaneID, BaseCard list>
    DrawPile: DeckCard NonEmptyList
    HandCards: Map<PlayerID, HandCard list>
}
type PostDrawGameInfo = {
    HandCards: Map<PlayerID, HandCard list>
    LaneWins: Map<LaneID, PlayerID>
}
type PostHandGameInfo = {
    LaneWins: Map<LaneID, PlayerID>
}
type GameStage =
| Early of EarlyGameInfo
| DrawPileEmpty of PostDrawGameInfo
| HandsEmpty of PostHandGameInfo

type CardsState = {
    Board: Board
    GetAbilities: GetAbilities
    GameStage: GameStage
    Removed: RemovedCard Set
}

type Remover<'Type, 'ID> = 'ID -> LaneID -> CardsState -> 'Type * CardsState
type PairRemover<'T, 'TID> = Remover<'T * 'T, 'TID * 'TID>
type ListRemover<'T, 'TID> = Remover<'T list, 'TID list>
type Adder<'T> = 'T -> LaneID -> CardsState -> CardsState
type ListAdder<'Card> = Adder<'Card list>

type AbilityEvent = InstantNonTargetAbility * LaneID * CardID
type ResolutionEpoch =
| OrderChoiceEpoch of PowerContext epoch
| OrderedAbilityEpoch of AbilityEvent epoch
| AbilityChoiceEpoch of AbilityChoiceContext
type ResolutionStack = ResolutionEpoch nonEmptyList
type AbilityChoice = {
    ChoiceContext: AbilityChoiceContext
    ResolutionStack: ResolutionStack option
}
type StackChoice = {
    EpochEvents: NonEmptyMap<EventID, PowerContext>
    ResolutionStack: ResolutionStack option
}
type TurnStage =
| ActionChoice
| AbilityChoice of AbilityChoice
| StackChoice of StackChoice

type PlayerReady = {
    Player: PlayerID
    NPlayers: NPlayers
    Actions: Actions
    FutureActionCounts: Actions list
}
type TurnInProgress = {
    CurrentPlayer: PlayerID
    NPlayers: NPlayers
    ActionsLeft: Actions
    FutureActionCounts: Actions list
}

type GameStateBetweenTurns = {
    CardsState: CardsState
    TurnState: PlayerReady
}
type GameStateDuringTurn = {
    CardsState: CardsState
    TurnState: TurnInProgress
    TurnStage: TurnStage
}
type GameStateWon = {
    EndLanes: Map<LaneID, Lane>
    Winner: PlayerID
    LaneWins: Map<LaneID, PlayerID>
}
type GameStateTied = {
    EndLanes: Map<LaneID, Lane>
    LaneWins: Map<LaneID, PlayerID>
}
type GameState =
| GameStateDuringTurn of GameStateDuringTurn
| GameStateBetweenTurns of GameStateBetweenTurns
| GameStateWon of GameStateWon
| GameStateTied of GameStateTied

type ActionPair =
| AbilityChoicePair of (CardsState * TurnInProgress * ResolutionStack option) * AbilityChoiceInfo
| StackChoicePair of (CardsState * TurnInProgress * StackChoice) * StackChoiceInfo
| TurnActionChoicePair of GameStateDuringTurn * TurnActionInfo
| StartTurnPair of GameStateBetweenTurns

type GameEvent =
| GameStarted
| TurnStarted of PlayerID
| TurnEnded of PlayerID
| StackChoiceMade of PlayerID * PowerContext
| ActionChosen of PlayerID * ActionChoiceInfo
| AbilityChoiceMade of PlayerID * AbilityChoiceInfo
| CardPlayed of InactiveUnit * LaneID
| CardActivated of ActiveUnit
| CardAttacked of Attacker * UnitCard
| CardDamaged of UnitCard * Damage
| CardsPaired of Pair

type ExecuteTurnActionType<'CardsInfo> = LaneID -> 'CardsInfo -> CardsState -> TurnInProgress -> GameEvent list * GameStateDuringTurn
type ExecutePlayAction = ExecuteTurnActionType<HandCardID>
type ExecuteActivateAction = ExecuteTurnActionType<InactiveUnitID>
type ExecuteSingleAttackAction = ExecuteTurnActionType<ActiveUnitID * AttackTargetInfo>
type ExecutePairAttackAction = ExecuteTurnActionType<(PairedUnitID * PairedUnitID) * AttackTargetInfo>
type ExecuteCreatePairAction = ExecuteTurnActionType<ActiveUnitID * ActiveUnitID>

type WithAdded<'From, 'Mid, 'To, 'Added> = ('From -> 'Mid) -> 'From -> 'Added * 'To
type StartTurnInput = CardsState * PlayerReady
type StartTurn = StartTurnInput -> GameStateDuringTurn
type ExecuteStartTurn = WithAdded<StartTurnInput, GameStateDuringTurn, GameState, GameEvent list>
type EndTurn = CardsState * TurnInProgress -> GameStateBetweenTurns
type ExecuteEndTurn = WithAdded<CardsState * TurnInProgress, GameStateBetweenTurns, GameState, GameEvent list>

type ExecuteTurnChoice<'Choice, 'FromState, 'ToState> = 'Choice -> 'FromState -> GameEvent list * 'ToState
type ExecuteTurnAction = ExecuteTurnChoice<ActionChoiceInfo, CardsState * TurnInProgress, GameStateDuringTurn>
type ExecuteAbilityChoice = ExecuteTurnChoice<AbilityChoiceInfo, CardsState * TurnInProgress * ResolutionStack option, GameStateDuringTurn>
type ExecuteStackChoice = ExecuteTurnChoice<EventID, CardsState * TurnInProgress * StackChoice, GameStateDuringTurn>

// Game state and ActionInfo go into action pair, just for ActionInfo to come out again at execution, seems silly
type CreateGame = NPlayers -> NLanes -> ActionResult
type GetPossibleActionPairs = GameState -> ActionPair list
type ExecuteAction = ActionPair -> GameEvent list * GameState * ActionInfo
type GetInProgress = GameEvent list -> GameState -> ActionResult
type GameEventToDisplayGameEvent = GameEvent -> DisplayGameEvent
type GameStateToDisplayInfo = GameState -> DisplayInfo
type CreateUIOutput = GetInProgress -> ActionPair -> CapabilityInfo<ActionInfo, ActionResult>
