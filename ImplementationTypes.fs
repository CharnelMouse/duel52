namespace ImplementationTypes
open Domain
open NonEmptyList
open NonEmptyMap
open EventStack
open PowerMaps
open CardTypes

type NPlayers = NPlayers of uint
type NLanes = NLanes of uint

type AttackerIDs =
| SingleAttackerID of ActiveUnitID
| PairAttackerIDs of PairedUnitID * PairedUnitID

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

type ChoiceMap = NonEmptyMap<EventID, PowerContext>

type AbilityEvent = InstantNonTargetAbility * LaneID * CardID
type ResolutionEpoch =
| OrderChoiceEpoch of PowerContext epoch
| OrderedAbilityEpoch of AbilityEvent epoch
| AbilityChoiceEpoch of AbilityChoiceContext
type StackHeadDecision =
| AbilityChoice of AbilityChoiceContext
| OrderChoice of ChoiceMap
type TurnStage =
| ActionChoice
| ResolvingStack of StackHeadDecision * ResolutionEpoch list

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
| AbilityChoicePair of (CardsState * TurnInProgress * ResolutionEpoch list) * AbilityChoiceInfo
| StackChoicePair of (CardsState * TurnInProgress * ChoiceMap * ResolutionEpoch list) * StackChoiceInfo
| TurnActionChoicePair of CardsState * TurnInProgress * TurnActionInfo
| StartTurnPair of CardsState * PlayerReady

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
| ActionsGained of PlayerID * Actions
| AttacksSet of PlayerID * ActiveCard * Actions // I want an active card here
| CannotDraw of PlayerID
| CardDrawn of PlayerID
| DrawPileExhausted
| LaneFrozen of LaneID
| CardFrozen of UnitCard
| CardHealed of UnitCard
| CardReactivated of ActiveCard
| CardFullyHealedSelf of UnitCard
| CardHealedSelf of UnitCard
| CardActivatedSelf of ActiveUnit

type EventStreamer<'From, 'To> = ('From -> GameEvent list * 'To) -> GameEvent list * 'From -> GameEvent list * 'To
type WithAdded<'From, 'Mid, 'To, 'Added> = ('From -> 'Mid) -> 'From -> 'Added * 'To

type ResolveInstantNonTargetAbility = (InstantNonTargetAbility * LaneID * CardID) -> CardsState -> TurnInProgress -> GameEvent list * (CardsState * TurnInProgress * ResolutionEpoch option)
type ProcessResolutionEpochs = CardsState -> TurnInProgress -> ResolutionEpoch list -> GameEvent list * GameStateDuringTurn
type ResolveActivationPower = LaneID -> ActiveUnitID -> CardsState -> TurnInProgress -> ResolutionEpoch list -> GameEvent list * GameStateDuringTurn

type ExecuteTurnActionType<'CardsInfo> = LaneID -> 'CardsInfo -> CardsState -> TurnInProgress -> GameEvent list * GameStateDuringTurn
type ExecutePlayAction = ExecuteTurnActionType<HandCardID>
type ExecuteActivateAction = ExecuteTurnActionType<InactiveUnitID>
type ExecuteSingleAttackAction = ExecuteTurnActionType<ActiveUnitID * AttackTargetInfo>
type ExecutePairAttackAction = ExecuteTurnActionType<(PairedUnitID * PairedUnitID) * AttackTargetInfo>
type ExecuteCreatePairAction = ExecuteTurnActionType<ActiveUnitID * ActiveUnitID>

type StartTurnInput = CardsState * PlayerReady
type StartTurn = StartTurnInput -> GameEvent list * GameStateDuringTurn
type TurnActionInput = CardsState * TurnInProgress
type EndTurn = TurnActionInput -> GameStateBetweenTurns
type ExecuteEndTurn = WithAdded<TurnActionInput, GameStateBetweenTurns, GameState, GameEvent list>

type ExecuteTurnChoice<'Choice, 'FromState, 'ToState> = 'Choice -> 'FromState -> GameEvent list * 'ToState
type ExecuteTurnAction = ExecuteTurnChoice<ActionChoiceInfo, TurnActionInput, GameStateDuringTurn>
type ExecuteAbilityChoice = ExecuteTurnChoice<AbilityChoiceInfo, CardsState * TurnInProgress * ResolutionEpoch list, GameStateDuringTurn>
type ExecuteOrderChoice = ExecuteTurnChoice<EventID, CardsState * TurnInProgress * ChoiceMap * ResolutionEpoch list, GameStateDuringTurn>

// Game state and ActionInfo go into action pair, just for ActionInfo to come out again at execution, seems silly
type CreateGame = NPlayers -> NLanes -> ActionResult
type GetPossibleActionPairs = GameState -> ActionPair list
type ExecuteAction = ActionPair -> GameEvent list * GameState
type GetInProgress = GameEvent list -> GameState -> ActionResult
type GameEventToDisplayGameEvent = GameEvent -> DisplayGameEvent
type GameStateToDisplayInfo = GameState -> DisplayInfo
type CreateUIOutput = GetInProgress -> ActionPair -> CapabilityInfo<ActionInfo, ActionResult>
