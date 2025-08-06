namespace Domain
open EventStack

type Rank =
| Two
| Three
| Four
| Five
| Six
| Seven
| Eight
| Nine
| Ten
| Jack
| Queen
| King
| Ace

type Suit =
| Clubs
| Diamonds
| Hearts
| Spades

[<Measure>] type health
[<Measure>] type PID
[<Measure>] type LID
[<Measure>] type CID
[<Measure>] type EID
[<Measure>] type action

type Damage = uint<health>
type PlayerID = uint<PID>
type LaneID = uint<LID>
type CardID = uint<CID>
type EventID = uint<EID>
type Actions = uint<action>
type UnitIDs =
| SingleCardID of CardID
| PairIDs of CardID * CardID

type PowerName = PowerName of string

type HandCardInfo = HandCardInfo of CardID * Rank * Suit * PowerName

type Hand = HandCardInfo list

type Actionability =
| Normal
| Frozen

type Readiness =
| Ready
| Exhausted

type ActiveStatus =
| Inactive // face-down
| Active // face-up

type BaseKnowledge =
| UnknownBaseCard of CardID * PlayerID * Actionability
| KnownBaseCard of CardID * PlayerID * Rank * Suit * PowerName * Actionability

type DeadCardKnowledge =
| UnknownDeadCard
| KnownFaceDownDeadCard of Rank * Suit
| KnownFaceUpDeadCard of Rank * Suit

type InactiveUnitKnowledge =
| UnknownInactiveCardKnowledge of CardID * Damage * Actionability
| KnownInactiveCardKnowledge of CardID * Rank * Suit * PowerName * Damage * Actionability

type ActiveUnitKnowledge = CardID * Rank * Suit * PowerName * Damage * Actions * Actionability

type PairKnowledge = CardID * CardID * Rank * Suit * Suit * PowerName * Damage * Damage * Actions * Actionability * Actionability

type TroopKnowledge = Map<PlayerID, InactiveUnitKnowledge list * ActiveUnitKnowledge list * PairKnowledge list>

type PreBaseFlipLaneKnowledge = {
    Bases: BaseKnowledge list
    Troops: TroopKnowledge
}

type ContestedLaneKnowledge = {
    Troops: TroopKnowledge
}

type WonLaneKnowledge = {
    Controller: PlayerID
    Troops: TroopKnowledge
}

type PostBaseFlipLaneKnowledge =
| ContestedLaneKnowledge of ContestedLaneKnowledge
| WonLaneKnowledge of WonLaneKnowledge

type DiscardKnowledge = DeadCardKnowledge list

type PreBaseFlipBoardKnowledge = {
    Lanes: Map<LaneID, PreBaseFlipLaneKnowledge>
    DrawPileSize: int
    Discard: DiscardKnowledge
}

type PostBaseFlipBoardKnowledge = {
    Lanes: Map<LaneID, PostBaseFlipLaneKnowledge>
    Discard: DiscardKnowledge
}

type BoardKnowledge =
| PreBaseFlipBoardKnowledge of PreBaseFlipBoardKnowledge
| PostBaseFlipBoardKnowledge of PostBaseFlipBoardKnowledge

type AbilityChoiceContext =
| DiscardChoiceContext of CardID
| ViewInactiveChoiceContext of CardID
| MayMoveAllyToOwnLaneChoiceContext of LaneID * CardID
| DamageExtraTargetChoiceContext of LaneID * UnitIDs * CardID
| ReturnDamagePairChoiceContext of LaneID * (CardID * CardID) * CardID

type PowerContext = LaneID * CardID * PowerName

type AbilityChoiceDisplayInfo = {
    CurrentPlayer: PlayerID
    ActionsLeft: Actions
    BoardKnowledge: BoardKnowledge
    PlayerHand: Hand
    OpponentHandSizes: (PlayerID * int) list
    ChoiceContext: AbilityChoiceContext
}

type StackChoiceDisplayInfo = {
    CurrentPlayer: PlayerID
    ActionsLeft: Actions
    BoardKnowledge: BoardKnowledge
    PlayerHand: Hand
    OpponentHandSizes: (PlayerID * int) list
    Stack: PowerContext stack
}

type TurnDisplayInfo = {
    CurrentPlayer: PlayerID
    ActionsLeft: Actions
    BoardKnowledge: BoardKnowledge
    PlayerHand: Hand
    OpponentHandSizes: (PlayerID * int) list
}

type WonGameDisplayInfo = {
    Winner: PlayerID
    LaneWins: (PlayerID * LaneID list) list
}

type TiedGameDisplayInfo = {
    LaneWins: (PlayerID * LaneID list) list
}

type DisplayInfo =
| AbilityChoiceDisplayInfo of AbilityChoiceDisplayInfo
| StackChoiceDisplayInfo of StackChoiceDisplayInfo
| TurnDisplayInfo of TurnDisplayInfo
| SwitchDisplayInfo of PlayerID
| WonGameDisplayInfo of WonGameDisplayInfo
| TiedGameDisplayInfo of TiedGameDisplayInfo

type AttackTargetInfo =
| InactiveTarget of PlayerID * CardID
| ActiveSingleTarget of PlayerID * CardID
| ActivePairMemberTarget of PlayerID * CardID

type AbilityChoiceInfo =
| DiscardChoice of CardID * CardID
| ViewInactiveChoice of CardID * CardID
| MayMoveAllyToOwnLaneChoice of (LaneID * CardID * LaneID * CardID) option
| DamageExtraTargetChoice of LaneID * UnitIDs * CardID
| ReturnDamagePairChoice of LaneID * UnitIDs * CardID * CardID

type StackChoiceInfo = EventID * PowerContext

type ActionChoiceInfo =
| Play of LaneID * CardID
| Activate of LaneID * CardID
| SingleAttack of LaneID * CardID * AttackTargetInfo
| PairAttack of LaneID * (CardID * CardID) * AttackTargetInfo
| CreatePair of LaneID * CardID * CardID

type DisplayGameEvent =
| DisplayGameStarted
| DisplayTurnStarted of PlayerID
| DisplayTurnEnded of PlayerID
| DisplayAbilityChoiceMade of PlayerID * AbilityChoiceInfo
| DisplayActionChosen of PlayerID * ActionChoiceInfo
| DisplayStackChoiceMade of PlayerID * PowerContext
| DisplayCardPlayed of PlayerID * CardID * LaneID
| DisplayCardActivated of PlayerID * CardID * Rank * Suit * PowerName
| DisplayCardAttacked of PlayerID * UnitIDs * PlayerID * CardID
| DisplayCardDamaged of PlayerID * CardID * Damage
| DisplayCardsPaired of PlayerID * CardID * CardID * Suit * Suit * Rank * PowerName
| DisplayActionsGained of PlayerID * Actions
| DisplayAttacksSet of PlayerID * CardID * Rank * Suit * PowerName * Actions
| DisplayCannotDraw of PlayerID
| DisplayCardDrawn of PlayerID
| DisplayDrawPileExhausted
| DisplayLaneFrozen of LaneID
| DisplayCardFrozen of Rank * Suit * PowerName * PlayerID
| DisplayCardHealed of Rank * Suit * PowerName * Damage
| DisplayCardReactivated of Rank * Suit * PowerName
| DisplayCardFullyHealedSelf of Rank * Suit * PowerName
| DisplayCardHealedSelf of Rank * Suit * PowerName * Damage
| DisplayCardActivatedSelf of CardID * Rank * Suit * PowerName

type TurnActionInfo =
| ActionChoiceInfo of ActionChoiceInfo
| EndTurn

type ActionInfo =
| AbilityChoiceInfo of AbilityChoiceInfo
| StackChoiceInfo of StackChoiceInfo
| TurnActionInfo of TurnActionInfo
| StartTurn

type Capability<'T> = unit -> 'T
type CapabilityInfo<'Action, 'T> = {
    Action: 'Action
    Capability: Capability<'T>
}

// CapabilityInfo<ActionInfo, ActionResult> in the below represents information about a next action
// The child ActionResults being contained within capabilities, i.e. niladic functions,
// prevents us from having to recursively generate the next ActionResults, which would result in
// generating the game's entire action decision tree at the beginning.
type ActionResult =
| InProgress of DisplayGameEvent list * DisplayInfo * CapabilityInfo<ActionInfo, ActionResult> list
| Exit

type API = {
    NewGame: unit -> ActionResult
}
