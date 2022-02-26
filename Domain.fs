namespace Domain
open EventStack

type ActivationPower =
| View
| Foresight
| Flip
| Freeze
| Heal
| Move
| Empower
| Action

type PassivePower =
| Retaliate
| Nimble
| TwinStrike
| Taunt
| Vampiric // replaces Trap and Foresight in solo mode

type InactiveDeathPower =
| Trap

type Power =
| ActivationPower of ActivationPower
| PassivePower of PassivePower
| InactiveDeathPower of InactiveDeathPower

[<Measure>] type health
[<Measure>] type PID
[<Measure>] type LID
[<Measure>] type CID
[<Measure>] type EID
[<Measure>] type action
 
type Damage = int<health>
type PlayerID = int<PID>
type LaneID = int<LID>
type CardID = int<CID>
type EventID = int<EID>
type Actions = int<action>
type UnitIDs =
| SingleCardID of CardID
| PairIDs of CardID * CardID

type HandCardInfo = HandCardInfo of CardID * Power

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
| UnknownBaseCard of PlayerID
| KnownBaseCard of PlayerID * Power

type DeadCardKnowledge =
| UnknownDeadCard
| KnownFaceDownDeadCard of Power
| KnownFaceUpDeadCard of Power

type InactiveUnitKnowledge =
| UnknownInactiveCardKnowledge of CardID * Damage * Actionability
| KnownInactiveCardKnowledge of CardID * Power * Damage * Actionability

type ActiveUnitKnowledge = CardID * Power * Damage * Actions * Actionability

type PairKnowledge = CardID * CardID * Power * Damage * Damage * Actions * Actionability * Actionability

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

type MidPassivePowerChoiceContext =
| TwinStrikeChoiceContext of PlayerID * LaneID * UnitIDs * CardID
| TwinStrikeRelatiatePairChoiceContext of PlayerID * LaneID * (CardID * CardID) * CardID

type MidActivationPowerChoiceContext =
| DiscardChoiceContext of PlayerID * CardID
| ForesightChoiceContext of PlayerID * CardID
| MoveChoiceContext of PlayerID * LaneID * CardID

type PassivePowerWithDecisionContext =
| TwinStrikePowerContext of PlayerID * LaneID * CardID

type ActivationPowerContext =
| ViewPowerContext of PlayerID * LaneID * CardID
| ForesightPowerContext of PlayerID * LaneID * CardID
| FlipPowerContext of PlayerID * LaneID * CardID
| FreezePowerContext of PlayerID * LaneID * CardID
| HealPowerContext of PlayerID * LaneID * CardID
| MovePowerContext of PlayerID * LaneID * CardID
| EmpowerPowerContext of PlayerID * LaneID * CardID
| ActionPowerContext of PlayerID * LaneID * CardID

type MidActivationPowerChoiceDisplayInfo = {
    CurrentPlayer: PlayerID
    ActionsLeft: Actions
    BoardKnowledge: BoardKnowledge
    PlayerHand: Hand
    OpponentHandSizes: (PlayerID * int) list
    ChoiceContext: MidActivationPowerChoiceContext
}

type MidPassivePowerChoiceDisplayInfo = {
    CurrentPlayer: PlayerID
    ActionsLeft: Actions
    BoardKnowledge: BoardKnowledge
    PlayerHand: Hand
    OpponentHandSizes: (PlayerID * int) list
    ChoiceContext: MidPassivePowerChoiceContext
}

type StackChoiceDisplayInfo = {
    CurrentPlayer: PlayerID
    ActionsLeft: Actions
    BoardKnowledge: BoardKnowledge
    PlayerHand: Hand
    OpponentHandSizes: (PlayerID * int) list
    Stack: ActivationPowerContext stack
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
| MidActivationPowerChoiceDisplayInfo of MidActivationPowerChoiceDisplayInfo
| MidPassivePowerChoiceDisplayInfo of MidPassivePowerChoiceDisplayInfo
| StackChoiceDisplayInfo of StackChoiceDisplayInfo
| TurnDisplayInfo of TurnDisplayInfo
| SwitchDisplayInfo of PlayerID
| WonGameDisplayInfo of WonGameDisplayInfo
| TiedGameDisplayInfo of TiedGameDisplayInfo

type AttackTargetInfo =
| InactiveTarget of PlayerID * CardID
| ActiveSingleTarget of PlayerID * CardID
| ActivePairMemberTarget of PlayerID * CardID

type MidActivationPowerChoiceInfo =
| DiscardChoice of PlayerID * CardID * CardID
| ForesightChoice of PlayerID * CardID * CardID
| MoveChoice of (PlayerID * LaneID * CardID * LaneID * CardID) option

type MidPassivePowerChoiceInfo =
| TwinStrikeChoice of PlayerID * LaneID * UnitIDs * CardID
| TwinStrikeRetaliatePairChoice of PlayerID * LaneID * UnitIDs * CardID * CardID

type StackChoiceInfo = PlayerID * EventID * ActivationPowerContext

type ActionChoiceInfo =
| Play of PlayerID * CardID * LaneID
| Activate of PlayerID * LaneID * CardID
| SingleAttack of PlayerID * LaneID * CardID * AttackTargetInfo
| PairAttack of PlayerID * LaneID * (CardID * CardID) * AttackTargetInfo
| CreatePair of PlayerID * LaneID * CardID * CardID

type TurnActionInfo =
| ActionChoiceInfo of ActionChoiceInfo
| EndTurn of PlayerID

type ActionInfo =
| MidActivationPowerChoiceInfo of MidActivationPowerChoiceInfo
| MidPassivePowerChoiceInfo of MidPassivePowerChoiceInfo
| StackChoiceInfo of StackChoiceInfo
| TurnActionInfo of TurnActionInfo
| StartTurn of PlayerID

type ActionCapability = unit -> ActionResult
and NextActionInfo = {
    Action: ActionInfo
    Capability: ActionCapability
}
and ActionResult =
| InProgress of DisplayInfo * NextActionInfo list
| Exit

type API = {
    NewGame: unit -> ActionResult
}
