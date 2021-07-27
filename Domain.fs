namespace Domain

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

type Damage = int<health>
type PlayerID = int<PID>
type LaneID = int<LID>
type CardID = int<CID>

type HandCard = HandCard of CardID * Power

type Hand = HandCard list

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

type KnownDeadCard =
| KnownFaceDownDeadCard of Power
| KnownFaceUpDeadCard of Power

type DeadCardKnowledge =
| UnknownDeadCard
| KnownDeadCard of KnownDeadCard

type InactiveUnitKnowledge =
| UnknownInactiveCardKnowledge of CardID * Damage * Actionability
| KnownInactiveCardKnowledge of CardID * Power * Damage * Actionability

type ActiveUnitKnowledge = CardID * Power * Damage * Readiness * Actionability

type PairKnowledge = CardID * CardID * Power * Damage * Damage * Readiness * Actionability * Actionability

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

type TurnDisplayInfo = {
    CurrentPlayer: PlayerID
    ActionsLeft: int
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
| TurnDisplayInfo of TurnDisplayInfo
| SwitchDisplayInfo of PlayerID
| WonGameDisplayInfo of WonGameDisplayInfo
| TiedGameDisplayInfo of TiedGameDisplayInfo

type AttackTargetInfo =
| InactiveTarget of PlayerID * CardID
| ActiveSingleTarget of PlayerID * CardID
| ActivePairMemberTarget of PlayerID * CardID

type TurnActionInfo =
| Play of PlayerID * CardID * LaneID
| Activate of PlayerID * LaneID * CardID
| SingleAttack of PlayerID * LaneID * CardID * AttackTargetInfo
| PairAttack of PlayerID * LaneID * (CardID * CardID) * AttackTargetInfo
| CreatePair of PlayerID * LaneID * CardID * CardID

type ActionInfo =
| TurnActionInfo of TurnActionInfo
| EndTurn of PlayerID
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
