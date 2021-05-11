namespace Domain

type ActivationPower =
| View
| Trap
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

type Power =
| ActivationPower of ActivationPower
| PassivePower of PassivePower

[<Measure>] type health
[<Measure>] type PID
[<Measure>] type TID
[<Measure>] type LID

type PlayerID = int<PID>
type TroopID = int<TID>
type LaneID = int<LID>
type Health = int<health>
type KnownBy = int<PID> list

type HandCard = HandCard of Power

type Hand = CountMap.CountMap<HandCard>

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

type TroopKnowledge =
| UnknownInactiveCardKnowledge of PlayerID * Health * KnownBy
| KnownInactiveCardKnowledge of PlayerID * Power * Health * KnownBy
| ActiveCardKnowledge of PlayerID * Power * Health * Readiness
| PairKnowledge of PlayerID * Power * Health * Health * Readiness

type PreBaseFlipLaneKnowledge = {
    Bases: BaseKnowledge list
    Troops: CountMap.CountMap<TroopKnowledge>
}

type ContestedLaneKnowledge = {
    Troops: CountMap.CountMap<TroopKnowledge>
}

type WonLaneKnowledge = {
    Controller: PlayerID
    Troops: CountMap.CountMap<TroopKnowledge>
}

type PostBaseFlipLaneKnowledge =
| ContestedLaneKnowledge of ContestedLaneKnowledge
| WonLaneKnowledge of WonLaneKnowledge
| TiedLaneKnowledge

type DiscardKnowledge = CountMap.CountMap<DeadCardKnowledge>

type PreBaseFlipBoardKnowledge = {
    Lanes: PreBaseFlipLaneKnowledge list
    DrawPileSize: int
    Discard: DiscardKnowledge
}

type PostBaseFlipBoardKnowledge = {
    Lanes: PostBaseFlipLaneKnowledge list
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

type AttackerInfo =
| SingleAttacker of Power * Health
| DoubleAttacker of Power * Health * Health

type AttackTargetInfo =
| UnknownInactiveTarget of PlayerID * Health
| KnownInactiveTarget of PlayerID * Power * Health
| ActiveSingleTarget of PlayerID * Power * Health
| ActivePairMemberTarget of PlayerID * Power * Health * Health

type ActivationTarget =
| KnownActivationTarget of Power * Health * KnownBy
| UnknownActivationTarget of Health * KnownBy

type TurnActionInfo =
| Play of PlayerID * Power * LaneID
| Activate of PlayerID * LaneID * ActivationTarget
| Attack of PlayerID * LaneID * AttackerInfo * AttackTargetInfo
| CreatePair of PlayerID * LaneID * Power * (Health * Readiness) * (Health * Readiness)

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
