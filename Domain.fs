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
[<Measure>] type CID
[<Measure>] type LID
[<Measure>] type HP

type PlayerID = int<PID>
type TroopID = int<TID>
type CardID = int<CID>
type LaneID = int<LID>
type Health = int<health>
type HandPosition = int<HP>

type HandCard = HandCard of Power

type Hand = HandCard list

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
| UnknownInactiveCardKnowledge of Health
| KnownInactiveCardKnowledge of Power * Health
| ActiveCardKnowledge of Power * Health * Readiness
| PairKnowledge of Power * Health * Health * Readiness

type TroopsKnowledge = Map<PlayerID, TroopKnowledge list>

type PreBaseFlipLaneKnowledge = {
    Bases: BaseKnowledge list
    Troops: TroopsKnowledge
}

type ContestedLaneKnowledge = {
    Troops: TroopsKnowledge
}

type WonLaneKnowledge = {
    Controller: PlayerID
    Troops: TroopsKnowledge
}

type PostBaseFlipLaneKnowledge =
| ContestedLaneKnowledge of ContestedLaneKnowledge
| WonLaneKnowledge of WonLaneKnowledge

type DiscardKnowledge = DeadCardKnowledge list

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
| KnownActivationTarget of Power * Health
| UnknownActivationTarget of Health

type TurnActionInfo =
| Play of HandPosition * LaneID
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
