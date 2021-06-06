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
[<Measure>] type LPIP
[<Measure>] type LPAP
[<Measure>] type LPPP

type PlayerID = int<PID>
type TroopID = int<TID>
type CardID = int<CID>
type LaneID = int<LID>
type Health = int<health>
type HandPosition = int<HP>
type LanePlayerInactivePosition = int<LPIP>
type LanePlayerActivePosition = int<LPAP>
type LanePlayerPairPosition = int<LPPP>
type PairUnitPosition = | One | Two

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

type InactiveUnitKnowledge =
| UnknownInactiveCardKnowledge of Health
| KnownInactiveCardKnowledge of Power * Health

type ActiveUnitKnowledge = Power * Health * Readiness

type PairKnowledge = Power * Health * Health * Readiness

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

type AttackTargetInfo =
| InactiveTarget of PlayerID * LanePlayerInactivePosition
| ActiveSingleTarget of PlayerID * LanePlayerActivePosition
| ActivePairMemberTarget of PlayerID * LanePlayerPairPosition * PairUnitPosition

type ActivationTarget =
| KnownActivationTarget of Power * Health
| UnknownActivationTarget of Health

type TurnActionInfo =
| Play of HandPosition * LaneID
| Activate of PlayerID * LaneID * LanePlayerInactivePosition
| SingleAttack of PlayerID * LaneID * LanePlayerActivePosition * AttackTargetInfo
| PairAttack of PlayerID * LaneID * LanePlayerPairPosition * AttackTargetInfo
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
