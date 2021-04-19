namespace Domain

type Power =
| View
| Trap
| Foresight
| Flip
| Freeze
| Heal
| Retaliate
| Nimble
| TwinStrike
| Taunt
| Vampiric // replaces Trap and Foresight in solo mode
| Move
| Empower
| Action

type PlayerID = int
type CardID = int
type TroopID = int
type LaneID = int

type Health = int

type Readiness =
| Ready
| Exhausted

type ActiveStatus =
| Inactive // face-down
| Active // face-up

type DrawCard = DrawCard of Power * CardID
type HandCard = HandCard of Power * CardID

type KnownBy = int list

type Base = Power * CardID * PlayerID * KnownBy
type BaseKnowledge =
| UnknownBaseCard of CardID * PlayerID
| KnownBaseCard of Power * CardID * PlayerID

type InactiveCard = Power * CardID * Health * PlayerID * KnownBy
type ActiveCard = Power * CardID * Health * Readiness * PlayerID
type FaceDownDeadCard = Power * KnownBy
type FaceUpDeadCard = Power
type DeadCard =
| FaceDownDeadCard of FaceDownDeadCard
| FaceUpDeadCard of FaceUpDeadCard
type KnownDeadCard =
| KnownFaceDownDeadCard of Power
| KnownFaceUpDeadCard of Power
type DeadCardKnowledge =
| UnknownDeadCard
| KnownDeadCard of KnownDeadCard
type RemovedCard = RemovedCard of Power

type Pair = Power * TroopID * (CardID * Health) * (CardID * Health) * Readiness * PlayerID

type Troop =
| InactiveCard of InactiveCard
| ActiveCard of ActiveCard
| Pair of Pair

type TroopKnowledge =
| UnknownInactiveCardKnowledge of CardID * Health * PlayerID
| KnownInactiveCardKnowledge of Power * CardID * Health * PlayerID
| ActiveCardKnowledge of ActiveCard
| PairKnowledge of Pair

type PreBaseFlipLane = {
    Bases: Base list
    Troops: Troop list
}

type PreBaseFlipLaneKnowledge = {
    Bases: BaseKnowledge list
    Troops: TroopKnowledge list
}

type ContestedLane = {
    Troops: Troop list
}

type ContestedLaneKnowledge = {
    Troops: TroopKnowledge list
}

type WonLane = {
    Controller: PlayerID
    Troops: Troop list
}

type WonLaneKnowledge = {
    Controller: PlayerID
    Troops: TroopKnowledge list
}

type Lane =
| PreBaseFlipLane of PreBaseFlipLane
| ContestedLane of ContestedLane
| WonLane of WonLane
| TiedLane

type LaneKnowledge =
| PreBaseFlipLaneKnowledge of PreBaseFlipLaneKnowledge
| ContestedLaneKnowledge of ContestedLaneKnowledge
| WonLaneKnowledge of WonLaneKnowledge
| TiedLaneKnowledge

type Hand = HandCard list

type DisplayInfo = {
    CurrentPlayer: PlayerID
    ActionsLeft: int
    BoardKnowledge: LaneKnowledge list
    PlayerHand: Hand
    OpponentHandSizes: (PlayerID * int) list
    DrawPileSize: int
    DiscardKnowledge: DeadCardKnowledge list
}

type ActionInfo =
| Play of CardID * Power * LaneID
| FlipCard of CardID * Power * LaneID * Health
| Attack of TroopID * CardID
| CreatePair of CardID * CardID * Power * LaneID * Health * Health

type ActionCapability = unit -> ActionResult
and NextActionInfo = {
    Action: ActionInfo
    Capability: ActionCapability
}
and ActionResult =
| InProgress of DisplayInfo * NextActionInfo list
| WonGame of DisplayInfo * PlayerID

type API = {
    NewGame: unit -> ActionResult
}
