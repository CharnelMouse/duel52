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

type HandCard = HandCard of Power * CardID
type Hand = HandCard list

type Readiness =
| Ready
| Exhausted

type ActiveStatus =
| Inactive // face-down
| Active // face-up

type BaseKnowledge =
| UnknownBaseCard of CardID * PlayerID
| KnownBaseCard of Power * CardID * PlayerID

type ActiveCard = Power * CardID * Health * Readiness * PlayerID
type KnownDeadCard =
| KnownFaceDownDeadCard of Power
| KnownFaceUpDeadCard of Power
type DeadCardKnowledge =
| UnknownDeadCard
| KnownDeadCard of KnownDeadCard

type Pair = Power * TroopID * (CardID * Health) * (CardID * Health) * Readiness * PlayerID

type TroopKnowledge =
| UnknownInactiveCardKnowledge of CardID * Health * PlayerID
| KnownInactiveCardKnowledge of Power * CardID * Health * PlayerID
| ActiveCardKnowledge of ActiveCard
| PairKnowledge of Pair

type PreBaseFlipLaneKnowledge = {
    Bases: BaseKnowledge list
    Troops: TroopKnowledge list
}

type ContestedLaneKnowledge = {
    Troops: TroopKnowledge list
}

type WonLaneKnowledge = {
    Controller: PlayerID
    Troops: TroopKnowledge list
}

type LaneKnowledge =
| PreBaseFlipLaneKnowledge of PreBaseFlipLaneKnowledge
| ContestedLaneKnowledge of ContestedLaneKnowledge
| WonLaneKnowledge of WonLaneKnowledge
| TiedLaneKnowledge

type TurnDisplayInfo = {
    CurrentPlayer: PlayerID
    ActionsLeft: int
    BoardKnowledge: LaneKnowledge list
    PlayerHand: Hand
    OpponentHandSizes: (PlayerID * int) list
    DrawPileSize: int
    DiscardKnowledge: DeadCardKnowledge list
}

type DisplayInfo =
| TurnDisplayInfo of TurnDisplayInfo
| SwitchDisplayInfo of PlayerID

type TurnActionInfo =
| Play of PlayerID * CardID * Power * LaneID
| Activate of PlayerID * CardID * Power * LaneID * Health
| Attack of PlayerID * TroopID * CardID
| CreatePair of PlayerID * CardID * CardID * Power * LaneID * Health * Health

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
| WonGame of DisplayInfo * PlayerID

type API = {
    NewGame: unit -> ActionResult
}
