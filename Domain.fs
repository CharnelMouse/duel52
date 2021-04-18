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

type Health = int

type Readiness =
| Ready
| Exhausted

type Face =
| Up
| Down

type DrawCard = DrawCard of Power * CardID
type HandCard = HandCard of Power * CardID
type KnownHiddenCard = KnownHiddenCard of Power * CardID * Health * PlayerID
type UnknownHiddenCard = HiddenCard of CardID * Health * PlayerID
type Base = Base of Power * CardID * PlayerID
type RevealedCard = RevealedCard of Power * CardID * Health * Readiness * PlayerID
type DeadCard =
| UnknownDeadCard of Power
| KnownDeadCard of Power * Face
type RemovedCard = RemovedCard of Power

type Pair = Power * TroopID * (CardID * Health) * (CardID * Health) * Readiness * PlayerID

type Troop =
| UnknownHiddenCard of UnknownHiddenCard
| KnownHiddenCard of KnownHiddenCard
| RevealedCard of RevealedCard
| Pair of Pair

type ContestedLane = {
    Bases: Base list
    Troops: Troop list
}

type WonLane = {
    Controller: PlayerID
    Troops: Troop list
}

type Lane =
| Contested of ContestedLane
| Won of WonLane

type Hand = HandCard list

type DisplayInfo = {
    CurrentPlayer: PlayerID
    ActionsLeft: int
    RevealedBoard: Lane list
    PlayerHand: Hand
    OpponentHandSizes: int list
    DrawPileSize: int
    Discard: DeadCard list
}

type Action =
| Play of CardID
| FlipCard of CardID
| Attack of TroopID * CardID
| CreatePair of CardID * CardID

type ActionCapability = unit -> ActionResult
and NextActionInfo = {
    Action: Action
    PlayerID: PlayerID
    Capability: ActionCapability
}
and ActionResult =
| InProgress of DisplayInfo * NextActionInfo list
| Won of DisplayInfo * PlayerID

type API = {
    NewGame: unit -> ActionResult
}
