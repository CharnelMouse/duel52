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
type HiddenCard = HiddenCard of Power * CardID * Health * PlayerID
type Base = Base of Power * CardID * PlayerID
type RevealedCard = RevealedCard of Power * CardID * Health * Readiness * PlayerID
type DeadCard = DeadCard of Power * CardID * Face
type RemovedCard = RemovedCard of Power

type Pair = Power * TroopID * (CardID * Health) * (CardID * Health) * Readiness * PlayerID

type Troop =
| HiddenCard of HiddenCard
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

type GameState = {
    Board: Lane list
    DrawPile: DrawCard list
    CurrentPlayer: PlayerID
    ActionsLeft: int
    Hands: Hand list
    Discard: DeadCard list
    Removed: RemovedCard list
}

type Action =
| Play of CardID
| FlipCard of CardID
| Attack of TroopID * CardID
| CreatePair of CardID * CardID
