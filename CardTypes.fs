namespace CardTypes
open Domain
open PowerMaps

type RemovedCardID = RemovedCardID of CardID
type DeckCardID = DeckCardID of CardID
type HandCardID = HandCardID of CardID
type BaseCardID = BaseCardID of CardID
type InactiveUnitID = InactiveUnitID of CardID
type ActiveUnitID = ActiveUnitID of CardID
type PairedUnitID = PairedUnitID of CardID
type FullPairedUnitID = FullPairedUnitID of CardID
type UnitID = UnitID of CardID
type DiscardedCardID = DiscardedCardID of CardID
type ForesightTargetID = ForesightTargetID of CardID

type FreezeStatus =
| FrozenBy of PlayerID
| NotFrozen

type RemovedCard = {
    RemovedCardID: RemovedCardID
    Rank: Rank
    Suit: Suit
}
type DeckCard = {
    DeckCardID: DeckCardID
    Rank: Rank
    Suit: Suit
}
type HandCard = {
    HandCardID: HandCardID
    Rank: Rank
    Suit: Suit
    Abilities: Abilities
    Owner: PlayerID
}
type BaseCard = {
    BaseCardID: BaseCardID
    Rank: Rank
    Suit: Suit
    Abilities: Abilities
    Owner: PlayerID
    KnownBy: PlayerID Set
}
type InactiveUnit = {
    InactiveUnitID: InactiveUnitID
    Rank: Rank
    Suit: Suit
    Abilities: Abilities
    Owner: PlayerID
    KnownBy: PlayerID Set
    Damage: Damage
    FreezeStatus: FreezeStatus
}
type ActiveUnit = {
    ActiveUnitID: ActiveUnitID
    Rank: Rank
    Suit: Suit
    Abilities: Abilities
    Owner: PlayerID
    Damage: Damage
    ActionsSpent: Actions
    MaxActions: Actions
    FreezeStatus: FreezeStatus
}
type PairedUnit = {
    PairedUnitID: PairedUnitID
    Suit: Suit
    Damage: Damage
    ActionsSpent: Actions
    MaxActions: Actions
    FreezeStatus: FreezeStatus
}
type FullPairedUnit = {
    FullPairedUnitID: FullPairedUnitID
    Rank: Rank
    Suit: Suit
    Abilities: Abilities
    Owner: PlayerID
    Damage: Damage
    ActionsSpent: Actions
    MaxActions: Actions
    FreezeStatus: FreezeStatus
}
type ActiveCard =
| Solo of ActiveUnit
| Paired of FullPairedUnit
type UnitCard =
| InactiveUnit of InactiveUnit
| ActiveUnit of ActiveUnit
| PairedUnit of FullPairedUnit
type Pair = {
    Cards: PairedUnit * PairedUnit
    Rank: Rank
    Abilities: Abilities
    Owner: PlayerID
}
type FaceDownDiscardedCard = {
    DiscardedCardID: DiscardedCardID
    Rank: Rank
    Suit: Suit
    KnownBy: PlayerID Set
}
type FaceUpDiscardedCard = {
    DiscardedCardID: DiscardedCardID
    Rank: Rank
    Suit: Suit
}
type DiscardedCard =
| FaceDownDiscardedCard of FaceDownDiscardedCard
| FaceUpDiscardedCard of FaceUpDiscardedCard
type CardConverter<'From, 'To> = 'From -> 'To
