module Card
open PointFree
open Domain
open PowerMaps
open CardTypes

let getHandCardInfo {HandCardID = HandCardID id; Rank = r; Suit = s; Abilities = a} =
    HandCardInfo (id, r, s, a.Name)

let unitID = function
    | InactiveUnit {InactiveUnitID = InactiveUnitID cardID}
    | ActiveUnit {ActiveUnitID = ActiveUnitID cardID}
    | PairedUnit {FullPairedUnitID = FullPairedUnitID cardID} ->
        UnitID cardID

let (|Exhausted|Ready|) = function
    | Solo {ActionsSpent = spent; MaxActions = m}
    | Paired {ActionsSpent = spent; MaxActions = m} ->
        if spent >= m then
            Exhausted
        else
            Ready

let private extraHealth = function
    | InactiveUnit _ -> 0u<health>
    | ActiveUnit {Abilities = {WhileActive = passiveAbilities}}
    | PairedUnit {Abilities = {WhileActive = passiveAbilities}} ->
        passiveAbilities
        |> List.fold (fun state passiveAbility ->
            match passiveAbility with
            | MaxHealthIncrease increase ->
                state + (uint increase)*1u<health>
            | _ -> state
        ) 0u<health>
let maxHealth = extraHealth >> (+) 2u<health>
let cardDamage = function
    | InactiveUnit {Damage = damage}
    | ActiveUnit {Damage = damage}
    | PairedUnit {Damage = damage} -> damage
let isDead =
    dup
    >> opPair cardDamage maxHealth
    >> uncurry (>=)
let (|Dead|Alive|) =
    isDead
    >> (function | true -> Dead | false -> Alive)

let deckCard (cardID, rank, suit) = {
    DeckCardID = DeckCardID cardID
    Rank = rank
    Suit = suit
}
let handCard getAbilities (cardID, rank, suit) owner = {
    HandCardID = HandCardID cardID
    Rank = rank
    Suit = suit
    Abilities = getAbilities rank
    Owner = owner
}
let removedCard (cardID, rank, suit) = {
    RemovedCardID = RemovedCardID cardID
    Rank = rank
    Suit = suit
}

let deckToHandCard (getAbilities: GetAbilities) playerID : CardConverter<DeckCard, HandCard> = fun deckCard ->
    let {DeckCardID = DeckCardID id} = deckCard
    {
        HandCardID = HandCardID id
        Rank = deckCard.Rank
        Suit = deckCard.Suit
        Abilities = getAbilities deckCard.Rank
        Owner = playerID
    }
let handToDiscardedCard: CardConverter<HandCard, DiscardedCard> = fun handCard ->
    let {HandCardID = HandCardID id} = handCard
    FaceDownDiscardedCard {
        DiscardedCardID = DiscardedCardID id
        Rank = handCard.Rank
        Suit = handCard.Suit
        KnownBy = Set.singleton handCard.Owner
    }
let handToInactiveUnit: CardConverter<HandCard, InactiveUnit> = fun handCard ->
    let {HandCardID = HandCardID id} = handCard
    {
        InactiveUnitID = InactiveUnitID id
        Rank = handCard.Rank
        Suit = handCard.Suit
        Abilities = handCard.Abilities
        Owner = handCard.Owner
        KnownBy = Set.singleton handCard.Owner
        Damage = 0u<health>
        FreezeStatus = NotFrozen
    }
let baseToInactiveUnit card =
    let {BaseCardID = BaseCardID cardID} = card
    {
        InactiveUnitID = InactiveUnitID cardID
        Rank = card.Rank
        Suit = card.Suit
        Abilities = card.Abilities
        Owner = card.Owner
        KnownBy = card.KnownBy
        Damage = 0u<health>
        FreezeStatus = card.FreezeStatus
    }
let inactiveToActiveUnit: CardConverter<InactiveUnit, ActiveUnit> = fun inactiveUnit ->
    let {InactiveUnitID = InactiveUnitID id} = inactiveUnit
    {
        ActiveUnitID = ActiveUnitID id
        Rank = inactiveUnit.Rank
        Suit = inactiveUnit.Suit
        Abilities = inactiveUnit.Abilities
        Owner = inactiveUnit.Owner
        Damage = inactiveUnit.Damage
        ActionsSpent = 0u<action>
        MaxActions = 1u<action>
        FreezeStatus = inactiveUnit.FreezeStatus
    }
let unitToDiscardedCard: CardConverter<UnitCard, DiscardedCard> = function
    | InactiveUnit {InactiveUnitID = InactiveUnitID id; Owner = pid; Rank = rank; Suit = suit; KnownBy = knownBy} ->
        FaceDownDiscardedCard {
            DiscardedCardID = DiscardedCardID id
            Rank = rank
            Suit = suit
            KnownBy = Set.add pid knownBy // player checks unknown base in case it's a Trap
        }
    | ActiveUnit {ActiveUnitID = ActiveUnitID id; Rank = rank; Suit = suit}
    | PairedUnit {FullPairedUnitID = FullPairedUnitID id; Rank = rank; Suit = suit} ->
        FaceUpDiscardedCard {
            DiscardedCardID = DiscardedCardID id
            Rank = rank
            Suit = suit
        }
let pairToFullPairedUnits: CardConverter<Pair, FullPairedUnit * FullPairedUnit> = fun {Cards = cards; Rank = rank; Abilities = abilities; Owner = owner} ->
    let toFull card =
        let {PairedUnitID = PairedUnitID cardID} = card
        {
            FullPairedUnitID = FullPairedUnitID cardID
            Suit = card.Suit
            Damage = card.Damage
            ActionsSpent = card.ActionsSpent
            MaxActions = card.MaxActions
            FreezeStatus = card.FreezeStatus
            Rank = rank
            Abilities = abilities
            Owner = owner
        }
    opBoth toFull cards
let activeToPairedUnit: CardConverter<ActiveUnit, PairedUnit> = fun card ->
    let {ActiveUnitID = ActiveUnitID cardID} = card
    {
        PairedUnitID = PairedUnitID cardID
        Suit = card.Suit
        Damage = card.Damage
        ActionsSpent = card.ActionsSpent
        MaxActions = card.MaxActions
        FreezeStatus = card.FreezeStatus
    }
let fullPairedToPairedUnit: CardConverter<FullPairedUnit, PairedUnit> = fun card ->
    let {FullPairedUnitID = FullPairedUnitID cardID} = card
    {
        PairedUnitID = PairedUnitID cardID
        Suit = card.Suit
        Damage = card.Damage
        ActionsSpent = card.ActionsSpent
        MaxActions = card.MaxActions
        FreezeStatus = card.FreezeStatus
    }
let activeToFullPairedUnit: CardConverter<ActiveUnit, FullPairedUnit> = fun card ->
    let {ActiveUnitID = ActiveUnitID cardID} = card
    {
        FullPairedUnitID = FullPairedUnitID cardID
        Suit = card.Suit
        Damage = card.Damage
        ActionsSpent = card.ActionsSpent
        MaxActions = card.MaxActions
        FreezeStatus = card.FreezeStatus
        Rank = card.Rank
        Abilities = card.Abilities
        Owner = card.Owner
    }
let fullPairedToActiveUnit: CardConverter<FullPairedUnit, ActiveUnit> = fun card ->
    let {FullPairedUnitID = FullPairedUnitID cardID} = card
    {
        ActiveUnitID = ActiveUnitID cardID
        Rank = card.Rank
        Suit = card.Suit
        Abilities = card.Abilities
        Owner = card.Owner
        Damage = card.Damage
        ActionsSpent = card.ActionsSpent
        MaxActions = card.MaxActions
        FreezeStatus = card.FreezeStatus
    }
let activeUnitsToPair: CardConverter<ActiveUnit * ActiveUnit, Pair> = fun (card1, card2) ->
    if (card1.Rank, card1.Abilities, card1.Owner) <> (card2.Rank, card2.Abilities, card2.Owner) then
        failwithf "Paired cards don't share rank / abilities / owner"
    {
        Cards = activeToPairedUnit card1, activeToPairedUnit card2
        Rank = card1.Rank
        Abilities = card1.Abilities
        Owner = card1.Owner
    }
let fullPairedUnitsToPair: CardConverter<FullPairedUnit * FullPairedUnit, Pair> = fun (card1, card2) ->
    if (card1.Rank, card1.Abilities, card1.Owner) <> (card2.Rank, card2.Abilities, card2.Owner) then
        failwithf "Paired cards don't share rank / abilities / owner"
    {
        Cards = fullPairedToPairedUnit card1, fullPairedToPairedUnit card2
        Rank = card1.Rank
        Abilities = card1.Abilities
        Owner = card1.Owner
    }
let pairedToActiveUnit: CardConverter<Rank * Abilities * PlayerID * PairedUnit, ActiveUnit> = fun (rank, abilities, owner, card) ->
    let {PairedUnitID = PairedUnitID cardID} = card
    {
        ActiveUnitID = ActiveUnitID cardID
        Rank = rank
        Suit = card.Suit
        Abilities = abilities
        Owner = owner
        Damage = card.Damage
        ActionsSpent = card.ActionsSpent
        MaxActions = card.MaxActions
        FreezeStatus = card.FreezeStatus
    }

let private uSubtract amount n =
    n - min n amount

let healInactiveCard amount (inactiveCard: InactiveUnit) =
    {inactiveCard with Damage = uSubtract amount inactiveCard.Damage}
let healActiveCard amount (activeCard: ActiveUnit) =
    {activeCard with Damage = uSubtract amount activeCard.Damage}
let healPairedCard amount (pairedCard: PairedUnit) =
    {pairedCard with Damage = uSubtract amount pairedCard.Damage}
let fullyHealActiveCard (activeCard: ActiveUnit) =
    {activeCard with Damage = 0u<health>}
let decrementInactiveCardDamage = healInactiveCard 1u<health>
let decrementActiveCardDamage = healActiveCard 1u<health>
let decrementPairedCardDamage = healPairedCard 1u<health>

let getActionability = function
    | InactiveUnit {FreezeStatus = fs}
    | ActiveUnit {FreezeStatus = fs}
    | PairedUnit {FreezeStatus = fs} ->
        match fs with
        | FrozenBy _ -> Frozen
        | NotFrozen -> Normal

let getBaseKnowledge playerID (baseCard: BaseCard) =
    let (BaseCardID cardID) = baseCard.BaseCardID
    let actionability =
        match baseCard.FreezeStatus with
        | FrozenBy _ -> Frozen
        | NotFrozen -> Normal
    if Set.contains playerID baseCard.KnownBy then
        KnownBaseCard (cardID, baseCard.Owner, baseCard.Rank, baseCard.Suit, baseCard.Abilities.Name, actionability)
    else
        UnknownBaseCard (cardID, baseCard.Owner, actionability)

let getPairKnowledge pair : PairKnowledge =
    let (fullCard1, fullCard2) = pairToFullPairedUnits pair
    let ({FullPairedUnitID = FullPairedUnitID id1}, {FullPairedUnitID = FullPairedUnitID id2}) = (fullCard1, fullCard2)
    let actionability1 = getActionability (PairedUnit fullCard1)
    let actionability2 = getActionability (PairedUnit fullCard2)
    id1, id2,
    pair.Rank,
    fullCard1.Suit, fullCard2.Suit,
    pair.Abilities.Name,
    fullCard1.Damage, fullCard2.Damage,
    min (fullCard1.MaxActions - fullCard1.ActionsSpent) (fullCard2.MaxActions - fullCard2.ActionsSpent),
    actionability1, actionability2

let getDeadCardKnowledge playerID = function
    | FaceDownDiscardedCard {Rank = r; Suit = s; KnownBy = kb} ->
        if Set.contains playerID kb then
            KnownFaceDownDeadCard (r, s)
        else
            UnknownDeadCard
    | FaceUpDiscardedCard {Rank = r; Suit = s} ->
        KnownFaceUpDeadCard (r, s)
