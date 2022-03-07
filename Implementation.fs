module Implementation
open Domain
open NonEmptyList
open NonEmptyMap
open EventStack
open PowerMaps

let private createIDs start lst =
    [for i in 0u..uint (List.length lst - 1) -> start + LanguagePrimitives.UInt32WithMeasure i]

let private createIDsToLength start len =
    [for i in 0u..(len - 1u) -> start + LanguagePrimitives.UInt32WithMeasure i]
let private zipIDs (start: uint<'a>) lst =
    let IDs = [for i in 0u..uint (List.length lst - 1) -> start + LanguagePrimitives.UInt32WithMeasure i]
    List.zip IDs lst
let private createIDMap start lst =
    zipIDs start lst
    |> Map.ofList

type private RemovedCardID = RemovedCardID of CardID
type private DeckCardID = DeckCardID of CardID
type private HandCardID = HandCardID of CardID
type private BaseCardID = BaseCardID of CardID
type private InactiveUnitID = InactiveUnitID of CardID
type private ActiveUnitID = ActiveUnitID of CardID
type private UnitID = UnitID of CardID
type private DiscardedCardID = DiscardedCardID of CardID

type private ForesightTargetID = ForesightTargetID of CardID
type private AttackerIDs =
| SingleAttackerID of ActiveUnitID
| PairAttackerIDs of ActiveUnitID * ActiveUnitID
let private transferAttackerIDs attackerIDs =
    match attackerIDs with
    | SingleAttackerID (ActiveUnitID id) -> SingleCardID id
    | PairAttackerIDs (ActiveUnitID id1, ActiveUnitID id2) -> PairIDs (id1, id2)

type private RemovedCard = {
    RemovedCardID: RemovedCardID
    Rank: Rank
    Suit: Suit
}

type private DeckCard = {
    DeckCardID: DeckCardID
    Rank: Rank
    Suit: Suit
}

type private HandCard = {
    HandCardID: HandCardID
    Rank: Rank
    Suit: Suit
    Abilities: Abilities
    Owner: PlayerID
}
let private getHandCardInfo {HandCardID = HandCardID id; Rank = r; Suit = s; Abilities = a} =
    HandCardInfo (id, r, s, a.Name)

type private BaseCard = {
    BaseCardID: BaseCardID
    Rank: Rank
    Suit: Suit
    Abilities: Abilities
    Owner: PlayerID
    KnownBy: PlayerID Set
}
let private getBaseKnowledge playerID (baseCard: BaseCard) =
    if Set.contains playerID baseCard.KnownBy then
        KnownBaseCard (baseCard.Owner, baseCard.Rank, baseCard.Suit, baseCard.Abilities.Name)
    else
        UnknownBaseCard baseCard.Owner

type private FreezeStatus =
| FrozenBy of PlayerID
| NotFrozen

type private InactiveUnit = {
    InactiveUnitID: InactiveUnitID
    Rank: Rank
    Suit: Suit
    Abilities: Abilities
    Owner: PlayerID
    KnownBy: PlayerID Set
    Damage: Damage
    FreezeStatus: FreezeStatus
}

type private ActiveUnit = {
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

type private UnitCard =
| InactiveUnit of InactiveUnit
| ActiveUnit of ActiveUnit

type private Pair = ActiveUnit * ActiveUnit

type private FaceDownDiscardedCard = {
    DiscardedCardID: DiscardedCardID
    Rank: Rank
    Suit: Suit
    KnownBy: PlayerID Set
}

type private FaceUpDiscardedCard = {
    DiscardedCardID: DiscardedCardID
    Rank: Rank
    Suit: Suit
}

type private DiscardedCard =
| FaceDownDiscardedCard of FaceDownDiscardedCard
| FaceUpDiscardedCard of FaceUpDiscardedCard

let private (|Exhausted|Ready|) (card: ActiveUnit) =
    if card.ActionsSpent >= card.MaxActions then
        Exhausted
    else
        Ready

let private maxHealth card =
    match card with
    | InactiveUnit _ -> 2u<health>
    | ActiveUnit {Abilities = triggers} ->
        triggers.WhileActive
        |> List.fold (fun state trigger ->
            match trigger with
            | MaxHealthIncrease increase ->
                state + (uint increase)*1u<health>
            | _ -> state
        ) 2u<health>
let private (|Dead|Alive|) (card: UnitCard) =
    let damage =
        match card with
        | InactiveUnit {Damage = damage}
        | ActiveUnit {Damage = damage} -> damage
    if damage < maxHealth card then
        Alive
    else
        Dead
let private isDead (card: UnitCard) =
    match card with
    | Alive -> false
    | Dead -> true

type private CardConverter<'From, 'To> = 'From -> 'To

let private deckToHandCard (getAbilities: GetAbilities) playerID : CardConverter<DeckCard, HandCard> = fun deckCard ->
    let {DeckCardID = DeckCardID id} = deckCard
    {
        HandCardID = HandCardID id
        Rank = deckCard.Rank
        Suit = deckCard.Suit
        Abilities = getAbilities deckCard.Rank
        Owner = playerID
    }
let private handToDiscardedCard: CardConverter<HandCard, DiscardedCard> = fun handCard ->
    let {HandCardID = HandCardID id} = handCard
    FaceDownDiscardedCard {
        DiscardedCardID = DiscardedCardID id
        Rank = handCard.Rank
        Suit = handCard.Suit
        KnownBy = Set.singleton handCard.Owner
    }
let private handToInactiveUnit: CardConverter<HandCard, InactiveUnit> = fun handCard ->
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
let private inactiveToActiveUnit: CardConverter<InactiveUnit, ActiveUnit> = fun inactiveUnit ->
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
let private unitToDiscardedCard: CardConverter<UnitCard, DiscardedCard> = fun unitCard ->
    match unitCard with
    | InactiveUnit {InactiveUnitID = InactiveUnitID id; Owner = pid; Rank = rank; Suit = suit; KnownBy = knownBy} ->
        FaceDownDiscardedCard {
            DiscardedCardID = DiscardedCardID id
            Rank = rank
            Suit = suit
            KnownBy = Set.add pid knownBy // player checks unknown base in case it's a Trap
        }
    | ActiveUnit {ActiveUnitID = ActiveUnitID id; Rank = rank; Suit = suit} ->
        FaceUpDiscardedCard {
            DiscardedCardID = DiscardedCardID id
            Rank = rank
            Suit = suit
        }

type private Lane = {
    InactiveUnits: InactiveUnit list
    ActiveUnits: ActiveUnit list
    Pairs: Pair list
}

let private emptyLane = {
    InactiveUnits = List.empty
    ActiveUnits = List.empty
    Pairs = List.empty
}

type private LaneControl =
| Contested
| Empty
| Won of PlayerID

let private laneSolePresence (lane: Lane) =
    let cardOwners =
        (lane.InactiveUnits |> List.map (fun card -> card.Owner))
        @ (lane.ActiveUnits |> List.map (fun card -> card.Owner))
        @ (lane.Pairs |> List.collect (fun (card1, card2) -> [card1.Owner; card2.Owner]))
    let playerCounts =
        cardOwners
        |> List.countBy id
    match playerCounts with
    | [] -> Empty
    | [(controller, _)] -> Won controller
    | _ -> Contested

type private Board = {
    Lanes: Map<LaneID, Lane>
    Discard: DiscardedCard list
}

let private findDeadCardsInLane laneID board =
    let lane = Map.find laneID board.Lanes
    (lane.InactiveUnits |> List.map InactiveUnit |> List.filter isDead)
    @ (lane.ActiveUnits |> List.map ActiveUnit |> List.filter isDead)
    @ (lane.Pairs |> List.collect (fun (card1, card2) -> [ActiveUnit card1; ActiveUnit card2]) |> List.filter isDead)
let private findDeadCardIDsInLane laneID board =
    let lane = Map.find laneID board.Lanes
    (lane.InactiveUnits |> List.choose (fun card ->
        let {InactiveUnitID = InactiveUnitID id} = card
        if isDead (InactiveUnit card) then Some (UnitID id) else None
        ))
    @ (lane.ActiveUnits |> List.choose (fun card ->
        let {ActiveUnitID = ActiveUnitID id} = card
        if isDead (ActiveUnit card) then Some (UnitID id) else None
        ))
    @ (lane.Pairs |> List.collect (fun (card1, card2) -> [card1; card2]) |> List.choose (fun card ->
        let {ActiveUnitID = ActiveUnitID id} = card
        if isDead (ActiveUnit card) then Some (UnitID id) else None
        ))

type private EarlyGameInfo = {
    Bases: Map<LaneID, BaseCard list>
    DrawPile: DeckCard NonEmptyList
    HandCards: Map<PlayerID, HandCard list>
}

type private PostDrawGameInfo = {
    HandCards: Map<PlayerID, HandCard list>
    LaneWins: Map<LaneID, PlayerID>
}

type private PostHandGameInfo = {
    LaneWins: Map<LaneID, PlayerID>
}

type private GameStage =
| Early of EarlyGameInfo
| DrawPileEmpty of PostDrawGameInfo
| HandsEmpty of PostHandGameInfo

type private CardsState = {
    Board: Board
    GetAbilities: GetAbilities
    GameStage: GameStage
    Removed: RemovedCard Set
}

type private CardRemover<'T, 'TID> = 'TID -> LaneID -> CardsState -> 'T * CardsState
type private CardsRemover<'T, 'TID> = 'TID list -> LaneID -> CardsState -> 'T list * CardsState
type private CardAdder<'T> = 'T -> LaneID -> CardsState -> CardsState
type private CardsAdder<'T> = 'T list -> LaneID -> CardsState -> CardsState

let private removeCardFromInactiveUnits: CardRemover<InactiveUnit, InactiveUnitID> = fun (InactiveUnitID cardID) laneID cardsState ->
    let board = cardsState.Board
    let lane = Map.find laneID board.Lanes
    let removedCards, newInactiveUnits =
        lane.InactiveUnits
        |> List.partition (fun {InactiveUnitID = InactiveUnitID id} -> id = cardID)
    let newLane = {lane with InactiveUnits = newInactiveUnits}
    removedCards.Head,
    {cardsState with Board = {board with Lanes = board.Lanes |> Map.add laneID newLane}}
let private removeCardsFromInactiveUnits: CardsRemover<InactiveUnit, InactiveUnitID> = fun cardIDs laneID cardsState ->
    let board = cardsState.Board
    let lane = Map.find laneID board.Lanes
    let removed, newInactiveUnits =
        lane.InactiveUnits
        |> List.partition (fun {InactiveUnitID = id} -> List.contains id cardIDs)
    let newLane = {lane with InactiveUnits = newInactiveUnits}
    removed,
    {cardsState with Board = {board with Lanes = board.Lanes |> Map.add laneID newLane}}
let private removeCardsFromActiveUnits: CardsRemover<ActiveUnit, ActiveUnitID> = fun cardIDs laneID cardsState ->
    let board = cardsState.Board
    let lane = Map.find laneID board.Lanes
    let removed, newActiveUnits =
        lane.ActiveUnits
        |> List.partition (fun {ActiveUnitID = id} -> List.contains id cardIDs)
    let newLane = {lane with ActiveUnits = newActiveUnits}
    removed,
    {cardsState with Board = {board with Lanes = board.Lanes |> Map.add laneID newLane}}
let private removeCardsFromPairs: CardsRemover<ActiveUnit, ActiveUnitID> = fun cardIDs laneID cardsState ->
    let board = cardsState.Board
    let lane = Map.find laneID board.Lanes
    let removed, newPairs =
        lane.Pairs
        |> List.partition (fun ({ActiveUnitID = id1}, {ActiveUnitID = id2}) ->
            List.contains id1 cardIDs || List.contains id2 cardIDs
            )
    let flatRemoved = List.collect (fun (c1, c2) -> [c1; c2]) removed
    let newLane = {lane with Pairs = newPairs}
    flatRemoved,
    {cardsState with Board = {board with Lanes = board.Lanes |> Map.add laneID newLane}}
let private addCardToInactiveUnits: CardAdder<InactiveUnit> = fun card laneID cardsState ->
    let board = cardsState.Board
    let lane = Map.find laneID board.Lanes
    let newLane = {lane with InactiveUnits = lane.InactiveUnits @ [card]}
    {cardsState with Board = {board with Lanes = board.Lanes |> Map.add laneID newLane}}
let private addCardsToInactiveUnits: CardsAdder<InactiveUnit> = fun cards laneID cardsState ->
    let board = cardsState.Board
    let lane = Map.find laneID board.Lanes
    let newLane = {lane with InactiveUnits = lane.InactiveUnits @ cards}
    {cardsState with Board = {board with Lanes = board.Lanes |> Map.add laneID newLane}}
let private addCardToActiveUnits: CardAdder<ActiveUnit> = fun cardID laneID cardsState ->
    let board = cardsState.Board
    let lane = Map.find laneID board.Lanes
    let newLane = {lane with ActiveUnits = lane.ActiveUnits @ [cardID]}
    {cardsState with Board = {board with Lanes = board.Lanes |> Map.add laneID newLane}}
let private addCardsToActiveUnits: CardsAdder<ActiveUnit> = fun cards laneID cardsState ->
    let board = cardsState.Board
    let lane = Map.find laneID board.Lanes
    let newLane = {lane with ActiveUnits = lane.ActiveUnits @ cards}
    {cardsState with Board = {board with Lanes = board.Lanes |> Map.add laneID newLane}}
let private addCardsToPairs: CardAdder<ActiveUnit*ActiveUnit> = fun (card1, card2) laneID cardsState ->
    let board = cardsState.Board
    let lane = Map.find laneID board.Lanes
    let newLane = {lane with Pairs = lane.Pairs @ [card1, card2]}
    {cardsState with Board = {board with Lanes = board.Lanes |> Map.add laneID newLane}}
let private addCardPairPartnersToActiveUnits: CardsAdder<ActiveUnitID> = fun cardIDs laneID cardsState ->
    let board = cardsState.Board
    let lane = Map.find laneID board.Lanes
    let partners =
        lane.Pairs
        |> List.collect (fun (card1, card2) ->
            let {ActiveUnitID = id1} = card1
            let {ActiveUnitID = id2} = card1
            match List.contains id1 cardIDs, List.contains id2 cardIDs with
            | true, true ->
                []
            | true, false ->
                [card2]
            | false, true ->
                [card1]
            | false, false ->
                []
            )
    addCardsToActiveUnits partners laneID cardsState

let private removeCardsFromLane: CardsRemover<UnitCard, UnitID> = fun cardIDs laneID cardsState ->
    let removedInactive, cs1 =
        cardsState
        |> removeCardsFromInactiveUnits (cardIDs |> List.map (fun (UnitID id) -> InactiveUnitID id)) laneID
    let removedActive, cs2 =
        cs1
        |> removeCardsFromActiveUnits (cardIDs |> List.map (fun (UnitID id) -> ActiveUnitID id)) laneID
    let cs3 =
        cs2
        |> addCardPairPartnersToActiveUnits (cardIDs |> List.map (fun (UnitID id) -> ActiveUnitID id)) laneID
    let removedPaired, cs4 =
        cs3
        |> removeCardsFromPairs (cardIDs |> List.map (fun (UnitID id) -> ActiveUnitID id)) laneID
    let removed =
        (List.map InactiveUnit removedInactive)
        @ (List.map ActiveUnit removedActive)
        @ (List.map ActiveUnit removedPaired)
    removed, cs4
let private changeCardLane cardID fromLaneID toLaneID cardsState =
    let moved, cs1 =
        cardsState
        |> removeCardsFromLane [cardID] fromLaneID
    let movedInactive =
        moved
        |> List.choose (function
            | InactiveUnit c -> Some c
            | ActiveUnit _ -> None
            )
    let movedActive =
        moved
        |> List.choose (function
            | InactiveUnit _ -> None
            | ActiveUnit c -> Some c
            )
    cs1
    |> addCardsToInactiveUnits movedInactive toLaneID
    |> addCardsToActiveUnits movedActive toLaneID

let private healInactiveCard amount (inactiveCard: InactiveUnit) =
    {inactiveCard with Damage = max (inactiveCard.Damage - amount) 0u<health>}
let private healActiveCard amount (activeCard: ActiveUnit) =
    {activeCard with Damage = max (activeCard.Damage - amount) 0u<health>}
let private fullyHealActiveCard (activeCard: ActiveUnit) =
    {activeCard with Damage = 0u<health>}

let private flipAndActivateInactiveDeathPowersInLane laneID zeroHealthInactiveDeathPowerUnits cardsState =
    let inactiveIDs =
        zeroHealthInactiveDeathPowerUnits
        |> List.choose (fun card ->
            match card with
            | InactiveUnit {InactiveUnitID = cid} -> Some cid
            | ActiveUnit _ -> None
            )
    let removedCards, cs1 =
        cardsState
        |> removeCardsFromInactiveUnits inactiveIDs laneID
    let newCards =
        removedCards
        |> List.map (inactiveToActiveUnit >> fullyHealActiveCard)
    addCardsToActiveUnits newCards laneID cs1

let private damageCard (UnitID cardID) damage laneID cardsState =
    let board = cardsState.Board
    let lane = Map.find laneID board.Lanes
    let newLane = {
        lane with
            InactiveUnits =
                lane.InactiveUnits
                |> List.map (fun card ->
                    let {InactiveUnitID = InactiveUnitID id} = card
                    if id = cardID then {card with Damage = card.Damage + damage} else card
                    )
            ActiveUnits =
                lane.ActiveUnits
                |> List.map (fun card ->
                    let {ActiveUnitID = ActiveUnitID id} = card
                    if id = cardID then {card with Damage = card.Damage + damage} else card
                    )
            Pairs =
                lane.Pairs
                |> List.map (fun (card1, card2) ->
                    let {ActiveUnitID = ActiveUnitID id1} = card1
                    let {ActiveUnitID = ActiveUnitID id2} = card2
                    (if id1 = cardID then {card1 with Damage = card1.Damage + damage} else card1),
                    (if id2 = cardID then {card2 with Damage = card2.Damage + damage} else card2)
                    )
        }
    {cardsState with Board = {board with Lanes = board.Lanes |> Map.add laneID newLane}}

let private incrementCardActionsUsed cardID laneID cardsState =
    let board = cardsState.Board
    let lane = Map.find laneID board.Lanes
    let newLane = {
        lane with
            ActiveUnits =
                lane.ActiveUnits
                |> List.map (fun card ->
                    let {ActiveUnitID = ActiveUnitID id} = card
                    if id = cardID then
                        {card with ActionsSpent = card.ActionsSpent + 1u<action>}
                    else
                        card
                    )
            Pairs =
                lane.Pairs
                |> List.map (fun (card1, card2) ->
                    let {ActiveUnitID = ActiveUnitID id1} = card1
                    let {ActiveUnitID = ActiveUnitID id2} = card2
                    if id1 = cardID then
                        {card1 with ActionsSpent = card1.ActionsSpent + 1u<action>}, card2
                    elif id2 = cardID then
                        card1, {card2 with ActionsSpent = card2.ActionsSpent + 1u<action>}
                    else
                        card1, card2
                    )
        }
    {cardsState with Board = {board with Lanes = board.Lanes |> Map.add laneID newLane}}

let private setMaxCardActions cardID left laneID cardsState =
    let board = cardsState.Board
    let lane = Map.find laneID board.Lanes
    let newLane = {
        lane with
            ActiveUnits =
                lane.ActiveUnits
                |> List.map (fun card ->
                    let {ActiveUnitID = ActiveUnitID id} = card
                    if id = cardID then
                        {card with MaxActions = left}
                    else
                        card
                    )
            Pairs =
                lane.Pairs
                |> List.map (fun (card1, card2) ->
                    let {ActiveUnitID = ActiveUnitID id1} = card1
                    let {ActiveUnitID = ActiveUnitID id2} = card2
                    if id1 = cardID then
                        {card1 with MaxActions = left}, card2
                    elif id2 = cardID then
                        card1, {card2 with MaxActions = left}
                    else
                        card1, card2
                    )
            }
    {cardsState with Board = {board with Lanes = board.Lanes |> Map.add laneID newLane}}

let private makeCardKnown cardID playerID cardsState =
    let board = cardsState.Board
    let newLane _ lane = {
        lane with
            InactiveUnits =
                lane.InactiveUnits
                |> List.map (fun card ->
                    let {InactiveUnitID = InactiveUnitID id} = card
                    if id = cardID then
                        {card with KnownBy = card.KnownBy |> Set.add playerID}
                    else
                        card
                    )
            }
    let lanesUpdated = {cardsState with Board = {board with Lanes = board.Lanes |> Map.map newLane}}
    match lanesUpdated.GameStage with
    | Early stage ->
        let newBases =
            stage.Bases
            |> Map.map (fun _ cards ->
                cards
                |> List.map (fun card ->
                    let {BaseCardID = BaseCardID id} = card
                    if id = cardID then
                        {card with KnownBy = card.KnownBy |> Set.add playerID}
                    else
                        card
                    )
                )
        let newStage = Early {stage with Bases = newBases}
        {lanesUpdated with GameStage = newStage}
    | DrawPileEmpty _
    | HandsEmpty _ ->
        lanesUpdated

let private removeCardFromHand (HandCardID cardID) playerID (cardsState: CardsState) =
    match cardsState.GameStage with
    | Early gs ->
        let hands = gs.HandCards
        let removedCard, newHand =
            match Map.tryFind playerID hands with
            | Some lst -> lst |> List.partition (fun {HandCardID = HandCardID id} -> id = cardID)
            | None -> failwithf "Can't remove card from non-existant player's hand"
        let newHands =
            hands
            |> Map.change playerID (fun oldHand -> Some newHand)
        List.head removedCard,
        {cardsState with
            GameStage = Early {gs with HandCards = newHands}
            }
    | DrawPileEmpty gs ->
        let hands = gs.HandCards
        let removedCard, newHand =
            match Map.tryFind playerID hands with
            | Some lst -> lst |> List.partition (fun {HandCardID = HandCardID id} -> id = cardID)
            | None -> failwithf "Can't remove card from non-existant player's hand"
        let newHands =
            hands
            |> Map.change playerID (fun oldHand -> Some newHand)
        List.head removedCard,
        {cardsState with
            GameStage = DrawPileEmpty {gs with HandCards = newHands}
            }
    | HandsEmpty _ ->
        failwithf "Can't remove card when hands are empty"
let private removeHandsIfAllEmpty (cardsState: CardsState) =
    match cardsState.GameStage with
    | DrawPileEmpty {HandCards = handCards; LaneWins = laneWins} ->
        if Map.forall (fun _ cards -> List.isEmpty cards) handCards then
            {cardsState with GameStage = HandsEmpty {LaneWins = laneWins}}
        else
            cardsState
    | Early _
    | HandsEmpty _ ->
        cardsState
let private flipBasesOnLane (bases: BaseCard list, lane: Lane) =
    let newUnits =
        bases
        |> List.map (fun baseCard ->
            let {BaseCardID = BaseCardID id} = baseCard
            {
                InactiveUnitID = InactiveUnitID id
                Rank = baseCard.Rank
                Suit = baseCard.Suit
                Abilities = baseCard.Abilities
                Owner = baseCard.Owner
                KnownBy = baseCard.KnownBy
                Damage = 0u<health>
                FreezeStatus = NotFrozen
                }
            )
    {lane with InactiveUnits = lane.InactiveUnits @ newUnits}
let private joinMaps map1 map2 = // left join
    map1
    |> Map.map (fun key value -> (value, Map.find key map2))
let private flipBasesOnBoard bases ({Lanes = lanes; Discard = discard}: Board) =
    {
        Lanes =
            joinMaps bases lanes
            |> Map.map (fun _ bl -> flipBasesOnLane bl)
        Discard = discard
        }

type private PlayerReady = {
    Player: PlayerID
    NPlayers: uint
    Actions: Actions
    FutureActionCounts: Actions list
}

type private TurnInProgress = {
    CurrentPlayer: PlayerID
    NPlayers: uint
    ActionsLeft: Actions
    FutureActionCounts: Actions list
}

type private TriggerEvent = InstantNonTargetAbility * PlayerID * LaneID * CardID

type private ResolutionEpoch =
| OrderChoiceEpoch of ActivationPowerContext epoch
| OrderedTriggerEpoch of TriggerEvent epoch
| MidAbilityChoice of MidActivationPowerChoiceContext

type private ResolutionStack = ResolutionEpoch nonEmptyList

type private MidActivationPowerChoice = {
    ChoiceContext: MidActivationPowerChoiceContext
    ResolutionStack: ResolutionStack option
}

type private StackChoice = {
    EpochEvents: NonEmptyMap<EventID, ActivationPowerContext>
    ResolutionStack: ResolutionStack option
}

type private TurnStage =
| MidPassivePowerChoice of MidPassivePowerChoiceContext
| MidActivationPowerChoice of MidActivationPowerChoice
| StackChoice of StackChoice
| ActionChoice

type private GameStateBetweenTurns = {
    CardsState: CardsState
    TurnState: PlayerReady
}

type private GameStateDuringTurn = {
    CardsState: CardsState
    TurnState: TurnInProgress
    TurnStage: TurnStage
}

type private GameStateWon = {
    Lanes: Map<LaneID, Lane>
    Winner: PlayerID
    LaneWins: Map<LaneID, PlayerID>
}

type private GameStateTied = {
    Lanes: Map<LaneID, Lane>
    LaneWins: Map<LaneID, PlayerID>
}

type private GameState =
| GameStateDuringTurn of GameStateDuringTurn
| GameStateBetweenTurns of GameStateBetweenTurns
| GameStateWon of GameStateWon
| GameStateTied of GameStateTied

let private incrementActionsLeft (gameState: GameStateDuringTurn) =
    {gameState with
        TurnState = {
            gameState.TurnState with
                ActionsLeft = gameState.TurnState.ActionsLeft + 1u<action>
        }
    }

let private changeCardsState (gameState: GameStateDuringTurn) newCardsState =
    {gameState with CardsState = newCardsState}
let private addMidPowerChoiceContext context gameState =
    {
        CardsState = gameState.CardsState
        TurnState = gameState.TurnState
        TurnStage = MidActivationPowerChoice {
            ChoiceContext = context
            ResolutionStack = None
        }
    }

let private flipInactiveCardsInLaneAndAddActivationPowersToStack playerID laneID cardID cardsState turnState =
    let lane = Map.find laneID cardsState.Board.Lanes
    let flippingIDs =
        lane.InactiveUnits
        |> List.choose (fun card -> if card.Owner = playerID then Some card.InactiveUnitID else None)
    let removedCards, cs1 =
        cardsState
        |> removeCardsFromInactiveUnits flippingIDs laneID
    let flippedCards =
        removedCards
        |> List.map inactiveToActiveUnit
    let cs2 =
        cs1
        |> addCardsToActiveUnits flippedCards laneID
    let powerChoices =
        flippedCards
        |> List.choose (fun {ActiveUnitID = ActiveUnitID id; Abilities = abilities} ->
            match abilities.OnActivation with
            | [] -> None
            | _ -> Some (ActivationPowerContext (playerID, laneID, id, abilities.Name))
        )
        |> tryFromList
    match powerChoices with
    | None ->
        cs2, turnState, None
    | Some pc -> cs2, turnState, Some (OrderChoiceEpoch pc)

let private toActionChoice gameState =
    {
        CardsState = gameState.CardsState
        TurnState = gameState.TurnState
        TurnStage = ActionChoice
    }
let private triggerTargetInactiveDeathPowers gameState =
    let cardsState = gameState.CardsState
    let board = cardsState.Board
    let laneIDs =
        board.Lanes
        |> Map.toList
        |> List.map (fun (id, _) -> id)
    let zeroHealthInactiveDeathPowerUnits =
        laneIDs
        |> List.collect (fun id -> findDeadCardsInLane id board)
        |> List.filter (fun card ->
            match card with
            | InactiveUnit {Abilities = a} ->
                match a.OnInactiveDying with
                | [] ->
                    false
                | _ ->
                    true
            | ActiveUnit _ ->
                false
            )
    laneIDs
    |> List.fold (fun cs laneID -> flipAndActivateInactiveDeathPowersInLane laneID zeroHealthInactiveDeathPowerUnits cs) cardsState
    |> changeCardsState gameState
let private decrementCardDamage card =
    if card.Damage = 0u<health> then
        card
    else
        {card with Damage = card.Damage - 1u<health>}
let private healAttackersIfDefenderDying (attackerIDs: AttackerIDs) (UnitID attackedID) laneID gameState =
    let board = gameState.CardsState.Board
    let lane = Map.find laneID board.Lanes
    let dyingCardIDs = findDeadCardIDsInLane laneID board
    if List.contains (UnitID attackedID) dyingCardIDs then
        match attackerIDs with
        | SingleAttackerID id ->
            let activeUnits =
                lane.ActiveUnits
                |> List.map (fun card ->
                    if card.ActiveUnitID = id then
                        decrementCardDamage card
                    else
                        card
                    )
            let newLane = {lane with ActiveUnits = activeUnits}
            let newLanes = Map.add laneID newLane board.Lanes
            let newBoard = {board with Lanes = newLanes}
            {gameState.CardsState with Board = newBoard}
            |> changeCardsState gameState
        | PairAttackerIDs (id1, id2) ->
            let pairs =
                lane.Pairs
                |> List.map (fun (card1, card2) ->
                    if List.contains card1.ActiveUnitID [id1; id2] then
                        decrementCardDamage card1,
                        decrementCardDamage card2
                    else
                        (card1, card2)
                    )
            let newLane = {lane with Pairs = pairs}
            let newLanes = Map.add laneID newLane board.Lanes
            let newBoard = {board with Lanes = newLanes}
            {gameState.CardsState with Board = newBoard}
            |> changeCardsState gameState
    else
        gameState
let private moveDeadCardsToDiscard gameState =
    let cardsState = gameState.CardsState
    let board = cardsState.Board
    let laneIDs =
        board.Lanes
        |> Map.toList
        |> List.map (fun (id, _) -> id)
    let removed, newCardsState =
        laneIDs
        |> List.fold (fun (removed, cs) id ->
            let deadCardIDs = findDeadCardIDsInLane id cs.Board
            let newRemoved, newCS = removeCardsFromLane deadCardIDs id cs
            (removed @ newRemoved), newCS
            ) ([], cardsState)
    let discardCards = List.map unitToDiscardedCard removed
    let newDiscard = {newCardsState.Board with Discard = (newCardsState.Board.Discard @ discardCards)}
    {cardsState with Board = newDiscard}
    |> changeCardsState gameState
let private updateLaneWins gameState =
    match gameState.CardsState.GameStage with
    | Early _ ->
        gameState
    | DrawPileEmpty gs ->
        let lanes = gameState.CardsState.Board.Lanes
        let currentLaneWins =
            lanes
            |> Map.toList
            |> List.choose (fun (laneID, lane) ->
                match laneSolePresence lane with
                | Contested
                | Empty -> None
                | Won controller -> Some (laneID, controller)
                )
            |> Map.ofList
        let newGameStage = DrawPileEmpty {gs with LaneWins = currentLaneWins}
        let newCardsState = {gameState.CardsState with GameStage = newGameStage}
        newCardsState
        |> changeCardsState gameState
    | HandsEmpty gs ->
        let lanes = gameState.CardsState.Board.Lanes
        let currentLaneWins =
            lanes
            |> Map.toList
            |> List.choose (fun (laneID, lane) ->
                match laneSolePresence lane with
                | Contested
                | Empty -> None
                | Won controller -> Some (laneID, controller)
                )
            |> Map.ofList
        let newGameStage = HandsEmpty {gs with LaneWins = currentLaneWins}
        let newCardsState = {gameState.CardsState with GameStage = newGameStage}
        newCardsState
        |> changeCardsState gameState
let private checkForGameEnd gameState =
    match gameState.TurnStage with
    | MidActivationPowerChoice _
    | MidPassivePowerChoice _
    | StackChoice _ ->
        GameStateDuringTurn gameState
    | ActionChoice ->
        let cs = gameState.CardsState
        match cs.GameStage with
        | Early _
        | DrawPileEmpty _ ->
            GameStateDuringTurn gameState
        | HandsEmpty {LaneWins = laneWins} ->
            let lanes = cs.Board.Lanes
            let wonLaneCounts =
                laneWins
                |> Map.toList
                |> List.countBy (fun (laneID, playerID) -> playerID)
            match wonLaneCounts with
            | [] -> GameStateDuringTurn gameState
            | lst ->
                let (leadingPlayer, leadingWins) =
                    lst
                    |> List.maxBy (fun (_, n) -> n)
                if leadingWins >= 2 then
                    GameStateWon {Winner = leadingPlayer; Lanes = lanes; LaneWins = laneWins}
                else
                    let contestedLanes =
                        lanes
                        |> Map.filter (fun laneID lane ->
                            let laneIsEmpty =
                                List.isEmpty lane.InactiveUnits
                                && List.isEmpty lane.ActiveUnits
                                && List.isEmpty lane.Pairs
                            match Map.containsKey laneID laneWins, laneIsEmpty with
                            | false, false ->
                                true
                            | false, true
                            | true, false // shouldn't happen?
                            | true, true ->
                                false
                            )
                    if Map.isEmpty contestedLanes then
                        GameStateTied {Lanes = lanes; LaneWins = laneWins}
                    else
                        GameStateDuringTurn gameState

let rec private shuffleRec unshuffled shuffled (sampler: System.Random) =
    match unshuffled with
    | [] -> shuffled
    | _ ->
        let index = sampler.Next(List.length unshuffled)
        let newUnshuffled =
            unshuffled
            |> List.indexed
            |> List.choose (function
                | n, _ when n = index -> None
                | _, el -> Some el 
            )
        shuffleRec newUnshuffled (unshuffled.[index] :: shuffled) sampler

let private shuffle lst =
    let sampler = System.Random()
    shuffleRec lst [] sampler

let private createUnshuffledDeck () : (Rank * Suit) list =
    let toRank (caseInfo: Reflection.UnionCaseInfo) =
        Reflection.FSharpValue.MakeUnion(caseInfo, [||]) :?> Rank
    let toSuit (caseInfo: Reflection.UnionCaseInfo) =
        Reflection.FSharpValue.MakeUnion(caseInfo, [||]) :?> Suit
    let ranks =
        Reflection.FSharpType.GetUnionCases typeof<Rank>
        |> Array.toList
        |> List.map toRank
    let suits =
        Reflection.FSharpType.GetUnionCases typeof<Suit>
        |> Array.toList
        |> List.map toSuit
    List.allPairs ranks suits

let private prepareHead fn n lst =
    let h, t = List.splitAt (int n) lst
    fn h, t

let private prepareBases getAbilities (nLanes: uint) lst =
    lst
    |> List.splitInto (int nLanes)
    |> createIDMap 1u<LID>
    |> Map.map (fun _ cardInfo ->
        cardInfo
        |> zipIDs 1u<PID>
        |> List.map (fun (ownerID, (cardID, rank, suit)) ->
            {
                BaseCardID = BaseCardID cardID
                Rank = rank
                Suit = suit
                Abilities = getAbilities rank
                Owner = ownerID
                KnownBy = Set.empty
            }
        )
    )

let private prepareHands getAbilities nPlayers lst =
    let playerIDs =
        createIDsToLength 1u<PID> nPlayers
        |> List.collect (List.replicate 5)
    List.zip playerIDs lst
    |> List.groupBy (fun (playerID, _) -> playerID)
    |> List.map (fun (playerID, lst) ->
        playerID,
        lst
        |> List.map (fun (playerID, (cardID, rank, suit)) -> {
                HandCardID = HandCardID cardID
                Owner = playerID
                Rank = rank
                Suit = suit
                Abilities = getAbilities rank
            })
        )
    |> Map.ofList

let private prepareRemoved lst =
    lst
    |> List.map (fun (cardID, rank, suit) -> {
        RemovedCardID = (RemovedCardID cardID)
        Rank = rank
        Suit = suit
        })
    |> Set.ofList

let private getActionability ({FreezeStatus = fs}: ActiveUnit) =
    match fs with
    | FrozenBy _ -> Frozen
    | NotFrozen -> Normal

let private getPairKnowledge (card1, card2) : PairKnowledge =
    let {ActiveUnitID = ActiveUnitID id1} = card1
    let {ActiveUnitID = ActiveUnitID id2} = card2
    let actionability1 = getActionability card1
    let actionability2 = getActionability card2
    id1, id2,
    card1.Rank,
    card1.Suit, card2.Suit,
    card1.Abilities.Name,
    card1.Damage, card2.Damage,
    min (card1.MaxActions - card1.ActionsSpent) (card2.MaxActions - card2.ActionsSpent),
    actionability1, actionability2

let private getLanePlayerTroopKnowledges viewerID ownerID (lane: Lane) : InactiveUnitKnowledge list * ActiveUnitKnowledge list * PairKnowledge list =
    let pairsKnowledge =
        lane.Pairs
        |> List.filter (fun (card1, card2) -> card1.Owner = ownerID)
        |> List.map getPairKnowledge
    let nonPairedActiveUnitKnowledge =
        lane.ActiveUnits
        |> List.filter (fun card -> card.Owner = ownerID)
        |> List.map (fun card ->
            let {ActiveUnitID = ActiveUnitID id} = card
            let actionability = getActionability card
            ((id, card.Rank, card.Suit, card.Abilities.Name, card.Damage, card.MaxActions - card.ActionsSpent, actionability): ActiveUnitKnowledge)
            )
    let inactiveUnitKnowledge =
        lane.InactiveUnits
        |> List.filter (fun card -> card.Owner = ownerID)
        |> List.map (fun card ->
            let {InactiveUnitID = InactiveUnitID id} = card
            let actionability = if card.FreezeStatus = NotFrozen then Normal else Frozen
            if Set.contains viewerID card.KnownBy then
                KnownInactiveCardKnowledge (id, card.Rank, card.Suit, card.Abilities.Name, card.Damage, actionability)
            else
                UnknownInactiveCardKnowledge (id, card.Damage, actionability)
            )
    inactiveUnitKnowledge,
    nonPairedActiveUnitKnowledge,
    pairsKnowledge

let private getDeadCardKnowledge (playerID: PlayerID) (card: DiscardedCard) =
    match card with
    | FaceDownDiscardedCard {Rank = r; Suit = s; KnownBy = kb} ->
        if Set.contains playerID kb then
            KnownFaceDownDeadCard (r, s)
        else
            UnknownDeadCard
    | FaceUpDiscardedCard {Rank = r; Suit = s} ->
        KnownFaceUpDeadCard (r, s)

let private getPlayerLaneWins (laneWins: Map<LaneID, PlayerID>) =
    laneWins
    |> Map.toList
    |> List.groupBy (fun (_, pid) -> pid)
    |> List.map (fun (key, pairs) ->
        key,
        pairs
        |> List.map (fun (lid, _) -> lid)
        )

let private getBoardKnowledge viewerID cardsState turnInProgress =
    let ({Lanes = l; Discard = d}: Board) = cardsState.Board
    let getBase = getBaseKnowledge viewerID
    let getDeadCard = getDeadCardKnowledge viewerID
    let getLanePlayerTroops = getLanePlayerTroopKnowledges viewerID
    match cardsState.GameStage with
    | Early {Bases = b; DrawPile = dp} ->
        let lanesKnowledge =
            joinMaps b l
            |> Map.map (fun _ (bases, lane) ->
                let troopKnowledge =
                    createIDsToLength 1u<PID> turnInProgress.NPlayers
                    |> List.map (fun playerID ->
                        playerID, getLanePlayerTroops playerID lane
                        )
                    |> Map.ofList
                {
                    Bases = List.map getBase bases
                    Troops = troopKnowledge
                    } : PreBaseFlipLaneKnowledge
                )
        let drawPileSize = NonEmptyList.length dp
        PreBaseFlipBoardKnowledge {
            Lanes = lanesKnowledge
            DrawPileSize = drawPileSize
            Discard = List.map getDeadCard d
            }
    | DrawPileEmpty {LaneWins = laneWins} ->
        let lanesKnowledge =
            l
            |> Map.map (fun laneID lane ->
                let troopKnowledge =
                    createIDsToLength 1u<PID> turnInProgress.NPlayers
                    |> List.map (fun playerID ->
                        playerID, getLanePlayerTroops playerID lane
                        )
                    |> Map.ofList
                match Map.tryFind laneID laneWins with
                | None ->
                    ContestedLaneKnowledge {
                        Troops = troopKnowledge
                        }
                | Some c ->
                    WonLaneKnowledge {
                        Controller = c
                        Troops = troopKnowledge
                        }
                )
        PostBaseFlipBoardKnowledge {
            Lanes = lanesKnowledge
            Discard = List.map getDeadCard d
            }
    | HandsEmpty {LaneWins = laneWins} ->
        let lanesKnowledge =
            l
            |> Map.map (fun laneID lane ->
                let troopKnowledge =
                    createIDsToLength 1u<PID> turnInProgress.NPlayers
                    |> List.map (fun playerID ->
                        playerID, getLanePlayerTroops playerID lane
                        )
                    |> Map.ofList
                match Map.tryFind laneID laneWins with
                | None ->
                    ContestedLaneKnowledge {
                        Troops = troopKnowledge
                        }
                | Some c ->
                    WonLaneKnowledge {
                        Controller = c
                        Troops = troopKnowledge
                        }
                )
        PostBaseFlipBoardKnowledge {
            Lanes = lanesKnowledge
            Discard = List.map getDeadCard d
            }

let private getHandInfos viewerID gameStage =
    let (playerHand, opponentHands) =
        match gameStage with
        | Early {HandCards = hco}
        | DrawPileEmpty {HandCards = hco} ->
            hco
            |> Map.partition (fun owner _ -> owner = viewerID)
        | HandsEmpty _ ->
            Map.empty, Map.empty
    let playerHandInfo =
        playerHand
        |> Map.toList
        |> List.collect (fun (_, cards) -> cards |> List.map getHandCardInfo)
    let opponentHandSizes =
        opponentHands
        |> Map.map (fun _ cards -> List.length cards)
        |> Map.toList
    playerHandInfo, opponentHandSizes

let private getDisplayInfo gameState =
    match gameState with
    | GameStateDuringTurn {CardsState = cs; TurnState = ts; TurnStage = MidPassivePowerChoice tg} ->
        let id = ts.CurrentPlayer
        let (playerHandInfo, opponentHandSizes) = getHandInfos id cs.GameStage
        let boardKnowledge = getBoardKnowledge id cs ts
        MidPassivePowerChoiceDisplayInfo {
            CurrentPlayer = id
            ActionsLeft = ts.ActionsLeft
            BoardKnowledge = boardKnowledge
            PlayerHand = playerHandInfo
            OpponentHandSizes = opponentHandSizes
            ChoiceContext = tg
        }
    | GameStateDuringTurn {CardsState = cs; TurnState = ts; TurnStage = MidActivationPowerChoice tg} ->
        let id = ts.CurrentPlayer
        let (playerHandInfo, opponentHandSizes) = getHandInfos id cs.GameStage
        let boardKnowledge = getBoardKnowledge id cs ts
        MidActivationPowerChoiceDisplayInfo {
            CurrentPlayer = id
            ActionsLeft = ts.ActionsLeft
            BoardKnowledge = boardKnowledge
            PlayerHand = playerHandInfo
            OpponentHandSizes = opponentHandSizes
            ChoiceContext = tg.ChoiceContext
        }
    | GameStateDuringTurn {CardsState = cs; TurnState = ts; TurnStage = StackChoice tg} ->
        let id = ts.CurrentPlayer
        let (playerHandInfo, opponentHandSizes) = getHandInfos id cs.GameStage
        let boardKnowledge = getBoardKnowledge id cs ts
        StackChoiceDisplayInfo {
            CurrentPlayer = id
            ActionsLeft = ts.ActionsLeft
            BoardKnowledge = boardKnowledge
            PlayerHand = playerHandInfo
            OpponentHandSizes = opponentHandSizes
            Stack = {
                Head = tg.EpochEvents |> NonEmptyMap.toMap |> Map.toList |> List.map snd |> NonEmptyList.fromList
                Tail =
                    match tg.ResolutionStack with
                    | None -> []
                    | Some rs ->
                        rs
                        |> NonEmptyList.toList
                        |> List.choose (function
                            | OrderChoiceEpoch e -> Some e
                            | _ -> None
                            )
                }
        }
    | GameStateDuringTurn {CardsState = cs; TurnState = ts; TurnStage = ActionChoice} ->
        let id = ts.CurrentPlayer
        let (playerHandInfo, opponentHandSizes) = getHandInfos id cs.GameStage
        let boardKnowledge = getBoardKnowledge id cs ts
        TurnDisplayInfo {
            CurrentPlayer = id
            ActionsLeft = ts.ActionsLeft
            BoardKnowledge = boardKnowledge
            PlayerHand = playerHandInfo
            OpponentHandSizes = opponentHandSizes
        }
    | GameStateBetweenTurns {TurnState = ts} ->
        SwitchDisplayInfo ts.Player
    | GameStateWon {Winner = winner; LaneWins = laneWins} ->
        WonGameDisplayInfo {
            Winner = winner
            LaneWins = getPlayerLaneWins laneWins
        }
    | GameStateTied {LaneWins = laneWins} ->
        TiedGameDisplayInfo {
            LaneWins = getPlayerLaneWins laneWins
        }

let private getPlayActionsInfo gameState =
    let playerID = gameState.TurnState.CurrentPlayer
    match gameState.CardsState.GameStage with
    | Early {HandCards = hc}
    | DrawPileEmpty {HandCards = hc} ->
        Map.find playerID hc
    | HandsEmpty _ ->
        List.empty
    |> List.allPairs (createIDsToLength 1u<LID> (gameState.CardsState.Board.Lanes |> Map.count |> uint))
    |> List.map (fun (laneID, {HandCardID = HandCardID id}) ->
        Play (playerID, id, laneID)
        )

let private getActivateActionsInfo gameState =
    let playerID = gameState.TurnState.CurrentPlayer
    let lanes = gameState.CardsState.Board.Lanes
    lanes
    |> Map.toList
    |> List.collect (fun (laneID, lane) ->
        let ownTroops =
            lane.InactiveUnits
            |> List.filter (fun card -> card.Owner = playerID)
        ownTroops
        |> List.filter (fun card -> card.FreezeStatus = NotFrozen)
        |> List.map (fun card ->
            let {InactiveUnitID = InactiveUnitID id} = card
            Activate (playerID, laneID, id)
            )
        )

let private getPairActionsInfoFromUnits playerID laneID (ownActiveUnits: ActiveUnit list) =
    let rec distPairs lst =
        match lst with
        | [] -> []
        | [_] -> []
        | h::t -> List.allPairs [h] t @ distPairs t
    ownActiveUnits
    |> distPairs
    |> List.choose (fun (card1, card2) ->
        if card1.Rank = card2.Rank then
            let {ActiveUnitID = ActiveUnitID id1} = card1
            let {ActiveUnitID = ActiveUnitID id2} = card2
            CreatePair (playerID, laneID, id1, id2)
            |> Some
        else
            None
        )

let private getAttackActionsInfo gameState =
    let playerID = gameState.TurnState.CurrentPlayer
    let lanes = gameState.CardsState.Board.Lanes
    lanes
    |> Map.toList
    |> List.collect (fun (laneID, lane) ->
        let pairToList (x, y) = [x; y]
        let findOwner (toList: 'a -> UnitCard list) x =
            let first = toList >> List.head
            match (first x) with
            | InactiveUnit {Owner = owner} -> owner
            | ActiveUnit {Owner = owner} -> owner

        let possibleAttackers toList lst =
            lst
            |> List.filter (fun attacker ->
                attacker
                |> toList
                |> List.forall (fun (attackerCard: ActiveUnit) ->
                    attackerCard.Owner = playerID
                    && match attackerCard with
                        | Ready ->
                            attackerCard.FreezeStatus = NotFrozen
                        | Exhausted ->
                            false
                        )
                )
        let possibleUnitAttackers = possibleAttackers List.singleton lane.ActiveUnits
        let possiblePairAttackers = possibleAttackers pairToList lane.Pairs

        let addTypeAndOwner T owner (x: UnitCard) =
            let id =
                match x with
                | InactiveUnit {InactiveUnitID = InactiveUnitID cid}
                | ActiveUnit {ActiveUnitID = ActiveUnitID cid} -> cid
            T (owner, id)
        let possibleTypeTargets toList T ids =
            let transform owner = toList >> List.map (addTypeAndOwner T owner)
            ids
            |> List.groupBy (findOwner toList)
            |> List.filter (fun (owner, _) -> owner <> playerID)
            |> List.collect (fun (owner, units) ->
                units
                |> List.collect (transform owner)
                )
        let possibleSingleTypeTargets T cardIDs = possibleTypeTargets List.singleton T cardIDs
        let possiblePairTypeTargets T pairIDs = possibleTypeTargets pairToList T pairIDs
        let possibleInactiveUnitTargets = possibleSingleTypeTargets InactiveTarget (lane.InactiveUnits |> List.map InactiveUnit)
        let possibleActiveUnitTargets = possibleSingleTypeTargets ActiveSingleTarget (lane.ActiveUnits |> List.map ActiveUnit)
        let possiblePairTargets = possiblePairTypeTargets ActivePairMemberTarget (lane.Pairs |> List.map (fun (c1, c2) -> ActiveUnit c1, ActiveUnit c2))
        let allTargets = possibleInactiveUnitTargets @ possibleActiveUnitTargets @ possiblePairTargets

        let tauntTargets =
            allTargets
            |> List.filter (fun target ->
                match target with
                | InactiveTarget _ ->
                    false
                | ActiveSingleTarget (_, cardID) ->
                    let card =
                        lane.ActiveUnits
                        |> List.find (fun {ActiveUnitID = ActiveUnitID id} -> id = cardID)
                    List.contains ProtectsNonTauntAlliesInLane card.Abilities.WhileActive
                | ActivePairMemberTarget (_, cardID) ->
                    let card =
                        lane.Pairs
                        |> List.unzip
                        |> (fun (lst1, lst2) -> lst1 @ lst2)
                        |> List.find (fun {ActiveUnitID = ActiveUnitID id} -> id = cardID)
                    List.contains ProtectsNonTauntAlliesInLane card.Abilities.WhileActive
            )

        let availableTargets =
            if List.isEmpty tauntTargets then
                allTargets
            else
                tauntTargets

        let singleAttacks =
            possibleUnitAttackers
            |> List.collect (fun attacker ->
                let {ActiveUnitID = ActiveUnitID attackerID} = attacker
                availableTargets
                |> List.map (fun target ->
                    SingleAttack (playerID, laneID, attackerID, target)
                    )
            )
        let pairAttacks =
            possiblePairAttackers
            |> List.collect (fun (attacker1, attacker2) ->
                let {ActiveUnitID = ActiveUnitID attackerID1; Abilities = abilities} = attacker1
                let {ActiveUnitID = ActiveUnitID attackerID2} = attacker2
                availableTargets
                |> List.map (fun target ->
                    PairAttack (playerID, laneID, (attackerID1, attackerID2), target)
                )
            )
        singleAttacks @ pairAttacks
        )

let private getPairActionsInfo gameState =
    let playerID = gameState.TurnState.CurrentPlayer
    let lanes = gameState.CardsState.Board.Lanes
    lanes
    |> Map.toList
    |> List.collect (fun (laneID, {ActiveUnits = activeUnits}) ->
        activeUnits
        |> List.filter (fun card -> card.Owner = playerID)
        |> getPairActionsInfoFromUnits playerID laneID
        )

type private ActionPair =
| MidActivationPowerChoicePair of CardsState * TurnInProgress * MidActivationPowerChoice * MidActivationPowerChoiceInfo
| MidPassivePowerChoicePair of CardsState * TurnInProgress * MidPassivePowerChoiceContext * MidPassivePowerChoiceInfo
| StackChoicePair of CardsState * TurnInProgress * StackChoice * StackChoiceInfo
| TurnActionChoicePair of CardsState * TurnInProgress * TurnActionInfo
| StartTurnPair of GameStateBetweenTurns * PlayerID
    
let private getPossibleActionPairs (gameState: GameState) =
    match gameState with
    | GameStateBetweenTurns gs ->
        StartTurnPair (gs, gs.TurnState.Player)
        |> List.singleton
    | GameStateDuringTurn gs ->
        match gs with
        | {CardsState = cs; TurnState = ts; TurnStage = ActionChoice} ->
            let currentPlayer = ts.CurrentPlayer
            if ts.ActionsLeft = 0u<action> then
                TurnActionChoicePair (cs, ts, EndTurn currentPlayer)
                |> List.singleton
            else
                let actions =
                    getPlayActionsInfo gs
                    @ getActivateActionsInfo gs
                    @ getAttackActionsInfo gs
                    @ getPairActionsInfo gs
                if List.isEmpty actions then
                    TurnActionChoicePair (cs, ts, EndTurn currentPlayer)
                    |> List.singleton
                else
                   actions
                   |> List.map (fun action -> TurnActionChoicePair (cs, ts, ActionChoiceInfo action))
        | {CardsState = cs; TurnState = ts; TurnStage = StackChoice sc} ->
            sc.EpochEvents
            |> NonEmptyMap.toMap
            |> Map.toList
            |> List.map (fun (eventID, event) ->
                StackChoicePair (cs, ts, sc, (gs.TurnState.CurrentPlayer, eventID, event))
            )
        | {CardsState = cs; TurnState = ts; TurnStage = MidActivationPowerChoice mapc} ->
            match mapc.ChoiceContext with
            | DiscardChoiceContext (playerID, powerCardID) ->
                match gs.CardsState.GameStage with
                | Early {HandCards = hc}
                | DrawPileEmpty {HandCards = hc} ->
                    hc
                    |> Map.find playerID
                    |> List.map (fun {HandCardID = HandCardID id} ->
                        MidActivationPowerChoicePair (cs, ts, mapc, DiscardChoice (playerID, powerCardID, id))
                        )
                | HandsEmpty _ ->
                    failwithf "Can't discard from an empty hand"
            | ForesightChoiceContext (playerID, powerCardID) ->
                let unknownInactiveUnitIDs =
                    gs.CardsState.Board.Lanes
                    |> Map.toList
                    |> List.collect (fun (_, lane) -> lane.InactiveUnits)
                    |> List.filter (fun card -> not (Set.contains playerID card.KnownBy))
                    |> List.map (fun {InactiveUnitID = InactiveUnitID id} -> ForesightTargetID id)
                let unknownFaceDownCardIDs =
                    match gs.CardsState.GameStage with
                    | Early {Bases = bases} ->
                        let unknownBaseIDs =
                            bases
                            |> Map.toList
                            |> List.collect (fun (laneID, b) ->
                                b
                                |> List.filter (fun card -> not (Set.contains playerID card.KnownBy))
                                |> List.map (fun {BaseCardID = BaseCardID id} -> ForesightTargetID id)
                                )
                        unknownBaseIDs @ unknownInactiveUnitIDs
                    | DrawPileEmpty _
                    | HandsEmpty _ ->
                        unknownInactiveUnitIDs
                unknownFaceDownCardIDs
                |> List.map (fun (ForesightTargetID id) ->
                    MidActivationPowerChoicePair (cs, ts, mapc, ForesightChoice (playerID, powerCardID, id))
                    )
            | MoveChoiceContext (playerID, laneID, powerCardID) ->
                let pairs =
                    gs.CardsState.Board.Lanes
                        |> Map.filter (fun targetLaneID _ -> targetLaneID <> laneID)
                    |> Map.toList
                    |> List.collect (fun (targetLaneID, lane) ->
                        (lane.InactiveUnits |> List.choose (fun {InactiveUnitID = InactiveUnitID id; Owner = ownerID}-> if ownerID = playerID then Some id else None))
                        @ (lane.ActiveUnits |> List.choose (fun {ActiveUnitID = ActiveUnitID id; Owner = ownerID} -> if ownerID = playerID then Some id else None))
                        @ (lane.Pairs |> List.collect (fun ({ActiveUnitID = ActiveUnitID id1; Owner = ownerID}, {ActiveUnitID = ActiveUnitID id2}) -> if ownerID = playerID then [id1; id2] else []))
                        |> List.map (fun id -> targetLaneID, id)
                    )
                    |> List.map (fun (targetLaneID, targetCardID) ->
                        MidActivationPowerChoicePair (cs, ts, mapc, MoveChoice (Some (playerID, laneID, powerCardID, targetLaneID, targetCardID)))
                    )
                if List.isEmpty pairs then
                    pairs
                else
                    MidActivationPowerChoicePair (cs, ts, mapc, MoveChoice (None)) :: pairs
        | {CardsState = cs; TurnState = ts; TurnStage = MidPassivePowerChoice mppcc} ->
            match mppcc with
            | TwinStrikeChoiceContext (playerID, laneID, powerCardID, originalTargetCardID) ->
                let lane = Map.find laneID gs.CardsState.Board.Lanes
                let originalTargetCard =
                    match List.tryFind (fun (card: InactiveUnit) -> card.InactiveUnitID = InactiveUnitID originalTargetCardID) lane.InactiveUnits with
                    | Some card -> InactiveUnit card
                    | None ->
                        match List.tryFind (fun (card: ActiveUnit) -> card.ActiveUnitID = ActiveUnitID originalTargetCardID) lane.ActiveUnits with
                        | Some card -> ActiveUnit card
                        | None ->
                            lane.Pairs
                            |> List.collect (fun (c1, c2) -> [c1; c2])
                            |> List.find (fun (card: ActiveUnit) -> card.ActiveUnitID = ActiveUnitID originalTargetCardID)
                            |> ActiveUnit
                let activeTauntCheckTargets, nonActiveTauntCheckTargets =
                    let inactiveTargets =
                        lane.InactiveUnits
                        |> List.filter (fun card -> card.Owner <> playerID && card.InactiveUnitID <> InactiveUnitID originalTargetCardID)
                    let activeTauntTargets, activeNonTauntTargets =
                        lane.ActiveUnits
                        |> List.filter (fun card -> card.Owner <> playerID && card.ActiveUnitID <> ActiveUnitID originalTargetCardID && not (List.contains (AttackAbility DamageExtraTarget) card.Abilities.Ignores))
                        |> List.partition (fun card -> List.contains ProtectsNonTauntAlliesInLane card.Abilities.WhileActive)
                    List.map (fun {ActiveUnitID = id} -> id) activeTauntTargets,
                    ((List.map (fun {InactiveUnitID = InactiveUnitID id} -> UnitID id) inactiveTargets)
                    @ (List.map (fun {ActiveUnitID = ActiveUnitID id} -> UnitID id) activeNonTauntTargets))
                let originalTargetIsActiveTaunt =
                    match originalTargetCard with
                    | InactiveUnit _ -> false
                    | ActiveUnit {Abilities = a} -> List.contains ProtectsNonTauntAlliesInLane a.WhileActive
                let legalTargets =
                    if List.isEmpty activeTauntCheckTargets && not (originalTargetIsActiveTaunt)
                    then
                        nonActiveTauntCheckTargets
                    else
                        activeTauntCheckTargets |> List.map (fun (ActiveUnitID id) -> UnitID id)
                legalTargets
                |> List.map (fun (UnitID cardID) ->
                    MidPassivePowerChoicePair (cs, ts, mppcc, TwinStrikeChoice (playerID, laneID, powerCardID, cardID))
                )
            | TwinStrikeRelatiatePairChoiceContext (playerID, laneID, powerCardIDs, originalTargetCardID) ->
                let (id1, id2) = powerCardIDs
                [id1; id2]
                |> List.map (fun id ->
                    MidPassivePowerChoicePair (
                        cs, ts, mppcc,
                        TwinStrikeRetaliatePairChoice (playerID, laneID, PairIDs (id1, id2), originalTargetCardID, id)
                    )
                )
    | GameStateWon _
    | GameStateTied _ ->
        List.empty

let private executeMidActivationPowerChoice midPowerChoice cardsState turnState midActivationPowerChoice =
    let gameState = {
        CardsState = cardsState
        TurnState = turnState
        TurnStage = MidActivationPowerChoice midActivationPowerChoice
    }
    match midPowerChoice with
    | DiscardChoice (playerID, _, discardeeCardID) ->
        let (discardeeCard, cs) = removeCardFromHand (HandCardID discardeeCardID) playerID cardsState
        let convertedCard = handToDiscardedCard discardeeCard
        let newCardsState = {
            cs with
                Board = {cs.Board with Discard = cs.Board.Discard @ [convertedCard]}
            }
        changeCardsState gameState newCardsState
    | ForesightChoice (playerID, powerCardID, targetCardID) ->
        gameState.CardsState
        |> makeCardKnown targetCardID playerID
        |> changeCardsState gameState
    | MoveChoice maybeMove ->
        match maybeMove with
        | Some (playerID, laneID, powerCardID, targetLaneID, targetCardID) ->
            gameState.CardsState
            |> changeCardLane (UnitID targetCardID) targetLaneID laneID
            |> changeCardsState gameState
        | None ->
            gameState
    |> toActionChoice

let private resolveReturnDamage playerID laneID attackerIDs targetCardID cardsState turnState midPassivePowerChoiceContext =
    let gameState = {CardsState = cardsState; TurnState = turnState; TurnStage = MidPassivePowerChoice midPassivePowerChoiceContext}
    let newState =
        gameState.CardsState
        |> damageCard (UnitID targetCardID) 1u<health> laneID
    match attackerIDs with
    | SingleCardID id ->
        newState
        |> damageCard (UnitID id) 1u<health> laneID
        |> changeCardsState gameState
        |> toActionChoice
        |> triggerTargetInactiveDeathPowers
        |> moveDeadCardsToDiscard
        |> toActionChoice
    | PairIDs (id1, id2) ->
        {
            CardsState = newState
            TurnState = gameState.TurnState
            TurnStage = MidPassivePowerChoice (TwinStrikeRelatiatePairChoiceContext (playerID, laneID, (id1, id2), targetCardID))
        }
 
let private executeMidPassivePowerChoice midPowerChoice cardsState turnState midPassivePowerChoiceContext =
    let gameState = {CardsState = cardsState; TurnState = turnState; TurnStage = MidPassivePowerChoice midPassivePowerChoiceContext}
    match midPowerChoice with
    | TwinStrikeChoice (playerID, laneID, powerCardIDs, targetCardID) ->
        let lane = Map.find laneID gameState.CardsState.Board.Lanes
        let activeTarget =
            lane.ActiveUnits @ (List.collect (fun (card1, card2) -> [card1; card2]) lane.Pairs)
            |> List.tryFind (fun {ActiveUnitID = ActiveUnitID id} -> id = targetCardID)
        match activeTarget with
        | Some target ->
            match target.Abilities.OnDamaged with
            | [] ->
                gameState.CardsState
                |> damageCard (UnitID targetCardID) 1u<health> laneID
                |> changeCardsState gameState
                |> toActionChoice
                |> triggerTargetInactiveDeathPowers
                |> moveDeadCardsToDiscard
                |> toActionChoice
            | [ReturnDamage] ->
                resolveReturnDamage playerID laneID powerCardIDs targetCardID cardsState turnState midPassivePowerChoiceContext
            | _ ->
                failwithf "Unrecognised OnDamaged contents: %A" target.Abilities.OnDamaged
        | None ->
            gameState.CardsState
            |> damageCard (UnitID targetCardID) 1u<health> laneID
            |> changeCardsState gameState
            |> toActionChoice
            |> triggerTargetInactiveDeathPowers
            |> moveDeadCardsToDiscard
            |> toActionChoice
    | TwinStrikeRetaliatePairChoice (playerID, laneID, powerCardID, targetCardID, whichID) ->
        gameState.CardsState
        |> damageCard (UnitID whichID) 1u<health> laneID
        |> changeCardsState gameState
        |> toActionChoice
        |> triggerTargetInactiveDeathPowers
        |> moveDeadCardsToDiscard
        |> toActionChoice

let private executePlayAction cardID laneID cardsState turnState =
    let gameState = {CardsState = cardsState; TurnState = turnState; TurnStage = ActionChoice}
    let playerID = gameState.TurnState.CurrentPlayer
    let cardsState = gameState.CardsState
    let playedCard, newCardsState = removeCardFromHand cardID playerID cardsState
    let newCard = handToInactiveUnit playedCard
    newCardsState
    |> addCardToInactiveUnits newCard laneID
    |> removeHandsIfAllEmpty
    |> changeCardsState gameState

let private tryDrawCard playerID cardsState =
    match cardsState.GameStage with
    | Early preInfo ->
        let boardInfo = cardsState.Board
        let hands = preInfo.HandCards
        let drawPile = preInfo.DrawPile
        match drawPile.Tail with
        | [] ->
            let newHandCards =
                hands
                |> Map.change playerID (function
                    | Some hc ->
                        Some (hc @ [deckToHandCard cardsState.GetAbilities playerID drawPile.Head])
                    | None ->
                        failwithf "non-existent players can't draw cards"
                    )
            let newCards = {
                cardsState with
                    Board = flipBasesOnBoard preInfo.Bases boardInfo
                    GameStage = DrawPileEmpty {HandCards = newHandCards; LaneWins = Map.empty}
                }
            newCards
        | newTopCard :: newRest ->
            let newHandCards =
                hands
                |> Map.change playerID (function
                    | Some hc ->
                        Some (hc @ [deckToHandCard cardsState.GetAbilities playerID drawPile.Head])
                    | None ->
                        failwithf "non-existent players can't draw cards"
                    )
            let newCards =
                {cardsState with
                    GameStage =
                        Early {
                            preInfo with
                                DrawPile = NonEmptyList.create newTopCard newRest
                                HandCards = newHandCards
                            }
                }
            newCards
    | DrawPileEmpty _
    | HandsEmpty _ ->
        cardsState

let private healOwnUnitsInLane playerID amount (lane: Lane) =
    {lane with
        InactiveUnits =
            lane.InactiveUnits
            |> List.map (fun card ->
                if card.Owner = playerID then
                    healInactiveCard amount card
                else
                    card
                )
        ActiveUnits =
            lane.ActiveUnits
            |> List.map (fun card ->
                if card.Owner = playerID then
                    healActiveCard amount card
                else
                    card
                )
        Pairs =
            lane.Pairs
            |> List.map (fun (card1, card2) ->
                if card1.Owner = playerID then
                    healActiveCard amount card1,
                    healActiveCard amount card2
                else
                    card1, card2
                )
        }
let private healOwnUnits playerID amount cardsState =
    let board = cardsState.Board
    let newLanes =
        board.Lanes
        |> Map.map (fun _ lane -> healOwnUnitsInLane playerID amount lane)
    {cardsState with Board = {board with Lanes = newLanes}}

let private freezeEnemyNonActiveNimbleUnitsInLane playerID laneID cardsState =
    let board = cardsState.Board
    let lane = Map.find laneID board.Lanes
    let newInactiveUnits =
        lane.InactiveUnits
        |> List.map (fun card ->
            if card.Owner <> playerID then
                {card with FreezeStatus = FrozenBy playerID}
            else
                card
            )
    let newActiveUnits =
        lane.ActiveUnits
        |> List.map (fun card ->
            if card.Owner <> playerID && not (List.contains (InstantNonTargetAbility FreezeEnemiesInLane) card.Abilities.Ignores) then
                {card with FreezeStatus = FrozenBy playerID}
            else
                card
            )
    let newPairs =
        lane.Pairs
        |> List.map (fun (c1, c2) ->
            if c1.Owner <> playerID && not (List.contains (InstantNonTargetAbility FreezeEnemiesInLane) c1.Abilities.Ignores) then
                {c1 with FreezeStatus = FrozenBy playerID}, {c2 with FreezeStatus = FrozenBy playerID}
            else
                c1, c2
            )
    let newLane = {lane with InactiveUnits = newInactiveUnits; ActiveUnits = newActiveUnits; Pairs = newPairs}
    {cardsState with Board = {board with Lanes = board.Lanes |> Map.add laneID newLane}}

let private addActiveNonEmpowerActivationPowersInLaneToStack playerID laneID (ActiveUnitID empowererCardID) cardsState turnState =
    let lane =
        cardsState.Board.Lanes
        |> Map.find laneID
    let powerChoices =
        lane.ActiveUnits @ (lane.Pairs |> List.unzip |> (fun (lst1, lst2) -> lst1 @ lst2))
        |> List.filter (fun {ActiveUnitID = ActiveUnitID id; Owner = owner} -> owner = playerID && id <> empowererCardID)
        |> List.choose (fun {ActiveUnitID = ActiveUnitID id; Abilities = abilities} ->
            match abilities.OnActivation with
            | [] -> None
            | oa ->
                if List.contains ReactivateNonEmpowerActivationPowersInLane oa then
                    None
                else
                    Some (ActivationPowerContext (playerID, laneID, id, abilities.Name))
        )
        |> NonEmptyList.tryFromList
    match powerChoices with
    | None -> cardsState, turnState, None
    | Some pc -> cardsState, turnState, Some (OrderChoiceEpoch pc)

let private resolveInstantNonTargetAbility event cardsState turnState =
    let ability, playerID, laneID, cardID = event
    match ability with
    | Draw n -> tryDrawCard playerID cardsState, turnState, None
    | Discard n -> cardsState, turnState, Some (MidAbilityChoice (DiscardChoiceContext (playerID, cardID)))
    | ViewInactive n -> cardsState, turnState, Some (MidAbilityChoice (ForesightChoiceContext (playerID, cardID)))
    | FreezeEnemiesInLane -> freezeEnemyNonActiveNimbleUnitsInLane playerID laneID cardsState, turnState, None
    | HealAllAllies n -> healOwnUnits playerID (n*1u<health>) cardsState, turnState, None
    | MayMoveAllyToOwnLane -> cardsState, turnState, Some (MidAbilityChoice (MoveChoiceContext (playerID, laneID, cardID)))
    | ExtraActions n ->
        let (newTurnState: TurnInProgress) = {turnState with ActionsLeft = turnState.ActionsLeft + n*1u<action>}
        cardsState, newTurnState, None
    | ChangeMaxAttacksThisTurn n -> setMaxCardActions cardID 2u<action> laneID cardsState, turnState, None
    | ActivateAlliesInLane -> flipInactiveCardsInLaneAndAddActivationPowersToStack playerID laneID (ActiveUnitID cardID) cardsState turnState
    | ReactivateNonEmpowerActivationPowersInLane ->
        addActiveNonEmpowerActivationPowersInLaneToStack playerID laneID (ActiveUnitID cardID) cardsState turnState
    | FullyHealSelf
    | HealSelf _
    | ActivateSelf -> cardsState, turnState, None

let private addCardActivationAbilitiesToStack playerID laneID (ActiveUnitID cardID) cardsState maybeStack =
    let {Board = board} = cardsState
    let lane = Map.find laneID board.Lanes
    let card = List.find (fun {ActiveUnitID = ActiveUnitID id} -> id = cardID) lane.ActiveUnits
    let maybeAbilitiesEpoch =
        card.Abilities.OnActivation
        |> List.map (fun ability -> ability, playerID, laneID, cardID)
        |> NonEmptyList.tryFromList
        |> Option.map OrderedTriggerEpoch
    maybeStack
    |> NonEmptyList.consOptions maybeAbilitiesEpoch

let rec private processResolutionStack cardsState turnState (maybeResolutionStack: ResolutionStack option) =
    match maybeResolutionStack with
    | None -> {
        CardsState = cardsState
        TurnState = turnState
        TurnStage = ActionChoice
        }
    | Some {Head = h; Tail = t} ->
        match h with
        | OrderChoiceEpoch activationPowerContexts -> {
            CardsState = cardsState
            TurnState = turnState
            TurnStage = StackChoice {
                EpochEvents =
                    activationPowerContexts
                    |> NonEmptyList.toList
                    |> createIDMap 1u<EID>
                    |> NonEmptyMap.fromMap
                ResolutionStack = NonEmptyList.tryFromList t
                }
            }
        | MidAbilityChoice choiceContext -> {
            CardsState = cardsState
            TurnState = turnState
            TurnStage = MidActivationPowerChoice {
                ChoiceContext = choiceContext
                ResolutionStack = NonEmptyList.tryFromList t
                }
            }
        | OrderedTriggerEpoch triggerEvents ->
            let ({Head = first; Tail = rest}: TriggerEvent epoch) = triggerEvents
            let remainingHead =
                NonEmptyList.tryFromList rest
                |> Option.map OrderedTriggerEpoch
            let remainingStack =
                NonEmptyList.tryFromList t
                |> NonEmptyList.consOptions remainingHead
            let cs, ts, maybeNewEpoch = resolveInstantNonTargetAbility first cardsState turnState
            let newStack = NonEmptyList.consOptions maybeNewEpoch remainingStack
            processResolutionStack cs ts newStack

let private resolveActivationPower playerID laneID (ActiveUnitID cardID) cardsState turnState stack =
    let {Board = board} = cardsState
    let lane = Map.find laneID board.Lanes
    let card = List.find (fun {ActiveUnitID = ActiveUnitID id} -> id = cardID) lane.ActiveUnits
    let newEpoch =
        card.Abilities.OnActivation
        |> List.map (fun ability -> ability, playerID, laneID, cardID)
        |> NonEmptyList.tryFromList
        |> Option.map OrderedTriggerEpoch
    processResolutionStack cardsState turnState (NonEmptyList.consOptions newEpoch stack)

let private resolveAttackerPassivePower playerID laneID attackerIDs (UnitID attackedCardID) cardsState turnState =
    let lane = Map.find laneID cardsState.Board.Lanes
    let card =
        match attackerIDs with
        | SingleAttackerID (ActiveUnitID id) -> List.find (fun {ActiveUnitID = ActiveUnitID thisID} -> thisID = id) lane.ActiveUnits
        | PairAttackerIDs ((ActiveUnitID id), _) -> List.find (fun {ActiveUnitID = ActiveUnitID thisID} -> thisID = id) (lane.Pairs |> List.collect (fun (c1, c2) -> [c1; c2]))
    let attackingAbilities = (List.map AttackAbility card.Abilities.OnAttack) @ (List.map InstantNonTargetAbility card.Abilities.OnKill)
    match attackingAbilities with
    | [] ->
        {CardsState = cardsState; TurnState = turnState; TurnStage = ActionChoice}
        |> triggerTargetInactiveDeathPowers
        |> moveDeadCardsToDiscard
    | [AttackAbility DamageExtraTarget] ->
        let activeLaneCards =
            lane.ActiveUnits @ List.collect (fun (card1, card2) -> [card1; card2]) lane.Pairs
        let activeTargetCard =
            activeLaneCards
            |> List.tryFind (fun {ActiveUnitID = (ActiveUnitID id)} -> id = attackedCardID)
        match activeTargetCard with
        | Some {Abilities = abilities} ->
            if List.contains (AttackAbility DamageExtraTarget) abilities.Ignores then
               {CardsState = cardsState; TurnState = turnState; TurnStage = ActionChoice}
               |> triggerTargetInactiveDeathPowers
               |> moveDeadCardsToDiscard
            else
                let context = TwinStrikeChoiceContext (playerID, laneID, (transferAttackerIDs attackerIDs), attackedCardID)
                {
                    CardsState = cardsState
                    TurnState = turnState
                    TurnStage = MidPassivePowerChoice context
                }
        | None ->
            let context = TwinStrikeChoiceContext (playerID, laneID, (transferAttackerIDs attackerIDs), attackedCardID)
            {
                CardsState = cardsState
                TurnState = turnState
                TurnStage = MidPassivePowerChoice context
            }
    | [InstantNonTargetAbility (HealSelf 1u)] ->
        {CardsState = cardsState; TurnState = turnState; TurnStage = ActionChoice}
        |> triggerTargetInactiveDeathPowers
        |> healAttackersIfDefenderDying attackerIDs (UnitID attackedCardID) laneID
        |> moveDeadCardsToDiscard
    | _ ->
        failwithf "Unrecognised OnAttack + OnKill contents: %A" attackingAbilities

let private executeActivateAction playerID laneID (InactiveUnitID cardID) cardsState turnState =
    let removedCard, cs1 = removeCardFromInactiveUnits (InactiveUnitID cardID) laneID cardsState
    let newCard = inactiveToActiveUnit removedCard
    resolveActivationPower playerID laneID (ActiveUnitID cardID) (addCardToActiveUnits newCard laneID cs1) turnState None

let private getBonusDefenderDamage attackerAbilities targetAbilities targetInfo =
    let hasBonusHealth =
        targetAbilities.WhileActive
        |> List.exists (function
            | MaxHealthIncrease _ -> true
            | _ -> false
        )
    if hasBonusHealth then
        attackerAbilities.OnAttack
        |> List.fold (fun acc ability ->
            match ability with
            | ExtraDamageAgainstExtraMaxHealth n -> acc + n*1u<health>
            | _ -> acc
        ) 0u<health>
    else
        0u<health>

let private getTargetIDFromTargetInfo targetInfo =
    match targetInfo with
    | InactiveTarget (owner, id)
    | ActiveSingleTarget (owner, id)
    | ActivePairMemberTarget (owner, id) ->
        UnitID id

let private getAttackerSelfDamage attackerAbilities targetAbilities =
    if not (List.contains (DefendAbility ReturnDamage) attackerAbilities.Ignores) && (List.contains ReturnDamage targetAbilities.OnDamaged) then
        1u<health>
    else
        0u<health>

let private getSingleAttackInfo attackerID targetInfo lane =
    let targetID = getTargetIDFromTargetInfo targetInfo
    let attackerAbilities =
        lane.ActiveUnits
        |> List.find (fun {ActiveUnitID = ActiveUnitID id} -> id = attackerID)
        |> (fun {Abilities = abilities} -> abilities)
    let targetList =
        (lane.InactiveUnits |> List.map InactiveUnit)
        @ (lane.ActiveUnits |> List.map ActiveUnit)
        @ (lane.Pairs |> List.collect (fun (card1, card2) -> [ActiveUnit card1; ActiveUnit card2]))
    let targetAbilities =
        targetList
        |> List.find (fun card ->
            match card with
            | InactiveUnit {InactiveUnitID = InactiveUnitID cid}
            | ActiveUnit {ActiveUnitID = ActiveUnitID cid} ->
                UnitID cid = targetID
            )
        |> (fun card ->
            match card with
            | InactiveUnit {Abilities = abilities}
            | ActiveUnit {Abilities = abilities} ->
                abilities
            )
    let baseDefenderDamage = 1u<health>
    let bonusDefenderDamage = getBonusDefenderDamage attackerAbilities targetAbilities targetInfo
    let selfDamage = getAttackerSelfDamage attackerAbilities targetAbilities
    targetID, baseDefenderDamage + bonusDefenderDamage, selfDamage

let private getPairAttackInfo pairMemberID1 pairMemberID2 targetInfo lane =
    let targetID = getTargetIDFromTargetInfo targetInfo
    let attackerAbilities =
        lane.Pairs
        |> List.find (fun ({ActiveUnitID = ActiveUnitID id1}, {ActiveUnitID = ActiveUnitID id2}) ->
            (id1, id2) = (pairMemberID1, pairMemberID2)
        )
        |> (fun ({Abilities = abilities}, _) -> abilities)
    let targetList =
        (lane.InactiveUnits |> List.map InactiveUnit)
        @ (lane.ActiveUnits |> List.map ActiveUnit)
        @ (lane.Pairs |> List.collect (fun (card1, card2) -> [ActiveUnit card1; ActiveUnit card2]))
    let targetAbilities =
        targetList
        |> List.find (fun card ->
            match card with
            | InactiveUnit {InactiveUnitID = InactiveUnitID cid}
            | ActiveUnit {ActiveUnitID = ActiveUnitID cid} ->
                UnitID cid = targetID
            )
        |> (fun card ->
            match card with
            | InactiveUnit {Abilities = abilities}
            | ActiveUnit {Abilities = abilities} ->
                abilities
            )
    let baseDefenderDamage = 2u<health>
    let bonusDefenderDamage = getBonusDefenderDamage attackerAbilities targetAbilities targetInfo
    let selfDamage = getAttackerSelfDamage attackerAbilities targetAbilities
    targetID, baseDefenderDamage + bonusDefenderDamage, selfDamage

let private executeSingleAttackAction playerID laneID (ActiveUnitID attackerID) targetInfo cardsState turnState =
    let board = cardsState.Board
    let targetID, damage, selfDamage =
        getSingleAttackInfo attackerID targetInfo (Map.find laneID board.Lanes)
    let newCardsState =
        cardsState
        |> incrementCardActionsUsed attackerID laneID
        |> damageCard targetID damage laneID
        |> damageCard (UnitID attackerID) selfDamage laneID
    resolveAttackerPassivePower playerID laneID (SingleAttackerID (ActiveUnitID attackerID)) targetID newCardsState turnState

let private executePairAttackAction playerID laneID (ActiveUnitID attackerID1, ActiveUnitID attackerID2) targetInfo cardsState turnState =
    let board = cardsState.Board
    let targetID, damage, selfDamage =
        getPairAttackInfo attackerID1 attackerID2 targetInfo (Map.find laneID board.Lanes)
    let newCardsState =
        cardsState
        |> incrementCardActionsUsed attackerID1 laneID
        |> incrementCardActionsUsed attackerID2 laneID
        |> damageCard targetID damage laneID
        |> damageCard (UnitID attackerID1) selfDamage laneID
        |> damageCard (UnitID attackerID2) selfDamage laneID
    resolveAttackerPassivePower playerID laneID (PairAttackerIDs (ActiveUnitID attackerID1, ActiveUnitID attackerID2)) targetID cardsState turnState

let private executeCreatePairAction laneID cardID1 cardID2 cardsState turnState =
    let removedUnits, newCardsState =
        cardsState
        |> removeCardsFromActiveUnits [cardID1; cardID2] laneID
    let card1 = removedUnits.[0]
    let card2 = removedUnits.[1]
    {
        CardsState = addCardsToPairs (card1, card2) laneID newCardsState
        TurnState = turnState
        TurnStage = ActionChoice
    }

let private decrementActionsLeft (turnState: TurnInProgress) =
    {turnState with ActionsLeft = turnState.ActionsLeft - 1u<action>}

let private executeTurnAction action cardsState turnState =
    let ts = decrementActionsLeft turnState
    let postAction =
        match action with
        | Play (playerID, cardID, laneID) ->
            executePlayAction (HandCardID cardID) laneID cardsState ts
        | Activate (playerID, laneID, cardID) ->
            executeActivateAction playerID laneID (InactiveUnitID cardID) cardsState ts
        | SingleAttack (playerID, laneID, attackerID, targetInfo) ->
            executeSingleAttackAction playerID laneID (ActiveUnitID attackerID) targetInfo cardsState ts
        | PairAttack (playerID, laneID, (attackerID1, attackerID2), targetInfo) ->
            executePairAttackAction playerID laneID (ActiveUnitID attackerID1, ActiveUnitID attackerID2) targetInfo cardsState ts
        | CreatePair (playerID, laneID, cardID1, cardID2) ->
            executeCreatePairAction laneID (ActiveUnitID cardID1) (ActiveUnitID cardID2) cardsState ts
    match postAction.TurnStage with
    | MidActivationPowerChoice _
    | MidPassivePowerChoice _
    | StackChoice _ ->
        postAction
    | ActionChoice ->
        updateLaneWins postAction

let private startPlayerTurn playerID (gameState: GameStateBetweenTurns) =
    let ts = gameState.TurnState
    {
        CardsState = gameState.CardsState
        TurnState = {
            CurrentPlayer = playerID
            NPlayers = ts.NPlayers
            ActionsLeft = ts.Actions
            FutureActionCounts = ts.FutureActionCounts
        }
        TurnStage = ActionChoice
    }

let private timeoutOwnedFreezeStatesInLane playerID lane =
    {
        lane with
            InactiveUnits =
                lane.InactiveUnits
                |> List.map (fun (card: InactiveUnit) ->
                    if card.FreezeStatus = FrozenBy playerID then
                        {card with FreezeStatus = NotFrozen}
                    else
                        card
                )
            ActiveUnits =
                lane.ActiveUnits
                |> List.map (fun card ->
                    if card.FreezeStatus = FrozenBy playerID then
                        {card with FreezeStatus = NotFrozen}
                    else
                        card
                )
            Pairs =
                lane.Pairs
                |> List.map (fun (c1, c2) ->
                    let newC1 =
                        if c1.FreezeStatus = FrozenBy playerID then
                                {c1 with FreezeStatus = NotFrozen}
                        else
                            c1
                    let newC2 =
                        if c2.FreezeStatus = FrozenBy playerID then
                            {c2 with FreezeStatus = NotFrozen}
                        else
                            c2
                    newC1, newC2
                )
    }
let private timeoutOwnedFreezeStates playerID cardsState =
    let board = cardsState.Board
    let newLanes =
        board.Lanes
        |> Map.map (fun _ lane -> timeoutOwnedFreezeStatesInLane playerID lane)
    {cardsState with Board = {board with Lanes = newLanes}}

let private resetAllCardActionsUsedInLane lane =
    {lane with
        ActiveUnits = lane.ActiveUnits |> List.map (fun card -> {card with ActionsSpent = 0u<action>})
        Pairs = lane.Pairs |> List.map (fun (card1, card2) -> {card1 with ActionsSpent = 0u<action>}, {card2 with ActionsSpent = 0u<action>})
        }
let private resetAllCardActionsUsed cardsState =
    let board = cardsState.Board
    let newLanes =
        board.Lanes
        |> Map.map (fun _ lane -> resetAllCardActionsUsedInLane lane)
    {cardsState with Board = {board with Lanes = newLanes}}

let private resetMaxActions activeUnit =
    {activeUnit with MaxActions = 1u<action>}
let private resetMaxActionsInLane lane =
    {lane with
        ActiveUnits = lane.ActiveUnits |> List.map resetMaxActions
        Pairs = lane.Pairs |> List.map (fun (card1, card2) -> resetMaxActions card1, resetMaxActions card2)
        }
let private resetAllMaxCardActions cardsState =
    let board = cardsState.Board
    let newLanes =
        board.Lanes
        |> Map.map (fun _ lane -> resetMaxActionsInLane lane)
    {cardsState with Board = {board with Lanes = newLanes}}

let private cancelPowerChoiceIfNoChoices state actionPairs =
    match state, actionPairs with
    | GameStateDuringTurn gs, [] ->
        match gs.TurnStage with
        | MidActivationPowerChoice mapc ->
            let cancelledState =
                toActionChoice gs
                |> triggerTargetInactiveDeathPowers
                |> moveDeadCardsToDiscard
                |> GameStateDuringTurn
            cancelledState, getPossibleActionPairs cancelledState
        | MidPassivePowerChoice mppcc ->
            let cancelledState =
                toActionChoice gs
                |> triggerTargetInactiveDeathPowers
                |> moveDeadCardsToDiscard
                |> GameStateDuringTurn
            cancelledState, getPossibleActionPairs cancelledState
        | _ ->
            state, actionPairs
    | _ ->
        state, actionPairs

let private executeEndingTurnAction cardsState turnState =
    let nextPlayer =
        if turnState.CurrentPlayer = turnState.NPlayers*1u<PID> then
            1u<PID>
        else
            turnState.CurrentPlayer + 1u<PID>
    let actions, nextFutureActionCounts =
        match turnState.FutureActionCounts with
        | [] -> 3u<action>, []
        | h :: t -> h, t
    GameStateBetweenTurns {
        CardsState =
            cardsState
            |> resetAllCardActionsUsed
            |> resetAllMaxCardActions
            |> timeoutOwnedFreezeStates turnState.CurrentPlayer
        TurnState = {
            Player = nextPlayer
            NPlayers = turnState.NPlayers
            Actions = actions
            FutureActionCounts = nextFutureActionCounts
            }
        }    

let private executeStackChoice cardsState turnState stackChoice choiceInfo =
    let playerID, index, (playerID2, laneID, cardID, powerName) = choiceInfo
    let maybeRemainingHead =
        stackChoice.EpochEvents
        |> NonEmptyMap.toMap
        |> Map.remove index
        |> Map.toList
        |> List.map snd
        |> NonEmptyList.tryFromList
        |> Option.map OrderChoiceEpoch
    let remainingStack = NonEmptyList.consOptions maybeRemainingHead stackChoice.ResolutionStack
    let newState = resolveActivationPower playerID laneID (ActiveUnitID cardID) cardsState turnState remainingStack
    match newState with
    | {CardsState = cs; TurnState = ts; TurnStage = ActionChoice} ->
        {
            CardsState = cs
            TurnState = ts
            TurnStage = ActionChoice
        }
    | {CardsState = cs; TurnState = ts; TurnStage = MidActivationPowerChoice c} ->
        {
            CardsState = cs
            TurnState = ts
            TurnStage = MidActivationPowerChoice c
        }
    | {CardsState = cs; TurnState = ts; TurnStage = MidPassivePowerChoice _} ->
        failwithf "Shouldn't be here!"
    | {CardsState = cs; TurnState = ts; TurnStage = StackChoice sc} ->
        {
            CardsState = cs
            TurnState = ts
            TurnStage = StackChoice sc
        }

let rec private makeNextActionInfo actionPair =
    // Want to do some checking that we have the right player
    let newState, action =
        match actionPair with
        | MidActivationPowerChoicePair (cardsState, turnState, midActivationPowerChoiceContext, choiceInfo) ->
            executeMidActivationPowerChoice choiceInfo cardsState turnState midActivationPowerChoiceContext
            |> checkForGameEnd,
            MidActivationPowerChoiceInfo choiceInfo
        | MidPassivePowerChoicePair (cardsState, turnState, midPassivePowerChoice, choiceInfo) ->
            executeMidPassivePowerChoice choiceInfo cardsState turnState midPassivePowerChoice
            |> checkForGameEnd,
            MidPassivePowerChoiceInfo choiceInfo
        | StackChoicePair (cardsState, turnState, stackChoice, choiceInfo) ->
            executeStackChoice cardsState turnState stackChoice choiceInfo
            |> GameStateDuringTurn,
            StackChoiceInfo choiceInfo
        | TurnActionChoicePair (gameState, turnState, ActionChoiceInfo choiceInfo) ->
            executeTurnAction choiceInfo gameState turnState
            |> checkForGameEnd,
            TurnActionInfo (ActionChoiceInfo choiceInfo)
        | TurnActionChoicePair (cardsState, turnState, EndTurn playerID) ->
            executeEndingTurnAction cardsState turnState,
            TurnActionInfo (EndTurn playerID)
        | StartTurnPair (state, playerID) ->
            let startTurn =
                state
                |> startPlayerTurn playerID
            {startTurn with CardsState = tryDrawCard playerID startTurn.CardsState}
            |> GameStateDuringTurn,
            StartTurn playerID
    let checkedGameState, checkedPossibleActionPairs =
        cancelPowerChoiceIfNoChoices newState (getPossibleActionPairs newState)
    let newDisplayInfo = getDisplayInfo checkedGameState
    // Generation of next action's resulting capabilities is part of the
    // generated capability's body, since assigning them here requires
    // generating the entire game space, blowing the stack
    let capability() =
        InProgress (
            newDisplayInfo,
            checkedPossibleActionPairs
            |> List.map makeNextActionInfo
            )
    {
        Action = action
        Capability = capability
        }

let private createGame (nPlayers: uint) (nLanes: uint) =
    let getAbilities = basePowers
    let shuffledDeck =
        createUnshuffledDeck()
        |> shuffle
    let cardIDs = createIDs 1u<CID> shuffledDeck
    let cardInfos =
        shuffledDeck
        |> List.zip cardIDs
        |> List.map (fun (cardID, (rank, suit)) -> cardID, rank, suit)
    let bases, notBaseCards =
        cardInfos
        |> prepareHead (prepareBases getAbilities nLanes) (nPlayers*nLanes)
    let handCards, notDeckCards =
        notBaseCards
        |> prepareHead (prepareHands getAbilities nPlayers) (5u*nPlayers)
    let removed, notRemoved =
        notDeckCards
        |> prepareHead prepareRemoved 10u
    let drawPile =
        notRemoved
        |> List.map (fun (id, rank, suit) ->
            {DeckCardID = DeckCardID id; Rank = rank; Suit = suit}
            )
        |> NonEmptyList.fromList
    let gameState = GameStateBetweenTurns {
        CardsState = {
            Board = {
                Lanes =
                    List.replicate (int nLanes) emptyLane
                    |> createIDMap 1u<LID>
                Discard = List.empty
                }
            GetAbilities = getAbilities
            GameStage = Early {
                Bases = bases
                DrawPile = drawPile
                HandCards = handCards
            }
            Removed = removed
            }
        TurnState = {
            Player = 1u<PID>
            NPlayers = nPlayers
            Actions = 2u<action>
            FutureActionCounts = List.empty
            }
        }
    let nextActions =
        getPossibleActionPairs gameState
        |> List.map makeNextActionInfo
    InProgress (getDisplayInfo gameState, nextActions)

let api = {
    NewGame = fun () -> createGame 2u 3u
}
