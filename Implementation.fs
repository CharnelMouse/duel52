module Implementation
open Domain

let private createIDs start lst =
    [for i in 0..List.length lst - 1 -> start + LanguagePrimitives.Int32WithMeasure i]

let private createIDsToLength start len =
    [for i in 0..len - 1 -> start + LanguagePrimitives.Int32WithMeasure i]
let private zipIDs start lst =
    let IDs = [for i in 0..List.length lst - 1 -> start + LanguagePrimitives.Int32WithMeasure i]
    List.zip IDs lst
let private createIDMap start lst =
    zipIDs start lst
    |> Map.ofList

type private CardPowers = Map<CardID, Power>

type private HiddenCardKnownBys = (CardID * PlayerID) Set
type private Bases = Map<PlayerID, CardID>
type private HandCards = CardID list
type private RevealedCards = CardID Set

type private UnitOwners = Map<CardID, PlayerID>
type private UnitDamages = Map<CardID, Damage>

type private InactiveUnits = CardID list
type private ActiveUnits = CardID list
type private Pairs = (CardID * CardID) list
type private Readinesses = Map<CardID, Readiness>
type private FrozenUnits = Map<CardID, PlayerID>
type private UnitPairs = Map<CardID, CardID>

type private Discard = CardID list
type private RemovedCards = CardID Set

type private DrawPile = {
    TopCard: CardID
    Rest: CardID list
    }

type private Lane = {
    UnitOwners: UnitOwners
    UnitDamages: UnitDamages
    InactiveUnits: InactiveUnits
    ActiveUnits: ActiveUnits
    Pairs: Pairs
    Readinesses: Readinesses
    FrozenUnits: FrozenUnits
    UnitPairs: UnitPairs
}

let private emptyLane = {
    UnitOwners = Map.empty
    UnitDamages = Map.empty
    InactiveUnits = List.empty
    ActiveUnits = List.empty
    Pairs = List.empty
    Readinesses = Map.empty
    FrozenUnits = Map.empty
    UnitPairs = Map.empty
}

type private LaneControl =
| Contested
| Empty
| Won of PlayerID

let private laneSolePresence (unitOwners: UnitOwners) =
    let playerCounts =
        unitOwners
        |> Map.toList
        |> List.countBy (fun (id, owner) -> owner)
    match playerCounts with
    | [] -> Empty
    | [(controller, _)] -> Won controller
    | _ -> Contested

let private removeCardFromInactiveUnits cardID (lane: Lane) =
    {lane with InactiveUnits = List.filter (fun c -> c <> cardID) lane.InactiveUnits}
let private addCardToActiveUnits cardID (lane: Lane) =
    {lane with ActiveUnits = lane.ActiveUnits @ [cardID]}
let private addCardsToActiveUnits cardIDs (lane: Lane) =
    {lane with ActiveUnits = lane.ActiveUnits @ cardIDs}
let private addCardToReadinesses cardID (lane: Lane) =
    {lane with Readinesses = Map.add cardID Ready lane.Readinesses}
let private addCardsToReadinesses cardIDs (lane: Lane) =
    {lane with Readinesses = List.fold (fun m id -> Map.add id Ready m) lane.Readinesses cardIDs}
let private removeCardsFromUnitOwners cardIDs lane =
    {lane with UnitOwners = Map.filter (fun id _ -> not (List.contains id cardIDs)) lane.UnitOwners}
let private removeCardsFromUnitDamages cardIDs lane =
    {lane with UnitDamages = Map.filter (fun id _ -> not (List.contains id cardIDs)) lane.UnitDamages}
let private removeCardsFromInactiveUnits cardIDs (lane: Lane) =
    {lane with InactiveUnits = List.filter (fun id -> not (List.contains id cardIDs)) lane.InactiveUnits}
let private removeCardsFromActiveUnits cardIDs (lane: Lane) =
    {lane with ActiveUnits = List.filter (fun id -> not (List.contains id cardIDs)) lane.ActiveUnits}
let private addCardPairPartnersToActiveUnits cardIDs (lane: Lane) =
    let partners =
        lane.Pairs
        |> List.collect (fun (id1, id2) ->
            match List.contains id1 cardIDs, List.contains id2 cardIDs with
            | true, true ->
                []
            | true, false ->
                [id2]
            | false, true ->
                [id1]
            | false, false ->
                []
            )
    partners
    |> List.fold (fun ln id -> addCardToActiveUnits id ln) lane
let private removeCardsFromPairs cardIDs (lane: Lane) =
    {lane with Pairs = List.filter (fun (id1, id2) -> not (List.contains id1 cardIDs || List.contains id2 cardIDs)) lane.Pairs}
let private removeCardsFromReadinesses cardIDs (lane: Lane) =
    {lane with Readinesses = Map.filter (fun id _ -> not (List.contains id cardIDs)) lane.Readinesses}
let private removeCardsFromUnitPairs cardIDs lane =
    {lane with UnitPairs = Map.filter (fun id id2 -> not (List.contains id cardIDs || List.contains id2 cardIDs)) lane.UnitPairs}
let private addCardsToPairs cardID1 cardID2 lane =
    {lane with
        Pairs = lane.Pairs @ [cardID1, cardID2]
        }
let private addCardsToUnitPairs cardID1 cardID2 lane =
    {lane with
        UnitPairs =
            lane.UnitPairs
            |> Map.add cardID1 cardID2
            |> Map.add cardID2 cardID1
        }
let private exhaustCard cardID (lane: Lane) =
    {lane with
        Readinesses =
            Map.add cardID Exhausted lane.Readinesses
        }
let private damageCard cardID damage (lane: Lane) =
    {
        lane with
            UnitDamages =
                lane.UnitDamages
                |> Map.change cardID (function | None -> Some damage | Some prevDamage -> Some (prevDamage + damage))
        }

type private Board = {
    Lanes: Map<LaneID, Lane>
    Discard: Discard
    HiddenCardKnownBys: HiddenCardKnownBys
    RevealedCards: RevealedCards
}

let private changeLanesWithFn (fn: Lane -> Lane) (board: Board) =
    {board with Lanes = Map.map (fun _ l -> fn l) board.Lanes}
let private changeLaneWithFn laneID (fn: Lane -> Lane) (board: Board) =
    {board with
        Lanes =
            board.Lanes
            |> Map.map (fun n l ->
                if n = laneID then
                    fn l
                else
                    l
            )
        }
let private addCardToRevealedCards cardID board =
    {board with RevealedCards = Set.add cardID board.RevealedCards}
let private addCardsToRevealedCards cardIDs board =
    {board with RevealedCards = Set.union board.RevealedCards (Set.ofList cardIDs)}
let private removeCardFromKnownBys cardID board =
    {board with
        HiddenCardKnownBys = Set.filter (fun (cid, _) -> cid <> cardID) board.HiddenCardKnownBys
        }
let private removeCardsFromKnownBys cardIDs board =
    {board with
        HiddenCardKnownBys = Set.filter (fun (cid, _) -> not (List.contains cid cardIDs)) board.HiddenCardKnownBys
        }
let private changeDiscard discard (board: Board) =
    {board with Discard = discard}
let private findMaxHealth id cardPowers inactiveUnits =
    let power = Map.find id cardPowers
    let isInactive = List.contains id inactiveUnits
    match power, isInactive with
    | PassivePower Taunt, false ->
        3<health>
    | _ ->
        2<health>
let private findDeadCardsInLane laneID board (cardPowers: CardPowers) =
    let lane = Map.find laneID board.Lanes
    lane.UnitDamages
    |> Map.toList
    |> List.choose (fun (id, damage) ->
        let maxHealth = findMaxHealth id cardPowers lane.InactiveUnits
        if damage >= maxHealth then Some id else None
        )
let private findDeadCards board cardPowers =
    board.Lanes
    |> Map.toList
    |> List.collect (fun (id, _) -> findDeadCardsInLane id board cardPowers)
let private moveDeadCardsToDiscard cardPowers board =
    let deadCards = findDeadCards board cardPowers
    board
    |> changeLanesWithFn (
        removeCardsFromUnitOwners deadCards
        >> removeCardsFromInactiveUnits deadCards
        >> removeCardsFromActiveUnits deadCards
        >> addCardPairPartnersToActiveUnits deadCards
        >> removeCardsFromPairs deadCards
        >> removeCardsFromReadinesses deadCards
        >> removeCardsFromUnitPairs deadCards
    )
    |> changeDiscard (List.fold (fun discard id -> discard @ [id]) board.Discard deadCards)
let private flipAndActivateInactiveDeathPowersInLane laneID zeroHealthInactiveDeathPowerUnits (board: Board) =
    board
    |> changeLaneWithFn laneID (
        removeCardsFromUnitDamages zeroHealthInactiveDeathPowerUnits
        >> removeCardsFromInactiveUnits zeroHealthInactiveDeathPowerUnits
        >> addCardsToActiveUnits zeroHealthInactiveDeathPowerUnits
        >> addCardsToReadinesses zeroHealthInactiveDeathPowerUnits
        )
let private triggerTargetInactiveDeathPowers laneID cardPowers (board: Board) =
    let inactiveUnits = (Map.find laneID board.Lanes).InactiveUnits
    let zeroHealthInactiveDeathPowerUnits =
        findDeadCardsInLane laneID board cardPowers
        |> List.filter (fun id ->
            match Map.find id cardPowers, List.contains id inactiveUnits with
            | InactiveDeathPower p, true -> true
            | _ -> false
            )
    board
    |> flipAndActivateInactiveDeathPowersInLane laneID zeroHealthInactiveDeathPowerUnits
    |> removeCardsFromKnownBys zeroHealthInactiveDeathPowerUnits
    |> addCardsToRevealedCards zeroHealthInactiveDeathPowerUnits

type private EarlyGameInfo = {
    Bases: Map<LaneID, Bases>
    DrawPile: DrawPile
    HandCards: Map<PlayerID, HandCards>
}

type private LaneWins = Map<LaneID, PlayerID>

type private PostDrawGameInfo = {
    HandCards: Map<PlayerID, HandCards>
    LaneWins: LaneWins
}

type private PostHandGameInfo = {
    LaneWins: LaneWins
}

type private GameStage =
| Early of EarlyGameInfo
| DrawPileEmpty of PostDrawGameInfo
| HandsEmpty of PostHandGameInfo

type private CardsState = {
    Board: Board
    GameStage: GameStage
    CardPowers: CardPowers
    Removed: RemovedCards
}

let private changeBoard cardsState newBoard =
    {cardsState with Board = newBoard}
let private addCardToBoard cardID playerID laneID (cardsState: CardsState) =
    cardsState.Board
    |> changeLaneWithFn laneID (fun lane ->
        {
            lane with
                UnitOwners = Map.add cardID playerID lane.UnitOwners
                InactiveUnits = lane.InactiveUnits @ [cardID]
        }
        )
    |> changeBoard cardsState
let private removeCardFromHand cardID playerID (cardsState: CardsState) =
    match cardsState.GameStage with
    | Early gs ->
        let hands = gs.HandCards
        let newHands =
            hands
            |> Map.change playerID (function
                | Some lst -> lst |> List.filter (fun c -> c <> cardID) |> Some
                | None -> failwithf "Can't remove card from non-existant player's hand"
                )
        {cardsState with
            GameStage = Early {gs with HandCards = newHands}
            }
    | DrawPileEmpty gs ->
        let hands = gs.HandCards
        let newHands =
            hands
            |> Map.change playerID (function
                | Some lst -> lst |> List.filter (fun c -> c <> cardID) |> Some
                | None -> failwithf "Can't remove card from non-existant player's hand"
                )
        {cardsState with
            GameStage = DrawPileEmpty {gs with HandCards = newHands}
            }
    | HandsEmpty _ ->
        failwithf "Shouldn't be here!"
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
let private flipBasesOnLane (bases: Bases, lane: Lane) =
    let newUnitOwners =
        bases
        |> Map.fold (fun table playerID cardID -> Map.add cardID playerID table) lane.UnitOwners
    {
        lane with
            UnitOwners = newUnitOwners
        }
let private joinMaps map1 map2 = // left join
    map1
    |> Map.map (fun key value -> (value, Map.find key map2))
let private flipBasesOnBoard bases hiddenCardKnownBys {Lanes = lanes; Discard = discard; RevealedCards = revealedCards} =
    {
        Lanes =
            joinMaps bases lanes
            |> Map.map (fun _ bl -> flipBasesOnLane bl)
        Discard = discard
        HiddenCardKnownBys = hiddenCardKnownBys
        RevealedCards = revealedCards
        }

type private PlayerReady = {
    Player: PlayerID
    NPlayers: int
    Actions: int
    FutureActionCounts: int list
}

type private TurnInProgress = {
    CurrentPlayer: PlayerID
    NPlayers: int
    ActionsLeft: int
    FutureActionCounts: int list
}

type private GameStateBetweenTurns = {
    CardsState: CardsState
    TurnState: PlayerReady
}

type private GameStateDuringMidActionChoice = {
    CardsState: CardsState
    TurnState: TurnInProgress
    ChoiceContext: MidActionChoiceContext
}

type private GameStateDuringTurn = {
    CardsState: CardsState
    TurnState: TurnInProgress
}

type private GameStateWon = {
    Lanes: Map<LaneID, Lane>
    Winner: PlayerID
    LaneWins: LaneWins
}

type private GameStateTied = {
    Lanes: Map<LaneID, Lane>
    LaneWins: LaneWins
}

type private GameState =
| GameStateDuringMidActionChoice of GameStateDuringMidActionChoice
| GameStateBetweenTurns of GameStateBetweenTurns
| GameStateDuringTurn of GameStateDuringTurn
| GameStateWon of GameStateWon
| GameStateTied of GameStateTied

let private changeCardsState (gameState: GameStateDuringTurn) newCardsState =
    {gameState with CardsState = newCardsState}
let private addMidActionChoiceContext context (gameState: GameStateDuringTurn) =
    {
        CardsState = gameState.CardsState
        TurnState = gameState.TurnState
        ChoiceContext = context
    }

let private removeMidActionChoiceContext (gameState: GameStateDuringMidActionChoice) =
    {
        CardsState = gameState.CardsState
        TurnState = gameState.TurnState
    }
let private updateLaneWins (gameState: GameStateDuringTurn) =
    match gameState.CardsState.GameStage with
    | Early _ ->
        gameState
    | DrawPileEmpty gs ->
        let lanes = gameState.CardsState.Board.Lanes
        let currentLaneWins =
            lanes
            |> Map.toList
            |> List.choose (fun (laneID, lane) ->
                match laneSolePresence lane.UnitOwners with
                | Contested
                | Empty -> None
                | Won controller -> Some (laneID, controller)
                )
            |> Map.ofList
        let newGameState = DrawPileEmpty {gs with LaneWins = currentLaneWins}
        let newCardsState = {gameState.CardsState with GameStage = newGameState}
        newCardsState
        |> changeCardsState gameState
    | HandsEmpty gs ->
        let lanes = gameState.CardsState.Board.Lanes
        let currentLaneWins =
            lanes
            |> Map.toList
            |> List.choose (fun (laneID, lane) ->
                match laneSolePresence lane.UnitOwners with
                | Contested
                | Empty -> None
                | Won controller -> Some (laneID, controller)
                )
            |> Map.ofList
        let newGameState = HandsEmpty {gs with LaneWins = currentLaneWins}
        let newCardsState = {gameState.CardsState with GameStage = newGameState}
        newCardsState
        |> changeCardsState gameState
let private checkForGameEnd gameState =
    match gameState with
    | GameStateDuringTurn {CardsState = cs} ->
        match cs.GameStage with
        | Early _
        | DrawPileEmpty _ ->
            gameState
        | HandsEmpty {LaneWins = laneWins} ->
            let lanes = cs.Board.Lanes
            let wonLaneCounts =
                laneWins
                |> Map.toList
                |> List.countBy (fun (laneID, playerID) -> playerID)
            match wonLaneCounts with
            | [] -> gameState
            | lst ->
                let (leadingPlayer, leadingWins) =
                    lst
                    |> List.maxBy (fun (_, n) -> n)
                if leadingWins >= 2 then
                    GameStateWon {Winner = leadingPlayer; Lanes = lanes; LaneWins = laneWins}
                else
                    let contestedLanes =
                        lanes
                        |> Map.filter (fun laneID {UnitOwners = unitOwners} ->
                            match Map.containsKey laneID laneWins, Map.isEmpty unitOwners with
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
                        gameState
    | GameStateDuringMidActionChoice _
    | GameStateBetweenTurns _
    | GameStateWon _
    | GameStateTied _ ->
        gameState

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

let private createUnshuffledPowerDeck () =
    [
        ActivationPower View
        InactiveDeathPower Trap
        ActivationPower Foresight
        ActivationPower Flip
        ActivationPower Freeze
        ActivationPower Heal
        PassivePower Retaliate
        PassivePower Nimble
        PassivePower TwinStrike
        PassivePower Taunt
        PassivePower Vampiric
        ActivationPower Move
        ActivationPower Empower
        ActivationPower Action
    ]
    |> List.filter (fun power -> power <> PassivePower Vampiric)
    |> List.collect (List.replicate 4)

let private prepareHead fn n lst =
    let h, t = List.splitAt n lst
    fn h, t

let private prepareBaseTable lst = createIDMap 1<PID> lst

let private prepareBases nLanes lst =
    lst
    |> List.splitInto nLanes
    |> List.map prepareBaseTable
    |> createIDMap 1<LID>

let private prepareHands nPlayers (lst: CardID list) =
    let playerIDs =
        createIDsToLength 1<PID> nPlayers
        |> List.collect (List.replicate 5)
    List.zip lst playerIDs
    |> List.groupBy (fun (_, playerID) -> playerID)
    |> List.map (fun (playerID, lst) -> playerID, List.map (fun (cardID, _) -> cardID) lst)
    |> Map.ofList

let private prepareRemoved lst =
    lst
    |> Set.ofList

let private prepareDrawPile lst =
    match lst with
    | h :: t -> {TopCard = h; Rest = t}
    | [] -> failwithf "can't prepare a draw pile with no cards"

let private getBaseKnowledge (playerID: PlayerID) (baseCard: PlayerID * Power * PlayerID Set) =
    let (ownerID, power, knownBy) = baseCard
    if Set.contains playerID knownBy then
        KnownBaseCard (ownerID, power)
    else
        UnknownBaseCard ownerID

let private getTroops viewerID ownerID (cardPowers: CardPowers) (revealedCards: RevealedCards) (lane: Lane) (hiddenCardKnownBys: HiddenCardKnownBys) : InactiveUnitKnowledge list * ActiveUnitKnowledge list * PairKnowledge list =
    let pairPowers =
        lane.Pairs
        |> List.map (fun (x, _) -> Map.find x cardPowers)
    let pairDamages1, pairDamages2 =
        lane.Pairs
        |> List.map (fun (x, y) -> Map.find x lane.UnitDamages, Map.find y lane.UnitDamages)
        |> List.unzip
    let pairReadinesses =
        lane.Pairs
        |> List.map (fun (x, y) -> max (Map.find x lane.Readinesses) (Map.find y lane.Readinesses))
    let pairActionabilities =
        lane.Pairs
        |> List.map (fun (x, y) ->
            (if Map.containsKey x lane.FrozenUnits then Frozen else Normal),
            (if Map.containsKey y lane.FrozenUnits then Frozen else Normal)
            )
    let pairOwners =
        lane.Pairs
        |> List.map (fun (x, _) -> Map.find x lane.UnitOwners)
    let pairKnowledge =
        pairReadinesses
        |> List.zip3 pairDamages1 pairDamages2
        |> List.zip3 pairPowers pairActionabilities
        |> List.zip lane.Pairs
        |> List.map (fun ((cardID1, cardID2), (p, (a1, a2: Actionability), (d1, d2, r))) ->
            (cardID1, cardID2, p, d1, d2, r, a1, a2): PairKnowledge
            )
        |> List.zip pairOwners
        |> List.filter (fun (owner, _) -> owner = ownerID)
    let nonPairedActiveUnitKnowledge =
        lane.ActiveUnits
        |> List.filter (fun cardID -> not (Map.containsKey cardID lane.UnitPairs) && Map.find cardID lane.UnitOwners = ownerID)
        |> List.map (fun id ->
            let owner = Map.find id lane.UnitOwners
            let damage =
                match Map.tryFind id lane.UnitDamages with
                | Some d -> d
                | None -> 0<health>
            let actionability = if Map.containsKey id lane.FrozenUnits then Frozen else Normal
            owner, ((id, Map.find id cardPowers, damage, Map.find id lane.Readinesses, actionability): ActiveUnitKnowledge)
            )
    let inactiveUnitKnowledge =
        lane.InactiveUnits
        |> List.filter (fun cardID -> Map.find cardID lane.UnitOwners = ownerID)
        |> List.map (fun cardID ->
            let owner = Map.find cardID lane.UnitOwners
            let health =
                match Map.tryFind cardID lane.UnitDamages with
                | Some d -> d
                | None -> 0<health>
            let actionability = if Map.containsKey cardID lane.FrozenUnits then Frozen else Normal
            if Set.contains (cardID, viewerID) hiddenCardKnownBys then
                let power = Map.find cardID cardPowers
                owner, KnownInactiveCardKnowledge (cardID, power, health, actionability)
            else
                owner, UnknownInactiveCardKnowledge (cardID, health, actionability)
            )
    inactiveUnitKnowledge
    |> List.map (fun (_, knowledge) -> knowledge),
    nonPairedActiveUnitKnowledge
    |> List.map (fun (_, knowledge) -> knowledge),
    pairKnowledge
    |> List.map (fun (_, knowledge) -> knowledge)

let private getDeadCardKnowledge (playerID: PlayerID) (cardPowers: CardPowers) knownBys revealedCards cardID =
    let power = Map.find cardID cardPowers
    if Set.contains cardID revealedCards then
        KnownFaceUpDeadCard power
        |> KnownDeadCard
    elif Set.contains (cardID, playerID) knownBys then
        KnownFaceDownDeadCard power
        |> KnownDeadCard
    else
        UnknownDeadCard

let private getPlayerLaneWins (laneWins: LaneWins) =
    laneWins
    |> Map.toList
    |> List.groupBy (fun (_, pid) -> pid)
    |> List.map (fun (key, pairs) ->
        key,
        pairs
        |> List.map (fun (lid, _) -> lid)
        )

let private getDisplayInfo gameState =
    match gameState with
    | GameStateDuringMidActionChoice gs ->
        let id = gs.TurnState.CurrentPlayer
        let (playerHandInfo, opponentHandsInfo) =
            match gs.CardsState.GameStage with
            | Early {HandCards = hco} ->
                hco
                |> Map.partition (fun owner _ -> owner = id)
            | DrawPileEmpty {HandCards= hco} ->
                hco
                |> Map.partition (fun owner _ -> owner = id)
            | HandsEmpty _ ->
                Map.empty, Map.empty
        let playerHand =
            playerHandInfo
            |> Map.toList
            |> List.collect (fun (_, cards) -> cards)
        let cardPowers = gs.CardsState.CardPowers
        let getBase = getBaseKnowledge id
        let boardKnowledge =
            let {Lanes = l; Discard = d; HiddenCardKnownBys = kb; RevealedCards = rc} = gs.CardsState.Board
            let getDeadCard = getDeadCardKnowledge id gs.CardsState.CardPowers kb rc
            match gs.CardsState.GameStage with
            | Early {Bases = b; DrawPile = dp} ->
                let lanesKnowledge =
                    joinMaps b l
                    |> Map.map (fun _ (bases, lane) ->
                        let troopKnowledge =
                            createIDsToLength 1<PID> gs.TurnState.NPlayers
                            |> List.map (fun playerID ->
                                playerID, getTroops id playerID cardPowers rc lane kb
                                )
                            |> Map.ofList
                        {
                            Bases =
                                bases
                                |> Map.toList
                                |> List.map (fun (owner, cardID) ->
                                    owner,
                                    Map.find cardID gs.CardsState.CardPowers,
                                    kb
                                    |> Set.filter (fun (key, _) -> key = cardID)
                                    |> Set.map (fun (_, pid) -> pid)
                                    )
                                |> List.map getBase
                            Troops = troopKnowledge
                            } : PreBaseFlipLaneKnowledge
                        )
                let drawPileSize = 1 + List.length dp.Rest
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
                            createIDsToLength 1<PID> gs.TurnState.NPlayers
                            |> List.map (fun playerID ->
                                playerID, getTroops id playerID cardPowers rc lane kb
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
                            createIDsToLength 1<PID> gs.TurnState.NPlayers
                            |> List.map (fun playerID ->
                                playerID, getTroops id playerID cardPowers rc lane kb
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
        MidActionChoiceDisplayInfo {
            CurrentPlayer = id
            ActionsLeft = gs.TurnState.ActionsLeft
            BoardKnowledge = boardKnowledge
            PlayerHand =
                playerHand
                |> List.map (fun card ->
                    let power =
                        gs.CardsState.CardPowers
                        |> Map.find card
                    HandCard (card, power)
                    )
            OpponentHandSizes =
                opponentHandsInfo
                |> Map.map (fun _ cards -> List.length cards)
                |> Map.toList
            ChoiceContext = gs.ChoiceContext
        }
    | GameStateDuringTurn gs ->
        let id = gs.TurnState.CurrentPlayer
        let (playerHandInfo, opponentHandsInfo) =
            match gs.CardsState.GameStage with
            | Early {HandCards = hco} ->
                hco
                |> Map.partition (fun owner _ -> owner = id)
            | DrawPileEmpty {HandCards= hco} ->
                hco
                |> Map.partition (fun owner _ -> owner = id)
            | HandsEmpty _ ->
                Map.empty, Map.empty
        let playerHand =
            playerHandInfo
            |> Map.toList
            |> List.collect (fun (_, cards) -> cards)
        let cardPowers = gs.CardsState.CardPowers
        let getBase = getBaseKnowledge id
        let boardKnowledge =
            let {Lanes = l; Discard = d; HiddenCardKnownBys = kb; RevealedCards = rc} = gs.CardsState.Board
            let getDeadCard = getDeadCardKnowledge id gs.CardsState.CardPowers kb rc
            match gs.CardsState.GameStage with
            | Early {Bases = b; DrawPile = dp} ->
                let lanesKnowledge =
                    joinMaps b l
                    |> Map.map (fun _ (bases, lane) ->
                        let troopKnowledge =
                            createIDsToLength 1<PID> gs.TurnState.NPlayers
                            |> List.map (fun playerID ->
                                playerID, getTroops id playerID cardPowers rc lane kb
                                )
                            |> Map.ofList
                        {
                            Bases =
                                bases
                                |> Map.toList
                                |> List.map (fun (owner, cardID) ->
                                    owner,
                                    Map.find cardID gs.CardsState.CardPowers,
                                    kb
                                    |> Set.filter (fun (key, _) -> key = cardID)
                                    |> Set.map (fun (_, pid) -> pid)
                                    )
                                |> List.map getBase
                            Troops = troopKnowledge
                            } : PreBaseFlipLaneKnowledge
                        )
                let drawPileSize = 1 + List.length dp.Rest
                PreBaseFlipBoardKnowledge {
                    Lanes = lanesKnowledge
                    DrawPileSize = drawPileSize
                    Discard = List.map getDeadCard d
                    }
            | DrawPileEmpty {LaneWins = laneWins} ->
                let lanesKnowledge =
                    l
                    |> Map.map (fun laneID lane  ->
                        let troopKnowledge =
                            createIDsToLength 1<PID> gs.TurnState.NPlayers
                            |> List.map (fun playerID ->
                                playerID, getTroops id playerID cardPowers rc lane kb
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
                            createIDsToLength 1<PID> gs.TurnState.NPlayers
                            |> List.map (fun playerID ->
                                playerID, getTroops id playerID cardPowers rc lane kb
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
        TurnDisplayInfo {
            CurrentPlayer = id
            ActionsLeft = gs.TurnState.ActionsLeft
            BoardKnowledge = boardKnowledge
            PlayerHand =
                playerHand
                |> List.map (fun card ->
                    let power =
                        gs.CardsState.CardPowers
                        |> Map.find card
                    HandCard (card, power)
                    )
            OpponentHandSizes =
                opponentHandsInfo
                |> Map.map (fun _ cards -> List.length cards)
                |> Map.toList
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

let private getPlayActionsInfo (gameState: GameStateDuringTurn) =
    let playerID = gameState.TurnState.CurrentPlayer
    match gameState.CardsState.GameStage with
    | Early {HandCards = hc}
    | DrawPileEmpty {HandCards = hc} ->
        Map.find playerID hc
    | HandsEmpty _ ->
        List.empty
    |> List.allPairs (createIDsToLength 1<LID> (Map.count gameState.CardsState.Board.Lanes))
    |> List.map (fun (laneID, cardID) ->
        Play (playerID, cardID, laneID)
        |> TurnActionInfo
        )

let private getActivateActionsInfo (gameState: GameStateDuringTurn) =
    let playerID = gameState.TurnState.CurrentPlayer
    let lanes = gameState.CardsState.Board.Lanes
    lanes
    |> Map.toList
    |> List.collect (fun (laneID, lane) ->
        let ownTroops =
            lane.InactiveUnits
            |> List.filter (fun id -> Map.find id lane.UnitOwners = playerID)
        ownTroops
        |> List.filter (fun id -> not (Map.containsKey id lane.FrozenUnits))
        |> List.map (fun id -> Activate (playerID, laneID, id) |> TurnActionInfo)
        )

let private getPairActionsInfoFromUnits powers playerID laneID ownActiveUnits =
    let rec distPairs lst =
        match lst with
        | [] -> []
        | [_] -> []
        | h::t -> List.allPairs [h] t @ distPairs t
    ownActiveUnits
    |> distPairs
    |> List.choose (fun (id1, id2) ->
        if Map.find id1 powers = Map.find id2 powers then
            CreatePair (playerID, laneID, id1, id2)
            |> TurnActionInfo
            |> Some
        else
            None
        )

let private getAttackActionsInfo (gameState: GameStateDuringTurn) =
    let playerID = gameState.TurnState.CurrentPlayer
    let lanes = gameState.CardsState.Board.Lanes
    lanes
    |> Map.toList
    |> List.collect (fun (laneID, lane) ->
        let pairToList (x, y) = [x; y]
        let findOwner toList x =
            let first = toList >> List.head
            Map.find (first x) lane.UnitOwners

        let possibleAttackers toList =
            List.filter (findOwner toList >> (=) playerID)
            >> List.filter (
                toList
                >> List.forall (fun id ->
                    Map.find id lane.Readinesses = Ready && not (Map.containsKey id lane.FrozenUnits)
                    )
                )
        let possibleUnitAttackers = possibleAttackers List.singleton lane.ActiveUnits
        let possiblePairAttackers = possibleAttackers pairToList lane.Pairs

        let addTypeAndOwner T owner x = T (owner, x)
        let possibleTypeTargets toList T  =
            let transform owner = toList >> List.map (addTypeAndOwner T owner)
            List.groupBy (findOwner toList)
            >> List.filter (fun (owner, _) -> owner <> playerID)
            >> List.collect (fun (owner, units) ->
                units
                |> List.collect (transform owner)
                )
        let possibleSingleTypeTargets T = possibleTypeTargets List.singleton T
        let possiblePairTypeTargets T = possibleTypeTargets pairToList T
        let possibleInactiveUnitTargets = possibleSingleTypeTargets InactiveTarget lane.InactiveUnits
        let possibleActiveUnitTargets = possibleSingleTypeTargets ActiveSingleTarget lane.ActiveUnits
        let possiblePairTargets = possiblePairTypeTargets ActivePairMemberTarget lane.Pairs
        let allTargets = possibleInactiveUnitTargets @ possibleActiveUnitTargets @ possiblePairTargets

        let singleAttacks =
            List.allPairs possibleUnitAttackers allTargets
            |> List.map (fun (attacker, target) ->
                SingleAttack (playerID, laneID, attacker, target)
                |> TurnActionInfo
                )
        let pairAttacks =
            List.allPairs possiblePairAttackers allTargets
            |> List.map (fun ((attackerID1, attackerID2), target) ->
                PairAttack (playerID, laneID, (attackerID1, attackerID2), target)
                |> TurnActionInfo
                )
        singleAttacks @ pairAttacks
        )

let private getPairActionsInfo (gameState: GameStateDuringTurn) =
    let playerID = gameState.TurnState.CurrentPlayer
    let lanes = gameState.CardsState.Board.Lanes
    lanes
    |> Map.toList
    |> List.collect (fun (laneID, {UnitOwners = unitOwners; ActiveUnits = activeUnits}) ->
        activeUnits
        |> List.filter (fun id -> Map.find id unitOwners = playerID)
        |> getPairActionsInfoFromUnits gameState.CardsState.CardPowers playerID laneID
        )

let private getPossibleActionsInfo (gameState: GameState) =
    match gameState with
    | GameStateBetweenTurns gs ->
        StartTurn gs.TurnState.Player
        |> List.singleton
    | GameStateDuringTurn gs ->
        if gs.TurnState.ActionsLeft = 0 then
            EndTurn gs.TurnState.CurrentPlayer
            |> List.singleton
        else
            let currentPlayer = gs.TurnState.CurrentPlayer
            if gs.TurnState.ActionsLeft = 0 then
                EndTurn currentPlayer
                |> List.singleton
            else
                let actions =
                    getPlayActionsInfo gs
                    @ getActivateActionsInfo gs
                    @ getAttackActionsInfo gs
                    @ getPairActionsInfo gs
                if List.isEmpty actions then
                    EndTurn currentPlayer
                    |> List.singleton
                else
                    actions
    | GameStateDuringMidActionChoice gs ->
        match gs.ChoiceContext with
        | DiscardChoiceContext (playerID, powerCardID) ->
            match gs.CardsState.GameStage with
            | Early {HandCards = hc}
            | DrawPileEmpty {HandCards = hc} ->
                hc
                |> Map.find playerID
                |> List.map (fun cardID -> DiscardChoice (playerID, powerCardID, cardID) |> MidActionChoiceInfo)
            | HandsEmpty _ ->
                failwithf "Can't discard from an empty hand"
        | ForesightChoiceContext (playerID, powerCardID) ->
            let inactiveUnits =
                gs.CardsState.Board.Lanes
                |> Map.toList
                |> List.collect (fun (laneID, lane) -> lane.InactiveUnits)
            let faceDownCards =
                match gs.CardsState.GameStage with
                | Early {Bases = bases} ->
                    let baseIDs =
                        bases
                        |> Map.toList
                        |> List.collect (fun (laneID, b) ->
                            b
                            |> Map.toList
                            |> List.map (fun (playerID, d) -> d)
                            )
                    baseIDs @ inactiveUnits
                | DrawPileEmpty _
                | HandsEmpty _ ->
                    inactiveUnits
            faceDownCards
            |> List.filter (fun id -> not (Set.contains (id, playerID) gs.CardsState.Board.HiddenCardKnownBys))
            |> List.map (fun id -> ForesightChoice (playerID, powerCardID, id) |> MidActionChoiceInfo)
        | TwinStrikeChoiceContext (playerID, laneID, powerCardID, originalTargetCardID) ->
            let lane = Map.find laneID gs.CardsState.Board.Lanes
            lane.UnitOwners
            |> Map.toList
            |> List.choose (fun (cardID, ownerID) ->
                if ownerID = playerID || cardID = originalTargetCardID then
                    None
                else
                    Some (TwinStrikeChoice (playerID, laneID, powerCardID, cardID) |> MidActionChoiceInfo)
                )
    | GameStateWon _
    | GameStateTied _ ->
        List.empty

let private executeMidActionChoice midActionChoice (gameState: GameStateDuringMidActionChoice) =
    match midActionChoice with
    | DiscardChoice (playerID, _, discardeeCardID) ->
        let cs = gameState.CardsState
        match cs.GameStage with
        | Early gs ->
            let currentHand = Map.find playerID gs.HandCards
            let newStage = Early {
                gs with
                    HandCards =
                        gs.HandCards
                        |> Map.add playerID (currentHand |> List.filter ((<>) discardeeCardID))
                }
            let newCardsState = {
                cs with
                    GameStage = newStage
                    Board = {cs.Board with Discard = cs.Board.Discard @ [discardeeCardID]}
                }
            {gameState with CardsState = newCardsState}
        | DrawPileEmpty gs ->
            let currentHand = Map.find playerID gs.HandCards
            let newStage = DrawPileEmpty {
                gs with
                    HandCards =
                        gs.HandCards
                        |> Map.add playerID (currentHand |> List.filter ((<>) discardeeCardID))
                }
            let newCardsState = {
                cs with
                    GameStage = newStage
                    Board = {cs.Board with Discard = cs.Board.Discard @ [discardeeCardID]}
                }
            {gameState with CardsState = newCardsState}
        | HandsEmpty _ ->
            failwithf "Can't discard from an empty hand"
    | ForesightChoice (playerID, powerCardID, targetCardID) ->
        let newKnownBy =
            gameState.CardsState.Board.HiddenCardKnownBys
            |> Set.add (targetCardID, playerID)
        let newBoard = {gameState.CardsState.Board with HiddenCardKnownBys = newKnownBy}
        let newCardsState =
            newBoard
            |> changeBoard gameState.CardsState
        {gameState with CardsState = newCardsState}
    | TwinStrikeChoice (playerID, laneID, powerCardID, targetCardID) ->
        let newCardsState =
            gameState.CardsState.Board
            |> changeLaneWithFn laneID (damageCard targetCardID 1<health>)
            |> moveDeadCardsToDiscard gameState.CardsState.CardPowers
            |> changeBoard gameState.CardsState
        {gameState with
            CardsState = newCardsState
            }
    |> removeMidActionChoiceContext

let private executePlayAction cardID laneID (gameState: GameStateDuringTurn) =
    let playerID = gameState.TurnState.CurrentPlayer
    let cardsState = gameState.CardsState
    cardsState
    |> addCardToBoard cardID playerID laneID
    |> removeCardFromHand cardID playerID
    |> removeHandsIfAllEmpty
    |> changeCardsState gameState

let private tryDrawCard playerID (gameState: GameStateDuringTurn) =
    let cardsState = gameState.CardsState
    match cardsState.GameStage with
    | Early preInfo ->
        let boardInfo = cardsState.Board
        let hands = preInfo.HandCards
        let drawPile = preInfo.DrawPile
        match drawPile.Rest with
        | [] ->
            let newHandCards =
                hands
                |> Map.change playerID (function
                    | Some hc ->
                        Some (hc @ [drawPile.TopCard])
                    | None ->
                        failwithf "non-existent players can't draw cards"
                    )
            let newKnownBys =
                boardInfo.HiddenCardKnownBys
                |> Set.add (drawPile.TopCard, playerID)
            let newCards = {
                cardsState with
                    Board = flipBasesOnBoard preInfo.Bases newKnownBys boardInfo
                    GameStage = DrawPileEmpty {HandCards = newHandCards; LaneWins = Map.empty}
                }
            newCards
            |> changeCardsState gameState
        | newTopCard :: newRest ->
            let newHandCards =
                hands
                |> Map.change playerID (function
                    | Some hc ->
                        Some (hc @ [drawPile.TopCard])
                    | None ->
                        failwithf "non-existent players can't draw cards"
                    )
            let newKnownBys =
                boardInfo.HiddenCardKnownBys
                |> Set.add (drawPile.TopCard, playerID)
            let newCards =
                {cardsState with
                    GameStage =
                        Early {
                            preInfo with
                                DrawPile = {TopCard = newTopCard; Rest = newRest}
                                HandCards = newHandCards
                            }
                    Board = {boardInfo with HiddenCardKnownBys = newKnownBys}
                }
            newCards
            |> changeCardsState gameState
    | DrawPileEmpty _
    | HandsEmpty _ ->
        gameState

let private healOwnUnitsInLane playerID amount (lane: Lane) =
    let ownUnitIDs =
        lane.UnitOwners
        |> Map.toList
        |> List.choose (fun (id, owner) ->
            if owner = playerID then
                Some id
            else
                None
        )
    let newDamages = 
        ownUnitIDs
        |> List.fold (fun damages id ->
            Map.change id (fun maybeDamage ->
                match maybeDamage with
                | Some d ->
                    if d <= amount then
                        None
                    else
                        Some (d - amount)
                | None -> None
                ) damages
            ) lane.UnitDamages
    {lane with UnitDamages = newDamages}

let private freezeEnemyNonActiveNimbleUnitsInLane playerID cardPowers lane =
    let nonPlayerOwnedNonActiveNimbleUnits =
        lane.UnitOwners
        |> Map.toList
        |> List.choose (fun (unitID, unitOwnerID) ->
            if unitOwnerID <> playerID then
                Some unitID
            else
                None
            )
        |> List.filter (fun id ->
            Map.find id cardPowers <> PassivePower Nimble
            || List.contains id lane.InactiveUnits
            )
    let newFrozenUnits =
        nonPlayerOwnedNonActiveNimbleUnits
        |> List.fold (fun fu id -> Map.add id playerID fu) lane.FrozenUnits
    {lane with FrozenUnits = newFrozenUnits}

let private resolveActivationPower playerID laneID cardID (gameState: GameStateDuringTurn) =
    let cardsState = gameState.CardsState
    let {Board = board; CardPowers = powers} = cardsState
    match Map.find cardID powers with
    | ActivationPower View ->
        gameState
        |> tryDrawCard playerID
        |> addMidActionChoiceContext (DiscardChoiceContext (playerID, cardID))
        |> GameStateDuringMidActionChoice
    | ActivationPower Foresight ->
        gameState
        |> addMidActionChoiceContext (ForesightChoiceContext (playerID, cardID))
        |> GameStateDuringMidActionChoice
    | ActivationPower Flip ->
        gameState
        |> GameStateDuringTurn
    | ActivationPower Freeze ->
        board
        |> changeLaneWithFn laneID (freezeEnemyNonActiveNimbleUnitsInLane playerID powers)
        |> changeBoard cardsState
        |> changeCardsState gameState
        |> GameStateDuringTurn
    | ActivationPower Heal ->
        board
        |> changeLanesWithFn (healOwnUnitsInLane playerID 2<health>)
        |> changeBoard cardsState
        |> changeCardsState gameState
        |> GameStateDuringTurn
    | ActivationPower Move
    | ActivationPower Empower
    | ActivationPower Action ->
        gameState
        |> GameStateDuringTurn
    | InactiveDeathPower _
    | PassivePower _ ->
        gameState
        |> GameStateDuringTurn

let private resolveAttackerPassivePower playerID laneID unitCardIDs attackedCardID (gameState: GameStateDuringTurn) =
    let cardsState = gameState.CardsState
    let cardID =
        match unitCardIDs with
        | SingleCardID id -> id
        | PairIDs (id, _) -> id
    match Map.find cardID cardsState.CardPowers with
    | PassivePower Retaliate
    | PassivePower Nimble
    | PassivePower Taunt ->
       gameState
       |> GameStateDuringTurn
    | PassivePower TwinStrike ->
        {
            CardsState = cardsState
            TurnState = gameState.TurnState
            ChoiceContext = TwinStrikeChoiceContext (playerID, laneID, unitCardIDs, attackedCardID)
        }
        |> GameStateDuringMidActionChoice
    | PassivePower Vampiric ->
        gameState
        |> GameStateDuringTurn
    | ActivationPower _
    | InactiveDeathPower _ ->
        gameState
        |> GameStateDuringTurn

let private executeActivateAction playerID laneID cardID (gameState: GameStateDuringTurn) =
    let cardsState = gameState.CardsState
    cardsState.Board
    |> changeLaneWithFn laneID (
        removeCardFromInactiveUnits cardID
        >> addCardToActiveUnits cardID
        >> addCardToReadinesses cardID
        )
    |> removeCardFromKnownBys cardID
    |> addCardToRevealedCards cardID
    |> changeBoard cardsState
    |> changeCardsState gameState
    |> resolveActivationPower playerID laneID cardID

let private getBonusDefenderDamage attackerPower targetPower targetInfo =
    match attackerPower, targetPower, targetInfo with
    | PassivePower Nimble, PassivePower Taunt, ActiveSingleTarget _
    | PassivePower Nimble, PassivePower Taunt, ActivePairMemberTarget _ ->
        1<health>
    | _ ->
        0<health>

let private getTargetIDFromTargetInfo targetInfo =
    match targetInfo with
    | InactiveTarget (owner, id) -> id
    | ActiveSingleTarget (owner, id) -> id
    | ActivePairMemberTarget (owner, id) -> id

let private getAttackerSelfDamage attackerPower targetPower targetInfo =
    match attackerPower, targetPower, targetInfo with
    | PassivePower Nimble, _, _ ->
        0<health>
    | _, PassivePower Retaliate, ActiveSingleTarget _
    | _, PassivePower Retaliate, ActivePairMemberTarget _ ->
        1<health>
    | _ ->
        0<health>

let private getSingleAttackInfo attackerID targetInfo cardPowers =
    let targetID = getTargetIDFromTargetInfo targetInfo
    let attackerPower = Map.find attackerID cardPowers
    let targetPower = Map.find targetID cardPowers
    let baseDefenderDamage = 1<health>
    let bonusDefenderDamage = getBonusDefenderDamage attackerPower targetPower targetInfo
    let selfDamage = getAttackerSelfDamage attackerPower targetPower targetInfo
    targetID, baseDefenderDamage + bonusDefenderDamage, selfDamage

let private getPairAttackInfo pairMemberID targetInfo cardPowers =
    let targetID = getTargetIDFromTargetInfo targetInfo
    let attackerPower = Map.find pairMemberID cardPowers
    let targetPower = Map.find targetID cardPowers
    let baseDefenderDamage = 2<health>
    let bonusDefenderDamage = getBonusDefenderDamage attackerPower targetPower targetInfo
    let selfDamage = getAttackerSelfDamage attackerPower targetPower targetInfo
    targetID, baseDefenderDamage + bonusDefenderDamage, selfDamage

let private executeSingleAttackAction playerID laneID attackerID targetInfo (gameState: GameStateDuringTurn) =
    let cardsState = gameState.CardsState
    let board = cardsState.Board
    let targetID, damage, selfDamage =
        getSingleAttackInfo attackerID targetInfo cardsState.CardPowers
    board
    |> changeLaneWithFn laneID (
        exhaustCard attackerID
        >> damageCard targetID damage
        >> damageCard attackerID selfDamage
        )
    |> triggerTargetInactiveDeathPowers laneID cardsState.CardPowers
    |> moveDeadCardsToDiscard cardsState.CardPowers
    |> changeBoard cardsState
    |> changeCardsState gameState
    |> resolveAttackerPassivePower playerID laneID (SingleCardID attackerID) targetID

let private executePairAttackAction playerID laneID (attackerID1, attackerID2) targetInfo (gameState: GameStateDuringTurn) =
    let cardsState = gameState.CardsState
    let board = cardsState.Board
    let targetID, damage, selfDamage =
        getPairAttackInfo attackerID1 targetInfo cardsState.CardPowers
    board
    |> changeLaneWithFn laneID (
        exhaustCard attackerID1
        >> exhaustCard attackerID2
        >> damageCard targetID damage
        >> damageCard attackerID1 selfDamage
        >> damageCard attackerID2 selfDamage
        )
    |> triggerTargetInactiveDeathPowers laneID cardsState.CardPowers
    |> moveDeadCardsToDiscard cardsState.CardPowers
    |> changeBoard cardsState
    |> changeCardsState gameState
    |> resolveAttackerPassivePower playerID laneID (PairIDs (attackerID1, attackerID2)) targetID

let private executeCreatePairAction laneID cardID1 cardID2 (gameState: GameStateDuringTurn) =
    gameState.CardsState.Board
    |> changeLaneWithFn laneID (
        removeCardsFromActiveUnits [cardID1; cardID2]
        >> addCardsToPairs cardID1 cardID2
        >> addCardsToUnitPairs cardID1 cardID2
        )
    |> changeBoard gameState.CardsState
    |> changeCardsState gameState

let private decrementActionsLeft (gameState: GameStateDuringTurn) =
    {gameState with
        TurnState = {
            gameState.TurnState with
                ActionsLeft = gameState.TurnState.ActionsLeft - 1
            }
        }

let private executeTurnAction action gameState =
    let gs = decrementActionsLeft gameState
    let postAction =
        match action with
        | Play (playerID, cardID, laneID) ->
            executePlayAction cardID laneID gs
            |> GameStateDuringTurn
        | Activate (playerID, laneID, cardID) ->
            executeActivateAction playerID laneID cardID gs
        | SingleAttack (playerID, laneID, attackerID, targetInfo) ->
            executeSingleAttackAction playerID laneID attackerID targetInfo gs
        | PairAttack (playerID, laneID, (attackerID1, attackerID2), targetInfo) ->
            executePairAttackAction playerID laneID (attackerID1, attackerID2) targetInfo gs
        | CreatePair (playerID, laneID, cardID1, cardID2) ->
            executeCreatePairAction laneID cardID1 cardID2 gs
            |> GameStateDuringTurn
    match postAction with
    | GameStateDuringMidActionChoice _ ->
        postAction
    | GameStateDuringTurn pa ->
        updateLaneWins pa
        |> GameStateDuringTurn
    | GameStateBetweenTurns _
    | GameStateWon _
    | GameStateTied _ ->
        failwithf "Shouldn't be here!"

let private startPlayerTurn playerID (gameState: GameStateBetweenTurns) : GameStateDuringTurn =
    let ts = gameState.TurnState
    {
        CardsState = gameState.CardsState
        TurnState = {
            CurrentPlayer = playerID
            NPlayers = ts.NPlayers
            ActionsLeft = ts.Actions
            FutureActionCounts = ts.FutureActionCounts
            }
        }

let private timeoutOwnedFreezeStatesInLane playerID lane =
    {lane with FrozenUnits = Map.filter (fun cardID _ -> Map.find cardID lane.UnitOwners <> playerID) lane.FrozenUnits}

let private timeoutOwnedFreezeStates playerID (cardsState: CardsState): CardsState =
    cardsState.Board
    |> changeLanesWithFn (timeoutOwnedFreezeStatesInLane playerID)
    |> changeBoard cardsState

let private readyActiveUnits activeUnits =
    activeUnits
    |> Map.map (fun _ _ -> Ready)

let private readyAllActiveCards cardsState =
    cardsState.Board
    |> changeLanesWithFn (fun lane ->
        {lane with Readinesses = readyActiveUnits lane.Readinesses}
        )
    |> changeBoard cardsState

let rec private makeNextActionInfo gameState action =
    let newGameState =
        match gameState, action with
        | GameStateDuringMidActionChoice gs, MidActionChoiceInfo maci ->
            executeMidActionChoice maci gs
            |> GameStateDuringTurn
            |> checkForGameEnd
        | GameStateDuringTurn gs, TurnActionInfo tai ->
            executeTurnAction tai gs
            |> checkForGameEnd
        | GameStateDuringTurn gs, EndTurn _ ->
            let tip = gs.TurnState
            let nextPlayer =
                if int tip.CurrentPlayer = tip.NPlayers then
                    1<PID>
                else
                    tip.CurrentPlayer + 1<PID>
            let actions, nextFutureActionCounts =
                match tip.FutureActionCounts with
                | [] -> 3, []
                | h :: t -> h, t
            GameStateBetweenTurns {
                CardsState =
                    readyAllActiveCards gs.CardsState
                    |> timeoutOwnedFreezeStates tip.CurrentPlayer
                TurnState = {
                    Player = nextPlayer
                    NPlayers = tip.NPlayers
                    Actions = actions
                    FutureActionCounts = nextFutureActionCounts
                    }
                }
        | GameStateBetweenTurns gs, StartTurn id ->
            gs
            |> startPlayerTurn id
            |> tryDrawCard id
            |> GameStateDuringTurn
        | GameStateDuringMidActionChoice _, TurnActionInfo _
        | GameStateDuringMidActionChoice _, StartTurn _
        | GameStateDuringMidActionChoice _, EndTurn _
        | GameStateDuringTurn _, MidActionChoiceInfo _
        | GameStateDuringTurn _, StartTurn _
        | GameStateDuringTurn _, EndTurn _
        | GameStateBetweenTurns _, MidActionChoiceInfo _
        | GameStateBetweenTurns _, TurnActionInfo _
        | GameStateBetweenTurns _, EndTurn _
        | GameStateWon _, _
        | GameStateTied _, _ ->
            failwithf "action incompatible with game state"
    let possibleActionsInfo = getPossibleActionsInfo newGameState
    let checkedGameState, checkedPossibleActionsInfo =
        match newGameState, possibleActionsInfo with
        | GameStateDuringMidActionChoice gs, [] ->
            let state = removeMidActionChoiceContext gs |> GameStateDuringTurn
            state, getPossibleActionsInfo state
        | _ ->
            newGameState, possibleActionsInfo
    let newDisplayInfo = getDisplayInfo checkedGameState
    // Generation of next action's resulting capabilities is part of the
    // generated capability's body, since assigning them here requires
    // generating the entire game space, blowing the stack
    let capability() =
        InProgress (
            newDisplayInfo,
            checkedPossibleActionsInfo
            |> List.map (makeNextActionInfo checkedGameState)
            )
    {
        Action = action
        Capability = capability
        }

let private createGame nPlayers nLanes =
    let shuffledPowerDeck =
        createUnshuffledPowerDeck()
        |> shuffle
    let cardIndices =
        createIDs 1<CID> shuffledPowerDeck
    let cardPowers =
        shuffledPowerDeck
        |> List.zip cardIndices
        |> Map.ofList
    let bases, notBaseCards =
        cardIndices
        |> prepareHead (prepareBases nLanes) (nPlayers*nLanes)
    let handCards, notDeckCards =
        notBaseCards
        |> prepareHead (prepareHands nPlayers) (5*nPlayers)
    let removed, notRemoved =
        notDeckCards
        |> prepareHead prepareRemoved 10
    let drawPile = prepareDrawPile notRemoved
    let gameState = GameStateBetweenTurns {
        CardsState = {
            Board = {
                Lanes =
                    List.replicate nLanes emptyLane
                    |> createIDMap 1<LID>
                Discard = List.empty
                HiddenCardKnownBys =
                    handCards
                    |> Map.toList
                    |> List.collect (fun (playerID, cards) -> List.map (fun card -> card, playerID) cards)
                    |> Set.ofList
                RevealedCards = Set.empty
                }
            GameStage = Early {
                Bases = bases
                DrawPile = drawPile
                HandCards = handCards
            }
            CardPowers = cardPowers
            Removed = removed
            }
        TurnState = {
            Player = 1<PID>
            NPlayers = nPlayers
            Actions = 2
            FutureActionCounts = List.empty
            }
        }
    let displayInfo = getDisplayInfo gameState
    let nextActions =
        getPossibleActionsInfo gameState
        |> List.map (makeNextActionInfo gameState)
    InProgress (displayInfo, nextActions)

let api = {
    NewGame = fun () -> createGame 2 3
}
