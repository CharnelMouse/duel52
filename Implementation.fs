module Implementation
open Domain

let private createIDs start lst =
    [for i in 0..List.length lst - 1 -> start + LanguagePrimitives.Int32WithMeasure i]

let private createIDsToLength start len =
    [for i in 0..len - 1 -> start + LanguagePrimitives.Int32WithMeasure i]

[<Measure>] type private CID
type private CardID = int<CID>

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
    UnitPairs: UnitPairs
}

let private emptyLane = {
    UnitOwners = Map.empty
    UnitDamages = Map.empty
    InactiveUnits = List.empty
    ActiveUnits = List.empty
    Pairs = List.empty
    Readinesses = Map.empty
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
let private exhaustCards cardIDs (lane: Lane) =
    {lane with
        Readinesses =
            cardIDs
            |> List.fold (fun rs id -> Map.add id Exhausted rs) lane.Readinesses
        }
let private damageCard cardID damage (lane: Lane) =
    {
        lane with
            UnitDamages =
                lane.UnitDamages
                |> Map.change cardID (function | None -> Some damage | Some prevDamage -> Some (prevDamage + damage))
        }

type private Board = {
    Lanes: Lane list
    Discard: Discard
    HiddenCardKnownBys: HiddenCardKnownBys
    RevealedCards: RevealedCards
}

let private changeLaneWithFn laneID (fn: Lane -> Lane) (board: Board) =
    {board with
        Lanes =
            board.Lanes
            |> List.mapi (fun n l ->
                if n = (int laneID - 1) then
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
let private findDeadCards (laneID: LaneID) (board: Board) (cardPowers: CardPowers) =
    let lane = List.item (int laneID - 1) board.Lanes
    lane.UnitDamages
    |> Map.toList
    |> List.choose (fun (id, damage) ->
        let maxHealth = findMaxHealth id cardPowers lane.InactiveUnits
        if damage >= maxHealth then Some id else None
        )
let private triggerTrapsAndMoveDeadCardsToDiscard laneID cardPowers (board: Board) =
    let zeroHealthCards = findDeadCards laneID board cardPowers
    let inactiveUnits = board.Lanes.[int laneID - 1].InactiveUnits
    let inactiveZeroHealthTraps, deadCards =
        zeroHealthCards
        |> List.partition (fun id ->
            match Map.find id cardPowers, List.contains id inactiveUnits with
            | ActivationPower Trap, true -> true
            | _ -> false
            )
    board
    |> changeLaneWithFn laneID (
        removeCardsFromUnitOwners deadCards
        >> removeCardsFromUnitDamages zeroHealthCards
        >> removeCardsFromInactiveUnits zeroHealthCards
        >> removeCardsFromActiveUnits deadCards
        >> addCardPairPartnersToActiveUnits deadCards
        >> removeCardsFromPairs deadCards
        >> removeCardsFromReadinesses deadCards
        >> removeCardsFromUnitPairs deadCards
        >> addCardsToActiveUnits inactiveZeroHealthTraps
        >> addCardsToReadinesses inactiveZeroHealthTraps
        )
    |> removeCardsFromKnownBys inactiveZeroHealthTraps
    |> addCardsToRevealedCards inactiveZeroHealthTraps
    |> changeDiscard (List.fold (fun discard id -> discard @ [id]) board.Discard deadCards)

type private EarlyGameInfo = {
    Bases: Bases list
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

let private addCardToBoard cardID playerID laneID (cardsState: CardsState) =
    let board = cardsState.Board
    let l = board.Lanes
    let newLanes =
        l
        |> List.mapi (fun n lane ->
            if (n + 1)*1<LID> = laneID then
                {
                    lane with
                        UnitOwners = Map.add cardID playerID lane.UnitOwners
                        InactiveUnits = lane.InactiveUnits @ [cardID]
                }
            else
                lane
            )
    {cardsState with Board = {board with Lanes = newLanes}}
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
let private changeBoard cardsState newBoard =
    {cardsState with Board = newBoard}
let private flipBasesOnLane (bases: Bases, lane: Lane) =
    let newUnitOwners =
        bases
        |> Map.fold (fun table playerID cardID -> Map.add cardID playerID table) lane.UnitOwners
    {
        lane with
            UnitOwners = newUnitOwners
        }
let private flipBasesOnBoard bases lanes discard hiddenCardKnownBys revealedCards =
    {
        Lanes = List.map flipBasesOnLane (List.zip bases lanes)
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

type private GameStateDuringTurn = {
    CardsState: CardsState
    TurnState: TurnInProgress
}

type private GameStateWon = {
    Lanes: Lane list
    Winner: PlayerID
    LaneWins: LaneWins
}

type private GameStateTied = {
    Lanes: Lane list
    LaneWins: LaneWins
}

type private GameState =
| GameStateBetweenTurns of GameStateBetweenTurns
| GameStateDuringTurn of GameStateDuringTurn
| GameStateWon of GameStateWon
| GameStateTied of GameStateTied

let private changeCardsState (gameState: GameStateDuringTurn) newCardsState =
    {gameState with CardsState = newCardsState}
let private updateLaneWins (gameState: GameStateDuringTurn) =
    match gameState.CardsState.GameStage with
    | Early _ ->
        gameState
    | DrawPileEmpty gs ->
        let lw = gs.LaneWins
        let lanes = gameState.CardsState.Board.Lanes
        let currentLanePresences =
            lanes
            |> List.zip (createIDs 1<LID> lanes)
            |> Map.ofList
            |> Map.map (fun _ lane -> laneSolePresence lane.UnitOwners)
        let newWins =
            currentLanePresences
            |> Map.fold (fun state laneID presence ->
                match presence with
                | Contested -> Map.remove laneID state
                | Won controller -> Map.add laneID controller state
                | Empty -> state
                ) lw
        let newGameState = DrawPileEmpty {gs with LaneWins = newWins}
        let newCardsState = {gameState.CardsState with GameStage = newGameState}
        {gameState with CardsState = newCardsState}
    | HandsEmpty gs ->
        let lw = gs.LaneWins
        let lanes = gameState.CardsState.Board.Lanes
        let currentLanePresences =
            lanes
            |> List.zip (createIDs 1<LID> lanes)
            |> Map.ofList
            |> Map.map (fun _ lane -> laneSolePresence lane.UnitOwners)
        let newWins =
            currentLanePresences
            |> Map.fold (fun state laneID presence ->
                match presence with
                | Contested -> Map.remove laneID state
                | Won controller -> Map.add laneID controller state
                | Empty -> state
                ) lw
        let newGameState = HandsEmpty {gs with LaneWins = newWins}
        let newCardsState = {gameState.CardsState with GameStage = newGameState}
        {gameState with CardsState = newCardsState}
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
                        |> List.zip (createIDs 1<LID> lanes)
                        |> List.filter (fun (laneID, {UnitOwners = unitOwners}) ->
                            match Map.containsKey laneID laneWins, Map.isEmpty unitOwners with
                            | false, false ->
                                true
                            | false, true
                            | true, false // shouldn't happen?
                            | true, true ->
                                false
                            )
                    if List.isEmpty contestedLanes then
                        GameStateTied {Lanes = lanes; LaneWins = laneWins}
                    else
                        gameState
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
        ActivationPower Trap
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

let private prepareBaseTable lst =
    let playerIDs = createIDs 1<PID> lst
    lst
    |> List.zip playerIDs
    |> Map.ofList

let private prepareBases nLanes lst =
    lst
    |> List.splitInto nLanes
    |> List.map prepareBaseTable

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

let private getTroops viewerID ownerID (cardPowers: CardPowers) (unitOwners: UnitOwners) (unitDamages: UnitDamages) (revealedCards: RevealedCards) inactiveUnits activeUnits pairs readinesses (unitPairs: UnitPairs) (hiddenCardKnownBys: HiddenCardKnownBys) : InactiveUnitKnowledge list * ActiveUnitKnowledge list * PairKnowledge list =
    let pairPowers =
        pairs
        |> List.map (fun (x, _) -> Map.find x cardPowers)
    let pairDamages1, pairDamages2 =
        pairs
        |> List.map (fun (x, y) -> Map.find x unitDamages, Map.find y unitDamages)
        |> List.unzip
    let pairReadinesses =
        pairs
        |> List.map (fun (x, y) -> max (Map.find x readinesses) (Map.find y readinesses))
    let pairOwners =
        pairs
        |> List.map (fun (x, _) -> Map.find x unitOwners)
    let pairKnowledge =
        pairReadinesses
        |> List.zip3 pairDamages1 pairDamages2
        |> List.zip pairPowers
        |> List.map (fun (p, (d1, d2, r)) -> PairKnowledge (p, d1, d2, r))
        |> List.zip pairOwners
        |> List.filter (fun (owner, _) -> owner = ownerID)
    let nonPairedActiveUnitKnowledge =
        activeUnits
        |> List.filter (fun cardID -> not (Map.containsKey cardID unitPairs) && Map.find cardID unitOwners = ownerID)
        |> List.map (fun id ->
            let owner = Map.find id unitOwners
            let damage =
                match Map.tryFind id unitDamages with
                | Some d -> d
                | None -> 0<health>
            owner, ActiveUnitKnowledge (Map.find id cardPowers, damage, Map.find id readinesses)
            )
    let inactiveUnitKnowledge =
        inactiveUnits
        |> List.filter (fun cardID -> Map.find cardID unitOwners = ownerID)
        |> List.map (fun cardID ->
            let owner = Map.find cardID unitOwners
            let health =
                match Map.tryFind cardID unitDamages with
                | Some d -> d
                | None -> 0<health>
            if Set.contains (cardID, viewerID) hiddenCardKnownBys then
                let power = Map.find cardID cardPowers
                owner, KnownInactiveCardKnowledge (power, health)
            else
                owner, UnknownInactiveCardKnowledge health
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

let private getDisplayInfo gameState =
    match gameState with
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
        let getDeadCard = getDeadCardKnowledge id
        let boardKnowledge =
            let {Lanes = l; Discard = d; HiddenCardKnownBys = kb; RevealedCards = rc} = gs.CardsState.Board
            match gs.CardsState.GameStage with
            | Early {Bases = b; DrawPile = dp} ->
                let lanesKnowledge =
                    List.zip b l
                    |> List.map (fun (bases, {UnitOwners = unitOwners; UnitDamages = unitDamages; InactiveUnits = inactiveUnits; ActiveUnits = activeUnits; Pairs = pairs; Readinesses = readinesses; UnitPairs = unitPairs}) ->
                        let troopKnowledge =
                            createIDsToLength 1<PID> gs.TurnState.NPlayers
                            |> List.map (fun playerID ->
                                playerID, getTroops id playerID cardPowers unitOwners unitDamages rc inactiveUnits activeUnits pairs readinesses unitPairs kb
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
                let discardKnowledge =
                    d
                    |> List.map (fun cardID -> getDeadCard gs.CardsState.CardPowers kb rc cardID)
                PreBaseFlipBoardKnowledge {
                    Lanes = lanesKnowledge
                    DrawPileSize = drawPileSize
                    Discard = discardKnowledge
                    }
            | DrawPileEmpty {LaneWins = laneWins} ->
                let lanesKnowledge =
                    l
                    |> List.zip (createIDs 1<LID> l)
                    |> List.map (fun (laneID, {UnitOwners = unitOwners; UnitDamages = unitDamages; InactiveUnits = inactiveUnits; ActiveUnits = activeUnits; Pairs = pairs; Readinesses = readinesses; UnitPairs = unitPairs}) ->
                        let troopKnowledge =
                            createIDsToLength 1<PID> gs.TurnState.NPlayers
                            |> List.map (fun playerID ->
                                playerID, getTroops id playerID cardPowers unitOwners unitDamages rc inactiveUnits activeUnits pairs readinesses unitPairs kb
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
                let discardKnowledge =
                    d
                    |> List.map (fun cardID -> getDeadCard gs.CardsState.CardPowers kb rc cardID)
                PostBaseFlipBoardKnowledge {
                    Lanes = lanesKnowledge
                    Discard = discardKnowledge
                    }
            | HandsEmpty {LaneWins = laneWins} ->
                let lanesKnowledge =
                    l
                    |> List.zip (createIDs 1<LID> l)
                    |> List.map (fun (laneID, {UnitOwners = unitOwners; UnitDamages = unitDamages; InactiveUnits = inactiveUnits; ActiveUnits = activeUnits; Pairs = pairs; Readinesses = readinesses; UnitPairs = unitPairs}) ->
                        let troopKnowledge =
                            createIDsToLength 1<PID> gs.TurnState.NPlayers
                            |> List.map (fun playerID ->
                                playerID, getTroops id playerID cardPowers unitOwners unitDamages rc inactiveUnits activeUnits pairs readinesses unitPairs kb
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
                let discardKnowledge =
                    d
                    |> List.map (fun cardID -> getDeadCard gs.CardsState.CardPowers kb rc cardID)
                PostBaseFlipBoardKnowledge {
                    Lanes = lanesKnowledge
                    Discard = discardKnowledge
                    }
        TurnDisplayInfo {
            CurrentPlayer = id
            ActionsLeft = gs.TurnState.ActionsLeft
            BoardKnowledge = boardKnowledge
            PlayerHand =
                playerHand
                |> List.map (fun card ->
                    gs.CardsState.CardPowers
                    |> Map.find card
                    |> HandCard
                    )
            OpponentHandSizes =
                opponentHandsInfo
                |> Map.map (fun _ cards -> List.length cards)
                |> Map.toList
        }
    | GameStateBetweenTurns {TurnState = ts} ->
        SwitchDisplayInfo ts.Player
    | GameStateWon {Lanes = lanes; Winner = winner; LaneWins = laneWins} ->
        let laneWins =
            createIDs 1<LID> lanes
            |> List.choose (fun laneID ->
                match Map.tryFind laneID laneWins with
                | Some id ->
                    Some (id, laneID)
                | None ->
                    None
                )
            |> List.groupBy (fun (pid, _) -> pid)
            |> List.map (fun (key, pairs) ->
                key,
                pairs
                |> List.map (fun (_, lid) -> lid)
                )
        WonGameDisplayInfo {
            Winner = winner
            LaneWins = laneWins
        }
    | GameStateTied {Lanes = lanes; LaneWins = laneWins} ->
        let laneWins =
            createIDs 1<LID> lanes
            |> List.choose (fun laneID ->
                match Map.tryFind laneID laneWins with
                | Some id ->
                    Some (id, laneID)
                | None ->
                    None
                )
            |> List.groupBy (fun (pid, _) -> pid)
            |> List.map (fun (key, pairs) ->
                key,
                pairs
                |> List.map (fun (_, lid) -> lid)
                )
        TiedGameDisplayInfo {
            LaneWins = laneWins
        }

let private getPlayActionsInfo (gameState: GameStateDuringTurn) =
    let playerID = gameState.TurnState.CurrentPlayer
    match gameState.CardsState.GameStage with
    | Early gs ->
        createIDs 1<HP> (Map.find playerID gs.HandCards)
    | DrawPileEmpty gs ->
        createIDs 1<HP> (Map.find playerID gs.HandCards)
    | HandsEmpty _ ->
        List.empty
    |> List.allPairs (createIDs 1<LID> gameState.CardsState.Board.Lanes)
    |> List.map (fun (lane, handIndex) ->
        Play (handIndex, lane)
        |> TurnActionInfo
        )

let private getActivateActionsInfo (gameState: GameStateDuringTurn) =
    let playerID = gameState.TurnState.CurrentPlayer
    let lanes = gameState.CardsState.Board.Lanes
    lanes
    |> List.zip (createIDs 1<LID> lanes)
    |> List.collect (fun (laneID, {UnitOwners = unitOwners; InactiveUnits = inactiveUnits}) ->
        let nOwnTroops =
            inactiveUnits
            |> List.filter (fun id -> Map.find id unitOwners = playerID)
            |> List.length
        createIDsToLength 1<LPIP> nOwnTroops
        |> List.map (fun position -> Activate (playerID, laneID, position) |> TurnActionInfo)
        )

let private getPairActionsInfoFromUnits powers playerID laneID ownActiveUnits =
    let rec distPairs lst =
        match lst with
        | [] -> []
        | [_] -> []
        | h::t -> List.allPairs [h] t @ distPairs t
    ownActiveUnits
    |> List.zip (createIDs 1<LPAP> ownActiveUnits)
    |> distPairs
    |> List.choose (fun ((position1, id1), (position2, id2)) ->
        if Map.find id1 powers = Map.find id2 powers then
            CreatePair (playerID, laneID, position1, position2)
            |> TurnActionInfo
            |> Some
        else
            None
        )

let private getAttackActionsInfo (gameState: GameStateDuringTurn) =
    let playerID = gameState.TurnState.CurrentPlayer
    let lanes = gameState.CardsState.Board.Lanes
    lanes
    |> List.zip (createIDs 1<LID> lanes)
    |> List.collect (fun (laneID, {UnitOwners = unitOwners; InactiveUnits = inactiveUnits; ActiveUnits = activeUnits; Pairs = pairs; Readinesses = readinesses}) ->
        let possibleUnitAttackers =
            let ownUnits =
                activeUnits
                |> List.filter (fun id -> Map.find id unitOwners = playerID)
            ownUnits
            |> List.zip (createIDs 1<LPAP> ownUnits)
            |> List.choose (fun (lpap, id) ->
                if Map.find id readinesses = Ready then
                    Some lpap
                else
                    None
                )
        let possiblePairAttackers =
            let pairs =
                pairs
                |> List.filter (fun (id, _) -> Map.find id unitOwners = playerID)
            pairs
            |> List.zip (createIDs 1<LPPP> pairs)
            |> List.choose (fun (lppp, (id1, id2)) ->
                if Map.find id1 readinesses = Ready && Map.find id2 readinesses = Ready then
                    Some lppp
                else
                    None
                )
        let possibleInactiveUnitTargets =
            inactiveUnits
            |> List.groupBy (fun id -> Map.find id unitOwners)
            |> List.filter (fun (owner, _) -> owner <> playerID)
            |> List.collect (fun (owner, inactiveUnits) ->
                createIDs 1<LPIP> inactiveUnits
                |> List.map (fun position ->
                    InactiveTarget (owner, position)
                    )
                )
        let possibleActiveUnitTargets =
            activeUnits
            |> List.groupBy (fun id -> Map.find id unitOwners)
            |> List.filter (fun (owner, _) -> owner <> playerID)
            |> List.collect (fun (owner, activeUnits) ->
                createIDs 1<LPAP> activeUnits
                |> List.map (fun position ->
                    ActiveSingleTarget (owner, position)
                    )
                )
        let possiblePairTargets =
            pairs
            |> List.groupBy (fun (id, _) -> Map.find id unitOwners)
            |> List.filter (fun (owner, _) -> owner <> playerID)
            |> List.collect (fun (owner, pairs) ->
                createIDs 1<LPPP> pairs
                |> List.collect (fun position ->
                    [
                        ActivePairMemberTarget (owner, position, One);
                        ActivePairMemberTarget (owner, position, Two)
                        ]
                    )
                )
        let allTargets = possibleInactiveUnitTargets @ possibleActiveUnitTargets @ possiblePairTargets
        let singleAttacks =
            List.allPairs possibleUnitAttackers allTargets
            |> List.map (fun (attacker, target) ->
                SingleAttack (playerID, laneID, attacker, target)
                |> TurnActionInfo
                )
        let pairAttacks =
            List.allPairs possiblePairAttackers allTargets
            |> List.map (fun (attackerPairPosition, target) ->
                PairAttack (playerID, laneID, attackerPairPosition, target)
                |> TurnActionInfo
                )
        singleAttacks @ pairAttacks
        )

let private getPairActionsInfo (gameState: GameStateDuringTurn) =
    let playerID = gameState.TurnState.CurrentPlayer
    let lanes = gameState.CardsState.Board.Lanes
    lanes
    |> List.zip (createIDs 1<LID> lanes)
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
    | GameStateWon _
    | GameStateTied _ ->
        List.empty

let private executePlayAction handPosition laneID gameState =
    let playerID = gameState.TurnState.CurrentPlayer
    let cardsState = gameState.CardsState
    let cardID =
        match cardsState.GameStage with
        | Early {HandCards = hco} ->
            hco
        | DrawPileEmpty {HandCards = hco} ->
            hco
        | HandsEmpty _ ->
            failwithf "can't play a card when hands are empty"
        |> Map.find playerID
        |> List.item (int handPosition - 1)
    cardsState
    |> addCardToBoard cardID playerID laneID
    |> removeCardFromHand cardID playerID
    |> removeHandsIfAllEmpty
    |> changeCardsState gameState

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

let private resolveActivationPower playerID cardID (powers: CardPowers) (board: Board) =
    match Map.find cardID powers with
    | ActivationPower View ->
        board
    | ActivationPower Trap ->
        board
    | ActivationPower Foresight
    | ActivationPower Flip
    | ActivationPower Freeze ->
        board
    | ActivationPower Heal ->
        let newLanes =
            board.Lanes
            |> List.map (healOwnUnitsInLane playerID 2<health>)
        {board with Lanes = newLanes}
    | PassivePower _
    | ActivationPower Move
    | ActivationPower Empower
    | ActivationPower Action ->
        board

let private executeActivateAction playerID laneID lanePlayerPosition gameState =
    let cardsState = gameState.CardsState
    let lanes = cardsState.Board.Lanes
    let lane = List.item (int laneID - 1) lanes
    let cardID =
        lane.InactiveUnits
        |> List.filter (fun id -> Map.find id lane.UnitOwners = playerID)
        |> List.item (int lanePlayerPosition - 1)
    cardsState.Board
    |> changeLaneWithFn laneID (
        removeCardFromInactiveUnits cardID
        >> addCardToActiveUnits cardID
        >> addCardToReadinesses cardID
        )
    |> removeCardFromKnownBys cardID
    |> addCardToRevealedCards cardID
    |> resolveActivationPower playerID cardID cardsState.CardPowers
    |> changeBoard cardsState
    |> changeCardsState gameState

let private getBonusDefenderDamage attackerPower targetPower targetInfo =
    match attackerPower, targetPower, targetInfo with
    | PassivePower Nimble, PassivePower Taunt, ActiveSingleTarget _
    | PassivePower Nimble, PassivePower Taunt, ActivePairMemberTarget _ ->
        1<health>
    | _ ->
        0<health>

let private getTargetIDFromTargetInfo targetInfo lane =
    let {UnitOwners = unitOwners; InactiveUnits = inactiveUnits; ActiveUnits = activeUnits; Pairs = pairs} = lane
    match targetInfo with
    | InactiveTarget (owner, position) ->
        inactiveUnits
        |> List.filter (fun id -> Map.find id unitOwners = owner)
        |> List.item (int position - 1)
    | ActiveSingleTarget (owner, position) ->
        activeUnits
        |> List.filter (fun id -> Map.find id unitOwners = owner)
        |> List.item (int position - 1)
    | ActivePairMemberTarget (owner, position, pairUnitPosition) ->
        pairs
        |> List.filter (fun (id, _) -> Map.find id unitOwners = owner)
        |> List.item (int position - 1)
        |> (fun (id1, id2) ->
            match pairUnitPosition with
            | One -> id1
            | Two -> id2
            )

let private getSingleAttackInfo lanes laneID playerID attackerLPAP targetInfo cardPowers =
    let lane = List.item (int laneID - 1) lanes
    let {UnitOwners = unitOwners; ActiveUnits = activeUnits} = lane
    let attackerID =
        activeUnits
        |> List.filter (fun id -> Map.find id unitOwners = playerID)
        |> List.item (int attackerLPAP - 1)
    let targetID = getTargetIDFromTargetInfo targetInfo lane
    let attackerPower = Map.find attackerID cardPowers
    let targetPower = Map.find targetID cardPowers
    let baseDefenderDamage = 1<health>
    let bonusDefenderDamage = getBonusDefenderDamage attackerPower targetPower targetInfo
    attackerID, targetID, baseDefenderDamage + bonusDefenderDamage

let private getPairAttackInfo lanes laneID playerID attackerPairPosition targetInfo cardPowers =
    let lane = List.item (int laneID - 1) lanes
    let {UnitOwners = unitOwners; Pairs = pairs} = lane
    let (attackerID1, attackerID2) =
        pairs
        |> List.filter (fun (id, _) -> Map.find id unitOwners = playerID)
        |> List.item (int attackerPairPosition - 1)
    let targetID = getTargetIDFromTargetInfo targetInfo lane
    let attackerPower = Map.find attackerID1 cardPowers
    let targetPower = Map.find targetID cardPowers
    let baseDefenderDamage = 2<health>
    let bonusDefenderDamage = getBonusDefenderDamage attackerPower targetPower targetInfo
    [attackerID1; attackerID2], targetID, baseDefenderDamage + bonusDefenderDamage

let private executeSingleAttackAction playerID laneID attackerLPAP targetInfo gameState =
    let cardsState = gameState.CardsState
    let board = cardsState.Board
    let attackerID, targetID, damage =
        getSingleAttackInfo board.Lanes laneID playerID attackerLPAP targetInfo cardsState.CardPowers
    board
    |> changeLaneWithFn laneID (
        exhaustCards [attackerID]
        >> damageCard targetID damage
        )
    |> triggerTrapsAndMoveDeadCardsToDiscard laneID cardsState.CardPowers
    |> changeBoard cardsState
    |> changeCardsState gameState

let private executePairAttackAction playerID laneID attackerPairPosition targetInfo gameState =
    let cardsState = gameState.CardsState
    let board = cardsState.Board
    let attackerIDs, targetID, damage =
        getPairAttackInfo board.Lanes laneID playerID attackerPairPosition targetInfo cardsState.CardPowers
    board
    |> changeLaneWithFn laneID (
        exhaustCards attackerIDs
        >> damageCard targetID damage
        )
    |> triggerTrapsAndMoveDeadCardsToDiscard laneID cardsState.CardPowers
    |> changeBoard cardsState
    |> changeCardsState gameState

let private executeCreatePairAction playerID laneID position1 position2 gameState =
    let boardInfo = gameState.CardsState.Board
    let lane = List.item (int laneID - 1) boardInfo.Lanes
    let ownActiveUnits =
        lane.ActiveUnits
        |> List.filter (fun id -> Map.find id lane.UnitOwners = playerID)
    let cardID1 =
        ownActiveUnits
        |> List.item (int position1 - 1)
    let cardID2 =
        ownActiveUnits
        |> List.item (int position2 - 1)
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
    match action with
    | Play (handPosition, laneID) ->
        executePlayAction handPosition laneID gameState
    | Activate (playerID, laneID, lanePlayerPosition) ->
        executeActivateAction playerID laneID lanePlayerPosition gameState
    | SingleAttack (playerID, laneID, attackerLPAP, targetInfo) ->
        executeSingleAttackAction playerID laneID attackerLPAP targetInfo gameState
    | PairAttack (playerID, laneID, attackerPairPosition, targetInfo) ->
        executePairAttackAction playerID laneID attackerPairPosition targetInfo gameState
    | CreatePair (playerID, laneID, position1, position2) ->
        executeCreatePairAction playerID laneID position1 position2 gameState
    |> decrementActionsLeft
    |> updateLaneWins

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
                    Board = flipBasesOnBoard preInfo.Bases boardInfo.Lanes boardInfo.Discard newKnownBys boardInfo.RevealedCards
                    GameStage = DrawPileEmpty {HandCards = newHandCards; LaneWins = Map.empty}
                }
            {gameState with CardsState = newCards}
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
            {gameState with CardsState = newCards}
    | DrawPileEmpty _
    | HandsEmpty _ ->
        gameState

let private readyActiveUnits activeUnits =
    activeUnits
    |> Map.map (fun _ _ -> Ready)

let private readyAllActiveCards cardsState =
    let newBoard =
        let boardInfo = cardsState.Board
        let newLanes =
            boardInfo.Lanes
            |> List.map (fun lane ->
                    {lane with Readinesses = readyActiveUnits lane.Readinesses}
                )
        {boardInfo with Lanes = newLanes}
    {cardsState with Board = newBoard}

let rec private makeNextActionInfo gameState action =
    let newGameState =
        match gameState, action with
        | GameStateDuringTurn gs, TurnActionInfo tai ->
            executeTurnAction tai gs
            |> GameStateDuringTurn
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
                CardsState = readyAllActiveCards gs.CardsState
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
        | _ ->
            failwithf "action incompatible with game state"
    let newDisplayInfo = getDisplayInfo newGameState
    // Generation of next action's resulting capabilities is part of the
    // generated capability's body, since assigning them here requires
    // generating the entire game space, blowing the stack
    let capability() =
        InProgress (
            newDisplayInfo,
            getPossibleActionsInfo newGameState
            |> List.map (makeNextActionInfo newGameState)
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
                Lanes = List.replicate nLanes emptyLane
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
