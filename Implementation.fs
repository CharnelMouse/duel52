module Implementation
open Domain

type private CardPowers = Map<CardID, Power>

type private HiddenCardKnownBys = (CardID * PlayerID) Set
type private Bases = Map<PlayerID, CardID>
type private HandCards = CardID list
type private RevealedCards = CardID Set

type private UnitOwners = Map<CardID, PlayerID>
type private UnitHealths = Map<CardID, Health>
type private InactiveUnits = CardID list
type private ActiveUnits = Map<CardID, Readiness>
type private UnitPairs = Map<CardID, CardID>

type private Discard = CardID list
type private RemovedCards = CardID Set

type private DrawPile = {
    TopCard: CardID
    Rest: CardID list
    }

type private Lane = {
    UnitOwners: UnitOwners
    UnitHealths: UnitHealths
    InactiveUnits: InactiveUnits
    ActiveUnits: ActiveUnits
    UnitPairs: UnitPairs
}

let private emptyLane = {
    UnitOwners = Map.empty
    UnitHealths = Map.empty
    InactiveUnits = List.empty
    ActiveUnits = Map.empty
    UnitPairs = Map.empty
}

let private laneSolePresence (unitOwners: UnitOwners) =
    let playerCounts =
        unitOwners
        |> Map.toList
        |> List.countBy (fun (id, owner) -> owner)
    match playerCounts with
    | [] -> None
    | [(controller, _)] -> Some controller
    | _ -> None

type private Board = {
    Lanes: Lane list
    Discard: Discard
    HiddenCardKnownBys: HiddenCardKnownBys
    RevealedCards: RevealedCards
}

type private EarlyGameInfo = {
    Bases: Bases list
    DrawPile: DrawPile
    HandCards: Map<PlayerID, HandCards>
}

type private PostDrawGameInfo = {
    HandCards: Map<PlayerID, HandCards>
}

type private LaneWins = Map<LaneID, PlayerID>

type private PostHandGameInfo = {
    LockedLaneWins: LaneWins
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
}

type private GameStateTied = {
    Lanes: Lane list
}

type private GameState =
| GameStateBetweenTurns of GameStateBetweenTurns
| GameStateDuringTurn of GameStateDuringTurn
| GameStateWon of GameStateWon
| GameStateTied of GameStateTied

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
    let playerIDs = [for i in 1..List.length lst -> i*1<PID>]
    lst
    |> List.zip playerIDs
    |> Map.ofList

let private prepareBases nLanes lst =
    lst
    |> List.splitInto nLanes
    |> List.map prepareBaseTable

let private prepareHands nPlayers (lst: CardID list) =
    let playerIDs =
        [1..nPlayers]
        |> List.map (fun n -> n*1<PID>)
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

let private getTroops playerID (cardPowers: CardPowers) (unitOwners: UnitOwners) (unitHealths: UnitHealths) (revealedCards: RevealedCards) inactiveUnits activeUnits unitPairs hiddenCardKnownBys : InactiveUnitsKnowledge * ActiveUnitsKnowledge * PairsKnowledge =
    let pairCardIDs =
        unitPairs
        |> Map.toList
        |> List.distinctBy (fun (x, y) -> min x y, max x y)
    let pairPowers =
        pairCardIDs
        |> List.map (fun (x, _) -> Map.find x cardPowers)
    let pairHealths1, pairHealths2 =
        pairCardIDs
        |> List.map (fun (x, y) -> Map.find x unitHealths, Map.find y unitHealths)
        |> List.unzip
    let pairReadinesses =
        pairCardIDs
        |> List.map (fun (x, y) -> max (Map.find x activeUnits) (Map.find y activeUnits))
    let pairOwners =
        pairCardIDs
        |> List.map (fun (x, _) -> Map.find x unitOwners)
    let pairKnowledge =
        pairReadinesses
        |> List.zip3 pairHealths1 pairHealths2
        |> List.zip pairPowers
        |> List.map (fun (p, (h1, h2, r)) -> PairKnowledge (p, h1, h2, r))
        |> List.zip pairOwners
    let nonPairedActiveUnitKnowledge =
        activeUnits
        |> Map.filter (fun cardID _ -> not (Map.containsKey cardID unitPairs))
        |> Map.toList
        |> List.map (fun (id, readiness) ->
            let owner, health = Map.find id unitOwners, Map.find id unitHealths
            owner, ActiveUnitKnowledge (Map.find id cardPowers, health, readiness)
            )
    let inactiveUnitKnowledge =
        inactiveUnits
        |> List.map (fun cardID ->
            let owner, health = Map.find cardID unitOwners, Map.find cardID unitHealths
            if Set.contains (cardID, playerID) hiddenCardKnownBys then
                let power = Map.find cardID cardPowers
                owner, KnownInactiveCardKnowledge (power, health)
            else
                owner, UnknownInactiveCardKnowledge health
            )
    inactiveUnitKnowledge
    |> List.groupBy (fun (pid, _) -> pid)
    |> List.map (fun (pid, pairs) -> pid, pairs |> List.map (fun (_, knowledge) -> knowledge))
    |> Map.ofList,
    nonPairedActiveUnitKnowledge
    |> List.groupBy (fun (pid, _) -> pid)
    |> List.map (fun (pid, pairs) -> pid, pairs |> List.map (fun (_, knowledge) -> knowledge))
    |> Map.ofList,
    pairKnowledge
    |> List.groupBy (fun (pid, _) -> pid)
    |> List.map (fun (pid, pairs) -> pid, pairs |> List.map (fun (_, knowledge) -> knowledge))
    |> Map.ofList

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
                    |> List.map (fun (bases, {UnitOwners = unitOwners; UnitHealths = unitHealths; InactiveUnits = inactiveUnits; ActiveUnits = activeUnits; UnitPairs = unitPairs}) ->
                        let inactiveUnits, activeUnits, pairs = getTroops id cardPowers unitOwners unitHealths rc inactiveUnits activeUnits unitPairs kb
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
                            InactiveUnits = inactiveUnits
                            ActiveUnits = activeUnits
                            Pairs = pairs
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
            | DrawPileEmpty _ ->
                let lanesKnowledge =
                    l
                    |> List.map (fun {UnitOwners = unitOwners; UnitHealths = unitHealths; InactiveUnits = inactiveUnits; ActiveUnits = activeUnits; UnitPairs = unitPairs} ->
                        let inactiveUnits, activeUnits, pairs = getTroops id cardPowers unitOwners unitHealths rc inactiveUnits activeUnits unitPairs kb
                        match (laneSolePresence unitOwners) with
                        | None ->
                            ContestedLaneKnowledge {
                                InactiveUnits = inactiveUnits
                                ActiveUnits = activeUnits
                                Pairs = pairs
                                }
                        | Some c ->
                            WonLaneKnowledge {
                                Controller = c
                                InactiveUnits = inactiveUnits
                                ActiveUnits = activeUnits
                                Pairs = pairs
                                }
                        )
                let discardKnowledge =
                    d
                    |> List.map (fun cardID -> getDeadCard gs.CardsState.CardPowers kb rc cardID)
                PostBaseFlipBoardKnowledge {
                    Lanes = lanesKnowledge
                    Discard = discardKnowledge
                    }
            | HandsEmpty {LockedLaneWins = laneWins} ->
                let lanesKnowledge =
                    l
                    |> List.zip ([for i in 1..List.length l -> i*1<LID>])
                    |> List.map (fun (laneID, {UnitOwners = unitOwners; UnitHealths = unitHealths; InactiveUnits = inactiveUnits; ActiveUnits = activeUnits; UnitPairs = unitPairs}) ->
                        let inactiveUnits, activeUnits, pairs = getTroops id cardPowers unitOwners unitHealths rc inactiveUnits activeUnits unitPairs kb
                        match Map.tryFind laneID laneWins with
                        | None ->
                            ContestedLaneKnowledge {
                                InactiveUnits = inactiveUnits
                                ActiveUnits = activeUnits
                                Pairs = pairs
                                }
                        | Some c ->
                            WonLaneKnowledge {
                                Controller = c
                                InactiveUnits = inactiveUnits
                                ActiveUnits = activeUnits
                                Pairs = pairs
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
    | GameStateWon {Lanes = lanes; Winner = winner} ->
        let laneWins =
            lanes
            |> List.indexed
            |> List.choose (fun (n, lane) ->
                match laneSolePresence lane.UnitOwners with
                | Some id ->
                    Some (id, (n + 1)*1<LID>)
                | _ ->
                    None
                )
            |> List.groupBy (fun (pid, lid) -> pid)
            |> List.map (fun (key, pairs) ->
                key,
                pairs
                |> List.map (fun (pid, lid) -> lid)
                )
        WonGameDisplayInfo {
            Winner = winner
            LaneWins = laneWins
        }
    | GameStateTied {Lanes = lanes} ->
        let laneWins =
            lanes
            |> List.indexed
            |> List.choose (fun (n, lane) ->
                match laneSolePresence lane.UnitOwners with
                | Some id ->
                    Some (id, (n + 1)*1<LID>)
                | None ->
                    None
                )
            |> List.groupBy (fun (pid, lid) -> pid)
            |> List.map (fun (key, pairs) ->
                key,
                pairs
                |> List.map (fun (pid, lid) -> lid)
                )
        TiedGameDisplayInfo {
            LaneWins = laneWins
        }

let private getPlayActionsInfo (gameState: GameStateDuringTurn) =
    let playerID = gameState.TurnState.CurrentPlayer
    match gameState.CardsState.GameStage with
    | Early gs ->
        [for i in 1..List.length (Map.find playerID gs.HandCards) -> i*1<HP>]
    | DrawPileEmpty gs ->
        [for i in 1..List.length (Map.find playerID gs.HandCards) -> i*1<HP>]
    | HandsEmpty _ ->
        List.empty
    |> List.allPairs [for i in 1..List.length gameState.CardsState.Board.Lanes -> i*1<LID>]
    |> List.map (fun (lane, handIndex) ->
        Play (handIndex, lane)
        |> TurnActionInfo
        )

let private getActivateActionsInfo (turnDisplayInfo: TurnDisplayInfo) =
    let playerID = turnDisplayInfo.CurrentPlayer
    match turnDisplayInfo.BoardKnowledge with
    | PreBaseFlipBoardKnowledge {Lanes = l} ->
        l
        |> List.zip [for i in 1..List.length l -> i*1<LID>]
        |> List.collect (fun (laneID, {InactiveUnits = inactiveUnits}) ->
            inactiveUnits
            |> Map.tryFind playerID
            |> function
            | None -> []
            | Some ownTroops ->
                ownTroops
                |> List.zip [for i in 1..List.length ownTroops -> i*1<LPIP>]
                |> List.choose (fun (position, troop) ->
                    match troop with
                    | UnknownInactiveCardKnowledge _
                    | KnownInactiveCardKnowledge _ ->
                        Activate (playerID, laneID, position)
                        |> TurnActionInfo
                        |> Some
                    )
            |> List.distinct
            )
    | PostBaseFlipBoardKnowledge {Lanes = l} ->
        l
        |> List.zip [for i in 1..List.length l -> i*1<LID>]
        |> List.collect (fun (laneID, lane) ->
            match lane with
            | ContestedLaneKnowledge {InactiveUnits = inactiveUnits} ->
                inactiveUnits
                |> Map.tryFind playerID
                |> function
                | None -> []
                | Some ownTroops ->
                    ownTroops
                    |> List.zip [for i in 1..List.length ownTroops -> i*1<LPIP>]
                    |> List.choose (fun (position, troop) ->
                        match troop with
                        | UnknownInactiveCardKnowledge _
                        | KnownInactiveCardKnowledge _ ->
                            Activate (playerID, laneID, position)
                            |> TurnActionInfo
                            |> Some
                        )
                |> List.distinct
            | WonLaneKnowledge {Controller = c; InactiveUnits = inactiveUnits} when c = playerID ->
                inactiveUnits
                |> Map.tryFind playerID
                |> function
                | None -> []
                | Some ownTroops ->
                    ownTroops
                    |> List.zip [for i in 1..List.length ownTroops -> i*1<LPIP>]
                    |> List.choose (fun (position, troop) ->
                        match troop with
                        | UnknownInactiveCardKnowledge _
                        | KnownInactiveCardKnowledge _ ->
                            Activate (playerID, laneID, position)
                            |> TurnActionInfo
                            |> Some
                        )
                |> List.distinct
            | WonLaneKnowledge _ ->
                []
            )

let private getPairActionsInfoFromUnits playerID laneID activeUnits =
    let potentialSingles =
        activeUnits
        |> Map.tryFind playerID
        |> function
        | None -> []
        | Some ownTroops ->
            ownTroops
    let rec distPairs lst =
        match lst with
        | [] -> []
        | [_] -> []
        | h::t -> List.allPairs [h] t @ distPairs t
    potentialSingles
    |> distPairs
    |> List.filter (fun ((power1, _, _), (power2, _, _)) -> power1 = power2)
    |> List.map (fun ((power, health1, readiness1), (_, health2, readiness2)) ->
        let vars =
            if health1 >= health2 then
                (playerID, laneID, power, (health1, readiness1), (health2, readiness2))
            else
                (playerID, laneID, power, (health2, readiness2), (health1, readiness1))
        vars
        |> CreatePair
        |> TurnActionInfo
        )
    |> List.distinct

let private getAttackActionsInfo (turnDisplayInfo: TurnDisplayInfo) =
    let playerID = turnDisplayInfo.CurrentPlayer
    match turnDisplayInfo.BoardKnowledge with
    | PreBaseFlipBoardKnowledge {Lanes = l} ->
        l
        |> List.zip [for i in 1..List.length l -> i*1<LID>]
        |> List.collect (fun (laneID, {InactiveUnits = inactiveUnits; ActiveUnits = activeUnits; Pairs = pairs}) ->
            let possibleUnitAttackers =
                activeUnits
                |> Map.tryFind playerID
                |> function
                | None -> []
                | Some ownTroops ->
                    ownTroops
                    |> List.distinct
                    |> List.choose (fun (p, h, r) ->
                        if r = Ready then
                            Some (SingleAttacker (p, h))
                        else
                            None
                        )
                    |> List.distinct
            let possiblePairAttackers =
                pairs
                |> Map.tryFind playerID
                |> function
                | None -> []
                | Some ownTroops ->
                    ownTroops
                    |> List.distinct
                    |> List.choose (fun (p, h1, h2, r) ->
                        if r = Ready then
                            Some (DoubleAttacker (p, h1, h2))
                        else
                            None
                        )
                    |> List.distinct
            let possibleInactiveUnitTargets =
                inactiveUnits
                |> Map.filter (fun pid _ -> pid <> playerID)
                |> Map.toList
                |> List.collect (fun (pid, tks) ->
                    tks
                    |> List.distinct
                    |> List.collect (function
                        | UnknownInactiveCardKnowledge h ->
                            UnknownInactiveTarget (pid, h)
                            |> List.singleton
                        | KnownInactiveCardKnowledge (p, h) ->
                            KnownInactiveTarget (pid, p, h)
                            |> List.singleton
                        )
                    |> List.distinct
                    )
            let possibleActiveUnitTargets =
                activeUnits
                |> Map.filter (fun pid _ -> pid <> playerID)
                |> Map.toList
                |> List.collect (fun (pid, tks) ->
                    tks
                    |> List.distinct
                    |> List.collect (fun (p, h, _) ->
                        ActiveSingleTarget (pid, p, h)
                        |> List.singleton
                        )
                    |> List.distinct
                    )
            let possiblePairTargets =
                pairs
                |> Map.filter (fun pid _ -> pid <> playerID)
                |> Map.toList
                |> List.collect (fun (pid, tks) ->
                    tks
                    |> List.distinct
                    |> List.collect (fun (p, h1, h2, _) ->
                        [
                            ActivePairMemberTarget (pid, p, h1, h2);
                            ActivePairMemberTarget (pid, p, h2, h1)
                            ]
                        |> List.distinct
                        )
                    |> List.distinct
                    )
            List.allPairs (possibleUnitAttackers @ possiblePairAttackers) (possibleInactiveUnitTargets @ possibleActiveUnitTargets @ possiblePairTargets)
            |> List.map (fun (attacker, target) ->
                Attack (playerID, laneID, attacker, target)
                |> TurnActionInfo
                )
            )
    | PostBaseFlipBoardKnowledge {Lanes = l} ->
        l
        |> List.indexed
        |> List.collect (fun (n, lane) ->
            let laneID = (n + 1)*1<LID>
            match lane with
            | ContestedLaneKnowledge {InactiveUnits = inactiveUnits; ActiveUnits = activeUnits; Pairs = pairs} ->
                let possibleUnitAttackers =
                    activeUnits
                    |> Map.tryFind playerID
                    |> function
                    | None -> []
                    | Some ownTroops ->
                        ownTroops
                        |> List.distinct
                        |> List.choose (fun (p, h, r) ->
                            if r = Ready then
                                Some (SingleAttacker (p, h))
                            else
                                None
                            )
                        |> List.distinct
                let possiblePairAttackers =
                    pairs
                    |> Map.tryFind playerID
                    |> function
                    | None -> []
                    | Some ownTroops ->
                        ownTroops
                        |> List.distinct
                        |> List.choose (fun (p, h1, h2, r) ->
                            if r = Ready then
                                Some (DoubleAttacker (p, h1, h2))
                            else
                                None
                            )
                        |> List.distinct
                let possibleInactiveUnitTargets =
                    inactiveUnits
                    |> Map.filter (fun pid _ -> pid <> playerID)
                    |> Map.toList
                    |> List.collect (fun (pid, tks) ->
                        tks
                        |> List.distinct
                        |> List.collect (function
                            | UnknownInactiveCardKnowledge h ->
                                UnknownInactiveTarget (pid, h)
                                |> List.singleton
                            | KnownInactiveCardKnowledge (p, h) ->
                                KnownInactiveTarget (pid, p, h)
                                |> List.singleton
                            )
                        |> List.distinct
                        )
                let possibleActiveUnitTargets =
                    activeUnits
                    |> Map.filter (fun pid _ -> pid <> playerID)
                    |> Map.toList
                    |> List.collect (fun (pid, tks) ->
                        tks
                        |> List.distinct
                        |> List.collect (fun (p, h, _) ->
                            ActiveSingleTarget (pid, p, h)
                            |> List.singleton
                            )
                        |> List.distinct
                        )
                let possiblePairTargets =
                    pairs
                    |> Map.filter (fun pid _ -> pid <> playerID)
                    |> Map.toList
                    |> List.collect (fun (pid, tks) ->
                        tks
                        |> List.distinct
                        |> List.collect (fun (p, h1, h2, _) ->
                            [
                                ActivePairMemberTarget (pid, p, h1, h2);
                                ActivePairMemberTarget (pid, p, h2, h1)
                                ]
                            |> List.distinct
                            )
                        |> List.distinct
                        )
                List.allPairs (possibleUnitAttackers @ possiblePairAttackers) (possibleInactiveUnitTargets @ possibleActiveUnitTargets @ possiblePairTargets)
                |> List.map (fun (attacker, target) ->
                    Attack (playerID, laneID, attacker, target)
                    |> TurnActionInfo
                    )
            | WonLaneKnowledge _ ->
                []
            )

let private getPairActionsInfo (turnDisplayInfo: TurnDisplayInfo) =
    let playerID = turnDisplayInfo.CurrentPlayer
    match turnDisplayInfo.BoardKnowledge with
    | PreBaseFlipBoardKnowledge {Lanes = l} ->
        l
        |> List.zip [for i in 1..List.length l -> i*1<LID>]
        |> List.collect (fun (laneID, {ActiveUnits = activeUnits}) ->
            getPairActionsInfoFromUnits playerID laneID activeUnits
            )
    | PostBaseFlipBoardKnowledge {Lanes = l} ->
        l
        |> List.zip [for i in 1..List.length l -> i*1<LID>]
        |> List.collect (fun (laneID, lane) ->
            match lane with
            | ContestedLaneKnowledge {ActiveUnits = activeUnits} ->
                getPairActionsInfoFromUnits playerID laneID activeUnits
            | WonLaneKnowledge {Controller = c; ActiveUnits = activeUnits} when c = playerID ->
                getPairActionsInfoFromUnits playerID laneID activeUnits
            | WonLaneKnowledge _ ->
                []
            )

let private getPossibleActionsInfo (gameState: GameState) (displayInfo: DisplayInfo) =
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
            match displayInfo with
            | TurnDisplayInfo turnDisplayInfo ->
                if gs.TurnState.ActionsLeft = 0 then
                    EndTurn currentPlayer
                    |> List.singleton
                else
                    let actions =
                        getPlayActionsInfo gs
                        @ (getActivateActionsInfo turnDisplayInfo)
                        @ (getAttackActionsInfo turnDisplayInfo)
                        @ (getPairActionsInfo turnDisplayInfo)
                    if List.isEmpty actions then
                        EndTurn currentPlayer
                        |> List.singleton
                    else
                        actions
            | SwitchDisplayInfo _
            | WonGameDisplayInfo _
            | TiedGameDisplayInfo _ ->
                failwithf "Impossible game state / turn display info pair"
    | GameStateWon _
    | GameStateTied _ ->
        List.empty

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
                        UnitHealths = Map.add cardID 2<health> lane.UnitHealths
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
    | DrawPileEmpty gs when Map.forall (fun _ cards -> List.isEmpty cards) gs.HandCards ->
        {cardsState with
            GameStage = HandsEmpty {LockedLaneWins = Map.empty}
            }
    | Early _
    | DrawPileEmpty _
    | HandsEmpty _ ->
        cardsState

let private changeCardsState (gameState: GameStateDuringTurn) newCardsState =
    {gameState with CardsState = newCardsState}

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

let private removeCardFromInactiveUnits cardID (lane: Lane) =
    {lane with InactiveUnits = List.filter (fun c -> c <> cardID) lane.InactiveUnits}

let private addCardToActiveUnits cardID (lane: Lane) =
    {lane with ActiveUnits = Map.add cardID Ready lane.ActiveUnits}

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

let private removeCardFromKnownBys cardID board =
    {board with
        HiddenCardKnownBys = Set.filter (fun (cid, _) -> cid <> cardID) board.HiddenCardKnownBys
        }

let private changeBoard cardsState newBoard =
    {cardsState with Board = newBoard}

let private addCardToRevealedCards cardID board =
    {board with RevealedCards = Set.add cardID board.RevealedCards}

let private executeActivateAction playerID laneID lanePlayerPosition gameState =
    let cardsState = gameState.CardsState
    let lanes = cardsState.Board.Lanes
    let lane = List.item (int laneID - 1) lanes
    let cardID =
        lane.InactiveUnits
        |> List.filter (fun id -> Map.find id lane.UnitOwners = playerID)
        |> List.item (int lanePlayerPosition - 1)
    cardsState.Board
    |> changeLaneWithFn laneID (removeCardFromInactiveUnits cardID >> addCardToActiveUnits cardID)
    |> removeCardFromKnownBys cardID
    |> addCardToRevealedCards cardID
    |> changeBoard cardsState
    |> changeCardsState gameState

let private getAttackInfo (cardPowers: CardPowers) lanes laneID playerID attackerInfo targetInfo revealedCards =
    let lane = List.item (int laneID - 1) lanes
    let {UnitOwners = unitOwners; UnitHealths = unitHealths; ActiveUnits = activeUnits; UnitPairs = unitPairs} = lane
    let attackerIDs =
        match attackerInfo with
        | SingleAttacker (power, health) ->
            unitOwners
            |> Map.toList
            |> List.choose (fun (id, owner) ->
                if owner = playerID then
                    Some id
                else
                    None
                )
            |> List.filter (fun id -> Map.find id unitHealths = health)
            |> List.filter (fun id -> Map.find id cardPowers = power)
            |> List.filter (fun id -> Map.tryFind id activeUnits = Some Ready)
            |> List.head
            |> List.singleton
        | DoubleAttacker (power, health1, health2) ->
            let readyCardsWithPower =
                cardPowers
                |> Map.toList
                |> List.choose (fun (id, p) ->
                    if p = power then
                        Some id
                    else
                        None
                    )
                |> List.filter (fun id -> Map.tryFind id activeUnits = Some Ready)
            let firstPartners =
                readyCardsWithPower
                |> List.filter (fun id ->
                    match Map.tryFind id unitOwners, Map.tryFind id unitHealths with
                    | Some pid, Some h -> pid = playerID && h = health1
                    | _ -> false
                    )
            let secondPartners =
                readyCardsWithPower
                |> List.filter (fun id ->
                    match Map.tryFind id unitOwners, Map.tryFind id unitHealths with
                    | Some pid, Some h -> pid = playerID && h = health2
                    | _ -> false
                    )
            let validPairIDs =
                List.allPairs firstPartners secondPartners
                |> List.filter (fun (x, y) -> Map.tryFind x unitPairs = Some y)
            validPairIDs
            |> List.tryHead
            |> function
            | Some (x, y) -> [x; y]
            | None -> failwithf "failed doubleattacker IDs search: player %i %A, %i and %i. Pair info: %A" playerID power health1 health2 unitPairs
    let targetID =
        match targetInfo with
        | UnknownInactiveTarget (owner, health) ->
            unitOwners
            |> Map.toList
            |> List.choose (fun (id, o) ->
                if o = owner then
                    Some id
                else
                    None
                )
            |> List.filter (fun id -> Map.find id unitHealths = health)
            |> List.head
        | KnownInactiveTarget (owner, power, health) ->
            unitOwners
            |> Map.toList
            |> List.choose (fun (id, o) ->
                if o = owner then
                    Some id
                else
                    None
                )
            |> List.filter (fun id -> Map.find id unitHealths = health)
            |> List.filter (fun id -> not (Set.contains id revealedCards))
            |> List.filter (fun id ->
                Map.find id cardPowers = power
                )
            |> List.head
        | ActiveSingleTarget (owner, power, health) ->
            unitOwners
            |> Map.toList
            |> List.choose (fun (id, o) ->
                if o = owner then
                    Some id
                else
                    None
                )
            |> List.filter (fun id -> Map.find id unitHealths = health)
            |> List.filter (fun id -> Map.containsKey id activeUnits)
            |> List.filter (fun id ->
                Map.find id cardPowers = power
                )
            |> List.head
        | ActivePairMemberTarget (owner, power, health, partnerHealth) ->
            unitOwners
            |> Map.toList
            |> List.choose (fun (id, o) ->
                if o = owner then
                    Some id
                else
                    None
                )
            |> List.filter (fun id -> Map.find id unitHealths = health)
            |> List.filter (fun id -> Map.containsKey id unitPairs)
            |> List.filter (fun id ->
                let partnerID = Map.find id unitPairs
                Map.find partnerID unitHealths = partnerHealth
                )
            |> List.filter (fun id ->
                Map.find id cardPowers = power
                )
            |> List.head
    attackerIDs, targetID, List.length attackerIDs * 1<health>

let private damageCard cardID damage (lane: Lane) =
    {
        lane with
            UnitHealths =
                lane.UnitHealths
                |> Map.change cardID (function | None -> None | Some health -> Some (health - damage))
        }

let private exhaustCards cardIDs (lane: Lane) =
    {lane with
        ActiveUnits =
            cardIDs
            |> List.fold (fun aus id -> Map.add id Exhausted aus) lane.ActiveUnits
        }

let private changeDiscard discard (board: Board) =
    {board with Discard = discard}

let private findDeadCards (laneID: LaneID) (board: Board) =
    let lane = List.item (int laneID - 1) board.Lanes
    lane.UnitHealths
    |> Map.toList
    |> List.choose (fun (id, health) -> if health = 0<health> then Some id else None)

let private removeCardsFromUnitOwners cardIDs lane =
    {lane with UnitOwners = Map.filter (fun id _ -> not (List.contains id cardIDs)) lane.UnitOwners}
let private removeCardsFromUnitHealths cardIDs lane =
    {lane with UnitHealths = Map.filter (fun id _ -> not (List.contains id cardIDs)) lane.UnitHealths}
let private removeCardsFromInactiveUnits cardIDs (lane: Lane) =
    {lane with InactiveUnits = List.filter (fun id -> not (List.contains id cardIDs)) lane.InactiveUnits}
let private removeCardsFromActiveUnits cardIDs (lane: Lane) =
    {lane with ActiveUnits = Map.filter (fun id _ -> not (List.contains id cardIDs)) lane.ActiveUnits}
let private removeCardsFromUnitPairs cardIDs lane =
    {lane with UnitPairs = Map.filter (fun id id2 -> not (List.contains id cardIDs || List.contains id2 cardIDs)) lane.UnitPairs}

let private moveDeadCardsToDiscard laneID (board: Board) =
    let deadCards = findDeadCards laneID board
    board
    |> changeLaneWithFn laneID (
        removeCardsFromUnitOwners deadCards
        >> removeCardsFromUnitHealths deadCards
        >> removeCardsFromInactiveUnits deadCards
        >> removeCardsFromActiveUnits deadCards
        >> removeCardsFromUnitPairs deadCards
        )
    |> changeDiscard (List.fold (fun discard id -> discard @ [id]) board.Discard deadCards)

let private executeAttackAction playerID laneID attackerInfo targetInfo gameState =
    let cardsState = gameState.CardsState
    let board = cardsState.Board
    let attackerIDs, targetID, damage =
        getAttackInfo cardsState.CardPowers board.Lanes laneID playerID attackerInfo targetInfo board.RevealedCards
    board
    |> changeLaneWithFn laneID (
        exhaustCards attackerIDs
        >> damageCard targetID damage
        )
    |> moveDeadCardsToDiscard laneID
    |> changeBoard cardsState
    |> changeCardsState gameState

let private findPairee ownerID health power readiness cardPowers activeUnits unitOwners unitHealths =
    unitOwners
    |> Map.toList
    |> List.choose (fun (id, owner) ->
        if owner = ownerID then
            Some id
        else
            None
        )
    |> List.filter (fun id ->
        match Map.tryFind id unitHealths with
        | Some h -> h = health
        | None -> false
    )
    |> List.filter (fun id -> Map.find id cardPowers = power)
    |> List.filter (fun id -> Map.tryFind id activeUnits = Some readiness)
    |> List.head

let private addCardsToPairs cardID1 cardID2 lane =
    {lane with
        UnitPairs =
            lane.UnitPairs
            |> Map.add cardID1 cardID2
            |> Map.add cardID2 cardID1
        }

let private executeCreatePairAction playerID laneID power (health1, readiness1) (health2, readiness2) gameState =
    let boardInfo = gameState.CardsState.Board
    let lane = List.item (int laneID - 1) boardInfo.Lanes
    let cardID1 =
        lane.UnitHealths
        |> findPairee playerID health1 power readiness1 gameState.CardsState.CardPowers lane.ActiveUnits lane.UnitOwners
    let cardID2 =
        lane.UnitHealths
        |> Map.filter (fun cardID _ -> cardID <> cardID1)
        |> findPairee playerID health2 power readiness2 gameState.CardsState.CardPowers lane.ActiveUnits lane.UnitOwners
    gameState.CardsState.Board
    |> changeLaneWithFn laneID (addCardsToPairs cardID1 cardID2)
    |> changeBoard gameState.CardsState
    |> changeCardsState gameState

let private updateLockedLaneWins (gameState: GameStateDuringTurn) =
    match gameState.CardsState.GameStage with
    | Early _
    | DrawPileEmpty _ ->
        gameState
    | HandsEmpty {LockedLaneWins = lw} ->
        let lanes = gameState.CardsState.Board.Lanes
        let currentLaneWins =
            lanes
            |> List.zip [for i in 1..List.length lanes -> i*1<LID>]
            |> List.choose (fun (laneID, lane) ->
                match laneSolePresence lane.UnitOwners with
                | Some c -> Some (laneID, c)
                | None -> None
                )
            |> Map.ofList
        let newWins =
            currentLaneWins
            |> Map.fold (fun state laneID winnerID -> Map.add laneID winnerID state) lw
        let newCardsState = {gameState.CardsState with GameStage = HandsEmpty {LockedLaneWins = newWins}}
        {gameState with CardsState = newCardsState}

let private checkForGameEnd gameState =
    match gameState with
    | GameStateDuringTurn {CardsState = cs} ->
        match cs.GameStage with
        | Early _
        | DrawPileEmpty _ ->
            gameState
        | HandsEmpty {LockedLaneWins = laneWins} ->
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
                    GameStateWon {Winner = leadingPlayer; Lanes = lanes}
                else
                    let contestedLanes =
                        lanes
                        |> List.zip [for i in 1..List.length lanes -> i*1<LID>]
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
                        GameStateTied {Lanes = lanes}
                    else
                        gameState
    | GameStateBetweenTurns _
    | GameStateWon _
    | GameStateTied _ ->
        gameState

let private executeTurnAction action gameState =
    let newStateBeforeActionUpdate =
        match action with
        | Play (handPosition, laneID) ->
            executePlayAction handPosition laneID gameState
        | Activate (playerID, laneID, lanePlayerPosition) ->
            executeActivateAction playerID laneID lanePlayerPosition gameState
        | Attack (playerID, laneID, attackerInfo, targetInfo) ->
            executeAttackAction playerID laneID attackerInfo targetInfo gameState
        | CreatePair (playerID, laneID, power, (health1, readiness1), (health2, readiness2)) ->
            executeCreatePairAction playerID laneID power (health1, readiness1) (health2, readiness2) gameState
    match gameState.CardsState.GameStage with
    | Early _ ->
        {newStateBeforeActionUpdate with
            TurnState = {
                newStateBeforeActionUpdate.TurnState with
                    ActionsLeft = newStateBeforeActionUpdate.TurnState.ActionsLeft - 1
                }
            }
    | DrawPileEmpty _ ->
        {newStateBeforeActionUpdate with
            TurnState = {
                newStateBeforeActionUpdate.TurnState with
                    ActionsLeft = newStateBeforeActionUpdate.TurnState.ActionsLeft - 1
                }
            }
    | HandsEmpty _ ->
        {newStateBeforeActionUpdate with
            TurnState = {
                newStateBeforeActionUpdate.TurnState with
                    ActionsLeft = newStateBeforeActionUpdate.TurnState.ActionsLeft - 1
                }
            }
        |> updateLockedLaneWins // only want to do this after a play or an attack

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

let private flipBasesOnLane (bases: Bases, lane: Lane) =
    let newUnitOwners =
        bases
        |> Map.fold (fun table playerID cardID -> Map.add cardID playerID table) lane.UnitOwners
    let newUnitHealths =
        bases
        |> Map.fold (fun table playerID cardID -> Map.add cardID 2<health> table) lane.UnitHealths
    {
        lane with
            UnitOwners = newUnitOwners
            UnitHealths = newUnitHealths
        }

let private flipBasesOnBoard bases lanes discard hiddenCardKnownBys revealedCards =
    {
        Lanes = List.map flipBasesOnLane (List.zip bases lanes)
        Discard = discard
        HiddenCardKnownBys = hiddenCardKnownBys
        RevealedCards = revealedCards
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
                    GameStage = DrawPileEmpty {HandCards = newHandCards}
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
                    {lane with ActiveUnits = readyActiveUnits lane.ActiveUnits}
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
            newDisplayInfo
            |> getPossibleActionsInfo newGameState
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
        [for i in 1..List.length shuffledPowerDeck -> i*1<CID>]
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
        getPossibleActionsInfo gameState displayInfo
        |> List.map (makeNextActionInfo gameState)
    InProgress (displayInfo, nextActions)

let api = {
    NewGame = fun () -> createGame 2 3
}
