module Implementation
open Domain

type private CardPowers = Map<CardID, Power>

type private HiddenCardKnownBys = (CardID * PlayerID) Set
type private Bases = Map<PlayerID, CardID>
type private HandCards = CardID list

type private Units = Map<CardID, (PlayerID * Health)>
type private InactiveUnits = CardID Set
type private ActiveUnits = Map<CardID, Readiness>
type private UnitPairs = Map<CardID, CardID>

type private Discard = CardID Set
type private RemovedCards = CardID Set

type private DrawPile = {
    TopCard: CardID
    Rest: CardID list
    }

type private Lane = {
    Units: Units
    InactiveUnits: InactiveUnits
    ActiveUnits: ActiveUnits
    UnitPairs: UnitPairs
}

let private emptyLane = {
    Units = Map.empty
    InactiveUnits = Set.empty
    ActiveUnits = Map.empty
    UnitPairs = Map.empty
}

let private laneSolePresence (units: Units) =
    let playerCounts =
        units
        |> Map.toList
        |> List.countBy (fun (id, (owner, health)) -> owner)
    match playerCounts with
    | [] -> None
    | [(controller, _)] -> Some controller
    | _ -> None

type private Board = {
    Lanes: Lane list
    Discard: Discard
    HiddenCardKnownBys: HiddenCardKnownBys
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

let private getTroops playerID (cardPowers: CardPowers) (units: Units) inactiveUnits activeUnits unitPairs hiddenCardKnownBys : TroopsKnowledge =
    let pairCardIDs =
        unitPairs
        |> Map.toList
        |> List.distinctBy (fun (x, y) -> min x y, max x y)
    let pairPowers =
        pairCardIDs
        |> List.map (fun (x, _) -> Map.find x cardPowers)
    let pairHealths1, pairHealths2 =
        pairCardIDs
        |> List.map (fun (x, y) -> Map.find x units, Map.find y units)
        |> List.map (fun ((pid1, health1), (pid2, health2)) -> health1, health2)
        |> List.unzip
    let pairReadinesses =
        pairCardIDs
        |> List.map (fun (x, y) -> max (Map.find x activeUnits) (Map.find y activeUnits))
    let pairOwners =
        pairCardIDs
        |> List.map (fun (x, _) -> Map.find x units |> fun (owner, _) -> owner)
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
            let owner, health = Map.find id units
            owner, ActiveCardKnowledge (Map.find id cardPowers, health, readiness)
            )
    let inactiveUnitKnowledge =
        inactiveUnits
        |> Set.toList
        |> List.map (fun cardID ->
            let owner, health = Map.find cardID units
            if Set.contains (cardID, playerID) hiddenCardKnownBys then
                let power = Map.find cardID cardPowers
                owner, KnownInactiveCardKnowledge (power, health)
            else
                owner, UnknownInactiveCardKnowledge health
            )
    inactiveUnitKnowledge @ nonPairedActiveUnitKnowledge @ pairKnowledge
    |> List.groupBy (fun (pid, _) -> pid)
    |> List.map (fun (pid, pairs) -> pid, pairs |> List.map (fun (_, knowledge) -> knowledge))
    |> Map.ofList

let private getDeadCardKnowledge (playerID: PlayerID) (cardPowers: CardPowers) knownBys cardID =
    let power = Map.find cardID cardPowers
    if Set.contains (cardID, playerID) knownBys then
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
            let {Lanes = l; Discard = d; HiddenCardKnownBys = kb} = gs.CardsState.Board
            match gs.CardsState.GameStage with
            | Early {Bases = b; DrawPile = dp} ->
                let lanesKnowledge =
                    List.zip b l
                    |> List.map (fun (bases, {Units = units; InactiveUnits = inactiveUnits; ActiveUnits = activeUnits; UnitPairs = unitPairs}) ->
                        let troops = getTroops id cardPowers units inactiveUnits activeUnits unitPairs kb
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
                            Troops = troops
                            } : PreBaseFlipLaneKnowledge
                        )
                let drawPileSize = 1 + List.length dp.Rest
                let discardKnowledge =
                    d
                    |> Set.toList
                    |> List.map (fun cardID -> getDeadCard gs.CardsState.CardPowers kb cardID)
                PreBaseFlipBoardKnowledge {
                    Lanes = lanesKnowledge
                    DrawPileSize = drawPileSize
                    Discard = discardKnowledge
                    }
            | DrawPileEmpty _ ->
                let lanesKnowledge =
                    l
                    |> List.map (fun {Units = units; InactiveUnits = inactiveUnits; ActiveUnits = activeUnits; UnitPairs = unitPairs} ->
                        let troops = getTroops id cardPowers units inactiveUnits activeUnits unitPairs kb
                        match (laneSolePresence units) with
                        | None ->
                            ContestedLaneKnowledge {
                                Troops = troops
                                }
                        | Some c ->
                            WonLaneKnowledge {
                                Controller = c
                                Troops = troops
                                }
                        )
                let discardKnowledge =
                    d
                    |> Set.toList
                    |> List.map (fun cardID -> getDeadCard gs.CardsState.CardPowers kb cardID)
                PostBaseFlipBoardKnowledge {
                    Lanes = lanesKnowledge
                    Discard = discardKnowledge
                    }
            | HandsEmpty {LockedLaneWins = laneWins} ->
                let lanesKnowledge =
                    l
                    |> List.zip ([for i in 1..List.length l -> i*1<LID>])
                    |> List.map (fun (laneID, {Units = units; InactiveUnits = inactiveUnits; ActiveUnits = activeUnits; UnitPairs = unitPairs}) ->
                        let troops = getTroops id cardPowers units inactiveUnits activeUnits unitPairs kb
                        match Map.tryFind laneID laneWins with
                        | None ->
                            ContestedLaneKnowledge {
                                Troops = troops
                                }
                        | Some c ->
                            WonLaneKnowledge {
                                Controller = c
                                Troops = troops
                                }
                        )
                let discardKnowledge =
                    d
                    |> Set.toList
                    |> List.map (fun cardID -> getDeadCard gs.CardsState.CardPowers kb cardID)
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
                match laneSolePresence lane.Units with
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
                match laneSolePresence lane.Units with
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

let private getPlayActionsInfo (turnDisplayInfo: TurnDisplayInfo) =
    let validPlayLanes =
        match turnDisplayInfo.BoardKnowledge with
        | PreBaseFlipBoardKnowledge {Lanes = l} ->
            [1..List.length l]
        | PostBaseFlipBoardKnowledge {Lanes = l} ->
            [1..List.length l]
    turnDisplayInfo.PlayerHand
    |> List.distinct
    |> List.allPairs validPlayLanes
    |> List.map (fun (lane, HandCard power) ->
        Play (turnDisplayInfo.CurrentPlayer, power, lane*1<LID>)
        |> TurnActionInfo
        )

let private getActivateActionsInfo (turnDisplayInfo: TurnDisplayInfo) =
    let playerID = turnDisplayInfo.CurrentPlayer
    match turnDisplayInfo.BoardKnowledge with
    | PreBaseFlipBoardKnowledge {Lanes = l} ->
        l
        |> List.indexed
        |> List.collect (fun (n, {Troops = troops}) ->
            troops
            |> Map.tryFind playerID
            |> function
            | None -> []
            | Some ownTroops ->
                ownTroops
                |> List.choose (fun troop ->
                    match troop with
                    | UnknownInactiveCardKnowledge health ->
                        Activate (playerID, (n + 1)*1<LID>, UnknownActivationTarget health)
                        |> TurnActionInfo
                        |> Some
                    | KnownInactiveCardKnowledge (power, health) ->
                        Activate (playerID, (n + 1)*1<LID>, KnownActivationTarget (power, health))
                        |> TurnActionInfo
                        |> Some
                    | ActiveCardKnowledge _
                    | PairKnowledge _ ->
                        None
                    )
            |> List.distinct
            )
    | PostBaseFlipBoardKnowledge {Lanes = l} ->
        l
        |> List.indexed
        |> List.collect (fun (n, lane) ->
            match lane with
            | ContestedLaneKnowledge {Troops = troops} ->
                troops
                |> Map.tryFind playerID
                |> function
                | None -> []
                | Some ownTroops ->
                    ownTroops
                    |> List.choose (fun troop ->
                        match troop with
                        | UnknownInactiveCardKnowledge health ->
                            Activate (playerID, (n + 1)*1<LID>, UnknownActivationTarget health)
                            |> TurnActionInfo
                            |> Some
                        | KnownInactiveCardKnowledge (power, health) ->
                            Activate (playerID, (n + 1)*1<LID>, KnownActivationTarget (power, health))
                            |> TurnActionInfo
                            |> Some
                        | ActiveCardKnowledge _
                        | PairKnowledge _ ->
                            None
                        )
                |> List.distinct
            | WonLaneKnowledge {Controller = c; Troops = troops} when c = playerID ->
                troops
                |> Map.tryFind playerID
                |> function
                | None -> []
                | Some ownTroops ->
                    ownTroops
                    |> List.choose (fun troop ->
                        match troop with
                        | UnknownInactiveCardKnowledge health ->
                            Activate (playerID, (n + 1)*1<LID>, UnknownActivationTarget health)
                            |> TurnActionInfo
                            |> Some
                        | KnownInactiveCardKnowledge (power, health) ->
                            Activate (playerID, (n + 1)*1<LID>, KnownActivationTarget (power, health))
                            |> TurnActionInfo
                            |> Some
                        | ActiveCardKnowledge _
                        | PairKnowledge _ ->
                            None
                        )
                |> List.distinct
            | WonLaneKnowledge _ ->
                []
            )

let private getPairActionsInfoFromTroops playerID laneID troops =
    let potentialSingles =
        troops
        |> Map.tryFind playerID
        |> function
        | None -> []
        | Some ownTroops ->
            ownTroops
            |> List.choose (fun troop ->
                match troop with
                | ActiveCardKnowledge (power, health, readiness) ->
                    Some (power, health, readiness)
                | UnknownInactiveCardKnowledge _
                | KnownInactiveCardKnowledge _
                | PairKnowledge _ ->
                    None
                )
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
        |> List.indexed
        |> List.collect (fun (n, {Troops = troops}) ->
            let laneID = (n + 1)*1<LID>
            let possibleAttackers =
                troops
                |> Map.tryFind playerID
                |> function
                | None -> []
                | Some ownTroops ->
                    ownTroops
                    |> List.distinct
                    |> List.choose (function
                        | UnknownInactiveCardKnowledge _
                        | KnownInactiveCardKnowledge _ ->
                            None
                        | ActiveCardKnowledge (p, h, r) ->
                            if r = Ready then
                                Some (SingleAttacker (p, h))
                            else
                                None
                        | PairKnowledge (p, h1, h2, r) ->
                            if r = Ready then
                                Some (DoubleAttacker (p, h1, h2))
                            else
                                None
                        )
                    |> List.distinct
            let possibleTargets =
                troops
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
                        | ActiveCardKnowledge (p, h, _) ->
                            ActiveSingleTarget (pid, p, h)
                            |> List.singleton
                        | PairKnowledge (p, h1, h2, _) ->
                            [
                                ActivePairMemberTarget (pid, p, h1, h2);
                                ActivePairMemberTarget (pid, p, h2, h1)
                                ]
                            |> List.distinct
                        )
                    |> List.distinct
                    )
            List.allPairs possibleAttackers possibleTargets
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
            | ContestedLaneKnowledge {Troops = troops} ->
                let possibleAttackers =
                    troops
                    |> Map.tryFind playerID
                    |> function
                    | None -> []
                    | Some ownTroops ->
                        ownTroops
                        |> List.distinct
                        |> List.choose (function
                            | UnknownInactiveCardKnowledge _
                            | KnownInactiveCardKnowledge _ ->
                                None
                            | ActiveCardKnowledge (p, h, r) ->
                                if r = Ready then
                                    Some (SingleAttacker (p, h))
                                else
                                    None
                            | PairKnowledge (p, h1, h2, r) ->
                                if r = Ready then
                                    Some (DoubleAttacker (p, h1, h2))
                                else
                                    None
                            )
                        |> List.distinct
                let possibleTargets =
                    troops
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
                            | ActiveCardKnowledge (p, h, _) ->
                                ActiveSingleTarget (pid, p, h)
                                |> List.singleton
                            | PairKnowledge (p, h1, h2, _) ->
                                [
                                    ActivePairMemberTarget (pid, p, h1, h2);
                                    ActivePairMemberTarget (pid, p, h2, h1)
                                    ]
                                |> List.distinct
                            )
                        |> List.distinct
                        )
                List.allPairs possibleAttackers possibleTargets
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
        |> List.indexed
        |> List.collect (fun (n, {Troops = troops}) ->
            let laneID = (n + 1)*1<LID>
            getPairActionsInfoFromTroops playerID laneID troops
            )
    | PostBaseFlipBoardKnowledge {Lanes = l} ->
        l
        |> List.indexed
        |> List.collect (fun (n, lane) ->
            let laneID = (n + 1)*1<LID>
            match lane with
            | ContestedLaneKnowledge {Troops = troops} ->
                getPairActionsInfoFromTroops playerID laneID troops
            | WonLaneKnowledge {Controller = c; Troops = troops} when c = playerID ->
                getPairActionsInfoFromTroops playerID laneID troops
            | WonLaneKnowledge _ ->
                []
            )

let private getPossibleActionsInfo (displayInfo: DisplayInfo) =
    match displayInfo with
    | TurnDisplayInfo turnDisplayInfo ->
        if turnDisplayInfo.ActionsLeft = 0 then
            EndTurn turnDisplayInfo.CurrentPlayer |> List.singleton
        else
            let actions =
                getPlayActionsInfo turnDisplayInfo
                @ (getActivateActionsInfo turnDisplayInfo)
                @ (getAttackActionsInfo turnDisplayInfo)
                @ (getPairActionsInfo turnDisplayInfo)
            if List.isEmpty actions then
                EndTurn turnDisplayInfo.CurrentPlayer |> List.singleton
            else
                actions
    | SwitchDisplayInfo playerID ->
        StartTurn playerID |> List.singleton
    | WonGameDisplayInfo _
    | TiedGameDisplayInfo _ ->
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
                        Units = Map.add cardID (playerID, 2<health>) lane.Units
                        InactiveUnits = Set.add cardID lane.InactiveUnits
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

let private executePlayAction playerID power laneID gameState =
    let handCards =
        match gameState.CardsState.GameStage with
        | Early {HandCards = hco} ->
            hco
        | DrawPileEmpty {HandCards = hco} ->
            hco
        | HandsEmpty _ ->
            failwithf "can't play a card when hands are empty"
    let currentPlayerHand = Map.find playerID handCards
    let cardID =
        gameState.CardsState.CardPowers
        |> Map.filter (fun _ p -> p = power)
        |> Map.toList
        |> List.map (fun (card, _) -> card)
        |> List.find (fun card -> List.contains card currentPlayerHand)
    gameState.CardsState
    |> addCardToBoard cardID playerID laneID
    |> removeCardFromHand cardID playerID
    |> removeHandsIfAllEmpty
    |> changeCardsState gameState

let private removeCardFromInactiveUnits cardID lane =
    {lane with InactiveUnits = Set.remove cardID lane.InactiveUnits}

let private addCardToActiveUnits cardID lane =
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

let private executeActivateAction playerID laneID activationTarget gameState =
    let cardsState = gameState.CardsState
    let lanes = cardsState.Board.Lanes
    let lane = List.item (int laneID - 1) lanes
    let cardID =
        match activationTarget with
        | UnknownActivationTarget health ->
            lane.Units
            |> Map.filter (fun index (owner, h) -> owner = playerID && h = health)
            |> Map.toList
            |> List.map (fun (index, _) -> index)
            |> List.head
        | KnownActivationTarget (power, health) ->
            lane.Units
            |> Map.filter (fun index (owner, h) -> owner = playerID && h = health)
            |> Map.toList
            |> List.map (fun (index, _) -> index)
            |> List.filter (fun index -> Map.find index cardsState.CardPowers = power)
            |> List.head
    cardsState.Board
    |> changeLaneWithFn laneID (removeCardFromInactiveUnits cardID >> addCardToActiveUnits cardID)
    |> removeCardFromKnownBys cardID
    |> changeBoard cardsState
    |> changeCardsState gameState

let private getAttackInfo (cardPowers: CardPowers) units (inactiveUnits: InactiveUnits) (activeUnits: ActiveUnits) unitPairs playerID attackerInfo targetInfo =
    let attackerIDs =
        match attackerInfo with
        | SingleAttacker (power, health) ->
            units
            |> Map.toList
            |> List.choose (fun (id, (owner, h)) ->
                if owner = playerID && h = health then
                    Some id
                else
                    None
                )
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
                    match Map.tryFind id units with
                    | Some (pid, h) -> pid = playerID && h = health1
                    | None -> false
                    )
            let secondPartners =
                readyCardsWithPower
                |> List.filter (fun id ->
                    match Map.tryFind id units with
                    | Some (pid, h) -> pid = playerID && h = health2
                    | None -> false
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
            units
            |> Map.toList
            |> List.choose (fun (id, (o, h)) ->
                if o = owner && h = health then
                    Some id
                else
                    None
                )
            |> List.head
        | KnownInactiveTarget (owner, power, health) ->
            units
            |> Map.toList
            |> List.choose (fun (id, (o, h)) ->
                if o = owner && h = health then
                    Some id
                else
                    None
                )
            |> List.filter (fun id -> Set.contains id inactiveUnits)
            |> List.filter (fun id ->
                Map.find id cardPowers = power
                )
            |> List.head
        | ActiveSingleTarget (owner, power, health) ->
            units
            |> Map.toList
            |> List.choose (fun (id, (o, h)) ->
                if o = owner && h = health then
                    Some id
                else
                    None
                )
            |> List.filter (fun id -> Map.containsKey id activeUnits)
            |> List.filter (fun id ->
                Map.find id cardPowers = power
                )
            |> List.head
        | ActivePairMemberTarget (owner, power, health, partnerHealth) ->
            units
            |> Map.toList
            |> List.choose (fun (id, (o, h)) ->
                if o = owner && h = health then
                    Some id
                else
                    None
                )
            |> List.filter (fun id -> Map.containsKey id unitPairs)
            |> List.filter (fun id ->
                let _, ph = Map.find (Map.find id unitPairs) units
                ph = partnerHealth
                )
            |> List.filter (fun id ->
                Map.find id cardPowers = power
                )
            |> List.head
    attackerIDs, targetID, List.length attackerIDs * 1<health>

let private exhaust cardID activeCards =
    activeCards
    |> Map.change cardID (function
        | None -> None
        | Some _ -> Some Exhausted
        )

let private executeAttackAction playerID laneID attackerInfo targetInfo gameState =
    let {Lanes = lanes; Discard = discard; HiddenCardKnownBys = hiddenCardKnownBys} = gameState.CardsState.Board
    let lane = List.item (int laneID - 1) lanes
    let attackerIDs, targetID, damage =
        getAttackInfo gameState.CardsState.CardPowers lane.Units lane.InactiveUnits lane.ActiveUnits lane.UnitPairs playerID attackerInfo targetInfo
    let _, targetHealth = Map.find targetID lane.Units
    let newUnits, newInactiveUnits, newActiveUnits, newUnitPairs, newDiscard =
        if targetHealth <= damage then
            Map.remove targetID lane.Units,
            lane.InactiveUnits
            |> Set.remove targetID,
            attackerIDs
            |> List.fold (fun activeUnits id -> exhaust id activeUnits) lane.ActiveUnits
            |> Map.remove targetID,
            Map.filter (fun key value -> key <> targetID && value <> targetID) lane.UnitPairs,
            Set.add targetID discard
        else
            lane.Units
            |> Map.change targetID (function | None -> None | Some (owner, health) -> Some (owner, health - damage)),
            lane.InactiveUnits,
            attackerIDs
            |> List.fold (fun activeUnits id -> exhaust id activeUnits) lane.ActiveUnits,
            lane.UnitPairs,
            discard
    let newLane = {
        lane with
            Units = newUnits
            InactiveUnits = newInactiveUnits
            ActiveUnits = newActiveUnits
            UnitPairs = newUnitPairs
        }
    let newBoard =
        {
            Lanes =
                lanes
                |> List.mapi (fun n ln ->
                    if n = int laneID - 1 then
                        newLane
                    else
                        ln
                    )
            Discard = newDiscard
            HiddenCardKnownBys = hiddenCardKnownBys
            }
    {gameState with CardsState = {gameState.CardsState with Board = newBoard}}

let private findPairee ownerID health power readiness cardPowers activeUnits units =
    units
    |> Map.toList
    |> List.choose (fun (id, (owner, h)) ->
        if owner = ownerID && h = health then
            Some id
        else
            None
        )
    |> List.filter (fun id -> Map.find id cardPowers = power)
    |> List.filter (fun id -> Map.tryFind id activeUnits = Some readiness)
    |> List.head

let private executeCreatePairAction playerID laneID power (health1, readiness1) (health2, readiness2) gameState =
    let newBoard =
        let boardInfo = gameState.CardsState.Board
        let newLanes =
            boardInfo.Lanes
            |> List.mapi (fun n lane ->
                if (n + 1)*1<LID> = laneID then
                    let cardID1 =
                        lane.Units
                        |> findPairee playerID health1 power readiness1 gameState.CardsState.CardPowers lane.ActiveUnits
                    let cardID2 =
                        lane.Units
                        |> Map.filter (fun cardID _ -> cardID <> cardID1)
                        |> findPairee playerID health2 power readiness2 gameState.CardsState.CardPowers lane.ActiveUnits
                    {lane with UnitPairs = lane.UnitPairs |> Map.add cardID1 cardID2 |> Map.add cardID2 cardID1}
                else
                    lane
                )
        {boardInfo with Lanes = newLanes}
    {gameState with CardsState = {gameState.CardsState with Board = newBoard}}

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
                match laneSolePresence lane.Units with
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
        | Early _ ->
            gameState
        | DrawPileEmpty _ ->
            let lanes = cs.Board.Lanes
            let wonLaneCounts =
                lanes
                |> List.choose (fun {Units = units} ->
                    laneSolePresence units
                    )
                |> List.countBy id
            match wonLaneCounts with
            | [] -> gameState
            | lst ->
                let (leadingPlayer, leadingWins) =
                    lst
                    |> List.maxBy (fun (_, n) -> n)
                if leadingWins >= 2 then
                    GameStateWon {Winner = leadingPlayer; Lanes = lanes}
                else
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
                        |> List.filter (fun (laneID, {Units = units}) ->
                            match Map.containsKey laneID laneWins, Map.isEmpty units with
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
        | Play (playerID, power, laneID) ->
            executePlayAction playerID power laneID gameState
        | Activate (playerID, laneID, activationTarget) ->
            executeActivateAction playerID laneID activationTarget gameState
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
    let newUnits =
        bases
        |> Map.fold (fun table playerID cardID -> Map.add cardID (playerID, 2<health>) table) lane.Units
    let newInactiveUnits =
        bases
        |> Map.fold (fun table playerID cardID -> Set.add cardID table) lane.InactiveUnits
    {
        Units = newUnits
        ActiveUnits = lane.ActiveUnits
        InactiveUnits = newInactiveUnits
        UnitPairs = lane.UnitPairs
        }

let private flipBasesOnBoard bases lanes discard hiddenCardKnownBys =
    {
        Lanes = List.map flipBasesOnLane (List.zip bases lanes)
        Discard = discard
        HiddenCardKnownBys = hiddenCardKnownBys
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
                preInfo.HandCards
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
                    Board = flipBasesOnBoard preInfo.Bases boardInfo.Lanes boardInfo.Discard newKnownBys
                    GameStage = DrawPileEmpty {HandCards = newHandCards}
                }
            {gameState with CardsState = newCards}
        | newTopCard :: newRest ->
            let newHandCards =
                preInfo.HandCards
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
            |> getPossibleActionsInfo
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
        [0..(List.length shuffledPowerDeck - 1)]
        |> List.map (fun n -> n*1<CID>)
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
                Discard = Set.empty
                HiddenCardKnownBys =
                    handCards
                    |> Map.toList
                    |> List.collect (fun (playerID, cards) -> List.map (fun card -> card, playerID) cards)
                    |> Set.ofList
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
        getPossibleActionsInfo displayInfo
        |> List.map (makeNextActionInfo gameState)
    InProgress (displayInfo, nextActions)

let api = {
    NewGame = fun () -> createGame 2 3
}
