module Implementation
open Domain

type private FaceDownDeadCard = Power * KnownBy
type private FaceUpDeadCard = Power
type private DeadCard =
| FaceDownDeadCard of FaceDownDeadCard
| FaceUpDeadCard of FaceUpDeadCard

type Units = Map<CardID, (PlayerID * Health)>
type InactiveUnits = Map<CardID, KnownBy>
type ActiveUnits = Map<CardID, Readiness>
type UnitPairs = Map<CardID, CardID>

type private DrawPile = {
    TopCard: CardID
    Rest: CardID list
    }

type private PreBaseFlipLane = {
    BasesTable: Map<PlayerID, (CardID * KnownBy)>
    Units: Units
    InactiveUnits: InactiveUnits
    ActiveUnits: ActiveUnits
    UnitPairs: UnitPairs
}

type private PostBaseFlipLane = {
    Units: Units
    InactiveUnits: InactiveUnits
    ActiveUnits: ActiveUnits
    UnitPairs: UnitPairs
}

let private laneControl (units: Units) =
    let playerCounts =
        units
        |> Map.toList
        |> List.countBy (fun (id, (owner, health)) -> owner)
    match playerCounts with
    | [] -> None
    | [(controller, _)] -> Some controller
    | _ -> None

type private Discard = CountMap.CountMap<DeadCard>

type private PreBaseFlipBoard = {
    Lanes: PreBaseFlipLane list
    DrawPile: DrawPile
    Discard: Discard
}

type private PostBaseFlipBoard = {
    Lanes: PostBaseFlipLane list
    Discard: Discard
}

type private Board =
| PreBaseFlipBoard of PreBaseFlipBoard
| PostBaseFlipBoard of PostBaseFlipBoard

type private CardPowers = Map<CardID, Power>

type private CardsState = {
    Board: Board
    HandCardOwners: Map<CardID, PlayerID>
    CardPowers: CardPowers
    Removed: CardID Set
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
    Lanes: PostBaseFlipLane list
    Winner: PlayerID
}

type private GameStateTied = {
    Lanes: PostBaseFlipLane list
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

let private createLane (basesInfo: (PlayerID * (CardID * KnownBy)) list) =
    {
        BasesTable = Map.ofList basesInfo
        Units = Map.empty
        InactiveUnits = Map.empty
        ActiveUnits = Map.empty
        UnitPairs = Map.empty
    }

let private prepareLane lst =
    let playerIDs =
        [1..List.length lst]
        |> List.map (fun n -> n*1<PID>)
    lst
    |> List.zip playerIDs
    |> List.map (fun (playerIndex, index) -> playerIndex, (index, []))
    |> createLane

let private prepareLanes nLanes lst =
    lst
    |> List.splitInto nLanes
    |> List.map prepareLane

let private prepareHands nPlayers lst =
    let playerIDs =
        [1..nPlayers]
        |> List.map (fun n -> n*1<PID>)
        |> List.collect (List.replicate 5)
    List.zip lst playerIDs
    |> Map.ofList

let private prepareRemoved lst =
    lst
    |> Set.ofList

let private prepareDrawPile lst =
    match lst with
    | h :: t -> {TopCard = h; Rest = t}
    | [] -> failwithf "can't prepare a draw pile with no cards"

let private getBaseKnowledge (playerID: PlayerID) (baseCard: PlayerID * Power * KnownBy) =
    let (ownerID, power, knownBy) = baseCard
    if List.contains playerID knownBy then
        KnownBaseCard (ownerID, power)
    else
        UnknownBaseCard ownerID

let private getTroops (playerID: PlayerID) (cardPowers: CardPowers) (units: Units) (inactiveUnits: InactiveUnits) (activeUnits: ActiveUnits) (unitPairs: UnitPairs) : TroopKnowledge list =
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
    let pairKnowledge =
        pairReadinesses
        |> List.zip3 pairHealths1 pairHealths2
        |> List.zip3 (List.replicate (List.length pairPowers) playerID) pairPowers
        |> List.map (fun (pid, p, (h1, h2, r)) -> PairKnowledge (pid, p, h1, h2, r))
    let nonPairedActiveUnitKnowledge =
        activeUnits
        |> Map.filter (fun cardID _ -> not (Map.containsKey cardID unitPairs))
        |> Map.toList
        |> List.map (fun (id, readiness) ->
            let owner, health = Map.find id units
            ActiveCardKnowledge (owner, Map.find id cardPowers, health, readiness)
            )
    let inactiveUnitKnowledge =
        inactiveUnits
        |> Map.toList
        |> List.map (fun (cardID, knownBy) ->
            let owner, health = Map.find cardID units
            if List.contains playerID knownBy then
                let power = Map.find cardID cardPowers
                KnownInactiveCardKnowledge (owner, power, health, knownBy)
            else
                UnknownInactiveCardKnowledge (owner, health, knownBy)
            )
    inactiveUnitKnowledge @ nonPairedActiveUnitKnowledge @ pairKnowledge

let private getDeadCardKnowledge (playerID: PlayerID) deadCard =
    match deadCard with
    | FaceDownDeadCard (power, knownBy) ->
        if List.contains playerID knownBy then
            KnownDeadCard (KnownFaceDownDeadCard power)
        else
            UnknownDeadCard
    | FaceUpDeadCard power ->
        KnownDeadCard (KnownFaceUpDeadCard power)

let private getDisplayInfo gameState =
    match gameState with
    | GameStateDuringTurn gs ->
        let id = gs.TurnState.CurrentPlayer
        let (playerHandInfo, opponentHandsInfo) =
            gs.CardsState.HandCardOwners
            |> Map.partition (fun _ owner -> owner = id)
        let playerHand =
            playerHandInfo
            |> Map.toList
            |> List.map (fun (card, _) -> card)
        let cardPowers = gs.CardsState.CardPowers
        let getBase = getBaseKnowledge id
        let getDeadCard = getDeadCardKnowledge id
        let boardKnowledge =
            match gs.CardsState.Board with
            | PreBaseFlipBoard {Lanes = l; DrawPile = dp; Discard = d} ->
                let lanesKnowledge =
                    l
                    |> List.map (fun {BasesTable = bases; Units = units; InactiveUnits = inactiveUnits; ActiveUnits = activeUnits; UnitPairs = unitPairs} ->
                        let troops = getTroops id cardPowers units inactiveUnits activeUnits unitPairs
                        {
                            Bases =
                                bases
                                |> Map.toList
                                |> List.map (fun (owner, (cardID, knownBy)) ->
                                    owner,
                                    Map.find cardID gs.CardsState.CardPowers,
                                    knownBy
                                    )
                                |> List.map getBase
                            Troops =
                                CountMap.ofList troops
                            } : PreBaseFlipLaneKnowledge
                        )
                let drawPileSize = 1 + List.length dp.Rest
                let discardKnowledge = CountMap.map getDeadCard d
                PreBaseFlipBoardKnowledge {
                    Lanes = lanesKnowledge
                    DrawPileSize = drawPileSize
                    Discard = discardKnowledge
                    }
            | PostBaseFlipBoard {Lanes = l; Discard = d} ->
                let lanesKnowledge =
                    l
                    |> List.map (fun {Units = units; InactiveUnits = inactiveUnits; ActiveUnits = activeUnits; UnitPairs = unitPairs} ->
                        let troops = getTroops id cardPowers units inactiveUnits activeUnits unitPairs
                        match (laneControl units) with
                        | None ->
                            ContestedLaneKnowledge {
                                Troops =
                                    CountMap.ofList troops
                                }
                        | Some c ->
                            WonLaneKnowledge {
                                Controller = c
                                Troops =
                                    CountMap.ofList troops
                                }
                        )
                let discardKnowledge = CountMap.map getDeadCard d
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
                |> CountMap.ofList
            OpponentHandSizes =
                opponentHandsInfo
                |> Map.toList
                |> List.countBy (fun (_, owner) -> owner)
                |> List.sortBy (fun (owner, _) -> owner)
        }
    | GameStateBetweenTurns {TurnState = ts} ->
        SwitchDisplayInfo ts.Player
    | GameStateWon {Lanes = lanes; Winner = winner} ->
        let laneWins =
            lanes
            |> List.indexed
            |> List.choose (fun (n, lane) ->
                match laneControl lane.Units with
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
                match laneControl lane.Units with
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
    |> CountMap.toList
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
            |> CountMap.choose (fun troop ->
                match troop with
                | UnknownInactiveCardKnowledge (ownerID, health, knownBy) when
                    ownerID = playerID ->
                    Activate (playerID, (n + 1)*1<LID>, UnknownActivationTarget (health, knownBy))
                    |> TurnActionInfo
                    |> Some
                | KnownInactiveCardKnowledge (ownerID, power, health, knownBy) when
                    ownerID = playerID ->
                    Activate (playerID, (n + 1)*1<LID>, KnownActivationTarget (power, health, knownBy))
                    |> TurnActionInfo
                    |> Some
                | UnknownInactiveCardKnowledge _
                | KnownInactiveCardKnowledge _
                | ActiveCardKnowledge _
                | PairKnowledge _ ->
                    None
                )
            |> CountMap.keyList
            )
    | PostBaseFlipBoardKnowledge {Lanes = l} ->
        l
        |> List.indexed
        |> List.collect (fun (n, lane) ->
            match lane with
            | ContestedLaneKnowledge {Troops = troops} ->
                troops
                |> CountMap.choose (fun troop ->
                    match troop with
                    | UnknownInactiveCardKnowledge (ownerID, health, knownBy) when
                        ownerID = playerID ->
                        Activate (playerID, (n + 1)*1<LID>, UnknownActivationTarget (health, knownBy))
                        |> TurnActionInfo
                        |> Some
                    | KnownInactiveCardKnowledge (ownerID, power, health, knownBy) when
                        ownerID = playerID ->
                        Activate (playerID, (n + 1)*1<LID>, KnownActivationTarget (power, health, knownBy))
                        |> TurnActionInfo
                        |> Some
                    | KnownInactiveCardKnowledge _
                    | UnknownInactiveCardKnowledge _
                    | ActiveCardKnowledge _
                    | PairKnowledge _ ->
                        None
                    )
                |> CountMap.keyList
            | WonLaneKnowledge {Controller = c; Troops = troops} when c = playerID ->
                troops
                |> CountMap.choose (fun troop ->
                    match troop with
                    | UnknownInactiveCardKnowledge (ownerID, health, knownBy) when
                        ownerID = playerID ->
                        Activate (playerID, (n + 1)*1<LID>, UnknownActivationTarget (health, knownBy))
                        |> TurnActionInfo
                        |> Some
                    | KnownInactiveCardKnowledge (ownerID, power, health, knownBy) when
                        ownerID = playerID ->
                        Activate (playerID, (n + 1)*1<LID>, KnownActivationTarget (power, health, knownBy))
                        |> TurnActionInfo
                        |> Some
                    | KnownInactiveCardKnowledge _
                    | UnknownInactiveCardKnowledge _
                    | ActiveCardKnowledge _
                    | PairKnowledge _ ->
                        None
                    )
                |> CountMap.keyList
            | WonLaneKnowledge _ ->
                []
            )

let private getPairActionsInfoFromTroops playerID laneID troops =
    let potentialSingles =
        troops
        |> CountMap.choose (fun troop ->
            match troop with
            | ActiveCardKnowledge (ownerID, power, health, readiness) ->
                if ownerID = playerID then
                    Some (power, health, readiness)
                else
                    None
            | UnknownInactiveCardKnowledge _
            | KnownInactiveCardKnowledge _
            | PairKnowledge _ ->
                None
            )
    let sameKeyPair =
        potentialSingles
        |> CountMap.filter (fun _ n -> n >= 2)
        |> CountMap.keyList
        |> List.map (fun troop ->
            match troop with
            | (power, health, readiness) ->
                (playerID, laneID, power, (health, readiness), (health, readiness))
                |> CreatePair
                |> TurnActionInfo
            )
    let rec distPairs lst =
        match lst with
        | [] -> []
        | [_] -> []
        | h::t -> List.allPairs [h] t @ distPairs t
    let differentKeyPair =
        potentialSingles
        |> CountMap.keyList
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
    sameKeyPair @ differentKeyPair

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
                |> CountMap.keyList
                |> List.choose (function
                    | UnknownInactiveCardKnowledge _
                    | KnownInactiveCardKnowledge _ ->
                        None
                    | ActiveCardKnowledge (pid, p, h, r) ->
                        if pid = playerID && r = Ready then
                            Some (SingleAttacker (p, h))
                        else
                            None
                    | PairKnowledge (pid, p, h1, h2, r) ->
                        if pid = playerID && r = Ready then
                            Some (DoubleAttacker (p, h1, h2))
                        else
                            None
                    )
                |> List.distinct
            let possibleTargets =
                troops
                |> CountMap.keyList
                |> List.collect (function
                    | UnknownInactiveCardKnowledge (pid, h, _) ->
                        if pid = playerID then
                            []
                        else
                            UnknownInactiveTarget (pid, h)
                            |> List.singleton
                    | KnownInactiveCardKnowledge (pid, p, h, _) ->
                        if pid = playerID then
                            []
                        else
                            KnownInactiveTarget (pid, p, h)
                            |> List.singleton
                    | ActiveCardKnowledge (pid, p, h, _) ->
                        if pid = playerID then
                            []
                        else
                            ActiveSingleTarget (pid, p, h)
                            |> List.singleton
                    | PairKnowledge (pid, p, h1, h2, _) ->
                        if pid = playerID then
                            []
                        else
                            [
                                ActivePairMemberTarget (pid, p, h1, h2);
                                ActivePairMemberTarget (pid, p, h2, h1)
                                ]
                            |> List.distinct
                    )
                |> List.distinct
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
                    |> CountMap.keyList
                    |> List.choose (function
                        | UnknownInactiveCardKnowledge _
                        | KnownInactiveCardKnowledge _ ->
                            None
                        | ActiveCardKnowledge (pid, p, h, r) ->
                            if pid = playerID && r = Ready then
                                Some (SingleAttacker (p, h))
                            else
                                None
                        | PairKnowledge (pid, p, h1, h2, r) ->
                            if pid = playerID && r = Ready then
                                Some (DoubleAttacker (p, h1, h2))
                            else
                                None
                        )
                    |> List.distinct
                let possibleTargets =
                    troops
                    |> CountMap.keyList
                    |> List.collect (fun troop ->
                        match troop with
                        | UnknownInactiveCardKnowledge (pid, h, _) ->
                            if pid = playerID then
                                []
                            else
                                UnknownInactiveTarget (pid, h)
                                |> List.singleton
                        | KnownInactiveCardKnowledge (pid, p, h, _) ->
                            if pid = playerID then
                                []
                            else
                                KnownInactiveTarget (pid, p, h)
                                |> List.singleton
                        | ActiveCardKnowledge (pid, p, h, _) ->
                            if pid = playerID then
                                []
                            else
                                ActiveSingleTarget (pid, p, h)
                                |> List.singleton
                        | PairKnowledge (pid, p, h1, h2, _) ->
                            if pid = playerID then
                                []
                            else
                                [
                                    ActivePairMemberTarget (pid, p, h1, h2);
                                    ActivePairMemberTarget (pid, p, h2, h1)
                                    ]
                                |> List.distinct
                        )
                    |> List.distinct
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

let private executePlayAction playerID power laneID gameState =
    let cardID =
        gameState.CardsState.CardPowers
        |> Map.filter (fun _ p -> p = power)
        |> Map.toList
        |> List.map (fun (card, _) -> card)
        |> List.find (fun card ->
            gameState.CardsState.HandCardOwners
            |> Map.filter (fun _ owner -> owner = playerID)
            |> Map.containsKey card
            )
    let newCards = {
        gameState.CardsState with
            HandCardOwners =
                gameState.CardsState.HandCardOwners
                |> Map.remove cardID
            Board =
                match gameState.CardsState.Board with
                | PreBaseFlipBoard pbfb ->
                    let l = pbfb.Lanes
                    let newLanes =
                        l
                        |> List.mapi (fun n lane ->
                            if (n + 1)*1<LID> = laneID then
                                {
                                    lane with
                                        Units = Map.add cardID (playerID, 2<health>) lane.Units
                                        InactiveUnits = Map.add cardID [playerID] lane.InactiveUnits
                                }
                            else
                                lane
                            )
                    PreBaseFlipBoard {pbfb with Lanes = newLanes}
                | PostBaseFlipBoard pbfb ->
                    let l = pbfb.Lanes
                    let newLanes =
                        l
                        |> List.mapi (fun n lane ->
                            if (n + 1)*1<LID> = laneID then
                                {
                                    lane with
                                        Units = Map.add cardID (playerID, 2<health>) lane.Units
                                        InactiveUnits = Map.add cardID [playerID] lane.InactiveUnits
                                }
                            else
                                lane
                            )
                    PostBaseFlipBoard {pbfb with Lanes = newLanes}
        }
    {gameState with CardsState = newCards}

let private executeActivateAction playerID laneID activationTarget gameState =
    let cardsState = gameState.CardsState
    let newBoard =
        match cardsState.Board, activationTarget with
        | PreBaseFlipBoard pbfb, UnknownActivationTarget (health, knownBy) ->
            let lanes = pbfb.Lanes
            let lane = List.item (int laneID - 1) lanes
            let cardID =
                lane.Units
                |> Map.filter (fun index (owner, h) -> owner = playerID && h = health)
                |> Map.toList
                |> List.map (fun (index, _) -> index)
                |> List.filter (fun index -> Map.find index lane.InactiveUnits = knownBy)
                |> List.head
            let newInactiveUnits = Map.remove cardID lane.InactiveUnits
            let newActiveUnits = Map.add cardID Ready lane.ActiveUnits
            let newLane = {
                lane with
                    InactiveUnits = newInactiveUnits
                    ActiveUnits = newActiveUnits
            }
            let newLanes =
                lanes
                |> List.mapi (fun n l ->
                    if n = (int laneID - 1) then
                        newLane
                    else
                        l
                )
            PreBaseFlipBoard {pbfb with Lanes = newLanes}
        | PreBaseFlipBoard pbfb, KnownActivationTarget (power, health, knownBy) ->
            let lanes = pbfb.Lanes
            let lane = List.item (int laneID - 1) lanes
            let cardID =
                lane.Units
                |> Map.filter (fun index (owner, h) -> owner = playerID && h = health)
                |> Map.toList
                |> List.map (fun (index, _) -> index)
                |> List.filter (fun index -> Map.find index cardsState.CardPowers = power)
                |> List.filter (fun index -> Map.find index lane.InactiveUnits = knownBy)
                |> List.head
            let newInactiveUnits = Map.remove cardID lane.InactiveUnits
            let newActiveUnits = Map.add cardID Ready lane.ActiveUnits
            let newLane = {
                lane with
                    InactiveUnits = newInactiveUnits
                    ActiveUnits = newActiveUnits
            }
            let newLanes =
                lanes
                |> List.mapi (fun n l ->
                    if n = (int laneID - 1) then
                        newLane
                    else
                        l
                )
            PreBaseFlipBoard {pbfb with Lanes = newLanes}
        | PostBaseFlipBoard pbfb, UnknownActivationTarget (health, knownBy) ->
            let lanes = pbfb.Lanes
            let lane = List.item (int laneID - 1) lanes
            let cardID =
                lane.Units
                |> Map.filter (fun index (owner, h) -> owner = playerID && h = health)
                |> Map.toList
                |> List.map (fun (index, _) -> index)
                |> List.filter (fun index -> Map.find index lane.InactiveUnits = knownBy)
                |> List.head
            let newInactiveUnits = Map.remove cardID lane.InactiveUnits
            let newActiveUnits = Map.add cardID Ready lane.ActiveUnits
            let newLane = {
                lane with
                    InactiveUnits = newInactiveUnits
                    ActiveUnits = newActiveUnits
            }
            let newLanes =
                lanes
                |> List.mapi (fun n l ->
                    if n = (int laneID - 1) then
                        newLane
                    else
                        l
                )
            PostBaseFlipBoard {pbfb with Lanes = newLanes}
        | PostBaseFlipBoard pbfb, KnownActivationTarget (power, health, knownBy) ->
            let lanes = pbfb.Lanes
            let lane = List.item (int laneID - 1) lanes
            let cardID =
                lane.Units
                |> Map.filter (fun index (owner, h) -> owner = playerID && h = health)
                |> Map.toList
                |> List.map (fun (index, _) -> index)
                |> List.filter (fun index -> Map.find index cardsState.CardPowers = power)
                |> List.filter (fun index -> Map.find index lane.InactiveUnits = knownBy)
                |> List.head
            let newInactiveUnits = Map.remove cardID lane.InactiveUnits
            let newActiveUnits = Map.add cardID Ready lane.ActiveUnits
            let newLane = {
                lane with
                    InactiveUnits = newInactiveUnits
                    ActiveUnits = newActiveUnits
            }
            let newLanes =
                lanes
                |> List.mapi (fun n l ->
                    if n = (int laneID - 1) then
                        newLane
                    else
                        l
                )
            PostBaseFlipBoard {pbfb with Lanes = newLanes}           
    {gameState with CardsState = {cardsState with Board = newBoard}}

let private getAttackInfo cardPowers units inactiveUnits activeUnits unitPairs playerID attackerInfo targetInfo =
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
            |> List.head
            |> List.singleton
        | DoubleAttacker (power, health1, health2) ->
            units
            |> Map.toList
            |> List.choose (fun (id, (owner, h)) ->
                if owner = playerID
                    && h = health1
                    && Map.find id cardPowers = power
                    && Map.containsKey id unitPairs
                then
                    let partnerID = Map.find id unitPairs
                    if Map.find partnerID units = (owner, health2) then
                        Some [id; partnerID]
                    else
                        None
                else
                    None
                )
            |> List.head
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
            |> List.filter (fun id -> Map.containsKey id inactiveUnits)
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
    match gameState.CardsState.Board with
    | PreBaseFlipBoard {Lanes = lanes; DrawPile = drawPile; Discard = discard} ->
        let lane = List.item (int laneID - 1) lanes
        let attackerIDs, targetID, damage =
            getAttackInfo gameState.CardsState.CardPowers lane.Units lane.InactiveUnits lane.ActiveUnits lane.UnitPairs playerID attackerInfo targetInfo
        let _, targetHealth = Map.find targetID lane.Units
        let newUnits, newInactiveUnits, newActiveUnits, newUnitPairs, newDiscard =
            if targetHealth <= damage then
                Map.remove targetID lane.Units,
                Map.remove targetID lane.InactiveUnits,
                Map.remove targetID lane.ActiveUnits,
                Map.filter (fun key value -> key <> targetID && value <> targetID) lane.UnitPairs,
                if Map.containsKey targetID lane.InactiveUnits then
                    let deadCard =
                        (Map.find targetID gameState.CardsState.CardPowers, Map.find targetID lane.InactiveUnits)
                        |> FaceDownDeadCard
                    CountMap.inc deadCard discard
                else
                    let deadCard =
                        Map.find targetID gameState.CardsState.CardPowers
                        |> FaceUpDeadCard
                    CountMap.inc deadCard discard
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
        let newBoard = PreBaseFlipBoard {
                Lanes =
                    lanes
                    |> List.mapi (fun n ln ->
                        if n = int laneID - 1 then
                            newLane
                        else
                            ln
                        )
                DrawPile = drawPile
                Discard = newDiscard
                }
        {gameState with CardsState = {gameState.CardsState with Board = newBoard}}
    | PostBaseFlipBoard {Lanes = lanes; Discard = discard} ->
        let lane = List.item (int laneID - 1) lanes
        let attackerIDs, targetID, damage =
            getAttackInfo gameState.CardsState.CardPowers lane.Units lane.InactiveUnits lane.ActiveUnits lane.UnitPairs playerID attackerInfo targetInfo
        let _, targetHealth = Map.find targetID lane.Units
        let newUnits, newInactiveUnits, newActiveUnits, newUnitPairs, newDiscard =
            if targetHealth <= damage then
                Map.remove targetID lane.Units,
                Map.remove targetID lane.InactiveUnits,
                Map.remove targetID lane.ActiveUnits,
                Map.filter (fun key value -> key <> targetID && value <> targetID) lane.UnitPairs,
                if Map.containsKey targetID lane.InactiveUnits then
                    let deadCard =
                        (Map.find targetID gameState.CardsState.CardPowers, Map.find targetID lane.InactiveUnits)
                        |> FaceDownDeadCard
                    CountMap.inc deadCard discard
                else
                    let deadCard =
                        Map.find targetID gameState.CardsState.CardPowers
                        |> FaceUpDeadCard
                    CountMap.inc deadCard discard
            else
                Map.change targetID (function | None -> None | Some (owner, health) -> Some (owner, health - damage)) lane.Units,
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
        let newBoard = PostBaseFlipBoard {
                Lanes =
                    lanes
                    |> List.mapi (fun n ln ->
                        if n = int laneID - 1 then
                            newLane
                        else
                            ln
                        )
                Discard = newDiscard
                }
        {gameState with CardsState = {gameState.CardsState with Board = newBoard}}

let private executeCreatePairAction playerID laneID power (health1, readiness1) (health2, readiness2) gameState =
    let newBoard =
        match gameState.CardsState.Board with
        | PreBaseFlipBoard pbfb ->
            let newLanes =
                pbfb.Lanes
                |> List.mapi (fun n lane ->
                    if (n + 1)*1<LID> = laneID then
                        let cardID1 =
                            lane.Units
                            |> Map.toList
                            |> List.choose (fun (id, (owner, h)) ->
                                if owner = playerID && h = health1 then
                                    Some id
                                else
                                    None
                                )
                            |> List.filter (fun id -> Map.find id gameState.CardsState.CardPowers = power)
                            |> List.filter (fun id -> Map.find id lane.ActiveUnits = readiness1)
                            |> List.head
                        let cardID2 =
                            lane.Units
                            |> Map.toList
                            |> List.choose (fun (id, (owner, h)) ->
                                if owner = playerID && h = health2 then
                                    Some id
                                else
                                    None
                                )
                            |> List.filter (fun id -> Map.find id gameState.CardsState.CardPowers = power)
                            |> List.filter (fun id -> Map.find id lane.ActiveUnits = readiness2)
                            |> List.head
                        {lane with UnitPairs = lane.UnitPairs |> Map.add cardID1 cardID2 |> Map.add cardID2 cardID1}
                    else
                        lane
                    )
            PreBaseFlipBoard {pbfb with Lanes = newLanes}
        | PostBaseFlipBoard pbfb ->
            let newLanes =
                pbfb.Lanes
                |> List.mapi (fun n lane ->
                    if (n + 1)*1<LID> = laneID then
                        let cardID1 =
                            lane.Units
                            |> Map.toList
                            |> List.choose (fun (id, (owner, h)) ->
                                if owner = playerID && h = health1 then
                                    Some id
                                else
                                    None
                                )
                            |> List.filter (fun id -> Map.find id gameState.CardsState.CardPowers = power)
                            |> List.filter (fun id -> Map.find id lane.ActiveUnits = readiness1)
                            |> List.head
                        let cardID2 =
                            lane.Units
                            |> Map.toList
                            |> List.choose (fun (id, (owner, h)) ->
                                if owner = playerID && h = health2 then
                                    Some id
                                else
                                    None
                                )
                            |> List.filter (fun id -> Map.find id gameState.CardsState.CardPowers = power)
                            |> List.filter (fun id -> Map.find id lane.ActiveUnits = readiness2)
                            |> List.head
                        {lane with UnitPairs = lane.UnitPairs |> Map.add cardID1 cardID2 |> Map.add cardID2 cardID1}
                    else
                        lane
                    )
            PostBaseFlipBoard {pbfb with Lanes = newLanes}
    {gameState with CardsState = {gameState.CardsState with Board = newBoard}}

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
    {newStateBeforeActionUpdate with
        TurnState = {
            newStateBeforeActionUpdate.TurnState with
                ActionsLeft = newStateBeforeActionUpdate.TurnState.ActionsLeft - 1
            }
        }

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

let private flipBasesOnLane cardPowers lane =
    let newUnits =
        lane.BasesTable
        |> Map.fold (fun table playerID (cardID, knownBy) -> Map.add cardID (playerID, 2<health>) table) lane.Units
    let newInactiveUnits =
        lane.BasesTable
        |> Map.fold (fun table playerID (cardID, knownBy) -> Map.add cardID knownBy table) lane.InactiveUnits
    {
        Units = newUnits
        ActiveUnits = lane.ActiveUnits
        InactiveUnits = newInactiveUnits
        UnitPairs = lane.UnitPairs
        }

let private flipBasesOnBoard lanes cardPowers discard =
    PostBaseFlipBoard {
        Lanes = List.map (flipBasesOnLane cardPowers) lanes
        Discard = discard
        }

let private tryDrawCard playerID (gameState: GameStateDuringTurn) =
    let cardsState = gameState.CardsState
    match cardsState.Board with
    | PreBaseFlipBoard pbfb ->
        let hands = cardsState.HandCardOwners
        let drawPile = pbfb.DrawPile
        match drawPile.Rest with
        | [] ->
            let newHands =
                hands
                |> Map.add drawPile.TopCard playerID
            let newCards = {
                cardsState with
                    HandCardOwners = newHands
                    Board = flipBasesOnBoard pbfb.Lanes gameState.CardsState.CardPowers pbfb.Discard
                }
            {gameState with CardsState = newCards}
        | newTopCard :: newRest ->
            let newHands =
                hands
                |> Map.add drawPile.TopCard playerID
            let newCards =
                {cardsState with
                    HandCardOwners = newHands
                    Board = PreBaseFlipBoard {
                        pbfb with
                            DrawPile = {TopCard = newTopCard; Rest = newRest}
                        }
                }
            {gameState with CardsState = newCards}
    | PostBaseFlipBoard _ ->
        gameState

let private readyActiveUnits activeUnits =
    activeUnits
    |> Map.map (fun _ _ -> Ready)

let private readyAllActiveCards cardsState =
    let board = cardsState.Board
    let newBoard =
        match board with
        | PreBaseFlipBoard pbfb ->
            let newLanes =
                pbfb.Lanes
                |> List.map (fun lane ->
                        {lane with ActiveUnits = readyActiveUnits lane.ActiveUnits}
                    )
            PreBaseFlipBoard {pbfb with Lanes = newLanes}
        | PostBaseFlipBoard pbfb ->
            let newLanes =
                pbfb.Lanes
                |> List.map (fun lane ->
                        {lane with ActiveUnits = readyActiveUnits lane.ActiveUnits}
                    )
            PostBaseFlipBoard {pbfb with Lanes = newLanes}
    {cardsState with Board = newBoard}

let private checkForGameEnd gameState =
    match gameState with
    | GameStateDuringTurn {CardsState = cs} ->
        match cs.Board with
        | PreBaseFlipBoard _ ->
            gameState
        | PostBaseFlipBoard {Lanes = lanes} ->
            let wonLaneCounts =
                lanes
                |> List.choose (fun {Units = units} ->
                    laneControl units
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
                    let contestedLanes =
                        lanes
                        |> List.filter (fun {Units = units} ->
                            match laneControl units, Map.isEmpty units with
                            | None, false ->
                                true
                            | None, true
                            | Some _, false // shouldn't happen?
                            | Some _, true ->
                                false
                            )
                    if List.isEmpty contestedLanes && Map.isEmpty cs.HandCardOwners then
                        GameStateTied {Lanes = lanes}
                    else
                        gameState
    | GameStateBetweenTurns _
    | GameStateWon _
    | GameStateTied _ ->
        gameState

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
    let lanes, notBaseCards =
        cardIndices
        |> prepareHead (prepareLanes nLanes) (nPlayers*nLanes)
    let hands, notDeckCards =
        notBaseCards
        |> prepareHead (prepareHands nPlayers) (5*nPlayers)
    let removed, notRemoved =
        notDeckCards
        |> prepareHead prepareRemoved 10
    let drawPile = prepareDrawPile notRemoved
    let gameState = GameStateBetweenTurns {
        CardsState = {
            Board = PreBaseFlipBoard {
                Lanes = lanes
                DrawPile = drawPile
                Discard = Map.empty
                }
            HandCardOwners = hands
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
