module Implementation
open Domain

type private DrawCard = DrawCard of Power
type private Base = PlayerID * Power * KnownBy
type private InactiveCard = PlayerID * Power * Health * KnownBy
type private ActiveCard = PlayerID * Power * Health * Readiness
type private FaceDownDeadCard = Power * KnownBy
type private FaceUpDeadCard = Power
type private DeadCard =
| FaceDownDeadCard of FaceDownDeadCard
| FaceUpDeadCard of FaceUpDeadCard
type private RemovedCard = RemovedCard of Power

type private Pair = PlayerID * Power * (Health * Readiness) * (Health * Readiness)

type private Troop =
| InactiveCard of InactiveCard
| ActiveCard of ActiveCard
| Pair of Pair

type private DrawPile = {
    TopCard: DrawCard
    Rest: DrawCard list
    }

type private PreBaseFlipLane = {
    Bases: Base list
    Troops: CountMap.CountMap<Troop>
}

type private ContestedLane = {
    Troops: CountMap.CountMap<Troop>
}

type private WonLane = {
    Controller: PlayerID
    Troops: CountMap.CountMap<Troop>
}

type private PostBaseFlipLane =
| ContestedLane of ContestedLane
| WonLane of WonLane
| TiedLane

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

type private CardsState = {
    Board: Board
    Hands: (PlayerID * Hand) list
    Removed: CountMap.CountMap<RemovedCard>
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

type private GameStateGameOver = {
    Lanes: PostBaseFlipLane list
    Winner: PlayerID
}

type private GameState =
| GameStateBetweenTurns of GameStateBetweenTurns
| GameStateDuringTurn of GameStateDuringTurn
| GameStateGameOver of GameStateGameOver

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

let private createUnshuffledDeck () =
    [
        View
        Trap
        Foresight
        Flip
        Freeze
        Heal
        Retaliate
        Nimble
        TwinStrike
        Taunt
        Vampiric
        Move
        Empower
        Action
    ]
    |> List.filter (fun power -> power <> Vampiric)
    |> List.collect (List.replicate 4)

let private prepareHead fn n lst =
    let h, t = List.splitAt n lst
    fn h, t

let private createLane bases = {
    Bases = bases
    Troops = Map.empty
} 

let private prepareLanes nLanes lst =
    lst
    |> List.splitInto nLanes
    |> List.map (fun lst ->
        lst
        |> List.mapi (fun playerIndex power ->
            Base ((playerIndex + 1)*1<PID>, power, [])
            )
        )
    |> List.map createLane

let private prepareHands nPlayers lst =
    lst
    |> List.splitInto nPlayers
    |> List.mapi (fun playerIndex lst ->
        (playerIndex + 1)*1<PID>,
        lst
        |> List.map (fun power -> HandCard power)
        |> CountMap.ofList
        )

let private prepareRemoved lst =
    lst
    |> List.map (fun power -> RemovedCard power)
    |> CountMap.ofList

let private prepareDrawPile lst =
    let drawCards =
        lst
        |> List.map (fun power -> DrawCard power)
    {TopCard = List.head drawCards; Rest = List.tail drawCards}

let private getBaseKnowledge (playerID: PlayerID) (baseCard: Base) =
    let (ownerID, power, knownBy) = baseCard
    if List.contains playerID knownBy then
        KnownBaseCard (ownerID, power)
    else
        UnknownBaseCard ownerID

let private getTroopKnowledge (playerID: PlayerID) troop =
    match troop with
    | InactiveCard (ownerID, power, health, knownBy) ->
        if List.contains playerID knownBy then
            KnownInactiveCardKnowledge (ownerID, power, health, List.filter (fun id -> id <> playerID) knownBy)
        else
            UnknownInactiveCardKnowledge (ownerID, health, knownBy)
    | ActiveCard (ownerID, power, health, readiness) ->
        ActiveCardKnowledge (ownerID, power, health, readiness)
    | Pair (ownerID, power, (health1, readiness1), (health2, readiness2)) ->
        let readiness =
            if readiness1 = Exhausted || readiness2 = Exhausted then
                Exhausted
            else
                Ready
        PairKnowledge (ownerID, power, health1, health2, readiness)

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
            gs.CardsState.Hands
            |> List.partition (fun (n, _) -> n = id)
        let playerHand =
            playerHandInfo
            |> List.head
            |> (fun (_, hand) -> hand)
        let getBase = getBaseKnowledge id
        let getTroop = getTroopKnowledge id
        let getDeadCard = getDeadCardKnowledge id
        let boardKnowledge =
            match gs.CardsState.Board with
            | PreBaseFlipBoard {Lanes = l; DrawPile = dp; Discard = d} ->
                let lanesKnowledge =
                    l
                    |> List.map (fun {Bases = bases; Troops = troops} ->
                            {
                                Bases = List.map getBase bases
                                Troops =
                                    CountMap.map getTroop troops
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
                    |> List.map (function
                        | ContestedLane {Troops = troops} ->
                            ContestedLaneKnowledge {
                                Troops =
                                    CountMap.map getTroop troops
                                }
                        | WonLane {Controller = c; Troops = troops} ->
                            WonLaneKnowledge {
                                Controller = c
                                Troops =
                                    CountMap.map getTroop troops
                                }
                        | TiedLane ->
                            TiedLaneKnowledge
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
            PlayerHand = playerHand
            OpponentHandSizes =
                opponentHandsInfo
                |> List.map (fun (n, hand) -> n, CountMap.count hand)
        }
    | GameStateBetweenTurns {TurnState = ts} ->
        SwitchDisplayInfo ts.Player
    | GameStateGameOver {Lanes = lanes; Winner = winner} ->
        let laneWins =
            lanes
            |> List.indexed
            |> List.choose (fun (n, lane) ->
                match lane with
                | WonLane {Controller = id} ->
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
        FinishedGameDisplayInfo {
            Winner = winner
            LaneWins = laneWins
        }

let private getPlayActionsInfo (turnDisplayInfo: TurnDisplayInfo) =
    let validPlayLanes =
        match turnDisplayInfo.BoardKnowledge with
        | PreBaseFlipBoardKnowledge {Lanes = l} ->
            [1..List.length l]
        | PostBaseFlipBoardKnowledge {Lanes = l} ->
            l
            |> List.indexed
            |> List.choose (fun (n, lane) ->
                match lane with
                | ContestedLaneKnowledge _ ->
                    Some (n + 1)
                | WonLaneKnowledge {Controller = c} ->
                    if c = turnDisplayInfo.CurrentPlayer then
                        Some (n + 1)
                    else
                        None
                | TiedLaneKnowledge ->
                    None
                )
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
            | WonLaneKnowledge _
            | TiedLaneKnowledge ->
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
            | WonLaneKnowledge _
            | TiedLaneKnowledge _ ->
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
            | WonLaneKnowledge _
            | TiedLaneKnowledge ->
                []
            )

let private getPossibleActionsInfo (displayInfo: DisplayInfo) =
    match displayInfo with
    | TurnDisplayInfo turnDisplayInfo ->
        if turnDisplayInfo.ActionsLeft = 0 then
            EndTurn turnDisplayInfo.CurrentPlayer |> List.singleton
        else
            getPlayActionsInfo turnDisplayInfo
            @ (getActivateActionsInfo turnDisplayInfo)
            @ (getAttackActionsInfo turnDisplayInfo)
            @ (getPairActionsInfo turnDisplayInfo)
    | SwitchDisplayInfo playerID ->
        StartTurn playerID |> List.singleton
    | FinishedGameDisplayInfo _ ->
        List.empty

let private incInPreLane card playerID (lane: PreBaseFlipLane) =
    let moveTo = CountMap.inc card
    {lane with Troops = moveTo lane.Troops}

let private incInPostLane card playerID (lane: PostBaseFlipLane) =
    let moveTo = CountMap.inc card
    match lane with
    | ContestedLane cl ->
        ContestedLane {
            cl with
                Troops = moveTo cl.Troops
            }
    | WonLane wl when wl.Controller = playerID ->
        WonLane {
            wl with
                Troops = moveTo wl.Troops
            }
    | WonLane _ ->
        failwithf "can't play cards in a lost lane"
    | TiedLane ->
        failwithf "can't play cards in a tied lane"                                

let private decInPreLane card playerID (lane: PreBaseFlipLane) =
    let dec = CountMap.dec card
    {lane with Troops = dec lane.Troops}

let private decInPostLane card playerID (lane: PostBaseFlipLane) =
    let dec = CountMap.dec card
    match lane with
    | ContestedLane cl ->
        ContestedLane {
            cl with
                Troops = dec cl.Troops
            }
    | WonLane wl when wl.Controller = playerID ->
        WonLane {
            wl with
                Troops = dec wl.Troops
            }
    | WonLane _ ->
        failwithf "can't play cards in a lost lane"
    | TiedLane ->
        failwithf "can't play cards in a tied lane"                                

let private changeInPreLane before after (lane: PreBaseFlipLane) =
    let f = (CountMap.dec before) >> (CountMap.inc after)
    {lane with Troops = f lane.Troops}

let private changeTroop before after troops =
    troops
    |> CountMap.dec before
    |> CountMap.inc after

let private getAttackTroops troops playerID attackerInfo targetInfo =
    let attacker, attackerAfter, attackDamage =
        match attackerInfo with
        | SingleAttacker (power, health) ->
            ActiveCard (playerID, power, health, Ready),
            ActiveCard (playerID, power, health, Exhausted),
            1<health>
        | DoubleAttacker (power, health1, health2) ->
            Pair (playerID, power, (health1, Ready), (health2, Ready)),
            Pair (playerID, power, (health1, Exhausted), (health2, Exhausted)),
            2<health>
    let target, targetAfter, deadCard =
        match targetInfo with
        | UnknownInactiveTarget (ownerID, health) ->
            troops
            |> CountMap.keyList
            |> List.choose (function
                | InactiveCard (pid, p, h, kb) when
                    pid = ownerID
                    && h = health
                    && not(List.contains playerID kb) ->
                    Some (
                        if h <= attackDamage then
                            InactiveCard (pid, p, h, kb),
                            None,
                            Some (FaceDownDeadCard (p, kb))
                        else
                            InactiveCard (pid, p, h, kb),
                            Some (InactiveCard (pid, p, h - attackDamage, kb)),
                            None
                    )
                | InactiveCard _
                | ActiveCard _
                | Pair _ -> None
                )
            |> List.head
        | KnownInactiveTarget (ownerID, power, health) ->
            troops
            |> CountMap.keyList
            |> List.choose (function
                | InactiveCard (pid, p, h, kb) when
                    pid = ownerID
                    && p = power
                    && h = health
                    && List.contains playerID kb ->
                    Some (
                        if h <= attackDamage then
                            InactiveCard (pid, p, h, kb),
                            None,
                            Some (FaceDownDeadCard (p, kb))
                        else
                            InactiveCard (pid, p, h, kb),
                            Some (InactiveCard (pid, p, h - attackDamage, kb)),
                            None
                    )
                | InactiveCard _
                | ActiveCard _
                | Pair _ -> None
                )
            |> List.head
        | ActiveSingleTarget (ownerID, power, health) ->
            troops
            |> CountMap.keyList
            |> List.choose (function
                | ActiveCard (pid, p, h, r) when
                    pid = ownerID
                    && p = power
                    && h = health ->
                    Some (
                        if h <= attackDamage then
                            ActiveCard (pid, p, h, r),
                            None,
                            Some (FaceUpDeadCard p)
                        else
                            ActiveCard (pid, p, h, r),
                            Some (ActiveCard (pid, p, h - attackDamage, r)),
                            None
                    )
                | ActiveCard _
                | InactiveCard _
                | Pair _ -> None
                )
            |> List.head
        | ActivePairMemberTarget (ownerID, power, health, partnerHealth) ->
            troops
            |> CountMap.keyList
            |> List.choose (function
                | Pair (pid, p, (h1, r1), (h2, r2)) when
                    pid = ownerID
                    && p = power
                    && (
                        (h1 = health && h2 = partnerHealth)
                        || (h2 = health && h1 = partnerHealth)
                    ) ->
                    Some (
                        if health = attackDamage then
                            if h1 = health then
                                Pair (pid, p, (h1, r1), (h2, r2)),
                                Some (ActiveCard (pid, p, h2, r2)),
                                Some (FaceUpDeadCard p)
                            else
                                Pair (pid, p, (h1, r1), (h2, r2)),
                                Some (ActiveCard (pid, p, h1, r1)),
                                Some (FaceUpDeadCard p)
                        else
                            if h1 = health then
                                Pair (pid, p, (h1, r1), (h2, r2)),
                                Some (Pair (pid, p, (h1 - attackDamage, r1), (h2, r2))),
                                None
                            else
                                Pair (pid, p, (h1, r1), (h2, r2)),
                                Some (Pair (pid, p, (h1, r1), (h2 - attackDamage, r2))),
                                None
                        )
                | Pair _
                | ActiveCard _
                | InactiveCard _ -> None
                )
            |> List.head
    attacker, attackerAfter, target, targetAfter, deadCard

let private checkContestedLaneForWin troops =
    let playerCounts =
        troops
        |> CountMap.keyList
        |> List.countBy (function
            | InactiveCard (playerID, _, _, _)
            | ActiveCard (playerID, _, _, _)
            | Pair (playerID, _, _, _) ->
                playerID
            )
    match playerCounts with
    | [] -> TiedLane
    | [(controller, _)] -> WonLane {Controller = controller; Troops = troops}
    | _ -> ContestedLane {Troops = troops}

let private executeTurnAction (action: TurnActionInfo) (gameState: GameStateDuringTurn) =
    let newStateBeforeActionUpdate =
        match action with
        | Play (playerID, power, laneID) ->
            let oldCard = HandCard power
            let newCard = InactiveCard (playerID, power, 2<health>, List.singleton playerID)
            let moveFrom = CountMap.dec oldCard
            let moveTo = CountMap.inc newCard
            let newCards = {
                gameState.CardsState with
                    Hands =
                        gameState.CardsState.Hands
                        |> List.map (fun (ownerID, cards) ->
                            ownerID,
                            if ownerID = playerID then
                                moveFrom cards
                            else
                                cards
                            )
                    Board =
                        match gameState.CardsState.Board with
                        | PreBaseFlipBoard pbfb ->
                            let l = pbfb.Lanes
                            let newLanes =
                                l
                                |> List.mapi (fun n lane ->
                                    if (n + 1)*1<LID> = laneID then
                                        incInPreLane newCard playerID lane
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
                                        incInPostLane newCard playerID lane
                                    else
                                        lane
                                    )
                            PostBaseFlipBoard {pbfb with Lanes = newLanes}
                }
            {gameState with CardsState = newCards}
        | Activate (playerID, laneID, activationTarget) ->
            let troops =
                match gameState.CardsState.Board with
                | PreBaseFlipBoard {Lanes = lanes} ->
                    (List.item (int laneID - 1) lanes).Troops
                | PostBaseFlipBoard {Lanes = lanes} ->
                    let lane = List.item (int laneID - 1) lanes
                    match lane with
                    | ContestedLane {Troops = troops} ->
                        troops
                    | WonLane {Controller = c; Troops = troops} when playerID = c ->
                        troops
                    | WonLane _
                    | TiedLane ->
                        failwithf "Can't activate troops in a lost or tied lane"
            let oldCard, newCard =
                match activationTarget with
                | UnknownActivationTarget (health, knownBy) ->
                    let firstValidPower =
                        troops
                        |> CountMap.keyList
                        |> List.choose (fun troop ->
                            match troop with
                            | InactiveCard (pid, p, h, kb)
                                when h = health && pid = playerID && kb = knownBy ->
                                Some p
                            | InactiveCard _
                            | ActiveCard _
                            | Pair _ ->
                                None
                            )
                        |> List.tryHead
                    match firstValidPower with
                    | Some p ->
                        InactiveCard (playerID, p, health, knownBy),
                        ActiveCard (playerID, p, health, Ready)
                    | None ->
                        let existingHealthValues =
                            troops
                            |> CountMap.keyList
                            |> List.choose (function
                                | InactiveCard (pid, p, h, kb) ->
                                    if pid = playerID && kb = knownBy then
                                        Some h
                                    else
                                        None
                                | _ ->
                                    None
                                )
                        failwithf "could not find unknown card with %i health. existing values: %A" health existingHealthValues
                | KnownActivationTarget (power, health, knownBy) ->
                    let fullKnownBy = List.sort (playerID :: knownBy)
                    InactiveCard (playerID, power, health, fullKnownBy),
                    ActiveCard (playerID, power, health, Ready)
            let newTroops =
                troops
                |> changeTroop oldCard newCard
            let newBoard =
                match gameState.CardsState.Board with
                | PreBaseFlipBoard pbfb ->
                    let newLanes =
                        pbfb.Lanes
                        |> List.mapi (fun n lane ->
                            if (n + 1)*1<LID> = laneID then
                                {lane with Troops = newTroops}
                            else
                                lane
                        )
                    PreBaseFlipBoard {pbfb with Lanes = newLanes}
                | PostBaseFlipBoard pbfb ->
                    let newLanes =
                        pbfb.Lanes
                        |> List.mapi (fun n lane ->
                            if (n + 1)*1<LID> = laneID then
                                match lane with
                                | ContestedLane _ ->
                                    ContestedLane {Troops = newTroops}
                                | WonLane {Controller = c} ->
                                    WonLane {
                                        Controller = c
                                        Troops = newTroops
                                        }
                                | TiedLane ->
                                    failwithf "Can't change troops in a tied lane"
                            else
                                lane
                        )
                    PostBaseFlipBoard {pbfb with Lanes = newLanes}
            {gameState with CardsState = {gameState.CardsState with Board = newBoard}}
        | Attack (playerID, laneID, attackerInfo, targetInfo) ->
            match gameState.CardsState.Board with
            | PreBaseFlipBoard pbfb ->
                let lanes = pbfb.Lanes
                let discard = pbfb.Discard
                let lane = List.item (int laneID - 1) lanes
                let troops = lane.Troops
                let attacker, attackerAfter, target, targetAfter, deadCard =
                    getAttackTroops troops playerID attackerInfo targetInfo
                let newTroops =
                    match targetAfter with
                    | Some ta ->
                        troops
                        |> CountMap.dec attacker
                        |> CountMap.inc attackerAfter
                        |> CountMap.dec target
                        |> CountMap.inc ta
                    | None ->
                        troops
                        |> CountMap.dec attacker
                        |> CountMap.inc attackerAfter
                        |> CountMap.dec target
                let newLane = {lane with Troops = newTroops}
                let newDiscard =
                    match deadCard with
                    | Some dc ->
                        discard
                        |> CountMap.inc dc
                    | None -> discard
                let newBoard = PreBaseFlipBoard {
                    pbfb with
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
            | PostBaseFlipBoard {Lanes = lanes; Discard = discard} ->
                let lane = List.item (int laneID - 1) lanes
                match lane with
                    | WonLane _
                    | TiedLane ->
                        failwithf "can't resolve attack in completed lane"
                    | ContestedLane cl ->
                        let troops = cl.Troops
                        let attacker, attackerAfter, target, targetAfter, deadCard =
                            getAttackTroops troops playerID attackerInfo targetInfo
                        let newTroops =
                            match targetAfter with
                            | Some ta ->
                                troops
                                |> CountMap.dec attacker
                                |> CountMap.inc attackerAfter
                                |> CountMap.dec target
                                |> CountMap.inc ta
                            | None ->
                                troops
                                |> CountMap.dec attacker
                                |> CountMap.inc attackerAfter
                                |> CountMap.dec target
                        let newLane = checkContestedLaneForWin newTroops
                        let newDiscard =
                            match deadCard with
                            | Some dc ->
                                discard
                                |> CountMap.inc dc
                            | None -> discard
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
        | CreatePair (playerID, laneID, power, (health1, readiness1), (health2, readiness2)) ->
            let single1 = ActiveCard (playerID, power, health1, readiness1)
            let single2 = ActiveCard (playerID, power, health2, readiness2)
            let pair = Pair (playerID, power, (health1, readiness1), (health2, readiness2))
            let newBoard =
                match gameState.CardsState.Board with
                | PreBaseFlipBoard pbfb ->
                    let newLanes =
                        pbfb.Lanes
                        |> List.mapi (fun n lane ->
                            if (n + 1)*1<LID> = laneID then
                                lane
                                |> decInPreLane single1 playerID
                                |> decInPreLane single2 playerID
                                |> incInPreLane pair playerID
                            else
                                lane
                            )
                    PreBaseFlipBoard {pbfb with Lanes = newLanes}
                | PostBaseFlipBoard pbfb ->
                    let newLanes =
                        pbfb.Lanes
                        |> List.mapi (fun n lane ->
                            if (n + 1)*1<LID> = laneID then
                                lane
                                |> decInPostLane single1 playerID
                                |> decInPostLane single2 playerID
                                |> incInPostLane pair playerID
                            else
                                lane
                            )
                    PostBaseFlipBoard {pbfb with Lanes = newLanes}
            {gameState with CardsState = {gameState.CardsState with Board = newBoard}}
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

let private flipBasesOnLane lane =
    let baseTroops =
        lane.Bases
        |> List.map (fun (pid, p, kb) -> InactiveCard (pid, p, 2<health>, kb))
    let newTroops =
        baseTroops
        |> List.fold (fun cm troop -> CountMap.inc troop cm) lane.Troops
    ContestedLane {Troops = newTroops}

let private flipBasesOnBoard lanes discard =
    PostBaseFlipBoard {
        Lanes = List.map flipBasesOnLane lanes
        Discard = discard
        }

let private tryDrawCard playerID (gameState: GameStateDuringTurn) =
    let cardsState = gameState.CardsState
    match cardsState.Board with
    | PreBaseFlipBoard pbfb ->
        let hands = cardsState.Hands
        let drawPile = pbfb.DrawPile
        match drawPile.Rest with
        | [] ->
            let newHandCard =
                match drawPile.TopCard with
                | DrawCard power -> HandCard power
            let newHands =
                cardsState.Hands
                |> List.map (fun (pid, h) ->
                    if pid = playerID then
                        pid, CountMap.inc newHandCard h
                    else
                        pid, h
                    )
            let newCards = {
                cardsState with
                    Hands = newHands
                    Board = flipBasesOnBoard pbfb.Lanes pbfb.Discard
                }
            {gameState with CardsState = newCards}
        | h :: t ->
            let newTopCard, newRest = h, t
            let newHandCard =
                match drawPile.TopCard with
                | DrawCard power -> HandCard power
            let newHands =
                hands
                |> List.map (fun (pid, h) ->
                    if pid = playerID then
                        pid, CountMap.inc newHandCard h
                    else
                        pid, h
                    )
            let newCards =
                {cardsState with
                    Hands = newHands
                    Board = PreBaseFlipBoard {
                        pbfb with
                            DrawPile = {TopCard = newTopCard; Rest = newRest}
                        }
                }
            {gameState with CardsState = newCards}
    | PostBaseFlipBoard _ ->
        gameState

let private readyTroop troop =
    match troop with
    | InactiveCard _ ->
        troop
    | ActiveCard (pid, p, h, _) ->
        ActiveCard (pid, p, h, Ready)
    | Pair (pid, p, (h1, _), (h2, _)) ->
        Pair (pid, p, (h1, Ready), (h2, Ready))

let private readyTroops troops =
    troops
    |> CountMap.toList
    |> List.map readyTroop
    |> CountMap.ofList

let private readyAllActiveCards cardsState =
    let board = cardsState.Board
    let newBoard =
        match board with
        | PreBaseFlipBoard pbfb ->
            let newLanes =
                pbfb.Lanes
                |> List.map (fun pbfl -> {
                    pbfl with Troops = readyTroops pbfl.Troops
                    }
                    )
            PreBaseFlipBoard {pbfb with Lanes = newLanes}
        | PostBaseFlipBoard pbfb ->
            let newLanes =
                pbfb.Lanes
                |> List.map (function
                    | ContestedLane {Troops = troops} ->
                        ContestedLane {Troops = readyTroops troops}
                    | WonLane {Controller = c; Troops = troops} ->
                        WonLane {Controller = c; Troops = readyTroops troops}
                    | TiedLane ->
                        TiedLane
                    )
            PostBaseFlipBoard {pbfb with Lanes = newLanes}
    {cardsState with Board = newBoard}

let private checkForGameWin gameState =
    match gameState with
    | GameStateDuringTurn {CardsState = cs} ->
        match cs.Board with
        | PreBaseFlipBoard _ ->
            gameState
        | PostBaseFlipBoard {Lanes = lanes} ->
            let wonLaneCounts =
                lanes
                |> List.choose (function
                    | WonLane {Controller = c} ->
                        Some c
                    | ContestedLane _
                    | TiedLane ->
                        None
                    )
                |> List.countBy id
            match wonLaneCounts with
            | [] -> gameState
            | lst ->
                let (leadingPlayer, leadingWins) =
                    lst
                    |> List.maxBy (fun (_, n) -> n)
                if leadingWins >= 2 then
                    GameStateGameOver {Winner = leadingPlayer; Lanes = lanes}
                else
                    gameState
    | GameStateBetweenTurns _
    | GameStateGameOver _ ->
        gameState

let rec private makeNextActionInfo gameState action =
    let newGameState =
        match gameState, action with
        | GameStateDuringTurn gs, TurnActionInfo tai ->
            executeTurnAction tai gs
            |> GameStateDuringTurn
            |> checkForGameWin
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
    let shuffledDeck =
        createUnshuffledDeck()
        |> shuffle
    let lanes, notBaseCards =
        shuffledDeck
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
            Hands = hands
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
