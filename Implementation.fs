module Implementation
open Domain

type DrawCard = DrawCard of Power
type Base = Power * PlayerID * KnownBy
type InactiveCard = Power * Health * PlayerID * KnownBy
type ActiveCard = Power * Health * Readiness * PlayerID
type FaceDownDeadCard = Power * KnownBy
type FaceUpDeadCard = Power
type DeadCard =
| FaceDownDeadCard of FaceDownDeadCard
| FaceUpDeadCard of FaceUpDeadCard
type RemovedCard = RemovedCard of Power

type Pair = Power * (Health * Readiness) * (Health * Readiness) * PlayerID

type Troop =
| InactiveCard of InactiveCard
| ActiveCard of ActiveCard
| Pair of Pair

type DrawPile = CountMap.CountMap<DrawCard>

type PreBaseFlipLane = {
    Bases: Base list
    Troops: CountMap.CountMap<Troop>
}

type ContestedLane = {
    Troops: CountMap.CountMap<Troop>
}

type WonLane = {
    Controller: PlayerID
    Troops: CountMap.CountMap<Troop>
}

type Lane =
| PreBaseFlipLane of PreBaseFlipLane
| ContestedLane of ContestedLane
| WonLane of WonLane
| TiedLane

type private GameState = {
    Board: Lane list
    DrawPile: DrawPile
    CurrentPlayer: PlayerID option
    NextPlayer: PlayerID
    ActionsLeft: int
    Hands: (PlayerID * Hand) list
    Discard: CountMap.CountMap<DeadCard>
    Removed: CountMap.CountMap<RemovedCard>
}

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

let private createLane bases = PreBaseFlipLane {
    Bases = bases
    Troops = Map.empty
} 

let private prepareBoard nLanes lst =
    lst
    |> List.splitInto nLanes
    |> List.map (fun lst ->
        lst
        |> List.mapi (fun playerIndex power ->
            Base (power, (playerIndex + 1)*1<PID>, [])
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
    lst
    |> List.map (fun power -> DrawCard power)
    |> CountMap.ofList

let private getBaseKnowledge (playerID: PlayerID) (baseCard: Base) =
    let (power, ownerID, knownBy) = baseCard
    if List.contains playerID knownBy then
        KnownBaseCard (ownerID, power)
    else
        UnknownBaseCard ownerID

let private getTroopKnowledge (playerID: PlayerID) troop =
    match troop with
    | InactiveCard (power, health, ownerID, knownBy) ->
        if List.contains playerID knownBy then
            KnownInactiveCardKnowledge (ownerID, power, health, List.filter (fun id -> id <> playerID) knownBy)
        else
            UnknownInactiveCardKnowledge (ownerID, health, List.filter (fun id -> id <> playerID) knownBy)
    | ActiveCard (power, health, readiness, ownerID) ->
        ActiveCardKnowledge (ownerID, power, health, readiness)
    | Pair (power, (health1, readiness1), (health2, readiness2), ownerID) ->
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
    match gameState.CurrentPlayer with
    | Some id ->
        let (playerHandInfo, opponentHandsInfo) =
            gameState.Hands
            |> List.partition (fun (n, _) -> n = id)
        let playerHand =
            playerHandInfo
            |> List.head
            |> (fun (_, hand) -> hand)
        let getBase = getBaseKnowledge id
        let getTroop = getTroopKnowledge id
        let getDeadCard = getDeadCardKnowledge id
        TurnDisplayInfo {
            CurrentPlayer = id
            ActionsLeft = gameState.ActionsLeft
            BoardKnowledge =
                gameState.Board
                |> List.map (function
                    | PreBaseFlipLane {Bases = bases; Troops = troops} ->
                        PreBaseFlipLaneKnowledge {
                            Bases = List.map getBase bases
                            Troops =
                                CountMap.map getTroop troops
                            }
                    | ContestedLane {Troops = troops} ->
                        ContestedLaneKnowledge {
                            Troops =
                                CountMap.map getTroop troops
                            }
                    | WonLane {Controller = controller; Troops = troops} ->
                        WonLaneKnowledge {
                            Controller = controller
                            Troops = CountMap.map getTroop troops
                            }
                    | TiedLane ->
                        TiedLaneKnowledge
                    )
            PlayerHand = playerHand
            OpponentHandSizes =
                opponentHandsInfo
                |> List.map (fun (n, hand) -> n, CountMap.count hand)
            DrawPileSize = CountMap.count gameState.DrawPile
            DiscardKnowledge = CountMap.map getDeadCard gameState.Discard
        }
    | None ->
        SwitchDisplayInfo gameState.NextPlayer

let private getPlayActionsInfo (turnDisplayInfo: TurnDisplayInfo) =
    turnDisplayInfo.PlayerHand
    |> CountMap.toList
    |> List.collect (fun (HandCard power) ->
        [1..(List.length turnDisplayInfo.BoardKnowledge)]
        |> List.map (fun laneID ->
            Play (turnDisplayInfo.CurrentPlayer, power, laneID*1<LID>)
            |> TurnActionInfo
            )
        )
    |> List.distinct

let private getActivateActionsInfo (turnDisplayInfo: TurnDisplayInfo) =
    let playerID = turnDisplayInfo.CurrentPlayer
    turnDisplayInfo.BoardKnowledge
    |> List.indexed
    |> List.collect (fun (n, lane) ->
        match lane with
        | PreBaseFlipLaneKnowledge {Troops = troops}
        | ContestedLaneKnowledge {Troops = troops} ->
            troops
            |> CountMap.choose (fun troop ->
                match troop with
                | KnownInactiveCardKnowledge (ownerID, power, health, knownBy) when
                    ownerID = playerID ->
                    let fullKnownBy = List.sortBy id (ownerID :: knownBy)
                    Activate (playerID, (n + 1)*1<LID>, (power, health, fullKnownBy))
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
                | KnownInactiveCardKnowledge (ownerID, power, health, knownBy) when
                    ownerID = playerID ->
                    let fullKnownBy = List.sortBy id (ownerID :: knownBy)
                    Activate (playerID, (n + 1)*1<LID>, (power, health, fullKnownBy))
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
    turnDisplayInfo.BoardKnowledge
    |> List.indexed
    |> List.collect (fun (n, lane) ->
        let laneID = (n + 1)*1<LID>
        match lane with
        | PreBaseFlipLaneKnowledge {Troops = troops}
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
    turnDisplayInfo.BoardKnowledge
    |> List.indexed
    |> List.collect (fun (n, lane) ->
        let laneID = (n + 1)*1<LID>
        match lane with
        | PreBaseFlipLaneKnowledge {Troops = troops}
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

let private incInLane card playerID lane =
    let moveTo = CountMap.inc card
    match lane with
    | PreBaseFlipLane pbfl ->
        PreBaseFlipLane {
            pbfl with
                Troops = moveTo pbfl.Troops
            }
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
    | TiedLane _ ->
        failwithf "can't play cards in a tied lane"                                

let private decInLane card playerID lane =
    let dec = CountMap.dec card
    match lane with
    | PreBaseFlipLane pbfl ->
        PreBaseFlipLane {
            pbfl with
                Troops = dec pbfl.Troops
            }
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
    | TiedLane _ ->
        failwithf "can't play cards in a tied lane"                                

let private changeInLane before after lane =
    let f = (CountMap.dec before) >> (CountMap.inc after)
    match lane with
    | PreBaseFlipLane pbfl ->
        PreBaseFlipLane {pbfl with Troops = f pbfl.Troops}
    | ContestedLane cl ->
        ContestedLane {cl with Troops = f cl.Troops}
    | WonLane wl ->
        WonLane {wl with Troops = f wl.Troops}
    | TiedLane ->
        failwithf "Can't flip cards in a tied lane"

let private getAttackTroops troops playerID attackerInfo targetInfo =
    let attacker, attackerAfter, attackDamage =
        match attackerInfo with
        | SingleAttacker (power, health) ->
            ActiveCard (power, health, Ready, playerID),
            ActiveCard (power, health, Exhausted, playerID),
            1<health>
        | DoubleAttacker (power, health1, health2) ->
            Pair (power, (health1, Ready), (health2, Ready), playerID),
            Pair (power, (health1, Exhausted), (health2, Exhausted), playerID),
            2<health>
    let target, targetAfter, deadCard =
        match targetInfo with
        | UnknownInactiveTarget (ownerID, health) ->
            troops
            |> CountMap.keyList
            |> List.choose (function
                | InactiveCard (p, h, pid, kb) when
                    pid = ownerID
                    && h = health
                    && not(List.contains playerID kb) ->
                    Some (
                        if h <= attackDamage then
                            InactiveCard (p, h, pid, kb),
                            None,
                            Some (FaceDownDeadCard (p, kb))
                        else
                            InactiveCard (p, h, pid, kb),
                            Some (InactiveCard (p, h - attackDamage, pid, kb)),
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
                | InactiveCard (p, h, pid, kb) when
                    pid = ownerID
                    && p = power
                    && h = health
                    && List.contains playerID kb ->
                    Some (
                        if h <= attackDamage then
                            InactiveCard (p, h, pid, kb),
                            None,
                            Some (FaceDownDeadCard (p, kb))
                        else
                            InactiveCard (p, h, pid, kb),
                            Some (InactiveCard (p, h - attackDamage, pid, kb)),
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
                | ActiveCard (p, h, r, pid) when
                    pid = ownerID
                    && p = power
                    && h = health ->
                    Some (
                        if h <= attackDamage then
                            ActiveCard (p, h, r, pid),
                            None,
                            Some (FaceUpDeadCard p)
                        else
                            ActiveCard (p, h, r, pid),
                            Some (ActiveCard (p, h - attackDamage, r, pid)),
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
                | Pair (p, (h1, r1), (h2, r2), pid) when
                    pid = ownerID
                    && p = power
                    && (
                        (h1 = health && h2 = partnerHealth)
                        || (h2 = health && h1 = partnerHealth)
                    ) ->
                    Some (
                        if health = attackDamage then
                            if h1 = health then
                                Pair (p, (h1, r1), (h2, r2), pid),
                                Some (ActiveCard (p, h2, r2, pid)),
                                Some (FaceUpDeadCard p)
                            else
                                Pair (p, (h1, r1), (h2, r2), pid),
                                Some (ActiveCard (p, h1, r1, pid)),
                                Some (FaceUpDeadCard p)
                        else
                            if h1 = health then
                                Pair (p, (h1, r1), (h2, r2), pid),
                                Some (Pair (p, (h1 - attackDamage, r1), (h2, r2), pid)),
                                None
                            else
                                Pair (p, (h1, r1), (h2, r2), pid),
                                Some (Pair (p, (h1, r1), (h2 - attackDamage, r2), pid)),
                                None
                        )
                | Pair _
                | ActiveCard _
                | InactiveCard _ -> None
                )
            |> List.head
    attacker, attackerAfter, target, targetAfter, deadCard

let private executeTurnAction (action: TurnActionInfo) (gameState: GameState) =
    let newStateBeforeActionUpdate =
        match action with
        | Play (playerID, power, laneID) ->
            let oldCard = HandCard power
            let newCard = InactiveCard (power, 2<health>, playerID, List.singleton playerID)
            let moveFrom = CountMap.dec oldCard
            let moveTo = CountMap.inc newCard
            {gameState with
                Hands =
                    gameState.Hands
                    |> List.map (fun (ownerID, cards) ->
                        ownerID,
                        if ownerID = playerID then
                            moveFrom cards
                        else
                            cards
                        )
                Board =
                    gameState.Board
                    |> List.mapi (fun n lane ->
                        if (n + 1)*1<LID> = laneID then
                            incInLane newCard playerID lane
                        else
                            lane
                        )
                }
        | Activate (playerID, laneID, (power, health, knownBy)) ->
            let oldCard = InactiveCard (power, health, playerID, knownBy)
            let newCard = ActiveCard (power, health, Ready, playerID)
            let activateIn = changeInLane oldCard newCard
            {gameState with
                Board =
                    gameState.Board
                    |> List.mapi (fun n lane ->
                        if (n + 1)*1<LID> = laneID then
                            activateIn lane
                        else
                            lane
                    )
                }
        | Attack (playerID, laneID, attackerInfo, targetInfo) ->
            let lane = List.item (int laneID - 1) gameState.Board
            match lane with
                | WonLane _
                | TiedLane ->
                    failwithf "can't resolve attack in completed lane"
                | PreBaseFlipLane pbfl ->
                    let troops = pbfl.Troops
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
                    let newLane = PreBaseFlipLane {pbfl with Troops = newTroops}
                    let newDiscard =
                        match deadCard with
                        | Some dc ->
                            gameState.Discard
                            |> CountMap.inc dc
                        | None -> gameState.Discard
                    {gameState with
                        Board =
                            gameState.Board
                            |> List.mapi (fun n ln ->
                                if n = int laneID - 1 then
                                    newLane
                                else
                                    ln
                                )
                        Discard = newDiscard
                        }
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
                    let newLane = ContestedLane {cl with Troops = newTroops}
                    let newDiscard =
                        match deadCard with
                        | Some dc ->
                            gameState.Discard
                            |> CountMap.inc dc
                        | None -> gameState.Discard
                    {gameState with
                        Board =
                            gameState.Board
                            |> List.mapi (fun n ln ->
                                if n = int laneID - 1 then
                                    newLane
                                else
                                    ln
                                )
                        Discard = newDiscard
                        }
        | CreatePair (playerID, laneID, power, (health1, readiness1), (health2, readiness2)) ->
            let single1 = ActiveCard (power, health1, readiness1, playerID)
            let single2 = ActiveCard (power, health2, readiness2, playerID)
            let pair = Pair (power, (health1, readiness1), (health2, readiness2), playerID)
            {gameState with
                Board =
                    gameState.Board
                    |> List.mapi (fun n lane ->
                        if (n + 1)*1<LID> = laneID then
                            lane
                            |> decInLane single1 playerID
                            |> decInLane single2 playerID
                            |> incInLane pair playerID
                        else
                            lane
                        )
                }
    {newStateBeforeActionUpdate with
        ActionsLeft = gameState.ActionsLeft - 1
        }

let rec private makeNextActionInfo gameState action =
    let newGameState =
        match action with
        | TurnActionInfo tai ->
            executeTurnAction tai gameState
        | EndTurn _ ->
            {gameState with CurrentPlayer = None}
        | StartTurn id ->
            let nPlayers = List.length gameState.Hands
            if id = nPlayers*1<PID> then
                {gameState with
                    CurrentPlayer = Some id
                    NextPlayer = 1<PID>
                    ActionsLeft = 3
                    }
            else
                {gameState with
                    CurrentPlayer = Some id
                    NextPlayer = id + 1<PID>
                    ActionsLeft = 3
                    }
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
    let board, notBaseCards =
        shuffledDeck
        |> prepareHead (prepareBoard nLanes) (nPlayers*nLanes)
    let hands, notDeckCards =
        notBaseCards
        |> prepareHead (prepareHands nPlayers) (5*nPlayers)
    let removed, notRemoved =
        notDeckCards
        |> prepareHead prepareRemoved 10
    let gameState = {
        Board = board
        DrawPile = prepareDrawPile notRemoved
        CurrentPlayer = Some 1<PID>
        NextPlayer = 2<PID>
        ActionsLeft = 2
        Hands = hands
        Discard = Map.empty
        Removed = removed
    }
    let displayInfo = getDisplayInfo gameState
    let nextActions =
        getPossibleActionsInfo displayInfo
        |> List.map (makeNextActionInfo gameState)
    InProgress (displayInfo, nextActions)

let api = {
    NewGame = fun () -> createGame 2 3
}
