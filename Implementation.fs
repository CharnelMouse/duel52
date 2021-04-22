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
        KnownBaseCard (power, ownerID)
    else
        UnknownBaseCard ownerID

let private getTroopKnowledge (playerID: PlayerID) troop =
    match troop with
    | InactiveCard (power, health, ownerID, knownBy) ->
        if List.contains playerID knownBy then
            KnownInactiveCardKnowledge (power, health, ownerID, List.filter (fun id -> id <> playerID) knownBy)
        else
            UnknownInactiveCardKnowledge (health, ownerID, List.filter (fun id -> id <> playerID) knownBy)
    | ActiveCard (power, health, readiness, ownerID) ->
        ActiveCardKnowledge (power, health, readiness, ownerID)
    | Pair (power, (health1, readiness1), (health2, readiness2), ownerID) ->
        let readiness =
            if readiness1 = Exhausted || readiness2 = Exhausted then
                Exhausted
            else
                Ready
        PairKnowledge (power, health1, health2, readiness, ownerID)

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
                | KnownInactiveCardKnowledge (power, health, ownerID, knownBy) when
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
                | KnownInactiveCardKnowledge (power, health, ownerID, knownBy) when
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
            | ActiveCardKnowledge (power, health, readiness, ownerID) ->
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
            (playerID, laneID, power, (health1, readiness1), (health2, readiness2))
            |> CreatePair
            |> TurnActionInfo
            )
    sameKeyPair @ differentKeyPair

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
        | Attack (playerID, laneID, attackingTroopID, (maybePower, health)) ->
            gameState
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
