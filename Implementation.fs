module Implementation
open Domain

type KnownBy = int list

type DrawCard = DrawCard of Power * CardID
type Base = Power * CardID * PlayerID * KnownBy
type InactiveCard = Power * CardID * Health * PlayerID * KnownBy
type FaceDownDeadCard = Power * KnownBy
type FaceUpDeadCard = Power
type DeadCard =
| FaceDownDeadCard of FaceDownDeadCard
| FaceUpDeadCard of FaceUpDeadCard
type RemovedCard = RemovedCard of Power

type Troop =
| InactiveCard of InactiveCard
| ActiveCard of ActiveCard
| Pair of Pair

type PreBaseFlipLane = {
    Bases: Base list
    Troops: Troop list
}

type ContestedLane = {
    Troops: Troop list
}

type WonLane = {
    Controller: PlayerID
    Troops: Troop list
}

type Lane =
| PreBaseFlipLane of PreBaseFlipLane
| ContestedLane of ContestedLane
| WonLane of WonLane
| TiedLane

type private GameState = {
    Board: Lane list
    DrawPile: DrawCard list
    CurrentPlayer: PlayerID option
    NextPlayer: PlayerID
    ActionsLeft: int
    Hands: (PlayerID * Hand) list
    Discard: DeadCard list
    Removed: RemovedCard list
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

let private addCardIDs shuffledDeck =
    shuffledDeck
    |> List.mapi (fun n p -> DrawCard (p, n))

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
    Troops = []
} 

let private prepareBoard nLanes lst =
    lst
    |> List.splitInto nLanes
    |> List.map (fun lst ->
        lst |> List.mapi (fun n (DrawCard (power, id)) -> Base (power, id, n + 1, []))
        )
    |> List.map createLane

let private prepareHands nPlayers lst =
    lst
    |> List.splitInto nPlayers
    |> List.mapi (fun n lst ->
        n + 1, lst |> List.map (fun (DrawCard (power, id)) -> HandCard (power, id))
    )

let private prepareRemoved lst =
    lst
    |> List.map (fun (DrawCard (power, _)) -> RemovedCard (power))

let private getBaseKnowledge (playerID: PlayerID) (baseCard: Base) =
    let (power, cardID, ownerID, knownBy) = baseCard
    if List.contains playerID knownBy then
        KnownBaseCard (power, cardID, ownerID)
    else
        UnknownBaseCard (cardID, ownerID)

let private getTroopKnowledge (playerID: PlayerID) troop =
    match troop with
    | InactiveCard (power, cardID, health, ownerID, knownBy) ->
        if List.contains playerID knownBy then
            KnownInactiveCardKnowledge (power, cardID, health, ownerID)
        else
            UnknownInactiveCardKnowledge (cardID, health, ownerID)
    | ActiveCard (power, cardID, health, readiness, ownerID) ->
        ActiveCardKnowledge (power, cardID, health, readiness, ownerID)
    | Pair (power, troopID, (cardID1, health1), (cardID2, health2), readiness, ownerID) ->
        PairKnowledge (power, troopID, (cardID1, health1), (cardID2, health2), readiness, ownerID)

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
                            Troops = List.map getTroop troops
                            }
                    | ContestedLane {Troops = troops} ->
                        ContestedLaneKnowledge {Troops = List.map getTroop troops}
                    | WonLane {Controller = controller; Troops = troops} ->
                        WonLaneKnowledge {
                            Controller = controller
                            Troops = List.map getTroop troops
                            }
                    | TiedLane ->
                        TiedLaneKnowledge
                    )
            PlayerHand = playerHand
            OpponentHandSizes =
                opponentHandsInfo
                |> List.map (fun (n, hand) -> n, List.length hand)
            DrawPileSize = List.length gameState.DrawPile
            DiscardKnowledge = List.map getDeadCard gameState.Discard
        }
    | None ->
        SwitchDisplayInfo gameState.NextPlayer

let private getPlayActionsInfo (turnDisplayInfo: TurnDisplayInfo) =
    turnDisplayInfo.PlayerHand
    |> List.collect (fun (HandCard (power, cardID)) ->
        [1..(List.length turnDisplayInfo.BoardKnowledge)]
        |> List.map (fun laneID ->
            Play (turnDisplayInfo.CurrentPlayer, cardID, power, laneID) |> TurnActionInfo
            )
        )

let private getActivateActionsInfo (turnDisplayInfo: TurnDisplayInfo) =
    let playerID = turnDisplayInfo.CurrentPlayer
    turnDisplayInfo.BoardKnowledge
    |> List.indexed
    |> List.collect (fun (n, lane) ->
        match lane with
        | PreBaseFlipLaneKnowledge {Troops = troops}
        | ContestedLaneKnowledge {Troops = troops} ->
            troops
            |> List.choose (fun troop ->
                match troop with
                | KnownInactiveCardKnowledge (power, cardID, health, ownerID) when
                    ownerID = playerID ->
                    Activate (playerID, cardID, power, n + 1, health)
                    |> TurnActionInfo
                    |> Some
                | KnownInactiveCardKnowledge _
                | UnknownInactiveCardKnowledge _
                | ActiveCardKnowledge _
                | PairKnowledge _ ->
                    None
                )
        | WonLaneKnowledge {Controller = c; Troops = troops} when c = playerID ->
            troops
            |> List.choose (fun troop ->
                match troop with
                | KnownInactiveCardKnowledge (power, cardID, health, ownerID) when
                    ownerID = playerID ->
                    Activate (playerID, cardID, power, n + 1, health)
                    |> TurnActionInfo
                    |> Some
                | KnownInactiveCardKnowledge _
                | UnknownInactiveCardKnowledge _
                | ActiveCardKnowledge _
                | PairKnowledge _ ->
                    None
                )
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
    | SwitchDisplayInfo playerID ->
        StartTurn playerID |> List.singleton

let private executeTurnAction (action: TurnActionInfo) (gameState: GameState) =
    let newStateBeforeActionUpdate =
        match action with
        | Play (playerID, cardID, power, laneID) ->
            let newCard = InactiveCard (power, cardID, 2, playerID, List.singleton playerID)
            {gameState with
                Hands =
                    gameState.Hands
                    |> List.map (fun (ownerID, cards) ->
                        ownerID,
                        cards
                        |> List.filter (fun (HandCard (power, handCardID)) ->
                            handCardID <> cardID
                            )
                        )
                Board =
                    gameState.Board
                    |> List.mapi (fun n lane ->
                        if n + 1 = laneID then
                            match lane with
                            | PreBaseFlipLane pbfl ->
                                PreBaseFlipLane {
                                    pbfl with Troops = newCard :: pbfl.Troops
                                    }
                            | ContestedLane cl ->
                                ContestedLane {
                                    cl with Troops = newCard :: cl.Troops
                                    }
                            | WonLane wl when wl.Controller = playerID ->
                                WonLane {
                                    wl with Troops = newCard :: wl.Troops
                                    }
                            | WonLane _ ->
                                failwithf "can't play cards in a lost lane"
                            | TiedLane _ ->
                                failwithf "can't play cards in a tied lane"                                
                        else
                            lane
                        )
                }
        | Activate (playerID, cardID, power, laneID, health) ->
            {gameState with
                Board =
                    gameState.Board
                    |> List.mapi (fun n lane ->
                        if n + 1 = laneID then
                            match lane with
                            | PreBaseFlipLane pbfl ->
                                PreBaseFlipLane {
                                    pbfl with
                                        Troops =
                                            pbfl.Troops
                                            |> List.map (fun troop ->
                                                match troop with
                                                | InactiveCard (p, cid, h, pid, _) when
                                                    p = power
                                                    && cid = cardID
                                                    && h = health
                                                    && pid = playerID ->
                                                    ActiveCard (p, cid, h, Ready, pid)
                                                | _ ->
                                                    troop
                                            )
                                }
                            | ContestedLane cl ->
                                ContestedLane {
                                    cl with
                                        Troops =
                                            cl.Troops
                                            |> List.map (fun troop ->
                                                match troop with
                                                | InactiveCard (p, cid, h, pid, _) when
                                                    p = power
                                                    && cid = cardID
                                                    && h = health
                                                    && pid = playerID ->
                                                    ActiveCard (p, cid, h, Ready, pid)
                                                | _ ->
                                                    troop
                                            )
                                }
                            | WonLane wl ->
                                WonLane {
                                    wl with
                                        Troops =
                                            wl.Troops
                                            |> List.map (fun troop ->
                                                match troop with
                                                | InactiveCard (p, cid, h, pid, _) when
                                                    p = power
                                                    && cid = cardID
                                                    && h = health
                                                    && pid = playerID ->
                                                    ActiveCard (p, cid, h, Ready, pid)
                                                | _ ->
                                                    troop
                                            )
                                }
                            | TiedLane ->
                                failwithf "Can't flip cards in a tied lane"
                        else
                            lane
                    )
                }
        | Attack (playerID, attackingTroopID, targetCardID) ->
            gameState
        | CreatePair (playerID, cardID1, cardID2, power, laneID, health1, health2) ->
            gameState
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
            if id = nPlayers then
                {gameState with
                    CurrentPlayer = Some id
                    NextPlayer = 1
                    ActionsLeft = 3
                    }
            else
                {gameState with
                    CurrentPlayer = Some id
                    NextPlayer = id + 1
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
        |> addCardIDs
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
        DrawPile = notRemoved
        CurrentPlayer = Some 1
        NextPlayer = 2
        ActionsLeft = 2
        Hands = hands
        Discard = []
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
