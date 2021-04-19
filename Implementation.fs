module Implementation
open Domain

type private GameState = {
    Board: Lane list
    DrawPile: DrawCard list
    CurrentPlayer: PlayerID
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
    let (playerHandInfo, opponentHandsInfo) =
        gameState.Hands
        |> List.partition (fun (n, _) -> n = gameState.CurrentPlayer)
    let playerHand =
        playerHandInfo
        |> List.head
        |> (fun (_, hand) -> hand)
    let getBase = getBaseKnowledge gameState.CurrentPlayer
    let getTroop = getTroopKnowledge gameState.CurrentPlayer
    let getDeadCard = getDeadCardKnowledge gameState.CurrentPlayer
    {
        CurrentPlayer = gameState.CurrentPlayer
        ActionsLeft = gameState.ActionsLeft
        BoardKnowledge =
            gameState.Board
            |> List.map (function
                | PreBaseFlipLane {Bases = bases; Troops = troops} ->
                    {
                        Bases = List.map getBase bases
                        Troops = List.map getTroop troops
                        }
                    |> PreBaseFlipLaneKnowledge
                | ContestedLane {Troops = troops} ->
                    {Troops = List.map getTroop troops}
                    |> ContestedLaneKnowledge
                | WonLane {Controller = controller; Troops = troops} ->
                    {
                        Controller = controller
                        Troops = List.map getTroop troops
                        }
                    |> WonLaneKnowledge
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
        CurrentPlayer = 1
        ActionsLeft = 2
        Hands = hands
        Discard = []
        Removed = removed
    }
    let displayInfo = getDisplayInfo gameState
    let nextActions =
        displayInfo.PlayerHand
        |> List.collect (fun (HandCard (power, cardID)) ->
            [1..(List.length board)]
            |> List.map (fun laneID -> {
                Action = Play (cardID, power, laneID)
                Capability = fun () -> InProgress (displayInfo, []) // dummy action
            }
            )
        )
    InProgress (displayInfo, nextActions)

let api = {
    NewGame = fun () -> createGame 2 3
}
