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
    let playerHand =
        gameState.Hands
        |> List.find (fun (n, _) -> n = gameState.CurrentPlayer)
        |> (fun (_, hand) -> hand)
    let displayInfo = {
        CurrentPlayer = 1
        ActionsLeft = 2
        BoardKnowledge =
            board
            |> List.choose (function
                | PreBaseFlipLane {Bases = bases} ->
                    {
                        Bases =
                            bases
                            |> List.map (fun (_, cardID, playerID, _) ->
                                UnknownBaseCard (cardID, playerID)
                                )
                        Troops = []
                        }
                    |> PreBaseFlipLaneKnowledge
                    |> Some
                | ContestedLane _
                | WonLane _
                | TiedLane ->
                    None
                )
        PlayerHand = playerHand
        OpponentHandSizes =
            gameState.Hands
            |> List.choose (fun (n, hand) ->
                if n = gameState.CurrentPlayer then
                    None
                else
                    Some (n, List.length hand)
                )
        DrawPileSize = List.length gameState.DrawPile
        DiscardKnowledge = []
    }
    let nextActions =
        playerHand
        |> List.collect (fun (HandCard (power, cardID)) ->
            [1..(List.length board + 1)]
            |> List.map (fun laneID -> {
                Action = Play (cardID, laneID)
                Capability = fun () -> InProgress (displayInfo, []) // dummy action
            }
            )
        )
    InProgress (displayInfo, nextActions)

let api = {
    NewGame = fun () -> createGame 2 3
}
