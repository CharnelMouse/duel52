module Implementation
open Domain

type GameState = {
    Board: Lane list
    DrawPile: DrawCard list
    CurrentPlayer: PlayerID
    ActionsLeft: int
    Hands: Hand list
    Discard: DeadCard list
    Removed: RemovedCard list
}

let rec shuffleRec unshuffled shuffled (sampler: System.Random) =
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

let shuffle lst =
    let sampler = System.Random()
    shuffleRec lst [] sampler

let addCardIDs shuffledDeck =
    shuffledDeck
    |> List.mapi (fun n p -> DrawCard (p, n))

let createUnshuffledDeck () =
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

let prepareHead fn n lst =
    let h, t = List.splitAt n lst
    fn h, t

let createLane bases = Contested {
    Bases = bases
    Troops = []
} 

let prepareBoard nPlayers lst =
    lst
    |> List.splitInto nPlayers
    |> List.mapi (fun player lst ->
        lst |> List.map (fun (DrawCard (power, id)) -> Base (power, id, player))
        )
    |> List.transpose
    |> List.map createLane

let prepareHands nPlayers lst =
    lst
    |> List.splitInto nPlayers
    |> List.map (fun lst ->
        lst |> List.map (fun (DrawCard (power, id)) -> HandCard (power, id))
    )

let prepareRemoved lst =
    lst
    |> List.map (fun (DrawCard (power, _)) -> RemovedCard (power))

let getDisplayInfo (gameState: GameState) = {
    CurrentPlayer = gameState.CurrentPlayer
    ActionsLeft = gameState.ActionsLeft
    RevealedBoard = gameState.Board
    PlayerHand = gameState.Hands.[gameState.CurrentPlayer]
    OpponentHandSizes =
        gameState.Hands
        |> List.indexed
        |> List.choose (fun (n, hand) ->
            if n = gameState.CurrentPlayer then
                None
            else
                Some (List.length hand)
        )
    DrawPileSize = List.length gameState.DrawPile
    Discard = gameState.Discard
}

let createGame nPlayers nLanes =
    let shuffledDeck =
        createUnshuffledDeck()
        |> shuffle
        |> addCardIDs
    let board, notBaseCards =
        shuffledDeck
        |> prepareHead (prepareBoard nPlayers) (nPlayers*nLanes)
    let hands, notDeckCards =
        notBaseCards
        |> prepareHead (prepareHands nPlayers) (5*nPlayers)
    let removed, notRemoved =
        notDeckCards
        |> prepareHead prepareRemoved 10
    let gameState = {
        Board = board
        DrawPile = notRemoved
        CurrentPlayer = 0
        ActionsLeft = 2
        Hands = hands
        Discard = []
        Removed = removed
    }
    let displayInfo = getDisplayInfo gameState
    let nextActions = []
    InProgress (displayInfo, nextActions)

let api = {
    NewGame = fun () -> createGame 2 3
}
