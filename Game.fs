module Game
open Domain

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
    |> List.mapi (fun n p -> DrawCard (p, n))

let prepareHead fn n lst =
    let h, t = List.splitAt n lst
    fn h, t

let prepareBoard nPlayers lst =
    lst
    |> List.splitInto nPlayers
    |> List.mapi (fun player lst ->
        lst |> List.map (fun (DrawCard (power, id)) -> Base (power, id, player))
        )
    |> List.transpose
    |> List.map Lane.create

let prepareHands nPlayers lst =
    lst
    |> List.splitInto nPlayers
    |> List.map (fun lst ->
        lst |> List.map (fun (DrawCard (power, id)) -> HandCard (power, id))
    )

let prepareRemoved lst =
    lst
    |> List.map (fun (DrawCard (power, _)) -> RemovedCard (power))

let tryCreate nPlayers nLanes =
    if nPlayers <> 2 then
        None
    else
        let shuffledDeck =
            createUnshuffledDeck()
            |> shuffle
        let board, notBaseCards =
            shuffledDeck
            |> prepareHead (prepareBoard nPlayers) (nPlayers*nLanes)
        let hands, notDeckCards =
            notBaseCards
            |> prepareHead (prepareHands nPlayers) (5*nPlayers)
        let removed, notRemoved =
            notDeckCards
            |> prepareHead prepareRemoved 10
        Some {
            Board = board
            DrawPile = notRemoved
            CurrentPlayer = 0
            ActionsLeft = 2
            Hands = hands
            Discard = []
            Removed = removed
        }
