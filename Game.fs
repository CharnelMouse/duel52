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

let create nPlayers nLanes =
    let shuffledDeck =
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
            Vampiric // replaces Trap and Foresight in solo mode
            Move
            Empower
            Action

        ]
        |> List.filter (fun power -> power <> Vampiric)
        |> List.collect (List.replicate 4)
        |> List.zip [1..52]
        |> List.map (fun (n, p) -> DrawCard (p, n))
        |> shuffle
    let baseCards, notBaseCards = List.splitAt (nPlayers * nLanes) shuffledDeck
    let preparedBaseCards =
        baseCards
        |> List.splitInto nPlayers
        |> List.mapi (fun player lst ->
            lst |> List.map (fun (DrawCard (power, id)) -> Base (power, id, player))
            )
        |> List.transpose
    let board =
        preparedBaseCards
        |> List.map Lane.create
    {
        Board = board
        DrawPile = []
        CurrentPlayer = 1
        ActionsLeft = 3
        Hands = List.replicate nPlayers []
        Discard = []
        Removed = []
    }
