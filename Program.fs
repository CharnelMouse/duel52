open Domain

let mainLoop (game: GameState) =
    game |> string |> printfn "%s"

[<EntryPoint>]
let main argv =
    let maybeGame = Game.tryCreate 2 3
    match maybeGame with
    | Some game -> mainLoop game
    | None -> printf "Failed to initialise game"
    0
