open Domain

let mainLoop (game: Game) =
    game.Board |> string |> printfn "%s"

[<EntryPoint>]
let main argv =
    let game = Game.create 2 3
    mainLoop game
    0
