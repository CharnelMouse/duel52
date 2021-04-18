open Domain

let mainLoop (game: ActionResult) =
    match game with
    | InProgress (displayInfo, nextActions) ->
        displayInfo |> string |> printfn "%s"
    | Won (displayInfo, winnerID) ->
        winnerID |> printfn "Player %i wins"

[<EntryPoint>]
let main argv =
    let api = Implementation.api
    let game = api.NewGame()
    mainLoop game
    0
