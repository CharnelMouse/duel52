module ConsoleUI
open Domain

let mainLoop (game: ActionResult) =
    match game with
    | InProgress (displayInfo, nextActions) ->
        displayInfo |> string |> printfn "%s"
    | Won (displayInfo, winnerID) ->
        winnerID |> printfn "Player %i wins"

let startGame api = mainLoop (api.NewGame())
