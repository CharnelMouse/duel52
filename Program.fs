[<EntryPoint>]
let main argv =
    let api = Implementation.api
    ConsoleUI.startGame api
    0
