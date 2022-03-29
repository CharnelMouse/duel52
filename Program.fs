// Console
//[<EntryPoint>]
//let main argv =
//    let api = Implementation.api
//    ConsoleUI.startGame api
//    0

// MonoGame
[<EntryPoint>]
let main _ =
    let api = Implementation.api
    MonoGameUI.startGame api
    0
