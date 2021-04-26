module ConsoleUI
open System
open Domain

let private displayBaseKnowledge baseKnowledge =
    match baseKnowledge with
    | UnknownBaseCard playerID ->
        ()
    | KnownBaseCard (playerID, power) ->
        printfn "Player %i: %A" playerID power

let private displayTroopKnowledge troopKnowledge n =
    match troopKnowledge with
    | UnknownInactiveCardKnowledge (playerID, health, knownBy) ->
        printfn "Player %i, (%i) Inactive, %i health" playerID n health
    | KnownInactiveCardKnowledge (playerID, power, health, knownBy) ->
        printfn "Player %i, (%i) Inactive %A, %i health" playerID n power health
    | ActiveCardKnowledge (playerID, power, health, readiness) ->
        printfn "Player %i, (%i) %A %A, %i health" playerID n readiness power health
    | PairKnowledge (playerID, power, health1, health2, readiness) ->
        printfn "Player %i, (%i) %A %A pair: health %i and %i" playerID n readiness power health1 health2

let private displayTroopKnowledges =
    CountMap.iter displayTroopKnowledge

let private displayLaneKnowledge (n, lane) =
    match lane with
    | PreBaseFlipLaneKnowledge {Bases = baseKnowledges; Troops = troops} ->
        printfn "Lane %i" (n + 1)
        printfn "Bases present"
        List.iter displayBaseKnowledge baseKnowledges
        if CountMap.isEmpty troops then
            printfn "No troops present"
        else
            printfn "Troops"
            displayTroopKnowledges troops
    | ContestedLaneKnowledge {Troops = troops} ->
        printfn "Lane %i" (n + 1)
        printfn "Troops"
        displayTroopKnowledges troops
    | WonLaneKnowledge {Controller = controller; Troops = troops} ->
        printfn "Lane %i (won by player %i)" (n + 1) controller
        if CountMap.isEmpty troops then
            printfn "No troops present"
        else
            printfn "Troops"
            troops
            |> displayTroopKnowledges
    | TiedLaneKnowledge ->
        printfn "Lane %i (tied)" (n + 1)
    printfn ""

let private displayLaneKnowledges laneKnowledges =
    laneKnowledges
    |> List.indexed
    |> List.iter displayLaneKnowledge

let private displayHandCard (HandCard power) n =
    printfn "(%i) %A" n power

let private displayOpponentHandSize (id, size) =
    if size = 0 then
        printfn "Player %i's hand is empty" id
    else
        printfn "Player %i has %i cards" id size

let private displayKnownDeadCard knownDeadCard n =
    match knownDeadCard with
    | KnownFaceDownDeadCard power -> printfn "(%i) %A, face-down" n power
    | KnownFaceUpDeadCard power -> printfn "(%i) %A, face-up" n power

let private displayDiscardKnowledge discardKnowledge =
    let (unknown, known) = 
        discardKnowledge
        |> CountMap.partition (function
            | UnknownDeadCard -> true
            | KnownDeadCard _ -> false
        )
    if not (CountMap.isEmpty unknown) then
        printfn "%i unknown face-down cards" (CountMap.count unknown)
    if not (CountMap.isEmpty known) then
        known
        |> CountMap.choose (function
            | UnknownDeadCard -> None
            | KnownDeadCard c -> Some c
            )
        |> CountMap.iter displayKnownDeadCard

let private displayOngoingGameInfo displayInfo =
    match displayInfo with
    | TurnDisplayInfo tdi ->
        let {
            CurrentPlayer = currentPlayer
            ActionsLeft = actionsLeft
            BoardKnowledge = boardKnowledge
            PlayerHand = playerHand
            OpponentHandSizes = opponentHandSizes
            DrawPileSize = drawPileSize
            DiscardKnowledge = discardKnowledge
            } = tdi
        displayLaneKnowledges boardKnowledge
        printfn "Player %i's turn, %i actions left\n" currentPlayer actionsLeft
        if Map.isEmpty playerHand then
            printfn "Hand is empty"
        else
            printfn "Hand"
            Map.iter displayHandCard playerHand
        printfn ""
        match opponentHandSizes with
        | [] -> failwithf "opponents expected"
        | [h] -> displayOpponentHandSize h
        | _ ->
            printfn "Opponent hand sizes"
            List.iter displayOpponentHandSize opponentHandSizes
        printfn ""
        if drawPileSize = 0 then
            printfn "Draw pile is empty"
        else
            printfn "Draw pile: %i" drawPileSize
        if Map.isEmpty discardKnowledge then
            printfn "Discard pile is empty"
        else
            displayDiscardKnowledge discardKnowledge
    | SwitchDisplayInfo playerID ->
        Console.Clear()
        printfn "Player %i's turn" playerID

let private actionString action =
    match action with
    | TurnActionInfo (Play (_, power, laneID)) ->
        "play " + string power
        + " to lane " + string laneID
    | TurnActionInfo (Activate (_, laneID, (power, health, knownBy))) ->
        "flip " + string power
        + " health " + string health
        + " in lane " + string laneID
    | TurnActionInfo (Attack (_, laneID, attackerInfo, targetInfo)) ->
        "troop " + string attackerInfo
        + " attacks card " + string targetInfo
        + " in lane " + string laneID
    | TurnActionInfo (CreatePair (_, laneID, power, health1, health2)) ->
        "make a " + string power + " pair"
        + " in lane " + string laneID
        + ", health " + string health1 + " and " + string health2
    | EndTurn _ ->
        "End turn"
    | StartTurn _ ->
        "Start turn"

let private displayNextActionsInfo nextActions =
    printfn "Available actions\n0) Quit"
    nextActions
    |> List.indexed
    |> List.iter (fun (n, {Action = action}) ->
        printfn "%i) %s" (n + 1) (actionString action)
    )

type InputResult =
| UseAction of int
| Quit

let rec private getValidInput nActions =
    printf "Enter action number: "
    Console.ReadLine()
    |> Int32.TryParse
    |> function
    | true, n when n > nActions || n < 0 ->
        printfn "Invalid action number"
        getValidInput nActions
    | true, 0 ->
        Quit
    | true, n ->
        UseAction (n - 1)
    | _ ->
        printfn "Invalid action"
        getValidInput nActions

let rec private mainLoop (game: ActionResult) =
    printfn "\n--------------------\n"
    match game with
    | InProgress (displayInfo, nextActions) ->
        displayOngoingGameInfo displayInfo
        printfn ""
        displayNextActionsInfo nextActions
        printfn ""
        match getValidInput (List.length nextActions) with
        | Quit ->
            printfn "Goodbye."
        | UseAction n ->
            nextActions.[n].Capability()
            |> mainLoop
    | WonGame (displayInfo, winnerID) ->
        winnerID |> printfn "Player %i wins"

let startGame api = mainLoop (api.NewGame())
