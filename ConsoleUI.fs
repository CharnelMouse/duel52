module ConsoleUI
open Domain

let private displayBaseKnowledge baseKnowledge =
    match baseKnowledge with
    | UnknownBaseCard (cardID, playerID) ->
        printfn "Player %i: card %i, unknown power" playerID cardID 
    | KnownBaseCard (power, cardID, playerID) ->
        printfn "Player %i: card %i, %A" playerID cardID power

let private displayTroopKnowledge troopKnowledge =
    match troopKnowledge with
    | UnknownInactiveCardKnowledge (cardID, health, playerID) ->
        printfn "Player %i, inactive card %i: unknown power, %i health" playerID cardID health
    | KnownInactiveCardKnowledge (power, cardID, health, playerID) ->
        printfn "Player %i, inactive card %i: %A, %i health" playerID cardID power health
    | ActiveCardKnowledge (power, cardID, health, readiness, playerID) ->
        printfn "Player %i, %A card %i: %A, %i health" playerID readiness cardID power health
    | PairKnowledge (power, troopID, (cardID1, health1), (cardID2, health2), readiness, playerID) ->
        printfn "Player %i, %A pair %i: %A, of card %i with %i health and card %i with %i health" playerID readiness troopID power cardID1 health1 cardID2 health2

let private displayLaneKnowledge (n, lane) =
    match lane with
    | PreBaseFlipLaneKnowledge {Bases = baseKnowledges; Troops = troops} ->
        printfn "Lane %i" (n + 1)
        printfn "Bases"
        List.iter displayBaseKnowledge baseKnowledges
        if List.length troops = 0 then
            printfn "No troops present"
        else
            printfn "Troops"
            List.iter displayTroopKnowledge troops
    | ContestedLaneKnowledge {Troops = troops} ->
        printfn "Lane %i" (n + 1)
        printfn "Troops"
        List.iter displayTroopKnowledge troops
    | WonLaneKnowledge {Controller = controller; Troops = troops} ->
        printfn "Lane %i (won by player %i)" (n + 1) controller
        if List.isEmpty troops then
            printfn "No troops present"
        else
            printfn "Troops"
            troops
            |> List.sortBy (function
                | UnknownInactiveCardKnowledge (_, _, playerID) -> playerID
                | KnownInactiveCardKnowledge (_, _, _, playerID) -> playerID
                | ActiveCardKnowledge (_, _, _, _, playerID) -> playerID
                | PairKnowledge (_, _, _, _, _, playerID) -> playerID
                )
            |> List.iter displayTroopKnowledge
    | TiedLaneKnowledge ->
        printfn "Lane %i (tied)" (n + 1)
    printfn ""

let private displayLaneKnowledges laneKnowledges =
    laneKnowledges
    |> List.indexed
    |> List.iter displayLaneKnowledge

let private displayHandCard (HandCard (power, cardID)) =
    printfn "Card %i: %A" cardID power

let private displayOpponentHandSize (id, size) =
    if size = 0 then
        printfn "Player %i's hand is empty" id
    else
        printfn "Player %i has %i cards" id size

let private displayKnownDeadCard knownDeadCard =
    match knownDeadCard with
    | KnownFaceDownDeadCard power -> printfn "%A, face-down" power
    | KnownFaceUpDeadCard power -> printfn "%A, face-up" power

let private displayDiscardKnowledge discardKnowledge =
    let (unknown, known) = 
        discardKnowledge
        |> List.partition (function
            | UnknownDeadCard -> true
            | KnownDeadCard _ -> false
        )
    if not (List.isEmpty unknown) then
        printfn "%i unknown face-down cards" (List.length unknown)
    if not (List.isEmpty known) then
        known
        |> List.choose (function
            | UnknownDeadCard -> None
            | KnownDeadCard c -> Some c
            )
        |> List.iter displayKnownDeadCard

let private displayOngoingGameInfo displayInfo =
    let {
        CurrentPlayer = currentPlayer
        ActionsLeft = actionsLeft
        BoardKnowledge = boardKnowledge
        PlayerHand = playerHand
        OpponentHandSizes = opponentHandSizes
        DrawPileSize = drawPileSize
        DiscardKnowledge = discardKnowledge
        } = displayInfo
    displayLaneKnowledges boardKnowledge
    printfn "Player %i's turn, %i actions left\n" currentPlayer actionsLeft
    if List.isEmpty playerHand then
        printfn "Hand is empty"
    else
        printfn "Hand"
        List.iter displayHandCard playerHand
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
    if List.isEmpty discardKnowledge then
        printfn "Discard pile is empty"
    else
        displayDiscardKnowledge discardKnowledge

let private actionString action =
    match action with
    | Play (cardID, power, laneID) ->
        "play card " + string cardID
        + " (" + string power + ")"
        + " to lane " + string laneID
    | FlipCard (cardID, power, laneID, health) ->
        "flip card " + string cardID
        + " ("
        + string power
        + ", "
        + "lane " + string laneID
        + ", "
        + string health + " health"
        + ")"
    | Attack (attackerID, defenderID) ->
        "troop " + string attackerID
        + " attacks card " + string defenderID
    | CreatePair (cardID1, cardID2, power, laneID, health1, health2) ->
        "pair " + string power + " cards "
        + string cardID1 + " (" + string health1 + " health)"
        + " and "
        + string cardID2 + " (" + string health2 + ")"
        + " (lane " + string laneID + ")"

let private displayNextActionsInfo nextActions =
    printfn "Available actions\n0) Quit"
    nextActions
    |> List.indexed
    |> List.iter (fun (n, {Action = action}) ->
        printfn "%i) %s" (n + 1) (actionString action)
    )

let private mainLoop (game: ActionResult) =
    printfn "\n--------------------\n"
    match game with
    | InProgress (displayInfo, nextActions) ->
        displayOngoingGameInfo displayInfo
        printfn ""
        displayNextActionsInfo nextActions
    | WonGame (displayInfo, winnerID) ->
        winnerID |> printfn "Player %i wins"

let startGame api = mainLoop (api.NewGame())
