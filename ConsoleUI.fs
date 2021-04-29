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

let private displayPreLaneKnowledge (n, (lane: PreBaseFlipLaneKnowledge)) =
    let {Bases = baseKnowledges; Troops = troops} = lane
    printfn "Lane %i" (n + 1)
    printfn "Bases present"
    List.iter displayBaseKnowledge baseKnowledges
    if CountMap.isEmpty troops then
        printfn "No troops present"
    else
        printfn "Troops"
        displayTroopKnowledges troops
    printfn ""

let private displayPostLaneKnowledge (n, (lane: PostBaseFlipLaneKnowledge)) =
    match lane with
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

let private displayPreLaneKnowledges laneKnowledges =
    laneKnowledges
    |> List.indexed
    |> List.iter displayPreLaneKnowledge

let private displayPostLaneKnowledges laneKnowledges =
    laneKnowledges
    |> List.indexed
    |> List.iter displayPostLaneKnowledge

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
            } = tdi
        match boardKnowledge with
        | PreBaseFlipBoardKnowledge {Lanes = lanes} ->
            displayPreLaneKnowledges lanes
        | PostBaseFlipBoardKnowledge {Lanes = lanes} ->
            displayPostLaneKnowledges lanes
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
        match boardKnowledge with
        | PreBaseFlipBoardKnowledge {DrawPileSize = dps; Discard = dk} ->
            printfn "Draw pile: %i" dps
            if Map.isEmpty dk then
                printfn "Discard pile is empty"
            else
                printfn "\nDiscard pile:"
                displayDiscardKnowledge dk
        | PostBaseFlipBoardKnowledge {Discard = dk} ->
            printfn "Draw pile is empty"
            if Map.isEmpty dk then
                printfn "Discard pile is empty"
            else
                printfn "\nDiscard pile:"
                displayDiscardKnowledge dk
    | SwitchDisplayInfo playerID ->
        Console.Clear()
        printfn "Player %i's turn" playerID

let private actionString action =
    match action with
    | TurnActionInfo (Play (_, power, laneID)) ->
        "Play " + string power
        + " to lane " + string laneID
    | TurnActionInfo (Activate (_, laneID, (power, health, knownBy))) ->
        "Activate " + string power
        +  " (" + string health + " HP)"
        + " in lane " + string laneID
    | TurnActionInfo (Attack (_, laneID, attackerInfo, targetInfo)) ->
        let attackerText =
            match attackerInfo with
            | SingleAttacker (power, health) ->
                string power + " (" + string health + " HP)"
            | DoubleAttacker (power, health1, health2) ->
                string power + " (" + string health1 + ", " + string health2 + " HP)"
        let targetText =
            match targetInfo with
            | UnknownInactiveTarget (pid, h) ->
                "player " + string pid + "'s"
                + " unknown inactive (" + string h + " HP)"
                + " in lane " + string laneID
            | KnownInactiveTarget (pid, p, h) ->
                "player " + string pid + "'s"
                + " inactive " + string p
                + " (" + string h + " HP)"
                + " in lane " + string laneID
            | ActiveSingleTarget (pid, p, h) ->
                "player " + string pid + "'s"
                + " active " + string p
                + " (" + string h + " HP)"
                + " in lane " + string laneID
            | ActivePairMemberTarget (pid, p, h1, h2) ->
                "player " + string pid + "'s"
                + " " + string p + " pair member"
                + " (" + string h1 + " HP, partner " + string h2 + ")"
                + " in lane " + string laneID
        attackerText + " attacks " + targetText
    | TurnActionInfo (CreatePair (_, laneID, power, (health1, readiness1), (health2, readiness2))) ->
        "Create " + string power + " pair"
        + " in lane " + string laneID
        + " from " + string readiness1 + " " + string health1 + " HP"
        + " and " + string readiness2 + " " + string health2 + " HP"
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
