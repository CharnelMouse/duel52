module ConsoleUI
open System
open Domain

let private deparsePower power =
    match power with
    | ActivationPower p ->
        string p
    | PassivePower p ->
        string p

let private displayBaseKnowledge baseKnowledge =
    match baseKnowledge with
    | UnknownBaseCard playerID ->
        ()
    | KnownBaseCard (playerID, power) ->
        printfn "Player %i: %s" playerID (deparsePower power)

let private displayTroopKnowledge currentPlayer troopKnowledge n =
    match troopKnowledge with
    | UnknownInactiveCardKnowledge (playerID, health, knownBy) ->
        printfn "Player %i, (%i) Inactive, %i health" playerID n health
    | KnownInactiveCardKnowledge (playerID, power, health, knownBy) ->
        printfn "Player %i, (%i) Inactive %s, %i health" playerID n (deparsePower power) health
    // later will need cases for active/pair when card is frozen
    | ActiveCardKnowledge (playerID, power, health, readiness) ->
        if playerID = currentPlayer then
            printfn "Player %i, (%i) %A %s, %i health" playerID n readiness (deparsePower power) health
        else
            printfn "Player %i, (%i) %s, %i health" playerID n (deparsePower power) health
    | PairKnowledge (playerID, power, health1, health2, readiness) ->
        if playerID = currentPlayer then
            printfn "Player %i, (%i) %A %s pair: health %i and %i" playerID n readiness (deparsePower power) health1 health2
        else
            printfn "Player %i, (%i) %s pair: health %i and %i" playerID n (deparsePower power) health1 health2

let private displayTroopKnowledges currentPlayer =
    CountMap.iter (displayTroopKnowledge currentPlayer)

let private displayPreLaneKnowledge currentPlayer (n, (lane: PreBaseFlipLaneKnowledge)) =
    let {Bases = baseKnowledges; Troops = troops} = lane
    printfn "Lane %i" (n + 1)
    printfn "Bases present"
    List.iter displayBaseKnowledge baseKnowledges
    if CountMap.isEmpty troops then
        printfn "No troops present"
    else
        printfn "Troops"
        displayTroopKnowledges currentPlayer troops
    printfn ""

let private displayPostLaneKnowledge currentPlayer (n, (lane: PostBaseFlipLaneKnowledge)) =
    match lane with
    | ContestedLaneKnowledge {Troops = troops} ->
        printfn "Lane %i" (n + 1)
        printfn "Troops"
        displayTroopKnowledges currentPlayer troops
    | WonLaneKnowledge {Controller = controller; Troops = troops} ->
        printfn "Lane %i (won by player %i)" (n + 1) controller
        if CountMap.isEmpty troops then
            printfn "No troops present"
        else
            printfn "Troops"
            troops
            |> (displayTroopKnowledges currentPlayer)
    | TiedLaneKnowledge ->
        printfn "Lane %i (tied)" (n + 1)
    printfn ""

let private displayPreLaneKnowledges currentPlayer laneKnowledges =
    laneKnowledges
    |> List.indexed
    |> List.iter (displayPreLaneKnowledge currentPlayer)

let private displayPostLaneKnowledges currentPlayer laneKnowledges =
    laneKnowledges
    |> List.indexed
    |> List.iter (displayPostLaneKnowledge currentPlayer)

let private displayHandCard (HandCard power) n =
    printfn "(%i) %s" n (deparsePower power)

let private displayOpponentHandSize (id, size) =
    if size = 0 then
        printfn "Player %i's hand is empty" id
    else
        printfn "Player %i has %i cards" id size

let private displayKnownDeadCard knownDeadCard n =
    match knownDeadCard with
    | KnownFaceDownDeadCard power -> printfn "(%i) %s, face-down" n (deparsePower power)
    | KnownFaceUpDeadCard power -> printfn "(%i) %s, face-up" n (deparsePower power)

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
    Console.Clear()
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
            displayPreLaneKnowledges currentPlayer lanes
        | PostBaseFlipBoardKnowledge {Lanes = lanes} ->
            displayPostLaneKnowledges currentPlayer lanes
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
        printfn "Player %i's turn" playerID
    | WonGameDisplayInfo {Winner = winner; LaneWins = laneWins} ->
        printfn "Player %i wins!" winner
        printfn "Lane wins:"
        laneWins
        |> List.iter (fun (pid, lanes) ->
            lanes
            |> List.map string
            |> String.concat ", "
            |> printfn "Player %i: %s" pid
            )
    | TiedGameDisplayInfo {LaneWins = laneWins} ->
        printfn "Game tie!"
        printfn "Lane wins:"
        laneWins
        |> List.iter (fun (pid, lanes) ->
            lanes
            |> List.map string
            |> String.concat ", "
            |> printfn "Player %i: %s" pid
            )

let private actionString action =
    match action with
    | TurnActionInfo (Play (_, power, laneID)) ->
        "Play " + deparsePower power
        + " to lane " + string laneID
    | TurnActionInfo (Activate (_, laneID, UnknownActivationTarget (health, knownBy))) ->
        "Activate " + "unknown card"
        +  " (" + string health + " HP)"
        + " in lane " + string laneID
    | TurnActionInfo (Activate (_, laneID, KnownActivationTarget (power, health, knownBy))) ->
        "Activate " + deparsePower power
        +  " (" + string health + " HP)"
        + " in lane " + string laneID
    | TurnActionInfo (Attack (_, laneID, attackerInfo, targetInfo)) ->
        let attackerText =
            match attackerInfo with
            | SingleAttacker (power, health) ->
                deparsePower power + " (" + string health + " HP)"
            | DoubleAttacker (power, health1, health2) ->
                deparsePower power + " (" + string health1 + ", " + string health2 + " HP)"
        let targetText =
            match targetInfo with
            | UnknownInactiveTarget (pid, h) ->
                "player " + string pid + "'s"
                + " unknown inactive (" + string h + " HP)"
                + " in lane " + string laneID
            | KnownInactiveTarget (pid, p, h) ->
                "player " + string pid + "'s"
                + " inactive " + deparsePower p
                + " (" + string h + " HP)"
                + " in lane " + string laneID
            | ActiveSingleTarget (pid, p, h) ->
                "player " + string pid + "'s"
                + " active " + deparsePower p
                + " (" + string h + " HP)"
                + " in lane " + string laneID
            | ActivePairMemberTarget (pid, p, h1, h2) ->
                "player " + string pid + "'s"
                + " " + deparsePower p + " pair member"
                + " (" + string h1 + " HP, partner " + string h2 + ")"
                + " in lane " + string laneID
        attackerText + " attacks " + targetText
    | TurnActionInfo (CreatePair (_, laneID, power, (health1, readiness1), (health2, readiness2))) ->
        "Create " + deparsePower power + " pair"
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

type private InputResult =
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
    | Exit ->
        ()

let startGame api = mainLoop (api.NewGame())
