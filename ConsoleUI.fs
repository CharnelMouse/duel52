module ConsoleUI
open System
open Domain

let private deparsePower power =
    match power with
    | ActivationPower View -> '2'
    | ActivationPower Trap -> '3'
    | ActivationPower Foresight -> '4'
    | ActivationPower Flip -> '5'
    | ActivationPower Freeze -> '6'
    | ActivationPower Heal -> '7'
    | PassivePower Retaliate -> '8'
    | PassivePower Nimble -> '9'
    | PassivePower TwinStrike -> 'X'
    | PassivePower Taunt -> 'J'
    | PassivePower Vampiric -> 'V'
    | ActivationPower Move -> 'Q'
    | ActivationPower Empower -> 'K'
    | ActivationPower Action -> 'A'

let private displayBaseKnowledge baseKnowledge =
    match baseKnowledge with
    | UnknownBaseCard playerID ->
        printf "?"
    | KnownBaseCard (playerID, power) ->
        printf "%c" (deparsePower power)

let private displayInactiveUnitKnowledge inactiveUnitKnowledge =
    match inactiveUnitKnowledge with
    | UnknownInactiveCardKnowledge health ->
        printfn "Inactive, %i health" health
    | KnownInactiveCardKnowledge (power, health) ->
        printfn "Inactive %c, %i health" (deparsePower power) health
    // later will need cases for active/pair when card is frozen

let private displayActiveUnitKnowledge currentPlayer ownerID activeUnitKnowledge =
    let (power, health, readiness) = activeUnitKnowledge
    if ownerID = currentPlayer then
        printfn "%A %c, %i health" readiness (deparsePower power) health
    else
        printfn "%c, %i health" (deparsePower power) health

let private displayPairKnowledge currentPlayer ownerID pairKnowledge =
    let (power, health1, health2, readiness) = pairKnowledge
    if ownerID = currentPlayer then
        printfn "%A %c pair: health %i and %i" readiness (deparsePower power) health1 health2
    else
        printfn "%c pair: health %i and %i" (deparsePower power) health1 health2

let private displayTroopKnowledges currentPlayer (troops: TroopKnowledge) =
    if Map.forall (fun _ (inactiveUnits, activeUnits, pairs) ->
        List.isEmpty inactiveUnits
        && List.isEmpty activeUnits
        && List.isEmpty pairs
        ) troops then
        printfn "No troops present"
    else
        printfn "Troops"
        troops
        |> Map.iter (fun owner (inactiveUnits, activeUnits, pairs) ->
            printfn "Player %i" owner
            List.iter displayInactiveUnitKnowledge inactiveUnits
            List.iter (displayActiveUnitKnowledge currentPlayer owner) activeUnits
            List.iter (displayPairKnowledge currentPlayer owner) pairs
            )

let private displayPreLaneKnowledge currentPlayer (n, (lane: PreBaseFlipLaneKnowledge)) =
    let {Bases = baseKnowledges; Troops = troops} = lane
    printfn "Lane %i" (n + 1)
    printf "Bases: "
    List.iter displayBaseKnowledge baseKnowledges
    printfn ""
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
        if Map.forall (fun _ (inactiveUnits, activeUnits, pairs) ->
            List.isEmpty inactiveUnits
            && List.isEmpty activeUnits
            && List.isEmpty pairs
            ) troops then
            printfn "No troops present"
        else
            printfn "Troops"
            displayTroopKnowledges currentPlayer troops
    printfn ""

let private displayPreLaneKnowledges currentPlayer laneKnowledges =
    laneKnowledges
    |> List.indexed
    |> List.iter (displayPreLaneKnowledge currentPlayer)

let private displayPostLaneKnowledges currentPlayer laneKnowledges =
    laneKnowledges
    |> List.indexed
    |> List.iter (displayPostLaneKnowledge currentPlayer)

let private displayHandCard (HandCard power) =
    printf "%c" (deparsePower power)

let private displayOpponentHandSize (id, size) =
    if size = 0 then
        printfn "Player %i's hand is empty" id
    else
        printfn "Player %i has %i cards" id size

let private displayDeadCard deadCard =
    match deadCard with
    | UnknownDeadCard -> printfn "unknown card"
    | KnownDeadCard (KnownFaceDownDeadCard power) -> printfn "%c, face-down" (deparsePower power)
    | KnownDeadCard (KnownFaceUpDeadCard power) -> printfn "%c, face-up" (deparsePower power)

let private displayDiscardKnowledge discardKnowledge =
    if List.isEmpty discardKnowledge then
        printfn "Discard pile is empty"
    else
        printfn "\nDiscard pile:"
        discardKnowledge
        |> List.iter displayDeadCard

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
        if List.isEmpty playerHand then
            printfn "Hand is empty"
        else
            printf "Hand: "
            List.iter displayHandCard playerHand
        printfn ""
        match opponentHandSizes with
        | [] -> ()
        | [h] -> displayOpponentHandSize h
        | _ ->
            printfn "Opponent hand sizes"
            List.iter displayOpponentHandSize opponentHandSizes
        printfn ""
        match boardKnowledge with
        | PreBaseFlipBoardKnowledge {DrawPileSize = dps; Discard = dk} ->
            printfn "Draw pile: %i" dps
            displayDiscardKnowledge dk
        | PostBaseFlipBoardKnowledge {Discard = dk} ->
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
    | TurnActionInfo (Play (handPosition, laneID)) ->
        "Play card " + string handPosition
        + " to lane " + string laneID
    | TurnActionInfo (Activate (_, laneID, position)) ->
        "Activate active card " + string position
        + " in lane " + string laneID
    | TurnActionInfo (SingleAttack (_, laneID, singleAttackerInfo, targetInfo)) ->
        let attackerText =
            let (power, health) = singleAttackerInfo
            string (deparsePower power) + " (" + string health + " HP)"
        let targetText =
            match targetInfo with
            | UnknownInactiveTarget (pid, h) ->
                "player " + string pid + "'s"
                + " unknown inactive (" + string h + " HP)"
                + " in lane " + string laneID
            | KnownInactiveTarget (pid, p, h) ->
                "player " + string pid + "'s"
                + " inactive " + string (deparsePower p)
                + " (" + string h + " HP)"
                + " in lane " + string laneID
            | ActiveSingleTarget (pid, p, h) ->
                "player " + string pid + "'s"
                + " active " + string (deparsePower p)
                + " (" + string h + " HP)"
                + " in lane " + string laneID
            | ActivePairMemberTarget (pid, p, h1, h2) ->
                "player " + string pid + "'s"
                + " " + string (deparsePower p) + " pair member"
                + " (" + string h1 + " HP, partner " + string h2 + ")"
                + " in lane " + string laneID
        attackerText + " attacks " + targetText
    | TurnActionInfo (PairAttack (_, laneID, pairAttackerInfo, targetInfo)) ->
        let attackerText =
            let (power, health1, health2) = pairAttackerInfo
            string (deparsePower power) + " (" + string health1 + ", " + string health2 + " HP)"
        let targetText =
            match targetInfo with
            | UnknownInactiveTarget (pid, h) ->
                "player " + string pid + "'s"
                + " unknown inactive (" + string h + " HP)"
                + " in lane " + string laneID
            | KnownInactiveTarget (pid, p, h) ->
                "player " + string pid + "'s"
                + " inactive " + string (deparsePower p)
                + " (" + string h + " HP)"
                + " in lane " + string laneID
            | ActiveSingleTarget (pid, p, h) ->
                "player " + string pid + "'s"
                + " active " + string (deparsePower p)
                + " (" + string h + " HP)"
                + " in lane " + string laneID
            | ActivePairMemberTarget (pid, p, h1, h2) ->
                "player " + string pid + "'s"
                + " " + string (deparsePower p) + " pair member"
                + " (" + string h1 + " HP, partner " + string h2 + ")"
                + " in lane " + string laneID
        attackerText + " attacks " + targetText
    | TurnActionInfo (CreatePair (_, laneID, power, (health1, readiness1), (health2, readiness2))) ->
        "Create " + string (deparsePower power) + " pair"
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
