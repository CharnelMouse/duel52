module ConsoleUI
open System
open Domain

let private deparsePower power =
    match power with
    | ActivationPower View -> '2'
    | InactiveDeathPower Trap -> '3'
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

let private displayInactiveUnitKnowledge (inactiveUnitKnowledge: InactiveUnitKnowledge) =
    match inactiveUnitKnowledge with
    | UnknownInactiveCardKnowledge (damage, actionability) ->
        match actionability with
        | Normal -> ()
        | Frozen -> printf "Frozen "
        match damage with
        | 0<health> -> printfn "Inactive"
        | d -> printfn "Inactive, %i damage" d
    | KnownInactiveCardKnowledge (power, damage, actionability) ->
        match actionability with
        | Normal -> ()
        | Frozen -> printf "Frozen "
        match damage with
        | 0<health> -> printfn "Inactive %c" (deparsePower power)
        | d -> printfn "Inactive %c, %i damage" (deparsePower power) d
    // later will need cases for active/pair when card is frozen

let private displayActiveUnitKnowledge currentPlayer ownerID (activeUnitKnowledge: ActiveUnitKnowledge) =
    let (power, damage, readiness, actionability) = activeUnitKnowledge
    match actionability with
    | Normal -> ()
    | Frozen -> printf "Frozen "
    if ownerID = currentPlayer then
        match damage with
        | 0<health> -> printfn "%A %c" readiness (deparsePower power)
        | d -> printfn "%A %c, %i damage" readiness (deparsePower power) d
    else
        match damage with
        | 0<health> -> printfn "%c" (deparsePower power)
        | d -> printfn "%c, %i damage" (deparsePower power) d

let private displayPairKnowledge currentPlayer ownerID (pairKnowledge: PairKnowledge) =
    let (power, damage1, damage2, readiness, actionability1, actionability2) = pairKnowledge
    match actionability1, actionability2 with
    | Normal, Normal -> ()
    | Normal, Frozen
    | Frozen, Normal
    | Frozen, Frozen -> printf "Frozen "
    if ownerID = currentPlayer then
        match damage1, damage2 with
        | 0<health>, 0<health> -> printfn "%A %c pair" readiness (deparsePower power)
        | d1, d2 -> printfn "%A %c pair: damage %i and %i" readiness (deparsePower power) d1 d2
    else
        match damage1, damage2 with
        | 0<health>, 0<health> -> printfn "%c pair" (deparsePower power)
        | d1, d2 -> printfn "%c pair: damage %i and %i" (deparsePower power) d1 d2

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

let private displayPreLaneKnowledge currentPlayer laneID (lane: PreBaseFlipLaneKnowledge) =
    let {Bases = baseKnowledges; Troops = troops} = lane
    printfn "Lane %i" laneID
    printf "Bases: "
    List.iter displayBaseKnowledge baseKnowledges
    printfn ""
    displayTroopKnowledges currentPlayer troops
    printfn ""

let private displayPostLaneKnowledge currentPlayer laneID (lane: PostBaseFlipLaneKnowledge) =
    match lane with
    | ContestedLaneKnowledge {Troops = troops} ->
        printfn "Lane %i" laneID
        printfn "Troops"
        displayTroopKnowledges currentPlayer troops
    | WonLaneKnowledge {Controller = controller; Troops = troops} ->
        printfn "Lane %i (won by player %i)" laneID controller
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
    |> Map.iter (displayPreLaneKnowledge currentPlayer)

let private displayPostLaneKnowledges currentPlayer laneKnowledges =
    laneKnowledges
    |> Map.iter (displayPostLaneKnowledge currentPlayer)

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
        "Activate inactive card " + string position
        + " in lane " + string laneID
    | TurnActionInfo (SingleAttack (_, laneID, singleAttackerPosition, targetInfo)) ->
        let attackerText =
            "Active card " + string singleAttackerPosition
        let targetText =
            match targetInfo with
            | InactiveTarget (pid, pos) ->
                "player " + string pid + "'s"
                + " inactive card " + string pos
                + " in lane " + string laneID
            | ActiveSingleTarget (pid, pos) ->
                "player " + string pid + "'s"
                + " active card " + string pos
                + " in lane " + string laneID
            | ActivePairMemberTarget (pid, pos, pairUnitPos) ->
                "player " + string pid + "'s"
                + " pair " + string pos + " member " + string pairUnitPos
                + " in lane " + string laneID
        attackerText + " attacks " + targetText
    | TurnActionInfo (PairAttack (_, laneID, attackerPairPosition, targetInfo)) ->
        let attackerText =
            "Pair " + string attackerPairPosition
        let targetText =
            match targetInfo with
            | InactiveTarget (pid, pos) ->
                "player " + string pid + "'s"
                + " inactive card " + string pos
                + " in lane " + string laneID
            | ActiveSingleTarget (pid, pos) ->
                "player " + string pid + "'s"
                + " active card " + string pos
                + " in lane " + string laneID
            | ActivePairMemberTarget (pid, pos, pairUnitPos) ->
                "player " + string pid + "'s"
                + " pair " + string pos + " member " + string pairUnitPos
                + " in lane " + string laneID
        attackerText + " attacks " + targetText
    | TurnActionInfo (CreatePair (_, laneID, position1, position2)) ->
        "Create pair"
        + " in lane " + string laneID
        + " from cards " + string position1 + " and " + string position2
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
