module ConsoleUI
open System
open Domain

let private readiness (actions: Actions) =
    if actions <= 0<action> then
        Exhausted
    else
        Ready

let private deparseRank rank =
    match rank with
    | Two -> '2'
    | Three -> '3'
    | Four -> '4'
    | Five -> '5'
    | Six -> '6'
    | Seven -> '7'
    | Eight -> '8'
    | Nine -> '9'
    | Ten -> 'X'
    | Jack -> 'J'
    | Queen -> 'Q'
    | King -> 'K'
    | Ace -> 'A'

let private displayBaseKnowledge baseKnowledge =
    match baseKnowledge with
    | UnknownBaseCard playerID ->
        printf "?"
    | KnownBaseCard (playerID, rank, suit, abilities) ->
        printf "%c" (deparseRank rank)

let private displayInactiveUnitKnowledge (inactiveUnitKnowledge: InactiveUnitKnowledge) =
    match inactiveUnitKnowledge with
    | UnknownInactiveCardKnowledge (cardID, damage, actionability) ->
        printf "%i: " cardID
        match actionability with
        | Normal -> ()
        | Frozen -> printf "Frozen "
        match damage with
        | 0<health> -> printfn "Inactive"
        | d -> printfn "Inactive, %i damage" d
    | KnownInactiveCardKnowledge (cardID, rank, suit, abilities, damage, actionability) ->
        printf "%i: " cardID
        match actionability with
        | Normal -> ()
        | Frozen -> printf "Frozen "
        match damage with
        | 0<health> -> printfn "Inactive %c" (deparseRank rank)
        | d -> printfn "Inactive %c, %i damage" (deparseRank rank) d
    // later will need cases for active/pair when card is frozen

let private displayActiveUnitKnowledge currentPlayer ownerID (activeUnitKnowledge: ActiveUnitKnowledge) =
    let (cardID, rank, suit, abilities, damage, actionsLeft, actionability) = activeUnitKnowledge
    printf "%i: " cardID
    match actionability with
    | Normal -> ()
    | Frozen -> printf "Frozen "
    if ownerID = currentPlayer then
        match damage with
        | 0<health> -> printfn "%A %c" (readiness actionsLeft) (deparseRank rank)
        | d -> printfn "%A %c, %i damage" (readiness actionsLeft) (deparseRank rank) d
    else
        match damage with
        | 0<health> -> printfn "%c" (deparseRank rank)
        | d -> printfn "%c, %i damage" (deparseRank rank) d

let private displayPairKnowledge currentPlayer ownerID (pairKnowledge: PairKnowledge) =
    let (cardID1, cardID2, rank, suit1, suit2, abilities, damage1, damage2, actionsLeft, actionability1, actionability2) = pairKnowledge
    printf "%i, %i: " cardID1 cardID2
    match actionability1, actionability2 with
    | Normal, Normal -> ()
    | Normal, Frozen
    | Frozen, Normal
    | Frozen, Frozen -> printf "Frozen "
    if ownerID = currentPlayer then
        match damage1, damage2 with
        | 0<health>, 0<health> -> printfn "%A %c pair" (readiness actionsLeft) (deparseRank rank)
        | d1, d2 -> printfn "%A %c pair: damage %i and %i" (readiness actionsLeft) (deparseRank rank) d1 d2
    else
        match damage1, damage2 with
        | 0<health>, 0<health> -> printfn "%c pair" (deparseRank rank)
        | d1, d2 -> printfn "%c pair: damage %i and %i" (deparseRank rank) d1 d2

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

let private displayHandCard (HandCardInfo (cardID, rank, suit, abilities)) =
    printf "%i:%c " (int cardID) (deparseRank rank)

let private displayOpponentHandSize (id, size) =
    if size = 0 then
        printfn "Player %i's hand is empty" id
    else
        printfn "Player %i has %i cards" id size

let private displayDeadCard deadCard =
    match deadCard with
    | UnknownDeadCard -> printfn "unknown card"
    | KnownFaceDownDeadCard (rank, suit) -> printfn "%c, face-down" (deparseRank rank)
    | KnownFaceUpDeadCard (rank, suit) -> printfn "%c, face-up" (deparseRank rank)

let private displayDiscardKnowledge discardKnowledge =
    if List.isEmpty discardKnowledge then
        printfn "Discard pile is empty"
    else
        printfn "\nDiscard pile:"
        discardKnowledge
        |> List.iter displayDeadCard

let private displayMidActivationPowerChoiceContext context =
    printf "Current choice: "
    match context with
    | DiscardChoiceContext (_, cardID) ->
        printfn "card %i: discard a card" cardID
    | ForesightChoiceContext (_, cardID) ->
        printfn "card %i: view a face-down card" cardID
    | MoveChoiceContext (_, laneID, cardID) ->
        printfn "card %i: move a card to lane %i" cardID laneID

let private displayMidPassivePowerChoiceContext context =
    printf "Current choice: "
    match context with
    | TwinStrikeChoiceContext (_, laneID, unitCardIDs, _) ->
        match unitCardIDs with
        | SingleCardID cardID ->
            printfn "card %i: damage a card in lane %i" cardID laneID
        | PairIDs (cardID1, cardID2) ->
            printfn "pair %i, %i: damage a card in lane %i" cardID1 cardID2 laneID
    | TwinStrikeRelatiatePairChoiceContext (_, _, (cardID1, cardID2), targetCardID) ->
        printfn "pair %i, %i: one takes Retaliate damage from card %i" cardID1 cardID2 targetCardID

let private displayOngoingGameInfo displayInfo =
    Console.Clear()
    match displayInfo with
    | MidActivationPowerChoiceDisplayInfo macdi ->
        match macdi.BoardKnowledge with
        | PreBaseFlipBoardKnowledge {Lanes = lanes} ->
            displayPreLaneKnowledges macdi.CurrentPlayer lanes
        | PostBaseFlipBoardKnowledge {Lanes = lanes} ->
            displayPostLaneKnowledges macdi.CurrentPlayer lanes
        printfn "Player %i's turn, %i actions left\n" macdi.CurrentPlayer macdi.ActionsLeft
        if List.isEmpty macdi.PlayerHand then
            printfn "Hand is empty"
        else
            printf "Hand: "
            List.iter displayHandCard macdi.PlayerHand
        printfn ""
        match macdi.OpponentHandSizes with
        | [] -> ()
        | [h] -> displayOpponentHandSize h
        | _ ->
            printfn "Opponent hand sizes"
            List.iter displayOpponentHandSize macdi.OpponentHandSizes
        printfn ""
        match macdi.BoardKnowledge with
        | PreBaseFlipBoardKnowledge {DrawPileSize = dps; Discard = dk} ->
            printfn "Draw pile: %i" dps
            displayDiscardKnowledge dk
        | PostBaseFlipBoardKnowledge {Discard = dk} ->
            displayDiscardKnowledge dk
        printfn ""
        displayMidActivationPowerChoiceContext macdi.ChoiceContext
    | MidPassivePowerChoiceDisplayInfo macdi ->
        match macdi.BoardKnowledge with
        | PreBaseFlipBoardKnowledge {Lanes = lanes} ->
            displayPreLaneKnowledges macdi.CurrentPlayer lanes
        | PostBaseFlipBoardKnowledge {Lanes = lanes} ->
            displayPostLaneKnowledges macdi.CurrentPlayer lanes
        printfn "Player %i's turn, %i actions left\n" macdi.CurrentPlayer macdi.ActionsLeft
        if List.isEmpty macdi.PlayerHand then
            printfn "Hand is empty"
        else
            printf "Hand: "
            List.iter displayHandCard macdi.PlayerHand
        printfn ""
        match macdi.OpponentHandSizes with
        | [] -> ()
        | [h] -> displayOpponentHandSize h
        | _ ->
            printfn "Opponent hand sizes"
            List.iter displayOpponentHandSize macdi.OpponentHandSizes
        printfn ""
        match macdi.BoardKnowledge with
        | PreBaseFlipBoardKnowledge {DrawPileSize = dps; Discard = dk} ->
            printfn "Draw pile: %i" dps
            displayDiscardKnowledge dk
        | PostBaseFlipBoardKnowledge {Discard = dk} ->
            displayDiscardKnowledge dk
        printfn ""
        displayMidPassivePowerChoiceContext macdi.ChoiceContext
    | StackChoiceDisplayInfo scdi ->
        match scdi.BoardKnowledge with
        | PreBaseFlipBoardKnowledge {Lanes = lanes} ->
            displayPreLaneKnowledges scdi.CurrentPlayer lanes
        | PostBaseFlipBoardKnowledge {Lanes = lanes} ->
            displayPostLaneKnowledges scdi.CurrentPlayer lanes
        printfn "Player %i's turn, %i actions left\n" scdi.CurrentPlayer scdi.ActionsLeft
        if List.isEmpty scdi.PlayerHand then
            printfn "Hand is empty"
        else
            printf "Hand: "
            List.iter displayHandCard scdi.PlayerHand
        printfn ""
        match scdi.OpponentHandSizes with
        | [] -> ()
        | [h] -> displayOpponentHandSize h
        | _ ->
            printfn "Opponent hand sizes"
            List.iter displayOpponentHandSize scdi.OpponentHandSizes
        printfn ""
        match scdi.BoardKnowledge with
        | PreBaseFlipBoardKnowledge {DrawPileSize = dps; Discard = dk} ->
            printfn "Draw pile: %i" dps
            displayDiscardKnowledge dk
        | PostBaseFlipBoardKnowledge {Discard = dk} ->
            displayDiscardKnowledge dk
    | TurnDisplayInfo tdi ->
        match tdi.BoardKnowledge with
        | PreBaseFlipBoardKnowledge {Lanes = lanes} ->
            displayPreLaneKnowledges tdi.CurrentPlayer lanes
        | PostBaseFlipBoardKnowledge {Lanes = lanes} ->
            displayPostLaneKnowledges tdi.CurrentPlayer lanes
        printfn "Player %i's turn, %i actions left\n" tdi.CurrentPlayer tdi.ActionsLeft
        if List.isEmpty tdi.PlayerHand then
            printfn "Hand is empty"
        else
            printf "Hand: "
            List.iter displayHandCard tdi.PlayerHand
        printfn ""
        match tdi.OpponentHandSizes with
        | [] -> ()
        | [h] -> displayOpponentHandSize h
        | _ ->
            printfn "Opponent hand sizes"
            List.iter displayOpponentHandSize tdi.OpponentHandSizes
        printfn ""
        match tdi.BoardKnowledge with
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
    | MidActivationPowerChoiceInfo (DiscardChoice (_, powerCardID, discardeeCardID)) ->
        string powerCardID + ": discard card " + string discardeeCardID
    | MidActivationPowerChoiceInfo (ForesightChoice (_, powerCardID, hiddenCardID)) ->
        string powerCardID + ": view card " + string hiddenCardID
    | MidActivationPowerChoiceInfo (MoveChoice maybeMove) ->
        match maybeMove with
        | Some (_, toLaneID, powerCardID, _, targetCardID) ->
            string powerCardID + ": move card " + string targetCardID + " to lane " + string toLaneID
        | None ->
            "Move nothing"
    | MidPassivePowerChoiceInfo (TwinStrikeChoice (_, _, powerCardIDs, targetCardID)) ->
        let powerCardsString =
            match powerCardIDs with
            | SingleCardID id -> string id
            | PairIDs (id1, id2) -> string id1 + ", " + string id2
        powerCardsString + ": damage card " + string targetCardID
    | MidPassivePowerChoiceInfo (TwinStrikeRetaliatePairChoice (_, _, _, targetCardID, choiceID)) ->
        "card " + string choiceID + " takes Retaliate damage from card " + string targetCardID
    | StackChoiceInfo (_, _, action) ->
        match action with
        | ViewPowerContext (_, _, cardID) -> string cardID + ": View"
        | ForesightPowerContext (_, _, cardID) -> string cardID + ": Foresight"
        | FlipPowerContext (_, laneID, cardID) -> string cardID + ": Flip in lane " + string laneID
        | FreezePowerContext (_, laneID, cardID) -> string cardID + ": Freeze lane " + string laneID
        | HealPowerContext (_, _, cardID) -> string cardID + ": Heal"
        | MovePowerContext (_, laneID, cardID) -> string cardID + ": Move to lane " + string laneID
        | EmpowerPowerContext (_, laneID, cardID) -> string cardID + ": Empower in lane " + string laneID
        | ActionPowerContext (_, _, cardID) -> string cardID + ": Action"
    | TurnActionInfo (ActionChoiceInfo (Play (_, cardID, laneID))) ->
        "Play card " + string cardID
        + " to lane " + string laneID
    | TurnActionInfo (ActionChoiceInfo (Activate (_, laneID, cardID))) ->
        "Activate inactive card " + string cardID
        + " in lane " + string laneID
    | TurnActionInfo (ActionChoiceInfo (SingleAttack (_, laneID, attackerID, targetInfo))) ->
        let attackerText =
            "Active card " + string attackerID
        let targetText =
            match targetInfo with
            | InactiveTarget (pid, cardID) ->
                "player " + string pid + "'s"
                + " inactive card " + string cardID
                + " in lane " + string laneID
            | ActiveSingleTarget (pid, cardID) ->
                "player " + string pid + "'s"
                + " active card " + string cardID
                + " in lane " + string laneID
            | ActivePairMemberTarget (pid, cardID) ->
                "player " + string pid + "'s"
                + " paired card " + string cardID
                + " in lane " + string laneID
        attackerText + " attacks " + targetText
    | TurnActionInfo (ActionChoiceInfo (PairAttack (_, laneID, (attackerID1, attackerID2), targetInfo))) ->
        let attackerText =
            "Paired cards " + string attackerID1 + " and " + string attackerID2
        let targetText =
            match targetInfo with
            | InactiveTarget (pid, cardID) ->
                "player " + string pid + "'s"
                + " inactive card " + string cardID
                + " in lane " + string laneID
            | ActiveSingleTarget (pid, cardID) ->
                "player " + string pid + "'s"
                + " active card " + string cardID
                + " in lane " + string laneID
            | ActivePairMemberTarget (pid, cardID) ->
                "player " + string pid + "'s"
                + " paired card " + string cardID
                + " in lane " + string laneID
        attackerText + " attacks " + targetText
    | TurnActionInfo (ActionChoiceInfo (CreatePair (_, laneID, cardID1, cardID2))) ->
        "Create pair"
        + " in lane " + string laneID
        + " from cards " + string cardID1 + " and " + string cardID2
    | TurnActionInfo (EndTurn _) ->
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
