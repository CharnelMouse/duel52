module ConsoleUI
open System
open Domain

let private readiness (actions: Actions) =
    if actions = 0u<action> then
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

let private deparseSuit = function
| Spades -> 'S'
| Diamonds -> 'D'
| Clubs -> 'C'
| Hearts -> 'H'

let private displayBaseKnowledge baseKnowledge =
    match baseKnowledge with
    | UnknownBaseCard _ ->
        printf "?"
    | KnownBaseCard (_, _, rank, _, _, _) ->
        printf "%c" (deparseRank rank)

let private displayInactiveUnitKnowledge (inactiveUnitKnowledge: InactiveUnitKnowledge) =
    match inactiveUnitKnowledge with
    | UnknownInactiveCardKnowledge (cardID, damage, actionability) ->
        printf "%i: " cardID
        match actionability with
        | Normal -> ()
        | Frozen -> printf "Frozen "
        match damage with
        | 0u<health> -> printfn "Inactive"
        | d -> printfn "Inactive, %i damage" d
    | KnownInactiveCardKnowledge (cardID, rank, suit, abilities, damage, actionability) ->
        printf "%i: " cardID
        match actionability with
        | Normal -> ()
        | Frozen -> printf "Frozen "
        match damage with
        | 0u<health> -> printfn "Inactive %c" (deparseRank rank)
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
        | 0u<health> -> printfn "%A %c" (readiness actionsLeft) (deparseRank rank)
        | d -> printfn "%A %c, %i damage" (readiness actionsLeft) (deparseRank rank) d
    else
        match damage with
        | 0u<health> -> printfn "%c" (deparseRank rank)
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
        | 0u<health>, 0u<health> -> printfn "%A %c pair" (readiness actionsLeft) (deparseRank rank)
        | d1, d2 -> printfn "%A %c pair: damage %i and %i" (readiness actionsLeft) (deparseRank rank) d1 d2
    else
        match damage1, damage2 with
        | 0u<health>, 0u<health> -> printfn "%c pair" (deparseRank rank)
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

let private displayAbilityChoiceContext context =
    printf "Current choice: "
    match context with
    | DiscardChoiceContext cardID ->
        printfn "card %i: discard a card" cardID
    | ViewInactiveChoiceContext cardID ->
        printfn "card %i: view a face-down card" cardID
    | MayMoveAllyToOwnLaneChoiceContext (laneID, cardID) ->
        printfn "card %i: move a card to lane %i" cardID laneID
    | DamageExtraTargetChoiceContext (laneID, unitCardIDs, _) ->
        match unitCardIDs with
        | SingleCardID cardID ->
            printfn "card %i: damage a card in lane %i" cardID laneID
        | PairIDs (cardID1, cardID2) ->
            printfn "pair %i, %i: damage a card in lane %i" cardID1 cardID2 laneID
    | ReturnDamagePairChoiceContext (_, (cardID1, cardID2), targetCardID) ->
        printfn "pair %i, %i: one takes Retaliate damage from card %i" cardID1 cardID2 targetCardID

let private displayEvent = function
| DisplayGameStarted -> printfn "Game started"
| DisplayTurnStarted pid -> printfn "Player %i starts their turn" pid
| DisplayTurnEnded pid -> printfn "Player %i ends their turn" pid
| DisplayAbilityChoiceMade (pid, aci) ->
    match aci with
    | DiscardChoice (discarderCardID, discardeeCardID) ->
        printfn "Card %i: Player %i discards card %i" discarderCardID pid discardeeCardID
    | ViewInactiveChoice (viewerCardID, vieweeCardID) ->
        printfn "Card %i: Player %i views card %i" viewerCardID pid vieweeCardID
    | MayMoveAllyToOwnLaneChoice None ->
        printfn "Player %i doesn't move a card" pid
    | MayMoveAllyToOwnLaneChoice (Some (destinationLaneID, moverID, originLaneID, moveeCardID)) ->
        printfn "Card %i: Player %i moves card %i from lane %i to lane %i" moverID pid moveeCardID originLaneID destinationLaneID
    | DamageExtraTargetChoice (laneID, (SingleCardID attackerCardID), defenderCardID) ->
        printfn "Player %i: Card %i deals extra damage to %i" pid attackerCardID defenderCardID
    | DamageExtraTargetChoice (laneID, (PairIDs (attackerCardID1, attackerCardID2)), defenderCardID) ->
        printfn "Player %i: Card pair %i, %i deals extra damage to %i" pid attackerCardID1 attackerCardID2 defenderCardID
    | ReturnDamagePairChoice (laneID, returneeUnitIDs, returnerCardID, returneeCardID) ->
        printfn "Player %i: Card %i returns damage to Card %i" pid returnerCardID returneeCardID
| DisplayActionChosen (pid, action) ->
    match action with
    | Play (laneID, cardID) ->
        printfn "Player %i plays card %i to lane %i" pid cardID laneID
    | Activate (laneID, cardID) ->
        printfn "Player %i activates card %i" pid cardID
    | SingleAttack (laneID, attackerCardID, defenderInfo) ->
        match defenderInfo with
        | InactiveTarget (targetOwnerID, targetCardID)
        | ActiveSingleTarget (targetOwnerID, targetCardID)
        | ActivePairMemberTarget (targetOwnerID, targetCardID) ->
            printfn "Player %i: card %i attacks player %i's card %i" pid attackerCardID targetOwnerID targetCardID
    | PairAttack (laneID, (attackerCardID1, attackerCardID2), defenderInfo) ->
        match defenderInfo with
        | InactiveTarget (targetOwnerID, targetCardID)
        | ActiveSingleTarget (targetOwnerID, targetCardID)
        | ActivePairMemberTarget (targetOwnerID, targetCardID) ->
            printfn "Player %i: card pair %i, %i attacks player %i's card %i" pid attackerCardID1 attackerCardID2 targetOwnerID targetCardID
    | CreatePair (laneID, pairCardID1, pairCardID2) ->
        printfn "Player %i creates a pair from cards %i and %i" pid pairCardID1 pairCardID2
| DisplayStackChoiceMade (pid, (laneID, cardID, (PowerName powerName))) ->
    printfn "Player %i triggers card %i's %s power" pid cardID powerName
| DisplayCardPlayed (pid, cardID, laneID) ->
    printfn "Player %i plays card %i to lane %i" pid cardID laneID
| DisplayCardActivated (pid, cardID, rank, suit, (PowerName powerName)) ->
    let rankChar = deparseRank rank
    let suitChar = deparseSuit suit
    printfn "Player %i activates card %i: %c%c (%s)" pid cardID rankChar suitChar powerName
| DisplayCardAttacked (attackerOwner, attackers, targetOwner, target) ->
    match attackers with
    | SingleCardID attackerID ->
        printfn "Player %i: card %i attacks player %i's card %i" attackerOwner attackerID targetOwner target
    | PairIDs (attackerID1, attackerID2) ->
        printfn "Player %i: pair %i, %i attacks player %i's card %i" attackerOwner attackerID1 attackerID2 targetOwner target
| DisplayCardDamaged (damagedOwner, damaged, damage) ->
    printfn "Player %i's card %i takes %i damage" damagedOwner damaged damage
| DisplayCardsPaired (owner, cardID1, cardID2, suit1, suit2, rank, (PowerName powerName)) ->
    let suitChar1 = deparseSuit suit1
    let suitChar2 = deparseSuit suit2
    let rankChar = deparseRank rank
    printfn "Player %i: %c%c and %c%c form a %s pair" owner rankChar suitChar1 rankChar suitChar2 powerName
| DisplayActionsGained (player, actions) ->
    printfn "Player %i gains %i actions" player actions
| DisplayAttacksSet (player, cardID, rank, suit, (PowerName powerName), attacks) ->
    printfn "Card %c%c (%s) can attack %i times in total this turn" (deparseRank rank) (deparseSuit suit) powerName attacks
| DisplayCannotDraw player ->
    printfn "Player %i can't draw: drawpile exhausted" player
| DisplayCardDrawn player ->
    printfn "Player %i draws a card" player
| DisplayDrawPileExhausted ->
    printfn "Draw pile exhausted, bases are now normal cards"
| DisplayLaneFrozen lane ->
    printfn "Lane %i is frozen" lane
| DisplayCardFrozen (rank, suit, PowerName powerName, owner) ->
    printfn "Player %i's %c%c (%s) is frozen" owner (deparseRank rank) (deparseSuit suit) powerName
| DisplayCardHealed (rank, suit, (PowerName powerName), damage) ->
    printfn "%c%c (%s) is healed, %i damage remaining" (deparseRank rank) (deparseSuit suit) powerName damage
| DisplayCardReactivated (rank, suit, (PowerName powerName)) ->
    printfn "%c%c (%s) reactivated" (deparseRank rank) (deparseSuit suit) powerName
| DisplayCardFullyHealedSelf (rank, suit, (PowerName powerName)) ->
    printfn "%c%c (%s) fully healed itself" (deparseRank rank) (deparseSuit suit) powerName
| DisplayCardHealedSelf (rank, suit, (PowerName powerName), damage) ->
    printfn "%c%c (%s) heals itself, %i damage remaining" (deparseRank rank) (deparseSuit suit) powerName damage
| DisplayCardActivatedSelf (cardID, rank, suit, (PowerName powerName)) ->
    let rankChar = deparseRank rank
    let suitChar = deparseSuit suit
    printfn "Card %i activates itself: %c%c (%s)" cardID rankChar suitChar powerName

let private displayEvents = List.iter displayEvent

let private displayOngoingGameInfo displayInfo =
    match displayInfo with
    | AbilityChoiceDisplayInfo acdi ->
        match acdi.BoardKnowledge with
        | PreBaseFlipBoardKnowledge {Lanes = lanes} ->
            displayPreLaneKnowledges acdi.CurrentPlayer lanes
        | PostBaseFlipBoardKnowledge {Lanes = lanes} ->
            displayPostLaneKnowledges acdi.CurrentPlayer lanes
        printfn "Player %i's turn, %i actions left\n" acdi.CurrentPlayer acdi.ActionsLeft
        if List.isEmpty acdi.PlayerHand then
            printfn "Hand is empty"
        else
            printf "Hand: "
            List.iter displayHandCard acdi.PlayerHand
        printfn ""
        match acdi.OpponentHandSizes with
        | [] -> ()
        | [h] -> displayOpponentHandSize h
        | _ ->
            printfn "Opponent hand sizes"
            List.iter displayOpponentHandSize acdi.OpponentHandSizes
        printfn ""
        match acdi.BoardKnowledge with
        | PreBaseFlipBoardKnowledge {DrawPileSize = dps; Discard = dk} ->
            printfn "Draw pile: %i" dps
            displayDiscardKnowledge dk
        | PostBaseFlipBoardKnowledge {Discard = dk} ->
            displayDiscardKnowledge dk
        printfn ""
        displayAbilityChoiceContext acdi.ChoiceContext
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

let private actionString actionInfo =
    match actionInfo with
    | AbilityChoiceInfo (DiscardChoice (powerCardID, discardeeCardID)) ->
        string powerCardID + ": discard card " + string discardeeCardID
    | AbilityChoiceInfo (ViewInactiveChoice (powerCardID, hiddenCardID)) ->
        string powerCardID + ": view card " + string hiddenCardID
    | AbilityChoiceInfo (MayMoveAllyToOwnLaneChoice maybeMove) ->
        match maybeMove with
        | Some (toLaneID, powerCardID, _, targetCardID) ->
            string powerCardID + ": move card " + string targetCardID + " to lane " + string toLaneID
        | None ->
            "Move nothing"
    | AbilityChoiceInfo (DamageExtraTargetChoice (_, powerCardIDs, targetCardID)) ->
        let powerCardsString =
            match powerCardIDs with
            | SingleCardID id -> string id
            | PairIDs (id1, id2) -> string id1 + ", " + string id2
        powerCardsString + ": damage card " + string targetCardID
    | AbilityChoiceInfo (ReturnDamagePairChoice (_, _, targetCardID, choiceID)) ->
        "card " + string choiceID + " takes Retaliate damage from card " + string targetCardID
    | StackChoiceInfo (_, (_, cardID, (PowerName powerName))) ->
        string cardID + ": " + powerName
    | TurnActionInfo (ActionChoiceInfo (Play (laneID, cardID))) ->
        "Play card " + string cardID
        + " to lane " + string laneID
    | TurnActionInfo (ActionChoiceInfo (Activate (laneID, cardID))) ->
        "Activate inactive card " + string cardID
        + " in lane " + string laneID
    | TurnActionInfo (ActionChoiceInfo (SingleAttack (laneID, attackerID, targetInfo))) ->
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
    | TurnActionInfo (ActionChoiceInfo (PairAttack (laneID, (attackerID1, attackerID2), targetInfo))) ->
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
    | TurnActionInfo (ActionChoiceInfo (CreatePair (laneID, cardID1, cardID2))) ->
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
    | InProgress (events, displayInfo, nextActions) ->
        displayEvents events
        printf "Press key to continue"
        Console.ReadKey() |> ignore
        Console.Clear()
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
