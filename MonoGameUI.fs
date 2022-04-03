module MonoGameUI
open PointFree
open Domain
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

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

type ActionAreas = Map<ActionInfo, Rectangle>
type LaneAreas = Map<LaneID * PlayerID, Rectangle * Point list>

type MouseStatus =
| Idle
| Dragging of Point
| Dragged of Point * Point

let limits length cardLength spacing =
    (length + spacing)/(cardLength + spacing)

let updateMouseStatus (mouse: MouseState) = function
| Idle ->
    match mouse.LeftButton with
    | ButtonState.Pressed -> Dragging mouse.Position
    | ButtonState.Released -> Idle
    | _ -> Idle
| Dragging origin ->
    match mouse.LeftButton with
    | ButtonState.Pressed -> Dragging origin
    | ButtonState.Released -> Dragged (origin, mouse.Position)
    | _ -> Dragging origin
| Dragged (origin, destination) ->
    Dragged (origin, destination)

let resetDraggedMouseState = function
| Idle -> Idle
| Dragging origin -> Dragging origin
| Dragged _ -> Idle

let duel52Colours = {|
    LightBrown = Color(237, 228, 217)
    DarkBrown = Color(219, 201, 179)
|}
let nLanes = 3
let nPlayers = 2
let windowSize = Point(800, 600)
let cardSize = Point(50, 70)
let statusBarHeight = 50
let sideBarWidth = cardSize.X
let cardSpacing = 5

type CardColours = {
    Background: Color
    Damage: Color
    Actions: Color
    FrozenBackground: Color
    Text: Color
    FrozenText: Color
}
let baseCardColours = {
    Background = Color.Black
    Damage = Color.Red
    Actions = Color.Blue
    FrozenBackground = Color.DarkBlue
    Text = Color.White
    FrozenText = Color.White
}
let inactiveCardColours = {
    Background = Color.Gray
    Damage = Color.Red
    Actions = Color.Blue
    FrozenBackground = Color.MediumBlue
    Text = Color.White
    FrozenText = Color.White
}
let activeCardColours = {
    Background = Color.White
    Damage = Color.Red
    Actions = Color.Blue
    FrozenBackground = Color.DeepSkyBlue
    Text = Color.Black
    FrozenText = Color.Black
}
let pairedCardColours = {
    Background = Color.White
    Damage = Color.Red
    Actions = Color.Blue
    FrozenBackground = Color.DeepSkyBlue
    Text = Color.Black
    FrozenText = Color.Black
}

let typeRectangle (spriteBatch: SpriteBatch) (font: SpriteFont) (string: string) (rect: Rectangle) colour =
    let centre = font.MeasureString(string)
    spriteBatch.DrawString(font, string, rect.Center.ToVector2() - centre/2f, colour)
let drawRectangle (spriteBatch: SpriteBatch) pixel (rect: Rectangle) colour =
    spriteBatch.Draw(pixel, rect, colour)
let drawOutline (spriteBatch: SpriteBatch) pixel colour (rect: Rectangle) =
    spriteBatch.Draw(pixel, Rectangle(rect.Location, Point(rect.Width, 1)), colour)
    spriteBatch.Draw(pixel, Rectangle(Point(rect.Left, rect.Bottom - 1), Point(rect.Width, 1)), colour)
    spriteBatch.Draw(pixel, Rectangle(rect.Location, Point(1, rect.Height)), colour)
    spriteBatch.Draw(pixel, Rectangle(Point(rect.Right - 1, rect.Top), Point(1, rect.Height)), colour)

let drawCard
    spriteBatch pixel stateFont (cardSize: Point)
    pos text (damage: Damage) actionsLeft actionability
    (colours: CardColours) =
    let rect = Rectangle(pos, cardSize)
    let mainColour =
        match actionability with
        | Normal  -> colours.Background
        | Frozen -> colours.FrozenBackground
    drawRectangle spriteBatch pixel rect mainColour
    typeRectangle spriteBatch stateFont text rect colours.Text
    if damage <> 0u<health> then
        let damageStringSize = stateFont.MeasureString(string damage)
        typeRectangle
            spriteBatch stateFont
            (string damage)
            (Rectangle(Point(rect.Right, rect.Bottom) - damageStringSize.ToPoint(), damageStringSize.ToPoint()))
            colours.Damage
    if actionsLeft <> 0u<action> then
        let actionStringSize = stateFont.MeasureString(string actionsLeft)
        typeRectangle
            spriteBatch stateFont
            (string actionsLeft)
            (Rectangle(Point(rect.Left, rect.Bottom) - Point(0, int actionStringSize.Y), actionStringSize.ToPoint()))
            colours.Actions
let typeStatus spriteBatch stateFont statusBarCentre status =
    typeRectangle spriteBatch stateFont status statusBarCentre Color.Black

let drawStatusBar (spriteBatch: SpriteBatch) pixel stateFont statusBarArea statusText =
    drawRectangle spriteBatch pixel statusBarArea duel52Colours.DarkBrown
    typeStatus spriteBatch stateFont statusBarArea statusText
let drawSwitchButton (spriteBatch: SpriteBatch) (stateFont: SpriteFont) pixel switchPanelArea (screenCentre: Point) =
    drawRectangle spriteBatch pixel switchPanelArea duel52Colours.DarkBrown
    typeStatus spriteBatch stateFont switchPanelArea "OK"
let drawEndTurnButton (spriteBatch: SpriteBatch) pixel stateFont (endTurnButtonArea: Rectangle) =
    drawRectangle spriteBatch pixel (Rectangle(endTurnButtonArea.Location - Point(1, 0), Point(1, endTurnButtonArea.Height))) Color.Black
    typeStatus spriteBatch stateFont endTurnButtonArea "End turn"
let drawNoTargetButton (spriteBatch: SpriteBatch) pixel stateFont (noTargetButtonArea: Rectangle) =
    drawRectangle spriteBatch pixel (Rectangle(Point(noTargetButtonArea.Right, noTargetButtonArea.Top), Point(1, noTargetButtonArea.Height))) Color.Black
    typeStatus spriteBatch stateFont noTargetButtonArea "No target"

let drawStatusBorder spriteBatch pixel (statusBarArea: Rectangle) =
    drawRectangle
        spriteBatch pixel
        (Rectangle(Point(0, statusBarArea.Bottom), Point(statusBarArea.Width, 1)))
        Color.Black
let drawSidebarBorder spriteBatch pixel (boardArea: Rectangle) =
    drawRectangle
        spriteBatch pixel
        (Rectangle(Point(boardArea.Right, boardArea.Top), Point(1, boardArea.Height)))
        Color.Black
let drawHandBorder spriteBatch pixel (handArea: Rectangle) =
    drawRectangle
        spriteBatch pixel
        (Rectangle(Point(0, handArea.Top - 1), Point(handArea.Width, 1)))
        Color.Black
let drawSideBarSplit spriteBatch pixel (deckArea: Rectangle) =
    drawRectangle
        spriteBatch pixel
        (Rectangle(Point(deckArea.Left, deckArea.Bottom), Point(deckArea.Width, 1)))
        Color.Black
let drawBoardInnerBorders spriteBatch pixel (boardArea: Rectangle) laneStartsX laneStartsY laneWidths laneHeights =
    List.zip laneStartsX laneWidths
    |> (fun lst -> List.removeAt (List.length lst - 1) lst)
    |> List.iter (fun (start, width) ->
        let laneBorder = start + width
        drawRectangle
            spriteBatch pixel
            (Rectangle(Point(laneBorder, boardArea.Top), Point(1, boardArea.Height)))
            Color.Black
        )
    List.zip laneStartsY laneHeights
    |> (fun lst -> List.removeAt (List.length lst - 1) lst)
    |> List.iter (fun (start, height) ->
        let playerBorder = start + height
        drawRectangle
            spriteBatch pixel
            (Rectangle(Point(boardArea.Left, playerBorder), Point(boardArea.Width, 1)))
            Color.Black
        )
let drawNonInnerBoardBorders
    spriteBatch pixel
    (statusBarArea: Rectangle) (boardArea: Rectangle) (handArea: Rectangle) (deckArea: Rectangle)
    =
    drawStatusBorder spriteBatch pixel statusBarArea
    drawSidebarBorder spriteBatch pixel boardArea
    drawHandBorder spriteBatch pixel handArea
    drawSideBarSplit spriteBatch pixel deckArea

let drawHand spriteBatch pixel stateFont (handArea: Rectangle) hand =
    [0..List.length hand - 1]
    |> List.iter (fun n ->
        let x = n*(cardSize.X + cardSpacing)
        let rect = (Rectangle(Point(x, handArea.Top), cardSize))
        let (HandCardInfo (_, rank, suit, _)) = hand[n]
        let text = string(deparseRank rank) + string(deparseSuit suit)
        drawRectangle spriteBatch pixel rect Color.White
        typeRectangle spriteBatch stateFont text rect Color.Black
        )

let drawBases spriteBatch pixel stateFont laneAreas lanes =
    lanes
    |> Map.iter (fun laneID lane ->
        let bases = lane.Bases
        bases
        |> List.indexed
        |> List.iter (fun (n, baseCard) ->
            let player = (uint n + 1u)*1u<PID>
            let location = Map.find (laneID, player) laneAreas |> snd |> List.head
            let cardText, actionability =
                match baseCard with
                | UnknownBaseCard (_, _, actionability) ->
                    "Base", actionability
                | KnownBaseCard (_, _, rank, suit, _, actionability) ->
                    string (deparseRank rank) + string (deparseSuit suit), actionability
            drawCard
                spriteBatch pixel stateFont cardSize
                location cardText 0u<health> 0u<action> actionability
                baseCardColours
            )
        )

let drawTroopsInLane spriteBatch pixel stateFont locations cards =
    let (inactives, actives, pairs) = cards
    if List.length inactives + List.length actives + List.length pairs > List.length locations then
        failwithf "Lane is too small on screen for all those cards!"
    List.zip locations[0..List.length inactives - 1] inactives
    |> List.iter (fun (pos, inactive) ->
        let cardText, damage, actionability =
            match inactive with
            | UnknownInactiveCardKnowledge (_, damage, actionability) ->
                "Inactive", damage, actionability
            | KnownInactiveCardKnowledge (_, rank, suit, _, damage, actionability) ->
                string (deparseRank rank) + string (deparseSuit suit), damage, actionability
        drawCard
            spriteBatch pixel stateFont cardSize
            pos cardText damage 0u<action> actionability
            inactiveCardColours
        )
    List.zip locations[List.length inactives..List.length inactives + List.length actives - 1] actives
    |> List.iter (fun (pos, (_, rank, suit, powerName, damage, actionsSpent, actionability)) ->
        let cardText = string (deparseRank rank) + string (deparseSuit suit)
        drawCard
            spriteBatch pixel stateFont cardSize
            pos cardText damage actionsSpent actionability
            activeCardColours
        )
    let locationsForPairs = locations[(List.length inactives + List.length actives)..]
    let pairLen = min (List.length locationsForPairs) (List.length pairs)
    List.zip (List.take pairLen locationsForPairs) (List.take pairLen pairs)
    |> List.iter (fun (pos, (_, _, rank, suit1, suit2, powerName, damage1, damage2, actions, actionability1, actionability2)) ->
        let cardText1 = string (deparseRank rank) + string (deparseSuit suit1)
        let cardText2 = string (deparseRank rank) + string (deparseSuit suit2)
        drawCard
            spriteBatch pixel stateFont (Point(cardSize.X, cardSize.Y/2))
            pos cardText1 damage1 actions actionability1
            pairedCardColours
        drawCard
            spriteBatch pixel stateFont (Point(cardSize.X, cardSize.Y/2))
            (pos + Point(0, cardSize.Y/2)) cardText2 damage2 actions actionability2
            pairedCardColours
        )

let drawPreFlipTroops spriteBatch pixel stateFont laneAreas (lanes: Map<LaneID, PreBaseFlipLaneKnowledge>) =
    lanes
    |> Map.iter (fun laneID lane ->
        let troops = lane.Troops
        troops
        |> Map.iter (fun playerID cards ->
            let locations = Map.find (laneID, playerID) laneAreas |> snd |> List.tail
            drawTroopsInLane spriteBatch pixel stateFont locations cards
            )
        )

let drawPostFlipTroops spriteBatch pixel stateFont laneAreas (lanes: Map<LaneID, PostBaseFlipLaneKnowledge>) =
    lanes
    |> Map.iter (fun laneID lane ->
        let troops =
            match lane with
            | ContestedLaneKnowledge {Troops =t} -> t
            | WonLaneKnowledge {Troops =t} -> t
        troops
        |> Map.iter (fun playerID cards ->
            let locations = Map.find (laneID, playerID) laneAreas |> snd
            drawTroopsInLane spriteBatch pixel stateFont locations cards
            )
        )

type ActionSelectionRectPair = Rectangle * Rectangle
type ActionSelectionAreas = (ActionInfo * ActionSelectionRectPair) list

let getBoardUnitAreas (laneAreas: LaneAreas) displayInfo =
    match displayInfo with
    | AbilityChoiceDisplayInfo {BoardKnowledge = bk}
    | StackChoiceDisplayInfo {BoardKnowledge = bk}
    | TurnDisplayInfo {BoardKnowledge = bk} ->
        match bk with
        | PreBaseFlipBoardKnowledge {Lanes = lanes} ->
            lanes
            |> Map.toList
            |> List.collect (fun (laneID, {Bases = bases; Troops = tk}) ->
                let baseAreas =
                    bases
                    |> List.map (fun baseCard ->
                        match baseCard with
                        | UnknownBaseCard (cardID, playerID, _)
                        | KnownBaseCard (cardID, playerID, _, _, _, _) ->
                            let areaPoint =
                                Map.find (laneID, playerID) laneAreas
                                |> snd
                                |> List.head
                            SingleCardID cardID, Rectangle(areaPoint, cardSize)
                        )
                let unitAreas =
                    tk
                    |> Map.toList
                    |> List.collect (fun (playerID, (inactives, actives, pairs)) ->
                        let possibleAreas =
                            Map.find (laneID, playerID) laneAreas
                            |> snd
                            |> List.tail
                        let inactiveIDs =
                            inactives
                            |> List.map (function
                                | UnknownInactiveCardKnowledge (cardID, _, _)
                                | KnownInactiveCardKnowledge (cardID, _, _, _, _, _) ->
                                    SingleCardID cardID
                                )
                        let activeIDs =
                            actives
                            |> List.map (fun (cardID, _, _, _, _, _, _) ->
                                    SingleCardID cardID
                                )
                        let pairIDs =
                            pairs
                            |> List.map (fun (cardID1, cardID2, _, _, _, _, _, _, _, _, _) ->
                                    PairIDs (cardID1, cardID2)
                                )
                        let unitIDs = inactiveIDs @ activeIDs @ pairIDs
                        let len = min (List.length unitIDs) (List.length possibleAreas)
                        List.zip
                            (List.take len unitIDs)
                            (possibleAreas |> List.take len |> List.map (fun p -> Rectangle(p, cardSize)))
                        )
                baseAreas @ unitAreas
                )
            |> Map.ofList
        | PostBaseFlipBoardKnowledge {Lanes = lanes} ->
            lanes
            |> Map.toList
            |> List.collect (fun (laneID, lane) ->
                let tk =
                    match lane with
                    | ContestedLaneKnowledge {Troops = tk}
                    | WonLaneKnowledge {Troops = tk} -> tk
                let unitAreas =
                    tk
                    |> Map.toList
                    |> List.collect (fun (playerID, (inactives, actives, pairs)) ->
                        let possibleAreas =
                            Map.find (laneID, playerID) laneAreas
                            |> snd
                        let inactiveIDs =
                            inactives
                            |> List.map (function
                                | UnknownInactiveCardKnowledge (cardID, _, _)
                                | KnownInactiveCardKnowledge (cardID, _, _, _, _, _) ->
                                    SingleCardID cardID
                                )
                        let activeIDs =
                            actives
                            |> List.map (fun (cardID, _, _, _, _, _, _) ->
                                    SingleCardID cardID
                                )
                        let pairIDs =
                            pairs
                            |> List.map (fun (cardID1, cardID2, _, _, _, _, _, _, _, _, _) ->
                                    PairIDs (cardID1, cardID2)
                                )
                        let unitIDs = inactiveIDs @ activeIDs @ pairIDs
                        let len = min (List.length unitIDs) (List.length possibleAreas)
                        List.zip
                            (List.take len unitIDs)
                            (possibleAreas |> List.take len |> List.map (fun p -> Rectangle(p, cardSize)))
                        )
                unitAreas
                )
            |> Map.ofList
    | SwitchDisplayInfo _
    | TiedGameDisplayInfo _
    | WonGameDisplayInfo _ ->
        Map.empty

let findBoardCardArea cardID (boardAreas: Map<UnitIDs, Rectangle>) =
    boardAreas
    |> Map.toList
    |> List.choose (fun (uids, rect) ->
        match uids with
        | SingleCardID cid when cid = cardID ->
            Some rect
        | PairIDs (cid1, cid2) when cid1 = cardID ->
            Some (Rectangle(rect.Location, Point(rect.Width, rect.Height/2)))
        | PairIDs (cid1, cid2) when cid2 = cardID ->
            Some (Rectangle(rect.Location + Point(0, rect.Height/2), Point(rect.Width, rect.Height/2)))
        | _ -> None
        )
    |> List.exactlyOne

let getActionSelectionAreas
    (handCardRects: Rectangle list) (laneAreas: Map<LaneID * PlayerID, _ * Point list>)
    switchPanelArea endTurnButtonArea noTargetButtonArea
    displayInfo action : ActionInfo * ActionSelectionRectPair =
    let boardAreas = getBoardUnitAreas laneAreas displayInfo
    action,
    match action with
    | AbilityChoiceInfo aci ->
        match aci with
        | DiscardChoice (_, discardee) ->
            match displayInfo with
            | AbilityChoiceDisplayInfo {PlayerHand = ph}
            | StackChoiceDisplayInfo {PlayerHand = ph}
            | TurnDisplayInfo {PlayerHand = ph} ->
                let discardeeHandPosition =
                    ph
                    |> List.findIndex (fun (HandCardInfo (cardID, _, _, _)) -> cardID = discardee)
                dup handCardRects[discardeeHandPosition]
            | SwitchDisplayInfo _
            | TiedGameDisplayInfo _
            | WonGameDisplayInfo _ ->
                failwithf "Can't discard outside of a player's turn"
        | ViewInactiveChoice (_, viewee) ->
            findBoardCardArea viewee boardAreas
            |> dup
        | MayMoveAllyToOwnLaneChoice choice ->
            match choice with
            | Some (_, _, _, pullee) ->
                findBoardCardArea pullee boardAreas
                |> dup
            | None ->
                dup noTargetButtonArea
        | DamageExtraTargetChoice (_, _, target) ->
            findBoardCardArea target boardAreas
            |> dup
        | ReturnDamagePairChoice (_, _, _, returnee) ->
            findBoardCardArea returnee boardAreas
            |> dup
    | StackChoiceInfo (_, (_, cardID, _)) ->
        Map.find (SingleCardID cardID) boardAreas
        |> dup
    | TurnActionInfo tai ->
        match tai with
        | ActionChoiceInfo aci ->
            match aci with
            | Play (laneID, playee) ->
                match displayInfo with
                | AbilityChoiceDisplayInfo _
                | StackChoiceDisplayInfo _ ->
                    failwithf "Can't be here!"
                | TurnDisplayInfo {CurrentPlayer = player; PlayerHand = ph} ->
                    let playeeHandPosition =
                        ph
                        |> List.findIndex (fun (HandCardInfo (cardID, _, _, _)) -> cardID = playee)
                    handCardRects[playeeHandPosition], fst (Map.find (laneID, player) laneAreas)
                | SwitchDisplayInfo _
                | TiedGameDisplayInfo _
                | WonGameDisplayInfo _ ->
                    failwithf "Can't play outside of a player's turn"
            | Activate (_, activatee) ->
                findBoardCardArea activatee boardAreas
                |> dup
            | SingleAttack (_, cardID, attackTargetInfo) ->
                let targetID =
                    match attackTargetInfo with
                    | InactiveTarget (_, targetID) -> targetID
                    | ActiveSingleTarget (_, targetID) -> targetID
                    | ActivePairMemberTarget (_, targetID) -> targetID
                boardAreas
                |> splitFun (findBoardCardArea cardID) (findBoardCardArea targetID)
            | PairAttack (_, (cardID1, cardID2), attackTargetInfo) ->
                let targetID =
                    match attackTargetInfo with
                    | InactiveTarget (_, targetID) -> targetID
                    | ActiveSingleTarget (_, targetID) -> targetID
                    | ActivePairMemberTarget (_, targetID) -> targetID
                boardAreas
                |> splitFun (Map.find (PairIDs (cardID1, cardID2))) (findBoardCardArea targetID)
            | CreatePair (_, pairee1, pairee2) ->
                boardAreas
                |> splitFun (findBoardCardArea pairee1) (findBoardCardArea pairee2)
        | EndTurn -> dup endTurnButtonArea
    | StartTurn -> dup switchPanelArea

let filterByMouseAction (actionSelectionAreas: ActionSelectionAreas) = function
    | Idle -> actionSelectionAreas |> List.map (snd >> fst), None, Idle
    | Dragging origin ->
        actionSelectionAreas
        |> List.filter (fun (_, (area1, _)) -> area1.Contains origin)
        |> List.map (snd >> snd),
        None,
        Dragging origin
    | Dragged (origin, destination) ->
        let remaining =
            actionSelectionAreas
            |> List.filter (fun (_, (area1, area2)) -> area1.Contains origin && area2.Contains destination)
        [], List.tryHead remaining |> Option.map fst, Idle

let drawCards spriteBatch pixel stateFont handArea laneAreas ph bk =
    match bk with
    | PreBaseFlipBoardKnowledge {Lanes = lanes; DrawPileSize = dps; Discard = d} ->
        drawHand spriteBatch pixel stateFont handArea ph
        drawBases spriteBatch pixel stateFont laneAreas lanes
        drawPreFlipTroops spriteBatch pixel stateFont laneAreas lanes
    | PostBaseFlipBoardKnowledge {Lanes = lanes; Discard = d} ->
        drawHand spriteBatch pixel stateFont handArea ph
        drawPostFlipTroops spriteBatch pixel stateFont laneAreas lanes

let drawAbilityDisplayInfo
    spriteBatch pixel stateFont
    (statusBarArea: Rectangle) (boardArea: Rectangle) (handArea: Rectangle) (deckArea: Rectangle)
    laneStartsX laneStartsY laneWidths laneHeights laneCardLimitsX laneCardLimitsY (laneAreas: Map<LaneID * PlayerID, Rectangle * Point list>)
    aci
    =
    let ({CurrentPlayer = cp; ActionsLeft = al; BoardKnowledge = bk; PlayerHand = ph; OpponentHandSizes = ohs}: AbilityChoiceDisplayInfo) = aci
    drawCards spriteBatch pixel stateFont handArea laneAreas ph bk

let drawStackDisplayInfo
    spriteBatch pixel stateFont
    (statusBarArea: Rectangle) (boardArea: Rectangle) (handArea: Rectangle) (deckArea: Rectangle)
    laneStartsX laneStartsY laneWidths laneHeights laneCardLimitsX laneCardLimitsY (laneAreas: Map<LaneID * PlayerID, Rectangle * Point list>)
    sci
    =
    let ({CurrentPlayer = cp; ActionsLeft = al; BoardKnowledge = bk; PlayerHand = ph; OpponentHandSizes = ohs}: StackChoiceDisplayInfo) = sci
    drawCards spriteBatch pixel stateFont handArea laneAreas ph bk

let drawTurnDisplayInfo
    spriteBatch pixel stateFont
    (statusBarArea: Rectangle) (boardArea: Rectangle) (handArea: Rectangle) (deckArea: Rectangle)
    laneStartsX laneStartsY laneWidths laneHeights laneCardLimitsX laneCardLimitsY (laneAreas: Map<LaneID * PlayerID, Rectangle * Point list>)
    tai
    =
    let ({CurrentPlayer = cp; ActionsLeft = al; BoardKnowledge = bk; PlayerHand = ph; OpponentHandSizes = ohs}: TurnDisplayInfo) = tai
    drawCards spriteBatch pixel stateFont handArea laneAreas ph bk

type CardGame (api: API) as this =
    inherit Game()

    let mutable gameState = api.NewGame()
    let graphics = new GraphicsDeviceManager(this)
    let mutable spriteBatch = Unchecked.defaultof<_>
    let mutable stateFont = Unchecked.defaultof<_>
    let mutable statusBarArea = Unchecked.defaultof<_>
    let mutable boardArea = Unchecked.defaultof<_>
    let mutable laneStartsX = Unchecked.defaultof<_>
    let mutable laneWidths = Unchecked.defaultof<_>
    let mutable laneStartsY = Unchecked.defaultof<_>
    let mutable laneHeights = Unchecked.defaultof<_>
    let mutable laneCardLimitsX = Unchecked.defaultof<_>
    let mutable laneCardLimitsY = Unchecked.defaultof<_>
    let mutable laneAreas = Unchecked.defaultof<_>
    let mutable switchPanelArea = Unchecked.defaultof<_>
    let mutable endTurnButtonArea = Unchecked.defaultof<_>
    let mutable noTargetButtonArea = Unchecked.defaultof<_>
    let mutable deckArea = Unchecked.defaultof<_>
    let mutable discardArea = Unchecked.defaultof<_>
    let mutable handArea = Unchecked.defaultof<_>
    let mutable handCardRects = Unchecked.defaultof<_>
    let mutable mouseStatus = Idle
    let mutable pixel = Unchecked.defaultof<_>
    let mutable filteredSelections = Unchecked.defaultof<_>
    let mutable boardUnitAreas = Unchecked.defaultof<_>

    do
        this.Content.RootDirectory <- "Content"
        this.IsMouseVisible <- true

    override this.Initialize() =
        graphics.PreferredBackBufferWidth <- windowSize.X
        graphics.PreferredBackBufferHeight <- windowSize.Y
        graphics.ApplyChanges()

        pixel <- new Texture2D(this.GraphicsDevice, 1, 1)
        pixel.SetData([|Color.White|])

        (*  Grid layout

            Status bar
            -------+-------+-------+-------
            Lane1.1|Lane2.1|Lane3.1|Deck
            -------+-------+-------+-------
            Lane1.2|Lane2.2|Lane3.2|Discard
            -------+-------+-------+-------
            Hand

            Rectangle.Bottoms are on the line after the last line in the rectange,
            not the last line itself. Similarly with Rectangle.Rights.
            Vertically, we lose nPlayers+1 lines to dividers.
            Horizontally, we lost nLanes.*)
        statusBarArea <- Rectangle(
            Point.Zero,
            Point(graphics.PreferredBackBufferWidth, statusBarHeight)
            )
        boardArea <- Rectangle(
            Point(0, statusBarArea.Bottom + 1),
            Point(
                graphics.PreferredBackBufferWidth - sideBarWidth - 1,
                graphics.PreferredBackBufferHeight - statusBarArea.Height - cardSize.Y - 2
                )
        )
        let laneWidth = float32 (boardArea.Width - (nLanes - 1)) / float32 nLanes
        let laneHeight = float32 (boardArea.Height - (nPlayers - 1)) / float32 nPlayers
        let laneShiftsX = [for n in 1..nLanes -> int (laneWidth * float32 (n - 1)) + n - 1]
        let laneShiftsY = [for p in 1..nPlayers -> int (laneHeight * float32 (p - 1)) + p - 1]
        laneStartsX <- List.map ((+) boardArea.Left) laneShiftsX
        laneStartsY <- List.map ((+) boardArea.Top) laneShiftsY
        laneWidths <- List.pairwise (laneShiftsX @ [boardArea.Width + 1]) |> List.map (fun (f, s) -> s - f - 1)
        laneHeights <- List.pairwise (laneShiftsY @ [boardArea.Height + 1]) |> List.map (fun (f, s) -> s - f - 1)
        laneCardLimitsX <- List.map (fun width -> limits width cardSize.X cardSpacing) laneWidths
        laneCardLimitsY <- List.map (fun height -> limits height cardSize.Y cardSpacing) laneHeights
        laneAreas <-
            List.allPairs
                (List.zip3 laneStartsX laneWidths laneCardLimitsX)
                (List.zip3 laneStartsY laneHeights laneCardLimitsY)
            |> List.map (fun ((startX, width, limitsX), (startY, height, limitsY)) ->
                let xs = [for n in 0..(limitsX - 1) -> startX + n*(cardSize.X + cardSpacing)]
                let ys = [for n in 0..(limitsY - 1) -> startY + n*(cardSize.Y + cardSpacing)]
                Rectangle(Point(startX, startY), Point(width, height)),
                List.allPairs ys xs |> List.map (swap >> Point) // swapped to have X cycle faster
                )
            |> List.zip (
                List.allPairs
                    (List.map (uint >> (*) 1u<LID>) [1..nLanes])
                    (List.map (uint >> (*) 1u<PID>) [1..nPlayers])
            )
            |> Map.ofList
        handArea <- Rectangle(
            Point(0, boardArea.Bottom + 1),
            Point(graphics.PreferredBackBufferWidth, cardSize.Y)
        )
        let rectsFromLimits start (distance: Point) (spacing: Point) size limits =
            List.map (fun n ->
                Rectangle(
                    start + Point(n*(distance.X + spacing.X), n*(distance.Y + spacing.Y)),
                    size
                    )
                ) [0..(limits - 1)]
        handCardRects <-
            let handLimits = limits handArea.Width cardSize.X cardSpacing
            rectsFromLimits handArea.Location (Point(cardSize.X, 0)) (Point(cardSpacing, 0)) cardSize handLimits
        deckArea <- Rectangle(
            Point(boardArea.Right + 1, boardArea.Top),
            Point(sideBarWidth, (graphics.PreferredBackBufferHeight - statusBarArea.Height - handArea.Height - 3)/2)
        )
        discardArea <- Rectangle(
            Point(deckArea.Left, deckArea.Bottom),
            Point(sideBarWidth, graphics.PreferredBackBufferHeight - deckArea.Bottom - handArea.Height - 1)
        )

        base.Initialize()

    override this.LoadContent() =
        spriteBatch <- new SpriteBatch(this.GraphicsDevice)
        stateFont <- this.Content.Load<SpriteFont>("state")
        switchPanelArea <-
            let size = stateFont.MeasureString("OK").ToPoint()
            Rectangle(
                boardArea.Center - size,
                Point(size.X*2, size.Y*2)
                )
        endTurnButtonArea <-
            let size = stateFont.MeasureString("End turn").ToPoint()
            Rectangle(
                Point(statusBarArea.Right - size.X - 2*cardSpacing, 0),
                Point(size.X + 2*cardSpacing, statusBarHeight)
                )
        noTargetButtonArea <-
            let size = stateFont.MeasureString("No target").ToPoint()
            Rectangle(
                statusBarArea.Location,
                Point(size.X + 2*cardSpacing, statusBarHeight)
                )

    override this.Update (gameTime) =
        if Keyboard.GetState().IsKeyDown(Keys.Escape) then
            this.Exit();

        let mouse = Mouse.GetState()
        mouseStatus <- updateMouseStatus mouse mouseStatus

        match gameState with
        | InProgress (_, displayInfo, capabilities) ->
            let selectionAreas =
                capabilities
                |> List.map (fun {Action = action} ->
                    getActionSelectionAreas
                        handCardRects laneAreas
                        switchPanelArea endTurnButtonArea noTargetButtonArea
                        displayInfo action
                )
            match displayInfo with
            | SwitchDisplayInfo _
            | AbilityChoiceDisplayInfo _
            | StackChoiceDisplayInfo _
            | TurnDisplayInfo _ ->
                let remainingSelections, finalAction, newMouseStatus = filterByMouseAction selectionAreas mouseStatus
                let newState =
                    finalAction
                    |> Option.map (fun action ->
                        capabilities
                        |> List.find (fun {Action = a} -> a = action)
                        |> (fun {Capability = c} -> c())
                        )
                filteredSelections <- remainingSelections
                gameState <- Option.defaultValue gameState newState
                mouseStatus <- newMouseStatus
            | WonGameDisplayInfo _
            | TiedGameDisplayInfo _ ->
                ()
        | Exit -> this.Exit()
        base.Update(gameTime)

    override this.Draw (gameTime) =
        match gameState with
        | InProgress (events, displayInfo, capabilities) ->
            this.GraphicsDevice.Clear duel52Colours.LightBrown

            spriteBatch.Begin()
            match displayInfo with
            | AbilityChoiceDisplayInfo aci ->
                let statusText = "Choose power target"
                drawNonInnerBoardBorders spriteBatch pixel statusBarArea boardArea handArea deckArea
                drawBoardInnerBorders spriteBatch pixel boardArea laneStartsX laneStartsY laneWidths laneHeights
                drawStatusBar spriteBatch pixel stateFont statusBarArea statusText
                match aci.ChoiceContext with
                | MayMoveAllyToOwnLaneChoiceContext _ ->
                    drawNoTargetButton spriteBatch pixel stateFont noTargetButtonArea
                | _ ->
                    ()
                drawAbilityDisplayInfo
                    spriteBatch pixel stateFont
                    statusBarArea boardArea handArea deckArea
                    laneStartsX laneStartsY laneWidths laneHeights laneCardLimitsX laneCardLimitsY laneAreas
                    aci
                filteredSelections |> List.iter (drawOutline spriteBatch pixel Color.Red)
            | StackChoiceDisplayInfo sci ->
                let statusText = "Choose card power to resolve next"
                drawNonInnerBoardBorders spriteBatch pixel statusBarArea boardArea handArea deckArea
                drawBoardInnerBorders spriteBatch pixel boardArea laneStartsX laneStartsY laneWidths laneHeights
                drawStatusBar spriteBatch pixel stateFont statusBarArea statusText
                drawStackDisplayInfo
                    spriteBatch pixel stateFont
                    statusBarArea boardArea handArea deckArea
                    laneStartsX laneStartsY laneWidths laneHeights laneCardLimitsX laneCardLimitsY laneAreas
                    sci
                filteredSelections |> List.iter (drawOutline spriteBatch pixel Color.Red)
            | TurnDisplayInfo tai ->
                let statusText = "Your turn. Actions left: " + string tai.ActionsLeft
                drawNonInnerBoardBorders spriteBatch pixel statusBarArea boardArea handArea deckArea
                drawBoardInnerBorders spriteBatch pixel boardArea laneStartsX laneStartsY laneWidths laneHeights
                drawStatusBar spriteBatch pixel stateFont statusBarArea statusText
                if tai.ActionsLeft = 0u<action> then
                    drawEndTurnButton spriteBatch pixel stateFont endTurnButtonArea
                drawTurnDisplayInfo
                    spriteBatch pixel stateFont
                    statusBarArea boardArea handArea deckArea
                    laneStartsX laneStartsY laneWidths laneHeights laneCardLimitsX laneCardLimitsY laneAreas
                    tai
                filteredSelections |> List.iter (drawOutline spriteBatch pixel Color.Red)
            | SwitchDisplayInfo switch ->
                let statusText = "Player " + string switch + "'s turn"
                drawNonInnerBoardBorders spriteBatch pixel statusBarArea boardArea handArea deckArea
                drawStatusBar spriteBatch pixel stateFont statusBarArea statusText
                drawSwitchButton spriteBatch stateFont pixel switchPanelArea boardArea.Center
                filteredSelections |> List.iter (drawOutline spriteBatch pixel Color.Red)
            | WonGameDisplayInfo winner ->
                let statusText = "Player " + string winner.Winner + " won!"
                drawStatusBar spriteBatch pixel stateFont statusBarArea statusText
            | TiedGameDisplayInfo _ ->
                let statusText = "Draw!"
                drawStatusBar spriteBatch pixel stateFont statusBarArea statusText
            spriteBatch.End()

            base.Draw(gameTime)
        | Exit ->
            this.Exit()

let startGame api =
    use game = new CardGame(api)
    game.Run()
