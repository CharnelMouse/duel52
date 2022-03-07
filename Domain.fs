namespace Domain
open EventStack

type Rank =
| Two
| Three
| Four
| Five
| Six
| Seven
| Eight
| Nine
| Ten
| Jack
| Queen
| King
| Ace

type Suit =
| Clubs
| Diamonds
| Hearts
| Spades

[<Measure>] type health
[<Measure>] type PID
[<Measure>] type LID
[<Measure>] type CID
[<Measure>] type EID
[<Measure>] type action
 
type Damage = uint<health>
type PlayerID = uint<PID>
type LaneID = uint<LID>
type CardID = uint<CID>
type EventID = uint<EID>
type Actions = uint<action>
type UnitIDs =
| SingleCardID of CardID
| PairIDs of CardID * CardID

type PowerName = PowerName of string

type HandCardInfo = HandCardInfo of CardID * Rank * Suit * PowerName

type Hand = HandCardInfo list

type Actionability =
| Normal
| Frozen

type Readiness =
| Ready
| Exhausted

type ActiveStatus =
| Inactive // face-down
| Active // face-up

type BaseKnowledge =
| UnknownBaseCard of PlayerID
| KnownBaseCard of PlayerID * Rank * Suit * PowerName

type DeadCardKnowledge =
| UnknownDeadCard
| KnownFaceDownDeadCard of Rank * Suit
| KnownFaceUpDeadCard of Rank * Suit

type InactiveUnitKnowledge =
| UnknownInactiveCardKnowledge of CardID * Damage * Actionability
| KnownInactiveCardKnowledge of CardID * Rank * Suit * PowerName * Damage * Actionability

type ActiveUnitKnowledge = CardID * Rank * Suit * PowerName * Damage * Actions * Actionability

type PairKnowledge = CardID * CardID * Rank * Suit * Suit * PowerName * Damage * Damage * Actions * Actionability * Actionability

type TroopKnowledge = Map<PlayerID, InactiveUnitKnowledge list * ActiveUnitKnowledge list * PairKnowledge list>

type PreBaseFlipLaneKnowledge = {
    Bases: BaseKnowledge list
    Troops: TroopKnowledge
}

type ContestedLaneKnowledge = {
    Troops: TroopKnowledge
}

type WonLaneKnowledge = {
    Controller: PlayerID
    Troops: TroopKnowledge
}

type PostBaseFlipLaneKnowledge =
| ContestedLaneKnowledge of ContestedLaneKnowledge
| WonLaneKnowledge of WonLaneKnowledge

type DiscardKnowledge = DeadCardKnowledge list

type PreBaseFlipBoardKnowledge = {
    Lanes: Map<LaneID, PreBaseFlipLaneKnowledge>
    DrawPileSize: int
    Discard: DiscardKnowledge
}

type PostBaseFlipBoardKnowledge = {
    Lanes: Map<LaneID, PostBaseFlipLaneKnowledge>
    Discard: DiscardKnowledge
}

type BoardKnowledge =
| PreBaseFlipBoardKnowledge of PreBaseFlipBoardKnowledge
| PostBaseFlipBoardKnowledge of PostBaseFlipBoardKnowledge

type MidPassivePowerChoiceContext =
| TwinStrikeChoiceContext of PlayerID * LaneID * UnitIDs * CardID
| TwinStrikeRelatiatePairChoiceContext of PlayerID * LaneID * (CardID * CardID) * CardID

type MidActivationPowerChoiceContext =
| DiscardChoiceContext of PlayerID * CardID
| ForesightChoiceContext of PlayerID * CardID
| MoveChoiceContext of PlayerID * LaneID * CardID

type PassivePowerWithDecisionContext =
| TwinStrikePowerContext of PlayerID * LaneID * CardID

type ActivationPowerContext = PlayerID * LaneID * CardID * PowerName

type MidActivationPowerChoiceDisplayInfo = {
    CurrentPlayer: PlayerID
    ActionsLeft: Actions
    BoardKnowledge: BoardKnowledge
    PlayerHand: Hand
    OpponentHandSizes: (PlayerID * int) list
    ChoiceContext: MidActivationPowerChoiceContext
}

type MidPassivePowerChoiceDisplayInfo = {
    CurrentPlayer: PlayerID
    ActionsLeft: Actions
    BoardKnowledge: BoardKnowledge
    PlayerHand: Hand
    OpponentHandSizes: (PlayerID * int) list
    ChoiceContext: MidPassivePowerChoiceContext
}

type StackChoiceDisplayInfo = {
    CurrentPlayer: PlayerID
    ActionsLeft: Actions
    BoardKnowledge: BoardKnowledge
    PlayerHand: Hand
    OpponentHandSizes: (PlayerID * int) list
    Stack: ActivationPowerContext stack
}

type TurnDisplayInfo = {
    CurrentPlayer: PlayerID
    ActionsLeft: Actions
    BoardKnowledge: BoardKnowledge
    PlayerHand: Hand
    OpponentHandSizes: (PlayerID * int) list
}

type WonGameDisplayInfo = {
    Winner: PlayerID
    LaneWins: (PlayerID * LaneID list) list
}

type TiedGameDisplayInfo = {
    LaneWins: (PlayerID * LaneID list) list
}

type DisplayInfo =
| MidActivationPowerChoiceDisplayInfo of MidActivationPowerChoiceDisplayInfo
| MidPassivePowerChoiceDisplayInfo of MidPassivePowerChoiceDisplayInfo
| StackChoiceDisplayInfo of StackChoiceDisplayInfo
| TurnDisplayInfo of TurnDisplayInfo
| SwitchDisplayInfo of PlayerID
| WonGameDisplayInfo of WonGameDisplayInfo
| TiedGameDisplayInfo of TiedGameDisplayInfo

type AttackTargetInfo =
| InactiveTarget of PlayerID * CardID
| ActiveSingleTarget of PlayerID * CardID
| ActivePairMemberTarget of PlayerID * CardID

type MidActivationPowerChoiceInfo =
| DiscardChoice of PlayerID * CardID * CardID
| ForesightChoice of PlayerID * CardID * CardID
| MoveChoice of (PlayerID * LaneID * CardID * LaneID * CardID) option

type MidPassivePowerChoiceInfo =
| TwinStrikeChoice of PlayerID * LaneID * UnitIDs * CardID
| TwinStrikeRetaliatePairChoice of PlayerID * LaneID * UnitIDs * CardID * CardID

type StackChoiceInfo = PlayerID * EventID * ActivationPowerContext

type ActionChoiceInfo =
| Play of PlayerID * CardID * LaneID
| Activate of PlayerID * LaneID * CardID
| SingleAttack of PlayerID * LaneID * CardID * AttackTargetInfo
| PairAttack of PlayerID * LaneID * (CardID * CardID) * AttackTargetInfo
| CreatePair of PlayerID * LaneID * CardID * CardID

type TurnActionInfo =
| ActionChoiceInfo of ActionChoiceInfo
| EndTurn of PlayerID

type ActionInfo =
| MidActivationPowerChoiceInfo of MidActivationPowerChoiceInfo
| MidPassivePowerChoiceInfo of MidPassivePowerChoiceInfo
| StackChoiceInfo of StackChoiceInfo
| TurnActionInfo of TurnActionInfo
| StartTurn of PlayerID

type Capability<'T> = unit -> 'T
type CapabilityInfo<'Action, 'T> = {
    Action: 'Action
    Capability: Capability<'T>
}

type ActionResult =
| InProgress of DisplayInfo * CapabilityInfo<ActionInfo, ActionResult> list
| Exit
type NextActionInfo = CapabilityInfo<ActionInfo, ActionResult>
type ActionCapability = Capability<ActionResult>

type API = {
    NewGame: unit -> ActionResult
}
