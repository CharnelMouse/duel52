module EventStack
open NonEmptyList

type Epoch<'Event> = 'Event nonEmptyList
type 'Event epoch = Epoch<'Event>

type Stack<'Event> = 'Event epoch nonEmptyList
type 'Event stack = Stack<'Event>

type EventResolver<'State, 'Event> = 'State -> 'Event -> 'State * ('Event epoch option)
type EventValidator<'State, 'Event> = 'State -> 'Event -> bool

let resolve (resolver: EventResolver<'State, 'Event>) (validator: EventValidator<'State, 'Event>) next state restInEpoch laterEpochs =
    let newState, maybeTriggeredEpoch = resolver state next
    let validate = validator newState
    let validLaterEpochs: 'Event stack option =
        laterEpochs
        |> List.choose (filter validate >> tryFromList)
        |> tryFromList
    let validRest =
        restInEpoch
        |> List.filter validate
        |> tryFromList
    let newMaybeStack: 'Event stack option =
        validLaterEpochs
        |> consOptions validRest
        |> consOptions maybeTriggeredEpoch
    newState, newMaybeStack

let resolveNext resolver validator state stack : 'State * 'Event stack option =
    let {Head = {Head = next; Tail = restInEpoch}; Tail = laterEpochs} = stack
    resolve resolver validator next state restInEpoch laterEpochs

let resolveOne resolver validator index state stack: 'State * 'Event stack option =
    let head = toList stack.Head
    let next = head.[index]
    let restInEpoch = List.removeAt index head
    resolve resolver validator next state restInEpoch stack.Tail
