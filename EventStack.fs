module EventStack
open NonEmptyList

type Epoch<'Event> = 'Event nonEmptyList
type 'Event epoch = Epoch<'Event>

type Stack<'Event> = 'Event epoch nonEmptyList
type 'Event stack = Stack<'Event>

type EventResolver<'State, 'Event> = 'State -> 'Event -> 'State * ('Event epoch option)
type EventValidator<'State, 'Event> = 'State -> 'Event -> bool

let resolveNext (resolver: EventResolver<'State, 'Event>) (validator: EventValidator<'State, 'Event>) (state: 'State) (stack: 'Event stack) : 'State * 'Event stack option =
    let {Head = {Head = next; Tail = restInEpoch}; Tail = laterEpochs} = stack
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
