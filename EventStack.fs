module EventStack
open NonEmptyList

type Epoch<'Event> = 'Event nonEmptyList
type 'Event epoch = Epoch<'Event>

type Stack<'Event> = 'Event epoch nonEmptyList
type 'Event stack = Stack<'Event>

type EventResolver<'State, 'Event> = 'Event -> 'State * ('Event epoch option)
type EventValidator<'State, 'Event> = 'State -> 'Event -> bool

let resolveNext (resolver: EventResolver<'State, 'Event>) (validator: EventValidator<'State, 'Event>) (stack: 'Event stack) =
    let {Head = {Head = next; Tail = restInEpoch}; Tail = laterEpochs} = stack
    let (newState, maybeTriggeredEpoch) = resolver next
    let validate = validator newState
    let validLaterEpochs =
        laterEpochs
        |> List.choose (filter validate >> tryFromList)
        |> tryFromList
    let validRest =
        restInEpoch
        |> List.filter validate
        |> tryFromList
    let newMaybeStack =
        validLaterEpochs
        |> consOptions validRest
        |> consOptions maybeTriggeredEpoch
    newState, newMaybeStack
