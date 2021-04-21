module CountMap

type CountMap<'T when 'T: comparison> = Map<'T, int>

let toList (cm: CountMap<'T>) =
    cm
    |> Map.toList
    |> List.collect (fun (key, n) -> List.replicate n key)

let ofList lst : CountMap<'T> =
    lst
    |> List.countBy (fun x -> x)
    |> Map.ofList

let map fn (cm: CountMap<'T>) : CountMap<'U> =
    cm
    |> toList
    |> List.map fn
    |> ofList

let count (cm: CountMap<'T>) =
    Map.fold (fun state _ n -> state + n) 0 cm

let dec key (cm: CountMap<'T>) =
    cm
    |> Map.change key (function
        | Some 1 ->
            None
        | Some n ->
            Some (n - 1)
        | None ->
            failwithf "Can't decrement a missing key: %A" key
        )

let inc key (cm: CountMap<'T>) =
    cm
    |> Map.change key (function
        | None ->
            Some 1
        | Some n ->
            Some (n + 1)
        )

let isEmpty (cm: CountMap<'T>) =
    Map.isEmpty cm

let partition (predicate: 'T -> bool) (cm: CountMap<'T>) =
    Map.partition (fun key _ -> predicate key) cm

let choose (fn: 'T -> 'U option) (cm: CountMap<'T>) =
    cm
    |> toList
    |> List.choose fn
    |> ofList

let iter fn (cm: CountMap<'T>) = Map.iter fn cm

let keyList (cm: CountMap<'T>) =
    cm
    |> Map.toList
    |> List.map (fun (t, n) -> t)
