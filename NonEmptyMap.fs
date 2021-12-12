module NonEmptyMap

type NonEmptyMap<'Key, 'T> when 'Key : comparison = {
    Head: 'Key * 'T
    Tail: Map<'Key, 'T>
}

let toMap nem = Map.add (fst nem.Head) (snd nem.Head) nem.Tail
let tryFromMap mp =
    match Map.toList mp with
    | [] -> None
    | h :: t -> Some {Head = h; Tail = Map.ofList t}
let fromMap mp =
    match Map.toList mp with
    | [] -> failwith "Map is empty"
    | h :: t -> {Head = h; Tail = Map.ofList t}    

let create head tail =
    if Map.containsKey (fst head) tail then
        failwithf "Head key is in tail"
    else
        {Head = head; Tail = tail}
