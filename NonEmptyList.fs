module NonEmptyList

type NonEmptyList<'T> = {
    Head: 'T
    Tail: 'T list
}
type 'T nonEmptyList = NonEmptyList<'T>

let toList nel = nel.Head :: nel.Tail
let tryFromList lst =
    match lst with
    | [] -> None
    | h :: t -> Some {Head = h; Tail = t}
let fromList lst =
    match lst with
    | [] -> failwith "List is empty"
    | h :: t -> {Head = h; Tail = t}    

let create head tail = {Head = head; Tail = tail}

let cons h nel = {
    Head = h
    Tail = toList nel
}
let consOptions maybeEl maybeNel =
    match maybeEl, maybeNel with
    | None, None -> None
    | None, Some nel -> Some nel
    | Some el, None -> Some {Head = el; Tail = []}
    | Some el, Some nel -> Some (cons el nel)

let collect mapping = toList >> List.collect mapping
let contains value = toList >> List.contains value
let choose chooser = toList >> List.choose chooser
let filter predicate = toList >> List.filter predicate
let indexed nel = toList nel |> List.indexed
let map mapping nel = {
    Head = mapping nel.Head
    Tail = List.map mapping nel.Tail
}
let unzip nel =
    let (h1, h2) = nel.Head
    let t1, t2 = List.unzip nel.Tail
    {Head = h1; Tail = t1}, {Head = h2; Tail = t2}
let zip nel1 nel2 = {
    Head = nel1.Head, nel2.Head
    Tail = List.zip nel1.Tail nel2.Tail
}
