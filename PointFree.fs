module PointFree

let pair a b = a, b
let pairRev b a = a, b
let swap (a, b) = b, a
let pairToList (a, b) = [a; b]

let dup a = a, a
let opLeft f (a, b) = f a, b
let opRight f (a, b) = a, f b
let opPair f g = opLeft f >> opRight g
let opBoth f = opPair f f
let splitFun f g = dup >> opPair f g

let toMiddle c (a, b) = a, c, b

let flattenLeft ((a, b), c) = a, b, c
let flattenRight (a, (b, c)) = a, b, c
let unflattenLeft (a, b, c) = (a, b), c
let unflattenRight (a, b, c) = (a, (b, c))

let call (f, a) = f a
let swapIn f b a = f a b
let uncurry f (a, b) = f a b
let curry f a b = f (a, b)

let prepare f =
    opRight f
    >> flattenRight
    >> unflattenLeft
    >> opLeft call
