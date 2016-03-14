#r "../packages/DequeNET.1.0.1/lib/portable-net4+win8/DequeNet.dll"

// I am angry, angry about Java.

// As expected, Fsharp broke me, there is no way I can stoop down to programming in Java.
// I'll give prototyping in F# a shot first.

// It is snobery, but whatever makes me happy.

type Deque2() =
    let mutable fp, occ = 0,0
    let mutable ar = Array.zeroCreate 2

    let wrapAround i =
        if i < 0 then i+ar.Length
        elif i >= ar.Length then i-ar.Length
        else i

    let resize() =
        let new_ar = Array.zeroCreate (occ*2)
        for i=0 to occ-1 do
            new_ar.[i] <- ar.[fp]
            fp <- wrapAround(fp+1)
        ar <- new_ar
        fp <- 0

    let maybeResize() =
        if (occ >= ar.Length) then resize()
        elif (occ < ar.Length/4 && occ > 0) then resize()

    member t.isEmpty() = occ = 0
    member t.size() = occ
    member t.addFirst item =
        fp <- wrapAround(fp-1)
        ar.[fp] <- item
        occ <- occ+1
        maybeResize()
    member t.addLast item =
        let ep = wrapAround(fp+occ)
        ar.[ep] <- item
        occ <- occ+1
        maybeResize()
    member t.removeFirst() =
        if occ < 1 then failwith "Cannot remove from empty queue."
        let t = ar.[fp]
        ar.[fp] <- Unchecked.defaultof<_>
        fp <- wrapAround(fp+1)
        occ <- occ-1
        maybeResize()
        t
    member t.removeLast() =
        if occ < 1 then failwith "Cannot remove from empty queue."
        occ <- occ-1
        let ep = wrapAround(fp+occ)
        let t = ar.[ep]
        ar.[ep] <- Unchecked.defaultof<_>
        maybeResize()
        t

type Moves =
| AddFirst = 0
| AddLast = 1
| RemoveFirst = 2
| RemoveLast = 3
| CheckSize = 4

let rng = System.Random()
let moves = Array.init 1000 (fun _ -> enum<Moves>(rng.Next(0,5)))

let mutable c = 0
let t1 = Deque2()
let t2 = DequeNet.Deque()
for x in moves do
    match x with
    | Moves.AddFirst -> 
        let r = rng.Next(0,100)
        t1.addFirst(r)
        t2.PushLeft(r)
        c <- c+1
    | Moves.AddLast -> 
        let r = rng.Next(0,100)
        t1.addLast(r)
        t2.PushRight(r)
        c <- c+1
    | Moves.RemoveFirst -> 
        if c > 0 then
            let r1 = t1.removeFirst()
            let r2 = t2.PopLeft()
            if r1 <> r2 then failwithf "r1=%i and r2=%i do not match. c=%i" r1 r2 c
            c <- c-1
    | Moves.RemoveLast ->
        if c > 0 then
            let r1 = t1.removeLast()
            let r2 = t2.PopRight()
            if r1 <> r2 then failwithf "r1=%i and r2=%i do not match. c=%i" r1 r2 c
            c <- c-1
    | Moves.CheckSize ->
        let r1 = t1.size()
        let r2 = t2.Count
        if r1 <> r2 then failwithf "r1=%i and r2=%i do not match. c=%i" r1 r2 c

t1.size()