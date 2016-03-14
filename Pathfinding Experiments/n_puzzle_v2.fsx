// Breadth first search for the N puzzle problem.

let k = 3
let init_pos, init = (2,0), [|5; 6; 8; 7; 2; 1; 0; 3; 4|]

open System
open System.Collections.Generic

type TabularPriorityQueue() =
    let d = Dictionary<int,Stack<int>>(1000)
    let mutable min_b = Int32.MaxValue
    let mutable max_b = Int32.MinValue
    let mutable size = 0

    member t.Add k v =
        min_b <- min min_b k
        size <- size+1
        match d.TryGetValue k with
        | true, stack -> stack.Push v
        | false, _ -> d.Add(k,Stack([|v|]))

    member t.PopMin =
        if size = 0 then failwith "Cannot pop an empty queue."
        size <- size-1
        match d.TryGetValue min_b with
        | true, stack -> 
            let t = stack.Pop()
            if stack.Count = 0 then 
                d.Remove(min_b) |> ignore
                min_b <- min_b+1
            t
        | false, _ ->
            min_b <- min_b+1
            t.PopMin

    member t.Size = size

let inline is_viable_swap (r,c) =
    r >= 0 && c >= 0 && r < k && c < k

let inline get (ar: int[]) (r,c as p) = // I could have used 2D arrays, but checking victory conditions would have been such a pain then.
    if is_viable_swap p then ar.[r*k+c]
    else failwith "Invalid array get."

let inline set (ar: int[]) (r,c as p) v =
    if is_viable_swap p then ar.[r*k+c] <- v
    else failwith "Invalid array set."

let inline swap (r1,c1) (r2,c2) (ar: int[]) =
    let ar = ar.Clone() :?> int[]
    set ar (r1,c1) (get ar (r1,c1) + get ar (r2,c2))
    set ar (r2,c2) (get ar (r1,c1) - get ar (r2,c2))
    set ar (r1,c1) (get ar (r1,c1) - get ar (r2,c2))
    ar

let inline check_victory (ar: int[]) = // Breaking out of a loop can be a real pain in the ass in F#. 2D arrays are also a pain in the ass.
    let rec loop i =
        if i < ar.Length then
            if ar.[i-1]+1 = ar.[i] then
                loop (i+1)
            else false
        else true
    loop 1

type Moves =
| UP = 0uy
| LEFT = 1uy
| RIGHT = 2uy
| DOWN = 3uy
       
let bfs() =
    let queue = Queue(10000)
    let mutable goal = None

    queue.Enqueue(init,init_pos,[||])
    let mutable max_len = 2

    let rec bfs() =
        if goal = None then
            let ar, (r,c as p), past_moves = queue.Dequeue()
            if past_moves.Length > max_len then
                max_len <- past_moves.Length
                printfn "max_len = %i" max_len
//            printfn "ar=%A p=%A past_moves=%A" ar p past_moves
            [|-1+r,c,Moves.UP; // UP
            r,-1+c,Moves.LEFT; // LEFT
            r,1+c,Moves.RIGHT; // RIGHT
            1+r,c,Moves.DOWN|] // DOWN
            |> Array.filter (fun (r,c,m) -> 
                is_viable_swap (r,c) &&
                if past_moves.Length > 0 then
                    let x = past_moves |> Array.last
                    match x,m with // Make sure not to retrace
                    | (Moves.LEFT,Moves.RIGHT) -> false
                    | (Moves.RIGHT,Moves.LEFT) -> false
                    | (Moves.UP,Moves.DOWN) -> false
                    | (Moves.DOWN,Moves.UP) -> false
                    | _ -> true
                else true)
            |> fun moves ->
//                printfn "moves=%A" moves
                let rec loop i =
                    if i < moves.Length then
                        let r,c,m = moves.[i]
                        let s = swap p (r,c) ar
//                        printfn "s=%A" s
                        if check_victory s then
                            goal <- Some (s, Array.append past_moves [|m|])
                        else
                            queue.Enqueue(s,(r,c), Array.append past_moves [|m|])
                            loop (i+1)
                loop 0
            bfs()
    bfs()
    goal.Value

#time // 1.7s on my machine. I guess I'll be using lists to memorize moves then.
let max_goal, path = bfs()
let l = path.Length
#time

//printfn "%i" l
//for x in path do printfn "%A" x
