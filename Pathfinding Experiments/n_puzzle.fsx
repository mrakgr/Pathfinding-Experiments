#r @"..\packages\OptimizedPriorityQueue.2.0.0\lib\net45\Priority Queue.dll"
open System
open System.Collections.Generic
open Priority_Queue

let k, init = 
    """3
0
3
8
4
1
7
2
6
5""" |> fun x -> x.Split [|'\n'|]
     |> Array.map Int32.Parse
     |> fun x -> x.[0], x.[1..]

let init_pos =
    init |> Array.findIndex ((=)0) // Partial application of the = operator.
         |> fun x -> x/k,x%k

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
| UP = 0
| LEFT = 1
| RIGHT = 2
| DOWN = 3

let bfs() =
    let queue = Queue(10000)
    let mutable goal = None

    queue.Enqueue(init,init_pos,[])

    let rec bfs() =
        if goal = None then
            let ar, (r,c as p), past_moves = queue.Dequeue()
//            printfn "ar=%A p=%A past_moves=%A" ar p past_moves
            [|-1+r,c,Moves.UP; // UP
            r,-1+c,Moves.LEFT; // LEFT
            r,1+c,Moves.RIGHT; // RIGHT
            1+r,c,Moves.DOWN|] // DOWN
            |> Array.filter (fun (r,c,m) -> 
                is_viable_swap (r,c) &&
                match past_moves with
                | x::_ ->
                    match x,m with // Make sure not to retrace
                    | (Moves.LEFT,Moves.RIGHT) -> false
                    | (Moves.RIGHT,Moves.LEFT) -> false
                    | (Moves.UP,Moves.DOWN) -> false
                    | (Moves.DOWN,Moves.UP) -> false
                    | _ -> true
                | [] -> true)
            |> fun moves ->
//                printfn "moves=%A" moves
                let rec loop i =
                    if i < moves.Length then
                        let r,c,m = moves.[i]
                        let s = swap p (r,c) ar
//                        printfn "s=%A" s
                        if check_victory s then
                            goal <- Some (s, m::past_moves |> List.rev)
                        else
                            queue.Enqueue(s,(r,c), m::past_moves)
                            loop (i+1)
                loop 0
            bfs()
    bfs()
    goal.Value

let max_goal, path = bfs()
printfn "%i" path.Length
for x in path do printfn "%A" x