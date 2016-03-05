#r @"..\packages\OptimizedPriorityQueue.2.0.0\lib\net45\Priority Queue.dll"
open System
open System.Collections.Generic
open Priority_Queue

let pac_r, pac_c as pac_pos = 35, 35
let food_r, food_c as food_pos = 35, 1
let rows, cols = 37, 37
let level = """%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%-----------------------------------%
%-----------------------------------%
%-----------------------------------%
%-----------------------------------%
%-----------------------------------%
%-----------------------------------%
%-----------------------------------%
%-----------------------------------%
%-----------------------------------%
%-----------------------------------%
%-----------------------------------%
%-----------------------------------%
%-----------------------------------%
%-----------------------------------%
%-----------------------------------%
%-----------------------------------%
%-----------------------------------%
%-----------------------------------%
%-----------------------------------%
%-----------------------------------%
%-----------------------------------%
%-----------------------------------%
%-----------------------------------%
%-----------------------------------%
%-----------------------------------%
%-----------------------------------%
%-----------------------------------%
%-----------------------------------%
%-----------------------------------%
%-----------------------------------%
%-----------------------------------%
%-----------------------------------%
%-----------------------------------%
%-----------------------------------%
%.---------------------------------P%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%""" |> fun x -> x.Split [|'\n'|]

let astar() =
    let mutable max_goal = Int32.MaxValue

    let trace = Array2D.create rows cols 0
    let inline set (r, c) i =
        trace.[r,c] <- i
    let inline get (r, c) = trace.[r,c]
    let inline is_visited_and_less_than i (r, c) =
        trace.[r,c] <> 0 && trace.[r,c] = i-1
    let inline is_not_visited (r, c) =
        trace.[r,c] = 0
    let rec get_trace_from (pac_r, pac_c as p) i accum =
        [|-1+pac_r,pac_c;pac_r,-1+pac_c;pac_r,1+pac_c;1+pac_r,pac_c;|] // UP, LEFT, RIGHT, DOWN
        |> Array.tryFind (is_visited_and_less_than i)
        |> function
           | Some n -> get_trace_from n (i-1) (p::accum)
           | None -> p::accum
    let inline manhattan_distance (r,c) dist_to_source =
        abs(r-food_r) + abs(c-food_c) + dist_to_source |> float

    let queue = SimplePriorityQueue()
    let expanded_nodes = ResizeArray()

    let rec astar() =
        let print_expansion ex =
            let t = Array2D.create rows cols '.'
            for i=0 to level.Length-1 do
                for j=0 to level.[i].Length-1 do
                    t.[i,j] <- level.[i].[j]
            for (l,r) in ex do t.[l,r] <- '*'

            for i=0 to level.Length-1 do
                for j=0 to level.[i].Length-1 do
                    printf "%c" t.[i,j]
                printfn ""

        if max_goal = Int32.MaxValue then
            let (pac_r,pac_c as pac_pos),i = queue.Dequeue()
//            print_expansion expanded_nodes
            [|-1+pac_r,pac_c; // UP
            pac_r,-1+pac_c; // LEFT
            pac_r,1+pac_c; // RIGHT
            1+pac_r,pac_c;|] // DOWN
            |> Array.filter (fun (n_r,n_c as x) -> 
                level.[n_r].[n_c] <> '%' && is_not_visited x && i+1 < max_goal)
            |> fun ar ->
                let next_i = i+1
                let rec loop j =
                    if j < ar.Length then
                        let x = ar.[j]
                        if x <> food_pos then
                            queue.Enqueue((x,next_i),manhattan_distance x next_i); set x next_i; expanded_nodes.Add(x)
                            loop <| j+1
                        else set x next_i; expanded_nodes.Add(x); max_goal <- next_i
                loop 0
            astar ()

    queue.Enqueue((pac_pos,1),manhattan_distance pac_pos 1)
    set pac_pos 1
    astar()
    max_goal, get_trace_from food_pos max_goal [], expanded_nodes

let max_goal, path, expanded_nodes = astar()
expanded_nodes.Count
