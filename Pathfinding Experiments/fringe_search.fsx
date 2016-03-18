// TODO

#r @"..\packages\OptimizedPriorityQueue.2.0.0\lib\net45\Priority Queue.dll"
open System
open System.Collections.Generic
open Priority_Queue

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
%---%-------------------------------%
%---%-----------------%%%%%%%%%-----%
%---%%%%%-------------%-------------%
%-------%-------------%-------------%
%-----.-%-------------%----P--------%
%-------%-------------%-------------%
%---%%%%%-------------%-------------%
%---------------------%%%%%%%%%-----%
%-----------------------------------%
%-----------------------------------%
%-----------------------------------%
%-----------------------------------%
%-----------------------------------%
%-----------------------------------%
%-----------------------------------%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%""" |> fun x -> x.Split [|'\n'|]

let rows, cols =
    level.Length,
    level 
    |> Array.map (fun x -> x.Length)
    |> Array.distinct
    |> fun x -> if x.Length <> 1 then failwith "Columns have to be evenly alligned." else x.[0]

type FoodPacmanPosType =
| NotFound
| FoodFound of food_r : int * food_c : int
| PacmanFound of pacman_r : int * pacman_c : int
| BothFound of food_r : int * food_c : int * pacman_r : int * pacman_c : int

let (food_r, food_c as food_pos),(pac_r, pac_c as pac_pos) = 
    let rec loop state i =
        let rec loop2 state j =
            if j < cols then
                match state with
                | NotFound ->
                    match level.[i].[j] with
                    | '.' -> loop2 <| FoodFound(i,j) <| j+1
                    | 'P' -> loop2 <| PacmanFound(i,j) <| j+1
                    | _ -> loop2 state <| j+1
                | PacmanFound(pacman_r, pacman_c) ->
                    match level.[i].[j] with
                    | '.' -> loop2 <| BothFound(i,j,pacman_r,pacman_c) <| j+1
                    | 'P' -> failwith "Two Pacmans are not allowed in the graph."
                    | _ -> loop2 state <| j+1
                | FoodFound(food_r, food_c) ->
                    match level.[i].[j] with
                    | '.' -> failwith "Two goals are not allowed in the graph."
                    | 'P' -> loop2 <| BothFound(food_r, food_c, i, j) <| j+1 
                    | _ -> loop2 state <| j+1
                | BothFound(food_r, food_c, pacman_r, pacman_c) ->
                    match level.[i].[j] with
                    | '.' -> failwith "Two goals are not allowed in the graph."
                    | 'P' -> failwith "Two Pacmans are not allowed in the graph."
                    | _ -> loop2 state <| j+1
            else state
        if i < rows then loop (loop2 state 0) <| i+1
        else
            match state with
            | BothFound(food_r, food_c, pacman_r, pacman_c) -> (food_r, food_c), (pacman_r, pacman_c)
            | _ -> failwith "Food or Pacman not found!"
        
    loop NotFound 0

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
        let a = abs(r-food_r) |> float |> fun x -> x*1.001
        let b = abs(c-food_c) |> float
        let c = dist_to_source |> float
        a+b+c

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
            print_expansion expanded_nodes
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