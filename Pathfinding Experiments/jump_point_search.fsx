// This form of Jump Point Search is far too slow. It is a failure.
// The big string of if statements make it 5x slower than Fringe Search.

// Edit: Actually when I turn off script debugging or do it in compiled mode, it is nearly as fast as Fringe Search. This is not bad at all.

// It should be worth noting that this implementation differs from the one in the paper in that continues running until it meets an obstacle while
// the original returns immediately when it enqueues a new node.

// Also here the optimal path is calculated backwards from the trace, while in JPS that is done by connecting the jump points. Just a thing to keep in mind.

// Edit3: It occurs to me that both this and the previous versions are incorrect as there are not enough constraints for orthogonal movement, but on the
// large BG maps it is even slower than Astar and it is not worth the bother for me to fix it.

#if INTERACTIVE
#r @"..\packages\OptimizedPriorityQueue.2.0.0\lib\net45\Priority Queue.dll"
#endif

open System
open System.IO
open System.Collections.Generic
open Priority_Queue

let level = 
    """%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%""" |> fun x -> x.Split [|'\n'|] |> Array.map (fun x -> x.TrimEnd())

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

let fringe_search() =
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
        let a = abs(r-food_r) // Symmetry breaking significantly reduces the performance of Fringe Search.
        let b = abs(c-food_c) // This surprised me a little, but Fringe Search is a bit like depth first search, so it makes some sense that it would work like that.
        let c = dist_to_source // Good thing I used a stack instead of a queue.
        a+b+c |> float

    let mutable later = Stack(2000)
    let mutable now = Stack(2000)
    //let expanded_nodes = ResizeArray(2000)
    let mutable later_upper_bound = Double.MaxValue

    let rec fringe_search upper_bound =
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
            if now.Count > 0 then
                let (pac_r,pac_c as pac_pos),i,heuristic_cost as current_item = now.Pop()
                if heuristic_cost > upper_bound 
                then 
                    later.Push current_item
                    fringe_search upper_bound
                else
                    //print_expansion expanded_nodes

                    let inline if_viable_execute x =
                        let inline is_viable (n_r, n_c as x) = level.[n_r].[n_c] <> '%' && is_not_visited x
                        let inline execute x =
                            let next_i = i+1
                            if x <> food_pos then
                                let c = manhattan_distance x next_i

                                if c <= upper_bound then now.Push((x,next_i,c))
                                else 
                                    if c < later_upper_bound then later_upper_bound <- c
                                    later.Push((x,next_i,c))

                                set x next_i 
                                //expanded_nodes.Add(x)
                            else set x next_i; max_goal <- next_i; //expanded_nodes.Add(x)

                        if is_viable x then execute x

                    if_viable_execute (-1+pac_r,pac_c) // UP
                    if_viable_execute (pac_r,-1+pac_c) // LEFT
                    if_viable_execute (pac_r,1+pac_c) // RIGHT
                    if_viable_execute (1+pac_r,pac_c) // DOWN
                        
                    fringe_search upper_bound
            else
                let t = now
                now <- later
                later <- t
                let t' = later_upper_bound
                later_upper_bound <- Double.MaxValue
                fringe_search t'
            

    now.Push((pac_pos,1,manhattan_distance pac_pos 1))
    set pac_pos 1
    fringe_search <| manhattan_distance pac_pos 1
    max_goal, get_trace_from food_pos max_goal []//, expanded_nodes


let jump_point_search() =
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
        let a = abs(r-food_r) 
        let b = abs(c-food_c) 
        let c = dist_to_source 
        a+b+c |> float

    let mutable queue = SimplePriorityQueue()

    let right = (0,1)
    let left = (0,-1)
    let up = (-1,0)
    let down = (1,0)

    let up_right = (-1,1)
    let down_right = (1,1)
    let up_left = (-1,-1)
    let down_left = (1,-1)

    let omni = (0,0)

    let mutable max_goal = Int32.MaxValue

    let rec jump (pac_r,pac_c as pac_pos) (dir_r, dir_c as dir_pos) i =
        let inline add_node_if_dir_place (pac_r,pac_c as pac_pos) (dir_r, dir_c as dir_pos) (place_r, place_c as place_pos) =
            let inline is_wall x = x = '%'
            let inline is_not_wall x = is_wall x |> not

            if is_wall level.[pac_r + dir_r].[pac_c + dir_c] && is_not_wall level.[pac_r + place_r].[pac_c + place_c]
            then 
                let t = pac_r + place_r,pac_c + place_c
//                printfn "Adding jump t=%A (i+2)=%i" t (i+2)
                queue.Enqueue(lazy jump t place_pos (i+2), manhattan_distance t (i+2))
                true
            else true

//        printfn "pac_pos=%A dir_pos=%A i=%i" pac_pos dir_pos i
        let print_trace (trace : int[,])=
            let t = Array2D.create rows cols '.'
            for i=0 to level.Length-1 do
                for j=0 to level.[i].Length-1 do
                    t.[i,j] <- 
                        if trace.[i,j] > 0 then '*' 
                        else level.[i].[j]
            

            for i=0 to level.Length-1 do
                for j=0 to level.[i].Length-1 do
                    printf "%c" t.[i,j]
                printfn ""

            Threading.Thread.Sleep(10)
            
        //print_trace trace

        if pac_pos = food_pos then 
            set pac_pos i
            max_goal <- i
            false
        elif is_not_visited pac_pos && level.[pac_r].[pac_c] <> '%' then
            
            set pac_pos i

            let inline add_pos_dir (pac_r,pac_c as pac_pos) (dir_r, dir_c as dir_pos) = 
                (pac_r+dir_r, pac_c+dir_c)

            let inline straight_jump() = jump (add_pos_dir pac_pos dir_pos) dir_pos (i+1)
            let inline diag_jump() = jump (add_pos_dir pac_pos dir_pos) dir_pos (i+2)
            let inline straight_dir_jump dir = jump (add_pos_dir pac_pos dir) dir (i+1)
            let inline diag_dir_jump dir = jump (add_pos_dir pac_pos dir) dir (i+2)

            if dir_pos = left then
                add_node_if_dir_place pac_pos up up_left // if wall is up then enqueue up_left
                && add_node_if_dir_place pac_pos down down_left 
                && straight_jump()
            elif dir_pos = right then
                add_node_if_dir_place pac_pos up up_right 
                && add_node_if_dir_place pac_pos down down_right
                && straight_jump()
            elif dir_pos = up then
                add_node_if_dir_place pac_pos left up_left 
                && add_node_if_dir_place pac_pos right up_right
                && straight_jump()
            elif dir_pos = down then
                add_node_if_dir_place pac_pos left down_left 
                && add_node_if_dir_place pac_pos right down_right
                && straight_jump()
            elif dir_pos = up_left then
                add_node_if_dir_place pac_pos down down_left
                && add_node_if_dir_place pac_pos right up_right
                && straight_dir_jump up
                && straight_dir_jump left
                && diag_jump()
            elif dir_pos = up_right then
                add_node_if_dir_place pac_pos down down_right
                && add_node_if_dir_place pac_pos left up_left
                && straight_dir_jump up
                && straight_dir_jump right
                && diag_jump()
            elif dir_pos = down_left then
                add_node_if_dir_place pac_pos up up_left
                && add_node_if_dir_place pac_pos right down_right
                && straight_dir_jump down
                && straight_dir_jump left
                && diag_jump()
            elif dir_pos = down_right then
                add_node_if_dir_place pac_pos up up_right
                && add_node_if_dir_place pac_pos left down_left
                && straight_dir_jump down
                && straight_dir_jump right
                && diag_jump()
            elif dir_pos = omni then
                straight_dir_jump down
                && straight_dir_jump right
                && straight_dir_jump up
                && straight_dir_jump left
                && diag_dir_jump down_left
                && diag_dir_jump down_right
                && diag_dir_jump up_left
                && diag_dir_jump up_right
            else failwith "Invalid match in jump."
        else true

    if jump pac_pos omni 1 then
        while queue.Dequeue().Force() do ()
        
    max_goal, get_trace_from food_pos max_goal []

let stopwatch = Diagnostics.Stopwatch.StartNew()
for i=1 to 10000 do
    jump_point_search() |> ignore
printfn "Time elapsed for jump_point_search: %A" stopwatch.Elapsed
stopwatch.Restart()
for i=1 to 10000 do
    fringe_search() |> ignore
printfn "Time elapsed for fringe_search: %A" stopwatch.Elapsed    