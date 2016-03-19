// Astar search for the N puzzle problem with the tabular priority queue.
// This is the corrected Astar one is with the closed set added back in. I really should have had this in the previous sections.

// Edit: Actually adding that visited_set does not seem to be doing anything.
// I need to remove the cruft from all the functions and bring in that encoder that I did a while ago. This is simply not getting the job done.

// Edit2: I forgot to add elements to the visited_set. Let me try again...no it is still slow. Nevermind this for now.
// I'll leave N puzzle alone for the time being.

open System
open System.Collections.Generic

let k = 4
let init_pos, init = (1, 2), [|15uy; 12uy; 9uy; 14uy; 5uy; 4uy; 0uy; 1uy; 3uy; 6uy; 2uy; 13uy; 7uy; 11uy; 8uy; 10uy|] // Hard puzzle
//let init_pos, init = (1, 2), [|4; 1; 2; 3; 8; 6; 0; 10; 9; 5; 15; 7; 12; 13; 11; 14|]
//let init_pos, init = (0,2), [|2;3;0;8;15;12;6;7;13;1;4;9;14;11;10;5|] |> Array.map byte // Super hard.
//let init_pos, init = (0,3),   [|1uy; 2uy; 3uy; 0uy; 5uy; 12uy; 7uy; 4uy; 13uy; 6uy; 14uy; 9uy; 10uy; 8uy; 11uy; 15uy|]
//let init_pos, init = (1,2), [|1uy; 2uy; 3uy; 4uy; 5uy; 6uy; 0uy; 8uy; 9uy; 10uy; 7uy; 12uy; 13uy; 14uy; 11uy; 15uy|] // All of these are harder than the hard puzzle.
//let init_pos, init = 
//     (1,2),
//     """ 1  2  3  4 
// 5  6  0  8 
// 9 10  7 12 
//13 14 11 15 """ // Converter
//    |> fun x -> x.Split [|' ';'\n'|] 
//    |> Array.filter (fun x -> x <> "")
//    |> Array.map (Int32.Parse >> byte)
    
let timer = Diagnostics.Stopwatch.StartNew()

type Moves =
| UP = 0
| LEFT = 1
| RIGHT = 2
| DOWN = 3

type TabularPriorityQueue(c : int) =
    let d = Dictionary<int,Stack<_>>(c)
    let mutable min_b = Int32.MaxValue
    let mutable max_b = Int32.MinValue
    let mutable size = 0
    let upper_size = 10000000
    let mutable num_removals = 0

    member t.Add k v =
        if k < min_b then min_b <- k
        if k > max_b then max_b <- k
        size <- size+1

        let rec remove_max() =
            match d.TryGetValue max_b with
            | true, stack -> 
                stack.Pop() |> ignore
                num_removals <- num_removals+1
                if num_removals % upper_size = 0 then printfn "Removed %i elements." num_removals; printfn "%A" timer.Elapsed
                size <- size-1
                if stack.Count = 0 then 
                    d.Remove(max_b) |> ignore
                    max_b <- max_b-1
            | false, _ ->
                max_b <- max_b-1
                remove_max()

        if size = upper_size then remove_max()
        match d.TryGetValue k with
        | true, stack -> stack.Push v
        | false, _ -> d.Add(k,Stack([|v|]))

    member t.PopMin =
        if size = 0 then failwith "Cannot pop an empty queue."
        match d.TryGetValue min_b with
        | true, stack -> 
            let t = stack.Pop()
            size <- size-1
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

let inline get (ar: byte[]) (r,c as p) = // I could have used 2D arrays, but checking victory conditions would have been such a pain then.
    if is_viable_swap p then ar.[r*k+c]
    else failwith "Invalid array get."

let inline set (ar: byte[]) (r,c as p) v =
    if is_viable_swap p then ar.[r*k+c] <- v
    else failwith "Invalid array set."

let inline swap (r1,c1) (r2,c2) (ar: byte[]) =
    let ar = ar.Clone() :?> byte[]
    set ar (r1,c1) (get ar (r1,c1) + get ar (r2,c2))
    set ar (r2,c2) (get ar (r1,c1) - get ar (r2,c2))
    set ar (r1,c1) (get ar (r1,c1) - get ar (r2,c2))
    ar

let inline check_victory (ar: _[]) = // Breaking out of a loop can be a real pain in the ass in F#. 2D arrays are also a pain in the ass.
    let rec loop i =
        if i < ar.Length then
            if ar.[i-1]+1uy = ar.[i] then
                loop (i+1)
            else false
        else true
    loop 1

let astar() =
    let conf_buffer = ResizeArray()
    let conf_buffer2 = ResizeArray()

    let inline heuristic_cost (ar: _[]) =
        let inline manhattan_distance_for_a_single_tile e (r,c) =
            if e <> 0 then abs(r-(e / k)) + abs(c-(e % k)) else 0

        let inline row_linear_conflicts r = // Counts the missmatches in a row
            conf_buffer.Clear()
            conf_buffer2.Clear()
            let columns_of_elements_in_the_correct_row = conf_buffer
            for c=0 to k-1 do
                let x = get ar (r,c) |> int
                if x/k = r && x <> 0 then conf_buffer.Add(x%k)

            let mutable conflicts = 0

            conf_buffer2.AddRange columns_of_elements_in_the_correct_row
            conf_buffer2.Sort()
        
            let sorted_columns_of_elements_in_the_correct_row = conf_buffer2
            for i=0 to sorted_columns_of_elements_in_the_correct_row.Count-1 do
                if sorted_columns_of_elements_in_the_correct_row.[i] <> columns_of_elements_in_the_correct_row.[i] then conflicts <- conflicts+1
            conflicts

        let inline column_linear_conflicts c = // Counts the missmatches in a column
            conf_buffer.Clear()
            conf_buffer2.Clear()

            let rows_of_elements_in_the_correct_column = conf_buffer
            for r=0 to k-1 do
                let x = get ar (r,c) |> int
                if x%k = c && x <> 0 then rows_of_elements_in_the_correct_column.Add(x/k)

            conf_buffer2.AddRange rows_of_elements_in_the_correct_column
            conf_buffer2.Sort()

            let mutable conflicts = 0
            let sorted_rows_of_elements_in_the_correct_column = conf_buffer2
            for i=0 to sorted_rows_of_elements_in_the_correct_column.Count-1 do
                if sorted_rows_of_elements_in_the_correct_column.[i] <> rows_of_elements_in_the_correct_column.[i] then conflicts <- conflicts+1
            conflicts

        let mutable s = 0
        for r=0 to k-1 do
            s <- s + (column_linear_conflicts r + row_linear_conflicts r)*2
            for c=0 to k-1 do
                let e = ar.[r*k+c] |> int
                s <- s + manhattan_distance_for_a_single_tile e (r,c)
        s


    let queue = TabularPriorityQueue(1000)
    let visited_set = Dictionary(1000000)

    let mutable goal = None

    queue.Add (heuristic_cost init) (init,init_pos,[])
    visited_set.Add(init,heuristic_cost init)

    let mutable max_len = 2
    let mutable num_ops = 0

    let rec astar() =
        if goal = None then
            num_ops <- num_ops+1
            let ar, (r,c as p), past_moves = queue.PopMin
            let past_moves_length = past_moves.Length
            if past_moves_length > max_len then
                max_len <- past_moves.Length
                printfn "max_len = %i" max_len
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
                let rec loop i =
                    if i < moves.Length then
                        let r,c,m = moves.[i]
                        let s = swap p (r,c) ar
                        let cs = heuristic_cost s + past_moves_length+1
                        match visited_set.TryGetValue s with
                        | true, v -> 
                            if v > cs then visited_set.[s] <- v
                        | false, _ ->
                            if check_victory s then
                                goal <- Some (s, m::past_moves |> List.rev)
                            else
                                visited_set.Add(s,cs)
                                queue.Add cs (s,(r,c), m::past_moves)
                                loop (i+1)
                loop 0
            astar()
    astar()
    goal.Value, num_ops

#time
let (max_goal, path), num_ops = astar()
let l = path.Length
#time

//printfn "%i" l
//for x in path do printfn "%A" x

