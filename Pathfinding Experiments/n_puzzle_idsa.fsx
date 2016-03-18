// Idsa search for the N puzzle problem with the tabular priority queue.

// Update: Idsa is kind of bad, but Fringe Search might be worth trying out: http://cswww.essex.ac.uk/cig/2005/papers/p1039.pdf
// Also, memory enhanced IDSA should be worth a look into.
// This paper on cache efficient structures caught my eye as well, referenced from the Fringe Search paper: http://cswww.essex.ac.uk/cig/2005/papers/p1039.pdf

let k = 4
let init_pos, init = (1, 2), [|15uy; 12uy; 9uy; 14uy; 5uy; 4uy; 0uy; 1uy; 3uy; 6uy; 2uy; 13uy; 7uy; 11uy; 8uy; 10uy|] // Hard puzzle
//let init_pos, init = (1, 2), [|4; 1; 2; 3; 8; 6; 0; 10; 9; 5; 15; 7; 12; 13; 11; 14|]

open System
open System.Collections.Generic

type Moves =
| UP = 0
| LEFT = 1
| RIGHT = 2
| DOWN = 3

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

let idsa_star() =
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
            //s <- s + (column_linear_conflicts r + row_linear_conflicts r)*2
            for c=0 to k-1 do
                let e = ar.[r*k+c] |> int
                s <- s + manhattan_distance_for_a_single_tile e (r,c)
        s

    let queue = Stack(100)
    let mutable goal = None

    queue.Push(init,init_pos,[])
    let mutable num_ops = 0

    let rec idsa_star max_heuristic_cost =
        if goal = None && queue.Count > 0 then
            num_ops <- num_ops+1
            let ar, (r,c as p), past_moves = queue.Pop()
            let past_moves_length = past_moves.Length
            [|-1+r,c,Moves.UP;
            r,-1+c,Moves.LEFT;
            r,1+c,Moves.RIGHT;
            1+r,c,Moves.DOWN|]
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
                        if check_victory s then
                            goal <- Some (s, m::past_moves |> List.rev)
                        else
                            if (heuristic_cost s + (past_moves_length+1)) <= max_heuristic_cost then
                                queue.Push(s,(r,c), m::past_moves)
                            loop (i+1)
                loop 0
            idsa_star max_heuristic_cost
        elif goal = None && queue.Count = 0 then
            printfn "Deepening to %i..." (max_heuristic_cost+1)
            queue.Push(init,init_pos,[])
            idsa_star <| max_heuristic_cost+1
    idsa_star <| heuristic_cost init
    goal.Value, num_ops

#time
let (max_goal, path), num_ops = idsa_star()
let l = path.Length
#time

//printfn "%i" l
//for x in path do printfn "%A" x

