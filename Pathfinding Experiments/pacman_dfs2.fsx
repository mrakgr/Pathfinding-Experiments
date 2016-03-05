// For the http://stackoverflow.com/questions/9201166/iterative-dfs-vs-recursive-dfs-and-different-elements-order

// I want to kill whoever made this shitty problem. I got it working in 45m and then spent like 3h trying to figure out what the 
// hell are the extra conditions.

// Edit: Well, actually my last version was really bad, but this DFS is actually proper. Too bad I still do not understand the problem 
// as it is defined on Hacker Rank

open System
open System.Collections.Generic

//let pac_r, pac_c as pac_pos = 3, 9
//let food_r, food_c as food_pos = 5, 1
//let rows, cols = 7, 20
//let level = """%%%%%%%%%%%%%%%%%%%%
//%--------------%---%  
//%-%%-%%-%%-%%-%%-%-%  
//%--------P-------%-%  
//%%%%%%%%%%%%%%%%%%-%  
//%.-----------------%  
//%%%%%%%%%%%%%%%%%%%%""" |> fun x -> x.Split [|'\n'|]

//let pac_r, pac_c as pac_pos = 25, 13
//let food_r, food_c as food_pos = 3, 1
//let rows, cols = 27, 28
//let level = """%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%------------%%------------%
//%-%%%%-%%%%%-%%-%%%%%-%%%%-%
//%.%%%%-%%%%%-%%-%%%%%-%%%%-%
//%-%%%%-%%%%%-%%-%%%%%-%%%%-%
//%--------------------------%
//%-%%%%-%%-%%%%%%%%-%%-%%%%-%
//%-%%%%-%%-%%%%%%%%-%%-%%%%-%
//%------%%----%%----%%------%
//%%%%%%-%%%%%-%%-%%%%%-%%%%%%
//%%%%%%-%%%%%-%%-%%%%%-%%%%%%
//%%%%%%-%------------%-%%%%%%
//%%%%%%-%-%%%%--%%%%-%-%%%%%%
//%--------%--------%--------%
//%%%%%%-%-%%%%%%%%%%-%-%%%%%%
//%%%%%%-%------------%-%%%%%%
//%%%%%%-%-%%%%%%%%%%-%-%%%%%%
//%------------%%------------%
//%-%%%%-%%%%%-%%-%%%%%-%%%%-%
//%-%%%%-%%%%%-%%-%%%%%-%%%%-%
//%---%%----------------%%---%
//%%%-%%-%%-%%%%%%%%-%%-%%-%%%
//%%%-%%-%%-%%%%%%%%-%%-%%-%%%
//%------%%----%%----%%------%
//%-%%%%%%%%%%-%%-%%%%%%%%%%-%
//%------------P-------------%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%""" |> fun x -> x.Split [|'\n'|]

let [|pac_r;pac_c|] = Console.ReadLine() |> fun x -> x.Split [|' '|] |> Array.map Int32.Parse
let pac_pos = pac_r, pac_c
let [|food_r;food_c|] = Console.ReadLine() |> fun x -> x.Split [|' '|] |> Array.map Int32.Parse
let food_pos = food_r, food_c
let [|rows;cols|] = Console.ReadLine() |> fun x -> x.Split [|' '|] |> Array.map Int32.Parse
let level = 
    [|
    for i=1 to rows do
        yield Console.ReadLine()
        |]  

let mutable num_results = 0

let mutable max_goal = Int32.MaxValue
let mutable max_trace = []

let trace = Array2D.create rows cols 0 // I should have used 2D arrays, but I was not thinking straight last night.
let inline set (x, y) i =
    trace.[x,y] <- i
let inline is_visited_and_less_than i (x, y) =
    trace.[x,y] <> 0 && trace.[x,y] < i
let inline is_not_visited (x, y) =
    trace.[x,y] = 0
let rec get_trace_from (x, y as p) i accum =
    [|1+x,0+y;0+x,1+y;0+x,-1+y;-1+x,0+y|] // UP, LEFT, RIGHT, DOWN
    |> Array.tryFind (is_visited_and_less_than i)
    |> function
       | Some n -> get_trace_from n (i-1) (p::accum)
       | None -> p::accum

let rec dfs ((pac_r, pac_c as pac_pos), i) =
    if pac_pos = food_pos then 
        num_results <- num_results+1
        if max_goal > i then 
            max_goal <- i
//            printfn "Goal: %i. #: %i" i num_results
            max_trace <- get_trace_from food_pos i []
    else
        set pac_pos i
        [|1+pac_r,pac_c;pac_r,1+pac_c;pac_r,-1+pac_c;-1+pac_r,pac_c|]
        |> Array.choose (fun (n_r,n_c as x) -> 
            if level.[n_r].[n_c] <> '%' && is_not_visited x && i+1 < max_goal then Some (x, i+1) else None)
        |> Array.iter (fun next -> dfs next)
        set pac_pos 0

dfs (pac_pos, 1)

printfn "%i" max_trace.Length
for (r,c) in max_trace do
    printfn "%i %i" r c

printfn "%i" (max_trace.Length-1)
for (r,c) in max_trace do
    printfn "%i %i" r c