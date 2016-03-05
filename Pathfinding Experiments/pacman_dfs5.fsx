// Hopefully this will be the final try. This 'easy' problem is taking up way too much of my time.
// As suggested in the forums, I'll just replace the queue with a stack.

// I got 10 points for this. I can't believe this did not work.
// Seriously, fuck this shitty assignment.
// I must have tried nearly a dozen different kinds of DFS search.

// Ah, enough. This is ridicoulous. I must have spent almost a day on this shit.

// Let me get A* working and then I'll go back to the GVGAI library.

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

let pac_r, pac_c as pac_pos = 11, 9
let food_r, food_c as food_pos = 2, 15
let rows, cols = 13, 20
let level = """%%%%%%%%%%%%%%%%%%%%
%----%--------%----%
%-%%-%-%%--%%-%.%%-%
%-%-----%--%-----%-%
%-%-%%-%%--%%-%%-%-%
%-----------%-%----%
%-%----%%%%%%-%--%-%
%-%----%----%-%--%-%
%-%----%-%%%%-%--%-%
%-%-----------%--%-%
%-%%-%-%%%%%%-%-%%-%
%----%---P----%----%
%%%%%%%%%%%%%%%%%%%%""" |> fun x -> x.Split [|'\n'|]

//let [|pac_r;pac_c|] = Console.ReadLine() |> fun x -> x.Split [|' '|] |> Array.map Int32.Parse
//let pac_pos = pac_r, pac_c
//let [|food_r;food_c|] = Console.ReadLine() |> fun x -> x.Split [|' '|] |> Array.map Int32.Parse
//let food_pos = food_r, food_c
//let [|rows;cols|] = Console.ReadLine() |> fun x -> x.Split [|' '|] |> Array.map Int32.Parse
//let level = 
//    [|
//    for i=1 to rows do
//        yield Console.ReadLine()
//        |]  

let dfs() =
    let mutable max_goal = Int32.MaxValue

    let trace = Array2D.create rows cols 0
    let inline set (x, y) i =
        trace.[x,y] <- i
    let inline is_visited_and_less_than i (x, y) =
        trace.[x,y] <> 0 && trace.[x,y] = i-1
    let inline is_not_visited (x, y) =
        trace.[x,y] = 0
    let rec get_trace_from (pac_r, pac_c as p) i accum =
        [|-1+pac_r,pac_c;pac_r,-1+pac_c;pac_r,1+pac_c;1+pac_r,pac_c;|] // UP, LEFT, RIGHT, DOWN
        |> Array.tryFind (is_visited_and_less_than i)
        |> function
           | Some n -> get_trace_from n (i-1) (p::accum)
           | None -> p::accum

    let stack = Stack(10000)
    let expanded_nodes = ResizeArray()

    let rec dfs() =
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
            let (pac_r,pac_c as pac_pos),i = stack.Pop()
            expanded_nodes.Add(pac_pos)
//            print_expansion expanded_nodes
            [|-1+pac_r,pac_c; // UP
            pac_r,-1+pac_c; // LEFT
            pac_r,1+pac_c; // RIGHT
            1+pac_r,pac_c;|] // DOWN
            |> Array.filter (fun (n_r,n_c as x) -> 
                level.[n_r].[n_c] <> '%' && is_not_visited x && i+1 < max_goal)
            |> fun ar ->
                let rec loop j =
                    if j < ar.Length then
                        let x = ar.[j]
                        if x <> food_pos then
                            stack.Push(x,i+1); set x (i+1);
                            loop <| j+1
                        else set x (i+1); max_goal <- i+1; expanded_nodes.Add(x)
                loop 0
            dfs ()

    stack.Push(pac_pos,1)
    set pac_pos 1
    dfs()
    max_goal, get_trace_from food_pos max_goal [], expanded_nodes.ToArray()
let max_goal, path, ex = dfs()

printfn "%A" ex.Length
for (l,r) in ex do printfn "%i %i" l r
printfn "%A" <| path.Length-1
for (l,r) in path do printfn "%i %i" l r
