open System
open System.Collections.Generic

let pac_r, pac_c as pac_pos = 35, 35
let food_r, food_c as food_pos = 35, 1
let rows, cols = 37, 37
let level = """%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%-------%-%-%-----------%---%-----%-%
%-%%%%%%%-%-%%%-%-%%%-%%%-%%%%%%%-%-%
%-------%-------%-%-----%-----%-%---%
%%%%%-%%%%%-%%%-%-%-%-%%%-%%%%%-%-%%%
%---%-%-%-%---%-%-%-%---%-%---%-%---%
%-%%%-%-%-%-%%%-%%%%%-%%%-%-%%%-%%%-%
%-------%-----%---%---%-----%-%-%---%
%%%-%%%%%%%%%-%%%%%%%-%%%-%%%-%-%-%-%
%-------------%-------%-%---%-----%-%
%-%-%%%%%-%-%%%-%-%-%%%-%-%%%-%%%-%-%
%-%-%-----%-%-%-%-%-----%---%-%-%-%-%
%-%-%-%%%%%%%-%-%%%%%%%%%-%%%-%-%%%-%
%-%-%-%-----%---%-----%-----%---%---%
%%%-%%%-%-%%%%%-%%%%%-%%%-%%%-%%%%%-%
%-----%-%-%-----%-%-----%-%---%-%-%-%
%-%-%-%-%-%%%-%%%-%%%-%%%-%-%-%-%-%-%
%-%-%-%-%-----------------%-%-%-----%
%%%-%%%%%%%-%-%-%%%%%-%%%-%-%%%-%%%%%
%-------%-%-%-%-----%---%-----%-%---%
%%%%%-%-%-%%%%%%%%%-%%%%%%%%%%%-%-%%%
%---%-%-----------%-%-----%---%-%---%
%-%%%-%%%%%-%%%%%%%%%-%%%%%-%-%-%%%-%
%-%---%------%--------%-----%-------%
%-%-%-%%%%%-%%%-%-%-%-%-%%%%%%%%%%%%%
%-%-%---%-----%-%-%-%-------%---%-%-%
%-%-%%%-%%%-%-%-%-%%%%%%%%%-%%%-%-%-%
%-%---%-%---%-%-%---%-%---%-%-%-----%
%-%%%-%%%-%%%%%-%%%-%-%-%%%%%-%-%%%%%
%-------%---%-----%-%-----%---%-%---%
%%%-%-%%%%%-%%%%%-%%%-%%%-%-%%%-%-%%%
%-%-%-%-%-%-%-%-----%-%---%-%---%-%-%
%-%-%%%-%-%-%-%-%%%%%%%%%-%-%-%-%-%-%
%---%---%---%-----------------%-----%
%-%-%-%-%%%-%%%-%%%%%%%-%%%-%%%-%%%-%
%.%-%-%-------%---%-------%---%-%--P%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%""" |> fun x -> x.Split [|'\n'|]

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

let bfs() =
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

    let queue = Queue(10000)

    let rec bfs() =
        if max_goal = Int32.MaxValue then
            let (pac_r,pac_c as pac_pos),i = queue.Dequeue()
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
                            queue.Enqueue(x,i+1); set x (i+1); 
                            loop <| j+1
                        else set x (i+1); max_goal <- i+1
                loop 0
            bfs ()

    queue.Enqueue(pac_pos,1)
    set pac_pos 1
    bfs()
    max_goal, get_trace_from food_pos max_goal []
let max_goal, path = bfs()

printfn "%A" <| path.Length-1
for (l,r) in path do printfn "%i %i" l r