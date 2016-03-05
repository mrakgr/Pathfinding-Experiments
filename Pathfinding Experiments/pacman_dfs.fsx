// For the http://stackoverflow.com/questions/9201166/iterative-dfs-vs-recursive-dfs-and-different-elements-order

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

let pac_r, pac_c as pac_pos = 25, 13
let food_r, food_c as food_pos = 3, 1
let rows, cols = 27, 28
let level = """%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%------------%%------------%
%-%%%%-%%%%%-%%-%%%%%-%%%%-%
%.%%%%-%%%%%-%%-%%%%%-%%%%-%
%-%%%%-%%%%%-%%-%%%%%-%%%%-%
%--------------------------%
%-%%%%-%%-%%%%%%%%-%%-%%%%-%
%-%%%%-%%-%%%%%%%%-%%-%%%%-%
%------%%----%%----%%------%
%%%%%%-%%%%%-%%-%%%%%-%%%%%%
%%%%%%-%%%%%-%%-%%%%%-%%%%%%
%%%%%%-%------------%-%%%%%%
%%%%%%-%-%%%%--%%%%-%-%%%%%%
%--------%--------%--------%
%%%%%%-%-%%%%%%%%%%-%-%%%%%%
%%%%%%-%------------%-%%%%%%
%%%%%%-%-%%%%%%%%%%-%-%%%%%%
%------------%%------------%
%-%%%%-%%%%%-%%-%%%%%-%%%%-%
%-%%%%-%%%%%-%%-%%%%%-%%%%-%
%---%%----------------%%---%
%%%-%%-%%-%%%%%%%%-%%-%%-%%%
%%%-%%-%%-%%%%%%%%-%%-%%-%%%
%------%%----%%----%%------%
%-%%%%%%%%%%-%%-%%%%%%%%%%-%
%------------P-------------%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%""" |> fun x -> x.Split [|'\n'|]

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

let rec dfs ((pac_r, pac_c as pac_pos),i) (nodes_visited : Map<(int*int),int>) (nodes_expanded : Map<(int*int),int>) =
    if pac_pos = food_pos then 
        (nodes_visited |> Map.add pac_pos (i+1), nodes_expanded) |> Some
    else
        if Map.containsKey pac_pos nodes_visited = false then
            let add (r, c) =
                pac_r+r, pac_c+c
            [|-1,0;0,-1;0,1;1,0|] // UP, LEFT, RIGHT, DOWN
            |> Array.rev // Recursive DFS becomes equivalent to iterative DFS when inputs are reversed. http://stackoverflow.com/questions/9201166/iterative-dfs-vs-recursive-dfs-and-different-elements-order
            |> Array.choose (fun x -> 
                let (n_r,n_c as x) = add x
                if level.[n_r].[n_c] <> '%' then Some (x, i+1) else None)
            |> fun new_nodes_expanded -> 
                new_nodes_expanded
                |> Array.choose (fun next ->
                    dfs next (nodes_visited |> Map.add pac_pos (i+1)) (Array.fold (fun state (pos, i) -> if state.ContainsKey pos = false then state |> Map.add pos i else state) nodes_expanded new_nodes_expanded)
                    )
                |> fun x -> if x.Length > 0 then Array.minBy (fun (nodes_visited : Map<_,_>,_) -> nodes_visited.Count) x |> Some else None
        else None

let (r1,r2) = 
    dfs (pac_pos,0) Map.empty <| Map([|pac_pos,0|])
    |> fun x -> x.Value
    |> fun (nodes_visited, nodes_expanded) -> nodes_visited |> Map.toArray, nodes_expanded |> Map.toArray
    |> fun (nodes_visited, nodes_expanded) -> nodes_visited |> Array.sortBy (fun (_,y) -> y), nodes_expanded |> Array.sortBy (fun (_,y) -> y)
    |> fun (nodes_visited, nodes_expanded) -> nodes_visited |> Array.map (fun (k,_) -> k), nodes_expanded |> Array.map (fun (k,_) -> k)

printfn "%i" r1.Length
for (r,c) in r1 do
    printfn "%i %i" r c

printfn "%i" (r1.Length-1)
for (r,c) in r1 do
    printfn "%i %i" r c

