// Before I continue with optimizing JPS, I need to figure out what would its max potential speed be.
// As it is right now, the code is really elegant and I am suspicious of the authors' performance claims over Astar anyway.
// I'll do the same for Fringe search.

// Edit: See comments for jump_point_search_v2.fsx. I am satisfied.

open System
open System.Collections.Generic

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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%""" |> fun x -> x.Split [|'\n'|] |> Array.map (fun x -> x.TrimEnd())
//                                         |> fun x -> // 2D char arrays are strangely less efficient than arrays of strings.
//                                            let t = Array2D.create 37 37 '.'
//                                            for i=0 to x.Length-1 do
//                                                for j=0 to x.[i].Length-1 do
//                                                    t.[i,j] <- x.[i].[j]
//                                            t

let lb_jps () =
    let trace = Array2D.create 37 37 0
    for i=1 to 35 do
        for j=1 to 35 do
            trace.[j,i] <- i+j-1
    trace

let lb_fringe() =
    let trace = Array2D.create 37 37 0
    let buf = Stack(1000)
    for i=1 to 35 do
        for j=1 to 35 do
            buf.Push(i,j)
    while (let i,j = buf.Pop()
           trace.[i,j] <- i+j-1
           buf.Count > 0) do ()
    trace

let lb_jps2 () = // A more realistic stress test. It seems JPS v2 is withing 2x of this
    let right = (0,1)
    let left = (0,-1)
    let up = (-1,0)
    let down = (1,0)

    let up_right = (-1,1)
    let down_right = (1,1)
    let up_left = (-1,-1)
    let down_left = (1,-1)

    let omni = (0,0)

    let doSomething() = printfn "Hello."
    let inline add_node_if_dir_place (pac_r,pac_c as pac_pos) (dir_r, dir_c as dir_pos) (place_r, place_c as place_pos) =
        let inline is_wall x = x = '%'
        let inline is_not_wall x = is_wall x |> not

        if is_wall level.[pac_r + dir_r].[pac_c + dir_c] && is_not_wall level.[pac_r + place_r].[pac_c + place_c]
        then doSomething()

    let trace = Array2D.create 37 37 0
    for i=1 to 35 do
        for j=1 to 35 do
            trace.[i,j] <- i+j-1
            let pac_pos = i,j
            add_node_if_dir_place pac_pos up up_right 
            add_node_if_dir_place pac_pos left down_left 
            add_node_if_dir_place pac_pos left down_left
            add_node_if_dir_place pac_pos right down_right
            add_node_if_dir_place pac_pos up up_right 
            add_node_if_dir_place pac_pos down down_right 
    trace

type IJType =
    struct
    val i : int
    val j : int
    new(a1,a2) = {i=a1;j=a2}
    end

let lb_fringe2() = // Strangely enough struct instead of tuples make the thing slower.
    let trace = Array2D.create 37 37 0
    let buf = Stack(4000)
    for i=1 to 35 do
        for j=1 to 35 do
            buf.Push(IJType(i,j))
    while (let ij = buf.Pop()
           let i,j = ij.i, ij.j
           trace.[i,j] <- i+j-1
           buf.Count > 0) do ()
    trace

let stopwatch = Diagnostics.Stopwatch.StartNew()
for i=1 to 10000 do
    lb_jps() |> ignore
printfn "Time elapsed for lb_jps: %A" stopwatch.Elapsed
stopwatch.Restart()
for i=1 to 10000 do
    lb_fringe() |> ignore
printfn "Time elapsed for lb_fringe: %A" stopwatch.Elapsed
stopwatch.Restart()
for i=1 to 10000 do
    lb_jps2() |> ignore
printfn "Time elapsed for lb_jps2: %A" stopwatch.Elapsed
stopwatch.Restart()
for i=1 to 10000 do
    lb_fringe2() |> ignore
printfn "Time elapsed for lb_fringe2: %A" stopwatch.Elapsed