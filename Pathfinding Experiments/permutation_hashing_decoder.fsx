#r "../packages/Combinatorics.1.0.3.2/lib/net40/Combinatorics.dll"

open System
open Combinatorics
open Combinatorics.Collections

let fact =
    [|
    yield 1L

    let mutable s = 1L
    for i=2L to 21L do
        yield s
        s <- s*i
    |]

let multinomial (carr : int64[]) = 
    let u = carr |> Array.sum |> fun x -> fact.[x |> int]
    let d = carr |> Array.fold (fun s e -> s*fact.[e |> int]) 1L
    u/d

let rec multinomial_generator (carr : int64[]) k accu =
    let num_vars = Array.sum carr
    if num_vars > 0L then
        let m = multinomial carr 
        [| // Filters out zeroes, scans and appends the index position of the variable in the carr array.
        let mutable s = 0L
        for i=0 to carr.Length-1 do
            if carr.[i] > 0L then
                yield s,i
                s <- s + carr.[i] * m / num_vars
        |] 
        |> Array.findBack (fun (l,_) -> k >= l)
        |> fun (v,i) ->
            let next_k = k - v
            let next_carr =
                carr
                |> Array.copy
                |> fun x -> x.[i] <- x.[i]-1L; x
            multinomial_generator next_carr next_k (i::accu)
    else accu |> List.toArray
    
let ar = [|4L;1L;1L;1L|]
let m = multinomial ar

let own =
    [|
    for i in {0L..m-1L} do
        yield multinomial_generator ar i []
    |] |> Array.sort

let lib =
    Permutations([|1;2;3;0;0;0;0|],GenerateOption.WithoutRepetition)
    |> Seq.toArray
    |> Array.map Seq.toArray
    |> Array.sort

own = lib