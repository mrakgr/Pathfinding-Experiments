#r "../packages/Combinatorics.1.0.3.2/lib/net40/Combinatorics.dll"

open System
open Combinatorics
open Combinatorics.Collections

let p = Permutations([|1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0|],GenerateOption.WithoutRepetition)

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

let arith_series n a1 an = n*(a1+an)/2

let l = multinomial [|13L;1L;1L;1L|]
