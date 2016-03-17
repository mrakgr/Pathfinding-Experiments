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

let multinomial_decoder (carr : int64[]) k =
    let rec multinomial_decoder (carr : int64[]) k accu =
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
                multinomial_decoder next_carr next_k ((int64 i)::accu)
        else accu |> List.toArray |> Array.rev
    multinomial_decoder carr k []

let multinomial_encoder (carr : int64[]) (str : int64[]) =
    let rec multinomial_encoder (carr : int64[]) (str : int64[]) ind lb =
        let num_vars = Array.sum carr
        if num_vars > 0L then
            let n = str.[ind] |> int
            let m = multinomial carr 
            [| // Filters out zeroes, scans and appends the index position of the variable in the carr array.
            let mutable s = 0L
            for i=0 to carr.Length-1 do
                yield s
                s <- s + carr.[i] * m / num_vars
            |] 
            |> fun x -> x.[n]
            |> fun v ->
                let next_lb = lb+v
                let next_carr =
                    carr
                    |> Array.copy
                    |> fun x -> 
                        x.[n] <- x.[n]-1L
                        if x.[n] < 0L then failwith "Invalid string given."
                        x
                multinomial_encoder next_carr str (ind+1) next_lb
        else lb
    multinomial_encoder carr str 0 0L
    
let multinomials = [|2L;2L|]
multinomial multinomials
let perm_ar = 
    [|
    for i=0 to multinomials.Length-1 do
        for j=1 to int multinomials.[i] do
            yield int64 i
    |]

let test =
    Permutations(perm_ar,GenerateOption.WithoutRepetition)
    |> Seq.toArray
    |> Array.map (Seq.toArray >> Array.map int64)
    |> Array.sort
    |> Array.mapi (fun i x -> 
        let k = multinomial_encoder multinomials x
        let str = multinomial_decoder multinomials k
        x, k, str, str = x)

test |> Array.filter (fun (_,_,_,x) -> not x)
