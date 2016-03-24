#if INTERACTIVE
#r "../packages/Combinatorics.1.0.3.2/lib/net40/Combinatorics.dll"
#endif

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
    let d = //carr |> Array.fold (fun s e -> s*fact.[e |> int]) 1L
        let mutable s = fact.[carr.[0] |> int] // Speed optimization
        for i=1 to carr.Length-1 do
            s <- s*fact.[carr.[i] |> int]
        s
    u/d

let multinomial_decoder (carr : int64[]) k =
    let num_vars = Array.sum carr
    let result = Array.zeroCreate (num_vars |> int)
    let rec multinomial_decoder (carr : int64[]) k ind =
        if ind < num_vars then
            let m = multinomial carr 
            // Filters out zeroes, scans and appends the index position of the variable in the carr array.
            let mutable s = 0L
            let rec findBack i =
                if i < carr.Length then
                    let t = s
                    s <- s + carr.[i] * m / (num_vars - ind (* The number of vars in the current coefficient array. *))
                    if k < s then t, i else findBack <| i+1
                else s, i
            findBack 0
            |> fun (l,i) ->
                let next_k = k - l
                let next_carr =
                    carr.[i] <- carr.[i]-1L; carr
                result.[ind |> int] <- i |> int64
                multinomial_decoder next_carr next_k (ind+1L)
        else result
    multinomial_decoder (carr |> Array.copy) k 0L

let multinomial_encoder (carr : int64[]) (str : int64[]) =
    let num_vars = Array.sum carr // This attempted optimization does not seem to be doing any better than summing the array, but nevermind it.
    let rec multinomial_encoder (carr : int64[]) (str : int64[]) ind lb =
        if ind < num_vars then
            let n = str.[ind |> int] |> int
            let m = multinomial carr 
            let mutable v = 0L
            for i=0 to n-1 do
                v <- v + carr.[i] * m / (num_vars - ind (* The number of vars in the current coefficient array. *) )

            let next_lb = lb+v
            let next_carr =
                carr.[n] <- carr.[n]-1L
                if carr.[n] < 0L then failwith "Invalid string given."
                carr
            multinomial_encoder next_carr str (ind+1L) next_lb
        else lb
    multinomial_encoder (carr |> Array.copy) str 0L 0L
    
let multinomials = [|11L;1L;1L;1L;1L;1L|]

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

test 
|> Array.filter (fun (_,_,_,x) -> not x)
|> fun x ->
    if x.Length > 0 then
        printfn "%A" x
        failwith "Encoder and decoder are broken!"

let stopwatch = Diagnostics.Stopwatch.StartNew()
for i=0 to 100000 do
    multinomial_encoder multinomials perm_ar |> ignore

printfn "Time elapsed for encoder: %A" stopwatch.Elapsed
stopwatch.Restart()

for i=0 to 100000 do
    multinomial_decoder multinomials 2L |> ignore

printfn "Time elapsed for decoder: %A" stopwatch.Elapsed

