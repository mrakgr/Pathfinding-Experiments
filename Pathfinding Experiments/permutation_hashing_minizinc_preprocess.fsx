// Given the failure of the previous brilliant idea, I will try solving this in MiniZinc

let multinomials = [|14;1;1|]

let perm_ar = 
    [|
    for i=0 to multinomials.Length-1 do
        for j=1 to multinomials.[i] do
            yield i
    |]

#r "../packages/Combinatorics.1.0.3.2/lib/net40/Combinatorics.dll"
open Combinatorics.Collections

let p = Permutations(perm_ar,GenerateOption.WithoutRepetition) // Lazily generates permutations without repetition.

let perms =
    [|
    for x in p do
        let x = x |> Seq.toArray
        yield x |> Seq.toArray
    |]

printfn "num_perms = %i;" perms.Length
printfn "num_vars = %i;" (multinomials |> Array.sum)

printfn "multinomial_coeffs = ["
for i=0 to perms.Length-1 do
    let sb = System.Text.StringBuilder()
    let perms = perms.[i]

    for j=0 to perms.Length-2 do
        sb.Append(perms.[j]) |> ignore
        sb.Append(", ") |> ignore
    sb.Append(perms |> Array.last) |> ignore
    printfn "   | %s" (sb.ToString())
printfn "   |];"

//printf "target_costs = ["
//for i=0 to perms.Length-2 do
//    printf "%i, " (i+1)
//printfn "%i];" (perms.Length)