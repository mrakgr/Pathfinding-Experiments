// I finally got it. A lot of combinatorics reduces to dynamic programming.
// Marking each node only once for every combination ensures that the correct coefficients are returned in the end.
// It also makes the multinomial function linear in time rather than exponential.

// ...Unfortunately, the hash function derived from the coefficients gets me only about 20-25% error. It is shit.

open System
open System.Collections.Generic

let memoize f (h : int[] -> int) = // h function hashes the int array to an int
    let cache = new Dictionary<int[],int>(HashIdentity.Structural)
    fun (x: int[]) p -> 
        match cache.TryGetValue(x) with
        | true, y -> y
        | _       -> 
            let v = f x p
            cache.Add(x, v)
            v

let hash_func hash_ar ar =
    Array.fold2 (fun s e1 e2 -> s+e1*e2) 0 ar hash_ar

let multinomial_coefs (start_ar : int[]) =

    let num_vars = start_ar.Length
    let ar_elems = start_ar |> Array.sum // The total size of the hash coefficient array.

    let ar_hash_func = // A more efficient hash function for the dictionary.
        let hash_ar =
            start_ar
            |> Array.scan (fun s e -> (e+1)*s) 1
            |> Array.take num_vars

        hash_func hash_ar // Partial function application

    let forward_combinations = ref (fun _ _ -> failwith "Not initialized yet.") // Forward declaration of the memoized function.
    let coef_ar = Array.create ar_elems 0

    let combinations (ar : int[]) p =
        if p < ar_elems then
            let mutable s = 0
            for i=0 to num_vars-1 do
                if ar.[i] > 0 then
                    let t = ar |> Array.copy
                    t.[i] <- t.[i]-1
                    s <- s + !forward_combinations t (p+1) // As the function is memoized, each node is called only once.
                    //s <- s + combinations t (p+1) // This is how the function would look without memoization.
            coef_ar.[p] <- coef_ar.[p]+s
            s
        else 1

    forward_combinations := memoize combinations ar_hash_func // While Fsharp supports recursion, this is the only way I can memoize a function calling itself.
    !forward_combinations start_ar 0 |> ignore // Calls the function and ignores the returned int.

    coef_ar
    |> Array.tail // Some postprocessing for the array.
    |> Array.rev
    |> Array.append [|1|]

let multinomials = [|3;1;1;1|]
let coefs = multinomial_coefs multinomials

// Past here is collision checking.

let perm_ar = // [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 2; 3|] or the like. Make sure the zeroes are the largest in number.
    [|
    for i=0 to multinomials.Length-1 do
        for j=1 to multinomials.[i] do
            yield i
    |]

#r "../packages/Combinatorics.1.0.3.2/lib/net40/Combinatorics.dll"
open Combinatorics.Collections

let p = Permutations(perm_ar,GenerateOption.WithoutRepetition) // Lazily generates permutations without repetition.

let perm_hash_func = 
    hash_func coefs // Partial function application.
let perms =
    [|
    for x in p do
        let x = x |> Seq.toArray
        yield x |> Seq.toArray, (perm_hash_func x)
        |]

let len_distict_perms = 
    perms
    |> Array.distinctBy (fun (l,r) -> r)
    |> fun x -> x.Length

let sorted_perms =
    perms
    |> Array.sortBy (fun (l,r) -> r)
