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

multinomial [|15L;1L|]

180*8

let binomial n k =
    if n >= k && n >= 0 && k >= 0 then
        fact.[n]/(fact.[k]*fact.[n-k])
    else 0L

(* 
// Not sure I get this part. I got some advice from /u/picado, but I cannot make it usable.
// I tried imagining the binomial coefficient pyramid as a tree, but each of the nodes has two parents which makes it impossible to trace along it.

let rec recurse n k i acc =
    printfn "    n=%i k=%i i=%i" n k i
    if n < k || n < 0 || k < 0 then acc |> List.rev
    else
        let l = binomial (n-1) (k-1)
        let r = binomial (n-1) k
        printfn "    i=%i l=%i l+r=%i selected=%i" i l (l+r) (if i >= l then 0 else 1)
        if i >= l then recurse (n-1) k (i-l) (0::acc) else recurse (n-1) (k-1) i (1::acc)

recurse 10 3 75L []
*)