// Generates random puzzles.
// Merely permuting an array [|0..k*k-1|] does not work as more than half the instances are unsolvable.

let k = 4
let num_random_moves = 40

open System

let init_pos, init = 
    let inline is_viable_swap (r,c) =
        r >= 0 && c >= 0 && r < k && c < k

    let inline get (ar: byte[]) (r,c as p) = // I could have used 2D arrays, but checking victory conditions would have been such a pain then.
        if is_viable_swap p then ar.[r*k+c]
        else failwith "Invalid array get."

    let inline set (ar: byte[]) (r,c as p) v =
        if is_viable_swap p then ar.[r*k+c] <- v
        else failwith "Invalid array set."

    let inline swap (r1,c1) (r2,c2) (ar: byte[]) =
        let ar = ar.Clone() :?> byte[]
        set ar (r1,c1) (get ar (r1,c1) + get ar (r2,c2))
        set ar (r2,c2) (get ar (r1,c1) - get ar (r2,c2))
        set ar (r1,c1) (get ar (r1,c1) - get ar (r2,c2))
        ar
    let mutable init = [|0uy..k*k-1 |> byte|]
    let mutable init_pos =
        init |> Array.findIndex ((=)0uy) // Partial application of the = operator.
             |> fun x -> x/k,x%k
    let rng = Random()
    for i=1 to num_random_moves do
        let r,c = init_pos
        let moves = [|-1+r,c; // UP
        r,-1+c; // LEFT
        r,1+c; // RIGHT
        1+r,c|] // DOWN
        let move = moves.[rng.Next(0,4)]
        if is_viable_swap move then
            init <- swap init_pos move init
            init_pos <- move
    init_pos, init

