// I fucked up. It should be possible to use a flat array to store all the values. This code is just spew.

let inline sequence_extractor (state : byte[]) (symbols : byte[]) = 
    state
    |> Array.mapi (fun pos symbol -> symbol, byte pos)
    |> fun x -> x |> Array.sortInPlaceBy (fun (symbol, pos) -> symbol); x
    |> fun symbol_sorted_sequence -> 
        [|for i=1 to symbols.Length-1 do 
            if symbols.[i-1] < symbols.[i] then yield symbol_sorted_sequence.[symbols.[i-1] |> int] else failwith "Symbols have to be ordered!"
            yield symbol_sorted_sequence.[symbols |> Array.last |> int] |]

type Trie =
| TrieNode of dict : Dictionary<byte,Trie> * symbol : byte
| TrieValue of int

    member t.GetValueAtState (state : byte[]) (symbols : byte[]) =
        let sequence = sequence_extractor state symbols

        let rec loop t i =
            if i < sequence.Length then
                let symbol, pos = sequence.[i]
                match t with
                | TrieNode(dict,node_symbol) ->
                    if symbol <> node_symbol then failwithf "Node symbol %i does not equal %i in the sequence at %i." node_symbol symbol i
                    match dict.TryGetValue pos with
                    | true, v -> loop v (i+1)
                    | false, _ -> None
                | TrieValue _ -> failwith "Value at wrong position in the trie."
            else
                match t with
                | TrieNode _ -> failwith "Value at wrong position in the trie."
                | TrieValue x -> Some x
        loop t 0

    member t.SetSequence(end_state : byte[], past_moves : Moves [], symbols : byte[]) =
        let set_sequence(sequence : (byte * byte) []) (value) =
            let rec loop t i =
                if i < sequence.Length then
                    let symbol, pos = sequence.[i]
                    match t with
                    | TrieNode(dict,node_symbol) ->
                        if symbol <> node_symbol then failwithf "Node symbol %i does not equal %i in the sequence at %i." node_symbol symbol i
                        match dict.TryGetValue pos with
                        | true, v -> loop v (i+1)
                        | false, _ ->
                            let v = if i < sequence.Length-1 then TrieNode(Dictionary(),symbol) else TrieValue value
                            dict.[pos] <- v
                            loop v (i+1)
                    | TrieValue _ -> if i < sequence.Length then failwith "Value at wrong position in the trie."
            loop t 0

        let rec loop (state, (r,c as p)) i  =
            let reflected_swap() =
                let move = past_moves.[i] |> int
                [|1+r,c; // Reflected UP
                r,1+c; // Reflected LEFT
                r,-1+c; // Reflected RIGHT
                -1+r,c|] // Reflected DOWN
                |> fun next_position -> next_position.[move] // Selects the swap move.
                |> fun next_position ->
                    swap p next_position state, next_position

            if i < past_moves.Length then
                set_sequence (sequence_extractor state symbols) (past_moves.Length-i)
                let (next_state, next_p as n) = reflected_swap()
                loop n (i+1)

        let start_seq = sequence_extractor end_state symbols
        let pos_of_zero =
            let symbol,pos = start_seq.[0]
            if symbol = 0uy then pos |> int |> fun pos -> pos/k, pos%k else failwith "Zero should always be in first place."
        set_sequence start_seq past_moves.Length
        loop (end_state, pos_of_zero) 0

