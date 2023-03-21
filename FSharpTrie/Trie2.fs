module FSharpTrie.Trie2

type T =
    | Root of Map<char, T>
    | Node of Map<char, T> * bool

let Leaf = Node(Map.empty, true)

let createRoot () = Root Map.empty

let getChildren trie =
    match trie with
    | Root c -> c
    | Node (c, _) -> c

let tryGetChild key = getChildren >> Map.tryFind key

let tryFindNode word trie =
    let rec tryFindNodeByChars chars currentTrie =
        match chars with
        | x :: xs ->
            tryGetChild x currentTrie
            |> Option.bind (tryFindNodeByChars xs)
        | [] -> Some currentTrie

    tryFindNodeByChars (word |> Seq.toList) trie

let rec createSubtree chars =
    match chars with
    | x :: xs -> Node(Map[(x, createSubtree xs)], false)
    | [] -> Leaf

let addChild key child trie =
    match trie with
    | Root c -> c |> Map.add key child |> Root
    | Node (c, completed) -> Node(c |> Map.add key child, completed)

let put word trie =
    let rec putChars chars currentTrie =
        match chars with
        | x :: xs ->
            match tryGetChild x currentTrie with
            | Some child -> addChild x (putChars xs child) currentTrie
            | None -> addChild x (createSubtree xs) currentTrie
        | [] -> Leaf

    putChars (word |> Seq.toList) trie
