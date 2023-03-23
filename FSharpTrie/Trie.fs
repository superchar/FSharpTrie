module FSharpTrie.Trie2

type T =
    | Root of Map<char, T>
    | Node of Map<char, T> * bool
    | Leaf

let private getChildren trie =
    match trie with
    | Root c -> c
    | Node (c, _) -> c
    | Leaf -> Map.empty

let private isCompleted trie =
    match trie with
    | Root _ -> false
    | Node (_, completed) -> completed
    | Leaf -> true

let private isCompletedOption =
    isCompleted >> Some

let private tryGetChild key = getChildren >> Map.tryFind key

let rec private createSubtree chars =
    match chars with
    | x :: xs -> Node(Map[(x, createSubtree xs)], false)
    | [] -> Leaf

let private addChild key child trie =
    match trie with
    | Root c -> c |> Map.add key child |> Root
    | Node (c, completed) -> Node(c |> Map.add key child, completed)
    | Leaf -> Node(Map [ (key, child) ], true)

let createRoot () = Root Map.empty

let tryFindNode word trie =
    let rec tryFindNodeByChars chars =
        match chars with
        | x :: xs ->
            tryGetChild x
            >> Option.bind (tryFindNodeByChars xs)
        | [] -> Some

    tryFindNodeByChars (word |> Seq.toList) trie

let containsPrefix word = tryFindNode word >> Option.isSome

let contains word =
    tryFindNode word
    >> Option.bind isCompletedOption
    >> Option.defaultValue false

let put word trie =
    let rec putChars chars currentTrie =
        match chars with
        | x :: xs ->
            match tryGetChild x currentTrie with
            | Some child -> addChild x (putChars xs child) currentTrie
            | None -> addChild x (createSubtree xs) currentTrie
        | [] -> Leaf

    putChars (word |> Seq.toList) trie

let words =
    let generateChildWords wordsChildFn =
        Map.toList
        >> List.collect (fun (key, child) ->
            let childWords =
                (wordsChildFn child)
                |> List.map (fun word -> key :: word)

            if isCompleted child then
                [ key ] :: childWords
            else
                childWords)

    let rec generateWords node =
        match node with
        | Root c -> generateChildWords generateWords c
        | Node (c, _) -> generateChildWords generateWords c
        | Leaf -> []

    generateWords
    >> List.map (fun wordList -> new string [| for c in wordList -> c |])
