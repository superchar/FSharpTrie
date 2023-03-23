module FSharpTrie.Trie2

type T =
    | Root of Map<char, T>
    | Node of Map<char, T> * bool

let private Leaf = Node(Map.empty, true)

let private getChildren trie =
    match trie with
    | Root c -> c
    | Node (c, _) -> c

let private isCompleted trie =
    match trie with
    | Root _ -> false
    | Node (_, completed) -> completed

let private isCompletedOption trie = Some(isCompleted trie)

let private tryGetChild key = getChildren >> Map.tryFind key

let rec private createSubtree chars =
    match chars with
    | x :: xs -> Node(Map[(x, createSubtree xs)], false)
    | [] -> Leaf

let private addChild key child trie =
    match trie with
    | Root c -> c |> Map.add key child |> Root
    | Node (c, completed) -> Node(c |> Map.add key child, completed)

let private removeChild key trie =
    match trie with
    | Root c -> c |> Map.remove key |> Root
    | Node (c, completed) -> Node(c |> Map.remove key, completed)

let createRoot () = Root Map.empty

let tryFindNode word trie =
    let rec tryFindNodeByChars chars currentTrie =
        match chars with
        | x :: xs ->
            tryGetChild x currentTrie
            |> Option.bind (tryFindNodeByChars xs)
        | [] -> Some currentTrie

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
        | Node (c, _) when c |> Map.isEmpty -> []
        | Node (c, _) -> generateChildWords generateWords c

    generateWords
    >> List.map (fun wordList -> new string [| for c in wordList -> c |])
