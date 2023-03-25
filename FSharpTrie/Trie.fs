module FSharpTrie.Trie

type T =
    | Root of Map<char, T>
    | Node of Map<char, T> * bool
    | Leaf

type private NodeOperation =
    | Keep of T
    | Remove

let private tryGetNode operation =
    match operation with
    | Keep node -> Some node
    | _ -> None

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

let private tryFindChild key = getChildren >> Map.tryFind key

let private findChild key = tryFindChild key >> Option.get

let rec private createSubtree chars =
    match chars with
    | x :: xs -> Node(Map[(x, createSubtree xs)], false)
    | [] -> Leaf

let private addChild key child trie =
    match trie with
    | Root children -> children |> Map.add key child |> Root
    | Node (children, completed) -> Node(children |> Map.add key child, completed)
    | Leaf -> Node(Map [ (key, child) ], true)

let private removeChild key trie =
    let shouldConvertToLeaf (Node (children, completed)) =
        completed
        && children |> Map.containsKey key
        && children |> Map.count = 1

    match trie with
    | Root children -> children |> Map.remove key |> Root
    | Node _ when shouldConvertToLeaf trie -> Leaf
    | Node (children, completed) -> Node(children |> Map.remove key, completed)
    | Leaf -> Leaf

let create () = Root Map.empty

let tryFindNode word =
    let rec tryFindNodeByChars chars =
        match chars with
        | x :: xs ->
            tryFindChild x
            >> Option.bind (tryFindNodeByChars xs)
        | [] -> Some

    tryFindNodeByChars (word |> Seq.toList)

let containsPrefix word = tryFindNode word >> Option.isSome

let contains word =
    tryFindNode word
    >> Option.map isCompleted
    >> Option.defaultValue false

let put: (string -> T -> T) =
    let rec putChars chars currentTrie =
        match chars with
        | x :: xs ->
            match tryFindChild x currentTrie with
            | Some child -> addChild x (putChars xs child) currentTrie
            | None -> addChild x (createSubtree xs) currentTrie
        | [] ->
            match currentTrie with
            | Node (children, _) -> Node(children, true)
            | _ -> Leaf

    Seq.toList >> putChars

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
        | Root children -> generateChildWords generateWords children
        | Node (children, _) -> generateChildWords generateWords children
        | Leaf -> []

    generateWords
    >> List.map (fun wordList -> new string [| for c in wordList -> c |])

let remove word trie =
    let rec removeChars word currentTrie =
        match word with
        | [] ->
            match currentTrie with
            | Root _ -> currentTrie |> Keep
            | Node (children, _) -> Node(children, false) |> Keep
            | Leaf -> Remove
        | childKey :: xs ->
            let nodeOperation =
                currentTrie
                |> findChild childKey
                |> removeChars xs

            let trie' =
                match nodeOperation with
                | Remove -> removeChild childKey currentTrie
                | Keep child -> addChild childKey child currentTrie

            match trie' with
            | Node (c, _) when c |> Map.isEmpty -> Remove
            | _ -> trie' |> Keep

    if contains word trie then
        removeChars (word |> Seq.toList) trie
        |> tryGetNode
    else
        None