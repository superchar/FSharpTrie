module FSharpTrie.Trie

type T =
    | Root of Map<char, T>
    | Node of Map<char, T> * bool
    | Leaf

type private NodeOperation =
    | Keep of T
    | Remove

let private tryGetNode (operation: NodeOperation) : T option =
    match operation with
    | Keep node -> Some node
    | _ -> None

let private getChildren (trie: T) : Map<char, T> =
    match trie with
    | Root children -> children
    | Node (c, _) -> c
    | Leaf -> Map.empty

let private isCompleted (trie: T) : bool =
    match trie with
    | Root _ -> false
    | Node (_, completed) -> completed
    | Leaf -> true

let private tryFindChild (key: char) : T -> T option = getChildren >> Map.tryFind key

let private findChild (key: char) : T -> T = tryFindChild key >> Option.get

let rec private createSubtree (chars: char list) : T =
    match chars with
    | x :: xs -> Node(Map[(x, createSubtree xs)], false)
    | [] -> Leaf

let private addChild (key: char) (child: T) (trie: T) : T =
    match trie with
    | Root children -> children |> Map.add key child |> Root
    | Node (children, completed) -> Node(children |> Map.add key child, completed)
    | Leaf -> Node(Map [ (key, child) ], true)

let private removeChild (key: char) (trie: T) : T =
    let shouldConvertToLeaf (Node (children, completed)) : bool =
        completed
        && children |> Map.containsKey key
        && children |> Map.count = 1

    match trie with
    | Root children -> children |> Map.remove key |> Root
    | Node _ when shouldConvertToLeaf trie -> Leaf
    | Node (children, completed) -> Node(children |> Map.remove key, completed)
    | Leaf -> Leaf

let create () : T = Root Map.empty

let tryFindNode (word: seq<char>) : T -> T option =
    let rec tryFindNodeByChars (chars: char list) : T -> T option =
        match chars with
        | x :: xs ->
            tryFindChild x
            >> Option.bind (tryFindNodeByChars xs)
        | [] -> Some

    tryFindNodeByChars (word |> Seq.toList)

let containsPrefix (word: seq<char>) : T -> bool = tryFindNode word >> Option.isSome

let contains (word: seq<char>) : T -> bool =
    tryFindNode word
    >> Option.map isCompleted
    >> Option.defaultValue false

let put: (seq<char> -> T -> T) =
    let rec putChars (chars: char list) (currentTrie: T) : T =
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

let words: T -> string list =
    let generateChildWords (wordsChildFn: T -> char list list) : Map<char, T> -> char list list =
        Map.toList
        >> List.collect (fun (key, child) ->
            let childWords =
                (wordsChildFn child)
                |> List.map (fun word -> key :: word)

            if isCompleted child then
                [ key ] :: childWords
            else
                childWords)

    let rec generateWords (node: T) : char list list =
        match node with
        | Root children -> generateChildWords generateWords children
        | Node (children, _) -> generateChildWords generateWords children
        | Leaf -> []

    generateWords
    >> List.map (fun wordList -> new string [| for c in wordList -> c |])

let remove (word: seq<char>) (trie: T) : T option =
    let rec removeChars (chars: char list) (currentTrie: T) : NodeOperation =
        match chars with
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
            | Node (children, _) when children |> Map.isEmpty -> Remove
            | _ -> trie' |> Keep

    if contains word trie then
        removeChars (word |> Seq.toList) trie
        |> tryGetNode
    else
        None