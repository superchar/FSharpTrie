module Trie

type TrieNode =
    { KeyPart: char
      IsComplete: bool
      Children: Map<char, TrieNode> }

    static member createLeaf keyPart =
        { KeyPart = keyPart
          IsComplete = true
          Children = Map.empty }

    static member createNode keyPart childKey child =
        { KeyPart = keyPart
          IsComplete = false
          Children = Map [ (childKey, child) ] }

let Root =
    { KeyPart = '0'
      IsComplete = false
      Children = Map.empty }

let private updateChild keyPart node child =
    { node with Children = node.Children |> Map.add keyPart child }

let rec private createSubtree key =
    match key with
    | [ head ] -> TrieNode.createLeaf head
    | head :: tail -> TrieNode.createNode head tail.Head (createSubtree tail)
    | _ -> failwith "key cannot be empty"

let rec private addRec node key =
    match key with
    | head :: tail ->
        match node.Children |> Map.tryFind head with
        | Some child -> updateChild head node (addRec child tail)
        | None -> updateChild head node (createSubtree key)
    | [] -> node

let add (key: string) node = key |> Seq.toList |> addRec node

let rec private containsKeyRec node key =
    match key with
    | head :: tail ->
        match node.Children |> Map.tryFind head with
        | Some child ->
            if tail = [] then
                child.IsComplete
            else
                containsKeyRec child tail
        | None -> false
    | [] -> true

let containsKey (key: string) (node: TrieNode) : bool =
    key |> Seq.toList |> containsKeyRec node

let rec private getStringsRec (node: TrieNode) : char list list =
    if node.Children.IsEmpty then
        [ [] ]
    else
        let strings =
            (node.Children
             |> Seq.toList
             |> List.collect (fun kv ->
                 getStringsRec kv.Value
                 |> List.map (fun l -> kv.Key :: l)))

        if node.IsComplete then
            [ node.KeyPart ] :: strings
        else
            strings

let getStrings (node: TrieNode) : string list =
    getStringsRec node
    |> List.map (fun w -> w |> List.toArray |> System.String)

let rec private tryGetSubtreeRec (node: TrieNode) (key: char list) : TrieNode option =
    match key with
    | head :: tail ->
        if node.Children.ContainsKey head then
            tryGetSubtreeRec (node.Children[head]) tail
        else
            None
    | [] -> Some(node)

let tryGetSubtree (key: string) (node: TrieNode) : TrieNode option =
    key |> Seq.toList |> tryGetSubtreeRec node