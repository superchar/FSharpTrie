module Trie

type TrieNode = {
    IsComplete: bool; Children: Map<char, TrieNode>
}

let Root = { IsComplete = false; Children = Map.empty }

let private updateChild (keyPart: char) (node: TrieNode) (child: TrieNode) =
    { node with Children = node.Children |> Map.add keyPart child }
    

let rec private addRec (key: list<char>) (node: TrieNode): TrieNode =
    match key with
    | head :: tail ->
        match node.Children |>
                        Map.tryFind head with
                        | Some(child) -> updateChild head node (addRec tail child)
                        | None ->  updateChild head node
                                       (addRec tail { IsComplete = tail = []; Children = Map.empty })
    | [] -> node

let add (key: string) (node: TrieNode) = key
                                         |> Seq.toList
                                         |> fun key -> addRec key node

let rec private containsKeyRec(key: list<char>) (node: TrieNode) =
    match key with
    | [ head ] -> match node.Children |>
                        Map.tryFind head with
                        | Some(child) -> child.IsComplete
                        | None -> false 
    | head :: tail ->
        match node.Children |>
                        Map.tryFind head with
                        | Some(child) -> containsKeyRec tail child
                        | None -> false 
    | [] -> true

let containsKey(key: string) (node: TrieNode) : bool = key
                                                       |> Seq.toList
                                                       |> fun key -> containsKeyRec key node