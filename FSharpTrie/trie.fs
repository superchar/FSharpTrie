module Trie

type TrieNode = {
    KeyPart: char; IsComplete: bool; Children: Map<char, TrieNode>
}

let Root = { KeyPart = '0'; IsComplete = false; Children = Map.empty }

let private updateChild (keyPart: char) (node: TrieNode) (child: TrieNode) =
    { node with Children = node.Children |> Map.add keyPart child }
    
let rec private addRec (node: TrieNode) (key: list<char>): TrieNode =
    match key with
    | head :: tail ->
        match node.Children |>
                        Map.tryFind head with
                        | Some(child) -> updateChild head node (addRec child tail)
                        | None ->  updateChild head node
                                       (addRec { KeyPart = head; IsComplete = tail = []; Children = Map.empty } tail)
    | [] -> node

let add (key: string) (node: TrieNode) = key
                                         |> Seq.toList
                                         |> addRec node

let rec private containsKeyRec(node: TrieNode) (key: list<char>) =
    match key with
    | [ head ] -> match node.Children |>
                        Map.tryFind head with
                        | Some(child) -> child.IsComplete
                        | None -> false 
    | head :: tail -> match node.Children |>
                        Map.tryFind head with
                        | Some(child) -> containsKeyRec child tail
                        | None -> false 
    | [] -> true

let containsKey(key: string) (node: TrieNode) : bool = key
                                                       |> Seq.toList
                                                       |> containsKeyRec node

let rec private getStringsRec(node: TrieNode): char list list =
    if(node.Children.IsEmpty) then [[]]
    else
    let strings = (node.Children
    |> Seq.toList
    |> List.collect (fun kv -> getStringsRec(kv.Value) |> List.map(fun l -> kv.Key :: l)))
    if node.IsComplete then [node.KeyPart] :: strings
    else strings

let getStrings(node: TrieNode): string list =
    getStringsRec node |> List.map(fun w -> w |> List.toArray |> System.String)