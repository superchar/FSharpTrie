module Tests

open FSharpTrie
open FsUnit
open FsUnit.CustomMatchers
open Xunit

[<Fact>]
let ``Test put with multiple strings, including prefixes`` () =
    let putFun = Trie.put "hello" >> Trie.put "world" >> Trie.put "hell" >> Trie.put "hello world!"
    let trie = Trie.create () |> putFun
    trie |> Trie.contains "hello" |> should be True
    trie |> Trie.contains "world" |> should be True
    trie |> Trie.contains "hell" |> should be True
    trie |> Trie.contains "hello world!" |> should be True

[<Fact>]   
let ``Test put with duplicate strings`` () =
    let putFun = Trie.put "hello" >> Trie.put "hello"
    let trie = Trie.create () |> putFun
    trie |> Trie.contains "hello" |> should be True
    
[<Fact>]   
let ``Test put with empty string`` () =
    let trie = Trie.create () |> Trie.put ""
    trie |> Trie.contains "" |> should be True
    
[<Fact>]   
let ``Test create`` () =
    Trie.create () |> should be (ofCase<@ Trie.T.Root @>)
    
[<Fact>]   
let ``Test remove`` () =
    let putFun = Trie.put "hello" >> Trie.put "world" >> Trie.put "hell" >> Trie.put "hello world!"
    let trie = Trie.create () |> putFun
    let trie' = trie |> Trie.remove "world" |> Option.get
    trie' |> Trie.contains "world" |> should be False
    
[<Fact>]   
let ``Test remove prefix of existing key`` () =
    let putFun = Trie.put "hello" >> Trie.put "world" >> Trie.put "hell" >> Trie.put "hello world!"
    let trie = Trie.create () |> putFun
    let trie' = trie |> Trie.remove "hell" |> Option.get
    trie' |> Trie.contains "hell" |> should be False
    trie' |> Trie.contains "hello" |> should be True
    trie' |> Trie.contains "hello world!" |> should be True
    
[<Fact>]   
let ``Test remove suffix of existing key`` () =
    let putFun = Trie.put "hello" >> Trie.put "world" >> Trie.put "hell" >> Trie.put "hello world!"
    let trie = Trie.create () |> putFun
    let trie' = trie |> Trie.remove "hello world!" |> Option.get
    trie' |> Trie.contains "hello world!" |> should be False
    trie' |> Trie.contains "hello" |> should be True
    trie' |> Trie.contains "hell" |> should be True
    
[<Fact>]   
let ``Test remove non existing key`` () =
    let putFun = Trie.put "hello" >> Trie.put "world" >> Trie.put "hell" >> Trie.put "hello world!"
    let trie = Trie.create () |> putFun
    let result = trie |> Trie.remove "hello world!!!!"
    result |> Option.isSome |> should be False
