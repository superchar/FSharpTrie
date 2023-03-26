module Tests

open FSharpTrie
open FsUnit
open FsUnit.CustomMatchers
open Xunit

[<Fact>]
let ``Calling put on word should add it to trie`` () =
    let trie =
        Trie.create () |> Trie.put "world"

    trie |> Trie.contains "world" |> should be True

[<Fact>]
let ``Calling put on prefix of existing key should keep the suffixes`` () =
    let trie =
        Trie.create ()
        |> Trie.put "hello"
        |> Trie.put "hell"
        |> Trie.put "hello world!"

    trie |> Trie.contains "hell" |> should be True
    trie |> Trie.contains "hello" |> should be True

    trie
    |> Trie.contains "hello world!"
    |> should be True

[<Fact>]
let ``Calling put on duplicated words should add it to trie once`` () =
    let putFun =
        Trie.put "hello" >> Trie.put "hello"

    let trie = Trie.create () |> putFun
    trie |> Trie.contains "hello" |> should be True

[<Fact>]
let ``Calling put on empty string should return root`` () =
    let trie = Trie.create () |> Trie.put ""
    trie |> Trie.contains "" |> should be True
    trie |> should be (ofCase <@ Trie.T.Root @>)

[<Fact>]
let ``Calling create should return root`` () =
    Trie.create ()
    |> should be (ofCase <@ Trie.T.Root @>)

[<Fact>]
let ``Calling remove on existing word should remove it from the trie`` () =
    let trie =
        Trie.create () |> Trie.put "world"

    let trie' =
        trie |> Trie.remove "world" |> Option.get

    trie' |> Trie.contains "world" |> should be False

[<Fact>]
let ``Calling remove with prefix of existing key keeps the suffixes`` () =
    let trie =
        Trie.create ()
        |> Trie.put "hello"
        |> Trie.put "hell"
        |> Trie.put "hello world!"

    let trie' =
        trie |> Trie.remove "hell" |> Option.get

    trie' |> Trie.contains "hell" |> should be False
    trie' |> Trie.contains "hello" |> should be True

    trie'
    |> Trie.contains "hello world!"
    |> should be True

[<Fact>]
let ``Calling remove with suffix of existing key keeps the prefixes`` () =
    let trie =
        Trie.create ()
        |> Trie.put "hello"
        |> Trie.put "hell"
        |> Trie.put "hello world!"

    let trie' =
        trie |> Trie.remove "hello world!" |> Option.get

    trie'
    |> Trie.contains "hello world!"
    |> should be False

    trie' |> Trie.contains "hello" |> should be True
    trie' |> Trie.contains "hell" |> should be True

[<Fact>]
let ``Calling remove with non existing word returns none`` () =
    let trie =
        Trie.create () |> Trie.put "hello world!"

    let result =
        trie |> Trie.remove "hello world!!!!"

    result |> Option.isSome |> should be False

[<Fact>]
let ``Calling remove with empty string returns root`` () =
    let trie =
        Trie.create () |> Trie.put "hello world!"

    let result = trie |> Trie.remove ""
    result |> Option.isSome |> should be True

    result
    |> Option.get
    |> should be (ofCase <@ Trie.T.Root @>)


[<Fact>]
let ``Calling tryFindNode on existing word returns node`` () =
    let trie =
        Trie.create () |> Trie.put "hello"

    let result =
        trie |> Trie.tryFindNode "hello"

    result |> Option.isSome |> should be True

[<Fact>]
let ``Calling tryFindNode on prefix returns node`` () =
    let trie =
        Trie.create () |> Trie.put "hello"

    let result = trie |> Trie.tryFindNode "he"
    result |> Option.isSome |> should be True

[<Fact>]
let ``Calling tryFindNode on word with redundant characters returns none`` () =
    let trie =
        Trie.create () |> Trie.put "hello"

    let result =
        trie |> Trie.tryFindNode "helooo"

    result |> Option.isSome |> should be False

[<Fact>]
let ``Calling tryFindNode on non existing word returns none`` () =
    let trie =
        Trie.create () |> Trie.put "hello"

    let result = trie |> Trie.tryFindNode "test"
    result |> Option.isSome |> should be False

[<Fact>]
let ``Calling tryFindNode on empty string returns root`` () =
    let trie =
        Trie.create () |> Trie.put "hello"

    let result = trie |> Trie.tryFindNode ""
    result |> Option.isSome |> should be True

    result
    |> Option.get
    |> should be (ofCase <@ Trie.T.Root @>)

[<Fact>]
let ``Calling containsPrefix on existing prefix returns true`` () =
    let trie =
        Trie.create () |> Trie.put "hello"

    let result =
        trie |> Trie.containsPrefix "hell"

    result |> should be True

[<Fact>]
let ``Calling containsPrefix on existing word returns true`` () =
    let trie =
        Trie.create () |> Trie.put "hello"

    let result =
        trie |> Trie.containsPrefix "hello"

    result |> should be True

[<Fact>]
let ``Calling containsPrefix on non existing returns false`` () =
    let trie =
        Trie.create () |> Trie.put "hello"

    let result =
        trie |> Trie.containsPrefix "te"

    result |> should be False

[<Fact>]
let ``Calling containsPrefix on empty string returns true`` () =
    let trie =
        Trie.create () |> Trie.put "hello"

    let result = trie |> Trie.containsPrefix ""
    result |> should be True

[<Fact>]
let ``Calling contains on existing word returns true`` () =
    let trie =
        Trie.create () |> Trie.put "hello"

    let result = trie |> Trie.contains "hello"
    result |> should be True

[<Fact>]
let ``Calling contains on existing prefix returns false`` () =
    let trie =
        Trie.create () |> Trie.put "hello"

    let result = trie |> Trie.contains "hell"
    result |> should be False

[<Fact>]
let ``Calling contains on non existing word returns false`` () =
    let trie =
        Trie.create () |> Trie.put "hello"

    let result = trie |> Trie.contains "test"
    result |> should be False

[<Fact>]
let ``Calling contains on empty string returns true`` () =
    let trie =
        Trie.create () |> Trie.put "hello"

    let result = trie |> Trie.contains ""
    result |> should be True

[<Fact>]
let ``Calling words on root returns all words in trie`` () =
    let trie =
        Trie.create ()
        |> Trie.put "hello"
        |> Trie.put "hell"
        |> Trie.put "hello world!"
        |> Trie.put "world"

    let result = trie |> Trie.words

    result
    |> should
        be
        (subsetOf [ "hello"
                    "hell"
                    "hello world!"
                    "world" ])

[<Fact>]
let ``Calling words on node returns suffixes from all the children`` () =
    let trie =
        Trie.create ()
        |> Trie.put "hello"
        |> Trie.put "hell"
        |> Trie.put "hello world!"
        |> Trie.put "world"
        |> Trie.tryFindNode "he"
        |> Option.get

    let result = trie |> Trie.words

    result
    |> should be (subsetOf [ "llo"; "ll"; "llo world!" ])

[<Fact>]
let ``Calling words on leaf returns empty string`` () =
    let trie =
        Trie.create ()
        |> Trie.put "hello"
        |> Trie.put "hell"
        |> Trie.put "hello world!"
        |> Trie.put "world"
        |> Trie.tryFindNode "hello world!"
        |> Option.get

    let result = trie |> Trie.words
    result |> should be Empty                                                                                                                                                                