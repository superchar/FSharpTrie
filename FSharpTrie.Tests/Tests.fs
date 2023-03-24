module Tests

open FSharpTrie
open FsUnit
open FsUnit.CustomMatchers
open Xunit


[<Fact>]
let ``Test put`` () =
    let trie =
        Trie.create () |> Trie.put "world"

    trie |> Trie.contains "world" |> should be True

[<Fact>]
let ``Test put with prefix of existing key`` () =
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
let ``Test put with duplicate strings`` () =
    let putFun =
        Trie.put "hello" >> Trie.put "hello"

    let trie = Trie.create () |> putFun
    trie |> Trie.contains "hello" |> should be True

[<Fact>]
let ``Test put with empty string`` () =
    let trie = Trie.create () |> Trie.put ""
    trie |> Trie.contains "" |> should be True

[<Fact>]
let ``Test create`` () =
    Trie.create ()
    |> should be (ofCase <@ Trie.T.Root @>)

[<Fact>]
let ``Test remove`` () =
    let trie =
        Trie.create () |> Trie.put "world"

    let trie' =
        trie |> Trie.remove "world" |> Option.get

    trie' |> Trie.contains "world" |> should be False

[<Fact>]
let ``Test remove prefix of existing key`` () =
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
let ``Test remove suffix of existing key`` () =
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
let ``Test remove non existing key`` () =
    let trie =
        Trie.create () |> Trie.put "hello world!"

    let result =
        trie |> Trie.remove "hello world!!!!"

    result |> Option.isSome |> should be False

[<Fact>]
let ``Test remove with empty string`` () =
    let trie =
        Trie.create () |> Trie.put "hello world!"

    let result = trie |> Trie.remove ""
    result |> Option.isSome |> should be False

[<Fact>]
let ``Test tryFindNode with full word`` () =
    let trie =
        Trie.create () |> Trie.put "hello"

    let result =
        trie |> Trie.tryFindNode "hello"

    result |> Option.isSome |> should be True

[<Fact>]
let ``Test tryFindNode with word prefix`` () =
    let trie =
        Trie.create () |> Trie.put "hello"

    let result = trie |> Trie.tryFindNode "he"
    result |> Option.isSome |> should be True

[<Fact>]
let ``Test tryFindNode with redundant characters`` () =
    let trie =
        Trie.create () |> Trie.put "hello"

    let result =
        trie |> Trie.tryFindNode "helooo"

    result |> Option.isSome |> should be False

[<Fact>]
let ``Test tryFindNode with non existing word`` () =
    let trie =
        Trie.create () |> Trie.put "hello"

    let result = trie |> Trie.tryFindNode "test"
    result |> Option.isSome |> should be False

[<Fact>]
let ``Test tryFindNode with empty string`` () =
    let trie =
        Trie.create () |> Trie.put "hello"

    let result = trie |> Trie.tryFindNode ""
    result |> Option.isSome |> should be True

[<Fact>]
let ``Test containsPrefix with existing prefix`` () =
    let trie =
        Trie.create () |> Trie.put "hello"

    let result =
        trie |> Trie.containsPrefix "hell"

    result |> should be True

[<Fact>]
let ``Test containsPrefix with existing word`` () =
    let trie =
        Trie.create () |> Trie.put "hello"

    let result =
        trie |> Trie.containsPrefix "hello"

    result |> should be True

[<Fact>]
let ``Test containsPrefix with non existing prefix`` () =
    let trie =
        Trie.create () |> Trie.put "hello"

    let result =
        trie |> Trie.containsPrefix "te"

    result |> should be False

[<Fact>]
let ``Test containsPrefix with empty string`` () =
    let trie =
        Trie.create () |> Trie.put "hello"

    let result = trie |> Trie.containsPrefix ""
    result |> should be True

[<Fact>]
let ``Test contains with existing word`` () =
    let trie =
        Trie.create () |> Trie.put "hello"

    let result = trie |> Trie.contains "hello"
    result |> should be True

[<Fact>]
let ``Test contains with existing prefix`` () =
    let trie =
        Trie.create () |> Trie.put "hello"

    let result = trie |> Trie.contains "hell"
    result |> should be False

[<Fact>]
let ``Test contains with non existing word`` () =
    let trie =
        Trie.create () |> Trie.put "hello"

    let result = trie |> Trie.contains "test"
    result |> should be False

[<Fact>]
let ``Test contains with empty string`` () =
    let trie =
        Trie.create () |> Trie.put "hello"

    let result = trie |> Trie.contains ""
    result |> should be False

[<Fact>]
let ``Test word with root`` () =
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
let ``Test word with node`` () =
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
let ``Test word with leaf`` () =
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