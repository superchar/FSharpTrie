open FSharpTrie

let putMultiple = Trie2.put "hello" >> Trie2.put "world" >> Trie2.put "hellowrld!"
let trie = putMultiple (Trie2.createRoot ())
let test = "test"
let node = Option.get (Trie2.tryFindNode "he" trie)
let result = Trie2.words node
let test2 = "test"