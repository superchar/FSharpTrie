module FSharpTrie.Main
open FSharpTrie

let putMultiple = Trie2.put "hello" >> Trie2.put "world" >> Trie2.put "hellowrld!"
let trie = putMultiple (Trie2.createRoot ())
let test = "test"