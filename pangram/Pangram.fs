module Pangram


//check if we can find a character in a string 
let canFind c s = 
    String.exists (fun i -> System.Char.ToLower(i) = c) s

let a2z = ['a'..'z']

let isPangram (input: string): bool = 
    List.fold (fun acc elem -> acc && (canFind elem input)) true a2z
  