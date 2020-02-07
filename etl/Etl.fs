module Etl

let transformItem (number, charList) =  
    charList |> List.fold (fun acc char -> (char, number)::acc) []

let transform (scoresWithLetters: Map<int, char list>): Map<char, int> = 
    scoresWithLetters
    |> Map.toList
    |> List.fold (fun acc item -> acc @ (transformItem item)) []
    |> List.map (fun (c, n) -> (System.Char.ToLower c ,n))
    |> Map.ofList
