module Raindrops
//open System

let convert (number: int): string = 
    [(3, "Pling"); (5, "Plang"); (7, "Plong")] 
    |> List.choose (fun(n, s) -> if number % n = 0 then Some s else None) 
    |> function
        | [] -> number.ToString()
        | x -> x |> String.concat ""

//can also use String.concat "" xs 
//with xs is a string list

