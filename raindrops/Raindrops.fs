module Raindrops
open System

let convert (number: int): string = 
    [(3, "Pling"); (5, "Plang"); (7, "Plong")] 
    |> List.map (fun(n, s) -> if number % n = 0 then s else "") 
    |> String.Concat
    |> function
        | "" -> number.ToString()
        | x -> x        


