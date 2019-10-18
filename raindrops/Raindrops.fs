module Raindrops

let convert (number: int): string = 
    let xs = [(3, "Pling"); (5, "Plang"); (7, "Plong")]
    let rainDrop acc (n, str) = acc + if (number % n = 0) then str else ""
    let str = xs |> List.fold rainDrop ""
    match str with
    | "" -> number.ToString()
    | _ -> str
