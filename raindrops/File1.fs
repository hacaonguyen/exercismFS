module File1

open System

let convert1 (number: int): string = 
    let rainDrop n str = if (number % n = 0) then str else ""
    let s3 = rainDrop 3 "Pling"
    let s5 = rainDrop 5 "Plang"
    let s7 = rainDrop 7 "Plong"
    let str = sprintf "%s%s%s" s3 s5 s7
    if str = "" then number.ToString() else str

let convert2 (number: int): string = 
    let xs = [(3, "Pling"); (5, "Plang"); (7, "Plong")]
    let rainDrop acc (n, str) = acc + if (number % n = 0) then str else ""
    let str = xs |> List.fold rainDrop ""
    match str with
    | "" -> number.ToString()
    | _ -> str

let convert3 (number: int): string = 
    [(3, "Pling"); (5, "Plang"); (7, "Plong")] 
    |> List.map (fun(n, s) -> if number % n = 0 then s else "") 
    |> String.Concat //or String.concat ""
    |> function
        | "" -> number.ToString()
        | x -> x        
