module LargestSeriesProduct

open System

// check if string contain non-numeric characters
// "123a5" -> None
let checkNumber (str, n) =
    let len = str |> String.filter Char.IsNumber |> String.length
    if len = (String.length str) then Some (str, n) else None

let checkNegative (str, n) = if n < 0 then None else Some (str, n)

// check if Span > length of input string
let checkSpan (str, n) = if n > (String.length str) then None else Some (str, n)

let products (str, n): int option = 
    if n = 0 then Some 1 else
        // int list -> int
        // [1; 2; 3] -> 1 * 2 * 3
        let multiply xs = xs |> List.fold (fun acc i -> acc * i) 1  
        
        // convert string to list
        // "123" -> [1; 2; 3]
        let xs = str |> Seq.toList |> List.map (fun c -> (int (string c)))
        
        //caculate max product
        [ for i in 0..(xs.Length - n) do yield (multiply xs.[i..n+i-1]) ] |> List.max |> Some

let largestProduct str n : int option = 
    (str, n) 
    |> checkNumber
    |> Option.bind checkNegative
    |> Option.bind checkSpan
    |> Option.bind products
    
