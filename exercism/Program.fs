// Learn more about F# at http://fsharp.org
open Util

let nextList (xs: int list): int list =
    let ys = 0 :: xs @ [0]
    [ for i = 1 to ys.Length - 1 do yield (ys.[i-1] + ys.[i]) ]  

let rows numberOfRows : int list list = 
    match numberOfRows with
    | 1 -> [[1]]
    | n -> [[1]; [1;1]] 

