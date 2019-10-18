module Grains

open System

let grain = seq<uint64> {for i = 0 to 63 do yield uint64(2.0**float(i))}

let square (n: int): Result<uint64,string> = 
    if n < 1 || n > 64 then Error "square must be between 1 and 64" 
    else grain |> (Seq.item (n-1)) |> Ok 

let total: Result<uint64,string> =
    grain |> Seq.sum |> Ok