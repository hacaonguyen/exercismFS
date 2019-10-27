// Learn more about F# at http://fsharp.org
open Util

let steps (number: int): int = 
    if 
        number <= 0 then 0
    else
        let mutable loop = 0
        let mutable n = number
        while n <> 1 do
            loop <- loop + 1
            if (n % 2 = 0) then n <- n/2 else n <- 3*n + 1
        loop

for i in [1..1000] do printf ("%d, ") (steps i) 