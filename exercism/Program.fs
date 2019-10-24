// Learn more about F# at http://fsharp.org
open Util

let countValues list value =
    let rec checkList list acc =
       match list with
       | head :: tail when head = value -> checkList tail (acc + 1)
       | _ :: tail -> checkList tail acc
       | [] -> acc
    checkList list 0

//let result = countValues [ for x in -10..10 -> x*x - 4 ] 0
let result = countValues [1; 2; 3; 4; 3; 5] 0

printfn "%d" result
