// Learn more about F# at http://fsharp.org
open Util

let append xs ys =
    let f x y = x :: y
    let rec appendAcc acc = function
        | [] -> acc
        | head :: tail -> appendAcc (acc << f head) tail
    appendAcc id xs ys        

let x: int list = append [] []

Out1 "" x

Out1 "" (append [] [])


