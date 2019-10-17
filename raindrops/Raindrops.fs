module Raindrops

let convert (number: int): string = 
    let rainDrop n str = if (number % n = 0) then str else ""
    let s3 = rainDrop 3 "Pling"
    let s5 = rainDrop 5 "Plang"
    let s7 = rainDrop 7 "Plong"
    let str = sprintf "%s%s%s" s3 s5 s7
    if str = "" then number.ToString() else str