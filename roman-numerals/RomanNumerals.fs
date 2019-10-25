module RomanNumerals
//let r1, r5, r10, r50, r100, r500, r1000 = "I", "V", "X", "L", "C", "D", "M"
//Tool to convert: https://tienichnho.com/chuyen-doi-so-la-ma

//repeat a string n times
//3 "I" -> "III"
let nStr n str = [ for i in 1..n do yield str ] |> String.concat ""

//Roman number of m*n with m = 1, 10, 100, 1000; n = 0..9
let subRoman (m:int) (n: int): string =
    let r1, r5, r10 = match m with
        | 1 -> "I", "V", "X"
        | 10 -> "X", "L", "C"
        | 100 -> "C", "D", "M"
        | 1000 -> "M", "", ""
        | _ -> "", "", ""
    match n with
    | 1 | 2 | 3 -> nStr n r1
    | 4 -> r1 + r5
    | 5 -> r5
    | 6 | 7 | 8 -> r5 + (nStr (n-5) r1)
    | 9 -> r1 + r10
    | _ -> ""

let roman n = 
    if n < 1 || n > 3999 then ""
    else
        let x1000 = n / 1000
        let x100 = (n % 1000) / 100
        let x10 = (n % 100) / 10
        let x1 = n % 10
        (subRoman 1000 x1000) + (subRoman 100 x100) + (subRoman 10 x10) + (subRoman 1 x1)  