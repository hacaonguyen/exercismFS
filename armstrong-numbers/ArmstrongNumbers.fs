module ArmstrongNumbers

let isArmstrongNumber (number: int): bool = 
    let str = number |> string
    let len = str |> String.length
    let number' = 
        str
        |> Seq.map (fun c -> int c - int '0')
        |> Seq.sumBy (fun i -> (pown i len))
    number = number'

(*
other ways to convert char -> int
c |> string |> int
c |> (string >> int)

we can use: Seq.map (string >> int) 
*)