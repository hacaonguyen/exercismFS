module RationalNumbers

//greatest common divisor
let rec gcd a b = 
    match a, b with
    | 0, _ -> b
    | _, 0 -> a
    | _ when a > b -> gcd b (a % b)
    | _ -> gcd a (b % a)

let reduce (a, b) = 
    let x = gcd (abs a) (abs b)
    if b < 0 then (-a / x, -b / x) else (a / x, b / x)

let create a b = (a, b)

let add (a1, b1) (a2, b2) = (a1*b2 + a2*b1, b1*b2) |> reduce

let sub (a1, b1) (a2, b2) = (a1*b2 - a2*b1, b1*b2) |> reduce 

let mul (a1, b1) (a2, b2) = (a1*a2, b1*b2) |> reduce

let div (a1, b1) (a2, b2) = (a1*b2, b1*a2) |> reduce

let abs (a, b) = (abs a, abs b) |> reduce

let exprational n (a, b) = (pown a n, pown b n) |> reduce

let expreal (a, b) n = (float n) ** ((float a) / (float b))

(*
//another way to implement gcd function
let rec gcd2 (a: int) (b: int): int =
    match a, b with
    | 0, _ -> b
    | _, 0 -> a 
    | _ when a = b -> a
    | _ when a > b -> gcd2 b (a - b)
    | _ -> gcd2 a (b - a)
*)