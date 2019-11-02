module File1

let rec gcd (a: int) (b: int): int =
    match a, b with
    | _ when a = b -> a
    | _ when a > b -> gcd b (a - b)
    | _ -> gcd a (b - a)

let reduce (a: int) (b: int) =
    let x = gcd a b
    (a / x, b / x)

