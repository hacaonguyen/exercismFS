module File1

let create (position: int * int) = 
    let invalid x = (x < 0) || (x > 7)
    match position with
    | (x, _) when (invalid x) -> false
    | (_, y) when (invalid y) -> false
    | _ -> true