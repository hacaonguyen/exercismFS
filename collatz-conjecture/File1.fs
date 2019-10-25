module File1
let stepsNonRec (number: int): int option = 
    if 
        number <= 0 then None
    else
        let mutable loop = 0
        let mutable n = number
        while n <> 1 do
            loop <- loop + 1
            if (n % 2 = 0) then n <- n/2 else n <- 3*n + 1
        Some loop

let rec stepsAcc' (acc:int) (n: int): int option = 
    match n with 
    | n when n < 1 -> None
    | 1 -> Some acc
    | n when n % 2 = 0 -> stepsAcc' (acc + 1) (n/2)
    | _ -> stepsAcc' (acc + 1) (3*n + 1) 

