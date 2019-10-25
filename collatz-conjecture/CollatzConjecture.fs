module CollatzConjecture

let steps (number: int): int option = 
    let rec stepsAcc acc = function
        | n when n < 1 -> None
        | 1 -> Some acc
        | n when n % 2 = 0 -> stepsAcc (acc + 1) (n/2)
        | n -> stepsAcc (acc + 1) (3*n + 1)    
    stepsAcc 0 number