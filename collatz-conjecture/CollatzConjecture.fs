module CollatzConjecture

let steps (number: int): int option = 
    if 
        number <= 0 then None
    else
        let mutable loop = 0
        let mutable n = number
        while n <> 1 do
            loop <- loop + 1
            if (n % 2 = 0) then n <- n/2 else n <- 3*n + 1
        Some loop
 