module Sieve

let primes limit = 
    
    //remove mutiples of n from xs
    let rec sieve n acc xs = ////int -> int -> int list -> int list
        if acc > limit then xs else sieve n (acc + n) (List.filter (fun x -> x <> acc) xs)
    
    let rec primesAcc acc = function
        | [] -> acc
        | h :: t -> primesAcc (h :: acc) (sieve h (2 * h) t) //sieve h (2 * h) t = (List.filter (fun x -> x % h <> 0)  t)
    
    if limit = 1 then [] else [2..limit] |> primesAcc [] |> List.rev
