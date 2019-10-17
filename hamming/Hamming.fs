module Hamming

let fCompare acc e1 e2 = 
    if e1 <> e2 then acc + 1 else acc

let distance (s1: string) (s2: string): int option = 
    if s1.Length <> s2.Length then None
    else
        Some (Seq.fold2 fCompare 0 s1 s2)