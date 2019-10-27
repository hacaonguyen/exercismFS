module Darts

let score (x: double) (y: double): int = 
    let r = sqrt (x*x + y*y)
    match r with
    | r when r <= 1. -> 10
    | r when r <= 5. -> 5
    | r when r <= 10. -> 1
    | _ -> 0