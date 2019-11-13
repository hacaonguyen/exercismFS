
let cont x xs = x::xs

let f = cont 1 << cont 2 << cont 3

let l = f []