module ListOps

let rec foldl func acc list = 
    match list with
    | [] -> acc
    | head :: tail -> foldl func (func acc head) tail 

let rec foldr func acc list = 
    match list with
    | [] -> acc
    | head :: tail -> func head (foldr func acc tail) //should improve to tail-rec

let concat xs = [ for i in xs do yield! i ]

let append xs ys =
    let fCont x y = x :: y
    let rec appendAcc fAcc = function
        | [] -> fAcc
        | head :: tail -> appendAcc (fAcc << fCont head) tail
    appendAcc id xs ys        

let reverse list = 
    let rec reverseAcc acc = function
    | [] -> acc
    | h :: t -> reverseAcc (h :: acc) t 
    reverseAcc [] list

let map f list = [ for i in list do yield (f i) ]

let filter f list = [ for i in list do if (f i) then yield i ]

let length list = 
    let rec lengthAcc acc = function
    | [] -> acc
    | h :: t -> lengthAcc (acc + 1) t
    lengthAcc 0 list