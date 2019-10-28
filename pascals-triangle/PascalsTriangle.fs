module PascalsTriangle

//generate row(n + 1) from row(n)
//[1; 2; 1] -> [1; 3; 3; 1]
let nextRow (xs: int list): int list =
    let xs' = 0 :: xs @ [0]
    [ for i = 1 to xs'.Length - 1 do yield (xs'.[i-1] + xs'.[i]) ]  

let rec rows numberOfRows : int list list = 
    match numberOfRows with
    | 0 -> []
    | 1 -> [[1]]
    | n -> 
        let preRows = rows (n - 1)        
        let preRow = preRows.[n - 2] //get the last row
        preRows @ [nextRow preRow]