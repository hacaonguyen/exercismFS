module BinarySearchTree

type BSTree =
    | Empty
    | Node of BSTree * int * BSTree 

let rec insert item bst =
    match bst with
    | Empty -> Node (Empty, item, Empty)
    | Node (left, x, right) -> 
        if item <= x then Node (insert item left, x, right)
        else Node (left, x, insert item right)

let create items = 
    List.fold (fun acc e -> insert e acc) Empty items 

let emptyTree = 0
 
let data node = 
    match node with
    | Empty -> emptyTree 
    | Node (left, x, right) -> x

let left node  = 
    match node with
    | Empty -> None
    | Node (Empty, _, _) -> None
    | Node (left, _, _) -> Some left 

let right node = 
    match node with
    | Empty -> None
    | Node (_, _, Empty) -> None
    | Node (_, _, right) -> Some right 

let rec sortedData node =
    match node with
    | Empty -> []
    | Node (left, x, right) -> (sortedData left) @ [x] @ (sortedData right) 