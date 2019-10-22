module TreeEval1
type Leaf =
    | TRUE
    | FALSE

type Op1 =
    | NOT
    
type Op2 =
    | AND
    | OR

type Tree =
    | Node of bool
    | Node1 of Op1 * Tree
    | Node2 of Tree * Op2 * Tree

let rec eval (t:Tree): bool = 
    match t with
    | Node x -> x
    | Node1 (NOT, t) -> not (eval t) 
    | Node2 (t1, AND, t2) -> (eval t1) && (eval t2)
    | Node2 (t1, OR, t2) -> (eval t1) || (eval t2)

let rec eval2 (t:Tree): bool = 
    match t with
    | Node (x) -> x
    | Node1 (NOT, t) -> not (eval2 t)
    | Node2 (Node false, AND, _) -> false
    | Node2 (_, AND, Node false) -> false
    | Node2 (t1, AND, t2) -> (eval2 t1) && (eval2 t2)
    | Node2 (Node true, OR, _) -> true
    | Node2 (_, OR, Node true) -> true
    | Node2 (t1, OR, t2) -> (eval2 t1) || (eval2 t2)

let t:Tree = Node2(Node true, AND, Node1(NOT,Node false))

let b = eval t

printfn "%A" b
