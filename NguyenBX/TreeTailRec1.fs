module TreeTailRec1

type Tree =
    | Leaf of bool
    | NodeAnd of Tree * Tree
    | NodeOr of Tree * Tree

let evalTree1 t =
    let rec Loop t cont =
        match t with
        | Leaf x -> cont x
        | NodeAnd (left, right) ->  Loop left (fun lacc -> 
                                    Loop right (fun racc -> cont (lacc && racc)))
        | NodeOr (left, right) ->   Loop left (fun lacc -> 
                                    Loop right (fun racc -> cont (lacc || racc)))
    Loop t id

let rec evalTree (facc: bool -> bool) (tree:Tree): bool =
    match tree with
    | Leaf x -> facc x
    | NodeAnd (t1, t2) ->
        //let facc' = (fun x -> evalTree t2 (fun y -> facc (x && y)))
        let f x y = x && y
        let facc' = f (evalTree facc t2) 
        evalTree facc' t1
    | NodeOr (t1, t2) ->
        //let f x y = x || y
        let facc' = (||) (evalTree facc t2) 
        evalTree facc' t1

let t1 = NodeAnd (Leaf true, NodeOr (Leaf true, Leaf false))
let t2 = NodeAnd (Leaf true, NodeAnd (Leaf true, Leaf false))