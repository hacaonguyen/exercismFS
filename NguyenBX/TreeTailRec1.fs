module TreeTailRec1

type Tree =
    | Leaf of bool
    | NodeAnd of Tree * Tree
    | NodeOr of Tree * Tree

let evalTree t =
    let rec Loop t cont =
        match t with
        | Leaf x -> cont x
        | NodeAnd (left, right) ->  Loop left (fun lacc -> 
                                    Loop right (fun racc -> cont (lacc && racc)))
        | NodeOr (left, right) ->   Loop left (fun lacc -> 
                                    Loop right (fun racc -> cont (lacc || racc)))
    Loop t id

let t1 = NodeAnd (Leaf true, NodeOr (Leaf true, Leaf false))
let t2 = NodeAnd (Leaf true, NodeAnd (Leaf true, Leaf false))