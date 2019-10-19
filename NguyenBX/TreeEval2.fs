module TreeEval2

type 'a BoolExpTree_Org =
    | Leaf of 'a
    | OrBranch of 'a BoolExpTree_Org list
    | AndBranch of 'a BoolExpTree_Org list
    | NotBranch of 'a BoolExpTree_Org

type BoolExpTree =
    | Leaf of bool
    | OrBranch of BoolExpTree list
    | AndBranch of BoolExpTree list
    | NotBranch of BoolExpTree

let rec evalBoolExpTree (tree: BoolExpTree): bool =
    match tree with
    | Leaf      x       -> x
    | NotBranch tree    -> not (evalBoolExpTree tree)
    | AndBranch []      -> true
    | AndBranch trees   -> trees |> List.fold (fun a i -> a && (evalBoolExpTree i)) true
    | OrBranch  []      -> true
    | OrBranch  trees   -> trees |> List.fold (fun a i -> a || (evalBoolExpTree i)) false