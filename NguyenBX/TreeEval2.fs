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

type BranchType =
    | AndTree
    | OrTree

let rec evalBoolExpTree (tree: BoolExpTree): bool =
    let rec evalTreeList (trees: BoolExpTree list) (treeType: BranchType): bool =
        match trees with
        | [] -> true
        | head::tail ->
            let x = evalBoolExpTree head
            match (treeType, x) with
            | AndTree, false -> false
            | AndTree, _ -> x && (evalTreeList tail AndTree)
            | OrTree, true -> true
            | OrTree, _ -> x || (evalTreeList tail OrTree)
            //match treeType with
            //| AndTree -> x && (evalTreeList tail AndTree)
            //| OrTree -> x || (evalTreeList tail OrTree) 
    match tree with
    | Leaf x -> x
    | NotBranch tree -> evalBoolExpTree tree
    | AndBranch trees -> evalTreeList trees AndTree
    | OrBranch trees -> evalTreeList trees OrTree