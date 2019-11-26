module Zipper

type Tree =
    | SubTree of int * Tree Option * Tree Option

let tree n t1 t2 = SubTree (n, t1, t2)

type Path =
    | Top
    | Left of Path * Tree 
    | Right of Path * Tree
    
type TreeZip =
    | TZ of Tree * Path

let fromTree tree = TZ (tree, Top)

let value = function
    | TZ (SubTree (n, _, _), _) -> n

let left = function    
    | TZ (SubTree (_, None, _), _) -> None
    | TZ (SubTree (n, l, r), path) -> 
        TZ (Option.get l, Left (path, SubTree(n, None, r))) |> Some 

let right = function
    | TZ (SubTree (_, _, None), _) -> None
    | TZ (SubTree (n, l, r), path) -> 
        TZ (Option.get r, Right (path, SubTree(n, l, None))) |> Some 

let up = function
    | TZ (_, Top) -> None
    | TZ (tree, Left (p, SubTree(n, _, r))) ->
        TZ (SubTree(n, Some tree, r) ,p) |> Some
    | TZ (tree, Right (p, SubTree(n, l, _))) ->
        TZ (SubTree(n, l, Some tree) ,p) |> Some

let rec toTree = function
    | TZ (tree, Top) -> tree
    | tz -> tz |> up |> Option.get |> toTree 

let setValue n = function
    | TZ (SubTree(_, l, r) , p) -> TZ (SubTree(n, l, r) , p) 

let setLeft value = function
    | TZ (SubTree(n, _, r) , p) -> TZ (SubTree(n, value, r) , p)

let setRight value = function
    | TZ (SubTree(n, l, _) , p) -> TZ (SubTree(n, l, value) , p)

