module TreeTailRec2
(*
open System.Collections.Generic
open FSharpPlus

[<ReferenceEquality>] // performing map lookups using structural equality is 10 times slower or worse
type 'a BoolExpTree =
    | Leaf of 'a
    | OrBranch of 'a BoolExpTree list
    | AndBranch of 'a BoolExpTree list
    | NotBranch of 'a BoolExpTree

module BoolExpTree =
    let evaluate (f: 'a -> bool) tree =
        let processed = new Dictionary<'a BoolExpTree, bool>()
        let backlog = new Stack<'a BoolExpTree * 'a BoolExpTree option>()

        let inline evaluateNodeInAndBranch parent peers node value =
            match value with
            | false -> processed.[parent] <- false // short-circuit
            | true ->
                // the last evaluated node's value is also the parent's evaluation value (no short-circuits happened)
                // since we are using stack, the last node to be evaluated is at the head of its peers
                if node = List.head peers then processed.[parent] <- true

        let inline evaluateNodeInOrBranch parent peers node value =
            match value with
            | true -> processed.[parent] <- true // short-circuit
            | false -> if node = List.head peers then processed.[parent] <- false

        let mutable result = true

        let inline evaluateBranch parent branch subtrees =
            let evaluateSubtreesThenBranch () =
                backlog.Push (branch, parent) // to revisit it after all its subtrees have been processed
                match subtrees with
                | [] -> processed.[branch] <- true // branch with no criteria is considered as a TRUE node
                | xs -> xs |> List.iter (fun subtree -> backlog.Push (subtree, Some branch))

            let processedValue = processed |> Dict.tryGetValue branch
            match processedValue, parent with
            | Some value, None -> result <- value
            | Some _, Some (Leaf x) -> invalidArg (sprintf "%A" x) "Leaf cannot be a parent node"
            | Some value, Some (AndBranch peers as parentBranch) ->
                match processed |> Dict.tryGetValue parentBranch with
                | None -> evaluateNodeInAndBranch parentBranch peers branch value
                | Some _ -> () // short-circuit
            | Some value, Some (OrBranch peers as parentBranch) ->
                match processed |> Dict.tryGetValue parentBranch with
                | None -> evaluateNodeInOrBranch parentBranch peers branch value
                | Some _ -> () // short-circuit
            | Some value, Some (NotBranch _ as parentBranch) -> processed.[parentBranch] <- not value
            | None, Some (Leaf x)  -> invalidArg (sprintf "%A" x) "Leaf cannot be a parent node"
            | None, Some parentBranch ->
                match processed |> Dict.tryGetValue parentBranch with
                | None -> evaluateSubtreesThenBranch ()
                | Some _ -> ()
            | None, None -> evaluateSubtreesThenBranch ()

        backlog.Push (tree, None)
        while backlog.Count > 0 do
            let current, parent = backlog.Pop()
            match current with
            | Leaf a ->
                match parent with
                | None -> result <- f a
                | Some (AndBranch peers as parentBranch) ->
                    match processed |> Dict.tryGetValue parentBranch with
                    | None -> evaluateNodeInAndBranch parentBranch peers current (f a)
                    | Some _ -> () // short-circuit
                | Some (OrBranch peers as parentBranch) ->
                    match processed |> Dict.tryGetValue parentBranch with
                    | None -> evaluateNodeInOrBranch parentBranch peers current (f a)
                    | Some _ -> () // short-circuit
                | Some (NotBranch _ as parentBranch) -> processed.[parentBranch] <- not <| f a
                | Some (Leaf x) -> invalidArg (sprintf "%A" x) "Leaf cannot be a parent node"
            | OrBranch subtrees
            | AndBranch subtrees -> evaluateBranch parent current subtrees
            | NotBranch subtree -> evaluateBranch parent current [subtree]
        result
*)
type Tree =
    | Leaf of bool
    | NodeAnd of Tree * Tree
    | NodeOr of Tree * Tree
(*
let rec evalTree (acc) (ts: Tree list) (t: Tree): bool = 
    match t with
    | Leaf x ->
        match ts with
        | [] -> acc x
        | head :: tail ->
            evalTree acc tail head
    | NodeAnd (t1, t2) -> 
        let acc' = (&&) (acc)   
        evalTree acc' (t2::ts) t1
    | NodeOr (t1, t2) ->
        let acc' = (||) (acc)
        evalTree acc' (t2::ts) t1
*)
let t1 = NodeAnd (Leaf true, NodeOr (Leaf true, Leaf false))
let t2 = NodeAnd (Leaf true, NodeAnd (Leaf true, Leaf false))