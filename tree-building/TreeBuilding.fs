// This is the file you need to modify for your own solution.
// The unit tests will use this code, and it will be used by the benchmark tests
// for the "Mine" row of the summary table.

// Remember to not only run the unit tests for this exercise, but also the
// benchmark tests using `dotnet run -c Release`.
// Please refer to the instructions for more information about the benchmark tests.

module TreeBuilding

open TreeBuildingTypes

type Tree =
    | Leaf of int
    | Branch of int * Tree list
 
let recordId t =
    match t with
    | Branch (id, c) -> id
    | Leaf id -> id

let isBranch t =
    match t with
    | Branch (id, c) -> true
    | Leaf id -> false

let children t =
    match t with
    | Branch (id, c) -> c
    | Leaf id -> []

let rec insert (record: Record) (tree: Tree): Tree = 
    let { RecordId=rid; ParentId=pid } = record
    if rid <= pid then failwith "Nodes with invalid parents"
    match tree with
    | Leaf x when x = pid            -> Branch(x, [Leaf rid])
    | Leaf x                         -> Leaf x
    | Branch (x, trees) when x = pid -> Branch (x, trees @ [Leaf rid])
    | Branch (x, trees)              -> Branch (x, trees |> List.map (insert record))
       
let buildTree (input: Record list) =
    let records = input |> List.sortBy (fun x -> x.RecordId)
    match records with
    | [] -> failwith "Empty input"
    | head::tail -> 
        if head <> { RecordId = 0; ParentId = 0} then failwith "Root node is invalid"
        let initTree = Leaf head.RecordId
        tail |> List.fold (fun acc e -> insert e acc) initTree
