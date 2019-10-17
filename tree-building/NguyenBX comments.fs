module abcxyz
//viết như vầy thì more idiomatic hơn và fix mấy cái compiler warnings 
(*

let rec insert (tree: Tree) record : Tree =
    let { RecordId=rid; ParentId=pid } = record
    match tree with
    | Leaf x when x = pid -> Branch(x, [Leaf rid])
    | Leaf x -> Leaf x
    | Branch (x, trees) when x = pid -> Branch (x, trees @ [Leaf rid])
    | Branch (x, trees) -> Branch (x, trees |> List.map (fun t -> insert t record))

let buildTree input =
    let records = input |> List.sortBy (fun x -> x.RecordId)
    match records with
    | [] -> None
    | head::tail ->
        let initTree = Leaf head.RecordId
        Some <| List.fold insert initTree tail
*)