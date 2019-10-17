module Notes
let checkLetters2 (input: string) =
    let str = input.ToLower()
    let a2z = ['a'..'z']
    let set1 = str |> Seq.toList |> Set.ofList
    let set2 = a2z |> Set.ofList
    let set = Set.intersect set1 set2
    if set |> Set.count > 0 then Error "letters not permitted"  
    else Ok str

let filter2 (input: string) = 
    let check x = String.exists (fun c -> c = x) "0123456789"
    Ok (String.filter check input)

