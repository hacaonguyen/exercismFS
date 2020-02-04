module NucleotideCount

let count char string =
    string |> Seq.filter ((=) char) |> Seq.length

let validStrand (strand: string) =
    let validSet = "ACGT" |> Set.ofSeq
    strand |> Set.ofSeq |> Set.isSuperset validSet

let nucleotideCounts (strand: string): Option<Map<char, int>> =  
    if validStrand strand then
        "ACGT"
        |> Seq.map (fun c -> (c, count c strand))
        |> Map.ofSeq
        |> Some
    else
        None



