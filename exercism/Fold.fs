module Fold

// FoldBack : ('a -> 'b -> 'b) -> list<'a> -> 'b -> 'b
let rec FoldBack (func: 'a -> 'b -> 'b) (input: 'a list) (acc: 'b): 'b =
    match input with
    | [] -> acc
    | head :: tail -> func head (FoldBack func tail acc) 

// FoldBack rec tail
let rec FoldBack1 (func: 'a -> 'b -> 'b) (input: 'a list) (facc: 'b -> 'b): ('b -> 'b) =
    match input with
    | [] -> facc
    | head :: tail -> 
        let facc' = facc << func head
        FoldBack1 func tail facc' 
// Map using FoldBack
let rec mapFB (fMap: 'a -> 'b) (input: 'a list): 'b list =
    let fFB e acc = (fMap e)::acc
    FoldBack1 fFB input id []

let x = mapFB (fun x -> x + 1) [1; 2; 3; 4]  

printfn "%d" (FoldBack1 (fun e acc -> e+acc) [1; 2; 3; 4] id 0) // sum: 10
printfn "%d" (FoldBack1 (fun _ acc -> 1+acc) [1; 2; 3; 4] id 0) // length: 4
printfn "%A" (FoldBack1 (fun e acc -> e::acc) [1; 2; 3; 4] id []) // reverse: [4; 3; 2; 1]

// Fold : ('a -> 'b -> 'a) -> 'a -> list<'b> -> 'a
let rec Fold (func: 'a -> 'b -> 'a) (acc: 'a) (input: 'b list): 'a =
    match input with
    | [] -> acc
    | head :: tail -> 
        let acc' = func acc head
        Fold func acc' tail

