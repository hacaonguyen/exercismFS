module BeerSong

//recite item n
let item n =     
    match n with
    | 0 -> [
        "No more bottles of beer on the wall, no more bottles of beer.";
        "Go to the store and buy some more, 99 bottles of beer on the wall." ] 
    | 1 -> [
        "1 bottle of beer on the wall, 1 bottle of beer.";
        "Take it down and pass it around, no more bottles of beer on the wall." ] 
    
    | _ -> [
        yield sprintf "%i bottles of beer on the wall, %i bottles of beer." n n
        let s = if n = 2 then "1 bottle" else sprintf "%i bottles" (n - 1) 
        yield sprintf "Take one down and pass it around, %s of beer on the wall." s ]

let recite (startBottles: int) (takeDown: int) =  
    [1..takeDown]
    |> List.collect (fun i -> item (startBottles - i + 1) |> List.append [""])
    |> List.tail
