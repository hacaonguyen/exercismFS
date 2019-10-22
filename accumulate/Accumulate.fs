module Accumulate

let rec accumulate' (func: 'a -> 'b) (acc: 'b list) (input: 'a list): 'b list = 
    match input with
    | [] -> []
    | head::tail ->
        let bs = acc @ [func head]
        match tail with        
        | [] -> bs
        | _ -> accumulate' func bs tail

let accumulate (func: 'a -> 'b) (input: 'a list): 'b list = 
    accumulate' func [] input

