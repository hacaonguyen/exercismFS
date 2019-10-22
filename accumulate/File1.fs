module File1

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

// Cách rút gọn
let accumulate1 (func: 'a -> 'b) (input: 'a list): 'b list = 
    let rec f func acc = function
    | [] -> acc
    | head::tail -> f func (acc @ [func head]) tail
    f func [] input    

// Cách non rec-tail
let rec accumulate0 (func: 'a -> 'b) (input: 'a list): 'b list = 
    match input with
    | [] -> []
    | head::tail -> (func head)::(accumulate0 func tail)