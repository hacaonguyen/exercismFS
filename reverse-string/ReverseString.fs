module ReverseString

let reverse (input: string): string = 
    Seq.fold (fun acc elem -> string (elem) + acc) "" input