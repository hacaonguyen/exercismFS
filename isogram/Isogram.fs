module Isogram
open System

//check if a letter repeating in a string. If not it returns true 
//spaces and hyphens are allowed to appear multiple times 
let notRepeating (c:char) (s:string): bool = 
    let exclude:string = " -"
    if exclude.IndexOf c >= 0 then true
    else (s.IndexOf c) = (s.LastIndexOf c)
 
let isIsogram (str: string): bool = 
    let x = str.ToLower()
    Seq.fold (fun acc elem -> acc && (notRepeating elem x)) true x
