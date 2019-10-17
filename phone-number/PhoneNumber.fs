module PhoneNumber
open System

//check null string
let checkNull (input: string) =
    match input with
    | null -> Error "null input"
    | _ -> Ok input
    
//check empty string and trim
let checkEmpty (input: string) = 
    let str = input.Trim()
    match str with 
    | "" -> Error "string empty"
    | _ -> Ok str

//check if a string have letter a, b, c, ...
let checkLetters (input: string) =
    let str = String.filter Char.IsLetter input
    match str with
    | "" -> Ok input
    | _ -> Error "letters not permitted"

//check punctuations
let checkPunct (input: string) = 
    let validChar x = String.exists ((=) x) "0123456789 +-.()"
    let str = String.filter validChar input
    if str.Length = input.Length then Ok input
    else Error "punctuations not permitted"

//remove non-numeric characters
let filter (input: string) = 
    let str = String.filter Char.IsNumber input
    match str with
    | "" -> Error "no number"
    | _ -> Ok str

let checkLength (input:string) =
    let len = input.Length
    if len < 10 then Error "incorrect number of digits"
    elif len > 11 then Error "more than 11 digits"
    else Ok input

//Check 11 Digits, if Ok convert to 10 Digits
let check11Digits (input: string) =
    if (input.Length = 10) then Ok input
    elif input.[0] = '1' then Ok (input.Substring(1))
    else Error "11 digits must start with 1"

let checkAreaCode (input: string) =
    let areaCode = input.[0]
    match areaCode with
    | '0' -> Error "area code cannot start with zero"
    | '1' -> Error "area code cannot start with one"
    | _ -> Ok input

let checkExchangeCode (input: string) =
    let exchangeCode = input.[3]
    match exchangeCode with
    | '0' -> Error "exchange code cannot start with zero"
    | '1' -> Error "exchange code cannot start with one"
    | _ -> Ok input

let clean input = 
    input 
    |> checkNull
    |> Result.bind checkEmpty
    |> Result.bind checkLetters
    |> Result.bind checkPunct
    |> Result.bind filter
    |> Result.bind checkLength
    |> Result.bind check11Digits
    |> Result.bind checkAreaCode
    |> Result.bind checkExchangeCode
    |> Result.map uint64
