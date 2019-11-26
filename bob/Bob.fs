module Bob

let noLetter (input: string): bool = 
    let lowerChars = ['a'..'z'] |> Set.ofList
    let upperChars = ['A'..'Z'] |> Set.ofList
    let allChars = Set.union lowerChars upperChars
    input |> Set.ofSeq |> Set.intersect allChars |> Set.isEmpty

let noNumber (input: string): bool =
    let number = ['0'..'9'] |> Set.ofList
    input |> Set.ofSeq |> Set.intersect number |> Set.isEmpty

// check if a string contains all capitalize chars
let allCap (input: string): bool = 
    (input.ToUpper() = input) && (input |> noLetter |> not)

let isAsk (input: string): bool = 
    input.TrimEnd().EndsWith('?')  

let isQuestion (input: string): bool = 
    (allCap input) && (isAsk input)

let isYell (input: string): bool = 
    allCap input

let isNothing (input: string): bool =
    (noLetter input) && (noNumber input)

let response (input: string): string = 
    if   isQuestion input then "Calm down, I know what I'm doing!"
    elif isAsk      input then "Sure."
    elif isYell     input then "Whoa, chill out!"
    elif isNothing  input then "Fine. Be that way!"
    else                       "Whatever."
