module Grep

open System

let grep (files: string list) (flagArguments: string list) (pattern: string): string list = 

    //1. Sub function: read text from files
    //return a list of tuple (file name, line number, text)
    let readFiles (files: string list) =
    
        //read text from a file
        let readFile file =
            System.IO.File.ReadLines file            
            |> Seq.mapi (fun i e -> (file, i + 1, e)) //convert to tuple
    
        //concat text from files
        //files |> Seq.fold (fun acc e -> Seq.append acc (readFile e)) Seq.empty
        seq { for file in files do yield! (readFile file) }

    //2. Sub function: matching line with pattern    
    let matchLine (line: string) (pattern: string): bool = 

        //2.1. Match entire lines or lines that contain a match
        let matchLine1 (line: string) (pattern: string): bool = 
            match flagArguments |> List.contains "-x" with
            | false -> line.Contains pattern
            | true -> line = pattern

        //2.2. -i Match line using a case-insensitive comparison.
        let matchLine2 (line: string) (pattern: string): bool = 
            match flagArguments |> List.contains "-i" with
            | false -> matchLine1 line pattern
            | true -> 
                let line' = line.ToLower()
                let pattern' = pattern.ToLower()
                matchLine1 line' pattern'

        //-v Invert the program -- collect all lines that fail to match the pattern.
        match flagArguments |> List.contains "-v" with
        | false -> matchLine2 line pattern
        | true -> not (matchLine2 line pattern)

    //3. Sub function: -n Print the line numbers of each matching line.
    //add line number (n) to text (t)
    let addLineNumber n t = 
        match flagArguments |> List.contains "-n" with
        | false -> t
        | true -> sprintf "%i:%s" n t

    //4. Sub funtion: Print filename for multiple files
    //add filename (f) to text (t)
    let addFileName f t = 
        match List.length files with
        | n when n > 1 -> sprintf "%s:%s" f t
        | _ -> t

    //0. Main flow: read data from files and proceed

    let textMatching =
        files
        |> readFiles
        |> Seq.filter (fun (_, _, t) -> matchLine t pattern)

    match flagArguments |> List.contains "-l" with
    | false ->
        textMatching
        |> Seq.map (fun (f, n, t) -> (f, n, (addLineNumber n t)))
        |> Seq.map (fun (f, n, t) -> (f, n, (addFileName f t)))
        |> Seq.map (fun (_, _, t) -> t)
        
    | true ->
        textMatching
        |> Seq.map (fun (f, _, _) -> f)
        |> Seq.distinct
 
    |> Seq.toList
