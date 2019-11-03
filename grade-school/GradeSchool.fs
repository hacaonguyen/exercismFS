module GradeSchool

type School = Map<int, string list>

let empty: School = Map.empty

let add (student: string) (grade: int) (school: School): School = 
    let students = school |> Map.tryFind grade |> Option.defaultValue [] //get current lists
    school |> Map.add grade (student :: students)
 
let roster (school: School): string list = 
    school 
    |> Map.toList 
    |> List.sortBy fst //sort by grade
    |> List.map (snd >> List.sort) //get name and sort by name
    |> List.concat 

let grade (number: int) (school: School): string list = 
    school 
    |> Map.tryFind number
    |> Option.defaultValue []
    |> List.sort
