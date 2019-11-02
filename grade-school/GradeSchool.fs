module GradeSchool

type School = Map<int, string list>

let empty: School = Map.empty

let add (student: string) (grade: int) (school: School): School = 
    match (Map.tryFind grade school) with //get current lists
    | None -> school |> Map.add grade [student] 
    | Some students -> school |> Map.add grade (student :: students) 

let roster (school: School): string list = 
    school 
    |> Map.toList 
    |> List.sortBy (fun (i, s) -> i) //sort by grade
    |> List.map (fun (i, s) -> s |> List.sort) //get name and sort by name
    |> List.concat 

let grade (number: int) (school: School): string list = 
    match (Map.tryFind number school) with
    | None -> [] 
    | Some students -> students |> List.sort 
    