// Learn more about F# at http://fsharp.org
open Util

type School = Map<int, string list>

let empty: School = Map.empty

let add (student: string) (grade: int) (school: School): School = 
    let students = school |> Map.tryFind grade
    match students with
    | None -> school |> Map.add grade [student] 
    | Some xs -> school |> Map.add grade (student :: xs) 

let roster (school: School): string list = 
    school |> Map.toList |> List.map (fun (i, s) -> s) |> List.concat

//let m = empty |> add "Nguyen" 1 |> add "Phong" 5 //|> roster //Map.toList |> List.map (fun (i, s) -> s)
//let m2 = add "Nguyen" 1 m1

//let y = Map.tryFind 2 m

let studentsToSchool (students: List<string*int>):School =
    let schoolFolder school (name,grade) =
        add name grade school
    List.fold schoolFolder empty students

//let school = studentsToSchool [("Aimee", 2)]
let school = studentsToSchool [("Blair", 2); ("James", 2); ("Paul", 2)]


let grade (number: int) (school: School): string list = 
    school.[number] 

let x = grade 2 school

Out1 "" 1



