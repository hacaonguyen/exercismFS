module KindergartenGarden

type Plant = Clover | Grass | Radishes | Violets 

let toPlant char = 
    match char with
    | 'C' -> Plant.Clover
    | 'G' -> Plant.Grass
    | 'R' -> Plant.Radishes
    | _   -> Plant.Violets

let plants (diagram: string) (student: string) = 
    let diagrams = diagram.Split '\n'
    let position = (int (student.ToUpper().[0]) - int 'A') * 2
    
    [ 
        diagrams.[0].[position]; 
        diagrams.[0].[position + 1]; 
        diagrams.[1].[position]; 
        diagrams.[1].[position + 1]
    ] 
    |> List.map toPlant