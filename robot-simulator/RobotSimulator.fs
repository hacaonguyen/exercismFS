module RobotSimulator

type Direction = North | East | South | West
type Position = int * int
type Robot = { direction: Direction; position: Position }

let create direction position = 
    {Robot.direction = direction; Robot.position = position}

let moveR (robot: Robot): Robot =
    let direction = 
        match robot.direction with
        | North -> East
        | East  -> South
        | South -> West
        | West  -> North
    
    { robot with direction = direction } 
    
let moveL (robot: Robot): Robot =
    let direction = 
        match robot.direction with
        | North -> West
        | East  -> North
        | South -> East
        | West  -> South
    
    { robot with direction = direction } 

let moveA (robot: Robot): Robot =
    let (x, y) = robot.position
    let position = 
        match robot.direction with
        | North -> (x, y + 1)
        | East  -> (x + 1, y)
        | South -> (x, y - 1)
        | West  -> (x - 1, y)

    { robot with position = position } 

let move1Step robot instruction = 
    match instruction with
    | 'L' -> moveL robot
    | 'R' -> moveR robot
    | 'A' -> moveA robot
    | _   -> failwith "Invalid instruction!"

let move instructions robot = 
    Seq.fold move1Step robot instructions
