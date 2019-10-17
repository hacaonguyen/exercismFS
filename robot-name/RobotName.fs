module RobotName
open System

let mkRobot() = 
    let A2Z = [|'A'..'Z'|] 
    let maxIndex = (int 'Z') - (int 'A') + 1
    let randomI = Random().Next
    let c1 = A2Z.[(randomI maxIndex)]
    let c2 = A2Z.[(randomI maxIndex)]
    let n = randomI 1000
    sprintf "%c%c%03i" c1 c2 n

let name robot = robot

let reset robot = mkRobot()