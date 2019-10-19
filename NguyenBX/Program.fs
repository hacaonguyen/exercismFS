// Learn more about F# at http://fsharp.org
open System

open TreeEval2

let myTree =
    AndBranch
        [ Leaf true
        ; OrBranch
            [ AndBranch
                [ Leaf true
                ; OrBranch [Leaf false; Leaf true; Leaf false]
                ]
            ; OrBranch [] // also a TRUE node
            ; NotBranch (Leaf true)
            ]
        ]

let x = evalBoolExpTree myTree

printfn "%A" x