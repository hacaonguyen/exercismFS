module Util

let Out1 str x = 
    let y = str + "%A"
    let z = Printf.TextWriterFormat<_> y
    printfn z x

//let Out2 = printfn "%A"