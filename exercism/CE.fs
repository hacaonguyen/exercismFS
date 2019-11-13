module CE

let strToInt (str: string) = 
    try 
        str |> int |> Some
    with
    | _ -> None

type MaybeBuilder() =
    member this.Bind(m, f) = Option.bind f m
    member this.Return(x) = Some x

let workflow = new MaybeBuilder()

let stringAddWorkflow x y z = 
    workflow 
        {
        let! a = strToInt x
        let! b = strToInt y
        let! c = strToInt z
        return a + b + c
        }    

// test
let good = stringAddWorkflow "12" "3" "2"
let bad = stringAddWorkflow "12" "xyz" "2"

//cách khác cho workflow
let workflow1 x y z =
    workflow.Bind ((strToInt x), (fun a ->
        workflow.Bind ((strToInt y), (fun b ->
            workflow.Bind ((strToInt z), (fun c -> 
                Some (a + b + c)))))))

//cách khác nữa cho workflow
let workflow2 x y z =
    strToInt x |> Option.bind (fun a ->
        strToInt y |> Option.bind (fun b ->
            strToInt z |> Option.bind (fun c -> 
                Some (a + b + c))))

//cách khác nữa
let (>>=) m f = Option.bind f m

let workflow3 x y z =
    strToInt x >>= fun a ->
    strToInt y >>= fun b ->
    strToInt z >>= fun c ->
    Some (a + b + c)

//Hoặc dùng hàm strAdd
let strAdd (str: string) i =
    try
        str |> int |> (+) i |> Some
    with
    | _ -> None

let good1 = Some 0 >>= strAdd "1" >>= strAdd "2" >>= strAdd "3"
let bad1 = strToInt "1" >>= strAdd "xyz" >>= strAdd "3"