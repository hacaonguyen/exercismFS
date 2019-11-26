module CustomSet

// TODO: define the Set type
type 'a MySet =
    | Empty
    | Set of 'a * 'a MySet

let empty = Empty

let singleton value = Set (value, Empty)

let isEmpty set = 
    match set with 
    | Empty -> true
    | _ -> false

let size set = 
    let rec sizeAcc acc = function 
        | Empty -> acc
        | Set (x, s) -> sizeAcc (acc + 1) s
    sizeAcc 0 set

// check if set contains value
let rec contains value set = 
    match set with 
    | Empty -> false
    | Set (x, s) ->
        match x = value with
        | true -> true
        | false -> contains value s 

let isIn set value = contains value set

let rec isSubsetOf left right = 
    match left with 
    | Empty -> true
    | Set (x, s) -> 
        match right |> contains x with
        | false -> false
        | true -> isSubsetOf s right 

let isEqualTo set1 set2 = 
    (isSubsetOf set1 set2) && (isSubsetOf set2 set1)  

let insert value set = 
    match set |> contains value with
    | true -> set
    | false -> Set (value, set) 

let rec union left right = 
    match left with
    | Empty -> right
    | Set (x, s) -> 
        let right' = insert x right
        union s right'

let intersection left right = 
    
    let rec intersectionAcc acc left right =
        match left with
        | Empty -> acc
        | Set (x, s) -> 
            let acc' = 
                match right |> contains x with 
                | true -> insert x acc 
                | false -> acc
            intersectionAcc acc' s right
    
    intersectionAcc Empty left right
    
// return items in left but not in right
let difference left right = 

    let rec diffAcc acc left right =
        match left with
        | Empty -> acc
        | Set (x, s) -> 
            let acc' = 
                match right |> contains x with 
                | false -> insert x acc 
                | true -> acc
            diffAcc acc' s right

    diffAcc Empty left right

let isDisjointFrom left right = 
    left |> intersection right |> isEmpty  

let fromList list = 
    list |> List.fold (fun acc item -> insert item acc) Empty

let toList set = 
    let cont x xs = x :: xs 
    let rec toListAcc fAcc set =    
        match set with
        | Empty -> fAcc []
        | Set (x, s) -> toListAcc (fAcc >> cont x) s
    toListAcc id set

let rec collect f set =
    match set with
    | Empty -> Empty
    | Set (x, s) -> union (f x) (collect f s)

type SetBuilder() =
    member this.Bind(set, f) = collect f set 
    member this.Yield(item) = singleton item
    member this.YieldFrom(set) = set
    member this.Return(item) = singleton item
    member this.ReturnForm(set) = set   
    member this.Zero() = Empty
    member this.For(set, f) = this.Bind(set, f)
    member this.For(list, f) = this.Bind(list |> fromList, f)
    member this.Combine(set1, set2) = union set1 set2
    member this.Delay(f) = f()

let Set = new SetBuilder()

let s = [1; 2; 3] |> fromList
let x = Set {1; 2; 3} |> toList
let y =
    Set {
        yield! s
        yield 4
        yield 5
        yield! [10..11] |> fromList
        for i in [7..9] |> fromList do yield i
        }
let z =
    Set {
        let! x = fromList [1; 2; 3]
        let! y = fromList [10; 20; 30]
        return x + y
        }
    |> toList |> List.sort