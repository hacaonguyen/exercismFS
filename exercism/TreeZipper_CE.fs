//Copy from http://tomasp.net/blog/tree-zipper-query.aspx/
module TreeZipper_CE

module TreeZip

type Tree<'T> = 
    | Node of Tree<'T> * Tree<'T>
    | Leaf of 'T

type Path<'T> = 
    | Top 
    | Left of Path<'T> * Tree<'T>
    | Right of Path<'T> * Tree<'T>

type TreeZipper<'T> = 
    | TZ of Tree<'T> * Path<'T>

/// Navigates to the left sub-tree
let left = function
    | TZ(Leaf _, _) -> failwith "cannot go left"
    | TZ(Node(l, r), p) -> TZ(l, Left(p, r))

/// Navigates to the right sub-tree
let right = function
    | TZ(Leaf _, _) -> failwith "cannot go right"
    | TZ(Node(l, r), p) -> TZ(r, Right(p, l))

/// Gets the value at the current position
let current = function
    | TZ(Leaf x, _) -> x
    | _ -> failwith "cannot get current"

// Create a sample tree
let branches = 
    Node( Node(Leaf 1, Leaf 3), 
        Node(Leaf 7, Node(Leaf 12, Leaf 20)) )

// Wrap it as a zipper & print
let sample = TZ(branches, Top)
//printfn "%O" sample 

// Get one of the tree leaves
//let x = sample |> right |> right |> left |> current

// Navigate to the parent node
let up = function
  | TZ(l, Left(p, r))
  | TZ(r, Right(p, l)) -> TZ(Node(l, r), p)
  | TZ(_, Top) -> failwith "cannot go up"

// Navigate to the root of the tree
let rec top = function
  | TZ(_, Top) as t -> t
  | tz -> top (up tz)

/// Build tree zipper with singleton tree
let unit v = TZ(Leaf v, Top)

/// Transform leaves in the current sub-tree of 'treeZip'
/// into other trees using the provided function 'f'
let bindSub f treeZip = 
  let rec bindT = function
    | Leaf x -> let (TZ(t, _)) = top (f x) in t
    //| Leaf x -> Leaf (f x)
    | Node(l, r) -> Node(bindT l, bindT r)
  let (TZ(current, path)) = treeZip
  TZ(bindT current, path)

type TreeZipperBuilder() = 
  /// Enables the 'for x in xs do ..' syntax
  member x.For(tz:TreeZipper<'T>, f) : TreeZipper<'T> = bindSub f tz
  /// Enables the 'yield x' syntax
  member x.Yield(v) = unit v

type TreeZipperBuilder with
  // Operations for navigation through the tree
  [<CustomOperation("left", MaintainsVariableSpace=true)>]
  member x.Left(tz) = left tz
  [<CustomOperation("right", MaintainsVariableSpace=true)>]
  member x.Right(tz) = right tz
  [<CustomOperation("up", MaintainsVariableSpace=true)>]
  member x.Up(tz) = up tz
  [<CustomOperation("top", MaintainsVariableSpace=true)>]
  member x.Top(tz) = top tz

  /// Extracts the current value and returns it
  [<CustomOperation("current", MaintainsVariableSpace=false)>]
  member x.Current(tz) = current tz

  /// Transform the current sub-tree using 'f'
  [<CustomOperation("map", MaintainsVariableSpace=true)>]
  member x.Select(tz, [<ProjectionParameter>] f) = bindSub (f >> unit) tz

/// Global instance of the computation builder
let tree = TreeZipperBuilder()

let x = tree { for x in sample do yield x * 2 }

let y = 
    tree { 
       for x in sample do
            right
            right
            left
            current
    }

let z = 
    tree {
        for x in sample do
        left
        map (x * 2)
        up
        right
        map (x / 2) 
        top 
    }

