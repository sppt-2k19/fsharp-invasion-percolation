//Taken from https://gist.github.com/tjaskula/18a51f84ef06b4819b0952bb7e691828
module InvasionPercolation.PriorityQueue

open System
open Microsoft.FSharp.Core

// maybe there is a better way to simplify this
let private (|Greater|_|) descendent compareResult =
    match compareResult with
    | n when n < 0 && descendent  -> None
    | n when n < 0 && not descendent  -> Some()
    | 0 -> None
    | n when n > 0 && descendent -> Some()
    | n when n > 0 && not descendent -> None
    | _ -> failwith "Impossible case for IComparable result"

let private isGreater x y descendent =
    match compare x y with
    | Greater descendent _ -> true
    | _ -> false

let private isLower x y descendent = not (isGreater x y descendent)

type PriorityQueue<'T when 'T : comparison>(values: seq<'T>, isDescending: bool) =
    let heap : System.Collections.Generic.List<'T> = System.Collections.Generic.List<'T>(values)
    
    let mutable size = heap.Count

    let parent i = (i - 1) / 2
    let leftChild i = 2 * i + 1
    let rightChild i = 2 * i + 2

    let swap i maxIndex =
        let temp = heap.[i]
        heap.[i] <- heap.[maxIndex]
        heap.[maxIndex] <- temp

    let siftUp i =
        let mutable indx = i
        while indx > 0 && isLower heap.[parent indx] heap.[indx] isDescending do
            swap (parent indx) indx
            indx <- parent indx

    let rec siftDown i =
        let l = leftChild i
        let r = rightChild i
        // maybe there is a cleaner way to express those
        let maxIndexLeft = if l < size && isGreater heap.[l] heap.[i] isDescending then l else i
        let maxIndex = if r < size && isGreater heap.[r] heap.[maxIndexLeft] isDescending then r else maxIndexLeft
        if i <> maxIndex then
            swap i maxIndex
            siftUp maxIndex
        else ()
    
    let build (unsortedValues: seq<'T>) =
        for i = size / 2 downto 0 do
            siftDown i
    
    do build heap

    new (values) = PriorityQueue<'T>(values, true)

    member this.IsEmpty = size = 0

    member this.Dequeue() =
        if this.IsEmpty then raise (new Exception("No more elements to dequeue"))
        let result = heap.[0]
        heap.[0] <- heap.[size - 1]
        // we limit the boundary but the last element stays in memory
        // we could use heap.Remove but it's O(n) operation so too slow
        size <- size - 1
        siftDown 0
        result

    member this.Enqueue(p: 'T) =
        if heap.Count = size then
            heap.Add(p)
        else
            heap.[size] <- p
        size <- size + 1
        siftUp (size - 1)
    
    member this.Exists (p: 'T) =
        heap |> Seq.filter (fun e -> e = p) |> Seq.isEmpty |> not

// usage (call Dequeue(), Enqueue ...)

let pMax = new PriorityQueue<int>([|3; 1; 4; 2|])
let pMin = new PriorityQueue<int>([|3; 1; 4; 2|], false)

[<CustomComparison; StructuralEquality>]
type Point = { X: int; 
               Y: int }
                interface IComparable<Point> with
                    member this.CompareTo other =
                        compare this.Y other.Y
                interface IComparable with
                    member this.CompareTo(obj: obj) =
                        match obj with
                        | :? Point -> compare this.Y (unbox<Point> obj).Y
                        | _ -> invalidArg "obj" "Must be of type Point"
                        

let p1 = {X = 10; Y = 1}
let p2 = {X = 1; Y = 10}

let pointMax = new PriorityQueue<Point>([|p1; p2|])
let pointMin = new PriorityQueue<Point>([|p1; p2|], false)

