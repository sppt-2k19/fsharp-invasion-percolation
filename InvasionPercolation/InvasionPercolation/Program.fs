// Learn more about F# at http://fsharp.org

open System
open System

    //let array = array2D [[26; 12; 72; 45; 38];
    //              [10; 38; 39; 92; 38];
    //              [44; 29;  0; 29; 77];
    //              [61; 26; 90; 35; 11];
    //              [83; 84; 18; 56; 52]]

type FillOrResist = Filled | Resistance of int

//TODO change to IComparable on FillOrResist
//let (<) (x: FillOrResist) (y: FillOrResist) = match x,y with
//        | Filled, Filled -> false
//        | Filled, _ -> true
//        | _, Filled -> false
//        | Resistance a, Resistance b -> a < b

let matrixBuilder n seed =
    let rnd = new Random(seed)
    let arr = Array2D.init<FillOrResist> n n (fun _ _ -> Resistance (rnd.Next()))
    let center = n/2
    arr.[center, center] <- Filled
    arr
    

let findMinIndex (matrixMask:FillOrResist[,], n) =
    let comIndex i j =
        [      0,-1;
        -1, 0;       1, 0;
               0, 1
        ] |> List.map (fun (dx,dy) ->
            let x,y = i+dx, j+dy
            if x >= 0 && x < n && y >= 0 && y < n && matrixMask.[x,y] = Filled then
                1
            else
                0
        ) |> List.sum
    let flat2Darray array2D = 
                seq { for x in [0..(Array2D.length1 array2D) - 1] do 
                          for y in [0..(Array2D.length2 array2D) - 1] do 
                              yield array2D.[x, y] }
                
                
    Array2D.mapi (fun i j e -> if (comIndex i j) > 0 then (i,j) else (-1,-1)) matrixMask
            |> flat2Darray
            |> Seq.filter (fun (a,b) -> not (a = -1) && not (b = -1))
            |> Seq.map (fun (x,y) -> (x,y,matrixMask.[x,y]))
            |> Seq.minBy (fun (a,b,c) -> c)
    
    
    
    
        

        
    


let rec invasionPercolation matrixMask n nfill =
    printfn "An iter"
    match nfill with
    | 0 -> matrixMask
    | _ -> 
        let tup = findMinIndex (matrixMask, n)
        let i,j,e = tup
        matrixMask.[i,j] <- Filled
        invasionPercolation matrixMask n (nfill - 1)
    


[<EntryPoint>]
let main argv =
    
    let n = 5
    let seed = 432235
    let mmask = matrixBuilder n seed
    let fuck = findMinIndex (mmask, n)
    let boi = invasionPercolation mmask n 10
        
    printfn "Hello World from F#!"
    0 // return an integer exit code
