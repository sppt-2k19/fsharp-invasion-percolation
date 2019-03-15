// Learn more about F# at http://fsharp.org

open System
open System.Drawing
open System.IO
open System.Threading.Tasks

    //let array = array2D [[26; 12; 72; 45; 38];
    //              [10; 38; 39; 92; 38];
    //              [44; 29;  0; 29; 77];
    //              [61; 26; 90; 35; 11];
    //              [83; 84; 18; 56; 52]]
    

type FillOrResist = Resistance of int | Filled 
    
let toFile (name : string) (bmp: Bitmap) =
    bmp.Save(name, Imaging.ImageFormat.Png) |> ignore
    bmp
    
    
let flat2Darray array2D = 
            seq { for x in [0..(Array2D.length1 array2D) - 1] do 
                      for y in [0..(Array2D.length2 array2D) - 1] do 
                          yield array2D.[x, y] }
            
let OneZeroPrinter matrix =
    let arr = Array2D.map (fun ele
                            -> match ele with
                                 | Filled -> "1"
                                 | Resistance a -> "0") matrix
    printfn "Lengths: %d,%d" arr.[0,*].Length arr.[*,0].Length
    for i in 0 .. arr.[0,*].Length - 1 do
        printfn ""
        for j in 0 .. arr.[*,0].Length - 1 do
            printf "%s" arr.[i,j]
    printfn "\n---------------"
    
let toColors matrix = 
    Array2D.mapi (fun i j ele
                        -> match ele with
                             | Filled -> (i,j,Color.Black)
                             | Resistance a -> (i,j,Color.White)) matrix
        |> flat2Darray
        |> Seq.toArray
    
// builds a bitmap instance from an array of tuples
let toBitmap a =
    let height = (a |> Array.Parallel.map (fun (x,_,_) -> x) |> Array.max) + 1
    let width = (a |> Array.Parallel.map (fun (_,y,_) -> y) |> Array.max) + 1
    let bmp = new Bitmap(width, height)
    a |> Array.iter (fun (x,y,c) -> bmp.SetPixel(x,y,c))
    bmp    

let matrixBuilder n seed r =
    let rnd = new Random(seed)
    let arr = Array2D.init<FillOrResist> n n (fun _ _ -> Resistance ((rnd.Next() + 1) % r))
    let center = n/2
    arr.[center, center] <- Filled
    arr
    

let findMinIndex (matrixMask:FillOrResist[,], n) =
    let comIndex i j =
        [|      0,-1;
         -1, 0;       1, 0;
                0, 1
        |] |> Array.Parallel.map (fun (dx,dy) ->
            let x,y = i+dx, j+dy
            if x >= 0 && x < n && y >= 0 && y < n && matrixMask.[x,y] = Filled then
                1
            else
                0
        ) |> Array.sum
                
    Array2D.mapi (fun i j e -> if (comIndex i j) > 0 then (i,j) else (-1,-1)) matrixMask
            |> flat2Darray
            |> Seq.filter (fun (a,b) -> not (a = -1) && not (b = -1))
            |> Seq.map (fun (x,y) -> (x,y,matrixMask.[x,y]))
            |> Seq.minBy (fun (a,b,c) -> c)
    
let rec invasionPercolation matrixMask n nfill =
    let m = matrixMask
    match nfill with
    | 0 -> m //Done
    | _ -> 
        let tup = findMinIndex (m, n)
        let i,j,e = tup
        m.[i,j] <- Filled
        invasionPercolation m n (nfill - 1)
    


[<EntryPoint>]
let main argv =
    
    //Benchmark-related part
    let n = 200
    let fill = 1000
    let R = 5000
    Parallel.For(0,100, fun i ->
        let seed = 4312335 * i
        let mmask = matrixBuilder n seed R
        let invPer = invasionPercolation mmask n fill
        
        
        
        //Next part is just for image, not benchmarking
        let runId = System.Random().Next() //For unique filenames
        let path = Path.Combine(Directory.GetCurrentDirectory(), "map",
                                    sprintf "Inv-%i Seed-%i Size-%i Fill-%i.png" runId seed n fill)
        
        toColors invPer |> toBitmap |> toFile path |> ignore
        
        printfn "Done Percolating \n\tSize: %ix%i\n\tFill: %i\n\tSeed: %i" n n fill seed) |> ignore
    0 // return an integer exit code
