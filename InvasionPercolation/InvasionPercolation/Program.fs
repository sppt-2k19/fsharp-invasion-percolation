module InvasionPercolation.Program
// Learn more about F# at http://fsharp.org

open System
open System.Drawing
open System.IO
open System.Numerics
open System.Threading.Tasks
open C5

type FillOrResist = Resistance of int | Filled 
    
let toFile (name : string) (bmp: Bitmap) =
    bmp.Save(name, Imaging.ImageFormat.Png) |> ignore
    bmp
    
    
let flat2Darray array2D = 
            seq { for x in [0..(Array2D.length1 array2D) - 1] do 
                      for y in [0..(Array2D.length2 array2D) - 1] do 
                          yield array2D.[x, y] }
    
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

let findPrioQueue (matrixMask:FillOrResist[,]) n (queue:IntervalHeap<int*int*int>) =
    let comIndex =
        [|      0,-1;
         -1, 0;       1, 0;
                0, 1
        |]
    
    let q = queue
    let value,posx,posy = q.DeleteMin()
    let m = matrixMask
    m.[posx, posy] <- Filled
    
    for pair in comIndex do
        let x,y = fst pair + posx, snd pair + posy
        if x >= 0 && x < n && y >= 0 && y < n then 
            match m.[x,y] with
            | Resistance a ->
                    let res = (a,x,y)
                    if not (q.Exists (fun (l,m,n) -> l = a && m = x && n = y)) then q.Add res  |> ignore //We can ignore the add result
            | _ -> ()
    
    m,q
let rec invPerPrioHelper matMask n nfill queue =
    match nfill with
    | 0 -> matMask //Done
    | _ ->
        let m,q = findPrioQueue matMask n queue
        invPerPrioHelper m n (nfill - 1) q
        
        
let invasionPercolationPriorityQueue n nfill dummy =
    let R = 5000
    let matrixMask = matrixBuilder n dummy R
    let p = new IntervalHeap<int*int*int>()
    p.Add((R*2,n/2, n/2)) |> ignore //Enqueue center element with a high value (and ignore the success result)
    invPerPrioHelper matrixMask n nfill p
    
let invPercoBenchmark n nfill dummy =
    let res = invasionPercolationPriorityQueue n nfill dummy
    float32 res.[0,*].Length //return *some* value so that the result isn't completely optimised away

[<EntryPoint>]
let main argv =
    let invPer = invasionPercolationPriorityQueue 200 6000 6
    
    let path = Path.Combine(Directory.GetCurrentDirectory(), "map", sprintf "Inv-%i.png" DateTime.UtcNow.Millisecond)
    let colors = (toColors invPer)
    let bitmap = toBitmap colors
    let file = toFile path bitmap
    printfn "Done"
    
    0 // return an integer exit code
