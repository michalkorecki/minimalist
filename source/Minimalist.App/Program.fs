open Minimalist.Core.Data
open Minimalist.Core.Detector
open System
open System.IO

let loadQuotes path rangeStart rangeEnd =
    use file = File.OpenRead(path)
    use reader = new StreamReader(file)
    reader
        .ReadToEnd()
        .Split([|Environment.NewLine|], StringSplitOptions.RemoveEmptyEntries)
    |> Seq.rev
    |> Seq.map (fun line -> (line, line.Split(',')))
    |> Seq.filter (fun (line, parts) -> parts.[0] >= rangeStart.ToString() && parts.[0] <= rangeEnd.ToString())
    |> Seq.map fst
    |> Seq.rev
    |> Seq.toArray

[<EntryPoint>]
let main argv =
    let path = argv |> Seq.head
    let ticker = Path.GetFileNameWithoutExtension(path)

    printfn "Running for %s in %s" ticker path

    let quotes = loadQuotes path 20160104 20161231 
    let extrema =
        quotes
        |> Seq.mapi parse
        |> Seq.toArray
        |> findExtrema

    extrema
        |> Seq.iter (fun e -> 
            match e with 
            | Max q ->
                printfn "max %A" q.Date
            | Min q -> 
                printfn "min %A" q.Date)
    0
