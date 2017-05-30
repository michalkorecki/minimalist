open Minimalist.Data
open Minimalist.Detector
open System
open System.IO
open System.Reflection

let loadQuotes rangeStart rangeEnd =
    let assembly = Assembly.GetExecutingAssembly()
    use resourceStream = assembly.GetManifestResourceStream("11b.dat")
    use reader = new StreamReader(resourceStream)
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
    let quotes = loadQuotes 20160104 20161231 
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
