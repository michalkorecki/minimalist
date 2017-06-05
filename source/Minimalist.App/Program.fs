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

    printfn "Ticker: %s" ticker
    printfn "File:   %s" path

    loadQuotes path 20160104 20161231
    |> Seq.mapi parse
    |> Seq.toArray
    |> findExtrema
    |> Seq.iter (fun (t, q) -> printf "%A %A" t q.Date)

    0
