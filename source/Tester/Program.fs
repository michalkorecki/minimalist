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
    loadQuotes 20160104 20161231
    //loadQuotes 20160301 20160322
    |> findMaxes
    |> Seq.iteri (fun i q -> printfn "%i %A %.2f" i q.Date q.High)
    0
