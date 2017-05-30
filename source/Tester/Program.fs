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
    let mins = 
        quotes
        |> findMins
        |> Seq.map (fun m -> ("min", m))
    let maxs = 
        quotes
        |> findMaxes
        |> Seq.map (fun m -> ("max", m))
    
    let all =
        Seq.concat [mins;maxs]
        |> Seq.sortBy (fun (_, q) -> q.Date)
        |> Seq.fold (fun acc (key, current) ->
            match acc, key with 
            | [], _ ->
                [(key, current)]
            | ("min", value)::tail, "min" ->
                if value.Low < current.Low then
                    acc
                else
                    ("min", current)::tail
            | ("max", value)::tail, "max" ->
                if value.High > current.High then
                    acc
                else
                    ("max", current)::tail
            | _, _ ->
                (key, current)::acc) []
        |> Seq.rev
        |> Seq.iter (fun (kind, q) -> printfn "%s %A" kind q.Date)
    0
