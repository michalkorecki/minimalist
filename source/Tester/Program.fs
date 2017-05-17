open Minimalist.Detector
open System
open System.IO
open System.Reflection

let loadYear year = fun () ->
    let assembly = Assembly.GetExecutingAssembly()
    use resourceStream = assembly.GetManifestResourceStream("11b.dat")
    use reader = new StreamReader(resourceStream)
    reader
        .ReadToEnd()
        .Split([|Environment.NewLine|], StringSplitOptions.RemoveEmptyEntries)
    |> Seq.rev
    |> Seq.filter (fun q -> q.StartsWith(year.ToString()))
    |> Seq.rev
    |> Seq.toArray

[<EntryPoint>]
let main argv =
    loadYear 2016
    |> findMaxes
    |> Seq.iteri (fun i q -> printfn "%i %A %.2f" i q.Date q.High)
    0
