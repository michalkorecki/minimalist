open Minimalist.Detector
open System
open System.IO
open System.Reflection

let loadRecent n = fun () ->
    let assembly = Assembly.GetExecutingAssembly()
    use resourceStream = assembly.GetManifestResourceStream("11b.dat")
    use reader = new StreamReader(resourceStream)
    reader
        .ReadToEnd()
        .Split([|Environment.NewLine|], StringSplitOptions.RemoveEmptyEntries)
    |> Seq.rev
    |> Seq.take n
    |> Seq.rev
    |> Seq.toArray

[<EntryPoint>]
let main argv =
    loadRecent 100
    |> findMaxes
    |> Seq.iter (printfn "%A")
    0
