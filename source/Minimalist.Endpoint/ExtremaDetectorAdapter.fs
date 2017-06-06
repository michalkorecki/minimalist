module Minimalist.Core.ExtremaDetectorAdapter

open System
open System.IO
open Minimalist.Core.Data
open Minimalist.Core.ExtremaDetector

let private loadQuotations year file =
    try
        use fileStream = File.OpenRead(file)
        use reader = new StreamReader(fileStream)
        let quotations =
            reader
                .ReadToEnd()
                .Split([|Environment.NewLine|], StringSplitOptions.RemoveEmptyEntries)
            |> Seq.map (fun line -> (line, line.Split(',')))
            |> Seq.filter (fun (line, parts) -> parts.[0].Substring(0, 4) = year.ToString())
            |> Seq.map fst
            |> Seq.mapi parse
            |> Seq.toArray
        Some quotations
    with
        _ -> None


let findExtrema ticker year storage =
    let quotationsFile = sprintf "%s.txt" ticker
    let quotationsFilePath = Path.Combine(storage, quotationsFile)
    quotationsFilePath
    |> loadQuotations year
    |> function
        | Some q ->
            findExtrema q
            |> Seq.map (fun (t, q) -> 
                match t with
                | Minimum -> { Type = t; Value = q.Low; Date = q.Date }
                | Maximum -> { Type = t; Value = q.High; Date = q.Date })
            |> Some
        | None ->
            None