module Minimalist.Core.Extrema

open System
open System.IO
open Minimalist.Core.Data
open Minimalist.Core.Detector

let private loadQuotations year file =
    try
        use fileStream = File.OpenRead(file)
        use reader = new StreamReader(fileStream)
        let quotations =
            reader
                .ReadToEnd()
                .Split([|Environment.NewLine|], StringSplitOptions.RemoveEmptyEntries)
            |> Seq.rev
            |> Seq.map (fun line -> (line, line.Split(',')))
            |> Seq.filter (fun (line, parts) -> parts.[0].Substring(0, 4) = year.ToString())
            |> Seq.map fst
            |> Seq.rev
            |> Seq.mapi parse
            |> Seq.toArray
        Some quotations
    with
        _ -> None

let private getQuotationFilePath ticker =
    //todo: might be better to pass this as dependency
    let quotationsStorage = @"D:\Stock\quotes\poland_d\wse stocks"
    let quotationsFile = sprintf "%s.txt" ticker
    Path.Combine(quotationsStorage, quotationsFile)


let extrema ticker year =
    getQuotationFilePath ticker
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