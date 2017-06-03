module Minimalist.Core.Extrema

open System
open System.IO
open Minimalist.Core.Data
open Minimalist.Core.Detector

type Ticker = Ticker of string

//todo: bad naming, clashing with detector code; think about domain
type ExtremumType =
    | Minimum
    | Maximum

type Extremum = {
    Type : ExtremumType
    Value : double;
    Date : DateTime;
}

type Extrema =
    | Extrema of seq<Extremum>
    | Error of string

let extrema ticker year =
    let quotationsStorage = @"D:\Stock\quotes\poland_d\wse stocks"
    try
        let quotationsFile = sprintf "%A.txt" ticker
        let quotationsFilePath = Path.Combine(quotationsStorage, quotationsFile)
        use file = File.OpenRead(quotationsFilePath)
        use reader = new StreamReader(file)
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
        let extrema =
            findExtrema quotations
            |> Seq.map (fun e ->
                match e with
                | Min q -> { Type = Minimum; Value = q.Low; Date = q.Date }
                | Max q -> { Type = Maximum; Value = q.High; Date = q.Date })
        Extrema extrema
    with 
        | :? FileNotFoundException -> Error "File not found"
        | _ -> Error "Failed"