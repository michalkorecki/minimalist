module Minimalist.Core.ExtremaAdapter

open System.IO
open Minimalist.Core.Data
open Minimalist.Core.Extrema


let findExtrema ticker dateRange storage =
    let quotationsFile = sprintf "%s.txt" ticker
    let quotationsFilePath = Path.Combine(storage, quotationsFile)
    quotationsFilePath
    |> loadQuotationsFromFile dateRange
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