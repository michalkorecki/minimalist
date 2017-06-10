module Minimalist.Core.Extrema

open Minimalist.Core
open Minimalist.Core.Data
open System


let private limitTo (rangeStart, rangeEnd) quotes =
    quotes
    |> Seq.skip rangeStart
    |> Seq.take (rangeEnd - rangeStart + 1)

let private (|Range|_|) (rangeStart, rangeEnd) = 
    if rangeStart >= rangeEnd then
        None
    else
        Some (rangeStart, rangeEnd)

let private findMaxes (quotes : Quotation[]) =
    let rec findMaxesBinary (results : list<Quotation>) = function
        | Range (rangeStart, rangeEnd) ->
            let max =
                quotes
                |> limitTo (rangeStart, rangeEnd)
                |> Seq.maxBy (fun q -> q.High)
            let bearTrendReversal = Trend.findReversalForward Trend.Bear (max.Index, rangeEnd) quotes
            let bear = 
                match bearTrendReversal with
                | Some index when index < rangeEnd && index > rangeStart ->
                    findMaxesBinary results (index, rangeEnd)
                | _ ->
                    []
            let bullTrendReversal = Trend.findReversalBackwards Trend.Bull (rangeStart, max.Index) quotes
            let bull =
                match bullTrendReversal with
                | Some index when index < rangeEnd && index > rangeStart ->
                    findMaxesBinary results (rangeStart, index)
                | _ ->
                    []

            List.concat [max::bull;bear]
        | _ ->
            results

    findMaxesBinary [] (0, quotes.Length - 1) 
    |> Seq.distinct
    |> Seq.toList

let private findMins (quotes : Quotation[]) =
    let rec findMinsImpl (results : list<Quotation>) = function
        | Range (rangeStart, rangeEnd) ->
            let min =
                quotes
                |> limitTo (rangeStart, rangeEnd)
                |> Seq.minBy (fun q -> q.Low)
            let bullTrendReversal = Trend.findReversalForward Trend.Bull (min.Index, rangeEnd) quotes
            let bull = 
                match bullTrendReversal with
                | Some index when index < rangeEnd && index > rangeStart ->
                    findMinsImpl results (index, rangeEnd)
                | _ ->
                    []
            let bearTrendReversal = Trend.findReversalBackwards Trend.Bear (rangeStart, min.Index) quotes
            let bear =
                match bearTrendReversal with
                | Some index when index < rangeEnd && index > rangeStart ->
                    findMinsImpl results (rangeStart, index)
                | _ ->
                    []
            
            List.concat [min::bull;bear]
        | _ ->
            results
    
    findMinsImpl [] (0, quotes.Length - 1)
    |> Seq.distinct
    |> Seq.toList


let findExtrema quotations =
    let mins = findMins quotations |> Seq.map (fun q -> (Minimum, q))
    let maxes = findMaxes quotations |> Seq.map (fun q -> (Maximum, q))
    [mins;maxes]
    |> Seq.concat
    |> Seq.sortBy (fun (_, q) -> q.Date)
    |> Seq.fold (
        fun acc extremum ->
            match acc, extremum with 
            | [], _ ->
                [extremum]
            | (Minimum, head)::tail, (Minimum, current) ->
                if head.Low < current.Low then
                    acc
                else
                    extremum::tail
            | (Maximum, head)::tail, (Maximum, current) ->
                if head.High > current.High then
                    acc
                else
                    extremum::tail
            | _, _ ->
                extremum::acc) []
    |> Seq.rev

    