module Minimalist.Core.ExtremaDetector

open Minimalist.Core
open Minimalist.Core.Data
open System




//todo: limit to
let private narrowTo (rangeStart, rangeEnd) quotes =
    quotes
    |> Seq.skip rangeStart
    |> Seq.take (rangeEnd - rangeStart + 1)

let private findMaxesBinary (quotes : Quotation[]) =
    //todo: make this tail recursive
    let rec findMaxesBinaryImpl range (results : list<Quotation>)=
        let rangeStart, rangeEnd = range
        if rangeStart >= rangeEnd then
            results
        else
            let max =
                quotes
                |> narrowTo range
                |> Seq.maxBy (fun q -> q.High)
            let bearTrendEnd = Trend.findReversalForward Trend.Bear (max.Index, rangeEnd) quotes
            let bear = 
                match bearTrendEnd with
                | Some index when index < rangeEnd && index > rangeStart ->
                    findMaxesBinaryImpl (index, rangeEnd) results
                | _ ->
                    []
            let bullTrendEnd = Trend.findReversalBackwards Trend.Bull (rangeStart, max.Index) quotes
            let bull =
                match bullTrendEnd with
                | Some index when index < rangeEnd && index > rangeStart ->
                    findMaxesBinaryImpl (rangeStart, index) results
                | _ ->
                    []

            List.concat [max::bull;bear]

    findMaxesBinaryImpl (0, quotes.Length - 1) []
    |> Seq.distinct
    |> Seq.toList

let private findMinsBinary (quotes : Quotation[]) =
    let rec findMinsBinaryImpl range (results : list<Quotation>) =
        let rangeStart, rangeEnd = range
        if rangeStart >= rangeEnd then
            results
        else
            let min =
                quotes
                |> narrowTo range
                |> Seq.minBy (fun q -> q.Low)
            let bullTrendEnd = Trend.findReversalForward Trend.Bull (min.Index, rangeEnd) quotes
            let bull = 
                match bullTrendEnd with
                | Some index when index < rangeEnd && index > rangeStart ->
                    findMinsBinaryImpl (index, rangeEnd) results
                | _ ->
                    []
            let bearTrendEnd = Trend.findReversalBackwards Trend.Bear (rangeStart, min.Index) quotes
            let bear =
                match bearTrendEnd with
                | Some index when index < rangeEnd && index > rangeStart ->
                    findMinsBinaryImpl (rangeStart, index) results
                | _ ->
                    []
            
            List.concat [min::bull;bear]
    
    findMinsBinaryImpl (0, quotes.Length - 1) []
    |> Seq.distinct
    |> Seq.toList


let findExtrema quotations =
    let mins = findMinsBinary quotations |> Seq.map (fun q -> (Minimum, q))
    let maxes = findMaxesBinary quotations |> Seq.map (fun q -> (Maximum, q))
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

    