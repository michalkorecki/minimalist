module Minimalist.Core.ExtremaDetector

open Minimalist.Core.Data
open Minimalist.Core.Indicators
open System


[<Literal>]
let private DmSeekSize = 5

[<Literal>]
let private DmChangeCount = 2

let private isExhausted (rangeStart, rangeEnd) =
    rangeStart + DmSeekSize > rangeEnd

let private narrowTo (rangeStart, rangeEnd) quotes =
    quotes
    |> Seq.skip rangeStart
    |> Seq.take (rangeEnd - rangeStart + 1)

let private directionalMovementIndicator skippedCount quotes =
    //todo: pairs as dependency?
    let pairs =
        quotes
        |> Seq.skip skippedCount
        |> Seq.take (DmSeekSize + 1)
        |> Seq.pairwise
    let trueRange =
        pairs
        |> Seq.map (fun (yesterday, today) -> trueRange yesterday today)
        |> Seq.sum
    let dmPlus, dmMinus =
        pairs
        |> Seq.map (fun (yesterday, today) -> directionalMovement yesterday today)
        |> Seq.fold (fun (plus, minus) element ->
            if element > 0.0 then
                (plus + element, minus)
            else
                (plus, minus + element * -1.0)) (0.0, 0.0)

    (dmPlus / trueRange, dmMinus / trueRange)

let rec private findBullTrendEndBefore range quotes changed =
    if isExhausted range then
        None
    else
        let rangeStart, rangeEnd = range
        let dmPlusIndicator, dmMinusIndicator = directionalMovementIndicator (rangeEnd - DmSeekSize) quotes
        if dmPlusIndicator > dmMinusIndicator then
            findBullTrendEndBefore (rangeStart, rangeEnd - 1) quotes 0
        else if changed < DmChangeCount then
            findBullTrendEndBefore (rangeStart, rangeEnd - 1) quotes (changed + 1)
        else
            Some (rangeEnd - 2)

let rec private findBearTrendEndAfter range quotes changed =
    if isExhausted range then
        None
    else
        let rangeStart, rangeEnd = range
        let dmPlusIndicator, dmMinusIndicator = directionalMovementIndicator rangeStart quotes
        if dmMinusIndicator > dmPlusIndicator then
            findBearTrendEndAfter (rangeStart + 1, rangeEnd) quotes 0
        else if changed < DmChangeCount then
            findBearTrendEndAfter (rangeStart + 1, rangeEnd) quotes (changed + 1)
        else
            Some (rangeStart + 2)
   
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
            let bearTrendEnd = findBearTrendEndAfter (max.Index, rangeEnd) quotes 0
            let bear = 
                match bearTrendEnd with
                | Some index when index < rangeEnd && index > rangeStart ->
                    findMaxesBinaryImpl (index, rangeEnd) results
                | _ ->
                    []
            let bullTrendEnd = findBullTrendEndBefore (rangeStart, max.Index) quotes 0
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

let rec private findBullTrendEndAfter range quotes changed =
    if isExhausted range then
        None
    else
        let rangeStart, rangeEnd = range
        let dmPlusIndicator, dmMinusIndicator = directionalMovementIndicator rangeStart quotes
        if dmPlusIndicator > dmMinusIndicator then
            findBullTrendEndAfter (rangeStart + 1, rangeEnd) quotes 0
        else if changed < DmChangeCount then
            findBullTrendEndAfter (rangeStart + 1, rangeEnd) quotes (changed + 1)
        else
            Some (rangeStart + 2)

let rec private findBearTrendEndBefore range quotes changed =
    if isExhausted range then
        None
    else
        let rangeStart, rangeEnd = range
        let dmPlusIndicator, dmMinusIndicator = directionalMovementIndicator (rangeEnd - DmSeekSize) quotes
        if dmMinusIndicator > dmPlusIndicator then
            findBearTrendEndBefore (rangeStart, rangeEnd - 1) quotes 0
        else if changed < DmChangeCount then
            findBearTrendEndBefore (rangeStart, rangeEnd - 1) quotes (changed + 1)
        else
            Some (rangeEnd - 2)

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
            let bullTrendEnd = findBullTrendEndAfter (min.Index, rangeEnd) quotes 0
            let bull = 
                match bullTrendEnd with
                | Some index when index < rangeEnd && index > rangeStart ->
                    findMinsBinaryImpl (index, rangeEnd) results
                | _ ->
                    []
            let bearTrendEnd = findBearTrendEndBefore (rangeStart, min.Index) quotes 0
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

    