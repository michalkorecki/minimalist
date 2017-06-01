module Minimalist.Core.Detector

open Minimalist.Core.Data
open Minimalist.Core.Indicators
open System

type Extremum = 
    | Min of Quotation
    | Max of Quotation

[<Literal>]
let private DmSeekSize = 5

[<Literal>]
let private DmChangeCount = 2

let isExhausted (rangeStart, rangeEnd) =
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

let printDmLog key info dmPlus dmMinus =
    printfn "%s (%s): DM+=%.2f DM-=%.2f" key info dmPlus dmMinus
    

let rec private findBullTrendEndBefore range quotes changed =
    if isExhausted range then
        printfn "max-bull range exhausted"
        None
    else
        let rangeStart, rangeEnd = range
        let dmPlusIndicator, dmMinusIndicator = directionalMovementIndicator (rangeEnd - DmSeekSize) quotes
        if dmPlusIndicator > dmMinusIndicator then
            printDmLog "max-bull" "+>-" dmPlusIndicator dmMinusIndicator
            findBullTrendEndBefore (rangeStart, rangeEnd - 1) quotes 0
        else if changed < DmChangeCount then
            printDmLog "max-bull" "failed" dmPlusIndicator dmMinusIndicator
            findBullTrendEndBefore (rangeStart, rangeEnd - 1) quotes (changed + 1)
        else
            printDmLog "max-bull" "returning" dmPlusIndicator dmMinusIndicator
            Some (rangeEnd - 2)

let rec private findBearTrendEndAfter range quotes changed =
    if isExhausted range then
        printfn "max-bear range exhausted"
        None
    else
        let rangeStart, rangeEnd = range
        let dmPlusIndicator, dmMinusIndicator = directionalMovementIndicator rangeStart quotes
        if dmMinusIndicator > dmPlusIndicator then
            printDmLog "max-bear" "->+" dmPlusIndicator dmMinusIndicator
            findBearTrendEndAfter (rangeStart + 1, rangeEnd) quotes 0
        else if changed < DmChangeCount then
            printDmLog "max-bear" "failed" dmPlusIndicator dmMinusIndicator
            findBearTrendEndAfter (rangeStart + 1, rangeEnd) quotes (changed + 1)
        else
            printDmLog "max-bear" "failed" dmPlusIndicator dmMinusIndicator
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
            printfn "new-max = %A" max.Date
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
        printfn "min-bull range exhausted"
        None
    else
        let rangeStart, rangeEnd = range
        let dmPlusIndicator, dmMinusIndicator = directionalMovementIndicator rangeStart quotes
        if dmPlusIndicator > dmMinusIndicator then
            printDmLog "min-bull" "+>-" dmPlusIndicator dmMinusIndicator
            findBullTrendEndAfter (rangeStart + 1, rangeEnd) quotes 0
        else if changed < DmChangeCount then
            printDmLog "min-bull" "failed" dmPlusIndicator dmMinusIndicator
            findBullTrendEndAfter (rangeStart + 1, rangeEnd) quotes (changed + 1)
        else
            printDmLog "min-bull" "returning" dmPlusIndicator dmMinusIndicator
            Some (rangeStart + 2)

let rec private findBearTrendEndBefore range quotes changed =
    if isExhausted range then
        printfn "min-bear range exhausted"
        None
    else
        let rangeStart, rangeEnd = range
        printfn "%A" range
        let dmPlusIndicator, dmMinusIndicator = directionalMovementIndicator (rangeEnd - DmSeekSize) quotes
        if dmMinusIndicator > dmPlusIndicator then
            printDmLog "min-bear" "->+" dmPlusIndicator dmMinusIndicator 
            findBearTrendEndBefore (rangeStart, rangeEnd - 1) quotes 0
        else if changed < DmChangeCount then
            printDmLog "min-bear" "failed" dmPlusIndicator dmMinusIndicator
            findBearTrendEndBefore (rangeStart, rangeEnd - 1) quotes (changed + 1)
        else
            printDmLog "min-bear" "returning" dmPlusIndicator dmMinusIndicator
            Some (rangeEnd - 2)

let findMinsBinary (quotes : Quotation[]) =
    let rec findMinsBinaryImpl range (results : list<Quotation>) =
        let rangeStart, rangeEnd = range
        if rangeStart >= rangeEnd then
            results
        else
            let min =
                quotes
                |> narrowTo range
                |> Seq.minBy (fun q -> q.Low)
            printfn "new-min = %A" min.Date
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
    let mins = findMinsBinary quotations |> Seq.map (fun q -> Min q)
    let maxes = findMaxesBinary quotations |> Seq.map (fun q -> Max q)
    [mins;maxes]
    |> Seq.concat
    |> Seq.sortBy (fun e ->
        match e with 
        | Max q 
        | Min q -> q.Date)
    |> Seq.fold (fun acc extremum ->
            match acc, extremum with 
            | [], _ ->
                [extremum]
            | (Min head)::tail, Min current ->
                if head.Low < current.Low then
                    acc
                else
                    extremum::tail
            | (Max head)::tail, Max current ->
                if head.High > current.High then
                    acc
                else
                    extremum::tail
            | _, _ ->
                extremum::acc) []
    |> Seq.rev

    