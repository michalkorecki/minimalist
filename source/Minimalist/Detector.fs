module Minimalist.Detector

open Minimalist.Data
open Minimalist.Indicators
open System
open System.IO


let private findMaxesImpl (quotes : Quotation[]) =
    let rec findMaxInbetween range (results : list<Quotation>) =
        let rangeStartIndex, rangeEndIndex = range
        let isExhausted = (rangeEndIndex - rangeStartIndex) < 1
        if isExhausted then
            results
        else
            let isRangeExhausted currentStart currentEnd =
                currentStart + 5 > currentEnd
            if isRangeExhausted rangeStartIndex rangeEndIndex then
                results
            else
                let rec directionalMovementChange2 rangeStart rangeEnd =
                    if rangeStart + 5 > rangeEnd then
                        None
                    else
                        let pairs =
                            quotes 
                            |> Seq.skip rangeStart
                            |> Seq.take 6
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

                        let dmPlusIndicator = dmPlus / trueRange
                        let dmMinusIndicator = dmMinus / trueRange
                        if dmMinusIndicator > dmPlusIndicator then
                            directionalMovementChange2 (rangeStart + 1) rangeEnd
                        else
                            Some rangeStart

                let movementChangePoint = directionalMovementChange2 rangeStartIndex rangeEndIndex
                match movementChangePoint with
                | Some idx when idx < rangeEndIndex ->
                    let maxInBetween = 
                        quotes
                        |> Seq.skip idx
                        |> Seq.take (rangeEndIndex - idx + 1)
                        |> Seq.maxBy (fun q -> q.High)
                    findMaxInbetween (maxInBetween.Index, idx) (maxInBetween::results)
                | _ -> results

    let rec findMaxInRange range (results : list<Quotation * int>) =
        let rangeStartIndex, rangeEndIndex = range
        let isExhausted = (rangeEndIndex - rangeStartIndex) < 1
        if isExhausted then
            results
        else
            let isRangeExhausted currentStart currentEnd =
                currentStart + 5 > currentEnd
            let maxInRange =
                quotes 
                |> Seq.skip rangeStartIndex
                |> Seq.take (rangeEndIndex - rangeStartIndex + 1)
                |> Seq.maxBy (fun q -> q.High)

            if isRangeExhausted rangeStartIndex maxInRange.Index then
                (maxInRange, maxInRange.Index)::results
            else
                let rec directionalMovementChange rangeStart rangeEnd =
                    //todo: fix recursion
                    if isRangeExhausted rangeStart rangeEnd then
                        None
                    else
                        let pairs =
                            quotes 
                            |> Seq.skip (rangeEnd - 5)
                            |> Seq.take 6
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

                        let dmPlusIndicator = dmPlus / trueRange
                        let dmMinusIndicator = dmMinus / trueRange
                        if dmPlusIndicator > dmMinusIndicator then
                            directionalMovementChange rangeStart (rangeEnd - 1)
                        else
                            Some rangeEnd
                            
                let movementChangePoint = directionalMovementChange rangeStartIndex maxInRange.Index
                match movementChangePoint with
                | Some idx when idx > rangeStartIndex ->
                    findMaxInRange (rangeStartIndex, idx) ((maxInRange, idx)::results)
                | _ -> (maxInRange, maxInRange.Index)::results

    let maxes = findMaxInRange (0, (quotes.Length - 1)) []
    let maxesInBetween =
        maxes
        |> Seq.pairwise
        |> Seq.map (fun ((rangeStart, _), (rangeEnd, directionChangeIndex)) -> findMaxInbetween (rangeStart.Index, directionChangeIndex) [])
        |> Seq.collect id
        |> Seq.toList

    let initialMaxes =
        maxes
        |> Seq.map (fun (q, _) -> q)
        |> Seq.toList

    List.concat [initialMaxes;maxesInBetween]
        |> Seq.sortBy (fun q -> q.Date)
        |> Seq.distinct
        |> Seq.toList

let findMaxes (fetchContentLines : unit -> string[]) =
    fetchContentLines()
    |> Seq.mapi parse
    |> Seq.toArray
    |> findMaxesImpl