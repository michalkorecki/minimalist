module Minimalist.Detector

open Minimalist.Data
open Minimalist.Indicators
open System
open System.IO


let private findMaxesImpl (quotes : Quotation[]) =
    let rec findMaxInRange range (results : list<Quotation>) =
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
                |> Seq.take (rangeEndIndex - rangeStartIndex)
                |> Seq.maxBy (fun q -> q.High)

            if isRangeExhausted rangeStartIndex maxInRange.Index then
                maxInRange::results
            else
                let rec directionalMovementChange rangeStart rangeEnd =
                    //todo: fix recursion
                    if isRangeExhausted rangeStart rangeEnd then
                        rangeEnd
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
                                    (plus, minus + element)) (0.0, 0.0)

                        let dmPlusIndicator = dmPlus / trueRange
                        let dmMinusIndicator = (dmMinus * -1.0) / trueRange
                        if dmPlusIndicator > dmMinusIndicator then
                            directionalMovementChange rangeStart (rangeEnd - 1)
                        else
                            rangeEnd
                            
                let movementChangePoint = directionalMovementChange rangeStartIndex maxInRange.Index
                if movementChangePoint > rangeStartIndex then
                    findMaxInRange (rangeStartIndex, movementChangePoint) (maxInRange::results)
                //todo: is this ever called?
                else
                    maxInRange::results

    let initialMaxes = findMaxInRange (0, (quotes.Length - 1)) []
    
    // possible qualifiers/scoring
    // 1) distance between subsequent maxes
    // 2) change between subsequent maxes
    // 3) change max1 - min vs min - max 2
    // needs more work
    let nextRangesToSearch nieghbourhoodSize = 
        let rec x items foundRanges =
            match items with
            | [] -> 
                foundRanges
            | head::next::tail ->
                let minDistance = pown (nieghbourhoodSize / 2) 2
                let isSufficientlyLarge = head.Index + minDistance < next.Index
                if isSufficientlyLarge then
                    x (next::tail) ((head, next)::foundRanges)
                else
                    x (next::tail) foundRanges
            | _ -> 
                foundRanges
            
        x initialMaxes []
    
    let ranges = nextRangesToSearch 20
    let secondaryMaxes = 
        ranges
        |> Seq.map (fun (start, finish) -> 
            let maxes = findMaxInRange (start.Index + 20, finish.Index) []
            (start, finish, maxes))

    initialMaxes

let findMaxes (fetchContentLines : unit -> string[]) =
    fetchContentLines()
    |> Seq.mapi parse
    |> Seq.toArray
    |> findMaxesImpl