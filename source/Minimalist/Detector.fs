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
            let maxInRange =
                quotes 
                |> Seq.skip rangeStartIndex
                |> Seq.take (rangeEndIndex - rangeStartIndex)
                |> Seq.maxBy (fun q -> q.High)

            // at least 3 data points are needed to compute directional movement
            let notEnoughDataPoints = maxInRange.Index <= rangeStartIndex + 6
            if notEnoughDataPoints then
                maxInRange::results
            else
                let rec directionalMovementChange rangeStart rangeEnd = 
                    if rangeStart + 6 > rangeEnd then
                        rangeEnd
                    else if rangeEnd <= rangeStart then
                        rangeEnd
                    else
                        let q1 = quotes.[rangeEnd]
                        let q2 = quotes.[rangeEnd - 1]
                        let q3 = quotes.[rangeEnd - 2]
                        let q4 = quotes.[rangeEnd - 3]
                        let q5 = quotes.[rangeEnd - 4]
                        let q6 = quotes.[rangeEnd - 5]
                        let tr1 = trueRange q2 q1
                        let tr2 = trueRange q3 q2
                        let tr3 = trueRange q4 q3
                        let tr4 = trueRange q5 q4
                        let tr5 = trueRange q6 q5
                        let tr = tr1 + tr2 + tr3 + tr4 + tr5
                        let dms =
                            [(q6,q5);(q5,q4);(q4,q3);(q3,q2);(q2,q1)]
                            |> Seq.map (fun (yesterday, today) -> directionalMovement yesterday today)
                        let dmPlus = dms |> Seq.filter (fun dm -> dm > 0.0) |> Seq.sum
                        let dmMinus = dms |> Seq.filter (fun dm -> dm < 0.0) |> Seq.sum
                        let dmiPlus = dmPlus / tr
                        let dmiMinus = (dmMinus * -1.0) / tr
                        if dmiPlus > dmiMinus then
                            directionalMovementChange rangeStart (rangeEnd - 1)
                        else
                            rangeEnd
                            
                let movementChangePoint = directionalMovementChange rangeStartIndex maxInRange.Index
                if movementChangePoint > rangeStartIndex then
                    findMaxInRange (rangeStartIndex, movementChangePoint) (maxInRange::results)
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