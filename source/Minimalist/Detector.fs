module Minimalist.Detector

open Minimalist.Data
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

            // walk backwards from max and track directional movement
            // once movement direction changes, start searching for max
            // from this point onwards

            // directional movement (aka trend)
            // 1 - up
            // 0 - no trend
            //-1 - down

            // at least 3 data points are needed to compute directional movement
            let notEnoughDataPoints = maxInRange.Index <= rangeStartIndex + 3
            if notEnoughDataPoints then
                maxInRange::results
            else
                let rec directionalMovementChange rangeStart rangeEnd = 
                    if rangeEnd <= rangeStart then
                        rangeEnd
                    else
                        let m4 = quotes.[rangeEnd]
                        let m3 = quotes.[rangeEnd - 1]
                        let m2 = quotes.[rangeEnd - 2]
                        let m1 = quotes.[rangeEnd - 3]
                        let change3 = (m4.High - m3.High) / m4.High
                        let change2 = (m4.High - m2.High) / m4.High
                        let change1 = (m4.High - m1.High) / m4.High
                        let weight = 0.5
                        let average = change3 * (1.0 - weight) + change2 * (pown (1.0 - weight) 2) + change3 * (pown (1.0 - weight) 3)
                        if average < 0.0 then
                            rangeEnd
                        else
                            directionalMovementChange rangeStart (rangeEnd - 1)
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