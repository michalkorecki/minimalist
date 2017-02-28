module Minimalist.Detector

open System
open System.IO

type Quotation = {
    High : double;
    Date : DateTime;
    Index : int;
}

let private toQuotation (index : int) (line : string) =
    let tokens = line.Split(',')
    let high = Double.Parse(tokens.[2], System.Globalization.CultureInfo.InvariantCulture)
    let date = DateTime.ParseExact(tokens.[0], "yyyyMMdd", null)
    { High = high; Date = date; Index = index }

let private buildQuotations fetchContentLines =
    fetchContentLines()
    |> Seq.mapi toQuotation
    |> Seq.toArray

let private find quotes =
    let rec findMax boundary (results : list<Quotation>) neighbourhoodSize =
        let minIndex, maxIndex, unsuccessfulAttempts = boundary
        let searchSpaceTooNarrow = (maxIndex - minIndex) < neighbourhoodSize
        if searchSpaceTooNarrow then
            results
        else
            let max = 
                quotes 
                |> Seq.skip minIndex
                |> Seq.take (maxIndex - minIndex)
                |> Seq.maxBy (fun q -> q.High)

            let shirnkageFactor = if unsuccessfulAttempts = 0 then 1 else unsuccessfulAttempts * unsuccessfulAttempts
            let shrinkingNeighbourhoodSize = neighbourhoodSize / shirnkageFactor
            if max.Index > (maxIndex - shrinkingNeighbourhoodSize) then
                let discardedNeighbourhoodSize = shrinkingNeighbourhoodSize / 2
                let nextSearchBoundary = maxIndex - discardedNeighbourhoodSize
                findMax (minIndex, nextSearchBoundary, unsuccessfulAttempts + 1) results neighbourhoodSize
            else
                findMax (minIndex, max.Index, 0) (max::results) neighbourhoodSize

    let max = quotes |> Seq.maxBy (fun q -> q.High)
    let initialMaxes = findMax (0, max.Index, 0) [max] 20
    
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
            let maxes = findMax (start.Index + 20, finish.Index, 0) [] 10
            (start, finish, maxes))

    initialMaxes

let findMaxes (fetchContentLines : unit -> string[]) =
    fetchContentLines
    |> buildQuotations
    |> find


//12,3 19.11.2010 00:00:00 14 
//26,49 29.03.2011 00:00:00 104 
//78,8 15.12.2014 00:00:00 1025 
//82,82 13.02.2015 00:00:00 1063 
//83 20.07.2015 00:00:00 1170 
//88,45 29.01.2016 00:00:00 1303 
//101,95 11.08.2016 00:00:00 1438 
//132,8 15.09.2016 00:00:00 1462 
//136 28.11.2016 00:00:00 1512 
//154 09.01.2017 00:00:00 1540 
//168 14.02.2017 00:00:00 1566