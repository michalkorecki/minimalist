module Minimalist.Detector

open Minimalist.Data
open Minimalist.Indicators
open System
open System.IO
open System.Diagnostics


let private dmSeekSize = 5

let rec private findBullDirectionalMove rangeStart rangeEnd quotes =
    if rangeStart + dmSeekSize > rangeEnd then
        None
    else
        let pairs =
            quotes 
            |> Seq.skip (rangeEnd - dmSeekSize)
            |> Seq.take (dmSeekSize + 1)
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
            findBullDirectionalMove rangeStart (rangeEnd - 1) quotes
        else
            Some (rangeEnd - 2)

let rec private findBearDirectionalMove rangeStart rangeEnd quotes =
    if rangeStart + dmSeekSize > rangeEnd then
        None
    else
        let pairs =
            quotes 
            |> Seq.skip rangeStart
            |> Seq.take (dmSeekSize + 1)
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
            findBearDirectionalMove (rangeStart + 1) rangeEnd quotes
        else
            Some (rangeStart + 2)

let private findMaxesBinary (quotes : Quotation[]) =
    let rec findMaxesBinaryImpl range (results : list<Quotation>)=
        let rangeStart, rangeEnd = range
        if rangeStart >= rangeEnd then
            results
        else
            let max =
                quotes
                |> Seq.skip rangeStart
                |> Seq.take (rangeEnd - rangeStart + 1)
                |> Seq.maxBy (fun q -> q.High)
            Debug.WriteLine( (sprintf "%O MAX (%O - %O)" max.Date quotes.[rangeStart].Date quotes.[rangeEnd].Date))
            Debug.WriteLine( (sprintf "%O Calling pivot BEAR" max.Date))
            let dmPivotBear = findBearDirectionalMove max.Index rangeEnd quotes
            let bear = 
                match dmPivotBear with
                | Some index when index < rangeEnd && index > rangeStart ->
                    Debug.WriteLine( (sprintf "%O Got pivot BEAR: %O" max.Date quotes.[index].Date))
                    findMaxesBinaryImpl (index, rangeEnd) results
                | _ ->
                    []
            Debug.WriteLine( (sprintf "%O Calling pivot BULL" max.Date))
            let dmPivotBull = findBullDirectionalMove rangeStart max.Index quotes
            let bull =
                match dmPivotBull with
                | Some index when index < rangeEnd && index > rangeStart ->
                    Debug.WriteLine( (sprintf "%O Got pivot BULL: %O" max.Date quotes.[index].Date))
                    findMaxesBinaryImpl (rangeStart, index) results
                | _ ->
                    []

            List.concat [max::bull;bear]

    findMaxesBinaryImpl (0, quotes.Length - 1) []
    |> Seq.distinct
    |> Seq.sortBy (fun q -> q.Date)
    |> Seq.toList


let findMaxes (fetchContentLines : unit -> string[]) =
    fetchContentLines()
    |> Seq.mapi parse
    |> Seq.toArray
    |> findMaxesBinary