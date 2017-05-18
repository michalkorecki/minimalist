﻿module Minimalist.Detector

open Minimalist.Data
open Minimalist.Indicators
open System
open System.IO
open System.Diagnostics


let private dmSeekSize = 5

let isExhausted (rangeStart, rangeEnd) =
    rangeStart + dmSeekSize > rangeEnd

let private directionalMovementIndicator skippedCount quotes =
    let pairs =
        quotes
        |> Seq.skip skippedCount
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

    (dmPlus / trueRange, dmMinus / trueRange)

let rec private findBullTrendEnd range quotes =
    if isExhausted range then
        None
    else
        let rangeStart, rangeEnd = range
        let dmPlusIndicator, dmMinusIndicator = directionalMovementIndicator (rangeEnd - dmSeekSize) quotes
        if dmPlusIndicator > dmMinusIndicator then
            findBullTrendEnd (rangeStart, rangeEnd - 1) quotes
        else
            Some (rangeEnd - 2)

let rec private findBearTrendEnd range quotes =
    if isExhausted range then
        None
    else
        let rangeStart, rangeEnd = range
        let dmPlusIndicator, dmMinusIndicator = directionalMovementIndicator rangeStart quotes
        if dmMinusIndicator > dmPlusIndicator then
            findBearTrendEnd (rangeStart + 1, rangeEnd) quotes
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
                |> Seq.skip rangeStart
                |> Seq.take (rangeEnd - rangeStart + 1)
                |> Seq.maxBy (fun q -> q.High)
            let bearTrendEnd = findBearTrendEnd (max.Index, rangeEnd) quotes
            let bear = 
                match bearTrendEnd with
                | Some index when index < rangeEnd && index > rangeStart ->
                    findMaxesBinaryImpl (index, rangeEnd) results
                | _ ->
                    []
            let bullTrendEnd = findBullTrendEnd (rangeStart, max.Index) quotes
            let bull =
                match bullTrendEnd with
                | Some index when index < rangeEnd && index > rangeStart ->
                    findMaxesBinaryImpl (rangeStart, index) results
                | _ ->
                    []

            List.concat [max::bull;bear]

    findMaxesBinaryImpl (0, quotes.Length - 1) []
    |> Seq.distinct
    |> Seq.sortBy (fun q -> q.Date)
    |> Seq.toList

//todo: why not take even more direct dependency, seq<Quotation> ?
let findMaxes contentLines =
    contentLines
    |> Seq.mapi parse
    |> Seq.toArray
    |> findMaxesBinary

let findMins contentLines =
    contentLines
    |> Seq.mapi parse
    |> Seq.toList