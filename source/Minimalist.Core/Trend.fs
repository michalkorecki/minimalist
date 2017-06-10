namespace Minimalist.Core

module Trend =

    open Minimalist.Core.Indicators

    [<Literal>]
    let private DmSeekSize = 5

    [<Literal>]
    let private DmReversalsLimit = 2

    type private TrendState =
    | Continues
    | Reversing
    | Finished

    type TrendType =
        | Bear
        | Bull

    let private trendState dmReversals trend quotes = 
        let dmPlus, dmMinus = directionalMovementIndicator quotes
        match trend with
        | Bear when dmMinus > dmPlus ->
            Continues
        | Bull when dmPlus > dmMinus ->
            Continues
        | _ when dmReversals < DmReversalsLimit -> 
            Reversing
        | _ ->
            Finished

    let private isExhausted (rangeStart, rangeEnd) =
        rangeStart + DmSeekSize > rangeEnd

    let findReversalForward trend range quotes =
        let rec findReversalForwardImpl range quotes reversals =
            if isExhausted range then
                None
            else
                let rangeStart, rangeEnd = range
                let state =
                    quotes
                    |> Seq.skip rangeStart
                    |> Seq.take (DmSeekSize + 1)
                    |> trendState reversals trend
                match state with
                | Continues -> 
                    findReversalForwardImpl (rangeStart + 1, rangeEnd) quotes 0
                | Reversing ->
                    findReversalForwardImpl (rangeStart + 1, rangeEnd) quotes (reversals + 1)
                | Finished ->
                    Some (rangeStart + 2)
    
        findReversalForwardImpl range quotes 0

    let findReversalBackwards trend range quotes =
        let rec findReversalBackwardsImpl range quotes reversals =
            if isExhausted range then
                None
            else
                let rangeStart, rangeEnd = range
                let state =
                    quotes
                    |> Seq.skip (rangeEnd - DmSeekSize)
                    |> Seq.take (DmSeekSize + 1)
                    |> trendState reversals trend
                match state with
                | Continues ->
                    findReversalBackwardsImpl (rangeStart, rangeEnd - 1) quotes 0
                | Reversing ->
                    findReversalBackwardsImpl (rangeStart, rangeEnd - 1) quotes (reversals + 1)
                | Finished ->
                    Some (rangeEnd - 2)
    
        findReversalBackwardsImpl range quotes 0
