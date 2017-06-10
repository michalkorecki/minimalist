namespace Minimalist.Core

module Trend =

    open Minimalist.Core.Indicators

    [<Literal>]
    let private SearchSpaceSize = 5

    [<Literal>]
    let private DmReversalsLimit = 2

    type private TrendState =
        | Continues
        | Reversing
        | Finished

    type TrendType =
        | Bear
        | Bull

    let private (|Range|_|) (rangeStart, rangeEnd) = 
        if rangeStart + SearchSpaceSize > rangeEnd then
            None
        else
            Some (rangeStart, rangeEnd)

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

    let findReversalForward trend range quotes =
        let rec findReversalForwardImpl quotes reversals = function
            | Range (rangeStart, rangeEnd) -> 
                let state =
                    quotes
                    |> Seq.skip rangeStart
                    |> Seq.take (SearchSpaceSize + 1)
                    |> trendState reversals trend
                match state with
                | Continues -> 
                    findReversalForwardImpl quotes 0 (rangeStart + 1, rangeEnd) 
                | Reversing ->
                    findReversalForwardImpl quotes (reversals + 1) (rangeStart + 1, rangeEnd)
                | Finished ->
                    Some (rangeStart + 2)
            | _ ->
                None
    
        findReversalForwardImpl quotes 0 range

    let findReversalBackwards trend range quotes =
        let rec findReversalBackwardsImpl quotes reversals = function
            | Range (rangeStart, rangeEnd) -> 
                let state =
                    quotes
                    |> Seq.skip (rangeEnd - SearchSpaceSize)
                    |> Seq.take (SearchSpaceSize + 1)
                    |> trendState reversals trend
                match state with
                | Continues ->
                    findReversalBackwardsImpl quotes 0 (rangeStart, rangeEnd - 1)
                | Reversing ->
                    findReversalBackwardsImpl quotes (reversals + 1) (rangeStart, rangeEnd - 1)
                | Finished ->
                    Some (rangeEnd - 2)
            | _ ->
                None
    
        findReversalBackwardsImpl quotes 0 range
