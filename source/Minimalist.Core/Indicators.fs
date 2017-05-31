﻿module Minimalist.Core.Indicators

open Minimalist.Core.Data

let trueRange yesterday today =
    let guaranteedMin = 0.01
    let todayHighLow = today.High - today.Low + guaranteedMin
    let todayHighYesterdayClose = today.High - yesterday.Close + guaranteedMin
    let yesterdayCloseTodayLow = yesterday.Close - today.Low + guaranteedMin

    [todayHighLow;todayHighYesterdayClose;yesterdayCloseTodayLow] |> Seq.max

let directionalMovement yesterday today =
    let highsDistance = today.High - yesterday.High
    let lowsDistance = today.Low - yesterday.Low
    let isInsideDay = today.High < yesterday.High && today.Low > yesterday.Low
    if isInsideDay then
        0.0
    else
        if highsDistance > (abs lowsDistance) then
            highsDistance
        else
            lowsDistance
    