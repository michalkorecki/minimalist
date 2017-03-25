module Minimalist.Indicators

open Minimalist.Data

let trueRange yesterday today =
    let guaranteedMin = 0.01
    let todayHighLow = today.High - today.Low + guaranteedMin
    let todayHighYesterdayClose = today.High - yesterday.Close + guaranteedMin
    let yesterdayCloseTodayLow = yesterday.Close - today.Low + guaranteedMin

    [todayHighLow;todayHighYesterdayClose;yesterdayCloseTodayLow] |> Seq.max

let directionalMovement yesterday today =
    let highsDistance = today.High - yesterday.High
    let lowsDistance = yesterday.Low - today.Low
    let max = max highsDistance lowsDistance

    if max < 0.0 then
        0.0
    else
        max
    