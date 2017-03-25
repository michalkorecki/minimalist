module Minimalist.Indicators

open Minimalist.Data

let trueRange yesterday today =
    let guaranteedMin = 0.01
    let todayHighLow = today.High - today.Low + guaranteedMin
    let todayHighYesterdayClose = today.High - yesterday.Close + guaranteedMin
    let yesterdayCloseTodayLow = yesterday.Close - today.Low + guaranteedMin

    [todayHighLow;todayHighYesterdayClose;yesterdayCloseTodayLow] |> Seq.max

