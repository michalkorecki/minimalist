﻿module Minimalist.Core.UnitTests.Indicators.Tests

open FsUnit
open Minimalist.Core.Data
open Minimalist.Core.Indicators
open NUnit.Framework
open System

let quotation h l c =
    { Open = 1.0; High = h; Low = l; Close = c; Date = DateTime.MinValue; Index = 0 }

[<Test>]
let ``True range equals to distance between today's H and L when it is the largest (outside day)`` () =
    let today = quotation 1.5 0.7 1.2
    let yesterday = quotation 1.4 0.8 1.2

    let tr = trueRange yesterday today

    tr |> should (equalWithin 0.01) 0.81

[<Test>]
let ``True range equals to distance between today's H and yesterday's C when it is the largest`` () =
    let today = quotation 1.5 1.1 1.2
    let yesterday = quotation 1.4 0.8 0.9
    
    let tr = trueRange yesterday today
    
    tr |> should (equalWithin 0.01) 0.61 

[<Test>]
let ``True range equals to distance bewteen yesterday's C and today's L when it is the largest`` () =
    let today = quotation 1.0 0.7 1.0
    let yesterday = quotation 1.3 0.8 1.2

    let tr = trueRange yesterday today

    tr |> should (equalWithin 0.01) 0.51

[<Test>]
let ``True range equals 0.01 when all of the prices are equal`` () =
    let today = quotation 1.0 1.0 1.0
    let yesterday = quotation 1.0 1.0 1.0

    let tr = trueRange yesterday today

    tr |> should (equalWithin 0.01) 0.01

[<Test>]
let ``Directional movement equals to distance between today's H and yesterday's H when it is the largest`` () =
    let today = quotation 1.5 1.0 1.2
    let yesterday = quotation 1.2 1.1 1.1

    let dm = directionalMovement yesterday today

    dm |> should (equalWithin 0.01) 0.3

[<Test>]
let ``Directional movement equals to distance between today's L and yesterday's L when it is the largest``() =
    let today = quotation 1.2 0.7 1.0
    let yesterday = quotation 1.2 0.9 1.0

    let dm = directionalMovement yesterday today

    dm |> should (equalWithin 0.01) -0.2

[<Test>]
let ``Directional movement equals 0 when today's H and L are contained (inside day)`` () =
    let today = quotation 1.2 1.0 1.0
    let yesterday = quotation 1.5 0.9 1.0

    let dm = directionalMovement yesterday today

    dm |> should (equalWithin 0.01) 0.0