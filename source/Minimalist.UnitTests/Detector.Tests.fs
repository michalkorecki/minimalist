module Minimalist.UnitTests.Detector.Tests

open NUnit.Framework
open FsUnit
open System
open System.IO
open System.Reflection
open Minimalist.Data
open Minimalist.Detector

let loadTestData ticker = fun () ->
    let file = sprintf "%s.2016.txt" ticker
    let assembly = Assembly.GetExecutingAssembly()
    use resourceStream = assembly.GetManifestResourceStream(file)
    use reader = new StreamReader(resourceStream)
    reader
        .ReadToEnd()
        .Split([|Environment.NewLine|], StringSplitOptions.RemoveEmptyEntries)

let shouldOccurAt year month day quotation =
    quotation.Date |> should equal (new DateTime(year, month, day))


[<Test>]
let ``Maxes are found for 11B quotations (algorithm basic idea demonstration)`` () =
    let maxes = findMaxes (loadTestData "11b")

    maxes |> should haveLength 4
    maxes.[0] |> shouldOccurAt 2016 01 29
    maxes.[1] |> shouldOccurAt 2016 08 11
    maxes.[2] |> shouldOccurAt 2016 09 15
    maxes.[3] |> shouldOccurAt 2016 12 14

[<Test>]
let ``Maxes are found for 11B quotations (target detection)`` () =
    let maxes = findMaxes (loadTestData "11b")

    maxes |> should haveLength 7
    maxes.[0] |> shouldOccurAt 2016 01 29
    maxes.[1] |> shouldOccurAt 2016 03 21
    maxes.[2] |> shouldOccurAt 2016 05 25
    maxes.[3] |> shouldOccurAt 2016 07 26
    maxes.[4] |> shouldOccurAt 2016 08 23
    maxes.[5] |> shouldOccurAt 2016 09 15
    maxes.[6] |> shouldOccurAt 2016 12 14

[<Test>]
let ``Maxes are found for PGN quotations (target detection)`` () =
    let maxes = findMaxes (loadTestData "pgn")

    maxes |> should haveLength 6
    maxes.[0] |> shouldOccurAt 2016 01 04
    maxes.[1] |> shouldOccurAt 2016 02 18
    maxes.[2] |> shouldOccurAt 2016 03 31
    maxes.[3] |> shouldOccurAt 2016 05 25
    maxes.[4] |> shouldOccurAt 2016 07 20
    maxes.[5] |> shouldOccurAt 2016 12 21

[<Test>]
let ``Maxes are found for WWL quotations (target detection)`` () =
    let maxes = findMaxes (loadTestData "wwl")

    maxes |> should haveLength 5
    maxes.[0] |> shouldOccurAt 2016 01 04
    maxes.[1] |> shouldOccurAt 2016 02 09
    maxes.[2] |> shouldOccurAt 2016 04 25
    maxes.[3] |> shouldOccurAt 2016 11 10
    maxes.[4] |> shouldOccurAt 2016 12 30
