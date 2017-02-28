module Minimalist.UnitTests.Detector.Tests

open NUnit.Framework
open FsUnit
open System
open System.IO
open System.Reflection
open Minimalist.Detector

let loadTestData () =
    let assembly = Assembly.GetExecutingAssembly()
    use resourceStream = assembly.GetManifestResourceStream("11b.dat")
    use reader = new StreamReader(resourceStream)
    reader
        .ReadToEnd()
        .Split([|Environment.NewLine|], StringSplitOptions.RemoveEmptyEntries)

let shouldOccurAt year month day quotation =
    quotation.Date |> should equal (new DateTime(year, month, day))


[<Test>]
let ``Maxes are found for 11B quotations (algorithm basic idea demonstration)`` () =
    let maxes = findMaxes loadTestData

    maxes |> should haveLength 11
    maxes.[0] |> shouldOccurAt 2010 11 19
    maxes.[1] |> shouldOccurAt 2011 03 29
    maxes.[2] |> shouldOccurAt 2014 12 15
    maxes.[3] |> shouldOccurAt 2015 02 13
    maxes.[4] |> shouldOccurAt 2015 07 20
    maxes.[5] |> shouldOccurAt 2016 01 29
    maxes.[6] |> shouldOccurAt 2016 08 11
    maxes.[7] |> shouldOccurAt 2016 09 15
    maxes.[8] |> shouldOccurAt 2016 11 28
    maxes.[9] |> shouldOccurAt 2017 01 09
    maxes.[10] |> shouldOccurAt 2017 02 22