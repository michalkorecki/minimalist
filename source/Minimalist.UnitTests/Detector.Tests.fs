module Minimalist.UnitTests.Detector.Tests

open NUnit.Framework
open FsUnit
open System
open System.IO
open System.Reflection
open Minimalist.Detector

let loadTestData () =
    let assembly = Assembly.GetExecutingAssembly()
    use resourceStream = assembly.GetManifestResourceStream("11b.2016.txt")
    use reader = new StreamReader(resourceStream)
    reader
        .ReadToEnd()
        .Split([|Environment.NewLine|], StringSplitOptions.RemoveEmptyEntries)

let shouldOccurAt year month day quotation =
    quotation.Date |> should equal (new DateTime(year, month, day))


[<Test>]
let ``Maxes are found for 11B quotations (algorithm basic idea demonstration)`` () =
    let maxes = findMaxes loadTestData

    maxes |> should haveLength 4
//    maxes.[0] |> shouldOccurAt 2010 11 19
//    maxes.[1] |> shouldOccurAt 2011 03 29
//    maxes.[2] |> shouldOccurAt 2014 12 15
//    maxes.[3] |> shouldOccurAt 2015 02 13
//    maxes.[4] |> shouldOccurAt 2015 07 20
    maxes.[0] |> shouldOccurAt 2016 01 29
    maxes.[1] |> shouldOccurAt 2016 08 11
    maxes.[2] |> shouldOccurAt 2016 09 15
    maxes.[3] |> shouldOccurAt 2016 12 14
//    maxes.[9] |> shouldOccurAt 2017 01 09
//    maxes.[10] |> shouldOccurAt 2017 02 22