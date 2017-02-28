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


[<Test>]
let ``Maxes are found for 11B quotations (algorithm basic idea demonstration)`` () =
    let data = loadTestData ()
    let maxes = findMaxes loadTestData

    maxes |> should haveCount 12