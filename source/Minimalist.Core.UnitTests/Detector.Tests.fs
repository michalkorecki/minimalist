module Minimalist.Core.UnitTests.Detector.Tests

open NUnit.Framework
open FsUnit
open System
open System.IO
open System.Reflection
open Minimalist.Core.Data
open Minimalist.Core.Detector

let loadQuotations ticker =
    let file = sprintf "%s.2016.txt" ticker
    let assembly = Assembly.GetExecutingAssembly()
    use resourceStream = assembly.GetManifestResourceStream(file)
    use reader = new StreamReader(resourceStream)
    let lines =
        reader
            .ReadToEnd()
            .Split([|Environment.NewLine|], StringSplitOptions.RemoveEmptyEntries)
    lines
    |> Seq.mapi parse
    |> Seq.toArray

let shouldContainMin extrema =
    extrema |> Seq.choose (fun e -> match e with | Min q -> Some q | _ -> None)

let shouldContainMax extrema =
    extrema |> Seq.choose (fun e -> match e with | Max q -> Some q | _ -> None)

let at year month day quotations =
    quotations |> Seq.map (fun q -> q.Date) |> should contain (new DateTime(year, month, day))

[<Test>]
let ``Extrema are found for 11B in strong bull trend`` () =
    let extrema = 
        "11b"
        |> loadQuotations
        |> findExtrema

    extrema |> Seq.toArray |> should haveLength 22
    extrema |> shouldContainMin |> at 2016 01 04
    extrema |> shouldContainMax |> at 2016 01 29
    extrema |> shouldContainMin |> at 2016 02 17
    extrema |> shouldContainMax |> at 2016 03 21
    extrema |> shouldContainMin |> at 2016 04 14 //todo: questionable
    extrema |> shouldContainMax |> at 2016 04 21 //todo: questionable
    extrema |> shouldContainMin |> at 2016 05 13
    extrema |> shouldContainMax |> at 2016 05 25
    extrema |> shouldContainMin |> at 2016 06 24
    extrema |> shouldContainMax |> at 2016 07 26
    extrema |> shouldContainMin |> at 2016 08 04
    extrema |> shouldContainMax |> at 2016 08 23
    extrema |> shouldContainMin |> at 2016 08 30
    extrema |> shouldContainMax |> at 2016 09 15
    extrema |> shouldContainMin |> at 2016 09 27
    extrema |> shouldContainMax |> at 2016 10 03
    extrema |> shouldContainMin |> at 2016 10 18
    extrema |> shouldContainMax |> at 2016 10 24
    extrema |> shouldContainMin |> at 2016 11 09
    extrema |> shouldContainMax |> at 2016 12 14
    extrema |> shouldContainMin |> at 2016 12 20
    extrema |> shouldContainMax |> at 2016 12 29

[<Test>]
let ``Extrema are found for PGN in volatile sideways trend`` () =
    let extrema = 
        "pgn"
        |> loadQuotations
        |> findExtrema

    extrema |> Seq.toArray |> should haveLength 23
    extrema |> shouldContainMax |> at 2016 01 04
    extrema |> shouldContainMin |> at 2016 01 18
    extrema |> shouldContainMax |> at 2016 01 29
    extrema |> shouldContainMin |> at 2016 02 11
    extrema |> shouldContainMax |> at 2016 02 18
    extrema |> shouldContainMin |> at 2016 03 04
    extrema |> shouldContainMax |> at 2016 03 31
    extrema |> shouldContainMin |> at 2016 04 11
    extrema |> shouldContainMax |> at 2016 04 22
    extrema |> shouldContainMin |> at 2016 05 06
    extrema |> shouldContainMax |> at 2016 05 25
    extrema |> shouldContainMin |> at 2016 06 24
    extrema |> shouldContainMax |> at 2016 07 05 //todo: questionable
    extrema |> shouldContainMin |> at 2016 07 06 //todo: questionable
    extrema |> shouldContainMax |> at 2016 07 20
    extrema |> shouldContainMin |> at 2016 08 01
    extrema |> shouldContainMax |> at 2016 08 11
    extrema |> shouldContainMin |> at 2016 09 12
    extrema |> shouldContainMax |> at 2016 09 22
    extrema |> shouldContainMin |> at 2016 09 26 //todo: questionable
    extrema |> shouldContainMax |> at 2016 10 12
    extrema |> shouldContainMin |> at 2016 11 18
    extrema |> shouldContainMax |> at 2016 12 21

[<Test>]
let ``Extrema are found for WWL in strong bear trend`` () =
    let extrema = 
        "wwl"
        |> loadQuotations
        |> findExtrema

    extrema |> shouldContainMax |> at 2016 01 04
    extrema |> shouldContainMin |> at 2016 01 22
    extrema |> shouldContainMax |> at 2016 02 09
    extrema |> shouldContainMin |> at 2016 02 26
    extrema |> shouldContainMax |> at 2016 03 02
    extrema |> shouldContainMin |> at 2016 03 31
    extrema |> shouldContainMax |> at 2016 04 25
    extrema |> shouldContainMin |> at 2016 05 18
    extrema |> shouldContainMax |> at 2016 05 30
    extrema |> shouldContainMin |> at 2016 06 02 //todo: questionable
    extrema |> shouldContainMax |> at 2016 06 06 //todo: questionable
    extrema |> shouldContainMin |> at 2016 06 15
    extrema |> shouldContainMax |> at 2016 06 23
    extrema |> shouldContainMin |> at 2016 07 12
    extrema |> shouldContainMax |> at 2016 07 19
    extrema |> shouldContainMin |> at 2016 08 01
    extrema |> shouldContainMax |> at 2016 08 24 //todo: questionable
    extrema |> shouldContainMin |> at 2016 08 31
    extrema |> shouldContainMax |> at 2016 09 06
    extrema |> shouldContainMin |> at 2016 09 30 //todo: this is interesting
    extrema |> shouldContainMax |> at 2016 09 30 //todo: this is interesting
    extrema |> shouldContainMin |> at 2016 10 13
    extrema |> shouldContainMax |> at 2016 12 30