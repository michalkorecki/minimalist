module Minimalist.UnitTests.Detector.Tests

open NUnit.Framework
open FsUnit
open System
open System.IO
open System.Reflection
open Minimalist.Data
open Minimalist.Detector

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

let filterMaxes extrema =
    extrema
    |> Seq.choose (fun e -> match e with | Max v -> Some v | _ -> None)
    |> Seq.toList
    
let filterMins extrema =
    extrema
    |> Seq.choose (fun e -> match e with | Min v -> Some v | _ -> None)
    |> Seq.toList

let shouldOccurAt year month day quotation =
    quotation.Date |> should equal (new DateTime(year, month, day))

let shouldContain year month day quotations =
    quotations |> Seq.map (fun q -> q.Date) |> should contain (new DateTime(year, month, day))

let shouldNotContain year month day quotations =
    quotations |> Seq.map (fun q -> q.Date) |> should not' (contain (new DateTime(year, month, day)))


[<Test>]
let ``Maxes are found for 11B quotations (core detection)`` () =
    let maxes = 
        "11b"
        |> loadQuotations
        |> findExtrema
        |> filterMaxes

    maxes |> shouldContain 2016 01 13
    maxes |> shouldContain 2016 01 29
    maxes |> shouldContain 2016 03 21
    maxes |> shouldContain 2016 05 25
    maxes |> shouldContain 2016 07 26
    maxes |> shouldContain 2016 08 23
    maxes |> shouldContain 2016 09 15
    maxes |> shouldContain 2016 12 14

[<Test>]
let ``Mins are found for 11B quotations (core detection)`` () =
    let mins = 
        "11b"
        |> loadQuotations
        |> findExtrema
        |> filterMins

    mins |> shouldContain 2016 01 04
    mins |> shouldContain 2016 02 17
    mins |> shouldContain 2016 05 13
    mins |> shouldContain 2016 06 24
    mins |> shouldContain 2016 08 04
    mins |> shouldContain 2016 08 30
    mins |> shouldContain 2016 09 27
    mins |> shouldContain 2016 11 09
    mins |> shouldContain 2016 12 20

[<Test>]
let ``11B maxes detection does not return definitely wrong maxes`` () =
    let maxes = 
        "11b"
        |> loadQuotations
        |> findExtrema
        |> filterMaxes

    maxes |> shouldNotContain 2016 02 26
    maxes |> shouldNotContain 2016 03 31
    maxes |> shouldNotContain 2016 04 08
    maxes |> shouldNotContain 2016 04 29
    maxes |> shouldNotContain 2016 06 03
    maxes |> shouldNotContain 2016 07 26
    maxes |> shouldNotContain 2016 10 13
    maxes |> shouldNotContain 2016 10 31
    maxes |> shouldNotContain 2016 11 10

[<Test>]
let ``Maxes are found for 11B quotations (precise detection)`` () =
    let maxes = 
        "11b"
        |> loadQuotations
        |> findExtrema
        |> filterMaxes

    maxes |> should haveLength 8
    maxes.[0] |> shouldOccurAt 2016 01 13
    maxes.[1] |> shouldOccurAt 2016 01 29
    maxes.[2] |> shouldOccurAt 2016 03 21
    maxes.[3] |> shouldOccurAt 2016 05 25
    maxes.[4] |> shouldOccurAt 2016 07 26
    maxes.[5] |> shouldOccurAt 2016 08 23
    maxes.[6] |> shouldOccurAt 2016 09 15
    maxes.[7] |> shouldOccurAt 2016 12 14

[<Test>]
let ``Maxes are found for PGN quotations (precise detection)`` () =
    let maxes = 
        "pgn"
        |> loadQuotations
        |> findExtrema
        |> filterMaxes

    maxes |> should haveLength 6
    maxes.[0] |> shouldOccurAt 2016 01 04
    maxes.[1] |> shouldOccurAt 2016 02 18
    maxes.[2] |> shouldOccurAt 2016 03 31
    maxes.[3] |> shouldOccurAt 2016 05 25
    maxes.[4] |> shouldOccurAt 2016 07 20
    maxes.[5] |> shouldOccurAt 2016 12 21

[<Test>]
let ``Maxes are found for WWL quotations (precise detection)`` () =
    let maxes =
        "wwl"
        |> loadQuotations
        |> findExtrema
        |> filterMaxes

    maxes |> should haveLength 5
    maxes.[0] |> shouldOccurAt 2016 01 04
    maxes.[1] |> shouldOccurAt 2016 02 09
    maxes.[2] |> shouldOccurAt 2016 04 25
    maxes.[3] |> shouldOccurAt 2016 11 10
    maxes.[4] |> shouldOccurAt 2016 12 30
