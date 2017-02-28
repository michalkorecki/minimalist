module Minimalist.UnitTests.Detector.Tests

open NUnit.Framework
open FsUnit

[<Test>]
let ``Test`` () =
    let x = 1
    let y = 2
    x |> should equal y