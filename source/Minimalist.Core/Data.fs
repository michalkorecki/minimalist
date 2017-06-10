module Minimalist.Core.Data

//todo: cleanup this

open System
open System.IO

type ExtremumType =
    | Minimum
    | Maximum

type Extremum = {
    Type : ExtremumType
    Value : double;
    Date : DateTime;
}

type Quotation = {
    Open : double;
    High : double;
    Low : double;
    Close : double;
    Date : DateTime;
    Index : int;
}


let parse index (line : string) = 
    let tokens = line.Split(',')
    let parseDoubleAt i = Double.Parse(tokens.[i], System.Globalization.CultureInfo.InvariantCulture) 
    let date = DateTime.ParseExact(tokens.[0], "yyyyMMdd", null)
    let o = parseDoubleAt 1
    let h = parseDoubleAt 2
    let l = parseDoubleAt 3
    let c = parseDoubleAt 4

    { Open = o; High = h; Low = l; Close = c; Date = date; Index = index }

let loadQuotationsFromFile (rangeStart, rangeEnd) file =
    try
        use fileStream = File.OpenRead(file)
        use reader = new StreamReader(fileStream)
        let quotations =
            reader
                .ReadToEnd()
                .Split([|Environment.NewLine|], StringSplitOptions.RemoveEmptyEntries)
            |> Seq.skip 1
            |> Seq.mapi parse
            |> Seq.filter (fun q -> q.Date >= rangeStart && q.Date <= rangeEnd)
            |> Seq.mapi (fun index q -> { q with Index = index })
            |> Seq.toArray
        Some quotations
    with
        //todo: log error
        | _ -> None