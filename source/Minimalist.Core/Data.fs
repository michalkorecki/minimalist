module Minimalist.Core.Data

open System

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