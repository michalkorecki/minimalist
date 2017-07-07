module Minimalist.Endpoint.ExtremaModule

open Nancy
open Minimalist.Core.Data
open Minimalist.Core.ExtremaAdapter
open System.Configuration
open System

let private parseParameter (parameters : obj) name converter =
    let raw = (parameters :?> Nancy.DynamicDictionary).[name]
    let text = raw.ToString()
    converter text


type ExtremaModule() as self =
    inherit NancyModule()
    do
        self.Get.[@"extrema/(?i)(?<ticker>[A-Z0-9\.]{2,})/(?<year>[0-9]{4})"] <- fun parameters ->
            self.getExtrema parameters self.fromYearRange
        self.Get.[@"extrema/(?i)(?<ticker>[A-Z0-9\.]{2,})/(?i)(?<range>3-months|6-months|year|all)"] <- fun parameters ->
            self.getExtrema parameters self.fromPredefinedRange

    member private self.fromYearRange parameters =
        let year = parseParameter parameters "year" System.Int32.Parse
        (new DateTime(year, 1, 1), new DateTime(year, 12, 31))

    member private self.fromPredefinedRange parameters =
        let range = parseParameter parameters "range" (fun s -> s.ToLower())
        let rangeEnd = DateTime.Now
        let rangeStart = 
            match range with
            | "3-months" -> rangeEnd.AddMonths(-3)
            | "6-months" -> rangeEnd.AddMonths(-6)
            | "year" -> rangeEnd.AddYears(-1)
            | "all" -> new DateTime(1900, 1, 1)
            | _ -> rangeEnd.AddYears(-1)
        (rangeStart, rangeEnd)

    member private self.getExtrema parameters createDateRange =
        let storage = ConfigurationManager.AppSettings.["Storage"]
        let ticker = parseParameter parameters "ticker" id
        let dateRange = createDateRange parameters

        let toDictionary extremum =
            dict [
                "type", extremum.Type.ToString();
                "value", extremum.Value.ToString();
                "date", extremum.Date.ToString()]

        let response =
            findExtrema ticker dateRange storage
            |> function
                | Some extrema ->
                    extrema 
                    |> Seq.map toDictionary
                    |> Seq.toArray
                    |> self.Response.AsJson                        
                | None ->
                    let response = new Response()
                    response.StatusCode <- HttpStatusCode.NotFound
                    response.ReasonPhrase <- sprintf "Resource not found: %s" ticker
                    response
            
        response :> obj
