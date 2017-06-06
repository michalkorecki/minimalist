module Minimalist.Endpoint.ExtremaModule

open Nancy
open Minimalist.Core.Data
open Minimalist.Core.ExtremaDetectorAdapter
open System.Configuration

let private parseParameter (parameters : obj) name converter =
    let raw = (parameters :?> Nancy.DynamicDictionary).[name]
    let text = raw.ToString()
    converter text

type ExtremaModule() as self =
    inherit NancyModule()
    do
        self.Get.[@"extrema/(?i)(?<ticker>[A-Z0-9\.]{2,})/(?<year>[0-9]{4})"] <- fun parameters ->
            let storage = ConfigurationManager.AppSettings.["Storage"]
            let ticker = parseParameter parameters "ticker" id
            let year = parseParameter parameters "year" System.Int32.Parse

            let toDictionary extremum =
                dict [
                    "type", extremum.Type.ToString();
                    "value", extremum.Value.ToString();
                    "date", extremum.Date.ToString()]

            let response =
                findExtrema ticker year storage
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

