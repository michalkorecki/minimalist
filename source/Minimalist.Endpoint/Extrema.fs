module Minimalist.Endpoint.Extrema

open Nancy
open Minimalist.Core.Extrema

let private parseParameter (parameters : obj) name converter =
    let raw = (parameters :?> Nancy.DynamicDictionary).[name]
    let text = raw.ToString()
    converter text

type ExtremaModule() as self =
    inherit NancyModule()
    do
        self.Get.[@"extrema/(?i)(?<ticker>[A-Z0-9\.]{2,})/(?<year>[0-9]{4})"] <- fun parameters ->
            let ticker = parseParameter parameters "ticker" id
            let year = parseParameter parameters "year" System.Int32.Parse

            let toDictionary extremum =
                dict [
                    "type", extremum.Type.ToString();
                    "value", extremum.Value.ToString();
                    "date", extremum.Date.ToString()]

            let response =
                extrema ticker year
                |> function
                    | Extrema extrema ->
                        extrema 
                        |> Seq.map toDictionary
                        |> Seq.toArray
                        |> self.Response.AsJson                        
                    | ErrorTickerNotFound ticker ->
                        let response = new Response()
                        response.StatusCode <- HttpStatusCode.NotFound
                        response.ReasonPhrase <- sprintf "Resource not found: %s" ticker
                        response
                    | Error ->
                        let response = new Response()
                        response.StatusCode <- HttpStatusCode.InternalServerError
                        response
            
            response :> obj

