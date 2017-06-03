module Minimalist.Endpoint.Extrema

open Nancy
open Minimalist.Core.Extrema

type ExtremaModule() as self =
    inherit NancyModule()
    do
        //todo: refactor, it's not the greatest
        self.Get.[@"extrema/(?i)(?<ticker>[A-Z0-9\.]{2,})/(?<year>[0-9]{4})"] <- fun parameters ->
            let ticker = (parameters :?> Nancy.DynamicDictionary).["ticker"]
            let year = (parameters :?> Nancy.DynamicDictionary).["year"]

            let extrema = extrema ticker year
            let response =
                match extrema with
                | Extrema ex ->
                    let x =
                        ex 
                        |> Seq.map (fun e -> dict [
                            "type", e.Type.ToString();
                            "value", e.Value.ToString();
                            "date", e.Date.ToString()])
                        |> Seq.toArray
                    self.Response.AsJson(x)                        
                | Error message ->
                    self.Response.AsJson([message])
            
            response :> obj

