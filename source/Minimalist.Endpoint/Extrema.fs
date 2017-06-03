module Minimalist.Endpoint.Extrema

open Nancy

type ExtremaModule() as self =
    inherit NancyModule()
    do
        self.Get.[@"extrema/(?i)(?<ticker>[A-Z0-9\.]{2,})/(?<year>[0-9]{4})"] <- fun parameters ->
            let ticker = (parameters :?> Nancy.DynamicDictionary).["ticker"]
            let year = (parameters :?> Nancy.DynamicDictionary).["year"]

            let result = (ticker, year)
            let response = self.Response.AsJson([result])
            
            response :> obj

