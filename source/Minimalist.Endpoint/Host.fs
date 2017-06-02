module Minimalist.Endpoint.Host

open Nancy
open Nancy.Hosting.Self
open System

type MinimalistHost () =
    let mutable host = null
    
    member this.Start () =
        let configuration = new HostConfiguration()
        configuration.UrlReservations.CreateAutomatically <- true
        host <- new NancyHost(new Uri("http://localhost:5702"), new DefaultNancyBootstrapper(), configuration)
        host.Start()
    
    member this.Stop() =
        host.Stop()
        host.Dispose()
        host <- null
    