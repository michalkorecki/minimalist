module Minimalist.Endpoint.Host

open Nancy
open Nancy.Hosting.Self
open System
open System.Configuration

type MinimalistHost () =
    let mutable host = null
    
    member this.Start () =
        let hostAddress = ConfigurationManager.AppSettings.["Host"]
        let configuration = new HostConfiguration()
        configuration.UrlReservations.CreateAutomatically <- true
        host <- new NancyHost(new Uri(hostAddress), new DefaultNancyBootstrapper(), configuration)
        host.Start()
    
    member this.Stop() =
        host.Stop()
        host.Dispose()
        host <- null
    