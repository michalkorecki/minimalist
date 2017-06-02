open Minimalist.Endpoint.Host
open Topshelf

[<EntryPoint>]
let main argv = 
    HostFactory.Run(fun (configuration : HostConfigurators.HostConfigurator) ->
        configuration.Service<MinimalistHost>(fun (service : ServiceConfigurators.ServiceConfigurator<MinimalistHost>) ->
            service.ConstructUsing(fun (name : string) -> new MinimalistHost()) |> ignore
            service.WhenStarted(fun host -> host.Start()) |> ignore
            service.WhenStopped(fun host -> host.Stop()) |> ignore
        ) |> ignore
        configuration.RunAsLocalSystem() |> ignore
        configuration.SetDescription("Exposes extrema finding algorithm through REST API") |> ignore
        configuration.SetDisplayName("Minimalist.Endpoint") |> ignore
        configuration.SetServiceName("Minimalist.Endpoint") |> ignore
    ) |> ignore

    0
