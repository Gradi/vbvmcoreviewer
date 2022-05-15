module VBoxVmcoreViewer.ResultComputation

let errorf format = Result.Error (sprintf format)

type ResultBuilder () =

    member _.Bind(m, f) = Result.bind f m

    member _.Return(comp) = Result.Ok comp

    member _.ReturnFrom(comp) = comp

let hopefully = ResultBuilder ()
