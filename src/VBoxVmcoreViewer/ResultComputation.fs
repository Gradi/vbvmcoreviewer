module VBoxVmcoreViewer.ResultComputation

type ResultBuilder () =
    member _.Bind(comp, func) = Result.bind func comp
    member _.Return(comp) = Result.Ok comp
    member _.ReturnFrom(comp) = comp
    member _.Zero() = Ok ()
    member _.Delay(func) = Ok func
    member _.Combine(comp, comp2) =
        match comp() with
        | Error value -> Error value
        | Ok _ -> comp2



let hopefully = ResultBuilder ()
