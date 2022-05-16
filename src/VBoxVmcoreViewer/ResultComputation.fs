module VBoxVmcoreViewer.ResultComputation

let errorf format = Error (sprintf format)

let allOrErrored (seq: Result<'a, 'b> seq) =
    use enum = seq.GetEnumerator ()

    let mutable results = []
    let mutable doWhile = true
    let mutable finalResult = Ok ()

    while doWhile && enum.MoveNext () do
        match enum.Current with
        | Ok value ->
            results <- results @ [ value ]
        | Error error ->
            doWhile <- false
            finalResult <- Error error

    match finalResult with
    | Error error -> Error error
    | Ok () -> Ok results

type ResultBuilder () =

    member _.Bind(m, f) = Result.bind f m

    member _.Return(comp) = Ok comp

    member _.ReturnFrom(comp) = comp

    member _.Zero() = Ok ()

    member _.Delay(f) = f

    member _.Combine(a: Result<unit, 'e>, b: unit -> Result<'a, 'e>) : Result<'a, 'e> =
        match a with
        | Ok () -> b ()
        | Error err -> Error err

    member _.Run(f) = f ()

let hopefully = ResultBuilder ()
