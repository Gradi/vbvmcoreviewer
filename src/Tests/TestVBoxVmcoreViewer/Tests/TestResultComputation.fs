module TestVBoxVmcoreViewer.Tests.TestResultComputation

open FsUnit
open NUnit.Framework
open VBoxVmcoreViewer.ResultComputation

module allOrErrored =

    [<Test>]
    let ``returns all good if no errors `` () =
        [ 1..10 ]
        |> List.map Ok
        |> Seq.ofList
        |> allOrErrored
        |> should equal (Ok [ 1..10 ])

    [<Test>]
    let ``returns error if any error`` () =
        [ 1..10 ]
        |> List.map Ok
        |> List.append [ Error () ]
        |> Seq.ofList
        |> allOrErrored
        |> should equal (Error () : Result<int list, Unit>)

    [<Test>]
    let ``returns error without evaluating further`` () =
        let init (_: int) : Result<int, Unit> = failwith "Ooops"

        Seq.initInfinite init
        |> Seq.append (Seq.ofList (([ 1..10 ] |> List.map Ok) @ [ Error () ]))
        |> allOrErrored
        |> should equal (Error () : Result<int list, Unit>)

