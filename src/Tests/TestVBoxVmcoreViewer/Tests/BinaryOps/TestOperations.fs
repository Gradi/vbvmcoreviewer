module TestVBoxVmcoreViewer.Tests.BinaryOps.TestOperations

open NUnit.Framework
open FsUnit
open VBoxVmcoreViewer.BinaryOps.Operations
open VBoxVmcoreViewer.BinaryOps.Types

module oppositeEndianess =

    [<Test>]
    let ``returns opposite endianess`` () =
        oppositeEndianess Endianess.Little |> should equal Endianess.Big
        oppositeEndianess Endianess.Big |> should equal Endianess.Little


module bytesto =

    let mkbytes array = { Endianess = Endianess.Little; Bytes = Array.map byte array }

    [<Test>]
    let ``fails when not enough bytes in buffer`` () =
        (fun () -> bytesto<int> (mkbytes [| 0 |]) |> ignore)
        |> should throw typeof<System.Exception>

    [<Test>]
    let ``reads 1 from little endian`` () =
        mkbytes [| 1; 0; 0; 0 |]
        |> bytesto<int>
        |> should equal 1

    [<Test>]
    let ``reads 16777216 from big endian`` () =
        mkbytes [| 0; 0; 0; 1 |]
        |> bytesto<int>
        |> should equal 16777216

    [<Test>]
    let ``reads 218893066 from 0A0B0C0D`` () =
        mkbytes [| 0x0A; 0x0B; 0x0C; 0x0D |]
        |> bytesto<int>
        |> should equal 218893066

    [<Test>]
    let ``reads 168496141 from 0D0C0B0A`` () =
        mkbytes [| 0x0D; 0x0C; 0x0B; 0x0A |]
        |> bytesto<int>
        |> should equal 168496141
