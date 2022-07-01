module VBoxVmcoreViewer.BinaryOps.Operations

open FSharp.NativeInterop
open VBoxVmcoreViewer.BinaryOps.Types

let nativeEndianess =
    if System.BitConverter.IsLittleEndian then Endianess.Little
    else Endianess.Big

let oppositeEndianess endianess =
    match endianess with
    | Endianess.Little -> Endianess.Big
    | Endianess.Big -> Endianess.Little

let readNBytes n stream =
    let bytes = Array.create n (byte 0)
    // I intentionally ignore possibility of 'not enough bytes in stream' because this is a toy project.
    stream.Stream.Read bytes |> ignore
    { Endianess = stream.Endianess; Bytes = bytes }

let bytesToNativeEndianess (bytes: Bytes) =
    if bytes.Endianess = nativeEndianess then bytes
    else
        { Endianess = oppositeEndianess bytes.Endianess; Bytes = Array.rev bytes.Bytes }

#nowarn "9"
#nowarn "51"
let bytesto<'a when 'a : unmanaged> (bytes: Bytes) =
    let resultSize = sizeof<'a>
    let mutable result = Unchecked.defaultof<'a>
    let presult = NativePtr.ofVoidPtr<byte> (NativePtr.toVoidPtr <| &&result)

    if resultSize <> Array.length bytes.Bytes then
        failwithf $"Not enough bytes(%d{Array.length bytes.Bytes}) for type %O{typeof<'a>} of size %d{resultSize} bytes."

    for i = 0 to (Array.length bytes.Bytes) - 1 do
        NativePtr.set presult i (Array.item i bytes.Bytes)

    result

let readBytes<'a when 'a: unmanaged> stream =
    readNBytes sizeof<'a> stream
    |> bytesToNativeEndianess
    |> bytesto<'a>

let mkbytes endianess bytes = { Endianess = endianess; Bytes = Array.map byte bytes }

let mkbytesLe = mkbytes Endianess.Little

let mkbytesBe = mkbytes Endianess.Big

let readString n stream =
    let bytes = readNBytes n stream
    System.Text.Encoding.ASCII.GetString bytes.Bytes

let alignStream (n: int) stream =
    let n = n |> int64
    let rem = stream.Stream.Position % n
    if rem <> 0 then
        stream.Stream.Seek (n - rem, System.IO.SeekOrigin.Current) |> ignore

let seekb n stream =
    stream.Stream.Seek (n, System.IO.SeekOrigin.Begin) |> ignore

let seekc n stream =
    stream.Stream.Seek (n, System.IO.SeekOrigin.Current) |> ignore

let seeke n stream =
    stream.Stream.Seek (n, System.IO.SeekOrigin.End) |> ignore

let skipBytes = seekc

let uint64Bit (bit: int) (value: uint64): Bit = ((value >>> bit) &&& 0x1UL) = 0x1UL

let uint64Bits (bits: int list) (value: uint64) : uint64 =
    let result =
        bits |>
        List.fold (fun state bit -> state ||| (value &&& (0x1UL <<< bit))) 0UL

    result >>> (List.min bits)
