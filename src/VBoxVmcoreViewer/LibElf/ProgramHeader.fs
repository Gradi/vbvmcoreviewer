module VBoxVmcoreViewer.LibElf.ProgramHeader

open VBoxVmcoreViewer.BinaryOps.Operations
open VBoxVmcoreViewer.BinaryOps.Types
open VBoxVmcoreViewer.LibElf.Types
open VBoxVmcoreViewer.ResultComputation

let readType stream = hopefully {
    let value = readBytes<uint32> stream
    match value with
    | 0u -> return PHType.Null
    | 1u -> return PHType.Load
    | 2u -> return PHType.Dynamic
    | 3u -> return PHType.Interp
    | 4u -> return PHType.Note
    | 5u -> return PHType.Shlib
    | 6u -> return PHType.Phdr
    | v when 0x60000000u >= v && v <= 0x6fffffffu -> return PHType.ProcessorSpecific
    | v when 0x70000000u >= v && v <= 0x7fffffffu -> return PHType.ProcessorSpecific
    | v -> return! errorf $"Unsupported program header type value (%d{v})."
}

let readFlags stream =
    let value = readBytes<uint32> stream

    [ ( PHFlags.Read, 0x4u ); ( PHFlags.Write, 0x2u ); ( PHFlags.Execute, 0x1u ) ]
    |> List.map (fun (flag, bit) -> if (value &&& bit) <> 0u then Some flag else Option.None )
    |> List.choose id

let readProgramHeader stream = hopefully {
    let! phType = readType stream
    let phFlags = readFlags stream
    let offset = readBytes<uint64> stream
    let vAddr = readBytes<uint64> stream
    let pAddr = readBytes<uint64> stream
    let fileSize = readBytes<uint64> stream
    let memSize = readBytes<uint64> stream
    let alignment = readBytes<uint64> stream

    return { Type = phType
             Flags = phFlags
             Offset = offset
             VirtualAddress = vAddr
             PhysicalAddress = pAddr
             FileSize = fileSize
             MemorySize = memSize
             Alignment = alignment }
}

let readProgramHeaders elfHeader (stream: Stream) = hopefully {
    if elfHeader.ProgramHeaderNumber <> 0UL then
        stream.Stream.Seek(elfHeader.ProgramHeaderTableOffset |> int64, System.IO.SeekOrigin.Begin) |> ignore

    return!
        Seq.init (elfHeader.ProgramHeaderNumber |> int)
        <| (fun _ -> readProgramHeader stream)
        |> allOrErrored
}

let seekToPHeader programHeader (stream: Stream) =
    stream.Stream.Seek (programHeader.Offset |> int64, System.IO.SeekOrigin.Begin) |> ignore

let isProgramHeaderOver programHeader (stream: Stream) =
    let position = stream.Stream.Position |> uint64
    position >= (programHeader.Offset + programHeader.FileSize)


