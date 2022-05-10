module VBoxVmcoreViewer.LibElf.ElfHeader

open VBoxVmcoreViewer.BinaryOps.Operations
open VBoxVmcoreViewer.Helpers
open VBoxVmcoreViewer.LibElf.Types
open VBoxVmcoreViewer.ResultComputation

let readEiMagic stream = hopefully {
        let actualMagic = readNBytes 4 stream
        let expectedMagic = mkbytesLe [| 0x7f; (int 'E'); (int 'L'); (int 'F') |]

        return! if actualMagic <> expectedMagic then errorf "Magic bytes don't match."
                else Ok ()
    }

let readEiClass stream = hopefully {
        let eiClass = readBytes<byte> stream
        match eiClass with
        | 0uy -> return Class.None
        | 1uy -> return Class.ElfClass32
        | 2uy -> return Class.ElfClass64
        | v -> return! errorf $"Invalid EI_CLASS value %d{v}"
    }

let readEiData stream = hopefully {
        let eiData = readBytes<byte> stream
        match eiData with
        | 0uy -> return DataEncoding.None
        | 1uy -> return DataEncoding.LittleEndian
        | 2uy -> return DataEncoding.BigEndian
        | v -> return! errorf $"Invalid EI_DATA valid %d{v}"
    }

let readEiIdentification stream = hopefully {
        do! readEiMagic stream
        let! eiClass = readEiClass stream
        let! eiData = readEiData stream
        let version = readBytes<byte> stream |> int
        let osAbi = readBytes<byte> stream |> int
        let abiVersion = readBytes<byte> stream |> int

        stream.Stream.Seek (16, System.IO.SeekOrigin.Begin) |> ignore //Skip rest of bytes of header.

        return { Class = eiClass
                 DataEncoding = eiData
                 Version = version
                 OsAbi = osAbi
                 AbiVersion = abiVersion }
    }

let readType stream =
    let typeVal = readBytes<uint16> stream
    match typeVal with
    | 0us -> Type.None
    | 1us -> Type.Relocatable
    | 2us -> Type.Executable
    | 3us -> Type.Dynamic
    | 4us ->  Type.Core
    | _ -> Type.ProcessorSpecific

let readMachine stream = hopefully {
        let machine = readBytes<uint16> stream
        match machine with
        | 0us  -> return Machine.None
        | 2us  -> return Machine.Sparc
        | 3us  -> return Machine.I386
        | 18us -> return Machine.Sparc32Plus
        | 43us -> return Machine.Sparcv9
        | 62us -> return Machine.Amd64
        | v    -> return! errorf $"Unknown machine type %d{v}"
    }

let readElfHeader stream = hopefully {
    let! eiIdent = readEiIdentification stream
    if eiIdent.Class <> Class.ElfClass64 then
        return! errorf $"Unsupported ELF class %A{eiIdent.Class}. Only 64 bits ELFs are supported."

    System.Int32.Try

    let elfType = readType stream
    let! machine = readMachine stream
    return ()
    }
