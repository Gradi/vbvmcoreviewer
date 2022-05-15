module VBoxVmcoreViewer.LibElf.ElfHeader

open VBoxVmcoreViewer.BinaryOps.Operations
open VBoxVmcoreViewer.BinaryOps.Types
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
    | 0uy -> return EIClass.None
    | 1uy -> return EIClass.ElfClass32
    | 2uy -> return EIClass.ElfClass64
    | v -> return! errorf $"Invalid EI_CLASS value %d{v}"
}

let readEiData stream = hopefully {
    let eiData = readBytes<byte> stream
    match eiData with
    | 0uy -> return EIDataEncoding.None
    | 1uy -> return EIDataEncoding.LittleEndian
    | 2uy -> return EIDataEncoding.BigEndian
    | v -> return! errorf $"Invalid EI_DATA valid %d{v}"
}

let readEiOsAbi stream = hopefully {
    let osAbi = readBytes<byte> stream
    match osAbi with
    | 0uy -> return EIOsAbi.None
    | 1uy -> return EIOsAbi.HPux
    | 2uy -> return EIOsAbi.NetBsd
    | 3uy -> return EIOsAbi.Linux
    | 6uy -> return EIOsAbi.Solaris
    | 7uy -> return EIOsAbi.Aix
    | 8uy -> return EIOsAbi.Irix
    | 9uy -> return EIOsAbi.FreeBsd
    | 10uy -> return EIOsAbi.Tru64
    | 11uy -> return EIOsAbi.ModeSto
    | 12uy -> return EIOsAbi.OpenBsd
    | 13uy -> return EIOsAbi.OpenVms
    | 14uy -> return EIOsAbi.Nsk
    | 15uy -> return EIOsAbi.Aros
    | 97uy -> return EIOsAbi.Arm
    | 255uy -> return EIOsAbi.Standalone
    | v -> return! errorf $"Unsupported EI_OSABI value (%d{v})."
}

let readEiIdentification stream = hopefully {
    do! readEiMagic stream
    let! eiClass = readEiClass stream
    let! eiData = readEiData stream
    let version = readBytes<byte> stream |> int
    let! osAbi = readEiOsAbi stream
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
    | 3us  -> return Machine.I386
    | 62us -> return Machine.Amd64
    | v    -> return! errorf $"Unsupported e_machine value (%d{v})."
}

let readElfHeader stream = hopefully {
    let! eiIdent = readEiIdentification stream
    let! stream = hopefully {
        return! (match eiIdent.DataEncoding with
                | EIDataEncoding.LittleEndian -> Ok { stream with Endianess = Endianess.Little }
                | EIDataEncoding.BigEndian -> Ok { stream with Endianess = Endianess.Big }
                | EIDataEncoding.None -> errorf "DataEncoding of value 'None' is unsupported.")
    }
    do! hopefully {
        if eiIdent.Class <> EIClass.ElfClass64 then
            return! errorf $"Unsupported EI_CLASS value (%A{eiIdent.Class}).
                             I support only %A{EIClass.ElfClass64}."
        else
            return ()
    }

    let elfType = readType stream
    let! machine = readMachine stream
    let version = readBytes<int> stream
    let entry = readBytes<uint64> stream
    let phOff = readBytes<uint64> stream
    let shOff = readBytes<uint64> stream
    let flags = readBytes<int> stream
    let headerSize = readBytes<uint16> stream |> uint64
    let phEntrySize = readBytes<uint16> stream |> uint64
    let phNumber = readBytes<uint16> stream |> uint64
    let shEntrySize = readBytes<uint16> stream |> uint64
    let shNumber = readBytes<uint16> stream |> uint64
    let shStringIndex = readBytes<uint16> stream |> uint64

    return (
        { Identification = eiIdent
          Type = elfType
          Machine = machine
          Version = version
          Entry = entry
          ProgramHeaderTableOffset = phOff
          SectionHeaderTableOffset = shOff
          Flags = flags
          HeaderSize = headerSize
          ProgramHeaderEntrySize = phEntrySize
          ProgramHeaderNumber = phNumber
          SectionHeaderEntrySize = shEntrySize
          SectionHeaderNumber = shNumber
          SectionHeaderStringIndex = shStringIndex },
        stream )
}
