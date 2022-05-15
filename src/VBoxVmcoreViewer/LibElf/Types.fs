module VBoxVmcoreViewer.LibElf.Types

type EIClass =
    | None
    | ElfClass32
    | ElfClass64

type EIDataEncoding =
    | None
    | LittleEndian
    | BigEndian

type EIOsAbi =
    | None
    | HPux
    | NetBsd
    | Linux
    | Solaris
    | Aix
    | Irix
    | FreeBsd
    | Tru64
    | ModeSto
    | OpenBsd
    | OpenVms
    | Nsk
    | Aros
    | Arm
    | Standalone

type EIIdentification =
    { Class: EIClass
      DataEncoding: EIDataEncoding
      Version: int
      OsAbi: EIOsAbi
      AbiVersion: int }

type Type =
    | None
    | Relocatable
    | Executable
    | Dynamic
    | Core
    | ProcessorSpecific

type Machine =
    | None
    | I386
    | Amd64

type ElfHeader =
    { Identification: EIIdentification
      Type: Type
      Machine: Machine
      Version: int
      Entry: uint64
      ProgramHeaderTableOffset: uint64
      SectionHeaderTableOffset: uint64
      Flags: int
      HeaderSize: uint64
      ProgramHeaderEntrySize: uint64
      ProgramHeaderNumber: uint64
      SectionHeaderEntrySize: uint64
      SectionHeaderNumber: uint64
      SectionHeaderStringIndex: uint64 }
