module VBoxVmcoreViewer.LibElf.Types

open VBoxVmcoreViewer.BinaryOps.Types

type Class =
    | None
    | ElfClass32
    | ElfClass64

type ElfStream = { Class: Class; Stream: Stream }

type DataEncoding =
    | None
    | LittleEndian
    | BigEndian

type Identification =
    { Class: Class
      DataEncoding: DataEncoding
      Version: int
      OsAbi: int
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
    | Sparc
    | I386
    | Sparc32Plus
    | Sparcv9
    | Amd64

type Header =
    { Identification: Identification
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
