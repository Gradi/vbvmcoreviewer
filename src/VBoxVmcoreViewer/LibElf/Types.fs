module VBoxVmcoreViewer.LibElf.Types

open VBoxVmcoreViewer.BinaryOps.Types
open VBoxVmcoreViewer.LibElf.VirtualBox

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

type ElfType =
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
      Type: ElfType
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

type PHType =
    | Null
    | Load
    | Dynamic
    | Interp
    | Note
    | Shlib
    | Phdr
    | ProcessorSpecific

type PHFlags =
    | Read
    | Write
    | Execute

type ProgramHeader =
    { Type: PHType
      Flags: PHFlags list
      Offset: uint64
      VirtualAddress: uint64
      PhysicalAddress: uint64
      FileSize: uint64
      MemorySize: uint64
      Alignment: uint64 }

type NoteType =
    | Prstatus
    | Prfpreg
    | Prpsinfo
    | VBoxCore of VBCoreDescriptor
    | VBoxCpu of CpuDump

type Note =
    { Name: string
      Type: NoteType }

type ElfFile =
    { Header: ElfHeader
      ProgramHeaders: ProgramHeader list
      Notes: Note list
      Stream: Stream }
