﻿module VBoxVmcoreViewer.LibElf.ElfFile

open VBoxVmcoreViewer.BinaryOps.Operations
open VBoxVmcoreViewer.LibElf.ElfHeader
open VBoxVmcoreViewer.LibElf.Notes
open VBoxVmcoreViewer.LibElf.ProgramHeader
open VBoxVmcoreViewer.LibElf.Types
open VBoxVmcoreViewer.ResultComputation

let readElfFile stream = hopefully {
    let! elfHeader, stream = readElfHeader stream
    let! programHeaders = readProgramHeaders elfHeader stream
    let! notes = readNotes programHeaders stream

    return { Header = elfHeader
             ProgramHeaders = programHeaders
             Notes = notes
             Stream = stream }
}

let readMem<'a when 'a : unmanaged> (index: uint64) elfFile: Result<'a, string> =
    let size = sizeof<'a> |> uint64

    let header =
        elfFile.ProgramHeaders
        |> Seq.ofList
        |> Seq.filter (fun h -> h.Type = PHType.Load)
        |> Seq.tryFind (fun h -> index >= h.PhysicalAddress && index < (h.PhysicalAddress + h.MemorySize - size))

    match header with
    | Option.None -> errorf $"Index (0x%X{index}) out of range"
    | Some header ->
        if size <= header.FileSize then
            seekToPHeader header elfFile.Stream
            seekc (int64 (index - header.PhysicalAddress)) elfFile.Stream
            Ok (readBytes<'a> elfFile.Stream)
        else
            Ok Unchecked.defaultof<'a>

let getTotalPhysicalMemorySize elfFile: uint64 =
    elfFile.ProgramHeaders
    |> List.filter (fun ph -> ph.Type = PHType.Load)
    |> List.map (fun ph -> ph.MemorySize)
    |> List.sum
