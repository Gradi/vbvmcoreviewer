module VBoxVmcoreViewer.LibElf.ElfFile

open VBoxVmcoreViewer.LibElf.Types
open VBoxVmcoreViewer.LibElf.ElfHeader
open VBoxVmcoreViewer.LibElf.ProgramHeader
open VBoxVmcoreViewer.LibElf.Notes
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
