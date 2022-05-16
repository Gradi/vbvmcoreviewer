module VBoxVmcoreViewer.LibElf.Notes

open VBoxVmcoreViewer.BinaryOps.Operations
open VBoxVmcoreViewer.ResultComputation
open VBoxVmcoreViewer.LibElf.Types
open VBoxVmcoreViewer.LibElf.ProgramHeader
open VBoxVmcoreViewer.LibElf.VirtualBox

let readNoteDescriptor typeValue stream = hopefully {
    match typeValue with
    | 1u -> return NoteType.Prstatus
    | 2u -> return NoteType.Prfpreg
    | 3u -> return NoteType.Prpsinfo
    | 0xb00u ->
        let! coreDesc = readVBCoreDescriptor stream
        return NoteType.VBoxCore coreDesc
    | 0xb01u ->
        return NoteType.VBoxCpu <| readCpuDump stream
    | v -> return! errorf $"Unsupported note n_type value (%d{v})."
}

let rec readNote stream = hopefully {
    let nameSize = readBytes<uint32> stream
    let descriptorSize = readBytes<uint32> stream
    let noteType = readBytes<uint32> stream

    alignStream 4 stream
    let name = readString (nameSize |> int) stream

    alignStream 4 stream
    let expectedPosition = stream.Stream.Position + (descriptorSize |> int64)

    let! descriptor = readNoteDescriptor noteType stream

    seekb expectedPosition stream

    return { Name = name
             Type = descriptor }
}

let readNotes (programHeaders: ProgramHeader list) stream = hopefully {

    let rec readAllNotes header = hopefully {
        let! note = readNote stream
        if not (isProgramHeaderOver header stream) then
            let! nextNotes = readAllNotes header
            return Seq.append [ note ] nextNotes
        else
            return Seq.singleton note
    }

    let! notes =
        programHeaders
        |> Seq.ofList
        |> Seq.filter (fun ph -> ph.Type = PHType.Note)
        |> Seq.map (fun header ->
            seekToPHeader header stream
            readAllNotes header)
        |> allOrErrored

    return List.collect (List.ofSeq) notes
}
