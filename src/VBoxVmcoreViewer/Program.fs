open System.IO
open VBoxVmcoreViewer.BinaryOps.Operations
open VBoxVmcoreViewer.BinaryOps.Types
open VBoxVmcoreViewer.LibElf.ElfFile
open VBoxVmcoreViewer.LibElf.PrettyPrint
open VBoxVmcoreViewer.X86.X86
open VBoxVmcoreViewer.LibElf.Types

let readAndPrintHeader file =
    try
        use fstream = File.OpenRead (file)
        let stream = { Endianess = nativeEndianess; Stream = fstream }
        let elfFile = readElfFile stream

        match elfFile with
        | Ok elfFile ->
            printfn $"%s{file}"

            printfn $"%A{elfFile.Header}"

            printfn $"Notes (%d{List.length elfFile.Notes}):"
            List.iter (printfn "%A") elfFile.Notes

            printfn $"Program headers (%d{List.length elfFile.ProgramHeaders}):"
            printfn $"%s{prettyPhHeader}"
            List.iter (fun header -> printfn $"%s{prettyPh header}") elfFile.ProgramHeaders

            List.iter (fun note ->
                match note with
                | { Note.Type = NoteType.VBoxCpu cpuDump } -> printfn $"Paging mode: %A{getPadingMode cpuDump}"
                | _ -> ())  elfFile.Notes

            0
        | Error msg ->
            printfn $"%s{msg}"
            1
    with
    | e ->
        printfn $"%O{e}"
        1

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] when File.Exists file -> readAndPrintHeader file
    | _ ->
        printfn "Usage: <path-to-elf-file>"
        2
