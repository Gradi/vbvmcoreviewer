open System.IO
open VBoxVmcoreViewer.BinaryOps.Operations
open VBoxVmcoreViewer.BinaryOps.Types
open VBoxVmcoreViewer.LibElf.ElfFile
open VBoxVmcoreViewer.LibElf.PrettyPrint
open VBoxVmcoreViewer.LibElf.Types
open VBoxVmcoreViewer.X86.X86

let readAndPrintFile file =
    try
        use fstream = File.OpenRead file
        let stream = { Endianess = nativeEndianess; Stream = fstream }
        let elfFile = readElfFile stream

        match elfFile with
        | Ok elfFile ->
            printfn $"%s{file}"
            printfn $"%A{elfFile.Header}"

            printfn $"Program headers (%d{List.length elfFile.ProgramHeaders}):"
            printfn $"%s{prettyPhHeader}"
            List.iter (fun header -> printfn $"%s{prettyPh header}") elfFile.ProgramHeaders

            printfn "VBoxCore: "
            elfFile.Notes
            |> List.filter (fun n -> match n.Type with NoteType.VBoxCore _ -> true | _ -> false)
            |> List.iter (printfn "%A")

            printfn "CPU paging modes: "
            elfFile.Notes
            |> List.choose (fun n -> match n.Type with VBoxCpu dump -> Some dump | _ -> Option.None)
            |> List.map (fun dump -> getPagingTables dump elfFile)
            |> List.iter (fun result ->
                match result with
                | Ok startOfPaging -> printfn $"%A{startOfPaging.Mode}"
                | Error err -> printfn $"%s{err}")

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
    | [| file |] when File.Exists file -> readAndPrintFile file
    | _ ->
        printfn "Usage: <path-to-elf-file>"
        2
