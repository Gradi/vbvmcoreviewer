open System.IO
open VBoxVmcoreViewer.BinaryOps.Operations
open VBoxVmcoreViewer.BinaryOps.Types
open VBoxVmcoreViewer.LibElf.ElfHeader

let readAndPrintHeader file =
    try
        use fstream = File.OpenRead (file)
        let stream = { Endianess = nativeEndianess; Stream = fstream }
        let header = readElfHeader stream

        match header with
        | Ok (header, _) ->
            printfn $"%s{file}"
            printfn $"%A{header}"
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
