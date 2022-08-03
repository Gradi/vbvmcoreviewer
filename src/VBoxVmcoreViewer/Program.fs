open System.IO
open VBoxVmcoreViewer.BinaryOps.Operations
open VBoxVmcoreViewer.BinaryOps.Types
open VBoxVmcoreViewer.Html
open VBoxVmcoreViewer.LibElf.ElfFile
open VBoxVmcoreViewer.LibElf.Types
open VBoxVmcoreViewer.ResultComputation
open VBoxVmcoreViewer.X86.X86

let readElfFileAndPagings stream = hopefully {
    let stream = { Endianess = nativeEndianess; Stream = stream }

    let! elfFile = readElfFile stream
    let! pagings =
        Seq.ofList elfFile.Notes
        |> Seq.choose (fun n -> match n.Type with VBoxCpu cpuDump -> Some cpuDump | _ -> Option.None)
        |> Seq.map (fun cpuDump -> readPagingTables cpuDump elfFile)
        |> allOrErrored

    return (elfFile, pagings)
}

[<EntryPoint>]
let main argv =
    match argv with
    | [| file; directory |] when File.Exists file ->
        use fsstream = File.OpenRead file

        match readElfFileAndPagings fsstream with
        | Error err ->
            printfn $"%s{err}"
            1
        | Ok (_, []) ->
            printfn "No CPU dumps in this VBox core dump file."
            2
        | Ok (elfFile, pagings) ->

            Directory.CreateDirectory directory |> ignore
            HtmlDumper.generateHtmlReport elfFile pagings directory

            0
    | _ ->
        printfn "Usage: <path-to-elf-file> <path-to-html-report-directory>"
        2
