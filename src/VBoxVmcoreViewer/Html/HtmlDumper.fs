module VBoxVmcoreViewer.Html.HtmlDumper

open System.IO
open VBoxVmcoreViewer.BinaryOps.Operations
open VBoxVmcoreViewer.BinaryOps.Types
open VBoxVmcoreViewer.LibElf.Types
open VBoxVmcoreViewer.TextWriterComputation
open VBoxVmcoreViewer.X86.Paging

let private writeHeader = writer {
    do! writeln "<head>"
    do! prefix (writer {
        do! writeln "<meta charset=\"utf-8\">"
        do! writeln "<title>VBoxVmcoreViewer report</title>"
    })
    do! writeln "</head>"
}

let private writeHtml (contentList: MTextWriter list) = writer {
    do! writeln "<html lang=\"en-US\">"

    do! prefix (writer {
        do! writeHeader

        do! writeln "<body>"
        do! prefix (writer {
            for content in contentList do
                do! content
        })
        do! writeln "</body>"
    })

    do! writeln "</html>"
}

let private writeElfFile elfFile =
    let header = writer {
        do! writeln "<h2>ELF header</h2>"
        do! writeln "<code>"
        do! writeln $"%A{elfFile.Header}"
        do! writeln "</code>"
    }
    let headers = writer {
        do! writeln "<h2>Program headers<h2>"
        do! writeln "<table style=\"font-family: monospace\">"
        do! prefix (writer {
            do! writeln "<thead>"
            do! prefix (writer {
                do! writeln "<tr>"
                do! prefix (writer {
                    do! writeln "<th>Type</th>"
                    do! writeln "<th>Flags</th>"
                    do! writeln "<th>Offset</th>"
                    do! writeln "<th>Virtual Address</th>"
                    do! writeln "<th>Physical Address</th>"
                    do! writeln "<th>File Size</th>"
                    do! writeln "<th>Memory size</th>"
                    do! writeln "<th>Alignment</th>"
                })
                do! writeln "</tr>"
            })
            do! writeln "</thead>"

            do! writeln "<tbody>"
            do! prefix (writer {
                for header in elfFile.ProgramHeaders do
                    do! writeln "<tr>"
                    do! prefix (writer {
                        do! writeln $"<td>%A{header.Type}</td>"
                        do! writeln $"<td>%A{header.Flags}</td>"
                        do! writeln $"<td>0x%016X{header.Offset}</td>"
                        do! writeln $"<td>0x%016X{header.VirtualAddress}</td>"
                        do! writeln $"<td>0x%016X{header.PhysicalAddress}</td>"
                        do! writeln $"<td>%d{header.FileSize}</td>"
                        do! writeln $"<td>%d{header.MemorySize}</td>"
                        do! writeln $"<td>%d{header.Alignment}</td>"
                    })
                    do! writeln "</tr>"
            })
            do! writeln "</tbody>"

        })
        do! writeln "</table>"
    }
    let notes = writer {
        do! writeln "<h2>Notes</h2>"
        do! writeln "<details>"
        do! writeln "<summary>Notes</summary>"
        do! writeln "<ol>"
        do! prefix (writer {
            for note in elfFile.Notes do
                do! writeln "<li>"
                do! writeln $"<p>%s{note.Name}</p>"
                do! writeln $"<code>%A{note.Type}</code>"
                do! writeln "</li>"
        })
        do! writeln "</ol>"
        do! writeln "</details>"
    }

    writer {
        do! header
        do! headers
        do! notes
    }

let private writeLinkToPagingsPage dir name cpuIndex = writer {
    do! writeln $"<h2><a href=\"%s{dir}/%s{name}\">Memory paging tables for CPU #%d{cpuIndex}</a></h2>"
}

let private createHtmlFile paths (m: MTextWriter) =
    let path = Path.Combine (Array.ofList paths)

    Directory.CreateDirectory (Path.GetDirectoryName path) |> ignore

    use file = File.Create path
    use writer = new StreamWriter (file, System.Text.Encoding.UTF8)
    let wrapper = { TextWriter = writer; Prefix = "    "; PrefixLevel = [] }
    m wrapper

let private bitformat prefix (bit: Bit option) =
    let suffix =
        match bit with
        | Option.None -> "?"
        | Some true -> "1"
        | Some false -> "0"

    sprintf $"%s{prefix}:%s{suffix}"

let private rndHtmlname () =
    let id = System.Guid.NewGuid().ToString("N")
    sprintf $"%s{id}.html"

let rec private writePointer baseDir pointer = writer {
    match pointer with
    | Frame _ -> return ()
    | Table table ->
        do! writeln $"<h2>Table at 0x%016X{table.Address}</h2>"

        let entries =
            table.Entries
            |> List.choose id

        match entries with
        | [] -> do! writeln "<p>Table is empty.</p>"
        | entries ->
            do! writeln "<table style=\"font-family: monospace\">"

            do! prefix (writer {
                for entry in entries do
                    let entryTexted = [
                        bitformat "RW" (Some entry.RW)
                        bitformat "US" (Some entry.US)
                        bitformat "PWT" (Some entry.PWT)
                        bitformat "PCD" (Some entry.PCD)
                        bitformat "A" (Some entry.Accessed)
                        bitformat "D" entry.Dirty
                        bitformat "PS" entry.PageSize
                        bitformat "G" entry.Global
                        bitformat "PAT" entry.PAT
                    ]
                    do! writeln "<tr>"
                    for text in entryTexted do
                        do! prefix (writer {
                            do! writeln $"<td>%s{text}</td>"
                        })

                    match entry.Pointer.Value with
                    | Error err -> do! writeln $"<td>Error: %s{err}</td>"

                    | Ok (Frame (size, address)) ->
                        let size = bytesToHumanString size
                        do! writeln $"<td>F: %016X{address} -> %s{size}</td>"

                    | Ok (Table table as pointer) ->
                        let id = rndHtmlname ()
                        do! writeln $"<td><a href=\"%s{id}\">T: %016X{table.Address}</a></td>"
                        createHtmlFile [ baseDir; id ] (writeHtml [writePointer baseDir pointer])

                    do! writeln "</tr>"
            })

            do! writeln "</table>"
}

let writeStartOfPaging baseDir startOfPaging = writer {
    do! writeln $"<h2>Mode: %A{startOfPaging.Mode}</h2>"
    do! writePointer baseDir startOfPaging.Pointer
}

let generateHtmlReport elfFile pagings directory =
    let pagings = List.indexed pagings
    let tablesDir = "tables"
    let baseDir = Path.Combine ([| directory; tablesDir |])

    let cpuTablesFilesnames =
        pagings
        |> List.map (fun (index, paging) -> (index, sprintf $"cpu_memory_table_%d{index}.html", paging))

    let linksToCpuTables =
        cpuTablesFilesnames
        |> List.map (fun (index, filename, _) -> writeLinkToPagingsPage tablesDir filename index )

    let content = writeElfFile elfFile :: linksToCpuTables
    createHtmlFile [directory; "index.html"] (writeHtml content)

    cpuTablesFilesnames
    |> List.iter (fun (_, filename, paging) ->
        createHtmlFile [ baseDir; filename ] (writeHtml [ writeStartOfPaging baseDir paging ]))
