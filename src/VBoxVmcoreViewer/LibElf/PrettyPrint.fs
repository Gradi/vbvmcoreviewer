module VBoxVmcoreViewer.LibElf.PrettyPrint

open VBoxVmcoreViewer.LibElf.Types

let prettyPhHeader =
    sprintf "%s  %18s  %18s  %18s  %18s  %18s  %18s  %s" "Type" "Flags" "Offset" "VirtualAddress" "PhysicalAddress" "FileSize" "MemorySize" "Aligment"

let prettyPh ph =
    let flags =
        List.map (sprintf "%A") ph.Flags
        |> List.map (fun str -> str[0].ToString())
        |> String.concat " "

    sprintf $"%A{ph.Type}  %18s{flags}  0x%016x{ph.Offset}  0x%016x{ph.VirtualAddress}  0x%016x{ph.PhysicalAddress}  0x%016x{ph.FileSize}  0x%016x{ph.MemorySize}  0x%x{ph.Alignment}"
