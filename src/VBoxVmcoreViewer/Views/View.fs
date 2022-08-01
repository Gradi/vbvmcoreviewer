module VBoxVmcoreViewer.Views.View

open Raylib_cs
open VBoxVmcoreViewer.BinaryOps.Operations
open VBoxVmcoreViewer.ContextComputation
open VBoxVmcoreViewer.LibElf.ElfFile
open VBoxVmcoreViewer.LibElf.Types
open VBoxVmcoreViewer.X86.Paging

type private WindowState =
    { Width: int
      Height: int
      MouseX: int
      MouseY: int
      LeftMouseButton: bool
      ButtonC: bool
      ButtonB: bool }

type private FontOpts =
    { FontSize: int
      FontColor: Color
      FontSelectedColor: Color
      Spacing: float32
      Font: Font }

type private DrawState =
    { Window: WindowState
      FontOpts: FontOpts }

type DrawableTableEntry =
    { Entry: TableEntry option
      Text: string
      X: int
      Y: int
      Width: int
      Height: int }


let private cbool (cbool: CBool) = CBool.op_Implicit cbool

let private getWindowState () =
    { Width = Raylib.GetScreenWidth ()
      Height = Raylib.GetScreenHeight ()
      MouseX = Raylib.GetMouseX ()
      MouseY = Raylib.GetMouseY ()
      LeftMouseButton = cbool <| Raylib.IsMouseButtonPressed MouseButton.MOUSE_LEFT_BUTTON
      ButtonC = cbool <| Raylib.IsKeyPressed KeyboardKey.KEY_C
      ButtonB = cbool <| Raylib.IsKeyPressed KeyboardKey.KEY_B }

let private defaultFont = lazy (
    let fontSize = 20
    let spacing = (float32 fontSize) / 10f // https://github.com/raysan5/raylib/blob/master/src/rtext.c#L1018

    let path = System.IO.Path.Combine (System.Environment.SystemDirectory, "..", "Fonts", "DejaVuSansMono.ttf" )

    {
    FontSize = fontSize
    FontColor = Color.BLACK
    FontSelectedColor = Color.RED
    Spacing = spacing
    Font = Raylib.LoadFont path
    })

let private getDrawState () = {
      Window = getWindowState ()
      FontOpts = defaultFont.Value
      }

let private draw = ContextBuilder<DrawState> ()

let private fontOpts = draw {
    let! state = context
    return state.FontOpts
}

let private windowState = draw {
    let! state = context
    return state.Window
}

let private drawText (text: string) x y color = draw {
    let! f = fontOpts
    Raylib.DrawTextEx (f.Font, text, System.Numerics.Vector2 (float32 x, float32 y), float32 f.FontSize, f.Spacing, color)
}

let private entryToDrawableEntry entry =
    let format (str, flag) =
        let suffix =
            match flag with
            | Some true -> "1"
            | Some false -> "0"
            | Option.None -> "?"
        sprintf $"%s{str}:%s{suffix}"


    let prefix =
        [
            ( "RW", Some entry.RW )
            ( "US", Some entry.US )
            ( "PWT", Some entry.PWT )
            ( "PCD", Some entry.PCD )
            ( "A", Some entry.Accessed )
            ( "D", entry.Dirty )
            ( "PS", entry.PageSize )
            ( "G", entry.Global )
            ( "PAT", entry.PAT )
        ]
        |> List.map format
        |> Seq.ofList
        |> String.concat " "

    let suffix =
        match entry.Pointer.Value with
        | Error error                -> sprintf $"Error: %s{error}"
        | Ok (Table table)           -> sprintf $"T: 0x%016X{table.Address} - 0x0000000000000000"
        | Ok (Frame (size, address)) -> sprintf $"F: 0x%016X{address} - %16s{bytesToHumanString size}"

    let text = sprintf $"| %s{prefix} | %s{suffix} |"

    { Entry = Some entry; Text = text; X = 0; Y = 0; Width = 0; Height = 0 }

let private recalcSizes drawableEntry = draw {
    let! f = fontOpts
    let size = Raylib.MeasureTextEx (f.Font, drawableEntry.Text, float32 f.FontSize, f.Spacing)
    return { drawableEntry with Width = int size.X; Height = int size.Y }
}

let private alignVertically seq x0 y0 vSpacing =
    seq
    |> Seq.indexed
    |> Seq.map (fun (index, entry) -> { entry with X = x0 - entry.Width / 2; Y = y0 + index * entry.Height + vSpacing })

let private pointerToTableEntries pointer = draw {
    let! w = windowState
    let zeroEntry = { Entry = Option.None; Text = ""; X = 0; Y = 0; Width = 0; Height = 0 }

    let entries =
        match pointer with
        | Error err -> Seq.singleton { zeroEntry with Text = err }
        | Ok (Frame _) -> Seq.singleton { zeroEntry with Text = "Raw mem frame" }
        | Ok (Table table) ->
            let entries = table.Entries |> List.choose id
            match entries with
            | [] -> Seq.singleton { zeroEntry with Text = "Table empty" }
            | xs ->
                xs
                |> Seq.ofList
                |> Seq.map entryToDrawableEntry


    let! entries = entries |> Seq.map recalcSizes |> ofSeq

    return
        alignVertically entries (w.Width / 2) 10 5
        |> Seq.cache
}

let private isEntryHovered entry = draw {
    let! window = windowState

    return window.MouseX >= entry.X && window.MouseX <= (entry.X + entry.Width) &&
           window.MouseY >= entry.Y && window.MouseY <= (entry.Y + entry.Height)
}

let drawCpuMemPaging elfFile startOfPaging =
    let memorySizeBytes = getTotalPhysicalMemorySize elfFile
    let leftHeader = sprintf $"0x0 - 0x%016X{memorySizeBytes} (%s{bytesToHumanString memorySizeBytes}) Mode - %A{startOfPaging.Mode}"

    let mutable tableEntries = pointerToTableEntries ( Ok startOfPaging.Pointer )
    let mutable history = []

    let drawLoop () = draw {
        let! tableEntriesM = tableEntries
        let! f = fontOpts
        let! w = windowState

        // Header
        do! drawText leftHeader 10 10 f.FontColor

        // Main table
        for entry in tableEntriesM do
            let! isEntryHovered = isEntryHovered entry
            let color = if isEntryHovered then f.FontSelectedColor else f.FontColor

            do! drawText entry.Text entry.X entry.Y color


            if isEntryHovered && w.LeftMouseButton then
                match entry.Entry with
                | Option.None -> ()
                | Some entry ->
                    history <- tableEntries :: history
                    tableEntries <- pointerToTableEntries entry.Pointer.Value

        // Controls
        if w.ButtonC then
            tableEntries <- pointerToTableEntries ( Ok startOfPaging.Pointer )
            history <- []

        if w.ButtonB then
            match history with
            | [] -> ()
            | x :: xs ->
                tableEntries <- x
                history <- xs
    }

    while (not (cbool <| Raylib.WindowShouldClose ())) do
        let drawState = getDrawState ()

        Raylib.BeginDrawing ()
        Raylib.ClearBackground Color.WHITE
        (drawLoop ()) drawState
        Raylib.EndDrawing ()


let init () =
    Raylib.SetConfigFlags (ConfigFlags.FLAG_WINDOW_RESIZABLE ||| ConfigFlags.FLAG_VSYNC_HINT)
    Raylib.SetTargetFPS <| Raylib.GetMonitorRefreshRate 0
    Raylib.InitWindow (800, 480, "VBoxVmCoreViewer")

let deinit () =
    Raylib.CloseWindow ()
