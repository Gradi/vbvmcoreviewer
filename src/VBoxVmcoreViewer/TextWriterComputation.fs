module VBoxVmcoreViewer.TextWriterComputation

open System.IO

type TextWriterWrapper =
    { TextWriter: TextWriter
      Prefix: string
      PrefixLevel: string list }

type MTextWriter = TextWriterWrapper -> unit

type TextWriterBuilder () =

    member _.Bind (m: MTextWriter, f: unit -> MTextWriter): MTextWriter = fun w -> f (m w) w

    member _.Zero (): MTextWriter = fun _ -> ()

    member _.For (s, f): MTextWriter = fun w ->
        s
        |> Seq.map f
        |> Seq.map (fun f -> f w)
        |> Seq.iter id

    member _.Return (_: unit): MTextWriter = fun _ -> ()

    member _.Combine (m: MTextWriter, f: unit -> MTextWriter): MTextWriter = fun w ->
        m w
        (f ()) w

    member _.Delay f = f

    member _.Run f = f ()


let private writePrefix: MTextWriter = fun w ->
    for prefix in w.PrefixLevel do
        w.TextWriter.Write prefix

let write text: MTextWriter = fun w ->
    writePrefix w
    (fprintf w.TextWriter text) |> ignore

let writeln text: MTextWriter = fun w ->
    writePrefix w
    (fprintfn w.TextWriter text) |> ignore

let prefix (m:MTextWriter): MTextWriter = fun w ->
    let w = { w with PrefixLevel = w.Prefix :: w.PrefixLevel }
    m w

let writer = TextWriterBuilder ()
