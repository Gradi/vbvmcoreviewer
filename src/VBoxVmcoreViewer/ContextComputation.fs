module VBoxVmcoreViewer.ContextComputation

type Context<'a, 'b> = 'a -> 'b

let context: Context<'a, 'a> = id

let ofSeq (s: Context<'a, 'b> seq) : Context<'a, 'b seq> = fun ctx -> s |> Seq.map (fun f -> f ctx)

let bind (f: 'b -> Context<'a, 'c>) (m: Context<'a, 'b>) : Context<'a, 'c> = fun ctx -> f (m ctx) ctx

type ContextBuilder<'a> () =

    member _.Delay f = f

    member _.Run f = f ()

    member _.Bind (m, f) = bind f m

    member _.Return a: Context<'a, 'b> = fun _ -> a

    member _.ReturnFrom a = a

    member _.Zero (): Context<'a, Unit> = fun _ -> ()

    member _.For (seq: 'b seq, f: 'b -> Context<'a, Unit>) = fun ctx -> seq |> Seq.map f |> Seq.map (fun f -> f ctx) |> Seq.iter id

    member _.Combine (m: Context<'a, Unit>, f: Unit -> Context<'a, 'b>) = fun ctx ->
        m ctx
        (f ()) ctx
