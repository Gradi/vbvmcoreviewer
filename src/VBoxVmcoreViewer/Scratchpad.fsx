#load "ContextComputation.fs"
open VBoxVmcoreViewer.ContextComputation

type MyContext = { X: int; Y: int }


let draw = ContextBuilder<MyContext> ()


draw {
    let! w = context
    printfn $"Sample %d{w.X}"
} <| { X = 1; Y = 0 }
