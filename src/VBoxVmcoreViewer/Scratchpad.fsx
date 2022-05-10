#load "ResultComputation.fs"
open VBoxVmcoreViewer.ResultComputation

let t: Result<int, string> = hopefully {
    let! t1 = Ok 1
    let! t2 = Error "not good"
    let! t3 = Ok 3
    if t2 = 2 then return 4
    else return 5
}
