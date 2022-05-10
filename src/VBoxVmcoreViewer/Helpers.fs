module VBoxVmcoreViewer.Helpers

let errorf format = Result.Error (sprintf format)
