module VBoxVmcoreViewer.BinaryOps.Types

type Endianess =
    | Little
    | Big

type Bytes = { Endianess: Endianess; Bytes: byte array }

type Stream = { Endianess: Endianess; Stream: System.IO.Stream }

type Bit = bool
