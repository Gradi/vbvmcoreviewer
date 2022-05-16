module VBoxVmcoreViewer.LibElf.VirtualBox

open VBoxVmcoreViewer.BinaryOps.Operations
open VBoxVmcoreViewer.ResultComputation

type SegmentSelector =
    { Base: uint64
      Limit: uint64
      Attr: uint64
      Sel: uint64 }

type SegmentDescriptor =
    { Addr: uint64
      Cb: uint64 }

type Sysenter =
    { Cs: uint64
      Eip: uint64
      Esp: uint64 }

type CpuDump =
    { Rax: uint64
      Rbx: uint64
      Rcx: uint64
      Rdx: uint64
      Rsi: uint64
      Rdi: uint64
      R8: uint64
      R9: uint64
      R10: uint64
      R11: uint64
      R12: uint64
      R13: uint64
      R14: uint64
      R15: uint64
      Rip: uint64
      Rsp: uint64
      Rbp: uint64
      RFlags: uint64
      Cs: SegmentSelector
      Ds: SegmentSelector
      Es: SegmentSelector
      Fs: SegmentSelector
      Gs: SegmentSelector
      Ss: SegmentSelector
      Cr0: uint64
      Cr2: uint64
      Cr3: uint64
      Cr4: uint64
      DbgRegs: uint64 list
      Gdtr: SegmentDescriptor
      Idtr: SegmentDescriptor
      Ldtr: SegmentSelector
      Tr: SegmentSelector
      Sysenter: Sysenter
      MsrEfer: uint64
      MsrStar: uint64
      MsrPat: uint64
      MsrLstar: uint64
      MsrCStar: uint64
      MsrSfMask: uint64
      MsrKernelGSBase: uint64
      MsrApicBase: uint64
      AXcr: uint64 list
      cbExt: uint64 }

type VBCoreDescriptor =
    { FormatVersion: uint64
      VirtualBoxVersion: System.Version
      VirtualBoxRevision: uint64
      CpusCount: uint64 }

let readVBCoreDescriptor stream = hopefully {
    let magic = readBytes<uint32> stream

    if magic <> 0xc01ac0deu then
        return! errorf "Can't read VirtualBox's DBGFCOREDESCRIPTOR. Magic doesn't match."

    let fmtVersion = readBytes<uint32> stream
    skipBytes 4 stream
    let vboxVersion = readBytes<uint32> stream
    let vboxRevision = readBytes<uint32> stream
    let cpuCount = readBytes<uint32> stream

    let major = (vboxVersion >>> 24) &&& 0xffu |> int
    let minor = (vboxVersion >>> 16) &&& 0xffu |> int
    let patch = vboxVersion &&& 0xffffu |> int
    let vboxVersion = System.Version (major, minor, patch)

    return { FormatVersion = fmtVersion |> uint64
             VirtualBoxVersion = vboxVersion
             VirtualBoxRevision = vboxRevision |> uint64
             CpusCount = cpuCount |> uint64 }
}

let readSegmentSelector stream =
    let uBase = readBytes<uint64> stream
    let uLimit = readBytes<uint32> stream
    let uAttr = readBytes<uint32> stream
    let uSel = readBytes<uint16> stream

    skipBytes 6 stream

    { Base = uBase
      Limit = uLimit |> uint64
      Attr = uAttr |> uint64
      Sel = uSel |> uint64 }

let readSegmentDescriptor stream =
    let addr = readBytes<uint64> stream
    let cb = readBytes<uint32> stream

    skipBytes 4 stream

    { Addr = addr
      Cb = cb |> uint64 }

let readSysenter stream =
    let cs = readBytes<uint64> stream
    let eip = readBytes<uint64> stream
    let esp = readBytes<uint64> stream

    { Cs = cs
      Eip = eip
      Esp = esp }

let readCpuDump stream =
    let rax = readBytes<uint64> stream
    let rbx = readBytes<uint64> stream
    let rcx = readBytes<uint64> stream
    let rdx = readBytes<uint64> stream
    let rsi = readBytes<uint64> stream
    let rdi = readBytes<uint64> stream
    let r8 = readBytes<uint64> stream
    let r9 = readBytes<uint64> stream
    let r10 = readBytes<uint64> stream
    let r11 = readBytes<uint64> stream
    let r12 = readBytes<uint64> stream
    let r13 = readBytes<uint64> stream
    let r14 = readBytes<uint64> stream
    let r15 = readBytes<uint64> stream
    let rip = readBytes<uint64> stream
    let rsp = readBytes<uint64> stream
    let rbp = readBytes<uint64> stream
    let rflags = readBytes<uint64> stream
    let cs  = readSegmentSelector stream
    let ds  = readSegmentSelector stream
    let es  = readSegmentSelector stream
    let fs  = readSegmentSelector stream
    let gs  = readSegmentSelector stream
    let ss  = readSegmentSelector stream
    let cr0 = readBytes<uint64> stream
    let cr2 = readBytes<uint64> stream
    let cr3 = readBytes<uint64> stream
    let cr4 = readBytes<uint64> stream
    let dbgRegs = List.init 8 (fun _ -> readBytes<uint64> stream)
    let gdtr = readSegmentDescriptor stream
    let idtr = readSegmentDescriptor stream
    let ldtr = readSegmentSelector stream
    let tr = readSegmentSelector stream
    let sysenter = readSysenter stream
    let msrEfer = readBytes<uint64> stream
    let msrStar  = readBytes<uint64> stream
    let msrPat  = readBytes<uint64> stream
    let msrLstar  = readBytes<uint64> stream
    let msrCStar  = readBytes<uint64> stream
    let msrSFMask  = readBytes<uint64> stream
    let msrKernelGsBase  = readBytes<uint64> stream
    let msrApicBase  = readBytes<uint64> stream
    let axcr = List.init 2 (fun _ -> readBytes<uint64> stream)
    let cbExt = readBytes<uint32> stream

    skipBytes 8196 stream // uPaddding0 + X86XSAVEAREA (p.s. too lazy to load that too)

    { Rax = rax
      Rbx = rbx
      Rcx = rcx
      Rdx = rdx
      Rsi = rsi
      Rdi = rdi
      R8 = r8
      R9 = r9
      R10 = r10
      R11 = r11
      R12 = r12
      R13 = r13
      R14 = r14
      R15 = r15
      Rip = rip
      Rsp = rsp
      Rbp = rbp
      RFlags = rflags
      Cs = cs
      Ds = ds
      Es = es
      Fs = fs
      Gs = gs
      Ss = ss
      Cr0 = cr0
      Cr2 = cr2
      Cr3 = cr3
      Cr4 = cr4
      DbgRegs = dbgRegs
      Gdtr = gdtr
      Idtr = idtr
      Ldtr = ldtr
      Tr = tr
      Sysenter = sysenter
      MsrEfer = msrEfer
      MsrStar = msrStar
      MsrPat = msrPat
      MsrLstar = msrLstar
      MsrCStar = msrCStar
      MsrSfMask = msrSFMask
      MsrKernelGSBase = msrKernelGsBase
      MsrApicBase = msrApicBase
      AXcr = axcr
      cbExt = cbExt |> uint64 }

