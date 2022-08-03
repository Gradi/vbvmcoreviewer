# VBoxVmcoreViewer

This is dummy program which reads VirtualBox's core dumps of virtual
machines and saves memory paging tables in HTML files.

This program is probably buggy and it has inifinite loop under unknown
conditions (probably in case of memory table pointing to itself), but
generally it works.

You can dump virtual machine with this command:

```
C:\Program Files\Oracle\VirtualBox\VBoxManage.exe debugvm <uuid|vmname> dumpvmcore [--filename=name]
```

## Building

Install .NET 6 and issue command:

```
dotnet build .\src\VBoxVmcoreViewer\VBoxVmcoreViewer.fsproj -p:Configuration=x64;Configuration=Debug
```

## Usage

```
.\VBoxVmcoreViewer.exe <path-to-elf-file> <path-to-html-report-directory>
```
