Mercury on Windows
==================

This file documents the port of Mercury to Microsoft Windows.

Contents
--------

* Supported versions of Windows
* Building Mercury on Windows
* Building Mercury for Windows on Linux
* Using Mercury on Windows

Supported versions of Windows
-----------------------------

Mercury has been tested with the following versions of Windows:

    * Windows 7
    * Windows 10
    * Windows 11

We no longer actively maintain support for older versions of Windows.

Building Mercury on Windows
---------------------------

The Mercury build process requires the use of a number of Unix tools such as
`sh` and `make`. This means that a Unix emulation environment is required to
build Mercury on Windows.

Three such environments are supported:

1. [Cygwin](https://www.cygwin.com). See [README.Cygwin](README.Cygwin).

2. [MSYS](https://osdn.net/projects/mingw/). See [README.MinGW](README.MinGW).

3. [MSYS2](https://www.msys2.org). See [README.MinGW](README.MinGW).

Mercury can also be built using the MS Visual C compiler (MSVC), although one
of the above environments is still required for the build process.
See [README.MS-VisualC.md](README.MS-VisualC.md) for instructions on how to
build Mercury with MSVC.

NOTE: while a Unix emulation environment is required to build Mercury on
Windows, one is NOT required to use Mercury on Windows.

Building Mercury for Windows on Linux
-------------------------------------

Alternatively, you can cross-compile Mercury on Linux with a MinGW
cross-compiler.  See [README.cross.md](README.cross.md).

Using Mercury on Windows
------------------------

On Windows systems the usual name for the Mercury compiler, `mmc`, conflicts
with the name of the executable for the Microsoft Management Console.
See the "Using the Mercury compiler" chapter of the
[Mercury Users's Guide](https://mercurylang.org/documentation/documentation.html) for
how to deal with this.

-----------------------------------------------------------------------------
