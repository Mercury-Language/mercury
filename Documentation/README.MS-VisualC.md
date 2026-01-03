Mercury with Microsoft Visual C++
=================================

This file documents the port of Mercury to Windows using Microsoft Visual
C++ (MSVC) as a C compiler. Version 19.0 or later of MSVC is required.
Version 19 shipped with Visual Studio 2022. Mercury is *not* supported
with older versions of MSVC.

With MSVC, you can install a Mercury compiler that generates either x86
(32-bit) or x64 (64-bit) Windows native code in C grades, but not both.
(You can, of course, have multiple Mercury installations that use MSVC,
and have one that targets x86 and another that targets x64.)

Setting up the build environment
--------------------------------

A Unix-like environment is required for building and installing Mercury.
[Cygwin](https://www.cygwin.com) or [MSYS2](https://www.msys2.org) will
suffice.

A Unix-like environment is *not* required in order to use Mercury once it
has been installed.

To make MSVC and its supporting tools available under the Cygwin or MSYS2
shells do the following:

1. Open the Visual Studio Command Prompt.

   This can typically be found in

         Start -> Visual Studio YYYY -> Visual Studio Tools

   although the name and location vary between different versions and editions
   of Visual Studio. (*YYYY* is the year.) Note that this will give you the
   x86 version of MSVC.

   The Visual Studio entry in the `Start` menu may also have items named
   similarly to:

        * x64 Native Tools Command Prompt for VS 2022
        * x86 Native Tools Command Prompt for VS 2022

   These let you choose between the x64 and x86 versions of MSVC toolchain.

   Alternatively, you can start a `cmd.exe` session and use the batch files supplied
   with Visual Studio (e.g. `vcvarsall.bat`, `vcvars32.bat` or `vcvars64.bat`) to
   set up the environment for MSVC.

2. Enter the following command to start MSYS2 shell with 32-bit MSVC:

        C:\> C:\msys64\msys2_shell.cmd -use-full-path -mingw32

   or this one to start the MSYS2 shell with 64-bit MSVC:
        
        C:\> C:\msys64\msys2_shell.cmd -use-full-path -mingw64

   or this one to start the Cygwin shell:

        C:\> C:\cygwin64\Cygwin.bat

(We assume the default installation locations for MSYS2 and Cygwin above.)

Users of MSYS2 should note that it is required that the configuration type
returned by autoconf match `*mingw*`; in particular it must *not* match
`*-pc-msys`. You can use the `config.guess` script to see what configuration
type autoconf detects.

To install the C# or Java grades, you will need a C# or Java compiler
respectively to be included in the Windows `PATH`.
(See the relevant README files for further details, e.g.
[README.Java.md](README.Java.md) etc)

Configuration and Installation
------------------------------

The MSVC port of Mercury is compatible with the prebuilt C files contained in
the Mercury source distribution. The `asm_fast*` and `reg*` grades will not work
with MSVC (see below). When using the prebuilt C files the compiler will be
built in the `hlc.gc.pregen` grade.

Alternatively, if you have an existing Mercury installation that uses the
MinGW-w64 or Cygwin GCC ports, or clang, then you can checkout the Mercury
source from the git repository and use your existing installation to
cross-compile the MSVC port.

In either case, to use MSVC as the C compiler with Mercury, invoke `configure`
as follows:


    ./configure --with-cc=cl [--with-msvcrt] [<any other options>]


The `--with-msvcrt` flag causes executables built with this install of Mercury
to be linked with the MS Visual C runtime, instead of the standard libC
runtime.

On Cygwin, `configure` and `mmake` will do translation of Unix style paths, so
you may specify the installation directory using either a Unix- or
Windows-style path. On MSYS2, you must use a full Windows-style path with a
drive letter, except that you must use `/` instead of `\` as a directory
separator. For example, this is acceptable:


    ./configure --prefix="c:/where/to/install/mercury"


but this is not:


    ./configure --prefix="c:\where\to\install\mercury"


Once `configure` has successfully finished, then do


    make


and then


    make install


as normal.

Limitations
-----------

The MSVC port currently has a number of limitations:

* The `asm_fast` and `reg` grades do not work with MSVC.
  Both use GNU extensions to C that MSVC does not provide.

* Time profiling does not (currently) work with MSVC.
  Time profiling grades (those whose name contains the `prof` grade component)
  will not be installed.

  Note that memory profiling _does_ work with MSVC. (Memory profiling grades
  are those whose name contains the `memprof` grade component.)

* Parallel grades (those whose name contains the `par` component) do not
  currently work with MSVC.

  In parallel grades the Mercury runtime currently requires the use of POSIX
  threads; it has not currently been ported to use Windows threads.
  (It might be possible to use the [pthreads-win32](https://sourceforge.net/projects/pthreads4w/)
  library with MSVC to provide POSIX threads but we have not tested that yet.) 

* Deep profiling (e.g. the `*.profdeep` grades) does not (currently) work
  with MSVC. (In principle, it should work if the clock tick metric is
  disabled.)

* The deep profiling tool (`mdprof_cgi`) does not currently work with MSVC.
  This is due to it containing a number of Unix dependencies. (Other
  tools that work with deep profiles should be fine.)

* When used directly from the Windows command prompt, mmake will not work.
  You should use `mmc --make` instead. (`mmake` requires a POSIX-like shell
  and GNU `make`; it will however work with the Cygwin or MSYS2 shells.)
  We do not intend to support `mmake` directly on Windows.

* Creation of shared libraries (DLLs) is not currently supported.

* The `--c-debug` option currently has no effect with MSVC since enabling
  it breaks parallel builds and disables some C compiler optimizations.

  If you *really* want to enable support for C level debugging, then enable the
  commented out definition of `DEBUG_OPTS` in `scripts/mgnuc.in` (in the `cl` case)
  and also enable the commented out definition of `CFLAGS_FOR_DEBUG` in
  `configure.ac` (in the `msvc*` case). You will then need to regenerate
  the `configure` script and rebuild the Mercury system.
  (See [../INSTALL.git](Install.git) for details of how to do this.)

Post-installation configuration
-------------------------------

The above instructions create a Mercury installation using MSVC that works
from within the Cygwin or MSYS2 shells. If you want to be able to run the
Mercury compiler directly from the Windows Command Prompt (e.g. `cmd.exe`)
or PowerShell, then you need to manually edit some configuration files in the
installation. (In future releases, this will all hopefully be automated.)

All references to files in the following are within the Mercury installation
directory

* In the file `lib/mercury/conf/Mercury.config`:
  
  + Replace any Unix-style paths with their Windows-style equivalent. 

* In the file `lib/mercury/mdb/mdbrc`

  + The backslash character, `\`, is used as an escape character in mdbrc
    files. It must be escaped it if it occurs in any paths used in the
    argument of `source` commands, for example

        source c:\mercury-11.07\lib\mercury\mdb\mdb_doc

    needs to be replaced with:

        source c:\\mercury-11.07\\lib\\mercury\\mdb\\mdb_doc

  + Delete the aliases for the `open` and `grep` commands.
    The scripts that implement these commands assume a Unix-style environment.

* The `bin` directory contains batch files equivalent to the `mmc`, `mdb` and
  `mprof` scripts. Note that the batch file equivalent to the `mmc` script is
   named `mercury.bat` in order to avoid clashing with the executable for the
   Microsoft Management Console.

* The following scripts do not currently have a Windows equivalent.

    + `mprof_merge_runs`
    + `mtc`

* The other shell scripts in the `bin` directory do not have (or need) Windows
  equivalents. (Most of them are part of the implementation of `mmake` which
  in the Windows Command Prompt or PowerShell.)

-----------------------------------------------------------------------------
