Mercury with Microsoft Visual C++
=================================

This file documents the port of Mercury to Windows that uses Microsoft Visual
C++ (MSVC) as a C compiler.

With MSVC, you can install a Mercury compiler that generates either x86
(32-bit) or x64 (64-bit) Windows native code in C grades, but not both.
(You can, of course, have multiple Mercury installations that use MSVC,
and have one that targets x86 and another that targets x64.)

Contents
--------

* Supported MSVC versions
* Setting up the build environment
* Configuration and installation
* Using Mercury in the MSYS2 or Cygwin shells
* Limitations

Supported MSVC versions
-----------------------

You need version 19.3 or later of MSVC. Version 19.3 was included with
Visual Studio 2022. Mercury is *not* supported with older versions of MSVC.
Your Visual Studio installation needs the "Desktop development with C++"
component to be installed.

Setting up the build environment
--------------------------------

You need a Unix-like environment to build and install Mercury.
You can use either [Cygwin](https://www.cygwin.com) or
[MSYS2](https://www.msys2.org).

You only need a Unix-like environment for the installation. You do not need
one to run Mercury later.

To make MSVC the toolchain available in the Cygwin or MSYS2 shells, do the
following:

1. Open the Visual Studio Command Prompt.

   Go to the correct tool in your Start Menu:

       Start Menu
       └── Visual Studio 2022
           ├── x64 Native Tools Command Prompt for VS 2022
           └── x86 Native Tools Command Prompt for VS 2022

   Select x64 to target 64-bit Windows or x86 to target 32-bit Windows.

   If you prefer not to use the Start Menu shortcuts, then you can manually set
   up the environment variables by running one of the batch files supplied with
   Visual Studio (e.g. `vcvars32.bat` for x86, `vcvars64.bat` for x64).

2. Start the MSYS2 shell using this command:

       C:\> C:\msys64\msys2_shell.cmd -use-full-path

   Or start the Cygwin shell using this command:

       C:\> C:\cygwin64\Cygwin.bat

   We assume the default installation locations for MSYS2 and Cygwin above.

   To install the `csharp` grade, you must add a C# compiler to your Windows
   `PATH`. See [README.CSharp.md](README.CSharp.md) for further details.

   To install the `java` grade, you must add a Java compiler to your Windows
   `PATH`. See [README.Java.md](README.Java.md) for further details.

Configuration and installation
------------------------------

You can install the MSVC port of Mercury directly from the Mercury source
distribution.

If you already have an existing Mercury installation that uses the
[MinGW-w64](https://www.mingw-w64.org) ports of GCC or Clang, or one that uses
the Cygwin port of GCC, then you can clone the Mercury source from the Git
repository and use your existing installation to cross-compile the MSVC port.

When using MSVC, you *must* set the Mercury installation directory using the
`--prefix` option to `configure`. 

* Use a full Windows path with a drive letter (e.g., `C:/mercury`).

* **Important**: Use forward slashes (`/`), not backslashes (`\`) as path
  separators (even though the latter is more usual on Windows).

For example, this is acceptable:

    ./configure --prefix="c:/where/to/install/mercury"

but this is not:
    
    ./configure --prefix="c:\where\to\install\mercury"

You *must* set the installation directory correctly, or the installation will
fail.

Run `configure` as follows:

    ./configure --with-cc=cl --prefix=<install-dir> [<any other options>]

Once `configure` has successfully finished, then do:

    make

and then:

    make install

as normal.

Using Mercury in the MSYS2 or Cygwin shells
-------------------------------------------

This section describes how to use a Mercury compiler that was built using MSVC
in the MSYS2 or Cygwin shells.

Check that the MSVC toolchain is in the Windows `PATH` and that the required
tools (`cl`, `link` and `lib`) are accessible from the Cygwin or MSYS2 shell.

Add the Mercury `bin` directory to the MSYS2 or Cygwin `PATH` using a
Unix-style path. For example, if Mercury is installed in "C:\mercury", then you
would add `/c/mercury/bin` to the MSYS2 `PATH` or `/cygdrive/c/mercury/bin` to
the Cygwin `PATH`.

After the Mercury `bin` directory has been added to the MSYS2 or Cygwin `PATH`,
then you should be able to use the Mercury compiler.

Using Mercury in the Windows Command Prompt
-------------------------------------------

This section explains how to use Mercury that was built using MSVC in the
Windows Command Prompt (i.e. `cmd.exe`).

Make sure that the following are present in the Windows `PATH`:

1. The MSVC toolchain (`cl`, `link` and `lib`).

2. The Java toolchain (`javac`, `jar` and `java`), if the `java` grade was
   installed.

3. The C# toolchain (`csc`), if the `csharp` grade was installed.

4. The Mercury `bin` directory. For example, if Mercury is installed in
   `C:\mercury`, then you would add `C:\mercury\bin` to the Windows `PATH`.

You can then run the Mercury compiler using the command `mercury`.
This is a batch file that is the same as `mmc` in other environments.
We use the name `mercury` instead of `mmc` in the Windows Command Prompt.
This is because the name `mmc` is also used by the Microsoft Management
Console.

For example, to build the "Hello, World" example in the `samples` directory
of the Mercury distribution, do:

    mercury hello.m

or, using the `--make` option:

    mercury --make hello

Note that `mmake` is not supported in the Windows Command Prompt.

Limitations
-----------

The MSVC port currently has the following limitations:

* The `asm_fast` and `reg` grades do not work.
  Both use GNU extensions to C that MSVC does not provide.

* Time profiling does not (currently) work with MSVC.
  Time profiling grades (those whose name contains the `.prof` grade component)
  will not be installed.

  Memory profiling _does_ work with MSVC. (Memory profiling grades
  are those whose name contains the `.memprof` grade component.)

* Parallel grades do not work because the Mercury runtime uses POSIX threads.
  It does not yet support Windows threads.
  
  (It might be possible to use the
  [pthreads-win32](https://sourceforge.net/projects/pthreads4w/)
  library with MSVC to provide POSIX threads, but we have not yet tested that.) 

* Deep profiling (e.g. the `.profdeep` grades) does not (currently) work
  with MSVC. (In principle, it should work if the clock tick metric is
  disabled.)

* The deep profiling tool (`mdprof_cgi`) does not currently work with MSVC.
  This is due to it containing Unix dependencies. (Other tools that work with
  deep profiles should be fine.)

* You cannot create libraries (DLLs) yet.

* The `--c-debug` option is disabled with MSVC since enabling it breaks
  parallel builds and disables some C compiler optimizations.

  If you *really* want to enable support for C level debugging, then enable the
  commented out definition of `DEBUG_OPTS` in `scripts/mgnuc.in`
  (for the `cl` case) and also enable the commented out definition of
  `CFLAGS_FOR_DEBUG` in `configure.ac` (for the `msvc` case). You will then need
  to regenerate the `configure` script and rebuild the Mercury system.
  (See [INSTALL.git](../INSTALL.git) for details of how to do this.)

* `mdb`'s `open` and `grep` commands do not work. The scripts that implement
  these commands only work on Unix-like systems.

* The following shell scripts included with the Mercury distribution do not
  work on Windows.

  - `mprof_merge_runs`
  - `mtc`

-----------------------------------------------------------------------------
