Mercury on MSYS2
================

This file documents the port of Mercury to Windows using the environments
provided by the [MSYS2](https://www.msys2.org) platform.

Note that MSYS2 is separate from the older MSYS / MinGW project.
The latter appears to be dead, and we do not recommend its use with Mercury.

Contents
--------

* Overview
* Installing Mercury with MSYS2
* Using Mercury in the MSYS2 shell
* Using Mercury in the Command Prompt
* Missing `libwinpthread-1.dll`

Overview
--------

[MSYS2](https://www.msys2.org) is a POSIX-like native build environment for
Windows. It provides a package manager, and tools like `bash`, `make` and `gcc`
(via the [MinGW-w64](https://www.mingw-w64.org/) port).

Using MSYS2 and the MinGW-w64 ports of GCC, you can build and install a Mercury
compiler that generates executables that will run natively on Windows *without*
the need for a support environment like MSYS2 or Cygwin.

In this document we assume that MSYS2 is installed in its default installation
directory (`C:\msys64`). If this is not the case on your system, then you will
need to adjust the examples below to use your MSYS2 installation path instead.

Installing Mercury with MSYS2
-----------------------------

To build and install the Mercury source distribution under MSYS2, follow these
steps:

1. Select which of the MSYS2 environments you are going to use.
   An MSYS2 environment controls what architecture, C compiler, linker
   and system libraries are used. Further details can be found on the
   [Environments](https://www.msys2.org/docs/environments) page of the
   MSYS2 website.

   Only the `UCRT64` and `MINGW64` environments are currently supported.
   These environments correspond to the `MSYS2 UCRT64` and `MSYS2 MINGW64`
   launchers installed by MSYS2 in the Windows Start Menu. Mercury compilers
   built in these environments create 64-bit Windows executables. We recommend
   using the `UCRT64` environment unless you require your executable to be
   dependent on the Microsoft Visual C++ Runtime (MSVCRT) for some reason (e.g.
   because it contains foreign code that has a dependency on the MSVCRT). If that
   is the case, then use the `MINGW64` environment instead.

   The legacy `MINGW32` environment should also work and can be used to build
   32-bit Windows executables, although we no longer actively support it.
   The `MINGW32` environment corresponds to the `MSYS2 MING32` launcher in
   the Windows Start Menu.

   The `MSYS`, `CLANG64`, `CLANGARM64` and `CLANG32` environments do *not*
   currently work with Mercury.

   In the MSYS2 shell the value of the `MSYSTEM` environment variable
   identifies which of the MSYS2 environments you are using.

2. Ensure that prerequisite packages are installed. The following packages
   must be installed in order to build Mercury in your MSYS2 system:

   * `tar`
   * `gzip` (if you have the `.tar.gz` download)
   * `xz` (if you have the `.tar.xz` download)
   * `make`
   * `gcc`
   * `binutils`
   * `diffutils`

   The `gcc` package must be the one provided by the selected environment (e.g.
   `mingw-w64-ucrt-x86_64-gcc` for `UCRT64`  or `mingw-w64-x86_64-gcc` for
   `MINGW64`). Be careful not to use the `gcc` package for the `MSYS` environment
   as that will *not* work.

   In addition, to build the documentation you will need the following
   packages:

   * `texinfo`
   * `lynx` (to convert HTML to plain text)

   If you are building the compiler directly from `git`, then you will also
   need the following packages:

   * `m4`
   * `autoconf`
   * `flex`
   * `bison`

   All of the above can be installed using `pacman` from within the appropriate
   MSYS2 shell.

3. Optional: To build and install the `java` grade with MSYS2, you will
   require a Windows version of the JDK to be installed on your system. The
   following tools must be present in the MSYS2 `PATH`:

   * The Java compiler (`javac`).
   * The Java archive tool (`jar`).
   * The Java runtime (`java`).

   To add them to the MSYS2 `PATH`, do the following:

       export PATH="/c/Program\ Files/Java/jdk-21/bin":$PATH

   (The details will vary depending on which distribution and version
   of Java you are using.)

   The Java tools must be Windows-native executables, not MSYS2-packaged
   Java binaries.

   See [README.Java.md](README.Java.md) for further details.

4. Optional: To build and install the `csharp` grade with MSYS2, you will
   require the Visual C# compiler to be installed on your system. The C#
   compiler (`csc.exe`) must be in the MSYS2 `PATH`.

   To add it to the MSYS2 `PATH`, do the following:

        export PATH="/c/WINDOWS/Microsoft.NET/Framework/v4.0.30319/":$PATH

   (The details will vary depending on which version of .NET you are using.)

   See [README.CSharp.md](README.CSharp.md) for further details.

5. Unpack the Mercury source distribution:
    
        tar -xvzf mercury-compiler-VERSION.tar.gz

   `VERSION` will be the Mercury version number like `22.01` or
   `rotd-2025-12-27`.

6. Change into the Mercury source directory and run the `configure` script:

        ./configure --prefix=c:/mercury

   The value of the `--prefix` option specifies the installation prefix for
   Mercury (here `c:/mercury`). It is _important_ to specify the installation
   prefix as a _full_ Windows path with a drive letter. In the installation
   prefix, you _must_ use `/` as a path separator instead of `\`.

   Do _NOT_ set the installation prefix to be a Unix-style path, for example:

      ./configure --prefix=/c/mercury

   This will *not* work because the MSYS2 shell and the generated executables
   will each interpret it differently. Specifically, MSYS2 performs path
   translation for Unix-style paths, while the generated Windows executables do
   not. The resulting Mercury installation will be broken.

   Other options to the `configure` script behave as they do on other systems.

7. Run `make` and then `make install`.

Using Mercury in the MSYS2 shell
--------------------------------

This section describes how to use a Mercury compiler that was installed using
the instructions in the previous section from within an MSYS2 environment. The
MSYS2 environment that you run the compiler in *must* match the one it was
built in (i.e. do not use a Mercury compiler built using the `MINGW64` in the
`UCRT64` environment).

Add the Mercury `bin` directory to the MSYS2 `PATH` using a Unix-style path.
For example, if Mercury is installed in "C:\mercury", then you would add
`/c/mercury/bin` to the MSYS2 `PATH`. You cannot add a Windows-style path with
a drive letter to the MSYS2 `PATH` since `:` is used as the path separator in
MSYS2.

After the Mercury `bin` has been added to the MSYS2 `PATH`, then you should be
able to use the compiler.

Note that while `mmake` does work in the MSYS2 shell, it is quite slow.
This is largely due to the MSYS2 port of GNU `cp` being relatively slow on
Windows filesystems. We suggest using `mmc --make`, which does not use `cp`
by default, when possible.

Using Mercury in the Command Prompt
-----------------------------------

This section describes how to use a Mercury compiler that was installed using
the instructions above from the Windows Command Prompt (i.e. `cmd.exe`).

Ensure that the following directories are present in the Windows `PATH`:

1. The `bin` directory of the MSYS2 environment you used to build Mercury.
   For the `UCRT64` environment this will be `C:\msys64\ucrt64\bin`.
   For the `MINGW64` environment this will be `C:\msys64\mingw64\bin`.

2. The directory containing the Java toolchain (`javac`, `jar` and `java`),
   if the `java` grade was installed.

3. The directory containing the C# toolchain, if the `csharp` grade was
   installed.

4. The Mercury `bin` directory. For example, if Mercury is installed in
   `C:\mercury`, then you would add `C:\mercury\bin` to the Windows `PATH`.

You can then invoke the Mercury compiler using the command `mercury`, which is
a batch file that is equivalent to `mmc` in other environments.
We do not use `mmc` in the Windows Command Prompt because that name clashes
with the executable for the Microsoft Management Console.

For example, to build the "Hello, World" example in the `samples` directory
of the Mercury distribution, do:

    mercury hello.m

or, using the `--make` option:

    mercury --make hello

Note that `mmake` is not supported in the Windows Command Prompt.

Missing `libwinpthread-1.dll`
-----------------------------

This issue typically occurs when C grade executables are run outside of an
MSYS2 shell (e.g. from `cmd.exe` or PowerShell). Executables run in `cmd.exe`
may abort and display a Windows loader error dialog with a message like the
following:

    The code execution cannot proceed because libwinpthread-1.dll was
    not found. Reinstalling the program may fix this problem.

The problem here is that `libwinpthread-1.dll`, which is required by Mercury
executables, cannot be found. There are several possible resolutions:

1. Statically link against all libraries. This can be done by passing the
   option `--linkage=static` to the Mercury compiler. This will increase the
   size of the executables.

2. Modify the dynamic-link library search order on Windows to include
   the directory containing `libwinpthread-1.dll`. There are a lot
   of ways to do this, but the two simple ones are to either copy the file
   `libwinpthread-1.dll` into the same directory as the executable
   or to add the directory containing that DLL to the Windows PATH.

   For the `UCRT64` MSYS2 environment, `libwinpthread-1.dll` can be found in
   the directory `C:\msys64\ucrt64\bin`. Add it to the Windows `PATH` by doing:

       set PATH=C:\msys64\ucrt64\bin;%PATH%

   For the `MINGW64` MSYS2 environment, `libwinpthread-1.dll` can be found in
   the directory `C:\msys64\mingw64\bin`. Add it to the Windows `PATH` by
   doing:

       set PATH=C:\msys64\mingw64\bin;%PATH%

   For a more complete discussion of how Windows searches for DLLs, consult the
   following documentation from Microsoft:

       <https://learn.microsoft.com/en-us/windows/win32/dlls/dynamic-link-library-search-order>

If you are running a Mercury executable from within PowerShell, then the same
problem with a "missing" `libwinpthread-1.dll` can occur.
In PowerShell, no error dialog is displayed. (PowerShell does not display the
Windows loader error dialog for missing DLLs.) Instead, the value of the
built-in variable `$LASTEXITCODE` is set to a non-zero value.
This issue can be resolved in PowerShell using the same resolutions described
above.

-----------------------------------------------------------------------------
