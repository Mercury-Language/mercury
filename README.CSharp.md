Mercury C# Backend
==================

The Mercury compiler has a backend that generates C# source code, that can be
compiled into bytecode suitable for running using the .NET or Mono runtime
systems. The backend is mostly complete, but some parts of the Mercury standard
library are not yet implemented.

The C# backend requires C# 5.0 or higher -- older versions of C# are *not*
supported.

Contents
--------

* Prerequisites
* Installing the `csharp` grade
* Compiling programs with the `csharp` grade
* Running `csharp` grade programs with Mono
* Running `csharp` grade programs on Windows with .NET
* Limitations
* Library support
* Interfacing with C#
* Mercury-level debugging
* Building the Mercury compiler in the `csharp` grade

Prerequisites
-------------

In order to use Mercury's C# backend you will need either:

* Microsoft .NET 4.5 or above.
* Mono 4.0 or above.

Installing the `csharp` grade
-----------------------------

The Mercury compiler uses the grade `csharp` to target C# source code that
is then compiled by a C# compiler.

Mercury's autoconfiguration script will cause the `csharp` grade to be installed
if it finds a suitable C# compiler (e.g. `csc`) and .NET runtime in your `PATH`.

You can check if your Mercury installation has been configured to include the
`csharp` grade by looking if `csharp` is included in the output of the Mercury
compiler's `--output-stdlib-grades` option.

Compiling programs with the `csharp` grade
------------------------------------------

Once you have a Mercury installation that includes the `csharp` grade, you
can build programs such as `hello.m` or `calculator.m` in the [samples](samples)
directory.

```
    $ cd samples
    $ mmc ---grade csharp --make hello
```

When building programs with the `csharp` grade you *must* use `mmc --make`; using
`mmake` to build programs using the `csharp` grade is _not_ supported.

Running `csharp` grade programs with Mono
-----------------------------------------

For the example in the previous section on a Unix (or more generally,
non-Windows) system using Mono, the Mercury compiler will generate a process
assembly, e.g. `hello.exe`, and a wrapper shell script named `hello`.

The wrapper shell script will set the `MONO_PATH` environment variable
to point to the location of the Mercury standard library assemblies.
It will then invoke then CLI execution environment on the process assembly.
You can run the program using wrapper shell script, for example:

```
    $ ./hello
```

Running `csharp` grade programs on Windows with .NET
----------------------------------------------------

On Windows, the Mercury compiler will only generate a process assembly, e.g.
`hello.exe`. On Windows there is no need to generate a wrapper shell script.

With .NET, the library assemblies (.dlls) for the Mercury standard
libraries must either (1) reside in (or under) the same directory as the process
assembly (.exe) or (2) be entered into the global assembly cache (GAC).
If neither of these things is done then execution will abort with a message that
begins:

```
    Unhandled Exception: System.IO.FileNotFoundException: Could not load file
    or assembly 'mer_std',  Version=...
```

For (1), you will need to copy the library assemblies from the Mercury library
installation directory into the same directory as the process assembly.
The files for the Mercury library assemblies are located in

```
     <prefix>\lib\mercury\lib\csharp
```

where `<prefix>` is the location of the Mercury installation.
Copy all of the .dll files in the above directory into that of the process
assembly.

To enter assemblies into the GAC, run the following command for each
assembly.

```
    gacutil /i mer_std.dll
```

Assemblies can be removed from the GAC by doing, for example

```
    gacutil /u mer_std.dll
```

Limitations
-----------

The following features of the Mercury implementation are not (currently)
supported by the C# backend:

* Mercury-level debugging (however, see further down).
* Mercury-level profiling.
* Trailing.
* Tabling.
* Backjumping.

Library support
----------------

The Mercury standard library has not been fully ported to C# yet.
The use of unimplemented procedures will result in a run-time error,
with a stack trace and a message like:

```
    Sorry, not implemented: foreign code for this function
```

If you find missing functionality, you can interface to C# using Mercury's
foreign language interface.

The following individual Mercury standard library procedures are either not
supported or not fully implemented:
   
1. `io.read_binary/{3,4}`    
   `io.write_binary/{3,4}`

    The current implementation of `read_binary` does not work with the
    way Mercury file streams are implemented for the C# backend.

2. `benchmarking.report_stats/0`    
   `benchmarking.report_full_memory_stats/0`

    Memory usage statistics are not yet available, and cpu time
    is not the same as in the C backends, as per `time.m`.

3. `store.arg_ref/5`    
   `store.new_arg_ref/5`

    Due to some limitations in RTTI support, dynamic type checking is missing
    for these predicates. They should be used with care.

4.  `math.fma/3`

    This function is not available because it is not supported by C# 5.0.
    (It will be supported once the minimum version of C# required by
    Mercury increases.)

Interfacing with C#
-------------------

You can call C# code directly from Mercury using the foreign language
interface. For example:

```
:- pred to_string(T::in, string::out) is det.
:- pragma foreign_proc("C#",
    to_string(T::in, Str::out),
    [promise_pure, will_not_call_mercury],
"
    Str = T.ToString();
").
```

The implementation will include this C# code in the module's .cs file, and you
can then call the predicate `to_string/2` exactly the same as if it were
implemented using pure Mercury code.

For more information about the foreign language interface, see the
[Mercury Language Reference Manual](https://www.mercurylang.org/information/documentation.html).
Additionally, the [samples/csharp_interface](samples/csharp_interface) directory in
the Mercury distribution contains examples of how to use the foreign language
interface with C#.

Mercury-level debugging
-----------------------

The only Mercury-level debugger available for the C# backend is the
_experimental_ source-to-source debugger; see [README.ssdebug.md](README.ssdebug.md)
for details.

Building the Mercury compiler in the `csharp` grade
---------------------------------------------------

Building the Mercury compiler and other related tools in the C# grade is NOT
generally supported and should be considered experimental.
In particular, a Mercury compiler built in the C# grade may be slower than
normal and some features may not be available.

However, if you want to give it a try, the required steps are:

1. Ensure that you have an existing working Mercury compiler in your `PATH`
   and a clean version of the Mercury source tree.

2. Run `prepare.sh` and `configure` as normal.

3. Add the line:

       GRADE=csharp

    to a file named `Mmake.params` at the top-level of the source tree.

4. Begin the build process using the following command:

       $ mmake --use-mmc-make GRADE=csharp

   The C# version of the compiler MUST be built using `mmake`'s `--use-mmc-make`
   option; the build will not work otherwise. Setting the variable `GRADE` in the
   invocation of mmake is currently necessary in order to avoid some variable
   definition ordering problems in `Mmake.workspace`.

5. To install the C# version of the compiler, do:

       $ mmake --use-mmc-make install GRADE=csharp

-----------------------------------------------------------------------------
