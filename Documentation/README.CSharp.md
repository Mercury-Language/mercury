Mercury C# Backend
==================

The Mercury compiler has a backend that generates C# source code, that is
compiled into a managed assembly by the .NET 10 SDK and run on the
.NET 10 runtime.  The backend is mostly complete, but some parts of the
Mercury standard library are not yet implemented.

The C# backend requires C# 14 or higher.  Older versions of C# (and the
.NET Framework / Mono runtimes) are *not* supported.

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

To use Mercury's C# backend you need a working .NET SDK at version 10.0
or above.  The `dotnet` command must be on your `PATH`.

There is no longer any support for Mono or for the .NET Framework
runtime.

Installing the `csharp` grade
-----------------------------

The Mercury compiler uses the grade `csharp` to target C# source code
that is then compiled by the .NET SDK.

Mercury's autoconfiguration script will install the `csharp` grade when
it detects `dotnet` on your `PATH` along with a usable >= 10.0 SDK.
You can check the result by running `mmc --output-stdlib-grades` and
looking for `csharp` in the list.

Compiling programs with the `csharp` grade
------------------------------------------

Once you have a Mercury installation that includes the `csharp` grade,
you can build programs such as `hello.m` or `calculator.m` in the
[samples](samples) directory.

```
    mmc --grade csharp --make hello
```

When building programs with the `csharp` grade you *must* use `mmc --make`.
Using `mmake` to build programs using the `csharp` grade is _not_ supported.

Behind the scenes, `mmc --make` generates a `<MainModule>.csproj` next
to the linked target and runs `dotnet build` on it.  The .NET SDK
produces:

* `<MainModule>.dll`             -- the managed assembly,
* `<MainModule>.exe`             -- the apphost / native launcher
                                    (renamed from the bare `<MainModule>`
                                    on Linux and macOS to match
                                    Mercury's csharp_executable convention),
* `<MainModule>.runtimeconfig.json`,
* `<MainModule>.deps.json`,

plus the standard `bin/` and `obj/` MSBuild scratch directories.  The
referenced Mercury standard-library assemblies (`mer_std.dll` and so
on) are copied next to the executable via the SDK's
`<Private>true</Private>` reference setting, so no `MONO_PATH`,
wrapper script or GAC registration is required.

You can run the resulting program directly:

```
    ./hello.exe
```

Trimmed publish
---------------

The generated `<MainModule>.csproj` for libraries carries
`<IsTrimmable>true</IsTrimmable>`, and the hand-written runtime and
standard-library C# code uses no name-based reflection that would defeat
the IL-linker.  Downstream consumers can therefore add their own
`<PublishTrimmed>true</PublishTrimmed>` and `dotnet publish` Mercury
applications without losing functionality.  `mmc` itself does not
invoke `dotnet publish`.

Native AOT publishing
---------------------

The `--csharp-aot` option flips a `csharp_executable` build from
`dotnet build` to `dotnet publish -p:PublishAot=true -r <rid>`.  The
generated csproj adds `<PublishAot>true</PublishAot>`,
`<SelfContained>true</SelfContained>`, `<InvariantGlobalization>true
</InvariantGlobalization>` and a `<RuntimeIdentifier>` derived from
Mercury's target architecture (e.g. `aarch64-w64-mingw32` -> `win-arm64`,
`x86_64-pc-linux-gnu` -> `linux-x64`).  `<PublishDir>` is forced to `./`
so the produced native binary lands next to the csproj where Mercury
expects it, identical to the regular build flow.  No `<MainModule>.dll`,
`runtimeconfig.json` or `deps.json` companions are emitted; the apphost
is the entire program.

The option is opt-in and the user owns the AOT-cleanliness contract:

* No module reachable from the program's `main/2` may import
  `type_desc`, `construct`, `deconstruct` or `term_to_xml`, nor call
  the generic forms of `io.write/3` or `compare_representation/3`.
  Such uses require runtime reflection, which the AOT compiler trims.
* Every linked Mercury library (the standard library and any `-l`
  reference) must have been built AOT-compatible.

If the target architecture cannot be mapped to a .NET RID, the build
falls back to a regular `dotnet build` and prints a notice to the
progress stream.  Trim or AOT warnings from `dotnet publish` surface
as a non-zero exit and abort the link step exactly like a normal C#
compilation error.

For `csharp_library` targets, `--csharp-aot` only adds
`<IsAotCompatible>true</IsAotCompatible>` to the generated csproj as
a marker for downstream consumers; the build itself is still a regular
`dotnet build`, and `mmc` does not run `dotnet publish` on libraries.

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

2. `store.arg_ref/5`    
   `store.new_arg_ref/5`

    Due to some limitations in RTTI support, dynamic type checking is
    missing for these predicates.  They should be used with care.

3. `deconstruct.functor_number/3`

    Not implemented; the C backend implements this through a header
    inclusion that is not portable to C#, and the C# RTTI layer does
    not yet expose an equivalent functor-number lookup.

4. `exception.catch_impl/3` for the `semidet` and `cc_nondet` modes.

    Currently throws `Sorry, not implemented'.  The `det`, `cc_multi`
    and `multi` modes are implemented.

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

4. Build the dependencies using the following command:

       mmake --use-mmc-make depend GRADE=csharp

5. Compile using the following command:

       mmake --use-mmc-make GRADE=csharp

6. To install the C# version of the compiler, do:

       mmake --use-mmc-make install GRADE=csharp

The C# version of the compiler MUST be built using `mmake`'s `--use-mmc-make`
option; the build will not work otherwise. Setting the variable `GRADE` in the
invocations of mmake is currently required to avoid some variable definition
ordering problems in `Mmake.workspace`.

-----------------------------------------------------------------------------
