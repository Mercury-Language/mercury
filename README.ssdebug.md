Source-to-Source Debugger
=========================

The source-to-source debugger (`ssdebug`, `ssdb`) operates by performing a
high-level transformation of Mercury source code to provide a rudimentary
debugger interface. As such, it has numerous limitations (see below), but
can potentially work with all backends. It is mainly intended for when you
cannot use the Mercury debugger `mdb`, such as with the Java or high-level C
backends.

`ssdebug` is still an experimental feature.

Installation
------------

To use the source-to-source debugger you can install the grades containing the
`.ssdebug` grade component. One way to do this is to invoke `configure` with
the option `--enable-ssdebug-grades`. This will add the grades
`hlc.gc.ssdebug`, `csharp.ssdebug` and `java.ssdebug` to the set of library
grades to install.

Compilation
-----------

Compile your program in a grade with the `.ssdebug` grade component (e.g.
`java.ssdebug` or `hlc.gc.ssdebug`). Your entire program will be then compiled
with `--ssdb-trace` level of `deep`.

An alternative way is to use `mmc --make --link-ssdb-libs` to compile your
program in any grade. You then just need to set `--ssdb-trace shallow` or
`--ssdb-trace deep` on the modules you wish to debug.

Tracing Levels
--------------

1. `--ssdb-trace none`

   None of the procedures in the module will generate trace events.

2. `--ssdb-trace shallow`

   All the procedures in the interface of the module will generate events
   of trace level `shallow`.  Events of trace level `shallow` are only
   displayed if the parent procedure in the call stack is compiled in
   trace level `deep`.

3. `--ssdb-trace deep`

   All procedures in the module will generate events of trace level `deep`.

Using the source-to-source debugger
-----------------------------------

You may run the program as usual. To bring up the debugger prompt, set
the environment variable `SSDB` beforehand.

```
    $ SSDB=1 ./calculator
        1:      1  1    CALL calculator.main
    ssdb>
```

If you set `SSDB=0` then you will need to explicitly enable the debugger later
in your code by calling `ssdb.enable_debugging/2`.

As in `mdb`, the three numbers are the event number, call sequence number (CSN)
and the stack depth. Type `help` to show a list of commands. All commands act
like their `mdb` counterparts (with reduced functionality), except for `list`
which prints the source code at the call site, not of the called procedure.

The debugger will execute commands from `$HOME/.ssdbrc` and `.ssdbrc` in
the current working directory, in order, at startup. You can put your
aliases and settings in those files. Lines starting with the `#` character
will be ignored.

Programs built in `.ssdebug` grades use much more stack space (tail call
optimisation is prevented by the transformation). You will likely need to
increase the stack size, e.g.

```
    $ JAVA='java -Xss10m' SSDB=1 ./calculator
```

Limitations
-----------

* There are no internal events. The only events are CALL, EXIT, REDO, FAIL,
  and, for Java and C# grades, EXCP.

* Standard library procedures are treated specially. Events will only be
  generated at the boundaries at which a user procedure calls and returns
  from a standard library procedure. No events will be generated when a
  standard library procedure calls another standard library procedure.

* The `retry` command works by executing forwards until reaching the end of
  the call to retry, then recursively calling that procedure. Any side
  effects of continuing execution will be visible. If it is not possible to
  reach the end of the procedure to retry, the program will simply keep
  executing. Press ^C to get back the debugger prompt.

* When running on Mono 2.8 and earlier, ^C can cause the program to hang.
  This is fixed in later versions.

* Exceptions are only handled in Java and C# grades. Only a single EXCP event
  is generated when an exception is thrown, instead of propagating EXCP events
  up the call stack to the nearest exception handler.

* In grades which don't handle exceptions, the debugger's internal state will
  be inconsistent with the program's execution after an exception, so you had
  better quit the program and restart.

* Breakpoints currently match procedures by module and name only. Predicates
  or functions with the same name but different arities will still match.
  The debugger will not warn you if you set a breakpoint on a non-existent
  procedure.

* We provide the filename and line number of call sites, but not the location
  of the source code for the called procedure itself. Use `mtags`.

* The print goal command does not distinguish predicates and functions.

* Procedures with arguments which are neither `in` nor `out` will not be
  transformed, hence will not generate events when called.

* Many commands available in `mdb` are not yet implemented for `ssdebug`.

* There is no tab completion.

* There is no I/O tabling.

* Debugging of multi-threaded programs is not supported.

-----------------------------------------------------------------------------
