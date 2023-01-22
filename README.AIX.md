Mercury on AIX
==============

This file documents the port of Mercury to AIX.

Mercury was tested on AIX in 2019 with the following configuration.

* AIX 7.1 on POWER9.
* GCC version 6.3.0.
* High-level C grades (`hlc.gc`, `hlc.par.gc`).

Prerequisites
-------------

Install `yum` by running this
[script](https://public.dhe.ibm.com/aix/freeSoftware/aixtoolbox/ezinstall/ppc/yum.sh).

Then install the necessary packages:

```
    $ yum install gcc make flex bison
```

Building with GCC
-----------------

GCC defaults to `-maix32`. To build for the 64-bit AIX ABI, it may be easiest to
create a shell script containing:

```
    exec gcc -maix64 "$@"
```

Mark the script executable and then run `configure` like this:

```
    $ CC=/path/to/gcc-maix64 ./configure [options]
```

If you are using `/usr/bin/ar` then you must also set an environment variable:

```
    $ export OBJECT_MODE=64
```

If you try to use GNU `ar` instead (from binutils) you will run into problems
when `/usr/bin/ranlib` is invoked on archives created by GNU `ar`.

The `mercury_compile` executable will overflow the PowerPC TOC with `gcc` in its
default configuration. One solution is to add this to `Mmake.params` before
running `make`:

```
    EXTRA_CFLAGS=-mminimal-toc
```

Your own programs may also overflow the TOC, depending on their size.

Building with IBM XL C
-----------------------

(Last tested in 2014.)

To use the IBM XL C compiler, pass `--with-cc=xlc` when configuring the Mercury
installation. Use high-level C grades for better performance.

Large programs will overflow the TOC. You can try passing the option
`-qpic=large` to the C compiler and the linker, or passing `-bbigtoc` to the
linker.

-----------------------------------------------------------------------------
