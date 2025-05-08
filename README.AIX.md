Mercury on AIX
==============

This file documents the port of Mercury to AIX.

Mercury was minimally tested on AIX in 2025 with the following configuration.

* AIX 7.3 on POWER9.
* GCC version 10.3.0
* High-level C grades (`hlc.gc`, `hlc.par.gc`).

Prerequisites
-------------

Install `dnf` using a script:

```
/usr/opt/perl5/bin/lwp-download \
    https://public.dhe.ibm.com/aix/freeSoftware/aixtoolbox/ezinstall/ppc/dnf_aixtoolbox.sh
chmod +x dnf_aixtoolbox.sh
./dnf_aixtoolbox.sh -d
```

Then install the necessary packages:

```
dnf install tar gcc make flex bison
```

Building with GCC
-----------------

GCC defaults to `-maix32`. To build for the 64-bit AIX ABI, it is easiest to
create a shell script containing:

```
#!/bin/sh
exec gcc -maix64 "$@"
```

Mark the script executable, then set up the environment:

```
export PATH=/opt/freeware/bin:$PATH
export CC=/path/to/gcc-maix64
export OBJECT_MODE=64
```

`OBJECT_MODE=64` is required if using `/usr/bin/ar` as the archive tool.
If you try to use GNU `ar` (from binutils) instead, you will run into problems
when `/usr/bin/ranlib` is invoked on archives created by GNU `ar`.

Add the following to `Mmake.params` before running `make`. This prevents `gcc`
overflowing the PowerPC TOC when creating the `mercury_compile` executable.

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
