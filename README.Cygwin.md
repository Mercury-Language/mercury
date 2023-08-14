Mercury on Cygwin
=================

This file documents the port of Mercury to Windows using
[Cygwin](https://www.cygwin.com) (i.e the `x86_64-pc-cygwin` configuration).

The x86 (32-bit) version of Cygwin has reached its end-of-life and
is no longer supported by Mercury.

Installations of Mercury on Cygwin will use the Cygwin port of the GNU C
compiler (GCC) by default. When using the Cygwin port of GCC, programs will
have a dependency on the Cygwin DLL (`cygwin1.dll`).

You may also use the Cygwin environment to build a Mercury installation that
uses an alternative C compiler, such as Microsoft Visual C.
(See [README.MS-VisualC.md](README.MS-VisualC.md) and
[README.MS-Windows.md](README.MS-Windows.md) for further details.)
