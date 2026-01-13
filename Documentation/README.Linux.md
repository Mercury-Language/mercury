Mercury on Linux
================

This file documents the Linux/x86 and Linux/x86_64 ports.

For the Linux/AArch64 (ARM64) port, see
[README.Linux-aarch64.md](README.Linux-aarch64.md).

For the Linux/PowerPC port, see
[README.Linux-PPC.md](README.Linux-PPC.md).

(Ports to Linux on other architectures have not yet
been attempted, but should not be difficult.)

Building a 32-bit Mercury Compiler on 64-bit Linux
--------------------------------------------------

To build a 32-bit Mercury compiler on 64-bit Linux, follow these steps:

1. Ensure that the 32-bit (e.g. i686 etc) packages for the C compiler and
   libraries are available on the build system.

2. Ensure that you have a working Mercury compiler in your PATH
   to bootstrap from.

3. Run `./prepare.sh` and then invoke `configure` as follows:

       ./configure --host=i686-pc-linux-gnu --with-cc="gcc -m32"

4. Do `mmake; mmake install` as normal.
