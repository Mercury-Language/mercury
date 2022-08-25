Cross-compiling Mercury with GCC or clang
========================================

You can cross-compile the Mercury system with GCC or with clang so that the
Mercury installation will run on a different platform from your host system.

The following targets are currently supported using a GCC cross-compiler:

  * Windows 64-bit, using MinGW-w64
  * Windows 32-bit, using MinGW-w64 or MinGW32
  * Linux AArch64 (ARM64)

The following targets are currently supported using clang:

  * FreeBSD x86-64
  * macOS x86-64
  * macOS AArch64 (ARM64)

Furthermore, instead of transferring the Mercury installation to the target
system and running it there, you can augment the cross-compiled installation
with files from a native Mercury installation, allowing you to cross-compile
Mercury programs for the target system on the host system.

NOTE: At one stage, there were problems with the `asm_fast*` grade when
targeting Windows 64-bit with MinGW-w64. The `none*` and `reg*` grades did
appear to work correctly. This has not been checked recently. We suggest using
the `hlc` grades for production usage.

-----------------------------------------------------------------------------

Instructions for GCC
====================

 1. Install Mercury for the host system as usual using GCC.

 2. Install a GCC cross-compiler.
    On Debian/Ubuntu you might install one of these packages:

      - gcc-mingw-w64-x86-64
      - gcc-mingw-w64-i686
      - gcc-aarch64-linux-gnu

    Alternatively, you can use <http://mxe.cc/> to install a MinGW-w64
    toolchain on Unix-like hosts.

    Whatever the means, you should have the C cross-compiler in your PATH,
    e.g. /usr/bin/x86_64-w64-mingw32-gcc.

 3. Unpack a fresh copy of the Mercury source tree.
    Now, instead of running ./configure, run:

        tools/configure_cross --host=HOST [--with-cc=PATH] \
            <other configure arguments>

    The `--host` option is required. HOST is the "host triplet" of your
    cross-compiler, e.g. x86_64-w64-mingw32, i686-w64-mingw32, or
    aarch64-linux-gnu.

    The `--with-cc` option can be used to pass the path of your GCC
    cross-compiler. It is required if the cross-compiler is not called
    `HOST-gcc`, where HOST is the value of the `--host` option.

    Those two options must be appear first. Any later options are passed
    through to the configure script. A call to the `configure_cross` script
    might look like:

        tools/configure_cross \
            --host=x86_64-w64-mingw32 \
            --prefix=/usr/local/mercury-x86_64-w64-mingw32 \
            --enable-libgrades=hlc.gc

 4. Now you can install Mercury as usual, e.g.

        mmake depend
        mmake
        mmake install

-----------------------------------------------------------------------------

Instructions for clang
======================

 1. Install Mercury for the host system as usual using clang.

 2. Obtain a "sysroot" file system for your target system, that is,
    a logical root directory in which the compiler will search for headers and
    libraries.

    For FreeBSD, you can copy these directories from a running system:

        /lib /usr/include /usr/lib /usr/local/include /usr/local/lib

    Place the sysroot somewhere, e.g. ~/sysroots/x86_64-unknown-freebsd13.0

 3. Set the CC environment variable as follows:

        CC="clang -target TRIPLE --sysroot=SYSROOT-PATH"
        export CC

    where TRIPLE is the triple that specifies the target architecture
    (e.g. x86_64-unknown-freebsd13.0) and SYSROOT-PATH points to the sysroot
    directory.

    It may be convenient to create a shell script that invokes clang with those
    options; then you can set CC to refer to that shell script.

 4. Unpack a fresh copy of the Mercury source tree.
    Now, instead of running ./configure, run:

        tools/configure_cross --host=TRIPLE [--with-cc=COMMAND] \
            <other configure arguments>

    The `--host` option is required. TRIPLE is the target triple as above.

    The `--with-cc` option can be used to specify the clang command instead of
    setting the CC environment variable as described above.

    Those two options must be appear first. Any later options are passed
    through to the configure script. A call to the `configure_cross` script
    might look like:

        tools/configure_cross \
            --host=x86_64-unknown-freebsd13.0 \
            --prefix=/usr/local/mercury-x86_64-unknown-freebsd13.0 \
            --enable-libgrades=hlc.gc

 5. Now you can install Mercury as usual, e.g.

        mmake depend
        mmake
        mmake install

-----------------------------------------------------------------------------

Adapting the Mercury installation for use on the host system
============================================================

To use the cross-compiled Mercury installation on the host system,
you need to copy executables from a native Mercury installation's `bin`
directory to the cross-compiled Mercury installation's `bin` directory.
This can be done using the `tools/copy_mercury_binaries` script,
which is called like this:

    tools/copy_mercury_binaries SRC DEST

where SRC is the path to the native Mercury installation,
and DEST is the path to the cross-compiled Mercury installation.
The versions of the two Mercury installations should be the same,
or at least very similar.

Once that is done, you can use the `mmc` script from the cross-compiled Mercury
installation to compile Mercury programs for the target system, e.g.

    % /usr/local/mercury-x86_64-w64-mingw32/bin/mmc -m hello
    Making Mercury/int3s/hello.int3
    Making Mercury/ints/hello.int
    Making Mercury/cs/hello.c
    Making Mercury/os/hello.o
    Making hello.exe

-----------------------------------------------------------------------------
