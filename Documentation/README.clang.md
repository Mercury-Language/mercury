Mercury with Clang
==================

This file documents how to use `clang` as the C compiler with Mercury.
Support for Mercury with `clang` has been tested on Linux, macOS and Windows.

To use `clang` as the C compiler for Mercury invoke Mercury's `configure`
script with the `--with-cc` option set as follows:

```
    $ ./configure --with-cc=clang
```

Note that the `asm_fast*` and `reg*` grades are not usable with `clang` as they
rely on GNU C extensions that clang does not provide.

Versions of LLVM (the underlying compiler infrastructure used by `clang`) before
about version 2.9 contain bugs that require most C compiler optimizations to be
disabled when compiling Mercury generated C code with `clang` (i.e. the
generated C files have to be compiled at `-O0`). If you are using a more recent
version of LLVM then the `configure` script will automatically enable a higher
level of optimization.
