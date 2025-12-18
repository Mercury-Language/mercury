Mercury on Linux / AArch64
==========================

This file documents the port of Mercury to Linux on AArch64 (ARM64)
(i.e. the `aarch64*-linux*` configuration).

Mercury should build and install "out-of-the-box" on Linux for AArch64.

Low-level C grades using non-local gotos, i.e. `asm_fast.*`, do not work
with GCC 9 or later. Non-PIC (position-independent code) actually does
still work, but PIC is commonly the default.
If you require a low-level C grade, please use a `reg.*` grade.
