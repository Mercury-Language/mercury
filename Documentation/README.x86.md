Mercury on x86
--------------

This file contains a note for users who are installing Mercury on x86 systems
that use ELF shared libraries. Note that the x86-64 is *not* affected by
the issue described below, only x86.

Due to improvements in GCC, the following combination is not supported:

- GCC version 5 and above
- x86
- PIC (position-independent code)
- Low-level C grades using non-local gotos, i.e. `asm_fast.*`

The configure script will not select `asm_fast` grades when the above
combination is detected. If you try to use that combination by selecting an
`asm_fast` grade manually, then you will encounter compilation errors.

We recommend using high-level C grades (`hlc.*`) on x86. If you require a
low-level C grade, e.g. for debugging, then you can use a `reg.*` grade.

If you must use an `asm_fast` grade, then you will need to tell the C compiler
not to generate position-independent code (which may be the default), *and*
disable building of shared libraries. Alternatively, you could downgrade to
GCC version 4.9.

Note that Windows is unaffected by this issue as Windows does not use PIC,
and we do not yet support shared libraries (DLLs) on Windows anyway.

See <https://bugs.mercurylang.org/view.php?id=453> for further details.
