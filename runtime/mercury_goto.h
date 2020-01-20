// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1995-2001, 2003-2007, 2009, 2011-2012 The University of Melbourne.
// Copyright (C) 2015-2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_goto.h - definitions for the "portable assembler" non-local gotos

#ifndef MERCURY_GOTO_H
#define MERCURY_GOTO_H

#include "mercury_conf.h"
#include "mercury_std.h"    // for MR_PASTE2 and MR_STRINGIFY
#include "mercury_types.h"  // for `MR_Code *'
#include "mercury_debug.h"  // for MR_debuggoto()
#include "mercury_label.h"  // for MR_insert_{entry,internal}_label()
#include "mercury_dummy.h"  // for MR_dummy_identify_function()

////////////////////////////////////////////////////////////////////////////

// Definitions for constructing the names of entry and internal labels,
// and the names of their layout structures.

#define MR_entry(label)     MR_PASTE2(_entry_,label)
#define MR_skip(label)      MR_PASTE2(skip_,label)

#define MR_add_prefix(label)                                             \
    MR_PASTE2(mercury__, label)

#define MR_label_name(proc_label, label)                                 \
    MR_PASTE3(proc_label, _i, label)

#define MR_proc_entry_user_name_base(mod, name, arity, mode)             \
    MR_PASTE7(mod, __, name, _, arity, _, mode)
#define MR_proc_entry_uci_name_base(mod, name, type, arity, mode)        \
    MR_PASTE9(name, _, mod, __, type, _, arity, _, mode)

#define MR_proc_entry_user_name(mod, name, arity, mode)                  \
    MR_PASTE2(mercury__,                                                 \
        MR_proc_entry_user_name_base(mod, name, arity, mode))
#define MR_proc_entry_uci_name(mod, name, type, arity, mode)             \
    MR_PASTE2(mercury__,                                                 \
        MR_proc_entry_uci_name_base(mod, name, type, arity, mode))

#define MR_proc_entry_user_name_str(mod, name, arity, mode)              \
    MR_STRINGIFY(MR_proc_entry_user_name(mod, name, arity, mode))
#define MR_proc_entry_uci_name_str(mod, name, type, arity, mode)         \
    MR_STRINGIFY(MR_proc_entry_uci_name(mod, name, type, arity, mode))

#define MR_proc_layout_user_name(mod, name, arity, mode)                 \
    MR_PASTE2(mercury_data__proc_layout__,                               \
        MR_proc_entry_user_name(mod, name, arity, mode))
#define MR_proc_layout_uci_name(mod, name, type, arity, mode)            \
    MR_PASTE2(mercury_data__proc_layout__,                               \
        MR_proc_entry_uci_name(mod, name, type, arity, mode))

#define MR_label_user_name_base(mod, name, a, mode, num)                 \
    MR_label_name(MR_proc_entry_user_name_base(mod, name, a, mode),      \
        num)
#define MR_label_uci_name_base(mod, name, type, a, mode, num)            \
    MR_label_name(MR_proc_entry_uci_name_base(mod, name, type, a, mode), \
        num)

#define MR_label_user_name(mod, name, a, mode, num)                      \
    MR_label_name(MR_proc_entry_user_name(mod, name, a, mode),           \
        num)
#define MR_label_uci_name(mod, name, type, a, mode, num)                 \
    MR_label_name(MR_proc_entry_uci_name(mod, name, type, a, mode),      \
        num)

#define MR_label_user_name_str(mod, name, arity, mode, num)              \
    MR_STRINGIFY(MR_label_user_name(mod, name, arity, mode, num))
#define MR_label_uci_name_str(mod, name, type, arity, mode, num)         \
    MR_STRINGIFY(MR_label_uci_name(mod, name, type, arity, mode, num))

#define MR_label_layout_user_name(mod, name, a, mode, num)               \
    MR_PASTE2(mercury_data__label_layout__,                              \
        MR_label_user_name(mod, name, a, mode, num))
#define MR_label_layout_uci_name(mod, name, type, a, mode, num)          \
    MR_PASTE2(mercury_data__label_layout__,                              \
        MR_label_uci_name(mod, name, type, a, mode, num))

////////////////////////////////////////////////////////////////////////////

#define MR_PROC_LAYOUT_NAME(label)                                       \
    MR_PASTE2(mercury_data__proc_layout__,label)
#define MR_LABEL_LAYOUT_NAME(label)                                      \
    MR_PASTE2(mercury_data__label_layout__,label)
#define MR_USER_LAYOUT_NAME(label)                                       \
    MR_PASTE2(mercury_data__user_event_layout__,label)

#define MR_PROC_LAYOUT(label)                                            \
    ((const MR_ProcLayout *) (MR_Word) &MR_PROC_LAYOUT_NAME(label))
#define MR_LABEL_LAYOUT(label)                                           \
    ((const MR_LabelLayout *) (MR_Word) &MR_LABEL_LAYOUT_NAME(label))

#define MR_PROC_LAYOUT1(label)                                           \
    MR_PROC_LAYOUT(MR_add_prefix(label)),

#define MR_LABEL_LAYOUT1(e, ln1)                                         \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln1)),

#define MR_LABEL_LAYOUT2(e, ln1, ln2)                                    \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln1)),               \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln2)),

#define MR_LABEL_LAYOUT3(e, ln1, ln2, ln3)                               \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln1)),               \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln2)),               \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln3)),

#define MR_LABEL_LAYOUT4(e, ln1, ln2, ln3, ln4)                          \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln1)),               \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln2)),               \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln3)),               \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln4)),

#define MR_LABEL_LAYOUT5(e, ln1, ln2, ln3, ln4, ln5)                     \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln1)),               \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln2)),               \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln3)),               \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln4)),               \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln5)),

#define MR_LABEL_LAYOUT6(e, ln1, ln2, ln3, ln4, ln5, ln6)                \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln1)),               \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln2)),               \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln3)),               \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln4)),               \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln5)),               \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln6)),

#define MR_LABEL_LAYOUT7(e, ln1, ln2, ln3, ln4, ln5, ln6, ln7)           \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln1)),               \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln2)),               \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln3)),               \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln4)),               \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln5)),               \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln6)),               \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln7)),

#define MR_LABEL_LAYOUT8(e, ln1, ln2, ln3, ln4, ln5, ln6, ln7, ln8)      \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln1)),               \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln2)),               \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln3)),               \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln4)),               \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln5)),               \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln6)),               \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln7)),               \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln8)),

#define MR_LABEL_LAYOUT9(e, ln1, ln2, ln3, ln4, ln5, ln6, ln7, ln8, ln9) \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln1)),               \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln2)),               \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln3)),               \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln4)),               \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln5)),               \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln6)),               \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln7)),               \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln8)),               \
    MR_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln9)),

#define MR_DECL_LABEL_LAYOUT(label)                                      \
    static const MR_LabelLayout MR_LABEL_LAYOUT_NAME(label);

#define MR_DECL_LABEL_LAYOUT1(e, ln1)                                    \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln1))

#define MR_DECL_LABEL_LAYOUT2(e, ln1, ln2)                               \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln1))           \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln2))

#define MR_DECL_LABEL_LAYOUT3(e, ln1, ln2, ln3)                          \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln1))           \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln2))           \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln3))

#define MR_DECL_LABEL_LAYOUT4(e, ln1, ln2, ln3, ln4)                     \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln1))           \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln2))           \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln3))           \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln4))

#define MR_DECL_LABEL_LAYOUT5(e, ln1, ln2, ln3, ln4, ln5)                \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln1))           \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln2))           \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln3))           \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln4))           \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln5))

#define MR_DECL_LABEL_LAYOUT6(e, ln1, ln2, ln3, ln4, ln5, ln6)           \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln1))           \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln2))           \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln3))           \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln4))           \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln5))           \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln6))

#define MR_DECL_LABEL_LAYOUT7(e, ln1, ln2, ln3, ln4, ln5, ln6, ln7)      \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln1))           \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln2))           \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln3))           \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln4))           \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln5))           \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln6))           \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln7))

#define MR_DECL_LABEL_LAYOUT8(e, ln1, ln2, ln3, ln4, ln5, ln6, ln7, ln8) \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln1))           \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln2))           \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln3))           \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln4))           \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln5))           \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln6))           \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln7))           \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln8))

#define MR_DECL_LABEL_LAYOUT9(e, ln1, ln2, ln3, ln4, ln5, ln6, ln7, ln8, ln9) \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln1))                \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln2))                \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln3))                \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln4))                \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln5))                \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln6))                \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln7))                \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln8))                \
    MR_DECL_LABEL_LAYOUT(MR_label_name(MR_add_prefix(e), ln9))

////////////////////////////////////////////////////////////////////////////

// Passing the name of a label to MR_insert_{internal,entry}_label
// causes that name to be included in the executable as static readonly data.
// Since label names are quite big, we include them only when needed.

#if defined(MR_INSERT_INTERNAL_LABEL_NAMES)
  #define MR_insert_internal(n, a, l)   MR_insert_internal_label(             \
                                            (const struct MR_LabelLayout *) n,\
                                            a, l)
#else
  #define MR_insert_internal(n, a, l)   MR_insert_internal_label(NULL, a, l)
#endif

#if defined(MR_INSERT_ENTRY_LABEL_NAMES)
  #define MR_insert_entry(n, a, l)  MR_insert_entry_label(n, a, l)
#else
  #define MR_insert_entry(n, a, l)  MR_insert_entry_label(NULL, a, l)
#endif

////////////////////////////////////////////////////////////////////////////

// Taking the address of a label can inhibit gcc's optimization, because it
// assumes that anything can jump there. Therefore we want to do it only if
// we are debugging, or if we need the label address for profiling or for
// accurate garbage collection.
//
// The versions of the macros below with the _ai, _an or _sl suffix always
// insert the label into the label table, the difference between them being
// that the _ai and _an variants do not include a layout structure. If the
// label *has* a layout structure, use the _sl variant. The difference between
// the _ai and the _an variants is that the latter always inserts the name
// of the label as well. This is intended for a small number of labels that
// are frequently needed in debugging, e.g. MR_do_fail.

#define MR_make_label_ai(n, a, l)   MR_insert_internal(                   \
                                        (const struct MR_LabelLayout *) n,\
                                        a, NULL)
#define MR_make_label_an(n, a, l)   MR_insert_internal_label(n, a, NULL)
#define MR_make_label_sl(n, a, l)   MR_insert_internal(n, a, l)

#define MR_make_local_ai(n, a, l)   MR_insert_entry(n, a, NULL)
#define MR_make_local_an(n, a, l)   MR_do_insert_entry_label(n, a, NULL)
#define MR_make_local_sl(n, a, l)   MR_insert_entry(n, a, MR_PROC_LAYOUT(l))

#define MR_make_entry_ai(n, a, l)   MR_insert_entry(n, a, NULL)
#define MR_make_entry_an(n, a, l)   MR_do_insert_entry_label(n, a, NULL)
#define MR_make_entry_sl(n, a, l)   MR_insert_entry(n, a, MR_PROC_LAYOUT(l))

////////////////////////////////////////////////////////////////////////////

#if defined(MR_INSERT_LABELS)
  #define MR_make_label(n, a, l)        MR_make_label_ai(n, a, l)
#else
  #define MR_make_label(n, a, l)        // nothing
#endif

#if defined(MR_INSERT_LABELS) || defined(MR_MPROF_PROFILE_CALLS)
  #define MR_make_local(n, a, l)        MR_make_local_ai(n, a, l)
#else
  #define MR_make_local(n, a, l)        // nothing
#endif

// Note that for the MLDS back-end, the calls to MR_init_entry(), which
// eventually expand to MR_make_entry(), are only output if the right
// compiler options are enabled. So if you change the condition of this
// `#ifdef', and you want your changes to apply to the MLDS back-end too,
// you may also need to change the `need_to_init_entries' predicate
// in compiler/mlds_to_c.m.

#if defined(MR_INSERT_LABELS) || defined(MR_MPROF_PROFILE_CALLS)
  #define MR_make_entry(n, a, l)        MR_make_entry_ai(n, a, l)
#else
  #define MR_make_entry(n, a, l)        // nothing
#endif

#ifdef MR_SPLIT_C_FILES
  #define MR_MODULE_STATIC_OR_EXTERN extern
#else
  #define MR_MODULE_STATIC_OR_EXTERN static
#endif

////////////////////////////////////////////////////////////////////////////

// MACHINE SPECIFIC STUFF REQUIRED FOR NON-LOCAL GOTOS

#if defined(__alpha__)

  // We need special handling for the "global pointer" (gp) register.

  // When doing a jump, we need to set $27, the "procedure value" register,
  // to the address we are jumping to, so that we can use an `ldgp'
  // instruction on entry to the procedure to set up the right gp value.

  #define MR_ASM_JUMP(address)                                           \
    __asm__("bis %0, %0, $27\n\t"                                        \
        : : "r"(address) : "$27");                                       \
    goto *(address)
    // Explanation:
    //  Move `address' to register $27,
    //  jump to `address'.

  // On entry to a procedure, we need to load the $gp register
  // with the correct value relative to the current address in $27.

  #define MR_INLINE_ASM_FIXUP_REGS                                       \
    "   ldgp $gp, 0($27)\n" : : : "memory"

  // On fall-thru, we need to skip the ldgp instruction.

  #define MR_ASM_FALLTHROUGH(label)                                      \
    goto MR_skip(label);

#elif defined(__i386__) || defined(__mc68000__) || defined(__x86_64__)

  // This hack is to prevent gcc 4.x optimizing away stores to succip before
  // jumping to a label in asm_fast.

  #if defined(MR_USE_GCC_GLOBAL_REGISTERS)
    #define MR_FORCE_SUCCIP_STORE                                        \
    __asm__ __volatile__(""                                              \
        : "=r"(MR_succip_word)                                           \
        : "0"(MR_succip_word)                                            \
        )
  #else
    #define MR_FORCE_SUCCIP_STORE   ((void) 0)
  #endif

  // The following hack works around a stack leak on the i386.
  // (and apparently the 68000 and x86_64 too).
  //
  // The problem is that gcc pushes function parameters onto the stack
  // when calling C functions such as GC_malloc(), and only restores
  // the stack pointer in the epilogue. With non-local gotos, we jump out
  // of the function without executing its epilogue, so the stack pointer
  // never gets restored. The result is a memory leak; for example,
  // `mmc --generate-dependencies mercury_compile' exceeds the Slackware Linux
  // default stack space limit of 8M.
  //
  // GNU C has an option `-fno-defer-pop' which is supposed to avoid this
  // sort of thing, but it doesn't work for our code using non-local gotos.
  //
  // We work around this using the dummy assembler code below, which pretends
  // to use the stack pointer, forcing gcc to flush any updates of the stack
  // pointer immediately, rather than deferring them until the function
  // epilogue.
  //
  // I know this is awful. It wasn't _my_ idea to use non-local gotos ;-)

  #if defined(__i386__)
    #define MR_ASM_JUMP(label)                                           \
    { register int stack_pointer __asm__("esp");                         \
    __asm__("" : : "g"(stack_pointer)); }                                \
    MR_FORCE_SUCCIP_STORE;                                               \
    goto *(label)
  #elif defined(__x86_64__)
    #define MR_ASM_JUMP(label)                                           \
    { register int stack_pointer __asm__("rsp");                         \
    __asm__("" : : "g"(stack_pointer)); }                                \
    MR_FORCE_SUCCIP_STORE;                                               \
    goto *(label)
  #elif defined(__mc68000__)
    #define MR_ASM_JUMP(label)                                           \
    { register int stack_pointer __asm__("sp");                          \
    __asm__("" : : "g"(stack_pointer)); }                                \
    goto *(label)
  #endif

  // That hack above needs to be done for all non-local jumps,
  // even if we are not using assembler labels.

  #define MR_JUMP(label)    MR_ASM_JUMP(label)

  // If we are using position-independent code, then we need to set up
  // the correct value of the GOT register (ebx on 386 or a5 on m68000).

  #if MR_PIC

    // At each entry point, where we may have been jump to from code in a
    // different C file, we need to set up `ebx'. We do this by pushing
    // the IP register using a `call' instruction whose target is the
    // very next label. We then pop this off the stack into `ebx', and
    // then use the value obtained to compute the correct value of `ebx'
    // by doing something with _GLOBAL_OFFSET_TABLE_ (I don't understand
    // the details exactly, this code is basically copied from the output
    // of `gcc -fpic -S'.) Note that `0f' means the label `0:' following
    // the current instruction, and `0b' means the label `0:' before
    // the current instruction.
    //
    // Note: this code clobbers `ebx', which is a callee-save register.
    // That means that it is essential that call_engine() save `ebx'
    // before entering Mercury code, and restore it before returning to C code.
    // However, gcc and/or setjmp()/longjmp() will do that for us
    // automatically, precisely because it is a callee-save register.
    //
    // This does not work with GCC 5+ because GCC no longer reserves `ebx'
    // to hold the GOT address; the code we jump into may expect to find
    // the GOT address in a different location.
    //

    #if defined(__i386__)

      #define MR_EBX "%%ebx"

      #define MR_INLINE_ASM_FIXUP_REGS                                   \
    "   call 0f\n"                                                       \
    "0:\n"                                                               \
    "   popl " MR_EBX "\n"                                               \
    "   addl $_GLOBAL_OFFSET_TABLE_+[.-0b]," MR_EBX "\n\t"               \
        : :
#if 0
    // The following doesn't seem to be necessary, and leaving it out
    // might make gcc generate slightly better code.

        // tell gcc we clobber ebx and memory
        : : : "%ebx", "memory"
#endif
    #elif defined(__mc68000__)

    // This piece of magic thanks to Roman Hodek
    // <Roman.Hodek@informatik.uni-erlangen.de>

      #define MR_INLINE_ASM_FIXUP_REGS                                   \
        "       lea (%%pc,_GLOBAL_OFFSET_TABLE_@GOTPC),%%a5\n" : : : "memory"

    #endif

    // It is safe to fall through into MR_INLINE_ASM_FIXUP_REGS,
    // but it might be more efficient to branch past it.
    // We should really measure both ways and find out which is
    // better... for the moment, we just fall through, since
    // that keeps the code smaller.

    #if 0
      #define MR_ASM_FALLTHROUGH(label)                                  \
    goto MR_skip(label);
    #endif

  #endif // MR_PIC

  // For Linux-ELF shared libraries, we need to declare that the type of labels
  // is @function (i.e. code, not data), otherwise the dynamic linker seems
  // to get confused, and we end up jumping into the data section.
  // Hence the `.type' directive below.

  #ifdef __ELF__
    #define MR_INLINE_ASM_ENTRY_LABEL_TYPE(label)                        \
    "   .type _entry_" MR_STRINGIFY(label) ",@function\n"
  #endif

#elif defined (__sparc)

  // If we are using position-independent code on the SPARC, then we need to
  // set up the correct value of the GOT register (l7).

  #if (defined(__pic__) || defined(__PIC__)) && !defined(MR_PIC)
    #define MR_PIC 1
  #endif

  #if MR_PIC

    // At each entry point, where we may have been jump to from
    // code in a difference C file, we need to set up `l7'.
    // We do this by getting the value the of the IP register using a `call'
    // instruction whose target is the very next label; this will
    // put the address of the call instruction in register `o7'.
    // We then use the value obtained in register `o7' to compute the correct
    // value of register `l7' by doing something with _GLOBAL_OFFSET_TABLE_
    // (I don't understand the details exactly, this code is
    // basically copied from the output of `gcc -fpic -S'.)
    // Note that `1f' means the label `1:' following the current instruction,
    // and `0b' means the label `0:' before the current instruction.

    #define MR_INLINE_ASM_FIXUP_REGS                                     \
    "0:\n"                                                               \
    "   call 1f\n"                                                       \
    "   nop\n"                                                           \
    "1:\n"                                                               \
    "   sethi %%hi(_GLOBAL_OFFSET_TABLE_-(0b-.)),%%l7\n"                 \
    "   or %%l7,%%lo(_GLOBAL_OFFSET_TABLE_-(0b-.)),%%l7\n"               \
    "   add %%l7,%%o7,%%l7\n"                                            \
        /* tell gcc we clobber l7, o7, and memory */                     \
        : : : "%l7", "%o7", "memory"

    // It is safe to fall through into MR_INLINE_ASM_FIXUP_REGS,
    // but it might be more efficient to branch past it.
    // We should really measure both ways and find out which is better...
    // for the moment, we just fall through, since that keeps the code smaller.

    #if 0
      #define MR_ASM_FALLTHROUGH(label)                                  \
    goto MR_skip(label);
    #endif

  #endif // MR_PIC

  // For Solaris 5.5.1, we need to declare that the type of labels is
  // #function (i.e. code, not data), otherwise the dynamic linker seems
  // to get confused, and we end up jumping into the data section.
  // Hence the `.type' directive below.

  #ifndef MR_CANNOT_GROK_ASM_TYPE_DIRECTIVE
    #define MR_INLINE_ASM_ENTRY_LABEL_TYPE(label)                        \
    "   .type _entry_" MR_STRINGIFY(label) ",#function\n"
  #endif

#elif defined(__arm__)

   // The following code for supporting non-local gotos and PIC on ARM
   // is thanks to Sergey Khorev <iamphet@nm.ru>.

  #if MR_PIC

     // At each entry point, where we may have been jumped to from code
     // in a different C file, we need to set up the PIC register.
     // We do this by adding _GLOBAL_OFFSET_TABLE_ value to the PC register
     // Current instruction address is (PC - 8) hence -(0b + 8)
     // (I don't understand the details exactly, this code is basically copied
     // from the output of `gcc -fpic -S' for a function containing setjmp.)
     // A little bit more detail can be obtained from GCC source
     // (function arm_finalize_pic in gcc/config/arm/arm.c)
     // Note that `0f' means the label `0:' following the current instruction,
     // and `0b' means the label `0:' before the current instruction.
     // GCC doesn't like it when we clobber the PIC register but it seems
     // we can safely keep GCC uninformed because we just do GCC's work
     // and the PIC register is saved over calls.
     // Loading arbitrary immediate values into ARM registers is not possible
     // directly hence the .word directive in the code section and the branch
     // instruction bypassing it.
     // If you change MR_ARM_PIC_REG, update CFLAGS_FOR_PIC (-mpic-register)
     // in configure.in.

    #define MR_ARM_PIC_REG "sl"

    #define MR_INLINE_ASM_FIXUP_REGS                                     \
       "        ldr " MR_ARM_PIC_REG ", 1f\n"                            \
       "0:\n"                                                            \
       "        add " MR_ARM_PIC_REG ", pc, " MR_ARM_PIC_REG "\n"        \
       "        b 2f\n"                                                  \
       "1:\n"                                                            \
       "        .word   _GLOBAL_OFFSET_TABLE_-(0b+8)\n"                  \
       "2:\n"

     // For Linux-ELF shared libraries, we need to declare that the type of
     // labels is #function (i.e. code, not data), otherwise the dynamic
     // linker seems to get confused, and we end up jumping into the data
     // section. Hence the `.type' directive below.

    #ifdef __ELF__
      #define MR_INLINE_ASM_ENTRY_LABEL_TYPE(label)                      \
        " .type _entry_" MR_STRINGIFY(label) ",#function\n"
    #endif

     // Save a few clock ticks branching past MR_INLINE_ASM_FIXUP_REGS.

    #define MR_ASM_FALLTHROUGH(label)                                    \
      goto MR_skip(label);

  #endif

#endif

////////////////////////////////////////////////////////////////////////////

// For other architectures, these macros have default definitions.

// MR_INLINE_ASM_FIXUP_REGS is used to fix up the value of
// any registers after an MR_ASM_JUMP to an entry point, if necessary.
// It is an assembler string, possibly followed by `: : : <clobbers>'
// where <clobbers> is an indication to gcc of what gets clobbered.

#ifdef MR_INLINE_ASM_FIXUP_REGS
  #define MR_ASM_FIXUP_REGS                                              \
    __asm__ __volatile__(                                                \
        MR_INLINE_ASM_FIXUP_REGS                                         \
    );
#else
  #define MR_ASM_FIXUP_REGS
  #define MR_INLINE_ASM_FIXUP_REGS
#endif

// MR_ASM_FALLTHROUGH is executed when falling through into an entry point.
// It may call `goto MR_skip(label)' if it wishes to skip past the
// label and the MR_INLINE_ASM_FIXUP_REGS code.

#ifndef MR_ASM_FALLTHROUGH
#define MR_ASM_FALLTHROUGH(label)
#endif

// MR_INLINE_ASM_GLOBALIZE_LABEL is an assembler string to declare
// an entry label as global. The following definition using `.globl'
// should work with the GNU assembler and with most Unix assemblers.

#ifndef MR_INLINE_ASM_GLOBALIZE_LABEL
#define MR_INLINE_ASM_GLOBALIZE_LABEL(label)                             \
    "   .globl _entry_" MR_STRINGIFY(label) "\n"
#endif

// MR_INLINE_ASM_ENTRY_LABEL_TYPE is an assembler string to declare the "type"
// of a label as function (i.e. code), not data, if this is needed.

#ifndef MR_INLINE_ASM_ENTRY_LABEL_TYPE
#define MR_INLINE_ASM_ENTRY_LABEL_TYPE(label)
#endif

// MR_INLINE_ASM_ENTRY_LABEL is an assembler string to define
// an assembler entry label.

#ifndef MR_INLINE_ASM_ENTRY_LABEL
#define MR_INLINE_ASM_ENTRY_LABEL(label)                                 \
    "_entry_" MR_STRINGIFY(label) ":\n"
#endif

// MR_INLINE_ASM_COMMENT is an assembler string to
// define an assembler comment.

#ifndef MR_INLINE_ASM_COMMENT
#define MR_INLINE_ASM_COMMENT(label)                                     \
    "// " MR_STRINGIFY(label) "\n"
#endif

// MR_ASM_JUMP is used to jump to an entry point defined with
// MR_ASM_ENTRY, MR_ASM_STATIC_ENTRY, or MR_ASM_LOCAL_ENTRY.

#ifndef MR_ASM_JUMP
#define MR_ASM_JUMP(address)    goto *(address)
#endif

////////////////////////////////////////////////////////////////////////////

// The code from here on should be architecture-independent.

////////////////////////////////////////////////////////////////////////////

// MR_ASM_ENTRY is used to declare an external entry point
// using a (global) assembler label.

#define MR_ASM_ENTRY(label)                                              \
    MR_ASM_FALLTHROUGH(label)                                            \
    MR_entry(label):                                                     \
    __asm__ __volatile__(                                                \
        MR_INLINE_ASM_GLOBALIZE_LABEL(label)                             \
        MR_INLINE_ASM_ENTRY_LABEL_TYPE(label)                            \
        MR_INLINE_ASM_ENTRY_LABEL(label)                                 \
        MR_INLINE_ASM_FIXUP_REGS                                         \
    );                                                                   \
    MR_skip(label): ;

// MR_ASM_STATIC_ENTRY is the same as MR_ASM_ENTRY,
// except that its scope is local to a C file, rather than global.
// Note that even static entry points must do MR_INLINE_ASM_FIXUP_REGS,
// since although there won't be any direct calls to them from another C file,
// their address may be taken and so there may be indirect calls.

#define MR_ASM_STATIC_ENTRY(label)                                       \
    MR_ASM_FALLTHROUGH(label)                                            \
    MR_entry(label):                                                     \
    __asm__ __volatile__(                                                \
        MR_INLINE_ASM_ENTRY_LABEL_TYPE(label)                            \
        MR_INLINE_ASM_ENTRY_LABEL(label)                                 \
        MR_INLINE_ASM_FIXUP_REGS                                         \
    );                                                                   \
    MR_skip(label): ;

// MR_ASM_LOCAL_ENTRY is the same as MR_ASM_ENTRY,
// except that its scope is local to a MR_BEGIN_MODULE...MR_END_MODULE
// block, rather than being global.
// Note that even local entry points must do MR_INLINE_ASM_FIXUP_REGS, since
// although there won't be any direct calls to them from another
// C file, their address may be taken and so there may be indirect calls.
//
// We need to use MR_ASM_COMMENT here to ensure that the code following
// each label is distinct; this is needed to ensure GCC doesn't try to merge
// two labels which point to identical code, but for which we have different
// information stored in the stack layout structs, etc.

#define MR_ASM_LOCAL_ENTRY(label)                                        \
    MR_ASM_FALLTHROUGH(label)                                            \
    MR_entry(label):                                                     \
    __asm__ __volatile__(                                                \
        MR_INLINE_ASM_COMMENT(label)                                     \
        MR_INLINE_ASM_FIXUP_REGS                                         \
    );                                                                   \
    MR_skip(label): ;

////////////////////////////////////////////////////////////////////////////

#if defined(MR_USE_GCC_NONLOCAL_GOTOS)

  #ifndef MR_GNUC
  #error "You must use gcc if you define MR_USE_GCC_NONLOCAL_GOTOS"
  #endif

  // Define the type of a module initialization function
  typedef void MR_ModuleFunc(void);

  // The following macro expands to a dummy assembler statement which
  // contains no code, but which tells gcc that it uses the specified address
  // as an input value. This is used to trick gcc into thinking that
  // the address is used, in order to suppress unwanted optimizations.
  // (We used to use `volatile_global_pointer = address' to suppress
  // optimization, but this way is better because it doesn't generate
  // any code.)

  #define MR_PRETEND_ADDRESS_IS_USED(address)                            \
    __asm__ __volatile__("" : : "g"(address))
  // Explanation:
  //   __asm__
  //   __volatile__     Don't optimize this asm away.
  //   (
  //       ""           Empty assembler code.
  //       :            No outputs.
  //       : "g" (address)
  //                    One input value, `address'; "g" means that
  //                    it can go in any general-purpose register.
  //   )

  // The following macro expands into a dummy assembler statement that
  // contains no code. It is used to suppress optimizations in gcc 4
  // and above that try to cache memory values in registers before labels
  // that Mercury uses as the target of non-local gotos. It has no effect
  // with versions of gcc before version 4.

  #define MR_PRETEND_REGS_ARE_USED                                       \
    __asm__ __volatile("":::"memory")
  // Explanation:
  //   __asm__
  //   __volatile__     Don't optimize this asm away.
  //   (
  //       ""           Empty assembler code.
  //       :            No outputs.
  //       :            No inputs.
  //       :"memory"    Tells gcc that this "instruction" might clobber
  //                    the register contents so it shouldn't cache
  //                    memory values in registers.
  //   )

  // Since we are jumping into and out of the middle of functions,
  // we need to make sure that gcc thinks that (1) the function's address
  // is used (otherwise it may optimize the whole function away) and
  // (2) the `return' statement is reachable (otherwise its dataflow
  // analysis for delay slot scheduling may think that global
  // register variables which are only assigned to in the function
  // cannot be live, when in fact they really are).
  // That is what the two occurrences of the MR_PRETEND_ADDRESS_IS_USED
  // macro are for.
  // For versions of gcc later than egcs 1.1.2 (which corresponds to gcc 2.91,
  // according to __GNUC_MINOR__), and in particular for gcc 2.95,
  // we also need to include at least one `goto *' with an unknown
  // target, so that gcc doesn't optimize away all the labels
  // because it thinks they are unreachable.
  // The MR_dummy_identify_function() function just returns the address
  // passed to it, so `goto *MR_dummy_identify_function(&& dummy_label);
  // dummy_label:' is the same as `goto dummy_label; label:', i.e. it just
  // falls through. For older versions of gcc, we don't do this, since it
  // adds significantly to the code size.

  #if MR_GNUC > 2 || (MR_GNUC == 2 && __GNUC_MINOR__ > 91)
    // gcc version > egcs 1.1.2
    #define MR_BEGIN_MODULE(module_name)                                 \
    MR_MODULE_STATIC_OR_EXTERN void module_name(void);                   \
    MR_MODULE_STATIC_OR_EXTERN void module_name(void) {                  \
        MR_PRETEND_ADDRESS_IS_USED(module_name);                         \
        MR_PRETEND_ADDRESS_IS_USED(                                      \
            &&MR_PASTE2(module_name,_dummy_label));                      \
        goto *MR_dummy_identify_function(                                \
            &&MR_PASTE2(module_name,_dummy_label));                      \
        MR_PASTE2(module_name,_dummy_label):                             \
        {
  #else // gcc version <= egcs 1.1.2
    #define MR_BEGIN_MODULE(module_name)                                 \
    MR_MODULE_STATIC_OR_EXTERN void module_name(void);                   \
    MR_MODULE_STATIC_OR_EXTERN void module_name(void) {                  \
        MR_PRETEND_ADDRESS_IS_USED(module_name);                         \
        MR_PRETEND_ADDRESS_IS_USED(                                      \
            &&MR_PASTE2(module_name,_dummy_label));                      \
        MR_PASTE2(module_name,_dummy_label):                             \
        {
  #endif // gcc version <= egcs 1.1.2

  // Initialization code for module goes between MR_BEGIN_MODULE
  // and MR_BEGIN_CODE.
  #define MR_BEGIN_CODE } return; {
  // Body of module goes between MR_BEGIN_CODE and MR_END_MODULE.
  #define MR_END_MODULE } }

  #if defined(MR_USE_ASM_LABELS)
    #define MR_declare_entry(label)                                      \
    extern void label(void) __asm__("_entry_" MR_STRINGIFY(label))
    #define MR_declare_static(label)                                     \
    static void label(void) __asm__("_entry_" MR_STRINGIFY(label))
    #define MR_define_extern_entry(label)   MR_declare_entry(label)
    #define MR_define_entry(label)                                       \
        MR_ASM_ENTRY(label)                                              \
    }                                                                    \
    label:                                                               \
    MR_PRETEND_ADDRESS_IS_USED(&&MR_entry(label));                       \
    {                                                                    \
    MR_PRETEND_REGS_ARE_USED;
    #define MR_define_static(label)                                      \
        MR_ASM_STATIC_ENTRY(label)                                       \
    }                                                                    \
    label:                                                               \
    MR_PRETEND_ADDRESS_IS_USED(&&MR_entry(label));                       \
    {                                                                    \
    MR_PRETEND_REGS_ARE_USED;
    // The MR_PRETEND_ADDRESS_IS_USED macro is necessary to prevent
    // an over-zealous gcc from optimizing away `label' and the code that
    // followed. The MR_PRETEND_REGS_ARE_USED macro is used to prevent gcc
    // from caching values in registers before the label.

    #define MR_init_entry(label)                                         \
    MR_PRETEND_ADDRESS_IS_USED(&&label);                                 \
    MR_make_entry(MR_STRINGIFY(label), label, label)
    #define MR_init_entry_ai(label)                                      \
    MR_PRETEND_ADDRESS_IS_USED(&&label);                                 \
    MR_make_entry_ai(MR_STRINGIFY(label), label, label)
    #define MR_init_entry_an(label)                                      \
    MR_PRETEND_ADDRESS_IS_USED(&&label);                                 \
    MR_make_entry_an(MR_STRINGIFY(label), label, label)
    #define MR_init_entry_sl(label)                                      \
    MR_PRETEND_ADDRESS_IS_USED(&&label);                                 \
    MR_make_entry_sl(MR_STRINGIFY(label), label, label)

    #define MR_pretend_address_is_used(label)                            \
    MR_PRETEND_ADDRESS_IS_USED(&&label);
    #define MR_entry_addr_wrapper(label)

    #define MR_ENTRY(label)     (&label)

    #ifndef MR_JUMP
    #define MR_JUMP(label)      MR_ASM_JUMP(label)
    #endif

  #else
    // !defined(MR_USE_ASM_LABELS)

    #define MR_declare_entry(label) extern MR_Code * MR_entry(label)
    #define MR_declare_static(label)    static MR_Code * MR_entry(label)
    #define MR_define_extern_entry(label) MR_Code * MR_entry(label)
    #define MR_define_entry(label)                                       \
    }                                                                    \
    MR_entry(label):                                                     \
    label:                                                               \
    {
    #define MR_define_static(label)                                      \
    }                                                                    \
    MR_entry(label):                                                     \
    label:                                                               \
    {
    #define MR_init_entry(label)                                         \
    MR_make_entry(MR_STRINGIFY(label), &&label, label);                  \
    MR_entry(label) = &&label
    #define MR_init_entry_ai(label)                                      \
    MR_make_entry_ai(MR_STRINGIFY(label), &&label, label);               \
    MR_entry(label) = &&label
    #define MR_init_entry_an(label)                                      \
    MR_make_entry_an(MR_STRINGIFY(label), &&label, label);               \
    MR_entry(label) = &&label
    #define MR_init_entry_sl(label)                                      \
    MR_make_entry_sl(MR_STRINGIFY(label), &&label, label);               \
    MR_entry(label) = &&label
    #define MR_ENTRY(label)     (MR_entry(label))

    #ifndef MR_JUMP
    #define MR_JUMP(label)      goto *(label)
    #endif

  #endif // !defined(MR_USE_ASM_LABELS)

  #define MR_declare_local(label)   // no declaration required
  #define MR_define_local(label)                                         \
        MR_ASM_LOCAL_ENTRY(label)                                        \
    }                                                                    \
    label:                                                               \
    {
  #define MR_init_local(label)                                           \
    MR_make_local(MR_STRINGIFY(label), &&MR_entry(label), label)
  #define MR_init_local_ai(label)                                        \
    MR_make_local_ai(MR_STRINGIFY(label), &&MR_entry(label), label)
  #define MR_init_local_an(label)                                        \
    MR_make_local_an(MR_STRINGIFY(label), &&MR_entry(label), label)
  #define MR_init_local_sl(label)                                        \
    MR_make_local_sl(MR_STRINGIFY(label), &&MR_entry(label), label)

  #define MR_define_label(label)    MR_define_local(label)
  #define MR_declare_label(label)   // no declaration required

  #define MR_init_label(label)                                           \
    MR_make_label(MR_STRINGIFY(label), &&MR_entry(label),                \
        MR_LABEL_LAYOUT(label))
  #define MR_init_label_ai(label)                                        \
    MR_make_label_ai(MR_STRINGIFY(label), &&MR_entry(label),             \
        MR_LABEL_LAYOUT(label))
  #define MR_init_label_an(label)                                        \
    MR_make_label_an(MR_STRINGIFY(label), &&MR_entry(label),             \
        MR_LABEL_LAYOUT(label))
  #define MR_init_label_sl(label)                                        \
    MR_make_label_sl(MR_STRINGIFY(label), &&MR_entry(label),             \
        MR_LABEL_LAYOUT(label))

  #define MR_init_label_ml(label, layout)                                \
    MR_make_label(MR_STRINGIFY(label), &&MR_entry(label), layout)
  #define MR_init_label_ml_ai(label, layout)                             \
    MR_make_label_ai(MR_STRINGIFY(label), &&MR_entry(label), layout)
  #define MR_init_label_ml_an(label, layout)                             \
    MR_make_label_an(MR_STRINGIFY(label), &&MR_entry(label), layout)
  #define MR_init_label_ml_sl(label, layout)                             \
    MR_make_label_sl(MR_STRINGIFY(label), &&MR_entry(label), layout)

  #define MR_LOCAL(label)   (&&MR_entry(label))
  #define MR_LABEL(label)   (&&MR_entry(label))
  #define MR_GOTO(label)    do {                                         \
                    MR_debuggoto(label);                                 \
                    MR_JUMP(label);                                      \
                } while (0)
  #define MR_GOTO_ENTRY(label)  MR_GOTO(MR_ENTRY(label))
  #define MR_GOTO_LOCAL(label)  MR_GOTO_LABEL(label)
  #define MR_GOTO_LABEL(label)  do {                                     \
                    MR_debuggoto(MR_LABEL(label));                       \
                    goto label;                                          \
                } while (0)

  // MR_GOTO_LABEL(label) is the same as MR_GOTO(MR_LABEL(label)) except that
  // it may allow gcc to generate slightly better code.

#else
  // !defined(MR_USE_GCC_NONLOCAL_GOTOS)

  // Define the type of a module initialization function.
  typedef MR_Code * MR_ModuleFunc(void);

  #define MR_BEGIN_MODULE(module_name)                                   \
    MR_MODULE_STATIC_OR_EXTERN MR_Code * module_name(void);              \
    MR_MODULE_STATIC_OR_EXTERN MR_Code * module_name(void) {
  #define MR_BEGIN_CODE         return 0;
  #define MR_END_MODULE         }

  #define MR_declare_entry(label)       extern MR_Code *label(void)
  #define MR_declare_static(label)      static MR_Code *label(void)
  #define MR_define_extern_entry(label)     MR_Code *label(void)
  #define MR_define_entry(label)                                         \
        MR_GOTO_LABEL(label);                                            \
    }                                                                    \
    MR_Code* label(void) {
  #define MR_define_static(label)                                        \
        MR_GOTO_LABEL(label);                                            \
    }                                                                    \
    static MR_Code* label(void) {

  #define MR_init_entry(label)                                           \
    MR_make_entry(MR_STRINGIFY(label), label, label)
  #define MR_init_entry_ai(label)                                        \
    MR_make_entry_ai(MR_STRINGIFY(label), label, label)
  #define MR_init_entry_an(label)                                        \
    MR_make_entry_an(MR_STRINGIFY(label), label, label)
  #define MR_init_entry_sl(label)                                        \
    MR_make_entry_sl(MR_STRINGIFY(label), label, label)

  #define MR_declare_local(label)   static MR_Code *label(void)
  #define MR_define_local(label)                                         \
        MR_GOTO_LABEL(label);                                            \
    }                                                                    \
    static MR_Code* label(void) {

  #define MR_init_local(label)                                           \
    MR_make_local(MR_STRINGIFY(label),    label, label)
  #define MR_init_local_ai(label)                                        \
    MR_make_local_ai(MR_STRINGIFY(label), label, label)
  #define MR_init_local_an(label)                                        \
    MR_make_local_an(MR_STRINGIFY(label), label, label)
  #define MR_init_local_sl(label)                                        \
    MR_make_local_sl(MR_STRINGIFY(label), label, label)

  #define MR_declare_label(label)   static MR_Code *label(void)
  #define MR_define_label(label)                                         \
        MR_GOTO_LABEL(label);                                            \
    }                                                                    \
    static MR_Code* label(void) {

  #define MR_init_label(label)                                           \
    MR_make_label(MR_STRINGIFY(label),    label, MR_LABEL_LAYOUT(label))
  #define MR_init_label_ai(label)                                        \
    MR_make_label_ai(MR_STRINGIFY(label), label, MR_LABEL_LAYOUT(label))
  #define MR_init_label_an(label)                                        \
    MR_make_label_an(MR_STRINGIFY(label), label, MR_LABEL_LAYOUT(label))
  #define MR_init_label_sl(label)                                        \
    MR_make_label_sl(MR_STRINGIFY(label), label, MR_LABEL_LAYOUT(label))

  #define MR_init_label_ml(label, layout)                                \
    MR_make_label(MR_STRINGIFY(label),    label, layout)
  #define MR_init_label_ml_ai(label, layout)                             \
    MR_make_label_ai(MR_STRINGIFY(label), label, layout)
  #define MR_init_label_ml_an(label, layout)                             \
    MR_make_label_an(MR_STRINGIFY(label), label, layout)
  #define MR_init_label_ml_sl(label, layout)                             \
    MR_make_label_sl(MR_STRINGIFY(label), label, layout)

  #define MR_ENTRY(label)   ((MR_Code *) (label))
  #define MR_LOCAL(label)   ((MR_Code *) (label))
  #define MR_LABEL(label)   ((MR_Code *) (label))
  // The call to MR_debuggoto() is in the driver function in mercury_engine.c,
  // which is why the following definitions have no MR_debuggoto().

  #define MR_GOTO(label)    return (label)
  #define MR_GOTO_ENTRY(label)  MR_GOTO(MR_ENTRY(label))
  #define MR_GOTO_LOCAL(label)  MR_GOTO(MR_LOCAL(label))
  #define MR_GOTO_LABEL(label)  MR_GOTO(MR_LABEL(label))

#endif // !defined(MR_USE_GCC_NONLOCAL_GOTOS)

////////////////////////////////////////////////////////////////////////////

#define MR_ENTRY_AP(label)      MR_ENTRY(MR_add_prefix(label))
#define MR_LOCAL_AP(label)      MR_LOCAL(MR_add_prefix(label))
#define MR_LABEL_AP(label)      MR_LABEL(MR_add_prefix(label))
#define MR_GOTO_AP(label)       MR_GOTO(MR_add_prefix(label))
#define MR_GOTO_ENT(label)      MR_GOTO_ENTRY(MR_add_prefix(label))
#define MR_GOTO_LOC(label)      MR_GOTO_LOCAL(MR_add_prefix(label))
#define MR_GOTO_LAB(label)      MR_GOTO_LABEL(MR_add_prefix(label))

#define MR_decl_user_entry(mod, name, arity, mode)                       \
    MR_declare_entry(MR_proc_entry_user_name(mod, name, arity, mode))
#define MR_decl_uci_entry(mod, name, type, arity, mode)                  \
    MR_declare_entry(MR_proc_entry_uci_name(mod, name, type, arity, mode))
#define MR_decl_user_static(mod, name, arity, mode)                      \
    MR_declare_static(MR_proc_entry_user_name(mod, name, arity, mode))
#define MR_decl_uci_static(mod, name, type, arity, mode)                 \
    MR_declare_static(MR_proc_entry_uci_name(mod, name, type, arity, mode))

#define MR_def_user_extern_entry(mod, name, arity, mode)                 \
    MR_define_extern_entry(                                              \
        MR_proc_entry_user_name(mod, name, arity, mode))
#define MR_def_uci_extern_entry(mod, name, type, arity, mode)            \
    MR_define_extern_entry(                                              \
        MR_proc_entry_uci_name(mod, name, type, arity, mode))
#define MR_def_user_entry(mod, name, arity, mode)                        \
    MR_define_entry(MR_proc_entry_user_name(mod, name, arity, mode))
#define MR_def_uci_entry(mod, name, type, arity, mode)                   \
    MR_define_entry(MR_proc_entry_uci_name(mod, name, type, arity, mode))
#define MR_def_user_static(mod, name, arity, mode)                       \
    MR_define_static(MR_proc_entry_user_name(mod, name, arity, mode))
#define MR_def_uci_static(mod, name, type, arity, mode)                  \
    MR_define_static(MR_proc_entry_uci_name(mod, name, type, arity, mode))

////////////////////////////////////////////////////////////////////////////

#if defined(MR_INSERT_LABELS) || defined(MR_MPROF_PROFILE_CALLS)
  #define MR_need_insert_entry(ai)      1
#else
  #define MR_need_insert_entry(ai)      ai
#endif

#if defined(MR_INSERT_LABELS)
  #define MR_need_insert_internal(ai)       1
#else
  #define MR_need_insert_internal(ai)       ai
#endif

#if defined(MR_INSERT_ENTRY_LABEL_NAMES)
  #define MR_need_entry_label_names(an)     1
#else
  #define MR_need_entry_label_names(an)     an
#endif

#if defined(MR_INSERT_INTERNAL_LABEL_NAMES)
  #define MR_need_internal_label_names(an)  1
#else
  #define MR_need_internal_label_names(an)  an
#endif

#define MR_init_entry_select(str, addr, layout, ai, an, sl)              \
    ( MR_need_insert_entry(ai) ?                                         \
        MR_insert_entry_label(                                           \
            MR_need_entry_label_names(an) ? str : NULL,                  \
            addr, sl ? layout : NULL)                                    \
        : (void) 0 )

#define MR_init_internal_select(str, addr, layout, ai, an, sl)           \
    ( MR_need_insert_internal(ai) ?                                      \
        MR_insert_internal_label(                                        \
            MR_need_internal_label_names(an) ? str : NULL,               \
            addr, sl ? layout : NULL)                                    \
        : (void) 0 )

#define MR_init_user_entry_select(mod, name, arity, mode, ai, an, sl)    \
    MR_pretend_address_is_used(                                          \
        MR_proc_entry_user_name(mod, name, arity, mode))                 \
    MR_init_entry_select(                                                \
        MR_proc_entry_user_name_str(mod, name, arity, mode),             \
        MR_entry_addr_wrapper(                                           \
            MR_proc_entry_user_name(mod, name, arity, mode)),            \
        MR_proc_layout_user_name(mod, name, arity, mode),                \
        ai, an, sl)

#define MR_init_uci_entry_select(mod, name, type, arity, mode, ai, an, sl) \
    MR_pretend_address_is_used(                                            \
        MR_proc_entry_uci_name(mod, name, type, arity, mode))              \
    MR_init_entry_select(                                                  \
        MR_proc_entry_uci_name_str(mod, name, type, arity, mode),          \
        MR_entry_addr_wrapper(                                             \
            MR_proc_entry_uci_name(mod, name, type, arity, mode)),         \
        MR_proc_layout_uci_name(mod, name, type, arity, mode),             \
        ai, an, sl)

#define MR_init_user_label_select(mod, name, arity, mode, num, ai, an, sl) \
    MR_init_label_select(                                                  \
        MR_label_user_name_str(mod, name, arity, mode, num),               \
        MR_entry_addr_wrapper(                                             \
            MR_label_user_name(mod, name, arity, mode, num)),              \
        MR_label_layout_user_name(mod, name, arity, mode, num),            \
        ai, an, sl)

#define MR_init_uci_label_select(mod, name, type, arity, mode, num, ai, an, sl)\
    MR_init_label_select(                                                      \
        MR_label_uci_name_str(mod, name, type, arity, mode, num),              \
        MR_entry_addr_wrapper(                                                 \
            MR_label_uci_name(mod, name, type, arity, mode, num)),             \
        MR_label_layout_uci_name(mod, name, type, arity, mode, num),           \
        ai, an, sl)

#define MR_init_user_entry_ai(mod, name, arity, mode)                    \
    MR_init_user_entry_select(mod, name, arity, mode, 1, 0, 0)
#define MR_init_user_entry_an(mod, name, arity, mode)                    \
    MR_init_user_entry_select(mod, name, arity, mode, 1, 1, 0)
#define MR_init_user_entry_sl(mod, name, arity, mode)                    \
    MR_init_user_entry_select(mod, name, arity, mode, 0, 0, 1)

#define MR_init_uci_entry_ai(mod, name, type, arity, mode)               \
    MR_init_uci_entry_select(mod, name, type, arity, mode, 1, 0, 0)
#define MR_init_uci_entry_an(mod, name, type, arity, mode)               \
    MR_init_uci_entry_select(mod, name, type, arity, mode, 1, 1, 0)
#define MR_init_uci_entry_sl(mod, name, type, arity, mode)               \
    MR_init_uci_entry_select(mod, name, type, arity, mode, 0, 0, 1)

#define MR_init_user_local_ai(mod, name, arity, mode, num)               \
    MR_init_user_local_select(mod, name, arity, mode, num, 1, 0, 0)
#define MR_init_user_local_an(mod, name, arity, mode, num)               \
    MR_init_user_local_select(mod, name, arity, mode, num, 1, 1, 0)
#define MR_init_user_local_sl(mod, name, arity, mode, num)               \
    MR_init_user_local_select(mod, name, arity, mode, num, 0, 0, 1)

#define MR_init_uci_local_ai(mod, name, type, arity, mode, num)          \
    MR_init_uci_local_select(mod, name, type, arity, mode, num, 1, 0, 0)
#define MR_init_uci_local_an(mod, name, type, arity, mode, num)          \
    MR_init_uci_local_select(mod, name, type, arity, mode, num, 1, 1, 0)
#define MR_init_uci_local_sl(mod, name, type, arity, mode, num)          \
    MR_init_uci_local_select(mod, name, type, arity, mode, num, 0, 0, 1)

#define MR_def_extern_entry(e)                                           \
    MR_define_extern_entry(MR_add_prefix(e));

#define MR_def_entry(e)                                                  \
    MR_define_entry(MR_add_prefix(e));

#define MR_def_static(e)                                                 \
    MR_define_static(MR_add_prefix(e));

#define MR_def_local(e)                                                  \
    MR_define_local(MR_add_prefix(e));

#define MR_def_label(e, ln)                                              \
    MR_define_label(MR_label_name(MR_add_prefix(e), ln));

#define MR_init_entry1(e)                                                \
    MR_init_entry(MR_add_prefix(e));

#define MR_init_entry1_sl(e)                                             \
    MR_init_entry_sl(MR_add_prefix(e));

#define MR_init_local1(e)                                                \
    MR_init_local(MR_add_prefix(e));

#define MR_init_local1_sl(e)                                             \
    MR_init_local_sl(MR_add_prefix(e));

#define MR_init_label1_sl(e)                                             \
    MR_init_label_sl(MR_add_prefix(e));

////////////////////////////////////////////////////////////////////////////

#define MR_init_label1(e, l1)                                            \
    MR_init_label(MR_label_name(MR_add_prefix(e), l1));

#define MR_init_label2(e, l1, l2)                                        \
    MR_init_label1(e, l1)                                                \
    MR_init_label1(e, l2)

#define MR_init_label3(e, l1, l2, l3)                                    \
    MR_init_label1(e, l1)                                                \
    MR_init_label1(e, l2)                                                \
    MR_init_label1(e, l3)

#define MR_init_label4(e, l1, l2, l3, l4)                                \
    MR_init_label1(e, l1)                                                \
    MR_init_label1(e, l2)                                                \
    MR_init_label1(e, l3)                                                \
    MR_init_label1(e, l4)

#define MR_init_label5(e, l1, l2, l3, l4, l5)                            \
    MR_init_label1(e, l1)                                                \
    MR_init_label1(e, l2)                                                \
    MR_init_label1(e, l3)                                                \
    MR_init_label1(e, l4)                                                \
    MR_init_label1(e, l5)

#define MR_init_label6(e, l1, l2, l3, l4, l5, l6)                        \
    MR_init_label1(e, l1)                                                \
    MR_init_label1(e, l2)                                                \
    MR_init_label1(e, l3)                                                \
    MR_init_label1(e, l4)                                                \
    MR_init_label1(e, l5)                                                \
    MR_init_label1(e, l6)

#define MR_init_label7(e, l1, l2, l3, l4, l5, l6, l7)                    \
    MR_init_label1(e, l1)                                                \
    MR_init_label1(e, l2)                                                \
    MR_init_label1(e, l3)                                                \
    MR_init_label1(e, l4)                                                \
    MR_init_label1(e, l5)                                                \
    MR_init_label1(e, l6)                                                \
    MR_init_label1(e, l7)

#define MR_init_label8(e, l1, l2, l3, l4, l5, l6, l7, l8)                \
    MR_init_label1(e, l1)                                                \
    MR_init_label1(e, l2)                                                \
    MR_init_label1(e, l3)                                                \
    MR_init_label1(e, l4)                                                \
    MR_init_label1(e, l5)                                                \
    MR_init_label1(e, l6)                                                \
    MR_init_label1(e, l7)                                                \
    MR_init_label1(e, l8)

#define MR_init_label9(e, l1, l2, l3, l4, l5, l6, l7, l8, l9)            \
    MR_init_label1(e, l1)                                                \
    MR_init_label1(e, l2)                                                \
    MR_init_label1(e, l3)                                                \
    MR_init_label1(e, l4)                                                \
    MR_init_label1(e, l5)                                                \
    MR_init_label1(e, l6)                                                \
    MR_init_label1(e, l7)                                                \
    MR_init_label1(e, l8)                                                \
    MR_init_label1(e, l9)

#define MR_init_label10(e, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10)      \
    MR_init_label1(e, l1)                                                \
    MR_init_label1(e, l2)                                                \
    MR_init_label1(e, l3)                                                \
    MR_init_label1(e, l4)                                                \
    MR_init_label1(e, l5)                                                \
    MR_init_label1(e, l6)                                                \
    MR_init_label1(e, l7)                                                \
    MR_init_label1(e, l8)                                                \
    MR_init_label1(e, l9)                                                \
    MR_init_label1(e, l10)

////////////////////////////////////////////////////////////////////////////

#define MR_init_label_sl1(e, l1)                                         \
    MR_init_label_sl(MR_label_name(MR_add_prefix(e), l1));

#define MR_init_label_sl2(e, l1, l2)                                     \
    MR_init_label_sl1(e, l1)                                             \
    MR_init_label_sl1(e, l2)

#define MR_init_label_sl3(e, l1, l2, l3)                                 \
    MR_init_label_sl1(e, l1)                                             \
    MR_init_label_sl1(e, l2)                                             \
    MR_init_label_sl1(e, l3)

#define MR_init_label_sl4(e, l1, l2, l3, l4)                             \
    MR_init_label_sl1(e, l1)                                             \
    MR_init_label_sl1(e, l2)                                             \
    MR_init_label_sl1(e, l3)                                             \
    MR_init_label_sl1(e, l4)

#define MR_init_label_sl5(e, l1, l2, l3, l4, l5)                         \
    MR_init_label_sl1(e, l1)                                             \
    MR_init_label_sl1(e, l2)                                             \
    MR_init_label_sl1(e, l3)                                             \
    MR_init_label_sl1(e, l4)                                             \
    MR_init_label_sl1(e, l5)

#define MR_init_label_sl6(e, l1, l2, l3, l4, l5, l6)                     \
    MR_init_label_sl1(e, l1)                                             \
    MR_init_label_sl1(e, l2)                                             \
    MR_init_label_sl1(e, l3)                                             \
    MR_init_label_sl1(e, l4)                                             \
    MR_init_label_sl1(e, l5)                                             \
    MR_init_label_sl1(e, l6)

#define MR_init_label_sl7(e, l1, l2, l3, l4, l5, l6, l7)                 \
    MR_init_label_sl1(e, l1)                                             \
    MR_init_label_sl1(e, l2)                                             \
    MR_init_label_sl1(e, l3)                                             \
    MR_init_label_sl1(e, l4)                                             \
    MR_init_label_sl1(e, l5)                                             \
    MR_init_label_sl1(e, l6)                                             \
    MR_init_label_sl1(e, l7)

#define MR_init_label_sl8(e, l1, l2, l3, l4, l5, l6, l7, l8)             \
    MR_init_label_sl1(e, l1)                                             \
    MR_init_label_sl1(e, l2)                                             \
    MR_init_label_sl1(e, l3)                                             \
    MR_init_label_sl1(e, l4)                                             \
    MR_init_label_sl1(e, l5)                                             \
    MR_init_label_sl1(e, l6)                                             \
    MR_init_label_sl1(e, l7)                                             \
    MR_init_label_sl1(e, l8)

#define MR_init_label_sl9(e, l1, l2, l3, l4, l5, l6, l7, l8, l9)         \
    MR_init_label_sl1(e, l1)                                             \
    MR_init_label_sl1(e, l2)                                             \
    MR_init_label_sl1(e, l3)                                             \
    MR_init_label_sl1(e, l4)                                             \
    MR_init_label_sl1(e, l5)                                             \
    MR_init_label_sl1(e, l6)                                             \
    MR_init_label_sl1(e, l7)                                             \
    MR_init_label_sl1(e, l8)                                             \
    MR_init_label_sl1(e, l9)

#define MR_init_label_sl10(e, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10)   \
    MR_init_label_sl1(e, l1)                                             \
    MR_init_label_sl1(e, l2)                                             \
    MR_init_label_sl1(e, l3)                                             \
    MR_init_label_sl1(e, l4)                                             \
    MR_init_label_sl1(e, l5)                                             \
    MR_init_label_sl1(e, l6)                                             \
    MR_init_label_sl1(e, l7)                                             \
    MR_init_label_sl1(e, l8)                                             \
    MR_init_label_sl1(e, l9)                                             \
    MR_init_label_sl1(e, l10)

////////////////////////////////////////////////////////////////////////////

#define MR_init_label_nvi1(e, m, l1, s1)                                 \
    MR_init_label_ml_sl(MR_label_name(MR_add_prefix(e), l1),             \
        &MR_no_var_label_layouts(m)[s1]);

#define MR_init_label_nvi2(e, m, l1,s1, l2,s2)                           \
    MR_init_label_nvi1(e, m, l1, s1)                                     \
    MR_init_label_nvi1(e, m, l2, s2)

#define MR_init_label_nvi3(e, m, l1,s1, l2,s2, l3,s3)                    \
    MR_init_label_nvi1(e, m, l1, s1)                                     \
    MR_init_label_nvi1(e, m, l2, s2)                                     \
    MR_init_label_nvi1(e, m, l3, s3)

#define MR_init_label_nvi4(e, m, l1,s1, l2,s2, l3,s3, l4,s4)             \
    MR_init_label_nvi1(e, m, l1, s1)                                     \
    MR_init_label_nvi1(e, m, l2, s2)                                     \
    MR_init_label_nvi1(e, m, l3, s3)                                     \
    MR_init_label_nvi1(e, m, l4, s4)

#define MR_init_label_nvi5(e, m, l1,s1, l2,s2, l3,s3, l4,s4, l5,s5)      \
    MR_init_label_nvi1(e, m, l1, s1)                                     \
    MR_init_label_nvi1(e, m, l2, s2)                                     \
    MR_init_label_nvi1(e, m, l3, s3)                                     \
    MR_init_label_nvi1(e, m, l4, s4)                                     \
    MR_init_label_nvi1(e, m, l5, s5)

#define MR_init_label_nvi6(e, m, l1,s1, l2,s2, l3,s3, l4,s4, l5,s5, l6,s6) \
    MR_init_label_nvi1(e, m, l1, s1)                                     \
    MR_init_label_nvi1(e, m, l2, s2)                                     \
    MR_init_label_nvi1(e, m, l3, s3)                                     \
    MR_init_label_nvi1(e, m, l4, s4)                                     \
    MR_init_label_nvi1(e, m, l5, s5)                                     \
    MR_init_label_nvi1(e, m, l6, s6)

#define MR_init_label_nvi7(e, m, l1,s1, l2,s2, l3,s3, l4,s4, l5,s5, l6,s6, l7,s7) \
    MR_init_label_nvi1(e, m, l1, s1)                                     \
    MR_init_label_nvi1(e, m, l2, s2)                                     \
    MR_init_label_nvi1(e, m, l3, s3)                                     \
    MR_init_label_nvi1(e, m, l4, s4)                                     \
    MR_init_label_nvi1(e, m, l5, s5)                                     \
    MR_init_label_nvi1(e, m, l6, s6)                                     \
    MR_init_label_nvi1(e, m, l7, s7\

#define MR_init_label_nvi8(e, m, l1,s1, l2,s2, l3,s3, l4,s4, l5,s5, l6,s6, l7,s7, l8,s8) \
    MR_init_label_nvi1(e, m, l1, s1)                                     \
    MR_init_label_nvi1(e, m, l2, s2)                                     \
    MR_init_label_nvi1(e, m, l3, s3)                                     \
    MR_init_label_nvi1(e, m, l4, s4)                                     \
    MR_init_label_nvi1(e, m, l5, s5)                                     \
    MR_init_label_nvi1(e, m, l6, s6)                                     \
    MR_init_label_nvi1(e, m, l7, s7)                                     \
    MR_init_label_nvi1(e, m, l8, s8)

#define MR_init_label_nvi9(e, m, l1,s1, l2,s2, l3,s3, l4,s4, l5,s5, l6,s6, l7,s7, l8,s8, l9,s9) \
    MR_init_label_nvi1(e, m, l1, s1)                                     \
    MR_init_label_nvi1(e, m, l2, s2)                                     \
    MR_init_label_nvi1(e, m, l3, s3)                                     \
    MR_init_label_nvi1(e, m, l4, s4)                                     \
    MR_init_label_nvi1(e, m, l5, s5)                                     \
    MR_init_label_nvi1(e, m, l6, s6)                                     \
    MR_init_label_nvi1(e, m, l7, s7)                                     \
    MR_init_label_nvi1(e, m, l8, s8)                                     \
    MR_init_label_nvi1(e, m, l9, s9)

#define MR_init_label_nvi10(e, m, l1,s1, l2,s2, l3,s3, l4,s4, l5,s5, l6,s6, l7,s7, l8,s8, l9,s9, l10,s10) \
    MR_init_label_nvi1(e, m, l1, s1)                                     \
    MR_init_label_nvi1(e, m, l2, s2)                                     \
    MR_init_label_nvi1(e, m, l3, s3)                                     \
    MR_init_label_nvi1(e, m, l4, s4)                                     \
    MR_init_label_nvi1(e, m, l5, s5)                                     \
    MR_init_label_nvi1(e, m, l6, s6)                                     \
    MR_init_label_nvi1(e, m, l7, s7)                                     \
    MR_init_label_nvi1(e, m, l8, s8)                                     \
    MR_init_label_nvi1(e, m, l9, s9)                                     \
    MR_init_label_nvi1(e, m, l10, s10)

////////////////////////////////////////////////////////////////////////////

#define MR_init_label_svi1(e, m, l1, s1)                                 \
    MR_init_label_ml_sl(MR_label_name(MR_add_prefix(e), l1),             \
        (MR_LabelLayout *) &MR_svar_label_layouts(m)[s1]);

#define MR_init_label_svi2(e, m, l1,s1, l2,s2)                           \
    MR_init_label_svi1(e, m, l1, s1)                                     \
    MR_init_label_svi1(e, m, l2, s2)

#define MR_init_label_svi3(e, m, l1,s1, l2,s2, l3,s3)                    \
    MR_init_label_svi1(e, m, l1, s1)                                     \
    MR_init_label_svi1(e, m, l2, s2)                                     \
    MR_init_label_svi1(e, m, l3, s3)

#define MR_init_label_svi4(e, m, l1,s1, l2,s2, l3,s3, l4,s4)             \
    MR_init_label_svi1(e, m, l1, s1)                                     \
    MR_init_label_svi1(e, m, l2, s2)                                     \
    MR_init_label_svi1(e, m, l3, s3)                                     \
    MR_init_label_svi1(e, m, l4, s4)

#define MR_init_label_svi5(e, m, l1,s1, l2,s2, l3,s3, l4,s4, l5,s5)      \
    MR_init_label_svi1(e, m, l1, s1)                                     \
    MR_init_label_svi1(e, m, l2, s2)                                     \
    MR_init_label_svi1(e, m, l3, s3)                                     \
    MR_init_label_svi1(e, m, l4, s4)                                     \
    MR_init_label_svi1(e, m, l5, s5)

#define MR_init_label_svi6(e, m, l1,s1, l2,s2, l3,s3, l4,s4, l5,s5, l6,s6) \
    MR_init_label_svi1(e, m, l1, s1)                                     \
    MR_init_label_svi1(e, m, l2, s2)                                     \
    MR_init_label_svi1(e, m, l3, s3)                                     \
    MR_init_label_svi1(e, m, l4, s4)                                     \
    MR_init_label_svi1(e, m, l5, s5)                                     \
    MR_init_label_svi1(e, m, l6, s6)

#define MR_init_label_svi7(e, m, l1,s1, l2,s2, l3,s3, l4,s4, l5,s5, l6,s6, l7,s7) \
    MR_init_label_svi1(e, m, l1, s1)                                     \
    MR_init_label_svi1(e, m, l2, s2)                                     \
    MR_init_label_svi1(e, m, l3, s3)                                     \
    MR_init_label_svi1(e, m, l4, s4)                                     \
    MR_init_label_svi1(e, m, l5, s5)                                     \
    MR_init_label_svi1(e, m, l6, s6)                                     \
    MR_init_label_svi1(e, m, l7, s7)

#define MR_init_label_svi8(e, m, l1,s1, l2,s2, l3,s3, l4,s4, l5,s5, l6,s6, l7,s7, l8,s8) \
    MR_init_label_svi1(e, m, l1, s1)                                     \
    MR_init_label_svi1(e, m, l2, s2)                                     \
    MR_init_label_svi1(e, m, l3, s3)                                     \
    MR_init_label_svi1(e, m, l4, s4)                                     \
    MR_init_label_svi1(e, m, l5, s5)                                     \
    MR_init_label_svi1(e, m, l6, s6)                                     \
    MR_init_label_svi1(e, m, l7, s7)                                     \
    MR_init_label_svi1(e, m, l8, s8)

#define MR_init_label_svi9(e, m, l1,s1, l2,s2, l3,s3, l4,s4, l5,s5, l6,s6, l7,s7, l8,s8, l9,s9) \
    MR_init_label_svi1(e, m, l1, s1)                                     \
    MR_init_label_svi1(e, m, l2, s2)                                     \
    MR_init_label_svi1(e, m, l3, s3)                                     \
    MR_init_label_svi1(e, m, l4, s4)                                     \
    MR_init_label_svi1(e, m, l5, s5)                                     \
    MR_init_label_svi1(e, m, l6, s6)                                     \
    MR_init_label_svi1(e, m, l7, s7)                                     \
    MR_init_label_svi1(e, m, l8, s8)                                     \
    MR_init_label_svi1(e, m, l9, s9)

#define MR_init_label_svi10(e, m, l1,s1, l2,s2, l3,s3, l4,s4, l5,s5, l6,s6, l7,s7, l8,s8, l9,s9, l10,s10) \
    MR_init_label_svi1(e, m, l1, s1)                                     \
    MR_init_label_svi1(e, m, l2, s2)                                     \
    MR_init_label_svi1(e, m, l3, s3)                                     \
    MR_init_label_svi1(e, m, l4, s4)                                     \
    MR_init_label_svi1(e, m, l5, s5)                                     \
    MR_init_label_svi1(e, m, l6, s6)                                     \
    MR_init_label_svi1(e, m, l7, s7)                                     \
    MR_init_label_svi1(e, m, l8, s8)                                     \
    MR_init_label_svi1(e, m, l9, s9)                                     \
    MR_init_label_svi1(e, m, l10, s10)

////////////////////////////////////////////////////////////////////////////

#define MR_init_label_lvi1(e, m, l1, s1)                                 \
    MR_init_label_ml_sl(MR_label_name(MR_add_prefix(e), l1),             \
        &MR_lvar_label_layouts(m)[s1]);

#define MR_init_label_lvi2(e, m, l1,s1, l2,s2)                           \
    MR_init_label_lvi1(e, m, l1, s1)                                     \
    MR_init_label_lvi1(e, m, l2, s2)

#define MR_init_label_lvi3(e, m, l1,s1, l2,s2, l3,s3)                    \
    MR_init_label_lvi1(e, m, l1, s1)                                     \
    MR_init_label_lvi1(e, m, l2, s2)                                     \
    MR_init_label_lvi1(e, m, l3, s3)

#define MR_init_label_lvi4(e, m, l1,s1, l2,s2, l3,s3, l4,s4)             \
    MR_init_label_lvi1(e, m, l1, s1)                                     \
    MR_init_label_lvi1(e, m, l2, s2)                                     \
    MR_init_label_lvi1(e, m, l3, s3)                                     \
    MR_init_label_lvi1(e, m, l4, s4)

#define MR_init_label_lvi5(e, m, l1,s1, l2,s2, l3,s3, l4,s4, l5,s5)      \
    MR_init_label_lvi1(e, m, l1, s1)                                     \
    MR_init_label_lvi1(e, m, l2, s2)                                     \
    MR_init_label_lvi1(e, m, l3, s3)                                     \
    MR_init_label_lvi1(e, m, l4, s4)                                     \
    MR_init_label_lvi1(e, m, l5, s5)

#define MR_init_label_lvi6(e, m, l1,s1, l2,s2, l3,s3, l4,s4, l5,s5, l6,s6) \
    MR_init_label_lvi1(e, m, l1, s1)                                     \
    MR_init_label_lvi1(e, m, l2, s2)                                     \
    MR_init_label_lvi1(e, m, l3, s3)                                     \
    MR_init_label_lvi1(e, m, l4, s4)                                     \
    MR_init_label_lvi1(e, m, l5, s5)                                     \
    MR_init_label_lvi1(e, m, l6, s6)

#define MR_init_label_lvi7(e, m, l1,s1, l2,s2, l3,s3, l4,s4, l5,s5, l6,s6, l7,s7) \
    MR_init_label_lvi1(e, m, l1, s1)                                     \
    MR_init_label_lvi1(e, m, l2, s2)                                     \
    MR_init_label_lvi1(e, m, l3, s3)                                     \
    MR_init_label_lvi1(e, m, l4, s4)                                     \
    MR_init_label_lvi1(e, m, l5, s5)                                     \
    MR_init_label_lvi1(e, m, l6, s6)                                     \
    MR_init_label_lvi1(e, m, l7, s7)

#define MR_init_label_lvi8(e, m, l1,s1, l2,s2, l3,s3, l4,s4, l5,s5, l6,s6, l7,s7, l8,s8) \
    MR_init_label_lvi1(e, m, l1, s1)                                     \
    MR_init_label_lvi1(e, m, l2, s2)                                     \
    MR_init_label_lvi1(e, m, l3, s3)                                     \
    MR_init_label_lvi1(e, m, l4, s4)                                     \
    MR_init_label_lvi1(e, m, l5, s5)                                     \
    MR_init_label_lvi1(e, m, l6, s6)                                     \
    MR_init_label_lvi1(e, m, l7, s7)                                     \
    MR_init_label_lvi1(e, m, l8, s8)

#define MR_init_label_lvi9(e, m, l1,s1, l2,s2, l3,s3, l4,s4, l5,s5, l6,s6, l7,s7, l8,s8, l9,s9) \
    MR_init_label_lvi1(e, m, l1, s1)                                     \
    MR_init_label_lvi1(e, m, l2, s2)                                     \
    MR_init_label_lvi1(e, m, l3, s3)                                     \
    MR_init_label_lvi1(e, m, l4, s4)                                     \
    MR_init_label_lvi1(e, m, l5, s5)                                     \
    MR_init_label_lvi1(e, m, l6, s6)                                     \
    MR_init_label_lvi1(e, m, l7, s7)                                     \
    MR_init_label_lvi1(e, m, l8, s8)                                     \
    MR_init_label_lvi1(e, m, l9, s9)

#define MR_init_label_lvi10(e, m, l1,s1, l2,s2, l3,s3, l4,s4, l5,s5, l6,s6, l7,s7, l8,s8, l9,s9, l10,s10) \
    MR_init_label_lvi1(e, m, l1, s1)                                     \
    MR_init_label_lvi1(e, m, l2, s2)                                     \
    MR_init_label_lvi1(e, m, l3, s3)                                     \
    MR_init_label_lvi1(e, m, l4, s4)                                     \
    MR_init_label_lvi1(e, m, l5, s5)                                     \
    MR_init_label_lvi1(e, m, l6, s6)                                     \
    MR_init_label_lvi1(e, m, l7, s7)                                     \
    MR_init_label_lvi1(e, m, l8, s8)                                     \
    MR_init_label_lvi1(e, m, l9, s9)                                     \
    MR_init_label_lvi1(e, m, l10, s10)

////////////////////////////////////////////////////////////////////////////

#define MR_decl_extern_entry(e)                                          \
    MR_declare_extern_entry(MR_add_prefix(e));

#define MR_decl_entry(e)                                                 \
    MR_declare_entry(MR_add_prefix(e));

#define MR_decl_static(e)                                                \
    MR_declare_static(MR_add_prefix(e));

#define MR_decl_local(e)                                                 \
    MR_declare_local(MR_add_prefix(e));

#define MR_decl_label1(e, l1)                                            \
    MR_declare_label(MR_label_name(MR_add_prefix(e), l1));

#define MR_decl_label2(e, l1, l2)                                        \
    MR_decl_label1(e, l1)                                                \
    MR_decl_label1(e, l2)

#define MR_decl_label3(e, l1, l2, l3)                                    \
    MR_decl_label1(e, l1)                                                \
    MR_decl_label1(e, l2)                                                \
    MR_decl_label1(e, l3)

#define MR_decl_label4(e, l1, l2, l3, l4)                                \
    MR_decl_label1(e, l1)                                                \
    MR_decl_label1(e, l2)                                                \
    MR_decl_label1(e, l3)                                                \
    MR_decl_label1(e, l4)

#define MR_decl_label5(e, l1, l2, l3, l4, l5)                            \
    MR_decl_label1(e, l1)                                                \
    MR_decl_label1(e, l2)                                                \
    MR_decl_label1(e, l3)                                                \
    MR_decl_label1(e, l4)                                                \
    MR_decl_label1(e, l5)

#define MR_decl_label6(e, l1, l2, l3, l4, l5, l6)                        \
    MR_decl_label1(e, l1)                                                \
    MR_decl_label1(e, l2)                                                \
    MR_decl_label1(e, l3)                                                \
    MR_decl_label1(e, l4)                                                \
    MR_decl_label1(e, l5)                                                \
    MR_decl_label1(e, l6)

#define MR_decl_label7(e, l1, l2, l3, l4, l5, l6, l7)                    \
    MR_decl_label1(e, l1)                                                \
    MR_decl_label1(e, l2)                                                \
    MR_decl_label1(e, l3)                                                \
    MR_decl_label1(e, l4)                                                \
    MR_decl_label1(e, l5)                                                \
    MR_decl_label1(e, l6)                                                \
    MR_decl_label1(e, l7)

#define MR_decl_label8(e, l1, l2, l3, l4, l5, l6, l7, l8)                \
    MR_decl_label1(e, l1)                                                \
    MR_decl_label1(e, l2)                                                \
    MR_decl_label1(e, l3)                                                \
    MR_decl_label1(e, l4)                                                \
    MR_decl_label1(e, l5)                                                \
    MR_decl_label1(e, l6)                                                \
    MR_decl_label1(e, l7)                                                \
    MR_decl_label1(e, l8)

#define MR_decl_label9(e, l1, l2, l3, l4, l5, l6, l7, l8, l9)            \
    MR_decl_label1(e, l1)                                                \
    MR_decl_label1(e, l2)                                                \
    MR_decl_label1(e, l3)                                                \
    MR_decl_label1(e, l4)                                                \
    MR_decl_label1(e, l5)                                                \
    MR_decl_label1(e, l6)                                                \
    MR_decl_label1(e, l7)                                                \
    MR_decl_label1(e, l8)                                                \
    MR_decl_label1(e, l9)

#define MR_decl_label10(e, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10)      \
    MR_decl_label1(e, l1)                                                \
    MR_decl_label1(e, l2)                                                \
    MR_decl_label1(e, l3)                                                \
    MR_decl_label1(e, l4)                                                \
    MR_decl_label1(e, l5)                                                \
    MR_decl_label1(e, l6)                                                \
    MR_decl_label1(e, l7)                                                \
    MR_decl_label1(e, l8)                                                \
    MR_decl_label1(e, l9)                                                \
    MR_decl_label1(e, l10)

////////////////////////////////////////////////////////////////////////////

// definitions for computed gotos

#define MR_COMPUTED_GOTO(val, labels)                                    \
    {                                                                    \
        static MR_Code *jump_table[] = {                                 \
            labels                                                       \
        };                                                               \
        MR_GOTO(jump_table[val]);                                        \
    }
#define MR_AND ,    // Used to separate the labels.

#endif // not MERCURY_GOTO_H

////////////////////////////////////////////////////////////////////////////
