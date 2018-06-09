// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1995-2006 The University of Melbourne.
// Copyright (C) 2014, 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_stacks.h - definitions for manipulating the det and nondet stacks.
//
// See compiler/notes/failure.html for documentation on the nondet stack
// frame handling.

#ifndef MERCURY_STACKS_H
#define MERCURY_STACKS_H

#include "mercury_regs.h"
#include "mercury_types.h"
#include "mercury_overflow.h"
#include "mercury_debug.h"
#include "mercury_overflow.h"
#include "mercury_goto.h"
#include "mercury_tabling.h"
#include "mercury_engine.h"

#ifdef  MR_STACK_FRAME_STATS
  #include "mercury_dword.h"

  extern MR_Dword   MR_det_frame_count;
  extern MR_Dword   MR_det_frame_total_size;
  extern MR_Word    *MR_det_frame_max;
  extern MR_Dword   MR_non_frame_count;
  extern MR_Dword   MR_non_frame_total_size;
  extern MR_Word    *MR_non_frame_max;

  // This temporary is for use in the MR_increment_dword_tmp macro only.
  // Making the temporary variable global (nonlocal to the macro) allows
  // the macro have the form of an expression, instead of a statement,
  // without relying on GNU extensions to C.

  extern MR_uint_least32_t MR_old_low_tmp;

  extern void       MR_init_stack_frame_stats(void);
  extern void       MR_print_stack_frame_stats(void);

  #define MR_collect_det_frame_stats(size)                                    \
    (                                                                         \
        MR_increment_dword_tmp(MR_det_frame_count, 1, MR_old_low_tmp),        \
        MR_increment_dword_tmp(MR_det_frame_total_size,                       \
            (size), MR_old_low_tmp),                                          \
        ((MR_sp > MR_det_frame_max) ?                                         \
            (MR_det_frame_max = MR_sp) : (void) 0)                            \
    )
  #define MR_collect_non_frame_stats(slots)                                   \
    (                                                                         \
        MR_increment_dword_tmp(MR_non_frame_count, 1, MR_old_low_tmp),        \
        MR_increment_dword_tmp(MR_non_frame_total_size,                       \
            (slots) + MR_NONDET_FIXED_SIZE, MR_old_low_tmp),                  \
        ((MR_maxfr > MR_non_frame_max) ?                                      \
            (MR_non_frame_max = MR_maxfr) : (void) 0)                         \
    )

#else   // !MR_STACK_FRAME_STATS

  #define MR_collect_det_frame_stats(size)      ((void) 0)
  #define MR_collect_non_frame_stats(slots)     ((void) 0)

#endif  // !MR_STACK_FRAME_STATS

////////////////////////////////////////////////////////////////////////////

#ifdef MR_STACK_SEGMENTS

  #define MR_detstack_extend_and_check(incr)                                  \
        do {                                                                  \
            MR_Word *new_sp;                                                  \
            MR_Word *threshold;                                               \
                                                                              \
            threshold = (MR_Word *) MR_CONTEXT(MR_ctxt_detstack_zone)->       \
                MR_zone_extend_threshold;                                     \
            new_sp = MR_sp + (incr);                                          \
            if (new_sp > threshold) {                                         \
                MR_save_registers();                                          \
                new_sp = MR_new_detstack_segment(MR_sp, (incr));              \
                MR_restore_registers();                                       \
                MR_succip_word = (MR_Word) MR_ENTRY(MR_pop_detstack_segment); \
            }                                                                 \
            MR_sp_word = (MR_Word) new_sp;                                    \
        } while (0)

  #define MR_detstack_extend_and_no_check(incr)                               \
        do {                                                                  \
            MR_sp_word = (MR_Word) (MR_sp + (incr));                          \
        } while (0)

  // Check whether there is room for a new frame whose size is incr words
  // in the current segment of the nondet stack, and if not, allocate
  // a new segment to hold it, and a sentinel frame at the bottom of that
  // new segment.
  //
  // The prevfr argument should be a variable that holds the value that
  // the caller proposes to assign to the prevfr field of the new stack frame.
  // If this macro finds it necessary to create a new segment, it will update
  // this variable to point to the sentinel frame, since *that* will be
  // the stack frame immediately before the new frame.
  //
  // The reason why we check for MR_maxfr < zone_min is that MR_maxfr may be
  // in a different stack segment than MR_CONTEXT(MR_ctxt_nondetstack_zone).
  // See the comment for MR_new_nondetstack_segment in mercury_stacks.c.

  #define MR_nondetstack_extend_and_check(prevfr, incr)                       \
        do {                                                                  \
            MR_Word *new_maxfr;                                               \
            MR_Word *threshold;                                               \
                                                                              \
            threshold = (MR_Word *) MR_CONTEXT(MR_ctxt_nondetstack_zone)->    \
                MR_zone_extend_threshold;                                     \
            new_maxfr = MR_maxfr + incr;                                      \
            if (MR_maxfr < MR_CONTEXT(MR_ctxt_nondetstack_zone->MR_zone_min)  \
                || new_maxfr > threshold)                                     \
            {                                                                 \
                MR_save_registers();                                          \
                new_maxfr = MR_new_nondetstack_segment(MR_maxfr, (incr));     \
                MR_restore_registers();                                       \
                prevfr = new_maxfr - (incr);                                  \
            }                                                                 \
            MR_maxfr_word = (MR_Word) new_maxfr;                              \
        } while (0)

  extern    MR_Word     *MR_new_detstack_segment(MR_Word *sp, int n);

  // This function reserves a stack frame of n words on the nondet stack,
  // after conceptually (a) freeing any nondet stack segments that are wholly
  // above old_maxfr, and (b) creating a new nondet stack segment.
  // In practice, it will reuse an existing memory zone, if one is available.
  // It returns the address that should be the new value of maxfr.

  extern    MR_Word     *MR_new_nondetstack_segment(MR_Word *old_maxfr, int n);

#else   // !MR_STACK_SEGMENTS

  #define MR_detstack_extend_and_check(incr)                                  \
        do {                                                                  \
            MR_sp_word = (MR_Word) (MR_sp + (incr));                          \
        } while (0)

  #define MR_detstack_extend_and_no_check(incr)                               \
        do {                                                                  \
            MR_sp_word = (MR_Word) (MR_sp + (incr));                          \
        } while (0)

  #define MR_nondetstack_extend_and_check(prevfr, incr)                       \
        do {                                                                  \
            MR_maxfr_word = (MR_Word) (MR_maxfr + (incr));                    \
        } while (0)

#endif  // MR_STACK_SEGMENTS

MR_declare_entry(MR_pop_detstack_segment);
MR_declare_entry(MR_pop_nondetstack_segment);

#ifdef  MR_EXTEND_STACKS_WHEN_NEEDED

  #define MR_detstack_post_extend_check()                                     \
    MR_IF (MR_sp >= MR_CONTEXT(MR_ctxt_detstack_zone)->MR_zone_end, (         \
        MR_extend_detstack()                                                  \
    ))
  #define MR_nondetstack_post_extend_check()                                  \
    MR_IF (MR_maxfr >= MR_CONTEXT(MR_ctxt_nondetstack_zone)->MR_zone_end, (   \
        MR_extend_nondetstack()                                               \
    ))

  extern    void            MR_extend_detstack(void);
  extern    void            MR_extend_nondetstack(void);

#else   // !MR_EXTEND_STACKS_WHEN_NEEDED

  #define MR_detstack_post_extend_check()       ((void) 0)
  #define MR_nondetstack_post_extend_check()    ((void) 0)

#endif  // MR_EXTEND_STACKS_WHEN_NEEDED

////////////////////////////////////////////////////////////////////////////

// DEFINITIONS FOR MANIPULATING THE DET STACK

// The first stack slot in each stack frame is MR_stackvar(1) while MR_sp
// points to the topmost used word on the stack: this is why we need to add 1.
// In compiler-generated code, the value of n is always known, so the C
// compiler will be able to compute 1-n at compile time. The debugger will
// need to perform the addition at runtime after it gets the value of n from
// a layout structure, but this is not performance critical. It is useful
// to have MR_sp and MR_maxfr both point to the topmost used words on their
// stacks when moving stack segments around in minimal model tabling.

#define MR_based_stackvar(base_sp, n)   ((base_sp)[1 - (n)])
#define MR_stackvar(n)                  MR_based_stackvar(MR_sp, (n))
#define MR_sv(n)                        MR_stackvar(n)
#define MR_parent_sv(n)                 MR_based_stackvar(MR_parent_sp, (n))

#define MR_incr_sp(n)                                                         \
    do {                                                                      \
        MR_debugincrsp(n, MR_sp);                                             \
        MR_detstack_extend_and_check(n);                                      \
        MR_detstack_post_extend_check();                                      \
        MR_detstack_overflow_check_msg("MR_incr_sp");                         \
        MR_collect_det_frame_stats(n);                                        \
    } while (0)

// We reserve MR_stack_margin_size_words words at the end of every det stack
// segment for leaf procedures whose stack frames fit into that many words.
// The check is done in compiler/llds_out_instr; you can find it if you
// search for uses of max_leaf_stack_frame_size.

#define MR_incr_sp_leaf(n)                                                    \
    do {                                                                      \
        MR_debugincrsp(n, MR_sp);                                             \
        MR_detstack_extend_and_no_check(n);                                   \
        MR_detstack_post_extend_check();                                      \
        MR_detstack_overflow_check_msg("MR_incr_sp_leaf");                    \
        MR_collect_det_frame_stats(n);                                        \
    } while (0)

#define MR_decr_sp(n)                                                         \
    do {                                                                      \
        MR_debugdecrsp(n, MR_sp);                                             \
        MR_sp_word = (MR_Word) (MR_sp - (n));                                 \
        MR_detstack_underflow_check_msg("MR_decr_sp");                        \
    } while (0)

#define MR_decr_sp_and_return(n)                                              \
    do {                                                                      \
        MR_Code *return_addr;                                                 \
                                                                              \
        return_addr = (MR_Word *) MR_stackvar(n);                             \
        MR_debugdecrsp(n, MR_sp);                                             \
        MR_sp_word = (MR_Word) (MR_sp - (n));                                 \
        MR_detstack_underflow_check_msg("MR_decr_sp_and_return");             \
        MR_debugproceed();                                                    \
        MR_GOTO(return_addr);                                                 \
    } while (0)

// The msg argument of MR_incr_sp_push_msg is not used at runtime. It is
// intended for use by tools/frame_sizes, which scans compiler-generated C
// source files.

#define MR_incr_sp_push_msg(n, msg) MR_incr_sp(n)

#define MR_decr_sp_pop_msg(n)       MR_decr_sp(n)

////////////////////////////////////////////////////////////////////////////

// DEFINITIONS FOR NONDET STACK FRAMES

#define MR_PREVFR       (-0)    // prev frame on stack, set up at call
#define MR_REDOIP       (-1)    // in this proc, set up at clause entry
#define MR_REDOFR       (-2)    // value for curfr on backtracking
#define MR_SUCCIP       (-3)    // in caller proc, set up at call
#define MR_SUCCFR       (-4)    // frame of caller proc, set up at call
#define MR_TMP_DETFR    (-3)    // sp, in model_det temp frames only
#define MR_TABLE_DETFR  (-5)    // sp, in minimal model main frames only

// This setup allows MR_USE_MINIMAL_MODEL_STACK_COPY_EXTRA_SLOT to be defined
// even if MR_USE_MINIMAL_MODEL_STACK_COPY isn't, which can be useful for
// performance testing.

#ifdef  MR_USE_MINIMAL_MODEL_STACK_COPY
  #define MR_USE_MINIMAL_MODEL_STACK_COPY_EXTRA_SLOT
#endif

// MR_Code that traverses the nondet stack depends on the relationship
// MR_NONDET_TEMP_SIZE < MR_DET_TEMP_SIZE < MR_NONDET_FIXED_SIZE.
// All three sizes are measured in words.

// prevfr, redoip, redofr
#define MR_NONDET_TEMP_SIZE         3

// prevfr, redoip, redofr, detfr
#define MR_DET_TEMP_SIZE            4

#ifdef  MR_USE_MINIMAL_MODEL_STACK_COPY_EXTRA_SLOT
// prevfr, redoip, redofr, succip, succfr, sp
#define MR_NONDET_FIXED_SIZE        6
#else
// prevfr, redoip, redofr, succip, succfr
#define MR_NONDET_FIXED_SIZE        5
#endif

// In grades that have stack segments, each segment has a sentinel frame
// at the bottom. These look like ordinary frames whose succfr and succip
// fields are not used for their usual purpose. For further documentation
// on these fields, see the comment on MR_new_nondetstack_segment in
// mercury_stacks.c.

#define MR_SAVEVAL              (-MR_NONDET_FIXED_SIZE)
                                // Saved values start at this offset.

#define MR_prevfr_addr(fr)      (&((MR_Word *) (fr))[MR_PREVFR])
#define MR_redoip_addr(fr)      (&((MR_Word *) (fr))[MR_REDOIP])
#define MR_redofr_addr(fr)      (&((MR_Word *) (fr))[MR_REDOFR])
#define MR_succip_addr(fr)      (&((MR_Word *) (fr))[MR_SUCCIP])
#define MR_succfr_addr(fr)      (&((MR_Word *) (fr))[MR_SUCCFR])
#define MR_tmp_detfr_addr(fr)   (&((MR_Word *) (fr))[MR_TMP_DETFR])
#define MR_table_detfr_addr(fr) (&((MR_Word *) (fr))[MR_TABLE_DETFR])

#define MR_succip_slot_addr(fr) (&(((MR_Code **) (fr))[MR_SUCCIP]))

#define MR_prevfr_slot_word(fr)         (((MR_Word *) (fr))[MR_PREVFR])
#define MR_redoip_slot_word(fr)         (((MR_Word *) (fr))[MR_REDOIP])
#define MR_redofr_slot_word(fr)         (((MR_Word *) (fr))[MR_REDOFR])
#define MR_succip_slot_word(fr)         (((MR_Word *) (fr))[MR_SUCCIP])
#define MR_succfr_slot_word(fr)         (((MR_Word *) (fr))[MR_SUCCFR])
#define MR_tmp_detfr_slot_word(fr)      (((MR_Word *) (fr))[MR_TMP_DETFR])
#define MR_table_detfr_slot_word(fr)    (((MR_Word *) (fr))[MR_TABLE_DETFR])

#define MR_prevfr_slot(fr)      ((MR_Word *) MR_prevfr_slot_word(fr))
#define MR_redoip_slot(fr)      ((MR_Code *) MR_redoip_slot_word(fr))
#define MR_redofr_slot(fr)      ((MR_Word *) MR_redofr_slot_word(fr))
#define MR_succip_slot(fr)      ((MR_Code *) MR_succip_slot_word(fr))
#define MR_succfr_slot(fr)      ((MR_Word *) MR_succfr_slot_word(fr))
#define MR_tmp_detfr_slot(fr)   ((MR_Word *) MR_tmp_detfr_slot_word(fr))
#define MR_table_detfr_slot(fr) ((MR_Word *) MR_table_detfr_slot_word(fr))

#define MR_based_framevar_addr(fr, n)                                   \
                                (&(((MR_Word *) (fr))[MR_SAVEVAL + 1 - (n)]))

#define MR_based_framevar(fr, n) (((MR_Word *) (fr))[MR_SAVEVAL + 1 - (n)])
#define MR_framevar(n)          MR_based_framevar(MR_curfr, n)
#define MR_fv(n)                MR_framevar(n)

////////////////////////////////////////////////////////////////////////////

// DEFINITIONS FOR MANIPULATING THE NONDET STACK

#define MR_nondet_zone_min  (MR_CONTEXT(MR_ctxt_nondetstack_zone)->MR_zone_min)

#ifdef  MR_EXTEND_STACKS_WHEN_NEEDED

  // Note: these macros don't work in the presence of stack segments,
  // which is why automatic stack extension and stack segments cannot
  // both enabled at the same time.

  #define   MR_save_maxfr(lval)                                               \
            do {                                                              \
                lval = (MR_Word) (MR_maxfr - MR_nondet_zone_min);             \
            } while (0)

  #define   MR_restore_maxfr(lval)                                            \
            do {                                                              \
                MR_maxfr_word = (MR_Word)                                     \
                    (((MR_Word *) lval) + MR_nondet_zone_min);                \
            } while (0)

#else

  #define   MR_save_maxfr(lval)                                               \
            do {                                                              \
                lval = (MR_Word) MR_maxfr;                                    \
            } while (0)
  #define   MR_restore_maxfr(lval)                                            \
            do {                                                              \
                MR_maxfr_word = (MR_Word) lval;                               \
            } while (0)

#endif

#ifdef  MR_USE_MINIMAL_MODEL_STACK_COPY_EXTRA_SLOT
  #define   MR_maybe_fill_table_detfr_slot()                                  \
            do {                                                              \
                MR_table_detfr_slot_word(MR_curfr) = MR_sp_word;              \
            } while (0)
#else
  #define   MR_maybe_fill_table_detfr_slot()                                  \
            ((void) 0)
#endif

#define MR_mkframe_basic(predname, numslots)                                  \
    do {                                                                      \
        MR_Word *prevfr;                                                      \
        MR_Word *succfr;                                                      \
                                                                              \
        prevfr = MR_maxfr;                                                    \
        succfr = MR_curfr;                                                    \
        MR_nondetstack_extend_and_check(prevfr,                               \
            MR_NONDET_FIXED_SIZE + (numslots));                               \
        MR_nondetstack_post_extend_check();                                   \
        MR_curfr_word = MR_maxfr_word;                                        \
        MR_prevfr_slot_word(MR_curfr) = (MR_Word) prevfr;                     \
        MR_succip_slot_word(MR_curfr) = (MR_Word) MR_succip;                  \
        MR_succfr_slot_word(MR_curfr) = (MR_Word) succfr;                     \
        MR_redofr_slot_word(MR_curfr) = MR_curfr_word;                        \
        MR_maybe_fill_table_detfr_slot();                                     \
        MR_debugmkframe(predname);                                            \
        MR_nondetstack_overflow_check_msg("MR_mkframe_basic");                \
        MR_collect_non_frame_stats(numslots);                                 \
    } while (0)

#define MR_mkframe(predname, numslots, redoip)                                \
    do {                                                                      \
        MR_mkframe_basic(predname, numslots);                                 \
        MR_redoip_slot_word(MR_curfr) = (MR_Word) redoip;                     \
    } while (0)

#define MR_mkframe_no_redoip(predname, numslots)                              \
    do {                                                                      \
        MR_mkframe_basic(predname, numslots);                                 \
    } while (0)

// Just like mkframe, but also reserves space for a struct
// with the given tag at the bottom of the nondet stack frame.
#define MR_mkpragmaframe(predname, numslots, structname, redoip)              \
    do {                                                                      \
        MR_mkframe_basic(predname, numslots +                                 \
            MR_bytes_to_words(sizeof(struct structname)));                    \
        MR_redoip_slot_word(MR_curfr) = (MR_Word) redoip;                     \
    } while (0)

#define MR_mkpragmaframe_no_redoip(predname, numslots, structname)            \
    do {                                                                      \
        MR_mkframe_basic(predname, numslots +                                 \
            MR_bytes_to_words(sizeof(struct structname)));                    \
    } while (0)

#define MR_mktempframe(redoip)                                                \
    do {                                                                      \
        MR_Word *prevfr;                                                      \
                                                                              \
        prevfr = MR_maxfr;                                                    \
        MR_nondetstack_extend_and_check(prevfr, MR_NONDET_TEMP_SIZE);         \
        MR_prevfr_slot_word(MR_maxfr) = (MR_Word) prevfr;                     \
        MR_redoip_slot_word(MR_maxfr) = (MR_Word) redoip;                     \
        MR_redofr_slot_word(MR_maxfr) = MR_curfr_word;                        \
        MR_debugmktempframe();                                                \
        MR_nondetstack_overflow_check_msg("MR_mktempframe");                  \
    } while (0)

#define MR_mkdettempframe(redoip)                                             \
    do {                                                                      \
        MR_Word *prevfr;                                                      \
                                                                              \
        prevfr = MR_maxfr;                                                    \
        MR_nondetstack_extend_and_check(prevfr, MR_DET_TEMP_SIZE);            \
        MR_prevfr_slot_word(MR_maxfr) = (MR_Word) prevfr;                     \
        MR_redoip_slot_word(MR_maxfr) = (MR_Word) redoip;                     \
        MR_redofr_slot_word(MR_maxfr) = MR_curfr_word;                        \
        MR_tmp_detfr_slot_word(MR_maxfr) = MR_sp_word;                        \
        MR_debugmkdettempframe();                                             \
        MR_nondetstack_overflow_check_msg("MR_mkdettempframe");               \
    } while (0)

#define MR_succeed()                                                          \
    do {                                                                      \
        MR_Word *childfr;                                                     \
                                                                              \
        MR_debugsucceed();                                                    \
        childfr = MR_curfr;                                                   \
        MR_curfr_word = MR_succfr_slot_word(childfr);                         \
        MR_GOTO(MR_succip_slot(childfr));                                     \
    } while (0)

#define MR_succeed_discard()                                                  \
    do {                                                                      \
        MR_Word *childfr;                                                     \
                                                                              \
        MR_debugsucceeddiscard();                                             \
        childfr = MR_curfr;                                                   \
        MR_maxfr_word = MR_prevfr_slot_word(childfr);                         \
        MR_curfr_word = MR_succfr_slot_word(childfr);                         \
        MR_GOTO(MR_succip_slot(childfr));                                     \
    } while (0)

#define MR_fail()                                                             \
    do {                                                                      \
        MR_debugfail();                                                       \
        MR_maxfr_word = MR_prevfr_slot_word(MR_maxfr);                        \
        MR_curfr_word = MR_redofr_slot_word(MR_maxfr);                        \
        MR_nondetstack_underflow_check_msg("MR_fail");                        \
        MR_GOTO(MR_redoip_slot(MR_maxfr));                                    \
    } while (0)

#define MR_redo()                                                             \
    do {                                                                      \
        MR_debugredo();                                                       \
        MR_curfr_word = MR_redofr_slot_word(MR_maxfr);                        \
        MR_GOTO(MR_redoip_slot(MR_maxfr));                                    \
    } while (0)

////////////////////////////////////////////////////////////////////////////

// DEFINITIONS FOR EXCEPTION HANDLING

#ifdef MR_CONSERVATIVE_GC
  #define MR_IF_NOT_CONSERVATIVE_GC(x)
#else
  #define MR_IF_NOT_CONSERVATIVE_GC(x) x
#endif

#ifdef MR_USE_TRAIL
  #define MR_IF_USE_TRAIL(x) x
#else
  #define MR_IF_USE_TRAIL(x)
#endif

// This enum specifies the kind of handler in an exception handler
// nondet stack frame.

enum MR_HandlerCodeModel {
    // For these three values, the exception handler is a Mercury closure with
    // the specified determinism. If an exception occurs, then after the
    // Mercury stacks have been unwound, the closure will be called.

    MR_MODEL_DET_HANDLER,
    MR_MODEL_SEMI_HANDLER,
    MR_MODEL_NON_HANDLER,
    // For this value, the exception will be handled by C code using
    // setjmp/longjmp. If an exception occurs, then after the Mercury stacks
    // have been unwound, `MR_longjmp(MR_ENGINE(MR_eng_jmp_buf))' will be
    // called.

    MR_C_LONGJMP_HANDLER
};

// Define a struct for the framevars that we use in an exception handler
// nondet stack frame. This struct gets allocated on the nondet stack
// using MR_mkpragmaframe(), with a special redoip of
// `MR_exception_handler_do_fail'.

typedef struct MR_Exception_Handler_Frame_struct {
    // The `code_model' field is used to identify what kind of handler it is.
    // It holds values of type MR_HandlerCodeModel (see above), but it is
    // declared to have type `MR_Word' to ensure that everything remains
    // word-aligned.

    MR_Word             MR_excp_code_model;

    // If code_model is MR_MODEL_*_HANDLER, then the `handler' field holds the
    // Mercury closure for the handler, which will be a closure of the
    // specified determinism. If code_model is MR_C_LONGJMP, then this field
    // is unused.

    MR_Word             MR_excp_handler;

    // The value of MR_trace_from_full, saved at the time the frame was
    // created. This holds a value of type MR_bool, but it is declared
    // to have type MR_Word to ensure that everything remains word-aligned.

    MR_Word             MR_excp_full_trace;

    // The remaining fields hold stuff that must be saved in order
    // to unwind the Mercury stacks.

    // The det stack pointer.
    MR_Word             *MR_excp_stack_ptr;

    // The trail state.
    MR_IF_USE_TRAIL(
        MR_Word         MR_excp_trail_ptr;
        MR_Word         MR_excp_ticket_counter;
    )

    // The heap state.
    MR_IF_NOT_CONSERVATIVE_GC(
        MR_Word         *MR_excp_heap_ptr;
        MR_Word         *MR_excp_solns_heap_ptr;
        MR_MemoryZone   *MR_excp_heap_zone;
    )
} MR_Exception_Handler_Frame;

// In deep profiling grades, we need (a) 1 stack slot (framevar 1)
// to save the input closure across the call port code; (b) up to 2 stack slots
// (framevars 1 and 2) to store the result and maybe the success indicator,
// and (c) 4 stack slots (frame vars 3 to 6) for use by the profiling
// routines themselves. We can reuse framevar 1 for two purposes because their
// live ranges do not overlap.

#ifdef  MR_DEEP_PROFILING
  #define MR_EXCEPTION_FRAMEVARS        6
  #define MR_EXCEPTION_FIRST_DEEP_SLOT  3
#else
  #define MR_EXCEPTION_FRAMEVARS        2
#endif

#define MR_EXCEPTION_STRUCT                                                   \
    (((MR_Exception_Handler_Frame *)                                          \
        (MR_curfr + 1 - MR_EXCEPTION_FRAMEVARS - MR_NONDET_FIXED_SIZE)) - 1)

#define MR_create_exception_handler(name,                                      \
        handler_code_model, handler_closure, redoip)                           \
    do {                                                                       \
        /*                                                                     \
        ** Create a handler on the stack with the special redoip of            \
        ** `MR_exception_handler_do_fail' (we'll look for this redoip when     \
        ** unwinding the nondet stack in builtin_throw/1), and save the stuff  \
        ** we will need if an exception is thrown.                             \
        */                                                                     \
        MR_mkpragmaframe((name), MR_EXCEPTION_FRAMEVARS,                       \
            MR_Exception_Handler_Frame_struct,                                 \
            MR_ENTRY(MR_exception_handler_do_fail));                           \
        /* Record the handler's code model. */                                 \
        MR_EXCEPTION_STRUCT->MR_excp_code_model = (handler_code_model);        \
        /* Save the handler's closure. */                                      \
        MR_EXCEPTION_STRUCT->MR_excp_handler = (handler_closure);              \
        /* Save the full tracing flag. */                                      \
        MR_EXCEPTION_STRUCT->MR_excp_full_trace = (MR_Word) MR_trace_from_full;\
        /* Save the det stack pointer. */                                      \
        MR_EXCEPTION_STRUCT->MR_excp_stack_ptr = MR_sp;                        \
        MR_IF_NOT_CONSERVATIVE_GC(                                             \
            /* Save the heap and solutions heap pointers. */                   \
            MR_EXCEPTION_STRUCT->MR_excp_heap_ptr = MR_hp;                     \
            MR_EXCEPTION_STRUCT->MR_excp_solns_heap_ptr = MR_sol_hp;           \
            MR_EXCEPTION_STRUCT->MR_excp_heap_zone =                           \
                MR_ENGINE(MR_eng_heap_zone);                                   \
        )                                                                      \
        MR_IF_USE_TRAIL(                                                       \
            /* Save the trail state. */                                        \
            MR_mark_ticket_stack(MR_EXCEPTION_STRUCT->MR_excp_ticket_counter); \
            MR_store_ticket(MR_EXCEPTION_STRUCT->MR_excp_trail_ptr);           \
        )                                                                      \
                                                                               \
        /*                                                                     \
        ** Now we need to create another frame. This is so that we can be sure \
        ** that no-one will hijack the redoip of the special frame we created  \
        ** above. (The compiler sometimes generates ``hijacking'' code that    \
        ** saves the topmost redoip on the stack, and temporarily replaces it  \
        ** with a new redoip that will do some processing on failure before    \
        ** restoring the original redoip. This would cause problems when doing \
        ** stack unwinding in builtin_throw/1, because we wouldn't be able to  \
        ** find the special redoip. But code will only ever hijack the topmost \
        ** frame, so we can avoid this by creating a second frame above the    \
        ** special frame.)                                                     \
        */                                                                     \
        MR_mktempframe(redoip);                                                \
    } while (0)

////////////////////////////////////////////////////////////////////////////

#ifdef  MR_USE_MINIMAL_MODEL_STACK_COPY

// DEFINITIONS FOR GENERATOR STACK FRAMES

// The generator stack has one entry for each call to a minimal model tabled
// procedure that is (a) acting as the generator for its subgoal and (b) is
// in the active state. The MR_gen_frame field points to the home nondet stack
// frame of the generator, and the MR_gen_subgoal field points to the
// generator's data structure.
//
// In systems such as XSB, each choice point has a flag saying whether it is
// an active generator or not, and if yes, where its subgoal's tabling
// information is stored. In Mercury, the equivalent test checks whether
// the generator stack has an entry whose MR_gen_frame field matches the
// address of the nondet stack frame. This approach that minimizes the
// performance impact of minimal model evaluation on non-tabled procedures.

struct MR_GenStackFrameStruct {
    MR_Word                 *MR_gen_frame;
    MR_SubgoalPtr           MR_gen_subgoal;
};

extern  MR_Integer          MR_gen_next_var;
extern  MR_GenStackFrame    *MR_gen_stack_var;

extern  void                MR_push_generator(MR_Word *frame_addr,
                                MR_SubgoalPtr subgoal);
extern  MR_Subgoal          *MR_top_generator_table(void);
extern  void                MR_pop_generator(void);
extern  void                MR_print_gen_stack(FILE *fp);
extern  void                MR_print_any_gen_stack(FILE *fp,
                                MR_Integer gen_next,
                                MR_GenStackFrame *gen_block);

// DEFINITIONS FOR CUT STACK FRAMES

// The cut stack has one entry for each commit goal currently active.
// (A commit goal is an existential quantification that has a different
// determinism than the goal being quantified over.) We use the cut stack
// to prevent generators in the quantified being left active but incomplete
// when the commit goal succeeds. We need to clean up any such generators
// because otherwise, consumers will be depend on the generator to find all
// the answers to the generator's subgoal, but the generator will never
// compute any more answers, since it will never be backtracked into.
// The MR_cut_generators field of a cut stack entry contains the list of
// generators that are inside the corresponding commit and not inside
// some nested commit.
//
// The MR_cut_frame field specifies the address of the nondet stack frame
// (it should be a temp frame) used by the commit to get control on the
// failure of the quantified goal. The MR_cut_gen_next field records
// the value of MR_gen_next (the size of the generator stack) when the
// commit goal started. When the quantified goal succeeds, the commit cuts
// away the nondet stack frame at and above MR_cut_frame; we must also throw
// away the generator stack entries that act as markers on the discarded nondet
// stack frames. This means the generator stack entries at index
// MR_cut_gen_next and above.
//
// The MR_cut_depth field records the depth of the cut stack entry in
// the interleaving of the cut stack and pneg stack entries dictated by
// the nesting of committed choice and possibly negated contexts currently
// active.

typedef struct MR_CutGeneratorListNode *MR_CutGeneratorList;
struct MR_CutGeneratorListNode {
    MR_SubgoalPtr           MR_cut_generator_ptr;
    MR_CutGeneratorList     MR_cut_next_generator;
};

struct MR_CutStackFrameStruct {
    MR_Word                 *MR_cut_frame;
    MR_CutGeneratorList     MR_cut_generators;
    MR_Integer              MR_cut_gen_next;
#ifdef  MR_MINIMAL_MODEL_DEBUG
    int                     MR_cut_depth;
#endif
};

extern  MR_Integer          MR_cut_next_var;
extern  MR_CutStackFrame    *MR_cut_stack_var;

extern  void                MR_commit_mark(void);
extern  void                MR_commit_cut(void);

extern  void                MR_register_generator_ptr(MR_SubgoalPtr);
extern  void                MR_print_cut_stack(FILE *fp);
extern  void                MR_print_any_cut_stack(FILE *fp,
                                MR_Integer cut_next,
                                MR_CutStackFrame *cut_block);

// DEFINITIONS FOR PNEG STACK FRAMES

// The pneg stack has one entry for each possibly negated context currently
// active. (Possibly negated contexts include the conditions of if-then-elses
// as well negated goals.) The MR_pneg_consumers field of a pneg stack entry
// records all the consumers that are inside the corresponding possibly negated
// context and not inside any nested possibly negated context. When the goal
// in the possibly negated context fails, we check whether any of these
// consumers are waiting for more answers from their generator. If yes,
// then the failure is an artifact of the tabling implementation, and
// committing to the else branch of the if-then-else or continuing after the
// negated goal would be incorrect, so we abort the program.
//
// The MR_pneg_frame field specifies the address of the nondet stack frame
// (it should be a temp frame) used by the negation or if-then-else to get
// control on the failure of the possibly negated goal. The MR_pneg_gen_next
// field records the value of MR_gen_next (the size of the generator stack)
// when the possibly negated goal started. Currently, neither field is used.
//
// The MR_pneg_depth field records the depth of the pneg stack entry in
// the interleaving of the cut stack and pneg stack entries dictated by
// the nesting of committed choice and possibly negated contexts currently
// active.

struct MR_PNegConsumerListNodeStruct {
    MR_Consumer             *MR_pneg_consumer_ptr;
    MR_PNegConsumerList     MR_pneg_next_consumer;
};

struct MR_PNegStackFrameStruct {
    MR_PNegConsumerList     MR_pneg_consumers;
    MR_Word                 *MR_pneg_frame;
#ifdef  MR_MINIMAL_MODEL_DEBUG
    MR_Integer              MR_pneg_gen_next;
    int                     MR_pneg_depth;
#endif
};

extern  MR_Integer          MR_pneg_next_var;
extern  MR_PNegStackFrame   *MR_pneg_stack_var;

extern  void                MR_register_suspension(MR_Consumer *consumer);
extern  void                MR_pneg_enter_cond(void);
extern  void                MR_pneg_enter_then(void);
extern  void                MR_pneg_enter_else(const char *context);

extern  void                MR_print_pneg_stack(FILE *fp);
extern  void                MR_print_any_pneg_stack(FILE *fp,
                                MR_Integer pneg_next,
                                MR_PNegStackFrame *pneg_block);

#endif  // MR_USE_MINIMAL_MODEL_STACK_COPY

#endif  // not MERCURY_STACKS_H
