// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2001, 2004 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// The internals of hand-written unification and comparison routines.
//
// The versions of builtin_catch for the various determinisms should define
// the following macros:
//
// proc_label
// proc_layout
// body_code
//
// It should also define may_need_fail_action for the model_non versions.

// Stackvar(1) and possibly stackvar(2) are used to save the inputs
// of the unification code. The first framevar available for saving
// deep profiling information is stackvar(3).

#define FIRST_DEEP_SLOT         3

// Each procedure defines several local labels. The local label numbers are
// allocated as follows.

#define CALL_PORT_RETURN_LABEL(pl)  MR_label_name(pl, 1)
#define EXIT_PORT_RETURN_LABEL(pl)  MR_label_name(pl, 2)
#define FAIL_LABEL(pl)          MR_label_name(pl, 3)
#define FAIL_PORT_RETURN_LABEL(pl)  MR_label_name(pl, 4)

////////////////////////////////////////////////////////////////////////////

MR_define_entry(proc_label);

#ifdef  MR_DEEP_PROFILING

    MR_incr_sp_push_msg(6, name);
    MR_stackvar(6) = MR_succip_word;
    MR_stackvar(1) = MR_r1;
    MR_stackvar(2) = MR_r2;

    MR_deep_semi_call(proc_label, proc_layout, FIRST_DEEP_SLOT,
        CALL_PORT_RETURN_LABEL(proc_label));

    MR_r1 = MR_stackvar(1);
    MR_r2 = MR_stackvar(2);

    MR_save_transient_registers();
    body_code;
    MR_restore_transient_registers();

    if (! MR_r1) {
        MR_GOTO_LABEL(FAIL_LABEL(proc_label));
    }

    MR_deep_semi_exit(proc_label, FIRST_DEEP_SLOT,
        EXIT_PORT_RETURN_LABEL(proc_label));

    MR_r1 = 1;
    MR_succip_word = MR_stackvar(6);
    MR_decr_sp_pop_msg(6);
    MR_proceed();

MR_define_label(FAIL_LABEL(proc_label));

    MR_deep_semi_fail(proc_label, FIRST_DEEP_SLOT,
        FAIL_PORT_RETURN_LABEL(proc_label));

    MR_r1 = 0;
    MR_succip_word = MR_stackvar(6);
    MR_decr_sp_pop_msg(6);
    MR_proceed();

#else

    body_code;
    MR_proceed();

#endif

////////////////////////////////////////////////////////////////////////////

#undef  CALL_PORT_RETURN_LABEL
#undef  EXIT_PORT_RETURN_LABEL
#undef  FAIL_LABEL
#undef  FAIL_PORT_RETURN_LABEL

#undef  FIRST_DEEP_SLOT
