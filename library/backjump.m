%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2007-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: backjump.m
% Author: Mark Brown <mark@csse.unimelb.edu.au>
% Stability: medium
%
% This module defines the Mercury interface for backjumping.
%
% An application can use this module to add backjumping to their search
% algorithms in the following way:
%
%   - At points in the search tree where you wish to backjump *to*, add a
%     call to get_choice_id/1.
%
%   - When in the search tree you discover that there are no further solutions
%     between the current execution point and the failure port of an earlier
%     call to get_choice_id/1, call backjump/1 with the relevant choice_id.
%     This takes execution immediately to that failure port.
%
% It is important to avoid backjumping to a choicepoint that has already
% been pruned away, either on failure or due to a commit. In the former case
% (which can occur if the choice_id is stored in a non-trailed mutable, for
% example) the implementation throws an exception. In the latter case,
% behaviour is undefined but most likely will be a seg fault. This can occur
% if multi/nondet code that returns a choice_id is called from a
% cc_multi/cc_nondet context, for example.
%
% Note that the definition of "solution" in the above means solution with
% respect to the search algorithm, and doesn't necessarily mean solution of
% the immediate parent or any particular ancestor.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module backjump.
:- interface.

%---------------------------------------------------------------------------%

    % Abstract type representing points in the search tree which can be
    % backjumped to.
    %
:- type choice_id.

    % Returns a single unused choice_id, then fails. We make this nondet
    % rather than det, however, so that:
    %    a) it leaves a nondet stack frame behind which can later be used
    %       by backjump/1, and
    %    b) the calling context is forced to deal with the case of no
    %       solutions, which may occur even if later conjuncts can never fail
    %       (since they may call backjump/1, which counts as an exceptional
    %       rather than a logical answer).
    %
:- impure pred get_choice_id(choice_id::out) is nondet.

    % Backjump to the point where the choice_id was created. That is,
    % jump immediately to the FAIL event of the corresponding call to
    % get_choice_id.
    %
:- impure pred backjump(choice_id::in) is erroneous.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

%---------------------------------------------------------------------------%

:- interface.

    % Debugging support.
    %
:- func to_int(choice_id) = int.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module io.
:- import_module list.
:- import_module string.

:- type choice_id == int.

get_choice_id(Id) :-
    impure builtin_choice_id(Id).

backjump(Id) :-
    impure builtin_backjump(Id).

%---------------------------------------------------------------------------%

:- impure pred builtin_choice_id(choice_id::out) is nondet.
:- pragma terminates(builtin_choice_id/1).

:- impure pred builtin_backjump(choice_id::in) is erroneous.
:- pragma terminates(builtin_backjump/1).

% builtin_choice_id and builtin_backjump are implemented below using
% hand-coded low-level C code.
%
% IMPORTANT: any changes or additions to external predicates should be
% reflected in the definition of pred_is_external in
% mdbcomp/program_representation.m. The debugger needs to know what predicates
% are defined externally, so that it knows not to expect events for those
% predicates.
:- pragma external_pred(builtin_choice_id/1).
:- pragma external_pred(builtin_backjump/1).

%---------------------------------------------------------------------------%

:- pragma foreign_decl("C", "#include \"mercury_backjump.h\"").

%---------------------------------------------------------------------------%
%
% The --high-level-code implementation.
%

:- pragma foreign_decl("C",
"
/* protect against multiple inclusion */
#ifndef ML_BACKJUMP_GUARD
#define ML_BACKJUMP_GUARD

#ifdef MR_HIGHLEVEL_CODE

#include <setjmp.h>

#define MR_CONT_PARAMS      MR_Cont cont, void *cont_env
#define MR_CONT_PARAM_TYPES MR_Cont, void *
#define MR_CONT_ARGS        cont, cont_env
#define MR_CONT_CALL()      cont(cont_env)

void MR_CALL
mercury__backjump__builtin_choice_id_1_p_0(
    MR_BackJumpChoiceId *id, MR_CONT_PARAMS);

void MR_CALL
mercury__backjump__builtin_backjump_1_p_0(MR_BackJumpChoiceId id);

#endif /* MR_HIGHLEVEL_CODE */
#endif /* ML_BACKJUMP_GUARD */
").

:- pragma foreign_code("C",
"
#ifdef MR_HIGHLEVEL_CODE

#ifdef MR_NATIVE_GC

/*
** XXX code is needed to trace the local variables
** in the builtin_choice_id predicate for accurate GC.
*/

#endif /* MR_NATIVE_GC */

void MR_CALL
mercury__backjump__builtin_choice_id_1_p_0(MR_Integer *id, MR_CONT_PARAMS)
{
    MR_BackJumpHandler this_handler;

    this_handler.MR_bjh_prev = MR_GET_BACKJUMP_HANDLER();
    this_handler.MR_bjh_id = MR_GET_NEXT_CHOICE_ID();
    MR_SET_BACKJUMP_HANDLER(&this_handler);

    if (setjmp(this_handler.MR_bjh_handler) == 0) {
    #ifdef MR_DEBUG_JMPBUFS
        fprintf(stderr, ""choice setjmp %p\\n"", this_handler.MR_bjh_handler);
    #endif

        *id = this_handler.MR_bjh_id;
        MR_CONT_CALL();
    } else {
    #ifdef MR_DEBUG_JMPBUFS
        fprintf(stderr, ""choice caught jmp %p\\n"",
            this_handler.MR_bjh_handler);
    #endif
    }

    MR_SET_BACKJUMP_HANDLER(this_handler.MR_bjh_prev);
}

void MR_CALL
mercury__backjump__builtin_backjump_1_p_0(MR_BackJumpChoiceId id)
{
    MR_BackJumpHandler *backjump_handler;

    backjump_handler = MR_GET_BACKJUMP_HANDLER();

    /*
    ** XXX when we commit and prune away nondet stack frames, we leave the
    ** backjump handlers on the stack. If the caller tries to backjump to
    ** a frame that has been pruned away, the handler may still be on the
    ** stack and we won't detect the problem.
    **
    ** We could avoid this problem by adding a trailing function which
    ** prunes back the handler stack on a commit, which would mean that in
    ** this case we will reach the bottom of the stack and call
    ** ML_report_invalid_backjump rather than seg faulting. But that would
    ** require trailing to be always available. Instead, we just rely on
    ** the caller only backjumping to frames that actually do exist.
    **
    ** (The same problem would occur if the caller tries to backjump to a
    ** frame that has already failed. In this case, though, the choice ID
    ** will also no longer be live since the call to get_choice_id would have
    ** been backtracked over, so this isn't as much of a problem as with
    ** commits.)
    */
    while (backjump_handler != NULL) {
        if (backjump_handler->MR_bjh_id == id) {
            break;
        }
        backjump_handler = backjump_handler->MR_bjh_prev;
    }

    if (backjump_handler == NULL) {
        ML_report_invalid_backjump(id);
        exit(EXIT_FAILURE);
    } else {

  #ifdef MR_DEBUG_JMPBUFS
        fprintf(stderr, ""backjump longjmp %p\\n"",
            backjump_handler->MR_bjh_handler);
  #endif
        longjmp(backjump_handler->MR_bjh_handler, 1);
    }
}

#endif /* MR_HIGHLEVEL_CODE */
").

%---------------------------------------------------------------------------%
%
% The --no-high-level-code implementation.
%

:- pragma foreign_decl("C",
"
#ifndef MR_HIGHLEVEL_CODE

#include ""mercury_stacks.h""
#include ""mercury_stack_trace.h""
#include ""mercury_trace_base.h""
#include ""mercury_layout_util.h""
#include ""mercury_deep_profiling_hand.h""

#endif /* !MR_HIGHLEVEL_CODE */
").

:- pragma foreign_code("C",
"
void mercury_sys_init_backjumps_init(void);
void mercury_sys_init_backjumps_init_type_tables(void);
#ifdef MR_DEEP_PROFILING
void mercury_sys_init_backjumps_write_out_proc_statics(FILE *deep_fp,
    FILE *procrep_fp);
#endif

#ifndef MR_HIGHLEVEL_CODE

#define ML_DUMMY_LINE 0

#define ML_BACKJUMP_STRUCT \
    (((MR_BackJumpHandler *) (MR_curfr + 1 - MR_NONDET_FIXED_SIZE)) - 1)

#ifdef ML_DEBUG_BACKJUMPS
#define ML_BACKJUMP_CHECKPOINT(s, p) \
    do { \
        fflush(stdout); \
        fprintf(stderr, ""backjumps (%s): "" \
            ""loc %p, prev %p, id %d, sp %p, fr %p\\n"", \
            s, p, p->MR_bjh_prev, p->MR_bjh_id, p->MR_bjh_saved_sp, \
            p->MR_bjh_saved_fr); \
    } while (0)
#else
#define ML_BACKJUMP_CHECKPOINT(s, p)
#endif

MR_define_extern_entry(mercury__backjump__builtin_choice_id_1_0);
MR_define_extern_entry(mercury__backjump__builtin_backjump_1_0);

MR_declare_label(mercury__backjump__builtin_choice_id_1_0_i1);

MR_proc_static_user_no_site(backjump, builtin_choice_id, 1, 0,
    ""backjump.m"", ML_DUMMY_LINE, MR_TRUE);
MR_proc_static_user_no_site(backjump, builtin_backjump, 1, 0,
    ""backjump.m"", ML_DUMMY_LINE, MR_TRUE);

MR_EXTERN_USER_PROC_STATIC_PROC_LAYOUT(MR_DETISM_NON,
    MR_PROC_NO_SLOT_COUNT, -1, MR_PREDICATE, backjump, builtin_choice_id,
    1, 0);
MR_EXTERN_USER_PROC_STATIC_PROC_LAYOUT(MR_DETISM_DET, 1,
    MR_LONG_LVAL_STACKVAR_INT(1), MR_PREDICATE, backjump,
    builtin_backjump, 1, 0);

MR_MAKE_USER_INTERNAL_LAYOUT(backjump, builtin_choice_id, 1, 0, 1);

MR_BEGIN_MODULE(hand_written_backjump_module)
    MR_init_entry_sl(mercury__backjump__builtin_choice_id_1_0);
    MR_init_entry_sl(mercury__backjump__builtin_backjump_1_0);

    MR_init_label_sl(mercury__backjump__builtin_choice_id_1_0_i1);
MR_BEGIN_CODE

MR_define_entry(mercury__backjump__builtin_choice_id_1_0);
{
    MR_mkpragmaframe(""builtin_choice_id/1"", 0, MR_BackJumpHandler_Struct,
        MR_LABEL(mercury__backjump__builtin_choice_id_1_0_i1));

    #if defined(MR_DEEP_PROFILING)
        MR_fatal_error(
            ""builtin_choice_id: NYI backjumping and deep profiling"");
    #endif

    ML_BACKJUMP_STRUCT->MR_bjh_prev = MR_GET_BACKJUMP_HANDLER();
    ML_BACKJUMP_STRUCT->MR_bjh_id = MR_GET_NEXT_CHOICE_ID();
    ML_BACKJUMP_STRUCT->MR_bjh_saved_sp = MR_sp;
    ML_BACKJUMP_STRUCT->MR_bjh_saved_fr = MR_curfr;
    MR_SET_BACKJUMP_HANDLER(ML_BACKJUMP_STRUCT);

    ML_BACKJUMP_CHECKPOINT(""create"", ML_BACKJUMP_STRUCT);

    MR_r1 = (MR_Word) ML_BACKJUMP_STRUCT->MR_bjh_id;
    MR_succeed();
}
MR_define_label(mercury__backjump__builtin_choice_id_1_0_i1);
{
    /* Restore the previous handler. */
    MR_SET_BACKJUMP_HANDLER(ML_BACKJUMP_STRUCT->MR_bjh_prev);
    MR_fail();
}

MR_define_entry(mercury__backjump__builtin_backjump_1_0);
{
    MR_BackJumpChoiceId id = MR_r1;
    MR_BackJumpHandler *backjump_handler;

    backjump_handler = MR_GET_BACKJUMP_HANDLER();

    #if defined(MR_DEEP_PROFILING)
        MR_fatal_error(
            ""builtin_backjump: NYI backjumping and deep profiling"");
    #endif

    /*
    ** XXX see comments in the high-level implementation.
    */
    while (backjump_handler != NULL) {
        if (backjump_handler->MR_bjh_id == id) {
            break;
        }

        ML_BACKJUMP_CHECKPOINT(""scan"", backjump_handler);

        backjump_handler = backjump_handler->MR_bjh_prev;
    }

    if (backjump_handler == NULL) {
        ML_report_invalid_backjump(id);
        exit(EXIT_FAILURE);
    } else {
        ML_BACKJUMP_CHECKPOINT(""found"", backjump_handler);

        /*
        ** XXX we should produce some EXCP trace events here, to give
        ** the user an opportunity to retry a goal that calculated a
        ** (possibly incorrect) backjump.
        */

        MR_SET_BACKJUMP_HANDLER(backjump_handler->MR_bjh_prev);
        MR_sp_word = (MR_Word) backjump_handler->MR_bjh_saved_sp;
        MR_maxfr_word = (MR_Word) backjump_handler->MR_bjh_saved_fr;
        MR_fail();
    }
}

MR_END_MODULE

#endif /* !MR_HIGHLEVEL_CODE */

/*
INIT mercury_sys_init_backjumps
*/

void mercury_sys_init_backjumps_init(void)
{
#ifndef MR_HIGHLEVEL_CODE
    hand_written_backjump_module();
#endif
}

void mercury_sys_init_backjumps_init_type_tables(void)
{
    /* no types to register */
}

#ifdef MR_DEEP_PROFILING
void
mercury_sys_init_backjumps_write_out_proc_statics(FILE *deep_fp,
    FILE *procrep_fp)
{
    MR_write_out_user_proc_static(deep_fp, procrep_fp,
        &MR_proc_layout_user_name(backjump, builtin_choice_id, 1, 0));
    MR_write_out_user_proc_static(deep_fp, procrep_fp,
        &MR_proc_layout_user_name(backjump, builtin_backjump, 1, 0));
}
#endif /* MR_DEEP_PROFILING */

").

%---------------------------------------------------------------------------%

:- pragma foreign_code("C#", "

    public static void
    builtin_choice_id_1_p_0(object cont, /* env_ptr */ object cont_env_ptr)
    {
        throw new System.Exception(""builtin_choice_id/1 not implemented"");
    }

    public static void
    builtin_backjump_1_p_0(int Id_2)
    {
        throw new System.Exception(""builtin_backjump/1 not implemented"");
    }

").

%---------------------------------------------------------------------------%

:- pragma foreign_code("Java", "

    public static void
    builtin_choice_id_1_p_0(jmercury.runtime.MethodPtr cont,
        /* env_ptr */ java.lang.Object cont_env_ptr)
    {
        throw new java.lang.Error(""builtin_choice_id/1 not implemented"");
    }

    public static void
    builtin_backjump_1_p_0(int Id_2)
    {
        throw new java.lang.Error(""builtin_backjump/1 not implemented"");
    }

").

%---------------------------------------------------------------------------%

:- pragma foreign_code("Erlang", "

    builtin_choice_id_1_p_0(_) ->
        throw(""builtin_choice_id/1 NYI for Erlang backend"").

    builtin_backjump_1_p_0(_) ->
        throw(""builtin_backjump/1 NYI for Erlang backend"").
").

%---------------------------------------------------------------------------%

:- pragma foreign_export("C", report_invalid_backjump(in, di, uo),
    "ML_report_invalid_backjump").

:- pred report_invalid_backjump(int::in, io::di, io::uo) is det.

report_invalid_backjump(Id, !IO) :-
    io.output_stream(CurOutStream, !IO),
    io.flush_output(CurOutStream, !IO),
    io.stderr_stream(StdErrStream, !IO),
    io.format(StdErrStream, "Uncaught Mercury backjump (%d)\n", [i(Id)], !IO),
    io.flush_output(StdErrStream, !IO).

%---------------------------------------------------------------------------%

to_int(P) = P.

%---------------------------------------------------------------------------%
:- end_module backjump.
%---------------------------------------------------------------------------%
