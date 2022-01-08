%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2003, 2005-2008 The University of Melbourne.
% Copyright (C) 2015, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: term_size_prof.m.
% Author: zs.
% Stability: low.
%
% This file is automatically imported into every module when term size
% profiling is enabled. It contains support predicates used for term size
% profiling.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module term_size_prof_builtin.
:- interface.

    % measure_size(Term, Size): return the size of Term as Size.
    % The cost of the operation is independent of the size of Term;
    % if Term points to the heap, it looks up the size stored at the
    % starts of the cell.
    %
:- pred measure_size(T::in, int::out) is det.

    % measure_size_acc(Term, Size0, Size): similar to measure_size,
    % but instead of returning just the size of term, it returns the
    % size plus Size0.
    %
:- pred measure_size_acc(T::in, int::in, int::out) is det.

    % increment_size(Term, Incr): Term must be a term on the heap
    % that is not fully ground, and whose size slot's contents represents
    % the size of the term in its original binding state. When some of
    % Term's free arguments are bound, we must increment its recorded size
    % by Incr, the sum of the sizes of the terms bound to those arguments.
    % This is what increment_size does. It is impure because it updates
    % Term destructively.
    %
:- impure pred increment_size(T::in, int::in) is det.

    % This function is exactly like int.plus, and is also implemented
    % as a builtin. It is duplicated in this module because only predicates
    % and functions in builtin modules like this one are immune to dead
    % procedure elimination.
    %
:- func term_size_plus(int, int) = int.

    % We want to take measurements only of the top-level invocations of
    % the procedures in the complexity experiment, not the recursive
    % invocations. This type says whether we are already executing the
    % relevant procedure. Its definition should be kept in sync with
    % MR_ComplexityIsActive in runtime/mercury_term_size.h.
:- type complexity_is_active
    --->    is_inactive
    ;       is_active.

    % For each procedure in the complexity experiment, we can take
    % measurements for many different top-level invocations. Values
    % of the complexity_slot type identify one of these invocations.
:- type complexity_slot == int.

:- impure pred complexity_is_active(complexity_is_active::out) is det.

:- impure pred complexity_call_proc(complexity_slot::out) is det.
:- impure pred complexity_exit_proc(complexity_slot::in) is det.
:- impure pred complexity_fail_proc(complexity_slot::in) is failure.
:- impure pred complexity_redo_proc(complexity_slot::in) is failure.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

% This module import is required by the Mercury clause for measure_size/2.
%
:- import_module require.

%---------------------------------------------------------------------------%


:- pragma foreign_decl("C", "
#ifndef MR_TERM_SIZE_PROFILING_GUARD
#define MR_TERM_SIZE_PROFILING_GUARD

  #ifdef MR_RECORD_TERM_SIZES
    #include ""mercury_term_size.h""
  #endif // MR_RECORD_TERM_SIZES

#endif // MR_TERM_SIZE_PROFILING_GUARD
").

:- pragma foreign_proc("C",
    measure_size(Term::in, Size::out),
    [thread_safe, promise_pure, will_not_call_mercury, may_not_duplicate],
"{
#ifdef MR_RECORD_TERM_SIZES
    MR_TypeInfo type_info;

    type_info = (MR_TypeInfo) TypeInfo_for_T;
    Size = MR_term_size(type_info, Term);
  #ifdef MR_DEBUG_TERM_SIZES
    if (MR_heapdebug && MR_lld_print_enabled) {
        printf(""measure_size: %p -> %d\\n"",
            (void *) Term, (int) Size);
    }
  #endif
#else
    MR_fatal_error(""measure_size: term size profiling not enabled"");
#endif
}").

measure_size(_Value, Size) :-
    ( if semidet_succeed then
        error("measure_size: not implemented")
    else
        Size = 0
    ).

:- pragma foreign_proc("C",
    measure_size_acc(Term::in, Size0::in, Size::out),
    [thread_safe, promise_pure, will_not_call_mercury, may_not_duplicate],
"{
#ifdef MR_RECORD_TERM_SIZES
    MR_TypeInfo type_info;

    type_info = (MR_TypeInfo) TypeInfo_for_T;
    Size = MR_term_size(type_info, Term) + Size0;
  #ifdef MR_DEBUG_TERM_SIZES
    if (MR_heapdebug && MR_lld_print_enabled) {
        printf(""measure_size_acc: %p + %d -> %d\\n"",
            (void *) Term, (int) Size0, (int) Size);
    }
  #endif
#else
    MR_fatal_error(""measure_size_acc: term size profiling not enabled"");
#endif
}").

measure_size_acc(_Value, Size0, Size) :-
    ( if semidet_succeed then
        error("measure_size_acc: not implemented")
    else
        Size = Size0
    ).

:- pragma foreign_proc("C",
    increment_size(Term::in, Incr::in),
    [thread_safe, will_not_call_mercury, may_not_duplicate],
"{
#ifdef MR_RECORD_TERM_SIZES
  #ifdef MR_DEBUG_TERM_SIZES
    if (MR_heapdebug && MR_lld_print_enabled) {
        printf(""increment_size: %p + %d\\n"",
            (void *) Term, (int) Incr);
    }
  #endif
    MR_mask_field(Term, -1) += Incr;
#else
    MR_fatal_error(""increment_size: term size profiling not enabled"");
#endif
}").

increment_size(_Value, _Incr) :-
    impure private_builtin.imp.

%---------------------------------------------------------------------------%

% None of the following predicates are designed to be called directly;
% they are designed only to hang foreign_procs onto.

:- pragma foreign_proc("C",
    complexity_is_active(IsActive::out),
    [thread_safe, will_not_call_mercury],
"
    // Mention IsActive to avoid warning.
    MR_fatal_error(""complexity_mark_active"");
").

complexity_is_active(IsActive) :-
    impure private_builtin.imp,
    ( if semidet_succeed then
        error("complexity_mark_active: not implemented")
    else
        % Required only to avoid warnings; never executed.
        IsActive = is_active
    ).

:- pragma foreign_proc("C",
    complexity_call_proc(Slot::out),
    [thread_safe, will_not_call_mercury],
"
    // Mention Slot to avoid warning.
    MR_fatal_error(""complexity_call_proc"");
").

complexity_call_proc(Slot) :-
    impure private_builtin.imp,
    % Required only to avoid warnings; never executed.
    private_builtin.unsafe_type_cast(0, Slot).

:- pragma foreign_proc("C",
    complexity_exit_proc(Slot::in),
    [thread_safe, will_not_call_mercury],
"
    // Mention Slot to avoid warning.
    MR_fatal_error(""complexity_exit_proc"");
").

complexity_exit_proc(_Slot) :-
    impure private_builtin.imp.

:- pragma foreign_proc("C",
    complexity_fail_proc(Slot::in),
    [thread_safe, will_not_call_mercury],
"
    // Mention Slot to avoid warning.
    MR_fatal_error(""complexity_fail_proc"");
").

complexity_fail_proc(_Slot) :-
    impure private_builtin.imp,
    fail.

:- pragma foreign_proc("C",
    complexity_redo_proc(Slot::in),
    [thread_safe, will_not_call_mercury],
"
    // Mention Slot to avoid warning.
    MR_fatal_error(""complexity_redo_proc"");
").

complexity_redo_proc(_Slot) :-
    impure private_builtin.imp,
    fail.

%---------------------------------------------------------------------------%
:- end_module term_size_prof_builtin.
%---------------------------------------------------------------------------%
