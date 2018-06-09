%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002, 2004-2007 The University of Melbourne.
% Copyright (C) 2014-2015, 2017-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: io_action.m.
% Author: zs.
%
% This module defines the representation of I/O actions used by the
% declarative debugger.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module mdb.io_action.
:- interface.

:- import_module mdb.browser_term.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.

:- import_module list.
:- import_module io.
:- import_module univ.

%---------------------------------------------------------------------------%

:- type io_action
    --->    io_action(
                io_action_proc_name :: string,
                io_action_pf        :: pred_or_func,
                io_action_args      :: list(univ)
            ).

:- type maybe_tabled_io_action
    --->    tabled(io_action)
    ;       untabled.

:- type io_seq_num  == int.

:- type io_action_range
    --->    io_action_range(
                from_io_action      :: io_seq_num,
                to_io_action        :: io_seq_num
            ).

:- pred get_maybe_io_action(io_seq_num::in, maybe_tabled_io_action::out,
    io::di, io::uo) is det.

:- func io_action_to_browser_term(io_action) = browser_term.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module maybe.

%---------------------------------------------------------------------------%

get_maybe_io_action(IoActionNum, MaybeTabledIoAction, !IO) :-
    pickup_io_action(IoActionNum, MaybeIoAction, !IO),
    (
        MaybeIoAction = yes(IoAction),
        MaybeTabledIoAction = tabled(IoAction)
    ;
        MaybeIoAction = no,
        MaybeTabledIoAction = untabled
    ).

io_action_to_browser_term(IoAction) = Term :-
    IoAction = io_action(ProcName, PredFunc, Args),
    (
        PredFunc = pf_predicate,
        IsFunc = no
    ;
        PredFunc = pf_function,
        IsFunc = yes
    ),
    Term = synthetic_term_to_browser_term(ProcName, Args, IsFunc).

:- pred pickup_io_action(int::in, maybe(io_action)::out, io::di, io::uo)
    is det.

:- pragma foreign_proc("C",
    pickup_io_action(SeqNum::in, MaybeIOAction::out, S0::di, S::uo),
    [thread_safe, promise_pure, tabled_for_io, may_call_mercury],
"{
    const char  *problem;
    const char  *proc_name;
    MR_bool     io_action_tabled;
    MR_Word     is_func;
    MR_bool     have_arg_infos;
    MR_Word     args;
    MR_String   ProcName;

    MR_save_transient_hp();
    io_action_tabled = MR_trace_get_action(SeqNum, &proc_name, &is_func,
        &have_arg_infos, &args);
    MR_restore_transient_hp();

    /* cast away const */
    ProcName = (MR_String) (MR_Integer) proc_name;
    if (io_action_tabled && have_arg_infos) {
        MaybeIOAction = MR_IO_ACTION_make_yes_io_action(ProcName, is_func,
            args);
    } else {
        MaybeIOAction = MR_IO_ACTION_make_no_io_action();
    }

    S = S0;
}").

:- pragma no_determinism_warning(pickup_io_action/4).
pickup_io_action(_, _, _, _) :-
    private_builtin.sorry("pickup_io_action").

:- func make_no_io_action = maybe(io_action).
:- pragma foreign_export("C", make_no_io_action = out,
    "MR_IO_ACTION_make_no_io_action").

make_no_io_action = no.

:- func make_yes_io_action(string, bool, list(univ)) = maybe(io_action).
:- pragma foreign_export("C", make_yes_io_action(in, in, in) = out,
    "MR_IO_ACTION_make_yes_io_action").

make_yes_io_action(ProcName, yes, Args) =
    yes(io_action(ProcName, pf_function, Args)).
make_yes_io_action(ProcName, no, Args) =
    yes(io_action(ProcName, pf_predicate, Args)).
