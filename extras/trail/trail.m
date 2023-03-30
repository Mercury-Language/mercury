%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2007 The University of Melbourne.
% Copyright (C) 2013, 2015, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% Author: Mark Brown.
%
% Mercury interface to the function trailing facilities.
%
% See the Trailing section of the Mercury Language Reference Manual for
% further information.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module trail.
:- interface.

:- import_module io.

%---------------------------------------------------------------------------%

    % The various reasons why a trail function may be called.
    %
:- type untrail_reason
    --->    untrail_undo
    ;       untrail_exception
    ;       untrail_retry
    ;       untrail_commit
    ;       untrail_solve
    ;       untrail_gc.

    % Textual name of the untrail reason.
    %
:- pred reason_name(untrail_reason, string).
:- mode reason_name(in, out) is det.
:- mode reason_name(out, in) is semidet.

%---------------------------------------------------------------------------%

    % Call the supplied closure when untrailing past this point.
    %
:- impure pred trail_closure(impure pred(untrail_reason)::in(pred(in) is det))
    is det.

    % As above, but using the I/O state rather than being impure.
    %
:- pred trail_closure_io(
    pred(untrail_reason, io, io)::in(pred(in, di, uo) is det),
    io::di, io::uo) is det.

    % Call the supplied closure on backtracking (that is, when the
    % untrail_reason is undo, exception or retry).
    %
:- impure pred trail_closure_on_backtrack(impure (pred)::in((pred) is det))
    is det.

    % As above, but using the I/O state rather than being impure.
    %
:- pred trail_closure_on_backtrack_io(pred(io, io)::in(pred(di, uo) is det),
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % Abstract type used to hold the identity of a choicepoint.
    %
:- type choicepoint_id.

    % Get the current choicepoint.
    %
:- impure func current_choicepoint_id = choicepoint_id.

    % Get the "null" choicepoint id.
    %
:- func null_choicepoint_id = choicepoint_id.

    % Compare choicepoints for which is newer.
    % See the reference manual for details.
    %
:- pred choicepoint_newer(choicepoint_id::in, choicepoint_id::in) is semidet.

    % Cast to an integer.
    %
:- func choicepoint_id_to_int(choicepoint_id) = int.

%---------------------------------------------------------------------------%

    % Output a debugging message when untrailing past this point.
    %
:- impure pred debug_trail(io.output_stream::in) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- pragma require_feature_set([trailing]).

:- import_module exception.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

:- pragma foreign_enum("C", untrail_reason/0, [
    untrail_undo        -   "MR_undo",
    untrail_exception   -   "MR_exception",
    untrail_retry       -   "MR_retry",
    untrail_commit      -   "MR_commit",
    untrail_solve       -   "MR_solve",
    untrail_gc          -   "MR_gc"
]).

reason_name(untrail_undo,       "undo").
reason_name(untrail_exception,  "exception").
reason_name(untrail_retry,      "retry").
reason_name(untrail_commit,     "commit").
reason_name(untrail_solve,      "solve").
reason_name(untrail_gc,         "gc").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    trail_closure(Pred::in(pred(in) is det)),
    [will_not_call_mercury],
"
    MR_trail_function(ML_call_trail_closure_save_regs, (void *) Pred);
").

:- pragma foreign_proc("C",
    trail_closure_io(Pred::in(pred(in, di, uo) is det), _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    MR_trail_function(ML_call_trail_closure_save_regs, (void *) Pred);
").

:- pragma foreign_proc("C",
    trail_closure_on_backtrack(Pred::in((pred) is det)),
    [will_not_call_mercury],
"
    MR_trail_function(ML_call_trail_closure_on_backtrack, (void *) Pred);
").

:- pragma foreign_proc("C",
    trail_closure_on_backtrack_io(Pred::in(pred(di, uo) is det),
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    MR_trail_function(ML_call_trail_closure_on_backtrack, (void *) Pred);
").

:- pragma foreign_decl("C", "
    #define MR_copy_fake_regs(src, dest)                                \\
        do {                                                            \\
            MR_memcpy(dest, src, sizeof(MR_Word) * MR_MAX_FAKE_REG);    \\
        } while(0)

    extern void
    ML_call_trail_closure_save_regs(void *pred, MR_untrail_reason reason);

    extern void
    ML_call_trail_closure_on_backtrack(void *pred, MR_untrail_reason reason);
").

:- pragma foreign_code("C", "
    void ML_call_trail_closure_save_regs(void *pred, MR_untrail_reason reason)
    {
        MR_Word     saved_regs[MR_MAX_FAKE_REG];

        // The current implementation of trailing does not preserve live
        // (real or fake) registers across calls to MR_reset_ticket.
        // Since the called Mercury code is likely to modify these,
        // we better make a copy here and restore them afterwards.

        MR_save_registers();
        MR_copy_fake_regs(MR_fake_reg, saved_regs);
        ML_call_trail_closure((MR_Word) pred, reason);
        MR_copy_fake_regs(saved_regs, MR_fake_reg);
        MR_restore_registers();
    }

    void ML_call_trail_closure_on_backtrack(void *pred,
        MR_untrail_reason reason)
    {
        MR_Word     saved_regs[MR_MAX_FAKE_REG];

        switch(reason) {
            case MR_undo:       // Fall through.
            case MR_exception:  // Fall through.
            case MR_retry:
                // See comment in ML_call_trail_closure_save_regs, above.
                MR_save_registers();
                MR_copy_fake_regs(MR_fake_reg, saved_regs);
                ML_call_pred((MR_Word) pred);
                MR_copy_fake_regs(saved_regs, MR_fake_reg);
                MR_restore_registers();
                break;

            case MR_solve:  // Fall through.
            case MR_commit:
                break;

            default:
                MR_fatal_error(""trail.m: unknown MR_untrail_reason"");
        }
    }
").

:- pragma foreign_export("C",
    call_pred(in(pred(di, uo) is det), di, uo),
    "ML_call_pred").

:- pred call_pred(pred(io, io)::in(pred(di, uo) is det), io::di, io::uo)
    is det.

call_pred(Pred, !IO) :-
    Pred(!IO).

:- pragma foreign_export("C",
    call_trail_closure(in(pred(in, di, uo) is det), in, di, uo),
    "ML_call_trail_closure").

:- pred call_trail_closure(
    pred(untrail_reason, io, io)::in(pred(in, di, uo) is det),
    untrail_reason::in, io::di, io::uo) is det.

call_trail_closure(Pred, Reason, !IO) :-
    Pred(Reason, !IO).

%---------------------------------------------------------------------------%

    % NOTE: it is safe to pass this as a Mercury type, since
    % `sizeof(MR_ChoicepointId) == sizeof(MR_Word)'.
    %
:- pragma foreign_type("C", choicepoint_id, "MR_ChoicepointId",
    [can_pass_as_mercury_type]).

:- pragma foreign_proc("C",
    current_choicepoint_id = (Id::out),
    [will_not_call_mercury],
"
    Id = MR_current_choicepoint_id();
").

:- pragma foreign_proc("C",
    null_choicepoint_id = (Id::out),
    [promise_pure, thread_safe, will_not_call_mercury],
"
    Id = MR_null_choicepoint_id();
").

:- pragma foreign_proc("C",
    choicepoint_newer(A::in, B::in),
    [promise_pure, thread_safe, will_not_call_mercury],
"
    SUCCESS_INDICATOR = MR_choicepoint_newer(A, B);
").

:- pragma foreign_proc("C",
    choicepoint_id_to_int(CP::in) = (N::out),
    [promise_pure, thread_safe, will_not_call_mercury],
"
    N = (MR_Integer) CP;
").

%---------------------------------------------------------------------------%

debug_trail(S) :-
    impure CP = current_choicepoint_id,
    trace [io(!IO)] (
        debug_trail_print(S, "setup", CP, !IO)
    ),
    impure trail_closure(debug_trail_pred(S, CP)).

:- impure pred debug_trail_pred(io.output_stream::in, choicepoint_id::in,
    untrail_reason::in) is det.

debug_trail_pred(S, CP, Reason) :-
    impure impure_true,
    reason_name(Reason, Name),
    trace [io(!IO)] (
        debug_trail_print(S, Name, CP, !IO)
    ).

:- pred debug_trail_print(io.output_stream::in, string::in, choicepoint_id::in,
    io::di, io::uo) is det.

debug_trail_print(S, Name, CP, !IO) :-
    N = choicepoint_id_to_int(CP),
    io.format(S, "TRAIL: %-10s %d\n", [s(Name), i(N)], !IO).

%---------------------------------------------------------------------------%
:- end_module trail.
%---------------------------------------------------------------------------%
