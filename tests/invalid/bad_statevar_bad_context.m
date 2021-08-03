
%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module bad_statevar_bad_context.

:- interface.

:- import_module io.

:- pred test_pred(int::in, int::in, io::di, io::uo) is det.

:- implementation.

:- import_module float.
:- import_module int.

:- type t
    --->    f(int, int, float).

test_pred(A, !.B, !IO) :-
    q(!B),
    % The !.A is a reference to a nonexistent state variable.
    % The bug we are testing for is that this used to cause state_var.m
    % to leave the context of the second arg of f initialized to the
    % dummy context, which typecheck.m then replaced with the context
    % of the clause as a whole, which is the context of the clause head.
    % The type error in the unification with T was thus reported
    % with the context of the clause head, which is quite confusing.
    T = f(!.B, A, !.A),
    io.write_line(T, !IO).

:- pred q(int::in, int::out) is det.

q(A, A).
