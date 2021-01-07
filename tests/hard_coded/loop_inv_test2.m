%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
%
% This module tests the loop invariant hoisting optimization.
% It does so using foreign_procs which abort if called twice:
% if loop invariant hoisting works, these procedures will only
% be called once, but if loop invariant hoisting doesn't work,
% these procedures will abort.
%
% This test checks that we do loop invariant hoisting for calls
% that occur in the condition of an if-then-else in the loop body.
%
% XXX We do not yet pass this test case, because the current loop
% invariant hoisting only hoists det subgoals.

:- module loop_inv_test2.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.
:- import_module string.

main(!IO) :-
    io.print("enter two integers, one on each line\n", !IO),
    io.flush_output(!IO),
    io.read_line_as_string(Res1, !IO),
    io.read_line_as_string(Res2, !IO),
    ( if Res1 = ok(L1), Res2 = ok(L2) then
        N1 = string.det_to_int(string.chomp(L1)),
        N2 = string.det_to_int(string.chomp(L2)),
        loop1(N1, N2, R1),
        loop2(N1, N2, R2),
        io.print("R1 = ", !IO), io.print_line(R1, !IO),
        io.print("R2 = ", !IO), io.print_line(R2, !IO)
    else
        io.print_line("input error", !IO)
    ).

:- pred loop1(int::in, int::in, int::out) is det.

loop1(N, Acc0, Acc) :-
    ( if N =< 0 then
        Acc = Acc0
    else
        ( if p(X) then
            Acc1 = Acc0 + X
        else
            Acc1 = Acc0 * 10
        ),
        loop1(N - 1, Acc1, Acc)
    ).

:- pred loop2(int::in, int::in, int::out) is det.

loop2(N, Acc0, Acc) :-
    ( if N =< 0 then
        Acc = Acc0
    else
        ( if q(X), r(N, X) then
            Acc1 = Acc0 + X
        else
            Acc1 = Acc0 * 10
        ),
        loop2(N - 1, Acc1, Acc)
    ).

:- pred p(int::out) is semidet.
:- pragma foreign_proc("C",
    p(X::out),
    [will_not_call_mercury, promise_pure],
"
    /* Test that p/1 only gets called once. */
    static int num_calls = 0;
    if (num_calls++) {
        MR_fatal_error(""loop_inv failed -- p/1 called twice"");
    }

    X = 42;
    SUCCESS_INDICATOR = MR_TRUE;
").
:- pragma foreign_proc("C#",
    p(X::out),
    [will_not_call_mercury, promise_pure],
"
    /* Test that p/1 only gets called once. */
    static int num_calls = 0;
    if (num_calls++) {
        mercury.runtime.Errors.fatal_error(
            ""loop_inv failed -- p/1 called twice"");
    }

    X = 42;
    SUCCESS_INDICATOR = true;
").

:- pred q(int::out) is det.
:- pragma foreign_proc("C",
    q(X::out),
    [will_not_call_mercury, promise_pure],
"
    /* Test that q/1 only gets called once. */
    static int num_calls = 0;
    if (num_calls++) {
        MR_fatal_error(""loop_inv failed -- q/1 called twice"");
    }

    X = 42;
").
:- pragma foreign_proc("C#",
    q(X::out),
    [will_not_call_mercury, promise_pure],
"
    /* Test that q/1 only gets called once. */
    static int num_calls = 0;
    if (num_calls++) {
        mercury.runtime.Errors.fatal_error(
            ""loop_inv failed -- q/1 called twice"");
    }

    X = 42;
").

:- pragma no_inline(r/2).
:- pred r(int::in, int::in) is semidet.
r(X, Y) :-
    X - 40 > Y.

%---------------------------------------------------------------------------%
