%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This module tests the loop invariant hoisting optimization.
% It does so using foreign_procs which abort if called twice:
% if loop invariant hoisting works, these procedures will only
% be called once, but if loop invariant hoisting doesn't work,
% these procedures will abort.
%
% This test checks that we do loop invariant hoisting for
% invariant goals which have no input arguments.
%

:- module loop_inv_test_1.
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

    % Test that we can do ordinary loop hoisting, for a goal with no inputs.
    % p/1 will abort if called twice.
    %
:- pred loop1(int::in, int::in, int::out) is det.

loop1(N, Acc0, Acc) :-
    ( if N =< 0 then
        Acc = Acc0
    else
        p(X),
        Acc1 = Acc0 + X,
        loop1(N - 1, Acc1, Acc)
    ).

    % Test that we can do ordinary loop hoisting for a goal with no inputs,
    % in the particular case where the invariant goal is an inline
    % foreign_proc. q/1 will abort if called twice.
    %
:- pred loop2(int::in, int::in, int::out) is det.

loop2(N, Acc0, Acc) :-
    ( if N =< 0 then
        Acc = Acc0
    else
        q(X),
        Acc1 = Acc0 + X,
        loop2(N - 1, Acc1, Acc)
    ).

:- pragma inline(p/1).
:- pragma no_inline(q/1).

:- pred p(int::out) is det.
:- pragma foreign_proc("C",
    p(X::out),
    [will_not_call_mercury, promise_pure],
"
    // Test that p/1 only gets called once.
    static int num_calls = 0;
    if (num_calls++) {
        MR_fatal_error(""p/1 called more than once"");
        abort();
    }

    X = 42;
").
:- pragma foreign_code("C#", "static int p_num_calls = 0;").
:- pragma foreign_proc("C#",
    p(X::out),
    [will_not_call_mercury, promise_pure],
"
    // Test that p/1 only gets called once.
    if (p_num_calls++ > 0) {
        mercury.runtime.Errors.fatal_error(""p/1 called more than once"");
    }

    X = 42;
").
:- pragma foreign_code("Java", "static int p_num_calls = 0;").
:- pragma foreign_proc("Java",
    p(X::out),
    [will_not_call_mercury, promise_pure],
"
    // Test that p/1 only gets called once.
    if (p_num_calls++ > 0) {
        throw new Error(""p/1 called more than once"");
    }

    X = 42;
").

:- pred q(int::out) is det.
:- pragma foreign_proc("C",
    q(X::out),
    [will_not_call_mercury, promise_pure],
"
    // Test that q/1 only gets called once.
    static int num_calls = 0;
    if (num_calls++) {
        MR_fatal_error(""q/1 called more than once"");
    }

    X = 53;
").
:- pragma foreign_code("C#", "static int q_num_calls = 0;").
:- pragma foreign_proc("C#",
    q(X::out),
    [will_not_call_mercury, promise_pure],
"
    // Test that q/1 only gets called once.
    if (q_num_calls++ > 0) {
        mercury.runtime.Errors.fatal_error(""q/1 called more than once"");
    }

    X = 53;
").
:- pragma foreign_code("Java", "static int q_num_calls = 0;").
:- pragma foreign_proc("Java",
    q(X::out),
    [will_not_call_mercury, promise_pure],
"
    // Test that q/1 only gets called once.
    if (q_num_calls++ > 0) {
        throw new Error(""q/1 called more than once"");
    }

    X = 53;
").

%---------------------------------------------------------------------------%
