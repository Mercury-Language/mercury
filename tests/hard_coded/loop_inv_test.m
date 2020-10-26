%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

% This module tests the loop invariant hoisting optimization.
% It does so using foreign_procs which abort if called twice:
% if loop invariant hoisting works, these procedures will only
% be called once, but if loop invariant hoisting doesn't work,
% these procedures will abort.

% This test checks that we do the basics of loop invariant hoisting.

:- module loop_inv_test.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.
:- import_module string.

main -->
    io__print("enter three integers, one on each line\n"), io__flush_output,
    io__read_line_as_string(Res1),
    io__read_line_as_string(Res2),
    io__read_line_as_string(Res3),
    ( { Res1 = ok(L1), Res2 = ok(L2), Res3 = ok(L3) } ->
        { N1 = string__det_to_int(string__chomp(L1)) },
        { N2 = string__det_to_int(string__chomp(L2)) },
        { N3 = string__det_to_int(string__chomp(L3)) },
        { loop1(N1, N2, N3, R1) },
        { loop2(N1, N2, N3, R2) },
        io__print("R1 = "), io__print(R1), io__nl,
        io__print("R2 = "), io__print(R2), io__nl
    ;
        io__print("input error"), io__nl
    ).

    % Test that we can do ordinary loop hoisting:
    % p/1 will abort if called twice.
    %
:- pred loop1(int::in, int::in, int::in, int::out) is det.

loop1(N, Inv, Acc0, Acc) :-
    ( N =< 0 ->
        Acc = Acc0
    ;
        p(Inv, X),
        Acc1 = Acc0 + X,
        loop1(N - 1, Inv, Acc1, Acc)
    ).

    % Test that we can do ordinary loop hoisting, in the case
    % where the invariant predicate is an inlined foreign_proc
    % q/1 will abort if called twice.
    %
:- pred loop2(int::in, int::in, int::in, int::out) is det.

loop2(N, Inv, Acc0, Acc) :-
    ( N =< 0 ->
        Acc = Acc0
    ;
        q(Inv, X),
        Acc1 = Acc0 + X,
        loop2(N - 1, Inv, Acc1, Acc)
    ).

:- pragma no_inline(p/2).
:- pragma inline(q/2).

:- pred p(int::in, int::out) is det.
:- pragma foreign_proc("C",
    p(Inv::in, X::out),
    [will_not_call_mercury, promise_pure],
"
    /* Test that p/1 only gets called once. */
    static int num_calls = 0;
    if (num_calls++) {
        MR_fatal_error(""p/1 called more than once"");
        abort();
    }

    X = Inv + 42;
").
:- pragma foreign_code("C#", "static int p_num_calls = 0;").
:- pragma foreign_proc("C#",
    p(Inv::in, X::out),
    [will_not_call_mercury, promise_pure],
"
    /* Test that p/1 only gets called once. */
    if (p_num_calls++ > 0) {
        mercury.runtime.Errors.fatal_error(""p/1 called more than once"");
    }

    X = Inv + 42;
").
:- pragma foreign_code("Java", "static int p_num_calls = 0;").
:- pragma foreign_proc("Java",
    p(Inv::in, X::out),
    [will_not_call_mercury, promise_pure],
"
    /* Test that p/1 only gets called once. */
    if (p_num_calls++ > 0) {
        throw new Error(""p/1 called more than once"");
    }

    X = Inv + 42;
").

:- pred q(int::in, int::out) is det.
:- pragma foreign_proc("C",
    q(Inv::in, X::out),
    [will_not_call_mercury, promise_pure],
"
    /* Test that q/1 only gets called once. */
    static int num_calls = 0;
    if (num_calls++) {
        MR_fatal_error(""q/1 called more than once"");
    }

    X = Inv + 53;
").
:- pragma foreign_code("C#", "static int q_num_calls = 0;").
:- pragma foreign_proc("C#",
    q(Inv::in, X::out),
    [will_not_call_mercury, promise_pure],
"
    /* Test that q/1 only gets called once. */
    if (q_num_calls++ > 0) {
        mercury.runtime.Errors.fatal_error(""q/1 called more than once"");
    }

    X = Inv + 53;
").
:- pragma foreign_code("Java", "static int q_num_calls = 0;").
:- pragma foreign_proc("Java",
    q(Inv::in, X::out),
    [will_not_call_mercury, promise_pure],
"
    /* Test that q/1 only gets called once. */
    if (q_num_calls++ > 0) {
        throw new Error(""q/1 called more than once"");
    }

    X = Inv + 53;
").

%---------------------------------------------------------------------------%
