%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% This module tests the loop invariant hoisting optimization.
% It does so using foreign_procs which abort if called twice.
% If loop invariant hoisting works, these procedures will only
% be called once, but if loop invariant hoisting doesn't work,
% these procedures will abort.

% This test checks that we can do loop invariant hoisting for calls
% which occur in different branches of an if-then-else.

% XXX we do not yet pass this test case (see XXX comment below).
% Should be easy to fix??

:- module loop_inv_test0.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.
:- import_module string.

main(!IO) :-
    io.print_line("enter three integers, one on each line", !IO),
    io.flush_output(!IO),
    io.read_line_as_string(Res1, !IO),
    io.read_line_as_string(Res2, !IO),
    io.read_line_as_string(Res3, !IO),
    ( if Res1 = ok(L1), Res2 = ok(L2), Res3 = ok(L3) then
        N1 = string.det_to_int(string.chomp(L1)),
        N2 = string.det_to_int(string.chomp(L2)),
        N3 = string.det_to_int(string.chomp(L3)),
        loop1(N1, N2, N3, R1),
        loop2(N1, N2, N3, R2),
        io.print("R1 = ", !IO), io.print_line(R1, !IO),
        io.print("R2 = ", !IO), io.print_line(R2, !IO)
    ;
        io.print_line("input error", !IO)
    ).

    %  Test that we can do ordinary loop hoisting:
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

    % Test that we can do loop hoisting for calls which occur in
    % different branches of an if-then-else: q/1 will abort if called twice.
    % XXX currently loop invariant hoisting does NOT optimize this case,
    % because the variable `X' gets renamed to two different variables
    % for the two different scopes in which it occurs, and then loop
    % invariant hoisting is not smart enough to notice that the two calls
    % are the same.
    %
:- pred loop2(int::in, int::in, int::in, int::out) is det.

loop2(N, Inv, Acc0, Acc) :-
    ( N =< 0 ->
        Acc = Acc0
    ;
        ( r(N, 3) ->
            q(Inv, X),
            Acc1 = Acc0 * 2 + X + 1
        ;
            q(Inv, X),
            Acc1 = Acc0 * 2 + X
        ),
        loop2(N - 1, Inv, Acc1, Acc)
    ).

:- pragma no_inline(p/2).
:- pragma no_inline(q/2).

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
:- pragma foreign_proc("C#",
    p(Inv::in, X::out),
    [will_not_call_mercury, promise_pure],
"
    /* Test that p/1 only gets called once. */
    static int num_calls = 0;
    if (num_calls++) {
        mercury.runtime.Errors.fatal_error(""p/1 called more than once"");
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
:- pragma foreign_proc("C#",
    q(Inv::in, X::out),
    [will_not_call_mercury, promise_pure],
"
    /* Test that q/1 only gets called once. */
    static int num_calls = 0;
    if (num_calls++) {
        mercury.runtime.Errors.fatal_error(""q/1 called more than once"");
    }

    X = Inv + 53;
").

:- pragma no_inline(r/2).
:- pred r(int::in, int::in) is semidet.
r(X, Y) :-
    X > Y.

%---------------------------------------------------------------------------%
