%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%

% This module tests the loop invariant hoisting optimization.
% It does so using foreign_procs which abort if called twice:
% if loop invariant hoisting works, these procedures will only
% be called once, but if loop invariant hoisting doesn't work,
% these procedures will abort.

% This test checks that we do loop invariant hoisting for calls
% that occur in the condition of an if-then-else in the loop body.

:- module loop_inv_test2.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int, string.

main -->
    io__print("enter two integers, one on each line\n"), io__flush_output,
    io__read_line_as_string(Res1),
    io__read_line_as_string(Res2),
    ( { Res1 = ok(L1), Res2 = ok(L2) } ->
	    { N1 = string__det_to_int(string__chomp(L1)) },
	    { N2 = string__det_to_int(string__chomp(L2)) },
	    { loop1(N1, N2, R1) },
	    { loop2(N1, N2, R2) },
	    io__print("R1 = "), io__print(R1), io__nl,
	    io__print("R2 = "), io__print(R2), io__nl
    ;
        io__print("input error"), io__nl
    ).

:- pred loop1(int::in, int::in, int::out) is det.
loop1(N, Acc0, Acc) :-
    ( N =< 0 ->
        Acc = Acc0
    ;
        ( p(X) ->
            Acc1 = Acc0 + X
        ;
            Acc1 = Acc0 * 10
        ),
        loop1(N - 1, Acc1, Acc)
    ).

:- pred loop2(int::in, int::in, int::out) is det.
loop2(N, Acc0, Acc) :-
    ( N =< 0 ->
        Acc = Acc0
    ;
        ( q(X), r(N, X) ->
            Acc1 = Acc0 + X
        ;
            Acc1 = Acc0 * 10
        ),
        loop2(N - 1, Acc1, Acc)
    ).

:- pred p(int::out) is semidet.
:- pragma foreign_proc("C", p(X::out),
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
:- pragma foreign_proc("C#", p(X::out),
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
:- pragma foreign_proc("C", q(X::out),
    [will_not_call_mercury, promise_pure],
"
    /* Test that p/1 only gets called once. */
    static int num_calls = 0;
    if (num_calls++) { 
        MR_fatal_error(""loop_inv failed -- p/1 called twice"");
    }

    X = 42;
").
:- pragma foreign_proc("C#", q(X::out),
    [will_not_call_mercury, promise_pure],
"
    /* Test that p/1 only gets called once. */
    static int num_calls = 0;
    if (num_calls++) { 
        mercury.runtime.Errors.fatal_error(
            ""loop_inv failed -- q/1 called twice"");
    }

    X = 42;
").

:- pragma no_inline(r/2).
:- pred r(int::in, int::in) is semidet.
r(X, Y) :- X - 40 > Y.

%-----------------------------------------------------------------------------%
