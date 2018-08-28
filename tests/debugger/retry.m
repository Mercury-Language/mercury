%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test case tests the handling of the direct retries in mdb in the
% situations that are most likely to cause problems. These are:
%
% - retry from within a committed-choice context
% - retry from a model_non context
% - retry that requires resetting the call table tips of some tabled
%   procedures.
%
% Note: Don't try to print the call stack from within solutions in the input
% script, since the results will depend on whether std_util.m was compiled
% with debugging or not.

:- module retry.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module solutions.

main(!IO) :-
    det_without_cut(1, A),
    det_with_cut(2, B),
    det_with_cut(3, C),
    solutions(nondet(4), Ds),
    solutions(nondet(5), Es),

    output(A, !IO),
    output(B, !IO),
    output(C, !IO),
    outputs(Ds, !IO),
    outputs(Es, !IO),
    fib(15, F),
    output(F, !IO),
    solutions(t(1, 2), T12),
    outputs(T12, !IO).

%---------------------------------------------------------------------------%

:- pred det_without_cut(int::in, int::out) is det.

det_without_cut(X0, X) :-
    det_without_cut_1(X0 + 2, X).

:- pred det_without_cut_1(int::in, int::out) is det.

det_without_cut_1(X0, X1 * 2) :-
    det_without_cut_2(X0, X1).

:- pred det_without_cut_2(int::in, int::out) is det.

det_without_cut_2(X, X).

%---------------------------------------------------------------------------%

:- pred det_with_cut(int::in, int::out) is det.

det_with_cut(X0, X) :-
    ( if det_with_cut_1(X0, _) then
        X = X0 * 2
    else
        X = X0 * 3
    ).

:- pred det_with_cut_1(int::in, int::out) is nondet.

det_with_cut_1(X0, X) :-
    X0 = 2,
    (
        det_with_cut_2(15, X)
    ;
        X = 10
    ).

:- pred det_with_cut_2(int::in, int::out) is det.

det_with_cut_2(X, X).

%---------------------------------------------------------------------------%

:- pred nondet(int::in, int::out) is multi.

nondet(X0, X) :-
    nondet_1(X0, X1),
    nondet_2(X1, X2),
    ( if X2 < 75 then
        X = X2
    else
        X = 2 * X2
    ).

:- pred nondet_1(int::in, int::out) is multi.

nondet_1(X0, X) :-
    X1 = 10 * X0,
    (
        X = X1
    ;
        X = X1 + 1
    ).

:- pred nondet_2(int::in, int::out) is det.

nondet_2(X, X).

%---------------------------------------------------------------------------%

:- pred fib(int::in, int::out) is det.
:- pragma memo(fib/2).

fib(N, F) :-
    ( if N < 2 then
        F = 1
    else
        fib(N - 1, F1),
        fib(N - 2, F2),
        F = F1 + F2
    ).

:- pred t(int::in, int::in, int::out) is nondet.
:- pragma memo(t/3).

t(A, B, C) :-
    marker("t", A, B, Zero),
    ( if A = 1 then
        (
            C = Zero + (A * 100) + (B * 10)
        ;
            C = Zero + (B * 100) + (A * 10)
        )
    else
        fail
    ).

:- pred marker(string::in, int::in, int::in, int::out) is det.

:- pragma foreign_proc("C",
    marker(S::in, A::in, B::in, X::out),
    [will_not_call_mercury, promise_pure],
"
    printf(""marker executed: %s %d %d\\n"", S, A, B);
    X = 0;
").

%---------------------------------------------------------------------------%

:- pred output(int::in, io::di, io::uo) is det.

output(X, !IO) :-
    io.write_int(X, !IO),
    io.write_string("\n", !IO).

:- pred outputs(list(int)::in, io::di, io::uo) is det.

outputs(Xs, !IO) :-
    outputs1(Xs, !IO),
    io.write_string("\n", !IO).

:- pred outputs1(list(int)::in, io::di, io::uo) is det.

outputs1([], !IO).
outputs1([X | Xs], !IO) :-
    io.write_int(X, !IO),
    io.write_string(" ", !IO),
    outputs1(Xs, !IO).
