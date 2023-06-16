%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Regression test.  Loop invariant hoisting incorrectly treated `unused'
% arguments in calls like outputs.
%---------------------------------------------------------------------------%

:- module loop_inv_test3.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.

%---------------------------------------------------------------------------%

main(!IO) :-
    loop(10, 10, [], L),
    io.write(L, !IO),
    io.nl(!IO).

:- pred loop(int::in, int::in, list(int)::in, list(int)::out) is det.

loop(Factor, N, L0, L) :-
    Value = foo(Factor, N1),
    bar(N, N1),
    ( if N1 = 0 then
        L = L0
    else
        L1 = [Factor * Value | L0],
        loop(Factor, N - 1, L1, L)
    ).

:- func foo(int::in, int::unused) = (int::out) is det.
:- pragma no_inline(foo/2).

foo(F, _) = F.

:- pred bar(int::in, int::out) is det.
:- pragma no_inline(bar/2).

bar(N, N).
