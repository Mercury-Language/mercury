%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test the warning for infinite recursion.
%
%---------------------------------------------------------------------------%

:- module infinite_recursion.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.
:- import_module list.

main(!IO) :-
    ( if
        funny_append([1, 2, 3], [4, 5, 6], Ns),
        sum(Ns, 0, Sum),
        Sum < 42
    then
        main(!IO)
    else
        loop
    ).

:- pred loop is det.

loop :-
    ( if semidet_succeed then
        loop
    else
        true
    ).

:- pred funny_append(list(T)::in, list(T)::in, list(T)::out) is det.

funny_append(L1, L2, L3) :-
    (
        L1 = [], L2 = L3
    ;
        L1 = [X | _Xs], L3 = [X | Zs],
        funny_append(L1, L2, Zs)        % L1 should be _Xs.
    ).

:- pred sum(list(int)::in, int::in, int::out) is det.

sum([], !Sum).
sum([N | Ns], !Sum) :-
    !:Sum = !.Sum + N,
    sum([N | Ns], !Sum).        % [N | Ns] should be N.
