%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module shallow2.

:- interface.

:- import_module list.

:- pred safe(list(int)::in) is semidet.

:- implementation.

:- import_module int.

safe([]).
safe([N | L]) :-
    nodiag(N, 1, L),
    safe(L).

:- pred nodiag(int::in, int::in, list(int)::in) is semidet.

nodiag(_, _, []).
nodiag(B, D, [N | L]) :-
    NmB = N - B,
    BmN = B - N,
    ( if D = NmB then
        fail
    else if D = BmN then
        fail
    else
        true
    ),
    D1 = D + 1,
    nodiag(B, D1, L).
