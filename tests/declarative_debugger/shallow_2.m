%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module shallow_2.
:- interface.

:- pred p(string::in, int::in, int::out) is det.
:- pred q(string::in, int::in, int::out) is det.
:- pred r(string::in, int::in, int::out) is det.

:- implementation.
:- import_module shallow_3.

p(S, M, N) :-
    ( if pp(S, 1, M) then
        N = 1
    else
        N = -11
    ).

:- pred pp(string::in, int::in, int::out) is multi.

pp(S, M, N) :-
    a(S, M, N).
pp(S, M, N) :-
    b(S, M, N).

q(S, M, N) :-
    ( if a(S, M, -1) then
        N = 11
    else
        N = 2
    ).

r(S, M, N) :-
    ( if
        not a(S, M, -3),
        b(S, M, 5)
    then
        N = 23
    else
        N = 0
    ).

% shallow_3 defines:
%
% :- pred a(string::in, int::in, int::out) is multi.
%
% a(_, X, X).
% a(_, _, 0).
%
% :- pred b(string::in, int::in, int::out) is det.
%
% b(_, _, 5).
