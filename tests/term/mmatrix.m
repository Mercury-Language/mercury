%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%------------------------------------------------------------------------------
%
% Benchmark Program - matrix*matrix multiplication
%
% Copyright by Manuel Hermenegildo
% Date: January 17 1986
%
%------------------------------------------------------------------------------

:- module mmatrix.

:- interface.

:- type list(T)
    --->    []
    ;       [T | list(T)].

:- pred mmultiply(list(list(int)), list(list(int)), list(list(int))).
:- mode mmultiply(in, in, out) is semidet.

:- implementation.

:- import_module int.
:- import_module prolog.

mmultiply([], _, []).
mmultiply([V0 | Rest], V1, [Result | Others]) :-
    mmultiply(Rest, V1, Others),
    multiply(V1, V0, Result).

:- pred multiply(list(list(int)), list(int), list(int)).
:- mode multiply(in, in, out).

multiply([], _, []).
multiply([V0 | Rest], V1, [Result | Others]) :-
    multiply(Rest, V1, Others),
    vmul(V0, V1, Result).

:- pred vmul(list(int), list(int), int).
:- mode vmul(in, in, out).

vmul([], [], 0).
vmul([H1 | T1], [H2 | T2], Result) :-
    vmul(T1, T2, Newresult),
    Product = H1 * H2,
    Result = Product + Newresult.
