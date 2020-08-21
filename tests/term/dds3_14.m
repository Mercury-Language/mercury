%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module dds3_14.

:- interface.

:- type list(T)
    --->    []
    ;       [T | list(T)].

:- pred sum(list(int)::in, list(int)::in, list(int)::out) is semidet.

:- implementation.

:- import_module int.
:- import_module prolog.

sum([], [], []).
sum([X1 | Y1], [X2 | Y2], [X3 | Y3]) :-
    X3 = X1+X2,
    sum(Y1, Y2, Y3).
