%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module subtype_abstract_2.
:- interface.

:- type abstract_pair(T, U).

:- func make_abstract_pair(T, U) = abstract_pair(T, U).

%---------------------------------------------------------------------------%

:- implementation.

:- type bar(T, U)
    --->    red
    ;       green
    ;       blue
    ;       pair(U, T).

:- type bar_pair(T, U) =< bar(T, U)
    --->    pair(U, T).

:- type eqv_bar_pair(T, U) == bar_pair(T, U).

:- type abstract_pair(T, U) =< eqv_bar_pair(T, U)
    --->    pair(U, T).

make_abstract_pair(X, Y) = pair(Y, X).
