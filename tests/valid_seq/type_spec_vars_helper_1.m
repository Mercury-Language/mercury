%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module type_spec_vars_helper_1.

:- interface.

:- import_module list.

:- pred p(T::in, list(T)::in) is semidet.
:- pragma type_spec(p/2, T = list(U)).

:- implementation.

p(X, L) :-
    p2(X, L).

:- pred p2(T::in, list(T)::in) is semidet.
:- pragma type_spec(p2/2, T = list(U)).
:- pragma no_inline(p2/2).

p2(X, [Y | Ys]) :-
    (
        X = Y
    ;
        p2(X, Ys)
    ).
