%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module complicated_unify.

:- interface.

:- type t
    --->    f(int).

:- pred p(t::in, t::in) is semidet.

:- implementation.

p(X, X).
