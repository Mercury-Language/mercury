%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module unify_typeinfo_bug.

:- interface.

:- import_module list.
:- import_module map.

:- type t
    --->    t(map(int, string), list(int)).

:- pred unify_t(t::in, t::in) is semidet.

:- implementation.

:- import_module univ.

unify_t(t(A1, B1), t(A2, B2)) :-
    unify_map(A1, A2),
    B1 = B2.

:- pred unify_map(map(int, string)::in, map(int, string)::in) is semidet.

unify_map(A, B) :-
    univ(A) = univ(B).
