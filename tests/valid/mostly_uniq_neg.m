%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A regression test, adapted from a bug report by Ralph Becket.
% Mercury 0.8.1 and earlier reported a spurious mode error for this code.

:- module mostly_uniq_neg.
:- interface.

:- import_module list.
:- import_module store.

:- type term(S) == store_mutvar(term_type(S), S).
:- type var(S) == term(S).
:- type term_type(S)
    --->    free
    ;       functor(string, int, list(term(S))).

:- pred unify(term(S)::in, term_type(S)::in, term(S)::in, term_type(S)::in,
    store(S)::mdi, store(S)::muo) is semidet.

:- implementation.

:- pred occurs(var(S)::in, list(term(S))::in, store(S)::mdi, store(S)::muo)
    is semidet.
:- pragma no_inline(occurs/4).

occurs(_, _, !S) :-
    semidet_true.

:- pred tr_store_set_mutvar(store_mutvar(T, S)::in, T::in,
    store(S)::mdi, store(S)::muo) is det.
:- pragma no_inline(tr_store_set_mutvar/4).

tr_store_set_mutvar(_, _, S, S).

unify(T1, free, _T2, functor(Name2, Arity2, Args2), !Store) :-
    not occurs(T1, Args2, !Store),
    tr_store_set_mutvar(T1, functor(Name2, Arity2, Args2), !Store).
