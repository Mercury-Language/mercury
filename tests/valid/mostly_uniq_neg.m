% A regression test, adapted from a bug report by Ralph Becket.
% Mercury 0.8.1 and earlier reported a spurious mode error
% for this code.

:- module mostly_uniq_neg.
:- interface.
:- import_module list, store.

:- type term(S) == store_mutvar(term_type(S), S).
:- type var(S) == term(S).
:- type term_type(S) ---> free ; functor(string, int, list(term(S))).


:- pred unify(term(S), term_type(S), term(S), term_type(S), store(S), store(S)).
:- mode unify(in, in, in, in, mdi, muo) is semidet.

:- implementation.

:- pred occurs(var(S), list(term(S)), store(S), store(S)).
:- mode occurs(in, in, mdi, muo) is semidet.

:- external(occurs/4).

:- pred tr_store_set_mutvar(store_mutvar(T, S), T, store(S), store(S)).
:- mode tr_store_set_mutvar(in, in, mdi, muo) is det.

:- external(tr_store_set_mutvar/4).

unify(T1, free, _T2, functor(Name2, Arity2, Args2)) -->
        \+ occurs(T1, Args2),
        tr_store_set_mutvar(T1, functor(Name2, Arity2, Args2)).

