% A regression test, adapted from a bug report by Ralph Becket.
% Mercury 0.8.1 and earlier reported a spurious mode error
% for this code.

:- module uniq_neg.
:- interface.
:- import_module list, store.

:- type term(S) == store_mutvar(term_type(S), S).
:- type var(S) == term(S).
:- type term_type(S) ---> free ; functor(string, int, list(term(S))).


:- pred unify(term(S), term_type(S), term(S), term_type(S), store(S), store(S)).
:- mode unify(in, in, in, in, di, uo) is semidet.

:- implementation.

:- pred occurs(var(S), list(term(S)), store(S), store(S)).
:- mode occurs(in, in, di, uo) is semidet.

:- external(occurs/4).

unify(T1, free, _T2, functor(Name2, Arity2, Args2)) -->
        \+ occurs(T1, Args2),
        store__set_mutvar(T1, functor(Name2, Arity2, Args2)).

