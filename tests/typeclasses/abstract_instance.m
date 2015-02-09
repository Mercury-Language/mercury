:- module abstract_instance.
:- interface.
:- import_module io, list.

:- typeclass runnable(T) where [
	pred run(T::in, io__state::di, io__state::uo) is det
].

:- instance runnable(int).
:- instance runnable(string).
:- instance runnable(list(T)) <= runnable(T).

:- implementation.

:- instance runnable(int) where [pred(run/3) is run_int].
:- instance runnable(string) where [pred(run/3) is run_string].
:- instance runnable(list(T)) <= runnable(T) where [pred(run/3) is run_list].

run_int(I) --> io__write_int(I), io__nl.
run_string(S) --> io__write_string(S), io__nl.
run_list([]) --> [].
run_list([X|Xs]) --> run(X), run(Xs).
