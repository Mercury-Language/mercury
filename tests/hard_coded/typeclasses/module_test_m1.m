:- module module_test_m1.
:- interface.
:- import_module io.

:- typeclass runnable(T) where [
	pred run(T::in, io__state::di, io__state::uo) is det
].

:- type t1.
:- instance runnable(t1).
:- instance runnable(string).
:- func a_t1 = t1.

:- implementation.

:- type t1 ---> t1(int).

a_t1 = t1(1).

:- instance runnable(t1) where [pred(run/3) is run_t1].
:- instance runnable(string) where [pred(run/3) is run_string].

run_t1(t1(I)) --> io__write_string("t1: "), io__write_int(I), io__nl.
run_string(S) --> io__write_string("string: "), io__write_string(S), io__nl.
