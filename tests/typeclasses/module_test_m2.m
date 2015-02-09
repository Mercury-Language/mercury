:- module module_test_m2.
:- interface.
:- import_module module_test_m1.

:- type t2.
:- func a_t2 = t2.
:- instance runnable(t2).
:- instance runnable(float).

:- implementation.
:- import_module io.

:- type t2 ---> t2(int).

:- instance runnable(t2) where [pred(run/3) is run_t2].
:- instance runnable(float) where [pred(run/3) is run_float].

a_t2 = t2(2).
run_t2(t2(I)) --> io__write_string("t2: "), io__write_int(I), io__nl.
run_float(S) --> io__write_string("float: "), io__write_float(S), io__nl.
