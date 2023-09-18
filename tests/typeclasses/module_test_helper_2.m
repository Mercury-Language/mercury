%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module module_test_helper_2.
:- interface.
:- import_module module_test_helper_1.

:- type t2.
:- func a_t2 = t2.
:- instance runnable(t2).
:- instance runnable(float).

:- implementation.
:- import_module io.

:- type t2
    --->    t2(int).

:- instance runnable(t2) where    [pred(run/3) is run_t2].
:- instance runnable(float) where [pred(run/3) is run_float].

a_t2 = t2(2).

run_t2(t2(I), !IO) :-
    io.write_string("t2: ", !IO),
    io.write_int(I, !IO),
    io.nl(!IO).

run_float(S, !IO) :-
    io.write_string("float: ", !IO),
    io.write_float(S, !IO),
    io.nl(!IO).
