%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module module_test_helper_1.
:- interface.
:- import_module io.

:- typeclass runnable(T) where [
    pred run(T::in, io::di, io::uo) is det
].

:- type t1.
:- instance runnable(t1).
:- instance runnable(string).
:- func a_t1 = t1.

:- implementation.

:- type t1
    --->    t1(int).

a_t1 = t1(1).

:- instance runnable(t1) where     [pred(run/3) is run_t1].
:- instance runnable(string) where [pred(run/3) is run_string].

run_t1(t1(I), !IO) :-
    io.write_string("t1: ", !IO),
    io.write_int(I, !IO),
    io.nl(!IO).

run_string(S, !IO) :-
    io.write_string("string: ", !IO),
    io.write_string(S, !IO),
    io.nl(!IO).
