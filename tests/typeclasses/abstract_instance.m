%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module abstract_instance.
:- interface.
:- import_module io.
:- import_module list.

:- typeclass runnable(T) where [
    pred run(T::in, io::di, io::uo) is det
].

:- instance runnable(int).
:- instance runnable(string).
:- instance runnable(list(T)) <= runnable(T).

:- implementation.

:- instance runnable(int) where [pred(run/3) is run_int].
:- instance runnable(string) where [pred(run/3) is run_string].
:- instance runnable(list(T)) <= runnable(T) where [pred(run/3) is run_list].

run_int(I, !IO) :-
    io.write_int(I, !IO),
    io.nl(!IO).

run_string(S, !IO) :-
    io.write_string(S, !IO),
    io.nl(!IO).

run_list([], !IO).
run_list([X | Xs], !IO) :-
    run(X, !IO),
    run(Xs, !IO).
