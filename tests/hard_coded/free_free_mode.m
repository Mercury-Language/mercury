%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module free_free_mode.
:- interface.
:- import_module io.

:- inst f == free.

:- pred foo(int::(f>>f)) is det.
:- pred bar(int::(f>>f)) is det.

:- pred main(io::di, io::uo) is det.

:- implementation.

foo(_).

bar(X) :-
    foo(X).

main(!IO) :-
    bar(X),
    X = 42,
    bar(X),
    io.write_int(X, !IO),
    io.write_string("\n", !IO).
