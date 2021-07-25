%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module app.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list.

:- type foo(T)
    --->    append(T, T, T).

main(!IO) :-
    A = [1, 2, 3, 4, 5],
    B = [6, 7, 8],
    app(A, B, C),
    io.write(app.append(A, B, C), !IO),
    io.write_string(".\n", !IO),
    D = [1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5],
    E = [6, 7, 8],
    app(D, E, F),
    io.write(app.append(D, E, F), !IO),
    io.write_string(".\n", !IO).

:- pred app(list(T)::in, list(T)::in, list(T)::out) is det.

app([], Bs, Bs).
app([A | As], Bs, [A | Cs]) :-
    app(As, Bs, Cs).
