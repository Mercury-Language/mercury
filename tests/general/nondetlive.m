%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
:- module nondetlive.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module solutions.

main(!IO) :-
    solutions(p, List),
    write_list(List, !IO),
    io.nl(!IO).

:- pred write_list(list(int)::in, io::di, io::uo) is det.

write_list([], !IO).
write_list([I | Is], !IO) :-
    io.write_int(I, !IO),
    io.write_string(" ", !IO),
    write_list(Is, !IO).

:- pred p(int::out) is multi.

p(X) :-
    q(W),
    some [Y] (
        (
            Y = 1
        ;
            Y = 2
        ),
        Z = Y + W
    ),
    q(V),
    X = Z * V.

:- pred q(int::out) is multi.

q(1).
q(2).
