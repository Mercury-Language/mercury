%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
:- module implied_instance.

:- interface.

:- pred main(io::di, io::uo) is det.

:- import_module io.

:- implementation.

:- import_module list.

:- typeclass printable(A) where [
    pred p(A::in, io::di, io::uo) is det
].

:- instance printable(int) where [
    pred(p/3) is io.write_int
].

:- instance printable(list(T)) <= printable(T) where [
    pred(p/3) is my_write_list
].

main(!IO) :-
    p(2, !IO),
    io.nl(!IO),
    p([42, 24, 1, 2, 3], !IO),
    io.nl(!IO).

:- pred my_write_list(list(T)::in, io::di, io::uo) is det <= printable(T).

my_write_list([], !IO) :-
    io.write_string("[]", !IO).
my_write_list([X | Xs], !IO) :-
    io.write_string("[\n", !IO),
    my_write_list_2([X | Xs], !IO),
    io.write_string("]", !IO).

:- pred my_write_list_2(list(T)::in, io::di, io::uo) is det <= printable(T).

my_write_list_2([], !IO).
my_write_list_2([X | Xs], !IO) :-
    p(X, !IO),
    io.nl(!IO),
    my_write_list_2(Xs, !IO).
