%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module complicated_constraint.

:- interface.

:- import_module io.
:- import_module list.

:- pred main(io::di, io::uo) is det.

:- typeclass printable(A) where [
    pred p(A::in, io.state::di, io.state::uo) is det
].
:- typeclass foo(A) <= printable(A) where [
    pred b(A::in) is semidet
].

:- instance printable(int).
:- instance foo(int).
:- instance printable(list(T)) <= foo(T).
:- instance foo(list(T)) <= foo(T).

:- implementation.
:- import_module int.

main(!IO) :-
    p(42, !IO),
    io.write_string("\n", !IO),
    p_list([1, 2, 3], !IO),
    io.write_string("\n", !IO),
    p([1, 2, 3], !IO),
    io.write_string("\n", !IO),
    blah(101, !IO),
    io.write_string("\n", !IO).

:- instance printable(int) where [
    pred(p/3) is io.write_int
].

:- instance foo(int) where [
    pred(b/1) is foo_b
].
:- instance foo(list(T)) <= foo(T) where [
    pred(b/1) is list_b
].
:- instance printable(list(T)) <= foo(T) where [
    pred(p/3) is p_list
].

:- pred p_list(list(T)::in, io::di, io::uo) is det <= printable(T).

p_list(Xs, !IO) :-
    list.foldl(p, Xs, !IO).

:- pred list_b(list(T)::in) is semidet <= foo(T).

list_b(List) :-
    list.map((pred(A::in, A::out) is semidet :- b(A)), List, _).

:- pred foo_b(int::in) is semidet.

foo_b(1).

% This tests complicated constraints of the form `foo(bar(T))'.

:- pred blah(T::in, io::di, io::uo) is det
    <= (foo(list(T)), printable(list(T))).

blah(X, !IO) :-
    ( if
        % This also tests the semidet class method call mechanism
        b([X, X])
    then
        io.write_string("true\n", !IO)
    else
        io.write_string("false\n", !IO)
    ),
    p([X], !IO).
