%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module filter.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.

main(!IO) :-
    ( if
        p(X),
        q(X)
    then
        io.write_string("yes\n", !IO)
    else
        io.write_string("no\n", !IO)
    ).

:- pred p(list(int)::out) is multi.

p(X) :-
    s1(A),
    s2(B),
    my_append(A, B, X).

:- pred s1(list(int)::out) is multi.

s1([1, 2]).
s1([1, 2, 3]).

:- pred s2(list(int)::out) is multi.

s2([9]).
s2([7, 8, 9]).

:- pred q(list(T)::in) is semidet.

q(X) :-
    my_length(X, L),
    L > 6.

:- pred my_append(list(T)::in, list(T)::in, list(T)::out) is det.

my_append(A, B, C) :-
    list.append(A, B, C).

:- pred my_length(list(T)::in, int::out) is det.

my_length(A, L) :-
    list.length(A, L).

