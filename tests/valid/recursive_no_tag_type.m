:- module recursive_no_tag_type.
:- interface.
:- import_module pair, unit.

:- type t ---> f(t).

:- type inftype(B) ---> a(pair(B, inftype(B))).

:- type either(A, B) ---> left(A) ; right(B).
:- type alist(A) ---> b(either(unit, pair(A,alist(A)))).

:- pred p(t, t).
:- mode p(in, out) is multi.

:- func infy = inftype(int).
:- func onetwothree = alist(int).

:- implementation.

p(f(X), X).
p(X, f(X)).

infy = a(1 - infy).
onetwothree = b(right(1 - b(right(2 - b(right(3 - b(left(unit)))))))).

