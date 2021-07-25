%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module family.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    ( if half_siblings(b, c) then
        Msg = "Ok"
    else
        Msg = "Missing answer: half_siblings(b, c)"
    ),
    io.write_string(Msg, !IO),
    io.nl(!IO).

:- type person
    --->    a
    ;   b
    ;   c
    ;   m
    ;   f
    ;   s.

:- pred parent(person, person).
:- mode parent(out, out) is multi.
:- mode parent(in, out) is nondet.

parent(m, a).
parent(m, b).
parent(f, a).
parent(f, b).
parent(f, c).
parent(s, c).

:- pred male(person::in) is semidet.

male(f).
male(s).

:- pred female(person::in) is semidet.

female(X) :-
    \+ male(X).

:- pred common_mother(person::out, person::out) is nondet.

common_mother(A, B) :-
    parent(M, A),
    parent(M, B),
    not A = B,
    female(M).

:- pred common_father(person::out, person::out) is nondet.

common_father(A, B) :-
    parent(F, A),
    parent(A, B),       % Oops, that's a bug.
    not A = B,
    male(F).

:- pred siblings(person::out, person::out) is nondet.

siblings(A, B) :-
    common_mother(A, B).
siblings(A, B) :-
    common_father(A, B).

:- pred full_siblings(person::out, person::out) is nondet.

full_siblings(A, B) :-
    common_mother(A, B),
    common_father(A, B).

:- pred half_siblings(person::out, person::out) is nondet.

half_siblings(A, B) :-
    siblings(A, B),
    not full_siblings(A, B).
