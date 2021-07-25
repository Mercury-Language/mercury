%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module output_term_dep.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list.

main -->
    % Test cases which track an output subterm.
    test1,      % basic det conjunction
    test2,      % construction unification
    test3,      % if-then-else
    test4,      % switch and disjunction
    test5.      % negation

%---------------------------------------------------------------------------%

:- pred test1(io::di, io::uo) is det.

test1(!IO) :-
    p(A, B, C),
    io.write_int(A, !IO),
    io.nl(!IO),
    io.write_int(B, !IO),
    io.nl(!IO),
    io.write_int(C, !IO),
    io.nl(!IO).

:- pred p(int::out, int::out, int::out) is det.

p(A, B, C) :-       % tracking subterm B
    pa(A),
    pb(B),
    pc(C).

:- pred pa(int::out) is det.

pa(5).

:- pred pb(int::out) is det.

pb(8).

:- pred pc(int::out) is det.

pc(13).

%---------------------------------------------------------------------------%

:- pred test2(io::di, io::uo) is det.

test2(!IO) :-
    q(X),
    io.write_line(X, !IO).

:- pred q(list(list(int))::out) is det.

q([A, B, C]) :-     % tracking subterm B
    qa(A),
    qb(B),
    qc(C).

:- pred qa(list(int)::out) is det.

qa([1, 2, 3]).

:- pred qb(list(int)::out) is det.

qb([]).

:- pred qc(list(int)::out) is det.

qc([99]).

%---------------------------------------------------------------------------%

:- pred test3(io::di, io::uo) is det.

test3(!IO) :-
    r(1, W),
    io.write_line(W, !IO),
    r(2, X),
    io.write_line(X, !IO),
    r(3, Y),
    io.write_line(Y, !IO),
    r(4, Z),
    io.write_line(Z, !IO).

:- pred r(int, int).
:- mode r(in, out) is det.

r(N, P) :-
    (
        N = 1
    ->
        P = 999
    ;
        ra(N)
    ->
        (
            rb(N)
        ->
            rc(P)
        ;
            P = 43
        )
    ;
        rd(P)
    ).

:- pred ra(int::in) is semidet.

ra(2).
ra(3).

:- pred rb(int::in) is semidet.

rb(3).

:- pred rc(int::out) is det.

rc(57).

:- pred rd(int::out) is det.

rd(-1).

%---------------------------------------------------------------------------%

:- pred test4(io::di, io::uo) is det.

test4(!IO) :-
    ( if
        s(1, _, X),
        sd(X)
    then
        io.write_string("yes\n", !IO)
    else
        io.write_string("no\n", !IO)
    ),
    ( if
        s(2, _, Y),
        sd(Y)
    then
        io.write_string("yes\n", !IO)
    else
        io.write_string("no\n", !IO)
    ).

:- pred s(int, int, int).
:- mode s(in, out, out) is nondet.

s(1, J, K) :-
    (
        sa(J)
    ;
        sb(J)
    ),
    (
        sa(K)
    ;
        sc(K)
    ).
s(2, J, K) :-
    (
        sa(J),
        sb(K)
    ;
        sb(J),
        sc(K)
    ).

:- pred sa(int::out) is det.

sa(7).

:- pred sb(int::out) is det.

sb(38).

:- pred sc(int::out) is det.

sc(155).

:- pred sd(int::in) is semidet.

sd(-3).

%---------------------------------------------------------------------------%

:- pred test5(io::di, io::uo) is det.

test5(!IO) :-
    ( if t(1, K) then
        io.write_int(K, !IO),
        io.nl(!IO)
    else
        io.write_string("no\n", !IO)
    ).

:- pred t(int::in, int::out) is semidet.

t(J, K) :-
    not ta(J),
    tb(K),
    not tc(K).

:- pred ta(int::in) is semidet.

ta(0).

:- pred tb(int::out) is det.

tb(77).

:- pred tc(int::in) is semidet.

tc(-654).

%---------------------------------------------------------------------------%

