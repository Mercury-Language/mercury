%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module input_term_dep.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list.

main(!IO) :-
    % Test cases which track an input subterm.
    test1(!IO),      % basic det conjunction
    test2(!IO),      % construction unification
    test3(!IO),      % if-then-else
    test4(!IO).      % negation

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

p(A, B, C) :-
    pa(A),
    pb(B),
    pc(A, C).

:- pred pa(int::out) is det.

pa(5).

:- pred pb(int::out) is det.

pb(8).

:- pred pc(int::in, int::out) is det.

pc(_, 13).

%---------------------------------------------------------------------------%

:- pred test2(io::di, io::uo) is det.

test2(!IO) :-
    ( if q(X) then
        io.write_line(X, !IO)
    else
        io.write_string("no\n", !IO)
    ).

:- pred q(list(list(int))::out) is semidet.

q(L) :-
    qa([C, A]),
    qb(B),
    qc([A, B, C], L).

:- pred qa(list(list(int))::out) is det.

qa([[1], [2, 3]]).

:- pred qb(list(int)::out) is det.

qb([]).

:- pred qc(list(list(int))::in, list(list(int))::out) is det.

qc(L, L).

%---------------------------------------------------------------------------%

:- pred test3(io::di, io::uo) is det.

test3(!IO) :-
    r(1, Z),
    io.write_line(Z, !IO).

:- pred r(int::in, int::out) is det.

r(N, P) :-
    ( if ra(N, A) then
        ( if rb(A) then
            rc(A, P)
        else
            P = 1
        )
    else
        P = 99
    ).

:- pred ra(int::in, int::out) is semidet.

ra(1, 3).

:- pred rb(int::in) is semidet.

rb(3).

:- pred rc(int::in, int::out) is det.

rc(_, 33).

%---------------------------------------------------------------------------%

:- pred test4(io::di, io::uo) is det.

test4(!IO) :-
    ( if s(1) then
        io.write_string("yes\n", !IO)
    else
        io.write_string("no\n", !IO)
    ).

:- pred s(int::in) is semidet.

s(N) :-
    not (
        sa(N, A),
        sb(A),
        not sc(A)
    ).

:- pred sa(int::in, int::out) is semidet.

sa(1, 7).

:- pred sb(int::in) is semidet.

sb(7).

:- pred sc(int::in) is semidet.

sc(7).

%---------------------------------------------------------------------------%
