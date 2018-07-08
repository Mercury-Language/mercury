%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This module tests minimal_model evaluation of semidet predicates.
% The question is: "should r(1, Soln1) succeed with Soln1 = 2, or
% should it throw an exception?

:- module semidet_minimal.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module exception.
:- import_module int.
:- import_module list.
:- import_module std_util.

:- pragma require_feature_set([memo]).

main(!IO) :-
    ( if r(1, RSoln1) then
        io.write_string("r(1) = ", !IO),
        io.write_int(RSoln1, !IO),
        io.write_string("\n", !IO)
    else
        io.write_string("r(1) failed\n", !IO)
    ),
    ( if r(2, RSoln2) then
        io.write_string("r(2) = ", !IO),
        io.write_int(RSoln2, !IO),
        io.write_string("\n", !IO)
    else
        io.write_string("r(2) failed\n", !IO)
    ),
    ( if s(1, SSoln1) then
        io.write_string("s(1) = ", !IO),
        io.write_int(SSoln1, !IO),
        io.write_string("\n", !IO)
    else
        io.write_string("s(1) failed\n", !IO)
    ),
    ( if s(2, SSoln2) then
        io.write_string("s(2) = ", !IO),
        io.write_int(SSoln2, !IO),
        io.write_string("\n", !IO)
    else
        io.write_string("s(2) failed\n", !IO)
    ),
    ( if t(1, TSoln1) then
        io.write_string("t(1) = ", !IO),
        io.write_int(TSoln1, !IO),
        io.write_string("\n", !IO)
    else
        io.write_string("t(1) failed\n", !IO)
    ),
    ( if t(2, TSoln2) then
        io.write_string("t(2) = ", !IO),
        io.write_int(TSoln2, !IO),
        io.write_string("\n", !IO)
    else
        io.write_string("t(2) failed\n", !IO)
    ),
    ( if v(1, VSoln1) then
        io.write_string("v(1) = ", !IO),
        io.write_int(VSoln1, !IO),
        io.write_string("\n", !IO)
    else
        io.write_string("v(1) failed\n", !IO)
    ),
    ( if v(2, VSoln2) then
        io.write_string("v(2) = ", !IO),
        io.write_int(VSoln2, !IO),
        io.write_string("\n", !IO)
    else
        io.write_string("v(2) failed\n", !IO)
    ),
    solutions(w(1), WSolns1),
    io.write_string("w(1) = ", !IO),
    io.write(WSolns1, !IO),
    io.nl(!IO),
    solutions(w(2), WSolns2),
    io.write_string("w(2) = ", !IO),
    io.write(WSolns2, !IO),
    io.nl(!IO).

:- pred r(int::in, int::out) is semidet.
:- pragma minimal_model(r/2).

r(A, B) :-
    (
        r(A, _),
        throw("recursive call successful")
    else
        ( A = 1 then
            B = 2
        else
            fail
        )
    ).

:- pred s(int::in, int::out) is semidet.
:- pragma minimal_model(s/2).

s(A, B) :-
    ( if A = 1 then
        fail
    else
        ( if
            some [P1, P2] (
                p(P1),
                p(P2),
                P1 \= P2,
                P1 \= A
            )
        then
            r(A, B)
        else
            fail
        )
    ).

:- pred t(int::in, int::out) is semidet.
:- pragma minimal_model(t/2).

t(A, B) :-
    u(A, B).

:- pred u(int::in, int::out) is semidet.
:- pragma minimal_model(u/2).

u(A, B) :-
    ( if A = 0 then
        B = 0
    else if A = 1 then
        t(A, B)
    else
        t(A - 1, B)
    ).

:- pred v(int::in, int::out) is semidet.
:- pragma minimal_model(v/2).

v(A, B) :-
    ( if some [C] w(A, C) then
        B = 2
    else
        B = 3
    ).

:- pred w(int::in, int::out) is nondet.
:- pragma minimal_model(w/2).

w(A, B) :-
    ( if some [C] v(A, C) then
        p(B)
    else
        fail
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The rest of the code is from the coup.m test case.

:- pragma minimal_model(p/1).
:- pred p(int).
:- mode p(out) is nondet.

p(X) :-
    q(X).
p(X) :-
    X = 1.

:- pragma minimal_model(q/1).
:- pred q(int).
:- mode q(out) is nondet.

q(3) :- q(_).
q(4) :- p(_).
