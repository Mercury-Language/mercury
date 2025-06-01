%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a test of the compiler's ability to properly diagnose non-contiguous
% clauses for a predicate.
%
%---------------------------------------------------------------------------%

:- module allow_non_contiguity_for.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module pair.
:- import_module string.

main(!IO) :-
    test(1, !IO),
    test(1, !IO),
    test(3, !IO),
    test(3, !IO).

:- pred test(int::in, io::di, io::uo) is det.

test(N, !IO) :-
    Ps = ["p1" - p1, "p2" - p2, "p3" - p3],
    Qs = ["q1" - q1, "q2" - q2, "q3" - q3],
    Rs = ["r1" - r1, "r2" - r2, "r3" - r3],
    Ss = ["s1" - s1, "s2" - s2, "s3" - s3],
    list.foldl(test_pred(N), Ps ++ Qs ++ Rs ++ Ss, !IO).

:- pred test_pred(int, pair(string, pred(int, int)), io, io).
:- mode test_pred(in, in(pair(ground, (pred(in, out) is semidet))),
    di, uo) is det.

test_pred(N, PredName - Pred, !IO) :-
    ( if Pred(N, PM) then
        io.format("%s(%d) = %d\n", [s(PredName), i(N), i(PM)], !IO)
    else
        io.format("%s(%d) failed\n", [s(PredName), i(N)], !IO)
    ).

:- pred p1(int::in, int::out) is semidet.   % +100
:- pred p2(int::in, int::out) is semidet.   % +200
:- pred p3(int::in, int::out) is semidet.   % +300

:- pred q1(int::in, int::out) is semidet.   % +1000
:- pred q2(int::in, int::out) is semidet.   % +2000
:- pred q3(int::in, int::out) is semidet.   % +3000

:- pred r1(int::in, int::out) is semidet.   % +10000
:- pred r2(int::in, int::out) is semidet.   % +20000
:- pred r3(int::in, int::out) is semidet.   % +30000

:- pred s1(int::in, int::out) is semidet.   % +100000
:- pred s2(int::in, int::out) is semidet.   % +200000
:- pred s3(int::in, int::out) is semidet.   % +200000

p1(1, 101).
p2(1, 201).
p1(2, 102).
p2(2, 202).
p3(1, 301).
p3(2, 302).

q1(1, 1001).
q2(1, 2001).
q3(1, 3001).
q1(3, 1003).
s3(3, 200003).
q2(3, 2003).
q3(3, 3003).

r1(1, 10001).
r2(1, 20001).
s1(1, 100001).
r3(1, 30001).
r1(2, 10002).
r2(2, 20002).
s2(2, 200002).
r3(2, 30002).

