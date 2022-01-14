%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module inst_perf_bug_2.
:- interface.

:- type t1 ---> f(t2, t2) ; g(t2, t2).
:- inst t1 ---> f(t2, t2) ; g(t2, t2).

:- type t2 ---> f(t3, t3) ; g(t3, t3).
:- inst t2 ---> f(t3, t3) ; g(t3, t3).

:- type t3 ---> f(t4, t4) ; g(t4, t4).
:- inst t3 ---> f(t4, t4) ; g(t4, t4).

:- type t4 ---> f(t5, t5) ; g(t5, t5).
:- inst t4 ---> f(t5, t5) ; g(t5, t5).

:- type t5 ---> f(t6, t6) ; g(t6, t6).
:- inst t5 ---> f(t6, t6) ; g(t6, t6).

:- type t6 ---> f(t7, t7) ; g(t7, t7).
:- inst t6 ---> f(t7, t7) ; g(t7, t7).

:- type t7 ---> f(t8, t8) ; g(t8, t8).
:- inst t7 ---> f(t8, t8) ; g(t8, t8).

:- type t8 ---> f(t9, t9) ; g(t9, t9).
:- inst t8 ---> f(t9, t9) ; g(t9, t9).

:- type t9 ---> f(t10, t10) ; g(t10, t10).
:- inst t9 ---> f(t10, t10) ; g(t10, t10).

:- type t10 ---> f(t11, t11) ; g(t11, t11).
:- inst t10 ---> f(t11, t11) ; g(t11, t11).

:- type t11 ---> f(t12, t12) ; g(t12, t12).
:- inst t11 ---> f(t12, t12) ; g(t12, t12).

% Commented out to avoid exceeding tools/test_mercury's 300 second limit
% on testing.mercurylang.org.
%
% :- type t12 ---> f(t13, t13) ; g(t13, t13).
% :- inst t12 ---> f(t13, t13) ; g(t13, t13).

:- type t12 ---> f ; g ; h.
:- inst t12 ---> f ; g.

:- pred p(t1::in(t1), t1::out(t1)) is det.

:- implementation.

p(f(X, Y), f(Y, X)).
p(g(X, Y), g(Y, X)).
