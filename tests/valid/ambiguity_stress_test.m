%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test case was contributed by Michael Day.
%
% With the current typechecker, this module generates roughly 35 million type
% assignments, which is far too many. To handle it, you need a constraint based
% type analysis algorithm.

:- module ambiguity_stress_test.

:- interface.

:- type a ---> foo ; bar.
:- type b ---> foo ; bar.
:- type c ---> foo ; bar.
:- type d ---> foo ; bar.
:- type e ---> foo ; bar.
:- type f ---> foo ; bar.
:- type g ---> foo ; bar.
:- type h ---> foo ; bar.
:- type i ---> foo ; bar.
:- type j ---> foo ; bar.
:- type k ---> foo ; bar.
:- type l ---> foo ; bar.
:- type m ---> foo ; bar.
:- type n ---> foo ; bar.
:- type o ---> foo ; bar.
:- type p ---> foo ; bar.
:- type q ---> foo ; bar.
:- type r ---> foo ; bar.
:- type s ---> foo ; bar.
:- type t ---> foo ; bar.
:- type u ---> foo ; bar.
:- type v ---> foo ; bar.
:- type w ---> foo ; bar.
:- type x ---> foo ; bar.
:- type y ---> foo ; bar.
:- type z ---> foo ; bar.
:- type a2 ---> foo ; bar.
:- type b2 ---> foo ; bar.
:- type c2 ---> foo ; bar.
:- type d2 ---> foo ; bar.
:- type e2 ---> foo ; bar.
:- type f2 ---> foo ; bar.
:- type g2 ---> foo ; bar.
:- type h2 ---> foo ; bar.
:- type i2 ---> foo ; bar.
:- type j2 ---> foo ; bar.
:- type k2 ---> foo ; bar.
:- type l2 ---> foo ; bar.
:- type m2 ---> foo ; bar.
:- type n2 ---> foo ; bar.
:- type o2 ---> foo ; bar.
:- type p2 ---> foo ; bar.
:- type q2 ---> foo ; bar.
:- type r2 ---> foo ; bar.
:- type s2 ---> foo ; bar.
:- type t2 ---> foo ; bar.
:- type u2 ---> foo ; bar.
:- type v2 ---> foo ; bar.
:- type w2 ---> foo ; bar.
:- type x2 ---> foo ; bar.
:- type y2 ---> foo ; bar.
:- type z2 ---> foo ; bar.
:- type a3 ---> foo ; bar.
:- type b3 ---> foo ; bar.
:- type c3 ---> foo ; bar.
:- type d3 ---> foo ; bar.
:- type e3 ---> foo ; bar.
:- type f3 ---> foo ; bar.
:- type g3 ---> foo ; bar.
:- type h3 ---> foo ; bar.
:- type i3 ---> foo ; bar.
:- type j3 ---> foo ; bar.
:- type k3 ---> foo ; bar.
:- type l3 ---> foo ; bar.
:- type m3 ---> foo ; bar.
:- type n3 ---> foo ; bar.
:- type o3 ---> foo ; bar.
:- type p3 ---> foo ; bar.
:- type q3 ---> foo ; bar.
:- type r3 ---> foo ; bar.
:- type s3 ---> foo ; bar.
:- type t3 ---> foo ; bar.
:- type u3 ---> foo ; bar.
:- type v3 ---> foo ; bar.
:- type w3 ---> foo ; bar.
:- type x3 ---> foo ; bar.
:- type y3 ---> foo ; bar.
:- type z3 ---> foo ; bar.

:- pred ambig(a::out, a::out, a::out, a::out) is det.

:- implementation.

ambig(A1, A2, A3, A4) :-
    X1 = foo,
    X2 = foo,
    X3 = foo,
    X4 = foo,
    constrain(X1, A1),
    constrain(X2, A2),
    constrain(X3, A3),
    constrain(X4, A4).

:- pred constrain(a::in, a::out) is det.

constrain(A, A).
