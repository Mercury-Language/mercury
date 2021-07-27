%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% mmc -c --grade hlc.gc static.c
% static.c(455) : error C2065: 'static__const_Result_5' : undeclared identifier

:- module static.

:- interface.

:- type t.
:- type t4.
:- type t5.

:- pred q(t::in, t5::out) is det.
:- pred r(t::in, t5::out, int::out) is multi.
:- pred s(int::in, t5::out) is cc_nondet.
:- pred t(t::in, t4::out, t5::out) is semidet.
:- pred u(t4::out, t5::out, int::out) is nondet.
:- pred v(t4::out, t5::out, int::out) is nondet.

:- implementation.
:- import_module int.
:- import_module list.

:- type t
    --->    a
    ;       b
    ;       c.

:- type t4
    --->    f(string, int).

:- type t5
    --->    g(t4, t4)
    ;       i.

% Test for ordinary if-then-else
q(X, Y) :-
    ( if
        X = a,
        % This line causes the problem. Move it into the then part
        % to avoid the above code gen problem. We can move it into the
        % then part because this line isn't part of the test.
        Result = f("hello", 0)
    then
        Y = g(Result, Result)
    else
        Y = i
    ).

% Test for if-then-else with nondet condition
r(X, Y, Z) :-
    ( if
        X = a,
        (Z0 = 1 ; Z0 = 2),

        % This line causes the problem. Move it into the then part
        % to avoid the above code gen problem. We can move it into the
        % then part because this line isn't part of the test.
        Result = f("hello", 0)
    then
        Z = Z0,
        Y = g(Result, Result)
    else
        Z = 0,
        Y = i
    ).

% Test for commit
s(X, Y) :-
    some [Z] (
        (Z = 1 ; Z = 2),
        X = Z * Z,
        Result = f("hello", 0)
    ),
    Y = g(Result, Result).

% Test same variable having different constant values
% in different branches (semidet)
t(X, Result, Y) :-
    (
        X = a,
        Result = f("hello", 0),
        Y = g(Result, Result)
    ;
        X = b,
        Result = f("goodbye", 0),
        Y = g(Result, Result)
    ).

% Test same variable having different constant values
% in different branches (nondet)
u(Result, Y, Z) :-
    (
        Result = f("hello", 0),
        list__member(Z, [1, 2]),
        Y = g(Result, Result)
    ;
        Result = f("hello again", 0),
        Result2 = f("GoodBye", 0),
        list__member(Z, [3, 4]),
        Y = g(Result, Result2)
    ).

% Exactly the same as u/3, but with different constants;
% this tests to ensure that any constant values hoisted
% out to the top level are given distinct names.
v(Result, Y, Z) :-
    (
        Result = f("xxxxx", 0),
        list__member(Z, [1, 2]),
        Y = g(Result, Result)
    ;
        Result = f("yyyyyyyyyyy", 0),
        Result2 = f("zzzzzzz", 0),
        list__member(Z, [3, 4]),
        Y = g(Result, Result2)
    ).
