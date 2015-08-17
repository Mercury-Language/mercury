% vim: ts=4 sw=4 et ft=mercury
%
% This is a regression test. When we invoked the compiler on this file
% with the command line
%
%   mmc -O0 --deforestation -C bug392.m
%
% we used to get this error:
%
%   Uncaught Mercury exception:
%   Software Error: ll_backend.code_gen:
%   predicate `ll_backend.code_gen.generate_goal'/5:
%   Unexpected: nondet model in det/semidet context
%
% The problem was in quux. Pushing the switch on R into the initial disjunction
% gave it an output variable (Q), turning it from a semidet disjunction into
% a nondet disjunction.

:- module bug392.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool.
:- import_module maybe.

main(!IO) :-
    foo(yes, Xyes),
    io.write_int(Xyes, !IO),
    io.nl(!IO),
    foo(no, Xno),
    io.write_int(Xno, !IO),
    io.nl(!IO).

:- pred foo(bool, int).
:- mode foo(in, out) is det.

foo(B, X) :-
    bar(X0),
    ( if quux(B, no, X1) then
	    X = X1
    else
	    X = X0
    ).

:- pred bar(int).
:- mode bar(out) is det.

bar(42).

:- pred quux(bool, maybe(int), int).
:- mode quux(in, in, out) is semidet.

quux(B, R, Q) :-
    (
	    B = yes
    ;
	    R = yes(_)
    ),
    (
	    R = yes(Q)
    ;
	    R = no,
	    Q = 55
    ).
