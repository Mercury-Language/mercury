%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This module is a regression test;
% it tests that we do correct tail recursion optimization
% for procedures with output arguments and determinism
% of `erroneous' or `failure', such as loop//1 below.
%
% This test is designed so that if the compiler doesn't do tail recursion
% optimization, then the program will overflow the limit on stack size.

:- module looptest.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module exception.
:- import_module int.
:- import_module string.

main(!IO) :-
    try(do_loop, R),
    io.print_line(R, !IO).

% No type declaration for do_loop.
:- mode do_loop(out) is det.

do_loop(X) :-
    loop(100000000, 42, X).

% No type or mode declaration for do_loop.

loop(N, !X) :-
    ( if N = 0 then
        throw("finished")
    else
        loop(N - 1, !X)
    ).
