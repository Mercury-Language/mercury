%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module multisoln_func.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module solutions.

main(!IO).

:- func f(int) = list(int).
:- mode f(in) = out is cc_multi.    % illegal

f(X) = L :-
    unsorted_solutions((pred(Y::out) is multi :- Y = X ; Y = X + 1), L).

:- func test = int.
:- mode test = out is cc_multi. % illegal

test = 123.
test = 456.

% test type inference

:- mode test2 = out is multi.   % illegal
test2 = 123.
test2 = 456.

:- mode test3(in) = out is nondet.  % illegal
test3(1) = 123.
test3(1) = 456.

:- mode test3b(in) = out is cc_nondet.  % illegal
test3b(1) = 123.
test3b(1) = 456.

% this one is legal
:- func test4(int::out) = (int::out) is multi.
test4(1) = 1.
test4(2) = 2.

% this one is legal
:- func test5(int::out) = (int::in) is cc_nondet.
test5(1) = 42.
test5(2) = 42.
