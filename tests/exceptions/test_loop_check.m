%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 1997-1998 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: test_loop_check.m.
% Main author: fjh.

% Test cases that combine exception handling and `pragma loop_check'.

%---------------------------------------------------------------------------%

:- module test_loop_check.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.
:- import_module solutions.
:- import_module exception.

% repeat the test several times, so that we test both
% construction of the tables and use of the tables.
main(!IO) :-
    io.print_line("--- first time ---", !IO),
    test(!IO),
    io.print_line("--- second time ---", !IO),
    test(!IO),
    io.print_line("--- third time ---", !IO),
    test(!IO).

:- pred test(io::di, io::uo) is cc_multi.

test(!IO) :-
    try(det_throw, DetThrowResult),
    io.print("det_throw: ", !IO),
    io.print_line(DetThrowResult, !IO),
    try(det_succeed, DetSucceedResult),
    io.print("det_succeed: ", !IO),
    io.print_line(DetSucceedResult, !IO),

    try(semidet_throw, SemidetThrowResult),
    io.print("semidet_throw: ", !IO),
    io.print_line(SemidetThrowResult, !IO),

    try(semidet_succeed, SemidetSucceedResult),
    io.print("semidet_succeed: ", !IO),
    io.print_line(SemidetSucceedResult, !IO),

    try(semidet_fail, SemidetFailResult),
    io.print("semidet_fail: ", !IO),
    io.print_line(SemidetFailResult, !IO),

    try(cc_multi_throw, CCMultiThrowResult),
    io.print("cc_multi_throw: ", !IO),
    io.print_line(CCMultiThrowResult, !IO),
    try(cc_multi_succeed, CCMultiSucceedResult),
    io.print("cc_multi_succeed: ", !IO),
    io.print_line(CCMultiSucceedResult, !IO),

    try(cc_nondet_throw, CCNondetThrowResult),
    io.print("cc_nondet_throw: ", !IO),
    io.print_line(CCNondetThrowResult, !IO),

    try(cc_nondet_succeed, CCNondetSucceedResult),
    io.print("cc_nondet_succeed: ", !IO),
    io.print_line(CCNondetSucceedResult, !IO),

    try(cc_nondet_fail, CCNondetFailResult),
    io.print("cc_nondet_fail: ", !IO),
    io.print_line(CCNondetFailResult, !IO),

    try((pred(R::out) is det :- solutions(multi_throw, R)),
        MultiThrowResult),
    io.print("multi_throw: ", !IO),
    io.print_line(MultiThrowResult, !IO),
    try((pred(R::out) is det :- solutions(multi_succeed, R)),
        MultiSucceedResult),
    io.print("multi_succeed: ", !IO),
    io.print_line(MultiSucceedResult, !IO),
    try((pred(R::out) is det :- solutions(multi_succeed_then_throw, R)),
        MultiSucceedThenThrowResult),
    io.print("multi_succeed_then_throw: ", !IO),
    io.print_line(MultiSucceedThenThrowResult, !IO),

    try((pred(R::out) is det :- solutions(nondet_throw, R)),
        NondetThrowResult),
    io.print("nondet_throw: ", !IO),
    io.print_line(NondetThrowResult, !IO),
    try((pred(R::out) is det :- solutions(nondet_succeed, R)),
        NondetSucceedResult),
    io.print("nondet_succeed: ", !IO),
    io.print_line(NondetSucceedResult, !IO),
    try((pred(R::out) is det :- solutions(nondet_fail, R)),
        NondetFailResult),
    io.print("nondet_fail: ", !IO),
    io.print_line(NondetFailResult, !IO),
    try((pred(R::out) is det :-
            solutions(nondet_succeed_then_throw, R)),
        NondetSucceedThenThrowResult),
    io.print("nondet_succeed_then_throw: ", !IO),
    io.print_line(NondetSucceedThenThrowResult, !IO).

:- pred det_throw(string::out) is det.
:- pragma loop_check(det_throw/1).
det_throw(_) :- throw("det_throw").

:- pred semidet_throw(string::out) is semidet.
:- pragma loop_check(semidet_throw/1).
semidet_throw(_) :- throw("semidet_throw").

:- pred nondet_throw(string::out) is nondet.
nondet_throw(_) :- throw("nondet_throw").

:- pred multi_throw(string::out) is multi.
multi_throw(_) :- throw("multi_throw").

:- pred cc_nondet_throw(string::out) is cc_nondet.
:- pragma loop_check(cc_nondet_throw/1).
cc_nondet_throw(_) :- throw("cc_nondet_throw").

:- pred cc_multi_throw(string::out) is cc_multi.
:- pragma loop_check(cc_multi_throw/1).
cc_multi_throw(_) :- throw("cc_multi_throw").

:- pred det_succeed(string::out) is det.
:- pragma loop_check(det_succeed/1).
det_succeed("det_succeed").

:- pred semidet_succeed(string::out) is semidet.
:- pragma loop_check(semidet_succeed/1).
semidet_succeed("semidet_succeed").

:- pred nondet_succeed(string::out) is nondet.
nondet_succeed("nondet_succeed 1").
nondet_succeed("nondet_succeed 2").

:- pred multi_succeed(string::out) is multi.
multi_succeed("multi_succeed 1").
multi_succeed("multi_succeed 2").

:- pred cc_nondet_succeed(string::out) is cc_nondet.
:- pragma loop_check(cc_nondet_succeed/1).
cc_nondet_succeed("cc_nondet_succeed").
cc_nondet_succeed("cc_nondet_succeed 2").

:- pred cc_multi_succeed(string::out) is cc_multi.
:- pragma loop_check(cc_multi_succeed/1).
cc_multi_succeed("cc_multi_succeed").
cc_multi_succeed("cc_multi_succeed 2").

:- pred semidet_fail(string::out) is semidet.
:- pragma loop_check(semidet_fail/1).
semidet_fail("semidet_fail") :- fail.

:- pred nondet_fail(string::out) is nondet.
nondet_fail("nondet_fail 1") :- fail.
nondet_fail("nondet_fail 2") :- fail.

:- pred cc_nondet_fail(string::out) is cc_nondet.
:- pragma loop_check(cc_nondet_fail/1).
cc_nondet_fail("cc_nondet_fail 1") :- fail.
cc_nondet_fail("cc_nondet_fail 2") :- fail.

:- pred nondet_succeed_then_throw(string::out) is nondet.
nondet_succeed_then_throw("nondet_succeed_then_throw 1").
nondet_succeed_then_throw("nondet_succeed_then_throw 2").
nondet_succeed_then_throw(_) :- throw("nondet_succeed_then_throw 3").
nondet_succeed_then_throw("nondet_succeed_then_throw 4").

:- pred multi_succeed_then_throw(string::out) is multi.
multi_succeed_then_throw("multi_succeed_then_throw 1").
multi_succeed_then_throw("multi_succeed_then_throw 2").
multi_succeed_then_throw(_) :- throw("multi_succeed_then_throw 3").
multi_succeed_then_throw("multi_succeed_then_throw 4").
