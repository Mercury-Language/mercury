%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 1997-1998 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: test_exceptions.m.
% Main author: fjh.

% Test cases for exception handling.

% XXX we should test nested exception handlers.
% XXX we should also test exceptions with nested calls to solutions/2.

%---------------------------------------------------------------------------%

:- module test_exceptions.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.
:- import_module exception.
:- import_module solutions.

main(!IO) :-
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

    try((pred(R::out) is det :- solutions(nondet_succeed_then_throw, R)),
        NondetSucceedThenThrowResult),
    io.print("nondet_succeed_then_throw: ", !IO),
    io.print_line(NondetSucceedThenThrowResult, !IO).

:- pred det_throw(string::out) is det.
det_throw(_) :- throw("det_throw").

:- pred semidet_throw(string::out) is semidet.
semidet_throw(_) :- throw("semidet_throw").

:- pred nondet_throw(string::out) is nondet.
nondet_throw(_) :- throw("nondet_throw").

:- pred multi_throw(string::out) is multi.
multi_throw(_) :- throw("multi_throw").

:- pred cc_nondet_throw(string::out) is cc_nondet.
cc_nondet_throw(_) :- throw("cc_nondet_throw").

:- pred cc_multi_throw(string::out) is cc_multi.
cc_multi_throw(_) :- throw("cc_multi_throw").

:- pred det_succeed(string::out) is det.
det_succeed("det_succeed").

:- pred semidet_succeed(string::out) is semidet.
semidet_succeed("semidet_succeed").

:- pred nondet_succeed(string::out) is nondet.
nondet_succeed("nondet_succeed 1").
nondet_succeed("nondet_succeed 2").

:- pred multi_succeed(string::out) is multi.
multi_succeed("multi_succeed 1").
multi_succeed("multi_succeed 2").

:- pred cc_nondet_succeed(string::out) is cc_nondet.
cc_nondet_succeed("cc_nondet_succeed").
cc_nondet_succeed("cc_nondet_succeed 2").

:- pred cc_multi_succeed(string::out) is cc_multi.
cc_multi_succeed("cc_multi_succeed").
cc_multi_succeed("cc_multi_succeed 2").

:- pred semidet_fail(string::out) is semidet.
semidet_fail("semidet_fail") :- fail.

:- pred nondet_fail(string::out) is nondet.
nondet_fail("nondet_fail 1") :- fail.
nondet_fail("nondet_fail 2") :- fail.

:- pred cc_nondet_fail(string::out) is cc_nondet.
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
