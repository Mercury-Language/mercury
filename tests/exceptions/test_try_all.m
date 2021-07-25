%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2000, 2004 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: test_try_all.m.
% Main author: fjh.

:- module test_try_all.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module exception.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module string.
:- import_module univ.

main(!IO) :-
    try_all(det_throw, DetThrowExcp, DetThrowSols),
    print_result("det_throw", DetThrowExcp, DetThrowSols, !IO),
    try_all(det_succeed, DetSucceedExcp, DetSucceedSols),
    print_result("det_succeed", DetSucceedExcp, DetSucceedSols, !IO),

    try_all(semidet_throw, SemidetThrowExcp, SemidetThrowSols),
    print_result("semidet_throw", SemidetThrowExcp, SemidetThrowSols, !IO),
    try_all(semidet_succeed, SemidetSucceedExcp, SemidetSucceedSols),
    print_result("semidet_succeed", SemidetSucceedExcp,
        SemidetSucceedSols, !IO),
    try_all(semidet_fail, SemidetFailExcp, SemidetFailSols),
    print_result("semidet_fail", SemidetFailExcp, SemidetFailSols, !IO),

    try_all(multi_throw, MultiThrowExcp, MultiThrowSols),
    print_result("multi_throw", MultiThrowExcp, MultiThrowSols, !IO),
    try_all(multi_succeed, MultiSucceedExcp, MultiSucceedSols),
    print_result("multi_succeed", MultiSucceedExcp, MultiSucceedSols, !IO),
    try_all(multi_succeed_then_throw, MultiSucceedThenThrowExcp,
        MultiSucceedThenThrowSols),
    print_result("multi_succeed_then_throw", MultiSucceedThenThrowExcp,
        MultiSucceedThenThrowSols, !IO),

    try_all(nondet_throw, NondetThrowExcp, NondetThrowSols),
    print_result("nondet_throw", NondetThrowExcp, NondetThrowSols, !IO),
    try_all(nondet_succeed, NondetSucceedExcp, NondetSucceedSols),
    print_result("nondet_succeed", NondetSucceedExcp, NondetSucceedSols, !IO),
    try_all(nondet_fail, NondetFailExcp, NondetFailSols),
    print_result("nondet_fail", NondetFailExcp, NondetFailSols, !IO),
    try_all(nondet_succeed_then_throw, NondetSucceedThenThrowExcp,
        NondetSucceedThenThrowSols),
    print_result("nondet_succeed_then_throw", NondetSucceedThenThrowExcp,
        NondetSucceedThenThrowSols, !IO).

:- pred print_result(string::in, maybe(univ)::in, list(string)::in,
    io::di, io::uo) is det.

print_result(Name, Excp, Sols, !IO) :-
    io.print(Name ++ ":\n\t", !IO),
    io.print(Excp, !IO),
    io.print("\n\t", !IO),
    io.print_line(Sols, !IO).

:- pred det_throw(string::out) is det.
det_throw(_) :- throw("det_throw").

:- pred semidet_throw(string::out) is semidet.
semidet_throw(_) :- throw("semidet_throw").

:- pred nondet_throw(string::out) is nondet.
nondet_throw(_) :- throw("nondet_throw").

:- pred multi_throw(string::out) is multi.
multi_throw(_) :- throw("multi_throw").

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

:- pred semidet_fail(string::out) is semidet.
semidet_fail("semidet_fail") :- fail.

:- pred nondet_fail(string::out) is nondet.
nondet_fail("nondet_fail 1") :- fail.
nondet_fail("nondet_fail 2") :- fail.

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
