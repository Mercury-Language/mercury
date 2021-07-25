%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 1997-1998 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: test_exceptions_func.m.
% Main author: fjh.
%
% Test cases for exception handling functions.
%
% XXX we should test nested exception handlers.
%
%---------------------------------------------------------------------------%

:- module test_exceptions_func.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.
:- import_module exception.

main(!IO) :-
    try(det_throw, R1),
    io.print("det_throw: ", !IO),
    print_r(R1, !IO), io.nl(!IO),
    try(det_succeed, R2),
    io.print("det_succeed: ", !IO),
    print_r(R2, !IO), io.nl(!IO),

    try(semidet_throw, SemidetThrowResult),
    io.print("semidet_throw: ", !IO),
    print_r(SemidetThrowResult, !IO), io.nl(!IO),
    try(semidet_succeed, SemidetSucceedResult),
    io.print("semidet_succeed: ", !IO),
    print_r(SemidetSucceedResult, !IO), io.nl(!IO),
    try(semidet_fail, SemidetFailResult),
    io.print("semidet_fail: ", !IO),
    print_r(SemidetFailResult, !IO), io.nl(!IO).

:- pred print_r(exception_result(T)::in, io::di, io::uo) is det.
print_r(E, !IO) :-
    print(E, !IO).

:- pred det_throw(string::out) is det.
det_throw(throw("det_throw")).

:- pred semidet_throw(string::out) is semidet.
semidet_throw(throw("semidet_throw")).

:- pred det_succeed(string::out) is det.
det_succeed("det_succeed").

:- pred semidet_succeed(string::out) is semidet.
semidet_succeed("semidet_succeed").

:- pred semidet_fail(string::out) is semidet.
semidet_fail("semidet_fail") :-
    fail.
