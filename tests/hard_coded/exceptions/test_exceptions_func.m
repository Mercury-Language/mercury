%---------------------------------------------------------------------------%
% Copyright (C) 1997-1998 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: test_exceptions_func.m.
% Main author: fjh.

% Test cases for exception handling functions.

% XXX we should test nested exception handlers.

%-----------------------------------------------------------------------------%

:- module test_exceptions_func.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is cc_multi.

:- implementation.
:- import_module exception.

main --> 
	{ try(det_throw, R1) },
	print("det_throw: "), print_r(R1), nl,
	{ try(det_succeed, R2) },
	print("det_succeed: "), print_r(R2), nl,

	{ try(semidet_throw, SemidetThrowResult) },
	print("semidet_throw: "), print_r(SemidetThrowResult), nl,
	{ try(semidet_succeed, SemidetSucceedResult) },
	print("semidet_succeed: "), print_r(SemidetSucceedResult), nl,
	{ try(semidet_fail, SemidetFailResult) },
	print("semidet_fail: "), print_r(SemidetFailResult), nl.

:- pred print_r(exception_result(T)::in, io__state::di, io__state::uo) is det.
print_r(E) --> print(E).

:- pred det_throw(string::out) is det.
det_throw(throw("det_throw")).

:- pred semidet_throw(string::out) is semidet.
semidet_throw(throw("semidet_throw")).

:- pred det_succeed(string::out) is det.
det_succeed("det_succeed").

:- pred semidet_succeed(string::out) is semidet.
semidet_succeed("semidet_succeed").


:- pred semidet_fail(string::out) is semidet.
semidet_fail("semidet_fail") :- fail.

