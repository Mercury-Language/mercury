%---------------------------------------------------------------------------%
% Copyright (C) 2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: test_try_all.m.
% Main author: fjh.

:- module test_try_all.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is cc_multi.

:- implementation.
:- import_module std_util, exception, list, int.

main --> 
	{ try_all(det_throw, DetThrowResult) },
	print("det_throw: "), print(DetThrowResult), nl,
	{ try_all(det_succeed, DetSucceedResult) },
	print("det_succeed: "), print(DetSucceedResult), nl,

	{ try_all(semidet_throw, SemidetThrowResult) },
	print("semidet_throw: "), print(SemidetThrowResult), nl,
	{ try_all(semidet_succeed, SemidetSucceedResult) },
	print("semidet_succeed: "), print(SemidetSucceedResult), nl,
	{ try_all(semidet_fail, SemidetFailResult) },
	print("semidet_fail: "), print(SemidetFailResult), nl,

	{ try_all(multi_throw, MultiThrowResult) },
	print("multi_throw: "), print(MultiThrowResult), nl,
	{ try_all(multi_succeed, MultiSucceedResult) },
	print("multi_succeed: "), print(MultiSucceedResult), nl,
	{ try_all(multi_succeed_then_throw, MultiSucceedThenThrowResult) },
	print("multi_succeed_then_throw: "),
	print(MultiSucceedThenThrowResult), nl,

	{ try_all(nondet_throw, NondetThrowResult) },
	print("nondet_throw: "), print(NondetThrowResult), nl,
	{ try_all(nondet_succeed, NondetSucceedResult) },
	print("nondet_succeed: "), print(NondetSucceedResult), nl,
	{ try_all(nondet_fail, NondetFailResult) },
	print("nondet_fail: "), print(NondetFailResult), nl,
	{ try_all(nondet_succeed_then_throw, NondetSucceedThenThrowResult) },
	print("nondet_succeed_then_throw: "),
	print(NondetSucceedThenThrowResult), nl.

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

