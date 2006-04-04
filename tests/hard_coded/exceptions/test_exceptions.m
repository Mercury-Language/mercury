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

%-----------------------------------------------------------------------------%

:- module test_exceptions.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is cc_multi.

:- implementation.
:- import_module exception.
:- import_module solutions.

main --> 
	{ try(det_throw, DetThrowResult) },
	print("det_throw: "), print(DetThrowResult), nl,
	{ try(det_succeed, DetSucceedResult) },
	print("det_succeed: "), print(DetSucceedResult), nl,

	{ try(semidet_throw, SemidetThrowResult) },
	print("semidet_throw: "), print(SemidetThrowResult), nl,

	{ try(semidet_succeed, SemidetSucceedResult) },
	print("semidet_succeed: "), print(SemidetSucceedResult), nl,

	{ try(semidet_fail, SemidetFailResult) },
	print("semidet_fail: "), print(SemidetFailResult), nl,

	{ try(cc_multi_throw, CCMultiThrowResult) },
	print("cc_multi_throw: "), print(CCMultiThrowResult), nl,
	{ try(cc_multi_succeed, CCMultiSucceedResult) },
	print("cc_multi_succeed: "), print(CCMultiSucceedResult), nl,

	{ try(cc_nondet_throw, CCNondetThrowResult) },
	print("cc_nondet_throw: "), print(CCNondetThrowResult), nl,

	{ try(cc_nondet_succeed, CCNondetSucceedResult) },
	print("cc_nondet_succeed: "), print(CCNondetSucceedResult), nl,

	{ try(cc_nondet_fail, CCNondetFailResult) },
	print("cc_nondet_fail: "), print(CCNondetFailResult), nl,

	{ try((pred(R::out) is det :- solutions(multi_throw, R)),
		MultiThrowResult) },
	print("multi_throw: "), print(MultiThrowResult), nl,
	{ try((pred(R::out) is det :- solutions(multi_succeed, R)),
		MultiSucceedResult) },
	print("multi_succeed: "), print(MultiSucceedResult), nl,
	{ try((pred(R::out) is det :-
			solutions(multi_succeed_then_throw, R)),
		MultiSucceedThenThrowResult) },
	print("multi_succeed_then_throw: "),
	print(MultiSucceedThenThrowResult), nl,

	{ try((pred(R::out) is det :- solutions(nondet_throw, R)),
		NondetThrowResult) },
	print("nondet_throw: "), print(NondetThrowResult), nl,
	{ try((pred(R::out) is det :- solutions(nondet_succeed, R)),
		NondetSucceedResult) },
	print("nondet_succeed: "), print(NondetSucceedResult), nl,
	{ try((pred(R::out) is det :- solutions(nondet_fail, R)),
		NondetFailResult) },
	print("nondet_fail: "), print(NondetFailResult), nl,
	{ try((pred(R::out) is det :-
			solutions(nondet_succeed_then_throw, R)),
		NondetSucceedThenThrowResult) },
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

