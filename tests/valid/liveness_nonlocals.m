% This is a regression test. Some versions of the compiler abort on this code.

% The bug that this test case tests for is described by this comment from
% liveness.m:

% If a variable is not live on entry to a goal, but the goal gives it a value,
% the code of this module assumes that
%
% (a) any parallel goals also give it a value, or
% (b) the variable is local to this goal and hence does not occur in parallel
%     goals.
%
% If a variable occurs in the nonlocal set of the goal, the code of this
% assumes that (b) is not true, and will therefore require (a) to be true.
% If some of the parallel goals cannot succeed, the first pass will include
% the variable in their post-birth sets.
%
% If a variable occurs in the nonlocal set of the goal, but is actually
% local to the goal, then any occurrence of that variable in the postbirth
% sets of parallel goals will lead to an inconsistency, because the variable
% will not die on those parallel paths, but will die on the path that
% actually gives a value to the variable.

:- module liveness_nonlocals.

:- interface.

:- import_module bool.

:- pred foo(T, bool).
:- mode foo(in, in(bound(yes))) is failure.

:- implementation.

:- import_module list, require, string.

foo(Foo, IsFoo) :-
	foo_int(Foo, Int),
	int_to_bool(Int, IsFoo).

:- pred foo_int(T, int).
:- mode foo_int(in, out(bound(0))) is det.

:- pragma c_code(foo_int(_V2::in, Res::out(bound(0))),
 	will_not_call_mercury, "Res = 0").
foo_int(_, 0).

:- pred int_to_bool(int, bool).
:- mode int_to_bool(in(bound(1)), out(bound(yes))) is det.
:- mode int_to_bool(in(bound(0)), out(bound(no))) is det.

int_to_bool(1, yes).
int_to_bool(0, no).
