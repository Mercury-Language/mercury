
:- module inline_nondet_pragma_c.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module pair, solutions.

main -->
	{ solutions(foo, FooList) },
	print(FooList), nl,
	{ solutions(bar, List) },
	print(List), nl.

:- pred bar(pair(int, pair(int, int))::out) is multi.
bar(X - (Y - Z)) :-
	( X = 1 ; X = 2),
	foo(Y),
	( Z = 1 ; Z = 2).
	
%
% This example implements the equivalent of
%     foo(X) :- X = 20 ; X = 10 ; X = 42 ; X = 99 ; fail.
%
:- pred foo(int).
:- mode foo(out) is multi.
:- pragma inline(foo/1).
:- pragma c_code(foo(X::out), [will_not_call_mercury, thread_safe],
     local_vars("
	     int state;
     "),
     first_code("
	     LOCALS->state = 1;
     "),
     retry_code("
	     LOCALS->state++;
     "),
     common_code("
	     switch (LOCALS->state) {
		     case 1: X = 20; SUCCEED; break;
		     case 2: X = 10; SUCCEED; break;
		     case 3: X = 42; SUCCEED; break;
		     case 4: X = 99; SUCCEED; break;
		     case 5: FAIL; break;
	     }
     ")
).
