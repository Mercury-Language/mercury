% Test removal of trailing primitives around semidet if-then-else conditions.
% 
% The following test case checks that trail usage optimization really does
% optimize trailing primitives from if-then-elses with semidet conditions that
% do not modify the trail.  The test works by calling a predicate that adds a
% function entry to the trail in the condition of an if_then_else.  We lie 
% to the compiler about the trailing status of that predicate and pretend
% that it does not modify the trail.  If the trail usage optimization is
% working the function placed on the trail should never be called when
% we commit to a solution.

:- module tu_test2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.

%----------------------------------------------------------------------------%

main(!IO) :-
	test(10, X),
	io.format("X = %d\n", [i(X)], !IO).

:- pragma promise_pure(test/2).
:- pred test(int::in, int::out) is det.

test(X, Y) :-
	(
		impure store_stuff_on_trail,
		X = 3
	->	
		Y = -100
	;
		Y = 100
	).

	% `will_not_modify_trail' is a lie.
	%
:- impure pred store_stuff_on_trail is det.
:- pragma foreign_proc("C",
	store_stuff_on_trail,
	[will_not_call_mercury, will_not_modify_trail],
"
	MR_trail_function(print_entry, NULL);
").

:- pragma foreign_decl("C", "
	#include <stdio.h>
	#include <stdlib.h>
	extern void print_entry(void *, MR_untrail_reason);
").

:- pragma foreign_code("C", "
void
print_entry(void *value, MR_untrail_reason reason)
{
	printf(\"Trail function called.\\n\");
}").

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%
