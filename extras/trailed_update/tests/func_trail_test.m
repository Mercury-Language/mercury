:- module func_trail_test.
:- interface.
:- import_module io.

:- impure pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module int.

main -->
	( { impure trail_test } ->
		io__write_string("Success\n")
	;
		io__write_string("Failure\n")
	).


:- impure pred trail_test is failure.

trail_test :-
	small_int(I),
	impure trail_test_message("before", I, 0),
	impure enter(I),
	small_int(J),
	impure trail_test_message("inside", I, J),
	impure leave(I),
	impure trail_test_message("after", I, J),
	fail.
	

:- pred small_int(int::out) is multi.

small_int(1).
small_int(2).
small_int(3).

:- impure pred trail_test_message(string::in, int::in, int::in) is det.

:- impure pred enter(int::in) is det.
:- impure pred leave(int::in) is det.

:- pragma c_header_code("
#include <stdio.h>
").

:- pragma c_code(trail_test_message(Prefix::in, I::in, J::in),
	will_not_call_mercury, "
	    printf(""%s: %d %d\n"",
		   (char *)Prefix, (int)I, (int)J);
").


:- pragma c_header_code("
#include <mercury_trail.h>


void enter_failing(int handle, MR_untrail_reason reason);
void leave_failing(int handle, MR_untrail_reason reason);


#include <stdio.h>


void enter_failing(int handle, MR_untrail_reason reason) {
	switch (reason) {
	    case MR_exception:
	    case MR_undo:
	    case MR_retry:
/*		printf(""enter_failing: exception/undo/retry\n""); */
	        printf(""=> fail back inside: %d\n"", handle);
		break;
	    default:
		printf(""enter_failing: default\n"");
		break;
	}
}


void leave_failing(int handle, MR_untrail_reason reason) {
	switch (reason) {
	    case MR_exception:
	    case MR_undo:
	    case MR_retry:
/*		printf(""leave_failing: exception/undo/retry\n""); */
	        printf(""<= fail back outside: %d\n"", handle);
		break;
	    case MR_commit:
	    case MR_solve:
		printf(""leave_failing: commit/solve\n"");
		break;
	    default:
		printf(""leave_failing: default\n"");
		/* we may need to do something if reason == MR_gc */
		break;
	}
}

").

:- pragma c_code(enter(I::in), will_not_call_mercury, "
	printf("">> enter (%d)\n"", (int) I);
	MR_trail_function(leave_failing, (void *) I);
").


:- pragma c_code(leave(I::in), will_not_call_mercury, "
	printf(""<< leave (%d)\n"", (int) I);
	MR_trail_function(enter_failing, (void *) I);
").
