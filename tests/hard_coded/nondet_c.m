% This test checks to see whether the compiler handles all three forms of
% nondet C code OK.

:- module nondet_c.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module std_util, list.

main -->
	{ solutions(split_pairs1("abcdef"), Pairs1) },
	print_pairs(Pairs1),
	{ solutions(split_pairs2("abcdef"), Pairs2) },
	print_pairs(Pairs2),
	{ solutions(split_pairs3("abcdef"), Pairs3) },
	print_pairs(Pairs3).

:- pred print_pairs(list(pair(string))::in, io__state::di, io__state::uo)
	is det.

print_pairs([]) --> [].
print_pairs([Left - Right | Pairs]) -->
	io__write_string(Left),
	io__write_string("/"),
	io__write_string(Right),
	io__write_string("\n"),
	print_pairs(Pairs).

:- pred split_pairs1(string::in, pair(string)::out) is multi.

split_pairs1(Whole, Left - Right) :-
	break_string1(Left, Right, Whole).

:- pred split_pairs2(string::in, pair(string)::out) is multi.

split_pairs2(Whole, Left - Right) :-
	break_string2(Left, Right, Whole).

:- pred split_pairs3(string::in, pair(string)::out) is multi.

split_pairs3(Whole, Left - Right) :-
	break_string3(Left, Right, Whole).

:- pred break_string1(string, string, string).
:- mode break_string1(out, out, in) is multi.

:- pragma c_header_code("
	#include <string.h>
	#include ""mercury_heap.h""
").

:- pragma c_code(break_string1(LeftHalf::out, RightHalf::out, WholeString::in),
will_not_call_mercury,
local_vars("
	/* here we declare any local variables that we need to save */
	MR_String s;
	size_t len;
	size_t count;
"),
first_code(/* This comment tests whether
the context of the following code is computed correctly
*/ "
	/* this code gets executed on a call, but not on a retry */
	LOCALS->s = WholeString;
	LOCALS->len = strlen(WholeString);
	LOCALS->count = 0;
"),
retry_code("
	/* this code gets executed on a retry */
	LOCALS->count++;
"),
shared_code("
	MR_Word	temp;

	/* this code gets executed for both calls and retries */
	MR_offset_incr_hp_atomic(temp, 0,
		(LOCALS->count + sizeof(MR_Word)) / sizeof(MR_Word));
	LeftHalf = (MR_String) temp;
	memcpy(LeftHalf, LOCALS->s, LOCALS->count);
	LeftHalf[LOCALS->count] = '\\0';
	MR_offset_incr_hp_atomic(temp, 0,
		(LOCALS->len - LOCALS->count + sizeof(MR_Word))
		/ sizeof(MR_Word));
	RightHalf = (MR_String) temp;
	strcpy(RightHalf, LOCALS->s + LOCALS->count);

	if (LOCALS->count < LOCALS->len) {
		SUCCEED;
	} else {
		SUCCEED_LAST;
	}
")).

:- pred break_string2(string, string, string).
:- mode break_string2(out, out, in) is multi.

:- pragma c_code(break_string2(LeftHalf::out, RightHalf::out, WholeString::in),
will_not_call_mercury,
local_vars("
	/* here we declare any local variables that we need to save */
	MR_String s;
	size_t len;
	size_t count;
"),
first_code("
	/* this code gets executed on a call, but not on a retry */
	LOCALS->s = WholeString;
	LOCALS->len = strlen(WholeString);
	LOCALS->count = 0;
"),
retry_code("
	/* this code gets executed on a retry */
	LOCALS->count++;
"),
duplicated_code("
	MR_Word	temp;

	/* this code gets executed for both calls and retries */
	MR_offset_incr_hp_atomic(temp, 0,
		(LOCALS->count + sizeof(MR_Word)) / sizeof(MR_Word));
	LeftHalf = (MR_String) temp;
	memcpy(LeftHalf, LOCALS->s, LOCALS->count);
	LeftHalf[LOCALS->count] = '\\0';
	MR_offset_incr_hp_atomic(temp, 0,
		(LOCALS->len - LOCALS->count + sizeof(MR_Word))
		/ sizeof(MR_Word));
	RightHalf = (MR_String) temp;
	strcpy(RightHalf, LOCALS->s + LOCALS->count);

	if (LOCALS->count < LOCALS->len) {
		SUCCEED;
	} else {
		SUCCEED_LAST;
	}
")).

:- pred break_string3(string, string, string).
:- mode break_string3(out, out, in) is multi.

:- pragma c_code(break_string3(LeftHalf::out, RightHalf::out, WholeString::in),
will_not_call_mercury,
local_vars("
	/* here we declare any local variables that we need to save */
	MR_String s;
	size_t    len;
	size_t    count;
"),
first_code("
	/* this code gets executed on a call, but not on a retry */
	LOCALS->s = WholeString;
	LOCALS->len = strlen(WholeString);
	LOCALS->count = 0;
"),
retry_code("
	/* this code gets executed on a retry */
	LOCALS->count++;
"),
common_code("
	MR_Word	temp;

	/* this code gets executed for both calls and retries */
	MR_offset_incr_hp_atomic(temp, 0,
		(LOCALS->count + sizeof(MR_Word)) / sizeof(MR_Word));
	LeftHalf = (MR_String) temp;
	memcpy(LeftHalf, LOCALS->s, LOCALS->count);
	LeftHalf[LOCALS->count] = '\\0';
	MR_offset_incr_hp_atomic(temp, 0,
		(LOCALS->len - LOCALS->count + sizeof(MR_Word))
		/ sizeof(MR_Word));
	RightHalf = (MR_String) temp;
	strcpy(RightHalf, LOCALS->s + LOCALS->count);

	if (LOCALS->count < LOCALS->len) {
		SUCCEED;
	} else {
		SUCCEED_LAST;
	}
")).
