% The 22 March 2000 version of the compiler generated C code that contained
% strings that were not properly quoted in the RTTI structures of the type
% below, due to the backslashes in the function names.

:- module rtti_strings.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module bool.

:- type boolean_constraint
	--->	const(bool)
	;	boolean_constraint =:= boolean_constraint
	;	boolean_constraint /\ boolean_constraint
	;	boolean_constraint \/ boolean_constraint
	;	boolean_constraint `implies` boolean_constraint
	;	not(boolean_constraint).

main -->
	io__write(const(no) /\ const(yes)),
	io__write_string("\n").
