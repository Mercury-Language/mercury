% This is a regression test. The 1998 August 24 version of the compiler
% generated incorrect code for this module, the bug being that the value
% numbering pass did not properly flush stack slots that hold the old values
% of hijacked nondet control slots.
%
% This is a cut-down version of cut_test.m

:- module nondet_ctrl_vn.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module int.

main -->
	( { middle(100) } ->
		io__write_string("test succeeded: BUG.\n")
	;
		io__write_string("test failed: OK.\n")
	).

:- pragma no_inline(middle/1).
:- pred middle(int::in) is semidet.

middle(A0) :-
	(
		A = A0 + 10
	;
		A = A0 + 20
	),
	A > 200,
	(
		_B = A
	;
		_B = A * 2
	).
