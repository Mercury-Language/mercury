% Test case for elimination of special predicates in base_type_infos.
%
% We can use arg/3 and det_arg/3 to retrieve arguments of types, and 
% unify them. Analysis procedures may incorrectly conclude that
% we cannot call the unification procedure (or other procedures), and
% so eliminate it.
%
% The Mercury compiler of February 13th, 1997 failed this test - the
% test ended with a runtime error indicating that an unused predicate 
% had been called.
% 
% Author: trd

:- module elim_special_pred.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module list, int, std_util, term, map, string.

:- type enum	--->	one	;	two	;	three.

:- type fruit	--->	banana(enum).

main -->
	{ X = banana(three) },
	{ Y = banana(two) },
	{ det_arg(1, X, XArg) },
	{ det_arg(1, Y, YArg) },
	( 
		{ XArg = YArg }
	->
		io__write_string("same\n")
	;
		io__write_string("different\n")
	).
	
