% This test is an extract from the garbage_out module.

% The code matches the middle-recursion pattern. However, the code of
% the base case is somewhat complex, and some versions of the compiler
% generate code that has more than copy of some labels, if given the
% --no-follow-vars option.

:- module middle_rec_labels.

:- interface.

:- import_module list, maybe.

:- type liveinfo	--->	live_lvalue(
					lval,
					shape_num,
					maybe(list(lval))
				).

:- type lval		--->	stackvar(int)
			;	framevar(int)
			;	reg(int).

:- type shape_num	==	int.

:- type det		---> 	deterministic
			;	nondeterministic
			;	commit.

:- pred garbage_out_get_det(list(liveinfo), maybe(det), det).
:- mode garbage_out_get_det(in, in, out) is det.

:- implementation.

garbage_out_get_det([], no, nondeterministic).
garbage_out_get_det([], yes(commit), commit).
garbage_out_get_det([], yes(nondeterministic), nondeterministic).
garbage_out_get_det([], yes(deterministic), deterministic).

garbage_out_get_det([L | Ls], OldD, NewDet) :-
	(
		L = live_lvalue(stackvar(_), _, _)
	->
		(
			OldD = yes(Detism)
		->
			( 
				Detism = nondeterministic 
			->
				Det = yes(commit)
			;
				Det = OldD
			)	
		;
			Det = yes(deterministic)
		)
	;
		L = live_lvalue(framevar(_), _, _)
	->
		(
			OldD = yes(Detism)
		->
			(
				Detism = deterministic
			->
				Det = yes(commit)
			;
				Det = OldD
			)
		;
			Det = yes(nondeterministic)
		)
	;
		Det = OldD
	),
	garbage_out_get_det(Ls, Det, NewDet).
