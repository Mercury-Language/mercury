% Regression test: rotd-1999-10-27 did not handle optimization of
% a singleton switch on an existentially typed constructor.
% Symptom:
% 	Uncaught exception:
%	Software Error: instmap_delta_from_mode_list_2
%------------------------------------------------------------------------------%

:- module existential_type_switch_opt.

:- interface.

:- import_module io.

:- pred main(state::di, state::uo) is det.

:- type maybe
	--->	e_no
	;	some [U] e_maybe(U)
	;	some [T] e_yes(T)
	.

:- pred p(maybe).
:- mode p(in) is semidet.
%------------------------------------------------------------------------------%

:- implementation.

:- import_module std_util.

main -->
	{ semidet_fail ->
		X = 'new e_maybe'(2)
	;
		X = 'new e_yes'(1)
	},
	(
		{ p(X) }
	->
		io__write_string("succeeded\n")
	;
		io__write_string("failed\n")
	).

:- pragma inline(p/1).
p(e_no).
p(e_yes(_)).

