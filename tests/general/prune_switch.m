% Regression test for the switch pruning code in simplify.m.
:- module prune_switch.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- type t
	--->	f(int)
	;	g(int)
	;	h(int).

main -->
	{ create_switch_var(SwitchedOn) },
	(	(
			{ SwitchedOn = f(Int) }
		;
			{ SwitchedOn = h(Int) }
		)
	->
		io__write_int(Int)
	;
		io__write_string("Failed")
	).

:- pred create_switch_var(t::out) is det.

create_switch_var(Var) :-
	create_switch_var_2(Var).

:- pred create_switch_var_2(t :: (free->bound(f(ground);g(ground)))) is det.
:- pragma no_inline(create_switch_var_2/1).

create_switch_var_2(f(1)).
