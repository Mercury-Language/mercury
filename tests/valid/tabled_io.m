:- module tabled_io.

:- interface.

:- import_module io.

:- pred p(io__state::di, io__state::uo) is det.
:- pred q(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module exception.

p(S0, _) :-
	nasty_fail_p(S0, S0).

q(S0, _) :-
	nasty_fail_q(S0, S0).

:- pred nasty_fail_p(io__state::in, io__state::in) is erroneous.
:- pred nasty_fail_q(io__state::in(any), io__state::in(any)) is erroneous.

nasty_fail_p(_, _) :-
	throw("ouch").

nasty_fail_q(_, _) :-
	throw("ouch").
