	%
	% Tests that if there is a call in the base case that we still
	% are able to introduce an accumulator.
	%
:- module call_in_base.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module list, int.

main -->
	io__write_string("l: "),
	{ l([1,10,100], Length) },
	io__write(Length),
	io__nl.

:- pred l(list(T)::in, int::out) is det.

l([], Init) :-
	init(Init).
l([_|T], L) :-
	l(T, L0),
	L is L0 + 1.

:- pred init(int::out) is det.
:- pragma no_inline(init/1).

init(0).
