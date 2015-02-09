	%
	% Tests that we recognise that even though the append/3 and +/3
	% are assocative, the call to drop/3 will drop different
	% amounts from H according to whether we start counting from the
	% start or end, so don't introduce accumulator.
	%
:- module nonrec.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module int, list.

main -->
	io__write_string("p(in, out, out): "),
	(
		{ p([[4,3,2,1],[3,2,1],[2,1]], Length, ListA) }
	->
		io__write(Length),
		io__write_string(" "),
		io__write(ListA)
	;
		io__write_string("failed")
	),
	io__nl,
	io__write_string("p(in, in, out): "),
	(
		{ p([[4,3,2,1],[3,2,1],[2,1]], 2, ListB) }
	->
		io__write(ListB)
	;
		io__write_string("failed")
	),
	io__nl.

:- pred p(list(list(T)), int, list(list(T))) is semidet.
:- mode p(in, out, out) is semidet.
:- mode p(in, in, out) is semidet.

p([],0,[]).
p([H|T], Length, DroppedList) :-
	p(T, Length0, DroppedList0),
	Length is Length0 + 1,
	list__drop(Length, H, NewHead), % Length or Length0, shouldn't matter.
	append([NewHead], DroppedList0, DroppedList).
