	%
	% Tests two things
	% 	* Recognise that Length will hold the same value no
	% 	  matter if we process left to right or right to left.
	% 	* Realise that OutInt will always hold 10 so change its 
	% 	  mode from out to in and set it to be 10
	% 
	% Used to work, doesn't work with the unfold/fold
	% transformation.
	%
:- module out_to_in.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

main -->
	io__write_string("q: "),
	(
		{ q([[4,3,2,1],[3,2,1],[2,1]], 2, List, Out) }
	->
		io__write(List),
		io__write_string(" "),
		io__write(Out)
	;
		io__write_string("failed")
	),
	io__nl.

:- import_module int, list.

:- pred q(list(list(T)), int, list(list(T)), int) is semidet.
:- mode q(in, in, out, out) is semidet.


q([], _, [], 10).
q([H|T], Length, DroppedList, OutInt) :-
	Length0 = 1,
	q(T, Length0, DroppedList0, OutInt),
	_X is OutInt + Length,
	list__drop(Length, H, NewHead),
	append(DroppedList0, [NewHead], DroppedList).
