% Various checks that promise_equivalent_solutions goals are treated properly.

:- module promise_equivalent_solutions_test.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int, list, string.

main(!IO) :-
	% The equality theory with respect to which all solutions of the goal
	% inside the promise_equivalent_solution are equivalent is the one that
	% views the lists as unsorted representations of sets, possibly with
	% duplicates.
	promise_equivalent_solutions [A, B] (
		( A = [1, 2]
		; A = [2, 1]
		),
		( B = [44, 33]
		; B = [33, 44]
		)
	),
	list__sort_and_remove_dups(A, ASorted),
	list__sort_and_remove_dups(B, BSorted),
	io__write(ASorted, !IO),
	io__nl(!IO),
	io__write(BSorted, !IO),
	io__nl(!IO),
	(
		promise_equivalent_solutions [C] (
			ASorted = [_ | ATail],
			( C = [5] ++ ATail
			; C = ATail ++ [5]
			)
		)
	->
		list__sort_and_remove_dups(C, CSorted),
		io__write(CSorted, !IO),
		io__nl(!IO)
	;
		io__write("cannot compute CSorted\n", !IO)
	).
