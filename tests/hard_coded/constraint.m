% Test constraint propagation.
:- module constraint.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module list.

main -->
	{ benchmark([1,16,100,15,20], Found) },
	io__write_string(Found).

:- pred benchmark(list(int), string).
:- mode benchmark(in, out) is det.
	% Disable unrolling of the loop.
:- pragma no_inline(benchmark/2).

benchmark(Data, Out) :-
	( mymember(X, Data), test(X) ->
		Out = "found"
	;
		Out = "not_found"
	).

:- pred mymember(int, list(int)).
:- mode mymember(out, in) is nondet.

mymember(X, [X|_]).
mymember(X, [_|Xs]) :- mymember(X, Xs).

:- pred test(int).
:- mode test(in) is semidet.

test(15).
