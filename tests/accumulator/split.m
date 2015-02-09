	%
	% Test that all the output variables must be related to 
	% a variable in the recursive call.
	%
:- module split.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module list, int.

main -->
	{ p([1,7,4], S) },
	io__write_string("p: "),
	io__write(S),
	io__nl.

:- pred p(list(int), int).
:- mode p(in, out) is det.

p([], 0).
p([H|T], S) :-
    p(T, _),
    Tmp = 0,
    S is H + Tmp.
