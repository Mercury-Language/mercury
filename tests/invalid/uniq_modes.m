:- module uniq_modes.
:- interface.
:- import_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

:- implementation.
:- import_module list,std_util.

main(In, _Out) :-
	io__write("looking for", In, Int1),
	io__nl(Int1, _Int2),
	fail.
main(In, Out) :-
	io__write("not found", In, Int),
	io__nl(Int, Out).

