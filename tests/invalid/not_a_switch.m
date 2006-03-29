% Test that switch_detection's search for deconstruction unifications
% doesn't go past any calls.
:- module not_a_switch.

:- interface.

:- import_module int, maybe.

:- pred not_a_switch(maybe(int)::in, int::out) is det.

:- implementation.

not_a_switch(A, Int1 + Int2) :-
	Int1 = 1 + 1,
	A = yes(Int2).
not_a_switch(no, 0).
