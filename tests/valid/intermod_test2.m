:- module intermod_test2.

:- interface.

:- import_module int.
:- pred baz(int::in) is semidet.

:- func plusone(int :: in) = (int :: out) is det.

:- implementation.

:- type t
	--->	f(int)
	;	g.

:- mode int_mode :: in.


baz(X) :- T = f(1), bar(T, X).

:- pred bar(t::in, int::int_mode) is semidet.

bar(T, 2) :- T = f(1).

:- pred local(pred(int), int).
:- mode local(pred(int_mode) is det, out) is det.

local(Pred, Int) :- call(Pred, Int0), Int is Int0 + 1.

% One version of the compiler incorrectly wrote this declaration to
% the .opt file as `:- pragma inline((intermod_test2:plusone)/2).'
% 		-- bromage  20 Nov 1997
:- pragma inline(plusone/1).

plusone(Int0) = Int :- Int is Int0 + 1.


