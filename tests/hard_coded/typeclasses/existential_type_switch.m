% Regression test: rotd-1999-10-12 did not handle switches on existential
% data types.
%------------------------------------------------------------------------------%

:- module existential_type_switch.

:- interface.

:- import_module io.

:- pred main(state::di, state::uo) is det.

:- typeclass tc(TC) where [
	pred r(TC, int),
	mode r(in, out) is semidet
].

:- type maybe
	--->	no
	;	some [T] (yes(T) => tc(T)).

:- pred p(maybe).
:- mode p(out) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module int.

main -->
	{ p(X) },
	(	{ X = yes(Y) },
		(if { r(Y, Z) } then
			print("r succeeded, Z = "), print(Z), nl
		else
			print("r failed"), nl
		)
	;
		{ X = no },
		print("no"), nl
	).

p(X) :-
	(
		q(1, Y)
	->
		X = 'new yes'(Y)
	;
		X = no
	).

:- some [T2] pred q(int::in, T2::out) is semidet => tc(T2).

q(1, 2).

%------------------------------------------------------------------------------%

:- instance tc(int) where [
	pred(r/2) is s
].

:- pred s(int::in, int::out) is semidet.

s(1, 111).
s(2, 42).

%------------------------------------------------------------------------------%
