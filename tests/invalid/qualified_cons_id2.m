% Test handling of valid module qualified cons_ids.
:- module qualified_cons_id2.
:- interface.

:- import_module std_util.

:- type foo(T)
	---> yes(T)
	;    no.

:- inst yes
	--->	qualified_cons_id2:yes(ground).

	% list__apply(Cs, Bs) takes a list of closures with one
	% output argument Cs, and calls the closures, returning
	% the resulting bindings in Bs.
:- pred test(maybe(T), T).
:- mode test(in(bound(qualified_cons_id2:yes(ground))), out) is det.
:- mode test(in(yes), out) is det.

:- implementation.

test(std_util:yes(T), T).
