% Test handling of invalid module qualified cons_ids.
:- module qualified_cons_id2.
:- interface.

:- import_module std_util.

:- type foo(T)
	---> yes(T)
	;    no.

:- inst yes
	--->	qualified_cons_id2.yes(ground).

:- pred test(maybe(T), T).
:- mode test(in(bound(qualified_cons_id2.yes(ground))), out) is det.
:- mode test(in(yes), out) is det.

:- implementation.

test(std_util.yes(T), T).
