% Test handling of valid module qualified cons_ids.
:- module qualified_cons_id.
:- interface.

:- import_module maybe.

:- type foo(T)
	---> yes(T)
	;    no.

:- inst yes
	--->	maybe.yes(ground).

:- pred test(maybe(T), T).
:- mode test(in(bound(maybe.yes(ground))), out) is det.

:- pred test2(maybe(T), T).
:- mode test2(in(yes), out) is det.

:- implementation.

test(maybe.yes(T), T).

test2(maybe.yes(T), T).

