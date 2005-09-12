% Test handling of valid module qualified cons_ids.
:- module qualified_cons_id.
:- interface.

:- import_module std_util.

:- type foo(T)
	---> yes(T)
	;    no.

:- inst yes
	--->	std_util.yes(ground).

:- pred test(maybe(T), T).
:- mode test(in(bound(std_util.yes(ground))), out) is det.

:- pred test2(maybe(T), T).
:- mode test2(in(yes), out) is det.

:- implementation.

test(std_util.yes(T), T).

test2(std_util.yes(T), T).

