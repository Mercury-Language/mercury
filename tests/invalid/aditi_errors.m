:- module aditi_errors.

:- interface.

:- import_module aditi.
:- import_module list, map.

:- pred aditi_error(aditi__state::aditi_mui,
		int::out, int::out, foo::out, map(int, int)::in) is nondet.
:- pragma aditi(aditi_error/5).

:- type foo
	---> foo(int).

:- type existq
	--->	some [T] existq(T).

:- pred p(aditi__state::aditi_mui, int::out, int::out) is nondet.
:- pragma base_relation(p/3).

:- pred q(aditi__state::aditi_mui, list(T)::in, existq::out,
		list(T)::out(list_skel(free))) is nondet.
:- pragma base_relation(q/4).

/*
	% Class constraints of the form used in this example
	% are not yet implemented.

:- pred tclass(aditi__state, int, list(int)) <= class(int).
:- mode tclass(aditi_mui, out, out) is nondet.
:- pragma base_relation(tclass/4).

:- typeclass class(T) where [].
*/

:- implementation.

aditi_error(DB, A, B, Thing, _) :- 
	Thing = foo(_),
	p(DB, A, B),
	Thing = foo(A).

