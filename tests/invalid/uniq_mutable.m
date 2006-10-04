:- module uniq_mutable.
:- interface.

:- type foo ---> foo.

:- implementation.

:- import_module io.

	% Some invalid mutable insts - we should get error messages.
	%
:- mutable(alpha, int, 3, unique, [untrailed]).
:- mutable(beta,  int, 4, mostly_unique, [untrailed]).
:- mutable(gamma, int, _, free, [untrailed]).
:- mutable(delta, int, _, dead, [untrailed]).
:- mutable(epsilon, int, _, mostly_dead, [untrailed]).
	
	% Some valid mutable insts - we should get no error messages.
	%
:- mutable(zeta, int, 7, ground, [untrailed]).
:- mutable(eta,  int, _, any, [untrailed]).
:- mutable(theta, pred(foo), get_foo, (pred(out) is det), [untrailed]). 
:- mutable(iota, pred(io, io), io.nl, (pred(di, uo) is det), [untrailed]).

:- pred get_foo(foo::out) is det.

get_foo(foo).
