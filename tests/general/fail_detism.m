% In some versions of the compiler, this results in a compiler abort.
% The problem is that when q and r are inlined in p, and the switch in r
% is pushed into the disjunction in q, the alternative for a is rightly noted
% as not being able to succeed, but the fail goal inserted after X=a has the
% determinism erroneous instead of fail. Since fail is represented internally
% as an empty disjunction, this violates an invariant required by the code
% generator (that every det disjunction must have an alternative that always
% succeeds).

:- module fail_detism.
:- interface.
:- import_module io, string.

:- pred main(io__state::di, io__state::uo) is cc_multi.

:- implementation.

main -->
	{ p(X) },
	io__write_string(X),
	io__write_string("\n").

:- pred p(string::out) is cc_multi.
p(X1) :-
	( q(X), r(X) ->
		X1 = X
	;
		X1 = "none"
	).

:- pred q(string::out) is multi.
q("a").
q("b").
q("c").

:- pred r(string::in) is semidet.
r("b").
r("c").
