%----------------------------------------------------------------------------
% Small test of functions and predicates that have the same name.
%----------------------------------------------------------------------------

:- module func_and_pred.

:- interface.

:- import_module list, io.

:- func concat(list(X), list(X)) = list(X).
:- mode concat(in, in) = out is det.

:- pred concat(list(X), list(X), list(X)).
:- mode concat(in, in, out) is det.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module string.

concat([], L) = L.
concat([E|R], L) = [E|concat(R,L)].

concat([], L, L).
concat([E|R], L, [E|Z]) :-
    concat(R,L,Z).

main -->
	{ concat(['H','e'],['l','l','o',' '], Hello) },
	{ World = concat(['w','o'],['r','l','d','!','\n']) },
	{ string__from_char_list(concat(Hello, World), HelloWorld) },
	io__write_string(HelloWorld).

%-----------------------------
