% This test should be compiled with --no-inlining. If inlining is enabled,
% the tests 42=43 and 43=43 will be evaluated at compile time, masking any
% bugs in the selection of which modes of p and q to call.

:- module cc_and_non_cc_test.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is cc_multi.

:- implementation.
:- import_module std_util.

main -->
	% test multi
	% call in single-solution context
	{ p(X) }, print(X), nl,
	% call in all-solutions context
	( { p(43) } -> print("yes") ; print("no") ), nl,

	% test nondet
	% call in single-solution context
	( { q(X) } -> print(X) ; print("no") ), nl,
	% call in all-solutions context
	( { q(43) } -> print("yes") ; print("no") ), nl.

:- pred p(int).
:- mode p(out) is multi.
:- mode p(out) is cc_multi.
p(42).
p(43).

:- pred q(int).
:- mode q(out) is cc_nondet.
:- mode q(out) is nondet.
q(42) :- semidet_succeed.
q(43) :- semidet_succeed.

