% Regression test for term_norm.m
% Symptom: "Software Error: Unmatched lists in functor_norm_filter_args."
% (see existential_error1.m for the general cause of the problem).
% In this particular case the mismatch in the number of arguments was 
% caused by the code generating the weight table not handling TypeClassInfos
% correctly.

:- module existential_error3.

:- interface.

:- type foo 
	---> 	some [A, B] foo(A, B) => baz(A, B)
	;	bar.

:- typeclass baz(A, B) where [].

:- pred test(foo::in) is semidet.

:- implementation.

test(X) :- X = foo(_, _).

:- end_module existential_error3.
