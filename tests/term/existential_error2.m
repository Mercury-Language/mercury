% Regression test for term_util.m
% Symptom: "Software Error: Unmatched lists in functor_norm_filter_args."
% (see existential_error1.m for a description of the cause)

:- module existential_error2.

:- interface.

:- type list_of_any
	--->	nil_any
	;	some [T] cons_arg(T, list_of_any).

:- pred construct_list_of_any(list_of_any::out) is det. 

:- implementation.

construct_list_of_any(X) :-
	X = 'new cons_arg'(123, nil_any).

:- end_module existential_error2.
