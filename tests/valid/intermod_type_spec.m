:- module intermod_type_spec.

:- interface.

:- import_module list.

:- pred call_p(list(int)::in, list(list(int))::in) is semidet.

:- implementation.

:- import_module intermod_type_spec_2.

call_p(A, B) :- p(A, B).

