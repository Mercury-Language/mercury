%
% Ye-olde map lookup failed strikes again.
% 
% tc_err2.m:024: In instance declaration for `tc_err2:actions(tc_err2:pstate)':
% tc_err2.m:024:   no implementation for type class predicate method
% tc_err2.m:024:   `tc_err2:handle_typedefs/3'.
% tc_err2.m:024: In instance declaration for `tc_err2:actions/1': incorrect
% tc_err2.m:024:   method name(s).
% Uncaught exception:
% Software Error: map__lookup: key not found
%	Key Type: prog_data:class_constraint
%	Key Functor: constraint/2
%	Value Type: hlds_data:constraint_proof
% Stack dump not available in this grade.
%

:- module tc_err2.

:- interface.

:- type pstate ---> pstate.

:- typeclass super(A) <= actions(A) where [
].

:- typeclass actions(A) where [
        pred handle_typedefs(int, A, A),
	mode handle_typedefs(in, in, out) is det
].

:- instance super(pstate).
:- instance actions(pstate).

:- pred foo(A) <= super(A).
:- mode foo(in) is det.

:- pred bar is det.

:- implementation.

:- instance super(pstate) where [
].

:- instance actions(pstate) where [
        pred(handle_typedefs/2) is id
].

bar :-
	foo(pstate).

foo(A) :- 
	handle_typedefs(4, A, _).
	



