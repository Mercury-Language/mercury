%
% The following module gave a software error
% with the compiler of May 2nd 2000.
%
% tc_err1.m:015: Error: no determinism declaration for exported
% tc_err1.m:015:   predicate `tc_err1:handle_typedefs/3'.
% tc_err1.m:018: In instance declaration for `tc_err1:actions(tc_err1:pstate)':
% tc_err1.m:018:   no implementation for type class predicate method
% tc_err1.m:018:   `tc_err1:handle_typedefs/3'.
% tc_err1.m:018: In instance declaration for `tc_err1:actions/1': incorrect
% tc_err1.m:018:   method name(s).
% Uncaught exception:
% Software Error: missing determinism decl. How did we get this far?
% Stack dump not available in this grade.
%

:- module tc_err1.

:- interface.

:- type pstate ---> pstate.

:- typeclass actions(A) where [
        pred handle_typedefs(int, A, A) is det,
	mode handle_typedefs(in, in, out) 
].

:- instance actions(pstate).

:- implementation.

:- instance actions(pstate) where [
        pred(handle_typedefs/2) is pstate_handle_typedefs
].

