% Test case for use of zero-arity higher-order function terms.
% 
% Author: fjh

:- module nullary_ho_func_error.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- func pi = float.

pi = 3.14159.

main -->
	print("apply_nullary_func(pi) = "),
	% this would be legal:
	% print(apply_nullary_func((func) = pi)), nl.
	% this one is not:
	print(apply_nullary_func(pi)), nl.

:- func apply_nullary_func((func) = T) = T.
:- mode apply_nullary_func(in((func) = out is det)) = out is det.

apply_nullary_func(F) = apply(F).

