% Causes the following assertion failure in rotd-2005-02-07
% and before:
%
%	Uncaught Mercury exception:
%	Software Error: map__lookup: key not found
%		Key Type: term.var(parse_tree.prog_data.prog_var_type)
%		Key Value: var(13)
%		Value Type: ll_backend.var_locn.var_state
%
% Compile with: mmc -C -O0 --common-struct --loop-invariants

:- module loop_inv_bug.

:- interface.

:- import_module sparse_bitset.
:- import_module term.

:- pred foo(var(T)::in, sparse_bitset(var(T))::in, sparse_bitset(var(T))::in)
	is det.

:- implementation.

foo(Var, TrueVars0, FalseVars0) :-
 	sparse_bitset.remove_leq(TrueVars0, Var, TrueVars),
	sparse_bitset.remove_leq(FalseVars0, Var, FalseVars), 
	foo(Var, TrueVars, FalseVars).
