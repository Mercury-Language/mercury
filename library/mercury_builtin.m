%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: mercury_builtin.m.
% Main author: fjh.
% Stability: low.

% This file is automatically imported into every module.
% It is intended for things that are part of the language,
% but which are implemented just as normal user-level code
% rather than with special coding in the compiler.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module mercury_builtin.
:- interface.

%-----------------------------------------------------------------------------%

% TYPES.

% The types `character', `int', `float', and `string',
% and the types `pred', `pred(T)', `pred(T1, T2)', `pred(T1, T2, T3)', ...
% are builtin and are implemented using special code in the
% type-checker.  (XXX TODO: report an error for attempts to redefine
% these types.)

%-----------------------------------------------------------------------------%

% INSTS.

% The standard insts `free', `ground', and `bound(...)' are builtin
% and are implemented using special code in the parser and mode-checker.

% Unique insts (aliases for backwards compatibility with old code).

:- inst dead = clobbered.

/*
:- inst dead = ground.
:- inst clobbered = ground.
:- inst unique = ground.
*/

%-----------------------------------------------------------------------------%

% MODES.

% The standard modes.

:- mode unused :: (free -> free).
:- mode output :: (free -> ground).
:- mode input :: (ground -> ground).

:- mode in :: (ground -> ground).
:- mode out :: (free -> ground).

:- mode in(Inst) :: (Inst -> Inst).
:- mode out(Inst) :: (free -> Inst).

% Unique modes.  These are still not fully implemented.

:- mode uo :: free -> unique.
:- mode ui :: unique -> unique.
:- mode di :: unique -> clobbered.

% Higher-order predicate modes are builtin.

%-----------------------------------------------------------------------------%

% PREDICATES.

% copy/2 is used to make a `unique' copy of a data structure,
% so that you can use destructive update.
% At the moment it doesn't actually do any copying, since we
% haven't implemented destructive update yet and so there is no need.

:- pred copy(T, T).
:- mode copy(ui, uo) is det.
:- mode copy(in, uo) is det.

% We define !/0 (and !/2 for dcgs) to be equivalent to `true'.  This is for
% backwards compatibility with Prolog systems.  But of course it only works
% if all your cuts are green cuts.

:- pred ! is det.

:- pred !(T, T).
:- mode !(di, uo) is det.
:- mode !(in, out) is det.

% The call/N family.  Note that the compiler (make_hlds.nl) will transform
% goals which are not atoms (e.g. goals which are free variables) into
% calls to call/1.

% call/N is really a polymorphically moded, builtin part of the language;
% the many declarations below are just an artifact of the current
% implementation.  They ought to be builtin.

:- pred call(pred).
:- mode call((pred) is semidet) is semidet.

:- pred call(pred(T), T).

:- mode call(pred(in)  is semidet, in)  is semidet.

:- mode call(pred(uo)  is det,     uo)  is det.

:- mode call(pred(out) is det,     out) is det.
:- mode call(pred(out) is semidet, out) is semidet.
:- mode call(pred(out) is multi,   out) is multi.
:- mode call(pred(out) is nondet,  out) is nondet.

:- pred call(pred(T1, T2), T1, T2).

:- mode call(pred(in,  in)  is semidet, in,  in)  is semidet.

:- mode call(pred(di,  uo)  is det,     di,  uo)  is det.

:- mode call(pred(in,  out) is det,     in,  out) is det.
:- mode call(pred(in,  out) is semidet, in,  out) is semidet.
:- mode call(pred(in,  out) is multi,   in,  out) is multi.
:- mode call(pred(in,  out) is nondet,  in,  out) is nondet.
 
/* The following modes are not supported.
   We require all input arguments to come before all output arguments.

:- mode call(pred(uo, di)   is det,     uo,  di)  is det.
:- mode call(pred(out, in)  is det,     out, in)  is det.
:- mode call(pred(out, in)  is semidet, out, in)  is semidet.
:- mode call(pred(out, in)  is multi,   out, in)  is multi.
:- mode call(pred(out, in)  is nondet,  out, in)  is nondet.
*/

:- mode call(pred(uo,  uo)  is det,     uo,  uo)  is det.

:- mode call(pred(out, out) is det,     out, out) is det.
:- mode call(pred(out, out) is semidet, out, out) is semidet.
:- mode call(pred(out, out) is multi,   out, out) is multi.
:- mode call(pred(out, out) is nondet,  out, out) is nondet.

:- pred call(pred(T1, T2, T3), T1, T2, T3).

:- mode call(pred(in,  in,  in)  is semidet, in,  in,  in)  is semidet.

:- mode call(pred(di,  di,  uo)  is det,     di,  di,  uo)  is det.

:- mode call(pred(in,  in,  out) is det,     in,  in,  out) is det.
:- mode call(pred(in,  in,  out) is semidet, in,  in,  out) is semidet.
:- mode call(pred(in,  in,  out) is multi,   in,  in,  out) is multi.
:- mode call(pred(in,  in,  out) is nondet,  in,  in,  out) is nondet.
 
:- mode call(pred(di,  uo,  uo)  is det,     di,  uo,  uo)  is det.

:- mode call(pred(in,  out, out) is det,     in,  out, out) is det.
:- mode call(pred(in,  out, out) is semidet, in,  out, out) is semidet.
:- mode call(pred(in,  out, out) is multi,   in,  out, out) is multi.
:- mode call(pred(in,  out, out) is nondet,  in,  out, out) is nondet.

:- mode call(pred(uo,  uo,  uo) is det,      uo,  uo,  uo) is det.

:- mode call(pred(out, out, out) is det,     out, out, out) is det.
:- mode call(pred(out, out, out) is semidet, out, out, out) is semidet.
:- mode call(pred(out, out, out) is multi,   out, out, out) is multi.
:- mode call(pred(out, out, out) is nondet,  out, out, out) is nondet.

% In addition, the following predicate-like constructs are builtin:
%
%	:- pred (T = T).
%	:- pred (T \= T).
%	:- pred (pred , pred).
%	:- pred (pred ; pred).
%	:- pred (\+ pred).
%	:- pred (not pred).
%	:- pred (pred -> pred).
%	:- pred (if pred then pred).
%	:- pred (if pred then pred else pred).
%	:- pred (pred => pred).
%	:- pred (pred <= pred).
%	:- pred (pred <=> pred).
%
%	(pred -> pred ; pred).
%	some Vars pred
%	all Vars pred

%-----------------------------------------------------------------------------%

	% unify(X, Y) is true iff X = Y.
:- pred unify(T::in, T::in) is semidet.

:- type comparison_result ---> (=) ; (<) ; (>).

	% compare(Res, X, Y) binds Res to =, <, or >
	% depending on whether X is =, <, or > Y in the
	% standard ordering.
:- pred compare(comparison_result::out, T::in, T::in) is det.

	% The following three predicates can convert values of any
	% type to the type `term' and back again.
	% However, they are not yet implemented.

:- pred term_to_type(term :: in, T :: out) is semidet.

:- pred det_term_to_type(term :: in, T :: out) is det.

:- pred type_to_term(T :: in, term :: out) is det.

% The following are used by the compiler, to implement polymorphism.
% They should not be used in programs.

:- pred index(T::in, int::out) is det.

:- pred builtin_unify_int(int::in, int::in) is semidet.
:- pred builtin_index_int(int::in, int::out) is det.
:- pred builtin_compare_int(comparison_result::out, int::in, int::in) is det.
:- pred builtin_term_to_type_int(term :: in, int :: out) is semidet.
:- pred builtin_type_to_term_int(int :: in, term :: out) is det.

:- pred builtin_unify_string(string::in, string::in) is semidet.
:- pred builtin_index_string(string::in, int::out) is det.
:- pred builtin_compare_string(comparison_result::out, string::in, string::in)
	is det.
:- pred builtin_term_to_type_string(term :: in, string :: out) is semidet.
:- pred builtin_type_to_term_string(string :: in, term :: out) is det.

:- pred builtin_unify_float(float::in, float::in) is semidet.
:- pred builtin_index_float(float::in, int::out) is det.
:- pred builtin_compare_float(comparison_result::out, float::in, float::in)
	is det.
:- pred builtin_term_to_type_float(term :: in, float :: out) is semidet.
:- pred builtin_type_to_term_float(float :: in, term :: out) is det.

:- pred builtin_unify_pred((pred)::in, (pred)::in) is semidet.
:- pred builtin_index_pred((pred)::in, int::out) is det.
:- pred builtin_compare_pred(comparison_result::out, (pred)::in, (pred)::in)
	is det.

:- pred compare_error is erroneous.

:- type type_info(T) ---> type_info(int /*, ... */).

% These should be defined in int.nl, but we define them here since
% they're need for the implementation of compare/3.

:- pred <(int, int).
:- mode <(in, in) is semidet.

:- pred >(int, int).
:- mode >(in, in) is semidet.

% The types term and const should be defined in term.m, but we define them here
% since they're need for implementation of term_to_type/2 and type_to_term/2.

:- type term		--->	term__functor(const, list(term), term__context)
			;	term__variable(var).
:- type const		--->	term__atom(string)
			;	term__integer(int)
			;	term__string(string)
			;	term__float(float).
:- type var.

% The type list should be defined in list.m, but we define it here since
% it's need for the implementation of term_to_type/2 and type_to_term/2.

:- type list(T) ---> [] ; [T | list(T)].

        % At the moment, the only context we store is the line
        % number.

:- type term__context	--->	term__context(string, int).
				% file, line number.

:- pred term__context_init(term__context).
:- mode term__context_init(out) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module require, std_util, float, list.

% Many of the predicates defined in this module are builtin -
% the compiler generates code for them inline.

%-----------------------------------------------------------------------------%

!.
!(X, X).

%-----------------------------------------------------------------------------%

:- external(copy/2).

:- external(unify/2).
:- external(index/2).
:- external(compare/3).
:- external(term_to_type/2).
:- external(type_to_term/2).

det_term_to_type(Term, X) :-
	( term_to_type(Term, X1) ->
		X = X1
	;
		error("det_term_to_type failed as term doesn't represent a valid ground value of the appropriate type")
	).


builtin_unify_int(X, X).

builtin_index_int(X, X).

builtin_compare_int(R, X, Y) :-
	( X < Y ->
		R = (<)
	; X = Y ->
		R = (=)
	;
		R = (>)
	).

builtin_term_to_type_int(term__functor(term__integer(Int), _TermList, _Context),
									Int).

builtin_type_to_term_int(Int, term__functor(term__integer(Int), [], Context)) :-
	term__context_init(Context).

builtin_unify_string(S, S).

builtin_index_string(_, -1).

builtin_compare_string(R, S1, S2) :-
	builtin_strcmp(Res, S1, S2),
	( Res < 0 ->
		R = (<)
	; Res = 0 ->
		R = (=)
	;
		R = (>)
	).

builtin_term_to_type_string(
	term__functor(term__string(String), _TermList, _Context), String).

builtin_type_to_term_string(
		String, term__functor(term__string(String), [], Context)) :- 
        term__context_init(Context).


builtin_unify_float(F, F).

builtin_index_float(_, -1).

builtin_compare_float(R, F1, F2) :-
	( builtin_float_lt(F1, F2) ->
		R = (<)
	; builtin_float_gt(F1, F2) ->
		R = (>)
	;
		R = (=)
	).

builtin_term_to_type_float(term__functor(
			term__float(Float), _TermList, _Context), Float).

builtin_type_to_term_float(
		Float, term__functor(term__float(Float), [], Context)) :-
	term__context_init(Context).

:- pred builtin_strcmp(int, string, string).
:- mode builtin_strcmp(out, in, in) is det.

:- external(builtin_strcmp/3).

builtin_unify_pred(_Pred1, _Pred2) :-
	% suppress determinism warning
	( semidet_succeed ->
		error("attempted unification of higher-order predicate terms")
	;
		semidet_fail
	).

builtin_compare_pred(Res, _Pred1, _Pred2) :-
	% suppress determinism warning
	( semidet_succeed ->
		error("attempted comparison of higher-order predicate terms")
	;
		% the following is never executed
		Res = (<)
	).

builtin_index_pred(_, -1).

	% This is used by the code that the compiler generates for compare/3.
compare_error :-
	error("internal error in compare/3").

term__context_init(term__context("", 0)).

:- end_module mercury_builtin.

%-----------------------------------------------------------------------------%
