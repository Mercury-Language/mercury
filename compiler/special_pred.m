%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% file: special_pred.m
% main author: fjh

% Certain predicates are implicitly defined for every type by the compiler.
% This module defines most of the characteristics of those predicates.
% (The actual code for these predicates is generated in unify_proc.m.)

%-----------------------------------------------------------------------------%

:- module special_pred.
:- interface.
:- import_module list, prog_io, hlds.

:- type special_pred_map	==	map(special_pred, pred_id).

:- type special_pred		==	pair(special_pred_id, type_id).

:- type special_pred_id
	--->	unify
	;	index
	;	compare
	;	term_to_type
	;	type_to_term.

:- pred special_pred_info(special_pred_id, type, string, list(type),
			list(mode), determinism).
:- mode special_pred_info(in, in, out, out, out, out) is det.

:- pred special_pred_name_arity(special_pred_id, string, string, int).
:- mode special_pred_name_arity(in, out, out, out) is det.
:- mode special_pred_name_arity(out, in, out, in) is semidet.
:- mode special_pred_name_arity(out, out, in, in) is semidet.

:- pred special_pred_mode_num(special_pred_id, int).
:- mode special_pred_mode_num(in, out) is det.

:- pred special_pred_list(list(special_pred_id)).
:- mode special_pred_list(out) is det.

:- pred special_pred_get_type(string, list(Type), Type).
:- mode special_pred_get_type(in, in, out) is semidet.

:- implementation.

special_pred_list([unify, index, compare]).

% **** Replace the above definition of special_pred_list with the following ****
% **** to have term_to_type and type_to_term as special preds also.	    ****
% special_pred_list([unify, index, compare, term_to_type, type_to_term]).

special_pred_name_arity(unify, "unify", "__Unify__", 2).
special_pred_name_arity(index, "index", "__Index__", 2).
special_pred_name_arity(compare, "compare", "__Compare__", 3).
special_pred_name_arity(type_to_term, "type_to_term", "__Type_To_Term__", 2).
special_pred_name_arity(term_to_type, "term_to_type", "__Term_To_Type__", 2).

	% mode num is 0 for semidet, 10000 for det
	% see make_hlds.m
special_pred_mode_num(unify, 0).
special_pred_mode_num(index, 10000).
special_pred_mode_num(compare, 10000).
special_pred_mode_num(type_to_term, 10000).
special_pred_mode_num(term_to_type, 0).

special_pred_info(unify, Type, "__Unify__", [Type, Type], [In, In2], semidet) :-
	in_mode(In),
	in_mode(In2).
		% we use `In2' to work around a bug with --static-ground-terms
		% which causes a duplicate label in the generated C code

special_pred_info(index, Type, "__Index__", [Type, IntType], [In, Out], det) :-
	term__context_init(Context),
	IntType = term__functor(term__atom("int"), [], Context),
	in_mode(In),
	out_mode(Out).

special_pred_info(compare, Type,
		 "__Compare__", [ResType, Type, Type], [Out, In, In2], det) :-
	term__context_init(Context),
	ResType = term__functor(term__atom("comparison_result"), [], Context),
	in_mode(In),
	in_mode(In2),
	out_mode(Out).

special_pred_info(term_to_type, Type,
		"__Term_To_Type__", [TermType, Type], [In, Out], semidet) :-
	term__context_init(Context),
	TermType = term__functor(term__atom("term"), [], Context),
	in_mode(In),
	out_mode(Out).

special_pred_info(type_to_term, Type,
		"__Type_To_Term__", [Type, TermType], [In, Out], det) :-
	term__context_init(Context),
	TermType = term__functor(term__atom("term"), [], Context),
	in_mode(In),
	out_mode(Out).

:- pred in_mode((mode)::out) is det.

in_mode(user_defined_mode(unqualified("in"), [])).

:- pred out_mode((mode)::out) is det.

out_mode(user_defined_mode(unqualified("out"), [])).


	% Given the mangled predicate name and the list of argument types,
	% work out which type this special predicate is for.
	% Note that this gets called after the polymorphism.m pass, so
	% type_info arguments may have been inserted at the start; hence we
	% find the type at a known position from the end of the list
	% (by using list__reverse).

	% Currently for most of the special predicates the type variable can be
	% found in the last type argument, except for index and type_to_term,
	% for which it is the second-last argument.

special_pred_get_type("__Unify__", Types, T) :-
	list__reverse(Types, [T | _]).
special_pred_get_type("__Index__", Types, T) :-
	list__reverse(Types, [_, T | _]).
special_pred_get_type("__Compare__", Types, T) :-
	list__reverse(Types, [T | _]).
special_pred_get_type("__Type_To_Term__", Types, T) :-
	list__reverse(Types, [_, T | _]).
special_pred_get_type("__Term_To_Type__", Types, T) :-
	list__reverse(Types, [T | _]).

%-----------------------------------------------------------------------------%
