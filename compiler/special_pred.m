%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1998 The University of Melbourne.
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
:- import_module prog_data, hlds_data, hlds_pred.
:- import_module list, map, std_util.

:- type special_pred_map	==	map(special_pred, pred_id).

:- type special_pred		==	pair(special_pred_id, type_id).

:- type special_pred_id
	--->	unify
	;	index
	;	compare.

:- pred special_pred_info(special_pred_id, type, string, list(type),
			list(mode), determinism).
:- mode special_pred_info(in, in, out, out, out, out) is det.

	% special_pred_name_arity(SpecialPredType, GenericPredName,
	%		TypeSpecificVersionPredName, Arity):
	%	true iff there is a special predicate of category
	%	SpecialPredType, called builtin:GenericPredName/Arity,
	%	for which the type-specific versions will be called
	%	TypeSpecificVersionPredName.
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

:- pred special_pred_description(special_pred_id, string).
:- mode special_pred_description(in, out) is det.

:- implementation.

:- import_module type_util, mode_util, prog_util.

special_pred_list([unify, index, compare]).

special_pred_name_arity(unify, "unify", "__Unify__", 2).
special_pred_name_arity(index, "index", "__Index__", 2).
special_pred_name_arity(compare, "compare", "__Compare__", 3).

	% mode num for special procs is always 0 (the first mode)
special_pred_mode_num(_, 0).

special_pred_info(unify, Type, "__Unify__", [Type, Type], [In, In], semidet) :-
	in_mode(In).

special_pred_info(index, Type, "__Index__", [Type, IntType], [In, Out], det) :-
	construct_type(unqualified("int") - 0, [], IntType),
	in_mode(In),
	out_mode(Out).

special_pred_info(compare, Type,
		 "__Compare__", [ResType, Type, Type], [Uo, In, In], det) :-
	mercury_public_builtin_module(PublicBuiltin),
	construct_type(qualified(PublicBuiltin, "comparison_result") - 0,
							[], ResType),
	in_mode(In),
	uo_mode(Uo).

	% Given the mangled predicate name and the list of argument types,
	% work out which type this special predicate is for.
	% Note that this gets called after the polymorphism.m pass, so
	% type_info arguments may have been inserted at the start; hence we
	% find the type at a known position from the end of the list
	% (by using list__reverse).

	% Currently for most of the special predicates the type variable
	% can be found in the last type argument, except for index, for
	% which it is the second-last argument.

special_pred_get_type("__Unify__", Types, T) :-
	list__reverse(Types, [T | _]).
special_pred_get_type("unify", Types, T) :-
	list__reverse(Types, [T | _]).
special_pred_get_type("__Index__", Types, T) :-
	list__reverse(Types, [_, T | _]).
special_pred_get_type("index", Types, T) :-
	list__reverse(Types, [_, T | _]).
special_pred_get_type("__Compare__", Types, T) :-
	list__reverse(Types, [T | _]).
special_pred_get_type("compare", Types, T) :-
	list__reverse(Types, [T | _]).

special_pred_description(unify, "unification predicate").
special_pred_description(compare, "comparison predicate").
special_pred_description(index, "indexing predicate").

%-----------------------------------------------------------------------------%
