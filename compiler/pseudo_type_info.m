%---------------------------------------------------------------------------%
% Copyright (C) 1996-2000,2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module generates representations for pseudo-type-infos.
%
% The documentation of the structures of pseudo-type-infos is in
% runtime/mercury_type_info.h; that file also contains a list of all
% the files that depend on such data structures.
%
% Authors: trd, zs.
%
%---------------------------------------------------------------------------%

:- module backend_libs__pseudo_type_info.
:- interface.
:- import_module parse_tree__prog_data, backend_libs__rtti.
:- import_module list.

	% pseudo_type_info__construct_pseudo_type_info(Type,
	% 	NumUnivQTvars, ExistQVars, PseudoTypeInfo)
	%
	% Given a Mercury type (`Type'), this predicate returns an
	% representation of the pseudo type info for that type.
	%
	% NumUnivQTvars is either the number of universally quantified type
	% variables of the enclosing type (so that all universally quantified
	% variables in the type have numbers in the range [1..NumUnivQTvars],
	% or is the special value -1, meaning that all variables in the type
	% are universally quantified. ExistQVars is the list of existentially
	% quantified type variables of the constructor in question.

:- pred pseudo_type_info__construct_pseudo_type_info((type)::in,
	int::in, existq_tvars::in, pseudo_type_info::out) is det.

:- type pseudo_type_info
	--->	type_var(int)
			% This represents a type variable.
			% Type variables are numbered consecutively,
			% starting from 1.
	;	type_ctor_info(
			%
			% This represents a zero-arity type,
			% i.e. a type constructor with no arguments.
			%
			rtti_type_ctor
		)
	;	type_info(
			%
			% This represents a type with arity > zero,
			% i.e. a type constructor applied to some arguments.
			% The argument list should not be empty.
			%
			rtti_type_ctor,
			list(pseudo_type_info)
		)
	;	higher_order_type_info(
			%
			% This represents a higher-order or tuple type.
			% The rtti_type_ctor field will be pred/0,
			% func/0 or tuple/0; the real arity is 
			% given in the arity field.
			%
			rtti_type_ctor,
			arity,
			list(pseudo_type_info)
		)
	.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree__prog_util, check_hlds__type_util.
:- import_module int, list, term, std_util, require.

%---------------------------------------------------------------------------%

pseudo_type_info__construct_pseudo_type_info(Type, NumUnivQTvars,
		ExistQTvars, Pseudo) :-
	(
		type_to_ctor_and_args(Type, TypeCtor, TypeArgs0)
	->
		(
			% The argument to typeclass_info types is not
			% a type - it encodes the class constraint.
			% So we replace the argument with type `void'.
			mercury_private_builtin_module(PrivateBuiltin),
			TypeCtor = qualified(PrivateBuiltin, TName) - 1,
			( TName = "typeclass_info"
			; TName = "base_typeclass_info"
			)
		->
			construct_type(unqualified("void") - 0, [], ArgType),
			TypeArgs = [ArgType]
		;
			TypeArgs = TypeArgs0
		),
		(
			% For higher order types: they all refer to the
			% defined pred_0 type_ctor_info, have an extra
			% argument for their real arity, and then type
			% arguments according to their types.
			% Tuples are similar -- they use the tuple_0
			% type_ctor_info.
			% polymorphism.m has a detailed explanation.
			% XXX polymorphism.m does not have a
			% detailed explanation.
			( type_is_higher_order(Type, _, _, _) ->
				TypeName = "pred"
			; type_is_tuple(Type, _) ->
				TypeName = "tuple"
			;
				fail
			)
		->
			TypeModule = unqualified(""),
			Arity = 0,
			RttiTypeCtor = rtti_type_ctor(TypeModule, TypeName,
				Arity),
			TypeCtor = _QualTypeName - RealArity,
			pseudo_type_info__generate_args(TypeArgs,
				NumUnivQTvars, ExistQTvars, PseudoArgs),
			Pseudo = higher_order_type_info(RttiTypeCtor, RealArity,
				PseudoArgs)
		;
			TypeCtor = QualTypeName - Arity,
			unqualify_name(QualTypeName, TypeName),
			sym_name_get_module_name(QualTypeName, unqualified(""),
					TypeModule),
			RttiTypeCtor = rtti_type_ctor(TypeModule, TypeName,
				Arity),
			pseudo_type_info__generate_args(TypeArgs,
				NumUnivQTvars, ExistQTvars, PseudoArgs),
			( PseudoArgs = [] ->
				Pseudo = type_ctor_info(RttiTypeCtor)
			;
				Pseudo = type_info(RttiTypeCtor, PseudoArgs)
			)
		)
	;
		type_util__var(Type, Var)
	->
			% In the case of a type variable, we need to assign a
			% variable number *for this constructor*, i.e. taking
			% only the existentially quantified variables of
			% this constructor (and not those of other functors in
			% the same type) into account.

			% XXX term__var_to_int doesn't guarantee anything
			% about the ints returned (other than that they be
			% distinct for different variables), but here we are
			% relying more, specifically, on the integers being
			% allocated densely (i.e. the first N vars get integers
			% 1 to N).
		term__var_to_int(Var, VarInt0),
		(
			( VarInt0 =< NumUnivQTvars
			; NumUnivQTvars < 0
			)
		->
				% This is a universally quantified variable.
			VarInt = VarInt0
		;
				% It is existentially quantified.
			(
				list__nth_member_search(ExistQTvars,
					Var, ExistNum0)
			->
				VarInt = ExistNum0 +
				pseudo_type_info__pseudo_typeinfo_exist_var_base
			;
				error("construct_pseudo_type_info: var not in list")
			)
		),
		require(VarInt =< pseudo_type_info__pseudo_typeinfo_max_var,
			"type_ctor_layout: type variable representation exceeds limit"),
		Pseudo = type_var(VarInt)
	;
		error("type_ctor_layout: type neither var nor non-var")
	).

:- pred pseudo_type_info__generate_args(list(type)::in,
		int::in, existq_tvars::in, list(pseudo_type_info)::out) is det.

pseudo_type_info__generate_args(TypeArgs, NumUnivQTvars, ExistQTvars,
		PseudoArgs) :-
	list__map((pred(T::in, P::out) is det :-
		pseudo_type_info__construct_pseudo_type_info(
			T, NumUnivQTvars, ExistQTvars, P)
	), TypeArgs, PseudoArgs).

%---------------------------------------------------------------------------%

	% This number corresponds to MR_PSEUDOTYPEINFO_MAX_VAR in
	% runtime/mercury_type_info.h, and must be kept in sync with it.
	% The documentation is located there as well.

:- func pseudo_type_info__pseudo_typeinfo_max_var = int.

pseudo_type_info__pseudo_typeinfo_max_var = 1024.

	% This number corresponds to MR_PSEUDOTYPEINFO_EXIST_VAR_BASE in
	% runtime/mercury_type_info.h, and must be kept in sync with it.
	% The documentation is located there as well.
:- func pseudo_type_info__pseudo_typeinfo_exist_var_base = int.

pseudo_type_info__pseudo_typeinfo_exist_var_base = 512.

%---------------------------------------------------------------------------%
