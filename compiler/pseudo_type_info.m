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

	% pseudo_type_info__construct_pseudo_type_info(Type,
	% 	NumUnivQTvars, ExistQVars, PseudoTypeInfo)
	%
	% Given a Mercury type (`Type'), this predicate returns a
	% representation of the pseudo type info for that type.
	%
	% NumUnivQTvars is either the number of universally quantified type
	% variables of the enclosing type (so that all universally quantified
	% variables in the type have numbers in the range [1..NumUnivQTvars],
	% or is the special value -1, meaning that all variables in the type
	% are universally quantified. ExistQVars is the list of existentially
	% quantified type variables of the constructor in question.

:- pred pseudo_type_info__construct_pseudo_type_info((type)::in,
	int::in, existq_tvars::in, rtti_pseudo_type_info::out) is det.

	% pseudo_type_info__construct_type_info(Type, TypeInfo)
	%
	% Given a ground Mercury type (`Type'), this predicate returns a
	% representation of the type info for that type.

:- pred pseudo_type_info__construct_type_info((type)::in, rtti_type_info::out)
	is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree__prog_util, check_hlds__type_util.
:- import_module int, list, term, std_util, require.

%---------------------------------------------------------------------------%

pseudo_type_info__construct_pseudo_type_info(Type, NumUnivQTvars,
		ExistQTvars, Pseudo) :-
	( type_to_ctor_and_args(Type, TypeCtor, TypeArgs0) ->
		canonicalize_type_args(TypeCtor, TypeArgs0, TypeArgs),
		( type_is_var_arity(Type, VarArityId) ->
			TypeCtor = _QualTypeName - RealArity,
			pseudo_type_info__generate_pseudo_args(TypeArgs,
				NumUnivQTvars, ExistQTvars, PseudoArgs),
			require(check_var_arity(VarArityId, PseudoArgs,
				RealArity),
				"construct_pseudo_type_info: arity mismatch"),
			Pseudo = var_arity_pseudo_type_info(VarArityId,
				PseudoArgs)
		;
			TypeCtor = QualTypeName - Arity,
			unqualify_name(QualTypeName, TypeName),
			sym_name_get_module_name(QualTypeName, unqualified(""),
				TypeModule),
			RttiTypeCtor = rtti_type_ctor(TypeModule, TypeName,
				Arity),
			pseudo_type_info__generate_pseudo_args(TypeArgs,
				NumUnivQTvars, ExistQTvars, PseudoArgs),
			require(check_arity(PseudoArgs, Arity),
				"construct_pseudo_type_info: arity mismatch"),
			( PseudoArgs = [] ->
				Pseudo = plain_arity_zero_pseudo_type_info(
					RttiTypeCtor)
			;
				Pseudo = plain_pseudo_type_info(RttiTypeCtor,
					PseudoArgs)
			)
		)
	; type_util__var(Type, Var) ->
		% In the case of a type variable, we need to assign a
		% variable number *for this constructor*, i.e. taking
		% only the existentially quantified variables of
		% this constructor (and not those of other functors in
		% the same type) into account.

		% XXX term__var_to_int doesn't guarantee anything about the
		% ints returned (other than that they be distinct for
		% different variables), but here we are relying more,
		% specifically, on the integers being allocated densely
		% (i.e. the first N vars get integers 1 to N).

		term__var_to_int(Var, VarInt0),
		(
			( VarInt0 =< NumUnivQTvars
			; NumUnivQTvars < 0
			)
		->
			% This is a universally quantified variable.
			VarInt = VarInt0
		;
			% This is an existentially quantified variable.
			(
				list__nth_member_search(ExistQTvars,
					Var, ExistNum0)
			->
				VarInt = ExistNum0 +
				pseudo_type_info__pseudo_typeinfo_exist_var_base
			;
				error("construct_pseudo_type_info: not in list")
			)
		),
		require(VarInt =< pseudo_type_info__pseudo_typeinfo_max_var,
			"construct_pseudo_type_info: type var exceeds limit"),
		Pseudo = type_var(VarInt)
	;
		error("construct_pseudo_type_info: neither var nor non-var")
	).

pseudo_type_info__construct_type_info(Type, TypeInfo) :-
	( type_to_ctor_and_args(Type, TypeCtor, TypeArgs0) ->
		canonicalize_type_args(TypeCtor, TypeArgs0, TypeArgs),
		( type_is_var_arity(Type, VarArityId) ->
			TypeCtor = _QualTypeName - RealArity,
			pseudo_type_info__generate_plain_args(TypeArgs,
				TypeInfoArgs),
			require(check_var_arity(VarArityId, TypeInfoArgs,
				RealArity),
				"construct_type_info: arity mismatch"),
			TypeInfo = var_arity_type_info(VarArityId,
				TypeInfoArgs)
		;
			TypeCtor = QualTypeName - Arity,
			unqualify_name(QualTypeName, TypeName),
			sym_name_get_module_name(QualTypeName, unqualified(""),
				TypeModule),
			RttiTypeCtor = rtti_type_ctor(TypeModule, TypeName,
				Arity),
			pseudo_type_info__generate_plain_args(TypeArgs,
				TypeInfoArgs),
			require(check_arity(TypeInfoArgs, Arity),
				"construct_type_info: arity mismatch"),
			( TypeInfoArgs = [] ->
				TypeInfo = plain_arity_zero_type_info(
					RttiTypeCtor)
			;
				TypeInfo = plain_type_info(RttiTypeCtor,
					TypeInfoArgs)
			)
		)
	;
		error("construct_type_info: type is var")
	).

:- pred check_var_arity(var_arity_ctor_id::in, list(T)::in, int::in)
	is semidet.

check_var_arity(VarArityId, Args, RealArity) :-
	list__length(Args, NumPseudoArgs),
	( VarArityId = func_type_info ->
		NumPseudoArgs = RealArity + 1
	;
		NumPseudoArgs = RealArity
	).

:- pred check_arity(list(T)::in, int::in) is semidet.

check_arity(Args, RealArity) :-
	list__length(Args, NumPseudoArgs),
	NumPseudoArgs = RealArity.

:- pred pseudo_type_info__generate_pseudo_args(list(type)::in, int::in,
	existq_tvars::in, list(rtti_maybe_pseudo_type_info)::out) is det.

pseudo_type_info__generate_pseudo_args(TypeArgs, NumUnivQTvars, ExistQTvars,
		PseudoArgs) :-
	list__map(pseudo_type_info__generate_pseudo_arg(NumUnivQTvars,
			ExistQTvars),
		TypeArgs, PseudoArgs).

:- pred pseudo_type_info__generate_pseudo_arg(int::in, existq_tvars::in,
	(type)::in, rtti_maybe_pseudo_type_info::out) is det.

pseudo_type_info__generate_pseudo_arg(NumUnivQTvars, ExistQTvars,
		TypeArg, MaybePseudoArg) :-
	( term__is_ground(TypeArg) ->
		pseudo_type_info__construct_type_info(TypeArg, PseudoArg),
		MaybePseudoArg = plain(PseudoArg)
	;
		pseudo_type_info__construct_pseudo_type_info(TypeArg,
			NumUnivQTvars, ExistQTvars, PseudoArg),
		MaybePseudoArg = pseudo(PseudoArg)
	).

:- pred pseudo_type_info__generate_plain_args(list(type)::in,
	list(rtti_type_info)::out) is det.

pseudo_type_info__generate_plain_args(TypeArgs,
		PseudoArgs) :-
	list__map(pseudo_type_info__construct_type_info, TypeArgs, PseudoArgs).

%---------------------------------------------------------------------------%

:- pred canonicalize_type_args(type_ctor::in, list(type)::in, list(type)::out)
	is det.

canonicalize_type_args(TypeCtor, TypeArgs0, TypeArgs) :-
	(
		% The argument to typeclass_info types is not
		% a type - it encodes the class constraint.
		% So we replace the argument with type `void'.
		mercury_private_builtin_module(PrivateBuiltin),
		TypeCtor = qualified(PrivateBuiltin, TypeName) - 1,
		( TypeName = "typeclass_info"
		; TypeName = "base_typeclass_info"
		)
	->
		construct_type(unqualified("void") - 0, [], ArgType),
		TypeArgs = [ArgType]
	;
		TypeArgs = TypeArgs0
	).

	% Type_infos and pseudo_type_infos whose principal type
	% constructor is a variable arity type constructor
	% must be handled specially, in that they must include
	% the actual arity of the given instance between the
	% type constructor and the arguments.
	% runtime/mercury_type_info.h has the details.
	%
	% All variable arity type constructors are builtins.
	% At the moment, we have three: pred, func, and tuple.

:- pred type_is_var_arity((type)::in, var_arity_ctor_id::out) is semidet.

type_is_var_arity(Type, VarArityCtorId) :-
	( type_is_higher_order(Type, PredOrFunc, _, _) ->
		(
			PredOrFunc = predicate,
			VarArityCtorId = pred_type_info
		;
			PredOrFunc = function,
			VarArityCtorId = func_type_info
		)
	; type_is_tuple(Type, _) ->
		VarArityCtorId = tuple_type_info
	;
		fail
	).

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
