%---------------------------------------------------------------------------%
% Copyright (C) 1996-2000 The University of Melbourne.
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

:- module pseudo_type_info.

:- interface.

:- import_module llds, prog_data.

	% pseudo_type_info__construct_typed_pseudo_type_info(Type,
	% 	NumUnivQTvars, ExistQVars, Rval, LldsType, LabelNum0, LabelNum)
	%
	% Given a Mercury type (`Type'), this predicate returns an rval
	% (`Rval') giving the pseudo type info for that type, plus the
	% llds_type (`LldsType') of that rval.
	%
	% NumUnivQTvars is either the number of universally quantified type
	% variables of the enclosing type (so that all universally quantified
	% variables in the type have numbers in the range [1..NumUnivQTvars],
	% or is the special value -1, meaning that all variables in the type
	% are universally quantified. ExistQVars is the list of existentially
	% quantified type variables of the constructor in question.
	%
	% The int arguments (`LabelNum0' and `LabelNum') are label numbers for
	% generating `create' rvals with.

:- pred pseudo_type_info__construct_typed_pseudo_type_info((type)::in,
	int::in, existq_tvars::in, rval::out, llds_type::out,
	int::in, int::out) is det.

	% This is the same as the previous predicate, but does not return
	% the LLDS type.

:- pred pseudo_type_info__construct_pseudo_type_info((type)::in,
	int::in, existq_tvars::in, rval::out, int::in, int::out) is det.

:- implementation.

:- import_module hlds_data, hlds_pred, hlds_out, builtin_ops, type_util.
:- import_module rtti, make_tags, code_util, globals, options, prog_util.
:- import_module list, assoc_list, bool, string, int, map, std_util, require.
:- import_module term.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

pseudo_type_info__construct_pseudo_type_info(Type, NumUnivQTvars,
		ExistQTvars, Pseudo, CNum0, CNum) :-
	pseudo_type_info__construct_typed_pseudo_type_info(Type, NumUnivQTvars,
		ExistQTvars, Pseudo, _LldsType, CNum0, CNum).

pseudo_type_info__construct_typed_pseudo_type_info(Type, NumUnivQTvars,
		ExistQTvars, Pseudo, LldsType, CNum0, CNum) :-
	(
		type_to_type_id(Type, TypeId, TypeArgs0)
	->
		(
			% The argument to typeclass_info types is not
			% a type - it encodes the class constraint.
			mercury_private_builtin_module(PrivateBuiltin),
			TypeId = qualified(PrivateBuiltin, TName) - _,
			( TName = "typeclass_info"
			; TName = "base_typeclass_info"
			)
		->
			TypeArgs = []
		;
			TypeArgs = TypeArgs0
		),
		(
			% For higher order types: they all refer to the
			% defined pred_0 type_ctor_info, have an extra
			% argument for their real arity, and then type
			% arguments according to their types.
			% polymorphism.m has a detailed explanation.
			% XXX polymorphism.m does not have a
			% detailed explanation.
			type_is_higher_order(Type, _PredFunc,
				_EvalMethod, _TypeArgs)
		->
			TypeModule = unqualified(""),
			TypeName = "pred",
			Arity = 0,
			TypeId = _QualTypeName - RealArity,
			RealArityArg = [yes(const(int_const(RealArity)))]
		;
			TypeId = QualTypeName - Arity,
			unqualify_name(QualTypeName, TypeName),
			sym_name_get_module_name(QualTypeName, unqualified(""),
					TypeModule),
			RealArityArg = []
		),
		RttiTypeId = rtti_type_id(TypeModule, TypeName, Arity),
		DataAddr = rtti_addr(RttiTypeId, type_ctor_info),
		Pseudo0 = yes(const(data_addr_const(DataAddr))),
		LldsType = data_ptr,
		CNum1 = CNum0 + 1,

			% generate args, but remove one level of create()s.
		list__map_foldl((pred(T::in, P::out, C0::in, C::out) is det :-
			pseudo_type_info__construct_pseudo_type_info(
				T, NumUnivQTvars, ExistQTvars, P, C0, C)
		), TypeArgs, PseudoArgs0, CNum1, CNum),
		list__map(pseudo_type_info__remove_create,
			PseudoArgs0, PseudoArgs1),

		list__append(RealArityArg, PseudoArgs1, PseudoArgs),

		Reuse = no,
		Pseudo = create(0, [Pseudo0 | PseudoArgs], uniform(no),
			must_be_static, CNum1, "type_layout", Reuse)
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
				error("base_type_layout: var not in list")
			)
		),
		require(VarInt =< pseudo_type_info__pseudo_typeinfo_max_var,
			"type_ctor_layout: type variable representation exceeds limit"),
		Pseudo = const(int_const(VarInt)),
		LldsType = integer,
		CNum = CNum0
	;
		error("type_ctor_layout: type neither var nor non-var")
	).

	% Remove a create() from an rval, if present.

:- pred pseudo_type_info__remove_create(rval::in, maybe(rval)::out) is det.

pseudo_type_info__remove_create(Rval0, MaybeRval) :-
	( Rval0 = create(_, [PTI], _, _, _, _, _) ->
		MaybeRval = PTI
	;
		MaybeRval = yes(Rval0)
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
