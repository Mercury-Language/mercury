%---------------------------------------------------------------------------%
% Copyright (C) 2000,2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% file: ll_pseudo_type_info.m
% author: fjh
%
% This module generates LLDS representations for pseudo-type-infos.
%
% Most of the work is done by pseudo_type_info.m, which generates
% a back-end-independent representation of pseudo-type-infos;
% this module just converts that representation to LLDS.
%
% The documentation of the structures of pseudo-type-infos is in
% runtime/mercury_type_info.h; that file also contains a list of all
% the files that depend on such data structures.
%
%---------------------------------------------------------------------------%

:- module ll_backend__ll_pseudo_type_info.

:- interface.

:- import_module parse_tree__prog_data, ll_backend__llds.
:- import_module counter.

	% ll_pseudo_type_info__construct_typed_pseudo_type_info(Type,
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

:- pred ll_pseudo_type_info__construct_typed_llds_pseudo_type_info((type)::in,
	int::in, existq_tvars::in, rval::out, llds_type::out,
	counter::in, counter::out) is det.

	% This is the same as the previous predicate, but does not return
	% the LLDS type.

:- pred ll_pseudo_type_info__construct_llds_pseudo_type_info((type)::in,
	int::in, existq_tvars::in, rval::out, counter::in, counter::out)
	is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module backend_libs__pseudo_type_info, backend_libs__rtti.
:- import_module std_util, list, bool, int.

ll_pseudo_type_info__construct_llds_pseudo_type_info(Type, NumUnivQTvars,
		ExistQTvars, Pseudo, C0, C) :-
	ll_pseudo_type_info__construct_typed_llds_pseudo_type_info(Type,
		NumUnivQTvars, ExistQTvars, Pseudo, _LldsType, C0, C).

ll_pseudo_type_info__construct_typed_llds_pseudo_type_info(Type, NumUnivQTvars,
		ExistQTvars, PseudoRval, LldsType, C0, C) :-
	pseudo_type_info__construct_pseudo_type_info(Type, NumUnivQTvars,
			ExistQTvars, Pseudo),
	convert_pseudo_type_info(Pseudo, PseudoRval, LldsType, C0, C).

:- pred convert_pseudo_type_info(rtti_pseudo_type_info::in,
	rval::out, llds_type::out, counter::in, counter::out) is det.

convert_pseudo_type_info(Pseudo, Rval, LldsType, C0, C) :-
	(
		Pseudo = type_var(Int),
		Rval = const(int_const(Int)),
		LldsType = integer,
		C = C0
	;
		Pseudo = plain_arity_zero_pseudo_type_info(RttiTypeCtor),
		DataAddr = rtti_addr(RttiTypeCtor, pseudo_type_info(Pseudo)),
		Rval = const(data_addr_const(DataAddr)),
		LldsType = data_ptr,
		C = C0
	;
		Pseudo = plain_pseudo_type_info(RttiTypeCtor, Args),
		convert_compound_pseudo_type_info(RttiTypeCtor, [], Args,
			Rval, LldsType, C0, C)
	;
		Pseudo = var_arity_pseudo_type_info(VarArityId, Args),
		list__length(Args, Arity),
		ArityArg = yes(const(int_const(Arity))),
		RttiTypeCtor = var_arity_id_to_rtti_type_ctor(VarArityId),
		convert_compound_pseudo_type_info(RttiTypeCtor, [ArityArg],
			Args, Rval, LldsType, C0, C)
	).

:- pred convert_plain_type_info(rtti_type_info::in,
	rval::out, llds_type::out, counter::in, counter::out) is det.

convert_plain_type_info(TypeInfo, Rval, LldsType, C0, C) :-
	(
		TypeInfo = plain_arity_zero_type_info(RttiTypeCtor),
		DataAddr = rtti_addr(RttiTypeCtor, type_info(TypeInfo)),
		Rval = const(data_addr_const(DataAddr)),
		LldsType = data_ptr,
		C = C0
	;
		TypeInfo = plain_type_info(RttiTypeCtor, Args),
		convert_compound_type_info(RttiTypeCtor, [], Args,
			Rval, LldsType, C0, C)
	;
		TypeInfo = var_arity_type_info(VarArityId, Args),
		list__length(Args, Arity),
		ArityArg = yes(const(int_const(Arity))),
		RttiTypeCtor = var_arity_id_to_rtti_type_ctor(VarArityId),
		convert_compound_type_info(RttiTypeCtor, [ArityArg],
			Args, Rval, LldsType, C0, C)
	).

:- pred convert_compound_pseudo_type_info(rtti_type_ctor::in,
	list(maybe(rval))::in, list(rtti_maybe_pseudo_type_info)::in,
	rval::out, llds_type::out, counter::in, counter::out) is det.

convert_compound_pseudo_type_info(RttiTypeCtor, ArgRvals0, Args,
		Rval, LldsType, C0, C) :-
	TypeCtorInfoData = pseudo_type_info(
		plain_arity_zero_pseudo_type_info(RttiTypeCtor)),
	TypeCtorInfoDataAddr = rtti_addr(RttiTypeCtor, TypeCtorInfoData),
	TypeCtorInfoRval = yes(const(data_addr_const(TypeCtorInfoDataAddr))),
	LldsType = data_ptr,
	counter__allocate(CNum, C0, C1),
	list__map_foldl((pred(A::in, yes(AR)::out, CS0::in, CS::out) is det :-
		(
			A = pseudo(PTI),
			convert_pseudo_type_info(PTI, AR, _LldsType, CS0, CS)
		;
			A = plain(TI),
			convert_plain_type_info(TI, AR, _LldsType, CS0, CS)
		)
	), Args, ArgRvals1, C1, C),
	list__append(ArgRvals0, ArgRvals1, ArgRvals),
	Reuse = no,
	Rval = create(0, [TypeCtorInfoRval | ArgRvals],
		uniform(no), must_be_static, CNum, "type_info", Reuse).

:- pred convert_compound_type_info(rtti_type_ctor::in, list(maybe(rval))::in,
	list(rtti_type_info)::in, rval::out, llds_type::out,
	counter::in, counter::out) is det.

convert_compound_type_info(RttiTypeCtor, ArgRvals0, Args,
		Rval, LldsType, C0, C) :-
	TypeCtorInfoData = type_info(plain_arity_zero_type_info(RttiTypeCtor)),
	TypeCtorInfoDataAddr = rtti_addr(RttiTypeCtor, TypeCtorInfoData),
	TypeCtorInfoRval = yes(const(data_addr_const(TypeCtorInfoDataAddr))),
	LldsType = data_ptr,
	counter__allocate(CNum, C0, C1),
	list__map_foldl((pred(A::in, yes(AR)::out, CS0::in, CS::out) is det :-
		convert_plain_type_info(A, AR, _LldsType, CS0, CS)
	), Args, ArgRvals1, C1, C),
	list__append(ArgRvals0, ArgRvals1, ArgRvals),
	Reuse = no,
	Rval = create(0, [TypeCtorInfoRval | ArgRvals],
		uniform(no), must_be_static, CNum, "type_info", Reuse).
