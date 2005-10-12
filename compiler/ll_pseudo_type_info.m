%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000,2002-2003, 2005 The University of Melbourne.
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

:- import_module ll_backend__global_data.
:- import_module ll_backend__llds.
:- import_module parse_tree__prog_data.

    % construct_typed_pseudo_type_info(Type, NumUnivQTvars, ExistQVars,
    %   Rval, LldsType):
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
:- pred construct_typed_llds_pseudo_type_info((type)::in, int::in,
    existq_tvars::in, static_cell_info::in, static_cell_info::out,
    rval::out, llds_type::out) is det.

    % This is the same as the previous predicate, but does not return
    % the LLDS type.
    %
:- pred construct_llds_pseudo_type_info((type)::in, int::in, existq_tvars::in,
    static_cell_info::in, static_cell_info::out, rval::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs__pseudo_type_info.
:- import_module backend_libs__rtti.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module std_util.

construct_llds_pseudo_type_info(Type, NumUnivQTvars, ExistQTvars,
        !StaticCellInfo, Pseudo) :-
    construct_typed_llds_pseudo_type_info(Type,
        NumUnivQTvars, ExistQTvars, !StaticCellInfo, Pseudo, _LldsType).

construct_typed_llds_pseudo_type_info(Type, NumUnivQTvars, ExistQTvars,
        !StaticCellInfo, PseudoRval, LldsType) :-
    pseudo_type_info__construct_pseudo_type_info(Type, NumUnivQTvars,
        ExistQTvars, Pseudo),
    convert_pseudo_type_info(Pseudo, !StaticCellInfo, PseudoRval, LldsType).

:- pred convert_pseudo_type_info(rtti_pseudo_type_info::in,
    static_cell_info::in, static_cell_info::out,
    rval::out, llds_type::out) is det.

convert_pseudo_type_info(Pseudo, !StaticCellInfo, Rval, LldsType) :-
    (
        Pseudo = type_var(Int),
        Rval = const(int_const(Int)),
        LldsType = integer
    ;
        Pseudo = plain_arity_zero_pseudo_type_info(RttiTypeCtor),
        DataAddr = rtti_addr(
            ctor_rtti_id(RttiTypeCtor, pseudo_type_info(Pseudo))),
        Rval = const(data_addr_const(DataAddr, no)),
        LldsType = data_ptr
    ;
        Pseudo = plain_pseudo_type_info(RttiTypeCtor, Args),
        convert_compound_pseudo_type_info(RttiTypeCtor, [], Args,
            !StaticCellInfo, Rval, LldsType)
    ;
        Pseudo = var_arity_pseudo_type_info(VarArityId, Args),
        list__length(Args, Arity),
        ArityArg = const(int_const(Arity)),
        RttiTypeCtor = var_arity_id_to_rtti_type_ctor(VarArityId),
        convert_compound_pseudo_type_info(RttiTypeCtor, [ArityArg],
            Args, !StaticCellInfo, Rval, LldsType)
    ).

:- pred convert_plain_type_info(rtti_type_info::in,
    static_cell_info::in, static_cell_info::out,
    rval::out, llds_type::out) is det.

convert_plain_type_info(TypeInfo, !StaticCellInfo, Rval, LldsType) :-
    (
        TypeInfo = plain_arity_zero_type_info(RttiTypeCtor),
        DataAddr = rtti_addr(ctor_rtti_id(RttiTypeCtor, type_info(TypeInfo))),
        Rval = const(data_addr_const(DataAddr, no)),
        LldsType = data_ptr
    ;
        TypeInfo = plain_type_info(RttiTypeCtor, Args),
        convert_compound_type_info(RttiTypeCtor, [], Args,
            !StaticCellInfo, Rval, LldsType)
    ;
        TypeInfo = var_arity_type_info(VarArityId, Args),
        list__length(Args, Arity),
        ArityArg = const(int_const(Arity)),
        RttiTypeCtor = var_arity_id_to_rtti_type_ctor(VarArityId),
        convert_compound_type_info(RttiTypeCtor, [ArityArg], Args,
            !StaticCellInfo, Rval, LldsType)
    ).

:- pred convert_compound_pseudo_type_info(rtti_type_ctor::in,
    list(rval)::in, list(rtti_maybe_pseudo_type_info)::in,
    static_cell_info::in, static_cell_info::out,
    rval::out, llds_type::out) is det.

convert_compound_pseudo_type_info(RttiTypeCtor, ArgRvals0, Args,
        !StaticCellInfo, Rval, LldsType) :-
    TypeCtorInfoDataAddr = rtti_addr(
        ctor_rtti_id(RttiTypeCtor, type_ctor_info)),
    TypeCtorInfoRval = const(data_addr_const(TypeCtorInfoDataAddr, no)),
    LldsType = data_ptr,
    list__map_foldl((pred(A::in, AR::out, SCI0::in, SCI::out) is det :-
        (
            A = pseudo(PTI),
            convert_pseudo_type_info(PTI, SCI0, SCI, AR, _LldsType)
        ;
            A = plain(TI),
            convert_plain_type_info(TI, SCI0, SCI, AR, _LldsType)
        )
    ), Args, ArgRvals1, !StaticCellInfo),
    list__append(ArgRvals0, ArgRvals1, ArgRvals),
    add_static_cell_natural_types([TypeCtorInfoRval | ArgRvals], DataAddr,
        !StaticCellInfo),
    Rval = const(data_addr_const(DataAddr, no)).

:- pred convert_compound_type_info(rtti_type_ctor::in, list(rval)::in,
    list(rtti_type_info)::in, static_cell_info::in, static_cell_info::out,
    rval::out, llds_type::out) is det.

convert_compound_type_info(RttiTypeCtor, ArgRvals0, Args, !StaticCellInfo,
        Rval, LldsType) :-
    TypeCtorInfoData = type_info(plain_arity_zero_type_info(RttiTypeCtor)),
    TypeCtorInfoDataAddr = rtti_addr(
        ctor_rtti_id(RttiTypeCtor, TypeCtorInfoData)),
    TypeCtorInfoRval = const(data_addr_const(TypeCtorInfoDataAddr, no)),
    LldsType = data_ptr,
    list__map_foldl((pred(A::in, AR::out, SCI0::in, SCI::out) is det :-
        convert_plain_type_info(A, SCI0, SCI, AR, _LldsType)
    ), Args, ArgRvals1, !StaticCellInfo),
    list__append(ArgRvals0, ArgRvals1, ArgRvals),
    add_static_cell_natural_types([TypeCtorInfoRval | ArgRvals], DataAddr,
        !StaticCellInfo),
    Rval = const(data_addr_const(DataAddr, no)).
