%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000,2002-2003, 2005-2006, 2009 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: ll_pseudo_type_info.m.
% Author: fjh.
%
% This module generates LLDS representations for pseudo-type-infos.
%
% Most of the work is done by pseudo_type_info.m, which generates a
% back-end-independent representation of pseudo-type-infos; this module just
% converts that representation to LLDS.
%
% The documentation of the structures of pseudo-type-infos is in
% runtime/mercury_type_info.h; that file also contains a list of all the files
% that depend on such data structures.
%
%-----------------------------------------------------------------------------%

:- module ll_backend.ll_pseudo_type_info.
:- interface.

:- import_module ll_backend.global_data.
:- import_module ll_backend.llds.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

%---------------------------------------------------------------------------%

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
:- pred construct_typed_llds_pseudo_type_info(mer_type::in, int::in,
    existq_tvars::in, static_cell_info::in, static_cell_info::out,
    rval::out, llds_type::out) is det.

    % This is the same as the previous predicate, but does not return
    % the LLDS type.
    %
:- pred construct_llds_pseudo_type_info(mer_type::in, int::in,
    existq_tvars::in, static_cell_info::in, static_cell_info::out,
    rval::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.pseudo_type_info.
:- import_module backend_libs.rtti.

:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

construct_typed_llds_pseudo_type_info(Type, NumUnivQTvars, ExistQTvars,
        !StaticCellInfo, PseudoRval, LldsType) :-
    pseudo_type_info.construct_pseudo_type_info(Type, NumUnivQTvars,
        ExistQTvars, Pseudo),
    convert_pseudo_type_info(Pseudo, !StaticCellInfo, PseudoRval, LldsType).

construct_llds_pseudo_type_info(Type, NumUnivQTvars, ExistQTvars,
        !StaticCellInfo, Pseudo) :-
    construct_typed_llds_pseudo_type_info(Type,
        NumUnivQTvars, ExistQTvars, !StaticCellInfo, Pseudo, _LldsType).

:- pred convert_pseudo_type_info(rtti_pseudo_type_info::in,
    static_cell_info::in, static_cell_info::out,
    rval::out, llds_type::out) is det.

convert_pseudo_type_info(Pseudo, !StaticCellInfo, Rval, LldsType) :-
    (
        Pseudo = type_var(Int),
        Rval = const(llconst_int(Int)),
        LldsType = lt_int(int_type_int)
    ;
        Pseudo = plain_arity_zero_pseudo_type_info(RttiTypeCtor),
        DataId = rtti_data_id(
            ctor_rtti_id(RttiTypeCtor, type_ctor_pseudo_type_info(Pseudo))),
        Rval = const(llconst_data_addr(DataId, no)),
        LldsType = lt_data_ptr
    ;
        Pseudo = plain_pseudo_type_info(RttiTypeCtor, Args),
        convert_compound_pseudo_type_info(RttiTypeCtor, [], Args,
            !StaticCellInfo, Rval, LldsType)
    ;
        Pseudo = var_arity_pseudo_type_info(VarArityId, Args),
        list.length(Args, Arity),
        ArityArg = const(llconst_int(Arity)),
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
        DataId = rtti_data_id(ctor_rtti_id(RttiTypeCtor,
            type_ctor_type_info(TypeInfo))),
        Rval = const(llconst_data_addr(DataId, no)),
        LldsType = lt_data_ptr
    ;
        TypeInfo = plain_type_info(RttiTypeCtor, Args),
        convert_compound_type_info(RttiTypeCtor, [], Args,
            !StaticCellInfo, Rval, LldsType)
    ;
        TypeInfo = var_arity_type_info(VarArityId, Args),
        list.length(Args, Arity),
        ArityArg = const(llconst_int(Arity)),
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
    TypeCtorInfoDataId = rtti_data_id(
        ctor_rtti_id(RttiTypeCtor, type_ctor_type_ctor_info)),
    TypeCtorInfoRval = const(llconst_data_addr(TypeCtorInfoDataId, no)),
    LldsType = lt_data_ptr,
    list.map_foldl((pred(A::in, AR::out, SCI0::in, SCI::out) is det :-
        (
            A = pseudo(PTI),
            convert_pseudo_type_info(PTI, SCI0, SCI, AR, _LldsType)
        ;
            A = plain(TI),
            convert_plain_type_info(TI, SCI0, SCI, AR, _LldsType)
        )
    ), Args, ArgRvals1, !StaticCellInfo),
    list.append(ArgRvals0, ArgRvals1, ArgRvals),
    add_scalar_static_cell_natural_types([TypeCtorInfoRval | ArgRvals],
        DataId, !StaticCellInfo),
    Rval = const(llconst_data_addr(DataId, no)).

:- pred convert_compound_type_info(rtti_type_ctor::in, list(rval)::in,
    list(rtti_type_info)::in, static_cell_info::in, static_cell_info::out,
    rval::out, llds_type::out) is det.

convert_compound_type_info(RttiTypeCtor, ArgRvals0, Args, !StaticCellInfo,
        Rval, LldsType) :-
    TypeCtorInfoData = type_ctor_type_info(
        plain_arity_zero_type_info(RttiTypeCtor)),
    TypeCtorInfoDataId = rtti_data_id(
        ctor_rtti_id(RttiTypeCtor, TypeCtorInfoData)),
    TypeCtorInfoRval = const(llconst_data_addr(TypeCtorInfoDataId, no)),
    LldsType = lt_data_ptr,
    list.map_foldl((pred(A::in, AR::out, SCI0::in, SCI::out) is det :-
        convert_plain_type_info(A, SCI0, SCI, AR, _LldsType)
    ), Args, ArgRvals1, !StaticCellInfo),
    list.append(ArgRvals0, ArgRvals1, ArgRvals),
    add_scalar_static_cell_natural_types([TypeCtorInfoRval | ArgRvals],
        DataId, !StaticCellInfo),
    Rval = const(llconst_data_addr(DataId, no)).

%-----------------------------------------------------------------------------%
:- end_module ll_backend.ll_pseudo_type_info.
%-----------------------------------------------------------------------------%
