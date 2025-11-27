%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019-2021, 2024-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: type_inst_mode_map.m.
% Main author: zs.
%
% This module provides ways to convert lists of type, inst, and mode
% definitions into maps from type_ctors, inst_ctors and mode_ctors to the
% list of definitions for a given ctor. It also provides ways to reverse
% this conversion.
%
%---------------------------------------------------------------------------%

:- module parse_tree.type_inst_mode_map.
:- interface.

:- import_module parse_tree.prog_item.

:- import_module list.

%---------------------------------------------------------------------------%

    % Functions for gathering up lists of particular kinds of items
    % and gathering all items for a given type_ctor (or its inst
    % or mode equivalent) together. These functions provide the inputs
    % for the operations in check_type_inst_mode_defns.m.
    %
:- func type_ctor_defn_items_to_map(list(item_type_defn_info))
    = type_ctor_defn_map.
:- func inst_ctor_defn_items_to_map(list(item_inst_defn_info))
    = inst_ctor_defn_map.
:- func mode_ctor_defn_items_to_map(list(item_mode_defn_info))
    = mode_ctor_defn_map.
:- func type_ctor_repn_items_to_map(list(item_type_repn_info))
    = type_ctor_repn_map.
:- func type_ctor_foreign_enum_items_to_map(list(item_foreign_enum_info))
    = type_ctor_foreign_enum_map.

%---------------------------------------------------------------------------%

    % Functions to reverse the above conversion. We use these when
    % we write out interface files.
    %
    % The reason there is no function for foreign_enums is that
    % foreign_enums are not allowed in module interfaces, and therefore
    % they are never put into interface files.
    %
:- func type_ctor_defn_map_to_type_defns(type_ctor_defn_map)
    = list(item_type_defn_info).
:- func inst_ctor_defn_map_to_inst_defns(inst_ctor_defn_map)
    = list(item_inst_defn_info).
:- func mode_ctor_defn_map_to_mode_defns(mode_ctor_defn_map)
    = list(item_mode_defn_info).
:- func type_ctor_repn_map_to_type_repns(type_ctor_repn_map)
    = list(item_type_repn_info).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.item_util.
:- import_module parse_tree.prog_data.

:- import_module cord.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module pair.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

type_ctor_defn_items_to_map(TypeDefnInfos) = TypeDefnMap :-
    list.foldl(add_type_defn_to_map, TypeDefnInfos, map.init, TypeDefnMap).

:- pred add_type_defn_to_map(item_type_defn_info::in,
    type_ctor_defn_map::in, type_ctor_defn_map::out) is det.

add_type_defn_to_map(TypeDefnInfo, !TypeDefnMap) :-
    TypeDefnInfo = item_type_defn_info(SymName, Params, TypeDefn,
        _TypeVarSet, _Context, _SeqNum),
    list.length(Params, Arity),
    TypeCtor = type_ctor(SymName, Arity),
    some [!AbsSolverDefns, !SolverDefns,
        !AbsStdDefns, !EqvDefns, !DuDefns, !SubDefns,
        !ForeignDefnsC, !ForeignDefnsJava, !ForeignDefnsCsharp]
    (
        ( if map.search(!.TypeDefnMap, TypeCtor, AllDefns0) then
            AllDefns0 = type_ctor_all_defns(
                !:AbsSolverDefns, !:SolverDefns,
                !:AbsStdDefns, !:EqvDefns, !:DuDefns, !:SubDefns,
                c_java_csharp(!:ForeignDefnsC, !:ForeignDefnsJava,
                    !:ForeignDefnsCsharp))
        else
            !:AbsSolverDefns = [],
            !:SolverDefns = [],

            !:AbsStdDefns = [],
            !:EqvDefns = [],
            !:DuDefns = [],
            !:SubDefns = [],
            !:ForeignDefnsC = [],
            !:ForeignDefnsJava = [],
            !:ForeignDefnsCsharp = []
        ),
        (
            TypeDefn = parse_tree_abstract_type(DetailsAbs),
            AbsDefnInfo = TypeDefnInfo ^ td_ctor_defn := DetailsAbs,
            (
                DetailsAbs = abstract_solver_type,
                !:AbsSolverDefns = !.AbsSolverDefns ++ [AbsDefnInfo]
            ;
                ( DetailsAbs = abstract_type_general
                ; DetailsAbs = abstract_type_fits_in_n_bits(_)
                ; DetailsAbs = abstract_dummy_type
                ; DetailsAbs = abstract_notag_type
                ; DetailsAbs = abstract_subtype(_)
                ),
                !:AbsStdDefns = !.AbsStdDefns ++ [AbsDefnInfo]
            )
        ;
            TypeDefn = parse_tree_solver_type(DetailsSolver),
            SolverDefnInfo = TypeDefnInfo ^ td_ctor_defn := DetailsSolver,
            !:SolverDefns = !.SolverDefns ++ [SolverDefnInfo]
        ;
            TypeDefn = parse_tree_eqv_type(DetailsEqv),
            EqvDefnInfo = TypeDefnInfo ^ td_ctor_defn := DetailsEqv,
            !:EqvDefns = !.EqvDefns ++ [EqvDefnInfo]
        ;
            TypeDefn = parse_tree_du_type(DetailsDu),
            DuDefnInfo = TypeDefnInfo ^ td_ctor_defn := DetailsDu,
            !:DuDefns = !.DuDefns ++ [DuDefnInfo]
        ;
            TypeDefn = parse_tree_sub_type(DetailsSub),
            SubDefnInfo = TypeDefnInfo ^ td_ctor_defn := DetailsSub,
            !:SubDefns = !.SubDefns ++ [SubDefnInfo]
        ;
            TypeDefn = parse_tree_foreign_type(DetailsForeign),
            ForeignDefnInfo = TypeDefnInfo ^ td_ctor_defn := DetailsForeign,
            DetailsForeign = type_details_foreign(LangType, _, _),
            (
                LangType = c(_),
                !:ForeignDefnsC = !.ForeignDefnsC ++ [ForeignDefnInfo]
            ;
                LangType = java(_),
                !:ForeignDefnsJava = !.ForeignDefnsJava ++ [ForeignDefnInfo]
            ;
                LangType = csharp(_),
                !:ForeignDefnsCsharp = !.ForeignDefnsCsharp ++
                    [ForeignDefnInfo]
            )
        ),
        AllDefns = type_ctor_all_defns(!.AbsSolverDefns, !.SolverDefns,
            !.AbsStdDefns, !.EqvDefns, !.DuDefns, !.SubDefns,
            c_java_csharp(!.ForeignDefnsC, !.ForeignDefnsJava,
                !.ForeignDefnsCsharp))
    ),
    map.set(TypeCtor, AllDefns, !TypeDefnMap).

%---------------------%

inst_ctor_defn_items_to_map(InstDefnInfos) = InstDefnMap :-
    list.foldl(add_inst_defn_to_map, InstDefnInfos, map.init, InstDefnMap).

:- pred add_inst_defn_to_map(item_inst_defn_info::in,
    inst_ctor_defn_map::in, inst_ctor_defn_map::out) is det.

add_inst_defn_to_map(InstDefnInfo, !InstDefnMap) :-
    InstDefnInfo = item_inst_defn_info(SymName, Params, _MaybeForTypeCtor,
        MaybeAbstractInstDefn, _InstVarSet, _Context, _SeqNum),
    list.length(Params, Arity),
    InstCtor = inst_ctor(SymName, Arity),
    ( if map.search(!.InstDefnMap, InstCtor, AllDefns0) then
        AllDefns0 = inst_ctor_all_defns(AbstractDefns0, EqvDefns0),
        (
            MaybeAbstractInstDefn = abstract_inst_defn,
            AbstractDefn = InstDefnInfo ^ id_inst_defn := no_inst_defn,
            AbstractDefns = [AbstractDefn | AbstractDefns0],
            AllDefns = inst_ctor_all_defns(AbstractDefns, EqvDefns0)
        ;
            MaybeAbstractInstDefn = nonabstract_inst_defn(InstDefn),
            EqvDefn = InstDefnInfo ^ id_inst_defn := InstDefn,
            EqvDefns = [EqvDefn | EqvDefns0],
            AllDefns = inst_ctor_all_defns(AbstractDefns0, EqvDefns)
        ),
        map.det_update(InstCtor, AllDefns, !InstDefnMap)
    else
        (
            MaybeAbstractInstDefn = abstract_inst_defn,
            AbstractDefn = InstDefnInfo ^ id_inst_defn := no_inst_defn,
            AllDefns = inst_ctor_all_defns([AbstractDefn], [])
        ;
            MaybeAbstractInstDefn = nonabstract_inst_defn(InstDefn),
            EqvDefn = InstDefnInfo ^ id_inst_defn := InstDefn,
            AllDefns = inst_ctor_all_defns([], [EqvDefn])
        ),
        map.det_insert(InstCtor, AllDefns, !InstDefnMap)
    ).

%---------------------%

mode_ctor_defn_items_to_map(ModeDefnInfos) = ModeDefnMap :-
    list.foldl(add_mode_defn_to_map, ModeDefnInfos, map.init, ModeDefnMap).

:- pred add_mode_defn_to_map(item_mode_defn_info::in,
    mode_ctor_defn_map::in, mode_ctor_defn_map::out) is det.

add_mode_defn_to_map(ModeDefnInfo, !ModeDefnMap) :-
    ModeDefnInfo = item_mode_defn_info(SymName, Params, MaybeAbstractModeDefn,
        _InstVarSet, _Context, _SeqNum),
    list.length(Params, Arity),
    ModeCtor = mode_ctor(SymName, Arity),
    ( if map.search(!.ModeDefnMap, ModeCtor, AllDefns0) then
        AllDefns0 = mode_ctor_all_defns(AbstractDefns0, EqvDefns0),
        (
            MaybeAbstractModeDefn = abstract_mode_defn,
            AbstractDefn = ModeDefnInfo ^ md_mode_defn := no_mode_defn,
            AbstractDefns = [AbstractDefn | AbstractDefns0],
            AllDefns = mode_ctor_all_defns(AbstractDefns, EqvDefns0)
        ;
            MaybeAbstractModeDefn = nonabstract_mode_defn(ModeDefn),
            EqvDefn = ModeDefnInfo ^ md_mode_defn := ModeDefn,
            EqvDefns = [EqvDefn | EqvDefns0],
            AllDefns = mode_ctor_all_defns(AbstractDefns0, EqvDefns)
        ),
        map.det_update(ModeCtor, AllDefns, !ModeDefnMap)
    else
        (
            MaybeAbstractModeDefn = abstract_mode_defn,
            AbstractDefn = ModeDefnInfo ^ md_mode_defn := no_mode_defn,
            AllDefns = mode_ctor_all_defns([AbstractDefn], [])
        ;
            MaybeAbstractModeDefn = nonabstract_mode_defn(ModeDefn),
            EqvDefn = ModeDefnInfo ^ md_mode_defn := ModeDefn,
            AllDefns = mode_ctor_all_defns([], [EqvDefn])
        ),
        map.det_insert(ModeCtor, AllDefns, !ModeDefnMap)
    ).

%---------------------%

type_ctor_repn_items_to_map(TypeRepnInfos) = TypeRepnMap :-
    list.foldl(add_type_repn_to_map, TypeRepnInfos, map.init, TypeRepnMap).

:- pred add_type_repn_to_map(item_type_repn_info::in,
    type_ctor_repn_map::in, type_ctor_repn_map::out) is det.

add_type_repn_to_map(TypeRepnInfo, !TypeRepnMap) :-
    TypeRepnInfo = item_type_repn_info(SymName, Params, _TypeRepn,
        _TypeVarSet, _Context, _SeqNum),
    list.length(Params, Arity),
    TypeCtor = type_ctor(SymName, Arity),
    % This could be a map.det_insert, because
    %
    % - we ensure the invariant that an interface file contains at most one
    %   type_repn item for a given type_ctor, and
    %
    % - source files should not contain any type_ctor items, because they are
    %   not a publicly documented part of the language.
    %
    % However, until we have code to filter out all type_repn items from
    % source files, they may contain duplicates, so we keep this as map.set
    % for now.
    map.set(TypeCtor, TypeRepnInfo, !TypeRepnMap).

%---------------------%

type_ctor_foreign_enum_items_to_map(ForeignEnums) = ForeignEnumMap :-
    list.foldl(add_foreign_enum_item_to_map, ForeignEnums,
        map.init, ForeignEnumMap).

:- pred add_foreign_enum_item_to_map(item_foreign_enum_info::in,
    type_ctor_foreign_enum_map::in, type_ctor_foreign_enum_map::out) is det.

add_foreign_enum_item_to_map(ForeignEnumInfo, !ForeignEnumMap) :-
    ForeignEnumInfo = item_foreign_enum_info(Lang, TypeCtor, _Values, _, _),
    some [!ForeignEnumsC, !ForeignEnumsJava, !ForeignEnumsCsharp]
    (
        ( if map.search(!.ForeignEnumMap, TypeCtor, AllEnums0) then
            AllEnums0 = c_java_csharp(!:ForeignEnumsC,
                !:ForeignEnumsJava, !:ForeignEnumsCsharp)
        else
            !:ForeignEnumsC = [],
            !:ForeignEnumsJava = [],
            !:ForeignEnumsCsharp = []
        ),
        (
            Lang = lang_c,
            !:ForeignEnumsC = !.ForeignEnumsC ++ [ForeignEnumInfo]
        ;
            Lang = lang_java,
            !:ForeignEnumsJava = !.ForeignEnumsJava ++ [ForeignEnumInfo]
        ;
            Lang = lang_csharp,
            !:ForeignEnumsCsharp = !.ForeignEnumsCsharp ++ [ForeignEnumInfo]
        ),
        AllEnums = c_java_csharp(!.ForeignEnumsC,
            !.ForeignEnumsJava, !.ForeignEnumsCsharp),
        map.set(TypeCtor, AllEnums, !ForeignEnumMap)
    ).

%---------------------------------------------------------------------------%

type_ctor_defn_map_to_type_defns(TypeCtorDefnMap) = TypeDefns :-
    map.foldl_values(accumulate_type_ctor_defns, TypeCtorDefnMap,
        cord.init, TypeDefnsCord),
    TypeDefns = cord.list(TypeDefnsCord).

:- pred accumulate_type_ctor_defns(type_ctor_all_defns::in,
    cord(item_type_defn_info)::in, cord(item_type_defn_info)::out) is det.

accumulate_type_ctor_defns(CtorAllDefns, !TypeDefns) :-
    CtorAllDefns = type_ctor_all_defns(AbstractSolverDefns, SolverDefns,
        AbstractStdDefns, EqvDefns, DuDefns, SubDefns, CJCsEDefns),
    CJCsEDefns = c_java_csharp(ForeignDefnsC, ForeignDefnsJava,
        ForeignDefnsCsharp),
    !:TypeDefns = !.TypeDefns ++ cord.from_list(
        list.map(wrap_abstract_type_defn, at_most_one(AbstractSolverDefns)) ++
        list.map(wrap_solver_type_defn, SolverDefns) ++
        list.map(wrap_abstract_type_defn, at_most_one(AbstractStdDefns)) ++
        list.map(wrap_eqv_type_defn, EqvDefns) ++
        list.map(wrap_du_type_defn, DuDefns) ++
        list.map(wrap_sub_type_defn, SubDefns) ++
        list.map(wrap_foreign_type_defn, ForeignDefnsC) ++
        list.map(wrap_foreign_type_defn, ForeignDefnsJava) ++
        list.map(wrap_foreign_type_defn, ForeignDefnsCsharp)).

:- func at_most_one(list(T)) = list(T).

at_most_one([]) = [].
at_most_one([X | _Xs]) = [X].

%---------------------%

inst_ctor_defn_map_to_inst_defns(InstCtorDefnMap) = InstDefns :-
    map.foldl_values(accumulate_inst_ctor_defns, InstCtorDefnMap,
        cord.init, InstDefnsCord),
    InstDefns = cord.list(InstDefnsCord).

:- pred accumulate_inst_ctor_defns(inst_ctor_all_defns::in,
    cord(item_inst_defn_info)::in, cord(item_inst_defn_info)::out) is det.

accumulate_inst_ctor_defns(CtorAllDefns, !InstDefns) :-
    CtorAllDefns = inst_ctor_all_defns(AbstractDefns, EqvDefns),
    !:InstDefns = !.InstDefns ++
        cord.from_list(list.map(wrap_abstract_inst_defn, AbstractDefns)) ++
        cord.from_list(list.map(wrap_eqv_inst_defn, EqvDefns)).

%---------------------%

mode_ctor_defn_map_to_mode_defns(ModeCtorDefnMap) = ModeDefns :-
    map.foldl_values(accumulate_mode_ctor_defns, ModeCtorDefnMap,
        cord.init, ModeDefnsCord),
    ModeDefns = cord.list(ModeDefnsCord).

:- pred accumulate_mode_ctor_defns(mode_ctor_all_defns::in,
    cord(item_mode_defn_info)::in, cord(item_mode_defn_info)::out) is det.

accumulate_mode_ctor_defns(CtorAllDefns, !ModeDefns) :-
    CtorAllDefns = mode_ctor_all_defns(AbstractDefns, EqvDefns),
    !:ModeDefns = !.ModeDefns ++
        cord.from_list(list.map(wrap_abstract_mode_defn, AbstractDefns)) ++
        cord.from_list(list.map(wrap_eqv_mode_defn, EqvDefns)).

%---------------------%

type_ctor_repn_map_to_type_repns(TypeCtorRepnMap) = TypeRepns :-
    map.foldl_values(accumulate_type_ctor_repns, TypeCtorRepnMap,
        cord.init, TypeRepnsCord),
    TypeRepns = cord.list(TypeRepnsCord).

:- pred accumulate_type_ctor_repns(item_type_repn_info::in,
    cord(item_type_repn_info)::in, cord(item_type_repn_info)::out) is det.

accumulate_type_ctor_repns(TypeRepn, !TypeRepns) :-
    !:TypeRepns = cord.snoc(!.TypeRepns, TypeRepn).

%---------------------------------------------------------------------------%
:- end_module parse_tree.type_inst_mode_map.
%---------------------------------------------------------------------------%
