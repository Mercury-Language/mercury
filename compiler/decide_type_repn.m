%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: decide_type_repn.m.
% Main author: zs.
%
% The task of this module is decide the representation of each type
% and to generate type_representation items recording those decisions
% for inclusion in automatically generated interface files.
%
% We decide the representation of simple types (direct dummy types,
% enum types, and notag types) when generating .int3 files.
% The intention is that we will decide the representation of
% all other types when generating .int/.int2 files.
%
% The task of this module differs from that of the (current) du_type_layout.m
% not just in that it does its work earlier, on the parse tree instead of
% on the HLDS, but also in that the information it puts into interface files
% must be grade-independent.
%
%---------------------------------------------------------------------------%
%
% Overall description of the task of this module when creating the different
% kinds of interface files, in terms of inputs and outputs:
%
% .int3 now:
%   output: type_repns for simple types in interface and all? eqv types
%   input:  type_defns in source module
%
% .int2 now:
%   output: type_repns for simple types in interface and all? eqv types
%   input:  type_defns in source module
%
% .int2 later:
%   output: type_repns for simple types in interface and all? eqv types
%   input:  type_defns in source module
%   input:  type_repns for simple/eqv types in direct/indirect imported .int3s
%
% .int1 later:
%   output: type_repns for all types that appear in interface
%   input:  type_defns in source module
%   input:  type_repns for simple/eqv types in direct/indirect imported .int3s
%
% .int0 later:
%   output: type_repns for all types
%   input:  type_defns in source module
%   input:  type_repns for simple/eqv types in direct/indirect imported .int3s
%
%---------------------------------------------------------------------------%
%
% XXX TYPE_REPN Including type_representation items in the interface
% for *all* equivalence type definitions in the module, even the ones
% that appear in implementation sections, is an overapproximation.
% However, including type_representation items for *none* of the
% type equivalence definitions in implementation sections may be
% an *under*estimate. Consider this setup:
%
%   :- interface.
%   :- type t1 == t2.
%   :- implementation.
%   :- type t2 == int8.
%
% Knowing whether t1's representation is sub-word-sized requires access
% to the type representation item derived from the private definition of t2
% as well as the one derived from the exported definition of t1.
%
% Exactly what subset of .int, .int2 and .int3 files this information
% should be included is a question for later work. For now, this overestimate
% is the most extensive possible test of the predicates that print out
% and read in type representation items.
%
%---------------------------------------------------------------------------%

:- module parse_tree.decide_type_repn.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.check_parse_tree_type_defns.
:- import_module parse_tree.prog_item.

:- import_module list.

    % decide_repns_for_simple_types_for_int3(ModuleName, TypeCtorCheckedMap,
    %   IntTypeRepnItems):
    %
    % Given TypeCtorCheckedMap's comprehensive picture of the
    % type definitions in the module,
    %
    % - figure out which type definitions define exported simple types,
    % - decide their representations, and
    % - generate items recording those decisions, for types that appear
    %   in the interfac
    %
:- pred decide_repns_for_simple_types_for_int3(module_name::in,
    type_ctor_checked_map::in, list(item_type_repn_info)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.prog_data.

:- import_module cord.
:- import_module map.
:- import_module maybe.
:- import_module set_tree234.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Make decisions about simple types.
%

decide_repns_for_simple_types_for_int3(_ModuleName, TypeCtorCheckedMap,
        IntRepns) :-
    map.init(RepnMap0),
    map.init(EqvMap0),
    map.init(WordAlignedMap0),
    ExportedTypesSet0 = set_tree234.init,
    map.foldl4(decide_type_repns_stage_1, TypeCtorCheckedMap,
        RepnMap0, RepnMap, EqvMap0, EqvMap,
        WordAlignedMap0, WordAlignedMap, ExportedTypesSet0, ExportedTypesSet),

    set_tree234.to_sorted_list(ExportedTypesSet, ExportedTypes),
    map.select_sorted_list(RepnMap, ExportedTypes, IntRepnMap),
    map.select_sorted_list(WordAlignedMap, ExportedTypes, IntWordAlignedMap),

    IntRepnsCord0 = cord.init,
    map.foldl_values(add_type_repn_item, IntRepnMap,
        IntRepnsCord0, IntRepnsCord1),
    map.foldl(maybe_add_word_aligned_repn_item, IntWordAlignedMap,
        IntRepnsCord1, IntRepnsCord2),

    map.foldl_values(add_eqv_repn_item, EqvMap,
        IntRepnsCord2, IntRepnsCord),

    IntRepns = cord.list(IntRepnsCord).

:- type eqv_map == map(type_ctor, item_type_defn_info_eqv).

:- type maybe_word_alignment
    --->    need_not_be_word_aligned
    ;       must_be_word_aligned.

:- type word_alignment_map == map(type_ctor, maybe_word_alignment).

:- pred decide_type_repns_stage_1(type_ctor::in, type_ctor_checked_defn::in,
    type_ctor_repn_map::in, type_ctor_repn_map::out,
    eqv_map::in, eqv_map::out,
    word_alignment_map::in, word_alignment_map::out,
    set_tree234(type_ctor)::in, set_tree234(type_ctor)::out) is det.

decide_type_repns_stage_1(TypeCtor, CheckedDefn,
        !RepnMap, !EqvMap, !WordAlignedMap, !ExportedTypes) :-
    (
        CheckedDefn = checked_defn_solver(_)
        % The representation of solver types is given by the type that
        % the definition names as the representation type. It has no
        % representation separate from that.
    ;
        CheckedDefn = checked_defn_std(StdDefn),
        (
            StdDefn = std_mer_type_eqv(EqvStatus, EqvDefn),
            (
                ( EqvStatus = std_eqv_type_mer_exported
                ; EqvStatus = std_eqv_type_abstract_exported
                ),
                set_tree234.insert(TypeCtor, !ExportedTypes)
            ;
                EqvStatus = std_eqv_type_all_private
            ),
            map.det_insert(TypeCtor, EqvDefn, !EqvMap)
        ;
            StdDefn = std_mer_type_du_all_plain_constants(DuStatus, DuDefn,
                HeadName, TailNames, MaybeDefnOrEnumCJCsE),
            (
                ( DuStatus = std_du_type_mer_ft_exported
                ; DuStatus = std_du_type_mer_exported
                ; DuStatus = std_du_type_abstract_exported
                ),
                set_tree234.insert(TypeCtor, !ExportedTypes)
            ;
                DuStatus = std_du_type_all_private
            ),
            decide_type_repns_stage_1_du_all_plain_constants(TypeCtor, DuDefn,
                HeadName, TailNames, MaybeDefnOrEnumCJCsE,
                !WordAlignedMap, !RepnMap)
        ;
            StdDefn = std_mer_type_du_not_all_plain_constants(DuStatus, DuDefn,
                MaybeDefnCJCsE),
            (
                ( DuStatus = std_du_type_mer_ft_exported
                ; DuStatus = std_du_type_mer_exported
                ; DuStatus = std_du_type_abstract_exported
                ),
                set_tree234.insert(TypeCtor, !ExportedTypes)
            ;
                DuStatus = std_du_type_all_private
            ),
            decide_type_repns_stage_1_du_not_all_plain_constants(TypeCtor,
                DuDefn, MaybeDefnCJCsE, !WordAlignedMap, !RepnMap)
        ;
            StdDefn = std_mer_type_abstract(AbsStatus, _AbsDefn,
                _MaybeDefnCJCsE),
            (
                ( AbsStatus = std_abs_type_ft_exported
                ; AbsStatus = std_abs_type_abstract_exported
                ),
                set_tree234.insert(TypeCtor, !ExportedTypes)
            ;
                AbsStatus = std_abs_type_all_private
            )
        )
    ).

:- pred decide_type_repns_stage_1_du_all_plain_constants(type_ctor::in,
    item_type_defn_info_du::in, string::in, list(string)::in,
    c_j_cs_e_maybe_defn_or_enum::in,
    word_alignment_map::in, word_alignment_map::out,
    type_ctor_repn_map::in, type_ctor_repn_map::out) is det.

decide_type_repns_stage_1_du_all_plain_constants(TypeCtor, DuDefn,
        HeadName, TailNames, MaybeDefnOrEnumCJCsE,
        !WordAlignmentMap, !TypeRepnMap) :-
    decide_type_repns_foreign_defns_or_enums(MaybeDefnOrEnumCJCsE,
        EnumForeignRepns),
    DuDefn = item_type_defn_info(_TypeCtorSymName, TypeParams, _DetailsDu,
        TVarSet, _Context, _SeqNum),
    (
        TailNames = [],
        % The type has exactly one data constructor.
        DirectDummyRepn = direct_dummy_repn(HeadName, EnumForeignRepns),
        DuRepn = dur_direct_dummy(DirectDummyRepn)
    ;
        TailNames = [HeadTailName | TailTailNames],
        % The type has at least two data constructors.
        EnumRepn = enum_repn(HeadName, HeadTailName, TailTailNames,
            EnumForeignRepns),
        DuRepn = dur_enum(EnumRepn)
    ),
    add_du_repn_to_type_map(TypeCtor, TypeParams, TVarSet, DuRepn,
        !TypeRepnMap),
    WordAligned = need_not_be_word_aligned,
    map.det_insert(TypeCtor, WordAligned, !WordAlignmentMap).

:- pred decide_type_repns_stage_1_du_not_all_plain_constants(type_ctor::in,
    item_type_defn_info_du::in, c_j_cs_e_maybe_defn::in,
    word_alignment_map::in, word_alignment_map::out,
    type_ctor_repn_map::in, type_ctor_repn_map::out) is det.

decide_type_repns_stage_1_du_not_all_plain_constants(TypeCtor, DuDefn,
        MaybeDefnCJCsE, !WordAlignmentMap, !TypeRepnMap) :-
    decide_type_repns_foreign_defns(MaybeDefnCJCsE, ForeignTypeRepns),
    DuDefn = item_type_defn_info(_TypeCtorSymName, TypeParams, DetailsDu,
        TVarSet, _Context, _SeqNum),
    DetailsDu = type_details_du(OoMCtors, MaybeCanonical, _MaybeDirectArgs),
    OoMCtors = one_or_more(HeadCtor, TailCtors),
    (
        TailCtors = [],
        % The type has exactly one data constructor.
        SingleCtor = HeadCtor,
        SingleCtor = ctor(_Ordinal, MaybeExistConstraints,
            SingleCtorSymName, Args, Arity, _SingleCtorContext),
        ( if
            MaybeExistConstraints = no_exist_constraints,
            Args = [_],
            Arity = 1,
            MaybeCanonical = canon
        then
            SingleCtorName = unqualify_name(SingleCtorSymName),
            NotagRepn = notag_repn(SingleCtorName, ForeignTypeRepns),
            DuRepn = dur_notag(NotagRepn),
            add_du_repn_to_type_map(TypeCtor, TypeParams, TVarSet, DuRepn,
                !TypeRepnMap),
            WordAligned = need_not_be_word_aligned
        else
            % NOTE We currently do not apply the direct arg optimization
            % to polymorphic argument types.
            % We could let the argument's type to have a set of type params
            % that is a subset of the type params of the containing type,
            % but that would require the runtime system to be able
            % to handle variables in the argument type, during unification
            % and comparison (mercury_unify_compare_body.h),
            % during deconstruction (mercury_ml_expand_body.h),
            % during deep copying (mercury_deep_copy_body.h), and maybe
            % during some other operations.
            TypeCtor = type_ctor(_, TypeCtorArity),
            ( if TypeCtorArity = 0 then
                WordAligned = must_be_word_aligned
            else
                WordAligned = need_not_be_word_aligned
            )
        )
    ;
        TailCtors = [_ | _],
        % The type has exactly two or more data constructors.
        WordAligned = need_not_be_word_aligned
    ),
    map.det_insert(TypeCtor, WordAligned, !WordAlignmentMap).

%---------------------------------------------------------------------------%

:- pred decide_type_repns_foreign_defns_or_enums(
    c_j_cs_e_maybe_defn_or_enum::in, c_j_cs_e_enum_repn::out) is det.

decide_type_repns_foreign_defns_or_enums(MaybeDefnOrEnumCJCsE,
        MaybeRepnCJCsE) :-
    MaybeDefnOrEnumCJCsE = c_java_csharp_erlang(MaybeDefnOrEnumC,
        MaybeDefnOrEnumJava, MaybeDefnOrEnumCsharp, MaybeDefnOrEnumErlang),
    represent_maybe_foreign_defn_or_enum(MaybeDefnOrEnumC,
        MaybeRepnC),
    represent_maybe_foreign_defn_or_enum(MaybeDefnOrEnumJava,
        MaybeRepnJava),
    represent_maybe_foreign_defn_or_enum(MaybeDefnOrEnumCsharp,
        MaybeRepnCsharp),
    represent_maybe_foreign_defn_or_enum(MaybeDefnOrEnumErlang,
        MaybeRepnErlang),
    MaybeRepnCJCsE = c_java_csharp_erlang(MaybeRepnC, MaybeRepnJava,
        MaybeRepnCsharp, MaybeRepnErlang).

:- pred represent_maybe_foreign_defn_or_enum(maybe(foreign_type_or_enum)::in,
    maybe(enum_foreign_repn)::out) is det.

represent_maybe_foreign_defn_or_enum(MaybeForeignDefnOrEnum,
        MaybeEnumForeignRepn) :-
    (
        MaybeForeignDefnOrEnum = no,
        MaybeEnumForeignRepn = no
    ;
        MaybeForeignDefnOrEnum = yes(ForeignDefnOrEnum),
        (
            ForeignDefnOrEnum = foreign_type_or_enum_type(TypeDefnInfo),
            foreign_type_defn_to_repn(TypeDefnInfo, ForeignTypeRepn),
            EnumForeignRepn = enum_foreign_type(ForeignTypeRepn)
        ;
            ForeignDefnOrEnum = foreign_type_or_enum_enum(ForeignEnumInfo),
            foreign_enum_defn_to_repn(ForeignEnumInfo, ForeignEnumRepn),
            EnumForeignRepn = enum_foreign_enum(ForeignEnumRepn)
        ),
        MaybeEnumForeignRepn = yes(EnumForeignRepn)
    ).

%---------------------%

:- pred decide_type_repns_foreign_defns(c_j_cs_e_maybe_defn::in,
    c_j_cs_e_repn::out) is det.

decide_type_repns_foreign_defns(MaybeDefnCJCsE, MaybeRepnCJCsE) :-
    MaybeDefnCJCsE = c_java_csharp_erlang(MaybeDefnC, MaybeDefnJava,
        MaybeDefnCsharp, MaybeDefnErlang),
    represent_maybe_foreign_defn(MaybeDefnC, MaybeRepnC),
    represent_maybe_foreign_defn(MaybeDefnJava, MaybeRepnJava),
    represent_maybe_foreign_defn(MaybeDefnCsharp, MaybeRepnCsharp),
    represent_maybe_foreign_defn(MaybeDefnErlang, MaybeRepnErlang),
    MaybeRepnCJCsE = c_java_csharp_erlang(MaybeRepnC, MaybeRepnJava,
        MaybeRepnCsharp, MaybeRepnErlang).

:- pred represent_maybe_foreign_defn(maybe(item_type_defn_info_foreign)::in,
    maybe(foreign_type_repn)::out) is det.

represent_maybe_foreign_defn(MaybeForeignDefn, MaybeForeignRepn) :-
    (
        MaybeForeignDefn = no,
        MaybeForeignRepn = no
    ;
        MaybeForeignDefn = yes(ForeignDefn),
        foreign_type_defn_to_repn(ForeignDefn, ForeignRepn),
        MaybeForeignRepn = yes(ForeignRepn)
    ).

%---------------------%

:- pred foreign_type_defn_to_repn(item_type_defn_info_foreign::in,
    foreign_type_repn::out) is det.

foreign_type_defn_to_repn(ItemTypeDefnInfo, ForeignTypeRepn) :-
    ItemTypeDefnInfo = item_type_defn_info(_TypeCtorSymName, _TypeCtorParams,
        DetailsForeign, _TVarSet, _Context, _SeqNum),
    DetailsForeign = type_details_foreign(LangType, _MaybeCanon, Assertions),
    ( LangType = c(c_type(ForeignTypeName))
    ; LangType = java(java_type(ForeignTypeName))
    ; LangType = csharp(csharp_type(ForeignTypeName))
    ; LangType = erlang(erlang_type), ForeignTypeName = ""
    ),
    ForeignTypeRepn = foreign_type_repn(ForeignTypeName, Assertions).

:- pred foreign_enum_defn_to_repn(checked_foreign_enum::in,
    one_or_more(string)::out) is det.

foreign_enum_defn_to_repn(CheckedForeignEnum, ForeignCtorNames) :-
    CheckedForeignEnum =
        checked_foreign_enum(_ItemForeignEnumInfo, ForeignCtorNames).

%---------------------------------------------------------------------------%

:- pred add_du_repn_to_type_map(type_ctor::in, list(tvar)::in, tvarset::in,
    du_repn::in, type_ctor_repn_map::in, type_ctor_repn_map::out) is det.

add_du_repn_to_type_map(TypeCtor, TypeParams, TVarSet, DuRepn, !TypeRepnMap) :-
    TypeRepn = tcrepn_du(DuRepn),
    TypeCtor = type_ctor(TypeCtorSymName, _TypeCtorArity),
    TypeRepnInfo = item_type_repn_info(TypeCtorSymName, TypeParams, TypeRepn,
        TVarSet, term.context_init, -1),
    map.det_insert(TypeCtor, TypeRepnInfo, !TypeRepnMap).

%---------------------------------------------------------------------------%

:- pred add_type_repn_item(item_type_repn_info::in,
    cord(item_type_repn_info)::in, cord(item_type_repn_info)::out) is det.

add_type_repn_item(RepnItem, !Items) :-
    !:Items = cord.snoc(!.Items, RepnItem).

:- pred maybe_add_word_aligned_repn_item(type_ctor::in,
    maybe_word_alignment::in,
    cord(item_type_repn_info)::in, cord(item_type_repn_info)::out) is det.

maybe_add_word_aligned_repn_item(TypeCtor, WordAligned, !Items) :-
    (
        WordAligned = need_not_be_word_aligned
    ;
        WordAligned = must_be_word_aligned,
        TypeCtor = type_ctor(TypeCtorSymName, TypeCtorArity),
        varset.init(TVarSet0),
        varset.new_vars(TypeCtorArity, TypeParams, TVarSet0, TVarSet),
        Item = item_type_repn_info(TypeCtorSymName, TypeParams,
            tcrepn_is_word_aligned_ptr, TVarSet, term.context_init, -1),
        !:Items = cord.snoc(!.Items, Item)
    ).

:- pred add_eqv_repn_item(item_type_defn_info_eqv::in,
    cord(item_type_repn_info)::in, cord(item_type_repn_info)::out) is det.

add_eqv_repn_item(DefnItem, !Items) :-
    DefnItem = item_type_defn_info(TypeCtorSymName, TypeParams,
        type_details_eqv(EqvType), TVarSet, Context, SeqNum),
    RepnItem = item_type_repn_info(TypeCtorSymName, TypeParams,
        tcrepn_is_eqv_to(EqvType), TVarSet, Context, SeqNum),
    !:Items = cord.snoc(!.Items, RepnItem).

%---------------------------------------------------------------------------%
:- end_module parse_tree.decide_type_repn.
%---------------------------------------------------------------------------%
