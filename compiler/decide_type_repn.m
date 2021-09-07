%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019-2021 The Mercury team.
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
% must be grade-independent, since interface files are supposed to be
% grade-independent. This means that while du_type_layout.m computes
% what type representation *this* compiler invocation should use for each type,
% this module computes what type representation *every* compiler invocation
% that generates target language code should use for each type, and puts
% that information into the .int file of the module defining the type
% in the form of type_repn items. (The algorithm that does this *does* bake
% into those type_repn items the effects of developer-only options such as
% --allow-packing-local-sectags, which are in effect parameters of that
% algorithm.) Then, compiler invocations that generate target language code
% decide which of these representations is applicable to their circumstances,
% specifically: what the target language is, and how many bits there are
% in a word on the target.
%
%---------------------------------------------------------------------------%
%
% Overall description of the task of this module when creating the different
% kinds of interface files, in terms of inputs and outputs:
%
% .int3 now:
%   input:  type_defns in source module
%   output: type_repns for simple types in interface, subtypes in interface,
%           and all? eqv types
%
% .int2 now:
%   input:  type_defns in source module
%   output: type_repns for simple types in interface, subtypes in interface,
%           and all? eqv types
%
% .int2 later:
%   input:  type_defns in source module
%           type_repns for simple/eqv/subtypes types in direct/indirect
%           imported .int3s
%   output: type_repns for simple types in interface, subtypes in interface,
%           and all? eqv types
%
% .int1 later:
%   input:  type_defns in source module
%           type_repns for simple/eqv/subtypes types in direct/indirect
%           imported .int3s
%   output: type_repns for all types that appear in interface
%           (possibly for all types, in the interface or not, since
%           .opt files may expose private types)
%
% .int0 later:
%   input:  type_defns in source module
%           type_repns for simple/eqv/subtypes types in direct/indirect
%           imported .int3s
%   output: type_repns for all types
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
%
% This module breaks backward compatibility with respect to the direct arg
% optimization in two ways.
%
% The first is that it does not pay attention to any "where direct_arg is"
% annotations on type definitions. This is because we have agreed to stop
% supporting such annotations, and once this module starts being used,
% we will remove them from the language.
%
% The second is that it does not (yet) support the term size profiling grades,
% because those grades want to *disable* the direct arg optimization. Since
% every other grade *will* want to have direct arg optimization enabled,
% supporting the term size profiling grades will require us to record
% in type_repn items in the .int file the representation of each type
% with the direct arg optimization both enabled and disabled. At the moment
% the definitions of the relevant parts of du_repns do not allow this,
% though they could be extended to allow it. However, such an extension
% is not a priority.
%
%---------------------------------------------------------------------------%
%
% XXX We should decide whether the auxiliary outputs that the options
% --show-*type-repn and --inform-suboptimal-packing ask for should take place
% (a) when we are generating the module's .int file, or
% (b) when we are generating target language code for the module.
%
%---------------------------------------------------------------------------%

:- module parse_tree.decide_type_repn.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.check_parse_tree_type_defns.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_item.

:- import_module list.
:- import_module map.

    % decide_repns_for_simple_types_for_int3(ModuleName, TypeCtorCheckedMap,
    %   IntTypeRepnItems):
    %
    % Given TypeCtorCheckedMap's comprehensive picture of the
    % type definitions in the module,
    %
    % - figure out which type definitions define exported simple types,
    % - decide their representations, and
    % - generate items recording those decisions, for types that appear
    %   in the interface
    %
:- pred decide_repns_for_simple_types_for_int3(module_name::in,
    type_ctor_checked_map::in, type_ctor_repn_map::out) is det.

:- pred decide_repns_for_all_types_for_int1(globals::in, module_name::in,
    type_ctor_checked_map::in,
    map(module_name, direct_int3_spec)::in,
    map(module_name, indirect_int3_spec)::in,
    type_ctor_repn_map::out, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module parse_tree.equiv_type.
:- import_module parse_tree.parse_tree_out_type_repn.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.

:- import_module bool.
:- import_module int.
:- import_module io.
:- import_module maybe.
:- import_module one_or_more.
:- import_module require.
:- import_module set.
:- import_module set_tree234.
:- import_module string.
:- import_module term.
:- import_module uint.
:- import_module uint8.
:- import_module varset.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Make decisions about simple types.
%

decide_repns_for_simple_types_for_int3(_ModuleName, TypeCtorCheckedMap,
        !:Int3RepnMap) :-
    map.init(EqvRepnMap0),
    map.init(SubtypeMap0),
    map.init(SimpleDuMap0),
    WordAlignedTypeCtorsC0 = set_tree234.init,
    ExportedTypes0 = set_tree234.init,
    map.foldl5(decide_simple_type_repns_stage_1, TypeCtorCheckedMap,
        EqvRepnMap0, EqvRepnMap,
        SubtypeMap0, SubtypeMap,
        SimpleDuMap0, SimpleDuMap,
        WordAlignedTypeCtorsC0, WordAlignedTypeCtorsC,
        ExportedTypes0, ExportedTypes),

    map.init(!:Int3RepnMap),
    map.foldl(add_eqv_repn_item, EqvRepnMap, !Int3RepnMap),
    map.foldl(maybe_add_subtype_repn_item(ExportedTypes),
        SubtypeMap, !Int3RepnMap),
    map.foldl(maybe_add_simple_du_repn_item(ExportedTypes),
        SimpleDuMap, !Int3RepnMap),
    set_tree234.foldl(maybe_add_word_aligned_repn_item(ExportedTypes),
        WordAlignedTypeCtorsC, !Int3RepnMap).

%---------------------------------------------------------------------------%

:- type eqv_repn_map == map(type_ctor, item_type_repn_info_eqv).

:- type subtype_repn_map == map(type_ctor, item_type_repn_info_subtype).

:- type simple_du_map == map(type_ctor, simple_du_repn).

:- type simple_du_repn
    --->    sdr_direct_dummy(list(type_param), tvarset, direct_dummy_repn)
    ;       sdr_enum(list(type_param), tvarset, enum_repn)
    ;       sdr_notag(list(type_param), tvarset, notag_repn).

    % The set of type_ctors that are word aligned when targeting this.
    % This may be because the definition when targeting C is a du type
    % with one functor and two or more arguments, or because that definition
    % is a foreign type that is asserted by the programmer to be word aligned.
:- type word_aligned_type_ctors_c == set_tree234(type_ctor).

%---------------------------------------------------------------------------%

:- pred decide_simple_type_repns_stage_1(
    type_ctor::in, type_ctor_checked_defn::in,
    eqv_repn_map::in, eqv_repn_map::out,
    subtype_repn_map::in, subtype_repn_map::out,
    simple_du_map::in, simple_du_map::out,
    word_aligned_type_ctors_c::in, word_aligned_type_ctors_c::out,
    set_tree234(type_ctor)::in, set_tree234(type_ctor)::out) is det.

decide_simple_type_repns_stage_1(TypeCtor, CheckedDefn,
        !EqvRepnMap, !SubtypeMap, !SimpleDuMap, !WordAlignedTypeCtorsC,
        !ExportedTypes) :-
    % The structure of this logic must match the structure of the logic
    % in decide_all_type_repns_stage_2.
    (
        CheckedDefn = checked_defn_solver(_, _)
        % The representation of solver types is given by the type that
        % the definition names as the representation type. It has no
        % representation separate from that.
    ;
        CheckedDefn = checked_defn_std(StdDefn, _),
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
            EqvDefn = item_type_defn_info(TypeCtorSymName, TypeParams,
                type_details_eqv(EqvType), TVarSet, _Context, _SeqNum),
            EqvRepnItem = item_type_repn_info(TypeCtorSymName, TypeParams,
                EqvType, TVarSet, term.dummy_context_init, item_no_seq_num),
            map.det_insert(TypeCtor, EqvRepnItem, !EqvRepnMap)
        ;
            StdDefn = std_mer_type_du_subtype(DuStatus, DuDefn),
            (
                ( DuStatus = std_sub_type_mer_exported
                ; DuStatus = std_sub_type_abstract_exported
                ),
                set_tree234.insert(TypeCtor, !ExportedTypes)
            ;
                DuStatus = std_sub_type_all_private
            ),
            DuDefn = item_type_defn_info(TypeCtorSymName, TypeParams,
                DetailsDu, TVarSet, _Context, _SeqNum),
            DetailsDu = type_details_du(MaybeSuperType, _, _, _),
            (
                MaybeSuperType = subtype_of(SuperType),
                type_to_ctor_det(SuperType, SuperTypeCtor)
            ;
                MaybeSuperType = not_a_subtype,
                unexpected($pred, "no supertype")
            ),
            SubtypeRepnItem = item_type_repn_info(TypeCtorSymName, TypeParams,
                SuperTypeCtor, TVarSet, term.dummy_context_init,
                item_no_seq_num),
            map.det_insert(TypeCtor, SubtypeRepnItem, !SubtypeMap)
        ;
            StdDefn = std_mer_type_du_all_plain_constants(DuStatus, DuDefn,
                HeadName, TailNames, MaybeDefnOrEnumCJCs),
            (
                ( DuStatus = std_du_type_mer_ft_exported
                ; DuStatus = std_du_type_mer_exported
                ; DuStatus = std_du_type_abstract_exported
                ),
                set_tree234.insert(TypeCtor, !ExportedTypes)
            ;
                DuStatus = std_du_type_all_private
            ),
            expect_not(du_defn_is_subtype(DuDefn), $pred, "type is subtype"),
            decide_type_repns_stage_1_du_all_plain_constants(TypeCtor, DuDefn,
                HeadName, TailNames, MaybeDefnOrEnumCJCs, !SimpleDuMap)
        ;
            StdDefn = std_mer_type_du_not_all_plain_constants(DuStatus, DuDefn,
                MaybeDefnCJCs),
            (
                ( DuStatus = std_du_type_mer_ft_exported
                ; DuStatus = std_du_type_mer_exported
                ; DuStatus = std_du_type_abstract_exported
                ),
                set_tree234.insert(TypeCtor, !ExportedTypes)
            ;
                DuStatus = std_du_type_all_private
            ),
            expect_not(du_defn_is_subtype(DuDefn), $pred, "type is subtype"),
            decide_type_repns_stage_1_du_not_all_plain_constants(TypeCtor,
                DuDefn, MaybeDefnCJCs, !SimpleDuMap, !WordAlignedTypeCtorsC)
        ;
            StdDefn = std_mer_type_abstract(AbsStatus, _AbsDefn,
                MaybeDefnCJCs),
            (
                ( AbsStatus = std_abs_type_ft_exported
                ; AbsStatus = std_abs_type_abstract_exported
                ),
                set_tree234.insert(TypeCtor, !ExportedTypes)
            ;
                AbsStatus = std_abs_type_all_private
            ),
            maybe_mark_type_ctor_as_word_aligned_for_c(TypeCtor,
                MaybeDefnCJCs, !WordAlignedTypeCtorsC)
        )
    ).

%---------------------%

:- pred maybe_mark_type_ctor_as_word_aligned_for_c(type_ctor::in,
    c_j_cs_maybe_defn::in,
    word_aligned_type_ctors_c::in, word_aligned_type_ctors_c::out) is det.

maybe_mark_type_ctor_as_word_aligned_for_c(TypeCtor, MaybeDefnCJCs,
        !WordAlignedTypeCtorsC) :-
    MaybeDefnCJCs = c_java_csharp(MaybeDefnC, _, _),
    ( if
        MaybeDefnC = yes(DefnC),
        DefnC ^ td_ctor_defn ^ foreign_assertions =
            foreign_type_assertions(Assertions),
        set.member(foreign_type_word_aligned_pointer, Assertions)
    then
        set_tree234.insert(TypeCtor, !WordAlignedTypeCtorsC)
    else
        true
    ).

%---------------------%

:- pred du_defn_is_subtype(item_type_defn_info_du::in) is semidet.

du_defn_is_subtype(DuDefn) :-
    DuDefn = item_type_defn_info(_, _, DetailsDu, _, _, _),
    DetailsDu = type_details_du(MaybeSuperType, _, _, _),
    MaybeSuperType = subtype_of(_).

:- pred decide_type_repns_stage_1_du_all_plain_constants(type_ctor::in,
    item_type_defn_info_du::in, string::in, list(string)::in,
    c_j_cs_maybe_defn_or_enum::in,
    simple_du_map::in, simple_du_map::out) is det.

decide_type_repns_stage_1_du_all_plain_constants(TypeCtor, DuDefn,
        HeadName, TailNames, MaybeDefnOrEnumCJCs, !SimpleDuMap) :-
    decide_type_repns_foreign_defns_or_enums(MaybeDefnOrEnumCJCs,
        EnumForeignRepns),
    DuDefn = item_type_defn_info(_TypeCtorSymName, TypeParams, DetailsDu,
        TVarSet, _Context, _SeqNum),
    DetailsDu = type_details_du(MaybeSuperType, _, _, _),
    (
        MaybeSuperType = not_a_subtype,
        (
            TailNames = [],
            % The type has exactly one data constructor.
            DirectDummyRepn = direct_dummy_repn(HeadName, EnumForeignRepns),
            SimpleDuRepn = sdr_direct_dummy(TypeParams, TVarSet,
                DirectDummyRepn)
        ;
            TailNames = [HeadTailName | TailTailNames],
            % The type has at least two data constructors.
            EnumRepn = enum_repn(HeadName, HeadTailName, TailTailNames,
                EnumForeignRepns),
            SimpleDuRepn = sdr_enum(TypeParams, TVarSet, EnumRepn)
        ),
        map.det_insert(TypeCtor, SimpleDuRepn, !SimpleDuMap)
    ;
        MaybeSuperType = subtype_of(_)
        % We cannot decide the representation of a subtype independently
        % of its base type, which may not even be in the same module.
    ).

:- pred decide_type_repns_stage_1_du_not_all_plain_constants(type_ctor::in,
    item_type_defn_info_du::in, c_j_cs_maybe_defn::in,
    simple_du_map::in, simple_du_map::out,
    word_aligned_type_ctors_c::in, word_aligned_type_ctors_c::out) is det.

decide_type_repns_stage_1_du_not_all_plain_constants(TypeCtor, DuDefn,
        MaybeDefnCJCs, !SimpleDuMap, !WordAlignedTypeCtorsC) :-
    decide_type_repns_foreign_defns(MaybeDefnCJCs, ForeignTypeRepns),
    DuDefn = item_type_defn_info(_TypeCtorSymName, TypeParams, DetailsDu,
        TVarSet, _Context, _SeqNum),
    DetailsDu = type_details_du(MaybeSuperType, OoMCtors, MaybeCanonical,
        _MaybeDirectArgs),
    (
        MaybeSuperType = not_a_subtype,
        OoMCtors = one_or_more(HeadCtor, TailCtors),
        (
            TailCtors = [],
            % The type has exactly one data constructor.
            SingleCtor = HeadCtor,
            SingleCtor = ctor(_Ordinal, MaybeExistConstraints,
                SingleCtorSymName, Args, Arity, _SingleCtorContext),
            ( if
                one_constructor_non_constant_is_notag(MaybeExistConstraints,
                    Args, Arity, MaybeCanonical, OneArg)
            then
                SingleCtorName = unqualify_name(SingleCtorSymName),
                OneArgType = OneArg ^ arg_type,
                NotagRepn =
                    notag_repn(SingleCtorName, OneArgType, ForeignTypeRepns),
                SimpleDuRepn = sdr_notag(TypeParams, TVarSet, NotagRepn),
                map.det_insert(TypeCtor, SimpleDuRepn, !SimpleDuMap)
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
                    set_tree234.insert(TypeCtor, !WordAlignedTypeCtorsC)
                else
                    true
                )
            )
        ;
            TailCtors = [_ | _]
            % The type has two or more data constructors.
            % This means that it need not be word aligned.
        )
    ;
        MaybeSuperType = subtype_of(_)
        % We cannot decide the representation of a subtype independently
        % of its base type, which may not even be in the same module.
    ).

%---------------------------------------------------------------------------%

:- pred decide_type_repns_foreign_defns_or_enums(
    c_j_cs_maybe_defn_or_enum::in, c_j_cs_enum_repn::out) is det.

decide_type_repns_foreign_defns_or_enums(MaybeDefnOrEnumCJCs,
        MaybeRepnCJCs) :-
    MaybeDefnOrEnumCJCs = c_java_csharp(MaybeDefnOrEnumC,
        MaybeDefnOrEnumJava, MaybeDefnOrEnumCsharp),
    represent_maybe_foreign_defn_or_enum(MaybeDefnOrEnumC,
        MaybeRepnC),
    represent_maybe_foreign_defn_or_enum(MaybeDefnOrEnumJava,
        MaybeRepnJava),
    represent_maybe_foreign_defn_or_enum(MaybeDefnOrEnumCsharp,
        MaybeRepnCsharp),
    MaybeRepnCJCs = c_java_csharp(MaybeRepnC, MaybeRepnJava,
        MaybeRepnCsharp).

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

:- pred decide_type_repns_foreign_defns(c_j_cs_maybe_defn::in,
    c_j_cs_repn::out) is det.

decide_type_repns_foreign_defns(MaybeDefnCJCs, MaybeRepnCJCs) :-
    MaybeDefnCJCs = c_java_csharp(MaybeDefnC, MaybeDefnJava,
        MaybeDefnCsharp),
    represent_maybe_foreign_defn(MaybeDefnC, MaybeRepnC),
    represent_maybe_foreign_defn(MaybeDefnJava, MaybeRepnJava),
    represent_maybe_foreign_defn(MaybeDefnCsharp, MaybeRepnCsharp),
    MaybeRepnCJCs = c_java_csharp(MaybeRepnC, MaybeRepnJava,
        MaybeRepnCsharp).

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
    ),
    ForeignTypeRepn = foreign_type_repn(ForeignTypeName, Assertions).

:- pred foreign_enum_defn_to_repn(checked_foreign_enum::in,
    one_or_more(string)::out) is det.

foreign_enum_defn_to_repn(CheckedForeignEnum, ForeignCtorNames) :-
    CheckedForeignEnum =
        checked_foreign_enum(_ItemForeignEnumInfo, ForeignCtorNames).

%---------------------------------------------------------------------------%

:- pred add_eqv_repn_item(type_ctor::in, item_type_repn_info_eqv::in,
    type_ctor_repn_map::in, type_ctor_repn_map::out) is det.

add_eqv_repn_item(TypeCtor, EqvRepnItem, !RepnMap) :-
    EqvRepnItem = item_type_repn_info(TypeCtorSymName, TypeParams,
        EqvType, TVarSet, _Context, _SeqNum),
    RepnItem = item_type_repn_info(TypeCtorSymName, TypeParams,
        tcrepn_is_eqv_to(EqvType), TVarSet, term.dummy_context_init,
        item_no_seq_num),
    map.det_insert(TypeCtor, RepnItem, !RepnMap).

%------------------%

:- pred maybe_add_subtype_repn_item(set_tree234(type_ctor)::in,
    type_ctor::in, item_type_repn_info_subtype::in,
    type_ctor_repn_map::in, type_ctor_repn_map::out) is det.

maybe_add_subtype_repn_item(ExportedTypes, TypeCtor, SubtypeRepnItem,
        !RepnMap) :-
    ( if set_tree234.member(TypeCtor, ExportedTypes) then
        add_subtype_repn_item(TypeCtor, SubtypeRepnItem, !RepnMap)
    else
        true
    ).

:- pred add_subtype_repn_item(type_ctor::in, item_type_repn_info_subtype::in,
    type_ctor_repn_map::in, type_ctor_repn_map::out) is det.

add_subtype_repn_item(TypeCtor, SubtypeRepnItem, !RepnMap) :-
    SubtypeRepnItem = item_type_repn_info(TypeCtorSymName, TypeParams,
        SuperTypeCtor, TVarSet, _Context, _SeqNum),
    RepnItem = item_type_repn_info(TypeCtorSymName, TypeParams,
        tcrepn_is_subtype_of(SuperTypeCtor), TVarSet,
        term.dummy_context_init, item_no_seq_num),
    map.det_insert(TypeCtor, RepnItem, !RepnMap).

%------------------%

:- pred maybe_add_simple_du_repn_item(set_tree234(type_ctor)::in,
    type_ctor::in, simple_du_repn::in,
    type_ctor_repn_map::in, type_ctor_repn_map::out) is det.

maybe_add_simple_du_repn_item(ExportedTypes, TypeCtor, SimpleDuRepn,
        !RepnMap) :-
    ( if set_tree234.member(TypeCtor, ExportedTypes) then
        add_simple_du_repn_item(TypeCtor, SimpleDuRepn, !RepnMap)
    else
        true
    ).

:- pred add_simple_du_repn_item(type_ctor::in, simple_du_repn::in,
    type_ctor_repn_map::in, type_ctor_repn_map::out) is det.

add_simple_du_repn_item(TypeCtor, SimpleDuRepn, !RepnMap) :-
    (
        SimpleDuRepn =
            sdr_direct_dummy(TypeParams, TVarSet, DirectDummyRepn),
        DuRepn = dur_direct_dummy(DirectDummyRepn)
    ;
        SimpleDuRepn = sdr_enum(TypeParams, TVarSet, EnumRepn),
        DuRepn = dur_enum(EnumRepn)
    ;
        SimpleDuRepn = sdr_notag(TypeParams, TVarSet, NotagRepn),
        DuRepn = dur_notag(NotagRepn)
    ),
    TypeCtor = type_ctor(TypeCtorSymName, _TypeCtorArity),
    Item = item_type_repn_info(TypeCtorSymName, TypeParams,
        tcrepn_du(DuRepn), TVarSet, term.context_init, item_no_seq_num),
    map.det_insert(TypeCtor, Item, !RepnMap).

%------------------%

:- pred maybe_add_word_aligned_repn_item(set_tree234(type_ctor)::in,
    type_ctor::in, type_ctor_repn_map::in, type_ctor_repn_map::out) is det.

maybe_add_word_aligned_repn_item(ExportedTypes, TypeCtor, !RepnMap) :-
    ( if set_tree234.member(TypeCtor, ExportedTypes) then
        TypeCtor = type_ctor(TypeCtorSymName, TypeCtorArity),
        varset.init(TVarSet0),
        varset.new_vars(TypeCtorArity, TypeParams, TVarSet0, TVarSet),
        Item = item_type_repn_info(TypeCtorSymName, TypeParams,
            tcrepn_is_word_aligned_ptr, TVarSet, term.context_init,
            item_no_seq_num),
        map.det_insert(TypeCtor, Item, !RepnMap)
    else
        true
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Make decisions about all types.
%

decide_repns_for_all_types_for_int1(Globals, _ModuleName, TypeCtorCheckedMap,
        DirectSpecMap, IndirectSpecMap, !:Int1RepnMap, Specs) :-
    map.foldl4_values(record_type_repns_in_direct_int3_spec,
        DirectSpecMap, map.init, EqvRepnMap0, map.init, SubtypeMap0,
        map.init, SimpleDuMap0, set_tree234.init, WordAlignedTypeCtorsC0),
    map.foldl4_values(record_type_repns_in_indirect_int3_spec,
        IndirectSpecMap,
        EqvRepnMap0, EqvRepnMap1,
        SubtypeMap0, SubtypeMap1,
        SimpleDuMap0, SimpleDuMap1,
        WordAlignedTypeCtorsC0, WordAlignedTypeCtorsC1),
    map.foldl5(decide_simple_type_repns_stage_1, TypeCtorCheckedMap,
        EqvRepnMap1, EqvRepnMap,
        SubtypeMap1, SubtypeMap,
        SimpleDuMap1, SimpleDuMap,
        WordAlignedTypeCtorsC1, WordAlignedTypeCtorsC,
        set_tree234.init, _ExportedTypes),

    setup_base_params(Globals, BaseParams),
    map.init(!:Int1RepnMap),
    map.map_values_only(item_type_repn_info_eqv_to_eqv_type_body,
        EqvRepnMap, EqvMap),
    map.foldl2(
        decide_all_type_repns_stage_2(BaseParams, EqvRepnMap, EqvMap,
            SubtypeMap, WordAlignedTypeCtorsC, SimpleDuMap),
        TypeCtorCheckedMap, !Int1RepnMap, [], Specs).

:- pred item_type_repn_info_eqv_to_eqv_type_body(item_type_repn_info_eqv::in,
    eqv_type_body::out) is det.

item_type_repn_info_eqv_to_eqv_type_body(ItemTypeRepnInfoEqv, EqvBody) :-
    ItemTypeRepnInfoEqv = item_type_repn_info(_TypeCtorSymName, TypeParams,
        EqvType, TVarSet, _Context, _SeqNum),
    EqvBody = eqv_type_body(TVarSet, TypeParams, EqvType).

%---------------------------------------------------------------------------%

:- pred record_type_repns_in_direct_int3_spec(direct_int3_spec::in,
    eqv_repn_map::in, eqv_repn_map::out,
    subtype_repn_map::in, subtype_repn_map::out,
    simple_du_map::in, simple_du_map::out,
    word_aligned_type_ctors_c::in, word_aligned_type_ctors_c::out) is det.

record_type_repns_in_direct_int3_spec(DirectInt3Spec,
        !EqvRepnMap, !SubtypeMap, !SimpleDuMap, !WordAlignedTypeCtorsC) :-
    DirectInt3Spec = direct_int3(ParseTreeInt3, _ReadWhy3),
    record_type_repns_in_parse_tree_int3(ParseTreeInt3,
        !EqvRepnMap, !SubtypeMap, !SimpleDuMap, !WordAlignedTypeCtorsC).

:- pred record_type_repns_in_indirect_int3_spec(indirect_int3_spec::in,
    eqv_repn_map::in, eqv_repn_map::out,
    subtype_repn_map::in, subtype_repn_map::out,
    simple_du_map::in, simple_du_map::out,
    word_aligned_type_ctors_c::in, word_aligned_type_ctors_c::out) is det.

record_type_repns_in_indirect_int3_spec(IndirectInt3Spec,
        !EqvRepnMap, !SubtypeMap, !SimpleDuMap, !WordAlignedTypeCtorsC) :-
    IndirectInt3Spec = indirect_int3(ParseTreeInt3, _ReadWhy3),
    record_type_repns_in_parse_tree_int3(ParseTreeInt3,
        !EqvRepnMap, !SubtypeMap, !SimpleDuMap, !WordAlignedTypeCtorsC).

:- pred record_type_repns_in_parse_tree_int3(parse_tree_int3::in,
    eqv_repn_map::in, eqv_repn_map::out,
    subtype_repn_map::in, subtype_repn_map::out,
    simple_du_map::in, simple_du_map::out,
    word_aligned_type_ctors_c::in, word_aligned_type_ctors_c::out) is det.

record_type_repns_in_parse_tree_int3(ParseTreeInt3,
        !EqvRepnMap, !SubtypeMap, !SimpleDuMap, !WordAlignedTypeCtorsC) :-
    ModuleName = ParseTreeInt3 ^ pti3_module_name,
    TypeRepns = ParseTreeInt3 ^ pti3_int_type_repns,
    map.foldl4(record_type_repn_in_parse_tree_int3(ModuleName), TypeRepns,
        !EqvRepnMap, !SubtypeMap, !SimpleDuMap, !WordAlignedTypeCtorsC).

:- pred record_type_repn_in_parse_tree_int3(module_name::in,
    type_ctor::in, item_type_repn_info::in,
    eqv_repn_map::in, eqv_repn_map::out,
    subtype_repn_map::in, subtype_repn_map::out,
    simple_du_map::in, simple_du_map::out,
    word_aligned_type_ctors_c::in, word_aligned_type_ctors_c::out) is det.

record_type_repn_in_parse_tree_int3(ModuleName, TypeCtor0, ItemTypeRepnInfo,
        !EqvRepnMap, !SubtypeMap, !SimpleDuMap, !WordAlignedTypeCtorsC) :-
    ItemTypeRepnInfo = item_type_repn_info(TypeCtorSymName0, TypeParams,
        RepnInfo, TVarSet, _Context, _SeqNum),
    TypeCtor0 = type_ctor(SymName0, Arity),
    Name = unqualify_name(SymName0),
    TypeCtor = type_ctor(qualified(ModuleName, Name), Arity),
    (
        RepnInfo = tcrepn_du(DuRepn),
        (
            DuRepn = dur_direct_dummy(DirectDummyRepn),
            SimpleDuRepn =
                sdr_direct_dummy(TypeParams, TVarSet, DirectDummyRepn),
            map.det_insert(TypeCtor, SimpleDuRepn, !SimpleDuMap)
        ;
            DuRepn = dur_notag(NotagRepn),
            SimpleDuRepn = sdr_notag(TypeParams, TVarSet, NotagRepn),
            map.det_insert(TypeCtor, SimpleDuRepn, !SimpleDuMap)
        ;
            DuRepn = dur_enum(EnumRepn),
            SimpleDuRepn = sdr_enum(TypeParams, TVarSet, EnumRepn),
            map.det_insert(TypeCtor, SimpleDuRepn, !SimpleDuMap)
        ;
            ( DuRepn = dur_gen_only_functor(_)
            ; DuRepn = dur_gen_more_functors(_)
            ),
            unexpected($pred, "dur_gen")
        )
    ;
        RepnInfo = tcrepn_is_word_aligned_ptr,
        set_tree234.insert(TypeCtor, !WordAlignedTypeCtorsC)
    ;
        RepnInfo = tcrepn_is_eqv_to(EqvType),
        TypeCtorName = unqualify_name(TypeCtorSymName0),
        TypeCtorSymName = qualified(ModuleName, TypeCtorName),
        RepnTypeCtor = type_ctor(TypeCtorSymName, list.length(TypeParams)),
        expect(unify(TypeCtor, RepnTypeCtor), $pred,
            "TypeCtor != RepnTypeCtor"),
        EqvRepnItem = item_type_repn_info(TypeCtorSymName, TypeParams,
            EqvType, TVarSet, term.dummy_context_init, item_no_seq_num),
        map.det_insert(TypeCtor, EqvRepnItem, !EqvRepnMap)
    ;
        RepnInfo = tcrepn_is_subtype_of(SuperType),
        TypeCtorName = unqualify_name(TypeCtorSymName0),
        TypeCtorSymName = qualified(ModuleName, TypeCtorName),
        RepnTypeCtor = type_ctor(TypeCtorSymName, list.length(TypeParams)),
        expect(unify(TypeCtor, RepnTypeCtor), $pred,
            "TypeCtor != RepnTypeCtor"),
        SubtypeRepnItem = item_type_repn_info(TypeCtorSymName, TypeParams,
            SuperType, TVarSet, term.dummy_context_init, item_no_seq_num),
        map.det_insert(TypeCtor, SubtypeRepnItem, !SubtypeMap)
    ;
        RepnInfo = tcrepn_foreign(_),
        unexpected($pred, "unexpected tc_repn_foreign")
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % XXX document me
    %
    % generate a type_repn item for every type defined in the module.
    %
    % - dereference all eqv types
    % - generate type repns for subtypes
    % - convert simple du types to generic type repns
    % - decide all complex types and generate type repns
    %
:- pred decide_all_type_repns_stage_2(base_params::in,
    eqv_repn_map::in, type_eqv_map::in, subtype_repn_map::in,
    set_tree234(type_ctor)::in, simple_du_map::in,
    type_ctor::in, type_ctor_checked_defn::in,
    type_ctor_repn_map::in, type_ctor_repn_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

decide_all_type_repns_stage_2(BaseParams, EqvRepnMap, EqvMap, SubtypeMap,
        WordAlignedTypeCtorsC, SimpleDuMap, TypeCtor, CheckedDefn,
        !Int1RepnMap, !Specs) :-
    % The structure of this logic must match the structure of the logic
    % in decide_simple_type_repns_stage_1.
    (
        CheckedDefn = checked_defn_solver(_, _)
        % The representation of solver types is given by the type that
        % the definition names as the representation type. It has no
        % representation separate from that.
    ;
        CheckedDefn = checked_defn_std(StdDefn, _),
        (
            StdDefn = std_mer_type_eqv(_, _),
            map.lookup(EqvRepnMap, TypeCtor, EqvRepnItem0),
            replace_in_type_repn_eqv(EqvMap,
                EqvRepnItem0, EqvRepnItem, !Specs),
            add_eqv_repn_item(TypeCtor, EqvRepnItem, !Int1RepnMap)
        ;
            StdDefn = std_mer_type_du_subtype(_, _),
            map.lookup(SubtypeMap, TypeCtor, SubtypeRepnItem),
            add_subtype_repn_item(TypeCtor, SubtypeRepnItem, !Int1RepnMap)
        ;
            StdDefn = std_mer_type_du_all_plain_constants(_, _, _, _, _),
            map.lookup(SimpleDuMap, TypeCtor, SimpleDuRepn),
            add_simple_du_repn_item(TypeCtor, SimpleDuRepn, !Int1RepnMap)
        ;
            StdDefn = std_mer_type_du_not_all_plain_constants(_DuStatus,
                DuDefn, MaybeDefnCJCs),
            decide_type_repns_stage_2_du_gen(BaseParams, EqvMap, SubtypeMap,
                WordAlignedTypeCtorsC, SimpleDuMap, TypeCtor,
                DuDefn, MaybeDefnCJCs, !Int1RepnMap, !Specs)
        ;
            StdDefn = std_mer_type_abstract(_AbsStatus, AbsDefn,
                MaybeDefnCJCs),
            decide_type_repns_foreign_defns(MaybeDefnCJCs, RepnCJCs),
            AbsDefn = item_type_defn_info(TypeCtorSymName, TypeParams,
                _, TVarSet, _Context, _SeqNum),
            RepnInfo = item_type_repn_info(TypeCtorSymName, TypeParams,
                tcrepn_foreign(RepnCJCs), TVarSet,
                term.dummy_context_init, item_no_seq_num),
            map.det_insert(TypeCtor, RepnInfo, !Int1RepnMap)
        )
    ).

%---------------------------------------------------------------------------%

:- pred decide_type_repns_stage_2_du_gen(base_params::in, type_eqv_map::in,
    subtype_repn_map::in, set_tree234(type_ctor)::in, simple_du_map::in,
    type_ctor::in, item_type_defn_info_du::in, c_j_cs_maybe_defn::in,
    type_ctor_repn_map::in, type_ctor_repn_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

decide_type_repns_stage_2_du_gen(BaseParams, EqvMap, SubtypeMap,
        WordAlignedTypeCtorsC, SimpleDuMap, TypeCtor, DuDefn, MaybeDefnCJCs,
        !Int1RepnMap, !Specs) :-
    decide_type_repns_foreign_defns(MaybeDefnCJCs, RepnCJCs),
    DuDefn = item_type_defn_info(TypeCtorSymName, TypeParams, DetailsDu,
        TVarSet, _Context, _SeqNum),

    TypeCtor = type_ctor(SymName, Arity),
    list.length(TypeParams, NumTypeParams),
    expect(unify(TypeCtorSymName, SymName), $pred, "sym_name mismatch"),
    expect(unify(NumTypeParams, Arity), $pred, "arity mismatch"),

    DetailsDu = type_details_du(MaybeSuperType, OoMCtors0, MaybeCanonical,
        _MaybeDirectArgs),
    expect(unify(MaybeSuperType, not_a_subtype), $pred, "type is subtype"),
    OoMCtors0 = one_or_more(HeadCtor0, TailCtors0),
    expand_eqv_sub_of_notag_types_in_constructor(EqvMap, SubtypeMap,
        SimpleDuMap, TVarSet, HeadCtor0, HeadCtor, !Specs),
    (
        TailCtors0 = [],
        % The type has exactly one data constructor.
        decide_type_repns_stage_2_du_gen_only_functor(BaseParams,
            SimpleDuMap, TypeCtor, TypeParams, TVarSet, MaybeCanonical,
            HeadCtor, RepnCJCs, !Int1RepnMap)
    ;
        TailCtors0 = [_ | _],
        % The type has two or more data constructors.
        expand_eqv_sub_of_notag_types_in_constructors(EqvMap, SubtypeMap,
            SimpleDuMap, TVarSet, TailCtors0, TailCtors, !Specs),
        decide_type_repns_stage_2_du_gen_more_functors(BaseParams,
            WordAlignedTypeCtorsC, SimpleDuMap, TypeCtor, TypeParams,
            TVarSet, [HeadCtor | TailCtors], RepnCJCs, !Int1RepnMap)
    ).

:- pred decide_type_repns_stage_2_du_gen_only_functor(base_params::in,
    simple_du_map::in, type_ctor::in, list(tvar)::in,
    tvarset::in, maybe_canonical::in, constructor::in, c_j_cs_repn::in,
    type_ctor_repn_map::in, type_ctor_repn_map::out) is det.

decide_type_repns_stage_2_du_gen_only_functor(BaseParams, SimpleDuMap,
        TypeCtor, TypeParams, TVarSet, MaybeCanonical, SingleCtor, RepnCJCs,
        !Int1RepnMap) :-
    SingleCtor = ctor(_Ordinal, MaybeExistConstraints,
        SingleCtorSymName, Args, Arity, _SingleCtorContext),
    ( if
        one_constructor_non_constant_is_notag(MaybeExistConstraints,
            Args, Arity, MaybeCanonical, _Arg)
    then
        map.lookup(SimpleDuMap, TypeCtor, SimpleDuRepn),
        add_simple_du_repn_item(TypeCtor, SimpleDuRepn, !Int1RepnMap)
    else
        PlatformParams64NoSpf = platform_params(word_size_64,
            no_double_word_floats, no_direct_args, BaseParams),
        PlatformParams32NoSpf = platform_params(word_size_32,
            use_double_word_floats, no_direct_args, BaseParams),
        PlatformParams32Spf = platform_params(word_size_32,
            no_double_word_floats, no_direct_args, BaseParams),

        decide_complex_du_only_functor(PlatformParams64NoSpf, SimpleDuMap,
            MaybeExistConstraints, Args, ArgsRepn64NoSpf),
        decide_complex_du_only_functor(PlatformParams32NoSpf, SimpleDuMap,
            MaybeExistConstraints, Args, ArgsRepn32NoSpf),
        decide_complex_du_only_functor(PlatformParams32Spf, SimpleDuMap,
            MaybeExistConstraints, Args, ArgsRepn32Spf),

        SingleCtorName = unqualify_name(SingleCtorSymName),
        % The direct arg optimization is not applicable to types with only
        % a single functor, so the representations with direct args
        % allowed and non-allowed are always the same.
        CRepns = make_c_repns_no_da(ArgsRepn64NoSpf,
            ArgsRepn32NoSpf, ArgsRepn32Spf),
        ArgTypes = list.map((func(CA) = CA ^ arg_type), Args),
        OnlyFunctorRepn = gen_du_only_functor_repn(SingleCtorName,
            ArgTypes, CRepns, RepnCJCs),
        DuRepn = dur_gen_only_functor(OnlyFunctorRepn),
        TypeCtor = type_ctor(TypeCtorSymName, _Arity),
        RepnInfo = item_type_repn_info(TypeCtorSymName, TypeParams,
            tcrepn_du(DuRepn), TVarSet, term.dummy_context_init,
            item_no_seq_num),
        map.det_insert(TypeCtor, RepnInfo, !Int1RepnMap)
    ).

%---------------------------------------------------------------------------%

:- pred decide_complex_du_only_functor(platform_params::in, simple_du_map::in,
    maybe_cons_exist_constraints::in, list(constructor_arg)::in,
    only_nonconstant_repn::out) is det.

decide_complex_du_only_functor(PlatformParams, SimpleDuMap,
        MaybeExistConstraints, Args, NonConstantRepn) :-
    % Can we pack all the args into a word?
    classify_args(PlatformParams, SimpleDuMap, Args, ClassifiedArgs),
    ( if
        MaybeExistConstraints = no_exist_constraints,
        PlatformParams ^ pp_base_params ^ bp_allow_packing_local_sectags = yes,
        are_all_args_packable(ClassifiedArgs, PackableArgs,
            0u, NumPackableBits),
        WordSize = PlatformParams ^ pp_word_size,
        NumPackableBits =< word_size_num_word_bits(WordSize)
    then
        decide_complex_du_only_functor_local_args(PlatformParams, PackableArgs,
            NonConstantRepn)
    else
        decide_complex_du_only_functor_remote_args(PlatformParams,
            MaybeExistConstraints, ClassifiedArgs, NonConstantRepn)
    ).

:- pred decide_complex_du_only_functor_local_args(platform_params::in,
    list(packable_arg)::in, only_nonconstant_repn::out) is det.

decide_complex_du_only_functor_local_args(PlatformParams, PackableArgs,
        NonConstantRepn) :-
    WordSize = PlatformParams ^ pp_word_size,
    NumWordBits = word_size_num_word_bits(WordSize),
    NumPtagBits = word_size_num_ptag_bits(WordSize),
    NumPrefixBits = NumPtagBits,
    decide_local_packed_arg_word_loop(NumPrefixBits, NextShift,
        PackableArgs, ArgRepns),
    expect(NextShift =< NumWordBits, $pred, "NextShift > NumWordBits"),
    det_list_to_one_or_more(ArgRepns, OoMArgRepns),
    LocalRepn = only_nonconstant_local_cell_repn(OoMArgRepns),
    NonConstantRepn = oncr_local_cell(LocalRepn).

:- pred decide_complex_du_only_functor_remote_args(platform_params::in,
    maybe_cons_exist_constraints::in, list(classified_arg)::in,
    only_nonconstant_repn::out) is det.

decide_complex_du_only_functor_remote_args(PlatformParams,
        MaybeExistConstraints, ClassifiedArgs, NonConstantRepn) :-
    NumRemoteSectagBits = 0u8,
    decide_remote_args(PlatformParams, NumRemoteSectagBits,
        MaybeExistConstraints, ClassifiedArgs, ArgRepns),
    det_list_to_one_or_more(ArgRepns, OoMArgRepns),
    RemoteRepn = only_nonconstant_remote_cell_repn(OoMArgRepns),
    NonConstantRepn = oncr_remote_cell(RemoteRepn).

%---------------------------------------------------------------------------%

:- pred decide_type_repns_stage_2_du_gen_more_functors(base_params::in,
    set_tree234(type_ctor)::in, simple_du_map::in,
    type_ctor::in, list(tvar)::in, tvarset::in,
    list(constructor)::in, c_j_cs_repn::in,
    type_ctor_repn_map::in, type_ctor_repn_map::out) is det.

decide_type_repns_stage_2_du_gen_more_functors(BaseParams,
        WordAlignedTypeCtorsC, SimpleDuMap, TypeCtor, TypeParams, TVarSet,
        Ctors, RepnCJCs, !Int1RepnMap) :-
    PlatformParams64NoSpfNoDa = platform_params(word_size_64,
        no_double_word_floats, no_direct_args, BaseParams),
    PlatformParams64NoSpfDa = platform_params(word_size_64,
        no_double_word_floats, use_direct_args, BaseParams),
    PlatformParams32NoSpfNoDa = platform_params(word_size_32,
        use_double_word_floats, no_direct_args, BaseParams),
    PlatformParams32NoSpfDa = platform_params(word_size_32,
        use_double_word_floats, use_direct_args, BaseParams),
    PlatformParams32SpfNoDa = platform_params(word_size_32,
        no_double_word_floats, no_direct_args, BaseParams),
    PlatformParams32SpfDa = platform_params(word_size_32,
        no_double_word_floats, use_direct_args, BaseParams),

    decide_gen_du_functors(PlatformParams64NoSpfNoDa, WordAlignedTypeCtorsC,
        SimpleDuMap, Ctors, RepnMap64NoSpfNoDa),
    decide_gen_du_functors(PlatformParams64NoSpfDa, WordAlignedTypeCtorsC,
        SimpleDuMap, Ctors, RepnMap64NoSpfDa),
    decide_gen_du_functors(PlatformParams32NoSpfNoDa, WordAlignedTypeCtorsC,
        SimpleDuMap, Ctors, RepnMap32NoSpfNoDa),
    decide_gen_du_functors(PlatformParams32NoSpfDa, WordAlignedTypeCtorsC,
        SimpleDuMap, Ctors, RepnMap32NoSpfDa),
    decide_gen_du_functors(PlatformParams32SpfNoDa, WordAlignedTypeCtorsC,
        SimpleDuMap, Ctors, RepnMap32SpfNoDa),
    decide_gen_du_functors(PlatformParams32SpfDa, WordAlignedTypeCtorsC,
        SimpleDuMap, Ctors, RepnMap32SpfDa),

    look_up_functor_repns(RepnMap64NoSpfNoDa, RepnMap64NoSpfDa,
        RepnMap32NoSpfNoDa, RepnMap32NoSpfDa, RepnMap32SpfNoDa, RepnMap32SpfDa,
        Ctors, CRepns),
    (
        CRepns = [],
        unexpected($pred, "CRepns has length 0")
    ;
        CRepns = [_],
        unexpected($pred, "CRepns has length 1")
    ;
        CRepns = [CRepn1, CRepn2 | CRepn3plus]
    ),

    MoreFunctorsRepn = gen_du_more_functors_repn(CRepn1, CRepn2, CRepn3plus,
        RepnCJCs),
    DuRepn = dur_gen_more_functors(MoreFunctorsRepn),
    TypeCtor = type_ctor(TypeCtorSymName, _Arity),
    RepnInfo = item_type_repn_info(TypeCtorSymName, TypeParams,
        tcrepn_du(DuRepn), TVarSet, term.dummy_context_init, item_no_seq_num),
    map.det_insert(TypeCtor, RepnInfo, !Int1RepnMap).

:- pred decide_gen_du_functors(platform_params::in, set_tree234(type_ctor)::in,
    simple_du_map::in, list(constructor)::in, ctor_repn_map::out) is det.

decide_gen_du_functors(PlatformParams, WordAlignedTypeCtorsC, SimpleDuMap,
        Ctors, !:RepnMap) :-
    separate_out_constants(PlatformParams, SimpleDuMap, Ctors,
        Constants, Functors),
    MaybeDirectArgs = PlatformParams ^ pp_direct_args,
    (
        MaybeDirectArgs = no_direct_args,
        DirectArgFunctors = [],
        NonDirectArgFunctors = Functors
    ;
        MaybeDirectArgs = use_direct_args,
        list.filter(
            is_direct_arg_ctor_for_c(WordAlignedTypeCtorsC),
            Functors, DirectArgFunctors, NonDirectArgFunctors)
    ),
    list.length(Constants, NumConstants),
    compute_local_packable_functors(PlatformParams, SimpleDuMap,
        NumConstants, NonDirectArgFunctors,
        LocalPackedFunctors, NonLocalPackedFunctors),
    some [!CurPtagUint8] (
        map.init(!:RepnMap),
        ( if
            Constants = [],
            LocalPackedFunctors = []
        then
            !:CurPtagUint8 = 0u8
        else
            % We allocate ptag 0 to be used by Constants and/or
            % LocalPackedFunctors.
            CurLocalSectag0 = 0u,
            (
                LocalPackedFunctors = [],
                num_bits_needed_for_n_things(NumConstants,
                    NumLocalSectagBitsUint),
                NumLocalSectagBits =
                    uint8.det_from_uint(NumLocalSectagBitsUint),
                LocalSectagSize = lsectag_rest_of_word(NumLocalSectagBits),
                assign_repns_to_constants(LocalSectagSize,
                    CurLocalSectag0, _CurLocalSectag, Constants, !RepnMap)
            ;
                LocalPackedFunctors = [_ | _],
                list.length(LocalPackedFunctors, NumLocalPackedFunctors),
                num_bits_needed_for_n_things(
                    NumConstants + NumLocalPackedFunctors,
                    NumLocalSectagBitsUint),
                NumLocalSectagBits =
                    uint8.det_from_uint(NumLocalSectagBitsUint),
                LocalSectagSize = lsectag_part_of_word(NumLocalSectagBits),
                assign_repns_to_constants(LocalSectagSize,
                    CurLocalSectag0, CurLocalSectag1, Constants, !RepnMap),
                assign_repns_to_local_packed_functors(PlatformParams,
                    NumLocalSectagBits, CurLocalSectag1, LocalPackedFunctors,
                    !RepnMap)
            ),
            !:CurPtagUint8 = 1u8
        ),
        WordSize = PlatformParams ^ pp_word_size,
        ( WordSize = word_size_32, MaxPtagUint8 = 3u8
        ; WordSize = word_size_64, MaxPtagUint8 = 7u8
        ),
        assign_repns_to_direct_arg_functors(MaxPtagUint8,
            !CurPtagUint8, DirectArgFunctors, NonDirectArgFunctors,
            LeftOverDirectArgFunctors, !RepnMap),
        RemoteFunctors = LeftOverDirectArgFunctors ++ NonLocalPackedFunctors, 
        assign_repns_to_remote_unshared(PlatformParams, MaxPtagUint8,
            !.CurPtagUint8, RemoteFunctors, RemoteSharedFunctors, !RepnMap),
        (
            RemoteSharedFunctors = []
        ;
            RemoteSharedFunctors = [_ | _],
            list.length(RemoteSharedFunctors, NumRemoteSharedFunctors),
            num_bits_needed_for_n_things(NumRemoteSharedFunctors,
                NumRemoteSectagBitsUint),
            NumRemoteSectagBits = uint8.det_from_uint(NumRemoteSectagBitsUint),
            MaxPtag = ptag(MaxPtagUint8),
            assign_repns_to_remote_shared(PlatformParams,
                NumRemoteSectagBits, 0u, RemoteSharedFunctors, RSIs,
                need_not_mask_remote_sectag, MustMask),
            (
                MustMask = need_not_mask_remote_sectag,
                RSectagSize = rsectag_full_word
            ;
                MustMask = must_mask_remote_sectag,
                RSectagSize = rsectag_part_of_word(NumRemoteSectagBits)
            ),
            add_remote_shared_functors_to_repn_map(MaxPtag, RSectagSize, RSIs,
                !RepnMap)
        )
    ).

:- type packable_constructor
    --->    packable_constructor(
                pc_ctor             :: constructor,
                pc_packable_args    :: list(packable_arg),
                pc_num_bits         :: uint
            ).

    % compute_local_packable_functors(PlatformParams, SimpleDuMap,
    %   NumConstants, Functors, PackedFunctors, NonPackedFunctors) :-
    %
    % Given how many constants a type has and a list of its nonconstant
    % functors, find out which (if any) of the nonconstant functors
    % can be represented *without* a heap cell, by packing the values
    % of their arguments next to the primary tag and any local secondary tag.
    % The local secondary tag may occupy zero bits, if there are no constants
    % and just one functor that can be packed this way.
    %
:- pred compute_local_packable_functors(platform_params::in, simple_du_map::in,
    int::in, list(classified_constructor)::in,
    list(packable_constructor)::out, list(classified_constructor)::out) is det.

compute_local_packable_functors(PlatformParams, SimpleDuMap,
        NumConstants, Functors, PackedFunctors, NonPackedFunctors) :-
    MayPackLocalSectags =
        PlatformParams ^ pp_base_params ^ bp_allow_packing_local_sectags,
    WordSize = PlatformParams ^ pp_word_size,
    ( WordSize = word_size_32, NumPtagBits = 2u, NumWordBits = 32u
    ; WordSize = word_size_64, NumPtagBits = 3u, NumWordBits = 64u
    ),
    (
        MayPackLocalSectags = yes,
        separate_out_local_sectag_packable(PlatformParams, NumWordBits,
            SimpleDuMap, Functors, SizedPackableFunctors, NonPackableFunctors)
    ;
        MayPackLocalSectags = no,
        SizedPackableFunctors = [],
        NonPackableFunctors = Functors
    ),
    (
        SizedPackableFunctors = [],
        PackedFunctors = [],
        NonPackedFunctors = Functors
    ;
        SizedPackableFunctors = [_ | _],
        list.sort(compare_packable_ctors_by_numbits_then_ordinal,
            SizedPackableFunctors, SortedSizedPackableFunctors),
        list.det_last(SortedSizedPackableFunctors, LastPackableFunctor),
        LastPackableFunctor = packable_constructor(_, _, MaxPackableBits),
        list.length(SizedPackableFunctors, NumPackable),
        num_bits_needed_for_n_things(NumConstants + NumPackable,
            NumSectagBitsCP),
        trace [io(!IO), compile_time(flag("decide_type_layout"))] (
            Stream = io.stdout_stream,
            io.write_string(Stream, "\nsized packable functors:\n", !IO),
            list.foldl(
                output_sized_packable_functor(Stream,
                    yes({PlatformParams, SimpleDuMap})),
                SizedPackableFunctors, !IO),
            io.write_string(Stream, "NumPtagBits: ", !IO),
            io.write_line(Stream, NumPtagBits, !IO),
            io.write_string(Stream, "NumSectagBitsCP: ", !IO),
            io.write_line(Stream, NumSectagBitsCP, !IO),
            io.write_string(Stream, "MaxPackableBits: ", !IO),
            io.write_line(Stream, MaxPackableBits, !IO),
            io.write_string(Stream, "NumWordBits: ", !IO),
            io.write_line(Stream, NumWordBits, !IO)
        ),
        ( if
            NumPtagBits + NumSectagBitsCP + MaxPackableBits =< NumWordBits
        then
            % We can pack all of PackableFunctors with their local sectag.
            PackedFunctors = SizedPackableFunctors,
            NonPackedFunctors = NonPackableFunctors
        else
            % We *cannot* pack all of PackableFunctors with their
            % local sectag, either because some require too many bits
            % on their own, or because there are too many such functors,
            % requiring too many sectag bits to distinguish them, or both.
            %
            % Our objective is to assign local sectags to as many
            % PackableFunctors as can. This requires giving preference
            % to PackableFunctors that take up the least number of bits
            % themselves. For example, on a 64 bit platform where
            % the primary tag is 3 bits, if we have one constant,
            % then we could pack that constant together with either
            %
            % - just one PackableFunctor that occupies 60 bits
            %   (since distinguishing one constant and two nonconstants
            %   would require two sectag bits, and 3 + 2 + 60 > 64), or
            %   (for example)
            %
            % - 15 PackableFunctors that each occupy up to 57 bits
            %   (because distinguishing them from the constant and each other
            %   requires 4 sectag bits, and 3 + 4 + 57 =< 64).
            %
            % This is why we try to pack nonconstant functors in increasing
            % order of how many bits they need for just the arguments.
            %
            % We start by computing the number of sectag bits needed
            % for just the constants, and seeing what nonconstant functors
            % we can pack using just that number of sectag bits. We then
            % try to see if we can pack in more function symbols with
            % one more sectag bit. We keep increasing the number of sectag bits
            % until we can pack in no more.

            num_bits_needed_for_n_things(NumConstants, NumSectagBitsC),
            NumSectagValues0 = 1 << uint.cast_to_int(NumSectagBitsC),
            TakeLimit0 = NumSectagValues0 - NumConstants,
            take_local_packable_functors_constant_sectag_bits(NumWordBits,
                NumPtagBits, NumSectagBitsC, TakeLimit0, 0, _NumTaken,
                SortedSizedPackableFunctors,
                [], RevSizedPackedFunctors0, SizedNonPackedFunctors0),
            % NumTaken may be 0 because TakeLimit0 was 0.
            take_local_packable_functors_incr_sectag_bits(NumWordBits,
                NumPtagBits, NumSectagBitsC,
                RevSizedPackedFunctors0, SizedNonPackedFunctors0,
                RevSizedPackedFunctors, SizedNonPackedFunctors),
            RevPackedFunctors = RevSizedPackedFunctors,
            NonPackedPackableFunctors =
                list.map(packable_to_classified_constructor,
                    SizedNonPackedFunctors),
            list.sort(compare_packable_ctors_by_ordinal,
                RevPackedFunctors, PackedFunctors),
            % The sorting operates on the first arguments first,
            % which is the functor's ordinal number.
            list.sort(NonPackableFunctors ++ NonPackedPackableFunctors,
                NonPackedFunctors)
        )
    ).

:- func packable_to_classified_constructor(packable_constructor)
    = classified_constructor.

packable_to_classified_constructor(PackableCtor) = ClassifiedCtor :-
    PackableCtor = packable_constructor(Ctor, PackableArgs, _NumBits),
    ClassifiedArgs = list.map(packable_to_classified_arg, PackableArgs),
    ClassifiedCtor = classified_constructor(Ctor, ClassifiedArgs).

:- func packable_to_classified_arg(packable_arg) = classified_arg.

packable_to_classified_arg(PackableArg) = ClassifiedArg :-
    PackableArg = packable_arg(Arg, PackClass),
    ( PackClass = apc_pack(FillKindSize), Class = ac_pack(FillKindSize)
    ; PackClass = apc_dummy, Class = ac_dummy
    ),
    ClassifiedArg = classified_arg(Arg, Class).

:- pred separate_out_local_sectag_packable(platform_params::in, uint::in,
    simple_du_map::in, list(classified_constructor)::in,
    list(packable_constructor)::out, list(classified_constructor)::out) is det.

separate_out_local_sectag_packable(_, _, _, [], [], []).
separate_out_local_sectag_packable(PlatformParams, Limit, SimpleDuMap,
        [ClassifiedCtor | ClassifiedCtors], Packables, NonPackables) :-
    separate_out_local_sectag_packable(PlatformParams, Limit, SimpleDuMap,
        ClassifiedCtors, PackablesTail, NonPackablesTail),
    ClassifiedCtor = classified_constructor(Ctor, ClassifiedArgs),
    ( if
        ctor_has_all_packable_args_within_limit(Ctor, ClassifiedArgs, Limit,
            PackableArgs, NumBits)
    then
        PackableCtor = packable_constructor(Ctor, PackableArgs, NumBits),
        Packables = [PackableCtor | PackablesTail],
        NonPackables = NonPackablesTail
    else
        Packables = PackablesTail,
        NonPackables = [ClassifiedCtor | NonPackablesTail]
    ).

:- pred ctor_has_all_packable_args_within_limit(constructor::in,
    list(classified_arg)::in, uint::in, list(packable_arg)::out, uint::out)
    is semidet.

ctor_has_all_packable_args_within_limit(Ctor, ClassifiedArgs,
        Limit, PackableArgs, NumBits) :-
    Ctor ^ cons_maybe_exist = no_exist_constraints,
    find_initial_packable_args_within_limit(Limit, 0u, NumBits,
        ClassifiedArgs, PackableArgs, LeftOverArgs),
    LeftOverArgs = [].

%---------------------%

:- pred take_local_packable_functors_incr_sectag_bits(uint::in,
    uint::in, uint::in,
    list(packable_constructor)::in, list(packable_constructor)::in,
    list(packable_constructor)::out, list(packable_constructor)::out) is det.

take_local_packable_functors_incr_sectag_bits(NumWordBits, NumPtagBits,
        NumSectagBits0,
        RevPackedFunctors0, NonPackedFunctors0,
        RevPackedFunctors, NonPackedFunctors) :-
    trace [io(!IO), compile_time(flag("decide_type_layout"))] (
        Stream = io.stdout_stream,
        io.write_string(Stream, "\nstart of incr_sectag_bits:\n", !IO),
        io.write_string(Stream, "RevPackedFunctors0:\n", !IO),
        list.foldl(output_sized_packable_functor(Stream, maybe.no),
            RevPackedFunctors0, !IO),
        io.write_string(Stream, "NumWordBits: ", !IO),
        io.write_line(Stream, NumWordBits, !IO),
        io.write_string(Stream, "NumPtagBits: ", !IO),
        io.write_line(Stream, NumPtagBits, !IO),
        io.write_string(Stream, "NumSectagBits0: ", !IO),
        io.write_line(Stream, NumSectagBits0, !IO)
    ),
    ( if
        (
            RevPackedFunctors0 = []
        ;
            RevPackedFunctors0 =
                [packable_constructor(_, _, MaxPackableBits) | _],
            NumPtagBits + NumSectagBits0 + MaxPackableBits + 1u =< NumWordBits
        )
    then
        NumSectagBits1 = NumSectagBits0 + 1u,
        TakeLimit =
            (1 << uint.cast_to_int(NumSectagBits1)) -
            (1 << uint.cast_to_int(NumSectagBits0)),
        take_local_packable_functors_constant_sectag_bits(NumWordBits,
            NumPtagBits, NumSectagBits1, TakeLimit, 0, NumTaken,
            NonPackedFunctors0,
            RevPackedFunctors0, RevPackedFunctors1, NonPackedFunctors1),
        ( if NumTaken > 0 then
            take_local_packable_functors_incr_sectag_bits(NumWordBits,
                NumPtagBits, NumSectagBits1,
                RevPackedFunctors1, NonPackedFunctors1,
                RevPackedFunctors, NonPackedFunctors)
        else
            RevPackedFunctors = RevPackedFunctors0,
            NonPackedFunctors = NonPackedFunctors0
        )
    else
        RevPackedFunctors = RevPackedFunctors0,
        NonPackedFunctors = NonPackedFunctors0
    ).

:- pred take_local_packable_functors_constant_sectag_bits(uint::in,
    uint::in, uint::in, int::in, int::in, int::out,
    list(packable_constructor)::in,
    list(packable_constructor)::in, list(packable_constructor)::out,
    list(packable_constructor)::out) is det.

take_local_packable_functors_constant_sectag_bits(_, _, _, _, !NumTaken, [],
        !RevPackedFunctors, []).
take_local_packable_functors_constant_sectag_bits(NumWordBits,
        PtagBits, SectagBits, TakeLimit, !NumTaken,
        [PackableFunctor | PackableFunctors],
        !RevPackedFunctors, NonPackedFunctors) :-
    PackableFunctor = packable_constructor(_Ctor, _PackableArgs, PackableBits),
    trace [io(!IO), compile_time(flag("decide_type_layout"))] (
        Stream = io.stdout_stream,
        io.write_string(Stream, "\nconstant_sectag_bits test:\n", !IO),
        io.write_string(Stream, "PackableFunctor: ", !IO),
        output_sized_packable_functor(Stream, maybe.no, PackableFunctor, !IO),
        io.write_string(Stream, "NumWordBits: ", !IO),
        io.write_line(Stream, NumWordBits, !IO),
        io.write_string(Stream, "PtagBits: ", !IO),
        io.write_line(Stream, PtagBits, !IO),
        io.write_string(Stream, "SectagBits: ", !IO),
        io.write_line(Stream, SectagBits, !IO),
        io.write_string(Stream, "TakeLimit: ", !IO),
        io.write_line(Stream, TakeLimit, !IO)
    ),
    ( if
        TakeLimit > 0,
        PtagBits + SectagBits + PackableBits =< NumWordBits
    then
        trace [io(!IO), compile_time(flag("decide_type_layout"))] (
            Stream = io.stdout_stream,
            io.write_string(Stream, "TAKEN\n", !IO)
        ),
        !:RevPackedFunctors = [PackableFunctor | !.RevPackedFunctors],
        !:NumTaken = !.NumTaken + 1,
        take_local_packable_functors_constant_sectag_bits(NumWordBits,
            PtagBits, SectagBits, TakeLimit - 1, !NumTaken, PackableFunctors,
            !RevPackedFunctors, NonPackedFunctors)
    else
        trace [io(!IO), compile_time(flag("decide_type_layout"))] (
            Stream = io.stdout_stream,
            io.write_string(Stream, "NOT TAKEN\n", !IO)
        ),
        NonPackedFunctors = [PackableFunctor | PackableFunctors]
    ).

%---------------------%

:- pred compare_packable_ctors_by_numbits_then_ordinal(
    packable_constructor::in, packable_constructor::in,
    comparison_result::out) is det.

compare_packable_ctors_by_numbits_then_ordinal(PackableCtorA, PackableCtorB,
        Result) :-
    PackableCtorA = packable_constructor(CtorA, _PackableArgsA, NumBitsA),
    PackableCtorB = packable_constructor(CtorB, _PackableArgsB, NumBitsB),
    compare(NumBitsResult, NumBitsA, NumBitsB),
    (
        ( NumBitsResult = (<)
        ; NumBitsResult = (>)
        ),
        Result = NumBitsResult
    ;
        NumBitsResult = (=),
        % Keep the sort stable.
        OrdinalA = CtorA ^ cons_ordinal,
        OrdinalB = CtorB ^ cons_ordinal,
        compare(Result, OrdinalA, OrdinalB)
    ).

:- pred compare_packable_ctors_by_ordinal(packable_constructor::in,
packable_constructor::in,
    comparison_result::out) is det.

compare_packable_ctors_by_ordinal(PackableCtorA, PackableCtorB, Result) :-
    PackableCtorA = packable_constructor(CtorA, _PackableArgsA, _NumBitsA),
    PackableCtorB = packable_constructor(CtorB, _PackableArgsB, _NumBitsB),
    OrdinalA = CtorA ^ cons_ordinal,
    OrdinalB = CtorB ^ cons_ordinal,
    compare(Result, OrdinalA, OrdinalB).

%---------------------%

:- type ctor_repn
    --->    ctor_constant(constant_repn)
    ;       ctor_nonconstant(more_nonconstant_repn).

    % Map from a functor's ordinal number to its representation.
:- type ctor_repn_map == map(uint32, ctor_repn).

%---------------------%

:- pred look_up_functor_repns(ctor_repn_map::in, ctor_repn_map::in,
    ctor_repn_map::in, ctor_repn_map::in, ctor_repn_map::in, ctor_repn_map::in,
    list(constructor)::in, list(gen_du_functor_repn)::out) is det.

look_up_functor_repns(_, _, _, _, _, _, [], []).
look_up_functor_repns(RepnMap64NoSpfNoDa, RepnMap64NoSpfDa,
        RepnMap32NoSpfNoDa, RepnMap32NoSpfDa, RepnMap32SpfNoDa, RepnMap32SpfDa,
        [Ctor | Ctors], [Repn | Repns]) :-
    Ctor = ctor(Ordinal, _MaybeExistConstraints, SymName, Args, _NumArgs,
        _Context),
    Name = unqualify_name(SymName),
    map.lookup(RepnMap64NoSpfNoDa, Ordinal, Repn64NoSpfNoDa),
    map.lookup(RepnMap64NoSpfDa,   Ordinal, Repn64NoSpfDa),
    map.lookup(RepnMap32NoSpfNoDa, Ordinal, Repn32NoSpfNoDa),
    map.lookup(RepnMap32NoSpfDa,   Ordinal, Repn32NoSpfDa),
    map.lookup(RepnMap32SpfNoDa,   Ordinal, Repn32SpfNoDa),
    map.lookup(RepnMap32SpfDa,     Ordinal, Repn32SpfDa),
    ( if
        Args = [],
        Ctor ^ cons_maybe_exist = no_exist_constraints
    then
        ConstantCRepns = make_c_repns(
            get_constant_repn(Repn64NoSpfNoDa),
            get_constant_repn(Repn64NoSpfDa),
            get_constant_repn(Repn32NoSpfNoDa),
            get_constant_repn(Repn32NoSpfDa),
            get_constant_repn(Repn32SpfNoDa),
            get_constant_repn(Repn32SpfDa)),
        Repn = gen_du_constant_functor_repn(Name, ConstantCRepns)
    else
        ArgTypes = list.map((func(CA) = CA ^ arg_type), Args),
        NonConstantCRepns = make_c_repns(
            get_nonconstant_repn(Repn64NoSpfNoDa),
            get_nonconstant_repn(Repn64NoSpfDa),
            get_nonconstant_repn(Repn32NoSpfNoDa),
            get_nonconstant_repn(Repn32NoSpfDa),
            get_nonconstant_repn(Repn32SpfNoDa),
            get_nonconstant_repn(Repn32SpfDa)),
        Repn = gen_du_nonconstant_functor_repn(Name, ArgTypes,
            NonConstantCRepns)
    ),
    look_up_functor_repns(RepnMap64NoSpfNoDa, RepnMap64NoSpfDa,
        RepnMap32NoSpfNoDa, RepnMap32NoSpfDa, RepnMap32SpfNoDa, RepnMap32SpfDa,
        Ctors, Repns).

:- func get_constant_repn(ctor_repn) = constant_repn.

get_constant_repn(ctor_constant(ConstantRepn)) = ConstantRepn.
get_constant_repn(ctor_nonconstant(_)) = _ :-
    unexpected($pred, "not constant").

:- func get_nonconstant_repn(ctor_repn) = more_nonconstant_repn.

get_nonconstant_repn(ctor_constant(_)) = _ :-
    unexpected($pred, "not nonconstant").
get_nonconstant_repn(ctor_nonconstant(NonConstantRepn)) = NonConstantRepn.

%---------------------%

:- pred assign_repns_to_constants(lsectag_word_or_size::in,
    uint::in, uint::out, list(constructor)::in,
    ctor_repn_map::in, ctor_repn_map::out) is det.

assign_repns_to_constants(_, !CurSectag, [], !RepnMap).
assign_repns_to_constants(SectagSize, !CurSectag, [Ctor | Ctors], !RepnMap) :-
    Ctor = ctor(Ordinal, _MaybeExistConstraints, _SymName, _Args, NumArgs,
        _Context),
    expect(unify(NumArgs, 0), $pred, "NumArgs != 0"),
    Repn = constant_repn(!.CurSectag, SectagSize),
    map.det_insert(Ordinal, ctor_constant(Repn), !RepnMap),
    !:CurSectag = !.CurSectag + 1u,
    assign_repns_to_constants(SectagSize, !CurSectag, Ctors, !RepnMap).

%---------------------%

:- pred assign_repns_to_local_packed_functors(platform_params::in,
    uint8::in, uint::in, list(packable_constructor)::in,
    ctor_repn_map::in, ctor_repn_map::out) is det.

assign_repns_to_local_packed_functors(_, _, _, [], !RepnMap).
assign_repns_to_local_packed_functors(PlatformParams, NumLocalSectagBits,
        !.CurSectag, [PackableCtor | PackableCtors], !RepnMap) :-
    PackableCtor = packable_constructor(Ctor, PackableArgs, NumBits),
    Ctor = ctor(Ordinal, _MaybeExistConstraints, _SymName, _Args, NumArgs,
        _Context),
    expect_not(unify(NumArgs, 0), $pred, "NumArgs != 0"),
    Sectag = cell_local_sectag(!.CurSectag, NumLocalSectagBits),
    WordSize = PlatformParams ^ pp_word_size,
    NumPtagBits = word_size_num_ptag_bits(WordSize),
    NumPrefixBits = NumPtagBits + uint8.cast_to_uint(NumLocalSectagBits),
    decide_local_packed_arg_word_loop(NumPrefixBits, NextShift,
        PackableArgs, ArgRepns),
    expect(unify(NextShift, NumPrefixBits + NumBits), $pred,
        "NextShift != NumPrefixBits + NumBits"),
    det_list_to_one_or_more(ArgRepns, OoMArgRepns),
    LocalRepn = more_nonconstant_local_cell_repn(Sectag, OoMArgRepns),
    Repn = mncr_local_cell(LocalRepn),
    map.det_insert(Ordinal, ctor_nonconstant(Repn), !RepnMap),
    !:CurSectag = !.CurSectag + 1u,
    assign_repns_to_local_packed_functors(PlatformParams, NumLocalSectagBits,
        !.CurSectag, PackableCtors, !RepnMap).

%---------------------%

:- pred assign_repns_to_direct_arg_functors(uint8::in, uint8::in, uint8::out,
    list(classified_constructor)::in, list(classified_constructor)::in,
    list(classified_constructor)::out,
    ctor_repn_map::in, ctor_repn_map::out) is det.

assign_repns_to_direct_arg_functors(_, !CurPtag, [], _, [], !RepnMap).
assign_repns_to_direct_arg_functors(MaxPtagUint8, !CurPtagUint8,
        [ClassifiedDirectArgCtor | ClassifiedDirectArgCtors],
        ClassifiedNonDirectArgCtors, LeftOverCtors, !RepnMap) :-
    ClassifiedDirectArgCtor = classified_constructor(DirectArgCtor, _),
    DirectArgCtor = ctor(Ordinal, _MaybeExistConstraints, _SymName, _Args,
        NumArgs, _Context),
    expect(unify(NumArgs, 1), $pred, "NumArgs != 1"),
    ( if
        % If we are about to run out of unshared tags, stop, and return
        % the leftovers.
        !.CurPtagUint8 = MaxPtagUint8,
        ( ClassifiedDirectArgCtors = [_ | _]
        ; ClassifiedNonDirectArgCtors = [_ | _]
        )
    then
        LeftOverCtors = [ClassifiedDirectArgCtor | ClassifiedDirectArgCtors]
    else
        Ptag = ptag(!.CurPtagUint8),
        Repn = mncr_direct_arg(Ptag),
        map.det_insert(Ordinal, ctor_nonconstant(Repn), !RepnMap),
        !:CurPtagUint8 = !.CurPtagUint8 + 1u8,
        assign_repns_to_direct_arg_functors(MaxPtagUint8, !CurPtagUint8,
            ClassifiedDirectArgCtors, ClassifiedNonDirectArgCtors,
            LeftOverCtors, !RepnMap)
    ).

%---------------------%

:- pred assign_repns_to_remote_unshared(platform_params::in,
    uint8::in, uint8::in,
    list(classified_constructor)::in, list(classified_constructor)::out,
    ctor_repn_map::in, ctor_repn_map::out) is det.

assign_repns_to_remote_unshared(_, _, _, [], [], !RepnMap).
assign_repns_to_remote_unshared(PlatformParams, MaxPtagUint8, !.CurPtagUint8,
        [ClassifiedCtor | ClassifiedCtors], LeftOverCtors, !RepnMap) :-
    ClassifiedCtor = classified_constructor(Ctor, ClassifiedArgs),
    Ctor = ctor(Ordinal, MaybeExistConstraints, _SymName, _Args, _NumArgs,
        _Context),
    ( if
        % We are about to run out of unshared tags. Don't let this happen
        % if this would leave some Ctors without representation.
        !.CurPtagUint8 = MaxPtagUint8,
        ClassifiedCtors = [_ | _]
    then
        LeftOverCtors = [ClassifiedCtor | ClassifiedCtors]
    else
        Ptag = ptag(!.CurPtagUint8),
        Sectag = cell_remote_no_sectag,
        NumRemoteSectagBits = 0u8,
        decide_remote_args(PlatformParams, NumRemoteSectagBits,
            MaybeExistConstraints, ClassifiedArgs, ArgRepns),
        det_list_to_one_or_more(ArgRepns, OoMArgRepns),
        RemoteRepn =
            more_nonconstant_remote_cell_repn(Ptag, Sectag, OoMArgRepns),
        Repn = mncr_remote_cell(RemoteRepn),
        map.det_insert(Ordinal, ctor_nonconstant(Repn), !RepnMap),
        !:CurPtagUint8 = !.CurPtagUint8 + 1u8,
        assign_repns_to_remote_unshared(PlatformParams, MaxPtagUint8,
            !.CurPtagUint8, ClassifiedCtors, LeftOverCtors, !RepnMap)
    ).

%---------------------%

    % Whether the remote secondary tag occupies the entire first word
    % of the heap cell or just the initial few bits depends on whether
    % any of the functors that share the last ptag value pack any arguments
    % next to the remote sectag.
    %
    % If any do such packing, then code that wants to read the value
    % of the sectag must mask away all the bits that aren't assigned
    % to contain the sectag, since (depending on the value of the sectag)
    % those may contain arguments, and those arguments may contain some
    % nonzero bits.
    %
    % On the other hand, if no functor stores arguments in the tagword,
    % then we will never set the bits beyond the sectag to any nonzero value.
    % This makes the mask operation is unnecessary, and we would prefer to
    % avoid it.
:- type must_mask_remote_sectag
    --->    need_not_mask_remote_sectag
    ;       must_mask_remote_sectag.

    % When we process each functor that shares the max ptag value,
    % we may not know yet whether the sectag can occupy the whole first word
    % of the heap cell. Since the functor's representation needs this info,
    % we can't add it to the !RepnMap yet. Instead, we return all the info
    % in the functor's representation *except* the ptag and the sectag size.
    % That is what this type is for. Once we know the sectag size, we call
    % add_remote_shared_functors_to_repn_map to construct the final
    % representation and add it to !RepnMap. (Since the ptag is the same
    % for all these functors, storing it here is simply not needed.)
:- type remote_shared_info
    --->    remote_shared_info(
                rsi_ordinal             :: uint32,
                rsi_sectag              :: uint,
                rsi_arg_repns           :: one_or_more(remote_arg_repn)
            ).

:- pred assign_repns_to_remote_shared(platform_params::in, uint8::in,
    uint::in, list(classified_constructor)::in,
    list(remote_shared_info)::out,
    must_mask_remote_sectag::in, must_mask_remote_sectag::out) is det.

assign_repns_to_remote_shared(_, _, _, [], [], !MustMask).
assign_repns_to_remote_shared(PlatformParams, NumRemoteSectagBits,
        !.CurRemoteSectag, [ClassifiedCtor | ClassifiedCtors], [RSI | RSIs],
        !MustMask) :-
    ClassifiedCtor = classified_constructor(Ctor, ClassifiedArgs),
    Ctor = ctor(Ordinal, MaybeExistConstraints, _SymName, _Args, _NumArgs,
        _Context),
    decide_remote_args(PlatformParams, NumRemoteSectagBits,
        MaybeExistConstraints, ClassifiedArgs, ArgRepns),
    IsShared = remote_sectag_is_shared_with_args(ArgRepns),
    (
        IsShared = yes,
        !:MustMask = must_mask_remote_sectag
    ;
        IsShared = no
    ),
    det_list_to_one_or_more(ArgRepns, OoMArgRepns),
    RSI = remote_shared_info(Ordinal, !.CurRemoteSectag, OoMArgRepns),
    !:CurRemoteSectag = !.CurRemoteSectag + 1u,
    assign_repns_to_remote_shared(PlatformParams, NumRemoteSectagBits,
        !.CurRemoteSectag, ClassifiedCtors, RSIs, !MustMask).

:- func remote_sectag_is_shared_with_args(list(remote_arg_repn)) = bool.

remote_sectag_is_shared_with_args([ArgRepn | ArgRepns]) = IsShared :-
    (
        ( ArgRepn = remote_full(_, _)
        ; ArgRepn = remote_double(_, _, _)
        ),
        IsShared = no
    ;
        ArgRepn = remote_partial_first(_, _, _, _),
        IsShared = yes
    ;
        ArgRepn = remote_partial_shifted(_, _, _, _),
        % These should occur only *after* a remote_partial_first,
        % after which we stop.
        unexpected($pred, "remote_partial_shifted")
    ;
        ArgRepn = remote_none_shifted(_, _),
        % These should occur only *after* a remote_partial_first,
        % after which we stop.
        unexpected($pred, "remote_none_shifted")
    ;
        ArgRepn = remote_none_nowhere,
        IsShared = remote_sectag_is_shared_with_args(ArgRepns)
    ).
remote_sectag_is_shared_with_args([]) = IsShared :-
    % We use heap cells only if there are arguments. We get here only if
    % the clause above has skipped all of those arguments. Since it skips
    % only remote_none_nowhere ArgRepns, this means that *all* arguments
    % are remote_none_nowhere. Any such functor would be represented
    % *without* a heap cell, and therefore without a remote sectag,
    % if argument packing next to a local sectag were turned on.
    % The question this predicate is answering is relevant only when
    % argument packing next to a remote sectag *is* turned on.
    % Allowing packing next to one kind of sectag but not the other
    % is a situation that should arise only during debugging; in normal
    % usage, the two should be allowed/disallowed together. Since this
    % mismatched configuration itself hasn't really been tested,
    % we return the conservative answer, even though it may leave
    % some performance on the table.
    IsShared = yes.

:- pred add_remote_shared_functors_to_repn_map(ptag::in,
    rsectag_word_or_size::in, list(remote_shared_info)::in,
    ctor_repn_map::in, ctor_repn_map::out) is det.

add_remote_shared_functors_to_repn_map(_, _, [], !RepnMap).
add_remote_shared_functors_to_repn_map(Ptag, SectagSize, [RSI | RSIs],
        !RepnMap) :-
    RSI = remote_shared_info(Ordinal, RemoteSectag, OoMArgRepns),
    Sectag = cell_remote_sectag(RemoteSectag, SectagSize),
    RemoteRepn = more_nonconstant_remote_cell_repn(Ptag, Sectag, OoMArgRepns),
    Repn = mncr_remote_cell(RemoteRepn),
    map.det_insert(Ordinal, ctor_nonconstant(Repn), !RepnMap),
    add_remote_shared_functors_to_repn_map(Ptag, SectagSize, RSIs, !RepnMap).

%---------------------------------------------------------------------------%

:- pred decide_remote_args(platform_params::in, uint8::in,
    maybe_cons_exist_constraints::in, list(classified_arg)::in,
    list(remote_arg_repn)::out) is det.

decide_remote_args(PlatformParams, NumRemoteSectagBits,
        MaybeExistConstraints, MaybePackableArgs, ArgRepns) :-
    % There are two schemes for what a memory cell for a term may look like.
    % In the traditional scheme, the cell consists of, in order:
    %
    % - a word containing the remote secondary tag bits, if this ctor
    %   has a remote secondary tag;
    %
    % - zero or more extra arguments containing typeinfos and/or
    %   typeclass_infos added by polymorphism, if this ctor has one or
    %   more existential constraints (this number should be given by
    %   NumExtraArgWords), and
    %
    % - the words containing the ctor's arguments themselves.
    %
    % In a newer scheme that applies only to functors that have secondary tags
    % but do not have extra arguments containing typeinfos and/or
    % typeclass_infos, the cell consists of:
    %
    % - a word containing the remote secondary tag bits, and
    %   as long a subsequence of the initial subword-sized arguments
    %   as will fit, and
    %
    % - the words containing the rest of the ctor's arguments.
    %
    (
        MaybeExistConstraints = no_exist_constraints,
        NumExtraArgWords = 0
    ;
        MaybeExistConstraints = exist_constraints(ExistConstraints),
        ExistConstraints = cons_exist_constraints(_ExistQTVars, Constraints,
            UnconstrainedExistQTVars, _ConstrainedExistQTVars),
        list.length(UnconstrainedExistQTVars, NumTypeInfos),
        list.length(Constraints, NumTypeClassInfos),
        NumExtraArgWords = NumTypeInfos + NumTypeClassInfos
    ),
    ( if NumRemoteSectagBits = 0u8 then
        NumTagwords = 0,
        TagwordArgRepns = [],
        NonTagwordMaybePackableArgs = MaybePackableArgs
    else
        ( if 
            % Is the optimization of packing args next to the remote sectag
            % allowed?
            PlatformParams ^ pp_base_params ^ bp_allow_packing_remote_sectags
                = yes,
            % We cannot put args next to the remote sectag if we have to put
            % extra typeinfos and/or typeclass_infos between them.
            NumExtraArgWords = 0,

            % If we pass all the above tests, try to find args to pack
            % next to the remote sectag.
            NumWordBits =
                word_size_num_word_bits(PlatformParams ^ pp_word_size),
            Limit = NumWordBits - uint8.cast_to_uint(NumRemoteSectagBits),
            find_initial_packable_args_within_limit(Limit, 0u, _NumBits,
                MaybePackableArgs, PackableArgs, LeftOverArgs),
            % Did we find any?
            PackableArgs = [_ | _]
        then
            % The code of runtime/mercury_ml_expand_body.h has traditionally
            % set the argument vector to point to the first argument, i.e.
            % *past* the word containing the secondary tag (which used to fill
            % the whole of a word). Setting ArgOnlyOffset to -1 for arguments
            % packed next to the sectag allows such code to still work.
            % XXX Eventually, we should redesign the RTTI system to avoid
            % the need for such shenanigans.
            ArgOnlyOffset = arg_only_offset(-1),
            CellOffset = cell_offset(0),
            NumPrefixBits = NumRemoteSectagBits,
            decide_remote_packed_arg_word_loop(treat_as_first_arg,
                ArgOnlyOffset, CellOffset, NumPrefixBits, _CurShift,
                PackableArgs, TagwordArgRepns),
            NonTagwordMaybePackableArgs = LeftOverArgs
        else
            TagwordArgRepns = [],
            NonTagwordMaybePackableArgs = MaybePackableArgs
        ),
        NumTagwords = 1
    ),
    FirstArgWordNum = 0,
    FirstCellWordNum = NumTagwords + NumExtraArgWords,
    decide_remote_arg_words_loop(PlatformParams,
        FirstArgWordNum, FirstCellWordNum,
        NonTagwordMaybePackableArgs, NonTagwordArgRepns),
    ArgRepns = TagwordArgRepns ++ NonTagwordArgRepns.

%---------------------%

    % Find an initial subsequence of arguments that may all be packed together
    % into a word which has NumAvailBits available. Return both this
    % initial subsequence and the rest of the argument list. Make sure that
    % the last packable argument is not of a dummy type; if the list of
    % packable arguments ends with a run of one or more arguments of
    % dummy types, return these as part of the LeftOverArgs.
    %
:- pred find_word_packable_args(platform_params::in,
    uint::in, uint::in, uint::out, list(classified_arg)::in,
    list(packable_arg)::out, list(classified_arg)::out) is det.

find_word_packable_args(_, _, NumUsedBits, NumUsedBits, [], [], []).
find_word_packable_args(PlatformParams, NumAvailBits,
        NumUsedBits0, NumUsedBits,
        [ClassifiedArg | ClassifiedArgs], PackableArgs, LeftOverArgs) :-
    ClassifiedArg = classified_arg(Arg, Class),
    ( if
        (
            Class = ac_pack(FillKindSize),
            ArgNumBits = fill_kind_size_num_bits(FillKindSize),
            HeadPackableArg = packable_arg(Arg, apc_pack(FillKindSize))
        ;
            Class = ac_dummy,
            ArgNumBits = 0u,
            HeadPackableArg = packable_arg(Arg, apc_dummy)
        ),
        NumUsedBits1 = NumUsedBits0 + ArgNumBits,
        NumAvailBits >= NumUsedBits1
    then
        find_word_packable_args(PlatformParams, NumAvailBits,
            NumUsedBits1, NumUsedBits2,
            ClassifiedArgs, TailPackableArgs, TailLeftOverArgs),
        % If we have allocated any bits at all to nondummy values,
        % do not let the PackableArgs list end with one or more dummy args.
        % The recursive application of this principle ensures that
        % if PackableArgs contains any nondummy args at all, then
        % the last element in PackableArgs will be a non-dummy.
        ( if
            NumUsedBits0 > 0u,
            TailPackableArgs = [],
            ArgNumBits = 0u
        then
            NumUsedBits = NumUsedBits0,
            PackableArgs = [],
            LeftOverArgs = [ClassifiedArg | ClassifiedArgs]
        else
            NumUsedBits = NumUsedBits2,
            PackableArgs = [HeadPackableArg | TailPackableArgs],
            LeftOverArgs = TailLeftOverArgs
        )
    else
        NumUsedBits = NumUsedBits0,
        PackableArgs = [],
        LeftOverArgs = [ClassifiedArg | ClassifiedArgs]
    ).

%---------------------%
%
% We have two rules governing how the bit fields of the arguments
% are allocated.
%
% - The bit fields of arguments are allocated in a contiguous region
%   as close to the least-significant end of the word as possible,
%   possibly constrained by the presence of any primary and/o
%   secondary tag bits.
%
% - Given two arguments i and j, if i < j, then argument i will be
%   allocated a bit field containing *more* significant bits than
%   argument j. This means that the argument next to the local secondary
%   tag will be the *last* (non-dummy type) argument of the constructor,
%   not the first.
%
% The reason for the second rule is this allows the automatically generated
% comparison predicate for the type to compare any consecutive sequence of
% two or more arguments to be compared at once, if they all compare
% as unsigned.
%

    % Decide the representations of heap cell arguments *beyond* any
    % initial word containing a remote secondary tag (and maybe an
    % initial subsequence of arguments packed next to it).
    %
:- pred decide_remote_arg_words_loop(platform_params::in, int::in, int::in,
    list(classified_arg)::in, list(remote_arg_repn)::out) is det.

decide_remote_arg_words_loop(_, _, _, [], []).
decide_remote_arg_words_loop(PlatformParams, CurAOWordNum, CurCellWordNum,
        ClassifiedArgs, ArgRepns) :-
    ClassifiedArgs = [HeadClassifiedArg | TailClassifiedArgs],
    ArgOnlyOffset = arg_only_offset(CurAOWordNum),
    CellOffset = cell_offset(CurCellWordNum),
    HeadClassifiedArg = classified_arg(_HeadArg, HeadClass),
    (
        HeadClass = ac_pack(_),
        NumWordBits = word_size_num_word_bits(PlatformParams ^ pp_word_size),
        NumUsedBits0 = 0u,
        find_word_packable_args(PlatformParams, NumWordBits,
            NumUsedBits0, NumUsedBits, ClassifiedArgs,
            PackableArgs, LeftOverClassifiedArgs),
        decide_remote_packed_arg_word_loop(treat_as_first_arg,
            ArgOnlyOffset, CellOffset, 0u8, _NextShift,
            PackableArgs, WordArgRepns),
        ( if NumUsedBits > 0u then
            NextAOWordNum = CurAOWordNum + 1,
            NextCellWordNum = CurCellWordNum + 1
        else
            NextAOWordNum = CurAOWordNum,
            NextCellWordNum = CurCellWordNum
        ),
        decide_remote_arg_words_loop(PlatformParams,
            NextAOWordNum, NextCellWordNum,
            LeftOverClassifiedArgs, TailArgRepns),
        ArgRepns = WordArgRepns ++ TailArgRepns
    ;
        (
            HeadClass = ac_dummy,
            NextAOWordNum = CurAOWordNum,
            NextCellWordNum = CurCellWordNum,
            HeadArgRepn = remote_none_nowhere
        ;
            HeadClass = ac_word,
            HeadArgRepn = remote_full(ArgOnlyOffset, CellOffset),
            NextAOWordNum = CurAOWordNum + 1,
            NextCellWordNum = CurCellWordNum + 1
        ;
            HeadClass = ac_double(DWKind),
            HeadArgRepn = remote_double(ArgOnlyOffset, CellOffset, DWKind),
            NextAOWordNum = CurAOWordNum + 2,
            NextCellWordNum = CurCellWordNum + 2
        ),
        decide_remote_arg_words_loop(PlatformParams,
            NextAOWordNum, NextCellWordNum, TailClassifiedArgs, TailArgRepns),
        ArgRepns = [HeadArgRepn | TailArgRepns]
    ).


%---------------------%

:- type maybe_treat_as_first_arg
    --->    do_not_treat_as_first_arg
    ;       treat_as_first_arg.

    % Assign representations to the arguments packed together into a single
    % word at the offset given by ArgOnlyOffset and CellOffset. This word
    % is *not* a tagword, i.e. it does not contain any remote sectag.
    %
    % We assume that the list of packable_args given to us ends with
    % a nondummy argument. This is intended to be ensured by having our caller
    % compute ArgPackable by calling find_word_packable_args. We do not
    % assume that they *start* with a nondummy argument, but we assign
    % the apw_none_nowhere representation to any dummy in any such initial
    % subsequence.
    %
:- pred decide_remote_packed_arg_word_loop(maybe_treat_as_first_arg::in,
    arg_only_offset::in, cell_offset::in, uint8::in, uint8::out,
    list(packable_arg)::in, list(remote_arg_repn)::out) is det.

decide_remote_packed_arg_word_loop(_, _, _, NumPrefixBits, NextShift,
        [], []) :-
    NextShift = NumPrefixBits.
decide_remote_packed_arg_word_loop(TreatAsFirst, ArgOnlyOffset, CellOffset,
        NumPrefixBits, NextShift,
        [PackableArg | PackableArgs], [ArgRepn | ArgRepns]) :-
    PackableArg = packable_arg(_Arg, PackClass),
    (
        PackClass = apc_dummy,
        decide_remote_packed_arg_word_loop(TreatAsFirst,
            ArgOnlyOffset, CellOffset, NumPrefixBits, CurShift,
            PackableArgs, ArgRepns),
        (
            TreatAsFirst = treat_as_first_arg,
            ArgRepn = remote_none_nowhere
        ;
            TreatAsFirst = do_not_treat_as_first_arg,
            ArgRepn = remote_none_shifted(ArgOnlyOffset, CellOffset)
        ),
        NextShift = CurShift
    ;
        PackClass = apc_pack(FillKindSize),
        decide_remote_packed_arg_word_loop(do_not_treat_as_first_arg,
            ArgOnlyOffset, CellOffset, NumPrefixBits, CurShift,
            PackableArgs, ArgRepns),
        (
            TreatAsFirst = treat_as_first_arg,
            ArgRepn = remote_partial_first(ArgOnlyOffset, CellOffset,
                CurShift, FillKindSize)
        ;
            TreatAsFirst = do_not_treat_as_first_arg,
            ArgRepn = remote_partial_shifted(ArgOnlyOffset, CellOffset,
                CurShift, FillKindSize)
        ),
        NextShift = CurShift +
            uint8.det_from_uint(fill_kind_size_num_bits(FillKindSize))
    ).

%---------------------%

    % Any one of the packable_args given to us may be apc_dummy;
    % there are no restrictions on where they may appear.
    %
    % With local arguments,
    % we don't need the first/shifted distinction
    %
:- pred decide_local_packed_arg_word_loop(uint::in, uint::out,
    list(packable_arg)::in, list(local_arg_repn)::out) is det.

decide_local_packed_arg_word_loop(NumPrefixBits, NextShift, [], []) :-
    NextShift = NumPrefixBits.
decide_local_packed_arg_word_loop(NumPrefixBits, NextShift,
        [PackableArg | PackableArgs], [ArgRepn | ArgRepns]) :-
    PackableArg = packable_arg(_Arg, PackClass),
    (
        PackClass = apc_dummy,
        decide_local_packed_arg_word_loop(NumPrefixBits, CurShift,
            PackableArgs, ArgRepns),
        ArgRepn = local_none,
        NextShift = CurShift
    ;
        PackClass = apc_pack(FillKindSize),
        decide_local_packed_arg_word_loop(NumPrefixBits, CurShift,
            PackableArgs, ArgRepns),
        ArgRepn = local_partial(CurShift, FillKindSize),
        NextShift = CurShift + fill_kind_size_num_bits(FillKindSize)
    ).

%---------------------------------------------------------------------------%

:- type arg_class
    --->    ac_pack(fill_kind_size)
    ;       ac_dummy
    ;       ac_word
    ;       ac_double(double_word_kind).

:- type arg_pack_class
    --->    apc_pack(fill_kind_size)
    ;       apc_dummy.

:- type classified_arg
    --->    classified_arg(constructor_arg, arg_class).

:- type packable_arg
    --->    packable_arg(constructor_arg, arg_pack_class).

%---------------------------------------------------------------------------%

:- pred classify_args(platform_params::in, simple_du_map::in,
    list(constructor_arg)::in, list(classified_arg)::out) is det.

classify_args(_, _, [], []).
classify_args(PlatformParams, SimpleDuMap,
        [Arg | Args], [ClassifiedArg | ClassifiedArgs]) :-
    Arg = ctor_arg(_FieldName, ArgType, _Context),
    classify_arg_type(PlatformParams, SimpleDuMap, ArgType, Class),
    ClassifiedArg = classified_arg(Arg, Class),
    classify_args(PlatformParams, SimpleDuMap, Args, ClassifiedArgs).

:- pred classify_arg_type(platform_params::in, simple_du_map::in,
    mer_type::in, arg_class::out) is det.

classify_arg_type(PlatformParams, SimpleDuMap, ArgType, Class) :-
    ( if
        ArgType = builtin_type(ArgBuiltinType),
        classify_builtin_arg_type(PlatformParams, ArgBuiltinType, ClassPrime)
    then
        Class = ClassPrime
    else
        ( if type_to_ctor(ArgType, ArgTypeCtor) then
            ( if map.search(SimpleDuMap, ArgTypeCtor, SimpleDuRepn) then
                ( if SimpleDuRepn = sdr_enum(_Params, _TVarSet, EnumRepn) then
                    EnumRepn = enum_repn(_HeadName, _HeadTailName,
                        TailTailNames, _EnumForeignRepns),
                    NumEnums = 2 + list.length(TailTailNames),
                    int.log2(NumEnums, NumBits),
                    NumBitsU = uint.det_from_int(NumBits),
                    Class = ac_pack(fk_enum(NumBitsU))
                else if SimpleDuRepn = sdr_direct_dummy(_, _, _) then
                    % XXX ARG_PACK For bootstrapping.
                    PackDummies = PlatformParams ^ pp_base_params
                        ^ bp_allow_packing_dummies,
                    (
                        PackDummies = yes,
                        Class = ac_dummy
                    ;
                        PackDummies = no,
                        Class = ac_word
                    )
                else
                    Class = ac_word
                )
            else
                Class = ac_word
            )
        else
            % Values of polymorphic types are always stored as one word.
            Class = ac_word
        )
    ).

:- pred classify_builtin_arg_type(platform_params::in,
    builtin_type::in, arg_class::out) is semidet.

classify_builtin_arg_type(PlatformParams, BuiltinType, Class) :-
    (
        BuiltinType = builtin_type_int(ArgIntType),
        % XXX PlatformParams ^ pp_base_params ^ bp_allow_packing_ints = yes,
        (
            ArgIntType = int_type_int8,
            Class = ac_pack(fk_int8)
        ;
            ArgIntType = int_type_uint8,
            Class = ac_pack(fk_uint8)
        ;
            ArgIntType = int_type_int16,
            Class = ac_pack(fk_int16)
        ;
            ArgIntType = int_type_uint16,
            Class = ac_pack(fk_uint16)
        ;
            ArgIntType = int_type_int32,
            ( if PlatformParams ^ pp_word_size = word_size_64 then
                Class = ac_pack(fk_int32)
            else
                Class = ac_word
            )
        ;
            ArgIntType = int_type_uint32,
            ( if PlatformParams ^ pp_word_size = word_size_64 then
                Class = ac_pack(fk_uint32)
            else
                Class = ac_word
            )
        ;
            ArgIntType = int_type_int64,
            DWI = PlatformParams ^ pp_base_params ^ bp_allow_double_word_ints,
            (
                DWI = use_double_word_int64s,
                Class = ac_double(dw_int64)
            ;
                DWI = no_double_word_int64s,
                Class = ac_word
            )
        ;
            ArgIntType = int_type_uint64,
            DWI = PlatformParams ^ pp_base_params ^ bp_allow_double_word_ints,
            (
                DWI = use_double_word_int64s,
                Class = ac_double(dw_uint64)
            ;
                DWI = no_double_word_int64s,
                Class = ac_word
            )
        )
    ;
        BuiltinType = builtin_type_float,
        DWF = PlatformParams ^ pp_double_word_floats,
        (
            DWF = use_double_word_floats,
            Class = ac_double(dw_float)
        ;
            DWF = no_double_word_floats,
            Class = ac_word
        )
    ;
        BuiltinType = builtin_type_char,
        PlatformParams ^ pp_base_params ^ bp_allow_packing_chars = yes,
        % Section 2.4 of the Unicode standard says that "The codespace
        % consists of the integers from 0 to 10FFFF", which means that
        % all Unicode characters fit into 21 bits.
        Class = ac_pack(fk_char21)
    ).

%---------------------------------------------------------------------------%

:- pred are_all_args_packable(list(classified_arg)::in,
    list(packable_arg)::out, uint::in, uint::out) is semidet.

are_all_args_packable([], [], !NumBits).
are_all_args_packable([ClassifiedArg | ClassifiedArgs],
        [PackableArg | PackableArgs], !NumBits) :-
    ClassifiedArg = classified_arg(Arg, Class),
    (
        Class = ac_pack(FillSize),
        PackClass = apc_pack(FillSize),
        !:NumBits = !.NumBits + fill_kind_size_num_bits(FillSize)
    ;
        Class = ac_dummy,
        PackClass = apc_dummy
        % !NumBits is unchanged.
    ),
    PackableArg = packable_arg(Arg, PackClass),
    are_all_args_packable(ClassifiedArgs, PackableArgs, !NumBits).

:- pred find_initial_packable_args_within_limit(uint::in, uint::in, uint::out,
    list(classified_arg)::in,
    list(packable_arg)::out, list(classified_arg)::out) is det.

find_initial_packable_args_within_limit(_, !NumBits, [], [], []).
find_initial_packable_args_within_limit(Limit, !NumBits,
        [ClassifiedArg | ClassifiedArgs], PackableArgs, LeftOverArgs) :-
    ClassifiedArg = classified_arg(Arg, ArgClass),
    (
        (
            ArgClass = ac_pack(FillKindSize),
            ArgNumBits = fill_kind_size_num_bits(FillKindSize),
            PackableArg = packable_arg(Arg, apc_pack(FillKindSize))
        ;
            ArgClass = ac_dummy,
            ArgNumBits = 0u,
            PackableArg = packable_arg(Arg, apc_dummy)
        ),
        ( if !.NumBits + ArgNumBits =< Limit then
            !:NumBits = !.NumBits + ArgNumBits,
            find_initial_packable_args_within_limit(Limit, !NumBits,
                ClassifiedArgs, PackableArgsTail, LeftOverArgs),
            PackableArgs = [PackableArg | PackableArgsTail]
        else
            PackableArgs = [],
            LeftOverArgs = [ClassifiedArg | ClassifiedArgs]
        )
    ;
        ( ArgClass = ac_word
        ; ArgClass = ac_double(_)
        ),
        PackableArgs = [],
        LeftOverArgs = [ClassifiedArg | ClassifiedArgs]
    ).

%---------------------------------------------------------------------------%

:- pred is_direct_arg_ctor_for_c(set_tree234(type_ctor)::in,
    classified_constructor::in) is semidet.

is_direct_arg_ctor_for_c(WordAlignedTypeCtorsC, ClassifiedCtor) :-
    ClassifiedCtor = classified_constructor(Ctor, _ClassifiedArgs),
    Ctor = ctor(_Ordinal, MaybeExistConstraints, _ConsSymName, ConsArgs,
        _ConsArity, _CtorContext),
    MaybeExistConstraints = no_exist_constraints,
    ConsArgs = [ConsArg],
    ConsArg = ctor_arg(_MaybeFieldName, ArgType, _ArgContext),
    type_to_ctor(ArgType, ArgTypeCtor),
    ( if
        % Tuples are always acceptable argument types for a direct_arg
        % functor as they are represented by word-aligned vector pointers.
        % Strings are *not* always word-aligned (yet), and so are
        % *not* acceptable.
        type_ctor_is_tuple(ArgTypeCtor)
    then
        true
    else
        set_tree234.member(ArgTypeCtor, WordAlignedTypeCtorsC)
    ).

%---------------------------------------------------------------------------%

    % XXX ARG_PACK Think about whether visibility differences should limit
    % the type expansion process.
    %
:- pred expand_eqv_sub_of_notag_types_in_constructors(type_eqv_map::in,
    subtype_repn_map::in, simple_du_map::in, tvarset::in,
    list(constructor)::in, list(constructor)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

expand_eqv_sub_of_notag_types_in_constructors(_, _, _, _, [], [], !Specs).
expand_eqv_sub_of_notag_types_in_constructors(EqvMap, SubtypeMap, SimpleDuMap,
        TVarSet, [Ctor0 | Ctors0], [Ctor | Ctors], !Specs) :-
    expand_eqv_sub_of_notag_types_in_constructor(EqvMap, SubtypeMap,
        SimpleDuMap, TVarSet, Ctor0, Ctor, !Specs),
    expand_eqv_sub_of_notag_types_in_constructors(EqvMap, SubtypeMap,
        SimpleDuMap, TVarSet, Ctors0, Ctors, !Specs).

:- pred expand_eqv_sub_of_notag_types_in_constructor(type_eqv_map::in,
    subtype_repn_map::in, simple_du_map::in, tvarset::in,
    constructor::in, constructor::out,
    list(error_spec)::in, list(error_spec)::out) is det.

expand_eqv_sub_of_notag_types_in_constructor(EqvMap, SubtypeMap, SimpleDuMap,
        TVarSet, Ctor0, Ctor, !Specs) :-
    Args0 = Ctor0 ^ cons_args,
    expand_eqv_sub_of_notag_types_in_constructor_args(EqvMap, SubtypeMap,
        SimpleDuMap, TVarSet, Args0, Args, no_change, Changed, !Specs),
    % Don't allocate memory if we don't have to. Many constructors
    % have no constructors containing no equivalence types.
    (
        Changed = no_change,
        Ctor = Ctor0
    ;
        Changed = changed,
        Ctor = Ctor0 ^ cons_args := Args
    ).

:- pred expand_eqv_sub_of_notag_types_in_constructor_args(type_eqv_map::in,
    subtype_repn_map::in, simple_du_map::in, tvarset::in,
    list(constructor_arg)::in, list(constructor_arg)::out,
    maybe_changed::in, maybe_changed::out,
    list(error_spec)::in, list(error_spec)::out) is det.

expand_eqv_sub_of_notag_types_in_constructor_args(_, _, _, _, [], [],
        !Changed, !Specs).
expand_eqv_sub_of_notag_types_in_constructor_args(TypeEqvMap, SubtypeMap,
        SimpleDuMap, TVarSet, [Arg0 | Args0], [Arg | Args],
        !Changed, !Specs) :-
    Arg0 = ctor_arg(MaybeFieldName, ArgType0, Context),
    expand_eqv_sub_of_notag_type_fixpoint(TypeEqvMap, SubtypeMap, SimpleDuMap,
        TVarSet, Context, 100, ArgType0, ArgType, ArgTypeChanged, !Specs),
    % Don't allocate memory if we don't have to. Most argument types
    % need no expansion.
    (
        ArgTypeChanged = no_change,
        Arg = Arg0
    ;
        ArgTypeChanged = changed,
        !:Changed = changed,
        Arg = ctor_arg(MaybeFieldName, ArgType, Context)
    ),
    expand_eqv_sub_of_notag_types_in_constructor_args(TypeEqvMap, SubtypeMap,
        SimpleDuMap, TVarSet, Args0, Args, !Changed, !Specs).

:- pred expand_eqv_sub_of_notag_type_fixpoint(type_eqv_map::in,
    subtype_repn_map::in, simple_du_map::in, tvarset::in, prog_context::in,
    int::in, mer_type::in, mer_type::out, maybe_changed::out,
    list(error_spec)::in, list(error_spec)::out) is det.

expand_eqv_sub_of_notag_type_fixpoint(TypeEqvMap, SubtypeMap, SimpleDuMap,
        TVarSet0, Context, IterationsLeft, Type0, Type, Changed, !Specs) :-
    % Each fixpoint iteration, we try to expand Type0 as either
    % a notag type, an equivalence type, or subtype. If we succeed with any
    % of the three, we may not have arrived at a fixpoint yet, so we recurse.
    % If we succeed with none, then we *have* arrived at a fixpoint,
    % and therefore we stop.
    %
    % It is possible to construct notag types, equivalence types and subtypes
    % that would cause us to never find the fixpoint, instead of iterating
    % forever. This should never happen with natural inputs, but only with
    % with inputs that has been specially crafted for this purpose,
    % but just in case, we stop after a fixed number of iterations even if
    % we haven't arrived at a fixpoint.
    ( if
        IterationsLeft < 0
    then
        Changed = no_change,
        Type = Type0
    else if
        % Is the Mercury definition of Type0 a notag type?
        type_to_ctor_and_args(Type0, TypeCtor0, ArgTypes0),
        map.search(SimpleDuMap, TypeCtor0, SimpleDuRepn),
        SimpleDuRepn = sdr_notag(NotagParams0, NotagTVarSet0, NotagRepn),
        % Is the Mercury definition of Type0 overridden by a foreign
        % language definition for C?
        NotagRepn = notag_repn(_NotagFunctorName, NotagFunctorArgType0,
            MaybeCJCsRepn),
        MaybeCJCsRepn = c_java_csharp(no, _, _)
    then
        varset.merge_renaming(TVarSet0, NotagTVarSet0, TVarSet1,
            RenamingNotagTo1),
        apply_variable_renaming_to_tvar_list(RenamingNotagTo1,
            NotagParams0, NotagParams1),
        apply_variable_renaming_to_type(RenamingNotagTo1,
            NotagFunctorArgType0, NotagFunctorArgType1),
        % The length of both ArgTypes0 and NotagParams1 should match
        % is the arity recorded in TypeCtor0, which means that the two lists
        % should be the same length. If they are not, the cause can only be
        % an internal compiler error, not a user error.
        map.from_corresponding_lists(NotagParams1, ArgTypes0, ParamsSubst),
        apply_subst_to_type(ParamsSubst, NotagFunctorArgType1, Type1),
        expand_eqv_sub_of_notag_type_fixpoint(TypeEqvMap, SubtypeMap,
            SimpleDuMap, TVarSet1, Context, IterationsLeft - 1, Type1, Type,
            _Changed, !Specs),
        Changed = changed
    else if
        % Is the Mercury definition of Type0 a subtype?
        type_to_ctor_and_args(Type0, TypeCtor0, _ArgTypes0),
        map.search(SubtypeMap, TypeCtor0, SubtypeRepn0),
        SubtypeRepn0 =
            item_type_repn_info(_TypeCtorSymName, _TypeParams, SuperTypeCtor,
                _TypeTVarSet, _Context, _SeqNum)
    then
        % We do not have the arguments of the super type, only the type ctor.
        % However, we can just substitute new type variables for the type
        % parameters because the result (Type) is only going to be used for
        % deciding type representations, and the representation of a type ctor
        % does not depend on the types bound to its parameters.
        SuperTypeCtor = type_ctor(_, SuperTypeCtorArity),
        varset.new_vars(SuperTypeCtorArity, NewTypeVars, TVarSet0, TVarSet1),
        var_list_to_type_list(map.init, NewTypeVars, NewTypeArgs),
        construct_type(SuperTypeCtor, NewTypeArgs, SuperType),
        expand_eqv_sub_of_notag_type_fixpoint(TypeEqvMap, SubtypeMap,
        SimpleDuMap, TVarSet1, Context, IterationsLeft - 1, SuperType, Type,
            _Changed, !Specs),
        Changed = changed
    else
        replace_in_type_report_circular_eqvs(TypeEqvMap, TVarSet0, Context,
            Type0, Type1, Changed, !Specs),
        (
            Changed = changed,
            expand_eqv_sub_of_notag_type_fixpoint(TypeEqvMap, SubtypeMap,
                SimpleDuMap, TVarSet0, Context, IterationsLeft - 1,
                Type1, Type, _Changed, !Specs)
        ;
            Changed = no_change,
            Type = Type0
        )
    ).

%---------------------------------------------------------------------------%

:- type classified_constructor
    --->    classified_constructor(constructor, list(classified_arg)).

:- pred separate_out_constants(platform_params::in, simple_du_map::in,
    list(constructor)::in,
    list(constructor)::out, list(classified_constructor)::out) is det.

separate_out_constants(_, _, [], [], []).
separate_out_constants(PlatformParams, SimpleDuMap, [Ctor | Ctors],
        Constants, Functors) :-
    separate_out_constants(PlatformParams, SimpleDuMap, Ctors,
        ConstantsTail, FunctorsTail),
    Args = Ctor ^ cons_args,
    (
        Args = [],
        expect(unify(Ctor ^ cons_maybe_exist, no_exist_constraints), $pred,
            "Args = [] but exist_constraints"),
        Constants = [Ctor | ConstantsTail],
        Functors = FunctorsTail
    ;
        Args = [_ | _],
        classify_args(PlatformParams, SimpleDuMap, Args, ClassifiedArgs),
        ClassifiedCtor = classified_constructor(Ctor, ClassifiedArgs),
        Constants = ConstantsTail,
        Functors = [ClassifiedCtor | FunctorsTail]
    ).

%---------------------------------------------------------------------------%

:- pred one_constructor_non_constant_is_notag(maybe_cons_exist_constraints::in,
    list(constructor_arg)::in, arity::in, maybe_canonical::in,
    constructor_arg::out) is semidet.

one_constructor_non_constant_is_notag(MaybeExistConstraints, Args, Arity,
        MaybeCanonical, Arg) :-
    MaybeExistConstraints = no_exist_constraints,
    Args = [Arg],
    Arity = 1,
    MaybeCanonical = canon.

%---------------------------------------------------------------------------%

:- func make_c_repns(T, T, T, T, T, T) = c_repns(T).

make_c_repns(Repn64NoSpfNoDa, Repn64NoSpfDa,
        Repn32NoSpfNoDa, Repn32NoSpfDa, Repn32SpfNoDa, Repn32SpfDa) = CRepns :-
    ( if
        Repn64NoSpfNoDa = Repn64NoSpfDa,
        Repn32NoSpfNoDa = Repn32NoSpfDa,
        Repn32NoSpfNoDa = Repn32SpfNoDa,
        Repn32NoSpfNoDa = Repn32SpfDa
    then
        ( if Repn64NoSpfNoDa = Repn32NoSpfNoDa then
            CRepns = c_repns_same(Repn64NoSpfNoDa)
        else
            CRepns = c_repns_64_32(Repn64NoSpfNoDa, Repn32NoSpfNoDa)
        )
    else
        CRepns = c_repns_all(Repn64NoSpfDa, Repn64NoSpfNoDa,
            Repn32NoSpfDa, Repn32NoSpfNoDa,
            Repn32SpfDa, Repn32SpfNoDa)
    ).

:- func make_c_repns_no_da(T, T, T) = c_repns(T).

make_c_repns_no_da(Repn64NoSpf, Repn32NoSpf, Repn32Spf) = CRepns :-
    ( if Repn32NoSpf = Repn32Spf then
        ( if Repn64NoSpf = Repn32NoSpf then
            CRepns = c_repns_same(Repn64NoSpf)
        else
            CRepns = c_repns_64_32(Repn64NoSpf, Repn32NoSpf)
        )
    else
        CRepns = c_repns_all(Repn64NoSpf, Repn64NoSpf,
            Repn32NoSpf, Repn32NoSpf, Repn32Spf, Repn32Spf)
    ).

%---------------------%

:- pred num_bits_needed_for_n_things(int::in, uint::out) is det.

num_bits_needed_for_n_things(NumSharers, NumBits) :-
    num_bits_needed_for_n_things_loop(NumSharers, 0, NumBitsInt),
    NumBits = uint.det_from_int(NumBitsInt).

:- pred num_bits_needed_for_n_things_loop(int::in, int::in, int::out) is det.

num_bits_needed_for_n_things_loop(N, NumBits0, NumBits) :-
    ( if N =< (1 << NumBits0) then
        NumBits = NumBits0
    else
        num_bits_needed_for_n_things_loop(N, NumBits0 + 1, NumBits)
    ).

%---------------------------------------------------------------------------%
%
% We divide the parameters that control the representation decision process
% into two categories: the base parameters, and the platform parameters.
%
% The base parameters control what kinds of packing we are allowed to do.
% The values of these parameters are controlled by compiler options.
% In general, if a form of packing is safe to turn on, it should be turned on.
%
% The platform parameters specify the properties of (a) the platform we are
% targeting, and (b) the grade we are targeting.
%

    % This setting applies to uint64s as well.
:- type maybe_double_word_int64s
    --->    no_double_word_int64s
    ;       use_double_word_int64s.

:- type base_params
    --->    base_params(
                % This structure is needed Only for bootstrapping.
                % Eventually, all the settings should be implicitly
                % "yes, allow the packing of everything".

                bp_allow_double_word_ints       :: maybe_double_word_int64s,
                % This is always no_double_word_int64s. Setting it to
                % use_double_word_int64s would apply only to 32 bit platforms,
                % and was never debugged.

                bp_allow_packing_ints           :: bool,
                bp_allow_packing_chars          :: bool,
                bp_allow_packing_dummies        :: bool,
                bp_allow_packing_local_sectags  :: bool,
                bp_allow_packing_remote_sectags :: bool

                % bp_allow_packing_mini_types    :: bool,
                % Mini types have never been implemented.
            ).

:- pred setup_base_params(globals::in, base_params::out) is det.

setup_base_params(Globals, BaseParams) :-
    AllowDoubleWordInts = no_double_word_int64s,
    globals.lookup_bool_option(Globals, pack_everything, PackEverything),
    (
        PackEverything = no,
        globals.lookup_bool_option(Globals, allow_packing_ints,
            AllowPackingInts),
        globals.lookup_bool_option(Globals, allow_packing_chars,
            AllowPackingChars),
        globals.lookup_bool_option(Globals, allow_packing_dummies,
            AllowPackingDummies),
        globals.lookup_bool_option(Globals, allow_packing_local_sectags,
            AllowPackingLocalSegtags),
        globals.lookup_bool_option(Globals, allow_packing_remote_sectags,
            AllowPackingRemoteSegtags)
    ;
        PackEverything = yes,
        AllowPackingInts = yes,
        AllowPackingChars = yes,
        AllowPackingDummies = yes,
        AllowPackingLocalSegtags = yes,
        AllowPackingRemoteSegtags = yes
    ),
    BaseParams = base_params(AllowDoubleWordInts,
        AllowPackingInts, AllowPackingChars, AllowPackingDummies,
        AllowPackingLocalSegtags, AllowPackingRemoteSegtags).

%---------------------%

:- type maybe_double_word_floats
    --->    no_double_word_floats
    ;       use_double_word_floats.

:- type maybe_direct_args
    --->    no_direct_args
    ;       use_direct_args.

:- type platform_params
    --->    platform_params(
                % The word size is usually controlled by the target hardware,
                % but in pregen grades we use 32 bits regardless of the
                % hardware.
                pp_word_size                    :: word_size,

                % We use single word floats
                % - on 64 bit platforms, and
                % - on 32 bit platforms in spf grades.
                pp_double_word_floats           :: maybe_double_word_floats,

                % The term size profiling grades disable the direct arg
                % optimization. In all other grades, it should be enabled.
                pp_direct_args                  :: maybe_direct_args,

                pp_base_params                  :: base_params
            ).

:- func word_size_num_word_bits(word_size) = uint.

word_size_num_word_bits(word_size_64) = 64u.
word_size_num_word_bits(word_size_32) = 32u.

:- func word_size_num_ptag_bits(word_size) = uint.

word_size_num_ptag_bits(word_size_64) = 3u.
word_size_num_ptag_bits(word_size_32) = 2u.

%---------------------------------------------------------------------------%

:- pred output_sized_packable_functor(io.text_output_stream::in,
    maybe({platform_params, simple_du_map})::in,
    packable_constructor::in, io::di, io::uo) is det.

output_sized_packable_functor(Stream, PrintArgSizes, PackableCtor, !IO) :-
    PackableCtor = packable_constructor(Ctor, PackableArgs, NumBits),
    Ctor = ctor(_Ordinal, _MaybeExist, SymName, _Args, NumArgs, _Context),
    Name = unqualify_name(SymName),
    io.format(Stream, "%2u: %s/%d", [u(NumBits), s(Name), i(NumArgs)], !IO),
    (
        PrintArgSizes = no,
        io.nl(Stream, !IO)
    ;
        PrintArgSizes = yes({PlatformParams, SimpleDuMap}),
        io.write_string(Stream, "(", !IO),
        output_sized_packable_functor_args(Stream, PlatformParams, SimpleDuMap,
            "", PackableArgs, !IO),
        io.write_string(Stream, ")\n", !IO)
    ).

:- pred output_sized_packable_functor_args(io.text_output_stream::in,
    platform_params::in, simple_du_map::in, string::in,
    list(packable_arg)::in, io::di, io::uo) is det.

output_sized_packable_functor_args(_, _, _, _, [], !IO).
output_sized_packable_functor_args(Stream, PlatformParams, SimpleDuMap,
        Prefix, [PackableArg | PackableArgs], !IO) :-
    PackableArg = packable_arg(_Arg, PackClass),
    (
        PackClass = apc_pack(FillKindSize),
        io.format(Stream, "%s%s",
            [s(Prefix), s(fill_kind_size_to_string(FillKindSize))], !IO)
    ;
        PackClass = apc_dummy,
        io.format(Stream, "%sdummy", [s(Prefix)], !IO)
    ),
    output_sized_packable_functor_args(Stream, PlatformParams, SimpleDuMap,
        ", ", PackableArgs, !IO).

%---------------------------------------------------------------------------%
:- end_module parse_tree.decide_type_repn.
%---------------------------------------------------------------------------%
