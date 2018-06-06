%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: du_type_layout.m.
% Main author: zs.
%
% The task of this module is decide the representation of each type.
% Once that is done, it will also invoke add_special_pred.m to declare
% and (if necessary) define the unify and compare predicates for each type.
%
% We decide representations in two passes.
%
% - The purpose of the first pass is to gather information about all types
%   that may be useful when those types occur as the arguments of function
%   symbols (whether in other types or in the same type).
%
%   However, some kinds of types are so simple that we never need such
%   information to decide their representations, and so we make those decisions
%   in the first pass. These simple types are:
%
%   - dummy types, types with one constructor of arity 0;
%   - enum types, types whose two or more constructors all have arity 0; and
%   - notag types, types with one constructor of arity 1 with no constraints.
%
%   The dummy and notag types have no function symbols with arguments.
%   For notag types, their representation is the same as the representation
%   of their single function symbol's argument type, whatever that happens
%   to be, and we need to record only the fact of the equivalence, not the
%   final representation type itself.
%
% - The second pass decides the representation of all other types.
%   For this, it uses two kinds of information gathered by the first pass:
%
%   - which types are representable in sub-word-size chunks of bits, and
%   - which types are guaranteed to be represented as a word-aligned pointers.
%
%   We use the first kind of information for packing arguments tightly
%   together, and we use the second kind to help decide which function symbols
%   we can apply the direct arg optimization to.
%
%---------------------------------------------------------------------------%
%
% Originally, I (zs) intended to run the du_type_layout pass
% after the whole semantic analysis phase of the compiler, i.e. after type,
% mode, and determinism checking and simplification. My reason for this,
% as discussed on m-dev in late october 2017, was to try to ensure that
% semantic analysis does not have access to, and thus cannot depend on,
% type representations, which are implementation-level details, and not
% a user-visible aspect of the program's semantics.
%
% I found out the hard way that this won't work, at least not without
% a significant amount of otherwise-unnecessary code duplication.
%
% The ultimate reason for this is that the code we want to generate
% for a type's unify and compare predicates depends on the type's
% representation. Consider a type such as
%
%     :- type t
%         --->    f(bool, bool, bool bool).
%
% One future packing optimization should allow us to represent values
% of this type as four bits in a register (no heap cell needed),
% If we can do that, then unifying two values X and Y of this type
% should be done by unsafe-casting both to int, and comparing the ints.
% If we cannot, then we should generate the usual code deconstructing
% both X and Y, and comparing the arguments (one by one or all together,
% depending on well we can pack the heap cell's contents).
%
% Deciding type representations after semantic analysis would therefore
% require us to generate the unify and compare (and maybe index) predicates
% of each type late as well, but then (since the clauses we generate are NOT
% guaranteed to include all the right types, modes and other annotations)
% we would have to repeat semantic analysis just on these late-auto-generated
% predicates. This would not be too hard with some semantic analysis passes
% (e.g. determinism analysis) that are already effectively done
% predicate-by-predicate, but *would* require large amounts of new work
% for the polymorphism pass, which currently works on the whole module at once.
%
% To minimize this problem, we execute the du_type_layout pass directly after
% the make_hlds pass, i.e. before all the semantic analysis passes.
%
%---------------------------------------------------------------------------%
% XXX TYPE_REPN: Record contexts in the type table, and use them to replace
% all the term.context_inits in error messages below.
%---------------------------------------------------------------------------%

:- module hlds.du_type_layout.
:- interface.

:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.error_util.

:- import_module list.

:- pred decide_type_repns(module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.type_util.
:- import_module hlds.add_foreign_enum.
:- import_module hlds.add_special_pred.
:- import_module hlds.hlds_data.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.      % undesirable dependency
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module int.
:- import_module io.
:- import_module map.
:- import_module maybe.
:- import_module multi_map.
:- import_module pair.
:- import_module require.
:- import_module set_tree234.
:- import_module string.
:- import_module term.
:- import_module uint.
:- import_module uint8.

%---------------------------------------------------------------------------%

:- type maybe_double_word_floats
    --->    no_double_word_floats
    ;       use_double_word_floats.

    % This setting applies to uint64s as well.
:- type maybe_double_word_int64s
    --->    no_double_word_int64s
    ;       use_double_word_int64s.

:- type maybe_primary_tags
    --->    no_primary_tags
    ;       max_primary_tag(ptag, int).
            % The maximum ptag, and the number of bits it occupies.

:- type maybe_unboxed_no_tag_types
    --->    no_unboxed_no_tag_types
    ;       use_unboxed_no_tag_types.

:- type maybe_direct_args
    --->    direct_args_disabled
    ;       direct_args_enabled.

:- type maybe_inform_about_packing
    --->    do_not_inform_about_packing
    ;       inform_about_packing.

:- type decide_du_params
    --->    decide_du_params(
                ddp_target                  :: compilation_target,
                ddp_double_word_floats      :: maybe_double_word_floats,
                ddp_double_word_int64s      :: maybe_double_word_int64s,
                ddp_unboxed_no_tag_types    :: maybe_unboxed_no_tag_types,
                ddp_maybe_primary_tags      :: maybe_primary_tags,
                ddp_arg_pack_bits           :: int,

                % Only for bootstrapping.
                ddp_allow_double_word_ints  :: bool,
                ddp_allow_packing_ints      :: bool,
                ddp_allow_packing_dummies   :: bool,

                % We use the direct_arg_map for two purposes:
                % - to optimize data representations, and
                % - to generate error messages for incorrect uses of
                %   "where direct_arg is" clauses.
                % We need the direct_arg for the first purpose
                % only when the direct_arg optimization is enabled,
                % but we use it for the second purpose even when it is
                % disabled.
                % XXX When we remove "where direct_arg is" clauses
                % from the language, the second purpose will go away.
                ddp_maybe_direct_args       :: maybe_direct_args,
                ddp_direct_arg_map          :: direct_arg_map,

                ddp_inform_suboptimal_pack  :: maybe_inform_about_packing
            ).

decide_type_repns(!ModuleInfo, !Specs) :-
    module_info_get_type_repn_dec(!.ModuleInfo, TypeRepnDec),
    TypeRepnDec = type_repn_decision_data(TypeRepns, DirectArgMap,
        ForeignEnums, ForeignExportEnums),

    module_info_get_globals(!.ModuleInfo, Globals),
    setup_decide_du_params(Globals, DirectArgMap, Params),

    % XXX TYPE_REPN The compiler does not yet generate type_repn items,
    % so for now, TypeRepns will be the empty list, which makes _TypeRepnMap
    % not yet useful.
    build_type_repn_map(TypeRepns, map.init, _TypeRepnMap),

    list.foldl2(add_pragma_foreign_enum(!.ModuleInfo), ForeignEnums,
        map.init, TypeCtorToForeignEnumMap, !Specs),

    module_info_get_type_table(!.ModuleInfo, TypeTable0),
    get_all_type_ctor_defns(TypeTable0, TypeCtorsTypeDefns0),

    % We use MustBeSingleFunctorTagTypes as a sanity check.
    % Pass 1 adds a type to it if it requires pass 2 to represent its one
    % constructor with a single_functor_tag; pass 2 removes a type from it
    % when it does so. At the end, we check that the resulting set is empty,
    % which means pass 2 fulfilled all the expectations of pass 1 in this
    % respect.
    MustBeSingleFunctorTagTypes0 = set_tree234.init,
    map.init(ComponentTypeMap0),
    map.init(NoTagTypeMap0),
    list.map_foldl4(
        decide_if_simple_du_type(!.ModuleInfo, Params,
            TypeCtorToForeignEnumMap),
        TypeCtorsTypeDefns0, TypeCtorsTypeDefns1,
        MustBeSingleFunctorTagTypes0, MustBeSingleFunctorTagTypes1,
        ComponentTypeMap0, ComponentTypeMap,
        NoTagTypeMap0, NoTagTypeMap, !Specs),
    module_info_set_no_tag_types(NoTagTypeMap, !ModuleInfo),

    list.map_foldl2(
        decide_if_complex_du_type(!.ModuleInfo, Params, ComponentTypeMap),
        TypeCtorsTypeDefns1, TypeCtorsTypeDefns,
        MustBeSingleFunctorTagTypes1, MustBeSingleFunctorTagTypes, !Specs),
    expect(set_tree234.is_empty(MustBeSingleFunctorTagTypes), $pred,
        "some MustBeSingleFunctionTag type is not SingleFunctionTag"),
    set_all_type_ctor_defns(TypeCtorsTypeDefns, TypeTable),
    module_info_set_type_table(TypeTable, !ModuleInfo),

    list.foldl2(add_pragma_foreign_export_enum, ForeignExportEnums,
        !ModuleInfo, !Specs),

    % XXX TYPE_REPN Fold over TypeCtorsTypeDefns instead.
    foldl_over_type_ctor_defns(
        add_special_pred_decl_defns_for_type_maybe_lazily,
        TypeTable, !ModuleInfo).

:- pred setup_decide_du_params(globals::in, direct_arg_map::in,
    decide_du_params::out) is det.

setup_decide_du_params(Globals, DirectArgMap, Params) :-
    % Compute Target.
    globals.get_target(Globals, Target),

    % Compute DoubleWordFloats and DoubleWordInt64s.
    globals.lookup_bool_option(Globals, allow_double_word_fields,
        AllowDoubleWords),
    (
        AllowDoubleWords = yes,
        globals.lookup_int_option(Globals, bits_per_word, TargetWordBits),
        globals.lookup_bool_option(Globals, single_prec_float,
            SinglePrecFloat),
        ( if
            TargetWordBits = 32,
            SinglePrecFloat = no
        then
            DoubleWordFloats = use_double_word_floats
        else
            DoubleWordFloats = no_double_word_floats
        ),
        ( if TargetWordBits = 32 then
            DoubleWordInt64s = use_double_word_int64s
        else
            DoubleWordInt64s = no_double_word_int64s
        )
    ;
        AllowDoubleWords = no,
        DoubleWordFloats = no_double_word_floats,
        DoubleWordInt64s = no_double_word_int64s
    ),

    % Compute UnboxedNoTagTypes.
    globals.lookup_bool_option(Globals, unboxed_no_tag_types,
        UnboxedNoTagTypesBool),
    (
        UnboxedNoTagTypesBool = no,
        UnboxedNoTagTypes = no_unboxed_no_tag_types
    ;
        UnboxedNoTagTypesBool = yes,
        UnboxedNoTagTypes = use_unboxed_no_tag_types
    ),

    % Compute MaybePrimaryTags.
    globals.lookup_int_option(Globals, num_ptag_bits, NumPtagBits),
    ( if NumPtagBits = 0 then
        MaybePrimaryTags = no_primary_tags
    else if NumPtagBits = 2 then
        MaybePrimaryTags = max_primary_tag(ptag(3u8), NumPtagBits)
    else if NumPtagBits = 3 then
        MaybePrimaryTags = max_primary_tag(ptag(7u8), NumPtagBits)
    else
        MaxPtagInt = int.pow(2, NumPtagBits) - 1,
        MaxPtagUint8 = uint8.det_from_int(MaxPtagInt),
        MaybePrimaryTags = max_primary_tag(ptag(MaxPtagUint8), NumPtagBits)
    ),

    % Compute ArgPackBits.
    globals.lookup_int_option(Globals, arg_pack_bits, ArgPackBits),

    % Compute AllowDoubleWordInts, AllowPackingInts and AllocPackingDummies.
    globals.lookup_bool_option(Globals, allow_double_word_ints,
        AllowDoubleWordInts),
    globals.lookup_bool_option(Globals, allow_packing_ints,
        AllowPackingInts),
    globals.lookup_bool_option(Globals, allow_packing_dummies,
        AllowPackingDummies),

    % Compute MaybeDirectArgs.
    (
        Target = target_c,
        globals.lookup_bool_option(Globals, record_term_sizes_as_words,
            TermSizeWords),
        globals.lookup_bool_option(Globals, record_term_sizes_as_cells,
            TermSizeCells),
        ( if
            TermSizeWords = no,
            TermSizeCells = no
        then
            MaybeDirectArgs = direct_args_enabled
        else
            % We cannot use direct arg functors in term size grades.
            MaybeDirectArgs = direct_args_disabled
        )
    ;
        ( Target = target_csharp
        ; Target = target_java
        ; Target = target_erlang
        ),
        % Direct arg functors have not (yet) been implemented on these targets.
        MaybeDirectArgs = direct_args_disabled
    ),

    globals.lookup_bool_option(Globals, highlevel_data, HighLevelData),
    (
        HighLevelData = no
    ;
        HighLevelData = yes,
        expect(unify(AllowDoubleWords, no), $pred,
            "AllowDoubleWords != no"),
        expect(unify(AllowPackingInts, no), $pred,
            "AllowPackingInts != no"),
        expect(unify(AllowPackingDummies, no), $pred,
            "AllowPackingDummies != no")
    ),

    % Compute MaybeInformPacking.
    globals.lookup_bool_option(Globals, inform_suboptimal_packing,
        InformPacking),
    (
        InformPacking = no,
        MaybeInformPacking = do_not_inform_about_packing
    ;
        InformPacking = yes,
        MaybeInformPacking = inform_about_packing
    ),

    Params = decide_du_params(Target, DoubleWordFloats, DoubleWordInt64s,
        UnboxedNoTagTypes, MaybePrimaryTags, ArgPackBits,
        AllowDoubleWordInts, AllowPackingInts, AllowPackingDummies,
        MaybeDirectArgs, DirectArgMap, MaybeInformPacking).

%---------------------------------------------------------------------------%

:- type type_repn_map == multi_map(type_ctor, item_type_repn_info).

:- pred build_type_repn_map(list(item_type_repn_info)::in,
    type_repn_map::in, type_repn_map::out) is det.

build_type_repn_map([], !TypeRepnMap).
build_type_repn_map([TypeRepn | TypeRepns], !TypeRepnMap) :-
    TypeRepn = item_type_repn_info(TypeCtorSymName, ArgTVars, _, _, _, _),
    list.length(ArgTVars, Arity),
    TypeCtor = type_ctor(TypeCtorSymName, Arity),
    multi_map.add(TypeCtor, TypeRepn, !TypeRepnMap),
    build_type_repn_map(TypeRepns, !TypeRepnMap).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Pass 1.
%

:- type word_aligned_why
    --->    foreign_type_assertion
    ;       mercury_type_defn(hlds_type_defn).

:- type packable_kind
    --->    packable_dummy
    ;       packable_n_bits(int, fill_kind).

:- type component_type_kind
    --->    packable(packable_kind)
    ;       is_word_aligned_ptr(word_aligned_why)
    ;       is_eqv_type(type_ctor).

:- type component_type_map == map(type_ctor, component_type_kind).

:- pred decide_if_simple_du_type(module_info::in, decide_du_params::in,
    type_ctor_to_foreign_enums_map::in,
    pair(type_ctor, hlds_type_defn)::in, pair(type_ctor, hlds_type_defn)::out,
    set_tree234(type_ctor)::in, set_tree234(type_ctor)::out,
    component_type_map::in, component_type_map::out,
    no_tag_type_table::in, no_tag_type_table::out,
    list(error_spec)::in, list(error_spec)::out) is det.

decide_if_simple_du_type(ModuleInfo, Params, TypeCtorToForeignEnumMap,
        TypeCtorTypeDefn0, TypeCtorTypeDefn, !MustBeSingleFunctorTagTypes,
        !ComponentTypeMap, !NoTagTypeMap, !Specs) :-
    TypeCtorTypeDefn0 = TypeCtor - TypeDefn0,
    get_type_defn_body(TypeDefn0, Body0),
    (
        Body0 = hlds_du_type(Ctors, MaybeCanonical, MaybeRepn0,
            MaybeForeign),
        expect(unify(MaybeRepn0, no), $pred, "MaybeRepn0 != no"),
        expect_not(unify(Ctors, []), $pred, "Ctors != []"),
        ( if
            map.search(TypeCtorToForeignEnumMap, TypeCtor, TCFE),
            TCFE = type_ctor_foreign_enums(_LangContextMap,
                MaybeForeignEnumTagMap),
            MaybeForeignEnumTagMap = yes(ForeignEnumTagMap)
        then
            decide_simple_type_foreign_enum(ModuleInfo, Params,
                TypeCtor, TypeDefn0, Body0, Ctors, ForeignEnumTagMap,
                TypeCtorTypeDefn, !Specs)
        else if
            ctors_are_all_constants(Ctors)
        then
            decide_simple_type_dummy_or_mercury_enum(ModuleInfo, Params,
                TypeCtor, TypeDefn0, Body0, Ctors, TypeCtorTypeDefn,
                !ComponentTypeMap, !Specs)
        else if
            Ctors = [SingleCtor]
        then
            ( if
                SingleCtor = ctor(no_exist_constraints, SingleCtorSymName,
                    [SingleArg], 1, SingleCtorContext),
                MaybeCanonical = canon,
                Params ^ ddp_unboxed_no_tag_types = use_unboxed_no_tag_types
            then
                decide_simple_type_notag(ModuleInfo, Params,
                    TypeCtor, TypeDefn0, Body0,
                    SingleCtorSymName, SingleArg, SingleCtorContext,
                    TypeCtorTypeDefn, !NoTagTypeMap, !Specs)
            else
                add_du_if_single_ctor_is_word_aligned_ptr(Params, TypeCtor,
                    TypeDefn0, MaybeForeign,
                    !MustBeSingleFunctorTagTypes, !ComponentTypeMap),

                % Figure out the representation of these types
                % in the second pass.
                TypeCtorTypeDefn = TypeCtorTypeDefn0
            )
        else
            % Figure out the representation of these types in the second pass.
            TypeCtorTypeDefn = TypeCtorTypeDefn0
        )
    ;
        Body0 = hlds_foreign_type(ForeignType),
        add_foreign_if_word_aligned_ptr(ModuleInfo, Params, TypeCtor,
            ForeignType, !ComponentTypeMap, !Specs),

        % There are no questions of representation to figure out.
        TypeCtorTypeDefn = TypeCtorTypeDefn0
    ;
        Body0 = hlds_abstract_type(AbstractDetails),
        add_abstract_if_packable(TypeCtor, AbstractDetails, !ComponentTypeMap),
        TypeCtorTypeDefn = TypeCtorTypeDefn0
    ;
        % XXX TYPE_REPN Enter type equivalences into ComponentTypeMap.
        ( Body0 = hlds_eqv_type(_)
        ; Body0 = hlds_solver_type(_)
        ),
        % There are no questions of representation to figure out.
        TypeCtorTypeDefn = TypeCtorTypeDefn0
    ).

%---------------------%

:- pred decide_simple_type_foreign_enum(module_info::in, decide_du_params::in,
    type_ctor::in, hlds_type_defn::in, hlds_type_body::in(hlds_du_type),
    list(constructor)::in, {cons_id_to_tag_map, foreign_language}::in,
    pair(type_ctor, hlds_type_defn)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

decide_simple_type_foreign_enum(_ModuleInfo, Params, TypeCtor, TypeDefn0,
        Body0, Ctors, ForeignEnums, TypeCtorTypeDefn, !Specs) :-
    % XXX TYPE_REPN Should MaybeForeign = yes(...) be allowed?
    ForeignEnums = {ForeignEnumTagMap, Lang},
    DuKind = du_type_kind_foreign_enum(Lang),
    DirectArgMap = Params ^ ddp_direct_arg_map,
    ( if map.search(DirectArgMap, TypeCtor, _DirectArgFunctors) then
        DirectArgPieces = [words("Error:"), type_ctor_sna(TypeCtor),
            words("has both a"), pragma_decl("foreign_enum"),
            words("declaration and a direct_arg specification."), nl],
        DirectArgMsg = simple_msg(term.context_init,
            [always(DirectArgPieces)]),
        DirectArgSpec = error_spec(severity_error, phase_type_check,
            [DirectArgMsg]),
        !:Specs = [DirectArgSpec | !.Specs]
    else
        true
    ),
    ( if ctors_are_all_constants(Ctors) then
        true
    else
        NonEnumArgPieces = [words("Error:"), type_ctor_sna(TypeCtor),
            words("has a"), pragma_decl("foreign_enum"), words("declaration,"),
            words("but it has function symbols whose arity is not zero."), nl],
        NonEnumArgMsg = simple_msg(term.context_init,
            [always(NonEnumArgPieces)]),
        NonEnumArgSpec = error_spec(severity_error, phase_type_check,
            [NonEnumArgMsg]),
        !:Specs = [NonEnumArgSpec | !.Specs]
    ),
    list.map_foldl(add_repn_to_foreign_enum_ctor(TypeCtor, ForeignEnumTagMap),
        Ctors, CtorRepns, map.init, CtorRepnMap),
    MaybeCheaperTagTest = no_cheaper_tag_test,
    MaybeDirectArgFunctors = no,
    Repn = du_type_repn(CtorRepns, CtorRepnMap, MaybeCheaperTagTest,
        DuKind, MaybeDirectArgFunctors),
    Body = Body0 ^ du_type_repn := yes(Repn),
    set_type_defn_body(Body, TypeDefn0, TypeDefn),
    TypeCtorTypeDefn = TypeCtor - TypeDefn.

:- pred add_repn_to_foreign_enum_ctor(type_ctor::in, cons_id_to_tag_map::in,
    constructor::in, constructor_repn::out,
    ctor_name_to_repn_map::in, ctor_name_to_repn_map::out) is det.

add_repn_to_foreign_enum_ctor(TypeCtor, ConsTagMap, Ctor, CtorRepn,
        !CtorRepnMap) :-
    Ctor = ctor(MaybeExistConstraints, SymName, Args, Arity, Context),
    ConsId = cons(SymName, Arity, TypeCtor),
    map.lookup(ConsTagMap, ConsId, ConsTag),
    % All function symbols of a foreign enum type should have arity zero.
    % If any have a nonzero arity, our caller will generate an error message,
    % and won't proceed to code generation.
    ArgRepns = list.map(add_dummy_repn_to_ctor_arg, Args),
    CtorRepn = ctor_repn(MaybeExistConstraints, SymName, ConsTag, ArgRepns,
        Arity, Context),
    insert_ctor_repn_into_map(CtorRepn, !CtorRepnMap).

:- func add_dummy_repn_to_ctor_arg(constructor_arg) = constructor_arg_repn.

add_dummy_repn_to_ctor_arg(ConsArg) = ConsArgRepn :-
    ConsArg = ctor_arg(MaybeFieldName, Type, Context),
    DummyWidth = apw_full(arg_only_offset(-1), cell_offset(-1)),
    ConsArgRepn = ctor_arg_repn(MaybeFieldName, Type, DummyWidth, Context).

%---------------------%

:- pred decide_simple_type_dummy_or_mercury_enum(module_info::in,
    decide_du_params::in, type_ctor::in, hlds_type_defn::in,
    hlds_type_body::in(hlds_du_type), list(constructor)::in,
    pair(type_ctor, hlds_type_defn)::out,
    component_type_map::in, component_type_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

decide_simple_type_dummy_or_mercury_enum(_ModuleInfo, Params,
        TypeCtor, TypeDefn0, Body0, Ctors, TypeCtorTypeDefn,
        !ComponentTypeMap, !Specs) :-
    (
        Ctors = [],
        % A type with no constructors is an abstract type, not a du type.
        unexpected($pred, "no constant constructors")
    ;
        Ctors = [SingleCtor],
        DuTypeKind = du_type_kind_direct_dummy,
        SingleCtor = ctor(_MaybeExistConstraints,
            SingleCtorSymName, _Args, _SingleCtorArity, SingleCtorContext),
        % XXX TYPE_REPN Should we have a special dummy_tag?
        % If not, we can use the same code as the enum case.
        % If yes, we can delete all the checks in the code generators
        % for "is the variable's type a dummy type?" when the variable
        % is unified with a cons_id whose tag is dummy_tag.
        SingleCtorTag = dummy_tag,
        SingleCtorRepn = ctor_repn(no_exist_constraints,
            SingleCtorSymName, SingleCtorTag, [], 0, SingleCtorContext),
        CtorRepns = [SingleCtorRepn],
        insert_ctor_repn_into_map(SingleCtorRepn, map.init, CtorRepnMap),
        ComponentKind = packable(packable_dummy)
    ;
        Ctors = [_, _ | _],
        DuTypeKind = du_type_kind_mercury_enum,
        assign_tags_to_enum_constants(Ctors, CtorRepns, 0, NextTag,
            map.init, CtorRepnMap),
        int.log2(NextTag, NumBits),
        ComponentKind = packable(packable_n_bits(NumBits, fill_enum))
    ),
    DirectArgMap = Params ^ ddp_direct_arg_map,
    ( if map.search(DirectArgMap, TypeCtor, _DirectArgFunctors) then
        Pieces = [words("Error: all the function symbols of"),
            type_ctor_sna(TypeCtor), words("have arity zero,"),
            words("yet it has a direct_arg specification."), nl],
        Msg = simple_msg(term.context_init, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_type_check, [Msg]),
        !:Specs = [Spec | !.Specs]
    else
        true
    ),
    MaybeCheaperTagTest = no_cheaper_tag_test,
    MaybeDirectArgFunctors = no,
    Repn = du_type_repn(CtorRepns, CtorRepnMap, MaybeCheaperTagTest,
        DuTypeKind, MaybeDirectArgFunctors),
    Body = Body0 ^ du_type_repn := yes(Repn),
    set_type_defn_body(Body, TypeDefn0, TypeDefn),
    TypeCtorTypeDefn = TypeCtor - TypeDefn,

    map.det_insert(TypeCtor, ComponentKind, !ComponentTypeMap).

:- pred assign_tags_to_enum_constants(
    list(constructor)::in, list(constructor_repn)::out, int::in, int::out,
    ctor_name_to_repn_map::in, ctor_name_to_repn_map::out) is det.

assign_tags_to_enum_constants([], [], !CurTag, !CtorRepnMap).
assign_tags_to_enum_constants([Ctor | Ctors], [CtorRepn | CtorRepns],
        !CurTag, !CtorRepnMap) :-
    Ctor = ctor(MaybeExistConstraints, SymName, Args, Arity, Context),
    expect(unify(MaybeExistConstraints, no_exist_constraints), $pred,
        "enum constant has existential constraints"),
    expect(unify(Args, []), $pred, "enum constant has arguments"),
    expect(unify(Arity, 0), $pred, "enum constant has nonzero arity"),
    CtorTag = int_tag(int_tag_int(!.CurTag)),
    CtorRepn = ctor_repn(no_exist_constraints, SymName, CtorTag, [], 0,
        Context),
    !:CurTag = !.CurTag + 1,
    insert_ctor_repn_into_map(CtorRepn, !CtorRepnMap),
    assign_tags_to_enum_constants(Ctors, CtorRepns, !CurTag, !CtorRepnMap).

%---------------------%

:- pred decide_simple_type_notag(module_info::in, decide_du_params::in,
    type_ctor::in, hlds_type_defn::in, hlds_type_body::in(hlds_du_type),
    sym_name::in, constructor_arg::in, prog_context::in,
    pair(type_ctor, hlds_type_defn)::out,
    no_tag_type_table::in, no_tag_type_table::out,
    list(error_spec)::in, list(error_spec)::out) is det.

decide_simple_type_notag(_ModuleInfo, Params, TypeCtor, TypeDefn0, Body0,
        SingleCtorSymName, SingleArg, SingleCtorContext,
        TypeCtorTypeDefn, !NoTagTypeMap, !Specs) :-
    SingleCtorTag = no_tag,
    SingleArg = ctor_arg(MaybeSingleArgFieldName, SingleArgType,
        SingleArgContext),
    % XXX TYPE_REPN The apw_full is a *lie*
    % if the arg type is a 64 bit float on a 32 bit platform.
    SingleArgRepn = ctor_arg_repn(MaybeSingleArgFieldName, SingleArgType,
        apw_full(arg_only_offset(0), cell_offset(0)), SingleArgContext),
    SingleCtorRepn = ctor_repn(no_exist_constraints, SingleCtorSymName,
        SingleCtorTag, [SingleArgRepn], 1, SingleCtorContext),
    insert_ctor_repn_into_map(SingleCtorRepn, map.init, CtorRepnMap),

    MaybeCheaperTagTest = no_cheaper_tag_test,
    (
        MaybeSingleArgFieldName = no,
        MaybeSingleArgName = no
    ;
        MaybeSingleArgFieldName =
            yes(ctor_field_name(SingleArgSymName, _FieldContext)),
        MaybeSingleArgName = yes(unqualify_name(SingleArgSymName))
    ),
    DuTypeKind = du_type_kind_notag(SingleCtorSymName, SingleArgType,
        MaybeSingleArgName),
    DirectArgMap = Params ^ ddp_direct_arg_map,
    ( if map.search(DirectArgMap, TypeCtor, _DirectArgFunctors) then
        Pieces = [words("Error:"), type_ctor_sna(TypeCtor),
            words("is a no_tag type,"),
            words("yet it has a direct_arg specification."), nl],
        Msg = simple_msg(term.context_init, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_type_check, [Msg]),
        !:Specs = [Spec | !.Specs]
    else
        true
    ),
    MaybeDirectArgFunctors = no,
    Repn = du_type_repn([SingleCtorRepn], CtorRepnMap, MaybeCheaperTagTest,
        DuTypeKind, MaybeDirectArgFunctors),
    Body = Body0 ^ du_type_repn := yes(Repn),
    set_type_defn_body(Body, TypeDefn0, TypeDefn),
    TypeCtorTypeDefn = TypeCtor - TypeDefn,

    get_type_defn_tparams(TypeDefn0, TypeParams),
    NoTagType = no_tag_type(TypeParams, SingleCtorSymName, SingleArgType),
    map.det_insert(TypeCtor, NoTagType, !NoTagTypeMap).

%---------------------%

:- pred add_du_if_single_ctor_is_word_aligned_ptr(decide_du_params::in,
    type_ctor::in, hlds_type_defn::in, maybe(foreign_type_body)::in,
    set_tree234(type_ctor)::in, set_tree234(type_ctor)::out,
    component_type_map::in, component_type_map::out) is det.

add_du_if_single_ctor_is_word_aligned_ptr(Params, TypeCtor, TypeDefn,
        MaybeForeign, !MustBeSingleFunctorTagTypes, !ComponentTypeMap) :-
    % Are we guaranteed to choose a word aligned pointer as the representation?
    ( if
        TypeCtor = type_ctor(_TypeCtorSymName, TypeCtorArity),

        % NOTE We could let the argument's type to have a set of type params
        % that is a subset of the type params of the containing type,
        % but that would require the runtime system to be able to handle
        % variables in the argument type, during unification and comparison
        % (mercury_unify_compare_body.h) during deconstruction
        % (mercury_ml_expand_body.h), during deep copying
        % (mercury_deep_copy_body.h), and maybe during some other
        % operations.
        TypeCtorArity = 0,

        % XXX TYPE_REPN Why this test? It is inherited from legacy code,
        % but the direct_arg optimization is not applicable to types
        % that have only a single constructor.
        DirectArgMap = Params ^ ddp_direct_arg_map,
        not map.search(DirectArgMap, TypeCtor, _DirectArgFunctors)
    then
        set_tree234.insert(TypeCtor, !MustBeSingleFunctorTagTypes),

        % XXX TYPE_REPN This test is only for backward compatibility.
        % The code we should use long term is the else arm.
        ( if
            MaybeForeign = yes(Foreign),
            Target = Params ^ ddp_target,
            is_foreign_type_body_for_target(Foreign, Target, Assertions)
        then
            ( if asserted_word_aligned_pointer(Assertions) then
                ComponentKind = is_word_aligned_ptr(foreign_type_assertion),
                map.det_insert(TypeCtor, ComponentKind, !ComponentTypeMap)
            else
                true
            )
        else
            ComponentKind = is_word_aligned_ptr(mercury_type_defn(TypeDefn)),
            map.det_insert(TypeCtor, ComponentKind, !ComponentTypeMap)
        )
    else
        true
    ).

%---------------------%

:- pred add_foreign_if_word_aligned_ptr(module_info::in, decide_du_params::in,
    type_ctor::in, foreign_type_body::in,
    component_type_map::in, component_type_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_foreign_if_word_aligned_ptr(ModuleInfo, Params, TypeCtor,
        ForeignType, !ComponentTypeMap, !Specs) :-
    DirectArgMap = Params ^ ddp_direct_arg_map,
    ( if map.search(DirectArgMap, TypeCtor, _DirectArgFunctors) then
        DirectArgPieces = [words("Error:"), type_ctor_sna(TypeCtor),
            words("has a foreign language representation on this backend,"),
            words("but it also has a direct_arg specification."), nl],
        DirectArgMsg = simple_msg(term.context_init,
            [always(DirectArgPieces)]),
        DirectArgSpec = error_spec(severity_error, phase_type_check,
            [DirectArgMsg]),
        !:Specs = [DirectArgSpec | !.Specs]
    else
        true
    ),
    module_info_get_globals(ModuleInfo, Globals),
    globals.get_target(Globals, Target),
    ( if is_foreign_type_body_for_target(ForeignType, Target, Assertions) then
        ( if asserted_word_aligned_pointer(Assertions) then
            ComponentKind = is_word_aligned_ptr(foreign_type_assertion),
            map.det_insert(TypeCtor, ComponentKind, !ComponentTypeMap)
        else
            true
        )
    else
        unexpected($pred, "foreign type is not for this backend")
    ).

%---------------------%

:- pred add_abstract_if_packable(type_ctor::in, type_details_abstract::in,
    component_type_map::in, component_type_map::out) is det.

add_abstract_if_packable(TypeCtor, AbstractDetails,
        !ComponentTypeMap) :-
    (
        AbstractDetails = abstract_type_fits_in_n_bits(NumBits),
        % XXX TYPE_REPN We should get Fill from AbstractDetails.
        Fill = fill_enum,
        ComponentKind = packable(packable_n_bits(NumBits, Fill)),
        map.det_insert(TypeCtor, ComponentKind, !ComponentTypeMap)
    ;
        AbstractDetails = abstract_dummy_type,
        ComponentKind = packable(packable_dummy),
        map.det_insert(TypeCtor, ComponentKind, !ComponentTypeMap)
    ;
        ( AbstractDetails = abstract_type_general
        ; AbstractDetails = abstract_notag_type
        ; AbstractDetails = abstract_solver_type
        )
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Pass 2.
%

:- pred decide_if_complex_du_type(module_info::in, decide_du_params::in,
    component_type_map::in,
    pair(type_ctor, hlds_type_defn)::in, pair(type_ctor, hlds_type_defn)::out,
    set_tree234(type_ctor)::in, set_tree234(type_ctor)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

decide_if_complex_du_type(ModuleInfo, Params, ComponentTypeMap,
        TypeCtorTypeDefn0, TypeCtorTypeDefn,
        !MustBeSingleFunctorTagTypes, !Specs) :-
    TypeCtorTypeDefn0 = TypeCtor - TypeDefn0,
    get_type_defn_body(TypeDefn0, Body0),
    (
        Body0 = hlds_du_type(Ctors, MaybeCanonical, MaybeRepn0, _MaybeForeign),
        (
            MaybeRepn0 = yes(_),
            % We have already decided this type's representation
            % in the first pass.
            TypeCtorTypeDefn = TypeCtorTypeDefn0
        ;
            MaybeRepn0 = no,
            decide_complex_du_type(ModuleInfo, Params, ComponentTypeMap,
                TypeCtor, TypeDefn0, Ctors, MaybeCanonical, Repn,
                !MustBeSingleFunctorTagTypes, !Specs),
            Body = Body0 ^ du_type_repn := yes(Repn),
            set_type_defn_body(Body, TypeDefn0, TypeDefn),
            TypeCtorTypeDefn = TypeCtor - TypeDefn
        )
    ;
        ( Body0 = hlds_foreign_type(_)
        ; Body0 = hlds_abstract_type(_)
        ; Body0 = hlds_eqv_type(_)
        ; Body0 = hlds_solver_type(_)
        ),
        % There are no questions of representation to figure out.
        TypeCtorTypeDefn = TypeCtorTypeDefn0
    ).

:- pred decide_complex_du_type(module_info::in, decide_du_params::in,
    component_type_map::in, type_ctor::in, hlds_type_defn::in,
    list(constructor)::in, maybe_canonical::in, du_type_repn::out,
    set_tree234(type_ctor)::in, set_tree234(type_ctor)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

decide_complex_du_type(ModuleInfo, Params, ComponentTypeMap, TypeCtor,
        TypeDefn0, Ctors, MaybeCanonical, Repn,
        !MustBeSingleFunctorTagTypes, !Specs) :-
    ( if set_tree234.remove(TypeCtor, !MustBeSingleFunctorTagTypes) then
        ( if Ctors = [SingleCtorPrime] then
            SingleCtor = SingleCtorPrime
        else
            unexpected($pred, "unexpected type in MustBeSingleFunctorTagTypes")
        ),
        get_type_defn_status(TypeDefn0, TypeStatus),
        decide_complex_du_type_single_ctor(ModuleInfo, Params,
            ComponentTypeMap, TypeStatus, SingleCtor, Repn, !Specs)
    else if Ctors = [SingleCtor] then
        get_type_defn_status(TypeDefn0, TypeStatus),
        decide_complex_du_type_single_ctor(ModuleInfo, Params,
            ComponentTypeMap, TypeStatus, SingleCtor, Repn, !Specs)
    else
        decide_complex_du_type_general(ModuleInfo, Params, ComponentTypeMap,
            TypeCtor, TypeDefn0, Ctors, MaybeCanonical, Repn, !Specs)
    ).

%---------------------------------------------------------------------------%

:- pred decide_complex_du_type_single_ctor(module_info::in,
    decide_du_params::in, component_type_map::in,
    type_status::in, constructor::in, du_type_repn::out,
    list(error_spec)::in, list(error_spec)::out) is det.

decide_complex_du_type_single_ctor(ModuleInfo, Params, ComponentTypeMap,
        TypeStatus, SingleCtor, Repn, !Specs) :-
    SingleCtor = ctor(MaybeExistConstraints, SingleCtorSymName,
        SingleCtorArgs, SingleCtorArity, SingleCtorContext),
    SingleCtorTag = single_functor_tag,
    NumRemoteSecTagBits = 0,
    decide_complex_du_ctor_args(ModuleInfo, Params, ComponentTypeMap,
        TypeStatus, NumRemoteSecTagBits, SingleCtorTag, MaybeExistConstraints,
        SingleCtorSymName, SingleCtorContext,
        SingleCtorArgs, SingleCtorArgRepns, !Specs),
    SingleCtorRepn = ctor_repn(MaybeExistConstraints,
        SingleCtorSymName, SingleCtorTag,
        SingleCtorArgRepns, SingleCtorArity, SingleCtorContext),

    CtorRepnMap = map.singleton(unqualify_name(SingleCtorSymName),
        one_or_more(SingleCtorRepn, [])),
    DuTypeKind = du_type_kind_general,
    MaybeDirectArgFunctors = no,
    Repn = du_type_repn([SingleCtorRepn], CtorRepnMap, no_cheaper_tag_test,
        DuTypeKind, MaybeDirectArgFunctors).

%---------------------------------------------------------------------------%

:- pred decide_complex_du_type_general(module_info::in, decide_du_params::in,
    component_type_map::in, type_ctor::in, hlds_type_defn::in,
    list(constructor)::in, maybe_canonical::in, du_type_repn::out,
    list(error_spec)::in, list(error_spec)::out) is det.

decide_complex_du_type_general(ModuleInfo, Params, ComponentTypeMap,
        TypeCtor, TypeDefn0, Ctors, _MaybeCanonical, Repn, !Specs) :-
    get_type_defn_status(TypeDefn0, TypeStatus),
    Target = Params ^ ddp_target,
    UsesConstructors = target_uses_constructors(Target),
    (
        UsesConstructors = no,
        AddedBy = sectag_added_by_unify
    ;
        UsesConstructors = yes,
        AddedBy = sectag_added_by_constructor
    ),
    MaybePrimaryTags = Params ^ ddp_maybe_primary_tags,
    (
        MaybePrimaryTags = no_primary_tags,
        assign_tags_to_non_direct_arg_functors(TypeCtor, ptag(0u8), 0u8,
            AddedBy, Ctors, NumRemoteSecTags, map.init, CtorTagMap),
        DirectArgFunctorNames = []
    ;
        MaybePrimaryTags = max_primary_tag(MaxPtag, NumPtagBits),
        separate_out_constants(Ctors, Constants, Functors),
        MaybeDirectArgs = Params ^ ddp_maybe_direct_args,
        (
            MaybeDirectArgs = direct_args_enabled,
            TypeCtor = type_ctor(TypeCtorSymName, _TypeCtorArity),
            det_sym_name_get_module_name(TypeCtorSymName, TypeCtorModuleName),
            DirectArgMap = Params ^ ddp_direct_arg_map,
            ( if map.search(DirectArgMap, TypeCtor, DirectArgMapEntry) then
                AssertedDirectArgFunctors = DirectArgMapEntry
            else
                AssertedDirectArgFunctors = []
            ),
            TypeIsImported = type_status_is_imported(TypeStatus),
            TypeDefinedHere = type_status_defined_in_this_module(TypeStatus),
            list.filter(
                is_direct_arg_ctor(ComponentTypeMap, TypeCtorModuleName,
                    TypeStatus, TypeIsImported, TypeDefinedHere,
                    AssertedDirectArgFunctors),
                Functors, DirectArgFunctors, NonDirectArgFunctors),
            check_direct_arg_assertions(AssertedDirectArgFunctors,
                NonDirectArgFunctors, !Specs),
            DirectArgFunctorNames =
                list.map(constructor_to_sym_name_and_arity, DirectArgFunctors)
        ;
            MaybeDirectArgs = direct_args_disabled,
            DirectArgFunctors = [],
            DirectArgFunctorNames = [],
            NonDirectArgFunctors = Functors
        ),
        some [!CurPtagUint8, !CtorTagMap] (
            !:CurPtagUint8 = 0u8,
            map.init(!:CtorTagMap),
            (
                Constants = []
            ;
                Constants = [_ | _],
                ConstantsPtag = ptag(!.CurPtagUint8),
                CurLocalSecTag = 0u,
                compute_sectag_bits(Constants, SectagBits),
                assign_tags_to_constants(TypeCtor, ConstantsPtag, NumPtagBits,
                    SectagBits, CurLocalSecTag, Constants, !CtorTagMap),
                !:CurPtagUint8 = !.CurPtagUint8 + 1u8
            ),
            assign_tags_to_direct_arg_functors(TypeCtor, MaxPtag,
                !CurPtagUint8, DirectArgFunctors, NonDirectArgFunctors,
                LeftOverDirectArgFunctors, !CtorTagMap),
            assign_tags_to_non_direct_arg_functors(TypeCtor, MaxPtag,
                !.CurPtagUint8, AddedBy,
                LeftOverDirectArgFunctors ++ NonDirectArgFunctors,
                NumRemoteSecTags, !CtorTagMap),
            CtorTagMap = !.CtorTagMap
        )
    ),
    ( if NumRemoteSecTags = 0u then
        NumRemoteSecTagBits = 0
    else
        NumRemoteSecTagsInt = uint.cast_to_int(NumRemoteSecTags),
        % XXX int.log2 should actually be uint.log2.
        int.log2(NumRemoteSecTagsInt, NumRemoteSecTagBits)
    ),
    list.map_foldl2(
        decide_complex_du_type_ctor(ModuleInfo, Params, ComponentTypeMap,
            TypeCtor, TypeStatus, CtorTagMap, NumRemoteSecTagBits),
        Ctors, CtorRepns, map.init, CtorRepnMap, !Specs),
    compute_cheaper_tag_test(TypeCtor, CtorRepns, CheaperTagTest),
    % XXX TYPE_REPN The maybe() wrapper is unnecessary; the info it contains
    % is already present in the nil vs cons distinction on the list.
    (
        DirectArgFunctorNames = [],
        MaybeDirectArgFunctorNames = no
    ;
        DirectArgFunctorNames = [_ | _],
        MaybeDirectArgFunctorNames = yes(DirectArgFunctorNames)
    ),
    Repn = du_type_repn(CtorRepns, CtorRepnMap, CheaperTagTest,
        du_type_kind_general, MaybeDirectArgFunctorNames).

%---------------------------------------------------------------------------%

:- pred decide_complex_du_type_ctor(module_info::in, decide_du_params::in,
    component_type_map::in, type_ctor::in, type_status::in,
    cons_id_to_tag_map::in, int::in, constructor::in, constructor_repn::out,
    ctor_name_to_repn_map::in, ctor_name_to_repn_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

decide_complex_du_type_ctor(ModuleInfo, Params, ComponentTypeMap,
        TypeCtor, TypeStatus, CtorTagMap, NumRemoteSecTagBits,
        Ctor, CtorRepn, !CtorRepnMap, !Specs) :-
    Ctor = ctor(MaybeExistConstraints, CtorSymName,
        CtorArgs, CtorArity, CtorContext),
    ConsId = cons(CtorSymName, CtorArity, TypeCtor),
    map.lookup(CtorTagMap, ConsId, CtorTag),
    decide_complex_du_ctor_args(ModuleInfo, Params, ComponentTypeMap,
        TypeStatus, NumRemoteSecTagBits, CtorTag, MaybeExistConstraints,
        CtorSymName, CtorContext, CtorArgs, CtorArgRepns, !Specs),
    CtorRepn = ctor_repn(MaybeExistConstraints, CtorSymName, CtorTag,
        CtorArgRepns, CtorArity, CtorContext),
    insert_ctor_repn_into_map(CtorRepn, !CtorRepnMap).

%---------------------------------------------------------------------------%

:- pred decide_complex_du_ctor_args(module_info::in, decide_du_params::in,
    component_type_map::in, type_status::in, int::in, cons_tag::in,
    maybe_cons_exist_constraints::in, sym_name::in, prog_context::in,
    list(constructor_arg)::in, list(constructor_arg_repn)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

decide_complex_du_ctor_args(ModuleInfo, Params, ComponentTypeMap,
        TypeStatus, _NumRemoteSecTagBits, CtorTag, MaybeExistConstraints,
        CtorSymName, CtorContext, CtorArgs, CtorArgRepns, !Specs) :-
    ( if
        CtorTag = shared_remote_tag(_, RemoteSectag),
        % If the target uses constructors, then the *Mercury* compiler
        % is not responsible for adding the secondary tag to the start
        % of the memory cell, and for the purposes of unifications,
        % the cell starts *after* the tag.
        RemoteSectag ^ rsectag_added = sectag_added_by_unify
    then
        NumSecTagWords = 1
    else
        NumSecTagWords = 0
    ),
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

    % The memory cell for a term consists of, in order:
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
    % The calls to decide_complex_du_ctor_args_loop decide the representation
    % only of the last category. FirstArgWordNum measures offset with respect
    % to the last category only. FirstCellWordNum measures the offset
    % with respect to the start of the cell.
    %
    % TODO In the future, if NumExtraArgWords is really zero, we should pass
    % FirstArgWordNum = 0, FirstShift = NumRemoteSecTagBits, to allow any
    % initial sub-word-sized arguments to be packed together with the
    % secondary tag. If *any* ctor in a du type actually does any such packing,
    % then accessing the remote secondary tag on values of that type
    % will require applying a mask to the first word of the cell.
    % The simplest way to ensure finding all the places where this must be done
    % is to change the second argument of shared_remote_tag from an int
    % to something that corresponds either to apw_full_word (the current
    % arrangement where the remote secondary tag takes a full word) or to
    % apw_partial_first (the new, optimized arrangement).
    %
    % TODO An even more aggressive possible future optimization would apply
    % to ctors whose arguments take up less than one word in total:
    % these could be stored next to the primary tag. If a type has
    % more than one such ctor, they would need to be distinguished by
    % a local secondary tag between the primary tag and the packed arguments.
    % Unlike the first todo above, implementing this todo will require
    % changing our current approach of (a) first deciding the tag of a ctor,
    % and (b) *then* deciding the representation of its arguments.
    %
    % Both of these changes would require changes to the representation of
    % RTTI, and therefore both to the compiler code that generates RTTI
    % and to the runtime code that interprets RTTI. Both changes would
    % probably need nontrivial bootstrapping.
    %
    FirstArgWordNum = 0,
    FirstCellWordNum = NumSecTagWords + NumExtraArgWords,
    FirstShift = 0,
    NoPackParams = ((Params
        ^ ddp_allow_packing_ints := no)
        ^ ddp_allow_packing_dummies := no),
    decide_complex_du_ctor_args_loop(ModuleInfo, NoPackParams, map.init,
        FirstArgWordNum, FirstCellWordNum, FirstShift,
        CtorArgs, CtorArgRepnsBase),
    decide_complex_du_ctor_args_loop(ModuleInfo, Params, ComponentTypeMap,
        FirstArgWordNum, FirstCellWordNum, FirstShift,
        CtorArgs, CtorArgRepnsPacked),
    WorthPacking = worth_arg_packing(CtorArgRepnsBase, CtorArgRepnsPacked),
    (
        WorthPacking = no,
        CtorArgRepns = CtorArgRepnsBase
    ;
        WorthPacking = yes,
        CtorArgRepns = CtorArgRepnsPacked
    ),
    MaybeInformPacking = Params ^ ddp_inform_suboptimal_pack,
    ( if
        MaybeInformPacking = inform_about_packing,
        type_status_defined_in_this_module(TypeStatus) = yes
    then
        inform_about_any_suboptimal_packing(Params, CtorSymName, CtorContext,
            CtorArgRepns, !Specs)
    else
        true
    ).

:- func target_uses_constructors(compilation_target) = bool.

target_uses_constructors(target_c) = no.
target_uses_constructors(target_csharp) = yes.
target_uses_constructors(target_java) = yes.
target_uses_constructors(target_erlang) = no.
% NOTE The information here is repeated in ml_target_uses_constructors in
% ml_type_gen.m; any changes here will require corresponding changes there.

:- pred decide_complex_du_ctor_args_loop(module_info::in, decide_du_params::in,
    component_type_map::in, int::in, int::in, int::in,
    list(constructor_arg)::in, list(constructor_arg_repn)::out) is det.

decide_complex_du_ctor_args_loop(_, _, _, _, _, _, [], []).
decide_complex_du_ctor_args_loop(ModuleInfo, Params, ComponentTypeMap,
        CurAOWordNum, CurCellWordNum, CurShift,
        [Arg | Args], [ArgRepn | ArgRepns]) :-
    Arg = ctor_arg(ArgName, ArgType, ArgContext),
    ( if may_pack_arg_type(Params, ComponentTypeMap, ArgType, Packable) then
        (
            Packable = packable_n_bits(NumArgBits, FillKind),
            ArgNumBits = arg_num_bits(NumArgBits),
            ArgMaskInt = int.pow(2, NumArgBits) - 1,
            ArgMask = arg_mask(ArgMaskInt),
            % Try to place Arg in the current word.
            % If it does not fit, move on to the next word.
            ( if CurShift + NumArgBits =< Params ^ ddp_arg_pack_bits then
                ArgOnlyOffset0 = arg_only_offset(CurAOWordNum),
                CellOffset0 = cell_offset(CurCellWordNum),
                ( if CurShift = 0 then
                    ArgPosWidth0 = apw_partial_first(ArgOnlyOffset0,
                        CellOffset0, ArgNumBits, ArgMask, FillKind)
                else
                    ArgPosWidth0 = apw_partial_shifted(ArgOnlyOffset0,
                        CellOffset0, arg_shift(CurShift), ArgNumBits,
                        ArgMask, FillKind)
                ),
                NextAOWordNum = CurAOWordNum,
                NextCellWordNum = CurCellWordNum,
                NextShift = CurShift + NumArgBits
            else
                padding_increment(CurShift, PaddingIncrement),
                AfterPaddingAOWordNum = CurAOWordNum + PaddingIncrement,
                AfterPaddingCellWordNum = CurCellWordNum + PaddingIncrement,
                ArgOnlyOffset0 = arg_only_offset(AfterPaddingAOWordNum),
                CellOffset0 = cell_offset(AfterPaddingCellWordNum),
                ArgPosWidth0 = apw_partial_first(ArgOnlyOffset0, CellOffset0,
                    ArgNumBits, ArgMask, FillKind),
                NextAOWordNum = AfterPaddingAOWordNum,
                NextCellWordNum = AfterPaddingCellWordNum,
                NextShift = NumArgBits
            )
        ;
            Packable = packable_dummy,
            ( if CurShift = 0 then
                ArgPosWidth0 = apw_none_nowhere
            else
                ArgOnlyOffset0 = arg_only_offset(CurAOWordNum),
                CellOffset0 = cell_offset(CurCellWordNum),
                ArgPosWidth0 = apw_none_shifted(ArgOnlyOffset0, CellOffset0)
            ),
            NextAOWordNum = CurAOWordNum,
            NextCellWordNum = CurCellWordNum,
            NextShift = CurShift
        ),
        decide_complex_du_ctor_args_loop(ModuleInfo, Params, ComponentTypeMap,
            NextAOWordNum, NextCellWordNum, NextShift, Args, ArgRepns),
        (
            ArgPosWidth0 = apw_partial_first(ArgOnlyOffset, CellOffset,
                _, _, _),
            % If this argument starts a word, then it is a *partial* word
            % only if (a) there is a next argument, and (b) it is packed
            % with it. Otherwise, it is not packed.
            ( if
                ArgRepns = [NextArgRepn | _],
                NextArgPosWidth = NextArgRepn ^ car_pos_width,
                ( NextArgPosWidth = apw_partial_shifted(_, _, _, _, _, _)
                ; NextArgPosWidth = apw_none_shifted(_, _)
                )
            then
                ArgPosWidth = ArgPosWidth0
            else
                ArgPosWidth = apw_full(ArgOnlyOffset, CellOffset)
            )
        ;
            ArgPosWidth0 = apw_none_shifted(_, _),
            % We represent a dummy argument as apw_none_shifted
            % only if it is packed with other sub-word arguments both
            % before it and after it. The "before it" part was tested above.
            % Here we test the "after it" part.
            ( if
                ArgRepns = [NextArgRepn | _],
                NextArgPosWidth = NextArgRepn ^ car_pos_width,
                ( NextArgPosWidth = apw_partial_shifted(_, _, _, _, _, _)
                ; NextArgPosWidth = apw_none_shifted(_, _)
                )
            then
                ArgPosWidth = ArgPosWidth0
            else
                ArgPosWidth = apw_none_nowhere
            )
        ;
            ArgPosWidth0 = apw_partial_shifted(_, _, _, _, _, _),
            % If this argument is shifted, then it is packed together
            % with whatever came before.
            ArgPosWidth = ArgPosWidth0
        ;
            ArgPosWidth0 = apw_none_nowhere,
            % This argument is effectively a zero-width sliver either
            % between two complete words, or after a complete word
            % at the end of the cell.
            ArgPosWidth = ArgPosWidth0
        ),
        ArgRepn = ctor_arg_repn(ArgName, ArgType, ArgPosWidth, ArgContext)
    else
        padding_increment(CurShift, PaddingIncrement),
        AfterPaddingAOWordNum = CurAOWordNum + PaddingIncrement,
        AfterPaddingCellWordNum = CurCellWordNum + PaddingIncrement,
        ArgOnlyOffset = arg_only_offset(AfterPaddingAOWordNum),
        CellOffset = cell_offset(AfterPaddingCellWordNum),
        deref_eqv_types(ModuleInfo, ArgType, DerefArgType),
        ( if
            DerefArgType = builtin_type(BuiltinType),
            (
                BuiltinType = builtin_type_float,
                Params ^ ddp_double_word_floats = use_double_word_floats,
                DWKind = dw_float
            ;
                BuiltinType = builtin_type_int(int_type_int64),
                Params ^ ddp_double_word_int64s = use_double_word_int64s,
                % XXX ARG_PACK For bootstrapping.
                Params ^ ddp_allow_double_word_ints = yes,
                DWKind = dw_int64
            ;
                BuiltinType = builtin_type_int(int_type_uint64),
                Params ^ ddp_double_word_int64s = use_double_word_int64s,
                % XXX ARG_PACK For bootstrapping.
                Params ^ ddp_allow_double_word_ints = yes,
                DWKind = dw_uint64
            )
        then
            ArgPosWidth = apw_double(ArgOnlyOffset, CellOffset, DWKind),
            NextAOWordNum = AfterPaddingAOWordNum + 2,
            NextCellWordNum = AfterPaddingCellWordNum + 2
        else
            ArgPosWidth = apw_full(ArgOnlyOffset, CellOffset),
            NextAOWordNum = AfterPaddingAOWordNum + 1,
            NextCellWordNum = AfterPaddingCellWordNum + 1
        ),
        ArgRepn = ctor_arg_repn(ArgName, ArgType, ArgPosWidth, ArgContext),
        NextShift = 0,
        decide_complex_du_ctor_args_loop(ModuleInfo, Params, ComponentTypeMap,
            NextAOWordNum, NextCellWordNum, NextShift, Args, ArgRepns)
    ).

:- pred may_pack_arg_type(decide_du_params::in, component_type_map::in,
    mer_type::in, packable_kind::out) is semidet.

may_pack_arg_type(Params, ComponentTypeMap, ArgType, PackableKind) :-
    % XXX ARG_PACK Make this code dereference eqv types,
    % subject to all types involved having the same visibility.
    type_to_ctor(ArgType, ArgTypeCtor),
    ( if map.search(ComponentTypeMap, ArgTypeCtor, ComponentKind) then
        ComponentKind = packable(PackableKind),
        (
            PackableKind = packable_n_bits(NumArgBits, _FillKind),
            NumArgBits < Params ^ ddp_arg_pack_bits
        ;
            PackableKind = packable_dummy,
            % XXX ARG_PACK For bootstrapping.
            Params ^ ddp_allow_packing_dummies = yes
        )
    else
        ArgType = builtin_type(builtin_type_int(ArgIntType)),
        Params ^ ddp_allow_packing_ints = yes,
        (
            (
                ArgIntType = int_type_int8,
                NumArgBits = 8,
                FillKind = fill_int8
            ;
                ArgIntType = int_type_int16,
                NumArgBits = 16,
                FillKind = fill_int16
            ;
                ArgIntType = int_type_int32,
                NumArgBits = 32,
                NumArgBits < Params ^ ddp_arg_pack_bits,
                FillKind = fill_int32
            )
        ;
            (
                ArgIntType = int_type_uint8,
                NumArgBits = 8,
                FillKind = fill_uint8
            ;
                ArgIntType = int_type_uint16,
                NumArgBits = 16,
                FillKind = fill_uint16
            ;
                ArgIntType = int_type_uint32,
                NumArgBits = 32,
                NumArgBits < Params ^ ddp_arg_pack_bits,
                FillKind = fill_uint32
            )
        ),
        PackableKind = packable_n_bits(NumArgBits, FillKind)
    ).

:- pred padding_increment(int::in, int::out) is det.

padding_increment(CurShift, PaddingIncrement) :-
    ( if CurShift = 0 then
        % No padding is needed.
        PaddingIncrement = 0
    else
        % The part of CurWordNum after CurShift is padding.
        PaddingIncrement = 1
    ).

%---------------------------------------------------------------------------%

:- pred assign_tags_to_constants(type_ctor::in, ptag::in, int::in,
    sectag_bits::in, uint::in, list(constructor)::in,
    cons_id_to_tag_map::in, cons_id_to_tag_map::out) is det.

assign_tags_to_constants(_, _, _, _, _, [], !CtorTagMap).
assign_tags_to_constants(TypeCtor, Ptag, NumPtagBits, SectagBits, CurSecTag,
        [Ctor | Ctors], !CtorTagMap) :-
    Ctor = ctor(_MaybeExistConstraints, SymName, _Args, Arity, _Context),
    ConsId = cons(SymName, Arity, TypeCtor),
    Ptag = ptag(PtagUint8),
    WholeWord = (CurSecTag << NumPtagBits) \/ uint8.cast_to_uint(PtagUint8),
    % XXX SectagBits is currently ignored.
    LocalSectag = local_sectag(CurSecTag, lsectag_rest_of_word(WholeWord)),
    ConsTag = shared_local_tag(Ptag, LocalSectag),
    map.det_insert(ConsId, ConsTag, !CtorTagMap),
    assign_tags_to_constants(TypeCtor, Ptag, NumPtagBits,
        SectagBits, CurSecTag + 1u, Ctors, !CtorTagMap).

:- pred assign_tags_to_direct_arg_functors(type_ctor::in,
    ptag::in, uint8::in, uint8::out,
    list(constructor)::in, list(constructor)::in, list(constructor)::out,
    cons_id_to_tag_map::in, cons_id_to_tag_map::out) is det.

assign_tags_to_direct_arg_functors(_, _, !CurPtag, [], _, [], !CtorTagMap).
assign_tags_to_direct_arg_functors(TypeCtor, MaxPtag, !CurPtagUint8,
        [DirectArgCtor | DirectArgCtors], NonDirectArgCtors, LeftOverCtors,
        !CtorTagMap) :-
    DirectArgCtor = ctor(_MaybeExistConstraints, Name, _Args, Arity, _Context),
    ConsId = cons(Name, Arity, TypeCtor),
    MaxPtag = ptag(MaxPtagUint8),
    ( if
        % If we are about to run out of unshared tags, stop, and return
        % the leftovers.
        !.CurPtagUint8 = MaxPtagUint8,
        ( DirectArgCtors = [_ | _]
        ; NonDirectArgCtors = [_ | _]
        )
    then
        LeftOverCtors = [DirectArgCtor | DirectArgCtors]
    else
        ConsTag = direct_arg_tag(ptag(!.CurPtagUint8)),
        map.det_insert(ConsId, ConsTag, !CtorTagMap),
        !:CurPtagUint8 = !.CurPtagUint8 + 1u8,
        assign_tags_to_direct_arg_functors(TypeCtor, MaxPtag, !CurPtagUint8,
            DirectArgCtors, NonDirectArgCtors, LeftOverCtors, !CtorTagMap)
    ).

:- pred assign_tags_to_non_direct_arg_functors(type_ctor::in,
    ptag::in, uint8::in, sectag_added_by::in, list(constructor)::in,
    uint::out, cons_id_to_tag_map::in, cons_id_to_tag_map::out) is det.

assign_tags_to_non_direct_arg_functors(_, _, _, _, [], 0u, !CtorTagMap).
assign_tags_to_non_direct_arg_functors(TypeCtor, MaxPtag, !.CurPtagUint8,
        AddedBy, [Ctor | Ctors], NumRemoteSecTags, !CtorTagMap) :-
    Ctor = ctor(_MaybeExistConstraints, Name, _Args, Arity, _Context),
    ConsId = cons(Name, Arity, TypeCtor),
    MaxPtag = ptag(MaxPtagUint8),
    ( if
        % If we are about to run out of unshared tags, start assigning
        % shared remote tags instead.
        !.CurPtagUint8 = MaxPtagUint8,
        Ctors = [_ | _]
    then
        CurRemoteSecTag0 = 0u,
        assign_shared_remote_tags_to_non_direct_arg_functors(TypeCtor,
            ptag(!.CurPtagUint8), AddedBy, [Ctor | Ctors],
            CurRemoteSecTag0, CurRemoteSecTag, !CtorTagMap),
        % We assigned remote sec tags 0 .. CurRemoteSecTag-1,
        % which is CurRemoteSecTag sec tags.
        NumRemoteSecTags = CurRemoteSecTag
    else
        ConsTag = unshared_tag(ptag(!.CurPtagUint8)),
        map.det_insert(ConsId, ConsTag, !CtorTagMap),
        !:CurPtagUint8 = !.CurPtagUint8 + 1u8,
        assign_tags_to_non_direct_arg_functors(TypeCtor, MaxPtag,
            !.CurPtagUint8, AddedBy, Ctors, NumRemoteSecTags, !CtorTagMap)
    ).

:- pred assign_shared_remote_tags_to_non_direct_arg_functors(type_ctor::in,
    ptag::in, sectag_added_by::in, list(constructor)::in, uint::in, uint::out,
    cons_id_to_tag_map::in, cons_id_to_tag_map::out) is det.

assign_shared_remote_tags_to_non_direct_arg_functors(_, _, _,
        [], !CurRemoteSecTag, !CtorTagMap).
assign_shared_remote_tags_to_non_direct_arg_functors(TypeCtor,
        Ptag, AddedBy, [Ctor | Ctors], !CurRemoteSecTag, !CtorTagMap) :-
    Ctor = ctor(_MaybeExistConstraints, SymName, _Args, Arity, _Context),
    ConsId = cons(SymName, Arity, TypeCtor),
    RemoteSectag = remote_sectag(!.CurRemoteSecTag, AddedBy),
    ConsTag = shared_remote_tag(Ptag, RemoteSectag),
    map.det_insert(ConsId, ConsTag, !CtorTagMap),
    !:CurRemoteSecTag = !.CurRemoteSecTag + 1u,
    assign_shared_remote_tags_to_non_direct_arg_functors(TypeCtor,
        Ptag, AddedBy, Ctors, !CurRemoteSecTag, !CtorTagMap).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Utility predicates.
% XXX TYPE_REPN Rationalise the order of the predicates from here onwards.
%

:- func worth_arg_packing(
    list(constructor_arg_repn), list(constructor_arg_repn)) = bool.

worth_arg_packing(UnpackedArgs, PackedArgs) = Worthwhile :-
    count_words(UnpackedArgs, 0, UnpackedLength),
    count_words(PackedArgs, 0, PackedLength),
    expect(PackedLength =< UnpackedLength, $pred,
        "packed length exceeds unpacked length"),
    worth_arg_packing_compare(UnpackedLength, PackedLength, Worthwhile).

:- pred worth_arg_packing_compare(int::in, int::in, bool::out) is det.

worth_arg_packing_compare(UnpackedLength, PackedLength, Worthwhile) :-
    % Boehm GC will round up allocations (at least) to the next even number
    % of words. There is no point saving a single word if that word will be
    % allocated anyway.
    % XXX TYPE_REPN Test this assertion. The saving in accesses to cache
    % and/or memory may be more important than the cost of shifts and masks.
    ( if PackedLength < UnpackedLength then
        ( if round_to_even(PackedLength) < round_to_even(UnpackedLength) then
            Worthwhile = yes
        else
            Worthwhile = no
        )
    else
        Worthwhile = no
    ).

:- pred count_words(list(constructor_arg_repn)::in, int::in, int::out) is det.

count_words([], !Count).
count_words([Arg | Args], !Count) :-
    ArgPosWidth = Arg ^ car_pos_width,
    (
        ( ArgPosWidth = apw_partial_shifted(_, _, _, _, _, _)
        ; ArgPosWidth = apw_none_shifted(_, _)
        ; ArgPosWidth = apw_none_nowhere
        )
    ;
        ( ArgPosWidth = apw_full(_, _)
        ; ArgPosWidth = apw_partial_first(_, _, _, _, _)
        ),
        !:Count = !.Count + 1
    ;
        ArgPosWidth = apw_double(_, _, _),
        !:Count = !.Count + 2
    ),
    count_words(Args, !Count).

:- func round_to_even(int) = int.

round_to_even(I) = E :-
    ( if int.even(I) then
        E = I
    else
        E = I + 1
    ).

%---------------------------------------------------------------------------%

:- pred is_direct_arg_ctor(component_type_map::in, module_name::in,
    type_status::in, bool::in, bool::in,
    list(sym_name_and_arity)::in, constructor::in) is semidet.

is_direct_arg_ctor(ComponentTypeMap, TypeCtorModule, TypeStatus,
        TypeIsImported, TypeDefinedHere, AssertedDirectArgCtors, Ctor) :-
    Ctor = ctor(MaybeExistConstraints, ConsSymName, ConsArgs, ConsArity,
        _CtorContext),
    MaybeExistConstraints = no_exist_constraints,
    ConsArgs = [ConsArg],
    expect(unify(ConsArity, 1), $pred, "ConsArity != 1"),
    ConsArg = ctor_arg(_MaybeFieldName, ArgType, _ArgContext),
    type_to_ctor_and_args(ArgType, ArgTypeCtor, ArgTypeCtorArgTypes),

    ConsConsId = sym_name_arity(ConsSymName, ConsArity),
    ( if
        % Trust the `direct_arg' attribute of an imported type.
        TypeIsImported = yes,
        list.contains(AssertedDirectArgCtors, ConsConsId)
    then
        ArgCond = direct_arg_asserted
    else if
        % Tuples are always acceptable argument types as they are represented
        % by word-aligned vector pointers.
        % Strings are *not* always word-aligned (yet) so are not acceptable.
        type_ctor_is_tuple(ArgTypeCtor)
    then
        ArgCond = arg_type_is_word_aligned_pointer
    else
        map.search(ComponentTypeMap, ArgTypeCtor, ArgComponentKind),
        ArgComponentKind = is_word_aligned_ptr(WordAlignedWhy),
        (
            WordAlignedWhy = foreign_type_assertion,
            ArgCond = arg_type_is_word_aligned_pointer
        ;
            WordAlignedWhy = mercury_type_defn(ArgTypeDefn),
            % The argument type is not a foreign type.

            % XXX TYPE_REPN Should be able to delete this test, since it
            % duplicates one that was done when adding this entry to
            % ComponentTypeMap.
            ArgTypeCtorArgTypes = [],
            % XXX We could let this be a subset of the type params, but that
            % would require the runtime system to be able to handle variables
            % in the argument type, during unification and comparison
            % (mercury_unify_compare_body.h) during deconstruction
            % (mercury_ml_expand_body.h), during deep copying
            % (mercury_deep_copy_body.h), and maybe during some other
            % operations.

            get_type_defn_body(ArgTypeDefn, ArgTypeDefnBody),
            ArgTypeDefnBody = hlds_du_type(_ArgCtors, _ArgMaybeUserEqComp,
                _ArgMaybeRepn, ArgMaybeForeign),

            ArgMaybeForeign = no,

            ( if
                TypeDefinedHere = yes,
                list.contains(AssertedDirectArgCtors, ConsConsId)
            then
                ArgCond = direct_arg_asserted
            else
                ArgTypeCtor = type_ctor(ArgTypeCtorSymName, _ArgTypeCtorArity),
                sym_name_get_module_name(ArgTypeCtorSymName,
                    ArgTypeCtorModule),
                ( if TypeCtorModule = ArgTypeCtorModule then
                    get_type_defn_status(ArgTypeDefn, ArgTypeStatus),
                    ArgCond = arg_type_defined_in_same_module(ArgTypeStatus)
                else
                    ArgCond = arg_type_defined_in_different_module
                )
            )
        )
    ),
    module_visibilities_allow_direct_arg(TypeStatus, ArgCond) = yes.

:- type direct_arg_cond
    --->    direct_arg_asserted
            % The constructor being checked has a single argument, and a
            % `where direct_arg' attribute asserts that the direct arg
            % representation may be used for the constructor.

    ;       arg_type_is_word_aligned_pointer
            % The constructor being checked has a single argument, and either
            % the argument has a builtin type that is represented with a
            % word-aligned pointer, or the argument has a foreign type with the
            % `word_aligned_pointer' assertion.

    ;       arg_type_defined_in_same_module(type_status)
            % The constructor being checked has a single argument, and the
            % argument type is defined in the same module as the constructor.
            % The argument type has the given import status.

    ;       arg_type_defined_in_different_module.
            % The constructor being checked has a single argument, and the
            % argument type is defined in a different module from the
            % constructor.

    % When this predicate is called, we should have checked that
    % the constructor has a single argument, and the argument has a type
    % such that the direct arg functor representation may apply
    % to the constructor. We still need to check that other modules
    % would infer the same type representation for the same constructor,
    % given that they may not have the same knowledge of the constructor's type
    % or the argument type.
    %
    % TypeStatus is import status of the type of the constructor being checked.
    %
:- func module_visibilities_allow_direct_arg(type_status, direct_arg_cond)
    = bool.

module_visibilities_allow_direct_arg(TypeStatus, ArgCond) = AllowDirectArg :-
    % XXX STATUS
    TypeStatus = type_status(OldImportStatus),
    (
        % If the outer type _definition_ is not exported from this module,
        % then the direct arg representation may be used. In the absence of
        % intermodule optimisation, only this module can [de]construct values
        % of this type.
        ( OldImportStatus = status_local
        ; OldImportStatus = status_abstract_exported
        ),
        AllowDirectArg = yes
    ;
        % If the outer type is opt-exported, another module may opt-import this
        % type, but abstract-import the argument type. It could not then infer
        % if the direct arg representation is required for any functors of the
        % outer type. The problem is overcome by adding `where direct_arg'
        % attributes to the opt-exported type definition in .opt files,
        % which state the functors that require the direct arg representation.
        OldImportStatus = status_opt_exported,
        AllowDirectArg = yes
    ;
        % If the outer type is exported from this module, then the direct arg
        % representation may be used, so long as any importing modules will
        % infer the same thing.
        ( OldImportStatus = status_exported
        ; OldImportStatus = status_exported_to_submodules
        ),
        (
            ( ArgCond = direct_arg_asserted
            ; ArgCond = arg_type_is_word_aligned_pointer
            ),
            AllowDirectArg = yes
        ;
            ArgCond = arg_type_defined_in_same_module(ArgTypeStatus),
            ArgTypeStatus = type_status(ArgOldTypeStatus),
            ( if
                (
                    OldImportStatus = status_exported,
                    ArgOldTypeStatus = status_exported
                ;
                    % If the wrapper type is exported to submodules only, then
                    % the only modules whose access to the argument type
                    % matters is those submodules. Each of these
                    % ArgOldTypeStatus values allows these submodules access
                    % to the argument type's definition. The fact that some
                    % of them also allow other modules access doesn't matter,
                    % because (due to their lack of visibility to the wrapper
                    % type) the question of the wrapper's type representation
                    % won't come up in them.
                    OldImportStatus = status_exported_to_submodules,
                    ( ArgOldTypeStatus = status_exported
                    ; ArgOldTypeStatus = status_exported_to_submodules
                    ; ArgOldTypeStatus = status_abstract_exported
                    )
                )
            then
                AllowDirectArg = yes
            else
                AllowDirectArg = no
            )
        ;
            ArgCond = arg_type_defined_in_different_module,
            AllowDirectArg = no
        )
    ;
        % The direct arg representation is required if the type of the
        % constructor being checked is imported, and:
        % - if a `where direct_arg' attribute says so
        % - if the argument value is a word-aligned pointer
        % - if the argument type is imported from the same module
        OldImportStatus = status_imported(TypeImportLocn),
        (
            ( ArgCond = direct_arg_asserted
            ; ArgCond = arg_type_is_word_aligned_pointer
            ),
            AllowDirectArg = yes
        ;
            ArgCond = arg_type_defined_in_same_module(ArgTypeStatus),
            ( if
                ArgTypeStatus = type_status(status_imported(ArgImportLocn)),
                % If the argument type is only exported by an ancestor to its
                % submodules (of which we are one), the outer type must also
                % only be exported to submodules. Otherwise submodules and
                % non-submodules would infer different things.
                (
                    ArgImportLocn =
                        import_locn_ancestor_private_interface_proper
                =>
                    TypeImportLocn =
                        import_locn_ancestor_private_interface_proper
                )
            then
                AllowDirectArg = yes
            else
                AllowDirectArg = no
            )
        ;
            ArgCond = arg_type_defined_in_different_module,
            AllowDirectArg = no
        )
    ;
        % If the outer type is opt-imported, there will always be a
        % `where direct_arg' attribute on the type definition which states
        % if the direct argument representation must be used.
        ( OldImportStatus = status_opt_imported
        ; OldImportStatus = status_abstract_imported
        ),
        (
            ArgCond = direct_arg_asserted,
            AllowDirectArg = yes
        ;
            ( ArgCond = arg_type_is_word_aligned_pointer
            ; ArgCond = arg_type_defined_in_same_module(_)
            ; ArgCond = arg_type_defined_in_different_module
            ),
            AllowDirectArg = no
        )
    ;
        ( OldImportStatus = status_external(_)
        ; OldImportStatus = status_pseudo_exported
        ; OldImportStatus = status_pseudo_imported
        ),
        unexpected($module, $pred, "inappropriate status for type")
    ).

:- pred is_foreign_type_body_for_target(foreign_type_body::in,
    compilation_target::in, foreign_type_assertions::out) is semidet.

is_foreign_type_body_for_target(ForeignType, Target, Assertions) :-
    (
        Target = target_c,
        ForeignType ^ c = yes(foreign_type_lang_data(_, _, Assertions))
    ;
        Target = target_java,
        ForeignType ^ java = yes(foreign_type_lang_data(_, _, Assertions))
    ;
        Target = target_csharp,
        ForeignType ^ csharp = yes(foreign_type_lang_data(_, _, Assertions))
    ;
        Target = target_erlang,
        ForeignType ^ erlang = yes(foreign_type_lang_data(_, _, Assertions))
    ).

%---------------------------------------------------------------------------%

    % check_direct_arg_assertions(AssertedDirectArgCtors, NonDirectArgCtors,
    %   !Specs):
    %
    % The caller should pass the list of constructors that have been determined
    % not to be direct_arg constructors. If any of these constructors
    % nevertheless appears in AssertedDirectArgCtors, generate an error message
    % for it.
    %
:- pred check_direct_arg_assertions(list(sym_name_and_arity)::in,
    list(constructor)::in, list(error_spec)::in, list(error_spec)::out) is det.

check_direct_arg_assertions(_AssertedDirectArgCtors, [], !Specs).
check_direct_arg_assertions(AssertedDirectArgCtors, [Ctor | Ctors], !Specs) :-
    Ctor = ctor(_, SymName, _Args, Arity, Context),
    SymNameArity = sym_name_arity(SymName, Arity),
    ( if list.contains(AssertedDirectArgCtors, SymNameArity) then
        Pieces = [words("Error:"),
            unqual_sym_name_and_arity(sym_name_arity(SymName, Arity)),
            words("cannot be represented as a direct pointer"),
            words("to its sole argument."), nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_type_check, [Msg]),
        !:Specs = [Spec | !.Specs]
    else
        true
    ),
    check_direct_arg_assertions(AssertedDirectArgCtors, Ctors, !Specs).

:- func constructor_to_sym_name_and_arity(constructor) = sym_name_and_arity.

constructor_to_sym_name_and_arity(ctor(_, Name, _Args, Arity, _)) =
    sym_name_arity(Name, Arity).

%---------------------------------------------------------------------------%

    % For data types with exactly two alternatives, one of which is a constant,
    % we can test against the constant (negating the result of the test,
    % if needed), since a test against a constant is cheaper than a tag test.
    %
:- pred compute_cheaper_tag_test(type_ctor::in, list(constructor_repn)::in,
    maybe_cheaper_tag_test::out) is det.

compute_cheaper_tag_test(TypeCtor, CtorRepns, CheaperTagTest) :-
    ( if CtorRepns = [CtorRepnA, CtorRepnB] then
        CtorRepnA = ctor_repn(_MaybeExistA, CtorSymNameA, CtorTagA,
            _CtorArgsA, CtorArityA, _CtorContextA),
        CtorRepnB = ctor_repn(_MaybeExistB, CtorSymNameB, CtorTagB,
            _CtorArgsB, CtorArityB, _CtorContextB),
        ( if
            CtorArityB = 0,
            CtorArityA > 0
        then
            ConsIdA = cons(CtorSymNameA, CtorArityA, TypeCtor),
            ConsIdB = cons(CtorSymNameB, CtorArityB, TypeCtor),
            CheaperTagTest = cheaper_tag_test(ConsIdA, CtorTagA,
                ConsIdB, CtorTagB)
        else if
            CtorArityA = 0,
            CtorArityB > 0
        then
            ConsIdA = cons(CtorSymNameA, CtorArityA, TypeCtor),
            ConsIdB = cons(CtorSymNameB, CtorArityB, TypeCtor),
            CheaperTagTest = cheaper_tag_test(ConsIdB, CtorTagB,
                ConsIdA, CtorTagA)
        else
            CheaperTagTest = no_cheaper_tag_test
        )
    else
        CheaperTagTest = no_cheaper_tag_test
    ).

%---------------------------------------------------------------------------%

:- pred inform_about_any_suboptimal_packing(decide_du_params::in,
    sym_name::in, prog_context::in, list(constructor_arg_repn)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

inform_about_any_suboptimal_packing(Params, CtorSymName, CtorContext,
        CtorArgRepns, !Specs) :-
    % Find the number of words we would need to store all the sub-word-sized
    % arguments in CtorArgRepns using the first-fit-decreasing algorithm
    % (see the wikipedia page on "Bin_packing_problem"). If this number,
    % NumSubWordBins, is smaller than ActualNumSubWords, then generate
    % an informational message giving SubWordBins as one possible better
    % packing order of the arguments than the one in the program.

    record_subword_args_and_count_their_words(CtorArgRepns, 0,
        [], SubWords, 0, ActualNumSubWords),
    list.sort(SubWords, SortedSubWords),
    list.reverse(SortedSubWords, RevSortedSubWords),
    BinSize = Params ^ ddp_arg_pack_bits,
    list.foldl(insert_subword_into_first_fit_bin(BinSize), RevSortedSubWords,
        [], SubWordBins),
    list.length(SubWordBins, NumSubWordBins),

    worth_arg_packing_compare(ActualNumSubWords, NumSubWordBins, WorthWhile),
    (
        WorthWhile = no
    ;
        WorthWhile = yes,
        list.length(CtorArgRepns, CtorArity),
        CtorSymNameArity = sym_name_arity(CtorSymName, CtorArity),
        StartPieces = [words("The arguments of the constructor"),
            unqual_sym_name_and_arity(CtorSymNameArity),
            words("could be packed more tightly."),
            words("Here is one arrangement for the arguments"),
            words("which take up less than one word each"),
            words("that would allow better packing."),
            words("(The position of the word sized arguments"),
            words("does not affect the effectiveness of the packing.)"), nl],
        EndPieces = [blank_line,
            words("This arrangement of the sub-word-sized arguments"),
            words("would take"), int_fixed(NumSubWordBins),
            words(choose_number(SubWordBins, "word", "words")), suffix(","),
            words("whereas their current arrangement takes"),
            int_fixed(ActualNumSubWords),
            words((if ActualNumSubWords = 1 then "word" else "words")),
            suffix("."), nl],
        list.map(describe_sub_word_bin, SubWordBins, SubWordBinPieceLists),
        Pieces = StartPieces ++ list.condense(SubWordBinPieceLists)
            ++ EndPieces,
        Msg = simple_msg(CtorContext, [always(Pieces)]),
        Spec = error_spec(severity_informational, phase_type_check, [Msg]),
        !:Specs = [Spec | !.Specs]
    ).

:- type field_id
    --->    field_id_name(string)
    ;       field_id_ordinal(int).

:- type sub_word
    --->    sub_word(
                sub_word_num_bits   :: int,
                sub_word_id         :: field_id
            ).

:- pred record_subword_args_and_count_their_words(
    list(constructor_arg_repn)::in, int::in,
    list(sub_word)::in, list(sub_word)::out, int::in, int::out) is det.

record_subword_args_and_count_their_words([], _, !SubWords, !NumWords).
record_subword_args_and_count_their_words([ArgRepn | ArgRepns], CurArgNum,
        !SubWords, !NumWords) :-
    ArgRepn = ctor_arg_repn(MaybeFieldName, _Type, PosWidth, _Context),
    (
        ( PosWidth = apw_full(_, _)
        ; PosWidth = apw_double(_, _, _)
        ; PosWidth = apw_none_nowhere
        ; PosWidth = apw_none_shifted(_, _)
        )
    ;
        (
            PosWidth = apw_partial_first(_, _, ArgNumBits, _, _),
            !:NumWords = !.NumWords + 1
        ;
            PosWidth = apw_partial_shifted(_, _, _, ArgNumBits, _, _)
        ),
        ArgNumBits = arg_num_bits(NumBits),
        (
            MaybeFieldName = yes(ctor_field_name(SymName, _FieldContext)),
            Name = unqualify_name(SymName),
            Id = field_id_name(Name)
        ;
            MaybeFieldName = no,
            Id = field_id_ordinal(CurArgNum)
        ),
        SubWord = sub_word(NumBits, Id),
        !:SubWords = [SubWord | !.SubWords]
    ),
    record_subword_args_and_count_their_words(ArgRepns, CurArgNum + 1,
        !SubWords, !NumWords).

:- type sub_word_bin
    --->    sub_word_bin(
                rev_sub_words_in_bin    :: list(sub_word),
                remaining_bits_in_bin   :: int
            ).

:- pred insert_subword_into_first_fit_bin(int::in, sub_word::in,
    list(sub_word_bin)::in, list(sub_word_bin)::out) is det.

insert_subword_into_first_fit_bin(BinSize, SubWord, Bins0, Bins) :-
    SubWord = sub_word(SubWordNumBits, _Id),
    (
        Bins0 = [],
        ( if SubWordNumBits < BinSize then
            Bins = [sub_word_bin([SubWord], BinSize - SubWordNumBits)]
        else
            unexpected($pred, "SubWordNumBits >= BinSize")
        )
    ;
        Bins0 = [HeadBin0 | TailBins0],
        HeadBin0 = sub_word_bin(RevSubWords0, RemainingBitsInBin0),
        ( if SubWordNumBits =< RemainingBitsInBin0 then
            RevSubWords = [SubWord | RevSubWords0],
            RemainingBitsInBin = RemainingBitsInBin0 - SubWordNumBits,
            HeadBin = sub_word_bin(RevSubWords, RemainingBitsInBin),
            Bins = [HeadBin | TailBins0]
        else
            insert_subword_into_first_fit_bin(BinSize, SubWord,
                TailBins0, TailBins),
            Bins = [HeadBin0 | TailBins]
        )
    ).

:- pred describe_sub_word_bin(sub_word_bin::in, list(format_component)::out)
    is det.

describe_sub_word_bin(SubWordBin, Pieces) :-
    SubWordBin = sub_word_bin(RevSubWords, _RemainingBits),
    list.reverse(RevSubWords, SubWords),
    list.map_foldl(describe_sub_word, SubWords, SubWordPieceLists,
        0, TotalNumBits),
    list.condense(SubWordPieceLists, SubWordPieces),
    Pieces = [blank_line,
        words("One word containing the following arguments:"), nl]
        ++ SubWordPieces ++
        [words("These total"), int_fixed(TotalNumBits),
        words((if TotalNumBits = 1 then "bit." else "bits.")), nl].

:- pred describe_sub_word(sub_word::in, list(format_component)::out,
    int::in, int::out) is det.

describe_sub_word(SubWord, Pieces, !TotalNumBits) :-
    SubWord = sub_word(SubWordNumBits, Id),
    !:TotalNumBits = !.TotalNumBits + SubWordNumBits,
    NumBitsStr = string.format("#bits = %d", [i(SubWordNumBits)]),
    (
        Id = field_id_ordinal(ArgNum),
        Pieces = [words("- the current"), nth_fixed(ArgNum),
            words("argument,"), fixed(NumBitsStr), nl]
    ;
        Id = field_id_name(ArgName),
        Pieces = [words("- the argument named"), quote(ArgName), suffix(","),
            fixed(NumBitsStr), nl]
    ).

%---------------------------------------------------------------------------%
%
% Auxiliary functions and predicates.
%

:- pred deref_eqv_types(module_info::in, mer_type::in, mer_type::out) is det.

deref_eqv_types(ModuleInfo, Type0, Type) :-
    ( if type_to_type_defn_body(ModuleInfo, Type0, TypeBody0) then
        ( if TypeBody0 = hlds_eqv_type(Type1) then
            % XXX Should we require that Type1 have the same visibility
            % as Type0? If it doesn't, then we this predicate may yield
            % a different final type when compiling different modules,
            % which means we can make different decisions about data
            % representations when compiling different modules.
            % However, we currently test only whether the final type
            % is float, and equivalences to float are always exported.
            % XXX This still leaves the possibility of t1 == t2, t2 == float,
            % which is a problem if *only the second* equivalence is exported.
            % XXX Something will need to change when we start caring whether
            % the dereferenced type is int64 or uint64.
            deref_eqv_types(ModuleInfo, Type1, Type)
        else
            Type = Type0
        )
    else
        Type = Type0
    ).

%---------------------%

:- inst hlds_du_type for hlds_type_body/0
    --->    hlds_du_type(ground, ground, ground, ground).

:- pred ctors_are_all_constants(list(constructor)::in) is semidet.

ctors_are_all_constants([]).
ctors_are_all_constants([Ctor | Ctors]) :-
    Ctor = ctor(MaybeExistConstraints, _Name, Args, Arity, _Context),
    MaybeExistConstraints = no_exist_constraints,
    Args = [],
    Arity = 0,
    ctors_are_all_constants(Ctors).

:- pred separate_out_constants(list(constructor)::in,
    list(constructor)::out, list(constructor)::out) is det.

separate_out_constants([], [], []).
separate_out_constants([Ctor | Ctors], Constants, Functors) :-
    separate_out_constants(Ctors, Constants0, Functors0),
    Args = Ctor ^ cons_args,
    % XXX TYPE_REPN Consider changing the representation of constructors
    % to encode the invariant (no arguments -> no_exist_constraints)
    % in the data structure.
    ( if
        Args = [],
        Ctor ^ cons_maybe_exist = no_exist_constraints
    then
        Constants = [Ctor | Constants0],
        Functors = Functors0
    else
        Constants = Constants0,
        Functors = [Ctor | Functors0]
    ).

:- func type_ctor_sna(type_ctor) = format_component.

type_ctor_sna(TypeCtor) = Piece :-
    TypeCtor = type_ctor(TypeCtorSymName, TypeCtorArity),
    Piece = qual_sym_name_and_arity(
        sym_name_arity(TypeCtorSymName, TypeCtorArity)).

:- pred output_direct_arg_functor_summary(module_name::in, type_ctor::in,
    list(sym_name_and_arity)::in, io::di, io::uo) is det.
:- pragma consider_used(output_direct_arg_functor_summary/5).

output_direct_arg_functor_summary(ModuleName, TypeCtor, DirectArgFunctorNames,
        !IO) :-
    write_sym_name(ModuleName, !IO),
    io.write_string(" : ", !IO),
    write_type_ctor(TypeCtor, !IO),
    io.write_string(" : ", !IO),
    io.write_list(DirectArgFunctorNames, ", ", write_sym_name_and_arity, !IO),
    io.nl(!IO).

%---------------------------------------------------------------------------%

:- pred compute_sectag_bits(list(T)::in, sectag_bits::out) is det.

compute_sectag_bits(Sharers, SectagBits) :-
    list.length(Sharers, NumSharers),
    ( if uint.from_int(NumSharers, UnsignedNumSharers) then
        num_bits_needed_for_n_things_loop(UnsignedNumSharers, 0, NumBits),
        NumBitsMask = (1 << NumBits) - 1,
        ( if
            uint8.from_int(NumBits, NumBitsUint8),
            uint.from_int(NumBitsMask, NumBitsMaskUint)
        then
            SectagBits = sectag_bits(NumBitsUint8, NumBitsMaskUint)
        else
            unexpected($pred, "NumBitsNeeded does not fit in 8 bits")
        )
    else
        unexpected($pred, "N < 0")
    ).

:- pred num_bits_needed_for_n_things_loop(uint::in, int::in, int::out) is det.

num_bits_needed_for_n_things_loop(N, NumBits0, NumBits) :-
    ( if N < (1u << NumBits0) then
        NumBits = NumBits0
    else
        num_bits_needed_for_n_things_loop(N, NumBits0 + 1, NumBits)
    ).

%---------------------------------------------------------------------------%
:- end_module hlds.du_type_layout.
%---------------------------------------------------------------------------%
