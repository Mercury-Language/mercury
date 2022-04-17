%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% Copyright (C) 2014-2021 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: type_ctor_info.m.
% Authors: zs, trd.
%
% This module is responsible for the generation of the static type_ctor_info
% structures of the types defined by the current module. This includes the
% RTTI data structures that describe the representation of each type.
% These structures form the type_ctor_rep, type_num_functors, type_functors
% and type_layout fields of a type_ctor_info. This RTTI information is
% used for several purposes: examples include deep copy, tabling, and functor,
% arg and their cousins.
%
% Since it is possible for the type_ctor_info of a type local to the module
% not to be referred to anywhere in the module (and therefore, not to be
% referred to anywhere in the program), this module works in two stages.
% In the first stage, it inserts type_ctor_gen_info structures describing the
% type_ctor_infos of all the locally-defined types into the HLDS; some of
% these type_ctor_gen_infos are later eliminated by dead_proc_elim.m. The
% second stage then generates lower-level RTTI descriptions of type_ctor_infos
% from the surviving type_ctor_gen_infos. These can then be easily
% turned into either LLDS or MLDS.
%
% The documentation of the data structures built in this module is in
% runtime/mercury_type_info.h; that file also contains a list of all
% the files that depend on these data structures.
%
%---------------------------------------------------------------------------%

:- module backend_libs.type_ctor_info.
:- interface.

:- import_module backend_libs.rtti.
:- import_module hlds.
:- import_module hlds.hlds_module.

:- import_module list.

%---------------------------------------------------------------------------%

:- pred generate_hlds(module_info::in, module_info::out) is det.

:- pred generate_rtti(module_info::in, list(rtti_data)::out) is det.

:- pred compute_du_ptag_layout_flags(sectag_table::in,
    du_ptag_layout_flags::out) is det.

    % Compute the "contains var" bit vector. The input is a list describing
    % the types of the arguments of a function symbol. The output is an
    % bit vector (represented as a 16 bit integer) in which each bit is set
    % if the type of the corresponding argument contains a type variable.
    % If the function symbol has more than 16 arguments, then the last bit
    % is true if any of the arguments after the 15th contain a type
    % variable in their type.
    %
:- func compute_contains_var_bit_vector(
    list(rtti_maybe_pseudo_type_info_or_self)) = uint16.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.foreign.
:- import_module backend_libs.pseudo_type_info.
:- import_module backend_libs.type_class_info.
:- import_module check_hlds.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module hlds.special_pred.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module int8.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module uint.
:- import_module uint8.
:- import_module uint16.
:- import_module uint32.
:- import_module univ.
:- import_module varset.

%---------------------------------------------------------------------------%

generate_hlds(!ModuleInfo) :-
    module_info_get_name(!.ModuleInfo, ModuleName),
    module_info_get_type_table(!.ModuleInfo, TypeTable),
    get_all_type_ctor_defns(TypeTable, TypeCtorsDefns),
    gen_type_ctor_gen_infos(!.ModuleInfo, ModuleName, TypeCtorsDefns,
        LocalTypeCtorGenInfos),
    ( if
        ModuleName = mercury_public_builtin_module,
        compiler_generated_rtti_for_builtins(!.ModuleInfo)
    then
        gen_builtin_type_ctor_gen_infos(!.ModuleInfo, ModuleName,
            builtin_type_ctors_with_no_hlds_type_defn,
            BuiltinTypeCtorGenInfos),
        AllTypeCtorGenInfos = BuiltinTypeCtorGenInfos ++ LocalTypeCtorGenInfos
    else
        AllTypeCtorGenInfos = LocalTypeCtorGenInfos
    ),
    module_info_set_type_ctor_gen_infos(AllTypeCtorGenInfos, !ModuleInfo).

    % Given a list of the ids of all the types in the type table, find the
    % types defined in this module, and return a type_ctor_gen_info for each.
    %
:- pred gen_type_ctor_gen_infos(module_info::in, module_name::in,
    assoc_list(type_ctor, hlds_type_defn)::in, list(type_ctor_gen_info)::out)
    is det.

gen_type_ctor_gen_infos(_, _, [], []).
gen_type_ctor_gen_infos(ModuleInfo, ModuleName, [TypeCtorDefn | TypeCtorDefns],
        TypeCtorGenInfos) :-
    gen_type_ctor_gen_infos(ModuleInfo, ModuleName, TypeCtorDefns,
        TypeCtorGenInfosTail),
    TypeCtorDefn = TypeCtor - TypeDefn,
    TypeCtor = type_ctor(SymName, TypeArity),
    (
        SymName = qualified(TypeModuleName, TypeName),
        ( if
            TypeModuleName = ModuleName,
            should_create_type_ctor_gen(ModuleInfo, TypeCtorDefn,
                TypeModuleName, TypeName, TypeArity, TypeDefn)
        then
            gen_type_ctor_gen_info(ModuleInfo, TypeCtor, TypeModuleName,
                TypeName, TypeArity, TypeDefn, TypeCtorGenInfo),
            TypeCtorGenInfos = [TypeCtorGenInfo | TypeCtorGenInfosTail]
        else
            TypeCtorGenInfos = TypeCtorGenInfosTail
        )
    ;
        SymName = unqualified(TypeName),
        unexpected($pred, "unqualified type " ++ TypeName)
    ).

    % Check if we should generate a type_ctor_info for this type.
    % These are four cases;
    %
    % - The builtin types which have no hlds_type_defn
    %   (i.e. no declaration and no definition).
    %
    % - The builtin types which are declared abstract and are not defined
    %   (i.e. they have a declaration, but no definition).
    %
    % - The builtin types which have a fake type body and as such have to have
    %   hand-defined RTTI.
    %
    % - All the rest of the types.
    %
    % The first category are handled by gen_builtin_type_ctor_gen_infos;
    % this predicate handles the other three.
    %
:- pred should_create_type_ctor_gen(module_info::in,
    pair(type_ctor, hlds_type_defn)::in, module_name::in, string::in, int::in,
    hlds_type_defn::out) is semidet.

should_create_type_ctor_gen(ModuleInfo, TypeCtor - TypeDefn, TypeModuleName,
        TypeName, TypeArity, TypeDefn) :-
    hlds_data.get_type_defn_body(TypeDefn, TypeBody),
    ( if
        ( TypeBody = hlds_abstract_type(_)
        ; type_ctor_has_hand_defined_rtti(TypeCtor, TypeBody)
        )
    then
        % The builtin types which are declared abstract or which have
        % hand defined rtti due to having a fake type body.
        compiler_generated_rtti_for_builtins(ModuleInfo),
        TypeModuleName = unqualified(ModuleNameString),
        (
            builtin_type_ctor(ModuleNameString, TypeName, TypeArity, _)
        ;
            impl_type_ctor(ModuleNameString, TypeName, TypeArity, _)
        )
    else
        true
    ).

:- pred gen_builtin_type_ctor_gen_infos(module_info::in, module_name::in,
    list(type_ctor)::in, list(type_ctor_gen_info)::out) is det.

gen_builtin_type_ctor_gen_infos(_ModuleInfo, _ModuleName, [], []).
gen_builtin_type_ctor_gen_infos(ModuleInfo, ModuleName, [TypeCtor | TypeCtors],
        [TypeCtorGenInfo | TypeCtorGenInfos]) :-
    gen_builtin_type_ctor_gen_infos(ModuleInfo, ModuleName, TypeCtors,
        TypeCtorGenInfos),
    TypeCtor = type_ctor(SymName, TypeArity),
    (
        SymName = qualified(TypeModuleName, TypeName),
        expect(unify(TypeModuleName, ModuleName), $pred, "module mismatch"),
        gen_type_ctor_gen_info(ModuleInfo, TypeCtor, TypeModuleName,
            TypeName, TypeArity, builtin_type_defn, TypeCtorGenInfo)
    ;
        SymName = unqualified(TypeName),
        unexpected($pred, "unqualified type " ++ TypeName)
    ).

    % Generate a type_defn for the builtin types which don't have one.
    %
:- func builtin_type_defn = hlds_type_defn.

builtin_type_defn = TypeDefn :-
    varset.init(TVarSet),
    Params = [],
    map.init(Kinds),
    TypeBody = hlds_abstract_type(abstract_type_general),
    TypeStatus = type_status(status_local),
    NeedQual = may_be_unqualified,
    term.context_init(Context),
    create_hlds_type_defn(TVarSet, Params, Kinds, TypeBody, no,
        TypeStatus, NeedQual, type_defn_no_prev_errors, Context, TypeDefn).

:- pred gen_type_ctor_gen_info( module_info::in, type_ctor::in,
    module_name::in, string::in, int::in, hlds_type_defn::in,
    type_ctor_gen_info::out) is det.

gen_type_ctor_gen_info(ModuleInfo, TypeCtor, ModuleName, TypeName, TypeArity,
        TypeDefn, TypeCtorGenInfo) :-
    module_info_get_special_pred_maps(ModuleInfo, SpecMaps),

    UnifyMap = SpecMaps ^ spm_unify_map,
    map.lookup(UnifyMap, TypeCtor, UnifyPredId),
    special_pred_mode_num(spec_pred_unify, UnifyProcInt),
    proc_id_to_int(UnifyProcId, UnifyProcInt),
    Unify = proc(UnifyPredId, UnifyProcId),

    CompareMap = SpecMaps ^ spm_compare_map,
    map.lookup(CompareMap, TypeCtor, ComparePredId),
    special_pred_mode_num(spec_pred_compare, CompareProcInt),
    proc_id_to_int(CompareProcId, CompareProcInt),
    Compare = proc(ComparePredId, CompareProcId),

    hlds_data.get_type_defn_status(TypeDefn, Status),
    TypeCtorGenInfo = type_ctor_gen_info(TypeCtor, ModuleName, TypeName,
        TypeArity, Status, TypeDefn, Unify, Compare).

%---------------------------------------------------------------------------%

generate_rtti(ModuleInfo, Tables) :-
    module_info_get_type_ctor_gen_infos(ModuleInfo, TypeCtorGenInfos),
    construct_type_ctor_infos(TypeCtorGenInfos, ModuleInfo,
        [], Dynamic, [], Static0),
    % The same pseudo_type_info may be generated in several places; we need
    % to eliminate duplicates here, to avoid duplicate definition errors
    % in the generated C code.
    Static = list.remove_dups(Static0),
    list.append(Dynamic, Static, Tables).

:- pred construct_type_ctor_infos(list(type_ctor_gen_info)::in,
    module_info::in,
    list(rtti_data)::in, list(rtti_data)::out,
    list(rtti_data)::in, list(rtti_data)::out) is det.

construct_type_ctor_infos([], _ModuleInfo, !Dynamic, !Static).
construct_type_ctor_infos([TypeCtorGenInfo | TypeCtorGenInfos],
        ModuleInfo, !Dynamic, !Static) :-
    construct_type_ctor_info(TypeCtorGenInfo, ModuleInfo, TypeCtorCModule),
    !:Dynamic = [TypeCtorCModule | !.Dynamic],
    construct_type_ctor_infos(TypeCtorGenInfos, ModuleInfo, !Dynamic, !Static).

    % Generate RTTI information for the given type.
    %
:- pred construct_type_ctor_info(type_ctor_gen_info::in, module_info::in,
    rtti_data::out) is det.

construct_type_ctor_info(TypeCtorGenInfo, ModuleInfo, RttiData) :-
    TypeCtorGenInfo = type_ctor_gen_info(_TypeCtor, ModuleName, TypeName,
        TypeArity, _Status, HldsDefn, UnifyPredProcId, ComparePredProcId),
    UnifyPredProcId = proc(UnifyPredId, UnifyProcId),
    UnifyProcLabel = make_rtti_proc_label(ModuleInfo,
        UnifyPredId, UnifyProcId),
    ComparePredProcId = proc(ComparePredId, CompareProcId),
    CompareProcLabel = make_rtti_proc_label(ModuleInfo,
        ComparePredId, CompareProcId),
    type_to_univ(UnifyProcLabel, UnifyUniv),
    type_to_univ(CompareProcLabel, CompareUniv),
    hlds_data.get_type_defn_body(HldsDefn, TypeBody),
    Version = type_ctor_info_rtti_version,

    % It is an error for a type body to be an abstract type unless
    % we are generating the RTTI for builtins.
    ( if
        TypeBody = hlds_abstract_type(_),
        not compiler_generated_rtti_for_builtins(ModuleInfo)
    then
        unexpected($pred, "abstract_type")
    else
        true
    ),

    % We check for hand-coded definitions before inspecting the type-bodys
    % as some type definitions have fake bodies, e.g.
    % private_builtin.typeclass_info.
    ( if
        ModuleName = unqualified(ModuleStr1),
        builtin_type_ctor(ModuleStr1, TypeName, TypeArity, BuiltinCtor)
    then
        Details = tcd_builtin(BuiltinCtor),
        LayoutIndexable = no
    else if
        ModuleName = unqualified(ModuleStr),
        impl_type_ctor(ModuleStr, TypeName, TypeArity, ImplCtor)
    then
        Details = tcd_impl_artifact(ImplCtor),
        LayoutIndexable = no
    else
        (
            TypeBody = hlds_abstract_type(_),
            unexpected($pred, "abstract_type")
        ;
            % We treat solver_types as being equivalent to their representation
            % types for RTTI purposes. Which may cause problems with construct,
            % similar to those for abstract types.
            TypeBody = hlds_solver_type(DetailsSolver),
            DetailsSolver =
                type_details_solver(SolverTypeDetails, _MaybeCanonical),
            RepnType = SolverTypeDetails ^ std_representation_type,
            % There can be no existentially typed args to an equivalence.
            UnivTVars = TypeArity,
            ExistTVars = [],
            pseudo_type_info.construct_maybe_pseudo_type_info(RepnType,
                UnivTVars, ExistTVars, MaybePseudoTypeInfo),
            Details = tcd_eqv(MaybePseudoTypeInfo),
            LayoutIndexable = no
        ;
            TypeBody = hlds_foreign_type(ForeignBody),
            foreign_type_body_to_exported_type(ModuleInfo, ForeignBody, _, _,
                Assertions),
            ( if asserted_stable(Assertions) then
                IsStable = is_stable
            else
                IsStable = is_not_stable
            ),
            Details = tcd_foreign(IsStable),
            LayoutIndexable = no
        ;
            TypeBody = hlds_eqv_type(Type),
            % There can be no existentially typed args to an equivalence.
            UnivTVars = TypeArity,
            ExistTVars = [],
            pseudo_type_info.construct_maybe_pseudo_type_info(Type,
                UnivTVars, ExistTVars, MaybePseudoTypeInfo),
            Details = tcd_eqv(MaybePseudoTypeInfo),
            LayoutIndexable = no
        ;
            TypeBody = hlds_du_type(TypeBodyDu),
            TypeBodyDu = type_body_du(_Ctors, MaybeSuperType, MaybeCanonical,
                MaybeRepn, _IsForeignType),
            (
                MaybeRepn = no,
                unexpected($pred, "MaybeRepn = no")
            ;
                MaybeRepn = yes(Repn),
                Repn = du_type_repn(CtorRepns, _ConsCtorMap, _CheaperTagTest,
                    DuTypeKind, _MaybeDirectArgCtors)
            ),
            (
                MaybeCanonical = noncanon(_),
                EqualityAxioms = user_defined
            ;
                MaybeCanonical = canon,
                EqualityAxioms = standard
            ),
            (
                DuTypeKind = du_type_kind_mercury_enum,
                make_mercury_enum_details(ModuleInfo, MaybeSuperType,
                    CtorRepns, enum_is_not_dummy, EqualityAxioms, Details,
                    IndexableByEnumValue),
                LayoutIndexable = IndexableByEnumValue
            ;
                DuTypeKind = du_type_kind_foreign_enum(Lang),
                make_foreign_enum_details(Lang, CtorRepns, EqualityAxioms,
                    Details),
                LayoutIndexable = no
            ;
                DuTypeKind = du_type_kind_direct_dummy,
                make_mercury_enum_details(ModuleInfo, MaybeSuperType,
                    CtorRepns, enum_is_dummy, EqualityAxioms, Details,
                    IndexableByEnumValue),
                LayoutIndexable = IndexableByEnumValue
            ;
                DuTypeKind = du_type_kind_notag(FunctorName, ArgType,
                    MaybeArgName),
                make_notag_details(ModuleInfo, TypeArity, MaybeSuperType,
                    FunctorName, ArgType, MaybeArgName, EqualityAxioms,
                    Details),
                LayoutIndexable = no
            ;
                DuTypeKind = du_type_kind_general,
                make_du_details(ModuleInfo, MaybeSuperType, CtorRepns,
                    TypeArity, EqualityAxioms, Details, IndexableByPtag),
                LayoutIndexable = IndexableByPtag
            )
        )
    ),
    some [!Flags] (
        !:Flags = set.init,
        (
            TypeBody = hlds_du_type(_),
            set.insert(kind_of_du_flag, !Flags)
        ;
            ( TypeBody = hlds_eqv_type(_)
            ; TypeBody = hlds_foreign_type(_)
            ; TypeBody = hlds_solver_type(_)
            ; TypeBody = hlds_abstract_type(_)
            )
        ),
        (
            LayoutIndexable = yes,
            set.insert(layout_indexable_flag, !Flags)
        ;
            LayoutIndexable = no
        ),
        TypeCtorData = type_ctor_data(Version, ModuleName, TypeName,
            uint16.det_from_int(TypeArity), UnifyUniv, CompareUniv,
            !.Flags, Details)
    ),
    RttiData = rtti_data_type_ctor_info(TypeCtorData).

:- pred builtin_type_ctor(string, string, int, builtin_ctor).
:- mode builtin_type_ctor(in, in, in, out) is semidet.
:- mode builtin_type_ctor(out, out, out, in) is det.

% Some of these type_ctors are listed in prog_type.m in the function
% builtin_type_ctors_with_no_hlds_type_defn; any changes here may need
% to be done there as well.
builtin_type_ctor("builtin", "int", 0, builtin_ctor_int).
builtin_type_ctor("builtin", "uint", 0, builtin_ctor_uint).
builtin_type_ctor("builtin", "int8", 0, builtin_ctor_int8).
builtin_type_ctor("builtin", "uint8", 0, builtin_ctor_uint8).
builtin_type_ctor("builtin", "int16", 0, builtin_ctor_int16).
builtin_type_ctor("builtin", "uint16", 0, builtin_ctor_uint16).
builtin_type_ctor("builtin", "int32", 0, builtin_ctor_int32).
builtin_type_ctor("builtin", "uint32", 0, builtin_ctor_uint32).
builtin_type_ctor("builtin", "int64", 0, builtin_ctor_int64).
builtin_type_ctor("builtin", "uint64", 0, builtin_ctor_uint64).
builtin_type_ctor("builtin", "string", 0, builtin_ctor_string).
builtin_type_ctor("builtin", "float", 0, builtin_ctor_float).
builtin_type_ctor("builtin", "character", 0, builtin_ctor_char).
builtin_type_ctor("builtin", "void", 0, builtin_ctor_void).
builtin_type_ctor("builtin", "c_pointer", 0,
    builtin_ctor_c_pointer(is_not_stable)).
builtin_type_ctor("builtin", "stable_c_pointer", 0,
    builtin_ctor_c_pointer(is_stable)).
builtin_type_ctor("builtin", "pred", 0, builtin_ctor_pred_ctor).
builtin_type_ctor("builtin", "func", 0, builtin_ctor_func_ctor).
builtin_type_ctor("builtin", "tuple", 0, builtin_ctor_tuple).
builtin_type_ctor("private_builtin", "ref", 1, builtin_ctor_ref).
builtin_type_ctor("type_desc", "type_ctor_desc", 0,
    builtin_ctor_type_ctor_desc).
builtin_type_ctor("type_desc", "pseudo_type_desc", 0,
    builtin_ctor_pseudo_type_desc).
builtin_type_ctor("type_desc", "type_desc", 0, builtin_ctor_type_desc).

:- pred impl_type_ctor(string::in, string::in, int::in, impl_ctor::out)
    is semidet.

impl_type_ctor("private_builtin", "type_ctor_info", 0,
    impl_ctor_type_ctor_info).
impl_type_ctor("private_builtin", "type_info", 0, impl_ctor_type_info).
impl_type_ctor("private_builtin", "typeclass_info", 0,
    impl_ctor_typeclass_info).
impl_type_ctor("private_builtin", "base_typeclass_info", 0,
    impl_ctor_base_typeclass_info).
impl_type_ctor("private_builtin", "heap_pointer", 0, impl_ctor_hp).
impl_type_ctor("private_builtin", "succip", 0, impl_ctor_succip).
impl_type_ctor("private_builtin", "curfr", 0, impl_ctor_curfr).
impl_type_ctor("private_builtin", "maxfr", 0, impl_ctor_maxfr).
impl_type_ctor("private_builtin", "redofr", 0, impl_ctor_redofr).
impl_type_ctor("private_builtin", "trail_ptr", 0, impl_ctor_trail_ptr).
impl_type_ctor("private_builtin", "ticket", 0, impl_ctor_ticket).
impl_type_ctor("table_builtin", "ml_subgoal", 0, impl_ctor_subgoal).

%---------------------------------------------------------------------------%

    % The version of the RTTI data structures -- useful for bootstrapping.
    % If you write runtime code that checks this version number and
    % can at least handle the previous version of the data
    % structure, it makes it easier to bootstrap changes to the data
    % structures used for RTTI.
    %
    % This number should be kept in sync with MR_RTTI_VERSION in
    % runtime/mercury_type_info.h. This means you need to update
    % the handwritten type_ctor_info structures (and the macros that
    % generate them) as well as the code in the runtime that uses RTTI
    % to conform to whatever changes the new version introduces.
    %
:- func type_ctor_info_rtti_version = uint8.

type_ctor_info_rtti_version = 18u8.

%---------------------------------------------------------------------------%

    % Make the functor and layout tables for a notag type.
    %
:- pred make_notag_details(module_info::in, int::in, maybe_subtype::in,
    sym_name::in, mer_type::in, maybe(string)::in, equality_axioms::in,
    type_ctor_details::out) is det.

make_notag_details(ModuleInfo, TypeArity, MaybeSuperType, SymName, ArgType,
        MaybeArgName, EqualityAxioms, Details) :-
    FunctorName = unqualify_name(SymName),
    NumUnivTVars = TypeArity,
    % There can be no existentially typed args to the functor in a notag type.
    ExistTVars = [],
    pseudo_type_info.construct_maybe_pseudo_type_info(ArgType,
        NumUnivTVars, ExistTVars, MaybePseudoTypeInfo),
    ( if ArgType = higher_order_type(_, _, higher_order(_), _, _) then
        FunctorSubtypeInfo = functor_subtype_exists
    else
        FunctorSubtypeInfo = functor_subtype_none
    ),
    Functor = notag_functor(FunctorName, MaybePseudoTypeInfo, MaybeArgName,
        FunctorSubtypeInfo),
    maybe_get_base_type_ctor(ModuleInfo, MaybeSuperType, MaybeBaseTypeCtor),
    Details = tcd_notag(EqualityAxioms, Functor, MaybeBaseTypeCtor).

%---------------------------------------------------------------------------%

:- type name_sort_info == assoc_list(pair(string, int), ctor_rtti_name).

    % Make the functor and layout tables for an enum type.
    %
:- pred make_mercury_enum_details(module_info::in, maybe_subtype::in,
    list(constructor_repn)::in, enum_maybe_dummy::in, equality_axioms::in,
    type_ctor_details::out, bool::out) is det.

make_mercury_enum_details(ModuleInfo, MaybeSuperType, CtorRepns, IsDummy,
        EqualityAxioms, Details, IndexableByEnumValue) :-
    (
        CtorRepns = [],
        unexpected($pred, "enum with no ctors")
    ;
        CtorRepns = [_],
        (
            MaybeSuperType = not_a_subtype,
            expect(unify(IsDummy, enum_is_dummy), $pred,
                "one ctor but not dummy")
        ;
            MaybeSuperType = subtype_of(_)
            % A subtype with one constructor is not necessarily a dummy type.
        )
    ;
        CtorRepns = [_, _ | _],
        expect(unify(IsDummy, enum_is_not_dummy), $pred,
            "more than one ctor but dummy")
    ),
    make_enum_functors(MaybeSuperType, CtorRepns, IsDummy, 0u32, EnumFunctors),
    OrdinalMap0 = map.init,
    NameMap0 = map.init,
    list.foldl3(make_enum_maps, EnumFunctors,
        OrdinalMap0, OrdinalMap, NameMap0, NameMap,
        yes, AllValueEqualsOrdinal),
    ( if is_enum_value_map_indexable(OrdinalMap, AllValueEqualsOrdinal) then
        IndexableByEnumValue = yes
    else
        IndexableByEnumValue = no
    ),
    FunctorNumberMap = make_functor_number_map(CtorRepns),
    maybe_get_base_type_ctor(ModuleInfo, MaybeSuperType, MaybeBaseTypeCtor),
    Details = tcd_enum(EqualityAxioms, IsDummy, EnumFunctors,
        OrdinalMap, NameMap, FunctorNumberMap, MaybeBaseTypeCtor).

    % Create an enum_functor structure for each functor in an enum type.
    % The functors are given to us in ordinal order (since that's how the HLDS
    % stored them), and that is how we return the list of rtti names of the
    % enum_functor_desc structures; that way, it is directly usable in the type
    % layout structure. We also return a structure that allows our caller to
    % sort this list on functor name, which is how the type functors structure
    % is constructed.
    %
:- pred make_enum_functors(maybe_subtype::in, list(constructor_repn)::in,
    enum_maybe_dummy::in, uint32::in, list(enum_functor)::out) is det.

make_enum_functors(_, [], _, _, []).
make_enum_functors(MaybeSuperType, [FunctorRepn | FunctorRepns], IsDummy,
        CurOrdinal, [EnumFunctor | EnumFunctors]) :-
    FunctorRepn = ctor_repn(Ordinal, MaybeExistConstraints, SymName, ConsTag,
        _FunctorArgRepns, Arity, _Context),
    % XXX ARG_PACK We should not need CurOrdinal.
    expect(unify(Ordinal, CurOrdinal), $pred, "Ordinal != CurOrdinal"),
    expect(unify(MaybeExistConstraints, no_exist_constraints), $pred,
        "existential constraints in functor in enum"),
    expect(unify(Arity, 0), $pred, "functor in enum has nonzero arity"),
    (
        IsDummy = enum_is_not_dummy,
        ( if ConsTag = int_tag(int_tag_int(ConsTagInt)) then
            ConsTagUint32 = uint32.det_from_int(ConsTagInt)
        else
            unexpected($pred, "enum functor's tag is not int_tag")
        ),
        (
            MaybeSuperType = not_a_subtype,
            expect(unify(ConsTagUint32, CurOrdinal), $pred,
                "enum functor's tag is not the expected int_tag")
        ;
            MaybeSuperType = subtype_of(_)
        ),
        EnumValue = enum_value(ConsTagUint32)
    ;
        IsDummy = enum_is_dummy,
        expect(unify(ConsTag, dummy_tag), $pred,
            "dummy functor's tag is not dummy_tag"),
        EnumValue = enum_value(CurOrdinal)
    ),
    FunctorName = unqualify_name(SymName),
    EnumFunctor = enum_functor(FunctorName, Ordinal, EnumValue),
    make_enum_functors(MaybeSuperType, FunctorRepns, IsDummy,
        CurOrdinal + 1u32, EnumFunctors).

:- pred make_enum_maps(enum_functor::in,
    map(uint32, enum_functor)::in, map(uint32, enum_functor)::out,
    map(string, enum_functor)::in, map(string, enum_functor)::out,
    bool::in, bool::out) is det.

make_enum_maps(EnumFunctor, !OrdinalMap, !NameMap, !ValueEqualsOrdinal) :-
    EnumFunctor = enum_functor(FunctorName, Ordinal, Value),
    map.det_insert(Ordinal, EnumFunctor, !OrdinalMap),
    map.det_insert(FunctorName, EnumFunctor, !NameMap),
    ( if Value = enum_value(Ordinal) then
        true
    else
        !:ValueEqualsOrdinal = no
    ).

:- pred is_enum_value_map_indexable(map(uint32, enum_functor)::in, bool::in)
    is semidet.

is_enum_value_map_indexable(OrdinalMap, AllValueEqualsOrdinal) :-
    AllValueEqualsOrdinal = yes,
    map.min_key(OrdinalMap) = 0u32,
    map.max_key(OrdinalMap) = MaxOrdinal,
    map.count(OrdinalMap, Count),
    uint32.from_int(Count - 1, MaxOrdinal).

%---------------------------------------------------------------------------%

    % Make the functor and layout tables for a foreign enum type.
    %
:- pred make_foreign_enum_details(foreign_language::in,
    list(constructor_repn)::in, equality_axioms::in,
    type_ctor_details::out) is det.

make_foreign_enum_details(Lang, CtorRepns, EqualityAxioms, Details) :-
    make_foreign_enum_functors(Lang, CtorRepns, 0u32, ForeignEnumFunctors),
    OrdinalMap0 = map.init,
    NameMap0 = map.init,
    list.foldl2(make_foreign_enum_maps, ForeignEnumFunctors,
        OrdinalMap0, OrdinalMap, NameMap0, NameMap),
    FunctorNumberMap = make_functor_number_map(CtorRepns),
    Details = tcd_foreign_enum(Lang, EqualityAxioms, ForeignEnumFunctors,
        OrdinalMap, NameMap, FunctorNumberMap).

    % Create a foreign_enum_functor structure for each functor in an enum type.
    % The functors are given to us in ordinal order (since that's how the HLDS
    % stored them), and that is how we return the list of rtti names of the
    % foreign_enum_functor_desc structures; that way, it is directly usable in
    % the type layout structure. We also return a structure that allows our
    % caller to sort this list on functor name, which is how the type functors
    % structure is constructed.
    %
:- pred make_foreign_enum_functors(foreign_language::in,
    list(constructor_repn)::in, uint32::in,
    list(foreign_enum_functor)::out) is det.

make_foreign_enum_functors(_, [], _, []).
make_foreign_enum_functors(Lang, [FunctorRepn | FunctorRepns], CurOrdinal,
        [ForeignEnumFunctor | ForeignEnumFunctors]) :-
    FunctorRepn = ctor_repn(Ordinal, MaybeExistConstraints, SymName, ConsTag,
        _FunctorArgRepns, Arity, _Context),
    % XXX ARG_PACK We should not need CurOrdinal.
    expect(unify(Ordinal, CurOrdinal), $pred, "Ordinal != CurOrdinal"),
    expect(unify(MaybeExistConstraints, no_exist_constraints), $pred,
        "existential constraints in functor in enum"),
    expect(unify(Arity, 0), $pred,
        "functor in foreign enum has nonzero arity"),
    (
        ConsTag = foreign_tag(ForeignTagLang, ForeignTagValue0),
        expect(unify(Lang, ForeignTagLang), $pred,
            "language mismatch between foreign tag and foreign enum"),
        ForeignTagValue = ForeignTagValue0
    ;
        ( ConsTag = string_tag(_)
        ; ConsTag = float_tag(_)
        ; ConsTag = int_tag(_)
        ; ConsTag = closure_tag(_, _, _)
        ; ConsTag = type_ctor_info_tag(_, _, _)
        ; ConsTag = base_typeclass_info_tag(_, _, _)
        ; ConsTag = type_info_const_tag(_)
        ; ConsTag = typeclass_info_const_tag(_)
        ; ConsTag = ground_term_const_tag(_, _)
        ; ConsTag = tabling_info_tag(_, _)
        ; ConsTag = deep_profiling_proc_layout_tag(_, _)
        ; ConsTag = table_io_entry_tag(_, _)
        ; ConsTag = direct_arg_tag(_)
        ; ConsTag = shared_local_tag_no_args(_, _, _)
        ; ConsTag = local_args_tag(_)
        ; ConsTag = remote_args_tag(_)
        ; ConsTag = no_tag
        ; ConsTag = dummy_tag
        ),
        unexpected($pred, "non foreign tag for foreign enum functor")
    ),
    FunctorName = unqualify_name(SymName),
    ForeignEnumFunctor = foreign_enum_functor(FunctorName, CurOrdinal,
        ForeignTagValue),
    make_foreign_enum_functors(Lang, FunctorRepns, CurOrdinal + 1u32,
        ForeignEnumFunctors).

:- pred make_foreign_enum_maps(foreign_enum_functor::in,
    map(uint32, foreign_enum_functor)::in,
    map(uint32, foreign_enum_functor)::out,
    map(string, foreign_enum_functor)::in,
    map(string, foreign_enum_functor)::out) is det.

make_foreign_enum_maps(ForeignEnumFunctor, !OrdinalMap, !NameMap) :-
    ForeignEnumFunctor = foreign_enum_functor(FunctorName, FunctorOrdinal, _),
    map.det_insert(FunctorOrdinal, ForeignEnumFunctor, !OrdinalMap),
    map.det_insert(FunctorName, ForeignEnumFunctor, !NameMap).

%---------------------------------------------------------------------------%

:- type tag_map == map(int,
    pair(sectag_locn, map(int, ctor_rtti_name))).
:- type tag_list == assoc_list(int,
    pair(sectag_locn, map(int, ctor_rtti_name))).

    % Make the functor and layout tables for a du type.
    %
:- pred make_du_details(module_info::in, maybe_subtype::in,
    list(constructor_repn)::in, int::in, equality_axioms::in,
    type_ctor_details::out, bool::out) is det.

make_du_details(ModuleInfo, MaybeSuperType, Ctors, TypeArity, EqualityAxioms,
        Details, IndexableByPtag) :-
    make_du_functors(ModuleInfo, Ctors, 0u32, TypeArity, DuFunctors),
    list.foldl(make_du_ptag_ordered_table, DuFunctors, map.init, DuPtagTable),
    ( if is_ptag_table_indexable(DuPtagTable) then
        IndexableByPtag = yes
    else
        IndexableByPtag = no
    ),
    FunctorNumberMap = make_functor_number_map(Ctors),
    list.foldl(make_du_name_ordered_table, DuFunctors,
        map.init, DuNameOrderedMap),
    maybe_get_base_type_ctor(ModuleInfo, MaybeSuperType, MaybeBaseTypeCtor),
    Details = tcd_du(EqualityAxioms, DuFunctors, DuPtagTable,
        DuNameOrderedMap, FunctorNumberMap, MaybeBaseTypeCtor).

    % Create a du_functor_desc structure for each functor in a du type.
    % Besides returning a list of the rtti names of their du_functor_desc
    % structures, we return two other items of information. The SortInfo
    % enables our caller to sort these rtti names on functor name and then
    % arity, which is how the type functors structure is constructed. The
    % TagMap groups the rttis into groups depending on their primary tags;
    % this is how the type layout structure is constructed.
    %
:- pred make_du_functors(module_info::in, list(constructor_repn)::in,
    uint32::in, int::in, list(du_functor)::out) is det.

make_du_functors(_, [], _, _, []).
make_du_functors(ModuleInfo, [CtorRepn | CtorRepns],
        CurOrdinal, TypeArity, [DuFunctor | DuFunctors]) :-
    CtorRepn = ctor_repn(Ordinal, MaybeExistConstraints, SymName, ConsTag,
        ConsArgRepns, Arity, _Context),
    % XXX ARG_PACK We should not need CurOrdinal.
    expect(unify(Ordinal, CurOrdinal), $pred, "Ordinal != CurOrdinal"),
    FunctorName = unqualify_name(SymName),
    get_du_rep(ConsTag, DuRep),
    (
        MaybeExistConstraints = no_exist_constraints,
        ExistTVars = [],
        MaybeExistInfo = no
    ;
        MaybeExistConstraints = exist_constraints(ExistConstraints),
        ExistConstraints = cons_exist_constraints(ExistTVars, _, _, _),
        module_info_get_class_table(ModuleInfo, ClassTable),
        generate_exist_info(ExistConstraints, ClassTable, ExistInfo),
        MaybeExistInfo = yes(ExistInfo)
    ),
    list.map_foldl(generate_du_arg_info(TypeArity, ExistTVars),
        ConsArgRepns, ArgInfos, functor_subtype_none, FunctorSubtypeInfo),
    DuFunctor = du_functor(FunctorName, uint16.det_from_int(Arity), CurOrdinal,
        DuRep, ArgInfos, MaybeExistInfo, FunctorSubtypeInfo),

    make_du_functors(ModuleInfo, CtorRepns,
        CurOrdinal + 1u32, TypeArity, DuFunctors).

:- pred get_du_rep(cons_tag::in, du_rep::out) is det.

get_du_rep(ConsTag, DuRep) :-
    (
        ConsTag = dummy_tag,
        DuRep = du_ll_rep(ptag(0u8), sectag_locn_none)
    ;
        ConsTag = direct_arg_tag(Ptag),
        DuRep = du_ll_rep(Ptag, sectag_locn_none_direct_arg)
    ;
        ConsTag = shared_local_tag_no_args(Ptag, LocalSectag, MustMask),
        LocalSectag = local_sectag(SectagUint, _PrimSec, SectagBits),
        (
            MustMask = lsectag_always_rest_of_word,
            SectagAndLocn = sectag_locn_local_rest_of_word(SectagUint)
        ;
            MustMask = lsectag_must_be_masked,
            SectagBits = sectag_bits(NumBits, Mask),
            SectagAndLocn = sectag_locn_local_bits(SectagUint, NumBits, Mask)
        ),
        DuRep = du_ll_rep(Ptag, SectagAndLocn)
    ;
        ConsTag = local_args_tag(LocalArgsTagInfo),
        (
            LocalArgsTagInfo = local_args_only_functor,
            Ptag = ptag(0u8),
            SectagUint = 0u,
            NumSectagBits = 0u8,
            SectagMask = 0u
        ;
            LocalArgsTagInfo = local_args_not_only_functor(Ptag, LocalSectag),
            LocalSectag = local_sectag(SectagUint, _PrimSec, SectagBits),
            SectagBits = sectag_bits(NumSectagBits, SectagMask)
        ),
        SectagAndLocn = sectag_locn_local_bits(SectagUint,
            NumSectagBits, SectagMask),
        DuRep = du_ll_rep(Ptag, SectagAndLocn)
    ;
        ConsTag = remote_args_tag(RemoteArgsTagInfo),
        (
            RemoteArgsTagInfo = remote_args_only_functor,
            DuRep = du_ll_rep(ptag(0u8), sectag_locn_none)
        ;
            RemoteArgsTagInfo = remote_args_unshared(Ptag),
            DuRep = du_ll_rep(Ptag, sectag_locn_none)
        ;
            RemoteArgsTagInfo = remote_args_shared(Ptag, RemoteSectag),
            RemoteSectag = remote_sectag(SectagUint, SectagSize),
            (
                SectagSize = rsectag_word,
                SectagAndLocn = sectag_locn_remote_word(SectagUint)
            ;
                SectagSize = rsectag_subword(SectagBits),
                SectagBits = sectag_bits(NumSectagBits, SectagMask),
                SectagAndLocn = sectag_locn_remote_bits(SectagUint,
                    NumSectagBits, SectagMask)
            ),
            DuRep = du_ll_rep(Ptag, SectagAndLocn)
        ;
            RemoteArgsTagInfo = remote_args_ctor(Data),
            DuRep = du_hl_rep(Data)
        )
    ;
        ( ConsTag = no_tag
        ; ConsTag = string_tag(_)
        ; ConsTag = int_tag(_)
        ; ConsTag = foreign_tag(_, _)
        ; ConsTag = float_tag(_)
        ; ConsTag = closure_tag(_, _, _)
        ; ConsTag = type_ctor_info_tag(_, _, _)
        ; ConsTag = base_typeclass_info_tag(_, _, _)
        ; ConsTag = type_info_const_tag(_)
        ; ConsTag = typeclass_info_const_tag(_)
        ; ConsTag = ground_term_const_tag(_, _)
        ; ConsTag = tabling_info_tag(_, _)
        ; ConsTag = deep_profiling_proc_layout_tag(_, _)
        ; ConsTag = table_io_entry_tag(_, _)
        ),
        unexpected($pred, "bad cons_tag for du function symbol")
    ).

:- pred generate_du_arg_info(int::in, existq_tvars::in,
    constructor_arg_repn::in, du_arg_info::out,
    functor_subtype_info::in, functor_subtype_info::out) is det.

generate_du_arg_info(NumUnivTVars, ExistTVars, ConsArgRepn, ArgInfo,
        !FunctorSubtypeInfo) :-
    ConsArgRepn = ctor_arg_repn(MaybeCtorFieldName, ArgType, ArgWidth, _Ctxt),
    (
        MaybeCtorFieldName = yes(ctor_field_name(SymName, _)),
        ArgName = unqualify_name(SymName),
        MaybeArgName = yes(ArgName)
    ;
        MaybeCtorFieldName = no,
        MaybeArgName = no
    ),
    % The C runtime cannot yet handle the "self" type representation,
    % so we do not generate it here.
    pseudo_type_info.construct_maybe_pseudo_type_info(ArgType,
        NumUnivTVars, ExistTVars, MaybePseudoTypeInfo),
    (
        MaybePseudoTypeInfo = plain(TypeInfo),
        MaybePseudoTypeInfoOrSelf = plain(TypeInfo)
        ;
        MaybePseudoTypeInfo = pseudo(PseudoTypeInfo),
        MaybePseudoTypeInfoOrSelf = pseudo(PseudoTypeInfo)
    ),
    ArgInfo = du_arg_info(MaybeArgName, MaybePseudoTypeInfoOrSelf, ArgWidth),
    ( if ArgType = higher_order_type(_, _, higher_order(_), _, _) then
        !:FunctorSubtypeInfo = functor_subtype_exists
    else
        true
    ).

    % Construct the RTTI structures that record information about the locations
    % of the typeinfos describing the types of the existentially typed
    % arguments of a functor.
    %
:- pred generate_exist_info(cons_exist_constraints::in, class_table::in,
    exist_info::out) is det.

generate_exist_info(ExistConstraints, ClassTable, ExistInfo) :-
    ExistConstraints = cons_exist_constraints(ExistTVars, Constraints,
        UnconstrainedTVars, ConstrainedTVars),
    map.init(LocnMap0),
    list.foldl2(
        ( pred(T::in, N0::in, N::out, Lm0::in, Lm::out) is det :-
            Locn = plain_typeinfo(uint16.det_from_int(N0)),
            map.det_insert(T, Locn, Lm0, Lm),
            N = N0 + 1
        ), UnconstrainedTVars, 0, TIsPlain, LocnMap0, LocnMap1),
    list.length(ExistTVars, AllTIs),
    TIsInTCIs = AllTIs - TIsPlain,
    list.foldl(find_type_info_index(Constraints, ClassTable, TIsPlain),
        ConstrainedTVars, LocnMap1, LocnMap),
    TCConstraints = list.map(generate_class_constraint, Constraints),
    list.map(
        ( pred(TVar::in, Locn::out) is det :-
            map.lookup(LocnMap, TVar, Locn)
        ), ExistTVars, ExistLocns),
    ExistInfo = exist_info(
        uint16.det_from_int(TIsPlain),
        uint16.det_from_int(TIsInTCIs),
        TCConstraints,
        ExistLocns
    ).

:- pred find_type_info_index(list(prog_constraint)::in, class_table::in,
    int::in, tvar::in, map(tvar, exist_typeinfo_locn)::in,
    map(tvar, exist_typeinfo_locn)::out) is det.

find_type_info_index(Constraints, ClassTable, StartSlot, TVar, !LocnMap) :-
    first_matching_type_class_info(Constraints, TVar,
        StartSlot, Slot, FirstConstraint, TypeInfoIndex),
    FirstConstraint = constraint(ClassName, ArgTypes),
    list.length(ArgTypes, ClassArity),
    map.lookup(ClassTable, class_id(ClassName, ClassArity), ClassDefn),
    list.length(ClassDefn ^ classdefn_supers, NumSuperClasses),
    RealTypeInfoIndex = TypeInfoIndex + NumSuperClasses,
    Locn = typeinfo_in_tci(
        uint16.det_from_int(Slot),
        uint16.det_from_int(RealTypeInfoIndex)
    ),
    map.det_insert(TVar, Locn, !LocnMap).

:- pred first_matching_type_class_info(list(prog_constraint)::in, tvar::in,
    int::in, int::out, prog_constraint::out, int::out) is det.

first_matching_type_class_info([], _, !N, _, _) :-
    unexpected($pred, "not found").
first_matching_type_class_info([Constraint | Constraints], TVar,
        !N, MatchingConstraint, TypeInfoIndex) :-
    Constraint = constraint(_, ArgTypes),
    type_vars_in_types(ArgTypes, TVs),
    ( if list.index1_of_first_occurrence(TVs, TVar, Index) then
        MatchingConstraint = Constraint,
        TypeInfoIndex = Index
    else
        !:N = !.N + 1,
        first_matching_type_class_info(Constraints, TVar,
            !N, MatchingConstraint, TypeInfoIndex)
    ).

%---------------------------------------------------------------------------%

:- pred make_du_ptag_ordered_table(du_functor::in,
    map(ptag, sectag_table)::in, map(ptag, sectag_table)::out) is det.

make_du_ptag_ordered_table(DuFunctor, !PtagTable) :-
    DuRep = DuFunctor ^ du_rep,
    (
        DuRep = du_ll_rep(Ptag, SectagAndLocn),
        (
            SectagAndLocn = sectag_locn_none,
            SectagLocn = sectag_none,
            Sectag = 0u,
            NumSectagBits = -1i8
        ;
            SectagAndLocn = sectag_locn_none_direct_arg,
            SectagLocn = sectag_none_direct_arg,
            Sectag = 0u,
            NumSectagBits = -1i8
        ;
            SectagAndLocn = sectag_locn_local_rest_of_word(Sectag),
            SectagLocn = sectag_local_rest_of_word,
            NumSectagBits = -1i8
        ;
            SectagAndLocn = sectag_locn_local_bits(Sectag, NumSectagBitsUint8,
                Mask),
            SectagLocn = sectag_local_bits(NumSectagBitsUint8, Mask),
            NumSectagBits = int8.cast_from_uint8(NumSectagBitsUint8)
        ;
            SectagAndLocn = sectag_locn_remote_word(Sectag),
            SectagLocn = sectag_remote_word,
            NumSectagBits = -1i8
        ;
            SectagAndLocn = sectag_locn_remote_bits(Sectag,
                NumSectagBitsUint8, Mask),
            SectagLocn = sectag_remote_bits(NumSectagBitsUint8, Mask),
            NumSectagBits = int8.cast_from_uint8(NumSectagBitsUint8)
        )
    ;
        DuRep = du_hl_rep(Data),
        % Treat this as it were
        %   du_ll_rep(ptag(0u8), sectag_locn_remote_word(Data)).
        Ptag = ptag(0u8),
        Sectag = Data,
        SectagLocn = sectag_remote_word,
        NumSectagBits = -1i8
    ),
    ( if map.search(!.PtagTable, Ptag, SectagTable0) then
        SectagTable0 = sectag_table(Locn0, NumSectagBits0, NumSharers0,
            SectagMap0),
        expect(unify(NumSectagBits0, NumSectagBits), $pred,
            "sectag num bits disagreement"),
        map.det_insert(Sectag, DuFunctor, SectagMap0, SectagMap),
        SectagTable = sectag_table(Locn0, NumSectagBits0, NumSharers0 + 1u32,
            SectagMap),
        map.det_update(Ptag, SectagTable, !PtagTable)
    else
        SectagMap = map.singleton(Sectag, DuFunctor),
        SectagTable = sectag_table(SectagLocn, NumSectagBits, 1u32,
            SectagMap),
        map.det_insert(Ptag, SectagTable, !PtagTable)
    ).

:- pred make_du_name_ordered_table(du_functor::in,
    map(string, map(uint16, du_functor))::in,
    map(string, map(uint16, du_functor))::out) is det.

make_du_name_ordered_table(DuFunctor, !NameTable) :-
    Name = DuFunctor ^ du_name,
    Arity = DuFunctor ^ du_orig_arity,
    ( if map.search(!.NameTable, Name, NameMap0) then
        map.det_insert(Arity, DuFunctor, NameMap0, NameMap),
        map.det_update(Name, NameMap, !NameTable)
    else
        NameMap = map.singleton(Arity, DuFunctor),
        map.det_insert(Name, NameMap, !NameTable)
    ).

:- pred is_ptag_table_indexable(map(ptag, sectag_table)::in) is semidet.

is_ptag_table_indexable(PtagTable) :-
    map.min_key(PtagTable) = ptag(0u8),
    map.max_key(PtagTable) = ptag(MaxPtagUint8),
    map.count(PtagTable, Count),
    uint8.from_int(Count - 1, MaxPtagUint8).

%---------------------------------------------------------------------------%

    % Construct the array mapping ordinal constructor numbers
    % to lexicographic constructor numbers.
    %
:- func make_functor_number_map(list(constructor_repn)) = list(uint32).

make_functor_number_map(OrdinalCtors) = OrdinalToLexicographicSeqNums :-
    OrdinalCtorNames = list.map(ctor_name_arity, OrdinalCtors),
    list.sort(OrdinalCtorNames, LexicographicCtorNames),
    LexicographicSeqNums = 0 `..` (list.length(OrdinalCtors) - 1),
    map.from_corresponding_lists(LexicographicCtorNames, LexicographicSeqNums,
        CtorNameToSeqNumMap),
    list.map(lookup_functor_number(CtorNameToSeqNumMap),
        OrdinalCtorNames, OrdinalToLexicographicSeqNums).

:- func ctor_name_arity(constructor_repn) = {sym_name, arity}.

ctor_name_arity(Ctor) = {Ctor ^ cr_name, list.length(Ctor ^ cr_args)}.

:- pred lookup_functor_number(map({sym_name, arity}, int)::in,
    {sym_name, arity}::in, uint32::out) is det.

lookup_functor_number(CtorNameToSeqNumMap, CtorName, SeqNumUint32) :-
    map.lookup(CtorNameToSeqNumMap, CtorName, SeqNum),
    SeqNumUint32 = uint32.det_from_int(SeqNum).

%---------------------------------------------------------------------------%

:- pred maybe_get_base_type_ctor(module_info::in, maybe_subtype::in,
    maybe(type_ctor)::out) is det.

maybe_get_base_type_ctor(ModuleInfo, MaybeSuperType, MaybeBaseTypeCtor) :-
    (
        MaybeSuperType = subtype_of(SuperType),
        module_info_get_type_table(ModuleInfo, TypeTable),
        ( if
            type_to_ctor(SuperType, SuperTypeCtor),
            get_base_type_ctor(TypeTable, SuperTypeCtor, BaseTypeCtor)
        then
            MaybeBaseTypeCtor = yes(BaseTypeCtor)
        else
            unexpected($pred, "cannot get base type ctor")
        )
    ;
        MaybeSuperType = not_a_subtype,
        MaybeBaseTypeCtor = no
    ).

%---------------------------------------------------------------------------%

compute_du_ptag_layout_flags(SectagTable, Flags) :-
    ( if is_sectag_table_indexable(SectagTable) then
        SectagAltsIndexable = yes
    else
        SectagAltsIndexable = no
    ),
    Flags = du_ptag_layout_flags(SectagAltsIndexable).

:- pred is_sectag_table_indexable(sectag_table::in) is semidet.

is_sectag_table_indexable(SectagTable) :-
    SectagTable = sectag_table(_Locn, _NumSectagBits, NumSharers, SectagMap),
    map.min_key(SectagMap) = 0u,
    map.max_key(SectagMap) = MaxSectag,
    map.count(SectagMap, Count),
    uint.from_int(Count - 1, MaxSectag),
    uint32.from_int(Count, NumSharers).

%---------------------------------------------------------------------------%

compute_contains_var_bit_vector(ArgTypes) = Vector :-
    compute_contains_var_bit_vector_2(ArgTypes, 0, 0u16, Vector).

:- pred compute_contains_var_bit_vector_2(
    list(rtti_maybe_pseudo_type_info_or_self)::in, int::in,
    uint16::in, uint16::out) is det.

compute_contains_var_bit_vector_2([], _, !Vector).
compute_contains_var_bit_vector_2([ArgType | ArgTypes], ArgNum, !Vector) :-
    (
        ArgType = plain(_)
    ;
        ArgType = pseudo(_),
        update_contains_var_bit_vector(ArgNum, !Vector)
    ;
        ArgType = self,
        % The backend currently doesn't perform the optimization that
        % lets it avoid memory allocation on self types.
        update_contains_var_bit_vector(ArgNum, !Vector)
    ),
    compute_contains_var_bit_vector_2(ArgTypes, ArgNum + 1, !Vector).

:- pred update_contains_var_bit_vector(int::in, uint16::in, uint16::out)
    is det.

update_contains_var_bit_vector(ArgNum, !Vector) :-
    ( if ArgNum >= contains_var_bit_vector_size - 1 then
        BitNum = contains_var_bit_vector_size - 1
    else
        BitNum = ArgNum
    ),
    !:Vector = !.Vector \/ (1u16 << BitNum).

    % This function gives the size of the MR_du_functor_arg_type_contains_var
    % field of the C type MR_DuFunctorDesc in bits.
    %
:- func contains_var_bit_vector_size = int.

contains_var_bit_vector_size = 16.

%---------------------------------------------------------------------------%
:- end_module backend_libs.type_ctor_info.
%---------------------------------------------------------------------------%
