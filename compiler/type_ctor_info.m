%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% Copyright (C) 2014-2018 The Mercury team.
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

    % Compute the "contains var" bit vector. The input is a list describing
    % the types of the arguments of a function symbol. The output is an
    % bit vector (represented as a 16 bit integer) in which each bit is set
    % if the type of the corresponding argument contains a type variable.
    % If the function symbol has more than 16 arguments, then the last bit
    % is true if any of the arguments after the 15th contain a type
    % variable in their type.
    %
:- func compute_contains_var_bit_vector(
    list(rtti_maybe_pseudo_type_info_or_self)) = int.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.foreign.
:- import_module backend_libs.pseudo_type_info.
:- import_module backend_libs.type_class_info.
:- import_module check_hlds.
:- import_module check_hlds.type_util.
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
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
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
            create_type_ctor_gen(ModuleInfo, TypeCtorDefn,
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
        unexpected($module, $pred, "unqualified type " ++ TypeName)
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
:- pred create_type_ctor_gen(module_info::in,
    pair(type_ctor, hlds_type_defn)::in, module_name::in, string::in, int::in,
    hlds_type_defn::out) is semidet.

create_type_ctor_gen(ModuleInfo, TypeCtor - TypeDefn, TypeModuleName,
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
        expect(unify(TypeModuleName, ModuleName), $module, $pred,
            "module mismatch"),
        gen_type_ctor_gen_info(ModuleInfo, TypeCtor, TypeModuleName,
            TypeName, TypeArity, builtin_type_defn, TypeCtorGenInfo)
    ;
        SymName = unqualified(TypeName),
        unexpected($module, $pred, "unqualified type " ++ TypeName)
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
        unexpected($module, $pred, "abstract_type")
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
        Details = tcd_builtin(BuiltinCtor)
    else if
        ModuleName = unqualified(ModuleStr),
        impl_type_ctor(ModuleStr, TypeName, TypeArity, ImplCtor)
    then
        Details = tcd_impl_artifact(ImplCtor)
    else
        (
            TypeBody = hlds_abstract_type(_),
            unexpected($module, $pred, "abstract_type")
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
            Details = tcd_eqv(MaybePseudoTypeInfo)
        ;
            TypeBody = hlds_foreign_type(ForeignBody),
            foreign_type_body_to_exported_type(ModuleInfo, ForeignBody, _, _,
                Assertions),
            ( if asserted_stable(Assertions) then
                IsStable = is_stable
            else
                IsStable = is_not_stable
            ),
            Details = tcd_foreign(IsStable)
        ;
            TypeBody = hlds_eqv_type(Type),
            % There can be no existentially typed args to an equivalence.
            UnivTVars = TypeArity,
            ExistTVars = [],
            pseudo_type_info.construct_maybe_pseudo_type_info(Type,
                UnivTVars, ExistTVars, MaybePseudoTypeInfo),
            Details = tcd_eqv(MaybePseudoTypeInfo)
        ;
            TypeBody = hlds_du_type(_Ctors, MaybeCanonical, MaybeRepn,
                _IsForeignType),
            (
                MaybeRepn = no,
                unexpected($pred, "MaybeRepn = no")
            ;
                MaybeRepn = yes(Repn),
                Repn = du_type_repn(_ConsTagMap, CtorRepns, _ConsCtorMap,
                    _CheaperTagTest, DuTypeKind, _MaybeDirectArgCtors,
                    ReservedAddr)
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
                make_mercury_enum_details(CtorRepns, EqualityAxioms, Details)
            ;
                DuTypeKind = du_type_kind_foreign_enum(Lang),
                make_foreign_enum_details(Lang, CtorRepns, EqualityAxioms,
                    Details)
            ;
                DuTypeKind = du_type_kind_direct_dummy,
                make_mercury_enum_details(CtorRepns, EqualityAxioms, Details)
            ;
                DuTypeKind = du_type_kind_notag(FunctorName, ArgType,
                    MaybeArgName),
                make_notag_details(TypeArity, FunctorName, ArgType,
                    MaybeArgName, EqualityAxioms, Details)
            ;
                DuTypeKind = du_type_kind_general,
                make_du_details(ModuleInfo, CtorRepns, TypeArity,
                    EqualityAxioms, ReservedAddr, Details)
            )
        )
    ),
    some [!Flags] (
        !:Flags = set.init,
        (
            TypeBody = hlds_du_type(_, _, _, _),
            set.insert(kind_of_du_flag, !Flags)
        ;
            ( TypeBody = hlds_eqv_type(_)
            ; TypeBody = hlds_foreign_type(_)
            ; TypeBody = hlds_solver_type(_)
            ; TypeBody = hlds_abstract_type(_)
            )
        ),
        TypeCtorData = type_ctor_data(Version, ModuleName, TypeName, TypeArity,
            UnifyUniv, CompareUniv, !.Flags, Details)
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
:- func type_ctor_info_rtti_version = int.

type_ctor_info_rtti_version = 17.

%---------------------------------------------------------------------------%

    % Make the functor and layout tables for a notag type.
    %
:- pred make_notag_details(int::in, sym_name::in, mer_type::in,
    maybe(string)::in, equality_axioms::in, type_ctor_details::out) is det.

make_notag_details(TypeArity, SymName, ArgType, MaybeArgName, EqualityAxioms,
        Details) :-
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
    Details = tcd_notag(EqualityAxioms, Functor).

%---------------------------------------------------------------------------%

:- type name_sort_info == assoc_list(pair(string, int), ctor_rtti_name).

    % Make the functor and layout tables for an enum type.
    %
:- pred make_mercury_enum_details(list(constructor_repn)::in,
    equality_axioms::in, type_ctor_details::out) is det.

make_mercury_enum_details(CtorRepns, EqualityAxioms, Details) :-
    make_enum_functors(CtorRepns, 0, EnumFunctors),
    ValueMap0 = map.init,
    NameMap0 = map.init,
    list.foldl2(make_enum_maps, EnumFunctors,
        ValueMap0, ValueMap, NameMap0, NameMap),
    (
        CtorRepns = [],
        unexpected($module, $pred, "enum with no ctors")
    ;
        CtorRepns = [_],
        IsDummy = yes
    ;
        CtorRepns = [_, _ | _],
        IsDummy = no
    ),
    FunctorNumberMap = make_functor_number_map(CtorRepns),
    Details = tcd_enum(EqualityAxioms, EnumFunctors, ValueMap, NameMap,
        IsDummy, FunctorNumberMap).

    % Create an enum_functor structure for each functor in an enum type.
    % The functors are given to us in ordinal order (since that's how the HLDS
    % stored them), and that is how we return the list of rtti names of the
    % enum_functor_desc structures; that way, it is directly usable in the type
    % layout structure. We also return a structure that allows our caller to
    % sort this list on functor name, which is how the type functors structure
    % is constructed.
    %
:- pred make_enum_functors(list(constructor_repn)::in, int::in,
    list(enum_functor)::out) is det.

make_enum_functors([], _, []).
make_enum_functors([FunctorRepn | FunctorRepns], NextOrdinal,
        [EnumFunctor | EnumFunctors]) :-
    FunctorRepn = ctor_repn(MaybeExistConstraints, SymName, ConsTag,
        _FunctorArgRepns, Arity, _Context),
    expect(unify(MaybeExistConstraints, no_exist_constraints), $module, $pred,
        "existential constraints in functor in enum"),
    expect(unify(Arity, 0), $module, $pred,
        "functor in enum has nonzero arity"),
    expect(unify(ConsTag, int_tag(int_tag_int(NextOrdinal))), $module, $pred,
        "mismatch on constant assigned to functor in enum"),
    FunctorName = unqualify_name(SymName),
    EnumFunctor = enum_functor(FunctorName, NextOrdinal),
    make_enum_functors(FunctorRepns, NextOrdinal + 1, EnumFunctors).

:- pred make_enum_maps(enum_functor::in,
    map(int, enum_functor)::in, map(int, enum_functor)::out,
    map(string, enum_functor)::in, map(string, enum_functor)::out) is det.

make_enum_maps(EnumFunctor, !ValueMap, !NameMap) :-
    EnumFunctor = enum_functor(FunctorName, Ordinal),
    map.det_insert(Ordinal, EnumFunctor, !ValueMap),
    map.det_insert(FunctorName, EnumFunctor, !NameMap).

%---------------------------------------------------------------------------%

    % Make the functor and layout tables for a foreign enum type.
    %
:- pred make_foreign_enum_details(foreign_language::in,
    list(constructor_repn)::in, equality_axioms::in,
    type_ctor_details::out) is det.

make_foreign_enum_details(Lang, CtorRepns, EqualityAxioms, Details) :-
    make_foreign_enum_functors(Lang, CtorRepns, 0, ForeignEnumFunctors),
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
    list(constructor_repn)::in, int::in,
    list(foreign_enum_functor)::out) is det.

make_foreign_enum_functors(_, [], _, []).
make_foreign_enum_functors(Lang, [FunctorRepn | FunctorRepns], NextOrdinal,
        [ForeignEnumFunctor | ForeignEnumFunctors]) :-
    FunctorRepn = ctor_repn(MaybeExistConstraints, SymName, ConsTag,
        _FunctorArgRepns, Arity, _Context),
    expect(unify(MaybeExistConstraints, no_exist_constraints), $module, $pred,
        "existential constraints in functor in enum"),
    expect(unify(Arity, 0), $module, $pred,
        "functor in foreign enum has nonzero arity"),
    (
        ConsTag = foreign_tag(ForeignTagLang, ForeignTagValue0),
        expect(unify(Lang, ForeignTagLang), $module, $pred,
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
        ; ConsTag = single_functor_tag
        ; ConsTag = unshared_tag(_)
        ; ConsTag = direct_arg_tag(_)
        ; ConsTag = shared_remote_tag(_, _)
        ; ConsTag = shared_local_tag(_, _)
        ; ConsTag = no_tag
        ; ConsTag = reserved_address_tag(_)
        ; ConsTag = shared_with_reserved_addresses_tag(_, _)
        ),
        unexpected($module, $pred, "non foreign tag for foreign enum functor")
    ),
    FunctorName = unqualify_name(SymName),
    ForeignEnumFunctor = foreign_enum_functor(FunctorName, NextOrdinal,
        ForeignTagValue),
    make_foreign_enum_functors(Lang, FunctorRepns, NextOrdinal + 1,
        ForeignEnumFunctors).

:- pred make_foreign_enum_maps(foreign_enum_functor::in,
    map(int, foreign_enum_functor)::in,
    map(int, foreign_enum_functor)::out,
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

:- type reserved_addr_map == map(reserved_address, rtti_data).

:- func is_du_functor(maybe_reserved_functor::in) = (du_functor::out)
    is semidet.

is_du_functor(du_func(DuFunctor)) = DuFunctor.

:- func is_reserved_functor(maybe_reserved_functor::in) =
    (reserved_functor::out) is semidet.

is_reserved_functor(res_func(ResFunctor)) = ResFunctor.

    % Make the functor and layout tables for a du type
    % (including reserved_addr types).
    %
:- pred make_du_details(module_info::in, list(constructor_repn)::in,
    int::in, equality_axioms::in, uses_reserved_address::in,
    type_ctor_details::out) is det.

make_du_details(ModuleInfo, Ctors, TypeArity, EqualityAxioms, ReservedAddr,
        Details) :-
    make_maybe_res_functors(ModuleInfo, Ctors, 0, TypeArity, MaybeResFunctors),
    DuFunctors = list.filter_map(is_du_functor, MaybeResFunctors),
    ResFunctors = list.filter_map(is_reserved_functor, MaybeResFunctors),
    list.foldl(make_du_ptag_ordered_table, DuFunctors,
        map.init, DuPtagTable),
    FunctorNumberMap = make_functor_number_map(Ctors),
    (
        ResFunctors = [],
        expect(unify(ReservedAddr, does_not_use_reserved_address),
            $module, $pred, "ReservedAddr is not does_not_use_reserved_addr"),
        list.foldl(make_du_name_ordered_table, DuFunctors,
            map.init, DuNameOrderedMap),
        Details = tcd_du(EqualityAxioms, DuFunctors, DuPtagTable,
            DuNameOrderedMap, FunctorNumberMap)
    ;
        ResFunctors = [_ | _],
        expect(unify(ReservedAddr, uses_reserved_address),
            $module, $pred, "ReservedAddr is not uses_reserved_addr"),
        list.foldl(make_res_name_ordered_table, MaybeResFunctors,
            map.init, ResNameOrderedMap),
        Details = tcd_reserved(EqualityAxioms, MaybeResFunctors,
            ResFunctors, DuPtagTable, ResNameOrderedMap, FunctorNumberMap)
    ).

:- type maybe_reserved_rep
    --->    reserved_rep(
                reserved_address
            )
    ;       du_rep(
                du_rep
            ).

    % Create a du_functor_desc structure for each functor in a du type.
    % Besides returning a list of the rtti names of their du_functor_desc
    % structures, we return two other items of information. The SortInfo
    % enables our caller to sort these rtti names on functor name and then
    % arity, which is how the type functors structure is constructed. The
    % TagMap groups the rttis into groups depending on their primary tags;
    % this is how the type layout structure is constructed.
    %
:- pred make_maybe_res_functors(module_info::in, list(constructor_repn)::in,
    int::in, int::in, list(maybe_reserved_functor)::out) is det.

make_maybe_res_functors(_, [], _, _, []).
make_maybe_res_functors(ModuleInfo, [CtorRepn | CtorRepns],
        NextOrdinal, TypeArity, [MaybeResFunctor | MaybeResFunctors]) :-
    CtorRepn = ctor_repn(MaybeExistConstraints, SymName, ConsTag,
        ConsArgRepns, Arity, _Context),
    FunctorName = unqualify_name(SymName),
    get_maybe_reserved_rep(ConsTag, ConsRep),
    (
        MaybeExistConstraints = no_exist_constraints,
        ExistTVars = [],
        MaybeExistInfo = no
    ;
        MaybeExistConstraints = exist_constraints(ExistConstraints),
        ExistConstraints = cons_exist_constraints(ExistTVars, Constraints),
        module_info_get_class_table(ModuleInfo, ClassTable),
        generate_exist_into(ExistTVars, Constraints, ClassTable, ExistInfo),
        MaybeExistInfo = yes(ExistInfo)
    ),
    list.map_foldl(generate_du_arg_info(TypeArity, ExistTVars),
        ConsArgRepns, ArgInfos, functor_subtype_none, FunctorSubtypeInfo),
    (
        ConsRep = du_rep(DuRep),
        DuFunctor = du_functor(FunctorName, Arity, NextOrdinal, DuRep,
            ArgInfos, MaybeExistInfo, FunctorSubtypeInfo),
        MaybeResFunctor = du_func(DuFunctor)
    ;
        ConsRep = reserved_rep(ResRep),
        expect(unify(Arity, 0), $module, $pred, "bad arity"),
        expect(unify(ArgInfos, []), $module, $pred, "bad args"),
        expect(unify(MaybeExistInfo, no), $module, $pred, "bad exist"),
        ResFunctor = reserved_functor(FunctorName, NextOrdinal, ResRep),
        MaybeResFunctor = res_func(ResFunctor)
    ),
    make_maybe_res_functors(ModuleInfo, CtorRepns,
        NextOrdinal + 1, TypeArity, MaybeResFunctors).

:- pred get_maybe_reserved_rep(cons_tag::in, maybe_reserved_rep::out) is det.

get_maybe_reserved_rep(ConsTag, ConsRep) :-
    (
        ConsTag = single_functor_tag,
        ConsPtag = 0,
        SecTagLocn = sectag_locn_none,
        ConsRep = du_rep(du_ll_rep(ConsPtag, SecTagLocn))
    ;
        ConsTag = unshared_tag(ConsPtag),
        SecTagLocn = sectag_locn_none,
        ConsRep = du_rep(du_ll_rep(ConsPtag, SecTagLocn))
    ;
        ConsTag = direct_arg_tag(ConsPtag),
        SecTagLocn = sectag_locn_none_direct_arg,
        ConsRep = du_rep(du_ll_rep(ConsPtag, SecTagLocn))
    ;
        ConsTag = shared_local_tag(ConsPtag, ConsStag),
        ConsRep = du_rep(du_ll_rep(ConsPtag, sectag_locn_local(ConsStag)))
    ;
        ConsTag = shared_remote_tag(ConsPtag, ConsStag),
        ConsRep = du_rep(du_ll_rep(ConsPtag, sectag_locn_remote(ConsStag)))
    ;
        ConsTag = reserved_address_tag(ReservedAddr),
        ConsRep = reserved_rep(ReservedAddr)
    ;
        ConsTag = shared_with_reserved_addresses_tag(_RAs, ThisTag),
        % Here we can just ignore the fact that this cons_tag is
        % shared with reserved addresses.
        get_maybe_reserved_rep(ThisTag, ConsRep)
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
        unexpected($module, $pred, "bad cons_tag for du function symbol")
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

    % This function gives the size of the MR_du_functor_arg_type_contains_var
    % field of the C type MR_DuFunctorDesc in bits.
    %
:- func contains_var_bit_vector_size = int.

contains_var_bit_vector_size = 16.

    % Construct the RTTI structures that record information about the locations
    % of the typeinfos describing the types of the existentially typed
    % arguments of a functor.
    %
:- pred generate_exist_into(list(tvar)::in, list(prog_constraint)::in,
    class_table::in, exist_info::out) is det.

generate_exist_into(ExistTVars, Constraints, ClassTable, ExistInfo) :-
    % XXX The Ts being gathered are not type variables, they are types.
    list.map((pred(C::in, Ts::out) is det :- C = constraint(_, Ts)),
        Constraints, ConstrainedTVars0),
    list.condense(ConstrainedTVars0, ConstrainedTVars1),
    type_vars_list(ConstrainedTVars1, ConstrainedTVars2),
    list.delete_elems(ExistTVars, ConstrainedTVars2, UnconstrainedTVars),
    % We do this to maintain the ordering of the type variables.
    list.delete_elems(ExistTVars, UnconstrainedTVars, ConstrainedTVars),
    map.init(LocnMap0),
    list.foldl2((pred(T::in, N0::in, N::out, Lm0::in, Lm::out) is det :-
            Locn = plain_typeinfo(N0),
            map.det_insert(T, Locn, Lm0, Lm),
            N = N0 + 1
        ), UnconstrainedTVars, 0, TIsPlain, LocnMap0, LocnMap1),
    list.length(ExistTVars, AllTIs),
    TIsInTCIs = AllTIs - TIsPlain,
    list.foldl(find_type_info_index(Constraints, ClassTable, TIsPlain),
        ConstrainedTVars, LocnMap1, LocnMap),
    TCConstraints = list.map(generate_class_constraint, Constraints),
    list.map((pred(TVar::in, Locn::out) is det :-
        map.lookup(LocnMap, TVar, Locn)),
        ExistTVars, ExistLocns),
    ExistInfo = exist_info(TIsPlain, TIsInTCIs, TCConstraints, ExistLocns).

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
    Locn = typeinfo_in_tci(Slot, RealTypeInfoIndex),
    map.det_insert(TVar, Locn, !LocnMap).

:- pred first_matching_type_class_info(list(prog_constraint)::in, tvar::in,
    int::in, int::out, prog_constraint::out, int::out) is det.

first_matching_type_class_info([], _, !N, _, _) :-
    unexpected($module, $pred, "not found").
first_matching_type_class_info([Constraint | Constraints], TVar,
        !N, MatchingConstraint, TypeInfoIndex) :-
    Constraint = constraint(_, ArgTypes),
    type_vars_list(ArgTypes, TVs),
    ( if list.index1_of_first_occurrence(TVs, TVar, Index) then
        MatchingConstraint = Constraint,
        TypeInfoIndex = Index
    else
        !:N = !.N + 1,
        first_matching_type_class_info(Constraints, TVar,
            !N, MatchingConstraint, TypeInfoIndex)
    ).

%---------------------------------------------------------------------------%

:- pred make_du_ptag_ordered_table(du_functor::in, map(int, sectag_table)::in,
    map(int, sectag_table)::out) is det.

make_du_ptag_ordered_table(DuFunctor, !PtagTable) :-
    DuRep = DuFunctor ^ du_rep,
    (
        DuRep = du_ll_rep(Ptag, SectagAndLocn),
        (
            SectagAndLocn = sectag_locn_none,
            SectagLocn = sectag_none,
            Sectag = 0
        ;
            SectagAndLocn = sectag_locn_none_direct_arg,
            SectagLocn = sectag_none_direct_arg,
            Sectag = 0
        ;
            SectagAndLocn = sectag_locn_local(Sectag),
            SectagLocn = sectag_local
        ;
            SectagAndLocn = sectag_locn_remote(Sectag),
            SectagLocn = sectag_remote
        ),
        ( if map.search(!.PtagTable, Ptag, SectagTable0) then
            SectagTable0 = sectag_table(Locn0, NumSharers0, SectagMap0),
            expect(unify(SectagLocn, Locn0), $module, $pred,
                "sectag locn disagreement"),
            map.det_insert(Sectag, DuFunctor, SectagMap0, SectagMap),
            SectagTable = sectag_table(Locn0, NumSharers0 + 1, SectagMap),
            map.det_update(Ptag, SectagTable, !PtagTable)
        else
            SectagMap = map.singleton(Sectag, DuFunctor),
            SectagTable = sectag_table(SectagLocn, 1, SectagMap),
            map.det_insert(Ptag, SectagTable, !PtagTable)
        )
    ;
        DuRep = du_hl_rep(_),
        unexpected($module, $pred, "du_hl_rep")
    ).

:- pred make_du_name_ordered_table(du_functor::in,
    map(string, map(int, du_functor))::in,
    map(string, map(int, du_functor))::out) is det.

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

:- pred make_res_name_ordered_table(maybe_reserved_functor::in,
    map(string, map(int, maybe_reserved_functor))::in,
    map(string, map(int, maybe_reserved_functor))::out) is det.

make_res_name_ordered_table(MaybeResFunctor, !NameTable) :-
    (
        MaybeResFunctor = du_func(DuFunctor),
        Name = DuFunctor ^ du_name,
        Arity = DuFunctor ^ du_orig_arity
    ;
        MaybeResFunctor = res_func(ResFunctor),
        Name = ResFunctor ^ res_name,
        Arity = 0
    ),
    ( if map.search(!.NameTable, Name, NameMap0) then
        map.det_insert(Arity, MaybeResFunctor, NameMap0, NameMap),
        map.det_update(Name, NameMap, !NameTable)
    else
        NameMap = map.singleton(Arity, MaybeResFunctor),
        map.det_insert(Name, NameMap, !NameTable)
    ).

%---------------------------------------------------------------------------%

    % Construct the array mapping ordinal constructor numbers
    % to lexicographic constructor numbers.
    %
:- func make_functor_number_map(list(constructor_repn)) = list(int).

make_functor_number_map(OrdinalCtors) = OrdinalToLexicographicSeqNums :-
    OrdinalCtorNames = list.map(ctor_name_arity, OrdinalCtors),
    list.sort(OrdinalCtorNames, LexicographicCtorNames),
    LexicographicSeqNums = 0 `..` (list.length(OrdinalCtors) - 1),
    map.from_corresponding_lists(LexicographicCtorNames, LexicographicSeqNums,
        LexicographicCtorNameToSeqNumMap),
    map.apply_to_list(OrdinalCtorNames, LexicographicCtorNameToSeqNumMap,
        OrdinalToLexicographicSeqNums).

:- func ctor_name_arity(constructor_repn) = {sym_name, arity}.

ctor_name_arity(Ctor) = {Ctor ^ cr_name, list.length(Ctor ^ cr_args)}.

%---------------------------------------------------------------------------%

compute_contains_var_bit_vector(ArgTypes) = Vector :-
    compute_contains_var_bit_vector_2(ArgTypes, 0, 0, Vector).

:- pred compute_contains_var_bit_vector_2(
    list(rtti_maybe_pseudo_type_info_or_self)::in, int::in, int::in, int::out)
    is det.

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

:- pred update_contains_var_bit_vector(int::in, int::in, int::out) is det.

update_contains_var_bit_vector(ArgNum, !Vector) :-
    ( if ArgNum >= contains_var_bit_vector_size - 1 then
        BitNum = contains_var_bit_vector_size - 1
    else
        BitNum = ArgNum
    ),
    !:Vector = !.Vector \/ (1 << BitNum).

%---------------------------------------------------------------------------%
:- end_module backend_libs.type_ctor_info.
%---------------------------------------------------------------------------%
