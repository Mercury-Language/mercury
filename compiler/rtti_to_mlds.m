%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2012, 2014 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: rtti_to_mlds.m.
% Authors: fjh, zs.
%
% This module defines routines to convert from the back-end-independent
% RTTI data structures into MLDS definitions.
% The RTTI data structures are used for static data that is used
% for handling RTTI, polymorphism, and typeclasses.
%
%-----------------------------------------------------------------------------%

:- module ml_backend.rtti_to_mlds.
:- interface.

:- import_module backend_libs.
:- import_module backend_libs.rtti.
:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module ml_backend.ml_global_data.
:- import_module ml_backend.mlds.

:- import_module list.

%-----------------------------------------------------------------------------%

    % Add the MLDS definitions for the given rtti_data(s) to the
    % ml_global_data structure.
    %
:- pred add_rtti_datas_to_mlds(module_info::in, list(rtti_data)::in,
    ml_global_data::in, ml_global_data::out) is det.
:- pred add_rtti_data_to_mlds(module_info::in, rtti_data::in,
    ml_global_data::in, ml_global_data::out) is det.

    % Given a list of MLDS RTTI data definitions (only), return the definitions
    % such that if X appears in the initialiser for Y then X appears earlier in
    % the list than Y.
    %
    % This function returns a list of cliques so that problems with ordering
    % within cliques, if any, may be easier to discover.
    %
:- func order_mlds_rtti_defns(list(mlds_defn)) = list(list(mlds_defn)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.foreign.
:- import_module backend_libs.type_ctor_info.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_rtti.
:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module ml_backend.ml_closure_gen.
:- import_module ml_backend.ml_code_util.
:- import_module ml_backend.ml_gen_info.
:- import_module ml_backend.ml_util.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.

:- import_module assoc_list.
:- import_module bool.
:- import_module counter.
:- import_module digraph.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module term.
:- import_module univ.

%-----------------------------------------------------------------------------%

add_rtti_datas_to_mlds(ModuleInfo, RttiDatas, !GlobalData) :-
    list.foldl(add_rtti_data_to_mlds(ModuleInfo), RttiDatas, !GlobalData).

add_rtti_data_to_mlds(ModuleInfo, RttiData, !GlobalData) :-
    ( if RttiData = rtti_data_pseudo_type_info(type_var(_)) then
        % These just get represented as integers, so we don't need to define
        % a structure for them; which is why rtti_data_to_id/3 does not
        % handle this case.
        true
    else
        gen_init_rtti_data_defn(ModuleInfo, RttiData, !GlobalData)
    ).

:- pred rtti_name_and_init_to_defn(rtti_type_ctor::in, ctor_rtti_name::in,
    mlds_initializer::in, ml_global_data::in, ml_global_data::out) is det.

rtti_name_and_init_to_defn(RttiTypeCtor, RttiName, Initializer, !GlobalData) :-
    RttiId = ctor_rtti_id(RttiTypeCtor, RttiName),
    rtti_id_and_init_to_defn(RttiId, Initializer, !GlobalData).

:- pred rtti_id_and_init_to_defn(rtti_id::in, mlds_initializer::in,
    ml_global_data::in, ml_global_data::out) is det.

rtti_id_and_init_to_defn(RttiId, Initializer, !GlobalData) :-
    Name = entity_data(mlds_rtti(RttiId)),
    rtti_entity_name_and_init_to_defn(Name, RttiId, Initializer, !GlobalData).

:- pred rtti_entity_name_and_init_to_defn(mlds_entity_name::in, rtti_id::in,
    mlds_initializer::in, ml_global_data::in, ml_global_data::out) is det.

rtti_entity_name_and_init_to_defn(Name, RttiId, Initializer, !GlobalData) :-
    % Generate the context.
    %
    % XXX The rtti_data ought to include a prog_context (the context of the
    % corresponding type or instance definition).
    term.context_init(Context),
    MLDS_Context = mlds_make_context(Context),

    % Generate the declaration flags.
    Exported = rtti_id_is_exported(RttiId),
    Flags = rtti_data_decl_flags(Exported),

    % The GC never needs to trace these definitions, because they are static
    % constants, and can point only to other static constants, not to the heap.
    GCStatement = gc_no_stmt,

    % Generate the declaration body, i.e. the type and the initializer.
    MLDS_Type = mlds_rtti_type(item_type(RttiId)),
    DefnBody = mlds_data(mlds_data_defn(MLDS_Type, Initializer, GCStatement)),
    Defn = mlds_defn(Name, MLDS_Context, Flags, DefnBody),

    ml_global_data_add_flat_rtti_defn(Defn, !GlobalData).

    % Return the declaration flags appropriate for an rtti_data.
    %
:- func rtti_data_decl_flags(bool) = mlds_decl_flags.

rtti_data_decl_flags(Exported) = MLDS_DeclFlags :-
    (
        Exported = yes,
        Access = acc_public
    ;
        Exported = no,
        Access = acc_private
    ),
    PerInstance = one_copy,
    Virtuality = non_virtual,
    Overridability = overridable,
    Constness = const,
    Abstractness = concrete,
    MLDS_DeclFlags = init_decl_flags(Access, PerInstance,
        Virtuality, Overridability, Constness, Abstractness).

%-----------------------------------------------------------------------------%

    % Return an MLDS initializer for the given RTTI definition
    % occurring in the given module.
    %
:- pred gen_init_rtti_data_defn(module_info::in, rtti_data::in,
    ml_global_data::in, ml_global_data::out) is det.

gen_init_rtti_data_defn(ModuleInfo, RttiData, !GlobalData) :-
    rtti_data_to_id(RttiData, RttiId),
    Name = entity_data(mlds_rtti(RttiId)),
    (
        RttiData = rtti_data_base_typeclass_info(_InstanceModule, _ClassId,
            _InstanceStr, BaseTypeClassInfo),
        BaseTypeClassInfo = base_typeclass_info(N1, N2, N3, N4, N5, Methods),
        NumExtra = BaseTypeClassInfo ^ num_extra,
        list.map_foldl(gen_init_method(ModuleInfo, NumExtra),
            Methods, MethodInitializers, !GlobalData),
        Initializer = init_array([
            gen_init_boxed_int(N1),
            gen_init_boxed_int(N2),
            gen_init_boxed_int(N3),
            gen_init_boxed_int(N4),
            gen_init_boxed_int(N5)
            | MethodInitializers
        ]),
        rtti_entity_name_and_init_to_defn(Name, RttiId, Initializer,
            !GlobalData)
    ;
        RttiData = rtti_data_type_info(TypeInfo),
        gen_type_info_defn(ModuleInfo, TypeInfo, Name, RttiId, !GlobalData)
    ;
        RttiData = rtti_data_pseudo_type_info(PseudoTypeInfo),
        gen_pseudo_type_info_defn(ModuleInfo, PseudoTypeInfo, Name, RttiId,
            !GlobalData)
    ;
        RttiData = rtti_data_type_class_decl(TCDecl),
        gen_type_class_decl_defn(ModuleInfo, TCDecl, Name, RttiId,
            !GlobalData)
    ;
        RttiData = rtti_data_type_class_instance(Instance),
        gen_type_class_instance_defn(ModuleInfo, Instance, Name, RttiId,
            !GlobalData)
    ;
        RttiData = rtti_data_type_ctor_info(TypeCtorData),
        TypeCtorData = type_ctor_data(Version, TypeModule, TypeName,
            TypeArity, UnifyUniv, CompareUniv, Flags, TypeCtorDetails),
        RttiTypeCtor = rtti_type_ctor(TypeModule, TypeName, TypeArity),
        TypeModuleName = sym_name_to_string(TypeModule),
        NumPtags = type_ctor_details_num_ptags(TypeCtorDetails),
        NumFunctors = type_ctor_details_num_functors(TypeCtorDetails),
        FunctorsRttiId = ctor_rtti_id(RttiTypeCtor, type_ctor_type_functors),
        LayoutRttiId = ctor_rtti_id(RttiTypeCtor, type_ctor_type_layout),

        gen_functors_layout_info(ModuleInfo, RttiTypeCtor, TypeCtorDetails,
            FunctorsInfo, LayoutInfo, NumberMapInfo, !GlobalData),

        % Note that gen_init_special_pred will by necessity add an extra
        % level of indirection to calling the special preds. However, the
        % backend compiler should be smart enough to ensure that this is
        % inlined away.
        gen_init_special_pred(ModuleInfo, UnifyUniv, UnifyInitializer,
            !GlobalData),
        gen_init_special_pred(ModuleInfo, CompareUniv, CompareInitializer,
            !GlobalData),

        Initializer = init_struct(mlds_rtti_type(item_type(RttiId)), [
            gen_init_int(TypeArity),
            gen_init_int(Version),
            gen_init_int(NumPtags),
            gen_init_type_ctor_rep(TypeCtorData),
            UnifyInitializer,
            CompareInitializer,
            gen_init_string(TypeModuleName),
            gen_init_string(TypeName),
            % In the C back-end, these two "structs" are actually unions.
            % We need to use `init_struct' here so that the initializers
            % get enclosed in curly braces.
            init_struct(mlds_rtti_type(item_type(FunctorsRttiId)), [
                FunctorsInfo
            ]),
            init_struct(mlds_rtti_type(item_type(LayoutRttiId)), [
                LayoutInfo
            ]),
            gen_init_int(NumFunctors),
            gen_init_int(encode_type_ctor_flags(Flags)),
            NumberMapInfo

            % These two are commented out while the corresponding fields of the
            % MR_TypeCtorInfo_Struct type are commented out.
            % gen_init_maybe(gen_init_rtti_name(RttiTypeCtor), MaybeHashCons),
            % XXX this may need to change to call
            % gen_init_special_pred, if this is re-enabled.
            % gen_init_proc_id_from_univ(ModuleInfo, PrettyprinterProc)
        ]),
        rtti_entity_name_and_init_to_defn(Name, RttiId, Initializer,
            !GlobalData)
    ).

%-----------------------------------------------------------------------------%

:- pred gen_type_class_decl_defn(module_info::in, tc_decl::in,
    mlds_entity_name::in, rtti_id::in,
    ml_global_data::in, ml_global_data::out) is det.

gen_type_class_decl_defn(ModuleInfo, TCDecl, Name, RttiId, !GlobalData) :-
    TCDecl = tc_decl(TCId, Version, Supers),
    TCId = tc_id(TCName, TVarNames, MethodIds),
    TCName = tc_name(ModuleSymName, ClassName, Arity),
    module_info_get_name(ModuleInfo, ModuleName),
    TVarNamesRttiId = tc_rtti_id(TCName, type_class_id_var_names),
    (
        TVarNames = [],
        TVarNamesInitType = mlds_rtti_type(item_type(TVarNamesRttiId)),
        TVarNamesInitializer = gen_init_null_pointer(TVarNamesInitType)
    ;
        TVarNames = [_ | _],
        gen_tc_id_var_names(TVarNamesRttiId, TVarNames, !GlobalData),
        TVarNamesInitializer = gen_init_rtti_id(ModuleName, TVarNamesRttiId)
    ),
    MethodIdsRttiId = tc_rtti_id(TCName, type_class_id_method_ids),
    (
        MethodIds = [],
        MethodIdsInitType = mlds_rtti_type(item_type(MethodIdsRttiId)),
        MethodIdsInitializer = gen_init_null_pointer(MethodIdsInitType)
    ;
        MethodIds = [_ | _],
        gen_tc_id_method_ids(MethodIdsRttiId, TCName, MethodIds, !GlobalData),
        MethodIdsInitializer = gen_init_rtti_id(ModuleName, MethodIdsRttiId)
    ),
    TCIdRttiId = tc_rtti_id(TCName, type_class_id),
    ModuleSymNameStr = sym_name_to_string(ModuleSymName),
    list.length(TVarNames, NumTVars),
    list.length(MethodIds, NumMethods),
    TCIdInitializer = init_struct(mlds_rtti_type(item_type(TCIdRttiId)), [
        gen_init_string(ModuleSymNameStr),
        gen_init_string(ClassName),
        gen_init_int(Arity),
        gen_init_int(NumTVars),
        gen_init_int(NumMethods),
        TVarNamesInitializer,
        MethodIdsInitializer
    ]),
    rtti_id_and_init_to_defn(TCIdRttiId, TCIdInitializer, !GlobalData),
    (
        Supers = []
    ;
        Supers = [_ | _],
        list.map_foldl2(
            gen_tc_constraint(ModuleInfo, make_decl_super_id(TCName)),
            Supers, SuperRttiIds, counter.init(1), _, !GlobalData),
        SuperArrayRttiName = type_class_decl_supers,
        SuperArrayRttiId = tc_rtti_id(TCName, SuperArrayRttiName),
        ElementType = mlds_rtti_type(element_type(SuperArrayRttiId)),
        SuperArrayInitializer = gen_init_array(
            gen_init_cast_rtti_id(ElementType, ModuleName), SuperRttiIds),
        rtti_id_and_init_to_defn(SuperArrayRttiId, SuperArrayInitializer,
            !GlobalData)
    ),
    % XXX Is MethodIdsRttiId the right thing to take the type from?
    SupersInitType = mlds_rtti_type(item_type(MethodIdsRttiId)),
    SupersInitializer = gen_init_null_pointer(SupersInitType),
    list.length(Supers, NumSupers),
    Initializer = init_struct(mlds_rtti_type(item_type(RttiId)), [
        gen_init_rtti_id(ModuleName, TCIdRttiId),
        gen_init_int(Version),
        gen_init_int(NumSupers),
        SupersInitializer
    ]),
    rtti_entity_name_and_init_to_defn(Name, RttiId, Initializer, !GlobalData).

:- pred make_decl_super_id(tc_name::in, int::in, int::in, rtti_id::out) is det.

make_decl_super_id(TCName, TCNum, Arity, RttiId) :-
    TCRttiName = type_class_decl_super(TCNum, Arity),
    RttiId = tc_rtti_id(TCName, TCRttiName).

:- pred gen_tc_id_var_names(rtti_id::in, list(string)::in,
    ml_global_data::in, ml_global_data::out) is det.

gen_tc_id_var_names(RttiId, Names, !GlobalData) :-
    Initializer = gen_init_array(gen_init_string, Names),
    rtti_id_and_init_to_defn(RttiId, Initializer, !GlobalData).

:- pred gen_tc_id_method_ids(rtti_id::in, tc_name::in, list(tc_method_id)::in,
    ml_global_data::in, ml_global_data::out) is det.

gen_tc_id_method_ids(RttiId, TCName, MethodIds, !GlobalData) :-
    Initializer = gen_init_array(gen_tc_id_method_id(TCName), MethodIds),
    rtti_id_and_init_to_defn(RttiId, Initializer, !GlobalData).

:- func gen_tc_id_method_id(tc_name, tc_method_id) = mlds_initializer.

gen_tc_id_method_id(TCName, MethodId) = Initializer :-
    MethodId = tc_method_id(MethodName, MethodArity, PredOrFunc),
    RttiId = tc_rtti_id(TCName, type_class_id_method_ids),
    Initializer = init_struct(mlds_rtti_type(element_type(RttiId)), [
        gen_init_string(MethodName),
        gen_init_int(MethodArity),
        gen_init_pred_or_func(PredOrFunc)
    ]).

%-----------------------------------------------------------------------------%

:- pred gen_type_class_instance_defn(module_info::in, tc_instance::in,
    mlds_entity_name::in, rtti_id::in,
    ml_global_data::in, ml_global_data::out) is det.

gen_type_class_instance_defn(ModuleInfo, Instance, Name, RttiId,
        !GlobalData) :-
    Instance = tc_instance(TCName, Types, NumTypeVars,
        InstanceConstraints, _Methods),
    TCDeclRttiId = tc_rtti_id(TCName, type_class_decl),
    list.length(InstanceConstraints, NumInstanceConstraints),
    InstanceTypesTCRttiName = type_class_instance_tc_type_vector(Types),
    InstanceTypesRttiId = tc_rtti_id(TCName, InstanceTypesTCRttiName),
    InstanceConstrsTCRttiName = type_class_instance_constraints(Types),
    InstanceConstrsRttiId = tc_rtti_id(TCName, InstanceConstrsTCRttiName),
    module_info_get_name(ModuleInfo, ModuleName),

    TypeRttiDatas = list.map(maybe_pseudo_type_info_to_rtti_data, Types),
    gen_pseudo_type_info_array(ModuleInfo, TypeRttiDatas, TypesInitializer,
        !GlobalData),
    rtti_id_and_init_to_defn(InstanceTypesRttiId, TypesInitializer,
        !GlobalData),

    list.map_foldl2(
        gen_tc_constraint(ModuleInfo, make_instance_constr_id(TCName, Types)),
        InstanceConstraints, TCConstrIds, counter.init(1), _, !GlobalData),
    ElementType = mlds_rtti_type(element_type(InstanceConstrsRttiId)),
    InstanceConstrsInitializer = gen_init_array(
        gen_init_cast_rtti_id(ElementType, ModuleName), TCConstrIds),
    rtti_id_and_init_to_defn(InstanceConstrsRttiId, InstanceConstrsInitializer,
        !GlobalData),

    Initializer = init_struct(mlds_rtti_type(item_type(RttiId)), [
        gen_init_rtti_id(ModuleName, TCDeclRttiId),
        gen_init_int(NumTypeVars),
        gen_init_int(NumInstanceConstraints),
        gen_init_rtti_id(ModuleName, InstanceTypesRttiId),
        gen_init_rtti_id(ModuleName, InstanceConstrsRttiId)
    ]),
    rtti_entity_name_and_init_to_defn(Name, RttiId, Initializer, !GlobalData).

:- pred make_instance_constr_id(tc_name::in, list(tc_type)::in,
    int::in, int::in, rtti_id::out) is det.

make_instance_constr_id(TCName, Types, TCNum, Arity, RttiId) :-
    RttiName = type_class_instance_constraint(Types, TCNum, Arity),
    RttiId = tc_rtti_id(TCName, RttiName).

%-----------------------------------------------------------------------------%

:- pred gen_type_info_defn(module_info::in, rtti_type_info::in,
    mlds_entity_name::in, rtti_id::in,
    ml_global_data::in, ml_global_data::out) is det.

gen_type_info_defn(ModuleInfo, RttiTypeInfo, Name, RttiId, !GlobalData) :-
    (
        RttiTypeInfo = plain_arity_zero_type_info(_),
        unexpected($module, $pred, "plain_arity_zero_type_info")
    ;
        RttiTypeInfo = plain_type_info(RttiTypeCtor, ArgTypes),
        ml_global_data_get_pdup_rval_type_map(!.GlobalData, PDupRvalTypeMap),
        ( if map.search(PDupRvalTypeMap, RttiId, _) then
            % We have already generated the required global data structures.
            true
        else
            ArgRttiDatas = list.map(type_info_to_rtti_data, ArgTypes),
            RealRttiDatas = list.filter(real_rtti_data, ArgRttiDatas),
            list.foldl(add_rtti_data_to_mlds(ModuleInfo), RealRttiDatas,
                !GlobalData),
            module_info_get_name(ModuleInfo, ModuleName),
            Initializer = init_struct(mlds_rtti_type(item_type(RttiId)), [
                gen_init_rtti_name(ModuleName, RttiTypeCtor,
                    type_ctor_type_ctor_info),
                gen_init_cast_rtti_datas_array(mlds_type_info_type,
                    ModuleName, ArgRttiDatas)
            ]),
            rtti_entity_name_and_init_to_defn(Name, RttiId, Initializer,
                !GlobalData),

            MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
            DataAddr = data_addr(MLDS_ModuleName, mlds_rtti(RttiId)),
            Rval = ml_const(mlconst_data_addr(DataAddr)),
            Type = mlds_rtti_type(item_type(RttiId)),
            RvalType = ml_rval_and_type(Rval, Type),

            ml_global_data_add_pdup_rtti_id(RttiId, RvalType, !GlobalData)
        )
    ;
        RttiTypeInfo = var_arity_type_info(VarArityId, ArgTypes),
        ml_global_data_get_pdup_rval_type_map(!.GlobalData, PDupRvalTypeMap),
        ( if map.search(PDupRvalTypeMap, RttiId, _) then
            % We have already generated the required global data structures.
            true
        else
            ArgRttiDatas = list.map(type_info_to_rtti_data, ArgTypes),
            RealRttiDatas = list.filter(real_rtti_data, ArgRttiDatas),
            list.foldl(add_rtti_data_to_mlds(ModuleInfo), RealRttiDatas,
                !GlobalData),
            RttiTypeCtor = var_arity_id_to_rtti_type_ctor(VarArityId),
            module_info_get_name(ModuleInfo, ModuleName),
            module_info_get_globals(ModuleInfo, Globals),
            globals.get_target(Globals, TargetLang),

            InitRttiName = gen_init_rtti_name(ModuleName, RttiTypeCtor,
                type_ctor_type_ctor_info),
            InitCastRttiDatasArray = gen_init_cast_rtti_datas_array(
                mlds_type_info_type, ModuleName, ArgRttiDatas),
            ( if TargetLang = target_java then
                % For Java we need to omit the arity argument as the
                % TypeInfo_Struct class doesn't have a constructor that
                % supports it -- see java/runtime/TypeInfo_Struct.java for
                % details.
                %
                % NOTE: this needs to be kept consistent with
                %
                %   polymorphism.polymorphism_construct_type_info/10
                %   java/runtime/TypeInfo_Struct.java
                %
                % as well as the code for handling pseudo type-infos below.
                %
                InitializerArgs = [InitRttiName, InitCastRttiDatasArray]
            else
                InitializerArgs = [
                    InitRttiName,
                    gen_init_int(list.length(ArgTypes)),
                    InitCastRttiDatasArray]
            ),
            Initializer = init_struct(mlds_rtti_type(item_type(RttiId)),
                InitializerArgs),
            rtti_entity_name_and_init_to_defn(Name, RttiId, Initializer,
                !GlobalData),

            MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
            DataAddr = data_addr(MLDS_ModuleName, mlds_rtti(RttiId)),
            Rval = ml_const(mlconst_data_addr(DataAddr)),
            Type = mlds_rtti_type(item_type(RttiId)),
            RvalType = ml_rval_and_type(Rval, Type),

            ml_global_data_add_pdup_rtti_id(RttiId, RvalType, !GlobalData)
        )
    ).

:- pred gen_pseudo_type_info_defn(module_info::in, rtti_pseudo_type_info::in,
    mlds_entity_name::in, rtti_id::in,
    ml_global_data::in, ml_global_data::out) is det.

gen_pseudo_type_info_defn(ModuleInfo, RttiPseudoTypeInfo, Name, RttiId,
        !GlobalData) :-
    (
        RttiPseudoTypeInfo = plain_arity_zero_pseudo_type_info(_),
        unexpected($module, $pred, "plain_arity_zero_pseudo_type_info")
    ;
        RttiPseudoTypeInfo = plain_pseudo_type_info(RttiTypeCtor, ArgTypes),
        ml_global_data_get_pdup_rval_type_map(!.GlobalData, PDupRvalTypeMap),
        ( if map.search(PDupRvalTypeMap, RttiId, _) then
            % We have already generated the required global data structures.
            true
        else
            ArgRttiDatas = list.map(maybe_pseudo_type_info_to_rtti_data,
                ArgTypes),
            RealRttiDatas = list.filter(real_rtti_data, ArgRttiDatas),
            list.foldl(add_rtti_data_to_mlds(ModuleInfo), RealRttiDatas,
                !GlobalData),
            module_info_get_name(ModuleInfo, ModuleName),
            Initializer = init_struct(mlds_rtti_type(item_type(RttiId)), [
                gen_init_rtti_name(ModuleName, RttiTypeCtor,
                    type_ctor_type_ctor_info),
                gen_init_cast_rtti_datas_array(mlds_pseudo_type_info_type,
                    ModuleName, ArgRttiDatas)
            ]),
            rtti_entity_name_and_init_to_defn(Name, RttiId, Initializer,
                !GlobalData),

            MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
            DataAddr = data_addr(MLDS_ModuleName, mlds_rtti(RttiId)),
            Rval = ml_const(mlconst_data_addr(DataAddr)),
            Type = mlds_rtti_type(item_type(RttiId)),
            RvalType = ml_rval_and_type(Rval, Type),

            ml_global_data_add_pdup_rtti_id(RttiId, RvalType, !GlobalData)
        )
    ;
        RttiPseudoTypeInfo = var_arity_pseudo_type_info(VarArityId, ArgTypes),
        ml_global_data_get_pdup_rval_type_map(!.GlobalData, PDupRvalTypeMap),
        ( if map.search(PDupRvalTypeMap, RttiId, _) then
            % We have already generated the required global data structures.
            true
        else
            ArgRttiDatas = list.map(maybe_pseudo_type_info_to_rtti_data,
                ArgTypes),
            RealRttiDatas = list.filter(real_rtti_data, ArgRttiDatas),
            list.foldl(add_rtti_data_to_mlds(ModuleInfo), RealRttiDatas,
                !GlobalData),
            RttiTypeCtor = var_arity_id_to_rtti_type_ctor(VarArityId),
            module_info_get_name(ModuleInfo, ModuleName),
            module_info_get_globals(ModuleInfo, Globals),
            globals.get_target(Globals, TargetLang),

            InitRttiName = gen_init_rtti_name(ModuleName, RttiTypeCtor,
                type_ctor_type_ctor_info),
            InitCastRttiDatasArray = gen_init_cast_rtti_datas_array(
                mlds_pseudo_type_info_type, ModuleName, ArgRttiDatas),
            ( if TargetLang = target_java then
                % For Java we need to omit the arity argument as the
                % TypeInfo_Struct class doesn't have a constructor that
                % supports it. The TypeInfo_Struct class is used to represent
                % pseudo type-infos with the Java backend.
                % (See java/runtime/PseudoTypeInfo.java for details.)
                InitializerArgs = [InitRttiName, InitCastRttiDatasArray]
            else
                InitializerArgs = [
                    InitRttiName,
                    gen_init_int(list.length(ArgTypes)),
                    InitCastRttiDatasArray
                ]
            ),
            Initializer = init_struct(mlds_rtti_type(item_type(RttiId)),
                InitializerArgs),
            rtti_entity_name_and_init_to_defn(Name, RttiId, Initializer,
                !GlobalData),

            MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
            DataAddr = data_addr(MLDS_ModuleName, mlds_rtti(RttiId)),
            Rval = ml_const(mlconst_data_addr(DataAddr)),
            Type = mlds_rtti_type(item_type(RttiId)),
            RvalType = ml_rval_and_type(Rval, Type),

            ml_global_data_add_pdup_rtti_id(RttiId, RvalType, !GlobalData)
        )
    ;
        RttiPseudoTypeInfo = type_var(_),
        unexpected($module, $pred, "type_var")
    ).

%-----------------------------------------------------------------------------%

:- pred gen_functors_layout_info(module_info::in, rtti_type_ctor::in,
    type_ctor_details::in, mlds_initializer::out, mlds_initializer::out,
    mlds_initializer::out, ml_global_data::in, ml_global_data::out) is det.

gen_functors_layout_info(ModuleInfo, RttiTypeCtor, TypeCtorDetails,
        FunctorInitializer, LayoutInitializer, NumberMapInitializer,
        !GlobalData) :-
    module_info_get_name(ModuleInfo, ModuleName),
    (
        TypeCtorDetails = tcd_enum(_, EnumFunctors, EnumByValue, EnumByName,
            _IsDummy, FunctorNumberMap),
        list.foldl(gen_enum_functor_desc(ModuleInfo, RttiTypeCtor),
            EnumFunctors, !GlobalData),
        gen_enum_value_ordered_table(ModuleInfo, RttiTypeCtor,
            EnumByValue, !GlobalData),
        gen_enum_name_ordered_table(ModuleInfo, RttiTypeCtor,
            EnumByName, !GlobalData),
        gen_functor_number_map(RttiTypeCtor, FunctorNumberMap, !GlobalData),
        LayoutInitializer = gen_init_rtti_name(ModuleName, RttiTypeCtor,
            type_ctor_enum_value_ordered_table),
        FunctorInitializer = gen_init_rtti_name(ModuleName, RttiTypeCtor,
            type_ctor_enum_name_ordered_table),
        NumberMapInitializer = gen_init_rtti_name(ModuleName, RttiTypeCtor,
            type_ctor_functor_number_map)
    ;
        TypeCtorDetails = tcd_foreign_enum(ForeignEnumLang, _,
            ForeignEnumFunctors, ForeignEnumByOrdinal, ForeignEnumByName,
            FunctorNumberMap),
        list.foldl(
            gen_foreign_enum_functor_desc(ModuleInfo, ForeignEnumLang,
                RttiTypeCtor),
            ForeignEnumFunctors, !GlobalData),
        gen_foreign_enum_ordinal_ordered_table(ModuleInfo, RttiTypeCtor,
            ForeignEnumByOrdinal, !GlobalData),
        gen_foreign_enum_name_ordered_table(ModuleInfo, RttiTypeCtor,
            ForeignEnumByName, !GlobalData),
        gen_functor_number_map(RttiTypeCtor, FunctorNumberMap, !GlobalData),
        LayoutInitializer = gen_init_rtti_name(ModuleName, RttiTypeCtor,
            type_ctor_foreign_enum_ordinal_ordered_table),
        FunctorInitializer = gen_init_rtti_name(ModuleName, RttiTypeCtor,
            type_ctor_foreign_enum_name_ordered_table),
        NumberMapInitializer = gen_init_rtti_name(ModuleName, RttiTypeCtor,
            type_ctor_functor_number_map)
    ;
        TypeCtorDetails = tcd_du(_, DuFunctors, DuByPtag, DuByName,
            FunctorNumberMap),
        list.foldl(gen_du_functor_desc(ModuleInfo, RttiTypeCtor), DuFunctors,
            !GlobalData),
        gen_du_ptag_ordered_table(ModuleInfo, RttiTypeCtor,
            DuByPtag, !GlobalData),
        gen_du_name_ordered_table(ModuleInfo, RttiTypeCtor,
            DuByName, !GlobalData),
        gen_functor_number_map(RttiTypeCtor, FunctorNumberMap, !GlobalData),
        LayoutInitializer = gen_init_rtti_name(ModuleName, RttiTypeCtor,
            type_ctor_du_ptag_ordered_table),
        FunctorInitializer = gen_init_rtti_name(ModuleName, RttiTypeCtor,
            type_ctor_du_name_ordered_table),
        NumberMapInitializer = gen_init_rtti_name(ModuleName, RttiTypeCtor,
            type_ctor_functor_number_map)
    ;
        TypeCtorDetails = tcd_reserved(_, MaybeResFunctors, ResFunctors,
            DuByPtag, MaybeResByName, FunctorNumberMap),
        list.foldl(gen_maybe_res_functor_desc(ModuleInfo, RttiTypeCtor),
            MaybeResFunctors, !GlobalData),
        gen_maybe_res_value_ordered_table(ModuleInfo, RttiTypeCtor,
            ResFunctors, DuByPtag, !GlobalData),
        gen_maybe_res_name_ordered_table(ModuleInfo, RttiTypeCtor,
            MaybeResByName, !GlobalData),
        gen_functor_number_map(RttiTypeCtor, FunctorNumberMap, !GlobalData),
        LayoutInitializer = gen_init_rtti_name(ModuleName, RttiTypeCtor,
            type_ctor_res_value_ordered_table),
        FunctorInitializer = gen_init_rtti_name(ModuleName, RttiTypeCtor,
            type_ctor_res_name_ordered_table),
        NumberMapInitializer = gen_init_rtti_name(ModuleName, RttiTypeCtor,
            type_ctor_functor_number_map)
    ;
        TypeCtorDetails = tcd_notag(_, NotagFunctor),
        gen_functor_number_map(RttiTypeCtor, [0], !GlobalData),
        LayoutInitializer = gen_init_rtti_name(ModuleName, RttiTypeCtor,
            type_ctor_notag_functor_desc),
        FunctorInitializer = gen_init_rtti_name(ModuleName, RttiTypeCtor,
            type_ctor_notag_functor_desc),
        NumberMapInitializer = gen_init_rtti_name(ModuleName, RttiTypeCtor,
            type_ctor_functor_number_map),
        gen_notag_functor_desc(ModuleInfo, RttiTypeCtor, NotagFunctor,
            !GlobalData)
    ;
        TypeCtorDetails = tcd_eqv(EqvType),
        TypeRttiData = maybe_pseudo_type_info_to_rtti_data(EqvType),
        gen_pseudo_type_info(ModuleInfo, TypeRttiData, LayoutInitializer,
            !GlobalData),
        % The type is a lie, but a safe one.
        FunctorInitializer = gen_init_null_pointer(mlds_generic_type),
        NumberMapInitializer = gen_init_null_pointer(mlds_generic_type)
    ;
        ( TypeCtorDetails = tcd_builtin(_)
        ; TypeCtorDetails = tcd_impl_artifact(_)
        ; TypeCtorDetails = tcd_foreign(_)
        ),
        LayoutInitializer = gen_init_null_pointer(mlds_generic_type),
        FunctorInitializer = gen_init_null_pointer(mlds_generic_type),
        NumberMapInitializer = gen_init_null_pointer(mlds_generic_type)
    ).

%-----------------------------------------------------------------------------%

:- pred gen_enum_functor_desc(module_info::in, rtti_type_ctor::in,
    enum_functor::in, ml_global_data::in, ml_global_data::out) is det.

gen_enum_functor_desc(_ModuleInfo, RttiTypeCtor, EnumFunctor, !GlobalData) :-
    EnumFunctor = enum_functor(FunctorName, Ordinal),
    RttiName = type_ctor_enum_functor_desc(Ordinal),
    RttiId = ctor_rtti_id(RttiTypeCtor, RttiName),
    Initializer = init_struct(mlds_rtti_type(item_type(RttiId)), [
        gen_init_string(FunctorName),
        gen_init_int(Ordinal)
    ]),
    rtti_id_and_init_to_defn(RttiId, Initializer, !GlobalData).

:- pred gen_foreign_enum_functor_desc(module_info::in, foreign_language::in,
    rtti_type_ctor::in, foreign_enum_functor::in,
    ml_global_data::in, ml_global_data::out) is det.

gen_foreign_enum_functor_desc(_ModuleInfo, Lang, RttiTypeCtor,
        ForeignEnumFunctor, !GlobalData) :-
    ForeignEnumFunctor = foreign_enum_functor(FunctorName, Ordinal, Value),
    RttiName = type_ctor_foreign_enum_functor_desc(Ordinal),
    RttiId = ctor_rtti_id(RttiTypeCtor, RttiName),
    Initializer = init_struct(mlds_rtti_type(item_type(RttiId)), [
        gen_init_string(FunctorName),
        gen_init_int(Ordinal),
        gen_init_foreign(Lang, Value)
    ]),
    rtti_id_and_init_to_defn(RttiId, Initializer, !GlobalData).

:- pred gen_notag_functor_desc(module_info::in, rtti_type_ctor::in,
    notag_functor::in, ml_global_data::in, ml_global_data::out) is det.

gen_notag_functor_desc(ModuleInfo, RttiTypeCtor, NotagFunctorDesc,
        !GlobalData) :-
    NotagFunctorDesc = notag_functor(FunctorName, ArgType, MaybeArgName,
        FunctorSubtypeInfo),
    ArgTypeRttiData = maybe_pseudo_type_info_to_rtti_data(ArgType),
    gen_pseudo_type_info(ModuleInfo, ArgTypeRttiData, PTIInitializer,
        !GlobalData),
    RttiName = type_ctor_notag_functor_desc,
    RttiId = ctor_rtti_id(RttiTypeCtor, RttiName),
    Initializer = init_struct(mlds_rtti_type(item_type(RttiId)), [
        gen_init_string(FunctorName),
        PTIInitializer,
        gen_init_maybe(ml_string_type, gen_init_string, MaybeArgName),
        gen_init_functor_subtype_info(FunctorSubtypeInfo)
    ]),
    rtti_id_and_init_to_defn(RttiId, Initializer, !GlobalData).

:- pred gen_du_functor_desc(module_info::in, rtti_type_ctor::in,
    du_functor::in, ml_global_data::in, ml_global_data::out) is det.

gen_du_functor_desc(ModuleInfo, RttiTypeCtor, DuFunctor, !GlobalData) :-
    DuFunctor = du_functor(FunctorName, Arity, Ordinal, Rep, ArgInfos,
        MaybeExistInfo, FunctorSubtypeInfo),
    ArgTypes = list.map(du_arg_info_type, ArgInfos),
    MaybeArgNames = list.map(du_arg_info_name, ArgInfos),
    HaveArgNames = (if list.member(yes(_), MaybeArgNames) then yes else no),
    ContainsVarBitVector = compute_contains_var_bit_vector(ArgTypes),
    module_info_get_name(ModuleInfo, ModuleName),
    (
        ArgInfos = [_ | _],
        gen_field_types(ModuleInfo, RttiTypeCtor, Ordinal, ArgTypes,
            !GlobalData),
        ArgTypeInitializer = gen_init_rtti_name(ModuleName, RttiTypeCtor,
            type_ctor_field_types(Ordinal))
    ;
        ArgInfos = [],
        ArgTypeInitializer = gen_init_null_pointer(
            mlds_rtti_type(item_type(
                ctor_rtti_id(RttiTypeCtor, type_ctor_field_types(0)))))
    ),
    (
        HaveArgNames = yes,
        gen_field_names(ModuleInfo, RttiTypeCtor, Ordinal,
            MaybeArgNames, !GlobalData),
        ArgNameInitializer = gen_init_rtti_name(ModuleName, RttiTypeCtor,
            type_ctor_field_names(Ordinal))
    ;
        HaveArgNames = no,
        ArgNameInitializer = gen_init_null_pointer(
            mlds_rtti_type(item_type(
                ctor_rtti_id(RttiTypeCtor, type_ctor_field_names(0)))))
    ),
    gen_field_locns(ModuleInfo, RttiTypeCtor, Ordinal, ArgInfos, HaveArgLocns,
        !GlobalData),
    (
        HaveArgLocns = yes,
        ArgLocnsInitializer = gen_init_rtti_name(ModuleName, RttiTypeCtor,
            type_ctor_field_locns(Ordinal))
    ;
        HaveArgLocns = no,
        ArgLocnsInitializer = gen_init_null_pointer(
            mlds_rtti_type(item_type(
                ctor_rtti_id(RttiTypeCtor, type_ctor_field_locns(0)))))
    ),
    (
        MaybeExistInfo = yes(ExistInfo),
        gen_exist_info(ModuleInfo, RttiTypeCtor, Ordinal, ExistInfo,
            !GlobalData),
        ExistInfoInitializer = gen_init_rtti_name(ModuleName, RttiTypeCtor,
            type_ctor_exist_info(Ordinal))
    ;
        MaybeExistInfo = no,
        ExistInfoInitializer = gen_init_null_pointer(
            mlds_rtti_type(item_type(
                ctor_rtti_id(RttiTypeCtor, type_ctor_exist_info(0)))))
    ),
    (
        Rep = du_ll_rep(Ptag, SectagAndLocn)
    ;
        Rep = du_hl_rep(_),
        unexpected($module, $pred, "du_hl_rep")
    ),
    (
        SectagAndLocn = sectag_locn_none,
        Locn = sectag_none,
        Stag = -1
    ;
        SectagAndLocn = sectag_locn_none_direct_arg,
        Locn = sectag_none_direct_arg,
        Stag = -1
    ;
        SectagAndLocn = sectag_locn_local(Stag),
        Locn = sectag_local
    ;
        SectagAndLocn = sectag_locn_remote(Stag),
        Locn = sectag_remote
    ),
    RttiName = type_ctor_du_functor_desc(Ordinal),
    RttiId = ctor_rtti_id(RttiTypeCtor, RttiName),
    Initializer = init_struct(mlds_rtti_type(item_type(RttiId)), [
        gen_init_string(FunctorName),
        gen_init_int(Arity),
        gen_init_int(ContainsVarBitVector),
        gen_init_sectag_locn(Locn),
        gen_init_int(Ptag),
        gen_init_int(Stag),
        gen_init_int(Ordinal),
        ArgTypeInitializer,
        ArgNameInitializer,
        ArgLocnsInitializer,
        ExistInfoInitializer,
        gen_init_functor_subtype_info(FunctorSubtypeInfo)
    ]),
    rtti_id_and_init_to_defn(RttiId, Initializer, !GlobalData).

:- pred gen_res_addr_functor_desc(module_info::in, rtti_type_ctor::in,
    reserved_functor::in, ml_global_data::in, ml_global_data::out) is det.

gen_res_addr_functor_desc(ModuleInfo, RttiTypeCtor, ResFunctor, !GlobalData) :-
    ResFunctor = reserved_functor(FunctorName, Ordinal, ReservedAddress),
    RttiName = type_ctor_res_functor_desc(Ordinal),
    RttiId = ctor_rtti_id(RttiTypeCtor, RttiName),
    Initializer = init_struct(mlds_rtti_type(item_type(RttiId)), [
        gen_init_string(FunctorName),
        gen_init_int(Ordinal),
        gen_init_reserved_address(ModuleInfo, ReservedAddress)
    ]),
    rtti_id_and_init_to_defn(RttiId, Initializer, !GlobalData).

:- pred gen_maybe_res_functor_desc(module_info::in, rtti_type_ctor::in,
    maybe_reserved_functor::in,
    ml_global_data::in, ml_global_data::out) is det.

gen_maybe_res_functor_desc(ModuleInfo, RttiTypeCtor, MaybeResFunctor,
        !GlobalData) :-
    (
        MaybeResFunctor = res_func(ResFunctor),
        gen_res_addr_functor_desc(ModuleInfo, RttiTypeCtor, ResFunctor,
            !GlobalData)
    ;
        MaybeResFunctor = du_func(DuFunctor),
        gen_du_functor_desc(ModuleInfo, RttiTypeCtor, DuFunctor, !GlobalData)
    ).

%-----------------------------------------------------------------------------%

:- func gen_init_exist_locn(rtti_type_ctor, exist_typeinfo_locn) =
    mlds_initializer.

gen_init_exist_locn(RttiTypeCtor, ExistTypeInfoLocn) = Initializer :-
    (
        ExistTypeInfoLocn = typeinfo_in_tci(SlotInCell, SlotInTci)
    ;
        ExistTypeInfoLocn = plain_typeinfo(SlotInCell),
        SlotInTci = -1
    ),
    RttiId = ctor_rtti_id(RttiTypeCtor, type_ctor_exist_locn),
    Initializer = init_struct(mlds_rtti_type(item_type(RttiId)), [
        gen_init_int(SlotInCell),
        gen_init_int(SlotInTci)
    ]).

:- pred gen_exist_locns_array(module_info::in, rtti_type_ctor::in, int::in,
    list(exist_typeinfo_locn)::in, ml_global_data::in, ml_global_data::out)
    is det.

gen_exist_locns_array(_ModuleInfo, RttiTypeCtor, Ordinal, Locns,
        !GlobalData) :-
    Initializer = gen_init_array(gen_init_exist_locn(RttiTypeCtor), Locns),
    RttiName = type_ctor_exist_locns(Ordinal),
    rtti_name_and_init_to_defn(RttiTypeCtor, RttiName, Initializer,
        !GlobalData).

:- pred gen_tc_constraint(module_info::in,
    pred(int, int, rtti_id)::in(pred(in, in, out) is det),
    tc_constraint::in, rtti_id::out, counter::in, counter::out,
    ml_global_data::in, ml_global_data::out) is det.

gen_tc_constraint(ModuleInfo, MakeRttiId, Constraint, RttiId, !Counter,
        !GlobalData) :-
    Constraint = tc_constraint(TCName, Types),
    list.length(Types, Arity),
    counter.allocate(TCNum, !Counter),
    MakeRttiId(TCNum, Arity, RttiId),
    TCDeclRttiName = type_class_decl,
    module_info_get_name(ModuleInfo, ModuleName),
    TypeRttiDatas = list.map(maybe_pseudo_type_info_to_rtti_data, Types),
    gen_pseudo_type_info_array(ModuleInfo, TypeRttiDatas, PTIInitializers,
        !GlobalData),
    Initializer = init_struct(mlds_rtti_type(item_type(RttiId)), [
        gen_init_tc_rtti_name(ModuleName, TCName, TCDeclRttiName),
        PTIInitializers
    ]),
    rtti_id_and_init_to_defn(RttiId, Initializer, !GlobalData).

:- pred make_exist_tc_constr_id(rtti_type_ctor::in, int::in, int::in, int::in,
    rtti_id::out) is det.

make_exist_tc_constr_id(RttiTypeCtor, Ordinal, TCNum, Arity, RttiId) :-
    RttiName = type_ctor_exist_tc_constr(Ordinal, TCNum, Arity),
    RttiId = ctor_rtti_id(RttiTypeCtor, RttiName).

:- pred gen_exist_info(module_info::in, rtti_type_ctor::in, int::in,
    exist_info::in, ml_global_data::in, ml_global_data::out) is det.

gen_exist_info(ModuleInfo, RttiTypeCtor, Ordinal, ExistInfo, !GlobalData) :-
    ExistInfo = exist_info(Plain, InTci, Constraints, Locns),
    module_info_get_name(ModuleInfo, ModuleName),
    RttiName = type_ctor_exist_info(Ordinal),
    RttiId = ctor_rtti_id(RttiTypeCtor, RttiName),
    list.length(Constraints, Tci),
    (
        Constraints = [],
        ConstrInitializer = gen_init_null_pointer(
            mlds_rtti_type(item_type(ctor_rtti_id(RttiTypeCtor,
                type_ctor_exist_tc_constrs(Ordinal)))))
    ;
        Constraints = [_ | _],
        ConstrInitializer = gen_init_rtti_name(ModuleName, RttiTypeCtor,
            type_ctor_exist_tc_constrs(Ordinal)),
        list.map_foldl2(
            gen_tc_constraint(ModuleInfo,
                make_exist_tc_constr_id(RttiTypeCtor, Ordinal)),
            Constraints, TCConstrIds, counter.init(1), _, !GlobalData),
        TCConstrArrayRttiName = type_ctor_exist_tc_constrs(Ordinal),
        TCConstrArrayRttiId = ctor_rtti_id(RttiTypeCtor,
            TCConstrArrayRttiName),
        ElementType = mlds_rtti_type(element_type(TCConstrArrayRttiId)),
        TCConstrArrayInitializer = gen_init_array(
            gen_init_cast_rtti_id(ElementType, ModuleName), TCConstrIds),
        rtti_name_and_init_to_defn(RttiTypeCtor, TCConstrArrayRttiName,
            TCConstrArrayInitializer, !GlobalData)
    ),
    gen_exist_locns_array(ModuleInfo, RttiTypeCtor, Ordinal, Locns,
        !GlobalData),
    Initializer = init_struct(mlds_rtti_type(item_type(RttiId)), [
        gen_init_int(Plain),
        gen_init_int(InTci),
        gen_init_int(Tci),
        gen_init_rtti_name(ModuleName, RttiTypeCtor,
            type_ctor_exist_locns(Ordinal)),
        ConstrInitializer
    ]),
    rtti_id_and_init_to_defn(RttiId, Initializer, !GlobalData).

:- pred gen_field_types(module_info::in, rtti_type_ctor::in, int::in,
    list(rtti_maybe_pseudo_type_info_or_self)::in,
    ml_global_data::in, ml_global_data::out) is det.

gen_field_types(ModuleInfo, RttiTypeCtor, Ordinal, Types, !GlobalData) :-
    TypeRttiDatas = list.map(maybe_pseudo_type_info_or_self_to_rtti_data,
        Types),
    gen_pseudo_type_info_array(ModuleInfo, TypeRttiDatas, Initializer,
        !GlobalData),
    RttiName = type_ctor_field_types(Ordinal),
    rtti_name_and_init_to_defn(RttiTypeCtor, RttiName, Initializer,
        !GlobalData).

:- pred gen_field_names(module_info::in, rtti_type_ctor::in, int::in,
    list(maybe(string))::in, ml_global_data::in, ml_global_data::out) is det.

gen_field_names(_ModuleInfo, RttiTypeCtor, Ordinal, MaybeNames, !GlobalData) :-
    StrType = builtin_type(builtin_type_string),
    Initializer = gen_init_array(
        gen_init_maybe(
            mercury_type(StrType, ctor_cat_builtin(cat_builtin_string),
                non_foreign_type(StrType)),
            gen_init_string),
        MaybeNames),
    RttiName = type_ctor_field_names(Ordinal),
    rtti_name_and_init_to_defn(RttiTypeCtor, RttiName, Initializer,
        !GlobalData).

:- pred gen_field_locns(module_info::in, rtti_type_ctor::in, int::in,
    list(du_arg_info)::in, bool::out, ml_global_data::in, ml_global_data::out)
    is det.

gen_field_locns(_ModuleInfo, RttiTypeCtor, Ordinal, ArgInfos, HaveArgLocns,
        !GlobalData) :-
    ( if
        some [ArgInfo] (
            list.member(ArgInfo, ArgInfos),
            ArgInfo ^ du_arg_width \= full_word
        )
    then
        HaveArgLocns = yes,
        RttiName = type_ctor_field_locns(Ordinal),
        RttiId = ctor_rtti_id(RttiTypeCtor, RttiName),
        list.map_foldl(gen_field_locn(RttiId), ArgInfos, ArgLocnInitializers,
            -1, _Offset),
        Initializer = init_array(ArgLocnInitializers),
        rtti_id_and_init_to_defn(RttiId, Initializer, !GlobalData)
    else
        HaveArgLocns = no
    ).

:- pred gen_field_locn(rtti_id::in, du_arg_info::in, mlds_initializer::out,
    int::in, int::out) is det.

gen_field_locn(RttiId, ArgInfo, ArgLocnInitializer, PrevOffset,
        NextPrevOffset) :-
    ArgWidth = ArgInfo ^ du_arg_width,
    (
        ArgWidth = full_word,
        FieldOffset = PrevOffset + 1,
        Shift = 0,
        Bits = 0,
        NextPrevOffset = FieldOffset
    ;
        ArgWidth = double_word,
        FieldOffset = PrevOffset + 1,
        Shift = 0,
        Bits = -1,
        NextPrevOffset = FieldOffset + 1
    ;
        ArgWidth = partial_word_first(Mask),
        FieldOffset = PrevOffset + 1,
        Shift = 0,
        int.log2(Mask + 1, Bits),
        NextPrevOffset = FieldOffset
    ;
        ArgWidth = partial_word_shifted(Shift, Mask),
        FieldOffset = PrevOffset,
        int.log2(Mask + 1, Bits),
        NextPrevOffset = FieldOffset
    ),
    ArgLocnInitializer = init_struct(mlds_rtti_type(item_type(RttiId)), [
        gen_init_int(FieldOffset),
        gen_init_int(Shift),
        gen_init_int(Bits)
    ]).

%-----------------------------------------------------------------------------%

:- pred gen_enum_value_ordered_table(module_info::in, rtti_type_ctor::in,
    map(int, enum_functor)::in,
    ml_global_data::in, ml_global_data::out) is det.

gen_enum_value_ordered_table(ModuleInfo, RttiTypeCtor, EnumByValue,
        !GlobalData) :-
    map.values(EnumByValue, Functors),
    module_info_get_name(ModuleInfo, ModuleName),
    FunctorRttiNames = list.map(enum_functor_rtti_name, Functors),
    Initializer = gen_init_rtti_names_array(ModuleName, RttiTypeCtor,
        FunctorRttiNames),
    RttiName = type_ctor_enum_value_ordered_table,
    rtti_name_and_init_to_defn(RttiTypeCtor, RttiName, Initializer,
        !GlobalData).

:- pred gen_enum_name_ordered_table(module_info::in, rtti_type_ctor::in,
    map(string, enum_functor)::in,
    ml_global_data::in, ml_global_data::out) is det.

gen_enum_name_ordered_table(ModuleInfo, RttiTypeCtor, EnumByName,
        !GlobalData) :-
    map.values(EnumByName, Functors),
    module_info_get_name(ModuleInfo, ModuleName),
    FunctorRttiNames = list.map(enum_functor_rtti_name, Functors),
    Initializer = gen_init_rtti_names_array(ModuleName, RttiTypeCtor,
        FunctorRttiNames),
    RttiName = type_ctor_enum_name_ordered_table,
    rtti_name_and_init_to_defn(RttiTypeCtor, RttiName, Initializer,
        !GlobalData).

:- pred gen_foreign_enum_ordinal_ordered_table(module_info::in,
    rtti_type_ctor::in, map(int, foreign_enum_functor)::in,
    ml_global_data::in, ml_global_data::out) is det.

gen_foreign_enum_ordinal_ordered_table(ModuleInfo, RttiTypeCtor,
        ForeignEnumByOrdinal, !GlobalData) :-
    map.values(ForeignEnumByOrdinal, Functors),
    module_info_get_name(ModuleInfo, ModuleName),
    FunctorRttiNames = list.map(foreign_enum_functor_rtti_name, Functors),
    Initializer = gen_init_rtti_names_array(ModuleName, RttiTypeCtor,
        FunctorRttiNames),
    RttiName = type_ctor_foreign_enum_ordinal_ordered_table,
    rtti_name_and_init_to_defn(RttiTypeCtor, RttiName, Initializer,
        !GlobalData).

:- pred gen_foreign_enum_name_ordered_table(module_info::in,
    rtti_type_ctor::in, map(string, foreign_enum_functor)::in,
    ml_global_data::in, ml_global_data::out) is det.

gen_foreign_enum_name_ordered_table(ModuleInfo, RttiTypeCtor,
        ForeignEnumByName, !GlobalData) :-
    map.values(ForeignEnumByName, Functors),
    module_info_get_name(ModuleInfo, ModuleName),
    FunctorRttiNames = list.map(foreign_enum_functor_rtti_name, Functors),
    Initializer = gen_init_rtti_names_array(ModuleName, RttiTypeCtor,
        FunctorRttiNames),
    RttiName = type_ctor_foreign_enum_name_ordered_table,
    rtti_name_and_init_to_defn(RttiTypeCtor, RttiName, Initializer,
        !GlobalData).

:- pred gen_du_ptag_ordered_table(module_info::in, rtti_type_ctor::in,
    map(int, sectag_table)::in, ml_global_data::in, ml_global_data::out)
    is det.

gen_du_ptag_ordered_table(ModuleInfo, RttiTypeCtor, PtagMap, !GlobalData) :-
    module_info_get_name(ModuleInfo, ModuleName),
    map.to_assoc_list(PtagMap, PtagList),
    list.foldl(gen_du_stag_ordered_table(ModuleName, RttiTypeCtor), PtagList,
        !GlobalData),
    (
        PtagList = [],
        PtagInitPrefix = [],
        FirstPtag = 0
    ;
        PtagList = [FirstPtag - _ | _],
        ( if FirstPtag = 0 then
            PtagInitPrefix = [],
            FirstPtag = 0
        else if  FirstPtag = 1 then
            % Output a dummy ptag definition for the reserved tag first.
            RttiElemName = type_ctor_du_ptag_layout(0),
            RttiElemId = ctor_rtti_id(RttiTypeCtor, RttiElemName),
            PtagInitPrefix = [
                init_struct(mlds_rtti_type(item_type(RttiElemId)),
                [gen_init_int(0),
                gen_init_builtin_const("MR_SECTAG_VARIABLE"),
                gen_init_null_pointer(
                    mlds_rtti_type(item_type(
                        ctor_rtti_id(RttiTypeCtor,
                            type_ctor_du_stag_ordered_table(0)))))]
            )],
            FirstPtag = 1
        else
            unexpected($module, $pred, "bad ptag list")
        )
    ),
    PtagInitializers = gen_du_ptag_ordered_table_body(ModuleName, RttiTypeCtor,
        PtagList, FirstPtag),
    RttiName = type_ctor_du_ptag_ordered_table,
    Initializer = init_array(PtagInitPrefix ++ PtagInitializers),
    rtti_name_and_init_to_defn(RttiTypeCtor, RttiName, Initializer,
        !GlobalData).

:- func gen_du_ptag_ordered_table_body(module_name, rtti_type_ctor,
    assoc_list(int, sectag_table), int) = list(mlds_initializer).

gen_du_ptag_ordered_table_body(_, _, [], _) = [].
gen_du_ptag_ordered_table_body(ModuleName, RttiTypeCtor,
        [Ptag - SectagTable | PtagTail], CurPtag)
        = [Initializer | Initializers] :-
    expect(unify(Ptag, CurPtag), $module, $pred, "ptag mismatch"),
    SectagTable = sectag_table(SectagLocn, NumSharers, _SectagMap),
    RttiName = type_ctor_du_ptag_layout(Ptag),
    RttiId = ctor_rtti_id(RttiTypeCtor, RttiName),
    Initializer = init_struct(mlds_rtti_type(item_type(RttiId)), [
        gen_init_int(NumSharers),
        gen_init_sectag_locn(SectagLocn),
        gen_init_rtti_name(ModuleName, RttiTypeCtor,
            type_ctor_du_stag_ordered_table(Ptag))
    ]),
    Initializers = gen_du_ptag_ordered_table_body(ModuleName, RttiTypeCtor,
        PtagTail, CurPtag + 1).

:- pred gen_du_stag_ordered_table(module_name::in, rtti_type_ctor::in,
    pair(int, sectag_table)::in,
    ml_global_data::in, ml_global_data::out) is det.

gen_du_stag_ordered_table(ModuleName, RttiTypeCtor, Ptag - SectagTable,
        !GlobalData) :-
    SectagTable = sectag_table(_SectagLocn, _NumSharers, SectagMap),
    map.values(SectagMap, SectagFunctors),
    FunctorRttiNames = list.map(du_functor_rtti_name, SectagFunctors),
    Initializer = gen_init_rtti_names_array(ModuleName, RttiTypeCtor,
        FunctorRttiNames),
    RttiName = type_ctor_du_stag_ordered_table(Ptag),
    rtti_name_and_init_to_defn(RttiTypeCtor, RttiName, Initializer,
        !GlobalData).

:- pred gen_du_name_ordered_table(module_info::in, rtti_type_ctor::in,
    map(string, map(int, du_functor))::in,
    ml_global_data::in, ml_global_data::out) is det.

gen_du_name_ordered_table(ModuleInfo, RttiTypeCtor, NameArityMap,
        !GlobalData) :-
    map.values(NameArityMap, ArityMaps),
    list.map(map.values, ArityMaps, FunctorLists),
    list.condense(FunctorLists, Functors),
    module_info_get_name(ModuleInfo, ModuleName),
    FunctorRttiNames = list.map(du_functor_rtti_name, Functors),
    Initializer = gen_init_rtti_names_array(ModuleName, RttiTypeCtor,
        FunctorRttiNames),
    RttiName = type_ctor_du_name_ordered_table,
    rtti_name_and_init_to_defn(RttiTypeCtor, RttiName, Initializer,
        !GlobalData).

:- pred gen_maybe_res_value_ordered_table(module_info::in, rtti_type_ctor::in,
    list(reserved_functor)::in, map(int, sectag_table)::in,
    ml_global_data::in, ml_global_data::out) is det.

gen_maybe_res_value_ordered_table(ModuleInfo, RttiTypeCtor, ResFunctors,
        DuByPtag, !GlobalData) :-
    ResFunctorReps = list.map(res_addr_rep, ResFunctors),
    list.filter(res_addr_is_numeric, ResFunctorReps,
        NumericResFunctorReps, SymbolicResFunctorReps),
    list.length(NumericResFunctorReps, NumNumericResFunctorReps),
    list.length(SymbolicResFunctorReps, NumSymbolicResFunctorReps),
    module_info_get_name(ModuleInfo, ModuleName),
    gen_res_addr_functor_table(ModuleName, RttiTypeCtor, ResFunctors,
        !GlobalData),
    ( if NumSymbolicResFunctorReps = 0 then
        ResAddrInitializer = gen_init_null_pointer(mlds_generic_type)
    else
        gen_res_addrs_list(ModuleInfo, RttiTypeCtor,
            SymbolicResFunctorReps, !GlobalData),
        ResAddrInitializer = gen_init_rtti_name(ModuleName, RttiTypeCtor,
            type_ctor_res_addrs)
    ),
    gen_du_ptag_ordered_table(ModuleInfo, RttiTypeCtor, DuByPtag, !GlobalData),
    RttiName = type_ctor_res_value_ordered_table,
    RttiId = ctor_rtti_id(RttiTypeCtor, RttiName),
    Initializer = init_struct(mlds_rtti_type(item_type(RttiId)), [
        gen_init_int(NumNumericResFunctorReps),
        gen_init_int(NumSymbolicResFunctorReps),
        ResAddrInitializer,
        gen_init_rtti_name(ModuleName, RttiTypeCtor,
            type_ctor_res_addr_functors),
        gen_init_rtti_name(ModuleName, RttiTypeCtor,
            type_ctor_du_ptag_ordered_table)
    ]),
    rtti_id_and_init_to_defn(RttiId, Initializer, !GlobalData).

:- pred gen_res_addr_functor_table(module_name::in, rtti_type_ctor::in,
    list(reserved_functor)::in,
    ml_global_data::in, ml_global_data::out) is det.

gen_res_addr_functor_table(ModuleName, RttiTypeCtor, ResFunctors,
        !GlobalData) :-
    FunctorRttiNames = list.map(res_functor_rtti_name, ResFunctors),
    Initializer = gen_init_rtti_names_array(ModuleName, RttiTypeCtor,
        FunctorRttiNames),
    RttiName = type_ctor_res_addr_functors,
    rtti_name_and_init_to_defn(RttiTypeCtor, RttiName, Initializer,
        !GlobalData).

:- pred gen_res_addrs_list(module_info::in, rtti_type_ctor::in,
    list(reserved_address)::in, ml_global_data::in, ml_global_data::out)
    is det.

gen_res_addrs_list(ModuleInfo, RttiTypeCtor, ResAddrs, !GlobalData) :-
    Initializer =
        gen_init_array(gen_init_reserved_address(ModuleInfo), ResAddrs),
    RttiName = type_ctor_res_addrs,
    rtti_name_and_init_to_defn(RttiTypeCtor, RttiName, Initializer,
        !GlobalData).

:- pred gen_maybe_res_name_ordered_table(module_info::in, rtti_type_ctor::in,
    map(string, map(int, maybe_reserved_functor))::in,
    ml_global_data::in, ml_global_data::out) is det.

gen_maybe_res_name_ordered_table(ModuleInfo, RttiTypeCtor, NameArityMap,
        !GlobalData) :-
    map.values(NameArityMap, ArityMaps),
    list.map(map.values, ArityMaps, FunctorLists),
    list.condense(FunctorLists, Functors),
    module_info_get_name(ModuleInfo, ModuleName),
    Initializer = gen_init_array(
        gen_maybe_res_name_ordered_table_element(ModuleName, RttiTypeCtor),
        Functors),
    RttiName = type_ctor_res_name_ordered_table,
    rtti_name_and_init_to_defn(RttiTypeCtor, RttiName, Initializer,
        !GlobalData).

:- func gen_maybe_res_name_ordered_table_element(module_name, rtti_type_ctor,
    maybe_reserved_functor) = mlds_initializer.

gen_maybe_res_name_ordered_table_element(ModuleName, RttiTypeCtor,
        MaybeResFunctor) = Initializer :-
    RttiName = type_ctor_maybe_res_addr_functor_desc,
    RttiId = ctor_rtti_id(RttiTypeCtor, RttiName),
    Type = mlds_rtti_type(item_type(RttiId)),
    (
        MaybeResFunctor = res_func(ResFunctor),
        Name = ResFunctor ^ res_name,
        Initializer = init_struct(Type, [
            gen_init_string(Name),
            gen_init_int(0),    % arity=0
            gen_init_bool(yes), % is_reserved = true
            gen_init_rtti_name(ModuleName, RttiTypeCtor,
                maybe_res_functor_rtti_name(MaybeResFunctor))
        ])
    ;
        MaybeResFunctor = du_func(DuFunctor),
        Name = DuFunctor ^ du_name,
        Initializer = init_struct(Type, [
            gen_init_string(Name),
            gen_init_int(DuFunctor ^ du_orig_arity),
            gen_init_bool(no), % is_reserved = false
            gen_init_rtti_name(ModuleName, RttiTypeCtor,
                maybe_res_functor_rtti_name(MaybeResFunctor))
        ])
    ).

:- pred gen_functor_number_map(rtti_type_ctor::in, list(int)::in,
    ml_global_data::in, ml_global_data::out) is det.

gen_functor_number_map(RttiTypeCtor, FunctorNumberMap, !GlobalData) :-
    Initializer = gen_init_array(gen_init_int, FunctorNumberMap),
    RttiName = type_ctor_functor_number_map,
    rtti_name_and_init_to_defn(RttiTypeCtor, RttiName, Initializer,
        !GlobalData).

%-----------------------------------------------------------------------------%

:- func gen_init_rtti_names_array(module_name, rtti_type_ctor,
    list(ctor_rtti_name)) = mlds_initializer.

gen_init_rtti_names_array(ModuleName, RttiTypeCtor, RttiNames) =
    gen_init_array(gen_init_rtti_name(ModuleName, RttiTypeCtor), RttiNames).

:- func gen_init_rtti_datas_array(module_name, list(rtti_data)) =
    mlds_initializer.

gen_init_rtti_datas_array(ModuleName, RttiDatas) =
    gen_init_array(gen_init_rtti_data(ModuleName), RttiDatas).

:- func gen_init_cast_rtti_datas_array(mlds_type, module_name,
    list(rtti_data)) = mlds_initializer.

gen_init_cast_rtti_datas_array(Type, ModuleName, RttiDatas) =
    gen_init_array(gen_init_cast_rtti_data(Type, ModuleName), RttiDatas).

    % Generate the MLDS initializer comprising the rtti_name
    % for a given rtti_data, converted to mlds_generic_type.
    %
:- func gen_init_cast_rtti_data(mlds_type, module_name, rtti_data) =
    mlds_initializer.

gen_init_cast_rtti_data(DestType, ModuleName, RttiData) = Initializer :-
    ( if
        RttiData = rtti_data_pseudo_type_info(type_var(VarNum))
    then
        % rtti_data_to_id/3 does not handle this case
        SrcType = mlds_native_int_type,
        Initializer = init_obj(ml_unop(gen_cast(SrcType, DestType),
            ml_const(mlconst_int(VarNum))))
    else if
        RttiData = rtti_data_base_typeclass_info(TCName, InstanceModuleName,
            InstanceString, _)
    then
        SrcType = mlds_rtti_type(item_type(tc_rtti_id(TCName,
            type_class_base_typeclass_info(InstanceModuleName,
                InstanceString)))),
        MLDS_ModuleName = mercury_module_name_to_mlds(InstanceModuleName),
        MLDS_DataName = mlds_rtti(tc_rtti_id(TCName,
            type_class_base_typeclass_info(
                InstanceModuleName, InstanceString))),
        DataAddr = data_addr(MLDS_ModuleName, MLDS_DataName),
        Rval = ml_const(mlconst_data_addr(DataAddr)),
        Initializer = init_obj(ml_unop(gen_cast(SrcType, DestType), Rval))
    else
        rtti_data_to_id(RttiData, RttiId),
        Initializer = gen_init_cast_rtti_id(DestType, ModuleName, RttiId)
    ).

    % Currently casts only store the destination type.
    %
:- func gen_cast(mlds_type, mlds_type) = mlds_unary_op.

gen_cast(_SrcType, DestType) = cast(DestType).

    % Generate the MLDS initializer comprising the rtti_name
    % for a given rtti_data.
    %
:- func gen_init_rtti_data(module_name, rtti_data) = mlds_initializer.

gen_init_rtti_data(ModuleName, RttiData) = Initializer :-
    rtti_data_to_id(RttiData, RttiId),
    Initializer = gen_init_rtti_id(ModuleName, RttiId).

    % Generate an MLDS initializer comprising just the rval
    % for a given rtti_id.
    %
:- func gen_init_rtti_id(module_name, rtti_id) = mlds_initializer.

gen_init_rtti_id(ModuleName, ctor_rtti_id(RttiTypeCtor, RttiName)) =
    gen_init_rtti_name(ModuleName, RttiTypeCtor, RttiName).
gen_init_rtti_id(ModuleName, tc_rtti_id(TCName, TCRttiName)) =
    gen_init_tc_rtti_name(ModuleName, TCName, TCRttiName).

    % Generate an MLDS initializer comprising just the rval
    % for a given rtti_name.
    %
:- func gen_init_rtti_name(module_name, rtti_type_ctor, ctor_rtti_name) =
    mlds_initializer.

gen_init_rtti_name(ModuleName, RttiTypeCtor, RttiName) =
    init_obj(gen_rtti_name(ModuleName, RttiTypeCtor, RttiName)).

    % Generate an MLDS initializer comprising just the rval
    % for a given tc_rtti_name.
    %
:- func gen_init_tc_rtti_name(module_name, tc_name, tc_rtti_name) =
    mlds_initializer.

gen_init_tc_rtti_name(ModuleName, TCName, TCRttiName) =
    init_obj(gen_tc_rtti_name(ModuleName, TCName, TCRttiName)).

    % Generate the MLDS initializer comprising the rtti_name
    % for a given rtti_name, converted to the given type.
    %
:- func gen_init_cast_rtti_id(mlds_type, module_name, rtti_id)
    = mlds_initializer.

gen_init_cast_rtti_id(DestType, ModuleName, RttiId) = Initializer :-
    SrcType = mlds_rtti_type(item_type(RttiId)),
    Initializer = init_obj(ml_unop(gen_cast(SrcType, DestType),
        gen_rtti_id(ModuleName, RttiId))).

    % Generate the MLDS rval for an rtti_id.
    %
:- func gen_rtti_id(module_name, rtti_id) = mlds_rval.

gen_rtti_id(ThisModuleName, ctor_rtti_id(RttiTypeCtor, RttiName)) =
    gen_rtti_name(ThisModuleName, RttiTypeCtor, RttiName).
gen_rtti_id(ThisModuleName, tc_rtti_id(TCName, TCRttiName)) =
    gen_tc_rtti_name(ThisModuleName, TCName, TCRttiName).

:- func gen_rtti_name(module_name, rtti_type_ctor, ctor_rtti_name)
    = mlds_rval.

gen_rtti_name(ThisModuleName, RttiTypeCtor0, RttiName) = Rval :-
    % Typeinfos and pseudo typeinfos are defined locally to each module.
    % Other kinds of RTTI data are defined in the module that defines
    % the type which they are for.
    ( if
        (
            RttiName = type_ctor_type_info(TypeInfo),
            ( TypeInfo = plain_type_info(_, _)
            ; TypeInfo = var_arity_type_info(_, _)
            )
        ;
            RttiName = type_ctor_pseudo_type_info(PseudoTypeInfo),
            ( PseudoTypeInfo = plain_pseudo_type_info(_, _)
            ; PseudoTypeInfo = var_arity_pseudo_type_info(_, _)
            )
        )
    then
        ModuleName = ThisModuleName,
        RttiTypeCtor = RttiTypeCtor0
    else
        RttiTypeCtor0 = rtti_type_ctor(RttiModuleName,
            RttiTypeName, RttiTypeArity),

        % Although the builtin types `int', `float', etc. are treated
        % as part of the `builtin' module, for historical reasons they
        % don't have any qualifiers at this point, so we need to add
        % the `builtin' qualifier now.
        ( if RttiModuleName = unqualified("") then
            ModuleName = mercury_public_builtin_module,
            RttiTypeCtor = rtti_type_ctor(RttiModuleName,
                RttiTypeName, RttiTypeArity)
        else
            ModuleName = RttiModuleName,
            RttiTypeCtor = RttiTypeCtor0
        )
    ),
    MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
    MLDS_DataName = mlds_rtti(ctor_rtti_id(RttiTypeCtor, RttiName)),
    DataAddr = data_addr(MLDS_ModuleName, MLDS_DataName),
    Rval = ml_const(mlconst_data_addr(DataAddr)).

:- func gen_tc_rtti_name(module_name, tc_name, tc_rtti_name) = mlds_rval.

gen_tc_rtti_name(_ThisModuleName, TCName, TCRttiName) = Rval :-
    (
        TCRttiName = type_class_base_typeclass_info(InstanceModuleName, _),
        MLDS_ModuleName = mercury_module_name_to_mlds(InstanceModuleName)
    ;
        TCRttiName = type_class_id,
        MLDS_ModuleName = mlds_module_name_from_tc_name(TCName)
    ;
        TCRttiName = type_class_decl,
        MLDS_ModuleName = mlds_module_name_from_tc_name(TCName)
    ;
        TCRttiName = type_class_decl_super(_, _),
        MLDS_ModuleName = mlds_module_name_from_tc_name(TCName)
    ;
        TCRttiName = type_class_decl_supers,
        MLDS_ModuleName = mlds_module_name_from_tc_name(TCName)
    ;
        TCRttiName = type_class_id_var_names,
        MLDS_ModuleName = mlds_module_name_from_tc_name(TCName)
    ;
        TCRttiName = type_class_id_method_ids,
        MLDS_ModuleName = mlds_module_name_from_tc_name(TCName)
    ;
        TCRttiName = type_class_instance(_Types),
        MLDS_ModuleName = mlds_module_name_from_tc_name(TCName)
    ;
        TCRttiName = type_class_instance_tc_type_vector(_Types),
        MLDS_ModuleName = mlds_module_name_from_tc_name(TCName)
    ;
        TCRttiName = type_class_instance_constraint(_Types, _, _),
        MLDS_ModuleName = mlds_module_name_from_tc_name(TCName)
    ;
        TCRttiName = type_class_instance_constraints(_Types),
        MLDS_ModuleName = mlds_module_name_from_tc_name(TCName)
    ;
        TCRttiName = type_class_instance_methods(_Types),
        MLDS_ModuleName = mlds_module_name_from_tc_name(TCName)
    ),
    MLDS_DataName = mlds_rtti(tc_rtti_id(TCName, TCRttiName)),
    DataAddr = data_addr(MLDS_ModuleName, MLDS_DataName),
    Rval = ml_const(mlconst_data_addr(DataAddr)).

:- func mlds_module_name_from_tc_name(tc_name) = mlds_module_name.

mlds_module_name_from_tc_name(TCName) = MLDS_ModuleName :-
    TCName = tc_name(ModuleName, _ClassName, _Arity),
    MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName).

%-----------------------------------------------------------------------------%

:- pred gen_pseudo_type_info(module_info::in, rtti_data::in,
    mlds_initializer::out, ml_global_data::in, ml_global_data::out) is det.

gen_pseudo_type_info(ModuleInfo, PTIRttiData, Initializer, !GlobalData) :-
    ( if real_rtti_data(PTIRttiData) then
        add_rtti_data_to_mlds(ModuleInfo, PTIRttiData, !GlobalData)
    else
        % Since PTIRttiData does not correspond to a global data definition,
        % we have nothing to do.
        true
    ),
    module_info_get_name(ModuleInfo, ModuleName),
    Initializer = gen_init_cast_rtti_data(mlds_pseudo_type_info_type,
        ModuleName, PTIRttiData).

:- pred gen_pseudo_type_info_array(module_info::in, list(rtti_data)::in,
    mlds_initializer::out, ml_global_data::in, ml_global_data::out) is det.

gen_pseudo_type_info_array(ModuleInfo, PTIRttiDatas, Initializer,
        !GlobalData) :-
    RealRttiDatas = list.filter(real_rtti_data, PTIRttiDatas),
    list.foldl(add_rtti_data_to_mlds(ModuleInfo), RealRttiDatas, !GlobalData),
    module_info_get_name(ModuleInfo, ModuleName),
    Initializer = gen_init_cast_rtti_datas_array(mlds_pseudo_type_info_type,
        ModuleName, PTIRttiDatas).

:- pred gen_pseudo_type_info_list(module_info::in, list(rtti_data)::in,
    list(mlds_initializer)::out,
    ml_global_data::in, ml_global_data::out) is det.

gen_pseudo_type_info_list(ModuleInfo, PTIRttiDatas, Initializers,
        !GlobalData) :-
    RealRttiDatas = list.filter(real_rtti_data, PTIRttiDatas),
    list.foldl(add_rtti_data_to_mlds(ModuleInfo), RealRttiDatas, !GlobalData),
    module_info_get_name(ModuleInfo, ModuleName),
    Initializers = list.map(
        gen_init_cast_rtti_data(mlds_pseudo_type_info_type, ModuleName),
        PTIRttiDatas).

%-----------------------------------------------------------------------------%

:- pred gen_init_method(module_info::in, int::in, rtti_proc_label::in,
    mlds_initializer::out, ml_global_data::in, ml_global_data::out) is det.

gen_init_method(ModuleInfo, NumExtra, RttiProcLabel, Initializer,
        !GlobalData) :-
    % We can't store the address of the typeclass method directly in the
    % base_typeclass_info; instead, we need to generate a wrapper function
    % that extracts the NumExtra parameters it needs from the typeclass_info,
    % and store the address of that wrapper function in the
    % base_typeclass_info.
    %
    % Note that this means there are two levels of wrappers: the wrapper that
    % we generate here calls the procedure introduced by check_typeclass.m,
    % and that in turn calls the user's procedure. Hopefully the Mercury
    % HLDS->HLDS inlining and/or the target code compiler will be able
    % to optimize this...
    %
    gen_wrapper_func_and_initializer(ModuleInfo, NumExtra, RttiProcLabel,
        typeclass_info_closure, Initializer, !GlobalData).

:- pred gen_init_special_pred(module_info::in, univ::in, mlds_initializer::out,
    ml_global_data::in, ml_global_data::out) is det.

gen_init_special_pred(ModuleInfo, RttiProcIdUniv, Initializer, !GlobalData) :-
    % We can't store the address of the special pred procedure directly in the
    % type_ctor_info because when the special pred is called by looking up
    % its address in the type_ctor_info it is always called with its arguments
    % boxed, but the generated special pred may operate on unboxed values,
    % hence we need to generate a wrapper function which unboxes the arguments
    % if necessary.
    det_univ_to_type(RttiProcIdUniv, RttiProcId),
    ( if RttiProcId ^ rpl_proc_arity = 0 then
        % If there are no arguments, then there's no unboxing to do,
        % so we don't need a wrapper. (This case can occur with
        % --no-special-preds, where the procedure will be
        % private_builtin.unused/0.)
        Initializer = gen_init_proc_id(ModuleInfo, RttiProcId)
    else
        NumExtra = 0,
        gen_wrapper_func_and_initializer(ModuleInfo, NumExtra, RttiProcId,
            special_pred_closure, Initializer, !GlobalData)
    ).

:- pred gen_wrapper_func_and_initializer(module_info::in, int::in,
    rtti_proc_label::in, closure_kind::in, mlds_initializer::out,
    ml_global_data::in, ml_global_data::out) is det.

gen_wrapper_func_and_initializer(ModuleInfo, NumExtra, RttiProcId,
        ClosureKind, Initializer, !GlobalData) :-
    some [!Info] (
        % We start off by creating a fresh MLGenInfo here, using the pred_id
        % and proc_id of the wrapped procedure. This requires considerable
        % care. We need to call ml_gen_info_bump_counters to ensure that
        % the function label allocated for the wrapper func does not overlap
        % with any function labels used when generating code for the wrapped
        % procedure.
        %
        % The empty const struct map is a lie, but a white lie; the RTTI
        % data cannot contain any type_info_const or typeclass_info_const
        % cons_ids.

        PredId = RttiProcId ^ rpl_pred_id,
        ProcId = RttiProcId ^ rpl_proc_id,
        module_info_proc_info(ModuleInfo, PredId, ProcId, ProcInfo),
        !:Info = ml_gen_info_init(ModuleInfo, map.init, PredId, ProcId,
            ProcInfo, !.GlobalData),
        ml_gen_info_bump_counters(!Info),

        % Now we can safely go ahead and generate the wrapper function.
        term.context_init(Context),
        ml_gen_closure_wrapper(PredId, ProcId, ClosureKind, NumExtra, Context,
            WrapperFuncRval, WrapperFuncType, !Info),
        ml_gen_info_get_closure_wrapper_defns(!.Info, ExtraDefns),
        ml_gen_info_get_global_data(!.Info, !:GlobalData),
        ml_global_data_add_maybe_nonflat_defns(ExtraDefns, !GlobalData),

        % The initializer for the wrapper is just the wrapper function's
        % address, converted to mlds_generic_type (by boxing).
        Initializer = init_obj(ml_unop(box(WrapperFuncType), WrapperFuncRval))
    ).

:- func gen_init_proc_id(module_info, rtti_proc_label) = mlds_initializer.

gen_init_proc_id(ModuleInfo, RttiProcId) = Initializer :-
    % Construct an rval for the address of this procedure
    % (this is similar to ml_gen_proc_addr_rval).
    ml_gen_pred_label_from_rtti(ModuleInfo, RttiProcId, PredLabel, PredModule),
    ProcId = RttiProcId ^ rpl_proc_id,
    QualifiedProcLabel = qual(PredModule, module_qual,
        mlds_proc_label(PredLabel, ProcId)),
    Params = ml_gen_proc_params_from_rtti(ModuleInfo, RttiProcId),
    Signature = mlds_get_func_signature(Params),
    ProcAddrRval = ml_const(mlconst_code_addr(
        code_addr_proc(QualifiedProcLabel, Signature))),

    % Convert the procedure address to a generic type. We need to use a
    % generic type because since the actual type for the procedure will
    % depend on how many type_info parameters it takes, which will depend
    % on the type's arity.
    ProcAddrArg = ml_unop(box(mlds_func_type(Params)), ProcAddrRval),
    Initializer = init_obj(ProcAddrArg).

:- func gen_init_proc_id_from_univ(module_info, univ) =
    mlds_initializer.

gen_init_proc_id_from_univ(ModuleInfo, ProcLabelUniv) = Initializer :-
    det_univ_to_type(ProcLabelUniv, ProcLabel),
    Initializer = gen_init_proc_id(ModuleInfo, ProcLabel).

    % Succeed iff the specified rtti_data is one that requires an
    % explicit mlds_defn to define it.
    %
:- pred real_rtti_data(rtti_data::in) is semidet.

real_rtti_data(RttiData) :-
    not (
        (
            RttiData = rtti_data_type_info(TypeInfo),
            TypeInfo = plain_arity_zero_type_info(_)
        ;
            RttiData = rtti_data_pseudo_type_info(PseudoTypeInfo),
            ( PseudoTypeInfo = plain_arity_zero_pseudo_type_info(_)
            ; PseudoTypeInfo = type_var(_)
            )
        )
    ).

%-----------------------------------------------------------------------------%
%
% Conversion functions for builtin enumeration types.
%
% This handles sectag_locn, functor_subtype_info and type_ctor_rep. The rvals
% generated are just named constants in the private_builtin module, which the
% Mercury runtime is expected to define.

:- func gen_init_pred_or_func(pred_or_func) = mlds_initializer.

gen_init_pred_or_func(PredOrFunc) = gen_init_builtin_const(Name) :-
    rtti.pred_or_func_to_string(PredOrFunc, Name).

:- func gen_init_sectag_locn(sectag_locn) = mlds_initializer.

gen_init_sectag_locn(Locn) = gen_init_builtin_const(Name) :-
    rtti.sectag_locn_to_string(Locn, Name).

:- func gen_init_functor_subtype_info(functor_subtype_info) = mlds_initializer.

gen_init_functor_subtype_info(FunctorSubtypeInfo) = Initializer :-
    rtti.functor_subtype_info_to_string(FunctorSubtypeInfo, Name),
    Initializer = gen_init_builtin_const(Name).

:- func gen_init_type_ctor_rep(type_ctor_data) = mlds_initializer.

gen_init_type_ctor_rep(TypeCtorData) = gen_init_builtin_const(Name) :-
    rtti.type_ctor_rep_to_string(TypeCtorData, Name).

%-----------------------------------------------------------------------------%
%
% Ordering RTTI definitions.
%

order_mlds_rtti_defns(Defns) = OrdDefns :-
    some [!Graph] (
        digraph.init(!:Graph),
        list.foldl2(add_rtti_defn_nodes, Defns, !Graph, map.init, NameMap),
        list.foldl(add_rtti_defn_arcs, Defns, !Graph),
        digraph.atsort(!.Graph, RevOrdSets)
    ),
    list.reverse(RevOrdSets, OrdSets),
    list.map(set.to_sorted_list, OrdSets, OrdLists),
    list.map(list.filter_map(map.search(NameMap)), OrdLists, OrdDefns).

:- pred add_rtti_defn_nodes(mlds_defn::in,
    digraph(mlds_data_name)::in, digraph(mlds_data_name)::out,
    map(mlds_data_name, mlds_defn)::in, map(mlds_data_name, mlds_defn)::out)
    is det.

add_rtti_defn_nodes(Defn, !Graph, !NameMap) :-
    Name = Defn ^ md_entity_name,
    (
        Name = entity_data(DataName),
        digraph.add_vertex(DataName, _, !Graph),
        map.det_insert(DataName, Defn, !NameMap)
    ;
        ( Name = entity_type(_, _)
        ; Name = entity_function(_, _, _, _)
        ; Name = entity_export(_)
        ),
        unexpected($module, $pred, "expected entity_data")
    ).

:- pred add_rtti_defn_arcs(mlds_defn::in,
    digraph(mlds_data_name)::in, digraph(mlds_data_name)::out) is det.

add_rtti_defn_arcs(Defn, !Graph) :-
    Defn = mlds_defn(EntityName, _, _, EntityDefn),
    ( if
        EntityName = entity_data(DefnDataName),
        EntityDefn = mlds_data(mlds_data_defn(Type, Initializer, _GCStmt)),
        Type = mlds_rtti_type(_)
    then
        add_rtti_defn_arcs_initializer(DefnDataName, Initializer, !Graph)
    else
        unexpected($module, $pred, "expected rtti entity_data")
    ).

:- pred add_rtti_defn_arcs_initializer(mlds_data_name::in,
    mlds_initializer::in,
    digraph(mlds_data_name)::in, digraph(mlds_data_name)::out) is det.

add_rtti_defn_arcs_initializer(DefnDataName, Initializer, !Graph) :-
    (
        Initializer = init_obj(Rval),
        add_rtti_defn_arcs_rval(DefnDataName, Rval, !Graph)
    ;
        ( Initializer = init_struct(_, Initializers)
        ; Initializer = init_array(Initializers)
        ),
        list.foldl(add_rtti_defn_arcs_initializer(DefnDataName), Initializers,
            !Graph)
    ;
        Initializer = no_initializer
    ).

:- pred add_rtti_defn_arcs_rval(mlds_data_name::in, mlds_rval::in,
    digraph(mlds_data_name)::in, digraph(mlds_data_name)::out) is det.

add_rtti_defn_arcs_rval(DefnDataName, Rval, !Graph) :-
    (
        Rval = ml_lval(Lval),
        add_rtti_defn_arcs_lval(DefnDataName, Lval, !Graph)
    ;
        Rval = ml_mkword(_Tag, RvalA),
        add_rtti_defn_arcs_rval(DefnDataName, RvalA, !Graph)
    ;
        Rval = ml_const(Const),
        add_rtti_defn_arcs_const(DefnDataName, Const, !Graph)
    ;
        Rval = ml_unop(_, RvalA),
        add_rtti_defn_arcs_rval(DefnDataName, RvalA, !Graph)
    ;
        Rval = ml_binop(_, RvalA, RvalB),
        add_rtti_defn_arcs_rval(DefnDataName, RvalA, !Graph),
        add_rtti_defn_arcs_rval(DefnDataName, RvalB, !Graph)
    ;
        Rval = ml_mem_addr(Lval),
        add_rtti_defn_arcs_lval(DefnDataName, Lval, !Graph)
    ;
        Rval = ml_scalar_common(_)
    ;
        Rval = ml_vector_common_row(_, RowRval),
        add_rtti_defn_arcs_rval(DefnDataName, RowRval, !Graph)
    ;
        Rval = ml_self(_)
    ).

:- pred add_rtti_defn_arcs_lval(mlds_data_name::in, mlds_lval::in,
    digraph(mlds_data_name)::in, digraph(mlds_data_name)::out) is det.

add_rtti_defn_arcs_lval(DefnDataName, Lval, !Graph) :-
    (
        Lval = ml_field(_, Rval, _, _, _),
        add_rtti_defn_arcs_rval(DefnDataName, Rval, !Graph)
    ;
        Lval = ml_mem_ref(Rval, _Type),
        add_rtti_defn_arcs_rval(DefnDataName, Rval, !Graph)
    ;
        Lval = ml_global_var_ref(env_var_ref(_))
    ;
        Lval = ml_var(_, _)
    ).

:- pred add_rtti_defn_arcs_const(mlds_data_name::in, mlds_rval_const::in,
    digraph(mlds_data_name)::in, digraph(mlds_data_name)::out) is det.

add_rtti_defn_arcs_const(DefnDataName, Const, !Graph) :-
    (
        Const = mlconst_data_addr(data_addr(_, DataName)),
        (
            DataName = mlds_rtti(_),
            digraph.add_vertices_and_edge(DefnDataName, DataName, !Graph)
        ;
            ( DataName = mlds_data_var(_)
            ; DataName = mlds_scalar_common_ref(_)
            ; DataName = mlds_module_layout
            ; DataName = mlds_proc_layout(_)
            ; DataName = mlds_internal_layout(_, _)
            ; DataName = mlds_tabling_ref(_, _)
            )
        )
    ;
        ( Const = mlconst_true
        ; Const = mlconst_false
        ; Const = mlconst_int(_)
        ; Const = mlconst_uint(_)
        ; Const = mlconst_enum(_, _)
        ; Const = mlconst_char(_)
        ; Const = mlconst_foreign(_, _, _)
        ; Const = mlconst_float(_)
        ; Const = mlconst_string(_)
        ; Const = mlconst_multi_string(_)
        ; Const = mlconst_named_const(_)
        ; Const = mlconst_code_addr(_)
        ; Const = mlconst_null(_)
        )
    ).

%-----------------------------------------------------------------------------%
:- end_module ml_backend.rtti_to_mlds.
%-----------------------------------------------------------------------------%
