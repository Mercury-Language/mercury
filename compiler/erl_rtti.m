%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2007, 2009-2012 The University of Melbourne.
% Copyright (C) 2014-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: erl_rtti.m.
% Main author: wangp, petdr
%
% This module converts from the back-end-independent RTTI data structures into
% ELDS function definitions.
%
% XXX currently we only do enough to allow type classes to work
%
%-----------------------------------------------------------------------------%

:- module erl_backend.erl_rtti.
:- interface.

:- import_module backend_libs.
:- import_module backend_libs.erlang_rtti.
:- import_module backend_libs.rtti.
:- import_module erl_backend.elds.
:- import_module hlds.
:- import_module hlds.hlds_module.

:- import_module list.

%-----------------------------------------------------------------------------%

    % erlang_rtti_data(MI, RD)
    %
    % Converts from rtti_data to erlang_rtti_data.
    %
:- func erlang_rtti_data(module_info, rtti_data) = erlang_rtti_data.

    % Generate a representation of all the erlang RTTI.
    %
:- pred rtti_data_list_to_elds(module_info::in,
    list(erlang_rtti_data)::in, list(elds_rtti_defn)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module erl_backend.erl_call_gen.
:- import_module erl_backend.erl_code_util.
:- import_module hlds.code_model.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module deconstruct.
:- import_module int.
:- import_module maybe.
:- import_module require.
:- import_module univ.
:- import_module varset.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

erlang_rtti_data(_, rtti_data_type_ctor_info(TypeCtorData)) = RttiData :-
    TypeCtorData = type_ctor_data(Version, ModuleName, TypeName,
        Arity, UnifyPred, ComparePred, _Flags, Details),
    ErlangUnify = maybe_get_special_predicate(UnifyPred),
    ErlangCompare = maybe_get_special_predicate(ComparePred),
    ErlangDetails = erlang_type_ctor_details(ModuleName,
        TypeName, Arity, Details),
    ErlangTypeCtorData = erlang_type_ctor_data(Version, ModuleName, TypeName,
        Arity, ErlangUnify, ErlangCompare, ErlangDetails),
    RttiData = erlang_rtti_data_type_ctor_info(ErlangTypeCtorData).
erlang_rtti_data(_, rtti_data_type_info(TypeInfo)) =
    erlang_rtti_data_type_info(TypeInfo).
erlang_rtti_data(_, rtti_data_pseudo_type_info(PseudoTypeInfo)) =
    erlang_rtti_data_pseudo_type_info(PseudoTypeInfo).
erlang_rtti_data(_, rtti_data_base_typeclass_info(Name, Module, Enc, TCI)) =
    erlang_rtti_data_base_typeclass_info(Name, Module, Enc, TCI).
erlang_rtti_data(_, rtti_data_type_class_decl(TCDecl)) =
    erlang_rtti_data_type_class_decl(TCDecl).
erlang_rtti_data(_, rtti_data_type_class_instance(TCInstance)) =
    erlang_rtti_data_type_class_instance(TCInstance).

:- func maybe_get_special_predicate(univ) = maybe(rtti_proc_label).

maybe_get_special_predicate(Univ) =
    ( if univ_to_type(Univ, ProcLabel) then
        yes(ProcLabel)
    else
        no
    ).

    % Given the type_ctor_details return the erlang version of those details.
    % This means conflating enum and no_tags into erlang_du,
    % aborting on reserved types, and specially handling the list type.
    %
:- func erlang_type_ctor_details(module_name, string,
    int, type_ctor_details) = erlang_type_ctor_details.

erlang_type_ctor_details(ModuleName, TypeName, Arity, Details) = D :-
    ( if
        ModuleName = unqualified("list"),
        TypeName = "list",
        Arity = 1
    then
        D = erlang_list
    else if
        ModuleName = unqualified("array"),
        TypeName = "array",
        Arity = 1
    then
        D = erlang_array
    else
        D = erlang_type_ctor_details_2(Details)
    ).

:- func erlang_type_ctor_details_2(type_ctor_details) =
    erlang_type_ctor_details.

erlang_type_ctor_details_2(CtorDetails) = Details :-
    (
        CtorDetails = tcd_enum(_, Functors, _, _, IsDummy, FunctorNums),
        (
            IsDummy = yes,
            ( if Functors = [F] then
                Details = erlang_dummy(F ^ enum_name)
            else
                unexpected($module, $pred,
                    "dummy type with more than one functor")
            )
        ;
            IsDummy = no,
            list.map_corresponding(convert_enum_functor, Functors, FunctorNums,
                ErlFunctors),
            Details = erlang_du(ErlFunctors)
        )
    ;
        CtorDetails = tcd_foreign_enum(_, _, _, _, _, _),
        sorry($module, $pred, "NYI foreign enumerations for Erlang.")
    ;
        CtorDetails = tcd_du(_, Functors, _, _, FunctorNums),
        list.map_corresponding(convert_du_functor, Functors, FunctorNums,
            ErlangFunctors),
        Details = erlang_du(ErlangFunctors)
    ;
        CtorDetails = tcd_notag(_, NoTagFunctor),
        NoTagFunctor = notag_functor(Name, TypeInfo, ArgName, SubtypeInfo),
        OrigArity = 1,
        Ordinal = 0,
        FunctorNum = 0,
        ArgTypeInfo = convert_to_rtti_maybe_pseudo_type_info_or_self(TypeInfo),
        ArgInfos = [du_arg_info(ArgName, ArgTypeInfo, full_word)],
        DUFunctor = erlang_du_functor(Name, OrigArity, Ordinal, FunctorNum,
            erlang_atom_raw(Name), ArgInfos, no, SubtypeInfo),
        Details = erlang_du([DUFunctor])
    ;
        CtorDetails = tcd_eqv(Type),
        Details = erlang_eqv(Type)
    ;
        CtorDetails = tcd_builtin(Builtin),
        Details = erlang_builtin(Builtin)
    ;
        CtorDetails = tcd_impl_artifact(Impl),
        Details = erlang_impl_artifact(erlang_impl_ctor(Impl))
    ;
        CtorDetails = tcd_foreign(_),
        Details = erlang_foreign
    ).

    % Convert an enum_functor into the equivalent erlang_du_functor
    %
:- pred convert_enum_functor(enum_functor::in, int::in, erlang_du_functor::out)
    is det.

convert_enum_functor(EnumFunctor, FunctorNum, ErlangFunctor) :-
    EnumFunctor = enum_functor(Name, Ordinal),
    ErlangFunctor = erlang_du_functor(Name, 0, Ordinal, FunctorNum,
        erlang_atom_raw(Name), [], no, functor_subtype_none).

    % Convert a du_functor into the equivalent erlang_du_functor
    %
:- pred convert_du_functor(du_functor::in, int::in, erlang_du_functor::out)
    is det.

convert_du_functor(Functor, FunctorNum, ErlangFunctor) :-
    Functor = du_functor(Name, Arity, Ordinal, _, ArgInfos, Exist,
        SubtypeInfo),
    ErlangFunctor = erlang_du_functor(Name, Arity, Ordinal, FunctorNum,
        erlang_atom_raw(Name), ArgInfos, Exist, SubtypeInfo).

:- func convert_to_rtti_maybe_pseudo_type_info_or_self(
    rtti_maybe_pseudo_type_info) = rtti_maybe_pseudo_type_info_or_self.

convert_to_rtti_maybe_pseudo_type_info_or_self(pseudo(P)) = pseudo(P).
convert_to_rtti_maybe_pseudo_type_info_or_self(plain(P)) = plain(P).

    % Restrict the implementation artifacts to only those
    % allowed on the erlang backend.
    %
:- func erlang_impl_ctor(impl_ctor) = erlang_impl_ctor.

erlang_impl_ctor(impl_ctor_hp) = erlang_impl_ctor_hp.
erlang_impl_ctor(impl_ctor_subgoal) = erlang_impl_ctor_subgoal.
erlang_impl_ctor(impl_ctor_ticket) = erlang_impl_ctor_ticket.
erlang_impl_ctor(impl_ctor_type_info) = erlang_impl_ctor_type_info.
erlang_impl_ctor(impl_ctor_type_ctor_info) = erlang_impl_ctor_type_ctor_info.
erlang_impl_ctor(impl_ctor_typeclass_info) = erlang_impl_ctor_typeclass_info.
erlang_impl_ctor(impl_ctor_base_typeclass_info) =
    erlang_impl_ctor_base_typeclass_info.

    % The following implementation artificats are never used
    % on the erlang backend.
erlang_impl_ctor(impl_ctor_succip) = _ :-
    unexpected($module, $pred, "impl_ctor_succip").
erlang_impl_ctor(impl_ctor_maxfr) = _ :-
    unexpected($module, $pred, "impl_ctor_maxfr").
erlang_impl_ctor(impl_ctor_curfr) = _ :-
    unexpected($module, $pred, "impl_ctor_curfr").
erlang_impl_ctor(impl_ctor_redofr) = _ :-
    unexpected($module, $pred, "impl_ctor_redofr").
erlang_impl_ctor(impl_ctor_redoip) = _ :-
    unexpected($module, $pred, "impl_ctor_redoip").
erlang_impl_ctor(impl_ctor_trail_ptr) = _ :-
    unexpected($module, $pred, "impl_ctor_trail_ptr").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

rtti_data_list_to_elds(ModuleInfo, RttiDatas, RttiDefns) :-
    list.map(rtti_data_to_elds(ModuleInfo), RttiDatas, RttiDefns0),

    % XXX See mlds_defn_is_potentially_duplicated for how this can
    % be made more efficient.
    RttiDefns = list.sort_and_remove_dups(list.condense(RttiDefns0)).

:- pred rtti_data_to_elds(module_info::in, erlang_rtti_data::in,
    list(elds_rtti_defn)::out) is det.

rtti_data_to_elds(ModuleInfo, RttiData, [RttiDefn]) :-
    RttiData = erlang_rtti_data_base_typeclass_info(TCName, InstanceModule,
        InstanceStr, BaseTypeClassInfo),
    BaseTypeClassInfo = base_typeclass_info(N1, N2, N3, N4, N5, Methods),
    NumExtra = BaseTypeClassInfo ^ num_extra,
    list.map_foldl(erl_gen_method_wrapper(ModuleInfo, NumExtra), Methods,
        MethodWrappers, varset.init, VarSet),

    % NOTE: if you modify this structure you may need to modify
    % erl_base_typeclass_info_method_offset.
    BaseTypeClassInfoData = elds_tuple([
        elds_term(elds_int(N1)),
        elds_term(elds_int(N2)),
        elds_term(elds_int(N3)),
        elds_term(elds_int(N4)),
        elds_term(elds_int(N5))
        | MethodWrappers
    ]),
    RttiId = elds_rtti_base_typeclass_id(TCName, InstanceModule, InstanceStr),
    IsExported = yes,
    RttiDefn = elds_rtti_defn(RttiId, IsExported, VarSet,
        elds_clause([], elds_term(BaseTypeClassInfoData))).

rtti_data_to_elds(ModuleInfo, RttiData, RttiDefns) :-
    RttiData = erlang_rtti_data_type_info(TypeInfo),
    rtti_type_info_to_elds(ModuleInfo, TypeInfo, RttiDefns).
rtti_data_to_elds(ModuleInfo, RttiData, RttiDefns) :-
    RttiData = erlang_rtti_data_pseudo_type_info(PseudoTypeInfo),
    rtti_pseudo_type_info_to_elds(ModuleInfo, PseudoTypeInfo, RttiDefns).
rtti_data_to_elds(_ModuleInfo, RttiData, []) :-
    RttiData = erlang_rtti_data_type_class_decl(_TCDecl).
rtti_data_to_elds(_ModuleInfo, RttiData, []) :-
    RttiData = erlang_rtti_data_type_class_instance(_Instance).
rtti_data_to_elds(ModuleInfo, RttiData, RttiDefns) :-
    RttiData = erlang_rtti_data_type_ctor_info(TypeCtorData),
    type_ctor_data_to_elds(ModuleInfo, TypeCtorData, RttiDefns).

%-----------------------------------------------------------------------------%

:- pred erl_gen_method_wrapper(module_info::in, int::in, rtti_proc_label::in,
    elds_expr::out, prog_varset::in, prog_varset::out) is det.

erl_gen_method_wrapper(ModuleInfo, NumExtra, RttiProcLabel, WrapperFun,
        !VarSet) :-
    PredId = RttiProcLabel ^ rpl_pred_id,
    ProcId = RttiProcLabel ^ rpl_proc_id,
    ArgTypes = RttiProcLabel ^ rpl_proc_arg_types,
    TopFunctorModes = RttiProcLabel ^ rpl_proc_top_modes,
    Detism = RttiProcLabel ^ rpl_proc_interface_detism,

    % We can't store the address of the typeclass method directly in the
    % base_typeclass_info; instead, we need to generate a wrapper function
    % that extracts the NumExtra parameters it needs from the typeclass_info,
    % and store the address of that wrapper function in the
    % base_typeclass_info.
    %
    % Note that this means there are two levels of wrappers: the wrapper that
    % we generate here calls the procedure introduced by check_typeclass.m,
    % and that in turn calls the user's procedure.
    %
    % A det wrapper looks like:
    %
    %   fun(TypeClassInfo, W1, W2, ...) ->
    %       /* extract NumExtra parameters from TypeClassInfo */
    %       E2 = element(2, TypeClassInfo),
    %       E3 = element(3, TypeClassInfo),
    %       ...
    %       {Y1, Y2, ...} = actual_method(TypeClassInfo,
    %           E2, E3, ..., W1, W2, ...),
    %       {Y1, Y2, ...}   /* may have additional outputs */
    %   end

    varset.new_named_var("TypeClassInfo", TCIVar, !VarSet),
    varset.new_vars(list.length(ArgTypes) - NumExtra, Ws, !VarSet),

    % Make the ``E<n> = element(<n>, TypeClassInfo)'' expressions.
    list.map2_foldl(extract_extra_arg(TCIVar), 1 .. NumExtra,
        ExtraVars, ExtractExtras, !VarSet),

    % Figure out the input and output variables for the call to the actual
    % method implementation.
    ExtraVarsWs = ExtraVars ++ Ws,
    erl_gen_arg_list_arg_modes(ModuleInfo, opt_dummy_args,
        ExtraVarsWs, ArgTypes, TopFunctorModes, CallInputArgs, CallOutputArgs),

    % Figure out the input variables and output variables for this wrapper
    % function.
    erl_gen_arg_list_arg_modes(ModuleInfo, no_opt_dummy_args,
        ExtraVarsWs, ArgTypes, TopFunctorModes,
        WrapperInputVarsPlusExtras, WrapperOutputVars),
    WrapperInputVars =
        list.delete_elems(WrapperInputVarsPlusExtras, ExtraVars),

    determinism_to_code_model(Detism, CodeModel),
    WrapperOutputVarsExprs = exprs_from_vars(WrapperOutputVars),
    (
        CodeModel = model_det,
        AllWrapperInputVars = [TCIVar | WrapperInputVars],
        % On success we either return a tuple of the output variables of the
        % call, or if there is exactly one output variable, just that output
        % variable.
        SuccessExpr0 = tuple_or_single_expr(WrapperOutputVarsExprs)
    ;
        CodeModel = model_semi,
        AllWrapperInputVars = [TCIVar | WrapperInputVars],
        % On success we return a tuple of the output variables of the call.
        SuccessExpr0 = elds_term(elds_tuple(WrapperOutputVarsExprs))
    ;
        CodeModel = model_non,
        % model_non wrappers need an additional argument which is the success
        % continuation.  On success we call the success continuation with the
        % output arguments of the call.
        varset.new_named_var("Succeed", SucceedVar, !VarSet),
        AllWrapperInputVars = [TCIVar | WrapperInputVars] ++ [SucceedVar],
        SuccessExpr0 = elds_call(elds_call_ho(expr_from_var(SucceedVar)),
            WrapperOutputVarsExprs)
    ),

    % Any variables which are outputs of the wrapper function but not of the
    % method need to be materialised by the wrapper.
    DummyOutputVars = list.delete_elems(WrapperOutputVars, CallOutputArgs),
    MaterialiseDummyOutputVars = list.map(var_eq_false, DummyOutputVars),
    SuccessExpr = join_exprs(elds_block(MaterialiseDummyOutputVars),
        SuccessExpr0),

    % Make the call to the underlying method implementation.
    CallTarget = elds_call_plain(proc(PredId, ProcId)),
    erl_make_call(CodeModel, CallTarget, CallInputArgs,
        CallOutputArgs, yes(SuccessExpr), DoCall),

    WrapperFun = elds_fun(elds_clause(terms_from_vars(AllWrapperInputVars),
        join_exprs(elds_block(ExtractExtras), DoCall))).

:- pred extract_extra_arg(prog_var::in, int::in, prog_var::out, elds_expr::out,
    prog_varset::in, prog_varset::out) is det.

extract_extra_arg(TCIVar, Index, Var, ExtractStatement, !VarSet) :-
    varset.new_named_var("Extra", Var, !VarSet),
    % Erlang's `element' builtin counts from 1.
    ExtractStatement = elds_eq(expr_from_var(Var),
        elds_call_element(TCIVar, 1 + Index)).

%-----------------------------------------------------------------------------%

    % Generate a representation of a type_info.
    % The generated type_info will always be local to the module.
    %
:- pred rtti_type_info_to_elds(module_info::in, rtti_type_info::in,
    list(elds_rtti_defn)::out) is det.

rtti_type_info_to_elds(ModuleInfo, TypeInfo, RttiDefns) :-
    (
        TypeInfo = plain_arity_zero_type_info(RttiTypeCtor),

        TypeCtorRttiId = elds_rtti_type_ctor_id(RttiTypeCtor),
        ELDSTypeInfo = elds_rtti_ref(TypeCtorRttiId),

        ArgRttiDefns = []
    ;
        TypeInfo = plain_type_info(TypeCtor, ArgTypeInfos),

        rtti_type_info_to_elds_2(ModuleInfo, ArgTypeInfos, ELDSArgTypeInfos,
            ArgRttiDefns),

        ELDSTypeInfo = elds_term(elds_tuple([
            elds_rtti_ref(elds_rtti_type_ctor_id(TypeCtor)) |
            ELDSArgTypeInfos]))
    ;
        TypeInfo = var_arity_type_info(VarCtorId, ArgTypeInfos),
        TypeCtor = var_arity_id_to_rtti_type_ctor(VarCtorId),

        rtti_type_info_to_elds_2(ModuleInfo, ArgTypeInfos, ELDSArgTypeInfos,
            ArgRttiDefns),

        ELDSTypeInfo = elds_term(elds_tuple([
            elds_rtti_ref(elds_rtti_type_ctor_id(TypeCtor)),
            elds_term(elds_int(list.length(ArgTypeInfos))) |
            ELDSArgTypeInfos]))
    ),

    % A type_info can contain a call to construct a type_ctor_info
    % which requires this type_info, leading to infinite recursion,
    % we break this recursion by creating a closure which will
    % evaluate to the type_info, if the type_info is needed.

    ELDSFun = elds_fun(elds_clause([], ELDSTypeInfo)),

    ELDSTuple = elds_term(elds_tuple([
        elds_term(elds_atom_raw("plain")),
        ELDSFun
    ])),

    RttiId = elds_rtti_type_info_id(TypeInfo),
    IsExported = no,
    RttiDefn = elds_rtti_defn(RttiId, IsExported, varset.init,
        elds_clause([], ELDSTuple)),

    RttiDefns = [RttiDefn | ArgRttiDefns].

:- pred rtti_type_info_to_elds_2(module_info::in,
    list(rtti_type_info)::in,
    list(elds_expr)::out, list(elds_rtti_defn)::out) is det.

rtti_type_info_to_elds_2(ModuleInfo,
        ArgTypeInfos, ELDSArgTypeInfos, ArgRttiDefns) :-
    list.map(rtti_type_info_to_elds(ModuleInfo), ArgTypeInfos, ArgRttiDefns0),
    ArgRttiDefns = list.sort_and_remove_dups(list.condense(ArgRttiDefns0)),

    ELDSArgTypeInfos = list.map(
        func(TI) = elds_rtti_ref(elds_rtti_type_info_id(TI)), ArgTypeInfos).

%-----------------------------------------------------------------------------%

    % Generate a representation of a pseudo_type_info.
    % The generated pseudo_type_info will always be local to the module.
    %
:- pred rtti_pseudo_type_info_to_elds(module_info::in,
    rtti_pseudo_type_info::in, list(elds_rtti_defn)::out) is det.

rtti_pseudo_type_info_to_elds(ModuleInfo, TypeInfo, RttiDefns) :-
    (
        TypeInfo = plain_arity_zero_pseudo_type_info(RttiTypeCtor),

        TypeCtorRttiId = elds_rtti_type_ctor_id(RttiTypeCtor),
        ELDSTypeInfo = elds_rtti_ref(TypeCtorRttiId),

        ArgRttiDefns = []
    ;

        TypeInfo = plain_pseudo_type_info(TypeCtor, ArgTypeInfos),

        rtti_pseudo_type_info_to_elds_2(ModuleInfo,
            ArgTypeInfos, ELDSArgTypeInfos, ArgRttiDefns),

        ELDSTypeInfo = elds_term(elds_tuple([
            elds_rtti_ref(elds_rtti_type_ctor_id(TypeCtor)) |
            ELDSArgTypeInfos]))
    ;
        TypeInfo = var_arity_pseudo_type_info(VarCtorId, ArgTypeInfos),
        TypeCtor = var_arity_id_to_rtti_type_ctor(VarCtorId),

        rtti_pseudo_type_info_to_elds_2(ModuleInfo,
            ArgTypeInfos, ELDSArgTypeInfos, ArgRttiDefns),

        ELDSTypeInfo = elds_term(elds_tuple([
            elds_rtti_ref(elds_rtti_type_ctor_id(TypeCtor)),
            elds_term(elds_int(list.length(ArgTypeInfos))) |
            ELDSArgTypeInfos]))
    ;
        TypeInfo = type_var(I),
        ELDSTypeInfo = elds_term(elds_int(I)),
        ArgRttiDefns = []
    ),

    % A pseudo_type_info can contain a call to construct a type_ctor_info
    % which requires this pseudo_type_info, leading to infinite recursion.
    % We break this recursion by creating a closure which will
    % evaluate to the pseudo_type_info, if the type_info is needed.
    %
    ELDSFun = elds_fun(elds_clause([], ELDSTypeInfo)),

    ELDSTuple = elds_term(elds_tuple([
        elds_term(elds_atom_raw("pseudo")),
        ELDSFun
    ])),

    RttiId = elds_rtti_pseudo_type_info_id(TypeInfo),
    IsExported = no,
    RttiDefn = elds_rtti_defn(RttiId, IsExported, varset.init,
        elds_clause([], ELDSTuple)),

    RttiDefns = [RttiDefn | ArgRttiDefns].

:- pred rtti_pseudo_type_info_to_elds_2(module_info::in,
    list(rtti_maybe_pseudo_type_info)::in,
    list(elds_expr)::out, list(elds_rtti_defn)::out) is det.

rtti_pseudo_type_info_to_elds_2(ModuleInfo,
        ArgTypeInfos, ELDSArgTypeInfos, ArgRttiDefns) :-
    list.map(rtti_maybe_pseudo_type_info_to_elds(ModuleInfo),
            ArgTypeInfos, ArgRttiDefns0),
    ArgRttiDefns = list.sort_and_remove_dups(list.condense(ArgRttiDefns0)),

    ELDSArgTypeInfos = list.map(
        (func(MPTI) = elds_rtti_ref(Id) :-
            (
                MPTI = pseudo(PTI),
                Id = elds_rtti_pseudo_type_info_id(PTI)
            ;
                MPTI = plain(TI),
                Id = elds_rtti_type_info_id(TI)
            )
        ), ArgTypeInfos).

:- pred rtti_maybe_pseudo_type_info_to_elds(module_info::in,
    rtti_maybe_pseudo_type_info::in, list(elds_rtti_defn)::out) is det.

rtti_maybe_pseudo_type_info_to_elds(ModuleInfo, plain(TypeInfo), Defns) :-
    rtti_type_info_to_elds(ModuleInfo, TypeInfo, Defns).
rtti_maybe_pseudo_type_info_to_elds(ModuleInfo, pseudo(TypeInfo), Defns) :-
    rtti_pseudo_type_info_to_elds(ModuleInfo, TypeInfo, Defns).

%-----------------------------------------------------------------------------%

    % This predicate defines the representation of type_ctor_info
    % for the erlang backend.
    %
:- pred type_ctor_data_to_elds(module_info::in, erlang_type_ctor_data::in,
    list(elds_rtti_defn)::out) is det.

type_ctor_data_to_elds(ModuleInfo, TypeCtorData, RttiDefns) :-
    TypeCtorData = erlang_type_ctor_data(Version, ModuleName, TypeName, Arity,
        UnifyProcLabel, CompareProcLabel, Details),

    some [!VarSet] (
        varset.init(!:VarSet),
        gen_init_special_pred(ModuleInfo, UnifyProcLabel, UnifyExpr, !VarSet),
        gen_init_special_pred(ModuleInfo,
            CompareProcLabel, CompareExpr, !VarSet),

        erlang_type_ctor_details(ModuleInfo, Details, ELDSDetails0,
            RttiDefns0),
        reduce_list_term_complexity(ELDSDetails0, ELDSDetails,
            [], RevAssignments, !VarSet),

        VarSet = !.VarSet
    ),

    ELDSTypeCtorData = elds_tuple([
        elds_term(elds_int(Arity)),
        elds_term(elds_int(Version)),
        UnifyExpr,
        CompareExpr,
        elds_term(elds_list_of_ints(sym_name_to_string(ModuleName))),
        elds_term(elds_list_of_ints(TypeName)),
        erlang_type_ctor_rep(Details),
        ELDSDetails
    ]),
    ClauseBody = elds_block(list.reverse(RevAssignments) ++
        [elds_term(ELDSTypeCtorData)]),

    TypeCtor = rtti_type_ctor(ModuleName, TypeName, Arity),
    RttiId = elds_rtti_type_ctor_id(TypeCtor),
    IsExported = yes,
    RttiDefn = elds_rtti_defn(RttiId, IsExported, VarSet,
        elds_clause([], ClauseBody)),
    RttiDefns = [RttiDefn | RttiDefns0].

:- func erlang_type_ctor_rep(erlang_type_ctor_details) = elds_expr.

erlang_type_ctor_rep(erlang_du(_)) =
    elds_term(make_enum_alternative("etcr_du")).
erlang_type_ctor_rep(erlang_dummy(_)) =
    elds_term(make_enum_alternative("etcr_dummy")).
erlang_type_ctor_rep(erlang_list) =
    elds_term(make_enum_alternative("etcr_list")).
erlang_type_ctor_rep(erlang_array) =
    elds_term(make_enum_alternative("etcr_array")).
erlang_type_ctor_rep(erlang_eqv(_)) =
    elds_term(make_enum_alternative("etcr_eqv")).
erlang_type_ctor_rep(erlang_builtin(builtin_ctor_int)) =
    elds_term(make_enum_alternative("etcr_int")).
erlang_type_ctor_rep(erlang_builtin(builtin_ctor_uint)) =
    elds_term(make_enum_alternative("etcr_uint")).
erlang_type_ctor_rep(erlang_builtin(builtin_ctor_int8)) =
    elds_term(make_enum_alternative("etcr_int8")).
erlang_type_ctor_rep(erlang_builtin(builtin_ctor_uint8)) =
    elds_term(make_enum_alternative("etcr_uint8")).
erlang_type_ctor_rep(erlang_builtin(builtin_ctor_int16)) =
    elds_term(make_enum_alternative("etcr_int16")).
erlang_type_ctor_rep(erlang_builtin(builtin_ctor_uint16)) =
    elds_term(make_enum_alternative("etcr_uint16")).
erlang_type_ctor_rep(erlang_builtin(builtin_ctor_int32)) =
    elds_term(make_enum_alternative("etcr_int32")).
erlang_type_ctor_rep(erlang_builtin(builtin_ctor_uint32)) =
    elds_term(make_enum_alternative("etcr_uint32")).
erlang_type_ctor_rep(erlang_builtin(builtin_ctor_int64)) =
    elds_term(make_enum_alternative("etcr_int64")).
erlang_type_ctor_rep(erlang_builtin(builtin_ctor_uint64)) =
    elds_term(make_enum_alternative("etcr_uint64")).
erlang_type_ctor_rep(erlang_builtin(builtin_ctor_float)) =
    elds_term(make_enum_alternative("etcr_float")).
erlang_type_ctor_rep(erlang_builtin(builtin_ctor_char)) =
    elds_term(make_enum_alternative("etcr_char")).
erlang_type_ctor_rep(erlang_builtin(builtin_ctor_string)) =
    elds_term(make_enum_alternative("etcr_string")).
erlang_type_ctor_rep(erlang_builtin(builtin_ctor_void)) =
    elds_term(make_enum_alternative("etcr_void")).
erlang_type_ctor_rep(erlang_builtin(builtin_ctor_c_pointer(is_stable))) =
    elds_term(make_enum_alternative("etcr_stable_c_pointer")).
erlang_type_ctor_rep(erlang_builtin(builtin_ctor_c_pointer(is_not_stable))) =
    elds_term(make_enum_alternative("etcr_c_pointer")).
erlang_type_ctor_rep(erlang_builtin(builtin_ctor_pred_ctor)) =
    elds_term(make_enum_alternative("etcr_pred")).
erlang_type_ctor_rep(erlang_builtin(builtin_ctor_func_ctor)) =
    elds_term(make_enum_alternative("etcr_func")).
erlang_type_ctor_rep(erlang_builtin(builtin_ctor_tuple)) =
    elds_term(make_enum_alternative("etcr_tuple")).
erlang_type_ctor_rep(erlang_builtin(builtin_ctor_ref)) =
    elds_term(make_enum_alternative("etcr_ref")).
erlang_type_ctor_rep(erlang_builtin(builtin_ctor_type_desc)) =
    elds_term(make_enum_alternative("etcr_type_desc")).
erlang_type_ctor_rep(erlang_builtin(builtin_ctor_pseudo_type_desc)) =
    elds_term(make_enum_alternative("etcr_pseudo_type_desc")).
erlang_type_ctor_rep(erlang_builtin(builtin_ctor_type_ctor_desc)) =
    elds_term(make_enum_alternative("etcr_type_ctor_desc")).
erlang_type_ctor_rep(erlang_impl_artifact(erlang_impl_ctor_type_info)) =
    elds_term(make_enum_alternative("etcr_type_info")).
erlang_type_ctor_rep(erlang_impl_artifact(erlang_impl_ctor_type_ctor_info)) =
    elds_term(make_enum_alternative("etcr_type_ctor_info")).
erlang_type_ctor_rep(erlang_impl_artifact(erlang_impl_ctor_typeclass_info)) =
    elds_term(make_enum_alternative("etcr_typeclass_info")).
erlang_type_ctor_rep(
    erlang_impl_artifact(erlang_impl_ctor_base_typeclass_info)) =
    elds_term(make_enum_alternative("etcr_base_typeclass_info")).
erlang_type_ctor_rep(erlang_foreign) =
    elds_term(make_enum_alternative("etcr_foreign")).

    % These three types should never actually be used in an Erlang program.
    %
erlang_type_ctor_rep(erlang_impl_artifact(erlang_impl_ctor_hp)) =
    elds_term(make_enum_alternative("etcr_hp")).
erlang_type_ctor_rep(erlang_impl_artifact(erlang_impl_ctor_subgoal)) =
    elds_term(make_enum_alternative("etcr_subgoal")).
erlang_type_ctor_rep(erlang_impl_artifact(erlang_impl_ctor_ticket)) =
    elds_term(make_enum_alternative("etcr_ticket")).

:- pred gen_init_special_pred(module_info::in, maybe(rtti_proc_label)::in,
    elds_expr::out, prog_varset::in, prog_varset::out) is det.

gen_init_special_pred(ModuleInfo, MaybeRttiProcLabel, Expr, !VarSet) :-
    (
        MaybeRttiProcLabel = yes(RttiProcLabel),
        erl_gen_special_pred_wrapper(ModuleInfo, RttiProcLabel, Expr, !VarSet)
    ;
        MaybeRttiProcLabel = no,
        unexpected($module, $pred, "no special pred")
    ).

:- pred erl_gen_special_pred_wrapper(module_info::in, rtti_proc_label::in,
    elds_expr::out, prog_varset::in, prog_varset::out) is det.

erl_gen_special_pred_wrapper(ModuleInfo, RttiProcLabel, WrapperFun, !VarSet) :-
    PredId = RttiProcLabel ^ rpl_pred_id,
    ProcId = RttiProcLabel ^ rpl_proc_id,
    ArgTypes = RttiProcLabel ^ rpl_proc_arg_types,
    TopFunctorModes = RttiProcLabel ^ rpl_proc_top_modes,
    Detism = RttiProcLabel ^ rpl_proc_interface_detism,

    % Create the variable list.
    varset.new_vars(list.length(ArgTypes), Ws, !VarSet),

    % Figure out the input and output variables for the call to the actual
    % special pred implementation.
    erl_gen_arg_list_arg_modes(ModuleInfo, opt_dummy_args,
        Ws, ArgTypes, TopFunctorModes, CallInputArgs, CallOutputArgs),

    % Figure out the input variables and output variables for this wrapper
    % function.
    erl_gen_arg_list_arg_modes(ModuleInfo, no_opt_dummy_args,
        Ws, ArgTypes, TopFunctorModes,
        WrapperInputVars, WrapperOutputVars),

    determinism_to_code_model(Detism, CodeModel),
    WrapperOutputVarsExprs = exprs_from_vars(WrapperOutputVars),
    (
        CodeModel = model_det,
        % On success we either return a tuple of the output variables of the
        % call, or if there is exactly one output variable, just that output
        % variable.
        SuccessExpr0 = tuple_or_single_expr(WrapperOutputVarsExprs)
    ;
        CodeModel = model_semi,
        % On success we return a tuple of the output arguments of the call.
        SuccessExpr0 = elds_term(elds_tuple(WrapperOutputVarsExprs))
    ;
        CodeModel = model_non,
        unexpected($module, $pred, "model_non code_model")
    ),

    % Any variables which are outputs of the wrapper function but not of the
    % method need to be materialised by the wrapper.
    DummyOutputVars = list.delete_elems(WrapperOutputVars, CallOutputArgs),
    MaterialiseDummyOutputVars = list.map(var_eq_false, DummyOutputVars),
    SuccessExpr = join_exprs(elds_block(MaterialiseDummyOutputVars),
        SuccessExpr0),

    % Make the call to the underlying method implementation.
    CallTarget = elds_call_plain(proc(PredId, ProcId)),
    erl_make_call(CodeModel, CallTarget, CallInputArgs,
        CallOutputArgs, yes(SuccessExpr), DoCall),

    WrapperFun = elds_fun(elds_clause(terms_from_vars(WrapperInputVars),
        DoCall)).

    % erlang_type_ctor_details(ModuleInfo, Details, Expr, Defns)
    %
    % will return the expr, Expr, which evaluates to an erlang term
    % which describes the type in more detail, plus the extra
    % definitions, Defns, needed to help define that term.
    %
    % Note two calls to this predicate may generate duplicate
    % definitions, so the user is responsible for getting rid
    % of duplicate definitions.
    %
:- pred erlang_type_ctor_details(module_info::in, erlang_type_ctor_details::in,
    elds_expr::out, list(elds_rtti_defn)::out) is det.

erlang_type_ctor_details(ModuleInfo, Details, Term, Defns) :-
    (
        Details = erlang_du(Functors),
        rtti_to_elds_expr(ModuleInfo, Functors, Term, [], Defns)
    ;
        Details = erlang_dummy(DummyFunctorName),
        rtti_to_elds_expr(ModuleInfo, DummyFunctorName, Term, [], Defns)
    ;
        Details = erlang_eqv(MaybePseudoTypeInfo),
        rtti_to_elds_expr(ModuleInfo, MaybePseudoTypeInfo, Term, [], Defns)
    ;
        % The types don't require any extra information
        ( Details = erlang_list
        ; Details = erlang_array
        ; Details = erlang_builtin(_)
        ; Details = erlang_impl_artifact(_)
        ; Details = erlang_foreign
        ),
        Term = elds_term(elds_tuple([])),
        Defns = []
    ).

    % For some types we can generate a very long list for the type ctor
    % details, such that the Erlang compiler aborts with a message "An
    % implementation limit was reached.  Try reducing the complexity of this
    % function."
    %
    % Work around this problem by lifting the tail expression of each cons cell
    % out and assigning it to a fresh variable.
    %
:- pred reduce_list_term_complexity(elds_expr::in, elds_expr::out,
    list(elds_expr)::in, list(elds_expr)::out,
    prog_varset::in, prog_varset::out) is det.

reduce_list_term_complexity(Expr0, Expr, !RevAssignments, !VarSet) :-
    ( if
        Expr0 = elds_term(elds_tuple([Functor, Head, Tail0])),
        Functor = elds_term(elds_atom(SymName)),
        unqualify_name(SymName) = "[|]"
    then
        reduce_list_term_complexity(Tail0, Tail, !RevAssignments, !VarSet),
        varset.new_var(V, !VarSet),
        Assign = elds_eq(expr_from_var(V), Tail),
        Expr = elds_term(elds_tuple([Functor, Head, expr_from_var(V)])),
        list.cons(Assign, !RevAssignments)
    else
        Expr = Expr0
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % rtti_to_elds_expr(MI, T, Expr, !Defns)
    %
    % Given some T which is a representation of the RTTI data,
    % it generates the elds_expr which would represent that T as an erlang
    % term.
    %
    % It specially handles the types
    %   * erlang_atom_raw
    %   * rtti_maybe_pseudo_type_info
    %   * rtti_maybe_pseudo_type_info_or_self
    %
:- pred rtti_to_elds_expr(module_info::in, T::in, elds_expr::out,
    list(elds_rtti_defn)::in, list(elds_rtti_defn)::out) is det.

rtti_to_elds_expr(MI, Term, ELDS, !Defns) :-
    ( if dynamic_cast(Term, Int) then
        ELDS = elds_term(elds_int(Int))
    else if dynamic_cast(Term, Char) then
        ELDS = elds_term(elds_char(Char))
    else if dynamic_cast(Term, String) then
        ELDS = elds_term(elds_list_of_ints(String))
    else if dynamic_cast(Term, Float) then
        ELDS = elds_term(elds_float(Float))

    % The RTTI types which have to be handled specially.
    else if dynamic_cast(Term, Atom) then
        Atom = erlang_atom_raw(S),
        ELDS = elds_term(elds_atom_raw(S))
    else if dynamic_cast(Term, MaybePseudoTypeInfo) then
        convert_maybe_pseudo_type_info_to_elds(MI,
            MaybePseudoTypeInfo, ELDS, !Defns)
    else if dynamic_cast(Term, MaybePseudoTypeInfoOrSelf) then
        convert_maybe_pseudo_type_info_or_self_to_elds(MI,
            MaybePseudoTypeInfoOrSelf, ELDS, !Defns)

    else
        functor(Term, do_not_allow, Functor, Arity),

        list.map_foldl(convert_arg_to_elds_expr(MI, Term),
            0 .. (Arity - 1), Exprs, !Defns),

        ( if Functor = "{}" then
            ELDS = elds_term(elds_tuple(Exprs))
        else
            FunctorTerm = elds_term(elds_atom(unqualified(Functor))),
            ELDS = elds_term(elds_tuple([FunctorTerm | Exprs]))
        )
    ).

:- pred convert_arg_to_elds_expr(module_info::in, T::in, int::in,
    elds_expr::out, list(elds_rtti_defn)::in, list(elds_rtti_defn)::out)
    is det.

convert_arg_to_elds_expr(MI, Term, Index, ELDS, !Defns) :-
    ( if arg(Term, do_not_allow, Index, Arg) then
        rtti_to_elds_expr(MI, Arg, ELDS, !Defns)
    else
        unexpected($module, $pred, "arg failed")
    ).

:- pred convert_maybe_pseudo_type_info_or_self_to_elds(module_info::in,
    rtti_maybe_pseudo_type_info_or_self::in,
    elds_expr::out, list(elds_rtti_defn)::in, list(elds_rtti_defn)::out)
    is det.

convert_maybe_pseudo_type_info_or_self_to_elds(MI, TI, Expr, !Defns) :-
    maybe_pseudo_type_info_or_self_to_elds(MI, TI, RttiId, Defns),
    !:Defns = list.sort_and_remove_dups(Defns ++ !.Defns),
    Expr = elds_rtti_ref(RttiId).

:- pred convert_maybe_pseudo_type_info_to_elds(module_info::in,
    rtti_maybe_pseudo_type_info::in,
    elds_expr::out, list(elds_rtti_defn)::in, list(elds_rtti_defn)::out)
    is det.

convert_maybe_pseudo_type_info_to_elds(MI, TI, Expr, !Defns) :-
    maybe_pseudo_type_info_to_elds(MI, TI, RttiId, Defns),
    !:Defns = list.sort_and_remove_dups(Defns ++ !.Defns),
    Expr = elds_rtti_ref(RttiId).

:- pred maybe_pseudo_type_info_or_self_to_elds(module_info::in,
    rtti_maybe_pseudo_type_info_or_self::in,
    elds_rtti_id::out, list(elds_rtti_defn)::out) is det.

maybe_pseudo_type_info_or_self_to_elds(MI, plain(TI), RttiId, Defns) :-
    maybe_pseudo_type_info_to_elds(MI, plain(TI), RttiId, Defns).
maybe_pseudo_type_info_or_self_to_elds(MI, pseudo(PTI), RttiId, Defns) :-
    maybe_pseudo_type_info_to_elds(MI, pseudo(PTI), RttiId, Defns).
maybe_pseudo_type_info_or_self_to_elds(_MI, self, _RttiId, _Defns) :-
    unexpected($module, $pred, "self not handled yet").

:- pred maybe_pseudo_type_info_to_elds(module_info::in,
    rtti_maybe_pseudo_type_info::in,
    elds_rtti_id::out, list(elds_rtti_defn)::out) is det.

maybe_pseudo_type_info_to_elds(ModuleInfo, plain(TypeInfo), RttiId, Defns) :-
    RttiId = elds_rtti_type_info_id(TypeInfo),
    rtti_type_info_to_elds(ModuleInfo, TypeInfo, Defns).
maybe_pseudo_type_info_to_elds(ModuleInfo, pseudo(PTypeInfo), RttiId, Defns) :-
    RttiId = elds_rtti_pseudo_type_info_id(PTypeInfo),
    rtti_pseudo_type_info_to_elds(ModuleInfo, PTypeInfo, Defns).

%-----------------------------------------------------------------------------%
:- end_module erl_backend.erl_rtti.
%-----------------------------------------------------------------------------%
