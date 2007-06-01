%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2007 The University of Melbourne.
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

:- import_module backend_libs.erlang_rtti.
:- import_module backend_libs.rtti.
:- import_module erl_backend.elds.
:- import_module hlds.hlds_module.

:- import_module list.

%-----------------------------------------------------------------------------%

    %
    % erlang_rtti_data(MI, RD)
    %
    % converts from rtti_data to erlang_rtti_data.
    %
:- func erlang_rtti_data(module_info, rtti_data) = erlang_rtti_data.

    %
    % Generate a representation of all the erlang RTTI
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
:- import_module libs.compiler_util.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module int.
:- import_module maybe.
:- import_module string.
:- import_module svvarset.
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
    ( univ_to_type(Univ, ProcLabel) ->
        yes(ProcLabel)
    ;
        no
    ).

    %
    % Given the type_ctor_details return the erlang version of those
    % details.
    % This means conflating enum and no_tags into erlang_du,
    % aborting on reserved types, and specially handling the list type.
    %
:- func erlang_type_ctor_details(module_name, string,
    int, type_ctor_details) = erlang_type_ctor_details.

erlang_type_ctor_details(ModuleName, TypeName, Arity, Details) = D :-
    (
        ModuleName = unqualified("list"),
        TypeName = "list",
        Arity = 1
    ->
        ( list_argument_type(Details, Type) ->
            D = erlang_list(Type)
        ;
            unexpected(this_file, "erlang_type_ctor_details: " ++
                "unable to determine type of list argument")
        )
    ;
        D = erlang_type_ctor_details_2(Details)
    ).

    %
    % Given a type_ctor_detail which represents a list,
    % determine the type of the argument to the list.
    %
:- pred list_argument_type(type_ctor_details::in,
    rtti_maybe_pseudo_type_info::out) is semidet.

list_argument_type(Details, Type) :-
    Functors = Details ^ du_functors,
    list_argument_type_2(Functors, Type).
    
:- pred list_argument_type_2(list(du_functor)::in,
    rtti_maybe_pseudo_type_info::out) is semidet.

list_argument_type_2([Functor | Functors], Type) :-
    ( Functor ^ du_name = "[|]" ->
        Functor ^ du_arg_infos = [du_arg_info(_, Type0), _],
        convert_to_rtti_maybe_pseudo_type_info(Type0, Type)
    ;
        list_argument_type_2(Functors, Type)
    ).
        
:- pred convert_to_rtti_maybe_pseudo_type_info(
    rtti_maybe_pseudo_type_info_or_self::in,
    rtti_maybe_pseudo_type_info::out) is semidet.

convert_to_rtti_maybe_pseudo_type_info(plain(P), plain(P)).
convert_to_rtti_maybe_pseudo_type_info(pseudo(P), pseudo(P)).

:- func erlang_type_ctor_details_2(type_ctor_details) =
    erlang_type_ctor_details.

erlang_type_ctor_details_2(enum(_, Functors, _, _, _IsDummy, _)) =
        % XXX Handle IsDummy
    erlang_du(list.map(convert_enum_functor, Functors)).
erlang_type_ctor_details_2(du(_, Functors, _, _, _)) =
    erlang_du(list.map(convert_du_functor, Functors)).
erlang_type_ctor_details_2(reserved(_, _, _, _, _, _)) =
        % Reserved types are not supported on the Erlang backend.
    unexpected(this_file, "erlang_type_ctor_details: reserved").
erlang_type_ctor_details_2(notag(_, NoTagFunctor)) = Details :-
    NoTagFunctor = notag_functor(Name, TypeInfo, ArgName),
    ArgTypeInfo = convert_to_rtti_maybe_pseudo_type_info_or_self(TypeInfo),
    ArgInfos = [du_arg_info(ArgName, ArgTypeInfo)],
    DUFunctor = erlang_du_functor(Name, 0, 1, Name, ArgInfos, no),
    Details = erlang_du([DUFunctor]).
erlang_type_ctor_details_2(eqv(Type)) = erlang_eqv(Type).
erlang_type_ctor_details_2(builtin(Builtin)) = erlang_builtin(Builtin).
erlang_type_ctor_details_2(impl_artifact(Impl)) = erlang_impl_artifact(EImpl) :-
    EImpl = erlang_impl_ctor(Impl).
erlang_type_ctor_details_2(foreign(_)) = erlang_foreign.
    
    %
    % Convert an enum_functor into the equivalent erlang_du_functor
    %
:- func convert_enum_functor(enum_functor) = erlang_du_functor.

convert_enum_functor(enum_functor(Name, _)) =
    erlang_du_functor(Name, 0, 1, Name, [], no).

    %
    % Convert a du_functor into the equivalent erlang_du_functor
    %
:- func convert_du_functor(du_functor) = erlang_du_functor.

convert_du_functor(du_functor(Name, Arity, Ordinal, _, ArgInfos, Exist)) =
    erlang_du_functor(Name, Arity, Ordinal + 1, Name, ArgInfos, Exist).

:- func convert_to_rtti_maybe_pseudo_type_info_or_self(
    rtti_maybe_pseudo_type_info) = rtti_maybe_pseudo_type_info_or_self.

convert_to_rtti_maybe_pseudo_type_info_or_self(pseudo(P)) = pseudo(P).
convert_to_rtti_maybe_pseudo_type_info_or_self(plain(P)) = plain(P).

    %
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
    unexpected(this_file, "erlang_impl_ctor: impl_ctor_succip").
erlang_impl_ctor(impl_ctor_maxfr) = _ :-
    unexpected(this_file, "erlang_impl_ctor: impl_ctor_maxfr").
erlang_impl_ctor(impl_ctor_curfr) = _ :-
    unexpected(this_file, "erlang_impl_ctor: impl_ctor_curfr").
erlang_impl_ctor(impl_ctor_redofr) = _ :-
    unexpected(this_file, "erlang_impl_ctor: impl_ctor_redofr").
erlang_impl_ctor(impl_ctor_redoip) = _ :-
    unexpected(this_file, "erlang_impl_ctor: impl_ctor_redoip").
erlang_impl_ctor(impl_ctor_trail_ptr) = _ :-
    unexpected(this_file, "erlang_impl_ctor: impl_ctor_trail_ptr").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

rtti_data_list_to_elds(ModuleInfo, RttiDatas, RttiDefns) :-
    list.map(rtti_data_to_elds(ModuleInfo), RttiDatas, RttiDefns0),
    RttiDefns = list.condense(RttiDefns0).

:- pred rtti_data_to_elds(module_info::in, erlang_rtti_data::in,
    list(elds_rtti_defn)::out) is det.

rtti_data_to_elds(ModuleInfo, RttiData, [RttiDefn]) :-
    RttiData = erlang_rtti_data_base_typeclass_info(TCName, InstanceModule,
        InstanceStr, BaseTypeClassInfo),
    BaseTypeClassInfo = base_typeclass_info(N1, N2, N3, N4, N5, Methods),
    NumExtra = BaseTypeClassInfo ^ num_extra,
    list.map_foldl(erl_gen_method_wrapper(ModuleInfo, NumExtra), Methods,
        MethodWrappers, varset.init, VarSet),
    % 
    % NOTE: if you modify this structure you may need to modify
    % erl_base_typeclass_info_method_offset.
    %
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

rtti_data_to_elds(_ModuleInfo, RttiData, []) :-
    RttiData = erlang_rtti_data_type_info(_TypeInfo),
    unexpected(this_file, "rtti_data_to_elds: rtti_data_type_info").
rtti_data_to_elds(_ModuleInfo, RttiData, []) :-
    RttiData = erlang_rtti_data_pseudo_type_info(_PseudoTypeInfo),
    unexpected(this_file, "rtti_data_to_elds: rtti_data_pseudo_type_info").
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

erl_gen_method_wrapper(ModuleInfo, NumExtra, RttiProcId, WrapperFun,
        !VarSet) :-
    PredId = RttiProcId ^ pred_id,
    ProcId = RttiProcId ^ proc_id,
    Arity = RttiProcId ^ proc_arity,
    ArgTypes = RttiProcId ^ proc_arg_types,
    ArgModes = RttiProcId ^ proc_arg_modes,
    Detism = RttiProcId ^ proc_interface_detism,

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
    %

    svvarset.new_named_var("TypeClassInfo", TCIVar, !VarSet),
    svvarset.new_vars(Arity, Ws, !VarSet),

    % Make the ``E<n> = element(<n>, TypeClassInfo)'' expressions.
    list.map2_foldl(extract_extra_arg(TCIVar), 1 .. NumExtra,
        ExtraVars, ExtractExtras, !VarSet),

    % Figure out the input and output variables for the call to the actual
    % method implementation.
    ExtraVarsWs = ExtraVars ++ Ws,
    erl_gen_arg_list_arg_modes(ModuleInfo, opt_dummy_args,
        ExtraVarsWs, ArgTypes, ArgModes, CallInputArgs, CallOutputArgs),

    % Figure out the input variables and output variables for this wrapper
    % function.
    erl_gen_arg_list_arg_modes(ModuleInfo, no_opt_dummy_args,
        ExtraVarsWs, ArgTypes, ArgModes,
        WrapperInputVarsPlusExtras, WrapperOutputVars),
    WrapperInputVars = list.delete_elems(WrapperInputVarsPlusExtras, ExtraVars),

    determinism_to_code_model(Detism, CodeModel),
    WrapperOutputVarsExprs = exprs_from_vars(WrapperOutputVars),
    (
        ( CodeModel = model_det
        ; CodeModel = model_semi
        ),
        AllWrapperInputVars = [TCIVar | WrapperInputVars],
        % On success we return a tuple of the output arguments of the call.
        SuccessExpr0 = elds_term(elds_tuple(WrapperOutputVarsExprs))
    ;
        CodeModel = model_non,
        % model_non wrappers need an additional argument which is the success
        % continuation.  On success we call the success continuation with the
        % output arguments of the call.
        svvarset.new_named_var("Succeed", SucceedVar, !VarSet),
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
    svvarset.new_named_var("Extra", Var, !VarSet),
    % Erlang's `element' builtin counts from 1.
    ExtractStatement = elds_eq(expr_from_var(Var),
        elds_call_element(TCIVar, 1 + Index)).

%-----------------------------------------------------------------------------%

    % XXX This code is dead, but I've left it in until I am sure
    % that we don't need it.
:- pred rtti_type_info_to_elds(module_info::in, rtti_type_info::in,
    list(elds_rtti_defn)::out) is det.

rtti_type_info_to_elds(_ModuleInfo, TypeInfo, RttiDefns) :-
    TypeInfo = plain_arity_zero_type_info(RttiTypeCtor),
    RttiTypeCtor = rtti_type_ctor(ModuleName, TypeName, Arity),
    TypeCtorRttiId = elds_rtti_type_ctor_id(ModuleName, TypeName, Arity),

    ELDSTypeInfo = elds_tuple([elds_rtti_ref(TypeCtorRttiId)]),

    
    RttiId = elds_rtti_type_info_id(ModuleName, TypeName, Arity),
    IsExported = yes,
    RttiDefn = elds_rtti_defn(RttiId, IsExported, varset.init,
        elds_clause([], elds_term(ELDSTypeInfo))),

    RttiDefns = [RttiDefn].
rtti_type_info_to_elds(_ModuleInfo, TypeInfo, RttiDefns) :-
    TypeInfo = plain_type_info(_, _),
    RttiDefns = [].
rtti_type_info_to_elds(_ModuleInfo, TypeInfo, RttiDefns) :-
    TypeInfo = var_arity_type_info(_, _),
    RttiDefns = [].

%-----------------------------------------------------------------------------%

    % See MR_TypeCtorInfo_Struct in runtime/mercury_type_info.h
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
        VarSet = !.VarSet
    ),

    ELDSTypeCtorData = elds_tuple([
        elds_term(elds_int(Arity)),
        elds_term(elds_int(Version)),
        UnifyExpr,
        CompareExpr,
        elds_term(elds_string(sym_name_to_string(ModuleName))),
        elds_term(elds_string(TypeName)),
        erlang_type_ctor_rep(Details)
        ]),
    RttiId = elds_rtti_type_ctor_id(ModuleName, TypeName, Arity),
    IsExported = yes,
    RttiDefn = elds_rtti_defn(RttiId, IsExported, VarSet,
        elds_clause([], elds_term(ELDSTypeCtorData))),
    RttiDefns = [RttiDefn].

:- func erlang_type_ctor_rep(erlang_type_ctor_details) = elds_expr.

erlang_type_ctor_rep(erlang_du(_)) =
    elds_term(make_enum_alternative("du")).
erlang_type_ctor_rep(erlang_list(_)) =
    elds_term(make_enum_alternative("list")).
erlang_type_ctor_rep(erlang_eqv(_)) =
    elds_term(make_enum_alternative("eqv")).
erlang_type_ctor_rep(erlang_builtin(builtin_ctor_int)) =
    elds_term(make_enum_alternative("int")).
erlang_type_ctor_rep(erlang_builtin(builtin_ctor_float)) =
    elds_term(make_enum_alternative("float")).
erlang_type_ctor_rep(erlang_builtin(builtin_ctor_char)) =
    elds_term(make_enum_alternative("char")).
erlang_type_ctor_rep(erlang_builtin(builtin_ctor_string)) =
    elds_term(make_enum_alternative("string")).
erlang_type_ctor_rep(erlang_builtin(builtin_ctor_void)) =
    elds_term(make_enum_alternative("void")).
erlang_type_ctor_rep(erlang_builtin(builtin_ctor_c_pointer(is_stable))) =
    elds_term(make_enum_alternative("stable_c_pointer")).
erlang_type_ctor_rep(erlang_builtin(builtin_ctor_c_pointer(is_not_stable))) =
    elds_term(make_enum_alternative("c_pointer")).
erlang_type_ctor_rep(erlang_builtin(builtin_ctor_pred_ctor)) = 
    elds_term(make_enum_alternative("pred")).
erlang_type_ctor_rep(erlang_builtin(builtin_ctor_func_ctor)) = 
    elds_term(make_enum_alternative("func")).
erlang_type_ctor_rep(erlang_builtin(builtin_ctor_tuple)) = 
    elds_term(make_enum_alternative("tuple")).
erlang_type_ctor_rep(erlang_builtin(builtin_ctor_ref)) = 
    elds_term(make_enum_alternative("ref")).
erlang_type_ctor_rep(erlang_builtin(builtin_ctor_type_desc)) = 
    elds_term(make_enum_alternative("type_desc")).
erlang_type_ctor_rep(erlang_builtin(builtin_ctor_pseudo_type_desc)) = 
    elds_term(make_enum_alternative("pseudo_type_desc")).
erlang_type_ctor_rep(erlang_builtin(builtin_ctor_type_ctor_desc)) = 
    elds_term(make_enum_alternative("type_ctor_desc")).

erlang_type_ctor_rep(erlang_impl_artifact(erlang_impl_ctor_hp)) =
    elds_term(make_enum_alternative("hp")).
erlang_type_ctor_rep(erlang_impl_artifact(erlang_impl_ctor_subgoal)) =
    elds_term(make_enum_alternative("subgoal")).
erlang_type_ctor_rep(erlang_impl_artifact(erlang_impl_ctor_ticket)) =
    elds_term(make_enum_alternative("ticket")).

erlang_type_ctor_rep(erlang_impl_artifact(erlang_impl_ctor_type_info)) =
    elds_term(make_enum_alternative("type_info")).
erlang_type_ctor_rep(erlang_impl_artifact(erlang_impl_ctor_type_ctor_info)) =
    elds_term(make_enum_alternative("type_ctor_info")).
erlang_type_ctor_rep(erlang_impl_artifact(erlang_impl_ctor_typeclass_info)) =
    elds_term(make_enum_alternative("typeclass_info")).
erlang_type_ctor_rep(
    erlang_impl_artifact(erlang_impl_ctor_base_typeclass_info)) =
    elds_term(make_enum_alternative("base_typeclass_info")).
erlang_type_ctor_rep(erlang_foreign) =
    elds_term(make_enum_alternative("foreign")).


:- pred gen_init_special_pred(module_info::in, maybe(rtti_proc_label)::in,
    elds_expr::out, prog_varset::in, prog_varset::out) is det.

gen_init_special_pred(ModuleInfo, MaybeRttiProcId, Expr, !VarSet) :-
    (
        MaybeRttiProcId = yes(RttiProcId),
        erl_gen_special_pred_wrapper(ModuleInfo, RttiProcId, Expr, !VarSet)
    ;
        MaybeRttiProcId = no,
        unexpected(this_file,
            "gen_init_special_pred: no special pred")
    ).
    
:- pred erl_gen_special_pred_wrapper(module_info::in, rtti_proc_label::in,
    elds_expr::out, prog_varset::in, prog_varset::out) is det.

erl_gen_special_pred_wrapper(ModuleInfo, RttiProcId, WrapperFun, !VarSet) :-
    PredId = RttiProcId ^ pred_id,
    ProcId = RttiProcId ^ proc_id,
    ArgTypes = RttiProcId ^ proc_arg_types,
    ArgModes = RttiProcId ^ proc_arg_modes,
    Detism = RttiProcId ^ proc_interface_detism,

    % Create the variable list.
    svvarset.new_vars(list.length(ArgTypes), Ws, !VarSet),

    % Figure out the input and output variables for the call to the actual
    % special pred implementation.
    erl_gen_arg_list_arg_modes(ModuleInfo, opt_dummy_args,
        Ws, ArgTypes, ArgModes, CallInputArgs, CallOutputArgs),

    % Figure out the input variables and output variables for this wrapper
    % function.
    erl_gen_arg_list_arg_modes(ModuleInfo, no_opt_dummy_args,
        Ws, ArgTypes, ArgModes,
        WrapperInputVars, WrapperOutputVars),

    determinism_to_code_model(Detism, CodeModel),
    WrapperOutputVarsExprs = exprs_from_vars(WrapperOutputVars),
    (
        ( CodeModel = model_det
        ; CodeModel = model_semi
        ),
        % On success we return a tuple of the output arguments of the call.
        SuccessExpr0 = elds_term(elds_tuple(WrapperOutputVarsExprs))
    ;
        CodeModel = model_non,
        unexpected(this_file,
            "erl_gen_special_pred_wrapper: model_non code_model")
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

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "erl_rtti.m".

%-----------------------------------------------------------------------------%
:- end_module erl_rtti.
%-----------------------------------------------------------------------------%
