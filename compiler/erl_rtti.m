%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: erl_rtti.m.
% Main author: wangp.
% 
% This module converts from the back-end-independent RTTI data structures into
% ELDS function definitions.
%
% XXX currently we only do enough to allow type classes to work
%
%-----------------------------------------------------------------------------%

:- module erl_backend.erl_rtti.
:- interface.

:- import_module backend_libs.rtti.
:- import_module erl_backend.elds.
:- import_module hlds.hlds_module.

:- import_module list.

%-----------------------------------------------------------------------------%

:- pred rtti_data_list_to_elds(module_info::in, list(rtti_data)::in,
    list(elds_rtti_defn)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module erl_backend.erl_call_gen.
:- import_module erl_backend.erl_code_util.
:- import_module hlds.code_model.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module libs.compiler_util.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module int.
:- import_module maybe.
:- import_module svvarset.
:- import_module varset.

%-----------------------------------------------------------------------------%

rtti_data_list_to_elds(ModuleInfo, RttiDatas, RttiDefns) :-
    list.map(rtti_data_to_elds(ModuleInfo), RttiDatas, RttiDefns0),
    RttiDefns = list.condense(RttiDefns0).

:- pred rtti_data_to_elds(module_info::in, rtti_data::in,
    list(elds_rtti_defn)::out) is det.

rtti_data_to_elds(ModuleInfo, RttiData, [RttiDefn]) :-
    RttiData = rtti_data_base_typeclass_info(TCName, InstanceModule,
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
    RttiData = rtti_data_type_info(_TypeInfo).
rtti_data_to_elds(_ModuleInfo, RttiData, []) :-
    RttiData = rtti_data_pseudo_type_info(_PseudoTypeInfo).
rtti_data_to_elds(_ModuleInfo, RttiData, []) :-
    RttiData = rtti_data_type_class_decl(_TCDecl).
rtti_data_to_elds(_ModuleInfo, RttiData, []) :-
    RttiData = rtti_data_type_class_instance(_Instance).
rtti_data_to_elds(_ModuleInfo, RttiData, []) :-
    RttiData = rtti_data_type_ctor_info(_TypeCtorData).

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

:- func this_file = string.

this_file = "erl_rtti.m".

%-----------------------------------------------------------------------------%
:- end_module erl_rtti.
%-----------------------------------------------------------------------------%
