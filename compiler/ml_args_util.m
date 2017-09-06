%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2017 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: ml_args_util.m.
%
% This module contains utility predicates that classify and process
% predicate arguments.
%
%---------------------------------------------------------------------------%

:- module ml_backend.ml_args_util.
:- interface.

:- import_module hlds.
:- import_module hlds.code_model.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module ml_backend.ml_gen_info.
:- import_module ml_backend.mlds.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module list.
:- import_module map.

%---------------------------------------------------------------------------%
%
% Various utility routines used for MLDS code generation.
%

:- type solo_or_tscc
    --->    sot_solo
    ;       sot_tscc.

    % Append an appropriate `return' statement for the given code_model
    % and returning the given lvals, if needed.
    %
:- pred ml_append_return_statement(code_model::in, solo_or_tscc::in,
    list(mlds_lval)::in, prog_context::in,
    list(mlds_stmt)::in, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

%---------------------------------------------------------------------------%
%
% Routines for generating function declarations (i.e. mlds_func_params).
%

% Note that when generating function *definitions*, the versions that take
% an ml_gen_info pair should be used, since those are the only ones that will
% generate the correct GC tracing code for the parameters.

    % Generate the function prototype for the given procedure.
    %
:- func ml_gen_proc_params_no_gc_stmts(module_info, pred_proc_id)
    = mlds_func_params.
:- pred ml_gen_info_proc_params(pred_proc_id::in, mlds_func_params::out,
    ml_gen_info::in, ml_gen_info::out) is det.

    % Generate the function prototype for *only the input arguments*
    % of the given procedure.
    %
:- func ml_gen_proc_params_inputs_only_no_gc_stmts(module_info, pred_proc_id)
    = list(mlds_argument).

    % As above, but from the rtti_proc_id rather than from the module_info,
    % pred_id, and proc_id.
    %
:- func ml_gen_proc_params_from_rtti_no_gc_stmts(module_info, rtti_proc_label)
    = mlds_func_params.

    % Generate the function prototype for a procedure with the
    % given argument names, types, modes, and code model.
    %
:- func ml_gen_params_no_gc_stmts(module_info, list(mlds_local_var_name),
    list(mer_type), list(mer_mode), pred_or_func, code_model)
    = mlds_func_params.
:- pred ml_gen_info_params(list(mlds_local_var_name)::in, list(mer_type)::in,
    list(mer_mode)::in, pred_or_func::in, code_model::in,
    mlds_func_params::out, ml_gen_info::in, ml_gen_info::out) is det.

    % ml_gen_tscc_arg_decls(ModuleInfo, Vars, Types, Modes, VarSet, Context,
    %     ProcIdInTscc, NextInArgNum0, NextOutArgNum0, NumOutputs,
    %     !OutArgNames, TsccInArgs, TsccInLocalVarDefns, TsccArgs,
    %     OwnLocalVarDefns, CopyTsccToOwnStmts):
    %
    % Given a procedure in a TSCC and information about its argument list
    % in the form of the arguments' variable names (Vars), types (Types),
    % and modes (Modes), return the information the code generator needs
    %
    % - to generate code for tail calls to this procedure, from other
    %   procedures in the TSCC as well from itself, and
    % - to generate parameter passing code in the procedure's prologue.
    %
    % The former is needed before we can start generating code for *any*
    % of the procedures in the TSCC.
    % 
    % Parameter passing between the procedures in a TSCC is done by
    % giving each procedure its own list of lvn_tscc_proc_input_vars
    % (representing the formal input parameters) and having each tail call
    % assign the actual input parameters to these. Results are returned
    % through lvn_tscc_output_vars, each of which contains the address
    % of the memory slot where the result should be stored.
    % Both lvn_tscc_proc_input_vars and lvn_tscc_output_vars include
    % an argument sequence number, with separate sequences for input and
    % output arguments. These sequences start at 1, so all top level calls
    % to ml_gen_tscc_arg_decls should pass 1 as both NextInArgNum0 and
    % NextOutArgNum0. ml_gen_tscc_arg_decls returns the total number of
    % output arguments, to allow our caller to do a sanity check (requiring
    % all procedures in a TSCC to have the same number of output arguments).
    %
    % ml_gen_tscc_arg_decls will record the names of the output arguments
    % in !OutArgNames, if the initial value of !.OutArgNames contains no
    % such record.
    %
    % The value returned in TsccInArgs will be a list of the
    % lvn_tscc_proc_input_vars of the input arguments only. This is used
    % during the generation of code for tailcalls.
    %
    % The value returned in TsccInLocalVarDefns will be a local var definition
    % of each variable in TsccInArgs. This is used to define those variables
    % in copies of the TSCC code that are *not* for this procedure.
    %
    % The value returned in TsccArgs is a list of all the arguments of the
    % procedure, both input and output, in declaration order. This will be
    % the argument list of the copy of the TSCC for this procedure,
    % and thus it will define the lvn_tscc_proc_input_vars in that copy.
    % (Thus the lvn_tscc_proc_input_vars will be defined by exactly one of
    % TsccInLocalVarDefns and TsccInArgs in each copy.)
    %
    % Each procedure's part in the TSCC defines local MLDS variables for each
    % argument, both input and output (returned in OwnLocalVarDefns), and
    % its code starts by copying the values of each lvn_tscc_proc_input_var
    % *and* lvn_tscc_output_var to the local var (from OwnLocalVarDefns)
    % corresponding to them. For input argument, this copies the value;
    % for output arguments, this copies the address where the argument's value
    % should be put. These assignments are returned in CopyTsccToOwnStmts.
    %
:- pred ml_gen_tscc_arg_decls(module_info::in,
    list(mlds_local_var_name)::in(list_skel(lvn_prog_var)),
    list(mer_type)::in, list(top_functor_mode)::in, prog_varset::in,
    prog_context::in, proc_id_in_tscc::in, int::in, int::in, int::out,
    map(int, string)::in, map(int, string)::out,
    list(mlds_argument)::out, list(mlds_local_var_defn)::out,
    list(mlds_argument)::out, list(mlds_local_var_defn)::out,
    list(mlds_stmt)::out) is det.

%---------------------------------------------------------------------------%
%
% Generate code for parameter passing.
%

:- type what_params
    --->    input_params_only
    ;       input_and_output_params.

    % Generate rvals and lvals for the arguments of a procedure call.
    %
:- pred ml_gen_args(list(mlds_local_var_name)::in, list(mlds_lval)::in,
    list(mer_type)::in, list(mer_type)::in, list(mer_mode)::in,
    pred_or_func::in, code_model::in, prog_context::in,
    bool::in, what_params::in, int::in,
    list(mlds_rval)::out, list(mlds_lval)::out, list(mlds_type)::out,
    list(mlds_local_var_defn)::out, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.program_representation.
:- import_module ml_backend.ml_accurate_gc.
:- import_module ml_backend.ml_code_util.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_util.

:- import_module int.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%
%
% Code for various utility routines.
%

ml_append_return_statement(CodeModel, SoloOrTscc, CopiedOutputVarLvals,
        Context, !Stmts, !Info) :-
    (
        CodeModel = model_semi,
        ml_gen_test_success(SucceededRval, !Info),
        CopiedOutputVarRvals = list.map(func(Lval) = ml_lval(Lval),
            CopiedOutputVarLvals),
        ReturnedRvals = [SucceededRval | CopiedOutputVarRvals],
        ReturnStmt = ml_stmt_return(ReturnedRvals, Context),
        !:Stmts = !.Stmts ++ [ReturnStmt]
    ;
        CodeModel = model_det,
        (
            CopiedOutputVarLvals = [_ | _],
            CopiedOutputVarRvals = list.map(func(Lval) = ml_lval(Lval),
                CopiedOutputVarLvals),
            ReturnStmt = ml_stmt_return(CopiedOutputVarRvals, Context),
            !:Stmts = !.Stmts ++ [ReturnStmt]
        ;
            CopiedOutputVarLvals = [],
            % This return statement is not needed in the usual case
            % where the code we generate for a HLDS procedure is the
            % entirety of an MLDS function (when our caller should pass
            % sot_solo), since the end of the function acts as an implicit
            % return, but it *is* needed when the MLDS function also contains
            % the code of other HLDS procedures in the same TSCC (when our
            % caller should pass sot_tscc).
            %
            % Note that adding a return statement after the body of a procedure
            % that throws an exception may, if this fact is visible to the
            % target language compiler, cause that compiler to generate an
            % error. The Java compiler does this when the body of a HLDS
            % procedure is defined by Java code that does a throw.
            (
                SoloOrTscc = sot_solo
            ;
                SoloOrTscc = sot_tscc,
                ReturnStmt = ml_stmt_return([], Context),
                !:Stmts = !.Stmts ++ [ReturnStmt]
            )
        )
    ;
        CodeModel = model_non
    ).

%---------------------------------------------------------------------------%
%
% Code for generating function declarations (i.e. mlds_func_params).
%

:- pred get_raw_data_for_proc_params(module_info::in, pred_proc_id::in,
    pred_info::out, list(mer_type)::out, list(top_functor_mode)::out,
    list(mlds_local_var_name)::out, pred_or_func::out, code_model::out) is det.

get_raw_data_for_proc_params(ModuleInfo, PredProcId, PredInfo, HeadTypes,
        TopFunctorModes, HeadVarNames, PredOrFunc, CodeModel) :-
    module_info_pred_proc_info(ModuleInfo, PredProcId, PredInfo, ProcInfo),
    proc_info_get_headvars(ProcInfo, HeadVars),
    pred_info_get_arg_types(PredInfo, HeadTypes),
    proc_info_get_argmodes(ProcInfo, HeadModes),
    modes_to_top_functor_modes(ModuleInfo, HeadModes, HeadTypes,
        TopFunctorModes),
    proc_info_get_varset(ProcInfo, VarSet),
    HeadVarNames = ml_gen_local_var_names(VarSet, HeadVars),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    CodeModel = proc_info_interface_code_model(ProcInfo).

%---------------------%

ml_gen_proc_params_no_gc_stmts(ModuleInfo, PredProcId) = FuncParams :-
    get_raw_data_for_proc_params(ModuleInfo, PredProcId, _PredInfo, HeadTypes,
        TopFunctorModes, HeadVarNames, PredOrFunc, CodeModel),
    ml_gen_params_base(ModuleInfo, HeadVarNames, HeadTypes, TopFunctorModes,
        PredOrFunc, CodeModel, input_and_output_params, FuncParams, no, _).

ml_gen_info_proc_params(PredProcId, FuncParams, !Info) :-
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    get_raw_data_for_proc_params(ModuleInfo, PredProcId, PredInfo, HeadTypes,
        TopFunctorModes, HeadVarNames, PredOrFunc, CodeModel),
    % We must not generate GC tracing code for no_type_info_builtin procedures,
    % because the generated GC tracing code would refer to type_infos
    % that don't get passed.
    PredModule = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    PredArity = pred_info_orig_arity(PredInfo),
    ( if no_type_info_builtin(PredModule, PredName, PredArity) then
        ml_gen_params_base(ModuleInfo, HeadVarNames, HeadTypes,
            TopFunctorModes, PredOrFunc, CodeModel, input_and_output_params,
            FuncParams, no, _)
    else
        ml_gen_params_base(ModuleInfo, HeadVarNames, HeadTypes,
            TopFunctorModes, PredOrFunc, CodeModel, input_and_output_params,
            FuncParams, yes(!.Info), yes(!:Info))
    ).

ml_gen_proc_params_inputs_only_no_gc_stmts(ModuleInfo, PredProcId)
        = FuncArgs :-
    get_raw_data_for_proc_params(ModuleInfo, PredProcId, _PredInfo, HeadTypes,
        TopFunctorModes, HeadVarNames, _PredOrFunc, CodeModel),
    module_info_get_globals(ModuleInfo, Globals),
    CopyOut = get_copy_out_option(Globals, CodeModel),
    ml_gen_arg_decls(ModuleInfo, HeadVarNames, HeadTypes, TopFunctorModes,
        CopyOut, input_params_only, FuncArgs, RetTypes, no, _),
    expect(unify(RetTypes, []), $pred, "RetTypes != []").

ml_gen_proc_params_from_rtti_no_gc_stmts(ModuleInfo, RttiProcId)
        = FuncParams :-
    HeadVars = RttiProcId ^ rpl_proc_headvars,
    ArgTypes = RttiProcId ^ rpl_proc_arg_types,
    TopFunctorModes = RttiProcId ^ rpl_proc_top_modes,
    PredOrFunc = RttiProcId ^ rpl_pred_or_func,
    Detism = RttiProcId ^ rpl_proc_interface_detism,
    determinism_to_code_model(Detism, CodeModel),
    HeadVarNames = list.map(
        ( func(Var - Name) = Result :-
            term.var_to_int(Var, N),
            Result = lvn_prog_var(Name, N)
        ), HeadVars),
    ml_gen_params_base(ModuleInfo, HeadVarNames, ArgTypes, TopFunctorModes,
        PredOrFunc, CodeModel, input_and_output_params, FuncParams, no, _).

ml_gen_params_no_gc_stmts(ModuleInfo, HeadVarNames, HeadTypes, HeadModes,
        PredOrFunc, CodeModel) = FuncParams :-
    modes_to_top_functor_modes(ModuleInfo, HeadModes, HeadTypes,
        TopFunctorModes),
    ml_gen_params_base(ModuleInfo, HeadVarNames, HeadTypes, TopFunctorModes,
        PredOrFunc, CodeModel, input_and_output_params, FuncParams, no, _).

ml_gen_info_params(HeadVarNames, HeadTypes, HeadModes,
        PredOrFunc, CodeModel, FuncParams, !Info) :-
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    modes_to_top_functor_modes(ModuleInfo, HeadModes, HeadTypes,
        TopFunctorModes),
    ml_gen_params_base(ModuleInfo, HeadVarNames, HeadTypes, TopFunctorModes,
        PredOrFunc, CodeModel, input_and_output_params, FuncParams,
        yes(!.Info), yes(!:Info)).

:- inst is_in for what_params/0
    --->    input_params_only.
:- inst is_inout for what_params/0
    --->    input_and_output_params.

:- inst is_no for maybe/1
    --->    no.
:- inst is_yes for maybe/1
    --->    yes(ground).

    % The mlds_arguments in the generated parameters have the gc stmt field
    % filled in only if !.MaybeInfo is yes(_).
    %
:- pred ml_gen_params_base(module_info, list(mlds_local_var_name),
    list(mer_type), list(top_functor_mode), pred_or_func,
    code_model, what_params,
    mlds_func_params, maybe(ml_gen_info), maybe(ml_gen_info)).
:- mode ml_gen_params_base(in, in, in, in, in, in, in(is_in), out,
    in(is_no), out(is_no)) is det.
:- mode ml_gen_params_base(in, in, in, in, in, in, in(is_inout), out,
    in(is_no), out(is_no)) is det.
:- mode ml_gen_params_base(in, in, in, in, in, in, in(is_inout), out,
    in(is_yes), out(is_yes)) is det.

ml_gen_params_base(ModuleInfo, HeadVarNames, HeadTypes, HeadModes, PredOrFunc,
        CodeModel, WhatParams, FuncParams, !MaybeInfo) :-
    module_info_get_globals(ModuleInfo, Globals),
    CopyOut = get_copy_out_option(Globals, CodeModel),
    ml_gen_arg_decls(ModuleInfo, HeadVarNames, HeadTypes, HeadModes,
        CopyOut, WhatParams, FuncArgs0, RetTypes0, !MaybeInfo),
    (
        CodeModel = model_det,
        % For model_det Mercury functions whose result argument has an
        % output mode, make the result into the MLDS return type.
        ( if
            RetTypes0 = [],
            PredOrFunc = pf_function,
            pred_args_to_func_args(HeadModes, _, ResultMode),
            ResultMode = top_out,
            pred_args_to_func_args(HeadTypes, _, ResultType),
            check_dummy_type(ModuleInfo, ResultType) = is_not_dummy_type
        then
            pred_args_to_func_args(FuncArgs0, FuncArgs, RetArg),
            RetArg = mlds_argument(_RetArgName, RetTypePtr, _GCStmt),
            ( if RetTypePtr = mlds_ptr_type(RetType) then
                RetTypes = [RetType]
            else
                unexpected($pred,
                    "output mode function result doesn't have pointer type")
            )
        else
            FuncArgs = FuncArgs0,
            RetTypes = RetTypes0
        )
    ;
        CodeModel = model_semi,
        % For model_semi procedures, return a bool.
        FuncArgs = FuncArgs0,
        RetTypes = [mlds_native_bool_type | RetTypes0]
    ;
        CodeModel = model_non,
        % For model_non procedures, we return values by passing them
        % to the continuation.
        (
            CopyOut = yes,
            ContType = mlds_cont_type(RetTypes0),
            RetTypes = []
        ;
            CopyOut = no,
            ContType = mlds_cont_type([]),
            RetTypes = RetTypes0
        ),
        ContVarName = lvn_comp_var(lvnc_cont),
        % The cont variable always points to code, not to the heap,
        % so the GC never needs to trace it.
        ContGCStmt = gc_no_stmt,
        ContArg = mlds_argument(ContVarName, ContType, ContGCStmt),
        ContEnvType = mlds_generic_env_ptr_type,
        ContEnvVarName = lvn_comp_var(lvnc_cont_env_ptr),
        % The cont_env_ptr always points to the stack, since continuation
        % environments are always allocated on the stack (unless
        % put_nondet_env_on_heap is true, which won't be the case when doing
        % our own GC -- this is enforced in handle_options.m).
        % So the GC doesn't need to trace it.
        ContEnvGCStmt = gc_no_stmt,
        ContEnvArg = mlds_argument(ContEnvVarName, ContEnvType,
            ContEnvGCStmt),
        globals.lookup_bool_option(Globals, gcc_nested_functions,
            NestedFunctions),
        (
            NestedFunctions = yes,
            FuncArgs = FuncArgs0 ++ [ContArg]
        ;
            NestedFunctions = no,
            FuncArgs = FuncArgs0 ++ [ContArg, ContEnvArg]
        )
    ),
    FuncParams = mlds_func_params(FuncArgs, RetTypes).

    % Given the argument variable names, and corresponding lists of their
    % types and modes, generate the MLDS argument declarations
    % and return types.
    %
    % The generated mlds_arguments have the gc stmt field filled in
    % only if !.MaybeInfo is yes(_).
    %
:- pred ml_gen_arg_decls(module_info, list(mlds_local_var_name),
    list(mer_type), list(top_functor_mode), bool, what_params,
    list(mlds_argument), mlds_return_types,
    maybe(ml_gen_info), maybe(ml_gen_info)).
:- mode ml_gen_arg_decls(in, in, in, in, in, in(is_in), out, out,
    in(is_no), out(is_no)) is det.
:- mode ml_gen_arg_decls(in, in, in, in, in, in(is_inout), out, out,
    in(is_no), out(is_no)) is det.
:- mode ml_gen_arg_decls(in, in, in, in, in, in(is_inout), out, out,
    in(is_yes), out(is_yes)) is det.

ml_gen_arg_decls(ModuleInfo, Vars, Types, Modes, CopyOut, WhatParams,
        FuncArgs, RetTypes, !MaybeInfo) :-
    ( if
        Vars = [],
        Types = [],
        Modes = []
    then
        FuncArgs = [],
        RetTypes = []
    else if
        Vars = [HeadVar | TailVars],
        Types = [HeadType | TailTypes],
        Modes = [HeadMode | TailModes]
    then
        ml_gen_arg_decls(ModuleInfo, TailVars, TailTypes, TailModes,
            CopyOut, WhatParams, TailFuncArgs, TailRetTypes, !MaybeInfo),
        HeadIsDummy = check_dummy_type(ModuleInfo, HeadType),
        (
            HeadIsDummy = is_dummy_type,
            % Exclude types such as io.state, etc.
            FuncArgs = TailFuncArgs,
            RetTypes = TailRetTypes
        ;
            HeadIsDummy = is_not_dummy_type,
            HeadMLDS_Type = mercury_type_to_mlds_type(ModuleInfo, HeadType),
            (
                HeadMode = top_unused,
                % Also exclude values with arg_mode `top_unused'.
                FuncArgs = TailFuncArgs,
                RetTypes = TailRetTypes
            ;
                HeadMode = top_in,
                % For inputs, generate an argument.
                HeadMLDS_ArgType = HeadMLDS_Type,
                ml_gen_arg_decl(HeadVar, HeadType, HeadMLDS_ArgType,
                    HeadFuncArg, !MaybeInfo),
                FuncArgs = [HeadFuncArg | TailFuncArgs],
                RetTypes = TailRetTypes
            ;
                HeadMode = top_out,
                (
                    WhatParams = input_params_only,
                    FuncArgs = TailFuncArgs,
                    RetTypes = TailRetTypes
                ;
                    WhatParams = input_and_output_params,
                    (
                        CopyOut = yes,
                        % For by-value outputs, generate a return type.
                        FuncArgs = TailFuncArgs,
                        RetTypes = [HeadMLDS_Type | TailRetTypes]
                    ;
                        CopyOut = no,
                        % For by-reference outputs, generate an argument.
                        HeadMLDS_ArgType = mlds_ptr_type(HeadMLDS_Type),
                        ml_gen_arg_decl(HeadVar, HeadType, HeadMLDS_ArgType,
                            HeadFuncArg, !MaybeInfo),
                        FuncArgs = [HeadFuncArg | TailFuncArgs],
                        RetTypes = TailRetTypes
                    )
                )
            )
        )
    else
        unexpected($pred, "length mismatch")
    ).

    % Given an argument variable, its actual type and its arg type,
    % generate an MLDS argument declaration for it.
    %
    % The generated mlds_argument has the gc stmt field filled in
    % only if !.MaybeInfo is yes(_).
    %
:- pred ml_gen_arg_decl(mlds_local_var_name, mer_type, mlds_type,
    mlds_argument, maybe(ml_gen_info), maybe(ml_gen_info)).
:- mode ml_gen_arg_decl(in, in, in, out, in(is_no), out(is_no)) is det.
:- mode ml_gen_arg_decl(in, in, in, out, in(is_yes), out(is_yes)) is det.

ml_gen_arg_decl(Var, Type, MLDS_ArgType, FuncArg, !MaybeInfo) :-
    (
        !.MaybeInfo = yes(Info0),
        % XXX We should fill in this Context properly.
        term.context_init(Context),
        ml_gen_gc_statement(Var, Type, Context, GCStmt, Info0, Info),
        !:MaybeInfo = yes(Info)
    ;
        !.MaybeInfo = no,
        GCStmt = gc_no_stmt,
        !:MaybeInfo = no
    ),
    FuncArg = mlds_argument(Var, MLDS_ArgType, GCStmt).

%---------------------%

ml_gen_tscc_arg_decls(ModuleInfo, Vars, Types, Modes, VarSet, Context,
        ProcIdInTscc, NextInArgNum0, NextOutArgNum0, NumOutputs, !OutArgNames,
        TsccInArgs, TsccInLocalVarDefns, TsccArgs, OwnLocalVarDefns,
        CopyTsccToOwnStmts) :-
    ( if
        Vars = [],
        Types = [],
        Modes = []
    then
        NumOutputs = NextOutArgNum0,
        TsccInArgs = [],
        TsccInLocalVarDefns = [],
        TsccArgs = [],
        OwnLocalVarDefns = [],
        CopyTsccToOwnStmts = []
    else if
        Vars = [HeadVar | TailVars],
        Types = [HeadType | TailTypes],
        Modes = [HeadMode | TailModes]
    then
        HeadIsDummy = check_dummy_type(ModuleInfo, HeadType),
        (
            HeadIsDummy = is_dummy_type,
            % Exclude types such as io.state, etc.
            ml_gen_tscc_arg_decls(ModuleInfo, TailVars, TailTypes, TailModes,
                VarSet, Context,
                ProcIdInTscc, NextInArgNum0, NextOutArgNum0, NumOutputs,
                !OutArgNames, TsccInArgs, TsccInLocalVarDefns,
                TsccArgs, OwnLocalVarDefns, CopyTsccToOwnStmts)
        ;
            HeadIsDummy = is_not_dummy_type,
            HeadVarName = ml_local_var_name_to_string(HeadVar),
            HeadMLDS_Type = mercury_type_to_mlds_type(ModuleInfo, HeadType),
            (
                HeadMode = top_unused,
                % Also exclude values with arg_mode `top_unused'.
                ml_gen_tscc_arg_decls(ModuleInfo,
                    TailVars, TailTypes, TailModes,
                    VarSet, Context,
                    ProcIdInTscc, NextInArgNum0, NextOutArgNum0, NumOutputs,
                    !OutArgNames, TsccInArgs, TsccInLocalVarDefns,
                    TsccArgs, OwnLocalVarDefns, CopyTsccToOwnStmts)
            ;
                (
                    HeadMode = top_in,
                    InArgNum = NextInArgNum0,
                    NextInArgNum = NextInArgNum0 + 1,
                    NextOutArgNum = NextOutArgNum0,
                    HeadTsccVar = lvn_tscc_proc_input_var(ProcIdInTscc,
                        InArgNum, HeadVarName),
                    HeadMLDS_ArgType = HeadMLDS_Type,
                    HeadTsccArg = mlds_argument(HeadTsccVar, HeadMLDS_ArgType,
                        gc_no_stmt),
                    HeadTsccInArgs = [HeadTsccArg],
                    HeadTsccInLocalVarDefn = mlds_local_var_defn(HeadTsccVar,
                        Context, HeadMLDS_ArgType, no_initializer, gc_no_stmt),
                    HeadTsccInLocalVarDefns = [HeadTsccInLocalVarDefn]
                ;
                    HeadMode = top_out,
                    OutArgNum = NextOutArgNum0,
                    NextInArgNum = NextInArgNum0,
                    NextOutArgNum = NextOutArgNum0 + 1,
                    ( if
                        map.search(!.OutArgNames, OutArgNum, OldOutArgName)
                    then
                        OutArgName = OldOutArgName
                    else
                        OutArgName = HeadVarName,
                        map.det_insert(OutArgNum, OutArgName, !OutArgNames)
                    ),
                    HeadTsccVar = lvn_tscc_output_var(OutArgNum, OutArgName),
                    HeadMLDS_ArgType = mlds_ptr_type(HeadMLDS_Type),
                    HeadTsccArg = mlds_argument(HeadTsccVar, HeadMLDS_ArgType,
                        gc_no_stmt),
                    HeadTsccInArgs = [],
                    HeadTsccInLocalVarDefns = []
                ),
                HeadOwnLocalVarDefn = mlds_local_var_defn(HeadVar, Context,
                    HeadMLDS_ArgType, no_initializer, gc_no_stmt),
                HeadCopyTsccToOwnStmt = ml_stmt_atomic(
                    assign(ml_local_var(HeadVar, HeadMLDS_ArgType),
                        ml_lval(ml_local_var(HeadTsccVar, HeadMLDS_ArgType))),
                    Context),
                ml_gen_tscc_arg_decls(ModuleInfo,
                    TailVars, TailTypes, TailModes,
                    VarSet, Context,
                    ProcIdInTscc, NextInArgNum, NextOutArgNum, NumOutputs,
                    !OutArgNames, TailTsccInArgs, TailTsccInLocalVarDefns,
                    TailTsccArgs, TailOwnLocalVarDefns,
                    TailCopyTsccToOwnStmts),
                TsccInArgs = HeadTsccInArgs ++ TailTsccInArgs,
                TsccInLocalVarDefns =
                    HeadTsccInLocalVarDefns ++ TailTsccInLocalVarDefns,
                TsccArgs = [HeadTsccArg | TailTsccArgs],
                OwnLocalVarDefns =
                    [HeadOwnLocalVarDefn | TailOwnLocalVarDefns],
                CopyTsccToOwnStmts =
                    [HeadCopyTsccToOwnStmt | TailCopyTsccToOwnStmts]
            )
        )
    else
        unexpected($pred, "length mismatch")
    ).

%---------------------------------------------------------------------------%
%
% Generate code for parameter passing.
%

ml_gen_args(VarNames, VarLvals, CallerTypes, CalleeTypes, Modes,
        PredOrFunc, CodeModel, Context, ForClosureWrapper, WhatParams, ArgNum,
        !:InputRvals, !:OutputLvals, !:OutputTypes,
        !:ConvOutputDefns, !:ConvOutputStmts, !Info) :-
    ( if
        VarNames = [],
        VarLvals = [],
        CallerTypes = [],
        CalleeTypes = [],
        Modes = []
    then
        !:InputRvals = [],
        !:OutputLvals = [],
        !:OutputTypes = [],
        !:ConvOutputDefns = [],
        !:ConvOutputStmts = []
    else if
        VarNames = [VarName | VarNamesTail],
        VarLvals = [VarLval | VarLvalsTail],
        CallerTypes = [CallerType | CallerTypesTail],
        CalleeTypes = [CalleeType | CalleeTypesTail],
        Modes = [Mode | ModesTail]
    then
        ml_gen_args(VarNamesTail, VarLvalsTail, CallerTypesTail,
            CalleeTypesTail, ModesTail, PredOrFunc, CodeModel, Context,
            ForClosureWrapper, WhatParams, ArgNum + 1,
            !:InputRvals, !:OutputLvals, !:OutputTypes,
            !:ConvOutputDefns, !:ConvOutputStmts, !Info),
        ml_gen_arg(VarName, VarLval, CallerType, CalleeType, Mode,
            PredOrFunc, CodeModel, Context, ForClosureWrapper, WhatParams,
            ArgNum, VarNamesTail,
            !InputRvals, !OutputLvals, !OutputTypes,
            !ConvOutputDefns, !ConvOutputStmts, !Info)
    else
        unexpected($pred, "length mismatch")
    ).

:- pred ml_gen_arg(mlds_local_var_name::in, mlds_lval::in, mer_type::in,
    mer_type::in, mer_mode::in, pred_or_func::in, code_model::in,
    prog_context::in, bool::in, what_params::in, int::in,
    list(mlds_local_var_name)::in,
    list(mlds_rval)::in, list(mlds_rval)::out,
    list(mlds_lval)::in, list(mlds_lval)::out,
    list(mlds_type)::in, list(mlds_type)::out,
    list(mlds_local_var_defn)::in, list(mlds_local_var_defn)::out,
    list(mlds_stmt)::in, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.
:- pragma inline(ml_gen_arg/24).

ml_gen_arg(VarName, VarLval, CallerType, CalleeType, Mode,
        PredOrFunc, CodeModel, Context, ForClosureWrapper, WhatParams,
        ArgNum, VarNamesTail,
        !InputRvals, !OutputLvals, !OutputTypes,
        !ConvOutputDefns, !ConvOutputStmts, !Info) :-
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    mode_to_top_functor_mode(ModuleInfo, Mode, CalleeType, ArgTopFunctorMode),
    CalleeIsDummy = check_dummy_type(ModuleInfo, CalleeType),
    (
        CalleeIsDummy = is_dummy_type
        % Exclude arguments of type io.state etc.
    ;
        CalleeIsDummy = is_not_dummy_type,
        (
            ArgTopFunctorMode = top_unused
            % Also exclude those with arg_mode `top_unused'.
        ;
            ArgTopFunctorMode = top_in,
            % It is an input argument.
            CallerIsDummy = check_dummy_type(ModuleInfo, CallerType),
            (
                CallerIsDummy = is_dummy_type,
                % The variable may not have been declared, so we need to
                % generate a dummy value for it. Using `0' here is more
                % efficient than using private_builtin.dummy_var, which is
                % what ml_gen_var will have generated for this variable.
                VarRval = ml_const(mlconst_int(0))
            ;
                CallerIsDummy = is_not_dummy_type,
                VarRval = ml_lval(VarLval)
            ),
            ml_gen_box_or_unbox_rval(ModuleInfo, CallerType, CalleeType,
                bp_native_if_possible, VarRval, ArgRval),
            !:InputRvals = [ArgRval | !.InputRvals]
        ;
            ArgTopFunctorMode = top_out,
            % It is an output argument.
            (
                WhatParams = input_and_output_params,
                ml_gen_box_or_unbox_lval(CallerType, CalleeType,
                    bp_native_if_possible, VarLval, VarName, Context,
                    ForClosureWrapper, ArgNum, ArgLval, ThisArgConvDecls,
                    _ThisArgConvInput, ThisArgConvOutput, !Info),
                !:ConvOutputDefns = ThisArgConvDecls ++ !.ConvOutputDefns,
                !:ConvOutputStmts = ThisArgConvOutput ++ !.ConvOutputStmts,

                ( if
                    (
                        % If this is the result argument of a model_det
                        % function, and it has an output mode (tested above),
                        % then return it as a value.
                        VarNamesTail = [],
                        CodeModel = model_det,
                        PredOrFunc = pf_function
                    ;
                        % If the target language allows multiple return values,
                        % then use them.
                        ml_gen_info_get_copy_out(!.Info, CodeModel, CopyOut),
                        CopyOut = yes
                    )
                then
                    !:OutputLvals = [ArgLval | !.OutputLvals],
                    ml_gen_type(!.Info, CalleeType, OutputType),
                    !:OutputTypes = [OutputType | !.OutputTypes]
                else
                    % Otherwise use the traditional C style of passing
                    % the address where the output value should be put.
                    !:InputRvals = [ml_gen_mem_addr(ArgLval) | !.InputRvals]
                )
            ;
                WhatParams = input_params_only
                % For tail calls, we ignore the output arguments.
                % The callee's output arguments are known to be exactly
                % the same as the caller's output arguments, and will be
                % set by the non-recursive paths through the procedure body.
                %
                % The reason why our caller cannot just ignore the value of
                % !:OutputLvals and !:OutputTypes, and why we need the
                % WhatParams argument, is the addition of the address
                % of ArgLval to !:InputRvals above.
            )
        )
    ).

    % ml_gen_mem_addr(Lval) returns a value equal to &Lval. For the case
    % where Lval = *Rval for some Rval, we optimize &*Rval to just Rval.
    %
:- func ml_gen_mem_addr(mlds_lval) = mlds_rval.

ml_gen_mem_addr(Lval) =
    (if Lval = ml_mem_ref(Rval, _) then Rval else ml_mem_addr(Lval)).

%---------------------------------------------------------------------------%
%
% Miscellaneous routines.
%

    % Get the value of the appropriate --det-copy-out or --nondet-copy-out
    % option, depending on the code model.
    %
:- func get_copy_out_option(globals, code_model) = bool.

get_copy_out_option(Globals, CodeModel) = CopyOut :-
    (
        CodeModel = model_non,
        globals.lookup_bool_option(Globals, nondet_copy_out, CopyOut)
    ;
        ( CodeModel = model_det
        ; CodeModel = model_semi
        ),
        globals.lookup_bool_option(Globals, det_copy_out, CopyOut)
    ).

%---------------------------------------------------------------------------%
:- end_module ml_backend.ml_args_util.
%---------------------------------------------------------------------------%
