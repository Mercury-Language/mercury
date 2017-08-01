%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1999-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: ml_call_gen.m.
% Main author: fjh.
%
% This module is part of the MLDS code generator. It handles code generation
% for both generic and plain procedure calls, and calls to builtins.
%
%---------------------------------------------------------------------------%

:- module ml_backend.ml_call_gen.
:- interface.

:- import_module hlds.
:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module ml_backend.ml_gen_info.
:- import_module ml_backend.mlds.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module list.

%---------------------------------------------------------------------------%

    % Generate MLDS code for an HLDS generic_call goal.
    % This includes boxing/unboxing the arguments if necessary.
    %
:- pred ml_gen_generic_call(generic_call::in, list(prog_var)::in,
    list(mer_mode)::in, determinism::in, prog_context::in,
    list(mlds_local_var_defn)::out, list(mlds_function_defn)::out,
    list(mlds_stmt)::out, ml_gen_info::in, ml_gen_info::out) is det.

    % ml_gen_plain_call(PredId, ProcId, ArgNames, ArgLvals, ArgTypes,
    %   CodeModel, Context, ForClosureWrapper, Defns, Stmts):
    %
    % Generate MLDS code for a plain HLDS procedure call, making sure to
    % box/unbox the arguments if necessary.
    %
    % If ForClosureWrapper = for_closure_wrapper, then the type_info
    % for type variables in CallerType may not be available in the current
    % procedure, so the GC tracing code for temps introduced for
    % boxing/unboxing (if any) should obtain the type_info from the
    % corresponding entry in the `type_params' local.
    %
:- pred ml_gen_plain_call(pred_id::in, proc_id::in,
    list(mlds_local_var_name)::in, list(mlds_lval)::in, list(mer_type)::in,
    code_model::in, prog_context::in, bool::in,
    list(mlds_local_var_defn)::out, list(mlds_function_defn)::out,
    list(mlds_stmt)::out, ml_gen_info::in, ml_gen_info::out) is det.

    % Generate MLDS code for a call to a builtin procedure.
    %
:- pred ml_gen_builtin(pred_id::in, proc_id::in, list(prog_var)::in,
    code_model::in, prog_context::in,
    list(mlds_local_var_defn)::out, list(mlds_function_defn)::out,
    list(mlds_stmt)::out, ml_gen_info::in, ml_gen_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module check_hlds.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_module.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module ml_backend.ml_code_util.
:- import_module parse_tree.prog_data_foreign.

:- import_module int.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module term.

%---------------------------------------------------------------------------%
%
% Code for generic calls.
%

ml_gen_generic_call(GenericCall, ArgVars, ArgModes, Determinism, Context,
        LocalVarDefns, FuncDefns, Stmts, !Info) :-
    % XXX For typeclass method calls, we do some unnecessary
    % boxing/unboxing of the arguments.
    (
        GenericCall = higher_order(_, _, _, _),
        ml_gen_main_generic_call(GenericCall, ArgVars, ArgModes, Determinism,
            Context, LocalVarDefns, FuncDefns, Stmts, !Info)
    ;
        GenericCall = class_method(_, _, _, _),
        ml_gen_main_generic_call(GenericCall, ArgVars, ArgModes, Determinism,
            Context, LocalVarDefns, FuncDefns, Stmts, !Info)
    ;
        GenericCall = event_call(_),
        % XXX For now, we can't generate events from the MLDS backend.
        LocalVarDefns = [],
        FuncDefns = [],
        Stmts = []
    ;
        GenericCall = cast(_),
        ml_gen_cast(Context, ArgVars, LocalVarDefns, FuncDefns, Stmts, !Info)
    ).

:- inst main_generic_call
    --->    higher_order(ground, ground, ground, ground)
    ;       class_method(ground, ground, ground, ground).

:- pred ml_gen_main_generic_call(generic_call::in(main_generic_call),
    list(prog_var)::in, list(mer_mode)::in, determinism::in, prog_context::in,
    list(mlds_local_var_defn)::out, list(mlds_function_defn)::out,
    list(mlds_stmt)::out, ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_main_generic_call(GenericCall, ArgVars, ArgModes, Determinism, Context,
        LocalVarDefns, FuncDefns, Stmts, !Info) :-
    % Allocate some fresh type variables to use as the Mercury types
    % of the boxed arguments.
    NumArgs = list.length(ArgVars),
    BoxedArgTypes = ml_make_boxed_types(NumArgs),

    % Create the boxed parameter types for the called function.
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    ml_gen_info_get_varset(!.Info, VarSet),
    ArgNames = ml_gen_local_var_names(VarSet, ArgVars),
    PredOrFunc = generic_call_pred_or_func(GenericCall),
    determinism_to_code_model(Determinism, CodeModel),
    Params0 = ml_gen_params(ModuleInfo, ArgNames,
        BoxedArgTypes, ArgModes, PredOrFunc, CodeModel),

    % Insert the `closure_arg' parameter.
    %
    % The GCStmt for `closure_arg' here is wrong, but it doesn't matter,
    % since `closure_arg' is only part of a type (a function parameter in the
    % function type). We won't use the GC tracing code generated here, since
    % we don't generate any actual local variable or parameter for
    % `closure_arg'.
    GCStmt = gc_no_stmt,
    ClosureArgType = mlds_generic_type,
    ClosureArg = mlds_argument(lvn_comp_var(lvnc_closure_arg), ClosureArgType,
        GCStmt),
    Params0 = mlds_func_params(ArgParams0, RetParam),
    Params = mlds_func_params([ClosureArg | ArgParams0], RetParam),
    Signature = mlds_get_func_signature(Params),

    % Compute the function address.
    (
        GenericCall = higher_order(ClosureVar, _Purity, _PredOrFunc, _Arity),
        ml_gen_var(!.Info, ClosureVar, ClosureLval),
        FieldId = ml_field_offset(ml_const(mlconst_int(1))),
        % XXX are these types right?
        FuncLval = ml_field(yes(0), ml_lval(ClosureLval), FieldId,
            mlds_generic_type, ClosureArgType),
        FuncType = mlds_func_type(Params),
        FuncRval = ml_unop(unbox(FuncType), ml_lval(FuncLval))
    ;
        GenericCall = class_method(TypeClassInfoVar, MethodNum,
            _ClassId, _PredName),

        % Create the lval for the typeclass_info, which is also the closure
        % in this case.
        ml_gen_var(!.Info, TypeClassInfoVar, TypeClassInfoLval),
        ClosureLval = TypeClassInfoLval,

        % Extract the base_typeclass_info from the typeclass_info.
        BaseTypeclassInfoFieldId = ml_field_offset(ml_const(mlconst_int(0))),
        BaseTypeclassInfoLval = ml_field(yes(0),
            ml_lval(TypeClassInfoLval), BaseTypeclassInfoFieldId,
            mlds_generic_type, ClosureArgType),

        % Extract the method address from the base_typeclass_info.
        Offset = ml_base_typeclass_info_method_offset,
        MethodFieldNum = MethodNum + Offset,
        MethodFieldId = ml_field_offset(ml_const(mlconst_int(MethodFieldNum))),
        FuncLval = ml_field(yes(0), ml_lval(BaseTypeclassInfoLval),
            MethodFieldId, mlds_generic_type, mlds_generic_type),
        FuncType = mlds_func_type(Params),
        FuncRval = ml_unop(unbox(FuncType), ml_lval(FuncLval))
    ),

    % Assign the function address rval to a new local variable. This makes
    % the generated code slightly more readable. More importantly, this is also
    % necessary when using a non-standard calling convention with GNU C,
    % since GNU C (2.95.2) ignores the function attributes on function
    % pointer types in casts.
    % XXX Is this limitation still there in currently used C compilers?
    %
    ml_gen_info_new_conv_var(ConvVarSeq, !Info),
    ConvVarSeq = conv_seq(ConvVarNum),
    FuncVarName = lvn_comp_var(lvnc_conv_var(ConvVarNum)),
    % The function address is always a pointer to code,
    % not to the heap, so the GC doesn't need to trace it.
    GCStmt = gc_no_stmt,
    FuncVarDecl = ml_gen_mlds_var_decl(FuncVarName, FuncType, GCStmt, Context),
    ml_gen_local_var_lval(!.Info, FuncVarName, FuncType, FuncVarLval),
    AssignFuncVar = ml_gen_assign(FuncVarLval, FuncRval, Context),
    FuncVarRval = ml_lval(FuncVarLval),

    % Generate code to box/unbox the arguments and compute the list of properly
    % converted rvals/lvals to pass as the function call's arguments and
    % return values.
    ml_gen_var_list(!.Info, ArgVars, ArgLvals),
    ml_variable_types(!.Info, ArgVars, ActualArgTypes),
    ml_gen_args(ArgNames, ArgLvals, ActualArgTypes, BoxedArgTypes,
        ArgModes, PredOrFunc, CodeModel, Context, no, 1,
        InputRvals, OutputLvals, OutputTypes,
        ConvArgLocalVarDefns, ConvOutputStmts, !Info),
    ClosureRval = ml_unop(unbox(ClosureArgType), ml_lval(ClosureLval)),

    ( if
        ConvArgLocalVarDefns = [],
        ConvOutputStmts = []
    then
        % Generate the call directly (as opposed to via DoGenCall)
        % in the common case.
        ml_gen_mlds_call(Signature, FuncVarRval,
            [ClosureRval | InputRvals], OutputLvals, OutputTypes,
            Determinism, Context, LocalVarDefns0, FuncDefns0, Stmts0, !Info)
    else
        % Prepare to generate the call, passing the closure as the first
        % argument. We can't actually generate the call yet, since it might be
        % nondet, and we don't yet know what its success continuation will be.
        % Instead we construct a higher-order term `DoGenCall', which, when
        % called by ml_combine_conj, will generate it.
        DoGenCall = ml_gen_mlds_call(Signature, FuncVarRval,
            [ClosureRval | InputRvals], OutputLvals, OutputTypes,
            Determinism, Context),

        % Construct a closure to generate code to convert the output arguments
        % and then succeed.
        DoGenConvOutputAndSucceed =
            ( pred(COAS_LocalVarDefns::out, COAS_FuncDefns::out,
                    COAS_Stmts::out, Info0::in, Info::out) is det :-
                COAS_LocalVarDefns = [],
                COAS_FuncDefns = [],
                ml_gen_success(CodeModel, Context, SucceedStmts, Info0, Info),
                COAS_Stmts = ConvOutputStmts ++ SucceedStmts
            ),

        % Conjoin the code generated by the two closures that we computed
        % above. `ml_combine_conj' will generate whatever kind of sequence
        % is necessary for this code model.
        ml_combine_conj(CodeModel, Context, DoGenCall,
            DoGenConvOutputAndSucceed,
            CallAndConvOutputLocalVarDefns, CallAndConvOutputFuncDefns,
            CallAndConvOutputStmts, !Info),
        LocalVarDefns0 =
            ConvArgLocalVarDefns ++ CallAndConvOutputLocalVarDefns,
        FuncDefns0 = CallAndConvOutputFuncDefns,
        Stmts0 = CallAndConvOutputStmts
    ),
    LocalVarDefns = [FuncVarDecl | LocalVarDefns0],
    FuncDefns = FuncDefns0,
    Stmts = [AssignFuncVar | Stmts0].

    % Generate MLDS code for a cast. The list of argument variables
    % must have only two elements, the input and the output.
    %
:- pred ml_gen_cast(prog_context::in, list(prog_var)::in,
    list(mlds_local_var_defn)::out, list(mlds_function_defn)::out,
    list(mlds_stmt)::out, ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_cast(Context, ArgVars, LocalVarDefns, FuncDefns, Stmts, !Info) :-
    ml_gen_var_list(!.Info, ArgVars, ArgLvals),
    ml_variable_types(!.Info, ArgVars, ArgTypes),
    ( if
        ArgVars = [SrcVar, DestVar],
        ArgLvals = [SrcLval, DestLval],
        ArgTypes = [SrcType, DestType]
    then
        ml_gen_info_get_module_info(!.Info, ModuleInfo),
        IsDummy = check_dummy_type(ModuleInfo, DestType),
        (
            IsDummy = is_dummy_type,
            Stmts = []
        ;
            IsDummy = is_not_dummy_type,
            ml_gen_box_or_unbox_rval(ModuleInfo, SrcType, DestType,
                bp_native_if_possible, ml_lval(SrcLval), CastRval),
            Assign = ml_gen_assign(DestLval, CastRval, Context),
            Stmts = [Assign]
        ),
        LocalVarDefns = [],
        FuncDefns = [],
        ( if ml_gen_info_search_const_var(!.Info, SrcVar, GroundTerm) then
            % If the source variable is a constant, so is the target after
            % this cast.
            ml_gen_info_set_const_var(DestVar, GroundTerm, !Info)
        else
            true
        )
    else
        unexpected($pred, "wrong number of args for cast")
    ).

%---------------------------------------------------------------------------%
%
% Code for ordinary calls.
%

ml_gen_plain_call(PredId, ProcId, ArgNames, ArgLvals, ActualArgTypes,
        CodeModel, Context, ForClosureWrapper,
        LocalVarDefns, FuncDefns, Stmts, !Info) :-
    % Generate the various parts of the code that is needed
    % for a procedure call:
    %
    % - declarations of variables needed for boxing/unboxing output arguments;
    % - code to call the function with the input arguments appropriately boxed,
    %   and
    % - code to unbox/box the return values.
    %
    % For example, if the callee is declared as
    %
    %   :- some [T2]
    %      pred callee(float::in, T1::in, float::out, T2::out, ...).
    %
    % then for a call `callee(Arg1, Arg2, Arg3, Arg4, ...)'
    % with arguments of types `U1, float, U2, float, ...',
    % we generate the following fragments:
    %
    %   /* declarations of variables needed for boxing/unboxing */
    %   Float conv_Arg3;
    %   MR_Box conv_Arg4;
    %   ...
    %
    %   /* code to call the function */
    %   func(unbox(Arg1), box(Arg2), &conv0_Arg3, &conv1_Arg4);
    %
    %   /* code to box/unbox the output arguments */
    %   *Arg3 = box(conv0_Arg3);
    %   *Arg4 = unbox(conv1_Arg4);
    %   ...
    %
    % Note that in general not every argument will need to be boxed/unboxed;
    % for those where no conversion is required, we just pass the
    % original argument unchanged.
    %

    % Compute the function signature.
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    Params = ml_gen_proc_params(ModuleInfo, PredId, ProcId),
    Signature = mlds_get_func_signature(Params),

    % Compute the function address.
    ml_gen_proc_addr_rval(PredId, ProcId, FuncRval, !Info),

    % Compute the callee's Mercury argument types and modes.
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, PredInfo, ProcInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    pred_info_get_arg_types(PredInfo, PredArgTypes),
    proc_info_get_argmodes(ProcInfo, ArgModes),

    % Generate code to box/unbox the arguments and compute the list of
    % properly converted rvals/lvals to pass as the function call's arguments
    % and return values.
    ml_gen_args(ArgNames, ArgLvals, ActualArgTypes, PredArgTypes,
        ArgModes, PredOrFunc, CodeModel, Context, ForClosureWrapper, 1,
        InputRvals, OutputLvals, OutputTypes,
        ConvArgLocalVarDefns, ConvOutputStmts, !Info),

    proc_info_interface_determinism(ProcInfo, Detism),

    ( if
        ConvArgLocalVarDefns = [],
        ConvOutputStmts = []
    then
        % Generate the call directly (as opposed to via DoGenCall)
        % in the common case.
        ml_gen_mlds_call(Signature, FuncRval,
            InputRvals, OutputLvals, OutputTypes, Detism, Context,
            LocalVarDefns, FuncDefns, Stmts, !Info)
    else
        % Construct a closure to generate the call. We can't actually generate
        % the call yet, since it might be nondet, and we don't yet know
        % what its success continuation will be. That is why we construct
        % a closure `DoGenCall', which, when called by ml_combine_conj, will
        % generate it.
        DoGenCall = ml_gen_mlds_call(Signature, FuncRval,
            InputRvals, OutputLvals, OutputTypes, Detism, Context),

        % Construct a closure to generate code to convert the output arguments
        % and then succeed.
        DoGenConvOutputAndSucceed =
            ( pred(COAS_LocalVarDefns::out, COAS_FuncDefns::out,
                    COAS_Stmts::out, Info0::in, Info::out) is det :-
                COAS_LocalVarDefns = [],
                COAS_FuncDefns = [],
                ml_gen_success(CodeModel, Context, SucceedStmts, Info0, Info),
                COAS_Stmts = ConvOutputStmts ++ SucceedStmts
            ),

        % Conjoin the code generated by the two closures that we computed
        % above. `ml_combine_conj' will generate whatever kind of sequence
        % is necessary for this code model.
        ml_combine_conj(CodeModel, Context, DoGenCall,
            DoGenConvOutputAndSucceed,
            CallAndConvOutputLocalVarDefns, CallAndConvOutputFuncDefns,
            CallAndConvOutputStmts, !Info),
        LocalVarDefns = ConvArgLocalVarDefns ++ CallAndConvOutputLocalVarDefns,
        FuncDefns = CallAndConvOutputFuncDefns,
        Stmts = CallAndConvOutputStmts
    ).

    % Generate an rval containing the address of the specified procedure.
    %
:- pred ml_gen_proc_addr_rval(pred_id::in, proc_id::in, mlds_rval::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_proc_addr_rval(PredId, ProcId, CodeAddrRval, !Info) :-
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    ml_gen_pred_label(ModuleInfo, PredId, ProcId, PredLabel, PredModule),
    ml_gen_proc_params(PredId, ProcId, Params, !Info),
    Signature = mlds_get_func_signature(Params),
    ProcLabel = mlds_proc_label(PredLabel, ProcId),
    QualifiedProcLabel = qual_proc_label(PredModule, ProcLabel),
    CodeAddrRval = ml_const(mlconst_code_addr(
        code_addr_proc(QualifiedProcLabel, Signature))).

%---------------------------------------------------------------------------%

    % This generates a call in the specified code model.
    % This is a lower-level routine called by both ml_gen_call
    % and ml_gen_generic_call.
    %
:- pred ml_gen_mlds_call(mlds_func_signature::in,
    mlds_rval::in, list(mlds_rval)::in, list(mlds_lval)::in,
    list(mlds_type)::in, determinism::in, prog_context::in,
    list(mlds_local_var_defn)::out, list(mlds_function_defn)::out,
    list(mlds_stmt)::out, ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_mlds_call(Signature, FuncRval, ArgRvals0, RetLvals0,
        RetTypes0, Detism, Context, LocalVarDefns, FuncDefns, Stmts, !Info) :-
    % Append the extra arguments or return val for this code_model.
    determinism_to_code_model(Detism, CodeModel),
    (
        CodeModel = model_non,
        % Create a new success continuation, if necessary.
        ml_gen_success_cont(RetTypes0, RetLvals0, Context,
            Cont, FuncDefns, !Info),
        % Append the success continuation to the ordinary arguments.
        Cont = success_cont(FuncPtrRval, EnvPtrRval, _, _),
        ml_gen_info_use_gcc_nested_functions(!.Info, UseNestedFuncs),
        (
            UseNestedFuncs = yes,
            ArgRvals = ArgRvals0 ++ [FuncPtrRval]
        ;
            UseNestedFuncs = no,
            ArgRvals = ArgRvals0 ++ [FuncPtrRval, EnvPtrRval]
        ),
        % For --nondet-copy-out, the output arguments will be passed to the
        % continuation rather than being returned.
        ml_gen_info_get_globals(!.Info, Globals),
        globals.lookup_bool_option(Globals, nondet_copy_out, NondetCopyOut),
        (
            NondetCopyOut = yes,
            RetLvals = []
        ;
            NondetCopyOut = no,
            RetLvals = RetLvals0
        )
    ;
        CodeModel = model_semi,
        % Return a bool indicating whether or not it succeeded.
        ml_success_lval(Success, !Info),
        ArgRvals = ArgRvals0,
        RetLvals = [Success | RetLvals0],
        FuncDefns = []
    ;
        CodeModel = model_det,
        ArgRvals = ArgRvals0,
        RetLvals = RetLvals0,
        FuncDefns = []
    ),
    LocalVarDefns = [],

    % Build the MLDS call statement.
    %
    % If the called procedure has determinism `erroneous', then mark it
    % as never returning (this will ensure that it gets treated as a tail
    % call).
    ( if Detism = detism_erroneous then
        CallKind = no_return_call
    else
        CallKind = ordinary_call
    ),
    ml_gen_info_get_disabled_warnings(!.Info, Warnings),
    ( if set.contains(Warnings, goal_warning_non_tail_recursive_calls) then
        Markers = set.make_singleton_set(mcm_disable_non_tail_rec_warning)
    else
        set.init(Markers)
    ),
    Stmt = ml_stmt_call(Signature, FuncRval, ArgRvals, RetLvals,
        CallKind, Markers, Context),
    Stmts = [Stmt].

:- pred ml_gen_success_cont(list(mlds_type)::in, list(mlds_lval)::in,
    prog_context::in, success_cont::out, list(mlds_function_defn)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_success_cont(OutputArgTypes, OutputArgLvals, Context,
        Cont, ContDecls, !Info) :-
    ml_gen_info_current_success_cont(!.Info, CurrentCont),
    CurrentCont = success_cont(_FuncPtrRval, _EnvPtrRval,
        CurrentContArgTypes, CurrentContArgLvals),
    ( if
        % As an optimization, check if the parameters expected by the current
        % continuation are the same as the ones expected by the new
        % continuation that we are generating; if so, we can just use the
        % current continuation rather than creating a new one.
        %
        CurrentContArgTypes = OutputArgTypes,
        CurrentContArgLvals = OutputArgLvals
    then
        Cont = CurrentCont,
        ContDecls = []
    else
        % Create a new continuation function that just copies the outputs
        % to locals and then calls the original current continuation.
        %
        % Note that ml_gen_cont_params does not fill in the gc_statement
        % for the parameters. This is OK, because the parameters of the
        % continuation function will not be live across any heap allocations or
        % procedure calls.
        %
        ml_gen_cont_params(OutputArgTypes, Params, !Info),
        ml_gen_new_func_label(yes(Params),
            ContFuncLabel, ContFuncLabelRval, !Info),
        % Push nesting level.
        ml_gen_copy_args_to_locals(!.Info, OutputArgLvals, OutputArgTypes,
            Context, CopyStmts),
        ml_gen_call_current_success_cont(Context, CallContStmt, !Info),
        CopyStmt = ml_gen_block([], [], CopyStmts ++ [CallContStmt], Context),
        % Pop nesting level.
        ml_gen_label_func(!.Info, ContFuncLabel, Params, Context,
            CopyStmt, ContFuncDefn),
        ContDecls = [ContFuncDefn],

        ml_get_env_ptr(!.Info, EnvPtrRval),
        NewSuccessCont = success_cont(ContFuncLabelRval, EnvPtrRval,
            OutputArgTypes, OutputArgLvals),
        Cont = NewSuccessCont
    ).

%---------------------------------------------------------------------------%

    % Generate the appropriate MLDS type for a continuation function
    % for a nondet procedure whose output arguments have the specified types.
    %
    % WARNING: this does not fill in the gc_statement for the function
    % parameters. It is the caller's responsibility to fill these in properly
    % if needed.
    %
:- pred ml_gen_cont_params(list(mlds_type)::in, mlds_func_params::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_cont_params(OutputArgTypes, Params, !Info) :-
    ml_gen_cont_params_loop(OutputArgTypes, 1, Args0),
    ml_gen_info_use_gcc_nested_functions(!.Info, UseNestedFuncs),
    (
        UseNestedFuncs = yes,
        Args = Args0
    ;
        UseNestedFuncs = no,
        ml_declare_env_ptr_arg(EnvPtrArg),
        Args = Args0 ++ [EnvPtrArg]
    ),
    Params = mlds_func_params(Args, []).

:- pred ml_gen_cont_params_loop(list(mlds_type)::in, int::in,
    list(mlds_argument)::out) is det.

ml_gen_cont_params_loop([], _, []).
ml_gen_cont_params_loop([Type | Types], ArgNum, [Argument | Arguments]) :-
    ArgName = lvn_comp_var(lvnc_arg(ArgNum)),
    % Figuring out the correct GC code here is difficult, since doing that
    % requires knowing the HLDS types, but here we only have the MLDS types.
    % So here we just leave it blank. The caller of ml_gen_cont_param has the
    % responsibility of filling this in properly if needed.
    GCStmt = gc_no_stmt,
    Argument = mlds_argument(ArgName, Type, GCStmt),
    ml_gen_cont_params_loop(Types, ArgNum + 1, Arguments).

:- pred ml_gen_copy_args_to_locals(ml_gen_info::in, list(mlds_lval)::in,
    list(mlds_type)::in, prog_context::in, list(mlds_stmt)::out) is det.

ml_gen_copy_args_to_locals(Info, ArgLvals, ArgTypes, Context, CopyStmts) :-
    ml_gen_copy_args_to_locals_loop(Info, ArgLvals, ArgTypes, 1, Context,
        CopyStmts).

:- pred ml_gen_copy_args_to_locals_loop(ml_gen_info::in, list(mlds_lval)::in,
    list(mlds_type)::in, int::in, prog_context::in,
    list(mlds_stmt)::out) is det.

ml_gen_copy_args_to_locals_loop(_Info, [], [], _, _, []).
ml_gen_copy_args_to_locals_loop(_Info, [], [_ | _], _, _, _) :-
    unexpected($pred, "length mismatch").
ml_gen_copy_args_to_locals_loop(_Info, [_ | _], [], _, _, _) :-
    unexpected($pred, "length mismatch").
ml_gen_copy_args_to_locals_loop(Info, [LocalLval | LocalLvals], [Type | Types],
        ArgNum, Context, [Stmt | Stmts]) :-
    ArgName = lvn_comp_var(lvnc_arg(ArgNum)),
    ml_gen_local_var_lval(Info, ArgName, Type, ArgLval),
    Stmt = ml_gen_assign(LocalLval, ml_lval(ArgLval), Context),
    ml_gen_copy_args_to_locals_loop(Info, LocalLvals, Types, ArgNum + 1,
        Context, Stmts).

    % Generate rvals and lvals for the arguments of a procedure call.
    %
:- pred ml_gen_args(list(mlds_local_var_name)::in, list(mlds_lval)::in,
    list(mer_type)::in, list(mer_type)::in, list(mer_mode)::in,
    pred_or_func::in, code_model::in, prog_context::in, bool::in, int::in,
    list(mlds_rval)::out, list(mlds_lval)::out, list(mlds_type)::out,
    list(mlds_local_var_defn)::out, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_args(VarNames, VarLvals, CallerTypes, CalleeTypes, Modes,
        PredOrFunc, CodeModel, Context, ForClosureWrapper, ArgNum,
        !:InputRvals, !:OutputLvals, !:OutputTypes, !:ConvDecls,
        !:ConvOutputStmts, !Info) :-
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
        !:ConvDecls = [],
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
            ForClosureWrapper, ArgNum + 1, !:InputRvals, !:OutputLvals,
            !:OutputTypes, !:ConvDecls, !:ConvOutputStmts, !Info),
        ml_gen_arg(VarName, VarLval, CallerType, CalleeType, Mode,
            PredOrFunc, CodeModel, Context, ForClosureWrapper, ArgNum,
            VarNamesTail, !InputRvals, !OutputLvals, !OutputTypes,
            !ConvDecls, !ConvOutputStmts, !Info)
    else
        unexpected($pred, "length mismatch")
    ).

:- pred ml_gen_arg(mlds_local_var_name::in, mlds_lval::in, mer_type::in,
    mer_type::in, mer_mode::in, pred_or_func::in, code_model::in,
    prog_context::in, bool::in, int::in, list(mlds_local_var_name)::in,
    list(mlds_rval)::in, list(mlds_rval)::out,
    list(mlds_lval)::in, list(mlds_lval)::out,
    list(mlds_type)::in, list(mlds_type)::out,
    list(mlds_local_var_defn)::in, list(mlds_local_var_defn)::out,
    list(mlds_stmt)::in, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.
:- pragma inline(ml_gen_arg/23).

ml_gen_arg(VarName, VarLval, CallerType, CalleeType, Mode,
        PredOrFunc, CodeModel, Context, ForClosureWrapper,
        ArgNum, VarNamesTail,
        !InputRvals, !OutputLvals, !OutputTypes,
        !ConvDecls, !ConvOutputStmts, !Info) :-
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
            ml_gen_box_or_unbox_lval(CallerType, CalleeType,
                bp_native_if_possible, VarLval, VarName, Context,
                ForClosureWrapper, ArgNum, ArgLval, ThisArgConvDecls,
                _ThisArgConvInput, ThisArgConvOutput, !Info),
            !:ConvDecls = ThisArgConvDecls ++ !.ConvDecls,
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
                    ml_gen_info_get_globals(!.Info, Globals),
                    get_copy_out_option(Globals, CodeModel) = yes
                )
            then
                !:OutputLvals = [ArgLval | !.OutputLvals],
                ml_gen_type(!.Info, CalleeType, OutputType),
                !:OutputTypes = [OutputType | !.OutputTypes]
            else
                % Otherwise use the traditional C style of passing the
                % address of the output value.
                !:InputRvals = [ml_gen_mem_addr(ArgLval) | !.InputRvals]
            )
        )
    ).

    % ml_gen_mem_addr(Lval) returns a value equal to &Lval.
    % For the case where Lval = *Rval, for some Rval,
    % we optimize &*Rval to just Rval.
    %
:- func ml_gen_mem_addr(mlds_lval) = mlds_rval.

ml_gen_mem_addr(Lval) =
    (if Lval = ml_mem_ref(Rval, _) then Rval else ml_mem_addr(Lval)).

%---------------------------------------------------------------------------%
%
% Code for builtins.
%

ml_gen_builtin(PredId, ProcId, ArgVars, CodeModel, Context,
        LocalVarDefns, FuncDefns, Stmts, !Info) :-
    ml_gen_var_list(!.Info, ArgVars, ArgLvals),
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    ModuleName = predicate_module(ModuleInfo, PredId),
    PredName = predicate_name(ModuleInfo, PredId),
    builtin_ops.translate_builtin(ModuleName, PredName, ProcId, ArgLvals,
        SimpleCode),
    (
        CodeModel = model_det,
        (
            SimpleCode = assign(Lval, SimpleExpr),
            ( if
                % We need to avoid generating assignments to dummy variables
                % introduced for types such as io.state.
                Lval = ml_local_var(_VarName, VarType),
                VarType = mercury_type(ProgDataType, _, _),
                check_dummy_type(ModuleInfo, ProgDataType) = is_dummy_type
            then
                Stmts = []
            else
                Rval = ml_gen_simple_expr(SimpleExpr),
                Stmt = ml_gen_assign(Lval, Rval, Context),
                Stmts = [Stmt]
            )
        ;
            SimpleCode = ref_assign(AddrLval, ValueLval),
            ( if ValueLval = ml_local_var(_ValueVarName, ValueType) then
                Stmt = ml_gen_assign(
                    ml_mem_ref(ml_lval(AddrLval), ValueType),
                    ml_lval(ValueLval), Context),
                Stmts = [Stmt]
            else
                unexpected($pred, "malformed ref_assign")
            )
        ;
            SimpleCode = test(_),
            unexpected($pred, "malformed model_det builtin predicate")
        ;
            SimpleCode = noop(_),
            Stmts = []
        )
    ;
        CodeModel = model_semi,
        (
            SimpleCode = test(SimpleTest),
            TestRval = ml_gen_simple_expr(SimpleTest),
            ml_gen_set_success(TestRval, Context, Stmt, !Info),
            Stmts = [Stmt]
        ;
            SimpleCode = ref_assign(_, _),
            unexpected($pred, "malformed model_semi builtin predicate")
        ;
            SimpleCode = assign(_, _),
            unexpected($pred, "malformed model_semi builtin predicate")
        ;
            SimpleCode = noop(_),
            unexpected($pred, "malformed model_semi builtin predicate")
        )
    ;
        CodeModel = model_non,
        unexpected($pred, "model_non builtin predicate")
    ),
    LocalVarDefns = [],
    FuncDefns = [].

:- func ml_gen_simple_expr(simple_expr(mlds_lval)) = mlds_rval.

ml_gen_simple_expr(leaf(Lval)) = ml_lval(Lval).
ml_gen_simple_expr(int_const(Int)) = ml_const(mlconst_int(Int)).
ml_gen_simple_expr(uint_const(UInt)) = ml_const(mlconst_uint(UInt)).
ml_gen_simple_expr(int8_const(Int8)) = ml_const(mlconst_int8(Int8)).
ml_gen_simple_expr(uint8_const(UInt8)) = ml_const(mlconst_uint8(UInt8)).
ml_gen_simple_expr(int16_const(Int16)) = ml_const(mlconst_int16(Int16)).
ml_gen_simple_expr(uint16_const(UInt16)) = ml_const(mlconst_uint16(UInt16)).
ml_gen_simple_expr(int32_const(Int32)) = ml_const(mlconst_int32(Int32)).
ml_gen_simple_expr(uint32_const(UInt32)) = ml_const(mlconst_uint32(UInt32)).
ml_gen_simple_expr(float_const(Float)) = ml_const(mlconst_float(Float)).
ml_gen_simple_expr(unary(Op, Expr)) =
    ml_unop(std_unop(Op), ml_gen_simple_expr(Expr)).
ml_gen_simple_expr(binary(Op, ExprA, ExprB)) =
    ml_binop(Op, ml_gen_simple_expr(ExprA), ml_gen_simple_expr(ExprB)).

%---------------------------------------------------------------------------%
:- end_module ml_backend.ml_call_gen.
%---------------------------------------------------------------------------%
