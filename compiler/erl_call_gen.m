%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: erl_call_gen.m.
% Main author: wangp.
% 
% This module is part of the ELDS code generator.  It handles code generation
% of procedures calls, calls to builtins, and other closely related stuff.
% 
%-----------------------------------------------------------------------------%

:- module erl_backend.erl_call_gen.
:- interface.

:- import_module erl_backend.elds.
:- import_module erl_backend.erl_code_util.
:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

    % erl_gen_call(PredId, ProcId, ArgNames, ArgTypes,
    %   CodeModel, Context, SuccessExpr, Statement, !Info):
    %
    % Generate ELDS code for an HLDS procedure call.
    %
:- pred erl_gen_call(pred_id::in, proc_id::in, prog_vars::in,
    list(mer_type)::in, code_model::in, prog_context::in, maybe(elds_expr)::in,
    elds_expr::out, erl_gen_info::in, erl_gen_info::out) is det.

    % Generate ELDS code for a higher order call.
    %
:- pred erl_gen_higher_order_call(generic_call::in(higher_order),
    prog_vars::in, list(mer_mode)::in, determinism::in, prog_context::in,
    maybe(elds_expr)::in, elds_expr::out,
    erl_gen_info::in, erl_gen_info::out) is det.

:- inst higher_order
    --->    higher_order(ground, ground, ground, ground).

    % Generate ELDS code for a call to a builtin procedure.
    %
:- pred erl_gen_builtin(pred_id::in, proc_id::in, prog_vars::in,
    code_model::in, prog_context::in, maybe(elds_expr)::in, elds_expr::out,
    erl_gen_info::in, erl_gen_info::out) is det.

    % Generate ELDS code for a cast. The list of argument variables
    % must have only two elements, the input and the output.
    %
:- pred erl_gen_cast(prog_context::in, prog_vars::in, maybe(elds_expr)::in,
    elds_expr::out, erl_gen_info::in, erl_gen_info::out) is det.

    % Generate ELDS code for a call to foreign code.
    %
:- pred erl_gen_foreign_code_call(list(foreign_arg)::in,
    maybe(trace_expr(trace_runtime))::in, pragma_foreign_code_impl::in,
    code_model::in, prog_context::in, maybe(elds_expr)::in,
    elds_expr::out, erl_gen_info::in, erl_gen_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.builtin_ops.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_module.
:- import_module libs.compiler_util.

:- import_module pair.

%-----------------------------------------------------------------------------%
%
% Code for procedure calls
%

erl_gen_call(PredId, ProcId, ArgVars, _ActualArgTypes,
        CodeModel, _Context, MaybeSuccessExpr, Statement, !Info) :-
    erl_gen_info_get_module_info(!.Info, ModuleInfo),

    % Compute the callee's Mercury argument types and modes.
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, PredInfo, ProcInfo),
    pred_info_get_arg_types(PredInfo, CalleeTypes),
    proc_info_get_argmodes(ProcInfo, ArgModes),

    erl_gen_arg_list(ModuleInfo, ArgVars, CalleeTypes, ArgModes,
        InputVars, OutputVars),
    PPId = proc(PredId, ProcId),
    NormalCallExpr = elds_call(PPId, exprs_from_vars(InputVars)),
    (
        CodeModel = model_det,
        make_det_call(NormalCallExpr, OutputVars, MaybeSuccessExpr, Statement)
    ;
        CodeModel = model_semi,
        SuccessExpr = det_expr(MaybeSuccessExpr),
        make_semidet_call(NormalCallExpr, OutputVars, SuccessExpr, Statement)
    ;
        CodeModel = model_non,
        SuccessExpr = det_expr(MaybeSuccessExpr),
        make_nondet_call(PPId, InputVars, OutputVars, SuccessExpr, Statement)
    ).

:- pred make_det_call(elds_expr::in, prog_vars::in, maybe(elds_expr)::in,
    elds_expr::out) is det.

make_det_call(Expr, OutputVars, MaybeSuccessExpr, Statement) :-
    (
        OutputVars = [],
        (if 
            ( MaybeSuccessExpr = yes(elds_term(elds_empty_tuple))
            ; MaybeSuccessExpr = no
            )
        then
            % Preserve tail calls.
            Statement = Expr
        else
            Statement = maybe_join_exprs(Expr, MaybeSuccessExpr)
        )
    ;
        OutputVars = [_ | _],
        UnpackTerm = elds_term(elds_tuple(elds.exprs_from_vars(OutputVars))),
        (if
            MaybeSuccessExpr = yes(UnpackTerm)
        then
            % Preserve tail calls.
            Statement = Expr
        else
            AssignCall = elds_eq(UnpackTerm, Expr),
            Statement = maybe_join_exprs(AssignCall, MaybeSuccessExpr)
        )
    ).

:- pred make_semidet_call(elds_expr::in, prog_vars::in, elds_expr::in,
    elds_expr::out) is det.

make_semidet_call(CallExpr, OutputVars, SuccessExpr, Statement) :-
    UnpackTerm = elds_tuple(exprs_from_vars(OutputVars)),
    (if
        SuccessExpr = elds_term(UnpackTerm)
    then
        % Avoid unnecessary unpacking.
        Statement = CallExpr
    else
        % case CallExpr of
        %   {OutputVars, ...} -> SuccessExpr ;
        %   _ -> fail
        % end
        %
        Statement = elds_case_expr(CallExpr, [TrueCase, FalseCase]),
        TrueCase  = elds_case(UnpackTerm, SuccessExpr),
        FalseCase = elds_case(elds_anon_var, elds_term(elds_fail))
    ).

:- pred make_nondet_call(pred_proc_id::in, prog_vars::in, prog_vars::in,
    elds_expr::in, elds_expr::out) is det.

make_nondet_call(PredProcId, InputVars, OutputVars, SuccessCont0, Statement) :-
    %
    % Proc(InputVars, ...,
    %   fun(OutputVars, ...) ->
    %       SuccessCont0
    %   end)
    %
    (if
        SuccessCont0 = elds_call_ho(SuccessCont1, exprs_from_vars(OutputVars))
    then
        % Avoid an unnecessary closure.
        SuccessCont = SuccessCont1
    else
        SuccessCont = elds_fun(elds_clause(terms_from_vars(OutputVars),
            SuccessCont0))
    ),
    Statement = elds_call(PredProcId,
        exprs_from_vars(InputVars) ++ [SuccessCont]).

%-----------------------------------------------------------------------------%
%
% Code for generic calls
%

erl_gen_higher_order_call(GenericCall, ArgVars, Modes, Detism,
        _Context, MaybeSuccessExpr, Statement, !Info) :-
    GenericCall = higher_order(ClosureVar, _, _, _),

    erl_gen_info_get_module_info(!.Info, ModuleInfo),
    erl_variable_types(!.Info, ArgVars, ArgTypes),
    erl_gen_arg_list(ModuleInfo, ArgVars, ArgTypes, Modes,
        InputVars, OutputVars),

    ClosureVarExpr = expr_from_var(ClosureVar),
    InputVarsExprs = exprs_from_vars(InputVars),
    NormalCallExpr = elds_call_ho(ClosureVarExpr, InputVarsExprs),

    determinism_to_code_model(Detism, CallCodeModel),
    (
        CallCodeModel = model_det,
        make_det_call(NormalCallExpr, OutputVars, MaybeSuccessExpr, Statement)
    ;
        CallCodeModel = model_semi,
        SuccessExpr = det_expr(MaybeSuccessExpr),
        make_semidet_call(NormalCallExpr, OutputVars, SuccessExpr, Statement)
    ;
        CallCodeModel = model_non,
        %
        % Proc(InputVars, ..., 
        %   fun(OutputVars, ...) ->
        %       SuccessCont0
        %   end)
        %
        SuccessCont = elds_fun(elds_clause(terms_from_vars(OutputVars),
            det_expr(MaybeSuccessExpr))),
        Statement = elds_call_ho(ClosureVarExpr,
            InputVarsExprs ++ [SuccessCont])
    ).

erl_gen_cast(_Context, ArgVars, MaybeSuccessExpr, Statement, !Info) :-
    erl_variable_types(!.Info, ArgVars, ArgTypes),
    (
        ArgVars = [SrcVar, DestVar],
        ArgTypes = [_SrcType, DestType]
    ->
        erl_gen_info_get_module_info(!.Info, ModuleInfo),
        ( is_dummy_argument_type(ModuleInfo, DestType) ->
            Statement = expr_or_void(MaybeSuccessExpr)
        ;
            % XXX this doesn't do anything yet
            Assign = elds_eq(expr_from_var(DestVar), expr_from_var(SrcVar)),
            Statement = maybe_join_exprs(Assign, MaybeSuccessExpr)
        )
    ;
        unexpected(this_file, "erl_gen_cast: wrong number of args for cast")
    ).

%-----------------------------------------------------------------------------%
%
% Code for builtins
%

    % XXX many of the "standard" builtins in builtin_ops.m do not apply to the
    % Erlang back-end.
    %
erl_gen_builtin(PredId, ProcId, ArgVars, CodeModel, _Context,
        MaybeSuccessExpr, Statement, !Info) :-
    erl_gen_info_get_module_info(!.Info, ModuleInfo),
    ModuleName = predicate_module(ModuleInfo, PredId),
    PredName = predicate_name(ModuleInfo, PredId),
    (
        builtin_ops.translate_builtin(ModuleName, PredName,
            ProcId, ArgVars, SimpleCode0)
    ->
        SimpleCode = SimpleCode0
    ;
        unexpected(this_file, "erl_gen_builtin: unknown builtin predicate")
    ),
    (
        CodeModel = model_det,
        (
            SimpleCode = assign(Lval, SimpleExpr),
            % XXX We need to avoid generating assignments to dummy variables
            % introduced for types such as io.state.
            Rval = erl_gen_simple_expr(SimpleExpr),
            Assign = elds.elds_eq(elds.expr_from_var(Lval), Rval),
            Statement = maybe_join_exprs(Assign, MaybeSuccessExpr)
        ;
            SimpleCode = ref_assign(_AddrLval, _ValueLval),
            unexpected(this_file, "ref_assign not supported in Erlang backend")
        ;
            SimpleCode = test(_),
            unexpected(this_file, "malformed model_det builtin predicate")
        ;
            SimpleCode = noop(_),
            Statement = expr_or_void(MaybeSuccessExpr)
        )
    ;
        CodeModel = model_semi,
        (
            SimpleCode = test(SimpleTest),
            TestRval = erl_gen_simple_expr(SimpleTest),
            % Unlike Mercury procedures, the builtin tests return true and
            % false instead of {} and fail.
            Statement = elds_case_expr(TestRval, [TrueCase, FalseCase]),
            TrueCase = elds_case(elds_true, expr_or_void(MaybeSuccessExpr)),
            FalseCase = elds_case(elds_false, elds_term(elds_fail))
        ;
            SimpleCode = ref_assign(_, _),
            unexpected(this_file, "malformed model_semi builtin predicate")
        ;
            SimpleCode = assign(_, _),
            unexpected(this_file, "malformed model_semi builtin predicate")
        ;
            SimpleCode = noop(_),
            unexpected(this_file, "malformed model_semi builtin predicate")
        )
    ;
        CodeModel = model_non,
        unexpected(this_file, "model_non builtin predicate")
    ).

:- func erl_gen_simple_expr(simple_expr(prog_var)) = elds_expr.

erl_gen_simple_expr(leaf(Var)) = elds.expr_from_var(Var).
erl_gen_simple_expr(int_const(Int)) = elds_term(elds_int(Int)).
erl_gen_simple_expr(float_const(Float)) = elds_term(elds_float(Float)).
erl_gen_simple_expr(unary(StdOp, Expr0)) = Expr :-
    ( std_unop_to_elds(StdOp, Op) ->
        SimpleExpr = erl_gen_simple_expr(Expr0),
        Expr = elds_unop(Op, SimpleExpr)
    ;
        sorry(this_file, "unary builtin not supported on erlang target")
    ).
erl_gen_simple_expr(binary(StdOp, Expr1, Expr2)) = Expr :-
    ( std_binop_to_elds(StdOp, Op) ->
        SimpleExpr1 = erl_gen_simple_expr(Expr1),
        SimpleExpr2 = erl_gen_simple_expr(Expr2),
        Expr = elds_binop(Op, SimpleExpr1, SimpleExpr2)
    ;
        sorry(this_file, "binary builtin not supported on erlang target")
    ).

:- pred std_unop_to_elds(unary_op::in, elds_unop::out) is semidet.

std_unop_to_elds(mktag, _) :- fail.
std_unop_to_elds(tag, _) :- fail.
std_unop_to_elds(unmktag, _) :- fail.
std_unop_to_elds(strip_tag, _) :- fail.
std_unop_to_elds(mkbody, _) :- fail.
std_unop_to_elds(unmkbody, _) :- fail.
std_unop_to_elds(hash_string, _) :- fail.
std_unop_to_elds(bitwise_complement, elds.bnot).
std_unop_to_elds(logical_not, elds.logical_not).

:- pred std_binop_to_elds(binary_op::in, elds_binop::out) is semidet.

std_binop_to_elds(int_add, elds.add).
std_binop_to_elds(int_sub, elds.sub).
std_binop_to_elds(int_mul, elds.mul).
std_binop_to_elds(int_div, elds.int_div).
std_binop_to_elds(int_mod, elds.(rem)).
std_binop_to_elds(unchecked_left_shift, elds.bsl).
std_binop_to_elds(unchecked_right_shift, elds.bsr).
std_binop_to_elds(bitwise_and, elds.band).
std_binop_to_elds(bitwise_or, elds.bor).
std_binop_to_elds(bitwise_xor, elds.bxor).
std_binop_to_elds(logical_and, elds.andalso).
std_binop_to_elds(logical_or, elds.orelse).
std_binop_to_elds(eq, elds.(=:=)).
std_binop_to_elds(ne, elds.(=/=)).
std_binop_to_elds(body, _) :- fail.
std_binop_to_elds(array_index(_), _) :- fail.
std_binop_to_elds(str_eq, elds.(=:=)).
std_binop_to_elds(str_ne, elds.(=/=)).
std_binop_to_elds(str_lt, elds.(<)).
std_binop_to_elds(str_gt, elds.(>)).
std_binop_to_elds(str_le, elds.(=<)).
std_binop_to_elds(str_ge, elds.(>=)).
std_binop_to_elds(int_lt, elds.(<)).
std_binop_to_elds(int_gt, elds.(>)).
std_binop_to_elds(int_le, elds.(=<)).
std_binop_to_elds(int_ge, elds.(>=)).
std_binop_to_elds(unsigned_le, _) :- fail.
std_binop_to_elds(float_plus, elds.add).
std_binop_to_elds(float_minus, elds.sub).
std_binop_to_elds(float_times, elds.mul).
std_binop_to_elds(float_divide, elds.float_div).
std_binop_to_elds(float_eq, elds.(=:=)).
std_binop_to_elds(float_ne, elds.(=/=)).
std_binop_to_elds(float_lt, elds.(<)).
std_binop_to_elds(float_gt, elds.(>)).
std_binop_to_elds(float_le, elds.(=<)).
std_binop_to_elds(float_ge, elds.(>=)).

%-----------------------------------------------------------------------------%
%
% Code for foreign code calls
%

% Currently dummy arguments do not exist at all.  The writer of the foreign
% proc must not reference dummy input variables and should not bind dummy
% output variables (it causes unused variable warnings from the Erlang
% compiler).  To avoid warnings from the Mercury compiler about arguments not
% appearing in the foreign proc, they must be named with an underscore.
%
% Materialising dummy input variables would not be a good idea unless
% unused variable warnings were switched off in the Erlang compiler.

erl_gen_foreign_code_call(ForeignArgs, MaybeTraceRuntimeCond,
        PragmaImpl, CodeModel, _OuterContext, MaybeSuccessExpr, Statement,
        !Info) :-
    (
        MaybeTraceRuntimeCond = yes(_),
        sorry(this_file, "trace runtime conditions in Erlang backend")
    ;
        MaybeTraceRuntimeCond = no
    ),
    (
        PragmaImpl = fc_impl_ordinary(ForeignCode, _Context),
        %
        % In the following, F<n> are input variables to the foreign code (with
        % fixed names), and G<n> are output variables from the foreign code
        % (also with fixed names).  The variables V<n> are input variables and
        % have arbitrary names.  We introduce variables with fixed names using
        % a lambda function rather than direct assignments in case a single
        % procedure makes calls to two pieces of foreign code which use the
        % same fixed names (this can happen due to inlining).
        %
        % We generate code for calls to model_det foreign code like this:
        %
        %   (fun(F1, F2, ...) ->
        %       <foreign code>,
        %       {G1, G2, ...}
        %   )(V1, V2, ...).
        %
        % We generate code for calls to model_semi foreign code like this:
        %
        %   (fun(F1, F2, ...) ->
        %       <foreign code>,
        %       case SUCCESS_INDICATOR of
        %           true ->
        %               {G1, G2, ...};
        %           false ->
        %               fail
        %       end
        %   )(V1, V2, ...)
        %
        % where `SUCCESS_INDICATOR' is a variable that should be set in the
        % foreign code snippet to `true' or `false'.
        %

        % Separate the foreign call arguments into inputs and outputs.
        erl_gen_info_get_module_info(!.Info, ModuleInfo),
        list.map2(foreign_arg_type_mode, ForeignArgs, ArgTypes, ArgModes),
        erl_gen_arg_list(ModuleInfo, ForeignArgs, ArgTypes, ArgModes,
            InputForeignArgs, OutputForeignArgs),

        % Get the variables involved in the call and their fixed names.
        InputVars = list.map(foreign_arg_var, InputForeignArgs),
        OutputVars = list.map(foreign_arg_var, OutputForeignArgs),
        InputVarsNames = list.map(foreign_arg_name, InputForeignArgs),
        OutputVarsNames = list.map(foreign_arg_name, OutputForeignArgs),

        ForeignCodeExpr = elds_foreign_code(ForeignCode),
        OutputTuple = elds_term(elds_tuple(
            exprs_from_fixed_vars(OutputVarsNames))),

        % Create the inner lambda function.
        (
            CodeModel = model_det,
            %
            %   <ForeignCodeExpr>,
            %   {Outputs, ...}
            %
            InnerFunStatement = join_exprs(ForeignCodeExpr, OutputTuple)
        ;
            CodeModel = model_semi,
            %
            %   <ForeignCodeExpr>,
            %   case SUCCESS_INDICATOR of
            %       true -> {Outputs, ...};
            %       false -> fail
            %   end
            %
            InnerFunStatement = join_exprs(ForeignCodeExpr, MaybePlaceOutputs),
            MaybePlaceOutputs = elds_case_expr(SuccessInd, [OnTrue, OnFalse]),
            SuccessInd = elds_term(elds_fixed_name_var("SUCCESS_INDICATOR")),
            OnTrue = elds_case(elds_true, OutputTuple),
            OnFalse = elds_case(elds_false, elds_term(elds_fail))
        ;
            CodeModel = model_non,
            sorry(this_file, "model_non foreign_procs in Erlang backend")
        ),
        InnerFun = elds_fun(elds_clause(terms_from_fixed_vars(InputVarsNames),
            InnerFunStatement)),

        % Call the inner function with the input variables.
        CallInner = elds_call_ho(InnerFun, exprs_from_vars(InputVars)),
        (
            CodeModel = model_det,
            make_det_call(CallInner, OutputVars, MaybeSuccessExpr, Statement)
        ;
            CodeModel = model_semi,
            SuccessExpr = det_expr(MaybeSuccessExpr),
            make_semidet_call(CallInner, OutputVars, SuccessExpr, Statement)
        )
    ;
        PragmaImpl = fc_impl_model_non(_, _, _, _, _, _, _, _, _),
        sorry(this_file, "erl_gen_goal_expr: fc_impl_model_non")
    ;
        PragmaImpl = fc_impl_import(_, _, _, _),
        sorry(this_file, "erl_gen_goal_expr: fc_impl_import")
    ).

:- pred foreign_arg_type_mode(foreign_arg::in, mer_type::out, mer_mode::out)
    is det.

foreign_arg_type_mode(foreign_arg(_, MaybeNameMode, Type, _), Type, Mode) :-
    (
        MaybeNameMode = yes(_Name - Mode)
    ;
        MaybeNameMode = no,
        % This argument is unused.
        Mode = (free -> free)
    ).

:- func foreign_arg_name(foreign_arg) = string.

foreign_arg_name(foreign_arg(_, MaybeNameMode, _, _)) = Name :-
    (
        MaybeNameMode = yes(Name - _)
    ;
        MaybeNameMode = no,
        % This argument is unused.
        Name = "_"
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "erl_call_gen.m".

%-----------------------------------------------------------------------------%
:- end_module erl_call_gen.
%-----------------------------------------------------------------------------%
