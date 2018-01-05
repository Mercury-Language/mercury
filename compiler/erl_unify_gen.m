%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2007-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: erl_unify_gen.m
% Main author: wangp.
%
% This module is part of the Erlang code generator.
% It handles Erlang code generation for unifications.
%
% TODO
% type t
%     --->    f(int, string)
%     ;       some [T] f(T).
%
% will generate for the first alternative
%     {f, Int, String}
% and for the second alternative
%     {f, TypeInfo_for_t, T}
%
% which means that the RTTI routines will not be able to distinguish
% between the two alternatives.
% The suggested fix is to place the arity on all functors for types
% for which at least one functor is existentially quantified.
% Once this fix is done, update the comment on
% erlang_rtti_implementation.matches_du_functor.
%
%-----------------------------------------------------------------------------%

:- module erl_backend.erl_unify_gen.
:- interface.

:- import_module erl_backend.elds.
:- import_module erl_backend.erl_code_util.
:- import_module hlds.
:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module maybe.

%-----------------------------------------------------------------------------%

    % Generate ELDS code for a unification.
    %
:- pred erl_gen_unification(unification::in, code_model::in, prog_context::in,
    maybe(elds_expr)::in, elds_expr::out, erl_gen_info::in, erl_gen_info::out)
    is det.

    % Convert a cons id to the ELDS equivalent term, if any. That is, any term
    % returned by this predicate must be useable as part of a pattern matching
    % operation.
    %
:- pred cons_id_to_term(cons_id, prog_vars, elds_term, elds_term,
    erl_gen_info, erl_gen_info).
:- mode cons_id_to_term(in, in, in, out, in, out) is semidet.
:- mode cons_id_to_term(in(termable_cons_id), in, in, out, in, out) is det.

:- inst termable_cons_id for cons_id/0
    --->    cons(ground, ground, ground)
    ;       tuple_cons(ground)
    ;       int_const(ground)
    ;       uint_const(ground)
    ;       int8_const(ground)
    ;       uint8_const(ground)
    ;       int16_const(ground)
    ;       uint16_const(ground)
    ;       int32_const(ground)
    ;       uint32_const(ground)
    ;       int64_const(ground)
    ;       uint64_const(ground)
    ;       float_const(ground)
    ;       char_const(ground)
    ;       string_const(ground).

    % Convert a cons id to the ELDS equivalent expression.
    %
:- pred cons_id_to_expr(cons_id::in, prog_vars::in, elds_term::in,
    elds_expr::out, erl_gen_info::in, erl_gen_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.rtti.
:- import_module check_hlds.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module erl_backend.erl_call_gen.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.

:- import_module int.
:- import_module list.
:- import_module require.

%-----------------------------------------------------------------------------%

erl_gen_unification(Unification, CodeModel, Context, MaybeSuccessExpr,
        Statement, !Info) :-
    (
        Unification = assign(TargetVar, SourceVar),
        erl_gen_info_get_module_info(!.Info, ModuleInfo),
        erl_variable_type(!.Info, TargetVar, VarType),
        IsDummy = check_dummy_type(ModuleInfo, VarType),
        (
            IsDummy = is_dummy_type,
            Statement = expr_or_void(MaybeSuccessExpr)
        ;
            IsDummy = is_not_dummy_type,
            Assign =
                elds_eq(expr_from_var(TargetVar), expr_from_var(SourceVar)),
            Statement = maybe_join_exprs(Assign, MaybeSuccessExpr)
        )
    ;
        Unification = simple_test(Var1, Var2),
        expect(unify(CodeModel, model_semi), $module, $pred,
            "simple_test not semidet"),
        erl_gen_info_get_module_info(!.Info, ModuleInfo),
        erl_variable_type(!.Info, Var1, VarType),
        IsDummy = check_dummy_type(ModuleInfo, VarType),
        (
            IsDummy = is_dummy_type,
            Statement = expr_or_void(MaybeSuccessExpr)
        ;
            IsDummy = is_not_dummy_type,
            %
            % case Var1 =:= Var2 of
            %   true  -> MaybeSuccessExpr ;
            %   false -> fail
            % end
            %
            Test      = elds_binop((=:=),
                            expr_from_var(Var1), expr_from_var(Var2)),
            TrueCase  = elds_case(elds_true, expr_or_void(MaybeSuccessExpr)),
            FalseCase = elds_case(elds_false, elds_term(elds_fail)),
            Statement = elds_case_expr(Test, [TrueCase, FalseCase])
        )
    ;
        Unification = construct(Var, ConsId, Args, ArgModes, _HowToConstruct,
            _CellIsUnique, SubInfo),
        expect(unify(CodeModel, model_det), $module, $pred,
            "construct not det"),
        (
            SubInfo = no_construct_sub_info
        ;
            SubInfo = construct_sub_info(_MaybeTakeAddr, MaybeSizeProfInfo),
            expect(unify(MaybeSizeProfInfo, no), $module, $pred,
                "term size profiling not yet supported")
        ),
        erl_gen_info_get_module_info(!.Info, ModuleInfo),
        erl_variable_type(!.Info, Var, VarType),
        IsDummy = check_dummy_type(ModuleInfo, VarType),
        (
            IsDummy = is_dummy_type,
            Statement = expr_or_void(MaybeSuccessExpr)
        ;
            IsDummy = is_not_dummy_type,
            erl_variable_types(!.Info, Args, ArgTypes),
            erl_gen_construct(Var, ConsId, Args, ArgTypes, ArgModes, Context,
                Construct, !Info),
            Statement = maybe_join_exprs(Construct, MaybeSuccessExpr)
        )
    ;
        Unification = deconstruct(Var, ConsId, Args, ArgModes, CanFail,
            _CanCGC),
        (
            CanFail = can_fail,
            SuccessExpr = det_expr(MaybeSuccessExpr),
            erl_gen_semidet_deconstruct(Var, ConsId, Args, ArgModes, Context,
                SuccessExpr, Statement, !Info)
        ;
            CanFail = cannot_fail,
            erl_gen_det_deconstruct(Var, ConsId, Args, ArgModes, Context,
                Statement0, !Info),
            Statement = maybe_join_exprs(Statement0, MaybeSuccessExpr)
        )
    ;
        Unification = complicated_unify(_, _, _),
        % Simplify.m should have converted these into procedure calls.
        unexpected($module, $pred, "complicated unify")
    ).

%-----------------------------------------------------------------------------%

:- pred erl_gen_construct(prog_var::in, cons_id::in, prog_vars::in,
    list(mer_type)::in, list(unify_mode)::in, prog_context::in, elds_expr::out,
    erl_gen_info::in, erl_gen_info::out) is det.

erl_gen_construct(Var, ConsId, Args, ArgTypes, ArgModes, _Context, Statement,
        !Info) :-
    cons_id_to_expr(ConsId, Args, elds_false, RHS, !Info),
    Construct = elds_eq(expr_from_var(Var), RHS),

    % If there are any free variables in Args, assign them to false first.
    % This can happen if we are constructing a partially instantiated
    % data structure.
    erl_gen_info_get_module_info(!.Info, ModuleInfo),
    AssignFreeVars = list.filter_map_corresponding3(
        assign_free_var(ModuleInfo), Args, ArgTypes, ArgModes),
    (
        AssignFreeVars = [],
        Statement = Construct
    ;
        AssignFreeVars = [_ | _],
        Statement = join_exprs(elds_block(AssignFreeVars), Construct)
    ).

:- func assign_free_var(module_info, prog_var, mer_type, unify_mode)
    = elds_expr is semidet.

assign_free_var(ModuleInfo, Var, ArgType, ArgMode) = var_eq_false(Var) :-
    ArgMode = unify_modes_lhs_rhs(_, RHSFromToInsts),
    not (
        from_to_insts_to_top_functor_mode(ModuleInfo, RHSFromToInsts, ArgType,
            top_in),
        check_dummy_type(ModuleInfo, ArgType) = is_not_dummy_type
        % XXX ml_unify_gen also checks if ConsArgType is dummy type,
        % do we need to do the same?
    ).

%-----------------------------------------------------------------------------%

:- pred erl_gen_det_deconstruct(prog_var::in, cons_id::in, prog_vars::in,
    list(unify_mode)::in, prog_context::in, elds_expr::out,
    erl_gen_info::in, erl_gen_info::out) is det.

erl_gen_det_deconstruct(Var, ConsId, Args, _Modes, _Context, Statement,
        !Info) :-
    cons_id_to_expr(ConsId, Args, elds_anon_var, LHS, !Info),
    Statement = elds_eq(LHS, expr_from_var(Var)).

:- pred erl_gen_semidet_deconstruct(prog_var::in, cons_id::in, prog_vars::in,
    list(unify_mode)::in, prog_context::in, elds_expr::in, elds_expr::out,
    erl_gen_info::in, erl_gen_info::out) is det.

erl_gen_semidet_deconstruct(Var, ConsId, Args, _Modes, _Context,
        SuccessExpr, Statement, !Info) :-
    ( if cons_id_to_term(ConsId, Args, elds_anon_var, Pattern0, !Info) then
        Pattern = Pattern0
    else
        unexpected($module, $pred, "undeconstructable object")
    ),
    %
    % case Var of
    %   Pattern -> SuccessExpr ;
    %   _       -> fail
    % end
    %
    SucceedCase = elds_case(Pattern, SuccessExpr),
    FailCase    = elds_case(elds_anon_var, elds_term(elds_fail)),
    Statement   = elds_case_expr(expr_from_var(Var), [SucceedCase, FailCase]).

%-----------------------------------------------------------------------------%

cons_id_to_term(ConsId, Args, DummyVarReplacement, Term, !Info) :-
    (
        (
            ConsId = cons(Name, _Arity, _TypeCtor)
        ;
            ConsId = tuple_cons(_Arity),
            Name = unqualified("{}")
        ),
        % XXX We should optimise the cases where we don't actually need a
        % distinguishing atom.
        Functor = elds_term(elds_atom(Name)),
        erl_gen_info_get_module_info(!.Info, ModuleInfo),
        erl_gen_info_get_var_types(!.Info, VarTypes),

        % Replace dummy variables in the term. In construction unifications
        % we would want to replace them with `false' (what we use for all
        % dummy values). In deconstructions we replace them by anonymous
        % variables (_).
        TermArgs = list.map(erl_var_or_dummy_replacement(ModuleInfo,
            VarTypes, DummyVarReplacement), Args),
        Term = elds_tuple([Functor | TermArgs])
    ;
        ConsId = int_const(Int),
        Term = elds_int(Int)
    ;
        ConsId = uint_const(UInt),
        Term = elds_uint(UInt)
    ;
        ConsId = int8_const(Int8),
        Term = elds_int8(Int8)
    ;
        ConsId = uint8_const(UInt8),
        Term = elds_uint8(UInt8)
    ;
        ConsId = int16_const(Int16),
        Term = elds_int16(Int16)
    ;
        ConsId = uint16_const(UInt16),
        Term = elds_uint16(UInt16)
    ;
        ConsId = int32_const(Int32),
        Term = elds_int32(Int32)
    ;
        ConsId = uint32_const(UInt32),
        Term = elds_uint32(UInt32)
    ;
        ConsId = int64_const(Int64),
        Term = elds_int64(Int64)
    ;
        ConsId = uint64_const(UInt64),
        Term = elds_uint64(UInt64)
    ;
        ConsId = float_const(Float),
        Term = elds_float(Float)
    ;
        ConsId = char_const(Char),
        Term = elds_char(Char)
    ;
        ConsId = string_const(String),
        Term = elds_binary(String)
    ).

cons_id_to_expr(ConsId, Args, DummyVarReplacement, Expr, !Info) :-
    (
        ( ConsId = cons(_, _, _)
        ; ConsId = tuple_cons(_)
        ; ConsId = int_const(_)
        ; ConsId = uint_const(_)
        ; ConsId = int8_const(_)
        ; ConsId = uint8_const(_)
        ; ConsId = int16_const(_)
        ; ConsId = uint16_const(_)
        ; ConsId = int32_const(_)
        ; ConsId = uint32_const(_)
        ; ConsId = int64_const(_)
        ; ConsId = uint64_const(_)
        ; ConsId = float_const(_)
        ; ConsId = char_const(_)
        ; ConsId = string_const(_)
        ),
        cons_id_to_term(ConsId, Args, DummyVarReplacement, Term, !Info),
        Expr = elds_term(Term)
    ;
        ConsId = impl_defined_const(_),
        unexpected($module, $pred, "impl_defined_const")
    ;
        ConsId = closure_cons(ShroudedPredProcId, lambda_normal),
        pred_const_to_closure(ShroudedPredProcId, Args, Expr, !Info)
    ;
        ConsId = type_ctor_info_const(ModuleName, TypeCtor, Arity),
        RttiTypeCtor = rtti_type_ctor(ModuleName, TypeCtor, Arity),
        RttiId = elds_rtti_type_ctor_id(RttiTypeCtor),
        Expr = elds_rtti_ref(RttiId)
    ;
        ConsId = base_typeclass_info_const(InstanceModule,
            class_id(ClassName, Arity), _Instance, InstanceStr),
        ( if sym_name_get_module_name(ClassName, ClassModuleName0) then
            ClassModuleName = ClassModuleName0
        else
            unexpected($module, $pred, "class has no module name")
        ),
        ClassNameStr = unqualify_name(ClassName),
        TCName = tc_name(ClassModuleName, ClassNameStr, Arity),
        RttiId = elds_rtti_base_typeclass_id(TCName, InstanceModule,
            InstanceStr),
        Expr = elds_rtti_ref(RttiId)
    ;
        ( ConsId = type_info_cell_constructor(_TypeCtor)
        ; ConsId = typeclass_info_cell_constructor
        ),
        % This represents type_infos and typeclass_infos as undistinguished
        % tuples, so the layout will be the same as corresponding arrays in C.
        Expr = elds_term(elds_tuple(exprs_from_vars(Args)))
    ;
        ConsId = type_info_const(_),
        unexpected($module, $pred, "type_info_const")
    ;
        ConsId = typeclass_info_const(_),
        unexpected($module, $pred, "typeclass_info_const")
    ;
        ConsId = ground_term_const(_, _),
        unexpected($module, $pred, "ground_term_const")
    ;
        ( ConsId = tabling_info_const(_)
        ; ConsId = deep_profiling_proc_layout(_)
        ; ConsId = table_io_entry_desc(_)
        ),
        sorry($module, $pred,
            "tabling and deep profiling not supported on Erlang backend")
    ).

:- pred pred_const_to_closure(shrouded_pred_proc_id::in, prog_vars::in,
    elds_expr::out, erl_gen_info::in, erl_gen_info::out) is det.

pred_const_to_closure(ShroudedPredProcId, CurriedArgs, FunExpr, !Info) :-
    erl_gen_info_get_module_info(!.Info, ModuleInfo),
    PredProcId = unshroud_pred_proc_id(ShroudedPredProcId),
    module_info_pred_proc_info(ModuleInfo, PredProcId, PredInfo, ProcInfo),
    pred_info_get_arg_types(PredInfo, CalleeTypes),
    proc_info_get_argmodes(ProcInfo, ArgModes),
    CodeModel = proc_info_interface_code_model(ProcInfo),

    % Create extra variables needed to complete the call to the procedure.
    NumExtraVars = list.length(CalleeTypes) - list.length(CurriedArgs),
    erl_gen_info_new_vars(NumExtraVars, AllExtraVars, !Info),

    % Separate the argument lists into inputs and outputs twice: once ignore
    % dummy and unused arguments, and once keeping them. Dummy arguments must
    % not be dropped in the closure's function signature, and dummy outputs
    % must be present in the return value. However, the underlying procedure
    % won't accept dummy arguments and won't return dummy values.

    CurriedAndExtraArgs = CurriedArgs ++ AllExtraVars,

    erl_gen_arg_list(ModuleInfo, opt_dummy_args, CurriedAndExtraArgs,
        CalleeTypes, ArgModes, CallInputVars, CallOutputVars),

    erl_gen_arg_list(ModuleInfo, no_opt_dummy_args, CurriedAndExtraArgs,
        CalleeTypes, ArgModes, _InputVarsInclDummy, OutputVarsInclDummy),
    InputExtraVars = list.delete_elems(AllExtraVars, OutputVarsInclDummy),

    OutputVarsInclDummyExprs = exprs_from_vars(OutputVarsInclDummy),
    (
        CodeModel = model_det,
        ClosureInputArgs = InputExtraVars,
        SuccessExpr0 = tuple_or_single_expr(OutputVarsInclDummyExprs)
    ;
        CodeModel = model_semi,
        ClosureInputArgs = InputExtraVars,
        SuccessExpr0 = elds_term(elds_tuple(OutputVarsInclDummyExprs))
    ;
        CodeModel = model_non,
        % One more extra variable is needed for the success continuation for
        % model_non procedures.
        erl_gen_info_new_named_var("Succeed", SucceedVar, !Info),
        ClosureInputArgs = InputExtraVars ++ [SucceedVar],
        SuccessExpr0 = elds_call(elds_call_ho(expr_from_var(SucceedVar)),
            OutputVarsInclDummyExprs)
    ),

    DummyOutputVars = list.delete_elems(OutputVarsInclDummy, CallOutputVars),
    MaterialiseDummyOutputs = list.map(var_eq_false, DummyOutputVars),
    SuccessExpr = join_exprs(elds_block(MaterialiseDummyOutputs),
        SuccessExpr0),

    % Make the call to the underlying procedure.
    CallTarget = elds_call_plain(PredProcId),
    erl_make_call(CodeModel, CallTarget, CallInputVars, CallOutputVars,
        yes(SuccessExpr), DoCall),

    FunExpr = elds_fun(elds_clause(terms_from_vars(ClosureInputArgs), DoCall)).

%-----------------------------------------------------------------------------%
:- end_module erl_backend.erl_unify_gen.
%-----------------------------------------------------------------------------%
