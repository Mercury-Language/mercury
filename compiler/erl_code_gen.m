%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: erl_code_gen.m.
% Main author: wangp.
% 
% ELDS code generation -- convert from HLDS to ELDS.
%
% XXX more documentation to come later
%
% For now, the notation `Foo [[ Bar ]]' means to generate the code for
% expression `Foo', ultimately evaluating to the value `Bar' on success.
% Code which can fail currently always evaluates to the atom `fail' (this will
% be changed to improve the code generated for disjuncts, which should rather
% evaluate to an expression representing the rest of the disjunction on
% failure).
%
% TODO: (this is incomplete)
% - nondet code
% - contexts are ignored at the moment
% - RTTI
% - type classes
% - many scope types not yet supported
% - foreign code
%
%-----------------------------------------------------------------------------%

:- module erl_backend.erl_code_gen.
:- interface.

:- import_module erl_backend.elds.
:- import_module erl_backend.erl_code_util.
:- import_module hlds.code_model.
:- import_module hlds.hlds_module.
:- import_module parse_tree.prog_data.

:- import_module io.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Generate Erlang code for an entire module.
    %
:- pred erl_code_gen(module_info::in, elds::out, io::di, io::uo) is det.

    % erl_gen_wrap_goal(OuterCodeModel, InnerCodeModel, Context,
    %   Statement0, Statement):
    %
    % OuterCodeModel is the code model expected by the context in which a goal
    % is called. InnerCodeModel is the code model which the goal actually has.
    % This predicate converts the code generated for the goal using
    % InnerCodeModel into code that uses the calling convention appropriate
    % for OuterCodeModel.
    %
:- pred erl_gen_wrap_goal(code_model::in, code_model::in, prog_context::in,
    elds_expr::in, elds_expr::out, erl_gen_info::in, erl_gen_info::out)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module erl_backend.erl_call_gen.
:- import_module erl_backend.erl_unify_gen.
:- import_module hlds.code_model.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module hlds.passes_aux.
:- import_module hlds.pred_table.
:- import_module libs.compiler_util.

:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module varset.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

erl_code_gen(ModuleInfo, ELDS, !IO) :-
    module_info_get_name(ModuleInfo, ModuleName),
    erl_gen_preds(ModuleInfo, Defns, !IO),
    ELDS = elds(ModuleName, Defns).

:- pred erl_gen_preds(module_info::in, list(elds_defn)::out, io::di, io::uo)
    is det.

erl_gen_preds(ModuleInfo, PredDefns, !IO) :-
    module_info_preds(ModuleInfo, PredTable),
    map.keys(PredTable, PredIds),
    erl_gen_preds_2(ModuleInfo, PredIds, PredTable, [], RevPredDefns, !IO),
    PredDefns = list.reverse(RevPredDefns).

:- pred erl_gen_preds_2(module_info::in, list(pred_id)::in, pred_table::in,
    list(elds_defn)::in, list(elds_defn)::out, io::di, io::uo) is det.

erl_gen_preds_2(ModuleInfo, PredIds0, PredTable, !Defns, !IO) :-
    (
        PredIds0 = [PredId | PredIds],
        map.lookup(PredTable, PredId, PredInfo),
        pred_info_get_import_status(PredInfo, ImportStatus),
        (
            (
                ImportStatus = status_imported(_)
            ;
                % XXX comment was from ml_code_gen.m, don't know if it applies.
                % We generate incorrect and unnecessary code for the external
                % special preds which are pseudo_imported, so just ignore them.
                is_unify_or_compare_pred(PredInfo),
                ImportStatus = status_external(status_pseudo_imported)
            )
        ->
            true
        ;
            erl_gen_pred(ModuleInfo, PredId, PredInfo, ImportStatus,
                !Defns, !IO)
        ),
        erl_gen_preds_2(ModuleInfo, PredIds, PredTable, !Defns, !IO)
    ;
        PredIds0 = []
    ).

    % Generate ELDS definitions for all the non-imported procedures
    % of a given predicate (or function).
    %
:- pred erl_gen_pred(module_info::in, pred_id::in, pred_info::in,
    import_status::in, list(elds_defn)::in, list(elds_defn)::out,
    io::di, io::uo) is det.

erl_gen_pred(ModuleInfo, PredId, PredInfo, ImportStatus, !Defns, !IO) :-
    ( ImportStatus = status_external(_) ->
        ProcIds = pred_info_procids(PredInfo)
    ;
        ProcIds = pred_info_non_imported_procids(PredInfo)
    ),
    (
        ProcIds = []
    ;
        ProcIds = [_ | _],
        write_pred_progress_message("% Generating ELDS code for ",
            PredId, ModuleInfo, !IO),
        pred_info_get_procedures(PredInfo, ProcTable),
        erl_gen_procs(ProcIds, ModuleInfo, PredId, PredInfo, ProcTable, !Defns)
    ).

:- pred erl_gen_procs(list(proc_id)::in, module_info::in, pred_id::in,
    pred_info::in, proc_table::in, list(elds_defn)::in, list(elds_defn)::out)
    is det.

erl_gen_procs([], _, _, _, _, !Defns).
erl_gen_procs([ProcId | ProcIds], ModuleInfo, PredId, PredInfo, ProcTable,
        !Defns) :-
    map.lookup(ProcTable, ProcId, ProcInfo),
    erl_gen_proc(ModuleInfo, PredId, ProcId, PredInfo, ProcInfo, !Defns),
    erl_gen_procs(ProcIds, ModuleInfo, PredId, PredInfo, ProcTable, !Defns).

%-----------------------------------------------------------------------------%
%
% Code for handling individual procedures
%

    % Generate ELDS code for the specified procedure.
    %
:- pred erl_gen_proc(module_info::in, pred_id::in, proc_id::in, pred_info::in,
    proc_info::in, list(elds_defn)::in, list(elds_defn)::out) is det.

erl_gen_proc(ModuleInfo, PredId, ProcId, _PredInfo, _ProcInfo, !Defns) :-
    erl_gen_proc_defn(ModuleInfo, PredId, ProcId, Arity, ProcVarSet,
        ProcClause),
    ProcDefn = elds_defn(proc(PredId, ProcId), Arity, ProcVarSet, ProcClause),
    !:Defns = [ProcDefn | !.Defns].

    % Generate an ELDS definition for the specified procedure.
    %
:- pred erl_gen_proc_defn(module_info::in, pred_id::in, proc_id::in,
    arity::out, prog_varset::out, elds_clause::out) is det.

erl_gen_proc_defn(ModuleInfo, PredId, ProcId, Arity, ProcVarSet, ProcClause) :-
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, PredInfo, ProcInfo),
    pred_info_get_import_status(PredInfo, ImportStatus),
    proc_info_interface_code_model(ProcInfo, CodeModel),
    proc_info_get_headvars(ProcInfo, HeadVars),
    proc_info_get_initial_instmap(ProcInfo, ModuleInfo, InstMap0),
    proc_info_get_goal(ProcInfo, Goal0),

    % The HLDS front-end sometimes over-estimates the set of non-locals.
    % We need to restrict the set of non-locals for the top-level goal
    % to just the headvars, because otherwise variables which occur in the
    % top-level non-locals but which are not really non-local will not be
    % declared.

    Goal0 = hlds_goal(GoalExpr, GoalInfo0),
    goal_info_get_code_gen_nonlocals(GoalInfo0, NonLocals0),
    set.list_to_set(HeadVars, HeadVarsSet),
    set.intersect(HeadVarsSet, NonLocals0, NonLocals),
    goal_info_set_code_gen_nonlocals(NonLocals, GoalInfo0, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo),

    goal_info_get_context(GoalInfo, _Context),

    some [!Info] (
        !:Info = erl_gen_info_init(ModuleInfo, PredId, ProcId),

        ( ImportStatus = status_external(_) ->
            sorry(this_file, "external procedures in Erlang backend")
        ;
            erl_gen_proc_body(CodeModel, InstMap0, Goal, ProcClause,
                !Info)
        ),

        erl_gen_info_get_input_vars(!.Info, InputVars),
        Arity = list.length(InputVars),

        erl_gen_info_get_varset(!.Info, ProcVarSet)
    ).

:- pred erl_gen_proc_body(code_model::in, instmap::in, hlds_goal::in,
    elds_clause::out, erl_gen_info::in, erl_gen_info::out) is det.

erl_gen_proc_body(CodeModel, InstMap0, Goal, ProcClause, !Info) :-
    erl_gen_info_get_input_vars(!.Info, InputVars),
    erl_gen_info_get_output_vars(!.Info, OutputVars),
    (
        ( CodeModel = model_det
        ; CodeModel = model_semi
        ),
        SuccessExpr = elds_term(elds_tuple(exprs_from_vars(OutputVars)))
    ;
        CodeModel = model_non,
        sorry(this_file, "nondet code in Erlang backend")
    ),
    erl_gen_goal(CodeModel, InstMap0, Goal, yes(SuccessExpr), Statement,
        !Info),
    ProcClause = elds_clause(terms_from_vars(InputVars), Statement).

%-----------------------------------------------------------------------------%
%
% Stuff to generate code for goals.
%

:- pred erl_gen_goal(code_model::in, instmap::in, hlds_goal::in,
    maybe(elds_expr)::in, elds_expr::out,
    erl_gen_info::in, erl_gen_info::out) is det.

    % Generate ELDS code for the specified goal in the specified code model.
    %
    % If MaybeSuccessExpr is `yes(SuccessExpr)' then SuccessExpr is the
    % expression that the code generated for Goal must evaluate to, if the Goal
    % succeeds.
    %
erl_gen_goal(CodeModel, InstMap, Goal, MaybeSuccessExpr, Code, !Info) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    goal_info_get_context(GoalInfo, Context),

    % Generate code for the goal in its own code model.
    goal_info_get_code_model(GoalInfo, GoalCodeModel),
    erl_gen_goal_expr(GoalExpr, GoalCodeModel, InstMap, Context,
        MaybeSuccessExpr, GoalCode, !Info),

    % Add whatever wrapper is needed to convert the goal's code model
    % to the desired code model.
    erl_gen_wrap_goal(CodeModel, GoalCodeModel, Context,
        GoalCode, Code, !Info).

    % If the inner and outer code models are equal, we don't need to do
    % anything special.

erl_gen_wrap_goal(model_det, model_det, _, !Code, !Info).
erl_gen_wrap_goal(model_semi, model_semi, _, !Code, !Info).
erl_gen_wrap_goal(model_non, model_non, _, !Code, !Info).

    % If the inner code model is more precise than the outer code model,
    % then we need to append some statements to convert the calling convention
    % for the inner code model to that of the outer code model.

erl_gen_wrap_goal(model_semi, model_det, _Context, !Code, !Info).
    % Currently nothing is required because det goals always
    % return their results in a tuple, which is exactly the same as
    % a successful return from a semidet goal.

erl_gen_wrap_goal(model_non, model_det, _Context, !Code, !Info) :-
    sorry(this_file, "nondet code in Erlang backend").

erl_gen_wrap_goal(model_non, model_semi, _Context, !Code, !Info) :-
    sorry(this_file, "nondet code in Erlang backend").

    % If the inner code model is less precise than the outer code model,
    % then simplify.m is supposed to wrap the goal inside a `some'
    % to indicate that a commit is needed.

erl_gen_wrap_goal(model_det, model_semi, _, _, _, !Info) :-
    unexpected(this_file,
        "erl_gen_wrap_goal: code model mismatch -- semi in det").
erl_gen_wrap_goal(model_det, model_non, _, _, _, !Info) :-
    unexpected(this_file,
        "erl_gen_wrap_goal: code model mismatch -- nondet in det").
erl_gen_wrap_goal(model_semi, model_non, _, _, _, !Info) :-
    unexpected(this_file,
        "erl_gen_wrap_goal: code model mismatch -- nondet in semi").

%-----------------------------------------------------------------------------%

    % Generate ELDS code for the different kinds of HLDS goals.
    %
:- pred erl_gen_goal_expr(hlds_goal_expr::in, code_model::in, instmap::in,
    prog_context::in, maybe(elds_expr)::in, elds_expr::out,
    erl_gen_info::in, erl_gen_info::out) is det.

erl_gen_goal_expr(switch(Var, CanFail, CasesList), CodeModel, InstMap,
        Context, MaybeSuccessExpr, Statement, !Info) :-
    erl_gen_switch(Var, CanFail, CasesList, CodeModel, InstMap,
        Context, MaybeSuccessExpr, Statement, !Info).

erl_gen_goal_expr(scope(ScopeReason, Goal), CodeModel, InstMap, Context,
        MaybeSuccessExpr, CodeExpr, !Info) :-
    (
        ( ScopeReason = exist_quant(_)
        ; ScopeReason = promise_solutions(_, _)
        ; ScopeReason = promise_purity(_, _)
        ; ScopeReason = commit(_)
        ; ScopeReason = barrier(_)
        ; ScopeReason = trace_goal(_, _, _, _, _)
        ),
        sorry(this_file, "exotic scope type in erlang code generator")
    ;
        ScopeReason = from_ground_term(_),
        Goal = hlds_goal(GoalExpr, _),
        erl_gen_goal_expr(GoalExpr, CodeModel, InstMap, Context,
            MaybeSuccessExpr, CodeExpr, !Info)
    ).

erl_gen_goal_expr(if_then_else(_Vars, Cond, Then, Else), CodeModel,
        InstMap, Context, MaybeSuccessExpr, Statement, !Info) :-
    erl_gen_ite(CodeModel, InstMap, Cond, Then, Else, Context,
        MaybeSuccessExpr, Statement, !Info).

erl_gen_goal_expr(negation(Goal), CodeModel, InstMap, Context,
        MaybeSuccessExpr, Statement, !Info) :-
    erl_gen_negation(Goal, CodeModel, InstMap, Context, MaybeSuccessExpr,
        Statement, !Info).

erl_gen_goal_expr(conj(_ConjType, Goals), CodeModel, InstMap, Context,
        MaybeSuccessExpr, Statement, !Info) :-
    % XXX Currently we treat parallel conjunction the same as
    % sequential conjunction -- parallelism is not yet implemented.
    erl_gen_conj(Goals, CodeModel, InstMap, Context, MaybeSuccessExpr,
        Statement, !Info).

erl_gen_goal_expr(disj(Goals), CodeModel, InstMap, Context,
        MaybeSuccessExpr, Statement, !Info) :-
    erl_gen_disj(Goals, CodeModel, InstMap, Context, MaybeSuccessExpr,
        Statement, !Info).

erl_gen_goal_expr(generic_call(GenericCall, Vars, Modes, Detism),
        CodeModel, _InstMap, Context, MaybeSuccessExpr, Statement, !Info) :-
    determinism_to_code_model(Detism, CallCodeModel),
    expect(unify(CodeModel, CallCodeModel), this_file,
        "erl_gen_generic_call: code model mismatch"),
    (
        GenericCall = higher_order(_, _, _, _),
        erl_gen_higher_order_call(GenericCall, Vars, Modes, Detism,
            Context, MaybeSuccessExpr, Statement, !Info)
    ;
        GenericCall = class_method(_, _, _, _),
        sorry(this_file, "class methods calls in erlang backend")
    ;
        GenericCall = event_call(_),
        sorry(this_file, "event_calls in erlang backend")
    ;
        GenericCall = cast(_),
        erl_gen_cast(Context, Vars, MaybeSuccessExpr, Statement, !Info)
    ).

erl_gen_goal_expr(plain_call(PredId, ProcId, ArgVars, BuiltinState, _, _),
        CodeModel, _InstMap, Context, MaybeSuccessExpr, Statement, !Info) :-
    (
        BuiltinState = not_builtin,
        erl_variable_types(!.Info, ArgVars, ActualArgTypes),
        erl_gen_call(PredId, ProcId, ArgVars, ActualArgTypes,
            CodeModel, Context, MaybeSuccessExpr, Statement, !Info)
    ;
        BuiltinState = inline_builtin,
        erl_gen_builtin(PredId, ProcId, ArgVars, CodeModel, Context,
            MaybeSuccessExpr, Statement, !Info)
    ;
        BuiltinState = out_of_line_builtin,
        unexpected(this_file, "erl_gen_goal_expr: out_of_line_builtin")
    ).

erl_gen_goal_expr(unify(_LHS, _RHS, _Mode, Unification, _UnifyContext),
        CodeModel, _InstMap, Context, MaybeSuccessExpr, Statement, !Info) :-
    erl_gen_unification(Unification, CodeModel, Context, MaybeSuccessExpr,
        Statement, !Info).

erl_gen_goal_expr(
        call_foreign_proc(_Attributes, _PredId, _ProcId, _Args, _ExtraArgs,
            _MaybeTraceRuntimeCond, _PragmaImpl), _CodeModel, _InstMap,
        _OuterContext, _MaybeSuccessExpr, _Statement, !_Info) :-
    sorry(this_file, "call_foreign_proc in erlang backend").

erl_gen_goal_expr(shorthand(_), _, _, _, _, _, !Info) :-
    % these should have been expanded out by now
    unexpected(this_file, "erl_gen_goal_expr: unexpected shorthand").

%-----------------------------------------------------------------------------%
%
% Code for switches
%

    % The generated code looks like:
    %
    % case Var of
    %   Pattern1 -> Expr1  [[ MaybeSuccessExpr ]];
    %   Pattern2 -> Expr2  [[ MaybeSuccessExpr ]];
    %   ...
    % end
    %
    % If the switch can fail, a default case is added:
    %
    %   _ -> fail
    %
:- pred erl_gen_switch(prog_var::in, can_fail::in, list(hlds_goal.case)::in,
    code_model::in, instmap::in, prog_context::in, maybe(elds_expr)::in,
    elds_expr::out, erl_gen_info::in, erl_gen_info::out) is det.

erl_gen_switch(Var, CanFail, CasesList, CodeModel, InstMap, _Context,
        MaybeSuccessExpr, Statement, !Info) :-
    % Get the union of all variables bound in all cases.
    erl_gen_info_get_module_info(!.Info, ModuleInfo),
    CasesGoals = list.map((func(case(_, Goal)) = Goal), CasesList),
    union_bound_nonlocals_in_goals(ModuleInfo, InstMap, CasesGoals,
        MustBindNonLocals),

    % Generate code for each case.
    list.map_foldl(erl_gen_case(CodeModel, InstMap, MustBindNonLocals,
        MaybeSuccessExpr), CasesList, ErlCases0, !Info),
    (
        CanFail = can_fail,
        % Add `_ -> fail' default case.
        DefaultCase = elds_case(elds_anon_var, elds_term(elds_fail)),
        ErlCases = ErlCases0 ++ [DefaultCase]
    ;
        CanFail = cannot_fail,
        ErlCases = ErlCases0
    ),
    Statement = elds_case_expr(expr_from_var(Var), ErlCases).

:- pred union_bound_nonlocals_in_goals(module_info::in, instmap::in,
    hlds_goals::in, set(prog_var)::out) is det.

union_bound_nonlocals_in_goals(ModuleInfo, InstMap, Goals, NonLocalsUnion) :-
    IsBound = erl_bound_nonlocals_in_goal(ModuleInfo, InstMap),
    list.map(IsBound, Goals, NonLocalsLists),
    NonLocalsUnion = set.union_list(NonLocalsLists).

:- pred erl_gen_case(code_model::in, instmap::in, set(prog_var)::in,
    maybe(elds_expr)::in, hlds_goal.case::in, elds_case::out, 
    erl_gen_info::in, erl_gen_info::out) is det.

erl_gen_case(CodeModel, InstMap, MustBindNonLocals, MaybeSuccessExpr,
        case(ConsId, Goal), ELDSCase, !Info) :-
    ( ConsId = cons(_, Arity) ->
        % Create dummy variables to fill the pattern with.
        erl_gen_info_new_anonymous_vars(Arity, DummyVars, !Info)
    ;
        DummyVars = []
    ),
    ( cons_id_to_term(ConsId, DummyVars, Pattern0, !Info) ->
        Pattern = Pattern0
    ;
        unexpected(this_file, "erl_gen_case: cannot pattern match on object")
    ),
    erl_gen_goal(CodeModel, InstMap, Goal, MaybeSuccessExpr, Statement0,
        !Info),
    %
    % To prevent warnings from the Erlang compiler we must make sure all cases
    % bind the same set of variables.  This might not be true if the Mercury
    % compiler knows that a case calls a procedure which throws an exception.
    %
    erl_gen_info_get_module_info(!.Info, ModuleInfo),
    erl_bind_unbound_vars(ModuleInfo, MustBindNonLocals, Goal, InstMap,
        Statement0, Statement),
    ELDSCase = elds_case(Pattern, Statement).

%-----------------------------------------------------------------------------%
%
% Code for if-then-else
%

:- pred erl_gen_ite(code_model::in, instmap::in,
    hlds_goal::in, hlds_goal::in, hlds_goal::in,
    prog_context::in, maybe(elds_expr)::in, elds_expr::out,
    erl_gen_info::in, erl_gen_info::out) is det.

erl_gen_ite(CodeModel, InstMap0, Cond, Then, Else, _Context, MaybeSuccessExpr,
        Statement, !Info) :-
    Cond = hlds_goal(_, CondGoalInfo),
    goal_info_get_code_model(CondGoalInfo, CondCodeModel),
    (
        %   model_det Cond:
        %       <(Cond -> Then ; Else)>
        %   ===>
        %       <Cond>,
        %       <Then>

        CondCodeModel = model_det,
        erl_gen_goal(model_det, InstMap0, Cond, no, CondStatement, !Info),
        update_instmap(Cond, InstMap0, CondInstMap),
        erl_gen_goal(CodeModel, CondInstMap, Then, MaybeSuccessExpr,
            ThenStatement, !Info),
        Statement = join_exprs(CondStatement, ThenStatement)
    ;
        %   model_semi cond:
        %       <(Cond -> Then ; Else)>
        %   ===>
        %       case 
        %           <Cond [[ Outputs ]]>
        %       of
        %           {Outputs} -> <Then> ;
        %           fail      -> <Else>
        %       end
        %
        % where Outputs is the set of variables bound by Bound.  To avoid
        % warnings from the Erlang compiler, we rename the set of output
        % variables in the code generated for Cond itself, so they are only
        % bound in the outer `case' statement.
        %

        CondCodeModel = model_semi,

        % Find the non-local variables bound in the condition.
        erl_gen_info_get_module_info(!.Info, ModuleInfo),
        erl_bound_nonlocals_in_goal(ModuleInfo, InstMap0, Cond, CondVarsSet),
        CondVars = set.to_sorted_list(CondVarsSet),

        % Generate the condition goal, making it evaluate to a tuple of the
        % non-local variables that it binds on success.
        CondVarsTerm = elds_tuple(exprs_from_vars(CondVars)),
        erl_gen_goal(model_semi, InstMap0, Cond,
            yes(elds_term(CondVarsTerm)), CondStatement0, !Info),

        % Rename the variables in the generated condition expression.
        erl_create_renaming(CondVars, Subn, !Info),
        erl_rename_vars_in_expr(Subn, CondStatement0, CondStatement),

        % Generate the Then and Else branches.
        update_instmap(Cond, InstMap0, InstMap1),
        erl_gen_goal(CodeModel, InstMap1, Then, MaybeSuccessExpr,
            ThenStatement0, !Info),
        erl_gen_goal(CodeModel, InstMap0, Else, MaybeSuccessExpr,
            ElseStatement0, !Info),

        % Make sure both branches bind the same sets of variables.
        erl_bound_nonlocals_in_goal(ModuleInfo, InstMap1, Then, ThenVars),
        erl_bound_nonlocals_in_goal(ModuleInfo, InstMap0, Else, ElseVars),
        erl_bind_unbound_vars(ModuleInfo, ElseVars, Then, InstMap1,
            ThenStatement0, ThenStatement),
        erl_bind_unbound_vars(ModuleInfo, ThenVars, Else, InstMap0,
            ElseStatement0, ElseStatement),

        Statement = elds_case_expr(CondStatement, [TrueCase, FalseCase]),
        TrueCase  = elds_case(CondVarsTerm, ThenStatement),
        FalseCase = elds_case(elds_anon_var, ElseStatement)
    ;
        CondCodeModel = model_non,
        sorry(this_file, "nondet code in Erlang backend")
    ).

%-----------------------------------------------------------------------------%
%
% Code for negation
%

:- pred erl_gen_negation(hlds_goal::in, code_model::in, instmap::in,
    prog_context::in, maybe(elds_expr)::in, elds_expr::out,
    erl_gen_info::in, erl_gen_info::out) is det.

erl_gen_negation(Cond, CodeModel, InstMap, _Context, MaybeSuccessExpr,
        Statement, !Info) :-
    Cond = hlds_goal(_, CondGoalInfo),
    goal_info_get_code_model(CondGoalInfo, CondCodeModel),
    (
        % model_det negation:
        %       <not(Goal)>
        %   ===>
        %       <Goal>  % must fail

        CodeModel = model_det,

        % The code generator expects semidet goals to always have a success
        % expression, although in this case we know it won't arise at run time.
        DummySuccessExpr = elds_term(elds_empty_tuple),
        erl_gen_goal(model_semi, InstMap, Cond, yes(DummySuccessExpr),
            CondStatement, !Info),
        Statement = maybe_join_exprs(CondStatement, MaybeSuccessExpr)
    ;
        % model_semi negation, model_det goal:
        %       <succeeded = not(Goal)>
        %   ===>
        %       <do Goal>,
        %       fail

        CodeModel = model_semi, CondCodeModel = model_det,
        erl_gen_goal(model_det, InstMap, Cond, no, CondStatement, !Info),
        Statement = join_exprs(CondStatement, elds_term(elds_fail))
    ;
        % model_semi negation, model_semi goal:
        %       <succeeded = not(Goal)>
        %   ===>
        %
        %   case
        %       <Goal [[ true ]]>
        %   of
        %       fail ->
        %           <SuccessExpr> ;
        %       _ ->
        %           fail
        %   end

        CodeModel = model_semi, CondCodeModel = model_semi,

        OnSuccess = yes(elds_term(elds_true)),  % anything other than fail
        erl_gen_goal(model_semi, InstMap, Cond, OnSuccess, CondStatement,
            !Info),
        Statement = elds_case_expr(CondStatement, [FailCase, OtherCase]),
        FailCase = elds_case(elds_fail, expr_or_void(MaybeSuccessExpr)),
        OtherCase = elds_case(elds_anon_var, elds_term(elds_fail))
    ;
        CodeModel = model_semi, CondCodeModel = model_non,
        unexpected(this_file, "erl_gen_negation: nondet cond")
    ;
        CodeModel = model_non,
        unexpected(this_file, "erl_gen_negation: nondet negation")
    ).

%-----------------------------------------------------------------------------%
%
% Code for conjunctions
%

:- pred erl_gen_conj(hlds_goals::in, code_model::in, instmap::in,
    prog_context::in, maybe(elds_expr)::in, elds_expr::out,
    erl_gen_info::in, erl_gen_info::out) is det.

erl_gen_conj([], CodeModel, _InstMap0, _Context, MaybeSuccessExpr,
        Statement, !Info) :-
    % XXX implement this for other code models
    require(unify(CodeModel, model_det),
        "erl_gen_conj: CodeModel != model_det"),
    Statement = expr_or_void(MaybeSuccessExpr).
erl_gen_conj([SingleGoal], CodeModel, InstMap0, _Context, MaybeSuccessExpr,
        Statement, !Info) :-
    erl_gen_goal(CodeModel, InstMap0, SingleGoal, MaybeSuccessExpr,
        Statement, !Info).
erl_gen_conj([First | Rest], CodeModel, InstMap0, Context, MaybeSuccessExpr,
        Statement, !Info) :-
    Rest = [_ | _],
    First = hlds_goal(_, FirstGoalInfo),
    goal_info_get_determinism(FirstGoalInfo, FirstDeterminism),
    ( determinism_components(FirstDeterminism, _, at_most_zero) ->
        % the `Rest' code is unreachable
        erl_gen_goal(CodeModel, InstMap0, First, MaybeSuccessExpr,
            Statement, !Info)
    ;
        determinism_to_code_model(FirstDeterminism, FirstCodeModel),
        update_instmap(First, InstMap0, InstMap1),
        (
            FirstCodeModel = model_det,
            erl_gen_goal(model_det, InstMap0, First, no,
                FirstStatement, !Info),
            erl_gen_conj(Rest, CodeModel, InstMap1, Context, MaybeSuccessExpr,
                RestStatement, !Info),
            Statement = join_exprs(FirstStatement, RestStatement)
        ;
            FirstCodeModel = model_semi,
            erl_gen_conj(Rest, CodeModel, InstMap1, Context, MaybeSuccessExpr,
                RestStatement, !Info),
            erl_gen_goal(model_semi, InstMap0, First, yes(RestStatement),
                Statement, !Info)
        ;
            FirstCodeModel = model_non,
            sorry(this_file, "nondet code in Erlang backend")
        )
    ).

%-----------------------------------------------------------------------------%
%
% Code for disjunctions
%

:- pred erl_gen_disj(hlds_goals::in, code_model::in, instmap::in,
    prog_context::in, maybe(elds_expr)::in, elds_expr::out,
    erl_gen_info::in, erl_gen_info::out) is det.

erl_gen_disj([], CodeModel, _InstMap, _Context, _MaybeSuccessExpr,
        Statement, !Info) :-
    (
        CodeModel = model_det,
        unexpected(this_file, "erl_gen_disj: `fail' has determinism `det'")
    ;
        ( CodeModel = model_semi
        ; CodeModel = model_non
        ),
        Statement = elds_term(elds_fail)
    ).

erl_gen_disj([SingleGoal], CodeModel, InstMap, _Context, MaybeSuccessExpr,
        Statement, !Info) :-
    % Handle singleton disjunctions.
    erl_gen_goal(CodeModel, InstMap, SingleGoal, MaybeSuccessExpr,
        Statement, !Info).

erl_gen_disj([First | Rest], CodeModel, InstMap, Context, MaybeSuccessExpr,
        Statement, !Info) :-
    Rest = [_ | _],
    ( CodeModel = model_non ->
        % model_non disj:
        %
        %       <(Goal ; Goals) && SUCCEED()>
        %   ===>

        sorry(this_file, "nondet code in Erlang backend")
    ;
        % model_det/model_semi disj:
        %
        %   model_det goal:
        %       <Goal ; Goals>
        %   ===>
        %       <Goal>
        %       /* <Goals> will never be reached */
        %
        %   model_semi goal:
        %       <Goal ; Goals>
        %   ===>
        %       case Goal of
        %           fail -> Goals ;
        %           Anything -> Anything
        %       end
        %
        % TODO This can lead to contorted code when <Goal> itself is a `case'
        % expression.  In that case it would be better for <Goals> to appear in
        % the failure case of <Goal> directly.
        %

        First = hlds_goal(_, FirstGoalInfo),
        goal_info_get_code_model(FirstGoalInfo, FirstCodeModel),
        (
            FirstCodeModel = model_det,
            erl_gen_goal(model_det, InstMap, First, MaybeSuccessExpr,
                GoalStatement, !Info),
            % Is this necessary?
            erl_gen_wrap_goal(CodeModel, model_det, Context,
                GoalStatement, Statement, !Info)
        ;
            FirstCodeModel = model_semi,

            erl_gen_goal(CodeModel, InstMap, First, MaybeSuccessExpr,
                FirstStatement0, !Info),
            erl_gen_disj(Rest, CodeModel, InstMap, Context, MaybeSuccessExpr,
                RestStatement, !Info),

            % Need to do some renaming otherwise FirstStatement and
            % RestStatement end up binding the same variables which triggers a
            % (spurious) warning from the Erlang compiler.
            %
            erl_gen_info_get_module_info(!.Info, ModuleInfo),
            erl_bound_nonlocals_in_goal(ModuleInfo, InstMap, First, 
                FirstVarsSet),
            FirstVars = set.to_sorted_list(FirstVarsSet),
            erl_create_renaming(FirstVars, Subn, !Info),
            erl_rename_vars_in_expr(Subn, FirstStatement0, FirstStatement),

            erl_gen_info_new_var(Dummy, !Info),
            Statement = elds_case_expr(FirstStatement, [FailCase, OtherCase]),
            FailCase  = elds_case(elds_fail, RestStatement),
            OtherCase = elds_case(term_from_var(Dummy), expr_from_var(Dummy))
        ;
            FirstCodeModel = model_non,
            % simplify.m should get wrap commits around these.
            unexpected(this_file, "model_non disj in model_det disjunction")
        )
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "erl_code_gen.m".

%-----------------------------------------------------------------------------%
:- end_module erl_code_gen.
%-----------------------------------------------------------------------------%
