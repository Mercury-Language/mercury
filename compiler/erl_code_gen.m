%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2007-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
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
% - contexts are ignored at the moment
% - RTTI
%
%-----------------------------------------------------------------------------%

:- module erl_backend.erl_code_gen.
:- interface.

:- import_module erl_backend.elds.
:- import_module hlds.
:- import_module hlds.hlds_module.

:- import_module io.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Generate Erlang code for an entire module.
    %
:- pred erl_code_gen(module_info::in, elds::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.foreign.
:- import_module check_hlds.
:- import_module check_hlds.type_util.
:- import_module erl_backend.erl_call_gen.
:- import_module erl_backend.erl_code_util.
:- import_module erl_backend.erl_unify_gen.
:- import_module hlds.code_model.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module hlds.passes_aux.
:- import_module hlds.pred_table.
:- import_module hlds.status.
:- import_module hlds.vartypes.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module varset.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

erl_code_gen(ModuleInfo, ELDS, !IO) :-
    module_info_get_name(ModuleInfo, ModuleName),
    erl_gen_preds(ModuleInfo, ProcDefns, !IO),
    erl_gen_imports(ModuleInfo, Imports),
    filter_erlang_foreigns(ModuleInfo, ForeignDecls, ForeignBodies,
        PragmaExports, !IO),
    erl_gen_foreign_exports(ProcDefns, PragmaExports, ForeignExportDefns),
    % RTTI function definitions are added later by rtti_data_list_to_elds.
    RttiDefns = [],
    module_info_user_init_pred_procs(ModuleInfo, InitPredProcs),
    module_info_user_final_pred_procs(ModuleInfo, FinalPredProcs),
    ELDS = elds(ModuleName, Imports, ForeignDecls, ForeignBodies, ProcDefns,
        ForeignExportDefns, RttiDefns, InitPredProcs, FinalPredProcs).

:- pred erl_gen_imports(module_info::in, set(module_name)::out) is det.

erl_gen_imports(ModuleInfo, AllImports) :-
    module_info_get_all_deps(ModuleInfo, AllImports0),
    % No module needs to import itself.
    module_info_get_name(ModuleInfo, ThisModule),
    AllImports = set.delete(AllImports0, ThisModule).

:- pred filter_erlang_foreigns(module_info::in, list(foreign_decl_code)::out,
    list(foreign_body_code)::out, list(pragma_exported_proc)::out,
    io::di, io::uo) is det.

filter_erlang_foreigns(ModuleInfo, ForeignDeclCodes, ForeignBodyCodes,
        PragmaExports, !IO) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.get_backend_foreign_languages(Globals, BackendForeignLanguages),
    ( if BackendForeignLanguages = [lang_erlang] then
        true
    else
        unexpected($module, $pred, "foreign language other than Erlang")
    ),
    module_info_get_foreign_decl_codes(ModuleInfo, ForeignDeclCodeCord),
    module_info_get_foreign_body_codes(ModuleInfo, ForeignBodyCodeCord),
    module_info_get_pragma_exported_procs(ModuleInfo, PragmaExportsCord),
    foreign.filter_decls(lang_erlang, cord.list(ForeignDeclCodeCord),
        ForeignDeclCodes, _OtherForeignDeclCodes),
    foreign.filter_bodys(lang_erlang, cord.list(ForeignBodyCodeCord),
        ForeignBodyCodes, _OtherForeignBodyCodes),
    foreign.filter_exports(lang_erlang, cord.list(PragmaExportsCord),
        PragmaExports, _OtherForeignExports).

%-----------------------------------------------------------------------------%

:- pred erl_gen_preds(module_info::in, list(elds_defn)::out, io::di, io::uo)
    is det.

erl_gen_preds(ModuleInfo, PredDefns, !IO) :-
    module_info_get_preds(ModuleInfo, PredTable),
    map.keys(PredTable, PredIds),
    erl_gen_preds_2(ModuleInfo, PredIds, PredTable, [], RevPredDefns, !IO),
    PredDefns = list.reverse(RevPredDefns).

:- pred erl_gen_preds_2(module_info::in, list(pred_id)::in, pred_table::in,
    list(elds_defn)::in, list(elds_defn)::out, io::di, io::uo) is det.

erl_gen_preds_2(ModuleInfo, PredIds0, PredTable, !Defns, !IO) :-
    (
        PredIds0 = [PredId | PredIds],
        map.lookup(PredTable, PredId, PredInfo),
        pred_info_get_status(PredInfo, PredStatus),
        ( if
            (
                PredStatus = pred_status(status_imported(_))
            ;
                % XXX comment was from ml_code_gen.m, don't know if it applies.
                % We generate incorrect and unnecessary code for the external
                % special preds which are pseudo_imported, so just ignore them.
                is_unify_or_compare_pred(PredInfo),
                PredStatus =
                    pred_status(status_external(status_pseudo_imported))
            )
        then
            true
        else
            % Generate ELDS definitions for all the predicate's
            % non-imported procedures.
            ( if PredStatus = pred_status(status_external(_)) then
                ProcIds = pred_info_procids(PredInfo)
            else
                ProcIds = pred_info_non_imported_procids(PredInfo)
            ),
            erl_gen_pred(ModuleInfo, PredId, PredInfo, ProcIds, !Defns, !IO)
        ),
        erl_gen_preds_2(ModuleInfo, PredIds, PredTable, !Defns, !IO)
    ;
        PredIds0 = []
    ).

    % Generate ELDS definitions for all the specified procedures
    % of a given predicate (or function).
    %
:- pred erl_gen_pred(module_info::in, pred_id::in, pred_info::in,
    list(proc_id)::in, list(elds_defn)::in, list(elds_defn)::out,
    io::di, io::uo) is det.

erl_gen_pred(ModuleInfo, PredId, PredInfo, ProcIds, !Defns, !IO) :-
    (
        ProcIds = []
    ;
        ProcIds = [_ | _],
        write_pred_progress_message("% Generating ELDS code for ",
            PredId, ModuleInfo, !IO),
        pred_info_get_proc_table(PredInfo, ProcTable),
        erl_gen_procs(ProcIds, ModuleInfo, PredId, PredInfo, ProcTable, !Defns)
    ).

:- pred erl_gen_procs(list(proc_id)::in, module_info::in, pred_id::in,
    pred_info::in, proc_table::in, list(elds_defn)::in, list(elds_defn)::out)
    is det.

erl_gen_procs([], _, _, _, _, !Defns).
erl_gen_procs([ProcId | ProcIds], ModuleInfo, PredId, PredInfo, ProcTable,
        !Defns) :-
    map.lookup(ProcTable, ProcId, ProcInfo),
    ( if
        erl_maybe_gen_simple_special_pred(ModuleInfo, PredId, ProcId,
            PredInfo, ProcInfo, !Defns)
    then
        true
    else
        erl_gen_proc(ModuleInfo, PredId, ProcId, PredInfo, ProcInfo, !Defns)
    ),
    erl_gen_procs(ProcIds, ModuleInfo, PredId, PredInfo, ProcTable, !Defns).

%-----------------------------------------------------------------------------%

    % erl_maybe_gen_simple_special_pred(ModuleInfo, PredId, ProcId,
    %   PredInfo, ProcInfo, !Defns)
    %
    % If the procedure is a compiler generated unification or comparison
    % procedure, and the arguments are ground, and the values of the types they
    % are comparing do not have user-defined equality or comparison then
    % generate simpler versions of those procedures using the Erlang comparison
    % operators.  Otherwise fail.
    %
:- pred erl_maybe_gen_simple_special_pred(module_info::in,
    pred_id::in, proc_id::in, pred_info::in, proc_info::in,
    list(elds_defn)::in, list(elds_defn)::out) is semidet.

erl_maybe_gen_simple_special_pred(ModuleInfo, PredId, ProcId,
        PredInfo, ProcInfo, !Defns) :-
    PredName = pred_info_name(PredInfo),
    PredArity = pred_info_orig_arity(PredInfo),
    special_pred_name_arity(SpecialId, _, PredName, PredArity),
    proc_info_get_headvars(ProcInfo, Args),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    (
        SpecialId = spec_pred_unify,
        in_in_unification_proc_id(ProcId),
        list.reverse(Args, [Y, X | _]),
        lookup_var_type(VarTypes, Y, Type),
        check_dummy_type(ModuleInfo, Type) = is_not_dummy_type,
        type_definitely_has_no_user_defined_equality_pred(ModuleInfo, Type),
        erl_gen_simple_in_in_unification(ModuleInfo, PredId, ProcId, X, Y,
            ProcDefn)
    ;
        SpecialId = spec_pred_compare,
        list.reverse(Args, [Y, X, _Res | _]),
        lookup_var_type(VarTypes, Y, Type),
        check_dummy_type(ModuleInfo, Type) = is_not_dummy_type,
        type_definitely_has_no_user_defined_equality_pred(ModuleInfo, Type),
        erl_gen_simple_compare(ModuleInfo, PredId, ProcId, X, Y, ProcDefn)
    ),
    !:Defns = [ProcDefn | !.Defns].

:- pred erl_gen_simple_in_in_unification(module_info::in,
    pred_id::in, proc_id::in, prog_var::in, prog_var::in, elds_defn::out)
    is det.

erl_gen_simple_in_in_unification(ModuleInfo, PredId, ProcId, X, Y, ProcDefn) :-
    Info = erl_gen_info_init(ModuleInfo, PredId, ProcId),
    erl_gen_info_get_input_vars(Info, InputVars),

    %   '__Unify__'(X, Y) ->
    %       case X =:= Y of
    %           true -> {};
    %           false -> fail
    %       end.

    Clause = elds_clause(terms_from_vars(InputVars), ClauseExpr),
    ClauseExpr = elds_case_expr(CompareXY, [TrueCase, FalseCase]),
    CompareXY = elds_binop((=:=), expr_from_var(X), expr_from_var(Y)),
    TrueCase = elds_case(elds_true, elds_term(elds_empty_tuple)),
    FalseCase = elds_case(elds_false, elds_term(elds_fail)),

    erl_gen_info_get_varset(Info, ProcVarSet),
    erl_gen_info_get_env_vars(Info, EnvVarNames),
    ProcDefn = elds_defn(proc(PredId, ProcId), ProcVarSet,
        body_defined_here(Clause), EnvVarNames).

:- pred erl_gen_simple_compare(module_info::in, pred_id::in, proc_id::in,
    prog_var::in, prog_var::in, elds_defn::out) is det.

erl_gen_simple_compare(ModuleInfo, PredId, ProcId, X, Y, ProcDefn) :-
    Info = erl_gen_info_init(ModuleInfo, PredId, ProcId),
    erl_gen_info_get_input_vars(Info, InputVars),

    XExpr = expr_from_var(X),
    YExpr = expr_from_var(Y),

    %   '__Compare__'(X, Y) ->
    %       case X =:= Y of
    %           true -> {'='};
    %           false ->
    %               case X < Y of
    %                   true -> {'<'};
    %                   false -> {'>'};
    %               end
    %       end.
    %
    Clause = elds_clause(terms_from_vars(InputVars), ClauseExpr),
    ClauseExpr = elds_case_expr(CondEq, [IsEq, IsNotEq]),

    CondEq = elds_binop((=:=), XExpr, YExpr),
    IsEq = elds_case(elds_true, elds_term(make_enum_alternative("="))),
    IsNotEq = elds_case(elds_false, elds_case_expr(CondLt, [IsLt, IsGt])),

    CondLt = elds_binop((<), XExpr, YExpr),
    IsLt = elds_case(elds_true, elds_term(make_enum_alternative("<"))),
    IsGt = elds_case(elds_false, elds_term(make_enum_alternative(">"))),

    erl_gen_info_get_varset(Info, ProcVarSet),
    erl_gen_info_get_env_vars(Info, EnvVarNames),
    ProcDefn = elds_defn(proc(PredId, ProcId), ProcVarSet,
        body_defined_here(Clause), EnvVarNames).

%-----------------------------------------------------------------------------%
%
% Code for handling individual procedures
%

    % Generate ELDS code for the specified procedure.
    %
:- pred erl_gen_proc(module_info::in, pred_id::in, proc_id::in, pred_info::in,
    proc_info::in, list(elds_defn)::in, list(elds_defn)::out) is det.

erl_gen_proc(ModuleInfo, PredId, ProcId, PredInfo, ProcInfo, !Defns) :-
    erl_gen_proc_defn(ModuleInfo, PredId, ProcId, PredInfo, ProcInfo,
        ProcVarSet, ProcBody, EnvVarNames),
    ProcDefn = elds_defn(proc(PredId, ProcId), ProcVarSet, ProcBody,
        EnvVarNames),
    !:Defns = [ProcDefn | !.Defns].

    % Generate an ELDS definition for the specified procedure.
    %
:- pred erl_gen_proc_defn(module_info::in, pred_id::in, proc_id::in,
    pred_info::in, proc_info::in, prog_varset::out, elds_body::out,
    set(string)::out) is det.

erl_gen_proc_defn(ModuleInfo, PredId, ProcId, PredInfo, ProcInfo,
        ProcVarSet, ProcBody, EnvVarNames) :-
    pred_info_get_status(PredInfo, PredStatus),
    CodeModel = proc_info_interface_code_model(ProcInfo),
    proc_info_get_headvars(ProcInfo, HeadVars),
    proc_info_get_initial_instmap(ProcInfo, ModuleInfo, InstMap0),
    proc_info_get_goal(ProcInfo, Goal0),

    % The HLDS front-end sometimes over-estimates the set of non-locals.
    % We need to restrict the set of non-locals for the top-level goal
    % to just the headvars, because otherwise variables which occur in the
    % top-level non-locals but which are not really non-local will not be
    % declared.

    Goal0 = hlds_goal(GoalExpr, GoalInfo0),
    NonLocals0 = goal_info_get_code_gen_nonlocals(GoalInfo0),
    set_of_var.list_to_set(HeadVars, HeadVarsSet),
    set_of_var.intersect(HeadVarsSet, NonLocals0, NonLocals),
    goal_info_set_code_gen_nonlocals(NonLocals, GoalInfo0, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo),

    _Context = goal_info_get_context(GoalInfo),

    some [!Info] (
        !:Info = erl_gen_info_init(ModuleInfo, PredId, ProcId),

        ( if PredStatus = pred_status(status_external(_)) then
            % This procedure is externally defined.
            pred_info_get_arg_types(PredInfo, ArgTypes),
            proc_info_get_argmodes(ProcInfo, ArgModes),
            erl_gen_arg_list(ModuleInfo, opt_dummy_args,
                HeadVars, ArgTypes, ArgModes, InputArgs, _OutputArgs),
            (
                ( CodeModel = model_det
                ; CodeModel = model_semi
                ),
                Arity = list.length(InputArgs)
            ;
                CodeModel = model_non,
                % Extra argument for success continuation.
                Arity = list.length(InputArgs) + 1
            ),
            ProcBody = body_external(Arity)
        else
            erl_gen_proc_body(CodeModel, InstMap0, Goal, ProcClause, !Info),
            ProcBody = body_defined_here(ProcClause)
        ),

        erl_gen_info_get_varset(!.Info, ProcVarSet),
        erl_gen_info_get_env_vars(!.Info, EnvVarNames)
    ).

:- pred erl_gen_proc_body(code_model::in, instmap::in, hlds_goal::in,
    elds_clause::out, erl_gen_info::in, erl_gen_info::out) is det.

erl_gen_proc_body(CodeModel, InstMap0, Goal, ProcClause, !Info) :-
    erl_gen_info_get_input_vars(!.Info, InputVars),
    erl_gen_info_get_output_vars(!.Info, OutputVars),
    OutputVarsExprs = exprs_from_vars(OutputVars),
    (
        CodeModel = model_det,
        InputVarsTerms = terms_from_vars(InputVars),
        %
        % On success, the procedure returns either:
        % - the single output variable, or
        % - a tuple of its output variables if there are zero or two or more
        %   output variables.
        %
        SuccessExpr = tuple_or_single_expr(OutputVarsExprs),
        InstMap = InstMap0
    ;
        CodeModel = model_semi,
        InputVarsTerms = terms_from_vars(InputVars),
        %
        % On success, the procedure returns a tuple of its output variables.
        %
        SuccessExpr = elds_term(elds_tuple(OutputVarsExprs)),
        InstMap = InstMap0
    ;
        CodeModel = model_non,
        %
        % On success, the procedure calls a continuation, passing the values of
        % its output variables as arguments.  The continuation is supplied as
        % an extra argument to the current procedure.
        %
        erl_gen_info_new_named_var("SucceedHeadVar", SucceedVar, !Info),
        ground_var_in_instmap(SucceedVar, InstMap0, InstMap),
        InputVarsTerms = terms_from_vars(InputVars ++ [SucceedVar]),
        SuccessExpr = elds_call(elds_call_ho(expr_from_var(SucceedVar)),
            OutputVarsExprs)
    ),
    erl_gen_goal(CodeModel, InstMap, Goal, yes(SuccessExpr), Statement, !Info),
    ProcClause = elds_clause(InputVarsTerms, Statement).

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
    % succeeds.  MaybeSuccessExpr can only be `no' for model_det code.
    % On failure, model_semi code returns the atom `fail'.
    % On failure, model_non code may return anything.
    %
erl_gen_goal(CodeModel, InstMap, Goal, MaybeSuccessExpr0, Code, !Info) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    Context = goal_info_get_context(GoalInfo),
    GoalCodeModel = goal_info_get_code_model(GoalInfo),
    ( if
        (
            CodeModel = model_det,
            GoalCodeModel = model_semi
        ;
            CodeModel = model_det,
            GoalCodeModel = model_non
        ;
            CodeModel = model_semi,
            GoalCodeModel = model_non
        )
    then
        unexpected($module, $pred, "code model mismatch")
    else
        Determinism = goal_info_get_determinism(GoalInfo),
        (
            Determinism = detism_erroneous,
            % This goal can't succeed. Don't pass a success expression
            % which, if inserted into the generated code, could contain
            % references to unbound variables (since the goal may have
            % aborted before binding them).
            MaybeSuccessExpr = no
        ;
            Determinism = detism_failure,
            % This goal can't succeed.  As above we don't want to pass a
            % success expression, but we must pass something to maintain the
            % invariant that a model_semi goal has a success expression.
            MaybeSuccessExpr = yes(elds_term(elds_fail))
        ;
            ( Determinism = detism_det
            ; Determinism = detism_semi
            ; Determinism = detism_non
            ; Determinism = detism_multi
            ; Determinism = detism_cc_non
            ; Determinism = detism_cc_multi
            ),
            MaybeSuccessExpr = MaybeSuccessExpr0
        ),
        erl_gen_goal_expr(GoalExpr, GoalCodeModel, Determinism, InstMap,
            Context, MaybeSuccessExpr, Code, !Info)
    ).

%-----------------------------------------------------------------------------%

    % Generate code for a commit.
    %
:- pred erl_gen_commit(hlds_goal::in, code_model::in, determinism::in,
    instmap::in, prog_context::in, maybe(elds_expr)::in, elds_expr::out,
    erl_gen_info::in, erl_gen_info::out) is det.

erl_gen_commit(Goal, CodeModel, ScopeDetism, InstMap, Context,
        MaybeSuccessExpr, Statement, !Info) :-
    Goal = hlds_goal(_, GoalInfo),
    GoalCodeModel = goal_info_get_code_model(GoalInfo),
    _GoalContext = goal_info_get_context(GoalInfo),

    ( if
        GoalCodeModel = model_non,
        CodeModel = model_semi
    then
        ( if ScopeDetism = detism_failure then
            % If the scope has determinism `failure' then Goal can't succeed.
            % The code is probably implementing a failure driven loop or
            % something similar.  No commit is required.
            %
            %   model_non in failure context:
            %       <succeeded = Goal>
            %   ===>
            %       <Goal && SUCCEED()>,
            %       fail
            %
            erl_gen_goal(GoalCodeModel, InstMap, Goal, MaybeSuccessExpr,
                GoalStatement, !Info),
            Statement = join_exprs(GoalStatement, elds_term(elds_fail))
        else
            %   model_non in semi context:
            %       <succeeded = Goal>
            %   ===>
            %
            %   let Throw = ``throw({'MERCURY_COMMIT', {NonLocals, ...})''
            %   where NonLocals are variables bound by Goal.
            %
            %   try
            %       <Goal && Throw()>
            %   of
            %       _ -> fail
            %   catch
            %       throw: {'MERCURY_COMMIT', {NonLocals, ...}} ->
            %           SuccessExpr
            %   end

            erl_gen_commit_pieces(Goal, InstMap, Context, no,
                GoalStatement, PackedNonLocals, !Info),

            Statement = elds_try(GoalStatement, [AnyCase], yes(Catch), no),
            AnyCase = elds_case(elds_anon_var, elds_term(elds_fail)),
            Catch = elds_catch(elds_throw_atom,
                elds_tuple([elds_commit_marker, PackedNonLocals]),
                det_expr(MaybeSuccessExpr))
        )
    else if
        GoalCodeModel = model_non,
        CodeModel = model_det
    then
        %   model_non in det context:
        %       <do Goal>
        %   ===>
        %
        %   let Throw = ``throw({'MERCURY_COMMIT', {NonLocals, ...}})''
        %   where NonLocals are variables bound by Goal.
        %
        %   {NonLocals, ...} =
        %       (try
        %           <Goal && Throw()>
        %       catch
        %           throw: {'MERCURY_COMMIT', Results} -> Results
        %       end)

        erl_gen_commit_pieces(Goal, InstMap, Context, yes,
            GoalStatement, PackedNonLocals, !Info),

        erl_gen_info_new_named_var("Results", ResultsVar, !Info),
        ResultsVarExpr = expr_from_var(ResultsVar),

        Statement = elds_eq(PackedNonLocals, TryExpr),
        TryExpr = elds_try(GoalStatement, [], yes(Catch), no),
        Catch = elds_catch(elds_throw_atom,
            elds_tuple([elds_commit_marker, ResultsVarExpr]), ResultsVarExpr)
    else
        % No commit required.
        erl_gen_goal(CodeModel, InstMap, Goal, MaybeSuccessExpr, Statement,
            !Info)
    ).

:- pred erl_gen_commit_pieces(hlds_goal::in, instmap::in, prog_context::in,
    bool::in, elds_expr::out, elds_expr::out,
    erl_gen_info::in, erl_gen_info::out) is det.

erl_gen_commit_pieces(Goal, InstMap, _Context, DoRenaming,
        GoalStatement, PackedNonLocals, !Info) :-
    % Find the nonlocal variables bound by the goal.
    erl_bound_nonlocals_in_goal(!.Info, InstMap, Goal, NonLocalsSet),
    NonLocals = set_of_var.to_sorted_list(NonLocalsSet),

    % Throw = ``throw({'MERCURY_COMMIT', {NonLocals, ...})''
    Throw = elds_throw(elds_term(ThrowValue)),
    ThrowValue = elds_tuple([elds_commit_marker, PackedNonLocals]),
    PackedNonLocals = elds_term(elds_tuple(exprs_from_vars(NonLocals))),

    % Generate the goal expression such that it throws the exception
    % at the first solution.
    erl_gen_goal(model_non, InstMap, Goal, yes(Throw), GoalStatement0, !Info),

    % Rename the nonlocal variables in the generated expression if we have to.
    (
        DoRenaming = yes,
        erl_create_renaming(NonLocals, Subn, !Info),
        erl_rename_vars_in_expr(Subn, GoalStatement0, GoalStatement)
    ;
        DoRenaming = no,
        GoalStatement = GoalStatement0
    ).

%-----------------------------------------------------------------------------%

    % Generate ELDS code for the different kinds of HLDS goals.
    %
:- pred erl_gen_goal_expr(hlds_goal_expr::in, code_model::in, determinism::in,
    instmap::in, prog_context::in, maybe(elds_expr)::in,
    elds_expr::out, erl_gen_info::in, erl_gen_info::out) is det.

erl_gen_goal_expr(GoalExpr, CodeModel, Detism, InstMap, Context,
        MaybeSuccessExpr, Statement, !Info) :-
    (
        GoalExpr = switch(Var, CanFail, CasesList),
        erl_gen_switch(Var, CanFail, CasesList, CodeModel, InstMap,
            Context, MaybeSuccessExpr, Statement, !Info)
    ;
        GoalExpr = scope(Reason, SubGoal),
        (
            ( Reason = promise_solutions(_, _)
            ; Reason = commit(_)
            ),
            erl_gen_commit(SubGoal, CodeModel, Detism, InstMap, Context,
                MaybeSuccessExpr, Statement, !Info)
        ;
            Reason = loop_control(_, _, _),
            unexpected($module, $pred, "loop_control")
        ;
            Reason = require_detism(_),
            unexpected($module, $pred, "require_detism")
        ;
            Reason = require_complete_switch(_),
            unexpected($module, $pred, "require_complete_switch")
        ;
            Reason = require_switch_arms_detism(_, _),
            unexpected($module, $pred, "require_complete_switch")
        ;
            ( Reason = disable_warnings(_, _)
            ; Reason = exist_quant(_)
            ; Reason = promise_purity(_)
            ; Reason = barrier(_)
            ; Reason = from_ground_term(_, _)
            ; Reason = trace_goal(_, _, _, _, _)
                % Trace goals with run-time conditions are transformed into
                % if-then-else goals where the condition is a special
                % foreign_proc call and the then branch is the actual
                % trace goal (i.e. this goal). Thus there is nothing special
                % we have to do here.
            ),
            SubGoal = hlds_goal(SubGoalExpr, SubGoalInfo),
            SubGoalDetism = goal_info_get_determinism(SubGoalInfo),
            erl_gen_goal_expr(SubGoalExpr, CodeModel, SubGoalDetism,
                InstMap, Context, MaybeSuccessExpr, Statement, !Info)
        )
    ;
        GoalExpr = if_then_else(_Vars, Cond, Then, Else),
        erl_gen_ite(CodeModel, InstMap, Cond, Then, Else, Context,
            MaybeSuccessExpr, Statement, !Info)
    ;
        GoalExpr = negation(SubGoal),
        erl_gen_negation(SubGoal, CodeModel, InstMap, Context,
            MaybeSuccessExpr, Statement, !Info)
    ;
        GoalExpr = conj(_ConjType, Goals),
        % XXX Currently we treat parallel conjunction the same as
        % sequential conjunction -- parallelism is not yet implemented.
        erl_gen_conj(Goals, CodeModel, Detism, InstMap, Context,
            MaybeSuccessExpr, Statement, !Info)
    ;
        GoalExpr = disj(Goals),
        erl_gen_disj(Goals, CodeModel, InstMap, Context, MaybeSuccessExpr,
            Statement, !Info)
    ;
        GoalExpr = generic_call(GenericCall, Vars, Modes, _, CallDetism),
        determinism_to_code_model(CallDetism, CallCodeModel),
        expect(unify(CodeModel, CallCodeModel), $module, $pred,
            "code model mismatch"),
        (
            GenericCall = higher_order(_, _, _, _),
            erl_gen_higher_order_call(GenericCall, Vars, Modes, CallDetism,
                Context, MaybeSuccessExpr, Statement, !Info)
        ;
            GenericCall = class_method(_, _, _, _),
            erl_gen_class_method_call(GenericCall, Vars, Modes, CallDetism,
                Context, MaybeSuccessExpr, Statement, !Info)
        ;
            GenericCall = event_call(_),
            sorry($module, $pred, "event_calls in erlang backend")
        ;
            GenericCall = cast(_),
            erl_gen_cast(Context, Vars, MaybeSuccessExpr, Statement, !Info)
        )
    ;
        GoalExpr = plain_call(PredId, ProcId, ArgVars, BuiltinState, _, _),
        (
            BuiltinState = not_builtin,
            erl_variable_types(!.Info, ArgVars, ActualArgTypes),
            erl_gen_call(PredId, ProcId, ArgVars, ActualArgTypes,
                CodeModel, Context, MaybeSuccessExpr, Statement, !Info)
        ;
            BuiltinState = inline_builtin,
            erl_gen_builtin(PredId, ProcId, ArgVars, CodeModel, Context,
                MaybeSuccessExpr, Statement, !Info)
        )
    ;
        GoalExpr = unify(_LHS, _RHS, _Mode, Unification, _UnifyContext),
        erl_gen_unification(Unification, CodeModel, Context, MaybeSuccessExpr,
            Statement, !Info)
    ;
        GoalExpr = call_foreign_proc(_Attributes, _PredId, _ProcId,
            Args, _ExtraArgs, MaybeTraceRuntimeCond, PragmaImpl),
        erl_gen_foreign_proc_call(Args, MaybeTraceRuntimeCond, PragmaImpl,
            CodeModel, Context, MaybeSuccessExpr, Statement, !Info)
    ;
        GoalExpr = shorthand(_),
        % These should have been expanded out by now.
        unexpected($module, $pred, "shorthand")
    ).

%-----------------------------------------------------------------------------%
%
% Code for switches.
%

:- func duplicate_expr_limit = int.

duplicate_expr_limit = 10.  % XXX arbitrary

:- func switch_strings_as_atoms_limit = int.

switch_strings_as_atoms_limit = 50. % XXX arbitrary

:- pred erl_gen_switch(prog_var::in, can_fail::in, list(hlds_goal.case)::in,
    code_model::in, instmap::in, prog_context::in, maybe(elds_expr)::in,
    elds_expr::out, erl_gen_info::in, erl_gen_info::out) is det.

erl_gen_switch(Var, CanFail, CasesList, CodeModel, InstMap0, _Context,
        MaybeSuccessExpr0, Statement, !Info) :-
    %
    % If the success expression is not too big, then we generate code for
    % a switch like this:
    %
    %   case Var of
    %       Pattern1 -> Expr1  [[ SuccessExpr ]] ;
    %       Pattern2 -> Expr2  [[ SuccessExpr ]] ;
    %       ...
    %   end
    %
    % Otherwise the success expression is put into a closure and the closure
    % is called on success of each case:
    %
    %   SuccessClosure = fun(Vars, ...) ->
    %       /* Vars are those variables bound by Expr<n> */
    %       SuccessExpr
    %   end,
    %   case Var of
    %       Pattern1 -> Expr1  [[ SuccessClosure() ]] ;
    %       Pattern2 -> Expr2  [[ SuccessClosure() ]] ;
    %       ...
    %   end
    %
    % If the switch can fail, a default case is added:
    %
    %   _ -> fail
    %

    % Get the union of all nonlocal variables bound in all cases.
    CasesGoals = list.map((func(case(_, _, Goal)) = Goal), CasesList),
    union_bound_nonlocals_in_goals(!.Info, InstMap0, CasesGoals,
        NonLocalsBoundInCases),

    % Create a closure for the success expression if it is too large to
    % duplicate into the disjuncts.
    maybe_create_closure_for_success_expr(NonLocalsBoundInCases,
        MaybeSuccessExpr0, MaybeMakeClosure, MaybeSuccessExpr,
        InstMap0, InstMap, !Info),

    erl_variable_type(!.Info, Var, VarType),
    erl_gen_info_get_module_info(!.Info, ModuleInfo),
    type_util.classify_type(ModuleInfo, VarType) = TypeCtorCategory,

    ( if
        % The HiPE compiler is extremely slow compiling functions containing
        % long case statements involving strings.  Workaround: for a string
        % switch with many cases, convert the string to an atom and switch on
        % atoms instead.
        TypeCtorCategory = ctor_cat_builtin(cat_builtin_string),

        % list_to_atom could throw an exception for long strings, so we don't
        % enable the workaround unless the user specifically passes
        % --erlang-switch-on-strings-as-atoms.
        module_info_get_globals(ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, erlang_switch_on_strings_as_atoms,
            yes),

        list.length(CasesList) > switch_strings_as_atoms_limit,

        % The Erlang implementation limits atoms to be 255 characters (bytes)
        % long or less, so we don't use the workaround if any cases are longer
        % than that.
        all [String] (
            (
                list.member(case(MainConsId, OtherConsIds, _), CasesList),
                (
                    MainConsId = string_const(String)
                ;
                    list.member(string_const(String), OtherConsIds)
                )
            )
        =>
            string.length(String) =< 255
        )
    then
        % Atom = list_to_atom(binary_to_list(Var))
        erl_gen_info_new_named_var("Atom", AtomVar, !Info),
        CharList = elds_call_builtin("binary_to_list", [expr_from_var(Var)]),
        StringToAtom = elds_eq(expr_from_var(AtomVar),
            elds_call_builtin("list_to_atom", [CharList])),
        MaybeConvertToAtom = yes(StringToAtom),
        SwitchVar = AtomVar,
        GenCase = erl_gen_case_on_atom(CodeModel, InstMap,
            NonLocalsBoundInCases, MaybeSuccessExpr)
    else
        MaybeConvertToAtom = no,
        SwitchVar = Var,
        GenCase = erl_gen_case(VarType,
            CodeModel, InstMap, NonLocalsBoundInCases, MaybeSuccessExpr)
    ),

    % Generate code for each case.
    list.map_foldl(GenCase, CasesList, ErlCases0, !Info),
    (
        CanFail = can_fail,
        % Add `_ -> fail' default case.
        DefaultCase = elds_case(elds_anon_var, elds_term(elds_fail)),
        ErlCases = ErlCases0 ++ [DefaultCase]
    ;
        CanFail = cannot_fail,
        ErlCases = ErlCases0
    ),

    % Create the overall switch statement,.
    CaseExpr = elds_case_expr(expr_from_var(SwitchVar), ErlCases),
    Statement = maybe_join_exprs1(MaybeMakeClosure,
        maybe_join_exprs1(MaybeConvertToAtom, CaseExpr)).

:- pred erl_gen_case(mer_type::in,
    code_model::in, instmap::in, set_of_progvar::in,
    maybe(elds_expr)::in, hlds_goal.case::in, elds_case::out,
    erl_gen_info::in, erl_gen_info::out) is det.

erl_gen_case(Type, CodeModel, InstMap, MustBindNonLocals, MaybeSuccessExpr,
        Case, ELDSCase, !Info) :-
    Case = case(MainConsId, OtherConsIds, Goal),
    expect(unify(OtherConsIds, []), $module, $pred,
        "multi-cons-id switch arms NYI"),
    erl_gen_info_get_module_info(!.Info, ModuleInfo),
    Size = cons_id_size(ModuleInfo, Type, MainConsId),
    erl_gen_info_new_anonymous_vars(Size, DummyVars, !Info),
    ( if
        cons_id_to_term(MainConsId, DummyVars, elds_anon_var, Pattern0, !Info)
    then
        Pattern = Pattern0
    else
        unexpected($module, $pred, "cannot pattern match on object")
    ),
    erl_fix_success_expr(InstMap, Goal, MaybeSuccessExpr,
        MaybeSuccessExprForCase, !Info),
    erl_gen_goal(CodeModel, InstMap, Goal, MaybeSuccessExprForCase, Statement0,
        !Info),

    % To prevent warnings from the Erlang compiler we must make sure all cases
    % bind the same set of variables.  This might not be true if the Mercury
    % compiler knows that a case calls a procedure which throws an exception.

    erl_bind_unbound_vars(!.Info, MustBindNonLocals, Goal, InstMap,
        Statement0, Statement),
    ELDSCase = elds_case(Pattern, Statement).

    % cons_id_size(ModuleInfo, Type, ConsId)
    %
    % Returns the size - 1 of the tuple which represents the type, Type,
    % with cons_id, ConsId.
    %
:- func cons_id_size(module_info, mer_type, cons_id) = int.

cons_id_size(ModuleInfo, Type, ConsId) = Size :-
    ( if
        type_to_ctor(Type, TypeCtor),
        get_cons_defn(ModuleInfo, TypeCtor, ConsId, ConsDefn)
    then
        % There will be a cell for each existential type variable
        % which isn't mentioned in a typeclass constraint and
        % for each constraint, as well as for each arg.

        list.length(ConsDefn ^ cons_args, NumArgs),
        MaybeExistConstraints = ConsDefn ^ cons_maybe_exist,
        (
            MaybeExistConstraints = no_exist_constraints,
            Size = NumArgs
        ;
            MaybeExistConstraints = exist_constraints(
                cons_exist_constraints(ExistTVars, Constraints)),
            constraint_list_get_tvars(Constraints, ConstrainedTVars),
            UnconstrainedTVars =
                list.delete_elems(ExistTVars, ConstrainedTVars),
            Size = NumArgs +
                list.length(UnconstrainedTVars) + list.length(Constraints)
        )
    else
        Size = 0
    ).

:- pred erl_gen_case_on_atom(code_model::in, instmap::in, set_of_progvar::in,
    maybe(elds_expr)::in, hlds_goal.case::in, elds_case::out,
    erl_gen_info::in, erl_gen_info::out) is det.

erl_gen_case_on_atom(CodeModel, InstMap, MustBindNonLocals, MaybeSuccessExpr,
        Case, ELDSCase, !Info) :-
    Case = case(MainConsId, OtherConsIds, Goal),
    expect(unify(OtherConsIds, []), $module, $pred,
        "multi-cons-id switch arms NYI"),
    ( if MainConsId = string_const(String0) then
        String = String0
    else
        unexpected($module, $pred, "non-string const")
    ),
    erl_fix_success_expr(InstMap, Goal, MaybeSuccessExpr,
        MaybeSuccessExprForCase, !Info),
    erl_gen_goal(CodeModel, InstMap, Goal, MaybeSuccessExprForCase, Statement0,
        !Info),

    % To prevent warnings from the Erlang compiler we must make sure all cases
    % bind the same set of variables.  This might not be true if the Mercury
    % compiler knows that a case calls a procedure which throws an exception.

    erl_bind_unbound_vars(!.Info, MustBindNonLocals, Goal, InstMap,
        Statement0, Statement),
    ELDSCase = elds_case(elds_atom_raw(String), Statement).

%-----------------------------------------------------------------------------%
%
% This code is shared by disjunctions and switches.
%

:- pred union_bound_nonlocals_in_goals(erl_gen_info::in, instmap::in,
    hlds_goals::in, set_of_progvar::out) is det.

union_bound_nonlocals_in_goals(Info, InstMap, Goals, NonLocalsUnion) :-
    IsBound = erl_bound_nonlocals_in_goal(Info, InstMap),
    list.map(IsBound, Goals, NonLocalsLists),
    NonLocalsUnion = set_of_var.union_list(NonLocalsLists).

    % If a success expression is too large to duplicate but is required after
    % two or more goals Gs, we generate a closure C containing the success
    % expression which takes the nonlocal variables bound by Gs as arguments.
    % Then we generate the code for Gs such that they call C on success.
    %
:- pred maybe_create_closure_for_success_expr(set_of_progvar::in,
    maybe(elds_expr)::in, maybe(elds_expr)::out, maybe(elds_expr)::out,
    instmap::in, instmap::out, erl_gen_info::in, erl_gen_info::out) is det.

maybe_create_closure_for_success_expr(NonLocals, MaybeSuccessExpr0,
        MaybeMakeClosure, MaybeSuccessExpr, InstMap0, InstMap, !Info) :-
    ( if
        MaybeSuccessExpr0 = yes(SuccessExpr0),
        erl_expr_size(SuccessExpr0) > duplicate_expr_limit
    then
        erl_gen_info_new_named_var("SuccessClosure", ClosureVar, !Info),
        ground_var_in_instmap(ClosureVar, InstMap0, InstMap),
        ClosureVarExpr = expr_from_var(ClosureVar),
        ClosureArgs0 = set_of_var.to_sorted_list(NonLocals),

        % Ignore dummy variables.
        erl_gen_info_get_module_info(!.Info, ModuleInfo),
        erl_variable_types(!.Info, ClosureArgs0, ClosureArgsTypes),
        ClosureArgs = list.filter_map_corresponding(non_dummy_var(ModuleInfo),
            ClosureArgs0, ClosureArgsTypes),
        ClosureArgsTerms = terms_from_vars(ClosureArgs),
        ClosureArgsExprs = exprs_from_vars(ClosureArgs),

        % ``SuccessClosure = fun(ClosureArgs, ...) -> SuccessExpr0 end''
        MakeClosure = elds_eq(ClosureVarExpr, ClosureFun),
        ClosureFun = elds_fun(elds_clause(ClosureArgsTerms, SuccessExpr0)),

        % ``SuccessClosure(ClosureArgs, ...)''
        CallClosure = elds_call(elds_call_ho(ClosureVarExpr),
            ClosureArgsExprs),

        MaybeMakeClosure = yes(MakeClosure),
        MaybeSuccessExpr = yes(CallClosure)
    else
        InstMap = InstMap0,
        MaybeMakeClosure = no,
        MaybeSuccessExpr = MaybeSuccessExpr0
    ).

:- func non_dummy_var(module_info, prog_var, mer_type) = prog_var is semidet.

non_dummy_var(ModuleInfo, Var, Type) = Var :-
    check_dummy_type(ModuleInfo, Type) = is_not_dummy_type.

:- pred ground_var_in_instmap(prog_var::in, instmap::in, instmap::out) is det.

ground_var_in_instmap(Var, !InstMap) :-
    % Sometimes we introduce variables which aren't in the HLDS, but which need
    % to be in an instmap so that they don't get renamed away (when we
    % duplicate success expressions, we rename away all variables which were
    % not bound before the place where the success expression will be
    % inserted).  For our purposes it doesn't matter what insts these variables
    % have, other than not being free, so we just use `ground'.
    instmap_set_var(Var, ground(shared, none_or_default_func), !InstMap).

%-----------------------------------------------------------------------------%
%
% Code for if-then-elses.
%

:- pred erl_gen_ite(code_model::in, instmap::in,
    hlds_goal::in, hlds_goal::in, hlds_goal::in,
    prog_context::in, maybe(elds_expr)::in, elds_expr::out,
    erl_gen_info::in, erl_gen_info::out) is det.

erl_gen_ite(CodeModel, InstMap0, Cond, Then, Else, _Context, MaybeSuccessExpr0,
        Statement, !Info) :-
    Cond = hlds_goal(_, CondGoalInfo),
    CondCodeModel = goal_info_get_code_model(CondGoalInfo),
    (
        %   model_det Cond:
        %       <(Cond -> Then ; Else)>
        %   ===>
        %       <Cond>,
        %       <Then>

        CondCodeModel = model_det,
        erl_gen_goal(model_det, InstMap0, Cond, no, CondStatement, !Info),
        CondDeterminism = goal_info_get_determinism(CondGoalInfo),
        ( if CondDeterminism = detism_erroneous then
            % The `Then' code is unreachable.
            Statement = CondStatement
        else
            update_instmap(Cond, InstMap0, CondInstMap),
            erl_gen_goal(CodeModel, CondInstMap, Then, MaybeSuccessExpr0,
                ThenStatement, !Info),
            Statement = join_exprs(CondStatement, ThenStatement)
        )
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
        erl_bound_nonlocals_in_goal(!.Info, InstMap0, Cond, CondVars),
        update_instmap(Cond, InstMap0, InstMap0PostCond),
        erl_bound_nonlocals_in_goal(!.Info, InstMap0PostCond, Then, ThenVars),
        erl_bound_nonlocals_in_goal(!.Info, InstMap0, Else, ElseVars),
        CondVarsList = set_of_var.to_sorted_list(CondVars),

        % Generate the condition goal, making it evaluate to a tuple of the
        % non-local variables that it binds on success.
        CondVarsTerm = elds_tuple(exprs_from_vars(CondVarsList)),
        erl_gen_goal(model_semi, InstMap0, Cond,
            yes(elds_term(CondVarsTerm)), CondStatement0, !Info),

        % Rename the variables in the generated condition expression.
        erl_create_renaming(CondVarsList, Subn, !Info),
        erl_rename_vars_in_expr(Subn, CondStatement0, CondStatement),

        % Create a closure for the success expression if it is too large to
        % duplicate into the branches.
        % (InstMap1 = InstMap0 + optionally a variable bound to a closure)
        BoundNonLocals = set_of_var.union(ThenVars, ElseVars),
        maybe_create_closure_for_success_expr(BoundNonLocals,
            MaybeSuccessExpr0, MaybeMakeClosure, MaybeSuccessExpr,
            InstMap0, InstMap1, !Info),

        % Generate the Then and Else branches.
        update_instmap(Cond, InstMap1, InstMap2),
        erl_gen_goal(CodeModel, InstMap2, Then, MaybeSuccessExpr,
            ThenStatement0, !Info),
        erl_gen_goal(CodeModel, InstMap1, Else, MaybeSuccessExpr,
            ElseStatement0, !Info),

        % Make sure both branches bind the same sets of variables.
        erl_bind_unbound_vars(!.Info, ElseVars, Then, InstMap1,
            ThenStatement0, ThenStatement),
        erl_bind_unbound_vars(!.Info, ThenVars, Else, InstMap0,
            ElseStatement0, ElseStatement),

        CondDeterminism = goal_info_get_determinism(CondGoalInfo),
        ( if CondDeterminism = detism_failure then
            % If the condition cannot succeed then just concatenate the
            % condition and the else branch.
            IfStatement = join_exprs(CondStatement, ElseStatement)
        else
            CaseExpr = elds_case_expr(CondStatement, [TrueCase, FalseCase]),
            TrueCase = elds_case(CondVarsTerm, ThenStatement),
            FalseCase = elds_case(elds_anon_var, ElseStatement),
            maybe_simplify_nested_cases(CaseExpr, IfStatement)
        ),
        Statement = maybe_join_exprs1(MaybeMakeClosure, IfStatement)
    ;
        CondCodeModel = model_non,
        %
        %   model_non cond:
        %       <(Cond -> Then ; Else)>
        %   ===>
        %
        %       let PutAndThen = ``put(Ref, true), <Then && SUCCEED()>''
        %
        %       Ref = make_ref(),       /* defaults to `undefined' */
        %       try
        %           <Cond && PutAndThen>
        %           case get(Ref) of
        %               true -> true ;
        %               _    -> <Else>
        %           end,
        %       after
        %           erase(Ref)
        %       end
        %

        erl_gen_info_new_named_var("Ref", Ref, !Info),
        ground_var_in_instmap(Ref, InstMap0, InstMap1),

        RefExpr = expr_from_var(Ref),
        MakeRef = elds_eq(RefExpr, elds_call_builtin("make_ref", [])),
        PutRef = elds_call_builtin("put", [RefExpr, elds_term(elds_true)]),
        GetRef = elds_call_builtin("get", [RefExpr]),
        EraseRef = elds_call_builtin("erase", [RefExpr]),

        % Due to the way we generate code for model_non conjunctions, the
        % success expression at this point should not be too large to
        % duplicate.

        update_instmap(Cond, InstMap1, InstMap2),
        erl_gen_goal(CodeModel, InstMap2, Then, MaybeSuccessExpr0,
            ThenStatement, !Info),
        PutAndThen = join_exprs(PutRef, ThenStatement),

        erl_gen_goal(CondCodeModel, InstMap1, Cond, yes(PutAndThen),
            CondThen, !Info),

        erl_gen_goal(CodeModel, InstMap1, Else, MaybeSuccessExpr0,
            ElseStatement, !Info),

        CaseElse = elds_case_expr(GetRef, [TrueCase, OtherCase]),
        TrueCase = elds_case(elds_true, elds_term(elds_true)),
        OtherCase = elds_case(elds_anon_var, ElseStatement),

        Statement = join_exprs(MakeRef,
            elds_try(join_exprs(CondThen, CaseElse), [], no, yes(EraseRef)))
    ).

%-----------------------------------------------------------------------------%
%
% Code for negation.
%

:- pred erl_gen_negation(hlds_goal::in, code_model::in, instmap::in,
    prog_context::in, maybe(elds_expr)::in, elds_expr::out,
    erl_gen_info::in, erl_gen_info::out) is det.

erl_gen_negation(Cond, CodeModel, InstMap, _Context, MaybeSuccessExpr,
        Statement, !Info) :-
    Cond = hlds_goal(_, CondGoalInfo),
    CondCodeModel = goal_info_get_code_model(CondGoalInfo),
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
        unexpected($module, $pred, "nondet cond")
    ;
        CodeModel = model_non,
        unexpected($module, $pred, "nondet negation")
    ).

%-----------------------------------------------------------------------------%
%
% Code for conjunctions.
%

:- pred erl_gen_conj(hlds_goals::in, code_model::in, determinism::in,
    instmap::in, prog_context::in, maybe(elds_expr)::in, elds_expr::out,
    erl_gen_info::in, erl_gen_info::out) is det.

erl_gen_conj(Goals, CodeModel, Detism, InstMap, Context, MaybeSuccessExpr,
        Statement, !Info) :-
    erl_gen_conj_2(Goals, CodeModel, InstMap, Context, MaybeSuccessExpr,
        Statement0, !Info),
    ( if Detism = detism_erroneous then
        % This conjunction may be part of a conditional statement, in which
        % this branch binds some variables Vars before throwing an exception.
        % Another, non-erroneous, branch might not bind those Vars, leaving
        % them to be bound after the conditional statement.  We rename away the
        % variables bound in this branch so that the Erlang compiler won't
        % complain about variables not being bound in all branches of a
        % conditional statement.
        erl_gen_info_get_module_info(!.Info, ModuleInfo),
        instmap_bound_vars(InstMap, ModuleInfo, BoundVars),
        erl_rename_vars_in_expr_except(BoundVars, Statement0, Statement, !Info)
    else
        Statement = Statement0
    ).

:- pred erl_gen_conj_2(hlds_goals::in, code_model::in, instmap::in,
    prog_context::in, maybe(elds_expr)::in, elds_expr::out,
    erl_gen_info::in, erl_gen_info::out) is det.

erl_gen_conj_2([], CodeModel, _InstMap0, _Context, MaybeSuccessExpr,
        Statement, !Info) :-
    (
        CodeModel = model_det,
        Statement = expr_or_void(MaybeSuccessExpr)
    ;
        ( CodeModel = model_semi
        ; CodeModel = model_non
        ),
        Statement = det_expr(MaybeSuccessExpr)
    ).
erl_gen_conj_2([SingleGoal], CodeModel, InstMap0, _Context, MaybeSuccessExpr,
        Statement, !Info) :-
    erl_gen_goal(CodeModel, InstMap0, SingleGoal, MaybeSuccessExpr,
        Statement, !Info).
erl_gen_conj_2([First | Rest], CodeModel, InstMap0, Context, MaybeSuccessExpr,
        Statement, !Info) :-
    Rest = [_ | _],
    First = hlds_goal(_, FirstGoalInfo),
    FirstDeterminism = goal_info_get_determinism(FirstGoalInfo),
    determinism_components(FirstDeterminism, _, FirstMaxSolns),
    (
        FirstMaxSolns = at_most_zero,
        % The `Rest' code is unreachable.
        % There is no success expression in this case.
        erl_gen_goal(CodeModel, InstMap0, First, no, Statement, !Info)
    ;
        ( FirstMaxSolns = at_most_one
        ; FirstMaxSolns = at_most_many
        ; FirstMaxSolns = at_most_many_cc
        ),
        determinism_to_code_model(FirstDeterminism, FirstCodeModel),
        update_instmap(First, InstMap0, InstMap1),
        (
            FirstCodeModel = model_det,
            %
            %   model_det Goal:
            %       <Goal, Goals>
            %   ===>
            %       <do Goal>,
            %       <Goals>
            %
            erl_gen_goal(model_det, InstMap0, First, no,
                FirstStatement, !Info),
            erl_gen_conj_2(Rest, CodeModel, InstMap1, Context,
                MaybeSuccessExpr, RestStatement, !Info),
            Statement = join_exprs(FirstStatement, RestStatement)
        ;
            FirstCodeModel = model_semi,
            %
            %   model_semi Goal:
            %       <Goal, Goals>
            %   ===>
            %       case <Goal> of
            %           {Outputs, ...} ->
            %               <Goals> ;
            %           _ ->
            %               fail
            %       end
            %
            erl_gen_conj_2(Rest, CodeModel, InstMap1, Context,
                MaybeSuccessExpr, RestStatement, !Info),
            erl_gen_goal(model_semi, InstMap0, First, yes(RestStatement),
                Statement, !Info)
        ;
            FirstCodeModel = model_non,
            %
            %   model_non Goal:
            %       <Goal, Goals>
            %   ===>
            %       SUCCEED1 = fun(Outputs, ...) ->
            %           <Goals && SUCCEED()>
            %       end,
            %       <Goal && SUCCEED1()>
            %

            % Generate the code for Rest.
            erl_gen_conj_2(Rest, CodeModel, InstMap1, Context,
                MaybeSuccessExpr, RestStatement, !Info),

            % Find the variables bound by First.
            erl_bound_nonlocals_in_goal(!.Info, InstMap0, First, NonLocalsSet),
            NonLocals = set_of_var.to_sorted_list(NonLocalsSet),

            % Make the success continuation.  Rename apart any variables bound
            % by First to avoid warnings about the closure shadowing variables.
            SucceedFunc0 = elds_fun(elds_clause(terms_from_vars(NonLocals),
                RestStatement)),
            erl_create_renaming(NonLocals, Subst, !Info),
            erl_rename_vars_in_expr(Subst, SucceedFunc0, SucceedFunc),

            % MakeSucceed == "SucceedConj = fun(...) -> ... end "
            % CallSucceed == "SucceedConj(...)"
            erl_gen_info_new_named_var("SucceedConj", SucceedVar, !Info),
            ground_var_in_instmap(SucceedVar, InstMap0, InstMap),

            SucceedVarExpr = expr_from_var(SucceedVar),
            MakeSucceed = elds_eq(SucceedVarExpr, SucceedFunc),
            CallSucceed = elds_call(elds_call_ho(SucceedVarExpr),
                exprs_from_vars(NonLocals)),

            % Generate the code for First, such that it calls the success
            % continuation on success.
            erl_gen_goal(model_non, InstMap, First, yes(CallSucceed),
                FirstStatement, !Info),

            Statement = join_exprs(MakeSucceed, FirstStatement)
        )
    ).

%-----------------------------------------------------------------------------%
%
% Code for disjunctions.
%

:- pred erl_gen_disj(hlds_goals::in, code_model::in, instmap::in,
    prog_context::in, maybe(elds_expr)::in, elds_expr::out,
    erl_gen_info::in, erl_gen_info::out) is det.

erl_gen_disj([], CodeModel, _InstMap, _Context, _MaybeSuccessExpr,
        Statement, !Info) :-
    % Handle empty disjunctions (a.ka. `fail').
    (
        CodeModel = model_det,
        unexpected($module, $pred, "`fail' has determinism `det'")
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

erl_gen_disj([First | Rest], CodeModel, InstMap0, Context, MaybeSuccessExpr0,
        Statement, !Info) :-
    Rest = [_ | _],

    % Get the union of all nonlocal variables bound in all disjuncts.
    union_bound_nonlocals_in_goals(!.Info, InstMap0, [First | Rest],
        NonLocalsBoundInGoals),

    % Create a closure for the success expression if it is too large to
    % duplicate into the disjuncts.
    maybe_create_closure_for_success_expr(NonLocalsBoundInGoals,
        MaybeSuccessExpr0, MaybeMakeClosure, MaybeSuccessExpr,
        InstMap0, InstMap, !Info),

    erl_gen_disjunct([First | Rest], CodeModel, InstMap, Context,
        MaybeSuccessExpr, DisjStatement, !Info),
    Statement = maybe_join_exprs1(MaybeMakeClosure, DisjStatement).

:- pred erl_gen_disjunct(hlds_goals::in, code_model::in, instmap::in,
    prog_context::in, maybe(elds_expr)::in, elds_expr::out,
    erl_gen_info::in, erl_gen_info::out) is det.

erl_gen_disjunct([], CodeModel, _InstMap, _Context,
        _MaybeSuccessExpr, Statement, !Info) :-
    % Handle empty disjunctions (a.ka. `fail').
    (
        CodeModel = model_det,
        unexpected($module, $pred, "`fail' has determinism `det'")
    ;
        ( CodeModel = model_semi
        ; CodeModel = model_non
        ),
        Statement = elds_term(elds_fail)
    ).

erl_gen_disjunct([First | Rest], CodeModel, InstMap, Context,
        MaybeSuccessExpr, Statement, !Info) :-
    (
        ( CodeModel = model_det
        ; CodeModel = model_semi
        ),

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
        %       case
        %           <Goal [[ {NonLocals, ...} ]]>
        %       of
        %           {NonLocals, ...} ->
        %               <SuccessExpr> ;
        %           fail ->
        %               <Goals [[ SuccessExpr ]]>
        %       end
        %
        %   where NonLocals are variables bound by Goal.
        %

        First = hlds_goal(_, FirstGoalInfo),
        FirstCodeModel = goal_info_get_code_model(FirstGoalInfo),
        FirstDeterminism = goal_info_get_determinism(FirstGoalInfo),
        (
            FirstCodeModel = model_det,
            erl_fix_success_expr(InstMap, First, MaybeSuccessExpr,
                MaybeSuccessExprForFirst, !Info),
            erl_gen_goal(model_det, InstMap, First, MaybeSuccessExprForFirst,
                Statement, !Info)
        ;
            FirstCodeModel = model_semi,

            erl_bound_nonlocals_in_goal(!.Info, InstMap, First, FirstVarsSet),
            FirstVars = set_of_var.to_sorted_list(FirstVarsSet),
            FirstVarsTerm = elds_tuple(exprs_from_vars(FirstVars)),

            % Generate code for the first goal, making it return a tuple of the
            % nonlocal variables it binds on success.
            erl_gen_goal(model_semi, InstMap, First,
                yes(elds_term(FirstVarsTerm)), FirstStatement0, !Info),

            % Generate the rest of the disjunction.
            erl_gen_disjunct(Rest, CodeModel, InstMap, Context,
                MaybeSuccessExpr, RestStatement, !Info),

            % Need to do some renaming otherwise FirstStatement and
            % RestStatement end up binding the same variables which triggers a
            % (spurious) warning from the Erlang compiler.
            erl_create_renaming(FirstVars, Subn, !Info),
            erl_rename_vars_in_expr(Subn, FirstStatement0, FirstStatement),

            ( if FirstDeterminism = detism_failure then
                % Special case the situation when the first disjunct has
                % determinism `failure'.  This can avoid some spurious
                % warnings from the Erlang compiler about "unsafe" variables
                % (it doesn't know that a particular branch of a case
                % statement will always be taken and therefore it doesn't
                % matter that some variables aren't bound in other branches).
                Statement = join_exprs(FirstStatement0, RestStatement)
            else
                erl_fix_success_expr(InstMap, First, MaybeSuccessExpr,
                    MaybeSuccessExprForFirst, !Info),
                ( if
                    MaybeSuccessExprForFirst = yes(elds_term(FirstVarsTerm)),
                    RestStatement = elds_term(elds_fail)
                then
                    % No need to wrap this with a case expression.
                    Statement = FirstStatement
                else
                    Statement0 = elds_case_expr(FirstStatement,
                        [SucceedCase, FailCase]),
                    SucceedCase = elds_case(FirstVarsTerm,
                        expr_or_void(MaybeSuccessExprForFirst)),
                    FailCase = elds_case(elds_fail, RestStatement),
                    maybe_simplify_nested_cases(Statement0, Statement)
                )
            )
        ;
            FirstCodeModel = model_non,
            % simplify.m should get wrap commits around these.
            unexpected($module, $pred,
                "model_non disj in model_det disjunction")
        )
    ;
        CodeModel = model_non,

        % model_non disj:
        %
        %       <(Goal ; Goals) && SUCCEED()>
        %   ===>
        %       <Goal && SUCCEED()>
        %       <Goals && SUCCEED()>
        %

        % Generate the first disjunct, renaming apart variables bound by it.
        % Otherwise the second and later disjuncts would try to bind the same
        % variables to different values.
        erl_fix_success_expr(InstMap, First, MaybeSuccessExpr,
            MaybeSuccessExprForFirst, !Info),
        erl_gen_goal(model_non, InstMap, First, MaybeSuccessExprForFirst,
            FirstStatement0, !Info),

        erl_bound_nonlocals_in_goal(!.Info, InstMap, First, FirstVarsSet),
        FirstVars = set_of_var.to_sorted_list(FirstVarsSet),
        erl_create_renaming(FirstVars, Subst, !Info),
        erl_rename_vars_in_expr(Subst, FirstStatement0, FirstStatement),

        % Generate the rest of the disjunction.
        erl_gen_disjunct(Rest, model_non, InstMap, Context, MaybeSuccessExpr,
            RestStatements, !Info),

        Statement = join_exprs(FirstStatement, RestStatements)
    ).

%-----------------------------------------------------------------------------%
%
% Code for generating foreign exported procedures.
%

:- pred erl_gen_foreign_exports(list(elds_defn)::in,
    list(pragma_exported_proc)::in, list(elds_foreign_export_defn)::out)
    is det.

erl_gen_foreign_exports(ProcDefns, PragmaExports, ForeignExportDefns) :-
    list.map(erl_gen_foreign_export_defn(ProcDefns), PragmaExports,
        ForeignExportDefns).

:- pred erl_gen_foreign_export_defn(list(elds_defn)::in,
    pragma_exported_proc::in, elds_foreign_export_defn::out) is det.

erl_gen_foreign_export_defn(ProcDefns, PragmaExport, ForeignExportDefn) :-
    PragmaExport = pragma_exported_proc(_Lang, PredId, ProcId, Name, _Context),
    PredProcId = proc(PredId, ProcId),
    ( if search_elds_defn(ProcDefns, PredProcId, TargetProc) then
        TargetProc = elds_defn(_TargetPPId, _TargetVarSet, TargetBody,
            _EnvVarNames),
        Arity = elds_body_arity(TargetBody),

        % ``Name(Vars, ...) -> PredProcId(Vars, ...)''
        varset.new_vars(Arity, Vars, varset.init, VarSet),
        Clause = elds_clause(terms_from_vars(Vars),
            elds_call(elds_call_plain(PredProcId), exprs_from_vars(Vars))),
        ForeignExportDefn = elds_foreign_export_defn(Name, VarSet, Clause)
    else
        unexpected($module, $pred,
            "missing definition of foreign exported procedure")
    ).

:- pred search_elds_defn(list(elds_defn)::in, pred_proc_id::in,
    elds_defn::out) is semidet.

search_elds_defn([Defn0 | Defns], PredProcId, Defn) :-
    ( if Defn0 = elds_defn(PredProcId, _, _, _) then
        Defn = Defn0
    else
        search_elds_defn(Defns, PredProcId, Defn)
    ).

%-----------------------------------------------------------------------------%
:- end_module erl_backend.erl_code_gen.
%-----------------------------------------------------------------------------%
