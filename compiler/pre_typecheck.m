%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% Copyright (C) 2014-2018, 2021-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: pre_typecheck.m.
%
% Prepare the clauses in the HLDS for typechecking. This involves four tasks.
%
% 1 We invoke goal_path.m to fill in the goal_id slots of goals.
%   We record goal_ids in constraint_ids, which allows us later to map
%   constraints back to the goal being constrained.
%
% 2 We supply field access functions with their default clauses, if the
%   user has not explicitly provided clauses for them. This pass is the
%   compiler's first chance to decide that there won't be any user-given
%   clauses coming, and that therefore the default clauses should be
%   the actual clauses.
%
% 3 The parser puts clauses into superhomogeneous form by filling the heads
%   of clauses with compiler-generated variables with compiler-generated names
%   such as HeadVar__1. We invoke headvar_names.m to give these variables
%   their user-provided names, provided the clauses agree on the names.
%   This is important for making any type error messages about these variables
%   easier to understand.
%
% 4 We generate warnings for unused state variables in clause heads.
%
% Each of those tasks requires access to all the clauses of each predicate;
% for example, a state variable is unused in a predicate only if it is
% unused in all of the predicate's clauses. Technically, each task needs
% only all the clauses of a given predicate, but it is simpler and cheaper
% (in terms of locality) to process all predicates together in this
% compiler pass.
%
%---------------------------------------------------------------------------%

:- module check_hlds.pre_typecheck.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.

:- import_module list.

:- pred prepare_for_typecheck_module(module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.goal_path.
:- import_module hlds.headvar_names.
:- import_module hlds.hlds_args.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_markers.
:- import_module hlds.hlds_pred.
:- import_module hlds.make_hlds.
:- import_module hlds.make_hlds.state_var.
:- import_module hlds.make_goal.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.maybe_util.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module set_tree234.
:- import_module string.
:- import_module term.
:- import_module uint.

%---------------------------------------------------------------------------%

prepare_for_typecheck_module(!ModuleInfo, !Specs) :-
    module_info_get_valid_pred_id_set(!.ModuleInfo, OrigValidPredIdSet),
    module_info_get_pred_id_table(!.ModuleInfo, PredIdTable0),
    map.to_sorted_assoc_list(PredIdTable0, PredIdsInfos0),

    prepare_for_typecheck(!.ModuleInfo, OrigValidPredIdSet,
        PredIdsInfos0, PredIdsInfos, !Specs),

    map.from_sorted_assoc_list(PredIdsInfos, PredIdTable),
    module_info_set_pred_id_table(PredIdTable, !ModuleInfo).

:- pred prepare_for_typecheck(module_info::in, set_tree234(pred_id)::in,
    assoc_list(pred_id, pred_info)::in, assoc_list(pred_id, pred_info)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

prepare_for_typecheck(_, _, [], [], !Specs).
prepare_for_typecheck(ModuleInfo, ValidPredIdSet,
        [PredIdInfo0 | PredIdsInfos0], [PredIdInfo | PredIdsInfos], !Specs) :-
    some [!PredInfo] (
        PredIdInfo0 = PredId - !:PredInfo,
        ( if set_tree234.contains(ValidPredIdSet, PredId) then
            % Task 1.
            % We use goal ids to identify typeclass constraints.
            pred_info_get_clauses_info(!.PredInfo, GoalIdClausesInfo0),
            fill_goal_id_slots_in_clauses(ModuleInfo, _ContainingGoalMap,
                GoalIdClausesInfo0, GoalIdClausesInfo),
            pred_info_set_clauses_info(GoalIdClausesInfo, !PredInfo),

            % Task 2.
            maybe_add_field_access_function_clause(ModuleInfo, !PredInfo),

            % Task 3.
            module_info_get_globals(ModuleInfo, Globals),
            maybe_improve_headvar_names(Globals, MaybeLookForUnusedSVars,
                !PredInfo),

            % Task 4.
            (
                MaybeLookForUnusedSVars = do_not_look_for_unused_statevars
            ;
                MaybeLookForUnusedSVars =
                    look_for_unused_statevars(HeadVarNames),
                warn_about_any_unused_or_nonupdated_statevars(!.PredInfo,
                    HeadVarNames, !Specs)
            ),
            PredIdInfo = PredId - !.PredInfo
        else
            PredIdInfo = PredIdInfo0
        )
    ),
    prepare_for_typecheck(ModuleInfo, ValidPredIdSet,
        PredIdsInfos0, PredIdsInfos, !Specs).

%---------------------------------------------------------------------------%

    % For a field access function for which the user has supplied
    % a declaration but no clauses, add a clause
    % 'foo :='(X, Y) = 'foo :='(X, Y).
    % As for the default clauses added for builtins, this is not a recursive
    % call -- post_typecheck.m will expand the body into unifications.
    %
:- pred maybe_add_field_access_function_clause(module_info::in,
    pred_info::in, pred_info::out) is det.

maybe_add_field_access_function_clause(ModuleInfo, !PredInfo) :-
    pred_info_get_status(!.PredInfo, PredStatus),
    pred_info_get_clauses_info(!.PredInfo, ClausesInfo0),
    clauses_info_get_clauses_rep(ClausesInfo0, ClausesRep0, _ItemNumbers0),
    ( if
        pred_info_is_field_access_function(ModuleInfo, !.PredInfo),
        clause_list_is_empty(ClausesRep0) = yes,
        pred_status_defined_in_this_module(PredStatus) = yes
    then
        clauses_info_get_arg_vector(ClausesInfo0, ArgVector),
        proc_arg_vector_to_func_args(ArgVector, FuncArgs, FuncRetVal),
        pred_info_get_context(!.PredInfo, Context),
        FuncModule = pred_info_module(!.PredInfo),
        FuncName = pred_info_name(!.PredInfo),
        pred_info_get_orig_arity(!.PredInfo, PredFormArity),
        user_arity_pred_form_arity(pf_function, UserArity, PredFormArity),
        UserArity = user_arity(UserArityInt),
        FuncSymName = qualified(FuncModule, FuncName),
        FuncDuCtor =
            du_ctor(FuncSymName, UserArityInt, cons_id_dummy_type_ctor),
        FuncConsId = du_data_ctor(FuncDuCtor),
        FuncRHS = rhs_functor(FuncConsId, is_not_exist_constr, FuncArgs),
        create_pure_atomic_complicated_unification(FuncRetVal,
            FuncRHS, Context, umc_explicit, [], Goal0),
        Goal0 = hlds_goal(GoalExpr, GoalInfo0),
        NonLocals = set_of_var.list_to_set(proc_arg_vector_to_list(ArgVector)),
        goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo),
        Goal = hlds_goal(GoalExpr, GoalInfo),
        Clause = clause(all_modes, Goal, impl_lang_mercury, Context,
            [], init_unused_statevar_arg_map, clause_is_not_a_fact),
        set_clause_list([Clause], ClausesRep),
        ItemNumbers = init_clause_item_numbers_comp_gen,
        clauses_info_set_clauses_rep(ClausesRep, ItemNumbers,
            ClausesInfo0, ClausesInfo),
        pred_info_update_goal_type(np_goal_type_clause, !PredInfo),
        pred_info_set_clauses_info(ClausesInfo, !PredInfo),
        pred_info_get_markers(!.PredInfo, Markers0),
        add_marker(marker_calls_are_fully_qualified, Markers0, Markers),
        pred_info_set_markers(Markers, !PredInfo)
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred warn_about_any_unused_or_nonupdated_statevars(pred_info::in,
    list(string)::in, list(error_spec)::in, list(error_spec)::out) is det.

warn_about_any_unused_or_nonupdated_statevars(PredInfo, HeadVarNames,
        !Specs) :-
    pred_info_get_clauses_info(PredInfo, ClausesInfo),
    clauses_info_get_clauses_rep(ClausesInfo, ClausesRep, _ItemNumbers),
    get_clause_list_maybe_repeated(ClausesRep, Clauses),
    (
        Clauses = []
    ;
        Clauses = [HeadClause | TailClauses],
        map.init(InitAndFinalMap0),
        MaybeAllFacts0 = all_clauses_are_facts,
        build_init_and_final_map(is_first,
            HeadClause, HeadClauseInitArgs,
            InitAndFinalMap0, InitAndFinalMap1,
            MaybeAllFacts0, MaybeAllFacts1),
        list.map_foldl2(build_init_and_final_map(is_not_first),
            TailClauses, TailClausesInitArgs,
            InitAndFinalMap1, InitAndFinalMap,
            MaybeAllFacts1, MaybeAllFacts),
        AllClausesInitArgs =
            set.intersect_list([HeadClauseInitArgs | TailClausesInitArgs]),
        HeadClauseContext = HeadClause ^ clause_context,
        list.foldl(
            warn_about_any_unused_statevars_in_clause(PredInfo, HeadVarNames,
                AllClausesInitArgs, InitAndFinalMap),
            Clauses, !Specs),
        (
            MaybeAllFacts = all_clauses_are_facts
            % Programmers can easily *see* that the state vars
            % in InitAndFinalMap cannot be updated in any of the clauses
            % of PredInfo, since no clause has a body.
            %
            % This heuristic allows programmers to write clauses such as
            %
            %   p(_, 0, !IO). % XXX Implement this properly later.
            %
            % without being prodded by the algorithm to change this to
            %
            %   p(_, 0, IO, IO).
        ;
            MaybeAllFacts = some_clause_is_not_a_fact,
            map.foldl(
                warn_about_nonupdated_statevar(PredInfo, HeadClauseContext,
                    TailClauses),
                InitAndFinalMap, !Specs)
        )
    ).

%---------------------%

:- type init_and_final
    --->    init_and_final(
                iaf_final_arg_num   :: uint,
                iaf_names           :: set(string)
            ).

:- type init_and_final_map == map(uint, init_and_final).

:- type are_all_clauses_facts
    --->    some_clause_is_not_a_fact
    ;       all_clauses_are_facts.

%---------------------%

:- pred build_init_and_final_map(is_first::in, clause::in, set(uint)::out,
    init_and_final_map::in, init_and_final_map::out,
    are_all_clauses_facts::in, are_all_clauses_facts::out) is det.

build_init_and_final_map(IsFirst, Clause, ClauseInitArgs,
        !InitAndFinalMap, !MaybeAllFacts) :-
    UnusedSVarArgMap = Clause ^ clause_unused_svar_arg_map,
    map.to_sorted_assoc_list(UnusedSVarArgMap, UnusedSVarArgAL),
    collect_init_and_final_args(IsFirst, UnusedSVarArgAL,
        set.init, ClauseInitArgs, !InitAndFinalMap, set.init, KeepAliveSet),
    map.select(!.InitAndFinalMap, KeepAliveSet, !:InitAndFinalMap),
    MaybeFact = Clause ^ clause_maybe_fact,
    (
        MaybeFact = clause_is_a_fact
    ;
        MaybeFact = clause_is_not_a_fact,
        !:MaybeAllFacts = some_clause_is_not_a_fact
    ).

:- pred collect_init_and_final_args(is_first::in,
    assoc_list(uint, statevar_arg_desc)::in,
    set(uint)::in, set(uint)::out,
    init_and_final_map::in, init_and_final_map::out,
    set(uint)::in, set(uint)::out) is det.

collect_init_and_final_args(_IsFirst, [],
        !ClauseInitArgs, !InitAndFinalMap, !KeepAliveSet).
collect_init_and_final_args(IsFirst, [ArgDesc | ArgDescs],
        !ClauseInitArgs, !InitAndFinalMap, !KeepAliveSet) :-
    ArgDesc = ArgNum - statevar_arg_desc(InitOrFinal, SVarName),
    (
        InitOrFinal = init_arg_only,
        InitArgNum = ArgNum,
        set.insert(InitArgNum, !ClauseInitArgs)
    ;
        InitOrFinal = final_arg_only
    ;
        InitOrFinal = init_and_final_arg(FinalArgNum),
        InitArgNum = ArgNum,
        set.insert(InitArgNum, !ClauseInitArgs),
        (
            IsFirst = is_first,
            InitAndFinal = init_and_final(FinalArgNum,
                set.make_singleton_set(SVarName)),
            map.det_insert(InitArgNum, InitAndFinal, !InitAndFinalMap),
            set.insert(InitArgNum, !KeepAliveSet)
        ;
            IsFirst = is_not_first,
            ( if map.search(!.InitAndFinalMap, InitArgNum, InitAndFinal0) then
                InitAndFinal0 = init_and_final(FinalArgNum0, SVarNames0),
                ( if FinalArgNum = FinalArgNum0 then
                    set.insert(SVarName, SVarNames0, SVarNames),
                    InitAndFinal = init_and_final(FinalArgNum, SVarNames),
                    map.det_update(InitArgNum, InitAndFinal, !InitAndFinalMap),
                    set.insert(InitArgNum, !KeepAliveSet)
                else
                    map.delete(InitArgNum, !InitAndFinalMap)
                )
            else
                true
            )
        )
    ),
    collect_init_and_final_args(IsFirst, ArgDescs,
        !ClauseInitArgs, !InitAndFinalMap, !KeepAliveSet).

%---------------------%

:- pred warn_about_any_unused_statevars_in_clause(pred_info::in,
    list(string)::in, set(uint)::in, init_and_final_map::in, clause::in,
    list(error_spec)::in, list(error_spec)::out) is det.

warn_about_any_unused_statevars_in_clause(PredInfo, HeadVarNames,
        AllClausesInitArgs, InitAndFinalMap, Clause, !Specs) :-
    Clause = clause(_, _, _, ClauseContext, _, UnusedSVarArgMap, _),
    map.to_sorted_assoc_list(UnusedSVarArgMap, UnusedSVarArgAL),
    warn_about_any_unused_statevars(PredInfo, HeadVarNames, AllClausesInitArgs,
        InitAndFinalMap, ClauseContext, UnusedSVarArgAL, !Specs).

:- pred warn_about_any_unused_statevars(pred_info::in,
    list(string)::in, set(uint)::in, init_and_final_map::in, prog_context::in,
    assoc_list(uint, statevar_arg_desc)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

warn_about_any_unused_statevars(_, _, _, _, _, [], !Specs).
warn_about_any_unused_statevars(PredInfo, HeadVars, AllClausesInitArgs,
        InitAndFinalMap, ClauseContext, [ArgDesc | ArgDescs], !Specs) :-
    ArgDesc = ArgNum - statevar_arg_desc(InitOrFinal, SVarName),
    (
        (
            InitOrFinal = init_arg_only,  Prefix = "!.",
% XXX This probably can be deleted, along with the AllClausesInitArgs
% and InitAndFinalMap arguments, but until I (zs) am sure of that, they stay.
%           (
%               set.contains(AllClausesInitArgs, ArgNum)
%           ;
%               map.search(InitAndFinalMap, ArgNum, InitAndFinal),
%               InitAndFinal = init_and_final(_, InitAndFinalSVarNames),
%               set.contains(InitAndFinalSVarNames, SVarName)
%           )
            list.det_index1(HeadVars, uint.cast_to_int(ArgNum), ArgName),
            ( if initial_state_var_name(SVarName) = ArgName then
                Warn = yes
            else
                Warn = no
            )
        ;
            InitOrFinal = final_arg_only, Prefix = "!:",
            Warn = yes
        ),
        (
            Warn = no
            % In this clause, !.SVarName could be replaced with non-statevar
            % variable, but this would make the argument's name inconsistent
            % between clauses. This in turn would lead headvar_names.m to keep
            % the name of the argument as HeadVar__N. This is a reasonable
            % stylistic argument in favor of keeping !.SVarName, so do not
            % generate a warning in this case.
        ;
            Warn = yes,
            PredNameColonPieces = describe_one_pred_info_name(no,
                should_not_module_qualify, [suffix(":")], PredInfo),
            % Please keep this wording in sync with the code of the
            % report_unused_svar_in_lambda predicate in state_var.m.
            Pieces = [words("In")] ++ PredNameColonPieces ++ [nl,
                words("warning: state variable")] ++
                color_as_subject([quote(Prefix ++ SVarName)]) ++
                [words("is")] ++
                color_as_incorrect([words("never updated")]) ++
                [words("in this clause, so it should be"),
                words("replaced with an ordinary variable."), nl],
            Spec = conditional_spec($pred,
                warn_unneeded_initial_statevars, yes,
                severity_warning, phase_pt2h, [msg(ClauseContext, Pieces)]),
            !:Specs = [Spec | !.Specs]
        )
    ;
        InitOrFinal = init_and_final_arg(_FinalArgNum)
    ),
    warn_about_any_unused_statevars(PredInfo, HeadVars, AllClausesInitArgs,
        InitAndFinalMap, ClauseContext, ArgDescs, !Specs).

%---------------------%

:- pred warn_about_nonupdated_statevar(pred_info::in, prog_context::in,
    list(clause)::in, uint::in, init_and_final::in,
    list(error_spec)::in, list(error_spec)::out) is det.

warn_about_nonupdated_statevar(PredInfo, HeadClauseContext, TailClauses,
        _InitArgNum, InitAndFinal, !Specs) :-
    PredNameColonPieces = describe_one_pred_info_name(no,
        should_not_module_qualify, [suffix(":")], PredInfo),
    InitAndFinal = init_and_final(FinalArgNum, SVarNameSet),
    set.to_sorted_list(SVarNameSet, SVarNames),
    BangColonSVarNames = list.map((func(N) = "!:" ++ N), SVarNames),
    (
        BangColonSVarNames = [],
        unexpected($pred, "BangColonSVarNames = []")
    ;
        BangColonSVarNames = [BangColonSVarName],
        BangColonSVarNameCommaPieces =
            [words("represented by the state variable")] ++
            color_as_subject([quote(BangColonSVarName), suffix(",")])
    ;
        BangColonSVarNames = [_, _ | _],
        % Since we get one SVarName for InitArgNum per clause,
        % we cannot get here without TailClauses being nonempty.
        BangColonSVarNameCommaPieces =
            [words("represented by one of the state variables")] ++
            quote_list_to_color_pieces(color_subject, "and", [],
                BangColonSVarNames) ++
            [words("in each clause,")]
    ),
    ( TailClauses = [],      InEachClausePieces = []
    ; TailClauses = [_ | _], InEachClausePieces = [words("in each clause")]
    ),
    pred_info_get_clauses_info(PredInfo, ClausesInfo),
    clauses_info_get_arg_vector(ClausesInfo, ArgVector),
    UserArgs = proc_arg_vector_get_user_args(ArgVector),
    list.length(UserArgs, NumUserArgs),
    ( if uint.cast_to_int(FinalArgNum) > NumUserArgs then
        expect(unify(uint.cast_to_int(FinalArgNum + 1u), NumUserArgs),
            $pred, "FinalArgNum is not numbered correctly for return value"),
        expect(unify(pred_info_is_pred_or_func(PredInfo), pf_function),
            $pred, "PredInfo is not a function"),
        FinalArgCommaPieces =
            [words("the function return value,")]
    else
        FinalArgCommaPieces =
            [words("the"), unth_fixed(FinalArgNum), words("argument,")]
    ),
    % Please keep this wording in sync with the code of the
    % report_unused_svar_in_lambda predicate in state_var.m.
    Pieces = [words("In")] ++ PredNameColonPieces ++ [nl,
        words("warning:")] ++ FinalArgCommaPieces ++
        BangColonSVarNameCommaPieces ++
        color_as_incorrect([words("could be deleted,")]) ++
        [words("because its value")] ++ InEachClausePieces ++
        [words("is always the same as its initial value."), nl],
    Spec = conditional_spec($pred, warn_unneeded_final_statevars, yes,
        severity_warning, phase_pt2h, [msg(HeadClauseContext, Pieces)]),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%
:- end_module check_hlds.pre_typecheck.
%---------------------------------------------------------------------------%
