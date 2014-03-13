%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: intermod.m.
% Main author: stayl.
%
% This module writes out the interface for inter-module optimization.
% The .opt file includes:
%   - The clauses for exported preds that can be inlined.
%   - The clauses for exported preds that have higher-order pred arguments.
%   - The pred/mode declarations for local predicates that the
%     above clauses use.
%   - Non-exported types, insts and modes used by the above
%   - Pragma reserve_tag, foreign_enum, or foreign_type declarations for
%     any types output due to the line above
%   - :- import_module declarations to import stuff used by the above.
%   - pragma declarations for the exported preds.
%   - pragma foreign_header declarations if any pragma_foreign_code
%     preds are written.
% All these items should be module qualified.
%
% This module also contains predicates to read in the .opt files and
% to adjust the import status of local predicates which are exported for
% intermodule optimization.
%
% Note that predicates which call predicates that do not have mode or
% determinism declarations do not have clauses exported, since this would
% require running mode analysis and determinism analysis before writing the
% .opt file, significantly increasing compile time for a very small gain.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.intermod.
:- interface.

:- import_module hlds.hlds_module.
:- import_module libs.globals.
:- import_module parse_tree.error_util.
:- import_module parse_tree.module_imports.
:- import_module parse_tree.prog_io.

:- import_module bool.
:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

    % Open the file "<module-name>.opt.tmp", and write out the declarations
    % and clauses for intermodule optimization. Note that update_interface
    % and touch_interface_datestamp are called from mercury_compile.m since
    % they must be called after unused_args.m appends its information
    % to the .opt.tmp file.
    %
:- pred write_opt_file(module_info::in, module_info::out, io::di, io::uo)
    is det.

    % Add the items from the .opt files of imported modules to
    % the items for this module.
    %
:- pred grab_opt_files(globals::in,
    module_and_imports::in, module_and_imports::out, bool::out,
    io::di, io::uo) is det.

    % Make sure that local preds which have been exported in the .opt
    % file get an exported(_) label.
    %
:- pred adjust_pred_import_status(module_info::in, module_info::out) is det.

:- type opt_file_type
    --->    opt_file
    ;       trans_opt_file.

    % update_error_status(Globals, OptFileType, FileName,
    %   ModuleSpecs, !Specs, ModuleError, !Error):
    %
    % Work out whether any fatal errors have occurred while reading
    % `.opt' files, updating !Error if there were fatal errors.
    %
    % A missing `.opt' file is only a fatal error if
    % `--warn-missing-opt-files --halt-at-warn' was passed the compiler.
    %
    % Syntax errors in `.opt' files are always fatal.
    %
    % This is also used by trans_opt.m for reading `.trans_opt' files.
    %
:- pred update_error_status(globals::in, opt_file_type::in, string::in,
    list(error_spec)::in, list(error_spec)::in, list(error_spec)::out,
    module_error::in, bool::in, bool::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.foreign.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_module.
:- import_module hlds.hlds_out.hlds_out_pred.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.hlds_pred.
:- import_module hlds.pred_table.
:- import_module hlds.special_pred.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.file_names.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.modules.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_io.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.
:- import_module transform_hlds.inlining.

:- import_module assoc_list.
:- import_module cord.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module multi_map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module solutions.
:- import_module string.
:- import_module term.
:- import_module term_io.
:- import_module varset.

%-----------------------------------------------------------------------------%

write_opt_file(!ModuleInfo, !IO) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    module_info_get_name(!.ModuleInfo, ModuleName),
    module_name_to_file_name(Globals, ModuleName, ".opt.tmp", do_create_dirs,
        TmpName, !IO),
    io.open_output(TmpName, Result, !IO),
    (
        Result = error(Err),
        Msg = io.error_message(Err),
        io.write_string(Msg, !IO),
        io.set_exit_status(1, !IO)
    ;
        Result = ok(FileStream),
        io.set_output_stream(FileStream, OutputStream, !IO),
        module_info_get_valid_predids(RealPredIds, !ModuleInfo),
        module_info_get_assertion_table(!.ModuleInfo, AssertionTable),
        assertion_table_pred_ids(AssertionTable, AssertPredIds),
        PredIds = AssertPredIds ++ RealPredIds,
        globals.lookup_int_option(Globals, intermod_inline_simple_threshold,
            Threshold),
        globals.lookup_bool_option(Globals, deforestation, Deforestation),
        globals.lookup_int_option(Globals, higher_order_size_limit,
            HigherOrderSizeLimit),
        some [!IntermodInfo] (
            init_intermod_info(!.ModuleInfo, !:IntermodInfo),
            gather_preds(PredIds, yes, Threshold, HigherOrderSizeLimit,
                Deforestation, !IntermodInfo),
            gather_instances(!IntermodInfo),
            gather_types(!IntermodInfo),
            write_intermod_info(!.IntermodInfo, !IO),
            intermod_info_get_module_info(!.IntermodInfo, !:ModuleInfo),
            io.set_output_stream(OutputStream, _, !IO),
            io.close_output(FileStream, !IO),
            do_adjust_pred_import_status(!.IntermodInfo, !ModuleInfo)
        )
    ).

%-----------------------------------------------------------------------------%
%
% Predicates to gather items to output to .opt file
%

:- pred gather_preds(list(pred_id)::in, bool::in, int::in, int::in, bool::in,
    intermod_info::in, intermod_info::out) is det.

gather_preds(AllPredIds, CollectTypes, InlineThreshold, HigherOrderSizeLimit,
        Deforestation, !Info) :-
    % First gather exported preds.
    ProcessLocalPreds = no,
    gather_pred_list(AllPredIds, ProcessLocalPreds, CollectTypes,
        InlineThreshold, HigherOrderSizeLimit, Deforestation, !Info),

    % Then gather preds used by exported preds (recursively).
    set.init(ExtraExportedPreds0),
    gather_preds_fixpoint(ExtraExportedPreds0, CollectTypes, InlineThreshold,
        HigherOrderSizeLimit, Deforestation, !Info).

:- pred gather_preds_fixpoint(set(pred_id)::in, bool::in, int::in, int::in,
    bool::in, intermod_info::in, intermod_info::out) is det.

gather_preds_fixpoint(ExtraExportedPreds0, CollectTypes, InlineThreshold,
        HigherOrderSizeLimit, Deforestation, !Info) :-
    intermod_info_get_pred_decls(!.Info, ExtraExportedPreds),
    NewlyExportedPreds = set.to_sorted_list(
        ExtraExportedPreds `set.difference` ExtraExportedPreds0),
    (
        NewlyExportedPreds = []
    ;
        NewlyExportedPreds = [_ | _],
        ProcessLocalPreds = yes,
        gather_pred_list(NewlyExportedPreds, ProcessLocalPreds, CollectTypes,
            InlineThreshold, HigherOrderSizeLimit, Deforestation, !Info),
        gather_preds_fixpoint(ExtraExportedPreds, CollectTypes,
            InlineThreshold, HigherOrderSizeLimit, Deforestation, !Info)
    ).

:- pred gather_pred_list(list(pred_id)::in, bool::in, bool::in,
    int::in, int::in, bool::in, intermod_info::in, intermod_info::out) is det.

gather_pred_list([], _, _, _, _, _, !Info).
gather_pred_list([PredId | PredIds], ProcessLocalPreds, CollectTypes,
        InlineThreshold, HigherOrderSizeLimit, Deforestation, !Info) :-
    intermod_info_get_module_info(!.Info, ModuleInfo0),
    module_info_get_preds(ModuleInfo0, PredTable0),
    map.lookup(PredTable0, PredId, PredInfo0),
    module_info_get_type_spec_info(ModuleInfo0, TypeSpecInfo),
    TypeSpecInfo = type_spec_info(_, TypeSpecForcePreds, _, _),
    pred_info_get_clauses_info(PredInfo0, ClausesInfo0),
    (
        clauses_info_get_explicit_vartypes(ClausesInfo0, ExplicitVarTypes),
        vartypes_is_empty(ExplicitVarTypes),
        should_be_processed(ProcessLocalPreds, PredId, PredInfo0,
            TypeSpecForcePreds, InlineThreshold, HigherOrderSizeLimit,
            Deforestation, ModuleInfo0)
    ->
        SavedInfo = !.Info,
        % Write a declaration to the `.opt' file for
        % `exported_to_submodules' predicates.
        add_proc(PredId, DoWrite0, !Info),
        clauses_info_get_clauses_rep(ClausesInfo0, ClausesRep0, ItemNumbers0),
        (
            DoWrite0 = yes,
            clauses_info_get_vartypes(ClausesInfo0, VarTypes),
            pred_info_get_typevarset(PredInfo0, TVarSet),
            intermod_info_set_var_types(VarTypes, !Info),
            intermod_info_set_tvarset(TVarSet, !Info),
            get_clause_list(ClausesRep0, Clauses0),
            intermod_traverse_clauses(Clauses0, Clauses, DoWrite, !Info),
            set_clause_list(Clauses, ClausesRep)
        ;
            DoWrite0 = no,
            ClausesRep = ClausesRep0,
            DoWrite = no
        ),
        (
            DoWrite = yes,
            clauses_info_set_clauses_rep(ClausesRep, ItemNumbers0,
                ClausesInfo0, ClausesInfo),
            pred_info_set_clauses_info(ClausesInfo, PredInfo0, PredInfo),
            map.det_update(PredId, PredInfo, PredTable0, PredTable),
            module_info_set_preds(PredTable, ModuleInfo0, ModuleInfo),
            intermod_info_get_preds(!.Info, Preds0),
            ( pred_info_pragma_goal_type(PredInfo) ->
                % pragma foreign_decls must be written since their contents
                % could be used by pragma foreign_procs.
                intermod_info_set_write_header(!Info)
            ;
                true
            ),
            set.insert(PredId, Preds0, Preds),
            intermod_info_set_preds(Preds, !Info),
            intermod_info_set_module_info(ModuleInfo, !Info)
        ;
            DoWrite = no,
            % Remove any items added for the clauses for this predicate.
            !:Info = SavedInfo
        )
    ;
        true
    ),
    gather_pred_list(PredIds, ProcessLocalPreds, CollectTypes,
        InlineThreshold, HigherOrderSizeLimit, Deforestation, !Info).

:- pred should_be_processed(bool::in, pred_id::in, pred_info::in,
    set(pred_id)::in, int::in, int::in, bool::in,
    module_info::in) is semidet.

should_be_processed(ProcessLocalPreds, PredId, PredInfo, TypeSpecForcePreds,
        InlineThreshold, HigherOrderSizeLimit, Deforestation, ModuleInfo) :-
    (
        ProcessLocalPreds = no,
        ( pred_info_is_exported(PredInfo)
        ; pred_info_is_exported_to_submodules(PredInfo)
        )
    ;
        ProcessLocalPreds = yes,
        pred_info_get_import_status(PredInfo, status_local)
    ),
    (
        pred_info_get_clauses_info(PredInfo, ClauseInfo),
        clauses_info_get_clauses_rep(ClauseInfo, ClausesRep, _ItemNumbers),
        get_clause_list(ClausesRep, Clauses),

        [ProcId | _ProcIds] = pred_info_procids(PredInfo),
        pred_info_get_procedures(PredInfo, Procs),
        map.lookup(Procs, ProcId, ProcInfo),

        % At this point, the goal size includes some dummy unifications
        % HeadVar1 = X, HeadVar2 = Y, etc. which will be optimized away
        % later. To counter for this, we add the arity to the size thresholds.
        Arity = pred_info_orig_arity(PredInfo),

        % Predicates with `class_method' markers contain class_method_call
        % goals which cannot be written to `.opt' files (they cannot be read
        % back in). They will be recreated in the importing module.
        pred_info_get_markers(PredInfo, Markers),
        \+ check_marker(Markers, marker_class_method),
        \+ check_marker(Markers, marker_class_instance_method),

        % Don't write stub clauses to `.opt' files.
        \+ check_marker(Markers, marker_stub),

        % Don't export builtins since they will be recreated in the
        % importing module anyway.
        \+ is_unify_or_compare_pred(PredInfo),
        \+ pred_info_is_builtin(PredInfo),

        % These will be recreated in the importing module.
        \+ set.member(PredId, TypeSpecForcePreds),

        % Don't export non-inlinable predicates.
        \+ check_marker(Markers, marker_user_marked_no_inline),

        % No point exporting code which isn't very inlinable.
        module_info_get_globals(ModuleInfo, Globals),
        globals.get_target(Globals, Target),
        \+ clauses_contain_noninlinable_foreign_code(Target, Clauses),

        % Don't export tabled predicates since they are not inlinable.
        proc_info_get_eval_method(ProcInfo, eval_normal),

        (
            inlining.is_simple_clause_list(Clauses, InlineThreshold + Arity)
        ;
            pred_info_requested_inlining(PredInfo)
        ;
            % Mutable access preds should always be included in .opt files.
            %
            check_marker(Markers, marker_mutable_access_pred)
        ;
            has_ho_input(ModuleInfo, ProcInfo),
            clause_list_size(Clauses, GoalSize),
            GoalSize =< HigherOrderSizeLimit + Arity
        ;
            Deforestation = yes,
            % Double the inline-threshold since goals we want to deforest
            % will have at least two disjuncts. This allows one simple goal
            % in each disjunct. The disjunction adds one to the goal size,
            % hence the `+1'.
            DeforestThreshold = InlineThreshold * 2 + 1,
            inlining.is_simple_clause_list(Clauses, DeforestThreshold + Arity),
            clause_list_is_deforestable(PredId, Clauses)
        )
    ;
        % Allow promises to be written.
        pred_info_is_promise(PredInfo, _)
    ).

    % If the clauses contains foreign code which requires an external
    % definition, there is not much point in exporting it.
    %
:- pred clauses_contain_noninlinable_foreign_code(compilation_target::in,
    list(clause)::in) is semidet.

clauses_contain_noninlinable_foreign_code(Target, [C | _Cs]) :-
    Target = target_il,
    Lang = C ^ clause_lang,
    Lang = impl_lang_foreign(ForeignLang),
    ForeignLang = lang_csharp.
clauses_contain_noninlinable_foreign_code(Target, [_ | Cs]) :-
    clauses_contain_noninlinable_foreign_code(Target, Cs).

:- pred intermod_traverse_clauses(list(clause)::in, list(clause)::out,
    bool::out, intermod_info::in, intermod_info::out) is det.

intermod_traverse_clauses([], [], yes, !Info).
intermod_traverse_clauses([Clause0 | Clauses0], [Clause | Clauses],
        DoWrite, !Info) :-
    Goal0 = Clause0 ^ clause_body,
    intermod_traverse_goal(Goal0, Goal, DoWrite1, !Info),
    Clause = Clause0 ^ clause_body := Goal,
    (
        DoWrite1 = yes,
        intermod_traverse_clauses(Clauses0, Clauses, DoWrite, !Info)
    ;
        DoWrite1 = no,
        Clauses = Clauses0,
        DoWrite = no
    ).

:- pred has_ho_input(module_info::in, proc_info::in) is semidet.

has_ho_input(ModuleInfo, ProcInfo) :-
    proc_info_get_headvars(ProcInfo, HeadVars),
    proc_info_get_argmodes(ProcInfo, ArgModes),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    check_for_ho_input_args(ModuleInfo, VarTypes, HeadVars, ArgModes).

:- pred check_for_ho_input_args(module_info::in, vartypes::in,
    list(prog_var)::in, list(mer_mode)::in) is semidet.

check_for_ho_input_args(ModuleInfo, VarTypes,
        [HeadVar | HeadVars], [ArgMode | ArgModes]) :-
    (
        mode_is_input(ModuleInfo, ArgMode),
        lookup_var_type(VarTypes, HeadVar, Type),
        classify_type(ModuleInfo, Type) = ctor_cat_higher_order
    ;
        check_for_ho_input_args(ModuleInfo, VarTypes, HeadVars, ArgModes)
    ).

    % Rough guess: a goal is deforestable if it contains a single
    % top-level branched goal and is recursive.
    %
:- pred clause_list_is_deforestable(pred_id::in, list(clause)::in) is semidet.

clause_list_is_deforestable(PredId, Clauses)  :-
    some [Clause1] (
        list.member(Clause1, Clauses),
        Goal1 = Clause1 ^ clause_body,
        goal_calls_pred_id(Goal1, PredId)
    ),
    (
        Clauses = [_, _ | _]
    ;
        Clauses = [Clause2],
        Goal2 = Clause2 ^ clause_body,
        goal_to_conj_list(Goal2, GoalList),
        goal_contains_one_branched_goal(GoalList)
    ).

:- pred goal_contains_one_branched_goal(list(hlds_goal)::in) is semidet.

goal_contains_one_branched_goal(GoalList) :-
    goal_contains_one_branched_goal(GoalList, no).

:- pred goal_contains_one_branched_goal(list(hlds_goal)::in, bool::in)
    is semidet.

goal_contains_one_branched_goal([], yes).
goal_contains_one_branched_goal([Goal | Goals], FoundBranch0) :-
    Goal = hlds_goal(GoalExpr, _),
    (
        goal_is_branched(GoalExpr),
        FoundBranch0 = no,
        FoundBranch = yes
    ;
        goal_expr_has_subgoals(GoalExpr) = does_not_have_subgoals,
        FoundBranch = FoundBranch0
    ),
    goal_contains_one_branched_goal(Goals, FoundBranch).

    % Go over the goal of an exported proc looking for proc decls, types,
    % insts and modes that we need to write to the optfile.
    %
:- pred intermod_traverse_goal(hlds_goal::in, hlds_goal::out, bool::out,
    intermod_info::in, intermod_info::out) is det.

intermod_traverse_goal(Goal0, Goal, DoWrite, !Info) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo),
    intermod_traverse_goal_expr(GoalExpr0, GoalExpr, DoWrite, !Info),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred intermod_traverse_goal_expr(hlds_goal_expr::in, hlds_goal_expr::out,
    bool::out, intermod_info::in, intermod_info::out) is det.

intermod_traverse_goal_expr(GoalExpr0, GoalExpr, DoWrite, !Info) :-
    (
        GoalExpr0 = unify(LVar, RHS0, Mode, Kind, UnifyContext),
        % Export declarations for preds used in higher order pred constants
        % or function calls.
        module_qualify_unify_rhs(RHS0, RHS, DoWrite, !Info),
        GoalExpr = unify(LVar, RHS, Mode, Kind, UnifyContext)
    ;
        GoalExpr0 = plain_call(PredId, _, _, _, _, _),
        % Ensure that the called predicate will be exported.
        add_proc(PredId, DoWrite, !Info),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = generic_call(CallType, _, _, _, _),
        GoalExpr = GoalExpr0,
        (
            CallType = higher_order(_, _, _, _),
            DoWrite = yes
        ;
            ( CallType = class_method(_, _, _, _)
            ; CallType = event_call(_)
            ; CallType = cast(_)
            ),
            DoWrite = no
        )
    ;
        GoalExpr0 = call_foreign_proc(Attrs, _, _, _, _, _, _),
        GoalExpr = GoalExpr0,
        % Inlineable exported pragma_foreign_code goals cannot use any
        % non-exported types, so we just write out the clauses.
        MaybeMayDuplicate = get_may_duplicate(Attrs),
        (
            MaybeMayDuplicate = yes(MayDuplicate),
            (
                MayDuplicate = proc_may_duplicate,
                DoWrite = yes
            ;
                MayDuplicate = proc_may_not_duplicate,
                DoWrite = no
            )
        ;
            MaybeMayDuplicate = no,
            DoWrite = yes
        )
    ;
        GoalExpr0 = conj(ConjType, Goals0),
        intermod_traverse_list_of_goals(Goals0, Goals, DoWrite, !Info),
        GoalExpr = conj(ConjType, Goals)
    ;
        GoalExpr0 = disj(Goals0),
        intermod_traverse_list_of_goals(Goals0, Goals, DoWrite, !Info),
        GoalExpr = disj(Goals)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        intermod_traverse_cases(Cases0, Cases, DoWrite, !Info),
        GoalExpr = switch(Var, CanFail, Cases)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        intermod_traverse_goal(Cond0, Cond, DoWrite1, !Info),
        intermod_traverse_goal(Then0, Then, DoWrite2, !Info),
        intermod_traverse_goal(Else0, Else, DoWrite3, !Info),
        bool.and_list([DoWrite1, DoWrite2, DoWrite3], DoWrite),
        GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ;
        GoalExpr0 = negation(SubGoal0),
        intermod_traverse_goal(SubGoal0, SubGoal, DoWrite, !Info),
        GoalExpr = negation(SubGoal)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        % Mode analysis hasn't been run yet, so we don't know yet whether
        % from_ground_term_construct scopes actually satisfy their invariants,
        % specifically the invariant that say they contain no calls or
        % higher-order constants. We therefore cannot special-case them here.
        intermod_traverse_goal(SubGoal0, SubGoal, DoWrite, !Info),
        GoalExpr = scope(Reason, SubGoal)
    ;
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal0, OrElseGoals0, OrElseInners),
            intermod_traverse_goal(MainGoal0, MainGoal, DoWrite1, !Info),
            intermod_traverse_list_of_goals(OrElseGoals0, OrElseGoals,
                DoWrite2, !Info),
            bool.and(DoWrite1, DoWrite2, DoWrite),
            ShortHand = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal, OrElseGoals, OrElseInners)
        ;
            ShortHand0 = try_goal(MaybeIO, ResultVar, SubGoal0),
            intermod_traverse_goal(SubGoal0, SubGoal, DoWrite, !Info),
            ShortHand = try_goal(MaybeIO, ResultVar, SubGoal)
        ;
            ShortHand0 = bi_implication(_, _),
            % These should have been expanded out by now.
            unexpected($module, $pred, "bi_implication")
        ),
        GoalExpr = shorthand(ShortHand)
    ).

:- pred intermod_traverse_list_of_goals(hlds_goals::in, hlds_goals::out,
    bool::out, intermod_info::in, intermod_info::out) is det.

intermod_traverse_list_of_goals([], [], yes, !Info).
intermod_traverse_list_of_goals([Goal0 | Goals0], [Goal | Goals], !:DoWrite,
        !Info) :-
    intermod_traverse_goal(Goal0, Goal, !:DoWrite, !Info),
    (
        !.DoWrite = yes,
        intermod_traverse_list_of_goals(Goals0, Goals, !:DoWrite, !Info)
    ;
        !.DoWrite = no,
        Goals = Goals0
    ).

:- pred intermod_traverse_cases(list(case)::in, list(case)::out, bool::out,
    intermod_info::in, intermod_info::out) is det.

intermod_traverse_cases([], [], yes, !Info).
intermod_traverse_cases([Case0 | Cases0], [Case | Cases], !:DoWrite, !Info) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    intermod_traverse_goal(Goal0, Goal, !:DoWrite, !Info),
    Case = case(MainConsId, OtherConsIds, Goal),
    (
        !.DoWrite = yes,
        intermod_traverse_cases(Cases0, Cases, !:DoWrite, !Info)
    ;
        !.DoWrite = no,
        Cases = Cases0
    ).

    % add_proc/4 tries to do what ever is necessary to ensure that the
    % specified predicate will be exported, so that it can be called from
    % clauses in the `.opt' file. If it can't, then it returns DoWrite = no,
    % which will prevent the caller from being included in the `.opt' file.
    %
    % If a proc called within an exported proc is local, we need to add
    % a declaration for the called proc to the .opt file. If a proc called
    % within an exported proc is from a different module, we need to include
    % an `:- import_module' declaration to import that module in the `.opt'
    % file.
    %
:- pred add_proc(pred_id::in, bool::out,
    intermod_info::in, intermod_info::out) is det.

add_proc(PredId, DoWrite, !Info) :-
    ( PredId = invalid_pred_id ->
        % This will happen for type class instance methods defined using
        % the clause syntax. Currently we cannot handle intermodule
        % optimization of those.
        DoWrite = no
    ;
        add_proc_2(PredId, DoWrite, !Info)
    ).

:- pred add_proc_2(pred_id::in, bool::out,
    intermod_info::in, intermod_info::out) is det.

add_proc_2(PredId, DoWrite, !Info) :-
    intermod_info_get_module_info(!.Info, ModuleInfo),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_import_status(PredInfo, Status),
    ProcIds = pred_info_procids(PredInfo),
    pred_info_get_markers(PredInfo, Markers),
    (
        % Calling compiler-generated procedures is fine; we don't need
        % to output declarations for them to the `.opt' file, since they
        % will be recreated every time anyway. We don't want declarations
        % for predicates representing promises either.

        ( is_unify_or_compare_pred(PredInfo)
        ; pred_info_is_promise(PredInfo, _)
        )
    ->
        DoWrite = yes
    ;
        % Don't write the caller to the `.opt' file if it calls a pred
        % without mode or determinism decls, because we'd need to include
        % the mode decls for the callee in the `.opt' file and (since
        % writing the `.opt' file happens before mode inference) we can't
        % do that because we don't know what the modes are.
        %
        % XXX This prevents intermodule optimizations in such cases,
        % which is a pity.

        (
            check_marker(Markers, marker_infer_modes)
        ;
            pred_info_get_procedures(PredInfo, Procs),
            list.member(ProcId, ProcIds),
            map.lookup(Procs, ProcId, ProcInfo),
            proc_info_get_declared_determinism(ProcInfo, no)
        )
    ->
        DoWrite = no
    ;
        % Goals which call impure predicates cannot be written due to
        % limitations in mode analysis. The problem is that only head
        % unifications are allowed to be reordered with impure goals.
        % For example,
        %
        %   p(A::in, B::in, C::out) :- impure foo(A, B, C).
        %
        % becomes
        %
        %   p(HeadVar1, HeadVar2, HeadVar3) :-
        %       A = HeadVar1, B = HeadVar2, C = HeadVar3,
        %       impure foo(A, B, C).
        %
        % In the clauses written to `.opt' files, the head unifications
        % are already expanded, and are expanded again when the `.opt' file
        % is read in. The `C = HeadVar3' unification cannot be reordered
        % with the impure goal, resulting in a mode error. Fixing this
        % in mode analysis would be tricky.
        % See tests/valid/impure_intermod.m.
        %
        % NOTE: the above restriction applies to user predicates.
        % For compiler generated mutable access predicates, we can ensure
        % that reordering is not necessary by construction, so it is safe
        % to include them in .opt files.

        pred_info_get_purity(PredInfo, purity_impure),
        not check_marker(Markers, marker_mutable_access_pred)
    ->
        DoWrite = no
    ;
        % If a pred whose code we are going to put in the .opt file calls
        % a predicate which is exported, then we do not need to do anything
        % special.

        (
            Status = status_exported
        ;
            Status = status_external(ExternalStatus),
            status_is_exported(ExternalStatus) = yes
        )
    ->
        DoWrite = yes
    ;
        % Declarations for class methods will be recreated from the class
        % declaration in the `.opt' file. Declarations for local classes
        % are always written to the `.opt' file.

        pred_info_get_markers(PredInfo, Markers),
        check_marker(Markers, marker_class_method)
    ->
        DoWrite = yes
    ;
        % If a pred whose code we are going to put in the `.opt' file calls
        % a predicate which is local to that module, then we need to put
        % the declaration for the called predicate in the `.opt' file.

        import_status_to_write(Status)
    ->
        DoWrite = yes,
        intermod_info_get_pred_decls(!.Info, PredDecls0),
        set.insert(PredId, PredDecls0, PredDecls),
        intermod_info_set_pred_decls(PredDecls, !Info)
    ;
        ( Status = status_imported(_)
        ; Status = status_opt_imported
        )
    ->
        % Imported pred - add import for module.

        DoWrite = yes,
        PredModule = pred_info_module(PredInfo),
        intermod_info_get_modules(!.Info, Modules0),
        set.insert(PredModule, Modules0, Modules),
        intermod_info_set_modules(Modules, !Info)
    ;
        unexpected($module, $pred, "unexpected status")
    ).

    % Resolve overloading and module qualify everything in a unify_rhs.
    % Fully module-qualify the right-hand-side of a unification.
    % For function calls and higher-order terms, call add_proc
    % so that the predicate or function will be exported if necessary.
    %
:- pred module_qualify_unify_rhs(unify_rhs::in, unify_rhs::out, bool::out,
    intermod_info::in, intermod_info::out) is det.

module_qualify_unify_rhs(RHS0, RHS, DoWrite, !Info) :-
    (
        RHS0 = rhs_var(_),
        RHS = RHS0,
        DoWrite = yes
    ;
        RHS0 = rhs_lambda_goal(Purity, HOGroundness, PorF, EvalMethod,
            NonLocals, QuantVars, Modes, Detism, Goal0),
        intermod_traverse_goal(Goal0, Goal, DoWrite, !Info),
        RHS = rhs_lambda_goal(Purity, HOGroundness, PorF, EvalMethod,
            NonLocals, QuantVars, Modes, Detism, Goal)
    ;
        RHS0 = rhs_functor(Functor, _Exist, _Vars),
        RHS = RHS0,
        % Is this a higher-order predicate or higher-order function term?
        ( Functor = closure_cons(ShroudedPredProcId, _) ->
            % Yes, the unification creates a higher-order term.
            % Make sure that the predicate/function is exported.

            proc(PredId, _) = unshroud_pred_proc_id(ShroudedPredProcId),
            add_proc(PredId, DoWrite, !Info)
        ;
            % It is an ordinary constructor, or a constant of a builtin type,
            % so just leave it alone.
            %
            % Constructors are module qualified by post_typecheck.m.
            %
            % Function calls and higher-order function applications
            % are transformed into ordinary calls and higher-order calls
            % by post_typecheck.m, so they cannot occur here.

            DoWrite = yes
        )
    ).

%-----------------------------------------------------------------------------%

:- pred gather_instances(intermod_info::in, intermod_info::out) is det.

gather_instances(!Info) :-
    intermod_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_instance_table(ModuleInfo, Instances),
    map.foldl(gather_instances_2(ModuleInfo), Instances, !Info).

:- pred gather_instances_2(module_info::in, class_id::in,
    list(hlds_instance_defn)::in,
    intermod_info::in, intermod_info::out) is det.

gather_instances_2(ModuleInfo, ClassId, InstanceDefns, !Info) :-
    list.foldl(gather_instances_3(ModuleInfo, ClassId), InstanceDefns, !Info).

:- pred gather_instances_3(module_info::in, class_id::in,
    hlds_instance_defn::in, intermod_info::in, intermod_info::out) is det.

gather_instances_3(ModuleInfo, ClassId, InstanceDefn, !Info) :-
    InstanceDefn = hlds_instance_defn(ModuleName, Status, Context,
        InstanceConstraints, Types, OriginalTypes, Interface0,
        MaybePredProcIds, TVarSet, Proofs),
    DefinedThisModule = status_defined_in_this_module(Status),
    (
        DefinedThisModule = yes,

        % The bodies are always stripped from instance declarations
        % before writing them to `int' files, so the full instance
        % declaration should be written even for exported instances.

        SaveInfo = !.Info,
        (
            Interface0 = instance_body_concrete(Methods0),
            (
                MaybePredProcIds = yes(ClassProcs),
                GetPredId =
                    (pred(Proc::in, PredId::out) is det :-
                        Proc = hlds_class_proc(PredId, _)
                    ),
                list.map(GetPredId, ClassProcs, ClassPreds0),

                % The interface is sorted on pred_id.
                list.remove_adjacent_dups(ClassPreds0, ClassPreds),
                assoc_list.from_corresponding_lists(ClassPreds, Methods0,
                    MethodAL)
            ;
                MaybePredProcIds = no,
                unexpected($module, $pred,
                    "method pred_proc_ids not filled in")
            ),
            list.map_foldl(qualify_instance_method(ModuleInfo),
                MethodAL, Methods, [], PredIds),
            list.map_foldl(add_proc, PredIds, DoWriteMethodsList, !Info),
            bool.and_list(DoWriteMethodsList, DoWriteMethods),
            (
                DoWriteMethods = yes,
                Interface = instance_body_concrete(Methods)
            ;
                DoWriteMethods = no,

                % Write an abstract instance declaration if any of the methods
                % cannot be written to the `.opt' file for any reason.
                Interface = instance_body_abstract,

                % Do not write declarations for any of the methods if one
                % cannot be written.
                !:Info = SaveInfo
            )
        ;
            Interface0 = instance_body_abstract,
            Interface = Interface0
        ),
        (
            % Don't write an abstract instance declaration
            % if the declaration is already in the `.int' file.
            (
                Interface = instance_body_abstract
            =>
                status_is_exported(Status) = no
            )
        ->
            InstanceDefnToWrite = hlds_instance_defn(ModuleName, Status,
                Context, InstanceConstraints, Types, OriginalTypes,
                Interface, MaybePredProcIds, TVarSet, Proofs),
            intermod_info_get_instances(!.Info, Instances0),
            Instances = [ClassId - InstanceDefnToWrite | Instances0],
            intermod_info_set_instances(Instances, !Info)
        ;
            true
        )
    ;
        DefinedThisModule = no
    ).

    % Resolve overloading of instance methods before writing them
    % to the `.opt' file.
    %
:- pred qualify_instance_method(module_info::in,
    pair(pred_id, instance_method)::in, instance_method::out,
    list(pred_id)::in, list(pred_id)::out) is det.

qualify_instance_method(ModuleInfo, MethodCallPredId - InstanceMethod0,
        InstanceMethod, PredIds0, PredIds) :-
    module_info_pred_info(ModuleInfo, MethodCallPredId, MethodCallPredInfo),
    pred_info_get_arg_types(MethodCallPredInfo, MethodCallTVarSet,
        MethodCallExistQTVars, MethodCallArgTypes),
    pred_info_get_head_type_params(MethodCallPredInfo,
        MethodCallHeadTypeParams),
    InstanceMethod0 = instance_method(PredOrFunc, MethodName,
        InstanceMethodDefn0, MethodArity, MethodContext),
    (
        InstanceMethodDefn0 = instance_proc_def_name(InstanceMethodName0),
        PredOrFunc = pf_function,
        (
            find_func_matching_instance_method(ModuleInfo, InstanceMethodName0,
                MethodArity, MethodCallTVarSet, MethodCallExistQTVars,
                MethodCallArgTypes, MethodCallHeadTypeParams, MethodContext,
                MaybePredId, InstanceMethodName)
        ->
            (
                MaybePredId = yes(PredId),
                PredIds = [PredId | PredIds0]
            ;
                MaybePredId = no,
                PredIds = PredIds0
            ),
            InstanceMethodDefn = instance_proc_def_name(InstanceMethodName)
        ;
            % This will force add_proc to return DoWrite = no.
            PredId = invalid_pred_id,
            PredIds = [PredId | PredIds0],

            % We can just leave the method definition unchanged.
            InstanceMethodDefn = InstanceMethodDefn0
        )
    ;
        InstanceMethodDefn0 = instance_proc_def_name(InstanceMethodName0),
        PredOrFunc = pf_predicate,
        init_markers(Markers),
        resolve_pred_overloading(ModuleInfo, Markers, MethodCallTVarSet,
            MethodCallExistQTVars, MethodCallArgTypes,
            MethodCallHeadTypeParams, MethodContext,
            InstanceMethodName0, InstanceMethodName, PredId),
        PredIds = [PredId | PredIds0],
        InstanceMethodDefn = instance_proc_def_name(InstanceMethodName)
    ;
        InstanceMethodDefn0 = instance_proc_def_clauses(_ItemList),
        % XXX For methods defined using this syntax it is a little tricky
        % to write out the .opt files, so for now I've just disabled
        % intermodule optimization for type class instance declarations
        % using the new syntax.
        %
        % This will force add_proc to return DoWrite = no.
        PredId = invalid_pred_id,
        PredIds = [PredId | PredIds0],
        % We can just leave the method definition unchanged.
        InstanceMethodDefn = InstanceMethodDefn0
    ),
    InstanceMethod = instance_method(PredOrFunc, MethodName,
        InstanceMethodDefn, MethodArity, MethodContext).

    % A `func(x/n) is y' method implementation can match an ordinary function,
    % a field access function or a constructor. For now, if there are multiple
    % possible matches, we don't write the instance method.
    %
:- pred find_func_matching_instance_method(module_info::in, sym_name::in,
    arity::in, tvarset::in, existq_tvars::in, list(mer_type)::in,
    head_type_params::in, prog_context::in, maybe(pred_id)::out,
    sym_name::out) is semidet.

find_func_matching_instance_method(ModuleInfo, InstanceMethodName0,
        MethodArity, MethodCallTVarSet, MethodCallExistQTVars,
        MethodCallArgTypes, MethodCallHeadTypeParams, MethodContext,
        MaybePredId, InstanceMethodName) :-
    module_info_get_ctor_field_table(ModuleInfo, CtorFieldTable),
    (
        is_field_access_function_name(ModuleInfo, InstanceMethodName0,
            MethodArity, _, FieldName),
        map.search(CtorFieldTable, FieldName, FieldDefns)
    ->
        TypeCtors0 = list.map(
            (func(FieldDefn) = TypeCtor :-
                FieldDefn = hlds_ctor_field_defn(_, _, TypeCtor, _, _)
            ), FieldDefns)
    ;
        TypeCtors0 = []
    ),
    module_info_get_cons_table(ModuleInfo, Ctors),
    (
        ConsId = cons(InstanceMethodName0, MethodArity,
            cons_id_dummy_type_ctor),
        search_cons_table(Ctors, ConsId, MatchingConstructors)
    ->
        TypeCtors1 = list.map(
            (func(ConsDefn) = TypeCtor :-
                ConsDefn ^ cons_type_ctor = TypeCtor
            ), MatchingConstructors)
    ;
        TypeCtors1 = []
    ),
    TypeCtors = TypeCtors0 ++ TypeCtors1,

    module_info_get_predicate_table(ModuleInfo, PredicateTable),
    predicate_table_lookup_func_sym_arity(PredicateTable,
        may_be_partially_qualified, InstanceMethodName0, MethodArity, PredIds),
    (
        PredIds = [_ | _],
        find_matching_pred_id(ModuleInfo, PredIds, MethodCallTVarSet,
            MethodCallExistQTVars, MethodCallArgTypes,
            MethodCallHeadTypeParams, no, MethodContext,
            PredId, InstanceMethodFuncName)
    ->
        TypeCtors = [],
        MaybePredId = yes(PredId),
        InstanceMethodName = InstanceMethodFuncName
    ;
        TypeCtors = [TheTypeCtor],
        MaybePredId = no,
        ( TheTypeCtor = type_ctor(qualified(TypeModule, _), _) ->
            UnqualMethodName = unqualify_name(InstanceMethodName0),
            InstanceMethodName = qualified(TypeModule, UnqualMethodName)
        ;
            unexpected($module, $pred, "unqualified type_ctor in " ++
                "hlds_cons_defn or hlds_ctor_field_defn")
        )
    ).

%-----------------------------------------------------------------------------%

:- pred gather_types(intermod_info::in, intermod_info::out) is det.

gather_types(!Info) :-
    intermod_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_type_table(ModuleInfo, TypeTable),
    foldl_over_type_ctor_defns(gather_types_2, TypeTable, !Info).

:- pred gather_types_2(type_ctor::in, hlds_type_defn::in,
    intermod_info::in, intermod_info::out) is det.

gather_types_2(TypeCtor, TypeDefn0, !Info) :-
    intermod_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_name(ModuleInfo, ModuleName),
    ( should_write_type(ModuleName, TypeCtor, TypeDefn0) ->
        hlds_data.get_type_defn_body(TypeDefn0, TypeBody0),
        (
            TypeBody0 = hlds_du_type(Ctors, Tags, CheaperTagTest, Enum,
                MaybeUserEqComp0, MaybeDirectArgCtors, ReservedTag,
                ReservedAddr, MaybeForeign0),
            module_info_get_globals(ModuleInfo, Globals),
            globals.get_target(Globals, Target),

            % Note that we don't resolve overloading for the definitions
            % which won't be used on this back-end, because their unification
            % and comparison predicates have not been typechecked. They are
            % only written to the `.opt' it can be handy when building
            % against a workspace for the other definitions to be present
            % (e.g. when testing compiling a module to IL when the workspace
            % was compiled to C).
            % XXX The above sentence doesn't make sense, and never did
            % (even in the first CVS version in which it appears).

            (
                MaybeForeign0 = yes(ForeignTypeBody0),
                have_foreign_type_for_backend(Target, ForeignTypeBody0, yes)
            ->
                % The header code must be written since it could be used
                % by the foreign type.
                intermod_info_set_write_header(!Info),
                resolve_foreign_type_body_overloading(ModuleInfo, TypeCtor,
                    ForeignTypeBody0, ForeignTypeBody, !Info),
                MaybeForeign = yes(ForeignTypeBody),
                MaybeUserEqComp = MaybeUserEqComp0
            ;
                resolve_unify_compare_overloading(ModuleInfo, TypeCtor,
                    MaybeUserEqComp0, MaybeUserEqComp, !Info),
                MaybeForeign = MaybeForeign0
            ),
            TypeBody = hlds_du_type(Ctors, Tags, CheaperTagTest, Enum,
                MaybeUserEqComp, MaybeDirectArgCtors, ReservedTag,
                ReservedAddr, MaybeForeign),
            hlds_data.set_type_defn_body(TypeBody, TypeDefn0, TypeDefn)
        ;
            TypeBody0 = hlds_foreign_type(ForeignTypeBody0),
            % The header code must be written since it could be used
            % by the foreign type.
            intermod_info_set_write_header(!Info),
            resolve_foreign_type_body_overloading(ModuleInfo, TypeCtor,
                ForeignTypeBody0, ForeignTypeBody, !Info),
            TypeBody = hlds_foreign_type(ForeignTypeBody),
            hlds_data.set_type_defn_body(TypeBody, TypeDefn0, TypeDefn)
        ;
            ( TypeBody0 = hlds_eqv_type(_)
            ; TypeBody0 = hlds_solver_type(_, _)
            ; TypeBody0 = hlds_abstract_type(_)
            ),
            TypeDefn = TypeDefn0
        ),
        intermod_info_get_types(!.Info, Types0),
        intermod_info_set_types([TypeCtor - TypeDefn | Types0], !Info)
    ;
        true
    ).

:- pred resolve_foreign_type_body_overloading(module_info::in,
    type_ctor::in, foreign_type_body::in, foreign_type_body::out,
    intermod_info::in, intermod_info::out) is det.

resolve_foreign_type_body_overloading(ModuleInfo, TypeCtor,
        ForeignTypeBody0, ForeignTypeBody, !Info) :-
    ForeignTypeBody0 = foreign_type_body(MaybeIL0, MaybeC0, MaybeJava0,
        MaybeCSharp0, MaybeErlang0),
    module_info_get_globals(ModuleInfo, Globals),
    globals.get_target(Globals, Target),

    % Note that we don't resolve overloading for the foreign definitions
    % which won't be used on this back-end, because their unification and
    % comparison predicates have not been typechecked. They are only written
    % to the `.opt' it can be handy when building against a workspace
    % for the other definitions to be present (e.g. when testing compiling
    % a module to IL when the workspace was compiled to C).

    (
        ( Target = target_c
        ; Target = target_x86_64
        ; Target = target_erlang
        ),
        resolve_foreign_type_body_overloading_2(ModuleInfo, TypeCtor,
            MaybeC0, MaybeC, !Info)
    ;
        ( Target = target_il
        ; Target = target_csharp
        ; Target = target_java
        ),
        MaybeC = MaybeC0
    ),
    (
        Target = target_il,
        resolve_foreign_type_body_overloading_2(ModuleInfo, TypeCtor,
            MaybeIL0, MaybeIL, !Info)
    ;
        ( Target = target_c
        ; Target = target_csharp
        ; Target = target_java
        ; Target = target_x86_64
        ; Target = target_erlang
        ),
        MaybeIL = MaybeIL0
    ),
    (
        Target = target_csharp,
        resolve_foreign_type_body_overloading_2(ModuleInfo, TypeCtor,
            MaybeCSharp0, MaybeCSharp, !Info)
    ;
        ( Target = target_c
        ; Target = target_il
        ; Target = target_java
        ; Target = target_x86_64
        ; Target = target_erlang
        ),
        MaybeCSharp = MaybeCSharp0
    ),
    (
        Target = target_java,
        resolve_foreign_type_body_overloading_2(ModuleInfo, TypeCtor,
            MaybeJava0, MaybeJava, !Info)
    ;
        ( Target = target_c
        ; Target = target_il
        ; Target = target_csharp
        ; Target = target_x86_64
        ; Target = target_erlang
        ),
        MaybeJava = MaybeJava0
    ),
    (
        Target = target_erlang,
        resolve_foreign_type_body_overloading_2(ModuleInfo, TypeCtor,
            MaybeErlang0, MaybeErlang, !Info)
    ;
        ( Target = target_c
        ; Target = target_il
        ; Target = target_csharp
        ; Target = target_x86_64
        ; Target = target_java
        ),
        MaybeErlang = MaybeErlang0
    ),
    ForeignTypeBody = foreign_type_body(MaybeIL, MaybeC, MaybeJava,
        MaybeCSharp, MaybeErlang).

:- pred resolve_foreign_type_body_overloading_2(module_info::in, type_ctor::in,
    foreign_type_lang_body(T)::in, foreign_type_lang_body(T)::out,
    intermod_info::in, intermod_info::out) is det.

resolve_foreign_type_body_overloading_2(_, _, no, no, !Info).
resolve_foreign_type_body_overloading_2(ModuleInfo, TypeCtor,
        yes(foreign_type_lang_data(Body, MaybeUserEqComp0, Assertions)),
        yes(foreign_type_lang_data(Body, MaybeUserEqComp, Assertions)),
        !Info) :-
    resolve_unify_compare_overloading(ModuleInfo, TypeCtor,
        MaybeUserEqComp0, MaybeUserEqComp, !Info).

:- pred resolve_unify_compare_overloading(module_info::in,
    type_ctor::in, maybe(unify_compare)::in, maybe(unify_compare)::out,
    intermod_info::in, intermod_info::out) is det.

resolve_unify_compare_overloading(_, _, no, no, !Info).
resolve_unify_compare_overloading(_, _,
        yes(abstract_noncanonical_type(IsSolverType)),
        yes(abstract_noncanonical_type(IsSolverType)), !Info).
resolve_unify_compare_overloading(ModuleInfo, TypeCtor,
        yes(unify_compare(MaybeUserEq0, MaybeUserCompare0)),
        yes(unify_compare(MaybeUserEq, MaybeUserCompare)), !Info) :-
    resolve_user_special_pred_overloading(ModuleInfo,
        spec_pred_unify, TypeCtor, MaybeUserEq0, MaybeUserEq, !Info),
    resolve_user_special_pred_overloading(ModuleInfo,
        spec_pred_compare, TypeCtor, MaybeUserCompare0, MaybeUserCompare,
        !Info).

:- pred resolve_user_special_pred_overloading(module_info::in,
    special_pred_id::in, type_ctor::in, maybe(sym_name)::in,
    maybe(sym_name)::out, intermod_info::in, intermod_info::out) is det.

resolve_user_special_pred_overloading(_, _, _, no, no, !Info).
resolve_user_special_pred_overloading(ModuleInfo, SpecialId,
        TypeCtor, yes(Pred0), yes(Pred), !Info) :-
    module_info_get_special_pred_map(ModuleInfo, SpecialPreds),
    map.lookup(SpecialPreds, SpecialId - TypeCtor, UnifyPredId),
    module_info_pred_info(ModuleInfo, UnifyPredId, UnifyPredInfo),
    pred_info_get_arg_types(UnifyPredInfo, TVarSet, ExistQVars, ArgTypes),
    pred_info_get_head_type_params(UnifyPredInfo, HeadTypeParams),
    init_markers(Markers0),
    add_marker(marker_calls_are_fully_qualified, Markers0, Markers),
    pred_info_get_context(UnifyPredInfo, Context),
    resolve_pred_overloading(ModuleInfo, Markers, TVarSet, ExistQVars,
        ArgTypes, HeadTypeParams, Context, Pred0, Pred, UserEqPredId),
    add_proc(UserEqPredId, _, !Info).

:- pred should_write_type(module_name::in, type_ctor::in, hlds_type_defn::in)
    is semidet.

should_write_type(ModuleName, TypeCtor, TypeDefn) :-
    hlds_data.get_type_defn_status(TypeDefn, ImportStatus),
    TypeCtor = type_ctor(Name, _Arity),
    Name = qualified(ModuleName, _),
    import_status_to_write(ImportStatus).

%-----------------------------------------------------------------------------%

    % Output module imports, types, modes, insts and predicates.
    %
:- pred write_intermod_info(intermod_info::in, io::di, io::uo) is det.

write_intermod_info(IntermodInfo, !IO) :-
    intermod_info_get_module_info(IntermodInfo, ModuleInfo),
    module_info_get_name(ModuleInfo, ModuleName),
    io.write_string(":- module ", !IO),
    mercury_output_bracketed_sym_name(ModuleName, !IO),
    io.write_string(".\n", !IO),

    intermod_info_get_preds(IntermodInfo, Preds),
    intermod_info_get_pred_decls(IntermodInfo, PredDecls),
    intermod_info_get_instances(IntermodInfo, Instances),
    (
        % If none of these item types need writing, nothing else
        % needs to be written.

        set.empty(Preds),
        set.empty(PredDecls),
        Instances = [],
        module_info_get_type_table(ModuleInfo, TypeTable),
        get_all_type_ctor_defns(TypeTable, TypeCtorsDefns),
        \+ (
            list.member(_TypeCtor - TypeDefn, TypeCtorsDefns),
            hlds_data.get_type_defn_status(TypeDefn, Status),
            ( Status = status_abstract_exported
            ; Status = status_exported_to_submodules
            )
        )
    ->
        true
    ;
        write_intermod_info_body(IntermodInfo, !IO)
    ).

:- pred write_intermod_info_body(intermod_info::in, io::di, io::uo) is det.

write_intermod_info_body(IntermodInfo, !IO) :-
    IntermodInfo = intermod_info(_, Preds0, PredDecls0, Instances, Types,
        ModuleInfo, WriteHeader, _, _),
    set.to_sorted_list(Preds0, Preds),
    set.to_sorted_list(PredDecls0, PredDecls),

    module_info_get_imported_module_specifiers(ModuleInfo, Modules0),
    set.to_sorted_list(Modules0, Modules),
    (
        Modules = [_ | _],
        % XXX Modules could and should be reduced to the set of modules
        % that are actually needed by the items being written.
        io.write_string(":- use_module ", !IO),
        intermod_write_modules(Modules, !IO)
    ;
        Modules = []
    ),

    module_info_get_globals(ModuleInfo, Globals),
    OutInfo0 = init_hlds_out_info(Globals),

    % We don't want to output line numbers in the .opt files,
    % since that causes spurious changes to the .opt files
    % when you make trivial changes (e.g. add comments) to the source files.
    MercInfo0 = OutInfo0 ^ hoi_mercury_to_mercury,
    MercInfo = merc_out_info_disable_line_numbers(MercInfo0),
    OutInfo = OutInfo0 ^ hoi_mercury_to_mercury := MercInfo,

    intermod_write_types(OutInfo, Types, !IO),
    intermod_write_insts(OutInfo, ModuleInfo, !IO),
    intermod_write_modes(OutInfo, ModuleInfo, !IO),
    intermod_write_classes(OutInfo, ModuleInfo, !IO),
    intermod_write_instances(OutInfo, Instances, !IO),

    % Disable verbose dumping of clauses.
    OutInfoForPreds = OutInfo ^ hoi_dump_hlds_options := "",
    (
        WriteHeader = yes,
        module_info_get_foreign_import_module(ModuleInfo, RevForeignImports),
        ForeignImports = list.reverse(RevForeignImports),

        list.foldl(
            (pred(ForeignImport::in, IO0::di, IO::uo) is det :-
                ForeignImport = foreign_import_module_info(Lang, Import, _),
                FIMInfo = pragma_info_foreign_import_module(Lang, Import),
                mercury_output_pragma_foreign_import_module(FIMInfo, IO0, IO)
            ), ForeignImports, !IO)
    ;
        WriteHeader = no
    ),
    intermod_write_pred_decls(ModuleInfo, PredDecls, !IO),
    intermod_write_preds(OutInfoForPreds, ModuleInfo, Preds, !IO).

:- pred intermod_write_modules(list(module_name)::in, io::di, io::uo) is det.

intermod_write_modules([], !IO).
intermod_write_modules([Module | Rest], !IO) :-
    mercury_output_bracketed_sym_name(Module, !IO),
    (
        Rest = [],
        io.write_string(".\n", !IO)
    ;
        Rest = [_ | _],
        io.write_string(", ", !IO),
        intermod_write_modules(Rest, !IO)
    ).

:- pred intermod_write_types(hlds_out_info::in,
    assoc_list(type_ctor, hlds_type_defn)::in, io::di, io::uo) is det.

intermod_write_types(OutInfo, Types, !IO) :-
    list.foldl(intermod_write_type(OutInfo), Types, !IO).

:- pred intermod_write_type(hlds_out_info::in,
    pair(type_ctor, hlds_type_defn)::in, io::di, io::uo) is det.

intermod_write_type(OutInfo, TypeCtor - TypeDefn, !IO) :-
    hlds_data.get_type_defn_tvarset(TypeDefn, VarSet),
    hlds_data.get_type_defn_tparams(TypeDefn, Args),
    hlds_data.get_type_defn_body(TypeDefn, Body),
    hlds_data.get_type_defn_context(TypeDefn, Context),
    TypeCtor = type_ctor(Name, _Arity),
    (
        Body = hlds_du_type(Ctors, _, _, _, MaybeUserEqComp,
            MaybeDirectArgCtors,
            _, _, _),
        TypeBody = parse_tree_du_type(Ctors, MaybeUserEqComp,
            MaybeDirectArgCtors)
    ;
        Body = hlds_eqv_type(EqvType),
        TypeBody = parse_tree_eqv_type(EqvType)
    ;
        Body = hlds_abstract_type(Details),
        TypeBody = parse_tree_abstract_type(Details)
    ;
        Body = hlds_foreign_type(_),
        TypeBody = parse_tree_abstract_type(abstract_type_general)
    ;
        Body = hlds_solver_type(SolverTypeDetails, MaybeUserEqComp),
        TypeBody = parse_tree_solver_type(SolverTypeDetails, MaybeUserEqComp)
    ),
    MainItemTypeDefn = item_type_defn_info(VarSet, Name, Args, TypeBody,
        cond_true, Context, -1),
    MainItem = item_type_defn(MainItemTypeDefn),
    MercInfo = OutInfo ^ hoi_mercury_to_mercury,
    mercury_output_item(MercInfo, MainItem, !IO),
    (
        ( Body = hlds_foreign_type(ForeignTypeBody)
        ; Body ^ du_type_is_foreign_type = yes(ForeignTypeBody)
        ),
        ForeignTypeBody = foreign_type_body(MaybeIL, MaybeC, MaybeJava,
            MaybeCSharp, MaybeErlang)
    ->
        (
            MaybeIL = yes(DataIL),
            DataIL = foreign_type_lang_data(ILForeignType, ILMaybeUserEqComp,
                AssertionsIL),
            ILItemTypeDefn = item_type_defn_info(VarSet, Name, Args,
                parse_tree_foreign_type(il(ILForeignType),
                    ILMaybeUserEqComp, AssertionsIL),
                cond_true, Context, -1),
            ILItem = item_type_defn(ILItemTypeDefn),
            mercury_output_item(MercInfo, ILItem, !IO)
        ;
            MaybeIL = no
        ),
        (
            MaybeC = yes(DataC),
            DataC = foreign_type_lang_data(CForeignType,
                CMaybeUserEqComp, AssertionsC),
            CItemTypeDefn = item_type_defn_info(VarSet, Name, Args,
                parse_tree_foreign_type(c(CForeignType),
                    CMaybeUserEqComp, AssertionsC),
                cond_true, Context, -1),
            CItem = item_type_defn(CItemTypeDefn),
            mercury_output_item(MercInfo, CItem, !IO)
        ;
            MaybeC = no
        ),
        (
            MaybeJava = yes(DataJava),
            DataJava = foreign_type_lang_data(JavaForeignType,
                JavaMaybeUserEqComp, AssertionsJava),
            JavaItemTypeDefn = item_type_defn_info(VarSet, Name, Args,
                parse_tree_foreign_type(java(JavaForeignType),
                    JavaMaybeUserEqComp, AssertionsJava),
                cond_true, Context, -1),
            JavaItem = item_type_defn(JavaItemTypeDefn),
            mercury_output_item(MercInfo, JavaItem, !IO)
        ;
            MaybeJava = no
        ),
        (
            MaybeCSharp = yes(DataCSharp),
            DataCSharp = foreign_type_lang_data(CSharpForeignType,
                CSharpMaybeUserEqComp, AssertionsCSharp),
            CSharpItemTypeDefn = item_type_defn_info(VarSet, Name, Args,
                parse_tree_foreign_type(csharp(CSharpForeignType),
                    CSharpMaybeUserEqComp, AssertionsCSharp),
                cond_true, Context, -1),
            CSharpItem = item_type_defn(CSharpItemTypeDefn),
            mercury_output_item(MercInfo, CSharpItem, !IO)
        ;
            MaybeCSharp = no
        ),
        (
            MaybeErlang = yes(DataErlang),
            DataErlang = foreign_type_lang_data(ErlangForeignType,
                ErlangMaybeUserEqComp, AssertionsErlang),
            ErlangItemTypeDefn = item_type_defn_info(VarSet, Name, Args,
                parse_tree_foreign_type(erlang(ErlangForeignType),
                    ErlangMaybeUserEqComp, AssertionsErlang),
                cond_true, Context, -1),
            ErlangItem = item_type_defn(ErlangItemTypeDefn),
            mercury_output_item(MercInfo, ErlangItem, !IO)
        ;
            MaybeErlang = no
        )
    ;
        true
    ),
    (
        ReservedTag = Body ^ du_type_reserved_tag,
        ReservedTag = uses_reserved_tag
    ->
        % The pragma_origin doesn't matter here.
        ReserveItemPragma = item_pragma_info(user,
            pragma_reserve_tag(TypeCtor), Context, -1),
        ReserveItem = item_pragma(ReserveItemPragma),
        mercury_output_item(MercInfo, ReserveItem, !IO)
    ;
        true
    ),
    (
        Body = hlds_du_type(_, ConsTagVals, _, DuTypeKind, _, _, _, _, _),
        DuTypeKind = du_type_kind_foreign_enum(Lang)
    ->
        map.foldl(gather_foreign_enum_value_pair, ConsTagVals, [],
            ForeignEnumVals),
        FEInfo = pragma_info_foreign_enum(Lang, TypeCtor, ForeignEnumVals),
        ForeignPragma = pragma_foreign_enum(FEInfo),
        ForeignItemPragma = item_pragma_info(user, ForeignPragma, Context, -1),
        ForeignItem = item_pragma(ForeignItemPragma),
        mercury_output_item(MercInfo, ForeignItem, !IO)
    ;
        true
    ).

:- pred gather_foreign_enum_value_pair(cons_id::in, cons_tag::in,
    assoc_list(sym_name, string)::in, assoc_list(sym_name, string)::out)
    is det.

gather_foreign_enum_value_pair(ConsId, ConsTag, !Values) :-
    ( ConsId = cons(SymName0, 0, _) ->
        SymName = SymName0
    ;
        unexpected($module, $pred, "expected enumeration constant")
    ),
    ( ConsTag = foreign_tag(_ForeignLang, ForeignTag0) ->
        ForeignTag = ForeignTag0
    ;
        unexpected($module, $pred, "expected foreign tag")
    ),
    !:Values = [SymName - ForeignTag | !.Values].

:- pred intermod_write_modes(hlds_out_info::in, module_info::in,
    io::di, io::uo) is det.

intermod_write_modes(OutInfo, ModuleInfo, !IO) :-
    module_info_get_name(ModuleInfo, ModuleName),
    module_info_get_mode_table(ModuleInfo, Modes),
    mode_table_get_mode_defns(Modes, ModeDefns),
    map.foldl(intermod_write_mode(OutInfo, ModuleName), ModeDefns, !IO).

:- pred intermod_write_mode(hlds_out_info::in, module_name::in, mode_id::in,
    hlds_mode_defn::in, io::di, io::uo) is det.

intermod_write_mode(OutInfo, ModuleName, ModeId, ModeDefn, !IO) :-
    ModeId = mode_id(SymName, _Arity),
    ModeDefn = hlds_mode_defn(Varset, Args, eqv_mode(Mode), Context,
        ImportStatus),
    (
        SymName = qualified(ModuleName, _),
        import_status_to_write(ImportStatus)
    ->
        ItemModeDefn = item_mode_defn_info(Varset, SymName, Args,
            eqv_mode(Mode), cond_true, Context, -1),
        Item = item_mode_defn(ItemModeDefn),
        MercInfo = OutInfo ^ hoi_mercury_to_mercury,
        mercury_output_item(MercInfo, Item, !IO)
    ;
        true
    ).

:- pred intermod_write_insts(hlds_out_info::in, module_info::in,
    io::di, io::uo) is det.

intermod_write_insts(OutInfo, ModuleInfo, !IO) :-
    module_info_get_name(ModuleInfo, ModuleName),
    module_info_get_inst_table(ModuleInfo, Insts),
    inst_table_get_user_insts(Insts, UserInsts),
    user_inst_table_get_inst_defns(UserInsts, InstDefns),
    map.foldl(intermod_write_inst(OutInfo, ModuleName), InstDefns, !IO).

:- pred intermod_write_inst(hlds_out_info::in, module_name::in, inst_id::in,
    hlds_inst_defn::in, io::di, io::uo) is det.

intermod_write_inst(OutInfo, ModuleName, InstId, InstDefn, !IO) :-
    InstId = inst_id(SymName, _Arity),
    InstDefn = hlds_inst_defn(Varset, Args, Body, Context, ImportStatus),
    (
        SymName = qualified(ModuleName, _),
        import_status_to_write(ImportStatus)
    ->
        (
            Body = eqv_inst(Inst2),
            InstBody = eqv_inst(Inst2)
        ;
            Body = abstract_inst,
            InstBody = abstract_inst
        ),
        ItemInstDefn = item_inst_defn_info(Varset, SymName, Args, InstBody,
            cond_true, Context, -1),
        Item = item_inst_defn(ItemInstDefn),
        MercInfo = OutInfo ^ hoi_mercury_to_mercury,
        mercury_output_item(MercInfo, Item, !IO)
    ;
        true
    ).

:- pred intermod_write_classes(hlds_out_info::in, module_info::in,
    io::di, io::uo) is det.

intermod_write_classes(OutInfo, ModuleInfo, !IO) :-
    module_info_get_name(ModuleInfo, ModuleName),
    module_info_get_class_table(ModuleInfo, Classes),
    map.foldl(intermod_write_class(OutInfo, ModuleName), Classes, !IO).

:- pred intermod_write_class(hlds_out_info::in, module_name::in, class_id::in,
    hlds_class_defn::in, io::di, io::uo) is det.

intermod_write_class(OutInfo, ModuleName, ClassId, ClassDefn, !IO) :-
    ClassDefn = hlds_class_defn(ImportStatus, Constraints, HLDSFunDeps,
        _Ancestors, TVars, _Kinds, Interface, _HLDSClassInterface, TVarSet,
        Context),
    ClassId = class_id(QualifiedClassName, _),
    (
        QualifiedClassName = qualified(ModuleName, _),
        import_status_to_write(ImportStatus)
    ->
        FunDeps = list.map(unmake_hlds_class_fundep(TVars), HLDSFunDeps),
        ItemTypeClass = item_typeclass_info(Constraints, FunDeps,
            QualifiedClassName, TVars, Interface, TVarSet, Context, -1),
        Item = item_typeclass(ItemTypeClass),
        MercInfo = OutInfo ^ hoi_mercury_to_mercury,
        mercury_output_item(MercInfo, Item, !IO)
    ;
        true
    ).

:- func unmake_hlds_class_fundep(list(tvar), hlds_class_fundep) = prog_fundep.

unmake_hlds_class_fundep(TVars, fundep(Domain0, Range0))
        = fundep(Domain, Range) :-
    Domain = unmake_hlds_class_fundep_2(TVars, Domain0),
    Range = unmake_hlds_class_fundep_2(TVars, Range0).

:- func unmake_hlds_class_fundep_2(list(tvar), set(hlds_class_argpos)) =
    list(tvar).

unmake_hlds_class_fundep_2(TVars, Set) = solutions.solutions(P) :-
    P = (pred(TVar::out) is nondet :-
        set.member(N, Set),
        TVar = list.det_index1(TVars, N)
    ).

:- pred intermod_write_instances(hlds_out_info::in,
    assoc_list(class_id, hlds_instance_defn)::in, io::di, io::uo) is det.

intermod_write_instances(OutInfo, Instances, !IO) :-
    list.foldl(intermod_write_instance(OutInfo), Instances, !IO).

:- pred intermod_write_instance(hlds_out_info::in,
    pair(class_id, hlds_instance_defn)::in, io::di, io::uo) is det.

intermod_write_instance(OutInfo, ClassId - InstanceDefn, !IO) :-
    InstanceDefn = hlds_instance_defn(ModuleName, _, Context, Constraints,
        Types, OriginalTypes, Body, _, TVarSet, _),
    ClassId = class_id(ClassName, _),
    ItemInstance = item_instance_info(Constraints, ClassName,
        Types, OriginalTypes, Body, TVarSet, ModuleName, Context, -1),
    Item = item_instance(ItemInstance),
    MercInfo = OutInfo ^ hoi_mercury_to_mercury,
    mercury_output_item(MercInfo, Item, !IO).

    % We need to write all the declarations for local predicates so
    % the procedure labels for the C code are calculated correctly.
    %
:- pred intermod_write_pred_decls(module_info::in, list(pred_id)::in,
    io::di, io::uo) is det.

intermod_write_pred_decls(_, [], !IO).
intermod_write_pred_decls(ModuleInfo, [PredId | PredIds], !IO) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    Module = pred_info_module(PredInfo),
    Name = pred_info_name(PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    pred_info_get_arg_types(PredInfo, TVarSet, ExistQVars, ArgTypes),
    pred_info_get_context(PredInfo, Context),
    pred_info_get_purity(PredInfo, Purity),
    pred_info_get_class_context(PredInfo, ClassContext),
    pred_info_get_goal_type(PredInfo, GoalType),
    (
        GoalType = goal_type_foreign,
        % For foreign code goals, we cannot append variable numbers to type
        % variables in the predicate declaration, because the foreign code
        % may contain references to variables such as `TypeInfo_for_T'
        % which will break if `T' is written as `T_1' in the pred declaration.
        AppendVarNums = no
    ;
        GoalType = goal_type_clause_and_foreign,
        % Because pragmas may be present, we treat this case like
        % pragmas above.
        AppendVarNums = no
    ;
        GoalType = goal_type_clause,
        AppendVarNums = yes
    ;
        GoalType = goal_type_promise(_),
        AppendVarNums = yes
    ;
        GoalType = goal_type_none,
        AppendVarNums = yes
    ),
    (
        PredOrFunc = pf_predicate,
        mercury_output_pred_type(TVarSet, ExistQVars, qualified(Module, Name),
            ArgTypes, no, Purity, ClassContext, Context, AppendVarNums, !IO)
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(ArgTypes, FuncArgTypes, FuncRetType),
        mercury_output_func_type(TVarSet, ExistQVars, qualified(Module, Name),
            FuncArgTypes, FuncRetType, no, Purity, ClassContext, Context,
            AppendVarNums, !IO)
    ),
    pred_info_get_procedures(PredInfo, Procs),
    ProcIds = pred_info_procids(PredInfo),
        % Make sure the mode declarations go out in the same order
        % they came in, so that the all the modes get the same proc_id
        % in the importing modules.
    CompareProcId =
        (pred(ProcId1::in, ProcId2::in, Result::out) is det :-
            proc_id_to_int(ProcId1, ProcInt1),
            proc_id_to_int(ProcId2, ProcInt2),
            compare(Result, ProcInt1, ProcInt2)
        ),
    list.sort(CompareProcId, ProcIds, SortedProcIds),
    intermod_write_pred_modes(Procs, qualified(Module, Name), PredOrFunc,
        SortedProcIds, !IO),
    intermod_write_pragmas(PredInfo, !IO),
    intermod_write_type_spec_pragmas(ModuleInfo, PredId, !IO),
    intermod_write_pred_decls(ModuleInfo, PredIds, !IO).

:- pred intermod_write_pred_modes(map(proc_id, proc_info)::in, sym_name::in,
    pred_or_func::in, list(proc_id)::in, io::di, io::uo) is det.

intermod_write_pred_modes(_, _, _, [], !IO).
intermod_write_pred_modes(Procs, SymName, PredOrFunc, [ProcId | ProcIds],
        !IO) :-
    map.lookup(Procs, ProcId, ProcInfo),
    proc_info_get_maybe_declared_argmodes(ProcInfo, MaybeArgModes),
    proc_info_get_declared_determinism(ProcInfo, MaybeDetism),
    (
        MaybeArgModes = yes(ArgModes0),
        MaybeDetism = yes(Detism0)
    ->
        ArgModes = ArgModes0,
        Detism = Detism0
    ;
        unexpected($module, $pred, "attempt to write undeclared mode")
    ),
    proc_info_get_context(ProcInfo, Context),
    varset.init(Varset),
    (
        PredOrFunc = pf_function,
        pred_args_to_func_args(ArgModes, FuncArgModes, FuncRetMode),
        mercury_output_func_mode_decl(Varset, SymName,
            FuncArgModes, FuncRetMode, yes(Detism), Context, !IO)
    ;
        PredOrFunc = pf_predicate,
        mercury_output_pred_mode_decl(Varset, SymName, ArgModes,
            yes(Detism), Context, !IO)
    ),
    intermod_write_pred_modes(Procs, SymName, PredOrFunc, ProcIds, !IO).

:- pred intermod_write_preds(hlds_out_info::in, module_info::in,
    list(pred_id)::in, io::di, io::uo) is det.

intermod_write_preds(_, _, [], !IO).
intermod_write_preds(OutInfo, ModuleInfo, [PredId | PredIds], !IO) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    Module = pred_info_module(PredInfo),
    Name = pred_info_name(PredInfo),
    SymName = qualified(Module, Name),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    intermod_write_pragmas(PredInfo, !IO),
    % The type specialization pragmas for exported preds should
    % already be in the interface file.

    pred_info_get_clauses_info(PredInfo, ClausesInfo),
    clauses_info_get_varset(ClausesInfo, VarSet),
    clauses_info_get_headvar_list(ClausesInfo, HeadVars),
    clauses_info_get_clauses_rep(ClausesInfo, ClausesRep, _ItemNumbers),
    clauses_info_get_vartypes(ClausesInfo, VarTypes),
    get_clause_list(ClausesRep, Clauses),

    pred_info_get_goal_type(PredInfo, GoalType),
    (
        GoalType = goal_type_promise(PromiseType),
        (
            Clauses = [Clause],
            AppendVarNums = no,
            write_promise(OutInfo, PromiseType, 0, ModuleInfo,
                PredId, VarSet, AppendVarNums, HeadVars, PredOrFunc, Clause,
                no_varset_vartypes, !IO)
        ;
            ( Clauses = []
            ; Clauses = [_, _ | _]
            ),
            unexpected($module, $pred, "assertion not a single clause.")
        )
    ;
        ( GoalType = goal_type_clause
        ; GoalType = goal_type_foreign
        ; GoalType = goal_type_clause_and_foreign
        ; GoalType = goal_type_none
        ),
        pred_info_get_typevarset(PredInfo, TypeVarset),
        MaybeVarTypes = varset_vartypes(TypeVarset, VarTypes),
        list.foldl(
            intermod_write_clause(OutInfo, ModuleInfo, PredId, VarSet,
                HeadVars, PredOrFunc, SymName, MaybeVarTypes),
            Clauses, !IO)
    ),
    intermod_write_preds(OutInfo, ModuleInfo, PredIds, !IO).

:- pred intermod_write_clause(hlds_out_info::in, module_info::in, pred_id::in,
    prog_varset::in, list(prog_var)::in, pred_or_func::in, sym_name::in,
    maybe_vartypes::in, clause::in, io::di, io::uo) is det.

intermod_write_clause(OutInfo, ModuleInfo, PredId, VarSet, HeadVars,
        PredOrFunc, SymName, MaybeVarTypes, Clause0, !IO) :-
    Clause0 = clause(ApplicableProcIds, Goal, ImplLang, _, _),
    (
        ImplLang = impl_lang_mercury,
        strip_headvar_unifications(HeadVars, Clause0, ClauseHeadVars, Clause),
        % Variable numbers need to be appended for the case
        % where the added arguments for a DCG pred expression
        % are named the same as variables in the enclosing clause.
        AppendVarNums = yes,
        UseDeclaredModes = yes,
        write_clause(OutInfo, output_mercury, 1, ModuleInfo, PredId, VarSet,
            AppendVarNums, ClauseHeadVars, PredOrFunc, Clause,
            UseDeclaredModes, MaybeVarTypes, !IO)
    ;
        ImplLang = impl_lang_foreign(_),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        pred_info_get_procedures(PredInfo, Procs),
        (
            (
                % Pull the foreign code out of the goal.
                Goal = hlds_goal(conj(plain_conj, Goals), _),
                list.filter(
                    (pred(G::in) is semidet :-
                        G = hlds_goal(GE, _),
                        GE = call_foreign_proc(_, _, _, _, _, _, _)
                    ), Goals, [ForeignCodeGoal]),
                ForeignCodeGoal = hlds_goal(ForeignCodeGoalExpr, _),
                ForeignCodeGoalExpr = call_foreign_proc(Attributes, _, _,
                    Args, _ExtraArgs, _MaybeTraceRuntimeCond, PragmaCode)
            ;
                Goal = hlds_goal(GoalExpr, _),
                GoalExpr = call_foreign_proc(Attributes, _, _,
                    Args, _ExtraArgs, _MaybeTraceRuntimeCond, PragmaCode)
            )
        ->
            (
                ApplicableProcIds = all_modes,
                unexpected($module, $pred, "all_modes foreign_proc")
            ;
                ApplicableProcIds = selected_modes(ProcIds),
                list.foldl(
                    intermod_write_foreign_clause(Procs, PredOrFunc,
                        PragmaCode, Attributes, Args, VarSet, SymName),
                    ProcIds, !IO)
            )
        ;
            unexpected($module, $pred, "did not find foreign_proc")
        )
    ).

:- pred intermod_write_foreign_clause(proc_table::in, pred_or_func::in,
    pragma_foreign_proc_impl::in, pragma_foreign_proc_attributes::in,
    list(foreign_arg)::in, prog_varset::in, sym_name::in, proc_id::in,
    io::di, io::uo) is det.

intermod_write_foreign_clause(Procs, PredOrFunc, PragmaImpl,
        Attributes, Args, ProgVarset0, SymName, ProcId, !IO) :-
    map.lookup(Procs, ProcId, ProcInfo),
    proc_info_get_maybe_declared_argmodes(ProcInfo, MaybeArgModes),
    (
        MaybeArgModes = yes(ArgModes),
        get_pragma_foreign_code_vars(Args, ArgModes,
            ProgVarset0, ProgVarset, PragmaVars),
        proc_info_get_inst_varset(ProcInfo, InstVarset),
        FPInfo = pragma_info_foreign_proc(Attributes, SymName,
            PredOrFunc, PragmaVars, ProgVarset, InstVarset, PragmaImpl),
        mercury_output_pragma_foreign_proc(FPInfo, !IO)
    ;
        MaybeArgModes = no,
        unexpected($module, $pred, "no mode declaration")
    ).

    % Strip the `Headvar.n = Term' unifications from each clause,
    % except if the `Term' is a lambda expression.
    %
    % At least two problems occur if this is not done:
    % - in some cases where nested unique modes were accepted by
    %   mode analysis, the extra aliasing added by the extra level
    %   of headvar unifications caused mode analysis to report
    %   an error (ground expected unique), when analysing the
    %   clauses read in from `.opt' files.
    % - only HeadVar unifications may be reordered with impure goals,
    %   so a mode error results for the second level of headvar
    %   unifications added when the clauses are read in again from
    %   the `.opt' file. Clauses containing impure goals are not
    %   written to the `.opt' file for this reason.
    %
:- pred strip_headvar_unifications(list(prog_var)::in,
    clause::in, list(prog_term)::out, clause::out) is det.

strip_headvar_unifications(HeadVars, Clause0, HeadTerms, Clause) :-
    Goal0 = Clause0 ^ clause_body,
    Goal0 = hlds_goal(_, GoalInfo0),
    goal_to_conj_list(Goal0, Goals0),
    map.init(HeadVarMap0),
    (
        strip_headvar_unifications_from_goal_list(Goals0, HeadVars,
            [], Goals, HeadVarMap0, HeadVarMap)
    ->
        list.map(
            (pred(HeadVar0::in, HeadTerm::out) is det :-
                ( map.search(HeadVarMap, HeadVar0, HeadTerm0) ->
                    HeadTerm = HeadTerm0
                ;
                    Context = Clause0 ^ clause_context,
                    HeadTerm = term.variable(HeadVar0, Context)
                )
            ), HeadVars, HeadTerms),
        conj_list_to_goal(Goals, GoalInfo0, Goal),
        Clause = Clause0 ^ clause_body := Goal
    ;
        term.var_list_to_term_list(HeadVars, HeadTerms),
        Clause = Clause0
    ).

:- pred strip_headvar_unifications_from_goal_list(list(hlds_goal)::in,
    list(prog_var)::in, list(hlds_goal)::in, list(hlds_goal)::out,
    map(prog_var, prog_term)::in,
    map(prog_var, prog_term)::out) is semidet.

strip_headvar_unifications_from_goal_list([], _, RevGoals, Goals,
        !HeadVarMap) :-
    list.reverse(RevGoals, Goals).
strip_headvar_unifications_from_goal_list([Goal | Goals0], HeadVars,
        RevGoals0, Goals, !HeadVarMap) :-
    (
        Goal = hlds_goal(unify(LHSVar, RHS, _, _, _), _),
        list.member(LHSVar, HeadVars),
        term.context_init(Context),
        (
            RHS = rhs_var(RHSVar),
            RHSTerm = term.variable(RHSVar, Context)
        ;
            RHS = rhs_functor(ConsId, _, Args),
            (
                ConsId = int_const(Int),
                RHSTerm = term.functor(term.integer(Int), [], Context)
            ;
                ConsId = float_const(Float),
                RHSTerm = term.functor(term.float(Float), [], Context)
            ;
                ConsId = char_const(Char),
                RHSTerm = term.functor(term.atom(term_io.escaped_char(Char)),
                    [], Context)
            ;
                ConsId = string_const(String),
                RHSTerm = term.functor(term.string(String), [], Context)
            ;
                ConsId = cons(SymName, _, _),
                term.var_list_to_term_list(Args, ArgTerms),
                construct_qualified_term(SymName, ArgTerms, RHSTerm)
            )
        )
    ->
        % Don't strip the headvar unifications if one of the
        % headvars appears twice. This should probably never happen.
        map.insert(LHSVar, RHSTerm, !HeadVarMap),
        RevGoals1 = RevGoals0
    ;
        RevGoals1 = [Goal | RevGoals0]
    ),
    strip_headvar_unifications_from_goal_list(Goals0, HeadVars,
        RevGoals1, Goals, !HeadVarMap).

:- pred intermod_write_pragmas(pred_info::in, io::di, io::uo) is det.

intermod_write_pragmas(PredInfo, !IO) :-
    Module = pred_info_module(PredInfo),
    Name = pred_info_name(PredInfo),
    Arity = pred_info_orig_arity(PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    SymName = qualified(Module, Name),
    pred_info_get_markers(PredInfo, Markers),
    markers_to_marker_list(Markers, MarkerList),
    intermod_write_pragma_markers(SymName, Arity, MarkerList, PredOrFunc, !IO).

:- pred intermod_write_pragma_markers(sym_name::in, int::in, list(marker)::in,
    pred_or_func::in, io::di, io::uo) is det.

intermod_write_pragma_markers(_, _, [], _, !IO).
intermod_write_pragma_markers(SymName, Arity, [Marker | Markers], PredOrFunc,
        !IO) :-
    should_output_marker(Marker, ShouldOutput),
    (
        ShouldOutput = yes,
        marker_name(Marker, Name),
        mercury_output_pragma_decl(SymName, Arity, PredOrFunc, Name, no, !IO)
    ;
        ShouldOutput = no
    ),
    intermod_write_pragma_markers(SymName, Arity, Markers, PredOrFunc, !IO).

:- pred intermod_write_type_spec_pragmas(module_info::in, pred_id::in,
    io::di, io::uo) is det.

intermod_write_type_spec_pragmas(ModuleInfo, PredId, !IO) :-
    module_info_get_type_spec_info(ModuleInfo, TypeSpecInfo),
    PragmaMap = TypeSpecInfo ^ pragma_map,
    ( multi_map.search(PragmaMap, PredId, TypeSpecPragmas) ->
        AppendVarnums = yes,
        list.foldl(mercury_output_pragma_type_spec(AppendVarnums),
            TypeSpecPragmas, !IO)
    ;
        true
    ).

    % Is a pragma declaration required in the `.opt' file for
    % a predicate with the given marker.
    %
:- pred should_output_marker(marker::in, bool::out) is det.

should_output_marker(marker_stub, no).
should_output_marker(marker_builtin_stub, no).
    % Since the inferred declarations are output, these
    % don't need to be done in the importing module.
should_output_marker(marker_infer_type, no).
should_output_marker(marker_infer_modes, no).
    % Purity is output as part of the pred/func decl.
should_output_marker(marker_is_impure, no).
should_output_marker(marker_is_semipure, no).
    % There is no pragma required for generated class methods.
should_output_marker(marker_class_method, no).
should_output_marker(marker_class_instance_method, no).
should_output_marker(marker_named_class_instance_method, no).
    % The warning for calls to local obsolete predicates should appear
    % once in the defining module, not in importing modules.
should_output_marker(marker_obsolete, no).
should_output_marker(marker_no_detism_warning, no).
should_output_marker(marker_user_marked_inline, yes).
should_output_marker(marker_user_marked_no_inline, yes).
should_output_marker(marker_heuristic_inline, no).
should_output_marker(marker_promised_pure, yes).
should_output_marker(marker_promised_semipure, yes).
should_output_marker(marker_promised_equivalent_clauses, yes).
should_output_marker(marker_terminates, yes).
should_output_marker(marker_does_not_terminate, yes).
    % Termination should only be checked in the defining module.
should_output_marker(marker_check_termination, no).
should_output_marker(marker_calls_are_fully_qualified, no).
should_output_marker(marker_mode_check_clauses, yes).
should_output_marker(marker_mutable_access_pred, no).
should_output_marker(marker_has_require_scope, no).
should_output_marker(marker_has_format_call, no).

:- pred get_pragma_foreign_code_vars(list(foreign_arg)::in, list(mer_mode)::in,
    prog_varset::in, prog_varset::out, list(pragma_var)::out) is det.

get_pragma_foreign_code_vars(Args, Modes, !VarSet, PragmaVars) :-
    (
        Args = [Arg | ArgsTail],
        Modes = [Mode | ModesTail],
        Arg = foreign_arg(Var, MaybeNameAndMode, _, _),
        (
            MaybeNameAndMode = no,
            Name = "_"
        ;
            MaybeNameAndMode = yes(Name - _Mode2)
        ),
        PragmaVar = pragma_var(Var, Name, Mode, native_if_possible),
        varset.name_var(Var, Name, !VarSet),
        get_pragma_foreign_code_vars(ArgsTail, ModesTail, !VarSet,
            PragmaVarsTail),
        PragmaVars = [PragmaVar | PragmaVarsTail]
    ;
        Args = [],
        Modes = [],
        PragmaVars = []
    ;
        Args = [],
        Modes = [_ | _],
        unexpected($module, $pred, "list length mismatch")
    ;
        Args = [_ | _],
        Modes = [],
        unexpected($module, $pred, "list length mismatch")
    ).

%-----------------------------------------------------------------------------%

    % A collection of stuff to go in the .opt file.
    %
:- type intermod_info
    --->    intermod_info(
                % Modules to import.
                im_modules              :: set(module_name),

                % Preds to output clauses for.
                im_preds                :: set(pred_id),

                % Preds to output decls for.
                im_pred_decls           :: set(pred_id),

                % Instances declarations to write.
                im_instances            :: assoc_list(class_id,
                                            hlds_instance_defn),

                % Type declarations to write.
                im_types                :: assoc_list(type_ctor,
                                            hlds_type_defn),

                im_module_info          :: module_info,

                % Do the pragma foreign_decls for the module need writing,
                % yes if there are pragma foreign_procs being exported.
                im_write_foreign_header :: bool,

                % Vartypes and tvarset for the current pred.
                im_var_types            :: vartypes,
                im_tvarset              :: tvarset
            ).

:- pred init_intermod_info(module_info::in, intermod_info::out) is det.

init_intermod_info(ModuleInfo, IntermodInfo) :-
    set.init(Modules),
    set.init(Procs),
    set.init(ProcDecls),
    init_vartypes(VarTypes),
    varset.init(TVarSet),
    Instances = [],
    Types = [],
    IntermodInfo = intermod_info(Modules, Procs, ProcDecls, Instances, Types,
        ModuleInfo, no, VarTypes, TVarSet).

:- pred intermod_info_get_modules(intermod_info::in, set(module_name)::out)
    is det.
:- pred intermod_info_get_preds(intermod_info::in, set(pred_id)::out) is det.
:- pred intermod_info_get_pred_decls(intermod_info::in, set(pred_id)::out)
    is det.
:- pred intermod_info_get_instances(intermod_info::in,
    assoc_list(class_id, hlds_instance_defn)::out) is det.
:- pred intermod_info_get_types(intermod_info::in,
    assoc_list(type_ctor, hlds_type_defn)::out) is det.
:- pred intermod_info_get_module_info(intermod_info::in, module_info::out)
    is det.
:- pred intermod_info_get_write_foreign_header(intermod_info::in, bool::out)
    is det.
:- pred intermod_info_get_var_types(intermod_info::in, vartypes::out) is det.
:- pred intermod_info_get_tvarset(intermod_info::in, tvarset::out) is det.

:- pred intermod_info_set_modules(set(module_name)::in,
    intermod_info::in, intermod_info::out) is det.
:- pred intermod_info_set_preds(set(pred_id)::in,
    intermod_info::in, intermod_info::out) is det.
:- pred intermod_info_set_pred_decls(set(pred_id)::in,
    intermod_info::in, intermod_info::out) is det.
:- pred intermod_info_set_instances(
    assoc_list(class_id, hlds_instance_defn)::in,
    intermod_info::in, intermod_info::out) is det.
:- pred intermod_info_set_types(assoc_list(type_ctor, hlds_type_defn)::in,
    intermod_info::in, intermod_info::out) is det.
%:- pred intermod_info_set_insts(set(inst_id)::in,
%   intermod_info::in, intermod_info::out) is det.
:- pred intermod_info_set_module_info(module_info::in,
    intermod_info::in, intermod_info::out) is det.
:- pred intermod_info_set_write_header(intermod_info::in,
    intermod_info::out) is det.
:- pred intermod_info_set_var_types(vartypes::in, intermod_info::in,
    intermod_info::out) is det.
:- pred intermod_info_set_tvarset(tvarset::in,
    intermod_info::in, intermod_info::out) is det.

intermod_info_get_modules(Info, Info ^ im_modules).
intermod_info_get_preds(Info, Info ^ im_preds).
intermod_info_get_pred_decls(Info, Info ^ im_pred_decls).
intermod_info_get_instances(Info, Info ^ im_instances).
intermod_info_get_types(Info, Info ^ im_types).
intermod_info_get_module_info(Info, Info ^ im_module_info).
intermod_info_get_write_foreign_header(Info, Info ^ im_write_foreign_header).
intermod_info_get_var_types(Info, Info ^ im_var_types).
intermod_info_get_tvarset(Info, Info ^ im_tvarset).

intermod_info_set_modules(Modules, Info, Info ^ im_modules := Modules).
intermod_info_set_preds(Procs, Info, Info ^ im_preds := Procs).
intermod_info_set_pred_decls(ProcDecls, Info,
        Info ^ im_pred_decls := ProcDecls).
intermod_info_set_instances(Instances, Info, Info ^ im_instances := Instances).
intermod_info_set_types(Types, Info, Info ^ im_types := Types).
intermod_info_set_module_info(ModuleInfo, Info,
        Info ^ im_module_info := ModuleInfo).
intermod_info_set_write_header(Info, Info ^ im_write_foreign_header := yes).
intermod_info_set_var_types(VarTypes, Info, Info ^ im_var_types := VarTypes).
intermod_info_set_tvarset(TVarSet, Info, Info ^ im_tvarset := TVarSet).

%-----------------------------------------------------------------------------%

    % Make sure the labels of local preds needed by predicates in
    % the .opt file are exported, and inhibit dead proc elimination
    % on those preds.
    %
adjust_pred_import_status(!ModuleInfo) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    trace [io(!IO)] (
        maybe_write_string(VeryVerbose,
            "% Adjusting import status of predicates in the `.opt' file...",
            !IO)
    ),

    module_info_get_valid_predids(PredIds, !ModuleInfo),
    globals.lookup_int_option(Globals, intermod_inline_simple_threshold,
        Threshold),
    globals.lookup_bool_option(Globals, deforestation, Deforestation),
    globals.lookup_int_option(Globals, higher_order_size_limit,
        HigherOrderSizeLimit),
    some [!Info] (
        init_intermod_info(!.ModuleInfo, !:Info),
        gather_preds(PredIds, yes, Threshold, HigherOrderSizeLimit,
            Deforestation, !Info),
        gather_instances(!Info),
        gather_types(!Info),
        do_adjust_pred_import_status(!.Info, !ModuleInfo)
    ),
    trace [io(!IO)] (
        maybe_write_string(VeryVerbose, " done\n", !IO)
    ).

:- pred do_adjust_pred_import_status(intermod_info::in,
    module_info::in, module_info::out) is det.

do_adjust_pred_import_status(Info, !ModuleInfo) :-
    intermod_info_get_pred_decls(Info, PredDecls0),
    set.to_sorted_list(PredDecls0, PredDecls),
    set_list_of_preds_exported(PredDecls, !ModuleInfo),
    adjust_type_status(!ModuleInfo),
    adjust_class_status(!ModuleInfo),
    adjust_instance_status(!ModuleInfo).

:- pred adjust_type_status(module_info::in, module_info::out) is det.

adjust_type_status(!ModuleInfo) :-
    module_info_get_type_table(!.ModuleInfo, TypeTable0),
    map_foldl_over_type_ctor_defns(adjust_type_status_2, TypeTable0, TypeTable,
        !ModuleInfo),
    module_info_set_type_table(TypeTable, !ModuleInfo).

:- pred adjust_type_status_2(type_ctor::in,
    hlds_type_defn::in, hlds_type_defn::out,
    module_info::in, module_info::out) is det.

adjust_type_status_2(TypeCtor, TypeDefn0, TypeDefn, !ModuleInfo) :-
    module_info_get_name(!.ModuleInfo, ModuleName),
    ( should_write_type(ModuleName, TypeCtor, TypeDefn0) ->
        hlds_data.set_type_defn_status(status_exported, TypeDefn0, TypeDefn),
        fixup_special_preds(TypeCtor, !ModuleInfo)
    ;
        TypeDefn = TypeDefn0
    ).

:- pred fixup_special_preds((type_ctor)::in,
    module_info::in, module_info::out) is det.

fixup_special_preds(TypeCtor, ModuleInfo0, ModuleInfo) :-
    special_pred_list(SpecialPredList),
    module_info_get_special_pred_map(ModuleInfo0, SpecPredMap),
    list.filter_map((pred(SpecPredId::in, PredId::out) is semidet :-
            map.search(SpecPredMap, SpecPredId - TypeCtor, PredId)
        ), SpecialPredList, PredIds),
    set_list_of_preds_exported(PredIds, ModuleInfo0, ModuleInfo).

:- pred adjust_class_status(module_info::in, module_info::out) is det.

adjust_class_status(!ModuleInfo) :-
    module_info_get_class_table(!.ModuleInfo, Classes0),
    map.to_assoc_list(Classes0, ClassAL0),
    list.map_foldl(adjust_class_status_2, ClassAL0, ClassAL, !ModuleInfo),
    map.from_sorted_assoc_list(ClassAL, Classes),
    module_info_set_class_table(Classes, !ModuleInfo).

:- pred adjust_class_status_2(pair(class_id, hlds_class_defn)::in,
    pair(class_id, hlds_class_defn)::out,
    module_info::in, module_info::out) is det.

adjust_class_status_2(ClassId - ClassDefn0, ClassId - ClassDefn,
        !ModuleInfo) :-
    ( import_status_to_write(ClassDefn0 ^ class_status) ->
        ClassDefn = ClassDefn0 ^ class_status := status_exported,
        class_procs_to_pred_ids(ClassDefn ^ class_hlds_interface, PredIds),
        set_list_of_preds_exported(PredIds, !ModuleInfo)
    ;
        ClassDefn = ClassDefn0
    ).

:- pred class_procs_to_pred_ids(list(hlds_class_proc)::in, list(pred_id)::out)
    is det.

class_procs_to_pred_ids(ClassProcs, PredIds) :-
    list.map(
        (pred(ClassProc::in, PredId::out) is det :-
            ClassProc = hlds_class_proc(PredId, _)
        ),
        ClassProcs, PredIds0),
    list.sort_and_remove_dups(PredIds0, PredIds).

:- pred adjust_instance_status(module_info::in, module_info::out) is det.

adjust_instance_status(!ModuleInfo) :-
    module_info_get_instance_table(!.ModuleInfo, Instances0),
    map.to_assoc_list(Instances0, InstanceAL0),
    list.map_foldl(adjust_instance_status_2, InstanceAL0, InstanceAL,
        !ModuleInfo),
    map.from_sorted_assoc_list(InstanceAL, Instances),
    module_info_set_instance_table(Instances, !ModuleInfo).

:- pred adjust_instance_status_2(pair(class_id, list(hlds_instance_defn))::in,
    pair(class_id, list(hlds_instance_defn))::out,
    module_info::in, module_info::out) is det.

adjust_instance_status_2(ClassId - InstanceList0, ClassId - InstanceList,
        !ModuleInfo) :-
    list.map_foldl(adjust_instance_status_3, InstanceList0, InstanceList,
        !ModuleInfo).

:- pred adjust_instance_status_3(hlds_instance_defn::in,
    hlds_instance_defn::out, module_info::in, module_info::out) is det.

adjust_instance_status_3(Instance0, Instance, !ModuleInfo) :-
    Instance0 = hlds_instance_defn(InstanceModule, Status0, Context,
        Constraints, Types, OriginalTypes, Body,
        HLDSClassInterface, TVarSet, ConstraintProofs),
    ( import_status_to_write(Status0) ->
        Instance = hlds_instance_defn(InstanceModule, status_exported,
            Context, Constraints, Types, OriginalTypes, Body,
            HLDSClassInterface, TVarSet, ConstraintProofs),
        (
            HLDSClassInterface = yes(ClassInterface),
            class_procs_to_pred_ids(ClassInterface, PredIds),
            set_list_of_preds_exported(PredIds, !ModuleInfo)
        ;
            % This can happen if an instance has multiple
            % declarations, one of which is abstract.
            HLDSClassInterface = no
        )
    ;
        Instance = Instance0
    ).

:- pred set_list_of_preds_exported(list(pred_id)::in, module_info::in,
        module_info::out) is det.

set_list_of_preds_exported(PredIds, !ModuleInfo) :-
    module_info_get_preds(!.ModuleInfo, Preds0),
    set_list_of_preds_exported_2(PredIds, Preds0, Preds),
    module_info_set_preds(Preds, !ModuleInfo).

:- pred set_list_of_preds_exported_2(list(pred_id)::in,
    pred_table::in, pred_table::out) is det.

set_list_of_preds_exported_2([], !Preds).
set_list_of_preds_exported_2([PredId | PredIds], !Preds) :-
    map.lookup(!.Preds, PredId, PredInfo0),
    (
        pred_info_get_import_status(PredInfo0, Status),
        import_status_to_write(Status)
    ->
        (
            pred_info_get_origin(PredInfo0, Origin),
            Origin = origin_special_pred(spec_pred_unify - _)
        ->
            NewStatus = status_pseudo_exported
        ;
            Status = status_external(_)
        ->
            NewStatus = status_external(status_opt_exported)
        ;
            NewStatus = status_opt_exported
        ),
        pred_info_set_import_status(NewStatus, PredInfo0, PredInfo),
        map.det_update(PredId, PredInfo, !Preds)
    ;
        true
    ),
    set_list_of_preds_exported_2(PredIds, !Preds).

    % Should a declaration with the given status be written to the `.opt' file.
    %
:- pred import_status_to_write(import_status::in) is semidet.

import_status_to_write(Status) :-
    import_status_to_write(Status) = yes.

:- func import_status_to_write(import_status) = bool.

import_status_to_write(status_imported(_)) = no.
import_status_to_write(status_abstract_imported) = no.
import_status_to_write(status_pseudo_imported) = no.
import_status_to_write(status_opt_imported) = no.
import_status_to_write(status_exported) = no.
import_status_to_write(status_opt_exported) = yes.
import_status_to_write(status_abstract_exported) = yes.
import_status_to_write(status_pseudo_exported) = no.
import_status_to_write(status_exported_to_submodules) = yes.
import_status_to_write(status_local) = yes.
import_status_to_write(status_external(Status)) =
    bool.not(status_is_exported(Status)).

%-----------------------------------------------------------------------------%

    % Read in and process the optimization interfaces.
    %
grab_opt_files(Globals, !Module, FoundError, !IO) :-
    % Read in the .opt files for imported and ancestor modules.
    ModuleName = !.Module ^ mai_module_name,
    Ancestors0 = !.Module ^ mai_parent_deps,
    InterfaceDeps0 = !.Module ^ mai_int_deps,
    ImplementationDeps0 = !.Module ^ mai_impl_deps,
    OptFiles = list.sort_and_remove_dups(list.condense(
        [Ancestors0, InterfaceDeps0, ImplementationDeps0])),
    globals.lookup_bool_option(Globals, read_opt_files_transitively,
        Transitive),
    ModulesProcessed = set.insert(set.sorted_list_to_set(OptFiles),
        ModuleName),
    read_optimization_interfaces(Globals, Transitive, ModuleName, OptFiles,
        ModulesProcessed, cord.empty, OptItemsCord, [], OptSpecs, no, OptError,
        !IO),

    % Append the items to the current item list, using a `opt_imported'
    % pseudo-declaration to let make_hlds know the opt_imported stuff
    % is coming.
    %
    % XXX Using this mechanism to let make_hlds know this is a bad design.
    OptItems = cord.list(OptItemsCord),
    AddedItems = [make_pseudo_decl(md_opt_imported) | OptItems],
    module_and_imports_add_items(cord.from_list(AddedItems), !Module),
    module_and_imports_add_specs(OptSpecs, !Module),

    % Get the :- pragma unused_args(...) declarations created when writing
    % the .opt file for the current module. These are needed because we can
    % probably remove more arguments with intermod_unused_args, but the
    % interface for other modules must remain the same.
    %
    % Similarly for the  :- pragma structure_reuse(...) declarations. With more
    % information available when making the target code than when writing the
    % `.opt' file, it can turn out that procedure which seemed to have
    % condition reuse actually has none. But we have to maintain the interface
    % for modules that use the conditional reuse information from the `.opt'
    % file.
    globals.lookup_bool_option(Globals, intermod_unused_args, UnusedArgs),
    globals.lookup_bool_option(Globals, structure_reuse_analysis,
        StructureReuse),
    (
        ( UnusedArgs = yes
        ; StructureReuse = yes
        )
    ->
        read_optimization_interfaces(Globals, no, ModuleName, [ModuleName],
            set.init, cord.empty, LocalItemsCord, [], LocalSpecs,
            no, UA_SR_Error, !IO),
        KeepPragma = (pred(Item::in) is semidet :-
            Item = item_pragma(ItemPragma),
            ItemPragma = item_pragma_info(_, Pragma, _, _),
            (
                UnusedArgs = yes,
                Pragma = pragma_unused_args(_)
            ;
                StructureReuse = yes,
                Pragma = pragma_structure_reuse(_)
            )
        ),
        cord.filter(KeepPragma, LocalItemsCord, PragmaItemsCord),
        module_and_imports_add_items(PragmaItemsCord, !Module),
        module_and_imports_add_specs(LocalSpecs, !Module)
    ;
        UA_SR_Error = no
    ),

    % Read .int0 files required by the `.opt' files.
    Int0Files = list.delete_all(
        list.condense(list.map(get_ancestors, OptFiles)), ModuleName),
    process_module_private_interfaces(Globals, ReadModules, Int0Files,
        make_pseudo_decl(md_opt_imported),
        make_pseudo_decl(md_opt_imported),
        [], AncestorImports1,
        [], AncestorImports2, !Module, !IO),

    % Figure out which .int files are needed by the .opt files
    get_dependencies(OptItems, NewImportDeps0, NewUseDeps0),
    get_implicit_dependencies(OptItems, Globals,
        NewImplicitImportDeps0, NewImplicitUseDeps0),
    NewDeps = list.sort_and_remove_dups(list.condense(
        [NewImportDeps0, NewUseDeps0,
        NewImplicitImportDeps0, NewImplicitUseDeps0,
        AncestorImports1, AncestorImports2])),

    % Read in the .int, and .int2 files needed by the .opt files.
    map.init(ReadModules),
    process_module_long_interfaces(Globals, ReadModules, must_be_qualified,
        NewDeps, ".int",
        make_pseudo_decl(md_opt_imported), make_pseudo_decl(md_opt_imported),
        [], NewIndirectDeps, [], NewImplIndirectDeps, !Module, !IO),
    process_module_short_interfaces_and_impls_transitively(Globals,
        ReadModules, NewIndirectDeps ++ NewImplIndirectDeps, ".int2",
        make_pseudo_decl(md_opt_imported), make_pseudo_decl(md_opt_imported),
        !Module, !IO),

    % Figure out whether anything went wrong.
    % XXX We should try to put all the relevant error indications into !Module,
    % and let our caller figure out what to do with them.
    module_and_imports_get_results(!.Module, _Items, _Specs, FoundError0),
    (
        ( FoundError0 \= no_module_errors
        ; OptError = yes
        ; UA_SR_Error = yes
        )
    ->
        FoundError = yes
    ;
        FoundError = no
    ).

:- pred read_optimization_interfaces(globals::in, bool::in, module_name::in,
    list(module_name)::in, set(module_name)::in,
    cord(item)::in, cord(item)::out,
    list(error_spec)::in, list(error_spec)::out,
    bool::in, bool::out, io::di, io::uo) is det.

read_optimization_interfaces(_, _, _, [], _, !Items, !Specs, !Error, !IO).
read_optimization_interfaces(Globals, Transitive, ModuleName,
        [ModuleToRead | ModulesToRead], ModulesProcessed0,
        !Items, !Specs, !Error, !IO) :-
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    maybe_write_out_errors_no_module(VeryVerbose, Globals, !Specs, !IO),
    maybe_write_string(VeryVerbose,
        "% Reading optimization interface for module", !IO),
    maybe_write_string(VeryVerbose, " `", !IO),
    ModuleToReadString = sym_name_to_string(ModuleToRead),
    maybe_write_string(VeryVerbose, ModuleToReadString, !IO),
    maybe_write_string(VeryVerbose, "'...\n", !IO),
    maybe_flush_output(VeryVerbose, !IO),

    module_name_to_search_file_name(Globals, ModuleToRead, ".opt", FileName,
        !IO),
    actually_read_opt_file(Globals, FileName, ModuleToRead, OptItems, OptSpecs,
        OptError, !IO),
    update_error_status(Globals, opt_file, FileName,
        OptSpecs, !Specs, OptError, !Error),
    !:Items = !.Items ++ cord.from_list(OptItems),
    maybe_write_out_errors_no_module(VeryVerbose, Globals, !Specs, !IO),
    maybe_write_string(VeryVerbose, "% done.\n", !IO),

    (
        Transitive = yes,
        get_dependencies(OptItems, NewImportDeps0, NewUseDeps0),
        get_implicit_dependencies(OptItems, Globals,
            NewImplicitImportDeps0, NewImplicitUseDeps0),
        NewDeps0 = list.condense([NewImportDeps0, NewUseDeps0,
            NewImplicitImportDeps0, NewImplicitUseDeps0]),
        set.list_to_set(NewDeps0, NewDepsSet0),
        set.difference(NewDepsSet0, ModulesProcessed0, NewDepsSet),
        set.union(ModulesProcessed0, NewDepsSet, ModulesProcessed),
        set.to_sorted_list(NewDepsSet, NewDeps)
    ;
        Transitive = no,
        ModulesProcessed = ModulesProcessed0,
        NewDeps = []
    ),
    read_optimization_interfaces(Globals, Transitive, ModuleName,
        NewDeps ++ ModulesToRead, ModulesProcessed,
        !Items, !Specs, !Error, !IO).

update_error_status(_Globals, FileType, FileName,
        ModuleSpecs, !Specs, ModuleError, !Error) :-
    (
        ModuleError = no_module_errors
        % OptSpecs contains no errors. I (zs) don't know whether it could
        % contain any warnings or informational messages, but if it could,
        % we should add those error_specs to !Specs. Not doing so preserves
        % old behavior.
    ;
        ModuleError = some_module_errors,
        !:Specs = ModuleSpecs ++ !.Specs,
        !:Error = yes
    ;
        ModuleError = fatal_module_errors,
        % We get here if we couldn't find and/or open the file.
        % ModuleSpecs will already contain an error_severity error_spec
        % about that, with more details than the message we generate below,
        % but the test case hard_coded/intermod_unused_args insists on
        % there being no error, only a warning, and on the text below.
        % That is why we do not add ModuleSpecs to !Specs here.
        %
        % I (zs) don't know whether adding a version of ModuleSpecs (possibly
        % with downgraded severity) to !Specs would be a better idea.
        (
            FileType = opt_file,
            WarningOption = warn_missing_opt_files
        ;
            FileType = trans_opt_file,
            WarningOption = warn_missing_trans_opt_files
        ),
        Severity =
            severity_conditional(WarningOption, yes, severity_warning, no),
        Pieces = [option_is_set(WarningOption, yes,
            [always([words("Warning: cannot open"), quote(FileName),
                suffix("."), nl])])],
        Msg = error_msg(no, treat_as_first, 0, Pieces),
        Spec = error_spec(Severity, phase_read_files, [Msg]),
        !:Specs = [Spec | !.Specs]
    ).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.intermod.
%-----------------------------------------------------------------------------%
