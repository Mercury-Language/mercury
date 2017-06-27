%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: intermod.m.
% Main author: stayl (the original intermod.m).
% Main author: crs (the original trans_opt.m).
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
% Note that predicates which call predicates that do not have mode or
% determinism declarations do not have clauses exported, since this would
% require running mode analysis and determinism analysis before writing the
% .opt file, significantly increasing compile time for a very small gain.
%
% This module also contains predicates to adjust the import status
% of local predicates which are exported for intermodule optimization.
%
%---------------------------------------------------------------------------%
%
% Transitive intermodule optimization allows the compiler to do intermodule
% optimization that depends on other .trans_opt files. In comparison to .opt
% files, .trans_opt files allow much more accurate optimization to occur,
% but at the cost of an increased number of compilations required. The fact
% that a .trans_opt file may depend on other .trans_opt files introduces
% the possibility of circular dependencies occurring. These circular
% dependencies would occur if the data in A.trans_opt depended on the data
% in B.trans_opt being correct, and vice versa.
%
% We use the following system to ensure that circular dependencies cannot
% occur:
%
%   When mmake <module>.depend is run, mmc calculates a suitable ordering.
%   This ordering is then used to create each of the .d files. This allows
%   make to ensure that all necessary trans_opt files are up to date before
%   creating any other trans_opt files. This same information is used by mmc
%   to decide which trans_opt files may be imported when creating another
%   .trans_opt file. By observing the ordering decided upon when mmake
%   module.depend was run, any circularities which may have been created
%   are avoided.
%
% This module writes out the interface for transitive intermodule optimization.
% The .trans_opt file includes:
%   :- pragma termination_info declarations for all exported preds
%   :- pragma exceptions declarations for all exported preds
%   :- pragma trailing_info declarations for all exported preds.
%
% All these items should be module qualified.
% Constructors should be explicitly type qualified.
%
% Note that the .trans_opt file does not (yet) include clauses, `pragma
% foreign_proc' declarations, or any of the other information that would be
% needed for inlining or other optimizations. Currently it is used only for
% recording the results of program analyses, such as termination analysis,
% exception and trail usage analysis.
%
%---------------------------------------------------------------------------%

:- module transform_hlds.intermod.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.prog_item.

:- import_module io.
:- import_module set.

%---------------------------------------------------------------------------%

    % Open the file "<module-name>.opt.tmp", and write out the declarations
    % and clauses for intermodule optimization.
    %
    % Although this predicate creates the .opt.tmp file, it does not
    % necessarily create it in its final form. Later compiler passes
    % may append to this file using the append_analysis_pragmas_to_opt_file
    % predicate below.
    % XXX This is not an elegant arrangement.
    %
    % Update_interface and touch_interface_datestamp are called from
    % mercury_compile.m, since they must be called after the last time
    % anything is appended to the .opt.tmp file.
    %
:- pred write_initial_opt_file(module_info::in, module_info::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% This predicate appends the results of program analyses to .opt files
% in the form of pragma items.
% It is called from mercury_compile_middle_passes.m.
%
% All the analysis results we write out come from the proc_infos of the
% procedures to which they apply, with one exception: the results of
% unused args analysis. This is because we detect unused arguments
% in procedures so we can optimize those arguments away. This makes storing
% information about unused arguments in the proc_infos of the procedures
% to which they apply somewhat tricky, since that procedure may,
% immediately after the unused args are discovered, be transformed to
% eliminate the unused arguments, in which case the recorded information
% becomes dangling; it applies to a procedure that no longer exists.
% This should *not* happen to exported procedures, which are the only
% ones we want to write unused arg pragmas about to an optimization file,
% since other modules compiled without the right flags would still call
% the unoptimized original procedure. Nevertheless, to avoid storing
% analysis results in proc_infos that may apply only to a no-longer-existing
% version of the procedure, we pass the info in unused args pragmas
% to append_unused_arg_pragmas_to_opt_file separately.
%

:- pred append_analysis_pragmas_to_opt_file(module_info::in,
    set(pragma_info_unused_args)::in, io::di, io::uo) is det.

%---------------------%

:- type should_write_for
    --->    for_analysis_framework
    ;       for_pragma.

:- type maybe_should_write
    --->    should_not_write
    ;       should_write.

:- pred should_write_exception_info(module_info::in, pred_id::in, proc_id::in,
    pred_info::in, should_write_for::in, maybe_should_write::out) is det.

:- pred should_write_trailing_info(module_info::in, pred_id::in, proc_id::in,
    pred_info::in, should_write_for::in, maybe_should_write::out) is det.

:- pred should_write_mm_tabling_info(module_info::in, pred_id::in, proc_id::in,
    pred_info::in, should_write_for::in, maybe_should_write::out) is det.

:- pred should_write_reuse_info(module_info::in, pred_id::in, proc_id::in,
    pred_info::in, should_write_for::in, maybe_should_write::out) is det.

:- pred should_write_sharing_info(module_info::in, pred_id::in, proc_id::in,
    pred_info::in, should_write_for::in, maybe_should_write::out) is det.

%---------------------------------------------------------------------------%

    % Open the file "<module-name>.trans_opt.tmp", and write out the
    % declarations.
    %
:- pred write_trans_opt_file(module_info::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % Change the status of opt-exported entities (predicates, types,
    % classes and instances) to opt-exported. This affects how the rest
    % of the compiler treats these entities. For example, the entry labels
    % at the starts of the C code fragments we generate for an opt-exported
    % local predicate needs to be exported from the .c file, and opt-exported
    % procedures should not be touched by dead proc elimination.
    %
    % The reason why we have a separate pass for this, instead of changing
    % the status of an item to reflect the fact that it is opt-exported
    % at the same time as we decide to opt-export it, is that the decision
    % to opt-export e.g. a procedure takes place inside invocations of
    % mmc --make-opt-int, but we also need the same status updates
    % in invocations of mmc that generate target language code.
    %
:- pred maybe_opt_export_entities(module_info::in, module_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.foreign.
:- import_module check_hlds.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.goal_form.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_module.
:- import_module hlds.hlds_out.hlds_out_pred.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.pred_table.
:- import_module hlds.special_pred.
:- import_module hlds.status.
:- import_module hlds.vartypes.
:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.lp_rational.
:- import_module libs.options.
:- import_module libs.polyhedron.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.file_names.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.parse_tree_out.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_pragma.
:- import_module parse_tree.parse_tree_out_pred_decl.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.
:- import_module transform_hlds.inlining.
:- import_module transform_hlds.term_constr_data.
:- import_module transform_hlds.term_constr_main_types.
:- import_module transform_hlds.term_constr_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module multi_map.
:- import_module pair.
:- import_module require.
:- import_module std_util.
:- import_module string.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

write_initial_opt_file(!ModuleInfo, !IO) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    module_info_get_name(!.ModuleInfo, ModuleName),
    module_name_to_file_name(Globals, do_create_dirs, ".opt.tmp",
        ModuleName, TmpName, !IO),
    io.open_output(TmpName, Result, !IO),
    (
        Result = error(Err),
        Msg = io.error_message(Err),
        io.write_string(Msg, !IO),
        io.set_exit_status(1, !IO)
    ;
        Result = ok(FileStream),
        io.set_output_stream(FileStream, OutputStream, !IO),
        module_info_get_valid_pred_ids(!.ModuleInfo, RealPredIds),
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
            gather_opt_export_preds(PredIds, yes, Threshold,
                HigherOrderSizeLimit, Deforestation, !IntermodInfo),
            gather_opt_export_instances(!IntermodInfo),
            gather_opt_export_types(!IntermodInfo),
            write_intermod_info(!.IntermodInfo, !IO),
            intermod_info_get_module_info(!.IntermodInfo, !:ModuleInfo),
            io.set_output_stream(OutputStream, _, !IO),
            io.close_output(FileStream, !IO),
            do_maybe_opt_export_entities(!.IntermodInfo, !ModuleInfo)
        )
    ).

%---------------------------------------------------------------------------%
%
% Predicates to gather items to output to .opt file.
%

:- pred gather_opt_export_preds(list(pred_id)::in, bool::in, int::in, int::in,
    bool::in, intermod_info::in, intermod_info::out) is det.

gather_opt_export_preds(AllPredIds, CollectTypes,
        InlineThreshold, HigherOrderSizeLimit, Deforestation, !IntermodInfo) :-
    % First gather exported preds.
    ProcessLocalPreds = no,
    gather_opt_export_preds_in_list(AllPredIds, ProcessLocalPreds,
        CollectTypes, InlineThreshold, HigherOrderSizeLimit, Deforestation,
        !IntermodInfo),

    % Then gather preds used by exported preds (recursively).
    set.init(ExtraExportedPreds0),
    gather_opt_export_preds_fixpoint(ExtraExportedPreds0, CollectTypes,
        InlineThreshold, HigherOrderSizeLimit, Deforestation, !IntermodInfo).

:- pred gather_opt_export_preds_fixpoint(set(pred_id)::in, bool::in,
    int::in, int::in, bool::in, intermod_info::in, intermod_info::out) is det.

gather_opt_export_preds_fixpoint(ExtraExportedPreds0, CollectTypes,
        InlineThreshold, HigherOrderSizeLimit, Deforestation, !IntermodInfo) :-
    intermod_info_get_pred_decls(!.IntermodInfo, ExtraExportedPreds),
    NewlyExportedPreds = set.to_sorted_list(
        set.difference(ExtraExportedPreds, ExtraExportedPreds0)),
    (
        NewlyExportedPreds = []
    ;
        NewlyExportedPreds = [_ | _],
        ProcessLocalPreds = yes,
        gather_opt_export_preds_in_list(NewlyExportedPreds, ProcessLocalPreds,
            CollectTypes, InlineThreshold, HigherOrderSizeLimit, Deforestation,
            !IntermodInfo),
        gather_opt_export_preds_fixpoint(ExtraExportedPreds, CollectTypes,
            InlineThreshold, HigherOrderSizeLimit, Deforestation,
            !IntermodInfo)
    ).

:- pred gather_opt_export_preds_in_list(list(pred_id)::in, bool::in, bool::in,
    int::in, int::in, bool::in, intermod_info::in, intermod_info::out) is det.

gather_opt_export_preds_in_list([], _, _, _, _, _, !IntermodInfo).
gather_opt_export_preds_in_list([PredId | PredIds], ProcessLocalPreds,
        CollectTypes, InlineThreshold, HigherOrderSizeLimit, Deforestation,
        !IntermodInfo) :-
    intermod_info_get_module_info(!.IntermodInfo, ModuleInfo0),
    module_info_get_preds(ModuleInfo0, PredTable0),
    map.lookup(PredTable0, PredId, PredInfo0),
    module_info_get_type_spec_info(ModuleInfo0, TypeSpecInfo),
    TypeSpecInfo = type_spec_info(_, TypeSpecForcePreds, _, _),
    pred_info_get_clauses_info(PredInfo0, ClausesInfo0),
    ( if
        clauses_info_get_explicit_vartypes(ClausesInfo0, ExplicitVarTypes),
        vartypes_is_empty(ExplicitVarTypes),
        should_opt_export_pred(ModuleInfo0, PredId, PredInfo0,
            ProcessLocalPreds, TypeSpecForcePreds, InlineThreshold,
            HigherOrderSizeLimit, Deforestation)
    then
        SavedIntermodInfo = !.IntermodInfo,
        % Write a declaration to the `.opt' file for
        % `exported_to_submodules' predicates.
        intermod_add_proc(PredId, DoWrite0, !IntermodInfo),
        clauses_info_get_clauses_rep(ClausesInfo0, ClausesRep0, ItemNumbers0),
        (
            DoWrite0 = yes,
            clauses_info_get_vartypes(ClausesInfo0, VarTypes),
            pred_info_get_typevarset(PredInfo0, TVarSet),
            intermod_info_set_var_types(VarTypes, !IntermodInfo),
            intermod_info_set_tvarset(TVarSet, !IntermodInfo),
            get_clause_list_for_replacement(ClausesRep0, Clauses0),
            gather_entities_to_opt_export_in_clauses(Clauses0, Clauses,
                DoWrite, !IntermodInfo),
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
            intermod_info_get_preds(!.IntermodInfo, Preds0),
            ( if pred_info_pragma_goal_type(PredInfo) then
                % pragma foreign_decls must be written since their contents
                % could be used by pragma foreign_procs.
                intermod_info_set_write_header(!IntermodInfo)
            else
                true
            ),
            set.insert(PredId, Preds0, Preds),
            intermod_info_set_preds(Preds, !IntermodInfo),
            intermod_info_set_module_info(ModuleInfo, !IntermodInfo)
        ;
            DoWrite = no,
            % Remove any items added for the clauses for this predicate.
            !:IntermodInfo = SavedIntermodInfo
        )
    else
        true
    ),
    gather_opt_export_preds_in_list(PredIds, ProcessLocalPreds, CollectTypes,
        InlineThreshold, HigherOrderSizeLimit, Deforestation, !IntermodInfo).

:- pred should_opt_export_pred(module_info::in, pred_id::in, pred_info::in,
    bool::in, set(pred_id)::in, int::in, int::in, bool::in) is semidet.

should_opt_export_pred(ModuleInfo, PredId, PredInfo, ProcessLocalPreds,
        TypeSpecForcePreds, InlineThreshold, HigherOrderSizeLimit,
        Deforestation) :-
    (
        ProcessLocalPreds = no,
        ( pred_info_is_exported(PredInfo)
        ; pred_info_is_exported_to_submodules(PredInfo)
        )
    ;
        ProcessLocalPreds = yes,
        pred_info_get_status(PredInfo, pred_status(status_local))
    ),
    (
        % Allow all promises to be opt-exported.
        % (may_opt_export_pred should succeed for all promises.)
        pred_info_is_promise(PredInfo, _)
    ;
        may_opt_export_pred(PredId, PredInfo, TypeSpecForcePreds),
        opt_exporting_pred_is_likely_worthwhile(ModuleInfo, PredId, PredInfo,
            InlineThreshold, HigherOrderSizeLimit, Deforestation)
    ).

:- pred opt_exporting_pred_is_likely_worthwhile(module_info::in,
    pred_id::in, pred_info::in, int::in, int::in, bool::in) is semidet.

opt_exporting_pred_is_likely_worthwhile(ModuleInfo, PredId, PredInfo,
        InlineThreshold, HigherOrderSizeLimit, Deforestation) :-
    pred_info_get_clauses_info(PredInfo, ClauseInfo),
    clauses_info_get_clauses_rep(ClauseInfo, ClausesRep, _ItemNumbers),
    get_clause_list_maybe_repeated(ClausesRep, Clauses),
    % At this point, the goal size includes some dummy unifications
    % HeadVar1 = X, HeadVar2 = Y, etc. which will be optimized away
    % later. To account for this, we add the arity to the size thresholds.
    Arity = pred_info_orig_arity(PredInfo),
    (
        inlining.is_simple_clause_list(Clauses, InlineThreshold + Arity)
    ;
        pred_info_requested_inlining(PredInfo)
    ;
        % Mutable access preds should always be included in .opt files.
        pred_info_get_markers(PredInfo, Markers),
        check_marker(Markers, marker_mutable_access_pred)
    ;
        pred_has_a_higher_order_input_arg(ModuleInfo, PredInfo),
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
    ).

:- pred may_opt_export_pred(pred_id::in, pred_info::in, set(pred_id)::in)
    is semidet. 

may_opt_export_pred(PredId, PredInfo, TypeSpecForcePreds) :-
    % Predicates with `class_method' markers contain class_method_call
    % goals which cannot be written to `.opt' files (they cannot be read
    % back in). They will be recreated in the importing module.
    pred_info_get_markers(PredInfo, Markers),
    not check_marker(Markers, marker_class_method),
    not check_marker(Markers, marker_class_instance_method),

    % Don't write stub clauses to `.opt' files.
    not check_marker(Markers, marker_stub),

    % Don't export builtins since they will be recreated in the
    % importing module anyway.
    not is_unify_or_compare_pred(PredInfo),
    not pred_info_is_builtin(PredInfo),

    % These will be recreated in the importing module.
    not set.member(PredId, TypeSpecForcePreds),

    % Don't export non-inlinable predicates.
    not check_marker(Markers, marker_user_marked_no_inline),

    % Don't export tabled predicates since they are not inlinable.
    pred_info_get_proc_table(PredInfo, ProcTable),
    map.values(ProcTable, ProcInfos),
    list.all_true(proc_eval_method_is_normal, ProcInfos).

:- pred proc_eval_method_is_normal(proc_info::in) is semidet.

proc_eval_method_is_normal(ProcInfo) :-
    proc_info_get_eval_method(ProcInfo, eval_normal).

:- pred gather_entities_to_opt_export_in_clauses(
    list(clause)::in, list(clause)::out,
    bool::out, intermod_info::in, intermod_info::out) is det.

gather_entities_to_opt_export_in_clauses([], [], yes, !IntermodInfo).
gather_entities_to_opt_export_in_clauses([Clause0 | Clauses0],
        [Clause | Clauses], DoWrite, !IntermodInfo) :-
    Goal0 = Clause0 ^ clause_body,
    gather_entities_to_opt_export_in_goal(Goal0, Goal,
        DoWrite1, !IntermodInfo),
    Clause = Clause0 ^ clause_body := Goal,
    (
        DoWrite1 = yes,
        gather_entities_to_opt_export_in_clauses(Clauses0, Clauses,
            DoWrite, !IntermodInfo)
    ;
        DoWrite1 = no,
        Clauses = Clauses0,
        DoWrite = no
    ).

:- pred pred_has_a_higher_order_input_arg(module_info::in, pred_info::in)
    is semidet.

pred_has_a_higher_order_input_arg(ModuleInfo, PredInfo) :-
    pred_info_get_proc_table(PredInfo, ProcTable),
    map.values(ProcTable, ProcInfos),
    list.find_first_match(proc_has_a_higher_order_input_arg(ModuleInfo),
        ProcInfos, _FirstProcInfoWithHoInput).

:- pred proc_has_a_higher_order_input_arg(module_info::in, proc_info::in)
    is semidet.

proc_has_a_higher_order_input_arg(ModuleInfo, ProcInfo) :-
    proc_info_get_headvars(ProcInfo, HeadVars),
    proc_info_get_argmodes(ProcInfo, ArgModes),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    some_input_arg_is_higher_order(ModuleInfo, VarTypes, HeadVars, ArgModes).

:- pred some_input_arg_is_higher_order(module_info::in, vartypes::in,
    list(prog_var)::in, list(mer_mode)::in) is semidet.

some_input_arg_is_higher_order(ModuleInfo, VarTypes,
        [HeadVar | HeadVars], [ArgMode | ArgModes]) :-
    ( if
        mode_is_input(ModuleInfo, ArgMode),
        lookup_var_type(VarTypes, HeadVar, Type),
        classify_type(ModuleInfo, Type) = ctor_cat_higher_order
    then
        true
    else
        some_input_arg_is_higher_order(ModuleInfo, VarTypes,
            HeadVars, ArgModes)
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
:- pred gather_entities_to_opt_export_in_goal(hlds_goal::in, hlds_goal::out,
    bool::out, intermod_info::in, intermod_info::out) is det.

gather_entities_to_opt_export_in_goal(Goal0, Goal, DoWrite, !IntermodInfo) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo),
    gather_entities_to_opt_export_in_goal_expr(GoalExpr0, GoalExpr,
        DoWrite, !IntermodInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred gather_entities_to_opt_export_in_goal_expr(
    hlds_goal_expr::in, hlds_goal_expr::out, bool::out,
    intermod_info::in, intermod_info::out) is det.

gather_entities_to_opt_export_in_goal_expr(GoalExpr0, GoalExpr,
        DoWrite, !IntermodInfo) :-
    (
        GoalExpr0 = unify(LVar, RHS0, Mode, Kind, UnifyContext),
        % Export declarations for preds used in higher order pred constants
        % or function calls.
        module_qualify_unify_rhs(RHS0, RHS, DoWrite, !IntermodInfo),
        GoalExpr = unify(LVar, RHS, Mode, Kind, UnifyContext)
    ;
        GoalExpr0 = plain_call(PredId, _, _, _, _, _),
        % Ensure that the called predicate will be exported.
        intermod_add_proc(PredId, DoWrite, !IntermodInfo),
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
        gather_entities_to_opt_export_in_goals(Goals0, Goals,
            DoWrite, !IntermodInfo),
        GoalExpr = conj(ConjType, Goals)
    ;
        GoalExpr0 = disj(Goals0),
        gather_entities_to_opt_export_in_goals(Goals0, Goals,
            DoWrite, !IntermodInfo),
        GoalExpr = disj(Goals)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        gather_entities_to_opt_export_in_cases(Cases0, Cases,
            DoWrite, !IntermodInfo),
        GoalExpr = switch(Var, CanFail, Cases)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        gather_entities_to_opt_export_in_goal(Cond0, Cond,
            DoWrite1, !IntermodInfo),
        gather_entities_to_opt_export_in_goal(Then0, Then,
            DoWrite2, !IntermodInfo),
        gather_entities_to_opt_export_in_goal(Else0, Else,
            DoWrite3, !IntermodInfo),
        bool.and_list([DoWrite1, DoWrite2, DoWrite3], DoWrite),
        GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ;
        GoalExpr0 = negation(SubGoal0),
        gather_entities_to_opt_export_in_goal(SubGoal0, SubGoal,
            DoWrite, !IntermodInfo),
        GoalExpr = negation(SubGoal)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        % Mode analysis hasn't been run yet, so we don't know yet whether
        % from_ground_term_construct scopes actually satisfy their invariants,
        % specifically the invariant that say they contain no calls or
        % higher-order constants. We therefore cannot special-case them here.
        gather_entities_to_opt_export_in_goal(SubGoal0, SubGoal,
            DoWrite, !IntermodInfo),
        GoalExpr = scope(Reason, SubGoal)
    ;
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal0, OrElseGoals0, OrElseInners),
            gather_entities_to_opt_export_in_goal(MainGoal0, MainGoal,
                DoWriteMain, !IntermodInfo),
            gather_entities_to_opt_export_in_goals(OrElseGoals0, OrElseGoals,
                DoWriteOrElse, !IntermodInfo),
            bool.and(DoWriteMain, DoWriteOrElse, DoWrite),
            ShortHand = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal, OrElseGoals, OrElseInners)
        ;
            ShortHand0 = try_goal(_MaybeIO, _ResultVar, _SubGoal0),
            % hlds_out_goal.m does not write out `try' goals properly.
            DoWrite = no,
            ShortHand = ShortHand0
        ;
            ShortHand0 = bi_implication(_, _),
            % These should have been expanded out by now.
            unexpected($module, $pred, "bi_implication")
        ),
        GoalExpr = shorthand(ShortHand)
    ).

:- pred gather_entities_to_opt_export_in_goals(hlds_goals::in, hlds_goals::out,
    bool::out, intermod_info::in, intermod_info::out) is det.

gather_entities_to_opt_export_in_goals([], [], yes, !IntermodInfo).
gather_entities_to_opt_export_in_goals([Goal0 | Goals0], [Goal | Goals],
        !:DoWrite, !IntermodInfo) :-
    gather_entities_to_opt_export_in_goal(Goal0, Goal,
        !:DoWrite, !IntermodInfo),
    (
        !.DoWrite = yes,
        gather_entities_to_opt_export_in_goals(Goals0, Goals, !:DoWrite,
            !IntermodInfo)
    ;
        !.DoWrite = no,
        Goals = Goals0
    ).

:- pred gather_entities_to_opt_export_in_cases(list(case)::in, list(case)::out,
    bool::out, intermod_info::in, intermod_info::out) is det.

gather_entities_to_opt_export_in_cases([], [], yes, !IntermodInfo).
gather_entities_to_opt_export_in_cases([Case0 | Cases0], [Case | Cases],
        !:DoWrite, !IntermodInfo) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    gather_entities_to_opt_export_in_goal(Goal0, Goal,
        !:DoWrite, !IntermodInfo),
    Case = case(MainConsId, OtherConsIds, Goal),
    (
        !.DoWrite = yes,
        gather_entities_to_opt_export_in_cases(Cases0, Cases,
            !:DoWrite, !IntermodInfo)
    ;
        !.DoWrite = no,
        Cases = Cases0
    ).

    % intermod_add_proc/4 tries to do what ever is necessary to ensure that the
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
:- pred intermod_add_proc(pred_id::in, bool::out,
    intermod_info::in, intermod_info::out) is det.

intermod_add_proc(PredId, DoWrite, !IntermodInfo) :-
    ( if PredId = invalid_pred_id then
        % This will happen for type class instance methods defined using
        % the clause syntax. Currently we cannot handle intermodule
        % optimization of those.
        DoWrite = no
    else
        intermod_do_add_proc(PredId, DoWrite, !IntermodInfo)
    ).

:- pred intermod_do_add_proc(pred_id::in, bool::out,
    intermod_info::in, intermod_info::out) is det.

intermod_do_add_proc(PredId, DoWrite, !IntermodInfo) :-
    intermod_info_get_module_info(!.IntermodInfo, ModuleInfo),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_status(PredInfo, PredStatus),
    ProcIds = pred_info_procids(PredInfo),
    pred_info_get_markers(PredInfo, Markers),
    ( if
        % Calling compiler-generated procedures is fine; we don't need
        % to output declarations for them to the `.opt' file, since they
        % will be recreated every time anyway. We don't want declarations
        % for predicates representing promises either.

        ( is_unify_or_compare_pred(PredInfo)
        ; pred_info_is_promise(PredInfo, _)
        )
    then
        DoWrite = yes
    else if
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
            pred_info_get_proc_table(PredInfo, Procs),
            list.member(ProcId, ProcIds),
            map.lookup(Procs, ProcId, ProcInfo),
            proc_info_get_declared_determinism(ProcInfo, no)
        )
    then
        DoWrite = no
    else if
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
    then
        DoWrite = no
    else if
        % If a pred whose code we are going to put in the .opt file calls
        % a predicate which is exported, then we do not need to do anything
        % special.

        (
            PredStatus = pred_status(status_exported)
        ;
            PredStatus = pred_status(status_external(OldExternalStatus)),
            old_status_is_exported(OldExternalStatus) = yes
        )
    then
        DoWrite = yes
    else if
        % Declarations for class methods will be recreated from the class
        % declaration in the `.opt' file. Declarations for local classes
        % are always written to the `.opt' file.

        pred_info_get_markers(PredInfo, Markers),
        check_marker(Markers, marker_class_method)
    then
        DoWrite = yes
    else if
        % If a pred whose code we are going to put in the `.opt' file calls
        % a predicate which is local to that module, then we need to put
        % the declaration for the called predicate in the `.opt' file.

        pred_status_to_write(PredStatus) = yes
    then
        DoWrite = yes,
        intermod_info_get_pred_decls(!.IntermodInfo, PredDecls0),
        set.insert(PredId, PredDecls0, PredDecls),
        intermod_info_set_pred_decls(PredDecls, !IntermodInfo)
    else if
        ( PredStatus = pred_status(status_imported(_))
        ; PredStatus = pred_status(status_opt_imported)
        )
    then
        % Imported pred - add import for module.

        DoWrite = yes,
        PredModule = pred_info_module(PredInfo),
        intermod_info_get_modules(!.IntermodInfo, Modules0),
        set.insert(PredModule, Modules0, Modules),
        intermod_info_set_modules(Modules, !IntermodInfo)
    else
        unexpected($module, $pred, "unexpected status")
    ).

    % Resolve overloading and module qualify everything in a unify_rhs.
    % Fully module-qualify the right-hand-side of a unification.
    % For function calls and higher-order terms, call add_proc
    % so that the predicate or function will be exported if necessary.
    %
:- pred module_qualify_unify_rhs(unify_rhs::in, unify_rhs::out, bool::out,
    intermod_info::in, intermod_info::out) is det.

module_qualify_unify_rhs(RHS0, RHS, DoWrite, !IntermodInfo) :-
    (
        RHS0 = rhs_var(_),
        RHS = RHS0,
        DoWrite = yes
    ;
        RHS0 = rhs_lambda_goal(Purity, HOGroundness, PorF, EvalMethod,
            NonLocals, QuantVars, Modes, Detism, Goal0),
        gather_entities_to_opt_export_in_goal(Goal0, Goal,
            DoWrite, !IntermodInfo),
        RHS = rhs_lambda_goal(Purity, HOGroundness, PorF, EvalMethod,
            NonLocals, QuantVars, Modes, Detism, Goal)
    ;
        RHS0 = rhs_functor(Functor, _Exist, _Vars),
        RHS = RHS0,
        % Is this a higher-order predicate or higher-order function term?
        ( if Functor = closure_cons(ShroudedPredProcId, _) then
            % Yes, the unification creates a higher-order term.
            % Make sure that the predicate/function is exported.

            proc(PredId, _) = unshroud_pred_proc_id(ShroudedPredProcId),
            intermod_add_proc(PredId, DoWrite, !IntermodInfo)
        else
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

%---------------------------------------------------------------------------%

:- pred gather_opt_export_instances(intermod_info::in, intermod_info::out)
    is det. 

gather_opt_export_instances(!IntermodInfo) :-
    intermod_info_get_module_info(!.IntermodInfo, ModuleInfo),
    module_info_get_instance_table(ModuleInfo, Instances),
    map.foldl(gather_opt_export_instances_in_class(ModuleInfo), Instances,
        !IntermodInfo).

:- pred gather_opt_export_instances_in_class(module_info::in, class_id::in,
    list(hlds_instance_defn)::in,
    intermod_info::in, intermod_info::out) is det.

gather_opt_export_instances_in_class(ModuleInfo, ClassId, InstanceDefns,
        !IntermodInfo) :-
    list.foldl(
        gather_opt_export_instance_in_instance_defn(ModuleInfo, ClassId),
        InstanceDefns, !IntermodInfo).

:- pred gather_opt_export_instance_in_instance_defn(module_info::in,
    class_id::in, hlds_instance_defn::in,
    intermod_info::in, intermod_info::out) is det.

gather_opt_export_instance_in_instance_defn(ModuleInfo, ClassId, InstanceDefn,
        !IntermodInfo) :-
    InstanceDefn = hlds_instance_defn(ModuleName, Types, OriginalTypes,
        InstanceStatus, Context, InstanceConstraints, Interface0,
        MaybePredProcIds, TVarSet, Proofs),
    DefinedThisModule = instance_status_defined_in_this_module(InstanceStatus),
    (
        DefinedThisModule = yes,

        % The bodies are always stripped from instance declarations
        % before writing them to `int' files, so the full instance
        % declaration should be written even for exported instances.

        SavedIntermodInfo = !.IntermodInfo,
        (
            Interface0 = instance_body_concrete(Methods0),
            (
                MaybePredProcIds = yes(ClassProcs),
                ClassPreds0 =
                    list.map(pred_proc_id_project_pred_id, ClassProcs),

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
            list.map_foldl(intermod_add_proc, PredIds, DoWriteMethodsList,
                !IntermodInfo),
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
                !:IntermodInfo = SavedIntermodInfo
            )
        ;
            Interface0 = instance_body_abstract,
            Interface = Interface0
        ),
        ( if
            % Don't write an abstract instance declaration
            % if the declaration is already in the `.int' file.
            (
                Interface = instance_body_abstract
            =>
                instance_status_is_exported(InstanceStatus) = no
            )
        then
            InstanceDefnToWrite = hlds_instance_defn(ModuleName,
                Types, OriginalTypes, InstanceStatus, Context,
                InstanceConstraints, Interface, MaybePredProcIds,
                TVarSet, Proofs),
            intermod_info_get_instances(!.IntermodInfo, Instances0),
            Instances = [ClassId - InstanceDefnToWrite | Instances0],
            intermod_info_set_instances(Instances, !IntermodInfo)
        else
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
    pred_info_get_external_type_params(MethodCallPredInfo,
        MethodCallExternalTypeParams),
    InstanceMethod0 = instance_method(PredOrFunc, MethodName,
        InstanceMethodDefn0, MethodArity, MethodContext),
    (
        InstanceMethodDefn0 = instance_proc_def_name(InstanceMethodName0),
        PredOrFunc = pf_function,
        ( if
            find_func_matching_instance_method(ModuleInfo, InstanceMethodName0,
                MethodArity, MethodCallTVarSet, MethodCallExistQTVars,
                MethodCallArgTypes, MethodCallExternalTypeParams,
                MethodContext, MaybePredId, InstanceMethodName)
        then
            (
                MaybePredId = yes(PredId),
                PredIds = [PredId | PredIds0]
            ;
                MaybePredId = no,
                PredIds = PredIds0
            ),
            InstanceMethodDefn = instance_proc_def_name(InstanceMethodName)
        else
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
            MethodCallExternalTypeParams, MethodContext,
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
    external_type_params::in, prog_context::in, maybe(pred_id)::out,
    sym_name::out) is semidet.

find_func_matching_instance_method(ModuleInfo, InstanceMethodName0,
        MethodArity, MethodCallTVarSet, MethodCallExistQTVars,
        MethodCallArgTypes, MethodCallExternalTypeParams, MethodContext,
        MaybePredId, InstanceMethodName) :-
    module_info_get_ctor_field_table(ModuleInfo, CtorFieldTable),
    ( if
        is_field_access_function_name(ModuleInfo, InstanceMethodName0,
            MethodArity, _, FieldName),
        map.search(CtorFieldTable, FieldName, FieldDefns)
    then
        TypeCtors0 = list.map(
            (func(FieldDefn) = TypeCtor :-
                FieldDefn = hlds_ctor_field_defn(_, _, TypeCtor, _, _)
            ), FieldDefns)
    else
        TypeCtors0 = []
    ),
    module_info_get_cons_table(ModuleInfo, Ctors),
    ( if
        ConsId = cons(InstanceMethodName0, MethodArity,
            cons_id_dummy_type_ctor),
        search_cons_table(Ctors, ConsId, MatchingConstructors)
    then
        TypeCtors1 = list.map(
            (func(ConsDefn) = TypeCtor :-
                ConsDefn ^ cons_type_ctor = TypeCtor
            ), MatchingConstructors)
    else
        TypeCtors1 = []
    ),
    TypeCtors = TypeCtors0 ++ TypeCtors1,

    module_info_get_predicate_table(ModuleInfo, PredicateTable),
    predicate_table_lookup_func_sym_arity(PredicateTable,
        may_be_partially_qualified, InstanceMethodName0, MethodArity, PredIds),
    ( if
        PredIds = [_ | _],
        find_matching_pred_id(ModuleInfo, PredIds, MethodCallTVarSet,
            MethodCallExistQTVars, MethodCallArgTypes,
            MethodCallExternalTypeParams, no, MethodContext,
            PredId, InstanceMethodFuncName)
    then
        TypeCtors = [],
        MaybePredId = yes(PredId),
        InstanceMethodName = InstanceMethodFuncName
    else
        TypeCtors = [TheTypeCtor],
        MaybePredId = no,
        ( if TheTypeCtor = type_ctor(qualified(TypeModule, _), _) then
            UnqualMethodName = unqualify_name(InstanceMethodName0),
            InstanceMethodName = qualified(TypeModule, UnqualMethodName)
        else
            unexpected($module, $pred, "unqualified type_ctor in " ++
                "hlds_cons_defn or hlds_ctor_field_defn")
        )
    ).

%---------------------------------------------------------------------------%

:- pred gather_opt_export_types(intermod_info::in, intermod_info::out) is det.

gather_opt_export_types(!IntermodInfo) :-
    intermod_info_get_module_info(!.IntermodInfo, ModuleInfo),
    module_info_get_type_table(ModuleInfo, TypeTable),
    foldl_over_type_ctor_defns(gather_opt_export_types_in_type_defn, TypeTable,
        !IntermodInfo).

:- pred gather_opt_export_types_in_type_defn(type_ctor::in, hlds_type_defn::in,
    intermod_info::in, intermod_info::out) is det.

gather_opt_export_types_in_type_defn(TypeCtor, TypeDefn0, !IntermodInfo) :-
    intermod_info_get_module_info(!.IntermodInfo, ModuleInfo),
    module_info_get_name(ModuleInfo, ModuleName),
    ( if should_opt_export_type_defn(ModuleName, TypeCtor, TypeDefn0) then
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

            ( if
                MaybeForeign0 = yes(ForeignTypeBody0),
                have_foreign_type_for_backend(Target, ForeignTypeBody0, yes)
            then
                % The header code must be written since it could be used
                % by the foreign type.
                intermod_info_set_write_header(!IntermodInfo),
                resolve_foreign_type_body_overloading(ModuleInfo, TypeCtor,
                    ForeignTypeBody0, ForeignTypeBody, !IntermodInfo),
                MaybeForeign = yes(ForeignTypeBody),
                MaybeUserEqComp = MaybeUserEqComp0
            else
                resolve_unify_compare_overloading(ModuleInfo, TypeCtor,
                    MaybeUserEqComp0, MaybeUserEqComp, !IntermodInfo),
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
            intermod_info_set_write_header(!IntermodInfo),
            resolve_foreign_type_body_overloading(ModuleInfo, TypeCtor,
                ForeignTypeBody0, ForeignTypeBody, !IntermodInfo),
            TypeBody = hlds_foreign_type(ForeignTypeBody),
            hlds_data.set_type_defn_body(TypeBody, TypeDefn0, TypeDefn)
        ;
            ( TypeBody0 = hlds_eqv_type(_)
            ; TypeBody0 = hlds_solver_type(_)
            ; TypeBody0 = hlds_abstract_type(_)
            ),
            TypeDefn = TypeDefn0
        ),
        intermod_info_get_types(!.IntermodInfo, Types0),
        intermod_info_set_types([TypeCtor - TypeDefn | Types0], !IntermodInfo)
    else
        true
    ).

:- pred resolve_foreign_type_body_overloading(module_info::in,
    type_ctor::in, foreign_type_body::in, foreign_type_body::out,
    intermod_info::in, intermod_info::out) is det.

resolve_foreign_type_body_overloading(ModuleInfo, TypeCtor,
        ForeignTypeBody0, ForeignTypeBody, !IntermodInfo) :-
    ForeignTypeBody0 = foreign_type_body(MaybeC0, MaybeJava0, MaybeCSharp0,
        MaybeErlang0),
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
        ; Target = target_erlang
        ),
        resolve_foreign_type_body_overloading_2(ModuleInfo, TypeCtor,
            MaybeC0, MaybeC, !IntermodInfo)
    ;
        ( Target = target_csharp
        ; Target = target_java
        ),
        MaybeC = MaybeC0
    ),
    (
        Target = target_csharp,
        resolve_foreign_type_body_overloading_2(ModuleInfo, TypeCtor,
            MaybeCSharp0, MaybeCSharp, !IntermodInfo)
    ;
        ( Target = target_c
        ; Target = target_java
        ; Target = target_erlang
        ),
        MaybeCSharp = MaybeCSharp0
    ),
    (
        Target = target_java,
        resolve_foreign_type_body_overloading_2(ModuleInfo, TypeCtor,
            MaybeJava0, MaybeJava, !IntermodInfo)
    ;
        ( Target = target_c
        ; Target = target_csharp
        ; Target = target_erlang
        ),
        MaybeJava = MaybeJava0
    ),
    (
        Target = target_erlang,
        resolve_foreign_type_body_overloading_2(ModuleInfo, TypeCtor,
            MaybeErlang0, MaybeErlang, !IntermodInfo)
    ;
        ( Target = target_c
        ; Target = target_csharp
        ; Target = target_java
        ),
        MaybeErlang = MaybeErlang0
    ),
    ForeignTypeBody = foreign_type_body(MaybeC, MaybeJava, MaybeCSharp,
        MaybeErlang).

:- pred resolve_foreign_type_body_overloading_2(module_info::in, type_ctor::in,
    foreign_type_lang_body(T)::in, foreign_type_lang_body(T)::out,
    intermod_info::in, intermod_info::out) is det.

resolve_foreign_type_body_overloading_2(ModuleInfo, TypeCtor,
        MaybeForeignTypeLangData0, MaybeForeignTypeLangData, !IntermodInfo) :-
    (
        MaybeForeignTypeLangData0 = no,
        MaybeForeignTypeLangData = no
    ;
        MaybeForeignTypeLangData0 =
            yes(foreign_type_lang_data(Body, MaybeUserEqComp0, Assertions)),
        resolve_unify_compare_overloading(ModuleInfo, TypeCtor,
            MaybeUserEqComp0, MaybeUserEqComp, !IntermodInfo),
        MaybeForeignTypeLangData =
            yes(foreign_type_lang_data(Body, MaybeUserEqComp, Assertions))
    ).

:- pred resolve_unify_compare_overloading(module_info::in,
    type_ctor::in, maybe(unify_compare)::in, maybe(unify_compare)::out,
    intermod_info::in, intermod_info::out) is det.

resolve_unify_compare_overloading(ModuleInfo, TypeCtor,
        MaybeUnifyCompare0, MaybeUnifyCompare, !IntermodInfo) :-
    (
        MaybeUnifyCompare0 = no,
        MaybeUnifyCompare = no
    ;
        MaybeUnifyCompare0 =
            yes(abstract_noncanonical_type(IsSolverType)),
        MaybeUnifyCompare =
            yes(abstract_noncanonical_type(IsSolverType))
    ;
        MaybeUnifyCompare0 =
            yes(unify_compare(MaybeUserEq0, MaybeUserCompare0)),
        resolve_user_special_pred_overloading(ModuleInfo, spec_pred_unify,
            TypeCtor, MaybeUserEq0, MaybeUserEq, !IntermodInfo),
        resolve_user_special_pred_overloading(ModuleInfo, spec_pred_compare,
            TypeCtor, MaybeUserCompare0, MaybeUserCompare, !IntermodInfo),
        MaybeUnifyCompare =
            yes(unify_compare(MaybeUserEq, MaybeUserCompare))
    ).

:- pred resolve_user_special_pred_overloading(module_info::in,
    special_pred_id::in, type_ctor::in, maybe(sym_name)::in,
    maybe(sym_name)::out, intermod_info::in, intermod_info::out) is det.

resolve_user_special_pred_overloading(_, _, _, no, no, !IntermodInfo).
resolve_user_special_pred_overloading(ModuleInfo, SpecialId,
        TypeCtor, yes(Pred0), yes(Pred), !IntermodInfo) :-
    module_info_get_special_pred_maps(ModuleInfo, SpecialPredMaps),
    lookup_special_pred_maps(SpecialPredMaps, SpecialId, TypeCtor,
        SpecialPredId),
    module_info_pred_info(ModuleInfo, SpecialPredId, SpecialPredInfo),
    pred_info_get_arg_types(SpecialPredInfo, TVarSet, ExistQVars, ArgTypes),
    pred_info_get_external_type_params(SpecialPredInfo, ExternalTypeParams),
    init_markers(Markers0),
    add_marker(marker_calls_are_fully_qualified, Markers0, Markers),
    pred_info_get_context(SpecialPredInfo, Context),
    resolve_pred_overloading(ModuleInfo, Markers, TVarSet, ExistQVars,
        ArgTypes, ExternalTypeParams, Context, Pred0, Pred, UserEqPredId),
    intermod_add_proc(UserEqPredId, _, !IntermodInfo).

:- pred should_opt_export_type_defn(module_name::in, type_ctor::in,
    hlds_type_defn::in) is semidet.

should_opt_export_type_defn(ModuleName, TypeCtor, TypeDefn) :-
    hlds_data.get_type_defn_status(TypeDefn, TypeStatus),
    TypeCtor = type_ctor(Name, _Arity),
    Name = qualified(ModuleName, _),
    type_status_to_write(TypeStatus) = yes.

%---------------------------------------------------------------------------%

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
    ( if
        % If none of these item types need writing, nothing else
        % needs to be written.

        set.is_empty(Preds),
        set.is_empty(PredDecls),
        Instances = [],
        module_info_get_type_table(ModuleInfo, TypeTable),
        get_all_type_ctor_defns(TypeTable, TypeCtorsDefns),
        some_type_needs_to_be_written(TypeCtorsDefns, no)
    then
        true
    else
        write_intermod_info_body(IntermodInfo, !IO)
    ).

:- pred some_type_needs_to_be_written(
    assoc_list(type_ctor, hlds_type_defn)::in, bool::out) is det.

some_type_needs_to_be_written([], no).
some_type_needs_to_be_written([_ - TypeDefn | TypeCtorDefns], NeedWrite) :-
    hlds_data.get_type_defn_status(TypeDefn, TypeStatus),
    ( if
        ( TypeStatus = type_status(status_abstract_exported)
        ; TypeStatus = type_status(status_exported_to_submodules)
        )
    then
        NeedWrite = yes
    else
        some_type_needs_to_be_written(TypeCtorDefns, NeedWrite)
    ).

:- pred write_intermod_info_body(intermod_info::in, io::di, io::uo) is det.

write_intermod_info_body(IntermodInfo, !IO) :-
    IntermodInfo = intermod_info(_, WritePredPredIdSet, WriteDeclPredIdSet,
        Instances, Types, ModuleInfo, WriteHeader, _, _),
    set.to_sorted_list(WritePredPredIdSet, WritePredPredIds),
    set.to_sorted_list(WriteDeclPredIdSet, WriteDeclPredIds),

    module_info_get_avail_module_map(ModuleInfo, AvailModuleMap),
    % XXX We could and should reduce AvailModules to the set of modules
    % that are *actually needed* by the items being written.
    map.keys(AvailModuleMap, AvailModuleNames),
    list.foldl(intermod_write_use_module, AvailModuleNames, !IO),

    module_info_get_globals(ModuleInfo, Globals),
    OutInfo0 = init_hlds_out_info(Globals, output_mercury),

    % We don't want to output line numbers in the .opt files,
    % since that causes spurious changes to the .opt files
    % when you make trivial changes (e.g. add comments) to the source files.
    MercInfo0 = OutInfo0 ^ hoi_mercury_to_mercury,
    MercInfo = merc_out_info_disable_line_numbers(MercInfo0),
    OutInfo = OutInfo0 ^ hoi_mercury_to_mercury := MercInfo,
    % Disable verbose dumping of clauses.
    OutInfoForPreds = OutInfo ^ hoi_dump_hlds_options := "",

    intermod_write_types(OutInfo, Types, !IO),
    intermod_write_insts(OutInfo, ModuleInfo, !IO),
    intermod_write_modes(OutInfo, ModuleInfo, !IO),
    intermod_write_classes(OutInfo, ModuleInfo, !IO),
    intermod_write_instances(OutInfo, Instances, !IO),
    (
        WriteHeader = yes,
        module_info_get_foreign_import_modules(ModuleInfo,
            ForeignImportModules),
        ForeignImports =
            get_all_foreign_import_module_infos(ForeignImportModules),
        ( if set.is_empty(ForeignImports) then
            true
        else
            io.nl(!IO),
            set.fold(intermod_write_foreign_import, ForeignImports, !IO)
        )
    ;
        WriteHeader = no
    ),
    generate_order_pred_infos(ModuleInfo, WriteDeclPredIds,
        DeclOrderPredInfos),
    generate_order_pred_infos(ModuleInfo, WritePredPredIds,
        PredOrderPredInfos),
    (
        DeclOrderPredInfos = []
    ;
        DeclOrderPredInfos = [_ | _],
        io.nl(!IO),
        intermod_write_pred_decls(ModuleInfo, DeclOrderPredInfos, !IO)
    ),
    % Each of these writes a newline at the start.
    intermod_write_preds(OutInfoForPreds, ModuleInfo, PredOrderPredInfos, !IO).

:- type maybe_first
    --->    is_not_first
    ;       is_first.

:- pred maybe_write_nl(maybe_first::in, maybe_first::out, io::di, io::uo)
    is det.

maybe_write_nl(!First, !IO) :-
    (
        !.First = is_first,
        io.nl(!IO),
        !:First = is_not_first
    ;
        !.First = is_not_first
    ).

%---------------------------------------------------------------------------%

:- pred intermod_write_use_module(module_name::in, io::di, io::uo) is det.

intermod_write_use_module(ModuleName, !IO) :-
    io.write_string(":- use_module ", !IO),
    mercury_output_bracketed_sym_name(ModuleName, !IO),
    io.write_string(".\n", !IO).

%---------------------------------------------------------------------------%

:- pred intermod_write_types(hlds_out_info::in,
    assoc_list(type_ctor, hlds_type_defn)::in, io::di, io::uo) is det.

intermod_write_types(OutInfo, Types, !IO) :-
    (
        Types = []
    ;
        Types = [_ | _],
        io.nl(!IO),
        list.sort(Types, SortedTypes),
        list.foldl(intermod_write_type(OutInfo), SortedTypes, !IO)
    ).

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
        TypeBody = parse_tree_du_type(DetailsDu),
        DetailsDu = type_details_du(Ctors, MaybeUserEqComp,
            MaybeDirectArgCtors)
    ;
        Body = hlds_eqv_type(EqvType),
        TypeBody = parse_tree_eqv_type(type_details_eqv(EqvType))
    ;
        Body = hlds_abstract_type(Details),
        TypeBody = parse_tree_abstract_type(Details)
    ;
        Body = hlds_foreign_type(_),
        TypeBody = parse_tree_abstract_type(abstract_type_general)
    ;
        Body = hlds_solver_type(DetailsSolver),
        TypeBody = parse_tree_solver_type(DetailsSolver)
    ),
    MainItemTypeDefn = item_type_defn_info(Name, Args, TypeBody, VarSet,
        Context, -1),
    MainItem = item_type_defn(MainItemTypeDefn),
    MercInfo = OutInfo ^ hoi_mercury_to_mercury,
    mercury_output_item(MercInfo, MainItem, !IO),
    ( if
        ( Body = hlds_foreign_type(ForeignTypeBody)
        ; Body ^ du_type_is_foreign_type = yes(ForeignTypeBody)
        ),
        ForeignTypeBody = foreign_type_body(MaybeC, MaybeJava,
            MaybeCSharp, MaybeErlang)
    then
        (
            MaybeC = yes(DataC),
            DataC = foreign_type_lang_data(CForeignType,
                CMaybeUserEqComp, AssertionsC),
            CDetailsForeign = type_details_foreign(c(CForeignType),
                CMaybeUserEqComp, AssertionsC),
            CItemTypeDefn = item_type_defn_info(Name, Args,
                parse_tree_foreign_type(CDetailsForeign),
                VarSet, Context, -1),
            CItem = item_type_defn(CItemTypeDefn),
            mercury_output_item(MercInfo, CItem, !IO)
        ;
            MaybeC = no
        ),
        (
            MaybeJava = yes(DataJava),
            DataJava = foreign_type_lang_data(JavaForeignType,
                JavaMaybeUserEqComp, AssertionsJava),
            JavaDetailsForeign = type_details_foreign(java(JavaForeignType),
                JavaMaybeUserEqComp, AssertionsJava),
            JavaItemTypeDefn = item_type_defn_info(Name, Args,
                parse_tree_foreign_type(JavaDetailsForeign),
                VarSet, Context, -1),
            JavaItem = item_type_defn(JavaItemTypeDefn),
            mercury_output_item(MercInfo, JavaItem, !IO)
        ;
            MaybeJava = no
        ),
        (
            MaybeCSharp = yes(DataCSharp),
            DataCSharp = foreign_type_lang_data(CSharpForeignType,
                CSharpMaybeUserEqComp, AssertionsCSharp),
            CSharpDetailsForeign = type_details_foreign(
                csharp(CSharpForeignType),
                CSharpMaybeUserEqComp, AssertionsCSharp),
            CSharpItemTypeDefn = item_type_defn_info(Name, Args,
                parse_tree_foreign_type(CSharpDetailsForeign),
                VarSet, Context, -1),
            CSharpItem = item_type_defn(CSharpItemTypeDefn),
            mercury_output_item(MercInfo, CSharpItem, !IO)
        ;
            MaybeCSharp = no
        ),
        (
            MaybeErlang = yes(DataErlang),
            DataErlang = foreign_type_lang_data(ErlangForeignType,
                ErlangMaybeUserEqComp, AssertionsErlang),
            ErlangDetailsForeign = type_details_foreign(
                erlang(ErlangForeignType),
                ErlangMaybeUserEqComp, AssertionsErlang),
            ErlangItemTypeDefn = item_type_defn_info(Name, Args,
                parse_tree_foreign_type(ErlangDetailsForeign),
                VarSet, Context, -1),
            ErlangItem = item_type_defn(ErlangItemTypeDefn),
            mercury_output_item(MercInfo, ErlangItem, !IO)
        ;
            MaybeErlang = no
        )
    else
        true
    ),
    ( if
        ReservedTag = Body ^ du_type_reserved_tag,
        ReservedTag = uses_reserved_tag
    then
        % The pragma's origin isn't printed, so what origin we pass here
        % doesn't matter.
        ReserveItemPragma = item_pragma_info(pragma_reserve_tag(TypeCtor),
            item_origin_user, Context, -1),
        ReserveItem = item_pragma(ReserveItemPragma),
        mercury_output_item(MercInfo, ReserveItem, !IO)
    else
        true
    ),
    ( if
        Body = hlds_du_type(_, ConsTagVals, _, DuTypeKind, _, _, _, _, _),
        DuTypeKind = du_type_kind_foreign_enum(Lang)
    then
        map.foldl(gather_foreign_enum_value_pair, ConsTagVals, [],
            ForeignEnumVals),
        FEInfo = pragma_info_foreign_enum(Lang, TypeCtor, ForeignEnumVals),
        ForeignPragma = pragma_foreign_enum(FEInfo),
        % The pragma's origin isn't printed, so what origin we pass here
        % doesn't matter.
        ForeignItemPragma = item_pragma_info(ForeignPragma, item_origin_user,
            Context, -1),
        ForeignItem = item_pragma(ForeignItemPragma),
        mercury_output_item(MercInfo, ForeignItem, !IO)
    else
        true
    ).

:- pred gather_foreign_enum_value_pair(cons_id::in, cons_tag::in,
    assoc_list(sym_name, string)::in, assoc_list(sym_name, string)::out)
    is det.

gather_foreign_enum_value_pair(ConsId, ConsTag, !Values) :-
    ( if ConsId = cons(SymName0, 0, _) then
        SymName = SymName0
    else
        unexpected($module, $pred, "expected enumeration constant")
    ),
    ( if ConsTag = foreign_tag(_ForeignLang, ForeignTag0) then
        ForeignTag = ForeignTag0
    else
        unexpected($module, $pred, "expected foreign tag")
    ),
    !:Values = [SymName - ForeignTag | !.Values].

%---------------------------------------------------------------------------%

:- pred intermod_write_insts(hlds_out_info::in, module_info::in,
    io::di, io::uo) is det.

intermod_write_insts(OutInfo, ModuleInfo, !IO) :-
    module_info_get_name(ModuleInfo, ModuleName),
    module_info_get_inst_table(ModuleInfo, Insts),
    inst_table_get_user_insts(Insts, UserInstMap),
    map.foldl2(intermod_write_inst(OutInfo, ModuleName), UserInstMap,
        is_first, _, !IO).

:- pred intermod_write_inst(hlds_out_info::in, module_name::in, inst_id::in,
    hlds_inst_defn::in, maybe_first::in, maybe_first::out,
    io::di, io::uo) is det.

intermod_write_inst(OutInfo, ModuleName, InstId, InstDefn, !First, !IO) :-
    InstId = inst_id(SymName, _Arity),
    InstDefn = hlds_inst_defn(Varset, Args, Body, IFTC, Context, InstStatus),
    ( if
        SymName = qualified(ModuleName, _),
        inst_status_to_write(InstStatus) = yes
    then
        maybe_write_nl(!First, !IO),
        (
            Body = eqv_inst(Inst2),
            InstBody = eqv_inst(Inst2)
        ;
            Body = abstract_inst,
            InstBody = abstract_inst
        ),
        (
            IFTC = iftc_applicable_declared(ForTypeCtor),
            MaybeForTypeCtor = yes(ForTypeCtor)
        ;
            ( IFTC = iftc_applicable_known(_)
            ; IFTC = iftc_applicable_not_known
            ; IFTC = iftc_applicable_error
            ; IFTC = iftc_not_applicable
            ),
            MaybeForTypeCtor = no
        ),
        ItemInstDefn = item_inst_defn_info(SymName, Args, MaybeForTypeCtor,
            InstBody, Varset, Context, -1),
        Item = item_inst_defn(ItemInstDefn),
        MercInfo = OutInfo ^ hoi_mercury_to_mercury,
        mercury_output_item(MercInfo, Item, !IO)
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred intermod_write_modes(hlds_out_info::in, module_info::in,
    io::di, io::uo) is det.

intermod_write_modes(OutInfo, ModuleInfo, !IO) :-
    module_info_get_name(ModuleInfo, ModuleName),
    module_info_get_mode_table(ModuleInfo, Modes),
    mode_table_get_mode_defns(Modes, ModeDefnMap),
    map.foldl2(intermod_write_mode(OutInfo, ModuleName), ModeDefnMap,
        is_first, _, !IO).

:- pred intermod_write_mode(hlds_out_info::in, module_name::in, mode_id::in,
    hlds_mode_defn::in, maybe_first::in, maybe_first::out,
    io::di, io::uo) is det.

intermod_write_mode(OutInfo, ModuleName, ModeId, ModeDefn, !First, !IO) :-
    ModeId = mode_id(SymName, _Arity),
    ModeDefn = hlds_mode_defn(Varset, Args, eqv_mode(Mode), Context,
        ModeStatus),
    ( if
        SymName = qualified(ModuleName, _),
        mode_status_to_write(ModeStatus) = yes
    then
        maybe_write_nl(!First, !IO),
        ItemModeDefn = item_mode_defn_info(SymName, Args, eqv_mode(Mode),
            Varset, Context, -1),
        Item = item_mode_defn(ItemModeDefn),
        MercInfo = OutInfo ^ hoi_mercury_to_mercury,
        mercury_output_item(MercInfo, Item, !IO)
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred intermod_write_classes(hlds_out_info::in, module_info::in,
    io::di, io::uo) is det.

intermod_write_classes(OutInfo, ModuleInfo, !IO) :-
    module_info_get_name(ModuleInfo, ModuleName),
    module_info_get_class_table(ModuleInfo, ClassDefnMap),
    map.foldl2(intermod_write_class(OutInfo, ModuleName), ClassDefnMap,
        is_first, _, !IO).

:- pred intermod_write_class(hlds_out_info::in, module_name::in, class_id::in,
    hlds_class_defn::in, maybe_first::in, maybe_first::out,
    io::di, io::uo) is det.

intermod_write_class(OutInfo, ModuleName, ClassId, ClassDefn, !First, !IO) :-
    ClassDefn = hlds_class_defn(TypeClassStatus, Constraints, HLDSFunDeps,
        _Ancestors, TVars, _Kinds, Interface, _HLDSClassInterface, TVarSet,
        Context),
    ClassId = class_id(QualifiedClassName, _),
    ( if
        QualifiedClassName = qualified(ModuleName, _),
        typeclass_status_to_write(TypeClassStatus) = yes
    then
        maybe_write_nl(!First, !IO),
        FunDeps = list.map(unmake_hlds_class_fundep(TVars), HLDSFunDeps),
        ItemTypeClass = item_typeclass_info(QualifiedClassName, TVars,
            Constraints, FunDeps, Interface, TVarSet, Context, -1),
        Item = item_typeclass(ItemTypeClass),
        MercInfo = OutInfo ^ hoi_mercury_to_mercury,
        mercury_output_item(MercInfo, Item, !IO)
    else
        true
    ).

:- func unmake_hlds_class_fundep(list(tvar), hlds_class_fundep) = prog_fundep.

unmake_hlds_class_fundep(TVars, HLDSFunDep) = ParseTreeFunDep :-
    HLDSFunDep = fundep(DomainArgPosns, RangeArgPosns),
    DomainTVars = unmake_hlds_class_fundep_arg_posns(TVars, DomainArgPosns),
    RangeTVars = unmake_hlds_class_fundep_arg_posns(TVars, RangeArgPosns),
    ParseTreeFunDep = fundep(DomainTVars, RangeTVars).

:- func unmake_hlds_class_fundep_arg_posns(list(tvar), set(hlds_class_argpos))
    = list(tvar).

unmake_hlds_class_fundep_arg_posns(TVars, ArgPosns) = ArgTVars :-
    ArgTVarsSet = set.map(list.det_index1(TVars), ArgPosns),
    set.to_sorted_list(ArgTVarsSet, ArgTVars).

%---------------------------------------------------------------------------%

:- pred intermod_write_instances(hlds_out_info::in,
    assoc_list(class_id, hlds_instance_defn)::in, io::di, io::uo) is det.

intermod_write_instances(OutInfo, Instances, !IO) :-
    (
        Instances = []
    ;
        Instances = [_ | _],
        io.nl(!IO),
        list.sort(Instances, SortedInstances),
        list.foldl(intermod_write_instance(OutInfo), SortedInstances, !IO)
    ).

:- pred intermod_write_instance(hlds_out_info::in,
    pair(class_id, hlds_instance_defn)::in, io::di, io::uo) is det.

intermod_write_instance(OutInfo, ClassId - InstanceDefn, !IO) :-
    InstanceDefn = hlds_instance_defn(ModuleName, Types, OriginalTypes, _,
        Context, Constraints, Body, _, TVarSet, _),
    ClassId = class_id(ClassName, _),
    ItemInstance = item_instance_info(ClassName, Types, OriginalTypes,
        Constraints, Body, TVarSet, ModuleName, Context, -1),
    Item = item_instance(ItemInstance),
    MercInfo = OutInfo ^ hoi_mercury_to_mercury,
    mercury_output_item(MercInfo, Item, !IO).

%---------------------------------------------------------------------------%

:- pred intermod_write_foreign_import(foreign_import_module_info::in,
    io::di, io::uo) is det.

intermod_write_foreign_import(ForeignImport, !IO) :-
    FIMInfo = pragma_info_foreign_import_module(ForeignImport),
    mercury_output_pragma_foreign_import_module(FIMInfo, !IO).

%---------------------------------------------------------------------------%

:- type order_pred_info
    --->    order_pred_info(string, arity, pred_or_func, pred_id, pred_info).

:- pred generate_order_pred_infos(module_info::in, list(pred_id)::in,
    list(order_pred_info)::out) is det.

generate_order_pred_infos(ModuleInfo, PredIds, SortedOrderPredInfos) :-
    generate_order_pred_infos_acc(ModuleInfo, PredIds, [], OrderPredInfos),
    list.sort(OrderPredInfos, SortedOrderPredInfos).

:- pred generate_order_pred_infos_acc(module_info::in, list(pred_id)::in,
    list(order_pred_info)::in, list(order_pred_info)::out) is det.

generate_order_pred_infos_acc(_, [], !OrderPredInfos).
generate_order_pred_infos_acc(ModuleInfo, [PredId | PredIds],
        !OrderPredInfos) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    PredName = pred_info_name(PredInfo),
    PredArity = pred_info_orig_arity(PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    OrderPredInfo = order_pred_info(PredName, PredArity, PredOrFunc,
        PredId, PredInfo),
    !:OrderPredInfos = [OrderPredInfo | !.OrderPredInfos],
    generate_order_pred_infos_acc(ModuleInfo, PredIds,
        !OrderPredInfos).

%---------------------------------------------------------------------------%

    % We need to write all the declarations for local predicates so
    % the procedure labels for the C code are calculated correctly.
    %
:- pred intermod_write_pred_decls(module_info::in, list(order_pred_info)::in,
    io::di, io::uo) is det.

intermod_write_pred_decls(_, [], !IO).
intermod_write_pred_decls(ModuleInfo, [OrderPredInfo | OrderPredInfos], !IO) :-
    intermod_write_pred_decl(ModuleInfo, OrderPredInfo, !IO),
    intermod_write_pred_decls(ModuleInfo, OrderPredInfos, !IO).

:- pred intermod_write_pred_decl(module_info::in, order_pred_info::in,
    io::di, io::uo) is det.

intermod_write_pred_decl(ModuleInfo, OrderPredInfo, !IO) :-
    OrderPredInfo = order_pred_info(PredName, _PredArity, PredOrFunc,
        PredId, PredInfo),
    ModuleName = pred_info_module(PredInfo),
    pred_info_get_arg_types(PredInfo, TVarSet, ExistQVars, ArgTypes),
    pred_info_get_purity(PredInfo, Purity),
    pred_info_get_class_context(PredInfo, ClassContext),
    pred_info_get_goal_type(PredInfo, GoalType),
    (
        GoalType = goal_type_foreign,
        % For foreign code goals, we cannot append variable numbers to type
        % variables in the predicate declaration, because the foreign code
        % may contain references to variables such as `TypeInfo_for_T'
        % which will break if `T' is written as `T_1' in the pred declaration.
        VarNamePrint = print_name_only
    ;
        GoalType = goal_type_clause_and_foreign,
        % Because pragmas may be present, we treat this case like
        % pragmas above.
        VarNamePrint = print_name_only
    ;
        ( GoalType = goal_type_clause
        ; GoalType = goal_type_promise(_)
        ; GoalType = goal_type_none
        ),
        VarNamePrint = print_name_and_num
    ),
    PredSymName = qualified(ModuleName, PredName),
    (
        PredOrFunc = pf_predicate,
        mercury_output_pred_type(TVarSet, VarNamePrint, ExistQVars,
            PredSymName, ArgTypes, no, Purity,
            ClassContext, !IO)
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(ArgTypes, FuncArgTypes, FuncRetType),
        mercury_output_func_type(TVarSet, VarNamePrint, ExistQVars,
            PredSymName, FuncArgTypes, FuncRetType, no, Purity,
            ClassContext, !IO)
    ),
    pred_info_get_proc_table(PredInfo, ProcMap),
    % Make sure the mode declarations go out in the same order they came in,
    % so that the all the modes get the same proc_id in the importing modules.
    % SortedProcPairs will sorted on proc_ids. (map.values is not *documented*
    % to return a list sorted by keys.)
    map.to_sorted_assoc_list(ProcMap, SortedProcPairs),
    intermod_write_pred_valid_modes(PredOrFunc, PredSymName,
        SortedProcPairs, !IO),
    intermod_write_pred_marker_pragmas(PredInfo, !IO),
    intermod_write_pred_type_spec_pragmas(ModuleInfo, PredId, !IO).

:- pred intermod_write_pred_valid_modes(pred_or_func::in, sym_name::in,
    assoc_list(proc_id, proc_info)::in, io::di, io::uo) is det.

intermod_write_pred_valid_modes(_, _, [], !IO).
intermod_write_pred_valid_modes(PredOrFunc, PredSymName,
        [ProcIdInfo | ProcIdInfos], !IO) :-
    ProcIdInfo = _ProcId - ProcInfo,
    ( if proc_info_is_valid_mode(ProcInfo) then
        intermod_write_pred_mode(PredOrFunc, PredSymName, ProcInfo, !IO)
    else
        true
    ),
    intermod_write_pred_valid_modes(PredOrFunc, PredSymName, ProcIdInfos, !IO).

:- pred intermod_write_pred_mode(pred_or_func::in, sym_name::in,
    proc_info::in, io::di, io::uo) is det.

intermod_write_pred_mode(PredOrFunc, PredSymName, ProcInfo, !IO) :-
    proc_info_get_maybe_declared_argmodes(ProcInfo, MaybeArgModes),
    proc_info_get_declared_determinism(ProcInfo, MaybeDetism),
    ( if
        MaybeArgModes = yes(ArgModesPrime),
        MaybeDetism = yes(DetismPrime)
    then
        ArgModes = ArgModesPrime,
        Detism = DetismPrime
    else
        unexpected($module, $pred, "attempt to write undeclared mode")
    ),
    varset.init(Varset),
    (
        PredOrFunc = pf_function,
        pred_args_to_func_args(ArgModes, FuncArgModes, FuncRetMode),
        mercury_output_func_mode_decl(output_mercury, Varset, PredSymName,
            FuncArgModes, FuncRetMode, yes(Detism), !IO)
    ;
        PredOrFunc = pf_predicate,
        MaybeWithInst = maybe.no,
        mercury_output_pred_mode_decl(output_mercury, Varset, PredSymName,
            ArgModes, MaybeWithInst, yes(Detism), !IO)
    ).

:- pred intermod_write_pred_marker_pragmas(pred_info::in,
    io::di, io::uo) is det.

intermod_write_pred_marker_pragmas(PredInfo, !IO) :-
    ModuleName = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    PredArity = pred_info_orig_arity(PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    PredSymName = qualified(ModuleName, PredName),
    pred_info_get_markers(PredInfo, Markers),
    markers_to_marker_list(Markers, MarkerList),
    intermod_write_pred_marker_pragmas(PredOrFunc, PredSymName, PredArity,
        MarkerList, !IO).

:- pred intermod_write_pred_marker_pragmas(pred_or_func::in,
    sym_name::in, int::in, list(pred_marker)::in, io::di, io::uo) is det.

intermod_write_pred_marker_pragmas(_, _, _, [], !IO).
intermod_write_pred_marker_pragmas(PredOrFunc, PredSymName, PredArity,
        [Marker | Markers], !IO) :-
    should_output_marker(Marker, ShouldOutput),
    (
        ShouldOutput = yes,
        marker_name(Marker, MarkerName),
        mercury_output_pragma_decl(PredSymName, PredArity, PredOrFunc,
            MarkerName, no, !IO)
    ;
        ShouldOutput = no
    ),
    intermod_write_pred_marker_pragmas(PredOrFunc, PredSymName, PredArity,
        Markers, !IO).

:- pred intermod_write_pred_type_spec_pragmas(module_info::in, pred_id::in,
    io::di, io::uo) is det.

intermod_write_pred_type_spec_pragmas(ModuleInfo, PredId, !IO) :-
    module_info_get_type_spec_info(ModuleInfo, TypeSpecInfo),
    PragmaMap = TypeSpecInfo ^ pragma_map,
    ( if multi_map.search(PragmaMap, PredId, TypeSpecPragmas) then
        list.foldl(
            mercury_output_pragma_type_spec(print_name_and_num,
                output_mercury),
            TypeSpecPragmas, !IO)
    else
        true
    ).

    % Is a pragma declaration required in the `.opt' file for
    % a predicate with the given marker.
    %
:- pred should_output_marker(pred_marker::in, bool::out) is det.

should_output_marker(marker_stub, no).
should_output_marker(marker_builtin_stub, no).
    % Since the inferred declarations are output, these
    % don't need to be done in the importing module.
should_output_marker(marker_infer_type, no).
should_output_marker(marker_infer_modes, no).
should_output_marker(marker_no_pred_decl, no).
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
should_output_marker(marker_consider_used, no).
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
should_output_marker(marker_has_incomplete_switch, no).
should_output_marker(marker_has_format_call, no).

%---------------------------------------------------------------------------%

:- pred intermod_write_preds(hlds_out_info::in, module_info::in,
    list(order_pred_info)::in, io::di, io::uo) is det.

intermod_write_preds(_, _, [], !IO).
intermod_write_preds(OutInfo, ModuleInfo, [OrderPredInfo | OrderPredInfos],
        !IO) :-
    intermod_write_pred(OutInfo, ModuleInfo, OrderPredInfo, !IO),
    intermod_write_preds(OutInfo, ModuleInfo, OrderPredInfos, !IO).

:- pred intermod_write_pred(hlds_out_info::in, module_info::in,
    order_pred_info::in, io::di, io::uo) is det.

intermod_write_pred(OutInfo, ModuleInfo, OrderPredInfo, !IO) :-
    io.nl(!IO),
    OrderPredInfo = order_pred_info(PredName, _PredArity, PredOrFunc,
        PredId, PredInfo),
    ModuleName = pred_info_module(PredInfo),
    PredSymName = qualified(ModuleName, PredName),
    intermod_write_pred_marker_pragmas(PredInfo, !IO),
    % The type specialization pragmas for exported preds should
    % already be in the interface file.

    pred_info_get_clauses_info(PredInfo, ClausesInfo),
    clauses_info_get_varset(ClausesInfo, VarSet),
    clauses_info_get_headvar_list(ClausesInfo, HeadVars),
    clauses_info_get_clauses_rep(ClausesInfo, ClausesRep, _ItemNumbers),
    clauses_info_get_vartypes(ClausesInfo, VarTypes),
    get_clause_list_maybe_repeated(ClausesRep, Clauses),

    pred_info_get_goal_type(PredInfo, GoalType),
    (
        GoalType = goal_type_promise(PromiseType),
        (
            Clauses = [Clause],
            write_promise(OutInfo, ModuleInfo, VarSet, no_varset_vartypes,
                print_name_only, 0, PromiseType, PredId, PredOrFunc,
                HeadVars, Clause, !IO)
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
        TypeQual = varset_vartypes(TypeVarset, VarTypes),
        list.foldl(
            intermod_write_clause(OutInfo, ModuleInfo, PredId, PredSymName,
                PredOrFunc, VarSet, TypeQual, HeadVars),
            Clauses, !IO)
    ).

:- pred intermod_write_clause(hlds_out_info::in, module_info::in,
    pred_id::in, sym_name::in, pred_or_func::in,
    prog_varset::in, maybe_vartypes::in, list(prog_var)::in, clause::in,
    io::di, io::uo) is det.

intermod_write_clause(OutInfo, ModuleInfo, PredId, SymName, PredOrFunc,
        VarSet, TypeQual, HeadVars, Clause0, !IO) :-
    Clause0 = clause(ApplicableProcIds, Goal, ImplLang, _, _),
    (
        ImplLang = impl_lang_mercury,
        strip_headvar_unifications(HeadVars, Clause0, ClauseHeadVars, Clause),
        % Variable numbers need to be used for the case where the added
        % arguments for a DCG pred expression are named the same
        % as variables in the enclosing clause.
        %
        % We don't need the actual names, and including them in the .opt file
        % would lead to unnecessary recompilations when the *only* changes
        % in a .opt file are changes in variable variables.
        %
        % We could standardize the variables in the clause before printing
        % it out, numbering them e.g. in the order of their appearance,
        % so that changes in variable *numbers* don't cause recompilations
        % either. However, the variable numbers *are* initially allocated
        % in such an order, both by the code that reads in terms and the
        % code that converts parse tree goals into HLDS goals, so this is
        % not likely to be necessary, while its cost may be be non-negligible.
        write_clause(OutInfo, output_mercury, ModuleInfo, PredId, PredOrFunc,
            varset.init, TypeQual, print_name_and_num, write_declared_modes, 1,
            ClauseHeadVars, Clause, !IO)
    ;
        ImplLang = impl_lang_foreign(_),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        pred_info_get_proc_table(PredInfo, Procs),
        ( if
            (
                % Pull the foreign code out of the goal.
                Goal = hlds_goal(conj(plain_conj, Goals), _),
                list.filter(
                    ( pred(G::in) is semidet :-
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
        then
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
        else
            unexpected($module, $pred, "did not find foreign_proc")
        )
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
    ( if
        strip_headvar_unifications_from_goal_list(Goals0, HeadVars,
            [], Goals, HeadVarMap0, HeadVarMap)
    then
        list.map(
            (pred(HeadVar0::in, HeadTerm::out) is det :-
                ( if map.search(HeadVarMap, HeadVar0, HeadTerm0) then
                    HeadTerm = HeadTerm0
                else
                    Context = Clause0 ^ clause_context,
                    HeadTerm = term.variable(HeadVar0, Context)
                )
            ), HeadVars, HeadTerms),
        conj_list_to_goal(Goals, GoalInfo0, Goal),
        Clause = Clause0 ^ clause_body := Goal
    else
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
    ( if
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
                RHSTerm = int_to_decimal_term(Int, Context)
            ;
                ConsId  = uint_const(UInt),
                RHSTerm = uint_to_decimal_term(UInt, Context)
            ;
                ConsId = float_const(Float),
                RHSTerm = term.functor(term.float(Float), [], Context)
            ;
                ConsId = char_const(Char),
                RHSTerm = term.functor(term.atom(string.from_char(Char)),
                    [], Context)
            ;
                ConsId = string_const(String),
                RHSTerm = term.functor(term.string(String), [], Context)
            ;
                ConsId = cons(SymName, _, _),
                term.var_list_to_term_list(Args, ArgTerms),
                construct_qualified_term(SymName, ArgTerms, RHSTerm)
            )
        ;
            RHS = rhs_lambda_goal(_, _, _, _, _, _, _, _, _),
            fail
        )
    then
        % Don't strip the headvar unifications if one of the
        % headvars appears twice. This should probably never happen.
        map.insert(LHSVar, RHSTerm, !HeadVarMap),
        RevGoals1 = RevGoals0
    else
        RevGoals1 = [Goal | RevGoals0]
    ),
    strip_headvar_unifications_from_goal_list(Goals0, HeadVars,
        RevGoals1, Goals, !HeadVarMap).

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
        mercury_output_pragma_foreign_proc(output_mercury, FPInfo, !IO)
    ;
        MaybeArgModes = no,
        unexpected($module, $pred, "no mode declaration")
    ).

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
            MaybeNameAndMode = yes(foreign_arg_name_mode(Name, _Mode2))
        ),
        PragmaVar = pragma_var(Var, Name, Mode, bp_native_if_possible),
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

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

append_analysis_pragmas_to_opt_file(ModuleInfo, UnusedArgsInfos, !IO) :-
    module_info_get_proc_analysis_kinds(ModuleInfo, ProcAnalysisKinds),
    ( if
        set.is_empty(ProcAnalysisKinds),
        set.is_empty(UnusedArgsInfos)
    then
        % We have nothing to append to the .opt file.
        true
    else
        module_info_get_globals(ModuleInfo, Globals),
        module_info_get_name(ModuleInfo, ModuleName),
        module_name_to_file_name(Globals, do_not_create_dirs, ".opt.tmp",
            ModuleName, OptFileName, !IO),
        io.open_append(OptFileName, OptFileRes, !IO),
        (
            OptFileRes = ok(OptFile),
            io.set_output_stream(OptFile, OldStream, !IO),

            module_info_get_valid_pred_ids(ModuleInfo, PredIds),
            generate_order_pred_infos(ModuleInfo, PredIds, OrderPredInfos),

            ( if set.contains(ProcAnalysisKinds, pak_exception) then
                list.foldl2(write_pragma_exceptions_for_pred(ModuleInfo),
                    OrderPredInfos, is_first, _, !IO)
            else
                true
            ),
            ( if set.contains(ProcAnalysisKinds, pak_trailing) then
                list.foldl2(write_pragma_trailing_info_for_pred(ModuleInfo),
                    OrderPredInfos, is_first, _, !IO)
            else
                true
            ),
            ( if set.contains(ProcAnalysisKinds, pak_mm_tabling) then
                list.foldl2(write_pragma_mm_tabling_info_for_pred(ModuleInfo),
                    OrderPredInfos, is_first, _, !IO)
            else
                true
            ),
            ( if set.is_non_empty(UnusedArgsInfos) then
                set.foldl2(write_pragma_unused_args, UnusedArgsInfos,
                    is_first, _, !IO)
            else
                true
            ),
            ( if set.contains(ProcAnalysisKinds, pak_termination) then
                list.foldl2(write_pragma_termination_for_pred(ModuleInfo),
                    OrderPredInfos, is_first, _, !IO)
            else
                true
            ),
            ( if set.contains(ProcAnalysisKinds, pak_termination2) then
                list.foldl2(write_pragma_termination2_for_pred(ModuleInfo),
                    OrderPredInfos, is_first, _, !IO)
            else
                true
            ),
            ( if set.contains(ProcAnalysisKinds, pak_structure_sharing) then
                list.foldl2(
                    write_pragma_structure_sharing_for_pred(ModuleInfo),
                    OrderPredInfos, is_first, _, !IO)
            else
                true
            ),
            ( if set.contains(ProcAnalysisKinds, pak_structure_reuse) then
                list.foldl2(write_pragma_structure_reuse_for_pred(ModuleInfo),
                    OrderPredInfos, is_first, _, !IO)
            else
                true
            ),

            io.set_output_stream(OldStream, _, !IO),
            io.close_output(OptFile, !IO)
        ;
            OptFileRes = error(IOError),
            io.error_message(IOError, IOErrorMessage),
            io.write_strings(["Error opening file `",
                OptFileName, "' for output: ", IOErrorMessage], !IO),
            io.set_exit_status(1, !IO)
        )
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Write out the exception pragmas for this predicate.
    %
:- pred write_pragma_exceptions_for_pred(module_info::in, order_pred_info::in,
    maybe_first::in, maybe_first::out, io::di, io::uo) is det.

write_pragma_exceptions_for_pred(ModuleInfo, OrderPredInfo, !First, !IO) :-
    OrderPredInfo = order_pred_info(_, _, _, _, PredInfo),
    pred_info_get_proc_table(PredInfo, ProcTable),
    map.foldl2(
        maybe_write_pragma_exceptions_for_proc(ModuleInfo, OrderPredInfo),
        ProcTable, !First, !IO).

:- pred maybe_write_pragma_exceptions_for_proc(module_info::in,
    order_pred_info::in, proc_id::in, proc_info::in,
    maybe_first::in, maybe_first::out, io::di, io::uo) is det.

maybe_write_pragma_exceptions_for_proc(ModuleInfo, OrderPredInfo,
        ProcId, ProcInfo, !First, !IO) :-
    OrderPredInfo = order_pred_info(PredName, PredArity, PredOrFunc,
        PredId, PredInfo),
    ( if
        proc_info_is_valid_mode(ProcInfo),
        procedure_is_exported(ModuleInfo, PredInfo, ProcId),
        not is_unify_or_compare_pred(PredInfo),

        module_info_get_type_spec_info(ModuleInfo, TypeSpecInfo),
        TypeSpecInfo = type_spec_info(_, TypeSpecForcePreds, _, _),
        not set.member(PredId, TypeSpecForcePreds),

        % XXX Writing out pragmas for the automatically generated class
        % instance methods causes the compiler to abort when it reads them
        % back in.
        pred_info_get_markers(PredInfo, Markers),
        not check_marker(Markers, marker_class_instance_method),
        not check_marker(Markers, marker_named_class_instance_method),

        proc_info_get_exception_info(ProcInfo, MaybeProcExceptionInfo),
        MaybeProcExceptionInfo = yes(ProcExceptionInfo)
    then
        ModuleName = pred_info_module(PredInfo),
        PredSymName = qualified(ModuleName, PredName),
        proc_id_to_int(ProcId, ModeNum),
        PredNameArityPFMn = pred_name_arity_pf_mn(PredSymName, PredArity,
            PredOrFunc, ModeNum),
        ProcExceptionInfo = proc_exception_info(Status, _),
        ExceptionInfo = pragma_info_exceptions(PredNameArityPFMn, Status),
        maybe_write_nl(!First, !IO),
        mercury_output_pragma_exceptions(ExceptionInfo, !IO)
    else
        true
    ).

%---------------------------------------------------------------------------%

    % Write out the trailing_info pragma for this module.
    %
:- pred write_pragma_trailing_info_for_pred(module_info::in,
    order_pred_info::in, maybe_first::in, maybe_first::out,
    io::di, io::uo) is det.

write_pragma_trailing_info_for_pred(ModuleInfo, OrderPredInfo, !First, !IO) :-
    OrderPredInfo = order_pred_info(_, _, _, _, PredInfo),
    pred_info_get_proc_table(PredInfo, ProcTable),
    map.foldl2(
        maybe_write_pragma_trailing_info_for_proc(ModuleInfo, OrderPredInfo),
        ProcTable, !First, !IO).

:- pred maybe_write_pragma_trailing_info_for_proc(module_info::in,
    order_pred_info::in, proc_id::in, proc_info::in,
    maybe_first::in, maybe_first::out, io::di, io::uo) is det.

maybe_write_pragma_trailing_info_for_proc(ModuleInfo, OrderPredInfo,
        ProcId, ProcInfo, !First, !IO) :-
    OrderPredInfo = order_pred_info(PredName, PredArity, PredOrFunc,
        PredId, PredInfo),
    proc_info_get_trailing_info(ProcInfo, MaybeProcTrailingInfo),
    ( if
        proc_info_is_valid_mode(ProcInfo),
        MaybeProcTrailingInfo = yes(ProcTrailingInfo),
        should_write_trailing_info(ModuleInfo, PredId, ProcId, PredInfo,
            for_pragma, ShouldWrite),
        ShouldWrite = should_write
    then
        ModuleName = pred_info_module(PredInfo),
        PredSymName = qualified(ModuleName, PredName),
        proc_id_to_int(ProcId, ModeNum),
        PredNameArityPFMn = pred_name_arity_pf_mn(PredSymName, PredArity,
            PredOrFunc, ModeNum),
        ProcTrailingInfo = proc_trailing_info(Status, _),
        TrailingInfo = pragma_info_trailing_info(PredNameArityPFMn, Status),
        maybe_write_nl(!First, !IO),
        mercury_output_pragma_trailing_info(TrailingInfo, !IO)
    else
        true
    ).

%---------------------------------------------------------------------------%

    % Write out the mm_tabling_info pragma for this predicate.
    %
:- pred write_pragma_mm_tabling_info_for_pred(module_info::in,
    order_pred_info::in, maybe_first::in, maybe_first::out,
    io::di, io::uo) is det.

write_pragma_mm_tabling_info_for_pred(ModuleInfo, OrderPredInfo,
        !First, !IO) :-
    OrderPredInfo = order_pred_info(_, _, _, _, PredInfo),
    pred_info_get_proc_table(PredInfo, ProcTable),
    map.foldl2(
        maybe_write_pragma_mm_tabling_info_for_proc(ModuleInfo, OrderPredInfo),
        ProcTable, !First, !IO).

:- pred maybe_write_pragma_mm_tabling_info_for_proc(module_info::in,
    order_pred_info::in, proc_id::in, proc_info::in,
    maybe_first::in, maybe_first::out, io::di, io::uo) is det.

maybe_write_pragma_mm_tabling_info_for_proc(ModuleInfo, OrderPredInfo,
        ProcId, ProcInfo, !First, !IO) :-
    OrderPredInfo = order_pred_info(PredName, PredArity, PredOrFunc,
        PredId, PredInfo),
    proc_info_get_mm_tabling_info(ProcInfo, MaybeProcMMTablingInfo),
    ( if
        proc_info_is_valid_mode(ProcInfo),
        MaybeProcMMTablingInfo = yes(ProcMMTablingInfo),
        should_write_mm_tabling_info(ModuleInfo, PredId, ProcId, PredInfo,
            for_pragma, ShouldWrite),
        ShouldWrite = should_write
    then
        ModuleName = pred_info_module(PredInfo),
        PredSymName = qualified(ModuleName, PredName),
        proc_id_to_int(ProcId, ModeNum),
        PredNameArityPFMn = pred_name_arity_pf_mn(PredSymName, PredArity,
            PredOrFunc, ModeNum),
        ProcMMTablingInfo = proc_mm_tabling_info(Status, _),
        MMTablingInfo = pragma_info_mm_tabling_info(PredNameArityPFMn,
            Status),
        maybe_write_nl(!First, !IO),
        mercury_output_pragma_mm_tabling_info(MMTablingInfo, !IO)
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred write_pragma_structure_reuse_for_pred(module_info::in,
    order_pred_info::in, maybe_first::in, maybe_first::out,
    io::di, io::uo) is det.

write_pragma_structure_reuse_for_pred(ModuleInfo, OrderPredInfo,
        !First, !IO) :-
    OrderPredInfo = order_pred_info(_, _, _, _, PredInfo),
    pred_info_get_proc_table(PredInfo, ProcTable),
    map.foldl2(
        maybe_write_pragma_structure_reuse_for_proc(ModuleInfo,
            OrderPredInfo),
        ProcTable, !First, !IO).

:- pred maybe_write_pragma_structure_reuse_for_proc(module_info::in,
    order_pred_info::in, proc_id::in, proc_info::in,
    maybe_first::in, maybe_first::out, io::di, io::uo) is det.

maybe_write_pragma_structure_reuse_for_proc(ModuleInfo, OrderPredInfo,
        ProcId, ProcInfo, !First, !IO) :-
    OrderPredInfo = order_pred_info(PredName, _PredArity, PredOrFunc,
        PredId, PredInfo),
    ( if
        proc_info_is_valid_mode(ProcInfo),
        should_write_reuse_info(ModuleInfo, PredId, ProcId, PredInfo,
            for_pragma, ShouldWrite),
        ShouldWrite = should_write,
        proc_info_get_structure_reuse(ProcInfo, MaybeStructureReuseDomain),
        MaybeStructureReuseDomain = yes(StructureReuseDomain)
    then
        proc_info_get_varset(ProcInfo, VarSet),
        pred_info_get_typevarset(PredInfo, TypeVarSet),
        ModuleName = pred_info_module(PredInfo),
        PredSymName = qualified(ModuleName, PredName),
        proc_info_declared_argmodes(ProcInfo, ArgModes),
        PredNameModesPF = pred_name_modes_pf(PredSymName, ArgModes,
            PredOrFunc),
        proc_info_get_headvars(ProcInfo, HeadVars),
        proc_info_get_vartypes(ProcInfo, VarTypes),
        lookup_var_types(VarTypes, HeadVars, HeadVarTypes),
        StructureReuseDomain =
            structure_reuse_domain_and_status(Reuse, _Status),
        ReuseInfo = pragma_info_structure_reuse(PredNameModesPF,
            HeadVars, HeadVarTypes, yes(Reuse)),
        write_pragma_structure_reuse_info(output_debug,
            yes(VarSet), yes(TypeVarSet), ReuseInfo,!IO)
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred write_pragma_structure_sharing_for_pred(module_info::in,
    order_pred_info::in, maybe_first::in, maybe_first::out,
    io::di, io::uo) is det.

write_pragma_structure_sharing_for_pred(ModuleInfo, OrderPredInfo,
        !First, !IO) :-
    OrderPredInfo = order_pred_info(_, _, _, _, PredInfo),
    pred_info_get_proc_table(PredInfo, ProcTable),
    map.foldl2(
        maybe_write_pragma_structure_sharing_for_proc(ModuleInfo,
            OrderPredInfo),
        ProcTable, !First, !IO).

:- pred maybe_write_pragma_structure_sharing_for_proc(module_info::in,
    order_pred_info::in, proc_id::in, proc_info::in,
    maybe_first::in, maybe_first::out, io::di, io::uo) is det.

maybe_write_pragma_structure_sharing_for_proc(ModuleInfo, OrderPredInfo,
        ProcId, ProcInfo, !First, !IO) :-
    OrderPredInfo = order_pred_info(PredName, _PredArity, PredOrFunc,
        PredId, PredInfo),
    ( if
        proc_info_is_valid_mode(ProcInfo),
        should_write_sharing_info(ModuleInfo, PredId, ProcId, PredInfo,
            for_pragma, ShouldWrite),
        ShouldWrite = should_write,
        proc_info_get_structure_sharing(ProcInfo, MaybeSharingStatus),
        MaybeSharingStatus = yes(SharingStatus)
    then
        proc_info_get_varset(ProcInfo, VarSet),
        pred_info_get_typevarset(PredInfo, TypeVarSet),
        ModuleName = pred_info_module(PredInfo),
        PredSymName = qualified(ModuleName, PredName),
        proc_info_declared_argmodes(ProcInfo, ArgModes),
        PredNameModesPF = pred_name_modes_pf(PredSymName, ArgModes,
            PredOrFunc),
        proc_info_get_headvars(ProcInfo, HeadVars),
        proc_info_get_vartypes(ProcInfo, VarTypes),
        lookup_var_types(VarTypes, HeadVars, HeadVarTypes),
        SharingStatus = structure_sharing_domain_and_status(Sharing, _Status),
        SharingInfo = pragma_info_structure_sharing(PredNameModesPF,
            HeadVars, HeadVarTypes, yes(Sharing)),
        maybe_write_nl(!First, !IO),
        write_pragma_structure_sharing_info(output_debug,
            yes(VarSet), yes(TypeVarSet), SharingInfo, !IO)
    else
        true
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Write out a termination_info pragma for the predicate if it is exported,
    % it is not a builtin and it is not a predicate used to force type
    % specialization.
    %
:- pred write_pragma_termination_for_pred(module_info::in, order_pred_info::in,
    maybe_first::in, maybe_first::out, io::di, io::uo) is det.

write_pragma_termination_for_pred(ModuleInfo, OrderPredInfo, !First, !IO) :-
    OrderPredInfo = order_pred_info(_PredName, _PredArity, _PredOrFunc,
        PredId, PredInfo),
    pred_info_get_status(PredInfo, PredStatus),
    module_info_get_type_spec_info(ModuleInfo, TypeSpecInfo),
    TypeSpecInfo = type_spec_info(_, TypeSpecForcePreds, _, _),
    ( if
        ( PredStatus = pred_status(status_exported)
        ; PredStatus = pred_status(status_opt_exported)
        ),
        not is_unify_or_compare_pred(PredInfo),

        % XXX These should be allowed, but the predicate declaration for
        % the specialized predicate is not produced before the termination
        % pragmas are read in, resulting in an undefined predicate error.
        not set.member(PredId, TypeSpecForcePreds)
    then
        pred_info_get_proc_table(PredInfo, ProcTable),
        map.foldl2(maybe_write_pragma_termination_for_proc(OrderPredInfo),
            ProcTable, !First, !IO)
    else
        true
    ).

:- pred maybe_write_pragma_termination_for_proc(order_pred_info::in,
    proc_id::in, proc_info::in, maybe_first::in, maybe_first::out,
    io::di, io::uo) is det.

maybe_write_pragma_termination_for_proc(OrderPredInfo, _ProcId, ProcInfo,
        !First, !IO) :-
    ( if proc_info_is_valid_mode(ProcInfo) then
        OrderPredInfo = order_pred_info(PredName, _PredArity, PredOrFunc,
            _PredId, PredInfo),
        ModuleName = pred_info_module(PredInfo),
        PredSymName = qualified(ModuleName, PredName),
        proc_info_declared_argmodes(ProcInfo, ArgModes),
        proc_info_get_maybe_arg_size_info(ProcInfo, ArgSize),
        proc_info_get_maybe_termination_info(ProcInfo, Termination),
        maybe_write_nl(!First, !IO),
        write_pragma_termination_info_components(output_mercury, PredOrFunc,
            PredSymName, ArgModes, ArgSize, Termination, !IO)
    else
        true
    ).

%---------------------------------------------------------------------------%

    % Write out termination2_info pragma for the procedures of a predicate if:
    %   - the predicate is exported.
    %   - the predicate is not compiler generated.
    %
:- pred write_pragma_termination2_for_pred(module_info::in,
    order_pred_info::in, maybe_first::in, maybe_first::out,
    io::di, io::uo) is det.

write_pragma_termination2_for_pred(ModuleInfo, OrderPredInfo, !First, !IO) :-
    OrderPredInfo = order_pred_info(_, _, _, PredId, PredInfo),
    pred_info_get_status(PredInfo, PredStatus),
    module_info_get_type_spec_info(ModuleInfo, TypeSpecInfo),
    TypeSpecInfo = type_spec_info(_, TypeSpecForcePreds, _, _),
    ( if
        ( PredStatus = pred_status(status_exported)
        ; PredStatus = pred_status(status_opt_exported)
        ),
        not hlds_pred.is_unify_or_compare_pred(PredInfo),
        not set.member(PredId, TypeSpecForcePreds)
    then
        pred_info_get_proc_table(PredInfo, ProcTable),
        map.foldl2(maybe_write_pragma_termination2_for_proc(OrderPredInfo),
            ProcTable, !First, !IO)
    else
        true
    ).

    % Write out a termination2_info pragma for the procedure.
    %
:- pred maybe_write_pragma_termination2_for_proc(order_pred_info::in,
    proc_id::in, proc_info::in, maybe_first::in, maybe_first::out,
    io::di, io::uo) is det.

maybe_write_pragma_termination2_for_proc(OrderPredInfo, _ProcId, ProcInfo,
        !First, !IO) :-
    ( if proc_info_is_valid_mode(ProcInfo) then
        OrderPredInfo = order_pred_info(PredName, _PredArity, PredOrFunc,
            _PredId, PredInfo),
        ModuleName = pred_info_module(PredInfo),
        PredSymName = qualified(ModuleName, PredName),

        proc_info_declared_argmodes(ProcInfo, ArgModes),
        proc_info_get_headvars(ProcInfo, HeadVars),
        proc_info_get_termination2_info(ProcInfo, Term2Info),
        MaybeSuccess = term2_info_get_success_constrs(Term2Info),
        MaybeFailure = term2_info_get_failure_constrs(Term2Info),
        MaybeTermination = term2_info_get_term_status(Term2Info),
        SizeVarMap = term2_info_get_size_var_map(Term2Info),
        HeadSizeVars = prog_vars_to_size_vars(SizeVarMap, HeadVars),

        % NOTE: If this predicate is changed, then parse_pragma.m must also
        % be changed, so that it can parse the resulting pragmas.
        % XXX Should construct a pragma, then call parse_tree_out_pragma.m.

        maybe_write_nl(!First, !IO),
        io.write_string(":- pragma termination2_info(", !IO),
        (
            PredOrFunc = pf_predicate,
            mercury_output_pred_mode_subdecl(output_mercury, varset.init,
                PredSymName, ArgModes, no, !IO)
        ;
            PredOrFunc = pf_function,
            pred_args_to_func_args(ArgModes, FuncArgModes, ReturnMode),
            mercury_output_func_mode_subdecl(output_mercury, varset.init,
                PredSymName, FuncArgModes, ReturnMode, no, !IO)
        ),

        list.length(HeadSizeVars, NumHeadSizeVars),
        HeadSizeVarIds = 0 .. NumHeadSizeVars - 1,
        map.det_insert_from_corresponding_lists(HeadSizeVars, HeadSizeVarIds,
            map.init, VarToVarIdMap),

        io.write_string(", ", !IO),
        output_maybe_constr_arg_size_info(VarToVarIdMap, MaybeSuccess, !IO),
        io.write_string(", ", !IO),
        output_maybe_constr_arg_size_info(VarToVarIdMap, MaybeFailure, !IO),
        io.write_string(", ", !IO),
        output_maybe_termination2_info(MaybeTermination, !IO),
        io.write_string(").\n", !IO)
    else
        true
    ).

:- pred output_maybe_constr_arg_size_info(map(size_var, int)::in,
    maybe(constr_arg_size_info)::in, io::di, io::uo) is det.

output_maybe_constr_arg_size_info(VarToVarIdMap, MaybeArgSizeConstrs, !IO) :-
    (
        MaybeArgSizeConstrs = no,
        io.write_string("not_set", !IO)
    ;
        MaybeArgSizeConstrs = yes(Polyhedron),
        io.write_string("constraints(", !IO),
        Constraints0 = polyhedron.non_false_constraints(Polyhedron),
        Constraints1 = list.filter(isnt(nonneg_constr), Constraints0),
        Constraints  = list.sort(Constraints1),
        OutputVar = (func(Var) = int_to_string(VarToVarIdMap ^ det_elem(Var))),
        lp_rational.output_constraints(OutputVar, Constraints, !IO),
        io.write_char(')', !IO)
    ).

:- pred output_maybe_termination2_info(maybe(constr_termination_info)::in,
    io::di, io::uo) is det.

output_maybe_termination2_info(MaybeConstrTermInfo, !IO) :-
    (
        MaybeConstrTermInfo = no,
        io.write_string("not_set", !IO)
    ;
        MaybeConstrTermInfo = yes(cannot_loop(_)),
        io.write_string("cannot_loop", !IO)
    ;
        MaybeConstrTermInfo = yes(can_loop(_)),
        io.write_string("can_loop", !IO)
    ).

%---------------------------------------------------------------------------%

:- pred write_pragma_unused_args(pragma_info_unused_args::in,
    maybe_first::in, maybe_first::out, io::di, io::uo) is det.

write_pragma_unused_args(UnusedArgInfo, !First, !IO) :-
    maybe_write_nl(!First, !IO),
    mercury_output_pragma_unused_args(UnusedArgInfo, !IO).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

should_write_exception_info(ModuleInfo, PredId, ProcId, PredInfo,
        WhatFor, ShouldWrite) :-
    ( if
        % XXX If PredInfo is not a unify or compare pred, then all its
        % procedures must share the same status.
        procedure_is_exported(ModuleInfo, PredInfo, ProcId),
        not is_unify_or_compare_pred(PredInfo),
        (
            WhatFor = for_analysis_framework
        ;
            WhatFor = for_pragma,
            module_info_get_type_spec_info(ModuleInfo, TypeSpecInfo),
            TypeSpecInfo = type_spec_info(_, TypeSpecForcePreds, _, _),
            not set.member(PredId, TypeSpecForcePreds),

            % XXX Writing out pragmas for the automatically generated class
            % instance methods causes the compiler to abort when it reads them
            % back in.
            pred_info_get_markers(PredInfo, Markers),
            not check_marker(Markers, marker_class_instance_method),
            not check_marker(Markers, marker_named_class_instance_method)
        )
    then
        ShouldWrite = should_write
    else
        ShouldWrite = should_not_write
    ).

should_write_trailing_info(ModuleInfo, PredId, ProcId, PredInfo, WhatFor,
        ShouldWrite) :-
    ( if
        % XXX If PredInfo is not a unify or compare pred, then all its
        % procedures must share the same status.
        procedure_is_exported(ModuleInfo, PredInfo, ProcId),
        not is_unify_or_compare_pred(PredInfo),
        (
            WhatFor = for_analysis_framework
        ;
            WhatFor = for_pragma,
            module_info_get_type_spec_info(ModuleInfo, TypeSpecInfo),
            TypeSpecInfo = type_spec_info(_, TypeSpecForcePreds, _, _),
            not set.member(PredId, TypeSpecForcePreds),
            %
            % XXX Writing out pragmas for the automatically generated class
            % instance methods causes the compiler to abort when it reads them
            % back in.
            %
            pred_info_get_markers(PredInfo, Markers),
            not check_marker(Markers, marker_class_instance_method),
            not check_marker(Markers, marker_named_class_instance_method)
        )
    then
        ShouldWrite = should_write
    else
        ShouldWrite = should_not_write
    ).

should_write_mm_tabling_info(ModuleInfo, PredId, ProcId, PredInfo, WhatFor,
        ShouldWrite) :-
    ( if
        % XXX If PredInfo is not a unify or compare pred, then all its
        % procedures must share the same status.
        procedure_is_exported(ModuleInfo, PredInfo, ProcId),
        not is_unify_or_compare_pred(PredInfo),
        (
            WhatFor = for_analysis_framework
        ;
            WhatFor = for_pragma,
            module_info_get_type_spec_info(ModuleInfo, TypeSpecInfo),
            TypeSpecInfo = type_spec_info(_, TypeSpecForcePreds, _, _),
            not set.member(PredId, TypeSpecForcePreds),

            % XXX Writing out pragmas for the automatically generated class
            % instance methods causes the compiler to abort when it reads them
            % back in.
            pred_info_get_markers(PredInfo, Markers),
            not check_marker(Markers, marker_class_instance_method),
            not check_marker(Markers, marker_named_class_instance_method)
        )
    then
        ShouldWrite = should_write
    else
        ShouldWrite = should_not_write
    ).

should_write_reuse_info(ModuleInfo, PredId, ProcId, PredInfo, WhatFor,
        ShouldWrite) :-
    ( if
        % XXX If PredInfo is not a unify or compare pred, then all its
        % procedures must share the same status.
        procedure_is_exported(ModuleInfo, PredInfo, ProcId),
        not is_unify_or_compare_pred(PredInfo),

        % Don't write out info for reuse versions of procedures.
        pred_info_get_origin(PredInfo, PredOrigin),
        PredOrigin \= origin_transformed(transform_structure_reuse, _, _),

        (
            WhatFor = for_analysis_framework
        ;
            WhatFor = for_pragma,
            % XXX These should be allowed, but the predicate declaration for
            % the specialized predicate is not produced before the structure
            % reuse pragmas are read in, resulting in an undefined predicate
            % error.
            module_info_get_type_spec_info(ModuleInfo, TypeSpecInfo),
            TypeSpecInfo = type_spec_info(_, TypeSpecForcePreds, _, _),
            not set.member(PredId, TypeSpecForcePreds)
        )
    then
        ShouldWrite = should_write
    else
        ShouldWrite = should_not_write
    ).

should_write_sharing_info(ModuleInfo, PredId, ProcId, PredInfo, WhatFor,
        ShouldWrite) :-
    ( if
        % XXX If PredInfo is not a unify or compare pred, then all its
        % procedures must share the same status.
        procedure_is_exported(ModuleInfo, PredInfo, ProcId),
        not is_unify_or_compare_pred(PredInfo),
        (
            WhatFor = for_analysis_framework
        ;
            WhatFor = for_pragma,
            % XXX These should be allowed, but the predicate declaration for
            % the specialized predicate is not produced before the structure
            % sharing pragmas are read in, resulting in an undefined predicate
            % error.
            module_info_get_type_spec_info(ModuleInfo, TypeSpecInfo),
            TypeSpecInfo = type_spec_info(_, TypeSpecForcePreds, _, _),
            not set.member(PredId, TypeSpecForcePreds)
        )
    then
        ShouldWrite = should_write
    else
        ShouldWrite = should_not_write
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Should a declaration with the given status be written to the `.opt' file.
    %
:- func type_status_to_write(type_status) = bool.
:- func inst_status_to_write(inst_status) = bool.
:- func mode_status_to_write(mode_status) = bool.
:- func typeclass_status_to_write(typeclass_status) = bool.
:- func instance_status_to_write(instance_status) = bool.
:- func pred_status_to_write(pred_status) = bool.

type_status_to_write(type_status(OldStatus)) =
    old_status_to_write(OldStatus).
inst_status_to_write(inst_status(InstModeStatus)) = ToWrite :-
    ToWrite = instmode_status_to_write(InstModeStatus).
mode_status_to_write(mode_status(InstModeStatus)) = ToWrite :-
    ToWrite = instmode_status_to_write(InstModeStatus).
typeclass_status_to_write(typeclass_status(OldStatus)) =
    old_status_to_write(OldStatus).
instance_status_to_write(instance_status(OldStatus)) =
    old_status_to_write(OldStatus).
pred_status_to_write(pred_status(OldStatus)) =
    old_status_to_write(OldStatus).

:- func instmode_status_to_write(new_instmode_status) = bool.

instmode_status_to_write(InstModeStatus) = ToWrite :-
    (
        InstModeStatus = instmode_defined_in_this_module(InstModeExport),
        (
            InstModeExport = instmode_export_anywhere,
            ToWrite = no
        ;
            ( InstModeExport = instmode_export_only_submodules
            ; InstModeExport = instmode_export_nowhere
            ),
            ToWrite = yes
        )
    ;
        InstModeStatus = instmode_defined_in_other_module(_),
        ToWrite = no
    ).

:- func old_status_to_write(old_import_status) = bool.

old_status_to_write(status_imported(_)) = no.
old_status_to_write(status_abstract_imported) = no.
old_status_to_write(status_pseudo_imported) = no.
old_status_to_write(status_opt_imported) = no.
old_status_to_write(status_exported) = no.
old_status_to_write(status_opt_exported) = yes.
old_status_to_write(status_abstract_exported) = yes.
old_status_to_write(status_pseudo_exported) = no.
old_status_to_write(status_exported_to_submodules) = yes.
old_status_to_write(status_local) = yes.
old_status_to_write(status_external(Status)) =
    bool.not(old_status_is_exported(Status)).

%---------------------------------------------------------------------------%

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

intermod_info_get_modules(IntermodInfo, X) :-
    X = IntermodInfo ^ im_modules.
intermod_info_get_preds(IntermodInfo, X) :-
    X = IntermodInfo ^ im_preds.
intermod_info_get_pred_decls(IntermodInfo, X) :-
    X = IntermodInfo ^ im_pred_decls.
intermod_info_get_instances(IntermodInfo, X) :-
    X = IntermodInfo ^ im_instances.
intermod_info_get_types(IntermodInfo, X) :-
    X = IntermodInfo ^ im_types.
intermod_info_get_module_info(IntermodInfo, X) :-
    X = IntermodInfo ^ im_module_info.
intermod_info_get_write_foreign_header(IntermodInfo, X) :-
    X = IntermodInfo ^ im_write_foreign_header.
intermod_info_get_var_types(IntermodInfo, X) :-
    X = IntermodInfo ^ im_var_types.
intermod_info_get_tvarset(IntermodInfo, X) :-
    X = IntermodInfo ^ im_tvarset.

intermod_info_set_modules(Modules, !IntermodInfo) :-
    !IntermodInfo ^ im_modules := Modules.
intermod_info_set_preds(Procs, !IntermodInfo) :-
    !IntermodInfo ^ im_preds := Procs.
intermod_info_set_pred_decls(ProcDecls, !IntermodInfo) :-
    !IntermodInfo ^ im_pred_decls := ProcDecls.
intermod_info_set_instances(Instances, !IntermodInfo) :-
    !IntermodInfo ^ im_instances := Instances.
intermod_info_set_types(Types, !IntermodInfo) :-
    !IntermodInfo ^ im_types := Types.
intermod_info_set_module_info(ModuleInfo, !IntermodInfo) :-
    !IntermodInfo ^ im_module_info := ModuleInfo.
intermod_info_set_write_header(!IntermodInfo) :-
    !IntermodInfo ^ im_write_foreign_header := yes.
intermod_info_set_var_types(VarTypes, !IntermodInfo) :-
    !IntermodInfo ^ im_var_types := VarTypes.
intermod_info_set_tvarset(TVarSet, !IntermodInfo) :-
    !IntermodInfo ^ im_tvarset := TVarSet.

%---------------------------------------------------------------------------%

write_trans_opt_file(ModuleInfo, !IO) :-
    module_info_get_globals(ModuleInfo, Globals),
    module_info_get_name(ModuleInfo, ModuleName),
    module_name_to_file_name(Globals, do_create_dirs, ".trans_opt.tmp",
        ModuleName, TmpOptName, !IO),
    io.open_output(TmpOptName, Result, !IO),
    (
        Result = error(Error),
        io.error_message(Error, Msg),
        io.progname_base("trans_opt.m", ProgName, !IO),
        io.write_string(ProgName, !IO),
        io.write_string(": cannot open transitive optimisation file `", !IO),
        io.write_string(TmpOptName, !IO),
        io.write_string("' \n", !IO),
        io.write_string(ProgName, !IO),
        io.write_string(": for output: ", !IO),
        io.write_string(Msg, !IO),
        io.nl(!IO),
        io.set_exit_status(1, !IO)
    ;
        Result = ok(Stream),
        io.set_output_stream(Stream, OldStream, !IO),
        io.write_string(":- module ", !IO),
        mercury_output_bracketed_sym_name(ModuleName, !IO),
        io.write_string(".\n", !IO),

        % All predicates to write global items into the .trans_opt
        % file should go here.

        % Select all the predicates for which something should be written
        % into the .trans_opt file.

        module_info_get_valid_pred_ids(ModuleInfo, PredIds),
        PredIdsSet = set.from_list(PredIds),
        module_info_get_structure_reuse_preds(ModuleInfo, ReusePredsSet),
        PredIdsNoReusePredsSet = set.difference(PredIdsSet, ReusePredsSet),
        PredIdsNoReuseVersions = set.to_sorted_list(PredIdsNoReusePredsSet),
        generate_order_pred_infos(ModuleInfo, PredIdsNoReuseVersions,
            NoReuseOrderPredInfos),

        % Don't try to output pragmas for an analysis unless that analysis
        % was actually run.
        module_info_get_proc_analysis_kinds(ModuleInfo, ProcAnalysisKinds),

        ( if set.contains(ProcAnalysisKinds, pak_termination) then
            list.foldl2(
                write_pragma_termination_for_pred(ModuleInfo),
                NoReuseOrderPredInfos, is_first, _, !IO)
        else
            true
        ),

        ( if set.contains(ProcAnalysisKinds, pak_termination2) then
            list.foldl2(
                write_pragma_termination2_for_pred(ModuleInfo),
                NoReuseOrderPredInfos, is_first, _, !IO)
        else
            true
        ),

        ( if set.contains(ProcAnalysisKinds, pak_structure_sharing) then
            list.foldl2(
                write_pragma_structure_sharing_for_pred(ModuleInfo),
                NoReuseOrderPredInfos, is_first, _, !IO)
        else
            true
        ),

        ( if set.contains(ProcAnalysisKinds, pak_structure_reuse) then
            list.foldl2(
                write_pragma_structure_reuse_for_pred(ModuleInfo),
                NoReuseOrderPredInfos, is_first, _, !IO)
        else
            true
        ),

        ( if set.contains(ProcAnalysisKinds, pak_exception) then
            list.foldl2(
                write_pragma_exceptions_for_pred(ModuleInfo),
                NoReuseOrderPredInfos, is_first, _, !IO)
        else
            true
        ),

        ( if set.contains(ProcAnalysisKinds, pak_trailing) then
            list.foldl2(
                write_pragma_trailing_info_for_pred(ModuleInfo),
                NoReuseOrderPredInfos, is_first, _, !IO)
        else
            true
        ),

        ( if set.contains(ProcAnalysisKinds, pak_mm_tabling) then
            list.foldl2(
                write_pragma_mm_tabling_info_for_pred(ModuleInfo),
                NoReuseOrderPredInfos, is_first, _, !IO)
        else
            true
        ),

        io.set_output_stream(OldStream, _, !IO),
        io.close_output(Stream, !IO),

        module_name_to_file_name(Globals, do_not_create_dirs, ".trans_opt",
            ModuleName, OptName, !IO),
        update_interface(Globals, OptName, !IO),
        touch_interface_datestamp(Globals, ModuleName, ".trans_opt_date", !IO)
    ).

%---------------------------------------------------------------------------%

maybe_opt_export_entities(!ModuleInfo) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    trace [io(!IO)] (
        maybe_write_string(VeryVerbose,
            "% Adjusting import status of predicates in the `.opt' file...",
            !IO)
    ),

    module_info_get_valid_pred_ids(!.ModuleInfo, PredIds),
    globals.lookup_int_option(Globals, intermod_inline_simple_threshold,
        Threshold),
    globals.lookup_bool_option(Globals, deforestation, Deforestation),
    globals.lookup_int_option(Globals, higher_order_size_limit,
        HigherOrderSizeLimit),
    some [!IntermodInfo] (
        init_intermod_info(!.ModuleInfo, !:IntermodInfo),
        gather_opt_export_preds(PredIds, yes, Threshold,
            HigherOrderSizeLimit, Deforestation, !IntermodInfo),
        gather_opt_export_instances(!IntermodInfo),
        gather_opt_export_types(!IntermodInfo),
        do_maybe_opt_export_entities(!.IntermodInfo, !ModuleInfo)
    ),
    trace [io(!IO)] (
        maybe_write_string(VeryVerbose, " done\n", !IO)
    ).

:- pred do_maybe_opt_export_entities(intermod_info::in,
    module_info::in, module_info::out) is det.

do_maybe_opt_export_entities(IntermodeInfo, !ModuleInfo) :-
    intermod_info_get_pred_decls(IntermodeInfo, PredDeclsSet),
    set.to_sorted_list(PredDeclsSet, PredDecls),
    opt_export_preds(PredDecls, !ModuleInfo),
    maybe_opt_export_types(!ModuleInfo),
    maybe_opt_export_classes(!ModuleInfo),
    maybe_opt_export_instances(!ModuleInfo).

%---------------------%

:- pred maybe_opt_export_types(module_info::in, module_info::out) is det.

maybe_opt_export_types(!ModuleInfo) :-
    module_info_get_type_table(!.ModuleInfo, TypeTable0),
    map_foldl_over_type_ctor_defns(maybe_opt_export_type_defn,
        TypeTable0, TypeTable, !ModuleInfo),
    module_info_set_type_table(TypeTable, !ModuleInfo).

:- pred maybe_opt_export_type_defn(type_ctor::in,
    hlds_type_defn::in, hlds_type_defn::out,
    module_info::in, module_info::out) is det.

maybe_opt_export_type_defn(TypeCtor, TypeDefn0, TypeDefn, !ModuleInfo) :-
    module_info_get_name(!.ModuleInfo, ModuleName),
    ( if should_opt_export_type_defn(ModuleName, TypeCtor, TypeDefn0) then
        hlds_data.set_type_defn_status(type_status(status_exported),
            TypeDefn0, TypeDefn),
        adjust_status_of_special_preds(TypeCtor, !ModuleInfo)
    else
        TypeDefn = TypeDefn0
    ).

:- pred adjust_status_of_special_preds((type_ctor)::in,
    module_info::in, module_info::out) is det.

adjust_status_of_special_preds(TypeCtor, ModuleInfo0, ModuleInfo) :-
    special_pred_list(SpecialPredList),
    module_info_get_special_pred_maps(ModuleInfo0, SpecPredMaps),
    list.filter_map(
        ( pred(SpecPredId::in, PredId::out) is semidet :-
            search_special_pred_maps(SpecPredMaps, SpecPredId, TypeCtor,
                PredId)
        ), SpecialPredList, PredIds),
    opt_export_preds(PredIds, ModuleInfo0, ModuleInfo).

%---------------------%

:- pred maybe_opt_export_classes(module_info::in, module_info::out) is det.

maybe_opt_export_classes(!ModuleInfo) :-
    module_info_get_class_table(!.ModuleInfo, Classes0),
    map.to_assoc_list(Classes0, ClassAL0),
    list.map_foldl(maybe_opt_export_class_defn, ClassAL0, ClassAL,
        !ModuleInfo),
    map.from_sorted_assoc_list(ClassAL, Classes),
    module_info_set_class_table(Classes, !ModuleInfo).

:- pred maybe_opt_export_class_defn(pair(class_id, hlds_class_defn)::in,
    pair(class_id, hlds_class_defn)::out,
    module_info::in, module_info::out) is det.

maybe_opt_export_class_defn(ClassId - ClassDefn0, ClassId - ClassDefn,
        !ModuleInfo) :-
    ToWrite = typeclass_status_to_write(ClassDefn0 ^ classdefn_status),
    (
        ToWrite = yes,
        ClassDefn = ClassDefn0 ^ classdefn_status :=
            typeclass_status(status_exported),
        class_procs_to_pred_ids(ClassDefn ^ classdefn_hlds_interface, PredIds),
        opt_export_preds(PredIds, !ModuleInfo)
    ;
        ToWrite = no,
        ClassDefn = ClassDefn0
    ).

:- pred class_procs_to_pred_ids(list(pred_proc_id)::in, list(pred_id)::out)
    is det.

class_procs_to_pred_ids(ClassProcs, PredIds) :-
    PredIds0 = list.map(pred_proc_id_project_pred_id, ClassProcs),
    list.sort_and_remove_dups(PredIds0, PredIds).

%---------------------%

:- pred maybe_opt_export_instances(module_info::in, module_info::out) is det.

maybe_opt_export_instances(!ModuleInfo) :-
    module_info_get_instance_table(!.ModuleInfo, Instances0),
    map.to_assoc_list(Instances0, InstanceAL0),
    list.map_foldl(maybe_opt_export_class_instances, InstanceAL0, InstanceAL,
        !ModuleInfo),
    map.from_sorted_assoc_list(InstanceAL, Instances),
    module_info_set_instance_table(Instances, !ModuleInfo).

:- pred maybe_opt_export_class_instances(
    pair(class_id, list(hlds_instance_defn))::in,
    pair(class_id, list(hlds_instance_defn))::out,
    module_info::in, module_info::out) is det.

maybe_opt_export_class_instances(ClassId - InstanceList0,
        ClassId - InstanceList, !ModuleInfo) :-
    list.map_foldl(maybe_opt_export_instance_defn, InstanceList0, InstanceList,
        !ModuleInfo).

:- pred maybe_opt_export_instance_defn(hlds_instance_defn::in,
    hlds_instance_defn::out, module_info::in, module_info::out) is det.

maybe_opt_export_instance_defn(Instance0, Instance, !ModuleInfo) :-
    Instance0 = hlds_instance_defn(InstanceModule, Types, OriginalTypes,
        InstanceStatus0, Context, Constraints, Body,
        HLDSClassInterface, TVarSet, ConstraintProofs),
    ToWrite = instance_status_to_write(InstanceStatus0),
    (
        ToWrite = yes,
        InstanceStatus = instance_status(status_exported),
        Instance = hlds_instance_defn(InstanceModule, Types, OriginalTypes,
            InstanceStatus, Context, Constraints, Body,
            HLDSClassInterface, TVarSet, ConstraintProofs),
        (
            HLDSClassInterface = yes(ClassInterface),
            class_procs_to_pred_ids(ClassInterface, PredIds),
            opt_export_preds(PredIds, !ModuleInfo)
        ;
            % This can happen if an instance has multiple
            % declarations, one of which is abstract.
            HLDSClassInterface = no
        )
    ;
        ToWrite = no,
        Instance = Instance0
    ).

%---------------------%

:- pred opt_export_preds(list(pred_id)::in,
    module_info::in, module_info::out) is det.

opt_export_preds(PredIds, !ModuleInfo) :-
    module_info_get_preds(!.ModuleInfo, Preds0),
    opt_export_preds_in_pred_table(PredIds, Preds0, Preds),
    module_info_set_preds(Preds, !ModuleInfo).

:- pred opt_export_preds_in_pred_table(list(pred_id)::in,
    pred_table::in, pred_table::out) is det.

opt_export_preds_in_pred_table([], !Preds).
opt_export_preds_in_pred_table([PredId | PredIds], !Preds) :-
    map.lookup(!.Preds, PredId, PredInfo0),
    pred_info_get_status(PredInfo0, PredStatus0),
    ToWrite = pred_status_to_write(PredStatus0),
    (
        ToWrite = yes,
        ( if
            pred_info_get_origin(PredInfo0, Origin),
            Origin = origin_special_pred(spec_pred_unify, _)
        then
            PredStatus = pred_status(status_pseudo_exported)
        else if
            PredStatus0 = pred_status(status_external(_))
        then
            PredStatus = pred_status(status_external(status_opt_exported))
        else
            PredStatus = pred_status(status_opt_exported)
        ),
        pred_info_set_status(PredStatus, PredInfo0, PredInfo),
        map.det_update(PredId, PredInfo, !Preds)
    ;
        ToWrite = no
    ),
    opt_export_preds_in_pred_table(PredIds, !Preds).

%---------------------------------------------------------------------------%
:- end_module transform_hlds.intermod.
%---------------------------------------------------------------------------%
