%-----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: structure_reuse.versions.m.
% Main authors: nancy.
%
% Provide the functionality to create optimised versions of those procedures
% for which reuse was detected.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.ctgc.structure_reuse.versions.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module transform_hlds.ctgc.structure_reuse.domain.

%-----------------------------------------------------------------------------%

    % For each of the entries in the reuse table:
    %
    % * if the listed reuse is conditional, then duplicate the
    %   pred-info/proc-info of the original procedure, changing all potential
    %   reuse annotation to real reuses;
    % * if the listed reuse is unconditional, then no duplication is needed,
    %   yet the goal needs to be traversed to correctly replace all
    %   procedure calls to calls to reuse versions whenever needed.
    % * if the listed reuse is "no reuse", then obviously, nothing needs to
    %   be done.
    %
    % This process updates the module information by adding the new predicates,
    % and recording the pred-proc-id to the reuse pred-proc-id mappings in
    % module_info.
    %
:- pred create_reuse_procedures(reuse_as_table::in, reuse_as_table::out,
    module_info::in, module_info::out) is det.

    % Create a copy of the predicate/procedure information specified by the
    % given pred_proc_id, and return the pred_proc_id of that copy.  The copy
    % is not altered w.r.t. structure reuse. It is a plain copy, nothing more
    % than that.
    %
:- pred create_fresh_pred_proc_info_copy(pred_proc_id::in, no_clobber_args::in,
    pred_proc_id::out, module_info::in, module_info::out) is det.

    % Create a fake reuse procedure that simply calls the non-reuse procedure.
    %
:- pred create_fake_reuse_procedure(pred_proc_id::in, no_clobber_args::in,
    module_info::in, module_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.mode_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.passes_aux.
:- import_module hlds.pred_table.
:- import_module hlds.quantification.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.op_mode.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_util.
:- import_module transform_hlds.ctgc.structure_reuse.analysis.

:- import_module bimap.
:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set.

%-----------------------------------------------------------------------------%

:- type reuse_name == sym_name.

:- func generate_reuse_name(module_info, pred_proc_id, list(int)) = reuse_name.

generate_reuse_name(ModuleInfo, PPId, NoClobbers) = ReuseName :-
    PPId = proc(_, ProcId),
    module_info_pred_proc_info(ModuleInfo, PPId, PredInfo, _ProcInfo),
    PredModule = pred_info_module(PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    PredName = pred_info_name(PredInfo),
    proc_id_to_int(ProcId, ProcInt),
    make_pred_name(PredModule, "ctgc", yes(PredOrFunc), PredName,
        newpred_structure_reuse(ProcInt, NoClobbers), ReuseName).

%-----------------------------------------------------------------------------%

create_reuse_procedures(!ReuseTable, !ModuleInfo) :-
    % This process can be split into separate steps:
    % - determine all the pred-proc-ids of procedure with conditional reuse;
    % - create duplicates of these procedures;
    % - traverse all these procedures + the procedures with unconditional reuse
    %   to correctly update the reuse annotations.

    % Get the list of conditional reuse procedures already created.
    ExistingReusePPIds = bimap.coordinates(!.ReuseTable ^ reuse_version_map),
    ExistingReusePPIdsSet = set.list_to_set(ExistingReusePPIds),

    map.foldl2(divide_reuse_procs(ExistingReusePPIdsSet),
        !.ReuseTable ^ reuse_info_map, [], CondOrigPPIds, [], UncondOrigPPIds),

    % Create duplicates of the procedures which have conditional reuse.  The
    % "intermediate" reuse procedures will already have been created during the
    % analysis, so this creates just the reuse versions where all possible
    % arguments are potentially reusable.
    list.map_foldl2(maybe_create_full_reuse_proc_copy,
        CondOrigPPIds, ReuseCondPPIds, !ModuleInfo, !ReuseTable),

    % Process all the goals to update the reuse annotations.  In the reuse
    % versions of procedures we can take advantage of potential reuse
    % opportunities.
    list.foldl(check_cond_process_proc(convert_potential_reuse, !.ReuseTable),
        ReuseCondPPIds, !ModuleInfo),
    list.foldl(check_cond_process_proc(convert_potential_reuse, !.ReuseTable),
        ExistingReusePPIds, !ModuleInfo),

    % In the original procedures, only the unconditional reuse opportunities
    % can be taken.
    list.foldl(process_proc(leave_potential_reuse, !.ReuseTable),
        CondOrigPPIds, !ModuleInfo),
    list.foldl(process_proc(leave_potential_reuse, !.ReuseTable),
        UncondOrigPPIds, !ModuleInfo).

    % Separate procedures in the reuse table into those with some conditional
    % reuse opportunities, and those with only unconditional reuse.
    % Skip any procedure which is already a reuse copy of another procedure.
    %
:- pred divide_reuse_procs(set(pred_proc_id)::in,
    pred_proc_id::in, reuse_as_and_status::in,
    list(pred_proc_id)::in, list(pred_proc_id)::out,
    list(pred_proc_id)::in, list(pred_proc_id)::out) is det.

divide_reuse_procs(ExistingReusePPIdsSet, PPId, ReuseAs_Status,
        !CondPPIds, !UncondPPIds) :-
    ReuseAs_Status = reuse_as_and_status(ReuseAs, _),
    ( if set.contains(ExistingReusePPIdsSet, PPId) then
        true
    else if reuse_as_conditional_reuses(ReuseAs) then
        !:CondPPIds = [PPId | !.CondPPIds]
    else if reuse_as_all_unconditional_reuses(ReuseAs) then
        !:UncondPPIds = [PPId | !.UncondPPIds]
    else if reuse_as_no_reuses(ReuseAs) then
        true
    else
        unexpected($pred, "conditions failed")
    ).

:- pred maybe_create_full_reuse_proc_copy(pred_proc_id::in, pred_proc_id::out,
    module_info::in, module_info::out, reuse_as_table::in, reuse_as_table::out)
    is det.

maybe_create_full_reuse_proc_copy(PPId, NewPPId, !ModuleInfo, !ReuseTable) :-
    NoClobbers = [],
    ( if
        reuse_as_table_search_reuse_version_proc(!.ReuseTable,
            PPId, NoClobbers, _)
    then
        unexpected($pred, "procedure already exists")
    else
        true
    ),
    create_fresh_pred_proc_info_copy(PPId, NoClobbers, NewPPId, !ModuleInfo),
    ( if reuse_as_table_search(!.ReuseTable, PPId, ReuseAs_Status) then
        reuse_as_table_set(NewPPId, ReuseAs_Status, !ReuseTable),
        reuse_as_table_insert_reuse_version_proc(PPId, NoClobbers, NewPPId,
            !ReuseTable)
    else
        unexpected($pred, "no reuse information")
    ).

%-----------------------------------------------------------------------------%

create_fresh_pred_proc_info_copy(PPId, NoClobbers, NewPPId, !ModuleInfo) :-
    module_info_pred_proc_info(!.ModuleInfo, PPId, PredInfo0, ProcInfo0),
    ReusePredName = generate_reuse_name(!.ModuleInfo, PPId, NoClobbers),
    PPId = proc(PredId, _),
    create_fresh_pred_proc_info_copy_2(PredId, PredInfo0, ProcInfo0,
        ReusePredName, ReusePredInfo, ReuseProcId),

    NewPPId = proc(ReusePredId, ReuseProcId),

    module_info_get_predicate_table(!.ModuleInfo, PredTable0),
    predicate_table_insert(ReusePredInfo, ReusePredId, PredTable0, PredTable),
    module_info_set_predicate_table(PredTable, !ModuleInfo),

    module_info_get_structure_reuse_preds(!.ModuleInfo, ReusePreds0),
    set.insert(ReusePredId, ReusePreds0, ReusePreds),
    module_info_set_structure_reuse_preds(ReusePreds, !ModuleInfo).

:- pred create_fresh_pred_proc_info_copy_2(pred_id::in, pred_info::in,
    proc_info::in, reuse_name::in, pred_info::out, proc_id::out) is det.

create_fresh_pred_proc_info_copy_2(PredId, PredInfo, ProcInfo, ReusePredName,
        ReusePredInfo, ReuseProcId) :-
    ModuleName = pred_info_module(PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    pred_info_get_context(PredInfo, ProgContext),
    pred_info_get_origin(PredInfo, PredOrigin),
    pred_info_get_status(PredInfo, PredStatus0),
    % If the predicate was opt_imported then the specialised copy should be
    % local, otherwise it will be eliminated by dead proc elimination.
    ( if PredStatus0 = pred_status(status_opt_imported) then
        PredStatus = pred_status(status_local)
    else
        PredStatus = PredStatus0
    ),
    pred_info_get_markers(PredInfo, PredMarkers),
    pred_info_get_arg_types(PredInfo, MerTypes),
    pred_info_get_typevarset(PredInfo, TVarset),
    pred_info_get_exist_quant_tvars(PredInfo, ExistQTVars),
    pred_info_get_class_context(PredInfo, ProgConstraints),
    pred_info_get_assertions(PredInfo, AssertIds),
    pred_info_get_var_name_remap(PredInfo, VarNameRemap),
    NewPredOrigin = origin_transformed(transform_structure_reuse, PredOrigin,
        PredId),
    pred_info_create(ModuleName, ReusePredName, PredOrFunc, ProgContext,
        NewPredOrigin, PredStatus, PredMarkers, MerTypes, TVarset,
        ExistQTVars, ProgConstraints, AssertIds, VarNameRemap,
        ProcInfo, ReuseProcId, ReusePredInfo).

%-----------------------------------------------------------------------------%

:- type convert_potential_reuse
    --->    convert_potential_reuse
    ;       leave_potential_reuse.

    % When generating target code, we may find a set of reuse conditions on a
    % procedure which are *harsher* than the reuse conditions that we found
    % during the `--make-analysis-registry' step.  This can happen due extra
    % analysis information gathered for other modules in the meantime.  In that
    % case, we may have external callers to the procedure which have verified
    % only against the *laxer* reuse conditions.
    %
    % Hence we need to be careful that we don't generate any code which
    % violates the weaker reuse conditions.  One (conservative) way to do that
    % is to ignore all the potential reuse annotations and only use the
    % unconditional reuse annotations.
    %
    % XXX the same problem occurs with `--intermodule-optimisation'
    %
:- pred check_cond_process_proc(convert_potential_reuse::in,
    reuse_as_table::in, pred_proc_id::in, module_info::in, module_info::out)
    is det.

check_cond_process_proc(ConvertPotentialReuse, ReuseTable, ReusePPId,
        !ModuleInfo) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, intermodule_analysis,
        IntermodAnalysis),
    globals.get_op_mode(Globals, OpMode),
    ( if
        IntermodAnalysis = yes,
        OpMode \= opm_top_args(opma_augment(opmau_make_analysis_registry))
    then
        structure_reuse_answer_harsher_than_in_analysis_registry(!.ModuleInfo,
            ReuseTable, ReusePPId, IsHarsher)
    else
        IsHarsher = no
    ),
    (
        IsHarsher = yes,
        % Ignoring potential reuse is equivalent to having only unconditional
        % structure reuse.
        process_proc(leave_potential_reuse, ReuseTable, ReusePPId, !ModuleInfo)
    ;
        IsHarsher = no,
        process_proc(ConvertPotentialReuse, ReuseTable, ReusePPId, !ModuleInfo)
    ).

    % Process the goal of the procedure with the given pred_proc_id so that
    % all potential reuses are replaced by real reuses, and all calls to
    % procedures that have a reuse version are replaced by calls to their
    % reuse version (if of course, that is in accordance with the reuse
    % annotations).
    %
:- pred process_proc(convert_potential_reuse::in, reuse_as_table::in,
    pred_proc_id::in, module_info::in, module_info::out) is det.

process_proc(ConvertPotentialReuse, ReuseTable, PPId, !ModuleInfo) :-
    trace [io(!IO)] (
        write_proc_progress_message("(reuse version) ", PPId, !.ModuleInfo,
            !IO)
    ),
    some [!ProcInfo] (
        module_info_pred_proc_info(!.ModuleInfo, PPId, PredInfo0, !:ProcInfo),
        pred_info_get_status(PredInfo0, PredStatus),
        ( if PredStatus = pred_status(status_imported(_)) then
            % The bodies may contain junk, so don't try to process.
            true
        else
            proc_info_get_goal(!.ProcInfo, Goal0),
            process_goal(ConvertPotentialReuse, ReuseTable, !.ModuleInfo,
                Goal0, Goal),
            proc_info_set_goal(Goal, !ProcInfo),

            % A dead variable needs to appear in the non-local set of the
            % construction unification in which its space is reused, so we
            % requantify.  Then we recompute instmap deltas with the updated
            % non-local sets.
            requantify_proc_general(ordinary_nonlocals_no_lambda, !ProcInfo),
            recompute_instmap_delta_proc(
                do_not_recompute_atomic_instmap_deltas,
                !ProcInfo, !ModuleInfo),
            module_info_set_pred_proc_info(PPId, PredInfo0, !.ProcInfo,
                !ModuleInfo)
        )
    ).

:- pred process_goal(convert_potential_reuse::in, reuse_as_table::in,
    module_info::in, hlds_goal::in, hlds_goal::out) is det.

process_goal(ConvertPotentialReuse, ReuseTable, ModuleInfo, !Goal) :-
    !.Goal = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = conj(ConjType, Goals0),
        list.map(process_goal(ConvertPotentialReuse, ReuseTable, ModuleInfo),
            Goals0, Goals),
        GoalExpr = conj(ConjType, Goals),
        !:Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = plain_call(CalleePredId, CalleeProcId, Args, BI, UC,
            CalleePredName),
        ReuseDescription0 = goal_info_get_reuse(GoalInfo0),
        ( if
            % If the reuse description already says "reuse", then this is
            % a call to a procedure which might have specified conditions, yet
            % whose conditions are always met, hence do not imply conditions on
            % the procedure in which this call appears. We must therefore
            % make sure to call the appropriate version of the called
            % procedure.
            ReuseDescription0 = reuse(reuse_call(_CondDescr, NoClobbers))
        then
            determine_reuse_version(ReuseTable, ModuleInfo, CalleePredId,
                CalleeProcId, CalleePredName, NoClobbers, ReuseCalleePredId,
                ReuseCalleeProcId, ReuseCalleePredName),
            GoalExpr = plain_call(ReuseCalleePredId, ReuseCalleeProcId,
                Args, BI, UC, ReuseCalleePredName),
            !:Goal = hlds_goal(GoalExpr, GoalInfo0)
        else if
            ReuseDescription0 = potential_reuse(reuse_call(CondDescr,
                NoClobbers)),
            ConvertPotentialReuse = convert_potential_reuse
        then
            ConvertPotentialReuse = convert_potential_reuse,
            % Replace the call to the reuse version, and change the
            % potential reuse annotation to a real annotation.
            determine_reuse_version(ReuseTable, ModuleInfo,
                CalleePredId, CalleeProcId, CalleePredName, NoClobbers,
                ReuseCalleePredId, ReuseCalleeProcId, ReuseCalleePredName),
            GoalExpr = plain_call(ReuseCalleePredId, ReuseCalleeProcId,
                Args, BI, UC, ReuseCalleePredName),
            ReuseDescription = reuse(reuse_call(CondDescr, NoClobbers)),
            goal_info_set_reuse(ReuseDescription, GoalInfo0, GoalInfo),
            !:Goal = hlds_goal(GoalExpr, GoalInfo)
        else
            true
        )
    ;
        GoalExpr0 = generic_call(_, _, _, _, _)
    ;
        GoalExpr0 = unify(_, _, _, Unification0, _),
        ReuseDescription0 = goal_info_get_reuse(GoalInfo0),
        (
            (
                ReuseDescription0 = reuse(Descr)
            ;
                ReuseDescription0 = potential_reuse(Descr),
                ConvertPotentialReuse = convert_potential_reuse
            ),
            ReuseDescription = reuse(Descr),
            unification_set_reuse(Descr, Unification0, Unification),
            GoalExpr = GoalExpr0 ^ unify_kind := Unification,
            goal_info_set_reuse(ReuseDescription, GoalInfo0, GoalInfo),
            !:Goal = hlds_goal(GoalExpr, GoalInfo)
        ;
            ReuseDescription0 = potential_reuse(_),
            ConvertPotentialReuse = leave_potential_reuse
        ;
            ReuseDescription0 = no_reuse_info
        ;
            ReuseDescription0 = no_possible_reuse
        ;
            ReuseDescription0 = missed_reuse(_)
        )
    ;
        GoalExpr0 = disj(Goals0),
        list.map(process_goal(ConvertPotentialReuse, ReuseTable, ModuleInfo),
            Goals0, Goals),
        GoalExpr = disj(Goals),
        !:Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = switch(A, B, Cases0),
        list.map(process_case(ConvertPotentialReuse, ReuseTable, ModuleInfo),
            Cases0, Cases),
        GoalExpr = switch(A, B, Cases),
        !:Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        % XXX To check and compare with the theory.
        GoalExpr0 = negation(_Goal)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        ( if Reason = from_ground_term(_, from_ground_term_construct) then
            true
        else
            process_goal(ConvertPotentialReuse, ReuseTable, ModuleInfo,
                SubGoal0, SubGoal),
            GoalExpr = scope(Reason, SubGoal),
            !:Goal = hlds_goal(GoalExpr, GoalInfo0)
        )
    ;
        GoalExpr0 = if_then_else(Vars, IfGoal0, ThenGoal0, ElseGoal0),
        process_goal(ConvertPotentialReuse, ReuseTable, ModuleInfo,
            IfGoal0, IfGoal),
        process_goal(ConvertPotentialReuse, ReuseTable, ModuleInfo,
            ThenGoal0, ThenGoal),
        process_goal(ConvertPotentialReuse, ReuseTable, ModuleInfo,
            ElseGoal0, ElseGoal),
        GoalExpr = if_then_else(Vars, IfGoal, ThenGoal, ElseGoal),
        !:Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = call_foreign_proc(_Attrs, _ForeignPredId, _ForeignProcId,
            _Args, _ExtraArgs, _MaybeTraceRuntimeCond, _Impl)
    ;
        GoalExpr0 = shorthand(_),
        % These should have been expanded out by now.
        unexpected($pred, "shorthand")
    ).

:- pred unification_set_reuse(short_reuse_description::in,
    unification::in, unification::out) is det.

unification_set_reuse(ShortReuseDescription, !Unification) :-
    ( if
        HowToConstruct0 = !.Unification ^ construct_how,
        ShortReuseDescription = cell_reused(DeadVar, _, PossibleConsIds,
            CellsToUpdate)
    then
        (
            HowToConstruct0 = construct_statically(_)
            % Leave static terms as-is.
        ;
            ( HowToConstruct0 = construct_dynamically
            ; HowToConstruct0 = construct_in_region(_)
            ; HowToConstruct0 = reuse_cell(_)
            ),
            CellToReuse = cell_to_reuse(DeadVar, PossibleConsIds,
                CellsToUpdate),
            HowToConstruct = reuse_cell(CellToReuse),
            !Unification ^ construct_how := HowToConstruct
        )
    else
        true
    ).

:- pred determine_reuse_version(reuse_as_table::in, module_info::in,
    pred_id::in, proc_id::in, sym_name::in, list(int)::in,
    pred_id::out, proc_id::out, reuse_name::out) is det.

determine_reuse_version(ReuseTable, ModuleInfo, PredId, ProcId, PredName,
        NoClobbers, ReusePredId, ReuseProcId, ReusePredName) :-
    ( if
        reuse_as_table_search_reuse_version_proc(ReuseTable,
            proc(PredId, ProcId), NoClobbers, Result)
    then
        Result = proc(ReusePredId, ReuseProcId),
        module_info_pred_info(ModuleInfo, ReusePredId, ReusePredInfo),
        ModuleName = pred_info_module(ReusePredInfo),
        Name = pred_info_name(ReusePredInfo),
        ReusePredName = qualified(ModuleName, Name)
    else
        ReusePredId = PredId,
        ReuseProcId = ProcId,
        ReusePredName = PredName
    ).

:- pred process_case(convert_potential_reuse::in, reuse_as_table::in,
    module_info::in, case::in, case::out) is det.

process_case(ConvertPotentialReuse, ReuseMap, ModuleInfo, Case0, Case) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    process_goal(ConvertPotentialReuse, ReuseMap, ModuleInfo, Goal0, Goal),
    Case = case(MainConsId, OtherConsIds, Goal).

%-----------------------------------------------------------------------------%

create_fake_reuse_procedure(PPId, NoClobbers, !ModuleInfo) :-
    PPId = proc(PredId, ProcId),
    module_info_pred_proc_info(!.ModuleInfo, PPId, OldPredInfo, OldProcInfo),
    OldPredModule = pred_info_module(OldPredInfo),
    OldPredName = pred_info_name(OldPredInfo),
    proc_info_interface_determinism(OldProcInfo, Determinism),

    create_fresh_pred_proc_info_copy(PPId, NoClobbers, NewPPId, !ModuleInfo),
    some [!PredInfo, !ProcInfo] (
        module_info_pred_proc_info(!.ModuleInfo, NewPPId,
            !:PredInfo, !:ProcInfo),
        proc_info_get_goal(!.ProcInfo, Body),
        Body = hlds_goal(_, GoalInfo0),
        proc_info_get_headvars(!.ProcInfo, HeadVars),
        GoalExpr = plain_call(PredId, ProcId, HeadVars, not_builtin, no,
            qualified(OldPredModule, OldPredName)),
        goal_info_set_determinism(Determinism, GoalInfo0, GoalInfo),
        Goal = hlds_goal(GoalExpr, GoalInfo),
        proc_info_set_goal(Goal, !ProcInfo),
        module_info_set_pred_proc_info(NewPPId, !.PredInfo, !.ProcInfo,
            !ModuleInfo)
    ).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.ctgc.structure_reuse.versions.
%-----------------------------------------------------------------------------%
