%------------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%------------------------------------------------------------------------------%
% Copyright (C) 2006-2008 The University of Melbourne.
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
%------------------------------------------------------------------------------%

:- module structure_reuse.versions.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module transform_hlds.ctgc.structure_reuse.domain.

:- import_module io.

%------------------------------------------------------------------------------%


    % For each of the entries in the reuse table:
    % * if the listed reuse is conditional, then duplicate the
    % pred-info/proc-info of the original procedure, changing all potential
    % reuse annotation to real reuses;
    % * if the listed reuse is unconditional, then no duplication is needed,
    % yet the goal needs to be traversed to correctly replace all
    % procedure calls to calls to reuse versions whenever needed.
    % * if the listed reuse is "no reuse", then obviously, nothing needs to
    % be done.
    %
    % This process updates the module information by adding the new predicates,
    % and recording the pred-proc-id to the reuse pred-proc-id mappings in
    % module_info.
    %
:- pred create_reuse_procedures(reuse_as_table::in, module_info::in,
    module_info::out, io::di, io::uo) is det.

    % Create a fake reuse procedure that simply calls the non-reuse procedure.
    %
:- pred create_fake_reuse_procedure(pred_proc_id::in, module_info::in,
    module_info::out) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.mode_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.passes_aux.
:- import_module hlds.pred_table.
:- import_module hlds.quantification.
:- import_module libs.compiler_util.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_util.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.

%------------------------------------------------------------------------------%

:- type reuse_name == sym_name.

:- func generate_reuse_name(module_info, pred_proc_id) = reuse_name.

generate_reuse_name(ModuleInfo, PPId) = ReuseName :-
    module_info_pred_proc_info(ModuleInfo, PPId, PredInfo, _ProcInfo),
    PPId = proc(_, ProcId),
    Line = 0,
    Counter = proc_id_to_int(ProcId),
    make_pred_name_with_context(pred_info_module(PredInfo), "ctgc",
        pred_info_is_pred_or_func(PredInfo), pred_info_name(PredInfo),
        Line, Counter, ReuseName).

%------------------------------------------------------------------------------%

    % This process can be split into separate steps:
    % - determine all the pred-proc-ids of procedure with conditional reuse;
    % - create duplicates of these procedures (and record the mapping in
    %   the structure_reuse_map in module_info);
    % - traverse all these procedures + the procedures with unconditional reuse
    %   to correctly update the reuse annotations.
    %
create_reuse_procedures(ReuseTable, !ModuleInfo, !IO):-
    map.foldl2(divide_reuse_procs, ReuseTable, [], CondPPIds, [], UncondPPIds),

    % Create duplicates of the procedures which have conditional reuse.
    list.map_foldl(create_fresh_pred_proc_info_copy,
        CondPPIds, ReuseCondPPIds, !ModuleInfo),

    % Process all the goals to update the reuse annotations.  In the reuse
    % versions of procedures we can take advantage of potential reuse
    % opportunities.
    module_info_get_structure_reuse_map(!.ModuleInfo, ReuseMap),
    list.foldl2(process_proc(convert_potential_reuse, ReuseMap),
        ReuseCondPPIds, !ModuleInfo, !IO),

    % In the original procedures, only the unconditional reuse opportunities
    % can be taken.
    list.foldl2(process_proc(leave_potential_reuse, ReuseMap),
        CondPPIds, !ModuleInfo, !IO),
    list.foldl2(process_proc(leave_potential_reuse, ReuseMap),
        UncondPPIds, !ModuleInfo, !IO).

    % Separate procedures in the reuse table into those with some conditional
    % reuse opportunities, and those with only unconditional reuse.
    %
:- pred divide_reuse_procs(pred_proc_id::in, reuse_as_and_status::in,
    list(pred_proc_id)::in, list(pred_proc_id)::out,
    list(pred_proc_id)::in, list(pred_proc_id)::out) is det.

divide_reuse_procs(PPId, ReuseAs_Status, !CondPPIds, !UncondPPIds) :-
    ReuseAs_Status = reuse_as_and_status(ReuseAs, _),
    ( reuse_as_conditional_reuses(ReuseAs) ->
        !:CondPPIds = [PPId | !.CondPPIds]
    ; reuse_as_all_unconditional_reuses(ReuseAs) ->
        !:UncondPPIds = [PPId | !.UncondPPIds]
    ; reuse_as_no_reuses(ReuseAs) ->
        true
    ;
        unexpected(this_file, "divide_reuse_procs")
    ).

%------------------------------------------------------------------------------%

    % Create a copy of the predicate/procedure information specified by the
    % given pred_proc_id, and return the pred_proc_id of that copy.  This
    % operation also updates the structure_reuse_map in the HLDS. Note that the
    % copy is not altered w.r.t. structure reuse. It is a plain copy, nothing
    % more than that.
    %
:- pred create_fresh_pred_proc_info_copy(pred_proc_id::in, pred_proc_id::out,
    module_info::in, module_info::out) is det.

create_fresh_pred_proc_info_copy(PPId, NewPPId, !ModuleInfo) :-
    module_info_pred_proc_info(!.ModuleInfo, PPId, PredInfo0, ProcInfo0),
    ReusePredName = generate_reuse_name(!.ModuleInfo, PPId),
    PPId = proc(PredId, _),
    create_fresh_pred_proc_info_copy_2(PredId, PredInfo0, ProcInfo0,
        ReusePredName, ReusePredInfo, ReuseProcId),

    module_info_get_predicate_table(!.ModuleInfo, PredTable0),
    predicate_table_insert(ReusePredInfo, ReusePredId, PredTable0, PredTable),
    NewPPId = proc(ReusePredId, ReuseProcId),
    module_info_set_predicate_table(PredTable, !ModuleInfo),

    module_info_get_structure_reuse_map(!.ModuleInfo, ReuseMap0),
    map.det_insert(ReuseMap0, PPId, NewPPId - ReusePredName, ReuseMap),
    module_info_set_structure_reuse_map(ReuseMap, !ModuleInfo).

:- pred create_fresh_pred_proc_info_copy_2(pred_id::in, pred_info::in,
    proc_info::in, reuse_name::in, pred_info::out, proc_id::out) is det.

create_fresh_pred_proc_info_copy_2(PredId, PredInfo, ProcInfo, ReusePredName,
        ReusePredInfo, ReuseProcId) :-
    ModuleName = pred_info_module(PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    pred_info_get_context(PredInfo, ProgContext),
    pred_info_get_origin(PredInfo, PredOrigin),
    pred_info_get_import_status(PredInfo, ImportStatus0),
    % If the predicate was opt_imported then the specialised copy should be
    % local otherwise it will be eliminated by dead proc elimination.
    ( ImportStatus0 = status_opt_imported ->
        ImportStatus = status_local
    ;
        ImportStatus = ImportStatus0
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
        NewPredOrigin, ImportStatus, PredMarkers, MerTypes, TVarset,
        ExistQTVars, ProgConstraints, AssertIds, VarNameRemap,
        ProcInfo, ReuseProcId, ReusePredInfo).

%------------------------------------------------------------------------------%

:- type convert_potential_reuse
    --->    convert_potential_reuse
    ;       leave_potential_reuse.

    % Process the goal of the procedure with the given pred_proc_id so that
    % all potential reuses are replaced by real reuses, and all calls to
    % procedures that have a reuse version are replaced by calls to their
    % reuse version (if of course, that is in accordance with the reuse
    % annotations).
    %
:- pred process_proc(convert_potential_reuse::in, structure_reuse_map::in,
    pred_proc_id::in, module_info::in, module_info::out,
    io::di, io::uo) is det.

process_proc(ConvertPotentialReuse, ReuseMap, PPId, !ModuleInfo, !IO) :-
    write_proc_progress_message("(reuse version) ", PPId, !.ModuleInfo, !IO),
    some [!ProcInfo] (
        module_info_pred_proc_info(!.ModuleInfo, PPId, PredInfo0, !:ProcInfo),
        pred_info_get_import_status(PredInfo0, ImportStatus),
        ( ImportStatus = status_imported(_) ->
            % Don't process the bodies of imported predicates.
            true
        ;
            proc_info_get_goal(!.ProcInfo, Goal0),
            process_goal(ConvertPotentialReuse, ReuseMap, Goal0, Goal, !IO),
            proc_info_set_goal(Goal, !ProcInfo),

            % A dead variable needs to appear in the non-local set of the
            % construction unification in which its space is reused, so we
            % requantify.  Then we recompute instmap deltas with the updated
            % non-local sets.
            requantify_proc(!ProcInfo),
            recompute_instmap_delta_proc(do_not_recompute_atomic_instmap_deltas,
                !ProcInfo, !ModuleInfo),
            module_info_set_pred_proc_info(PPId, PredInfo0, !.ProcInfo,
                !ModuleInfo)
        )
    ).

:- pred process_goal(convert_potential_reuse::in, structure_reuse_map::in,
    hlds_goal::in, hlds_goal::out, io::di, io::uo) is det.

process_goal(ConvertPotentialReuse, ReuseMap, !Goal, !IO) :-
    !.Goal = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = conj(ConjType, Goals0),
        list.map_foldl(process_goal(ConvertPotentialReuse, ReuseMap),
            Goals0, Goals, !IO),
        GoalExpr = conj(ConjType, Goals),
        !:Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = plain_call(CalleePredId, CalleeProcId, Args, BI, UC,
            CalleePredName),
        ReuseDescription0 = goal_info_get_reuse(GoalInfo0),
        (
            % If the reuse description already says "reuse", then this is
            % a call to a procedure which might have specified conditions, yet
            % whose conditions are always met, hence do not imply conditions on
            % the procedure in which this call appears. We must therefore
            % make sure to call the appropriate version of the called
            % procedure.
            ReuseDescription0 = reuse(reuse_call(_CondDescr))
        ->
            determine_reuse_version(ReuseMap, CalleePredId, CalleeProcId,
                CalleePredName, ReuseCalleePredId, ReuseCalleeProcId,
                ReuseCalleePredName),
            GoalExpr = plain_call(ReuseCalleePredId, ReuseCalleeProcId,
                Args, BI, UC, ReuseCalleePredName),
            !:Goal = hlds_goal(GoalExpr, GoalInfo0)
        ;
            ReuseDescription0 = potential_reuse(reuse_call(CondDescr)),
            ConvertPotentialReuse = convert_potential_reuse
        ->
            % Replace the call to the reuse version, and change the
            % potential reuse annotation to a real annotation.
            determine_reuse_version(ReuseMap, CalleePredId, CalleeProcId,
                CalleePredName, ReuseCalleePredId, ReuseCalleeProcId,
                ReuseCalleePredName),
            GoalExpr = plain_call(ReuseCalleePredId, ReuseCalleeProcId,
                Args, BI, UC, ReuseCalleePredName),
            ReuseDescription = reuse(reuse_call(CondDescr)),
            goal_info_set_reuse(ReuseDescription, GoalInfo0, GoalInfo),
            !:Goal = hlds_goal(GoalExpr, GoalInfo)
        ;
            true
        )
    ;
        GoalExpr0 = generic_call(_, _, _, _)
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
            ReuseDescription0 = missed_reuse(_)
        )
    ;
        GoalExpr0 = disj(Goals0),
        list.map_foldl(process_goal(ConvertPotentialReuse, ReuseMap),
            Goals0, Goals, !IO),
        GoalExpr = disj(Goals),
        !:Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = switch(A, B, Cases0),
        list.map_foldl(process_case(ConvertPotentialReuse, ReuseMap),
            Cases0, Cases, !IO),
        GoalExpr = switch(A, B, Cases),
        !:Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        % XXX To check and compare with the theory.
        GoalExpr0 = negation(_Goal)
    ;
        GoalExpr0 = scope(A, SubGoal0),
        process_goal(ConvertPotentialReuse, ReuseMap, SubGoal0, SubGoal, !IO),
        GoalExpr = scope(A, SubGoal),
        !:Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = if_then_else(A, IfGoal0, ThenGoal0, ElseGoal0),
        process_goal(ConvertPotentialReuse, ReuseMap, IfGoal0, IfGoal, !IO),
        process_goal(ConvertPotentialReuse, ReuseMap, ThenGoal0, ThenGoal,
            !IO),
        process_goal(ConvertPotentialReuse, ReuseMap, ElseGoal0, ElseGoal,
            !IO),
        GoalExpr = if_then_else(A, IfGoal, ThenGoal, ElseGoal),
        !:Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = call_foreign_proc(_Attrs, _ForeignPredId, _ForeignProcId,
            _Args, _ExtraArgs, _MaybeTraceRuntimeCond, _Impl)
    ;
        GoalExpr0 = shorthand(_),
        % These should have been expanded out by now.
        unexpected(this_file, "process_goal: shorthand goal.")
    ).

:- pred unification_set_reuse(short_reuse_description::in,
    unification::in, unification::out) is det.

unification_set_reuse(ShortReuseDescription, !Unification) :-
    (
        !.Unification = construct(A, B, C, D, _HowToConstruct, F, G),
        ShortReuseDescription = cell_reused(DeadVar, _, PossibleConsIds,
            CellsToUpdate)
    ->
        CellToReuse = cell_to_reuse(DeadVar, PossibleConsIds,
            CellsToUpdate),
        HowToConstruct = reuse_cell(CellToReuse),
        !:Unification = construct(A, B, C, D, HowToConstruct, F, G)
    ;
        true
    ).

:- pred determine_reuse_version(structure_reuse_map::in, pred_id::in,
    proc_id::in, sym_name::in, pred_id::out, proc_id::out,
    reuse_name::out) is det.

determine_reuse_version(ReuseMap, PredId, ProcId, PredName,
        ReusePredId, ReuseProcId, ReusePredName) :-
    ( map.search(ReuseMap, proc(PredId, ProcId), Result) ->
        Result = proc(ReusePredId, ReuseProcId) - ReusePredName
    ;
        ReusePredId = PredId,
        ReuseProcId = ProcId,
        ReusePredName = PredName
    ).

:- pred process_case(convert_potential_reuse::in, structure_reuse_map::in,
    case::in, case::out, io::di, io::uo) is det.

process_case(ConvertPotentialReuse, ReuseMap, Case0, Case, !IO) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    process_goal(ConvertPotentialReuse, ReuseMap, Goal0, Goal, !IO),
    Case = case(MainConsId, OtherConsIds, Goal).

%------------------------------------------------------------------------------%

create_fake_reuse_procedure(PPId, !ModuleInfo) :-
    PPId = proc(PredId, ProcId),
    module_info_pred_proc_info(!.ModuleInfo, PPId, OldPredInfo, OldProcInfo),
    OldPredModule = pred_info_module(OldPredInfo),
    OldPredName = pred_info_name(OldPredInfo),
    proc_info_interface_determinism(OldProcInfo, Determinism),

    create_fresh_pred_proc_info_copy(PPId, NewPPId, !ModuleInfo),
    some [!PredInfo, !ProcInfo] (
        module_info_pred_proc_info(!.ModuleInfo, NewPPId, !:PredInfo,
            !:ProcInfo),
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

%------------------------------------------------------------------------------%

:- func this_file = string.

this_file = "structure_reuse.versions.m".

%------------------------------------------------------------------------------%
:- end_module structure_reuse.versions.
%------------------------------------------------------------------------------%
