%------------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%------------------------------------------------------------------------------%
% Copyright (C) 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: structure_reuse.versions.m
% Main authors: nancy 
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

    % Create a copy of the predicate/procedure information specified by the
    % given pred_proc_id, and return the pred_proc_id of that copy.  This
    % operation also updates the structure_reuse_map in the HLDS. Note that the
    % copy is not altered w.r.t. structure reuse. It is a plain copy, nothing
    % more than that. 
    %
:- pred create_fresh_pred_proc_info_copy(pred_proc_id::in, pred_proc_id::out,
    module_info::in, module_info::out) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_goal.
:- import_module hlds.passes_aux.
:- import_module hlds.pred_table.
:- import_module libs.compiler_util.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_util.

:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module string.
:- import_module list.

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
    PPIds = map.keys(ReuseTable), 
    CondPPIds = list.filter(has_conditional_reuse(ReuseTable), PPIds), 
    UncondPPIds = list.filter(has_unconditional_reuse(ReuseTable), PPIds), 
    
    % Create all the duplicates: 
    list.map_foldl(create_fresh_pred_proc_info_copy, 
        CondPPIds, ReuseCondPPIds, !ModuleInfo), 

    % Process all the goals to update the reuse annotations:
    module_info_get_structure_reuse_map(!.ModuleInfo, ReuseMap), 
    list.foldl2(process_proc(ReuseMap), list.append(ReuseCondPPIds, 
        UncondPPIds), !ModuleInfo, !IO).
   
:- pred has_conditional_reuse(reuse_as_table::in, pred_proc_id::in) is semidet.

has_conditional_reuse(ReuseTable, PPId) :- 
    ReuseAs = reuse_as_table_search(PPId, ReuseTable), 
    reuse_as_conditional_reuses(ReuseAs).

:- pred has_unconditional_reuse(reuse_as_table::in, pred_proc_id::in) 
    is semidet.
has_unconditional_reuse(ReuseTable, PPId) :- 
    ReuseAs = reuse_as_table_search(PPId, ReuseTable), 
    reuse_as_all_unconditional_reuses(ReuseAs).

%------------------------------------------------------------------------------%

create_fresh_pred_proc_info_copy(PPId, NewPPId, !ModuleInfo):- 
    module_info_pred_proc_info(!.ModuleInfo, PPId, PredInfo0, ProcInfo0),
    ReusePredName = generate_reuse_name(!.ModuleInfo, PPId),
    create_fresh_pred_proc_info_copy_2(PredInfo0, ProcInfo0, ReusePredName, 
        ReusePredInfo, ReuseProcId), 

    module_info_get_predicate_table(!.ModuleInfo, PredTable0),
    predicate_table_insert(ReusePredInfo, ReusePredId, PredTable0, PredTable),
    NewPPId = proc(ReusePredId, ReuseProcId), 
    module_info_set_predicate_table(PredTable, !ModuleInfo), 

    module_info_get_structure_reuse_map(!.ModuleInfo, ReuseMap0),
    map.det_insert(ReuseMap0, PPId, NewPPId - ReusePredName, ReuseMap), 
    module_info_set_structure_reuse_map(ReuseMap, !ModuleInfo).
     
                
:- pred create_fresh_pred_proc_info_copy_2(pred_info::in, proc_info::in, 
    reuse_name::in, pred_info::out, proc_id::out) is det.

create_fresh_pred_proc_info_copy_2(PredInfo, ProcInfo, ReusePredName, 
        ReusePredInfo, ReuseProcId):-
    ModuleName = pred_info_module(PredInfo), 
    PredOrFunc = pred_info_is_pred_or_func(PredInfo), 
    pred_info_context(PredInfo, ProgContext), 
    pred_info_get_origin(PredInfo, PredOrigin), 
    pred_info_get_import_status(PredInfo, ImportStatus), 
    pred_info_get_markers(PredInfo, PredMarkers), 
    pred_info_get_arg_types(PredInfo, MerTypes), 
    pred_info_get_typevarset(PredInfo, TVarset), 
    pred_info_get_exist_quant_tvars(PredInfo, ExistQTVars), 
    pred_info_get_class_context(PredInfo, ProgConstraints), 
    pred_info_get_assertions(PredInfo, AssertIds), 
    pred_info_create(ModuleName, ReusePredName, PredOrFunc, ProgContext,
        PredOrigin, ImportStatus, PredMarkers, MerTypes, TVarset, 
        ExistQTVars, ProgConstraints, AssertIds, ProcInfo, ReuseProcId, 
        ReusePredInfo).
        
%------------------------------------------------------------------------------%

    % Process the goal of the procedure with the given pred_proc_id so that
    % all potential reuses are replaced by real reuses, and all calls to 
    % procedures that have a reuse version are replaced by calls to their
    % reuse version (if of course, that is in accordance with the reuse
    % annotations). 
    %
:- pred process_proc(structure_reuse_map::in, 
    pred_proc_id::in, module_info::in, module_info::out, 
    io::di, io::uo) is det.

process_proc(ReuseMap, PPId, !ModuleInfo, !IO):- 
    write_proc_progress_message("(reuse version) ", PPId, !.ModuleInfo, !IO),
    module_info_pred_proc_info(!.ModuleInfo, PPId, PredInfo0, ProcInfo0),
    proc_info_get_goal(ProcInfo0, Goal0), 
    process_goal(ReuseMap, Goal0, Goal, !IO),
    proc_info_set_goal(Goal, ProcInfo0, ProcInfo),
    module_info_set_pred_proc_info(PPId, PredInfo0, ProcInfo, !ModuleInfo).

:- pred process_goal(structure_reuse_map::in, hlds_goal::in, hlds_goal::out, 
    io::di, io::uo) is det.

process_goal(ReuseMap, !Goal, !IO) :- 
    !.Goal = GoalExpr0 - GoalInfo0, 
    (
        GoalExpr0 = conj(ConjType, Goals0),
        list.map_foldl(process_goal(ReuseMap), Goals0, Goals, !IO), 
        GoalExpr = conj(ConjType, Goals),
        !:Goal = GoalExpr - GoalInfo0
    ;
        GoalExpr0 = call(CalleePredId, CalleeProcId, A, B, C, CalleePredName),
        ReuseDescription0 = goal_info_get_reuse(GoalInfo0), 
        (
            % If the reuse description already sais "reuse", then this is
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
            GoalExpr = call(ReuseCalleePredId, ReuseCalleeProcId, A, B, C, 
                ReuseCalleePredName), 
            !:Goal = GoalExpr - GoalInfo0
        ;
            ReuseDescription0 = potential_reuse(reuse_call(CondDescr))
        ->
            % Replace the call to the reuse version, and change the
            % potential reuse annotation to a real annotation. 
            determine_reuse_version(ReuseMap, CalleePredId, CalleeProcId,
                CalleePredName, ReuseCalleePredId, ReuseCalleeProcId, 
                ReuseCalleePredName), 
            GoalExpr = call(ReuseCalleePredId, ReuseCalleeProcId, A, B, C, 
                ReuseCalleePredName), 
            ReuseDescription = reuse(reuse_call(CondDescr)), 
            goal_info_set_reuse(ReuseDescription, GoalInfo0, GoalInfo), 
            !:Goal = GoalExpr - GoalInfo
        ;
            true
        )
    ;
        GoalExpr0 = generic_call(_, _, _, _)
    ;
        GoalExpr0 = unify(_, _, _, _, _),
        ReuseDescription0 = goal_info_get_reuse(GoalInfo0),
        (
            ReuseDescription0 = potential_reuse(Descr)
        ->
            ReuseDescription = reuse(Descr),
            goal_info_set_reuse(ReuseDescription, GoalInfo0, GoalInfo),
            !:Goal = GoalExpr0 - GoalInfo
        ;
            true
        )
    ;
        GoalExpr0 = disj(Goals0),
        list.map_foldl(process_goal(ReuseMap), Goals0, Goals, !IO),
        GoalExpr = disj(Goals),
        !:Goal = GoalExpr - GoalInfo0
    ;
        GoalExpr0 = switch(A, B, Cases0),
        list.map_foldl(process_case(ReuseMap), Cases0, Cases, !IO),
        GoalExpr = switch(A, B, Cases),
        !:Goal = GoalExpr - GoalInfo0
    ;
        % XXX To check and compare with the theory. 
        GoalExpr0 = not(_Goal)
    ;
        GoalExpr0 = scope(A, SubGoal0),
        process_goal(ReuseMap, SubGoal0, SubGoal, !IO),
        GoalExpr = scope(A, SubGoal),
        !:Goal = GoalExpr - GoalInfo0
    ;
        GoalExpr0 = if_then_else(A, IfGoal0, ThenGoal0, ElseGoal0),
        process_goal(ReuseMap, IfGoal0, IfGoal, !IO), 
        process_goal(ReuseMap, ThenGoal0, ThenGoal, !IO), 
        process_goal(ReuseMap, ElseGoal0, ElseGoal, !IO), 
        GoalExpr = if_then_else(A, IfGoal, ThenGoal, ElseGoal),
        !:Goal = GoalExpr - GoalInfo0
    ;
        GoalExpr0 = foreign_proc(_Attrs, _ForeignPredId, _ForeignProcId,
            _ForeignArgs, _, _)
    ;
        GoalExpr0 = shorthand(_),
        unexpected(this_file, "process_goal: shorthand goal.")
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

:- pred process_case(structure_reuse_map::in, case::in, case::out, 
    io::di, io::uo) is det.

process_case(ReuseMap, !Case, !IO) :- 
    !.Case = case(ConsId, Goal0), 
    process_goal(ReuseMap, Goal0, Goal, !IO),
    !:Case = case(ConsId, Goal).

%------------------------------------------------------------------------------%
:- func this_file = string.
this_file = "structure_reuse.versions.m".

:- end_module structure_reuse.versions.
%------------------------------------------------------------------------------%
