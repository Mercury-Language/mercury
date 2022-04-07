%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2007-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: rbmm.region_transformation.m
% Main author: quan.
%
% This module annotates the HLDS with region information.
% The region information includes:
% - Add extra region arguments (to pass regions around) to procedures and
%   calls.
% - Update how_to_construct of construction unifications so that we can
%   construct terms in a region.
% - Add region builtin calls (defined in region_builtin.m).
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.rbmm.region_transformation.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module transform_hlds.rbmm.points_to_info.
:- import_module transform_hlds.rbmm.region_arguments.
:- import_module transform_hlds.rbmm.region_instruction.
:- import_module transform_hlds.rbmm.region_resurrection_renaming.

:- import_module map.

%-----------------------------------------------------------------------------%

    % Represent mapping from region name to a program variable that
    % represents the region.
    %
:- type name_to_prog_var_table == map(pred_proc_id, name_to_prog_var).
:- type name_to_prog_var == map(string, prog_var).

    % The names of the predicates for creating and removing regions.
    % The predicates are in region_builtin library module.
    %
:- func create_region_pred_name = string.
:- func remove_region_pred_name = string.

    % Besides changing the HLDS with region information, this predicate also
    % returns a mapping from a region name to a program variable which
    % represents the region. We will only create a new program variable for a
    % region name which is not yet in the map. This information is needed when
    % computing rbmm_goal_info. Note that because we only pass as argument to
    % a procedure a region that will be allocated into, be removed, or be
    % created in the procedure, some regions in the rpt graph of the procedure
    % may not need to be presented by program variables. They are the regions
    % that only be read from in the procedure. Therefore there are NO entries
    % for them in the region_name-to-prog_var map.
    %
:- pred region_transform(rpta_info_table::in,
    proc_formal_region_args_table::in, proc_pp_actual_region_args_table::in,
    rbmm_renaming_table::in, rbmm_renaming_table::in, region_instr_table::in,
    rbmm_renaming_annotation_table::in, rbmm_renaming_annotation_table::in,
    name_to_prog_var_table::in, name_to_prog_var_table::out,
    module_info::in, module_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.purity.
:- import_module check_hlds.recompute_instmap_deltas.
:- import_module check_hlds.type_util.
:- import_module hlds.goal_form.
:- import_module hlds.goal_path.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.instmap.
:- import_module hlds.pred_table.
:- import_module hlds.quantification.
:- import_module hlds.vartypes.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.set_of_var.
:- import_module transform_hlds.rbmm.points_to_graph.
:- import_module transform_hlds.smm_common.

:- import_module int.
:- import_module list.
:- import_module require.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

create_region_pred_name = "create_region".
remove_region_pred_name = "remove_region".

region_transform(RptaInfoTable, FormalRegionArgTable, ActualRegionArgTable,
        ResurRenamingTable, IteRenamingTable, RegionInstructionTable,
        ResurRenamingAnnoTable, IteRenamingAnnoTable, !NameToVarTable,
        !ModuleInfo) :-
    map.foldl2(annotate_pred, FormalRegionArgTable, [], PredIds, !ModuleInfo),
    list.foldl2(region_transform_pred(RptaInfoTable, FormalRegionArgTable,
        ActualRegionArgTable, ResurRenamingTable, IteRenamingTable,
        RegionInstructionTable, ResurRenamingAnnoTable, IteRenamingAnnoTable),
        PredIds, !NameToVarTable, !ModuleInfo),

    % We can only do the followings when all the procedures have been
    % annotated with region information and recorded. This is necessary
    % because recompute_instmap_delta_proc and repuritycheck_proc need to
    % look up information about the (annotated) called procedures.
    list.foldl(update_instmap_delta_pred, PredIds, !ModuleInfo),
    list.foldl(recheck_purity_pred, PredIds, !ModuleInfo).

    % This predicate updates pred_info structure. The following information
    % is updated:
    %
    % 1. Original arity: orig_arity, which is updated with the old value +
    %    the number of region arguments.
    % 2. Argument types: arg_types, updated with region type for region
    %    arguments.
    %
:- pred annotate_pred(pred_proc_id::in, region_args::in,
    list(pred_id)::in, list(pred_id)::out,
    module_info::in, module_info::out) is det.

annotate_pred(PPId, FormalRegionArgs, !Processed, !ModuleInfo) :-
    PPId = proc(PredId, _),
    ( if list.member(PredId, !.Processed) then
        true
    else
        some [!PredInfo] (
            module_info_pred_info(!.ModuleInfo, PredId, !:PredInfo),
            FormalRegionArgs = region_args(Constants, Deads, Borns),
            NumberOfRegArgs = list.length(Constants) + list.length(Deads) +
                list.length(Borns),
            Arity = pred_info_orig_arity(!.PredInfo),
            pred_info_set_orig_arity(Arity + NumberOfRegArgs, !PredInfo),

            list.duplicate(NumberOfRegArgs, region_type, RegionTypes),
            pred_info_get_arg_types(!.PredInfo, TypeVarSet, ExistQuantTVars,
                ArgTypes0),
            PredOrFunc = pred_info_is_pred_or_func(!.PredInfo),
            (
                PredOrFunc = pf_predicate,
                ArgTypes = ArgTypes0 ++ RegionTypes
            ;
                PredOrFunc = pf_function,
                % The output of function is always at the last.
                list.det_split_last(ArgTypes0, BeforeLast, Last),
                ArgTypes = BeforeLast ++ RegionTypes ++ [Last]
            ),
            pred_info_set_arg_types(TypeVarSet, ExistQuantTVars, ArgTypes,
                !PredInfo),
            module_info_set_pred_info(PredId, !.PredInfo, !ModuleInfo)
        ),
        !:Processed = [PredId | !.Processed]
    ).

    % This predicate transforms the procedures of a predicate.
    %
:- pred region_transform_pred(rpta_info_table::in,
    proc_formal_region_args_table::in,
    proc_pp_actual_region_args_table::in,
    rbmm_renaming_table::in, rbmm_renaming_table::in, region_instr_table::in,
    rbmm_renaming_annotation_table::in, rbmm_renaming_annotation_table::in,
    pred_id::in, name_to_prog_var_table::in, name_to_prog_var_table::out,
    module_info::in, module_info::out) is det.

region_transform_pred(RptaInfoTable, FormalRegionArgTable,
        ActualRegionArgTable, ResurRenamingTable, IteRenamingTable,
        RegionInstructionTable, ResurRenamingAnnoTable, IteRenamingAnnoTable,
        PredId, !NameToVarTable, !ModuleInfo) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_valid_non_imported_procids(PredInfo),
    list.foldl2(
        region_transform_proc(RptaInfoTable,
            FormalRegionArgTable, ActualRegionArgTable,
            ResurRenamingTable, IteRenamingTable,
            RegionInstructionTable, ResurRenamingAnnoTable,
            IteRenamingAnnoTable, PredId),
        ProcIds, !NameToVarTable, !ModuleInfo).

    % This predicate updates the proc_info data structure, representing
    % a procedure.
    %
    % - Introduce new variables for regions (and their region types).
    % - Update headvars with region arguments (types and modes).
    % - Update the body
    %   + region instructions,
    %   + actual region arguments at call sites,
    %   + how_to_construct at construction unifications.
    % - *Requantify* the annotated proc.
    %
    % As said above, we will recompute instmap delta, recheck purity for
    % this annotated procedure after all the procedures have been transformed.
    %
:- pred region_transform_proc(rpta_info_table::in,
    proc_formal_region_args_table::in, proc_pp_actual_region_args_table::in,
    rbmm_renaming_table::in, rbmm_renaming_table::in, region_instr_table::in,
    rbmm_renaming_annotation_table::in, rbmm_renaming_annotation_table::in,
    pred_id::in, proc_id::in, name_to_prog_var_table::in,
    name_to_prog_var_table::out, module_info::in, module_info::out) is det.

region_transform_proc(RptaInfoTable, FormalRegionArgTable,
        ActualRegionArgTable, ResurRenamingTable, IteRenamingTable,
        RegionInstructionTable, ResurRenamingAnnoTable, IteRenamingAnnoTable,
        PredId, ProcId, !NameToVarTable, !ModuleInfo) :-
    PPId = proc(PredId, ProcId),
    module_info_pred_proc_info(!.ModuleInfo, PPId, PredInfo0, ProcInfo0),
    fill_goal_path_slots_in_proc(!.ModuleInfo, ProcInfo0, ProcInfo1),
    proc_info_get_varset_vartypes(ProcInfo1, VarSet0, VarTypes0),
    proc_info_get_headvars(ProcInfo1, HeadVars0),
    proc_info_get_argmodes(ProcInfo1, ActualArgModes0),
    proc_info_get_goal(ProcInfo1, Goal0),
    map.lookup(RptaInfoTable, PPId, rpta_info(Graph, _)),
    map.lookup(FormalRegionArgTable, PPId, FormalRegionArgProc),
    map.lookup(ActualRegionArgTable, PPId, ActualRegionArgProc),
    ( if map.search(ResurRenamingTable, PPId, ResurRenamingProc0) then
        ResurRenamingProc = ResurRenamingProc0,
        map.lookup(ResurRenamingAnnoTable, PPId, ResurRenamingAnnoProc)
    else
        ResurRenamingProc = map.init,
        ResurRenamingAnnoProc = map.init
    ),
    ( if map.search(IteRenamingTable, PPId, IteRenamingProc0) then
        IteRenamingProc = IteRenamingProc0,
        map.lookup(IteRenamingAnnoTable, PPId, IteRenamingAnnoProc)
    else
        IteRenamingProc = map.init,
        IteRenamingAnnoProc = map.init
    ),
    map.lookup(RegionInstructionTable, PPId, RegionInstructionProc),
    NameToVar0 = map.init,
    annotate_proc(!.ModuleInfo, PredInfo0, Graph, FormalRegionArgProc,
        ActualRegionArgProc, ResurRenamingProc, IteRenamingProc,
        RegionInstructionProc, ResurRenamingAnnoProc, IteRenamingAnnoProc,
        VarSet0, _, VarTypes0, _, HeadVars0, _, ActualArgModes0, _,
        Goal0, _, NameToVar0, NameToVar, ProcInfo1, ProcInfo2),
    requantify_proc_general(ordinary_nonlocals_no_lambda, ProcInfo2, ProcInfo),
    module_info_set_pred_proc_info(PPId, PredInfo0, ProcInfo, !ModuleInfo),
    map.det_insert(PPId, NameToVar, !NameToVarTable).

    % Currently for a procedure we annotate the following information:
    %
    % 1. VarSet with region variables
    % 2. VarTypes with region variables and their types
    % 3. HeadVars with formal region arguments
    % 4. ActualHeadModes with the modes for region variables
    % 5. Body:
    %    + new region arguments at calls
    %    + new calls to region instructions
    %
:- pred annotate_proc(module_info::in, pred_info::in, rpt_graph::in,
    region_args::in, pp_actual_region_args_table::in, rbmm_renaming_proc::in,
    rbmm_renaming_proc::in, region_instr_proc::in,
    rbmm_renaming_annotation_proc::in, rbmm_renaming_annotation_proc::in,
    prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out, list(prog_var)::in, list(prog_var)::out,
    list(mer_mode)::in, list(mer_mode)::out, hlds_goal::in, hlds_goal::out,
    name_to_prog_var::in, name_to_prog_var::out, proc_info::in,
    proc_info::out) is det.

annotate_proc(ModuleInfo, PredInfo, Graph, FormalRegionArgProc,
        ActualRegionArgProc, ResurRenamingProc, IteRenamingProc,
        RegionInstructionProc, ResurRenamingAnnoProc, IteRenamingAnnoProc,
        !VarSet, !VarTypes, !HeadVars, !ActualArgModes, !Goal,
        !NameToVar, !ProcInfo) :-
    region_transform_goal(ModuleInfo, Graph, ResurRenamingProc,
        IteRenamingProc, ActualRegionArgProc,
        RegionInstructionProc, ResurRenamingAnnoProc, IteRenamingAnnoProc,
        !Goal, !NameToVar, !VarSet, !VarTypes),

    % Add extra variables to the head of the procedure that correspond
    % to the introduced region arguments.
    % They are added as follows.
    %
    % For predicates:
    %   <OrigArgs> ==> <OrigArgs> <InputRegionArgs> <OutputRegionArgs>
    %
    % For functions:
    %   <OrigArgs> <RetVal> ==>
    %       <OrigArgs> <InputRegionArgs> <OutputRegionArgs><RetVal>
    %
    % Note that formal region arguments are not subjected to renaming.
    %
    % Along with the extra arguments we also add extra modes for them.

    FormalRegionArgProc = region_args(Constants, Deads, Borns),
    FormalInputNodes = Constants ++ Deads,
    FormalNodes = FormalInputNodes ++ Borns,
    list.map_foldl3(node_to_var(Graph), FormalNodes, FormalRegionArgs,
        !NameToVar, !VarSet, !VarTypes),

    InMode = in_mode,
    OutMode = out_mode,
    list.duplicate(list.length(FormalInputNodes), InMode, InModes),
    list.duplicate(list.length(Borns), OutMode, OutModes),

    % Notice that the output of a function needs to be the last argument.
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    (
        PredOrFunc = pf_predicate,
        !:HeadVars = !.HeadVars ++ FormalRegionArgs,
        !:ActualArgModes = !.ActualArgModes ++ InModes ++ OutModes
    ;
        PredOrFunc = pf_function,
        list.det_split_last(!.HeadVars, BeforeLastHeadVar, LastHeadVar),
        !:HeadVars = BeforeLastHeadVar ++ FormalRegionArgs ++ [LastHeadVar],
        list.det_split_last(!.ActualArgModes, BeforeLastHeadMode,
            LastHeadMode),
        !:ActualArgModes = BeforeLastHeadMode ++ InModes
            ++ OutModes ++ [LastHeadMode]
    ),

    proc_info_set_varset_vartypes(!.VarSet, !.VarTypes, !ProcInfo),
    proc_info_set_goal(!.Goal, !ProcInfo),
    proc_info_set_headvars(!.HeadVars, !ProcInfo),
    proc_info_set_argmodes(!.ActualArgModes, !ProcInfo).

    % Basically, we will turn this atomic goal and all the region annotations
    % attached to (before and after) it into a conjunction. When there is
    % no annotation, the goal is just transformed and returned.
    % If the newly created conjunction is a conjunct of a compounding
    % conjunction then it will be flattened.
    %
    % Note: When both renamings (for resurrection and if-then-else) of a
    % region exist at a program point, we will apply the resurrection one.
    % This is due to the fact that when reasonning about what renaming is
    % needed for if-then-else we have taken into account the changes
    % caused by renaming and annotations needed for resurrection problem.
    %
:- pred region_transform_goal(module_info::in, rpt_graph::in,
    rbmm_renaming_proc::in, rbmm_renaming_proc::in,
    pp_actual_region_args_table::in, region_instr_proc::in,
    rbmm_renaming_annotation_proc::in, rbmm_renaming_annotation_proc::in,
    hlds_goal::in, hlds_goal::out, name_to_prog_var::in, name_to_prog_var::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

region_transform_goal(ModuleInfo, Graph, ResurRenamingProc, IteRenamingProc,
        ActualRegionArgProc, RegionInstructionProc, ResurRenamingAnnoProc,
        IteRenamingAnnoProc, !Goal, !NameToVar, !VarSet, !VarTypes) :-
    !.Goal = hlds_goal(GoalExpr0, GoalInfo0),
    HasSubGoals = goal_expr_has_subgoals(GoalExpr0),
    (
        HasSubGoals = does_not_have_subgoals,
        ProgPoint = program_point_init(GoalInfo0),
        ProgPoint = pp(Context, _),
        find_renamings_at_prog_point(ResurRenamingProc, IteRenamingProc,
            ProgPoint, ResurRenaming, IteRenaming),

        % Depending on the expression, this call will annotate
        % - a call with actual region arguments,
        % - a construction unification with a region to construct in.
        region_transform_goal_expr(ModuleInfo, Graph, ResurRenaming,
            IteRenaming, ActualRegionArgProc, ProgPoint, GoalExpr0, GoalExpr,
            GoalInfo0, GoalInfo, !NameToVar, !VarSet, !VarTypes),

        % Assignment unifications due to ite renaming.
        assignments_from_ite_renaming_anno(IteRenamingAnnoProc, ProgPoint,
            !NameToVar, !VarSet, !VarTypes, [], IteRenamingAssignments),

        % Region instructions before and after this program point.
        ( if
            map.search(RegionInstructionProc, ProgPoint,
                instrs_before_after(Before, After))
        then
            % Region instructions before this program point.
            list.foldl4(region_instruction_to_conj_before(ModuleInfo, Context,
                ResurRenaming, IteRenaming), Before, !NameToVar,
                !VarSet, !VarTypes, IteRenamingAssignments, Conjs1),

            % The goal at this program point itself.
            Conjs2 = Conjs1 ++ [hlds_goal(GoalExpr, GoalInfo)],

            % Region instructions after this program point.
            list.foldl4(region_instruction_to_conj(ModuleInfo, Context,
                ResurRenaming, IteRenaming), After, !NameToVar,
                !VarSet, !VarTypes, Conjs2, Conjs3)
        else
            % The goal at this program point itself.
            Conjs3 = IteRenamingAssignments ++ [hlds_goal(GoalExpr, GoalInfo)]
        ),

        % Assignment unifications due to region resurrection renaming.
        assignments_from_resur_renaming_anno(ResurRenamingAnnoProc,
            ProgPoint, IteRenaming, !NameToVar, !VarSet, !VarTypes,
            Conjs3, Conjs),

        ( if Conjs = [_, _ | _] then
            !:Goal = hlds_goal(conj(plain_conj, Conjs), GoalInfo)
        else
            !:Goal = hlds_goal(GoalExpr, GoalInfo)
        )
    ;
        HasSubGoals = has_subgoals,
        region_transform_compound_goal(ModuleInfo, Graph,
            ResurRenamingProc, IteRenamingProc, ActualRegionArgProc,
            RegionInstructionProc, ResurRenamingAnnoProc,
            IteRenamingAnnoProc, !Goal, !NameToVar, !VarSet, !VarTypes)
    ).

:- pred region_transform_goal_expr(module_info::in, rpt_graph::in,
    rbmm_renaming::in, rbmm_renaming::in, pp_actual_region_args_table::in,
    program_point::in, hlds_goal_expr::in, hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out, name_to_prog_var::in,
    name_to_prog_var::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out) is det.

    % Annotate procedure calls with actual region arguments.
    %
region_transform_goal_expr(ModuleInfo, Graph, ResurRenaming, IteRenaming,
        ActualRegionArgProc, ProgPoint, !GoalExpr, !GoalInfo,
        !NameToVar, !VarSet, !VarTypes) :-
    !.GoalExpr = plain_call(CalleePredId, CalleeProcId, Args0, Builtin,
        Context, Name),
    % XXX Callee may be a builtin or an imported procedure that we have
    % not analysed, we just ignore such a call for now.
    ( if map.search(ActualRegionArgProc, ProgPoint, ActualNodes0) then
        ActualNodes = ActualNodes0
    else
        ActualNodes = region_args([], [], [])
    ),
    ActualNodes = region_args(Constants, Ins, Outs),
    AllNodes = Constants ++ Ins ++ Outs,
    list.map_foldl3(
        node_to_var_with_both_renamings(Graph, ResurRenaming, IteRenaming),
        AllNodes, ActualRegionArgs, !NameToVar, !VarSet, !VarTypes),
    module_info_pred_info(ModuleInfo, CalleePredId, CalleePredInfo),
    CalleePredOrFunc = pred_info_is_pred_or_func(CalleePredInfo),
    (
        CalleePredOrFunc = pf_predicate,
        Args = Args0 ++ ActualRegionArgs
    ;
        CalleePredOrFunc = pf_function,
        % The output of function is always at the last.
        list.det_split_last(Args0, BeforeLast, Last),
        Args = BeforeLast ++ ActualRegionArgs ++ [Last]
    ),

    !:GoalExpr = plain_call(CalleePredId, CalleeProcId, Args, Builtin,
        Context, Name).

    % Annotate construction unifications with regions to construct in.
    %
region_transform_goal_expr(ModuleInfo, Graph, ResurRenaming, IteRenaming,
        _, _, !GoalExpr, !GoalInfo, !NameToVar, !VarSet, !VarTypes) :-
    !.GoalExpr = unify(LHS, RHS, Mode, Unification0, Context),
    annotate_constructions_unification(ModuleInfo, Graph,
        ResurRenaming, IteRenaming, Unification0, Unification,
        !NameToVar, !VarSet, !VarTypes),
    !:GoalExpr = unify(LHS, RHS, Mode, Unification, Context).

region_transform_goal_expr(_, _, _, _, _, _, !GoalExpr, !GoalInfo, !NameToVar,
        !VarSet, !VarTypes) :-
    !.GoalExpr = generic_call(_, _, _, _, _),
    sorry($pred, "generic call").

region_transform_goal_expr(_, _, _, _, _, _, !GoalExpr, !GoalInfo, !NameToVar,
        !VarSet, !VarTypes) :-
    !.GoalExpr = call_foreign_proc(_, _, _, _, _, _, _),
    sorry($pred, "call_foreign_proc").

region_transform_goal_expr(_, _, _, _, _, _, !GoalExpr, !GoalInfo, !NameToVar,
        !VarSet, !VarTypes) :-
    ( !.GoalExpr = conj(_, [])
    ; !.GoalExpr = disj([])
    ).

region_transform_goal_expr(_, _, _, _, _, _, !GoalExpr, !GoalInfo, !NameToVar,
        !VarSet, !VarTypes) :-
    ( !.GoalExpr = conj(_, [_ | _])
    ; !.GoalExpr = disj([_ | _])
    ; !.GoalExpr = if_then_else(_, _, _, _)
    ; !.GoalExpr = negation(_)
    ; !.GoalExpr = switch(_, _, _)
    ; !.GoalExpr = scope(_, _)
    ; !.GoalExpr = shorthand(_)
    ),
    unexpected($pred, "compound goal").

    % Because an atomic goal is turned into a conjunction, we need to
    % flatten its compounding conjunction if it is in one.
:- pred region_transform_compound_goal(module_info::in, rpt_graph::in,
    rbmm_renaming_proc::in, rbmm_renaming_proc::in,
    pp_actual_region_args_table::in, region_instr_proc::in,
    rbmm_renaming_annotation_proc::in, rbmm_renaming_annotation_proc::in,
    hlds_goal::in, hlds_goal::out, name_to_prog_var::in, name_to_prog_var::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

region_transform_compound_goal(ModuleInfo, Graph,
        ResurRenamingProc, IteRenamingProc, ActualRegionArgProc,
        RegionInstructionProc, ResurRenamingAnnoProc, IteRenamingAnnoProc,
        hlds_goal(!.GoalExpr, !.GoalInfo), hlds_goal(!:GoalExpr, !:GoalInfo),
        !NameToVar, !VarSet, !VarTypes) :-
    (
        !.GoalExpr = conj(ConjType, [Conj0 | Conjs0]),
        list.map_foldl3(region_transform_goal(ModuleInfo, Graph,
            ResurRenamingProc, IteRenamingProc,
            ActualRegionArgProc, RegionInstructionProc,
            ResurRenamingAnnoProc, IteRenamingAnnoProc),
            [Conj0 | Conjs0], Conjs1, !NameToVar, !VarSet, !VarTypes),
        flatten_conj(Conjs1, Conjs),
        !:GoalExpr = conj(ConjType, Conjs)
    ;
        !.GoalExpr = disj([Disj0 | Disjs0]),
        list.map_foldl3(region_transform_goal(ModuleInfo, Graph,
            ResurRenamingProc, IteRenamingProc,
            ActualRegionArgProc, RegionInstructionProc,
            ResurRenamingAnnoProc, IteRenamingAnnoProc),
            [Disj0 | Disjs0], Disjs, !NameToVar, !VarSet, !VarTypes),
        !:GoalExpr = disj(Disjs)
    ;
        !.GoalExpr = switch(Var, CanFail, Cases0),
        list.map_foldl3(region_transform_case(ModuleInfo, Graph,
            ResurRenamingProc, IteRenamingProc,
            ActualRegionArgProc, RegionInstructionProc,
            ResurRenamingAnnoProc, IteRenamingAnnoProc,
            hlds_goal(!.GoalExpr, !.GoalInfo)),
            Cases0, Cases, !NameToVar, !VarSet, !VarTypes),
        !:GoalExpr = switch(Var, CanFail, Cases)
    ;
        !.GoalExpr = negation(Goal0),
        region_transform_goal(ModuleInfo, Graph, ResurRenamingProc,
            IteRenamingProc, ActualRegionArgProc, RegionInstructionProc,
            ResurRenamingAnnoProc, IteRenamingAnnoProc, Goal0, Goal,
            !NameToVar, !VarSet, !VarTypes),
        !:GoalExpr = negation(Goal)
    ;
        !.GoalExpr = scope(Reason0, Goal0),
        % XXX We should special-case the handling of from_ground_term_construct
        % scopes.
        % qph: A safe but potentially inefficient way is to turn these scopes
        % into from_ground_term_other, i.e., we expect that some region
        % instructions are added in these scopes. This expectation seems
        % reasonable because a region is often created before a heap
        % allocation.
        % zs: This is very inefficient. Scopes that construct ground terms
        % construct STATIC ground terms. Since these are not created
        % dynamically, they cannot possibly refer to any dynamically created
        % region, so RBMM transformations should ignore them.
        ( if Reason0 = from_ground_term(Var, _Kind) then
            Reason = from_ground_term(Var, from_ground_term_other)
        else
            Reason = Reason0
        ),
        region_transform_goal(ModuleInfo, Graph, ResurRenamingProc,
            IteRenamingProc, ActualRegionArgProc, RegionInstructionProc,
            ResurRenamingAnnoProc, IteRenamingAnnoProc, Goal0, Goal,
            !NameToVar, !VarSet, !VarTypes),
        !:GoalExpr = scope(Reason, Goal)
    ;
        !.GoalExpr = if_then_else(Vars, Cond0, Then0, Else0),
        region_transform_goal(ModuleInfo, Graph, ResurRenamingProc,
            IteRenamingProc, ActualRegionArgProc, RegionInstructionProc,
            ResurRenamingAnnoProc, IteRenamingAnnoProc, Cond0, Cond,
            !NameToVar, !VarSet, !VarTypes),
        region_transform_goal(ModuleInfo, Graph, ResurRenamingProc,
            IteRenamingProc, ActualRegionArgProc, RegionInstructionProc,
            ResurRenamingAnnoProc, IteRenamingAnnoProc, Then0, Then,
            !NameToVar, !VarSet, !VarTypes),
        region_transform_goal(ModuleInfo, Graph, ResurRenamingProc,
            IteRenamingProc, ActualRegionArgProc, RegionInstructionProc,
            ResurRenamingAnnoProc, IteRenamingAnnoProc, Else0, Else,
            !NameToVar, !VarSet, !VarTypes),
        !:GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ;
        ( !.GoalExpr = shorthand(_)
        ; !.GoalExpr = unify(_, _, _, _, _)
        ; !.GoalExpr = plain_call(_, _, _, _, _, _)
        ; !.GoalExpr = generic_call(_, _, _, _, _)
        ; !.GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ; !.GoalExpr = conj(_, [])
        ; !.GoalExpr = disj([])
        ),
        unexpected($pred, "shorthand or atomic goal")
    ).

    % This predicate needs to be consistent with what are done in
    % unify_gen.m, i.e., we will change how_to_construct to
    % construct_in_region(RegVar) only when the term is actually
    % stored in the heap.
    % The current implementation may not be correct.
    %
:- pred annotate_constructions_unification(module_info::in, rpt_graph::in,
    rbmm_renaming::in, rbmm_renaming::in, unification::in, unification::out,
    name_to_prog_var::in, name_to_prog_var::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

annotate_constructions_unification(ModuleInfo, Graph, ResurRenaming,
        IteRenaming, !Unification, !NameToVar, !VarSet, !VarTypes) :-
    !.Unification = construct(Var, ConsId, Args, ArgModes, _HowToConstruct0,
        IsUnique, SubInfo),
    rptg_get_node_by_variable(Graph, Var, Node),
    NodeType = rptg_lookup_node_type(Graph, Node),
    ( if type_not_stored_in_region(NodeType, ModuleInfo) then
        true
    else
        Name = rptg_lookup_region_name(Graph, Node),
        region_name_to_var_with_both_renamings(Name, ResurRenaming,
            IteRenaming, RegVar, !NameToVar, !VarSet, !VarTypes),
        HowToConstruct = construct_in_region(RegVar),
        !:Unification = construct(Var, ConsId, Args, ArgModes,
            HowToConstruct, IsUnique, SubInfo)
    ).

annotate_constructions_unification(_, _, _, _, !Unification, !VarSet,
        !VarTypes, !NameToVar) :-
    (
        ( !.Unification = deconstruct(_, _, _, _, _, _)
        ; !.Unification = assign(_, _)
        ; !.Unification = simple_test(_, _)
        )
    ;
        !.Unification = complicated_unify(_, _, _),
        unexpected($pred, "complicated unify")
    ).

    % The process here is related to the way we treat the unifications
    % between the switch vars and a constant or a functor of arity zero.
    % For more information about the treatment, see rbmm.execution_path.m.
    % These unifications are not explicitly present in the goal but we
    % still need to insert annotations derived for them into the goal.
    % Therefore we will make a conjunction of the annotations attached to an
    % implicit unification. We transform the goal separately. Then we make
    % another conjunction of the conjunction and the transformed goal.
    % Finally, we try to flatten this new conjunction.
    %
:- pred region_transform_case(module_info::in, rpt_graph::in,
    rbmm_renaming_proc::in, rbmm_renaming_proc::in,
    pp_actual_region_args_table::in, region_instr_proc::in,
    rbmm_renaming_annotation_proc::in, rbmm_renaming_annotation_proc::in,
    hlds_goal::in, case::in, case::out,
    name_to_prog_var::in, name_to_prog_var::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

region_transform_case(ModuleInfo, Graph, ResurRenamingProc,
        IteRenamingProc, ActualRegionArgProc, RegionInstructionProc,
        ResurRenamingAnnoProc, IteRenamingAnnoProc, Switch,
        case(MainConsId, OtherConsIds, !.Goal),
        case(MainConsId, OtherConsIds, !:Goal),
        !NameToVar, !VarSet, !VarTypes) :-
    expect(unify(OtherConsIds, []), $pred, "NYI: multi-cons-id cases"),
    ( if
        ( MainConsId = cons(_, 0, _)
        ; MainConsId = some_int_const(_)
        ; MainConsId = float_const(_)
        ; MainConsId = char_const(_)
        ; MainConsId = string_const(_)
        ),
        Switch = hlds_goal(switch(_, _, _), Info)
    then
        ProgPoint = program_point_init(Info),
        ProgPoint = pp(Context, _),
        find_renamings_at_prog_point(ResurRenamingProc, IteRenamingProc,
            ProgPoint, ResurRenaming, IteRenaming),

        % Assignment unifications due to ite renaming.
        assignments_from_ite_renaming_anno(IteRenamingAnnoProc, ProgPoint,
            !NameToVar, !VarSet, !VarTypes, [], IteRenamingAssignments),

        % Region instructions before and after this program point.
        ( if
            map.search(RegionInstructionProc, ProgPoint,
                instrs_before_after(Before, After))
        then
            % Region instructions before this program point.
            list.foldl4(
                region_instruction_to_conj_before(ModuleInfo, Context,
                    ResurRenaming, IteRenaming),
                Before, !NameToVar, !VarSet, !VarTypes,
                IteRenamingAssignments, Conjs1),

            % Region instructions after this program point.
            list.foldl4(
                region_instruction_to_conj(ModuleInfo, Context, ResurRenaming,
                    IteRenaming),
                After, !NameToVar, !VarSet, !VarTypes, Conjs1, Conjs2)
        else
            Conjs2 = IteRenamingAssignments
        ),

        % Assignment unifications due to region resurrection renaming.
        assignments_from_resur_renaming_anno(ResurRenamingAnnoProc, ProgPoint,
            IteRenaming, !NameToVar, !VarSet, !VarTypes, Conjs2, Conjs),

        RemovedGoal = hlds_goal(conj(plain_conj, Conjs), Info)
    else
        Switch = hlds_goal(_, Info),
        RemovedGoal = hlds_goal(conj(plain_conj, []), Info)
    ),
    region_transform_goal(ModuleInfo, Graph, ResurRenamingProc,
        IteRenamingProc, ActualRegionArgProc, RegionInstructionProc,
        ResurRenamingAnnoProc, IteRenamingAnnoProc, !Goal, !NameToVar,
        !VarSet, !VarTypes),
    flatten_conj([RemovedGoal, !.Goal], FlatConjs),
    Switch = hlds_goal(_, ConjsInfo),
    !:Goal = hlds_goal(conj(plain_conj, FlatConjs), ConjsInfo).

:- pred find_renamings_at_prog_point(rbmm_renaming_proc::in,
    rbmm_renaming_proc::in, program_point::in,
    rbmm_renaming::out, rbmm_renaming::out) is det.

find_renamings_at_prog_point(ResurRenamingProc, IteRenamingProc, ProgPoint,
        ResurRenaming, IteRenaming) :-
    ( if map.search(ResurRenamingProc, ProgPoint, ResurRenaming0) then
        ResurRenaming = ResurRenaming0
    else
        ResurRenaming = map.init
    ),
    ( if map.search(IteRenamingProc, ProgPoint, IteRenaming0) then
        IteRenaming = IteRenaming0
    else
        IteRenaming = map.init
    ).

:- pred assignments_from_ite_renaming_anno(rbmm_renaming_annotation_proc::in,
    program_point::in, name_to_prog_var::in, name_to_prog_var::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    hlds_goals::in, hlds_goals::out) is det.

assignments_from_ite_renaming_anno(IteRenamingAnnoProc, ProgPoint,
        !NameToVar, !VarSet, !VarTypes, !IteRenamingAssignments) :-
    % Assignment unifications due to ite renaming.
    ( if map.search(IteRenamingAnnoProc, ProgPoint, IteRenamingAnnos) then
        list.foldl4(ite_renaming_annotation_to_assignment, IteRenamingAnnos,
            !NameToVar, !VarSet, !VarTypes, !IteRenamingAssignments)
    else
        true
    ).

:- pred assignments_from_resur_renaming_anno(rbmm_renaming_annotation_proc::in,
    program_point::in, rbmm_renaming::in, name_to_prog_var::in,
    name_to_prog_var::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out, hlds_goals::in, hlds_goals::out) is det.

assignments_from_resur_renaming_anno(ResurRenamingAnnoProc, ProgPoint,
        IteRenaming, !NameToVar, !VarSet, !VarTypes, !Conjs) :-
    ( if map.search(ResurRenamingAnnoProc, ProgPoint, ResurRenamingAnnos) then
        list.foldl4(resur_renaming_annotation_to_assignment(IteRenaming),
            ResurRenamingAnnos, !NameToVar, !VarSet, !VarTypes, !Conjs)
    else
        true
    ).

    % Return the program variable representing the region which is
    % represented by the node in the points-to graph.
    % Come up with a new program variable if none exists yet.
    % Each node is associated with a region name, so this predicate just
    % delegates the task for region_name_to_var.
    %
:- pred node_to_var(rpt_graph::in, rptg_node::in,
    prog_var::out, name_to_prog_var::in, name_to_prog_var::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

node_to_var(Graph, Node, RegVar, !NameToVar, !VarSet, !VarTypes) :-
    RegName = rptg_lookup_region_name(Graph, Node),
    region_name_to_var(RegName, RegVar, !NameToVar, !VarSet, !VarTypes).

    % Return the program variable representing the region name.
    % Come up with a new one if none exists.
    %
:- pred region_name_to_var(string::in, prog_var::out,
    name_to_prog_var::in, name_to_prog_var::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

region_name_to_var(Name, RegVar, !NameToVar, !VarSet, !VarTypes) :-
    ( if map.search(!.NameToVar, Name, RegVar0) then
        RegVar = RegVar0
    else
        varset.new_named_var(Name, RegVar, !VarSet),
        add_var_type(RegVar, region_type, !VarTypes),
        map.det_insert(Name, RegVar, !NameToVar)
    ).

    % The same as node_to_var, but the corresponding region name is
    % subjected to resurrection and if-then-else renaming beforehand.
    %
:- pred node_to_var_with_both_renamings(rpt_graph::in, rbmm_renaming::in,
    rbmm_renaming::in, rptg_node::in, prog_var::out,
    name_to_prog_var::in, name_to_prog_var::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

node_to_var_with_both_renamings(Graph, ResurRenaming, IteRenaming,
        Node, RegVar, !NameToVar, !VarSet, !VarTypes) :-
    RegName = rptg_lookup_region_name(Graph, Node),
    region_name_to_var_with_both_renamings(RegName, ResurRenaming, IteRenaming,
        RegVar, !NameToVar, !VarSet, !VarTypes).

    % Resurrection renaming will be applied first. If a renaming exists
    % for the name (i.e., the name will be changed to another name) then
    % ite renaming need not to be applied because actually it is not
    % applicable anymore. If more than one renaming exist, then we use
    % the last one.
    %
:- pred region_name_to_var_with_both_renamings(string::in,
    rbmm_renaming::in, rbmm_renaming::in, prog_var::out,
    name_to_prog_var::in, name_to_prog_var::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

region_name_to_var_with_both_renamings(Name0, ResurRenaming, IteRenaming,
        RegVar, !NameToVar, !VarSet, !VarTypes) :-
    ( if map.search(ResurRenaming, Name0, ResurNameList) then
        list.det_last(ResurNameList, Name)
    else if map.search(IteRenaming, Name0, IteNameList) then
        list.det_last(IteNameList, Name)
    else
        Name = Name0
    ),
    region_name_to_var(Name, RegVar, !NameToVar, !VarSet, !VarTypes).

    % This predicate is the same as the above except that if more than one
    % renaming exist we will use the first one. This is for use *only* when
    % renaming the region in a remove instruction added before a program
    % point.
    %
:- pred region_name_to_var_with_both_renamings_before(string::in,
    rbmm_renaming::in, rbmm_renaming::in, prog_var::out,
    name_to_prog_var::in, name_to_prog_var::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

region_name_to_var_with_both_renamings_before(Name0, ResurRenaming,
        IteRenaming, RegVar, !NameToVar, !VarSet, !VarTypes) :-
    ( if map.search(ResurRenaming, Name0, ResurNameList) then
        Name = list.det_index0(ResurNameList, 0)
    else if map.search(IteRenaming, Name0, IteNameList) then
        Name = list.det_index0(IteNameList, 0)
    else
        Name = Name0
    ),
    region_name_to_var(Name, RegVar, !NameToVar, !VarSet, !VarTypes).

    % The same as region_name_to_var, but the region name here is
    % subjected to resurrection renaming in advance.
    %
:- pred region_name_to_var_with_renaming(string::in, rbmm_renaming::in,
    prog_var::out, name_to_prog_var::in, name_to_prog_var::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

region_name_to_var_with_renaming(Name0, ResurRenaming, RegVar,
        !NameToVar, !VarSet, !VarTypes) :-
    ( if map.search(ResurRenaming, Name0, ResurNameList) then
        Name = list.det_last(ResurNameList)
    else
        Name = Name0
    ),
    region_name_to_var(Name, RegVar, !NameToVar, !VarSet, !VarTypes).

    % The region name in a region instruction is subjected to renaming due
    % to if-then-else and region resurrection. This predicate turns such an
    % instruction into a call to a suitable region builtin.
    % XXX Call to generate_plain_call here seems to be an overkill because we
    % will recompute nonlocals, instmap delta anyway.
    %
:- pred region_instruction_to_conj(module_info::in, term.context::in,
    rbmm_renaming::in, rbmm_renaming::in, region_instr::in,
    name_to_prog_var::in, name_to_prog_var::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    hlds_goals::in, hlds_goals::out) is det.

region_instruction_to_conj(ModuleInfo, Context, ResurRenaming, IteRenaming,
        RegionInstruction, !NameToVar, !VarSet, !VarTypes, Conjs0, Conjs) :-
    (
        RegionInstruction = create_region(RegionName),
        region_name_to_var_with_both_renamings(RegionName, ResurRenaming,
            IteRenaming, RegionVar, !NameToVar, !VarSet, !VarTypes),
        generate_plain_call(ModuleInfo, pf_predicate,
            mercury_region_builtin_module, create_region_pred_name,
            [], [RegionVar], instmap_delta_bind_no_var, only_mode,
            detism_det, purity_impure, [], Context, CallGoal)
    ;
        RegionInstruction = remove_region(RegionName),
        region_name_to_var_with_both_renamings(RegionName, ResurRenaming,
            IteRenaming, RegionVar, !NameToVar, !VarSet, !VarTypes),
        generate_plain_call(ModuleInfo, pf_predicate,
            mercury_region_builtin_module, remove_region_pred_name,
            [], [RegionVar], instmap_delta_bind_no_var, only_mode,
            detism_det, purity_impure, [], Context, CallGoal)
    ;
        RegionInstruction = rename_region(_, _),
        unexpected($pred, "neither create nor remove instruction")
    ),
    Conjs = Conjs0 ++ [CallGoal].

    % The same as the one right above except that to a region in a remove
    % instruction we apply the first resurrection renaming.
    %
:- pred region_instruction_to_conj_before(module_info::in, term.context::in,
    rbmm_renaming::in, rbmm_renaming::in, region_instr::in,
    name_to_prog_var::in, name_to_prog_var::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    hlds_goals::in, hlds_goals::out) is det.

region_instruction_to_conj_before(ModuleInfo, Context, ResurRenaming,
        IteRenaming, RegionInstruction, !NameToVar, !VarSet, !VarTypes,
        Conjs0, Conjs) :-
    (
        RegionInstruction = create_region(RegionName),
        region_name_to_var_with_both_renamings(RegionName, ResurRenaming,
            IteRenaming, RegionVar, !NameToVar, !VarSet, !VarTypes),
        generate_plain_call(ModuleInfo, pf_predicate,
            mercury_region_builtin_module, create_region_pred_name,
            [], [RegionVar], instmap_delta_bind_no_var, only_mode,
            detism_det, purity_impure, [], Context, CallGoal)
    ;
        RegionInstruction = remove_region(RegionName),
        region_name_to_var_with_both_renamings_before(RegionName,
            ResurRenaming, IteRenaming, RegionVar, !NameToVar, !VarSet,
            !VarTypes),
        generate_plain_call(ModuleInfo, pf_predicate,
            mercury_region_builtin_module, remove_region_pred_name,
            [], [RegionVar], instmap_delta_bind_no_var, only_mode,
            detism_det, purity_impure, [], Context, CallGoal)
    ;
        RegionInstruction = rename_region(_, _),
        unexpected($pred, "neither create nor remove instruction")
    ),
    Conjs = Conjs0 ++ [CallGoal].

    % A resurrection renaming annotation is in the form Rx = Rx_resur_y,
    % where Rx is the original name of the region, the other is the one
    % the region is renamed to.
    % This predicate converts the anotation into an assigment unification.
    % The original name of the region is subjected to the renaming due to
    % if-then-else, if such a renaming exists at the current program point.
    %
:- pred resur_renaming_annotation_to_assignment(rbmm_renaming::in,
    region_instr::in, name_to_prog_var::in, name_to_prog_var::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    hlds_goals::in, hlds_goals::out) is det.

resur_renaming_annotation_to_assignment(IteRenaming, Annotation,
        !NameToVar, !VarSet, !VarTypes, Conjs0, Conjs) :-
    (
        ( Annotation = create_region(_)
        ; Annotation = remove_region(_)
        ),
        unexpected($pred, "annotation is not assigment")
    ;
        Annotation = rename_region(Right, Left),
        % Only the left region needs to be renamed. Ite renaming does not
        % involve the region on the right side.
        region_name_to_var_with_renaming(Left, IteRenaming, LeftRegVar,
            !NameToVar, !VarSet, !VarTypes),
        region_name_to_var(Right, RightRegVar, !NameToVar, !VarSet, !VarTypes),
        make_assignment_goal(LeftRegVar, RightRegVar,
            "resurrection renaming annotation", AssignmentGoal),
        Conjs = Conjs0 ++ [AssignmentGoal]
    ).

    % This predicate turns a renaming annotation due to if-then-else into
    % an assignment. No renaming needs to be applied to the
    % if-then-else renaming annotations.
    %
:- pred ite_renaming_annotation_to_assignment(region_instr::in,
    name_to_prog_var::in, name_to_prog_var::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    hlds_goals::in, hlds_goals::out) is det.

ite_renaming_annotation_to_assignment(Annotation, !NameToVar,
        !VarSet, !VarTypes, Conjs0, Conjs) :-
    (
        ( Annotation = create_region(_)
        ; Annotation = remove_region(_)
        ),
        unexpected($pred, "annotation is not assignment")
    ;
        Annotation = rename_region(Right, Left),
        region_name_to_var(Left, LeftRegVar, !NameToVar, !VarSet, !VarTypes),
        region_name_to_var(Right, RightRegVar, !NameToVar, !VarSet, !VarTypes),
        make_assignment_goal(LeftRegVar, RightRegVar,
            "ite renaming annotation", AssignmentGoal),
        Conjs = Conjs0 ++ [AssignmentGoal]
    ).

:- pred make_assignment_goal(prog_var::in, prog_var::in, string::in,
    hlds_goal::out) is det.

make_assignment_goal(LeftRegVar, RightRegVar, Context, AssignmentGoal) :-
    AssignmentExpr = unify(LeftRegVar, rhs_var(RightRegVar),
        unify_modes_li_lf_ri_rf(free, ground_inst, ground_inst, ground_inst),
        assign(LeftRegVar, RightRegVar),
        unify_context(umc_implicit(Context), [])),
    % Nonlocals and instmap delta will be recomputed anyway, so just put
    % dummy values in.
    % XXX  We need to setup the instantiationess for LeftRegVar here.
    % I think it is always recomputed but it seems that I am wrong.
    % It should be able to be recomputed from the modes in the assigment.
    % Maybe I am missing or doing something wrong here.
    NonLocals = set_of_var.init,
    InstmapDelta = instmap_delta_bind_var(LeftRegVar),
    goal_info_init(NonLocals, InstmapDelta, detism_det, purity_pure,
        AssignmentInfo),
    AssignmentGoal = hlds_goal(AssignmentExpr, AssignmentInfo).

%-----------------------------------------------------------------------------%
%
% Recompute instmap delta.
%

:- pred update_instmap_delta_pred(pred_id::in,
    module_info::in, module_info::out) is det.

update_instmap_delta_pred(PredId, !ModuleInfo) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_valid_non_imported_procids(PredInfo),
    list.foldl(update_instmap_delta_proc(PredId), ProcIds, !ModuleInfo).

:- pred update_instmap_delta_proc(pred_id::in, proc_id::in,
    module_info::in, module_info::out) is det.

update_instmap_delta_proc(PredId, ProcId, !ModuleInfo) :-
    PPId = proc(PredId, ProcId),
    module_info_pred_proc_info(!.ModuleInfo, PPId, PredInfo, ProcInfo0),
    recompute_instmap_delta_proc(recompute_atomic_instmap_deltas,
        ProcInfo0, ProcInfo, !ModuleInfo),
    module_info_set_pred_proc_info(PPId, PredInfo, ProcInfo, !ModuleInfo).

%-----------------------------------------------------------------------------%
%
% Recheck purity.
%

:- pred recheck_purity_pred(pred_id::in, module_info::in, module_info::out)
    is det.

recheck_purity_pred(PredId, !ModuleInfo) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_valid_non_imported_procids(PredInfo),
    list.foldl(recheck_purity_proc(PredId), ProcIds, !ModuleInfo).

    % Recheck purity of the procedure.
    % This predicate is only called when all the procedures have been
    % annotated with region information and recorded. This is necessary
    % because repuritycheck_proc looks up information about procedures.
    %
:- pred recheck_purity_proc(pred_id::in, proc_id::in, module_info::in,
    module_info::out) is det.

recheck_purity_proc(PredId, ProcId, !ModuleInfo) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    % Recheck purity of this procedure.
    PPId = proc(PredId, ProcId),
    repuritycheck_proc(!.ModuleInfo, PPId, PredInfo0, PredInfo),
    module_info_set_pred_info(PredId, PredInfo, !ModuleInfo).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.rbmm.region_transformation.
%-----------------------------------------------------------------------------%
