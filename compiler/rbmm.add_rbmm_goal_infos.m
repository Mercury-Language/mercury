%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2007-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File rbmm.add_rbmm_goal_infos.m.
% Main author: Quan Phan.
%
% This module fills in rbmm_goal_info field in hlds_goal_extra_info data
% structure. The details of this information can be read in hlds_goal.m.
%
% This information is used by the code generator to optimize the runtime
% region support for backtracking in region-based memory management.
%
% To support programs with backtracking in region-based memory management we
% have to deal with region resurrection (i.e. a region is dead during forward
% execution but is still needed when the program backtracks) and we may also
% want to do instant reclaiming when backtracking happens. In Mercury, the
% support is needed for if-then-else, nondet disjunction and commit context.
% This runtime support certainly incurs runtime overhead to programs therefore
% we would like to pay for it only when we actually need it. For example, if
% we know that the condition of an if-then-else does not allocate memory into
% any existing regions, does not create any regions nor destroy any regions,
% then we do not need to provide support for the if-then-else. This situation
% is very often the case with if-then-elses therefore if we can exploit it we
% will hopefully boost the performance of region-based memory management. The
% rbmm_goal_info gives the code generator the information which is needed to
% fulfill this optimization.
%
% XXX More information about the advanced runtime support for backtracking
% should be documented or found somewhere but it is not avaiable yet because
% it is still in experimental state. We should fix this when it is more
% stable.
%
%---------------------------------------------------------------------------%

:- module transform_hlds.rbmm.add_rbmm_goal_infos.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module transform_hlds.rbmm.points_to_info.
:- import_module transform_hlds.rbmm.region_arguments.
:- import_module transform_hlds.rbmm.region_resurrection_renaming.
:- import_module transform_hlds.rbmm.region_transformation.

:- pred collect_rbmm_goal_info(rpta_info_table::in,
    proc_pp_actual_region_args_table::in,
    rbmm_renaming_table::in, rbmm_renaming_table::in,
    name_to_prog_var_table::in, module_info::in, module_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.
:- import_module transform_hlds.rbmm.points_to_graph.
:- import_module transform_hlds.smm_common.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set.

%---------------------------------------------------------------------------%
%
% Collect rbmm_goal_info.
%

    % It only makes sense to collect rbmm_goal_info for the procedures
    % which have been region-analyzed. To find such procedures we can use one
    % of the result tables of any previous region analyses.
    % Note: The use of ActualRegionArgumentTable here is convenient because
    % apart from the procedure id, we also need the information about actual
    % region arguments in this pass.
    %
collect_rbmm_goal_info(RptaInfoTable, ActualRegionArgumentTable,
        ResurRenamingTable, IteRenamingTable, NameToRegionVarTable,
        !ModuleInfo) :-
    map.foldl(collect_rbmm_goal_info_proc(RptaInfoTable, ResurRenamingTable,
        IteRenamingTable, NameToRegionVarTable), ActualRegionArgumentTable,
        !ModuleInfo).

:- pred collect_rbmm_goal_info_proc(rpta_info_table::in,
    rbmm_renaming_table::in, rbmm_renaming_table::in,
    name_to_prog_var_table::in, pred_proc_id::in,
    pp_actual_region_args_table::in,
    module_info::in, module_info::out) is det.

collect_rbmm_goal_info_proc(RptaInfoTable, ResurRenamingTable,
        IteRenamingTable, NameToRegionVarTable, PPId, ActualRegionsArgsProc,
        !ModuleInfo) :-
    module_info_pred_proc_info(!.ModuleInfo, PPId, PredInfo, ProcInfo0),
    proc_info_get_goal(ProcInfo0, Body0),
    map.lookup(RptaInfoTable, PPId, RptaInfo),
    RptaInfo = rpta_info(Graph, _),
    ( if map.search(ResurRenamingTable, PPId, ResurRenamingProc0) then
        ResurRenamingProc = ResurRenamingProc0
    else
        ResurRenamingProc = map.init
    ),
    ( if map.search(IteRenamingTable, PPId, IteRenamingProc0) then
        IteRenamingProc = IteRenamingProc0
    else
        IteRenamingProc = map.init
    ),
    map.lookup(NameToRegionVarTable, PPId, NameToRegionVar),
    collect_rbmm_goal_info_goal(!.ModuleInfo, ProcInfo0, Graph,
        ActualRegionsArgsProc, ResurRenamingProc, IteRenamingProc,
        NameToRegionVar, Body0, Body),
    proc_info_set_goal(Body, ProcInfo0, ProcInfo),
    module_info_set_pred_proc_info(PPId, PredInfo, ProcInfo, !ModuleInfo).

:- pred collect_rbmm_goal_info_goal(module_info::in, proc_info::in,
    rpt_graph::in, pp_actual_region_args_table::in,
    rbmm_renaming_proc::in, rbmm_renaming_proc::in, name_to_prog_var::in,
    hlds_goal::in, hlds_goal::out) is det.

collect_rbmm_goal_info_goal(ModuleInfo, ProcInfo, Graph,
        ActualRegionsArgsProc, ResurRenamingProc, IteRenamingProc,
        NameToRegionVar, !Goal) :-
    !.Goal = hlds_goal(Expr0, Info0),
    collect_rbmm_goal_info_goal_expr(ModuleInfo, ProcInfo, Graph,
        ActualRegionsArgsProc, ResurRenamingProc, IteRenamingProc,
        NameToRegionVar, Expr0, Expr, Info0, Info),
    !:Goal = hlds_goal(Expr, Info).

:- pred collect_rbmm_goal_info_goal_expr(module_info::in, proc_info::in,
    rpt_graph::in, pp_actual_region_args_table::in,
    rbmm_renaming_proc::in, rbmm_renaming_proc::in, name_to_prog_var::in,
    hlds_goal_expr::in, hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out) is det.

collect_rbmm_goal_info_goal_expr(ModuleInfo, ProcInfo, Graph,
        ActualRegionsArgsProc, ResurRenamingProc, IteRenamingProc,
        NameToRegionVar, !Expr, !Info) :-
    (
        !.Expr = unify(_, _, _, Unification, _),
        ProgPoint = program_point_init(!.Info),
        ( if map.search(ResurRenamingProc, ProgPoint, ResurRenaming0) then
            ResurRenaming = ResurRenaming0
        else
            ResurRenaming = map.init
        ),
        ( if map.search(IteRenamingProc, ProgPoint, IteRenaming0) then
            IteRenaming = IteRenaming0
        else
            IteRenaming = map.init
        ),
        collect_rbmm_goal_info_unification(Unification, ModuleInfo, Graph,
            ResurRenaming, IteRenaming, NameToRegionVar, !Info)
    ;
        !.Expr = plain_call(PredId, ProcId, Args, _, _, _),
        CalleePPId = proc(PredId, ProcId),
        ( if
            is_create_region_call(!.Expr, ModuleInfo, CreatedRegion)
        then
            RbmmGoalInfo = rbmm_goal_info(
                set.make_singleton_set(CreatedRegion),
                set.init, set.init, set.init, set.init),
            goal_info_set_maybe_rbmm(yes(RbmmGoalInfo), !Info)
        else if
            is_remove_region_call(!.Expr, ModuleInfo, RemovedRegion)
        then
            RbmmGoalInfo = rbmm_goal_info(set.init,
                set.make_singleton_set(RemovedRegion), set.init, set.init,
                set.init),
            goal_info_set_maybe_rbmm(yes(RbmmGoalInfo), !Info)
        else if
            some_are_special_preds([CalleePPId], ModuleInfo)
        then
            goal_info_set_maybe_rbmm(yes(rbmm_info_init), !Info)
        else
            CallSite = program_point_init(!.Info),
            proc_info_get_var_table(ModuleInfo, ProcInfo, VarTable),
            RegionArgs = list.filter(is_region_var(VarTable), Args),
            map.lookup(ActualRegionsArgsProc, CallSite, ActualRegionArgs),
            ActualRegionArgs = region_args(Constants, Inputs, _Outputs),
            list.det_split_list(list.length(Constants), RegionArgs,
                CarriedRegions, RemovedAndCreated),
            list.det_split_list(list.length(Inputs), RemovedAndCreated,
                RemovedRegions, CreatedRegions),
            % XXX We safely approximate that RemovedRegions and CarriedRegions
            % are allocated into and read from in this call.
            AllocatedIntoAndReadFrom =
                set.list_to_set(RemovedRegions ++ CarriedRegions),

            module_info_get_globals(ModuleInfo, Globals),
            globals.lookup_bool_option(Globals, use_alloc_regions,
                UseAllocRegions),
            (
                UseAllocRegions = yes,
                % Keep only the regions that are removed but
                % also allocated into.
                keep_allocated_regions(Inputs, RemovedRegions, Graph,
                    RemovedAndAllocRegions),
                % Allocated regions are the above plus the carried ones.
                % The carried here are those that are also allocated into.
                AllocatedIntoRegions =
                    set.list_to_set(RemovedAndAllocRegions ++ CarriedRegions)
            ;
                UseAllocRegions = no,
                AllocatedIntoRegions = AllocatedIntoAndReadFrom
            ),

            % The read-from set is not very important so we are not precise
            % in estimating it.
            RbmmGoalInfo = rbmm_goal_info(set.list_to_set(CreatedRegions),
                set.list_to_set(RemovedRegions),
                set.list_to_set(CarriedRegions),
                AllocatedIntoRegions, AllocatedIntoAndReadFrom),
            goal_info_set_maybe_rbmm(yes(RbmmGoalInfo), !Info)
        )
    ;
        !.Expr = conj(ConjType, Conjs0),
        list.map(
            collect_rbmm_goal_info_goal(ModuleInfo, ProcInfo, Graph,
                ActualRegionsArgsProc, ResurRenamingProc, IteRenamingProc,
                NameToRegionVar),
            Conjs0, Conjs),
        !:Expr = conj(ConjType, Conjs),

        % Calculate created, removed, allocated into, used regions.
        compute_rbmm_info_conjunction(Conjs, rbmm_info_init, RbmmInfo0),

        % Calculate carried regions.
        % The carried regions are those that are NOT removed, NOT created
        % by all the conjuncts.
        RbmmInfo0 = rbmm_goal_info(Created, Removed, _, AllocatedInto, Used),
        NonLocals = goal_info_get_nonlocals(!.Info),
        proc_info_get_var_table(ModuleInfo, ProcInfo, VarTable),
        NonLocalRegionsSet =
            set_of_var.filter(is_region_var(VarTable), NonLocals),
        NonLocalRegions = set_of_var.bitset_to_set(NonLocalRegionsSet),
        set.difference(NonLocalRegions, set.union(Created, Removed), Carried),
        RbmmInfo =
            rbmm_goal_info(Created, Removed, Carried, AllocatedInto, Used),
        goal_info_set_maybe_rbmm(yes(RbmmInfo), !Info)
    ;
        !.Expr = disj(Disjs0),
        list.map(collect_rbmm_goal_info_goal(ModuleInfo, ProcInfo, Graph,
            ActualRegionsArgsProc, ResurRenamingProc, IteRenamingProc,
            NameToRegionVar), Disjs0, Disjs),
        !:Expr = disj(Disjs),
        (
            Disjs = [],
            goal_info_set_maybe_rbmm(yes(rbmm_info_init), !Info)
        ;
            % Well-modedness requires that each disjunct creates, removes and
            % carries the same regions, which are also created, removed,
            % carried by the disjunction.
            Disjs = [hlds_goal(_, DInfo) | _],
            DRbmmInfo = goal_info_get_rbmm(DInfo),
            DRbmmInfo = rbmm_goal_info(Created, Removed, Carried, _, _),
            RbmmInfo0 = rbmm_goal_info(Created, Removed, Carried, set.init,
                set.init),

            % Calculate allocated-into and used regions.
            compute_rbmm_info_goals(Disjs, RbmmInfo0, RbmmInfo),
            goal_info_set_maybe_rbmm(yes(RbmmInfo), !Info)
        )
    ;
        !.Expr = switch(A, B, Cases0),
        list.map(collect_rbmm_goal_info_case(ModuleInfo, ProcInfo, Graph,
            ActualRegionsArgsProc, ResurRenamingProc, IteRenamingProc,
            NameToRegionVar), Cases0, Cases),
        !:Expr = switch(A, B, Cases),
        (
            Cases = [],
            unexpected($pred, "empty switch")
        ;
            % The process here is similar to the above code for disjunctions.
            Cases = [Case | _],
            Case = case(_, _, Goal),
            Goal = hlds_goal(_, CaseInfo),
            CaseRbmmInfo = goal_info_get_rbmm(CaseInfo),
            CaseRbmmInfo = rbmm_goal_info(Created, Removed, Carried, _, _),
            SwitchRbmmInfo0 =
                rbmm_goal_info(Created, Removed, Carried, set.init, set.init),
            list.foldl(
                ( pred(C::in, Gs0::in, Gs::out) is det :-
                    C = case(_, _, G),
                    Gs = [G | Gs0]
                ), Cases, [], Goals),
            compute_rbmm_info_goals(Goals, SwitchRbmmInfo0, SwitchRbmmInfo),
            goal_info_set_maybe_rbmm(yes(SwitchRbmmInfo), !Info)
        )
    ;
        !.Expr = negation(Goal0),
        collect_rbmm_goal_info_goal(ModuleInfo, ProcInfo, Graph,
            ActualRegionsArgsProc, ResurRenamingProc, IteRenamingProc,
            NameToRegionVar, Goal0, Goal),
        !:Expr = negation(Goal),
        Goal = hlds_goal(_, Info),
        RbmmInfo = goal_info_get_rbmm(Info),
        goal_info_set_maybe_rbmm(yes(RbmmInfo), !Info)
    ;
        !.Expr = scope(Reason, Goal0),
        % XXX We should special-case the handling of from_ground_term_construct
        % scopes.
        collect_rbmm_goal_info_goal(ModuleInfo, ProcInfo, Graph,
            ActualRegionsArgsProc, ResurRenamingProc, IteRenamingProc,
            NameToRegionVar, Goal0, Goal),
        !:Expr = scope(Reason, Goal),
        Goal = hlds_goal(_, Info),
        RbmmInfo = goal_info_get_rbmm(Info),
        goal_info_set_maybe_rbmm(yes(RbmmInfo), !Info)
    ;
        !.Expr = if_then_else(Vars, Cond0, Then0, Else0),
        collect_rbmm_goal_info_goal(ModuleInfo, ProcInfo, Graph,
            ActualRegionsArgsProc, ResurRenamingProc, IteRenamingProc,
            NameToRegionVar, Cond0, Cond),
        collect_rbmm_goal_info_goal(ModuleInfo, ProcInfo, Graph,
            ActualRegionsArgsProc, ResurRenamingProc, IteRenamingProc,
            NameToRegionVar, Then0, Then),
        collect_rbmm_goal_info_goal(ModuleInfo, ProcInfo, Graph,
            ActualRegionsArgsProc, ResurRenamingProc, IteRenamingProc,
            NameToRegionVar, Else0, Else),
        !:Expr = if_then_else(Vars, Cond, Then, Else),

        % Well-modedness requires that the (cond, then) and the else parts
        % create, remove and carry the same regions, which are also created,
        % removed, carried by the if-then-else.
        Else = hlds_goal(_, ElseInfo),
        ElseRbmmInfo = goal_info_get_rbmm(ElseInfo),
        ElseRbmmInfo = rbmm_goal_info(Created, Removed, Carried, _, _),
        IteRbmmInfo0 = rbmm_goal_info(Created, Removed, Carried, set.init,
            set.init),
        compute_rbmm_info_goals([Cond, Then, Else], IteRbmmInfo0, IteRbmmInfo),
        goal_info_set_maybe_rbmm(yes(IteRbmmInfo), !Info)
    ;
        % We do not handle generic calls and calls to foreign procs
        % in RBMM yet, so if a program has any of them, the RBMM compilation
        % will fail before reaching here. We just call sorry now so that
        % they are not forgotten when we deal with these explicitly
        % in the future.
        (
            !.Expr = generic_call(_, _, _, _, _),
            sorry($pred, "generic call not handled")
        ;
            !.Expr = call_foreign_proc(_, _, _, _, _, _, _),
            sorry($pred, "call to foreign proc not handled")
        )
    ;
        !.Expr = shorthand(_),
        % These should have been expanded out by now.
        unexpected($pred, "shorthand")
    ).

    % The elements in the first and second lists are corresponding.
    % Ones in the first list are the nodes in rpt graph therefore we can
    % check their allocated property, ones in the second list are prog_vars
    % (after renaming) that represent those in the first.
    %
:- pred keep_allocated_regions(list(rptg_node)::in, list(prog_var)::in,
    rpt_graph::in, list(prog_var)::out) is det.

keep_allocated_regions([], [], _, []).
keep_allocated_regions([], [_ | _], _, []) :-
    unexpected($pred, "length mismatch").
keep_allocated_regions([_ | _], [], _, []) :-
    unexpected($pred, "length mismatch").
keep_allocated_regions([Input | Inputs], [RemovedRegion | RemovedRegions],
        Graph, RemovedAndAllocRegions) :-
    keep_allocated_regions(Inputs, RemovedRegions, Graph,
        RemovedAndAllocRegions0),
    ( if rptg_is_allocated_node(Graph, Input) then
        RemovedAndAllocRegions = [RemovedRegion | RemovedAndAllocRegions0]
    else
        RemovedAndAllocRegions = RemovedAndAllocRegions0
    ).

    % The regions that are created inside this conjunction are all that are
    % created inside each conjunct.
    % The regions that are removed inside this conjunction are all that are
    % removed inside each conjunct AND EXIST before the conjunction,
    % i.e. not being created in the condition.
    % The regions that are allocated into inside this conjunction are those
    % allocated into by any conjuncts AND EXIST ...
    % The regions that are read from inside this conjunction are those that
    % are read from by any conjuncts AND EXIST ...
    %
:- pred compute_rbmm_info_conjunction(list(hlds_goal)::in,
    rbmm_goal_info::in, rbmm_goal_info::out) is det.

compute_rbmm_info_conjunction([], !RbmmInfo).
compute_rbmm_info_conjunction([Conj | Conjs], !RbmmInfo) :-
    Conj = hlds_goal(_, CInfo),
    CRbmmInfo = goal_info_get_rbmm(CInfo),
    CRbmmInfo = rbmm_goal_info(CCreated, CRemoved, _, CAllocatedInto, CUsed),
    !.RbmmInfo = rbmm_goal_info(Created0, Removed0, Carried, AllocatedInto0,
        Used0),
    set.union(CCreated, Created0, Created),
    set.difference(set.union(CRemoved, Removed0), Created, Removed),
    set.difference(set.union(CAllocatedInto, AllocatedInto0), Created,
        AllocatedInto),
    set.difference(set.union(CUsed, Used0), Created, Used),

    !:RbmmInfo = rbmm_goal_info(Created, Removed, Carried, AllocatedInto,
        Used),
    compute_rbmm_info_conjunction(Conjs, !RbmmInfo).

    % The regions that are allocated into inside this list of goals are those
    % allocated into by any goals AND EXIST ...
    % The regions that are read from inside this list of goals are those that
    % are read from by any goals AND EXIST ...
    %
    % This predicate are used to compute the above information for the goals
    % in disjunction, cases, and if-then-else.
    %
:- pred compute_rbmm_info_goals(list(hlds_goal)::in,
    rbmm_goal_info::in, rbmm_goal_info::out) is det.

compute_rbmm_info_goals([], !RbmmInfo).
compute_rbmm_info_goals([Goal | Goals], !RbmmInfo) :-
    Goal = hlds_goal(_, Info),
    GoalRbmmInfo = goal_info_get_rbmm(Info),
    GoalRbmmInfo = rbmm_goal_info(_, _, _, GoalAllocatedInto, GoalUsed),
    !.RbmmInfo = rbmm_goal_info(Created, Removed, Carried, AllocatedInto0,
        Used0),
    set.difference(set.union(GoalAllocatedInto, AllocatedInto0), Created,
        AllocatedInto),
    set.difference(set.union(GoalUsed, Used0), Created, Used),
    !:RbmmInfo = rbmm_goal_info(Created, Removed, Carried, AllocatedInto,
        Used),
    compute_rbmm_info_goals(Goals, !RbmmInfo).

:- pred collect_rbmm_goal_info_unification(unification::in, module_info::in,
    rpt_graph::in, rbmm_renaming::in, rbmm_renaming::in, name_to_prog_var::in,
    hlds_goal_info::in, hlds_goal_info::out) is det.

collect_rbmm_goal_info_unification(Unification, ModuleInfo, Graph,
        ResurRenaming, IteRenaming, RegionNameToVar, !Info) :-
    (
        Unification = construct(_, _, _, _, HowToConstruct, _, _),
        (
            HowToConstruct = construct_in_region(AllocatedIntoRegion),
            RbmmInfo = rbmm_goal_info(set.init, set.init,
                set.make_singleton_set(AllocatedIntoRegion),
                set.make_singleton_set(AllocatedIntoRegion), set.init),
            goal_info_set_maybe_rbmm(yes(RbmmInfo), !Info)
        ;
            ( HowToConstruct = construct_statically(_)
            ; HowToConstruct = construct_dynamically
            ; HowToConstruct = reuse_cell(_)
            ),
            goal_info_set_maybe_rbmm(yes(rbmm_info_init), !Info)
        )
    ;
        Unification = deconstruct(DeconsCellVar, _, _, _, _, _),
        rptg_get_node_by_variable(Graph, DeconsCellVar, Node),
        NodeType = rptg_lookup_node_type(Graph, Node),
        ( if type_not_stored_in_region(NodeType, ModuleInfo) then
            goal_info_set_maybe_rbmm(yes(rbmm_info_init), !Info)
        else
            % We need to try to apply renaming here because the
            % region name in the Graph may need to be changed.
            OriginalName = rptg_lookup_region_name(Graph, Node),
            ( if map.search(ResurRenaming, OriginalName, ResurNameList) then
                Name = list.det_last(ResurNameList)
            else if map.search(IteRenaming, OriginalName, IteNameList) then
                Name = list.det_last(IteNameList)
            else
                Name = OriginalName
            ),

            % This region may only be read from in the procedure containing
            % this deconstruction therefore there might be no entry for it
            % in RegionNameToVar.
            ( if map.search(RegionNameToVar, Name, RegionVar) then
                RbmmInfo = rbmm_goal_info(set.init, set.init,
                    set.make_singleton_set(RegionVar),
                    set.init, set.make_singleton_set(RegionVar))
            else
                RbmmInfo = rbmm_info_init
            ),
            goal_info_set_maybe_rbmm(yes(RbmmInfo), !Info)
        )
    ;
        % XXX We do have assignments between two region variables.
        % But we do not consider either of them created or removed because
        % the region they are bound to is still there, i.e. it has been
        % created and will not be removed.
        ( Unification = assign(_, _)
        ; Unification = simple_test(_, _)
        ),
        goal_info_set_maybe_rbmm(yes(rbmm_info_init), !Info)
    ;
        Unification = complicated_unify(_, _, _),
        unexpected($pred, "complicated unification")
    ).

:- pred is_remove_region_call(hlds_goal_expr::in, module_info::in,
    prog_var::out) is semidet.

is_remove_region_call(plain_call(PredId, _ProcId, Args, _, _, _),
        ModuleInfo, RemovedRegion) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_module(PredInfo) = mercury_region_builtin_module,
    pred_info_name(PredInfo) = remove_region_pred_name,
    Args = [RemovedRegion].

:- pred is_create_region_call(hlds_goal_expr::in, module_info::in,
    prog_var::out) is semidet.

is_create_region_call(plain_call(PredId, _ProcId, Args, _, _, _),
        ModuleInfo, CreatedRegion) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_module(PredInfo) = mercury_region_builtin_module,
    pred_info_name(PredInfo) = create_region_pred_name,
    Args = [CreatedRegion].

:- pred collect_rbmm_goal_info_case(module_info::in, proc_info::in,
    rpt_graph::in, pp_actual_region_args_table::in,
    rbmm_renaming_proc::in, rbmm_renaming_proc::in, name_to_prog_var::in,
    case::in, case::out) is det.

collect_rbmm_goal_info_case(ModuleInfo, ProcInfo, Graph,
        ActualRegionsArgsProc, ResurRenamingProc, IteRenamingProc,
        NameToRegionVar, !Case) :-
    !.Case = case(MainConsId, OtherConsIds, Goal0),
    collect_rbmm_goal_info_goal(ModuleInfo, ProcInfo, Graph,
        ActualRegionsArgsProc, ResurRenamingProc, IteRenamingProc,
        NameToRegionVar, Goal0, Goal),
    !:Case = case(MainConsId, OtherConsIds, Goal).

%---------------------------------------------------------------------------%
:- end_module transform_hlds.rbmm.add_rbmm_goal_infos.
%---------------------------------------------------------------------------%
