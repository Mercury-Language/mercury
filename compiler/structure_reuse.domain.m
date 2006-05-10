%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: structure_reuse.domain.m
% Main authors: nancy
%
% Definition of the abstract domain for keeping track of opportunities for
% structure reuse. 
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.ctgc.structure_reuse.domain.

:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.prog_data.
:- import_module transform_hlds.ctgc.structure_sharing.domain.

:- import_module map. 
:- import_module set.
:- import_module list.

    % A reuse condition stores all the necessary information to check if 
    % a procedure call is safe w.r.t. a structure reuse opportunity within
    % the body of the called procedure. 
    %
:- type reuse_condition. 
:- type reuse_conditions == list(reuse_condition).

    % Abstract representation for a set of reuse conditions.
    %
:- type reuse_as. 

%-----------------------------------------------------------------------------%
% reuse_condition
%

    % reuse_condition_init(ModuleInfo, ProcInfo, DeadVar, LocalForwardUse,
    %   LocalBackwardUse, SharingAs) = NewReuseCondition.
    %
    % Create a reuse condition for DeadVar, knowing the set of variables in
    % local forward and backward use, as well as the local structure sharing.
    %
:- func reuse_condition_init(module_info, proc_info, dead_var,
    set(live_var), set(live_var), sharing_as) = reuse_condition. 

:- pred reuse_condition_is_conditional(reuse_condition::in) is semidet.

    % Renaming operation. 
    % This operation renames all occurrences of program variables and
    % type variables according to a program and type variable mapping.
    %
:- pred reuse_condition_rename(map(prog_var, prog_var)::in, tsubst::in,
    reuse_condition::in, reuse_condition::out) is det.

    % Succeeds if the first condition is subsumed by the second one, i.e.,
    % if a procedure call verifies the second condition, then it also 
    % verifies the first condition. 
    %
:- pred reuse_condition_subsumed_by(module_info::in, proc_info::in,
    reuse_condition::in, reuse_condition::in) is semidet.

    % Translate a reuse condition to its new environment. 
    % E.g. Consider a procedure definition: p :- ..., q, ... ,
    % where q contains a potential reuse expressed by a condition C, then this
    % condition needs to be translated to calls to p, such that calls to p
    % can be checked w.r.t. reuses that are possible in q. 
    %
    % In order to translate a condition to its new environment, the same kind
    % of information is needed as when creating an initial condition, i.e.: 
    % set of variables in local forward and backward use, as well as the 
    % structure sharing existing at that place. 
    %  
% XXX To be implemented. Used at indirect-reuse-analysis stage.
% :- pred reuse_condition_translate(module_info::in, proc_info::in,
%    set(prog_var)::in, set(prog_var)::in, sharing_as::in, reuse_condition::in,
%    reuse_condition::out) is det.

%-----------------------------------------------------------------------------%
% reuse_as
%
% XXX The implementation of this type has changed wrt. its counterpart in the
% reuse branch (called memo_reuse). While memo_reuse's didn't always keep a
% minimal representation, reuse_as does. 
%

    % Create an initial set of reuse descriptions. 
    %
:- func reuse_as_init = reuse_as.
:- func reuse_as_init_with_one_condition(reuse_condition) = reuse_as.

    % Succeeds if the first reuses description is subsumed by the second
    % description, i.e., if a procedure call satisfies all the conditions
    % expressed by the second reuses description, then it also satisfies all 
    % the conditions expressed by the first reuses description. 
    %
:- pred reuse_as_subsumed_by(module_info::in, proc_info::in, reuse_as::in, 
    reuse_as::in) is semidet.

    % Tests to see whether the reuses description describes no reuses at all, 
    % only unconditional reuses, or conditional reuses resp.
    %
:- pred reuse_as_no_reuses(reuse_as::in) is semidet.
:- pred reuse_as_all_unconditional_reuses(reuse_as::in) is semidet.
:- pred reuse_as_conditional_reuses(reuse_as::in) is semidet.

    % Given a variable and type variable mapping, rename the reuses 
    % conditions accordingly. 
    %
:- pred reuse_as_rename(map(prog_var, prog_var)::in, tsubst::in, reuse_as::in,
    reuse_as::out) is det.

    % Add a reuse condition to the reuses description. The information of
    % module_info and proc_info are needed to verify subsumption before adding
    % the new condition.
    %
:- pred reuse_as_add_condition(module_info::in, proc_info::in, 
    reuse_condition::in, reuse_as::in, reuse_as::out) is det.

    % Compute the least upper bound of two reuses descriptions. Module_info 
    % and proc_info are needed for verifying subsumption.
    %
:- pred reuse_as_least_upper_bound(module_info::in, proc_info::in, 
    reuse_as::in, reuse_as::in, reuse_as::out) is det.

% XXX TO DO!
% :- func from_reuse_domain(reuse_domain) = reuse_as.
% :- func to_reuse_domain(reuse_as) = reuse_domain.

%-----------------------------------------------------------------------------%
%
% reuse_as_table
%

    % Intermediate storage of the reuse results for individual procedures.
    %
:- type reuse_as_table == map(pred_proc_id, reuse_as).

:- func reuse_as_table_init = reuse_as_table.
:- func reuse_as_table_search(pred_proc_id, reuse_as_table) 
    = reuse_as is semidet.
:- pred reuse_as_table_set(pred_proc_id::in, reuse_as::in, 
    reuse_as_table::in, reuse_as_table::out) is det.
% XXX TO DO!
% :- func load_structure_reuse_table(module_info) = reuse_as_table.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module transform_hlds.ctgc.datastruct.
:- import_module parse_tree.prog_ctgc.

:- import_module set.

:- type reuse_condition 
    --->    always      % The reuse is always safe and does not actually
                        % have a condition.
    ;       condition(
                reuseable_nodes     ::  dead_datastructs,
                    % Description of the datastructures pointing to the 
                    % memory that can be reused within a procedure. 
                local_use_headvars  ::  live_datastructs,
                    % Set of (headvar-related) datastructures that are 
                    % inherently live at the place where the reuse is
                    % decided. 
                local_sharing_headvars  ::  sharing_as
                    % Description of the (headvar-related) structure sharing
                    % that exists at the place where the reuse is decided.
            ).

:- type reuse_as
    --->    no_reuse 
                % = fictive bottom element representing the fact that no
                % reuse has been detected so far.
    ;       unconditional  
                % no_reuse < unconditional.
                % = element representing the fact that all reuses detected
                % so far are unconditional. 
                % Semantically equivalent to "conditional(Cs)" where every C in
                % Cs is "always".
    ;       conditional(reuse_conditions).
                % no_reuse < unconditional < conditional(List)
                % = element representing the collection of reuse conditions
                % collected for the reuses detected so far. 

%-----------------------------------------------------------------------------%
%
% reuse_condition.
%

reuse_condition_init(ModuleInfo, ProcInfo, DeadVar, LFU, LBU, 
        Sharing) = Condition :- 
    proc_info_get_headvars(ProcInfo, HeadVars), 

    % First determine the nodes to which the reuse is related. 
    % There are two cases:
    % 1. Var is a headvar, then it is sufficient to keep the top cell of that
    %    Var as only node. HeadVar-datastructures shared with that node will
    %    still be retraceable at the moment of verifying the condition
    % 2. Var is a local var, then we must compute all the headvar-
    %   datastructures sharing the same memory representation as the top cell
    %   of this var (note that the datastructures that share with some
    %   substructure of this var are not relevant for the nodes). All the
    %   obtained datastructures are kept as the nodes for our condition. 
    TopCell = ctgc.datastruct.datastruct_init(DeadVar),
    (
        list__member(DeadVar, HeadVars)
    ->
        Nodes = [TopCell]
    ;
        SharedDatastructs = extend_datastruct(ModuleInfo, ProcInfo, 
            Sharing, TopCell), 
        Nodes = datastructs_project(HeadVars, SharedDatastructs)
    ),

    % It is possible that the obtained set of nodes is empty, in that
    % case the condition is always satisfied, independent of any calling
    % environment.
    (
        Nodes = []
    ->
        Condition = always
    ;
        set__union(LFU, LBU, LU),
            % XXX the old implementation did not bother about extending at
            % this place, which was contrary to the theory. Check the effect
            % of this change!
        SharedLU = list.condense(
            list.map(extend_datastruct(ModuleInfo, ProcInfo, Sharing),
                list.map(datastruct_init, set.to_sorted_list(LU)))),
        HeadVarSharedLU = datastructs_project(HeadVars, SharedLU),

        structure_sharing.domain.sharing_as_project(HeadVars, Sharing, 
            HeadVarSharing),
        Condition = condition(Nodes, HeadVarSharedLU, HeadVarSharing)
    ).

reuse_condition_is_conditional(condition(_, _, _)).

reuse_condition_subsumed_by(ModuleInfo, ProcInfo, Cond1, Cond2) :- 
    (   
        Cond1 = always
    ;
        Cond1 = condition(Nodes1, LocalUse1, LocalSharing1),
        Cond2 = condition(Nodes2, LocalUse2, LocalSharing2), 
        datastructs_subsumed_by_list(ModuleInfo, ProcInfo, Nodes1, Nodes2),
        datastructs_subsumed_by_list(ModuleInfo, ProcInfo, LocalUse1, 
            LocalUse2),
        sharing_as_is_subsumed_by(ModuleInfo, 
            ProcInfo, LocalSharing1, LocalSharing2)
    ).

:- pred reuse_condition_subsumed_by_list(module_info::in, proc_info::in,
    reuse_condition::in, reuse_conditions::in) is semidet.
reuse_condition_subsumed_by_list(ModuleInfo, ProcInfo, Cond, [Cond1|Rest]) :-
    (
        reuse_condition_subsumed_by(ModuleInfo, ProcInfo, Cond, Cond1)
    ;
        reuse_condition_subsumed_by_list(ModuleInfo, ProcInfo, Cond, Rest)
    ).
        
:- pred reuse_conditions_subsume_reuse_condition(module_info::in,
    proc_info::in, reuse_conditions::in, reuse_condition::in) is semidet.
reuse_conditions_subsume_reuse_condition(ModuleInfo, ProcInfo, Conds, Cond):-
    reuse_condition_subsumed_by_list(ModuleInfo, ProcInfo, Cond, Conds).

reuse_condition_rename(MapVar, TypeSubst, Condition, RenamedCondition):- 
    (
        Condition = always,
        RenamedCondition = always
    ;
        Condition = condition(DeadNodes, InUseNodes, LocalSharing),
        RenamedDeadNodes = list.map(rename_datastruct(MapVar, TypeSubst),
            DeadNodes),
        RenamedInUseNodes = list.map(rename_datastruct(MapVar, TypeSubst),
            InUseNodes),
        sharing_as_rename(MapVar, TypeSubst, LocalSharing, 
            RenamedLocalSharing),
        RenamedCondition = condition(RenamedDeadNodes, RenamedInUseNodes,
            RenamedLocalSharing)
    ).

%-----------------------------------------------------------------------------%
%
% reuse_as
%

reuse_as_init = no_reuse.
reuse_as_init_with_one_condition(ReuseCondition) = ReuseAs :- 
    (
        reuse_condition_is_conditional(ReuseCondition)
    ->
        ReuseAs = conditional([ReuseCondition])
    ;
        ReuseAs = unconditional
    ).

reuse_as_subsumed_by(ModuleInfo, ProcInfo, FirstReuseAs, SecondReuseAs) :- 
    (
        FirstReuseAs = no_reuse
    ;
        FirstReuseAs = unconditional,
        SecondReuseAs = conditional(_)
        % Every calling environment satisfies the reuse conditions as all
        % reuse is unconditional, hence also the calling environments that
        % satisfy the conditions expressed by SecondReuseAs.
    ;
        FirstReuseAs = conditional(ReuseConditionsFirst),
        SecondReuseAs = conditional(ReuseConditionsSecond),
        list.takewhile(reuse_conditions_subsume_reuse_condition(ModuleInfo,
            ProcInfo, ReuseConditionsSecond), ReuseConditionsFirst, _, 
            NotSubsumed),
        NotSubsumed = []
    ).

reuse_as_no_reuses(no_reuse).
reuse_as_all_unconditional_reuses(unconditional).
reuse_as_conditional_reuses(conditional(_)).

reuse_as_rename(MapVar, TypeSubst, ReuseAs, RenamedReuseAs) :- 
    (
        ReuseAs = no_reuse,
        RenamedReuseAs = no_reuse
    ;
        ReuseAs = unconditional,
        RenamedReuseAs = unconditional
    ;
        ReuseAs = conditional(ReuseConditions),
        list.map(reuse_condition_rename(MapVar, TypeSubst),
            ReuseConditions, RenamedReuseConditions),
        RenamedReuseAs = conditional(RenamedReuseConditions)
    ).

reuse_as_add_condition(ModuleInfo, ProcInfo, Condition, !ReuseAs) :- 
    (
        Condition = always,
        (
            !.ReuseAs = no_reuse
        -> 
            !:ReuseAs = unconditional
        ;
            true
        )
    ;
        Condition = condition(_, _, _),
        (
            !.ReuseAs = no_reuse,
            !:ReuseAs = conditional([Condition])
        ;
            !.ReuseAs = unconditional,
            !:ReuseAs = conditional([Condition])
        ;
            !.ReuseAs = conditional(Conditions),
            reuse_conditions_add_condition(ModuleInfo, ProcInfo, 
                Condition, Conditions, NewConditions),
            !:ReuseAs = conditional(NewConditions)
        )
    ).

:- pred reuse_conditions_add_condition(module_info::in, proc_info::in,
    reuse_condition::in, reuse_conditions::in, reuse_conditions::out) is det.
reuse_conditions_add_condition(ModuleInfo, ProcInfo, Condition, !Conds):- 
    (
        reuse_condition_subsumed_by_list(ModuleInfo, ProcInfo, 
            Condition, !.Conds)
    -> 
        true
    ;
        !:Conds = [Condition|!.Conds]
    ).

:- pred reuse_conditions_add_conditions(module_info::in, proc_info::in,
    reuse_conditions::in, reuse_conditions::in, reuse_conditions::out) is det.
reuse_conditions_add_conditions(ModuleInfo, ProcInfo, NewConds, !Conds):- 
    (
        NewConds = [Cond|RemainingConds],
        reuse_conditions_add_condition(ModuleInfo, ProcInfo, Cond, !Conds),
        reuse_conditions_add_conditions(ModuleInfo, ProcInfo, 
            RemainingConds, !Conds)
    ;
        NewConds = []
    ).

reuse_as_least_upper_bound(ModuleInfo, ProcInfo, NewReuseAs, !ReuseAs) :- 
    (
        NewReuseAs = no_reuse
    ;
        NewReuseAs = unconditional,
        (
            !.ReuseAs = no_reuse
        ->
            !:ReuseAs = unconditional
        ;
            true
        )
    ;
        NewReuseAs = conditional(NewConditions),
        (
            !.ReuseAs = no_reuse,
            !:ReuseAs = NewReuseAs
        ;
            !.ReuseAs = unconditional,
            !:ReuseAs = NewReuseAs
        ;
            !.ReuseAs = conditional(Conditions),
            reuse_conditions_add_conditions(ModuleInfo, ProcInfo, 
                NewConditions, Conditions, AllConditions), 
            !:ReuseAs = conditional(AllConditions)
        )
            
    ).

%-----------------------------------------------------------------------------%
%
% reuse_as_table
%

reuse_as_table_init = map.init.
reuse_as_table_search(PPId, Table) = Table ^ elem(PPId). 
reuse_as_table_set(PPId, ReuseAs, !Table) :- 
    !:Table = !.Table ^ elem(PPId) := ReuseAs.

%-----------------------------------------------------------------------------%
:- func this_file = string.
this_file = "structure_reuse.domain.m".

:- end_module transform_hlds.ctgc.structure_reuse.domain.


