%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006-2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: structure_reuse.domain.m.
% Main authors: nancy.
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
:- import_module transform_hlds.ctgc.livedata.
:- import_module transform_hlds.ctgc.structure_sharing.domain.

:- import_module bool. 
:- import_module io. 
:- import_module map. 
:- import_module set.
:- import_module list.

%-----------------------------------------------------------------------------%

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
%
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

:- pred reuse_condition_reusable_nodes(reuse_condition::in,
    dead_datastructs::out) is semidet.

    % Renaming operation. 
    % This operation renames all occurrences of program variables and
    % type variables according to a program and type variable mapping.
    %
:- pred reuse_condition_rename(prog_var_renaming::in, tsubst::in,
    reuse_condition::in, reuse_condition::out) is det.

    % Succeeds if the first condition is subsumed by the second one, i.e.,
    % if a procedure call verifies the second condition, then it also 
    % verifies the first condition. 
    %
:- pred reuse_condition_subsumed_by(module_info::in, proc_info::in,
    reuse_condition::in, reuse_condition::in) is semidet.

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

    % Return a short description of the reuse information.
    %
:- func reuse_as_short_description(reuse_as) = string.

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

    % reuse_as_rename_using_module_info(ModuleInfo, PPId,
    %   ActualVars, ActualTypes, CallerTypeVarSet, CallerHeadTypeParams,
    %   FormalReuse, ActualReuse):
    %
    % Renaming of the formal description of structure reuse conditions to the
    % actual description of these conditions. The information about the formal
    % variables needs to be extracted from the module information. 
    % The actual names are determined by the actual variables names, the 
    % actual types, and the type-variables occurring in those types. 
    %
:- pred reuse_as_rename_using_module_info(module_info::in, 
    pred_proc_id::in, prog_vars::in, list(mer_type)::in, tvarset::in,
    head_type_params::in, reuse_as::in, reuse_as::out) is det.

    % Given a variable and type variable mapping, rename the reuses 
    % conditions accordingly. 
    %
:- pred reuse_as_rename(prog_var_renaming::in, tsubst::in, reuse_as::in,
    reuse_as::out) is det.

    % Add a reuse condition to the reuses description. The information of
    % module_info and proc_info are needed to verify subsumption before adding
    % the new condition.
    %
:- pred reuse_as_add_condition(module_info::in, proc_info::in, 
    reuse_condition::in, reuse_as::in, reuse_as::out) is det.
    
    % A shortcut version of the above procedure when the additional condition
    % is "unconditional". 
    %
:- pred reuse_as_add_unconditional(reuse_as::in, reuse_as::out) is det.

    % Compute the least upper bound of two reuses descriptions. Module_info 
    % and proc_info are needed for verifying subsumption.
    %
:- pred reuse_as_least_upper_bound(module_info::in, proc_info::in, 
    reuse_as::in, reuse_as::in, reuse_as::out) is det.
:- func reuse_as_least_upper_bound(module_info, proc_info, reuse_as, 
    reuse_as) = reuse_as.

    % reuse_as_from_called_procedure_to_local_reuse_as(ModuleInfo,
    %   ProcInfo, HeadVars, InUseData, SharingAs, CalledReuseAs) = 
    %       LocalReuseAs.
    %
    % Translate the reuse description of a called procedure to the 
    % environment of the caller. This means taking into account the local
    % sets of in use variables, as well as the local sharing. 
    %
    % Pre-condition: the reuse description of the called procedure is already
    % correctly renamed to the caller's environment.
    % Pre-condition: the reuse_as from the called procedure contains at 
    % least one conditional reuse condition.
    %
:- func reuse_as_from_called_procedure_to_local_reuse_as(module_info,
    proc_info, prog_vars, live_datastructs, sharing_as, reuse_as) = reuse_as.

    % Taking into account the live data and static variables, check if the
    % reuse conditions expressed by reuse_as are all satisfied, hence making
    % the associated memory reuses safe for that particular calling
    % environment. If the conditions are not satisfied, return the
    % variables which caused one of the conditions to be violated.
    % 
:- pred reuse_as_satisfied(module_info::in, proc_info::in, livedata::in,
    sharing_as::in, prog_vars::in, reuse_as::in, reuse_satisfied_result::out)
    is det.

:- type reuse_satisfied_result
    --->    reuse_possible
    ;       reuse_not_possible(reuse_not_possible_reason).

:- type reuse_not_possible_reason
    --->    no_reuse
            % No reuse version of the procedure.

    ;       unknown_livedata
            % We had to assume everything was live.

    ;       reuse_condition_violated(list(prog_var))
            % At least these variables couldn't be allowed to be clobbered.

    ;       reuse_nodes_have_sharing.
            % The reuse conditions are individually satisfied, but the
            % arguments for reuse have sharing between them which would lead
            % to undefined behaviour in the reuse version of the procedure.

    % Conversion procedures between the public (structure_reuse_domain) 
    % and private (reuse_as) representation for structure reuse conditions. 
    %
:- func from_structure_reuse_domain(structure_reuse_domain) = reuse_as.
:- func to_structure_reuse_domain(reuse_as) = structure_reuse_domain.

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

:- pred reuse_as_table_maybe_dump(bool::in, module_info::in,
    reuse_as_table::in, io::di, io::uo) is det.

    % Load all the structure reuse information present in the HLDS into
    % a reuse table. 
    %
:- func load_structure_reuse_table(module_info) = reuse_as_table.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_out.
:- import_module libs.compiler_util.
:- import_module parse_tree.prog_ctgc.
:- import_module transform_hlds.ctgc.datastruct.
:- import_module transform_hlds.ctgc.util.

:- import_module maybe. 
:- import_module pair.
:- import_module set.
:- import_module string.

%-----------------------------------------------------------------------------%

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
% reuse_condition
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
        list.member(DeadVar, HeadVars)
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
        Nodes = [],
        Condition = always
    ;
        Nodes = [_ | _],
        set.union(LFU, LBU, LU),
        % XXX the old implementation did not bother about extending at this
        % place, which was contrary to the theory. Check the effect of this
        % change!
        LuData = list.map(datastruct_init, set.to_sorted_list(LU)),
        ExtendedLuData = list.map(
            extend_datastruct(ModuleInfo, ProcInfo, Sharing), LuData),
        SharedLU = list.condense(ExtendedLuData),
        HeadVarSharedLU = datastructs_project(HeadVars, SharedLU),

        structure_sharing.domain.sharing_as_project(HeadVars, Sharing, 
            HeadVarSharing),
        Condition = condition(Nodes, HeadVarSharedLU, HeadVarSharing)
    ).

reuse_condition_is_conditional(condition(_, _, _)).

reuse_condition_reusable_nodes(condition(Nodes, _, _), Nodes).

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

reuse_as_short_description(no_reuse) = "no_reuse".
reuse_as_short_description(unconditional) = "uncond".
reuse_as_short_description(conditional(Conds)) = "cond(" ++ Size ++ ")" :- 
    Size = string.int_to_string(list.length(Conds)).
      

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
        FirstReuseAs = unconditional,
        SecondReuseAs = unconditional
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

reuse_as_rename_using_module_info(ModuleInfo, PPId, ActualArgs, ActualTypes,
        CallerTypeVarSet, CallerHeadTypeParams, FormalReuse, ActualReuse) :- 
    VarRenaming = get_variable_renaming(ModuleInfo, PPId, ActualArgs),
    TypeSubst = get_type_substitution(ModuleInfo, PPId, ActualTypes,
        CallerTypeVarSet, CallerHeadTypeParams),
    reuse_as_rename(VarRenaming, TypeSubst, FormalReuse, ActualReuse).

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

reuse_as_add_unconditional(!ReuseAs) :- 
    (
        !.ReuseAs = no_reuse,
        !:ReuseAs = unconditional
    ;
        !.ReuseAs = unconditional
    ;
        !.ReuseAs = conditional(_)
    ).

:- pred reuse_conditions_add_condition(module_info::in, proc_info::in,
    reuse_condition::in, reuse_conditions::in, reuse_conditions::out) is det.

reuse_conditions_add_condition(_ModuleInfo, _ProcInfo, Condition, !Conds):- 
    (
        % XXX this used to check if Condition was subsumed by !.Conds, but
        % that can produce conditions which are too weak, I think.
        % The test suite has an example of this. --pw
        list.member(Condition, !.Conds)
    ->
        true
    ;
        !:Conds = [Condition | !.Conds]
    ).

:- pred reuse_conditions_add_conditions(module_info::in, proc_info::in,
    reuse_conditions::in, reuse_conditions::in, reuse_conditions::out) is det.

reuse_conditions_add_conditions(ModuleInfo, ProcInfo, NewConds, !Conds):- 
    (
        NewConds = [Cond | RemainingConds],
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

reuse_as_least_upper_bound(ModuleInfo, ProcInfo, Reuse1, Reuse2) = Reuse :-
    reuse_as_least_upper_bound(ModuleInfo, ProcInfo, Reuse1, Reuse2, Reuse).

reuse_as_from_called_procedure_to_local_reuse_as(ModuleInfo, ProcInfo,
        HeadVars, LuData, SharingAs, CalledReuseAs) = LocalReuseAs :- 
    (
        CalledReuseAs = no_reuse,
        unexpected(this_file, 
            "reuse_as_from_called_procedure_to_local_reuse_as: " ++ 
            "reuse_as does not specify any reuses.")
    ; 
        CalledReuseAs = unconditional,
        unexpected(this_file,
            "reuse_as_from_called_procedure_to_local_reuse_as: " ++
            "reuse_as is unconditional.")
    ; 
        CalledReuseAs = conditional(ConditionsCaller),
        ConditionsCallee = 
            list.map(reuse_condition_from_called_proc_to_local_condition(
                ModuleInfo, ProcInfo, HeadVars, LuData, SharingAs), 
                ConditionsCaller),
        list.foldl(reuse_as_add_condition(ModuleInfo, ProcInfo), 
            ConditionsCallee, reuse_as_init, LocalReuseAs)
    ).
        
:- func reuse_condition_from_called_proc_to_local_condition(module_info,
    proc_info, prog_vars, live_datastructs, sharing_as, reuse_condition) = 
    reuse_condition.

reuse_condition_from_called_proc_to_local_condition(ModuleInfo, ProcInfo, 
        HeadVars, LuData, SharingAs, CalledCondition) = LocalCondition :- 
    (
        CalledCondition = always, 
        unexpected(this_file, 
            "reuse_condition_from_called_proc_to_local_condition: " ++
            "explicit condition expected.")
    ; 
        CalledCondition = condition(CalledDeadNodes, 
            CalledInUseNodes, CalledSharingAs),

        % Translate the dead nodes: 
        AllDeadNodes = extend_datastructs(ModuleInfo, ProcInfo, 
            SharingAs, CalledDeadNodes),
        AllDeadHeadVarNodes = datastructs_project(HeadVars, AllDeadNodes),

        (
            AllDeadHeadVarNodes = [],
            LocalCondition = always
        ;
            AllDeadHeadVarNodes = [_ | _],
            % Translate the in use nodes:
            AllInUseNodes = extend_datastructs(ModuleInfo, ProcInfo, 
                SharingAs, list.append(LuData, CalledInUseNodes)),
            AllInUseHeadVarNodes = datastructs_project(HeadVars, 
                AllInUseNodes),

            % Translate the sharing information: 
            AllLocalSharing = sharing_as_comb(ModuleInfo, ProcInfo, 
                CalledSharingAs, SharingAs),
            AllHeadVarLocalSharing = sharing_as_project(HeadVars, 
                AllLocalSharing),

            LocalCondition = condition(AllDeadHeadVarNodes, 
                AllInUseHeadVarNodes, AllHeadVarLocalSharing)
        )
    ). 

reuse_as_satisfied(ModuleInfo, ProcInfo, LiveData, SharingAs, StaticVars,
        ReuseAs, Result) :- 
    (
        ReuseAs = no_reuse,
        Result = reuse_not_possible(no_reuse)
    ;
        ReuseAs = unconditional,
        Result = reuse_possible
    ; 
        ReuseAs = conditional(Conditions),
        reuse_as_satisfied_2(ModuleInfo, ProcInfo, LiveData, SharingAs,
            StaticVars, Conditions, Result0),

        % Next to verifying each condition separately, one has to verify
        % whether the nodes which are reused in each of the conditions are
        % not aliased within the current context. If this would be the
        % case, then reuse is not allowed. If this would be allowed, then
        % the callee want to reuse the different parts of the input while
        % these may point to exactly the same structure, resulting in
        % undefined behaviour.
        (
            Result0 = reuse_possible,
            (
                no_aliases_between_reuse_nodes(ModuleInfo, ProcInfo,
                    SharingAs, Conditions)
            ->
                Result = reuse_possible
            ;
                Result = reuse_not_possible(reuse_nodes_have_sharing)
            )
        ;
            Result0 = reuse_not_possible(_),
            Result = Result0
        )
    ).

:- pred reuse_as_satisfied_2(module_info::in, proc_info::in, livedata::in,
    sharing_as::in, prog_vars::in, reuse_conditions::in,
    reuse_satisfied_result::out) is det.

reuse_as_satisfied_2(_, _, _, _, _, [], reuse_possible).
reuse_as_satisfied_2(ModuleInfo, ProcInfo, LiveData, SharingAs, StaticVars,
        [Cond | Conds], Result) :- 
    reuse_condition_satisfied(ModuleInfo, ProcInfo, 
        LiveData, SharingAs, StaticVars, Cond, Result0),
    (
        Result0 = reuse_possible,
        reuse_as_satisfied_2(ModuleInfo, ProcInfo, LiveData, SharingAs,
            StaticVars, Conds, Result)
    ;
        Result0 = reuse_not_possible(_),
        Result = Result0
    ).

:- pred no_aliases_between_reuse_nodes(module_info::in, proc_info::in,
    sharing_as::in, list(reuse_condition)::in) is semidet.

no_aliases_between_reuse_nodes(ModuleInfo, ProcInfo, SharingAs, Conditions):-
    list.filter_map(reuse_condition_reusable_nodes, Conditions, ListNodes),
    list.condense(ListNodes, AllNodes),
    (
        AllNodes = [Node | Rest],
        no_aliases_between_reuse_nodes_2(ModuleInfo, ProcInfo, SharingAs,
            Node, Rest)
    ;
        AllNodes = [],
        unexpected(this_file, "no_aliases_between_reuse_nodes: no nodes")
    ).

:- pred no_aliases_between_reuse_nodes_2(module_info::in, proc_info::in,
    sharing_as::in, datastruct::in, list(datastruct)::in) is semidet.

no_aliases_between_reuse_nodes_2(ModuleInfo, ProcInfo, SharingAs, Node,
        OtherNodes):-
    SharingNodes0 = extend_datastruct(ModuleInfo, ProcInfo, SharingAs, Node),
    list.delete(SharingNodes0, Node, SharingNodes),

    % Check whether none of the structures to which the current Node is
    % aliased is subsumed by or subsumes one of the other nodes, including the
    % current node itself.
    all [SharingNode] (
        list.member(SharingNode, SharingNodes)
    =>
        not there_is_a_subsumption_relation(ModuleInfo, ProcInfo,
            [Node | OtherNodes], SharingNode)
    ),
    (
        OtherNodes = [NextNode | NextOtherNodes],
        no_aliases_between_reuse_nodes_2(ModuleInfo, ProcInfo, SharingAs,
            NextNode, NextOtherNodes)
    ;
        OtherNodes = []
    ).

    % Succeed if Data is subsumed or subsumes some of the datastructures in
    % Datastructs.
    %
:- pred there_is_a_subsumption_relation(module_info::in, proc_info::in,
    list(datastruct)::in, datastruct::in) is semidet.

there_is_a_subsumption_relation(ModuleInfo, ProcInfo, Datastructs, DataA):-
    list.member(DataB, Datastructs),
    (
        datastruct_subsumed_by(ModuleInfo, ProcInfo, DataA, DataB)
    ;
        datastruct_subsumed_by(ModuleInfo, ProcInfo, DataB, DataA)
    ).

%-----------------------------------------------------------------------------%

:- pred reuse_condition_satisfied(module_info::in, proc_info::in,
    livedata::in, sharing_as::in, prog_vars::in, reuse_condition::in,
    reuse_satisfied_result::out) is det.

reuse_condition_satisfied(ModuleInfo, ProcInfo, LiveData, SharingAs, 
        StaticVars, Condition, Result) :- 
    (
        Condition = always,
        Result = reuse_possible
    ;
        Condition = condition(DeadNodes, InUseNodes, SharingNodes),
        % Reuse of static vars is not allowed:
        StaticDeadNodes = datastructs_project(StaticVars, DeadNodes),
        (
            StaticDeadNodes = [],

            % Using the InUseNodes, and the sharing recorded by the condition, 
            % compute a new set of livedata that (safely) approximates the
            % set of livedata that would have been obtained when looking at
            % the program point from where the reuse condition actually comes
            % from.
            NewSharing = sharing_as_comb(ModuleInfo, ProcInfo, SharingNodes, 
                SharingAs),
            UpdatedLiveData = livedata_add_liveness(ModuleInfo, ProcInfo, 
                InUseNodes, NewSharing, LiveData),
            nodes_are_not_live(ModuleInfo, ProcInfo, DeadNodes,
                UpdatedLiveData, NotLiveResult),
            (
                NotLiveResult = nodes_all_live,
                Result = reuse_not_possible(unknown_livedata)
            ;
                NotLiveResult = nodes_are_live(StillLive),
                (
                    StillLive = [],
                    Result = reuse_possible
                ;
                    StillLive = [_ | _],
                    Vars = datastructs_vars(StillLive),
                    Result = reuse_not_possible(reuse_condition_violated(Vars))
                )
            )
        ;
            StaticDeadNodes = [_ | _],
            Vars = datastructs_vars(StaticDeadNodes),
            Result = reuse_not_possible(reuse_condition_violated(Vars))
        )
    ).

%-----------------------------------------------------------------------------%

from_structure_reuse_domain(ReuseDomain) = ReuseAs :- 
    (
        ReuseDomain = has_no_reuse,
        ReuseAs = no_reuse
    ;
        ReuseDomain = has_only_unconditional_reuse,
        ReuseAs = unconditional
    ; 
        ReuseDomain = has_conditional_reuse(PublicReuseConditions),
        ReuseAs = conditional(
            from_public_reuse_conditions(PublicReuseConditions))
    ).

:- func from_public_reuse_conditions(structure_reuse_conditions) = 
    reuse_conditions.

from_public_reuse_conditions(PublicReuseConditions) =
    list.map(from_public_reuse_condition, PublicReuseConditions).

:- func from_public_reuse_condition(structure_reuse_condition) = 
    reuse_condition.

from_public_reuse_condition(PublicReuseCondition) = ReuseCondition :- 
    PublicReuseCondition = structure_reuse_condition(DeadNodes, LiveNodes,
        PublicSharing),
    ReuseCondition = condition(DeadNodes, LiveNodes, 
        from_structure_sharing_domain(PublicSharing)).
   
to_structure_reuse_domain(ReuseAs) = ReuseDomain :- 
    (
        ReuseAs = no_reuse,
        ReuseDomain = has_no_reuse
    ;
        ReuseAs = unconditional,
        ReuseDomain = has_only_unconditional_reuse
    ; 
        ReuseAs = conditional(ReuseConditions),
        ReuseDomain = has_conditional_reuse(
            to_structure_reuse_conditions(ReuseConditions))
    ).

:- func to_structure_reuse_conditions(reuse_conditions) = 
    structure_reuse_conditions.

to_structure_reuse_conditions(ReuseConditions) = 
    list.filter_map(to_structure_reuse_condition, ReuseConditions).

:- func to_structure_reuse_condition(reuse_condition) = 
    structure_reuse_condition is semidet.

to_structure_reuse_condition(Condition) = StructureReuseCondition :- 
    Condition = condition(DeadNodes, LiveNodes, SharingAs), 
    StructureReuseCondition = structure_reuse_condition(DeadNodes, LiveNodes,
        to_structure_sharing_domain(SharingAs)).

%-----------------------------------------------------------------------------%
%
% reuse_as_table
%

reuse_as_table_init = map.init.
reuse_as_table_search(PPId, Table) = Table ^ elem(PPId). 
reuse_as_table_set(PPId, ReuseAs, !Table) :- 
    !:Table = !.Table ^ elem(PPId) := ReuseAs.

reuse_as_table_maybe_dump(DoDump, ModuleInfo, Table, !IO) :-
    (
        DoDump = no
    ;   
        DoDump = yes,
        reuse_as_table_dump(ModuleInfo, Table, !IO)
    ).

:- pred reuse_as_table_dump(module_info::in, reuse_as_table::in,
    io::di, io::uo) is det.

reuse_as_table_dump(ModuleInfo, Table, !IO) :-
    ( map.is_empty(Table) ->
        io.write_string("% ReuseTable: Empty", !IO)
    ;
        io.write_string("% ReuseTable: PPId --> Reuse\n", !IO), 
        map.foldl(dump_entries(ModuleInfo), Table, !IO)
    ).

:- pred dump_entries(module_info::in, pred_proc_id::in, reuse_as::in,
    io::di, io::uo) is det.

dump_entries(ModuleInfo, PPId, ReuseAs, !IO) :-
    io.write_string("% ", !IO),
    write_pred_proc_id(ModuleInfo, PPId, !IO),
    io.write_string("\t--> ", !IO),
    io.write_string(reuse_as_short_description(ReuseAs), !IO),
    io.nl(!IO).

load_structure_reuse_table(ModuleInfo) = ReuseTable :- 
    module_info_predids(PredIds, ModuleInfo, _ModuleInfo),
    list.foldl(load_structure_reuse_table_2(ModuleInfo), PredIds,
        reuse_as_table_init, ReuseTable).

:- pred load_structure_reuse_table_2(module_info::in, pred_id::in,
    reuse_as_table::in, reuse_as_table::out) is det.

load_structure_reuse_table_2(ModuleInfo, PredId, !ReuseTable) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_procids(PredInfo),
    list.foldl(load_structure_reuse_table_3(ModuleInfo, PredId),
        ProcIds, !ReuseTable).

:- pred load_structure_reuse_table_3(module_info::in, pred_id::in,
    proc_id::in, reuse_as_table::in, reuse_as_table::out) is det.

load_structure_reuse_table_3(ModuleInfo, PredId, ProcId, !ReuseTable) :-
    module_info_proc_info(ModuleInfo, PredId, ProcId, ProcInfo),
    proc_info_get_structure_reuse(ProcInfo, MaybePublicReuse),
    (
        MaybePublicReuse = yes(PublicReuse),
        PPId = proc(PredId, ProcId),
        PrivateReuse = from_structure_reuse_domain(PublicReuse),
        reuse_as_table_set(PPId, PrivateReuse, !ReuseTable)
    ;
        MaybePublicReuse = no
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "structure_reuse.domain.m".

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.ctgc.structure_reuse.domain.
%-----------------------------------------------------------------------------%
