%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: higher_order.m.
% Main author: stayl.
% 
% Specializes calls to higher order or polymorphic predicates where the value
% of one or more higher order, type_info or typeclass_info arguments are known.
% 
% Since this creates a new copy of the called procedure I have limited the
% specialization to cases where the called procedure's goal contains less than
% 20 calls and unifications. For predicates above this size the overhead of
% the higher order call becomes less significant while the increase in code
% size becomes significant. The limit can be changed using
% `--higher-order-size-limit'.
% 
% If a specialization creates new opportunities for specialization, the
% specialization process will be iterated until no further opportunities
% arise.  The specialized version for predicate 'foo' is named 'foo.ho<n>',
% where n is a number that uniquely identifies this specialized version.
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module transform_hlds.higher_order.
:- interface.

:- import_module hlds.hlds_module.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred specialize_higher_order(module_info::in, module_info::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.mode_util.
:- import_module check_hlds.polymorphism.
:- import_module check_hlds.type_util.
:- import_module check_hlds.unify_proc.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module hlds.instmap.
:- import_module hlds.pred_table.
:- import_module hlds.quantification.
:- import_module hlds.special_pred.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module transform_hlds.inlining.

:- import_module assoc_list.
:- import_module bool.
:- import_module counter.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module string.
:- import_module svmap.
:- import_module svset.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

    % Iterate collecting requests and processing them until there are no more
    % requests remaining.
    %
specialize_higher_order(!ModuleInfo, !IO) :-
    globals.io_get_globals(Globals, !IO),
    globals.lookup_bool_option(Globals, optimize_higher_order, HigherOrder),
    globals.lookup_bool_option(Globals, type_specialization, TypeSpec),
    globals.lookup_bool_option(Globals, user_guided_type_specialization,
        UserTypeSpec),
    globals.lookup_int_option(Globals, higher_order_size_limit, SizeLimit),
    globals.lookup_int_option(Globals, higher_order_arg_limit, ArgLimit),
    Params = ho_params(HigherOrder, TypeSpec, UserTypeSpec, SizeLimit,
        ArgLimit),
    map.init(NewPreds0),
    map.init(GoalSizes0),
    set.init(Requests0),
    map.init(VersionInfo0),
    some [!Info] (
        !:Info = higher_order_global_info(Requests0, NewPreds0, VersionInfo0,
            !.ModuleInfo, GoalSizes0, Params, counter.init(1)),

        module_info_predids(!.ModuleInfo, PredIds0),
        module_info_get_type_spec_info(!.ModuleInfo, TypeSpecInfo),
        TypeSpecInfo = type_spec_info(_, UserSpecPreds, _, _),
        %
        % Make sure the user requested specializations are processed first,
        % since we don't want to create more versions if one of these
        % matches. We need to process these even if specialization is
        % not being performed in case any of the specialized versions
        % are called from other modules.
        %
        ( set.empty(UserSpecPreds) ->
            PredIds = PredIds0,
            UserSpecPredList = []
        ;
            set.list_to_set(PredIds0, PredIdSet0),
            set.difference(PredIdSet0, UserSpecPreds, PredIdSet),
            set.to_sorted_list(PredIdSet, PredIds),

            set.to_sorted_list(UserSpecPreds, UserSpecPredList),
            !:Info = !.Info ^ ho_params ^ user_type_spec := yes,
            list.foldl(get_specialization_requests, UserSpecPredList, !Info),
            process_requests(!Info, !IO)
        ),

        ( bool.or_list([HigherOrder, TypeSpec, UserTypeSpec], yes) ->
            %
            % Process all other specializations until no more requests
            % are generated.
            %
            list.foldl(get_specialization_requests, PredIds, !Info),
            recursively_process_requests(!Info, !IO)
        ;
            true
        ),
        %
        % Remove the predicates which were used to force the production of
        % user-requested type specializations, since they are not called
        % from anywhere and are no longer needed.
        %
        list.foldl(module_info_remove_predicate,
            UserSpecPredList, !.Info ^ module_info, !:ModuleInfo)
    ).

    % Process one lot of requests, returning requests for any
    % new specializations made possible by the first lot.
    %
:- pred process_requests(higher_order_global_info::in,
    higher_order_global_info::out, io::di, io::uo) is det.

process_requests(!Info, !IO) :-
    filter_requests(Requests, LoopRequests, !Info, !IO),
    (
        Requests = []
    ;
        Requests = [_ | _],
        some [!PredProcsToFix] (
            set.init(!:PredProcsToFix),
            create_new_preds(Requests, [], NewPredList, !PredProcsToFix,
                !Info, !IO),
            list.foldl(check_loop_request(!.Info), LoopRequests,
                !PredProcsToFix),
            set.to_sorted_list(!.PredProcsToFix, PredProcs)
        ),
        fixup_specialized_versions(NewPredList, !Info),
        fixup_preds(PredProcs, !Info),
        (
            NewPredList = [_ | _],
            % The dependencies may have changed, so the dependency graph
            % needs to rebuilt for inlining to work properly.
            module_info_clobber_dependency_info(!.Info ^ module_info,
                ModuleInfo),
            !:Info = !.Info ^ module_info := ModuleInfo
        ;
            NewPredList = []
        )
    ).

    % Process requests until there are no new requests to process.
    %
:- pred recursively_process_requests(higher_order_global_info::in,
    higher_order_global_info::out, io::di, io::uo) is det.

recursively_process_requests(!Info, !IO) :-
    ( set.empty(!.Info ^ requests) ->
        true
    ;
        process_requests(!Info, !IO),
        recursively_process_requests(!Info, !IO)
    ).

%-----------------------------------------------------------------------------%

:- type higher_order_global_info
    --->    higher_order_global_info(
                requests            :: set(request),
                                    % Requested versions.

                new_preds           :: new_preds,
                                    % Specialized versions for each predicate
                                    % not changed by traverse_goal.

                version_info        :: map(pred_proc_id, version_info),
                                    % Extra information about each specialized
                                    % version.

                module_info         :: module_info,
                goal_sizes          :: goal_sizes,
                ho_params           :: ho_params,

                next_ho_id          :: counter
                                    % Number identifying a specialized
                                    % version.
            ).

    % Used while traversing goals.
    %
:- type higher_order_info
    --->    higher_order_info(
                global_info         :: higher_order_global_info,

                pred_vars           :: pred_vars,
                                    % higher_order variables.

                pred_proc_id        :: pred_proc_id,
                                    % pred_proc_id of goal being traversed.

                pred_info           :: pred_info,
                                    % pred_info of goal being traversed.

                proc_info           :: proc_info,
                                    % proc_info of goal being traversed.

                changed             :: changed
            ).

:- type request
    --->    ho_request(
                rq_caller           :: pred_proc_id,
                                    % calling pred

                rq_callee           :: pred_proc_id,
                                    % called pred

                rq_args             :: list(prog_var),
                                    % call args

                rq_tvars            :: list(tvar),
                                    % Type variables for which
                                    % extra type-infos must be
                                    % passed from the caller if
                                    % --typeinfo-liveness is set.

                rq_ho_args          :: list(higher_order_arg),
                rq_caller_types     :: list(mer_type),
                                    % argument types in caller

                rq_typeinfo_liveness :: bool,
                                    % Should the interface of
                                    % the specialized procedure
                                    % use typeinfo liveness?

                rq_caller_tvarset   :: tvarset,
                                    % Caller's typevarset.

                rq_user_req_spec    :: bool,
                                    % Is this a user-requested specialization?

                rq_call_context     :: context
                                    % Context of the call which
                                    % caused the request to be
                                    % generated.
            ).

    % Stores cons_id, index in argument vector, number of
    % curried arguments of a higher order argument, higher-order
    % curried arguments with known values.
    % For cons_ids other than pred_const and `type_info',
    % the arguments must be constants
    %
:- type higher_order_arg
    --->    higher_order_arg(
                hoa_cons_id         :: cons_id,
                hoa_index           :: int,
                                    % Index in argument vector.

                hoa_num_curried_args :: int,
                                    % Number of curried args.

                hoa_curry_arg_in_caller :: list(prog_var),
                                    % Curried arguments in caller.

                hoa_curry_type_in_caller :: list(mer_type),
                                    % Curried argument types in caller.

                hoa_curry_rtti_type :: list(rtti_var_info),
                                    % Types associated with type_infos and
                                    % constraints associated with
                                    % typeclass_infos in the arguments.

                hoa_known_curry_args :: list(higher_order_arg),
                                    % Higher-order curried arguments
                                    % with known values.

                hoa_is_constant     :: bool
                                    % Is this higher_order_arg a constant?
            ).

    % Stores the size of each predicate's goal used in the heuristic
    % to decide which preds are specialized.
    %
:- type goal_sizes == map(pred_id, int).

    % Used to hold the value of known higher order variables.
    % If a variable is not in the map, it does not have a value yet.
    %
:- type pred_vars == map(prog_var, maybe_const).

:- type new_preds == map(pred_proc_id, set(new_pred)).

    % The list of vars is a list of the curried arguments, which must
    % be explicitly passed to the specialized predicate.
    % For cons_ids other than pred_const and `type_info', the arguments
    % must be constants. For pred_consts and type_infos, non-constant
    % arguments are passed through to any specialised version.
    %
:- type maybe_const
    --->    constant(cons_id, list(prog_var))
                                    % Unique possible value.

    ;       multiple_values.        % Multiple possible values,
                                    % cannot specialise.

:- type ho_params
    --->    ho_params(
                optimize_higher_order :: bool,
                                      % Propagate higher-order constants.
                type_spec             :: bool,
                                      % Propagate type-info constants.
                user_type_spec        :: bool,
                                      % User-guided type specialization.
                size_limit            :: int,
                                      % Size limit on requested version.
                arg_limit             :: int
                                      % The maximum size of the higher-order
                                      % arguments of a specialized version.
            ).

:- type version_info
    --->    version_info(
                pred_proc_id,
                        % The procedure from the original program
                        % from which this version was created.

                int,    % Depth of the higher_order_args for this version.

                pred_vars,
                        % Higher-order or constant input variables
                        % for a specialised version.

                list(parent_version_info)
                        % The chain of specialized versions which caused this
                        % version to be created.  For each element in the list
                        % with the same pred_proc_id, the depth must decrease.
                        % This ensures that the specialization process must
                        % terminate.
            ).

:- type parent_version_info
    --->    parent_version_info(
                pred_proc_id,
                        % The procedure from the original program from which
                        % this parent was created.

                int
                        % Depth of the higher_order_args for this version.
            ).

:- type new_pred
    --->    new_pred(
                np_version_ppid         :: pred_proc_id,
                                        % version pred_proc_id
                np_old_ppid             :: pred_proc_id,
                                        % old pred_proc_id
                np_req_ppid             :: pred_proc_id,
                                        % requesting caller
                np_name                 :: sym_name,
                                        % name
                np_spec_args            :: list(higher_order_arg),
                                        % specialized args
                np_unspec_actuals       :: list(prog_var),
                                        % Unspecialised argument vars in
                                        % caller.
                np_extra_act_ti_vars    :: list(tvar),
                                        % Extra typeinfo tvars in caller.
                np_unspec_act_types     :: list(mer_type),
                                        % Unspecialised argument types
                                        % in requesting caller.
                np_typeinfo_liveness    :: bool,
                                        % Does the interface of the
                                        % specialized version use type-info
                                        % liveness?
                np_call_tvarset         :: tvarset,
                                        % Caller's typevarset.
                np_is_user_spec         :: bool
                                        % Is this a user-specified type
                                        % specialization?
            ).

    % Returned by traverse_goal.
    %
:- type changed
    --->    changed     % Need to requantify goal + check other procs
    ;       request     % Need to check other procs
    ;       unchanged.  % Do nothing more for this predicate

:- func get_np_version_ppid(new_pred) = pred_proc_id.

get_np_version_ppid(NewPred) = NewPred ^ np_version_ppid.

%-----------------------------------------------------------------------------%

:- pred get_specialization_requests(pred_id::in,
    higher_order_global_info::in, higher_order_global_info::out) is det.

get_specialization_requests(PredId, !GlobalInfo) :-
    module_info_pred_info(!.GlobalInfo ^ module_info, PredId, PredInfo0),
    NonImportedProcs = pred_info_non_imported_procids(PredInfo0),
    (
        NonImportedProcs = []
    ;
        NonImportedProcs = [ProcId | _],
        MustRecompute = no,
        list.foldl(traverse_proc(MustRecompute, PredId), NonImportedProcs,
            !GlobalInfo),
        module_info_proc_info(!.GlobalInfo ^ module_info, PredId, ProcId,
            ProcInfo),
        proc_info_get_goal(ProcInfo, Goal),
        goal_size(Goal, GoalSize),
        map.set(!.GlobalInfo ^ goal_sizes, PredId, GoalSize, GoalSizes),
        !:GlobalInfo = !.GlobalInfo ^ goal_sizes := GoalSizes
    ).

    % This is called when the first procedure of a predicate was changed.
    % It fixes up all the other procedures, ignoring the goal_size and requests
    % that come out, since that information has already been collected.
    %
:- pred traverse_proc(bool::in, pred_id::in, proc_id::in,
    higher_order_global_info::in, higher_order_global_info::out) is det.

traverse_proc(MustRecompute, PredId, ProcId, !GlobalInfo) :-
    map.init(PredVars0),
    module_info_pred_proc_info(!.GlobalInfo ^ module_info,
        PredId, ProcId, PredInfo0, ProcInfo0),
    Info0 = higher_order_info(!.GlobalInfo, PredVars0, proc(PredId, ProcId),
        PredInfo0, ProcInfo0, unchanged),
    traverse_goal(MustRecompute, Info0, Info),
    Info = higher_order_info(!:GlobalInfo, _, _, PredInfo, ProcInfo, _),
    module_info_set_pred_proc_info(PredId, ProcId, PredInfo, ProcInfo,
        !.GlobalInfo ^ module_info, ModuleInfo),
    !:GlobalInfo = !.GlobalInfo ^ module_info := ModuleInfo.

%-----------------------------------------------------------------------------%
%
% Goal traversal
%

:- pred traverse_goal(bool::in,
    higher_order_info::in, higher_order_info::out) is det.

traverse_goal(MustRecompute, !Info) :-
    VersionInfoMap = !.Info ^ global_info ^ version_info,
    %
    % Lookup the initial known bindings of the variables if this
    % procedure is a specialised version.
    %
    (
        map.search(VersionInfoMap, !.Info ^ pred_proc_id, VersionInfo),
        VersionInfo = version_info(_, _, PredVars, _)
    ->
        !:Info = !.Info ^ pred_vars := PredVars
    ;
        true
    ),
    proc_info_get_goal(!.Info ^ proc_info, Goal0),
    traverse_goal_2(Goal0, Goal, !Info),
    fixup_proc_info(MustRecompute, Goal, !Info).

:- pred fixup_proc_info(bool::in, hlds_goal::in,
    higher_order_info::in, higher_order_info::out) is det.

fixup_proc_info(MustRecompute, Goal0, !Info) :-
    (
        ( !.Info ^ changed = changed
        ; MustRecompute = yes
        )
    ->
        some [!ModuleInfo, !ProcInfo] (
            !:ModuleInfo = !.Info ^ global_info ^ module_info,
            !:ProcInfo   = !.Info ^ proc_info,
            proc_info_set_goal(Goal0, !ProcInfo),
            requantify_proc(!ProcInfo),
            proc_info_get_goal(!.ProcInfo, Goal2),
            RecomputeAtomic = no,
            proc_info_get_initial_instmap(!.ProcInfo, !.ModuleInfo, InstMap),
            proc_info_get_vartypes(!.ProcInfo, VarTypes),
            proc_info_get_inst_varset(!.ProcInfo, InstVarSet),
            recompute_instmap_delta(RecomputeAtomic, Goal2, Goal3,
                VarTypes, InstVarSet, InstMap, !ModuleInfo),
            proc_info_set_goal(Goal3, !ProcInfo),
            !:Info = !.Info ^ proc_info := !.ProcInfo,
            !:Info = !.Info ^ global_info ^ module_info := !.ModuleInfo
        )
    ;
        true
    ).

    % Traverses the goal collecting higher order variables for which the value
    % is known, and specializing calls and adding specialization requests
    % to the request_info structure. The first time through the only predicate
    % we can specialize is call/N. The pred_proc_id is that of the current
    % procedure, used to find out which procedures need fixing up later.
    %
:- pred traverse_goal_2(hlds_goal::in, hlds_goal::out,
    higher_order_info::in, higher_order_info::out) is det.

traverse_goal_2(conj(ConjType, Goals0) - GoalInfo,
        conj(ConjType, Goals) - GoalInfo, !Info) :-
    (
        ConjType = plain_conj,
        list.map_foldl(traverse_goal_2, Goals0, Goals, !Info)
    ;
        ConjType = parallel_conj,
        traverse_independent_goals(Goals0, Goals, !Info)
    ).

traverse_goal_2(disj(Goals0) - GoalInfo, disj(Goals) - GoalInfo, !Info) :-
    traverse_independent_goals(Goals0, Goals, !Info).

traverse_goal_2(switch(Var, CanFail, Cases0) - GoalInfo,
        switch(Var, CanFail, Cases) - GoalInfo, !Info) :-
    % A switch is treated as a disjunction.
    %
    traverse_cases(Cases0, Cases, !Info).

    % Check whether this call could be specialized.
    %
traverse_goal_2(Goal0, Goal, !Info) :-
    Goal0 = generic_call(GenericCall, Args, _, _) - GoalInfo,
    (
        (
            GenericCall = higher_order(Var, _, _, _),
            MaybeMethod = no
        ;
            GenericCall = class_method(Var, Method, _, _),
            MaybeMethod = yes(Method)
        )
    ->
        maybe_specialize_higher_order_call(Var, MaybeMethod, Args,
            Goal0, Goals, !Info),
        conj_list_to_goal(Goals, GoalInfo, Goal)
    ;
        Goal = Goal0
    ).

traverse_goal_2(Goal0, Goal, !Info) :-
    % Check whether this call can be specialized.
    %
    Goal0 = plain_call(_, _, _, _, _, _) - _,
    maybe_specialize_call(Goal0, Goal, !Info).

traverse_goal_2(Goal0, Goal, !Info) :-
    % if-then-elses are handled as disjunctions.
    %
    Goal0 = if_then_else(Vars, Cond0, Then0, Else0) - GoalInfo,
    get_pre_branch_info(!.Info, PreInfo),
    traverse_goal_2(Cond0, Cond, !Info),
    traverse_goal_2(Then0, Then, !Info),
    get_post_branch_info(!.Info, PostThenInfo),
    set_pre_branch_info(PreInfo, !Info),
    traverse_goal_2(Else0, Else, !Info),
    get_post_branch_info(!.Info, PostElseInfo),
    Goal = if_then_else(Vars, Cond, Then, Else) - GoalInfo,
    merge_post_branch_infos(PostThenInfo, PostElseInfo, PostInfo),
    set_post_branch_info(PostInfo, !Info).

traverse_goal_2(negation(NegGoal0) - GoalInfo, negation(NegGoal) - GoalInfo,
        !Info) :-
    traverse_goal_2(NegGoal0, NegGoal, !Info).

traverse_goal_2(scope(Reason, Goal0) - GoalInfo,
        scope(Reason, Goal) - GoalInfo, !Info) :-
    traverse_goal_2(Goal0, Goal, !Info).

traverse_goal_2(Goal, Goal, !Info) :-
    Goal = call_foreign_proc(_, _, _, _, _, _, _) - _.

traverse_goal_2(Goal0, Goal, !Info) :-
    Goal0 = GoalExpr0 - _,
    GoalExpr0 = unify(_, _, _, Unify0, _),
    ( Unify0 = construct(_, pred_const(_, _), _, _, _, _, _) ->
        maybe_specialize_pred_const(Goal0, Goal, !Info)
    ;
        Goal = Goal0
    ),
    ( Goal = unify(_, _, _, Unify, _) - _ ->
        check_unify(Unify, !Info)
    ;
        true
    ).

traverse_goal_2(shorthand(_) - _, _, !Info) :-
    % These should have been expanded out by now.
    unexpected(this_file, "traverse_goal_2: unexpected shorthand").

    % To process a disjunction, we process each disjunct with the
    % specialization information before the goal, then merge the
    % results to give the specialization information after the disjunction.
    %
    % We do the same for parallel conjunctions.
    %
:- pred traverse_independent_goals(hlds_goals::in, hlds_goals::out,
    higher_order_info::in, higher_order_info::out) is det.

traverse_independent_goals(Goals0, Goals, !Info) :-
    % We handle empty lists separately because merge_post_branch_infos_into_one
    % works only on nonempty lists.
    (
        Goals0 = [],
        Goals = []
    ;
        Goals0 = [_ | _],
        get_pre_branch_info(!.Info, PreInfo),
        traverse_independent_goals_2(PreInfo, Goals0, Goals, [], PostInfos,
            !Info),
        merge_post_branch_infos_into_one(PostInfos, PostInfo),
        set_post_branch_info(PostInfo, !Info)
    ).

:- pred traverse_independent_goals_2(pre_branch_info::in,
    hlds_goals::in, hlds_goals::out,
    list(post_branch_info)::in, list(post_branch_info)::out,
    higher_order_info::in, higher_order_info::out) is det.

traverse_independent_goals_2(_, [], [], !PostInfos, !Info).
traverse_independent_goals_2(PreInfo, [Goal0 | Goals0], [Goal | Goals],
        !PostInfos, !Info) :-
    set_pre_branch_info(PreInfo, !Info),
    traverse_goal_2(Goal0, Goal, !Info),
    get_post_branch_info(!.Info, GoalPostInfo),
    !:PostInfos = [GoalPostInfo | !.PostInfos],
    traverse_independent_goals_2(PreInfo, Goals0, Goals, !PostInfos, !Info).

    % Switches are treated in exactly the same way as disjunctions.
    %
:- pred traverse_cases(list(case)::in, list(case)::out,
    higher_order_info::in, higher_order_info::out) is det.

traverse_cases(Cases0, Cases, !Info) :-
    % We handle empty lists separately because merge_post_branch_infos_into_one
    % works only on nonempty lists.
    (
        Cases0 = [],
        unexpected(this_file, "traverse_cases: empty list of cases")
    ;
        Cases0 = [_ | _],
        get_pre_branch_info(!.Info, PreInfo),
        traverse_cases_2(PreInfo, Cases0, Cases, [], PostInfos, !Info),
        merge_post_branch_infos_into_one(PostInfos, PostInfo),
        set_post_branch_info(PostInfo, !Info)
    ).

:- pred traverse_cases_2(pre_branch_info::in, list(case)::in, list(case)::out,
    list(post_branch_info)::in, list(post_branch_info)::out,
    higher_order_info::in, higher_order_info::out) is det.

traverse_cases_2(_, [], [], !PostInfos, !Info).
traverse_cases_2(PreInfo, [Case0 | Cases0], [Case | Cases], !PostInfos,
        !Info) :-
    set_pre_branch_info(PreInfo, !Info),
    Case0 = case(ConsId, Goal0),
    traverse_goal_2(Goal0, Goal, !Info),
    Case = case(ConsId, Goal),
    get_post_branch_info(!.Info, GoalPostInfo),
    !:PostInfos = [GoalPostInfo | !.PostInfos],
    traverse_cases_2(PreInfo, Cases0, Cases, !PostInfos, !Info).

:- type pre_branch_info
    --->    pre_branch_info(pred_vars).

:- type post_branch_info
    --->    post_branch_info(pred_vars).

:- pred get_pre_branch_info(higher_order_info::in, pre_branch_info::out)
    is det.

get_pre_branch_info(Info, pre_branch_info(Info ^ pred_vars)).

:- pred set_pre_branch_info(pre_branch_info::in,
    higher_order_info::in, higher_order_info::out) is det.

set_pre_branch_info(pre_branch_info(PreInfo),
    Info, Info ^ pred_vars := PreInfo).

:- pred get_post_branch_info(higher_order_info::in, post_branch_info::out)
    is det.

get_post_branch_info(Info, post_branch_info(Info ^ pred_vars)).

:- pred set_post_branch_info(post_branch_info::in,
    higher_order_info::in, higher_order_info::out) is det.

set_post_branch_info(post_branch_info(PostInfo),
    Info, Info ^ pred_vars := PostInfo).

    % Merge a bunch of post_branch_infos into one.
    %
    % The algorithm we use has a complexity of N log N, whereas the obvious
    % algorithm is quadratic. Since N can be very large for predicates defined
    % lots of facts, this can be the difference between being able to compile
    % them and having the compiler exhaust available memory in the attempt.
    %
:- pred merge_post_branch_infos_into_one(list(post_branch_info)::in,
    post_branch_info::out) is det.

merge_post_branch_infos_into_one([], _) :-
    unexpected(this_file, "merge_post_branch_infos_into_one: empty list").
merge_post_branch_infos_into_one([PostInfo], PostInfo).
merge_post_branch_infos_into_one(PostInfos @ [_, _ | _], PostInfo) :-
    merge_post_branch_info_pass(PostInfos, [], MergedPostInfos),
    merge_post_branch_infos_into_one(MergedPostInfos, PostInfo).

:- pred merge_post_branch_info_pass(list(post_branch_info)::in,
    list(post_branch_info)::in, list(post_branch_info)::out) is det.

merge_post_branch_info_pass([], !MergedPostInfos).
merge_post_branch_info_pass([PostInfo], !MergedPostInfos) :-
    !:MergedPostInfos = [PostInfo | !.MergedPostInfos].
merge_post_branch_info_pass([PostInfo1, PostInfo2 | Rest], !MergedPostInfos) :-
    merge_post_branch_infos(PostInfo1, PostInfo2, PostInfo12),
    !:MergedPostInfos = [PostInfo12 | !.MergedPostInfos],
    merge_post_branch_info_pass(Rest, !MergedPostInfos).

    % Merge two post_branch_infos.
    %
    % The algorithm we use is designed to minimize worst case complexity,
    % to minimize compilation time for predicates defined by clauses in which
    % each clause contains lots of variables. This will happen e.g. when the
    % clause contains some large ground terms.
    %
    % We separate out the variables that occur in only one post_branch_info
    % to avoid having to process them at all, while allowing the variables
    % occur in both post_branch_infos to be processed using a linear algorithm.
    % The algorithm here is mostly linear, with an extra log N factor coming in
    % from the operations on maps.
    %
:- pred merge_post_branch_infos(post_branch_info::in,
    post_branch_info::in, post_branch_info::out) is det.

merge_post_branch_infos(PostA, PostB, Post) :-
    PostA = post_branch_info(VarConstMapA),
    PostB = post_branch_info(VarConstMapB),
    map.keys(VarConstMapA, VarListA),
    map.keys(VarConstMapB, VarListB),
    set.sorted_list_to_set(VarListA, VarsA),
    set.sorted_list_to_set(VarListB, VarsB),
    set.intersect(VarsA, VarsB, CommonVars),
    VarConstCommonMapA = map.select(VarConstMapA, CommonVars),
    VarConstCommonMapB = map.select(VarConstMapB, CommonVars),
    map.to_assoc_list(VarConstCommonMapA, VarConstCommonListA),
    map.to_assoc_list(VarConstCommonMapB, VarConstCommonListB),
    merge_common_var_const_list(VarConstCommonListA, VarConstCommonListB,
        [], VarConstCommonList),
    set.difference(VarsA, CommonVars, OnlyVarsA),
    set.difference(VarsB, CommonVars, OnlyVarsB),
    VarConstOnlyMapA = map.select(VarConstMapA, OnlyVarsA),
    VarConstOnlyMapB = map.select(VarConstMapB, OnlyVarsB),
    map.to_assoc_list(VarConstOnlyMapA, VarConstOnlyListA),
    map.to_assoc_list(VarConstOnlyMapB, VarConstOnlyListB),
    FinalList = VarConstOnlyListA ++ VarConstOnlyListB ++ VarConstCommonList,
    map.from_assoc_list(FinalList, FinalVarConstMap),
    Post = post_branch_info(FinalVarConstMap).

:- pred merge_common_var_const_list(assoc_list(prog_var, maybe_const)::in,
    assoc_list(prog_var, maybe_const)::in,
    assoc_list(prog_var, maybe_const)::in,
    assoc_list(prog_var, maybe_const)::out) is det.

merge_common_var_const_list([], [], !List).
merge_common_var_const_list([], [_ | _], !MergedList) :-
    unexpected(this_file, "merge_common_var_const_list: mismatched list").
merge_common_var_const_list([_ | _], [], !MergedList) :-
    unexpected(this_file, "merge_common_var_const_list: mismatched list").
merge_common_var_const_list([VarA - ValueA | ListA], [VarB - ValueB | ListB],
        !MergedList) :-
    expect(unify(VarA, VarB), this_file,
        "merge_common_var_const_list: var mismatch"),
    ( ValueA = ValueB ->
        % It does not matter whether ValueA is bound to constant(_, _)
        % or to multiple_values, in both cases, if ValueA = ValueB, the
        % right value for Value is ValueA.
        Value = ValueA
    ;
        % Either ValueA and ValueB are both bound to different constants,
        % or one is constant and the other is multiple_values. In both cases,
        % the right value for Value is multiple_values.
        Value = multiple_values
    ),
    !:MergedList = [VarA - Value | !.MergedList],
    merge_common_var_const_list(ListA, ListB, !MergedList).

:- pred check_unify(unification::in,
    higher_order_info::in, higher_order_info::out) is det.

check_unify(simple_test(_, _), !Info).
    % Testing two higher order terms for equality is not allowed.
check_unify(assign(Var1, Var2), !Info) :-
    maybe_add_alias(Var1, Var2, !Info).
check_unify(deconstruct(_, _, _, _, _, _), !Info).
    % Deconstructing a higher order term is not allowed.
check_unify(construct(LVar, ConsId, Args, _Modes, _, _, _), !Info) :-
    ( is_interesting_cons_id(!.Info ^ global_info ^ ho_params, ConsId) = yes ->
        ( map.search(!.Info ^ pred_vars, LVar, Specializable) ->
            (
                % We cannot specialize calls involving a variable with
                % more than one possible value.
                Specializable = constant(_, _),
                map.det_update(!.Info ^ pred_vars, LVar,
                    multiple_values, PredVars),
                !:Info = !.Info ^ pred_vars := PredVars
            ;
                % If a variable is already non-specializable, it can't
                % become specializable.
                Specializable = multiple_values
            )
        ;
            map.det_insert(!.Info ^ pred_vars, LVar,
                constant(ConsId, Args), PredVars),
            !:Info = !.Info ^ pred_vars := PredVars
        )
    ;
        true
    ).
check_unify(complicated_unify(_, _, _), !Info) :-
    unexpected(this_file, "check_unify - complicated unification").

:- func is_interesting_cons_id(ho_params, cons_id) = bool.

is_interesting_cons_id(_Params, cons(_, _)) = no.
    % We need to keep track of int_consts so we can interpret
    % superclass_info_from_typeclass_info and
    % typeinfo_from_typeclass_info.  We don't specialize based on them.
is_interesting_cons_id(Params, int_const(_)) = Params ^ user_type_spec.
is_interesting_cons_id(_Params, string_const(_)) = no.
is_interesting_cons_id(_Params, float_const(_)) = no.
is_interesting_cons_id(Params, pred_const(_, _)) =
    Params ^ optimize_higher_order.
is_interesting_cons_id(Params, type_ctor_info_const(_, _, _)) =
    Params ^ user_type_spec.
is_interesting_cons_id(Params, base_typeclass_info_const(_, _, _, _)) =
    Params ^ user_type_spec.
is_interesting_cons_id(Params, type_info_cell_constructor(_)) =
    Params ^ user_type_spec.
is_interesting_cons_id(Params, typeclass_info_cell_constructor) =
    Params ^ user_type_spec.
is_interesting_cons_id(_Params, tabling_info_const(_)) = no.
is_interesting_cons_id(_Params, deep_profiling_proc_layout(_)) = no.
is_interesting_cons_id(_Params, table_io_decl(_)) = no.

    % Process a higher-order call or class_method_call to see if it
    % could possibly be specialized.
    %
:- pred maybe_specialize_higher_order_call(prog_var::in, maybe(int)::in,
    list(prog_var)::in, hlds_goal::in, list(hlds_goal)::out,
    higher_order_info::in, higher_order_info::out) is det.

maybe_specialize_higher_order_call(PredVar, MaybeMethod, Args,
        Goal0 - GoalInfo, Goals, !Info) :-
    ModuleInfo = !.Info ^ global_info ^ module_info,
    % We can specialize calls to call/N and class_method_call/N
    % if the closure or typeclass_info has a known value.
    (
        map.search(!.Info ^ pred_vars, PredVar, constant(ConsId, CurriedArgs)),
        (
            ConsId = pred_const(ShroudedPredProcId, _),
            MaybeMethod = no
        ->
            proc(PredId, ProcId) = unshroud_pred_proc_id(ShroudedPredProcId),
            list.append(CurriedArgs, Args, AllArgs)
        ;
            % A typeclass_info variable should consist of a known
            % base_typeclass_info and some argument typeclass_infos.
            %
            ConsId = typeclass_info_cell_constructor,
            CurriedArgs = [BaseTypeClassInfo | OtherTypeClassArgs],
            map.search(!.Info ^ pred_vars, BaseTypeClassInfo,
                constant(BaseConsId, _)),
            BaseConsId = base_typeclass_info_const(_, ClassId, Instance, _),
            MaybeMethod = yes(Method),
            module_info_get_instance_table(ModuleInfo, Instances),
            map.lookup(Instances, ClassId, InstanceList),
            list.index1_det(InstanceList, Instance, InstanceDefn),
            InstanceDefn = hlds_instance_defn(_, _, _,
                InstanceConstraints, InstanceTypes0, _,
                yes(ClassInterface), _, _),
            type_vars_list(InstanceTypes0, InstanceTvars),
            get_unconstrained_tvars(InstanceTvars,
                InstanceConstraints, UnconstrainedTVars),
            NumArgsToExtract = list.length(InstanceConstraints)
                + list.length(UnconstrainedTVars),
            list.take(NumArgsToExtract, OtherTypeClassArgs,
                InstanceConstraintArgs)
        ->
            list.index1_det(ClassInterface, Method,
                hlds_class_proc(PredId, ProcId)),
            list.append(InstanceConstraintArgs, Args, AllArgs)
        ;
            fail
        )
    ->
        construct_specialized_higher_order_call(PredId, ProcId,
            AllArgs, GoalInfo, Goal, !Info),
        Goals = [Goal]
    ;
        % Handle a class method call where we know which instance is being
        % used, but we haven't seen a construction for the typeclass_info.
        % This can happen for user-guided typeclass specialization, because
        % the type-specialized class constraint is still in the constraint
        % list, so a typeclass_info is passed in by the caller rather than
        % being constructed locally.
        %
        % The problem is that in importing modules we don't know which
        % instance declarations are visible in the imported module, so we
        % don't know which class constraints are redundant after type
        % specialization.
        %
        MaybeMethod = yes(Method),

        CallerProcInfo0 = !.Info ^ proc_info,
        CallerPredInfo0 = !.Info ^ pred_info,
        proc_info_get_rtti_varmaps(CallerProcInfo0, CallerRttiVarMaps),
        rtti_varmaps_var_info(CallerRttiVarMaps, PredVar,
            typeclass_info_var(ClassConstraint)),
        ClassConstraint = constraint(ClassName, ClassArgs),
        list.length(ClassArgs, ClassArity),
        module_info_get_instance_table(ModuleInfo, InstanceTable),
        map.lookup(InstanceTable, class_id(ClassName, ClassArity), Instances),
        pred_info_get_typevarset(CallerPredInfo0, TVarSet0),
        find_matching_instance_method(Instances, Method, ClassArgs,
            PredId, ProcId, InstanceConstraints, UnconstrainedTVarTypes,
            TVarSet0, TVarSet)
    ->
        pred_info_set_typevarset(TVarSet, CallerPredInfo0, CallerPredInfo),
        % Pull out the argument typeclass_infos.
        (
            InstanceConstraints = [],
            UnconstrainedTVarTypes = []
        ->
            ExtraGoals = [],
            CallerProcInfo = CallerProcInfo0,
            AllArgs = Args
        ;
            get_unconstrained_instance_type_infos(ModuleInfo,
                PredVar, UnconstrainedTVarTypes, 1,
                ArgTypeInfoGoals, ArgTypeInfoVars,
                CallerProcInfo0, CallerProcInfo1),
            FirstArgTypeclassInfo = list.length(UnconstrainedTVarTypes) + 1,
            get_arg_typeclass_infos(ModuleInfo, PredVar,
                InstanceConstraints, FirstArgTypeclassInfo,
                ArgTypeClassInfoGoals, ArgTypeClassInfoVars,
                CallerProcInfo1, CallerProcInfo),
            list.condense([ArgTypeInfoVars, ArgTypeClassInfoVars, Args],
                AllArgs),
            list.append(ArgTypeInfoGoals, ArgTypeClassInfoGoals, ExtraGoals)
        ),
        !:Info = !.Info ^ pred_info := CallerPredInfo,
        !:Info = !.Info ^ proc_info := CallerProcInfo,
        construct_specialized_higher_order_call(PredId, ProcId,
            AllArgs, GoalInfo, Goal, !Info),
        list.append(ExtraGoals, [Goal], Goals)
    ;
        % Non-specializable call/N or class_method_call/N.
        Goals = [Goal0 - GoalInfo]
    ).

:- pred find_matching_instance_method(list(hlds_instance_defn)::in, int::in,
    list(mer_type)::in, pred_id::out, proc_id::out,
    list(prog_constraint)::out, list(mer_type)::out,
    tvarset::in, tvarset::out) is semidet.

find_matching_instance_method([Instance | Instances], MethodNum, ClassTypes,
        PredId, ProcId, Constraints, UnconstrainedTVarTypes, !TVarSet) :-
    (
        instance_matches(ClassTypes, Instance, Constraints0,
            UnconstrainedTVarTypes0, !TVarSet)
    ->
        Constraints = Constraints0,
        UnconstrainedTVarTypes = UnconstrainedTVarTypes0,
        yes(ClassInterface) = Instance ^ instance_hlds_interface,
        list.index1_det(ClassInterface, MethodNum,
            hlds_class_proc(PredId, ProcId))
    ;
        find_matching_instance_method(Instances, MethodNum, ClassTypes,
            PredId, ProcId, Constraints, UnconstrainedTVarTypes, !TVarSet)
    ).

:- pred instance_matches(list(mer_type)::in, hlds_instance_defn::in,
    list(prog_constraint)::out, list(mer_type)::out,
    tvarset::in, tvarset::out) is semidet.

instance_matches(ClassTypes, Instance, Constraints, UnconstrainedTVarTypes,
        TVarSet0, TVarSet) :-
    Instance = hlds_instance_defn(_, _, _, Constraints0,
        InstanceTypes0, _, _, InstanceTVarSet, _),
    tvarset_merge_renaming(TVarSet0, InstanceTVarSet, TVarSet, Renaming),
    apply_variable_renaming_to_type_list(Renaming, InstanceTypes0,
        InstanceTypes),
    apply_variable_renaming_to_prog_constraint_list(Renaming, Constraints0,
        Constraints1),
    type_vars_list(InstanceTypes, InstanceTVars),
    get_unconstrained_tvars(InstanceTVars, Constraints1, UnconstrainedTVars0),

    type_list_subsumes(InstanceTypes, ClassTypes, Subst),
    apply_rec_subst_to_prog_constraint_list(Subst, Constraints1, Constraints),

    % XXX kind inference:
    % we assume all tvars have kind `star'.
    map.init(KindMap),
    apply_rec_subst_to_tvar_list(KindMap, Subst, UnconstrainedTVars0,
        UnconstrainedTVarTypes).

    % Build calls to
    % `private_builtin.instance_constraint_from_typeclass_info/3'
    % to extract the typeclass_infos for the constraints on an instance.
    % This simulates the action of `do_call_class_method' in
    % runtime/mercury_ho_call.c.
    %
:- pred get_arg_typeclass_infos(module_info::in, prog_var::in,
    list(prog_constraint)::in, int::in, list(hlds_goal)::out,
    list(prog_var)::out, proc_info::in, proc_info::out) is det.

get_arg_typeclass_infos(ModuleInfo, TypeClassInfoVar, InstanceConstraints,
        Index, Goals, Vars, !ProcInfo) :-

    MakeResultType = polymorphism.build_typeclass_info_type,
    get_typeclass_info_args(ModuleInfo, TypeClassInfoVar,
        "instance_constraint_from_typeclass_info", MakeResultType,
        InstanceConstraints, Index, Goals, Vars, !ProcInfo).

    % Build calls to
    % `private_builtin.unconstrained_type_info_from_typeclass_info/3'
    % to extract the type-infos for the unconstrained type variables
    % of an instance declaration.
    % This simulates the action of `do_call_class_method' in
    % runtime/mercury_ho_call.c.
    %
:- pred get_unconstrained_instance_type_infos(module_info::in,
    prog_var::in, list(mer_type)::in, int::in, list(hlds_goal)::out,
    list(prog_var)::out, proc_info::in, proc_info::out) is det.

get_unconstrained_instance_type_infos(ModuleInfo, TypeClassInfoVar,
        UnconstrainedTVarTypes, Index, Goals, Vars, !ProcInfo) :-
    MakeResultType = polymorphism.build_type_info_type,
    get_typeclass_info_args(ModuleInfo, TypeClassInfoVar,
        "unconstrained_type_info_from_typeclass_info",
        MakeResultType, UnconstrainedTVarTypes,
        Index, Goals, Vars, !ProcInfo).

:- pred get_typeclass_info_args(module_info::in, prog_var::in, string::in,
    pred(T, mer_type)::(pred(in, out) is det),
    list(T)::in, int::in, list(hlds_goal)::out,
    list(prog_var)::out, proc_info::in, proc_info::out) is det.

get_typeclass_info_args(ModuleInfo, TypeClassInfoVar, PredName, MakeResultType,
        Args, Index, Goals, Vars, !ProcInfo) :-
    lookup_builtin_pred_proc_id(ModuleInfo, mercury_private_builtin_module,
        PredName, predicate, 3, only_mode, ExtractArgPredId, ExtractArgProcId),
    get_typeclass_info_args_2(TypeClassInfoVar,
        ExtractArgPredId, ExtractArgProcId,
        qualified(mercury_private_builtin_module, PredName),
        MakeResultType, Args, Index, Goals, Vars, !ProcInfo).

:- pred get_typeclass_info_args_2(prog_var::in, pred_id::in, proc_id::in,
    sym_name::in, pred(T, mer_type)::(pred(in, out) is det),
    list(T)::in, int::in, list(hlds_goal)::out,
    list(prog_var)::out, proc_info::in, proc_info::out) is det.

get_typeclass_info_args_2(_, _, _, _, _, [], _, [], [], !ProcInfo).
get_typeclass_info_args_2(TypeClassInfoVar, PredId, ProcId, SymName,
        MakeResultType, [Arg | Args], Index, [IndexGoal, CallGoal | Goals],
        [ResultVar | Vars], !ProcInfo) :-
    MakeResultType(Arg, ResultType),
    proc_info_create_var_from_type(ResultType, no, ResultVar, !ProcInfo),
    MaybeContext = no,
    make_int_const_construction_alloc_in_proc(Index, no, IndexGoal, IndexVar,
        !ProcInfo),
    CallArgs = [TypeClassInfoVar, IndexVar, ResultVar],

    set.list_to_set(CallArgs, NonLocals),
    instmap_delta_init_reachable(InstMapDelta0),
    instmap_delta_insert(ResultVar, ground(shared, none),
        InstMapDelta0, InstMapDelta),
    goal_info_init(NonLocals, InstMapDelta, detism_det, purity_pure, GoalInfo),
    CallGoal = plain_call(PredId, ProcId, CallArgs, not_builtin,
        MaybeContext, SymName) - GoalInfo,
    get_typeclass_info_args_2(TypeClassInfoVar, PredId, ProcId, SymName,
        MakeResultType, Args, Index + 1, Goals, Vars, !ProcInfo).

%-----------------------------------------------------------------------------%

:- pred construct_specialized_higher_order_call(pred_id::in, proc_id::in,
    list(prog_var)::in, hlds_goal_info::in, hlds_goal::out,
    higher_order_info::in, higher_order_info::out) is det.

construct_specialized_higher_order_call(PredId, ProcId, AllArgs, GoalInfo,
        Goal - GoalInfo, !Info) :-
    ModuleInfo = !.Info ^ global_info ^ module_info,
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ModuleName = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    SymName = qualified(ModuleName, PredName),
    proc(CallerPredId, _) = !.Info ^ pred_proc_id,
    Builtin = builtin_state(ModuleInfo, CallerPredId, PredId, ProcId),

    MaybeContext = no,
    Goal1 = plain_call(PredId, ProcId, AllArgs, Builtin, MaybeContext,
        SymName),
    !:Info = !.Info ^ changed := changed,
    maybe_specialize_call(Goal1 - GoalInfo, Goal - _, !Info).

:- pred maybe_specialize_call(hlds_goal::in, hlds_goal::out,
    higher_order_info::in, higher_order_info::out) is det.

maybe_specialize_call(Goal0 - GoalInfo, Goal - GoalInfo, !Info) :-
    ModuleInfo0 = !.Info ^ global_info ^ module_info,
    ( Goal0 = plain_call(_, _, _, _, _, _) ->
        Goal0 = plain_call(CalledPred, CalledProc, Args0, IsBuiltin,
            MaybeContext, _SymName0)
    ;
        unexpected(this_file, "maybe_specialize_call: expected call")
    ),
    module_info_pred_proc_info(ModuleInfo0, CalledPred, CalledProc,
        CalleePredInfo, CalleeProcInfo),
    module_info_get_globals(ModuleInfo0, Globals),
    globals.lookup_bool_option(Globals, special_preds, HaveSpecialPreds),
    (
        % Look for calls to unify/2 and compare/3 that can be specialized.
        specialize_special_pred(CalledPred, CalledProc, Args0,
            MaybeContext, GoalInfo, HaveSpecialPreds, Goal1, !Info)
    ->
        Goal = Goal1,
        !:Info = !.Info ^ changed := changed
    ;
        polymorphism.is_typeclass_info_manipulator(ModuleInfo0,
            CalledPred, Manipulator)
    ->
        interpret_typeclass_info_manipulator(Manipulator, Args0,
            Goal0, Goal, !Info)
    ;
        (
            pred_info_is_imported(CalleePredInfo),
            module_info_get_type_spec_info(ModuleInfo0, TypeSpecInfo),
            TypeSpecInfo = type_spec_info(TypeSpecProcs, _, _, _),
            \+ set.member(proc(CalledPred, CalledProc), TypeSpecProcs)
        ;
            pred_info_is_pseudo_imported(CalleePredInfo),
            hlds_pred.in_in_unification_proc_id(CalledProc)
        ;
            pred_info_pragma_goal_type(CalleePredInfo)
        )
    ->
        Goal = Goal0
    ;
        CanRequest = yes,
        maybe_specialize_ordinary_call(CanRequest, CalledPred, CalledProc,
            CalleePredInfo, CalleeProcInfo, Args0, IsBuiltin, MaybeContext,
            GoalInfo, Result, !Info),
        (
            Result = specialized(ExtraTypeInfoGoals, Goal1),
            goal_to_conj_list(Goal1 - GoalInfo, GoalList1),
            list.append(ExtraTypeInfoGoals, GoalList1, GoalList),
            Goal = conj(plain_conj, GoalList)
        ;
            Result = not_specialized,
            Goal = Goal0
        )
    ).

    % Try to specialize constructions of higher-order terms.
    % This is useful if we don't have the code for predicates
    % to which this higher-order term is passed.
    %
    % The specialization is done by treating
    %   Pred = foo(A, B, ...)
    % as
    %   pred(X::<mode1>, Y::<mode2>, ...) is <det> :-
    %       foo(A, B, ..., X, Y, ...)
    % and specializing the call.
    %
:- pred maybe_specialize_pred_const(hlds_goal::in, hlds_goal::out,
    higher_order_info::in, higher_order_info::out) is det.

maybe_specialize_pred_const(Goal0 - GoalInfo, Goal - GoalInfo, !Info) :-
    NewPreds   = !.Info ^ global_info ^ new_preds,
    ModuleInfo = !.Info ^ global_info ^ module_info,
    ProcInfo0  = !.Info ^ proc_info,
    (
        Goal0 = unify(_, _, UniMode, Unify0, Context),
        Unify0 = construct(LVar, ConsId0, Args0, _,
            HowToConstruct, CellIsUnique, SubInfo),
        (
            SubInfo = no_construct_sub_info
        ;
            SubInfo = construct_sub_info(no, no)
        ),
        ConsId0 = pred_const(ShroudedPredProcId, EvalMethod),
        PredProcId = unshroud_pred_proc_id(ShroudedPredProcId),
        proc(PredId, ProcId) = PredProcId,
        map.contains(NewPreds, PredProcId),
        proc_info_get_vartypes(ProcInfo0, VarTypes0),
        map.lookup(VarTypes0, LVar, LVarType),
        type_is_higher_order_details(LVarType, _, _, _, ArgTypes)
    ->
        % Create variables to represent
        proc_info_create_vars_from_types(ArgTypes, UncurriedArgs,
            ProcInfo0, ProcInfo1),
        list.append(Args0, UncurriedArgs, Args1),
        !:Info = !.Info ^ proc_info := ProcInfo1,

        module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
            CalleePredInfo, CalleeProcInfo),
        %
        % We don't create requests for higher-order terms because that would
        % result in duplication of effort if all uses of the constant end up
        % being specialized.  For parser combinator programs it would also
        % result in huge numbers of requests with no easy way to control which
        % ones should be created.
        %
        CanRequest = no,
        IsBuiltin = not_builtin,
        MaybeContext = no,
        maybe_specialize_ordinary_call(CanRequest, PredId, ProcId,
            CalleePredInfo, CalleeProcInfo, Args1, IsBuiltin, MaybeContext,
            GoalInfo, Result, !Info),
        (
            Result = specialized(ExtraTypeInfoGoals0, Goal1),
            (
                Goal1 = plain_call(NewPredId0, NewProcId0, NewArgs0, _, _, _),
                list.remove_suffix(NewArgs0, UncurriedArgs, NewArgs1)
            ->
                NewPredId = NewPredId0,
                NewProcId = NewProcId0,
                NewArgs = NewArgs1
            ;
                unexpected(this_file, "maybe_specialize_pred_const")
            ),

            module_info_proc_info(ModuleInfo, NewPredId, NewProcId,
                NewCalleeProcInfo),
            proc_info_get_argmodes(NewCalleeProcInfo, NewCalleeArgModes),
            (
                list.take(list.length(NewArgs), NewCalleeArgModes,
                    CurriedArgModes0)
            ->
                CurriedArgModes = CurriedArgModes0
            ;
                unexpected(this_file, "maybe_specialize_pred_const")
            ),
            modes_to_uni_modes(ModuleInfo, CurriedArgModes,
                CurriedArgModes, UniModes),

            % The dummy arguments can't be used anywhere.
            ProcInfo2 = !.Info ^ proc_info,
            proc_info_get_vartypes(ProcInfo2, VarTypes2),
            map.delete_list(VarTypes2, UncurriedArgs, VarTypes),
            proc_info_set_vartypes(VarTypes, ProcInfo2, ProcInfo),
            !:Info = !.Info ^ proc_info := ProcInfo,

            NewPredProcId = proc(NewPredId, NewProcId),
            NewShroudedPredProcId = shroud_pred_proc_id(NewPredProcId),
            NewConsId = pred_const(NewShroudedPredProcId, EvalMethod),
            Unify = construct(LVar, NewConsId, NewArgs, UniModes,
                HowToConstruct, CellIsUnique, no_construct_sub_info),
            Goal2 = unify(LVar, rhs_functor(NewConsId, no, NewArgs),
                UniMode, Unify, Context),

            % Make sure any constants in the ExtraTypeInfoGoals are recorded.
            list.map_foldl(traverse_goal_2, ExtraTypeInfoGoals0,
                ExtraTypeInfoGoals, !Info),
            (
                ExtraTypeInfoGoals = [],
                Goal = Goal2
            ;
                ExtraTypeInfoGoals = [_ | _],
                Goal = conj(plain_conj,
                    ExtraTypeInfoGoals ++ [Goal2 - GoalInfo])
            )
        ;
            Result = not_specialized,
            % The dummy arguments can't be used anywhere.
            !:Info = !.Info ^ proc_info := ProcInfo0,
            Goal = Goal0
        )
    ;
        Goal = Goal0
    ).

:- type specialization_result
    --->    specialized(
                list(hlds_goal),    % Goals to construct extra
                                    % type-infos.
                hlds_goal_expr      % The specialized call.
            )
    ;       not_specialized.

:- pred maybe_specialize_ordinary_call(bool::in, pred_id::in, proc_id::in,
    pred_info::in, proc_info::in, list(prog_var)::in, builtin_state::in,
    maybe(call_unify_context)::in, hlds_goal_info::in,
    specialization_result::out, higher_order_info::in, higher_order_info::out)
    is det.

maybe_specialize_ordinary_call(CanRequest, CalledPred, CalledProc,
        CalleePredInfo, CalleeProcInfo, Args0, IsBuiltin,
        MaybeContext, GoalInfo, Result, !Info) :-
    ModuleInfo0 = !.Info ^ global_info ^ module_info,
    pred_info_get_import_status(CalleePredInfo, CalleeStatus),
    proc_info_get_vartypes(CalleeProcInfo, CalleeVarTypes),
    proc_info_get_headvars(CalleeProcInfo, CalleeHeadVars),
    map.apply_to_list(CalleeHeadVars, CalleeVarTypes, CalleeArgTypes),

    CallerProcInfo0 = !.Info ^ proc_info,
    proc_info_get_vartypes(CallerProcInfo0, VarTypes),
    proc_info_get_rtti_varmaps(CallerProcInfo0, RttiVarMaps),
    find_higher_order_args(ModuleInfo0, CalleeStatus, Args0,
        CalleeArgTypes, VarTypes, RttiVarMaps, !.Info ^ pred_vars, 1,
        [], HigherOrderArgs0),

    proc(CallerPredId, _) = !.Info ^ pred_proc_id,
    module_info_get_type_spec_info(ModuleInfo0, TypeSpecInfo),
    TypeSpecInfo = type_spec_info(_, ForceVersions, _, _),
    IsUserSpecProc = ( set.member(CallerPredId, ForceVersions) -> yes ; no ),
    (
        (
            HigherOrderArgs0 = [_ | _]
        ;
            % We should create these even if there is no specialization
            % to avoid link errors.
            IsUserSpecProc = yes
        ;
            !.Info ^ global_info ^ ho_params ^ user_type_spec = yes,
            map.apply_to_list(Args0, VarTypes, ArgTypes),

            % Check whether any typeclass constraints now match an instance.
            pred_info_get_class_context(CalleePredInfo, CalleeClassContext),
            CalleeClassContext = constraints(CalleeUnivConstraints0, _),
            pred_info_get_typevarset(CalleePredInfo, CalleeTVarSet),
            pred_info_get_exist_quant_tvars(CalleePredInfo, CalleeExistQTVars),
            CallerPredInfo0 = !.Info ^ pred_info,
            pred_info_get_typevarset(CallerPredInfo0, TVarSet),
            pred_info_get_univ_quant_tvars(CallerPredInfo0, CallerUnivQTVars),
            type_subst_makes_instance_known(ModuleInfo0,
                CalleeUnivConstraints0, TVarSet,
                CallerUnivQTVars, ArgTypes, CalleeTVarSet,
                CalleeExistQTVars, CalleeArgTypes)
        )
    ->
        list.reverse(HigherOrderArgs0, HigherOrderArgs),
        goal_info_get_context(GoalInfo, Context),
        find_matching_version(!.Info, CalledPred, CalledProc, Args0,
            Context, HigherOrderArgs, IsUserSpecProc, FindResult),
        (
            FindResult = find_result_match(match(Match, _, Args1,
                ExtraTypeInfoTypes)),
            Match = new_pred(NewPredProcId, _, _, NewName, _HOArgs,
                _, _, _, _, _, _),
            NewPredProcId = proc(NewCalledPred, NewCalledProc),

            construct_extra_type_infos(ExtraTypeInfoTypes,
                ExtraTypeInfoVars, ExtraTypeInfoGoals, !Info),

            list.append(ExtraTypeInfoVars, Args1, Args),
            CallGoal = plain_call(NewCalledPred, NewCalledProc, Args,
                IsBuiltin, MaybeContext, NewName),
            Result = specialized(ExtraTypeInfoGoals, CallGoal),
            !:Info = !.Info ^ changed := changed
        ;
            % There is a known higher order variable in the call, so we
            % put in a request for a specialized version of the pred.
            FindResult = find_result_request(Request),
            Result = not_specialized,
            (
                CanRequest = yes,
                set.insert(!.Info ^ global_info ^ requests, Request, Requests),
                update_changed_status(!.Info ^ changed, request, Changed),
                !:Info = !.Info ^ global_info ^ requests := Requests,
                !:Info = !.Info ^ changed := Changed
            ;
                CanRequest = no
            )
        ;
            FindResult = find_result_no_request,
            Result = not_specialized
        )
    ;
        Result = not_specialized
    ).

    % Returns a list of the higher-order arguments in a call that have
    % a known value.
    %
:- pred find_higher_order_args(module_info::in, import_status::in,
    list(prog_var)::in, list(mer_type)::in, vartypes::in,
    rtti_varmaps::in, pred_vars::in, int::in, list(higher_order_arg)::in,
    list(higher_order_arg)::out) is det.

find_higher_order_args(_, _, [], _, _, _, _, _, !HOArgs).
find_higher_order_args(_, _, [_ | _], [], _, _, _, _, _, _) :-
    unexpected(this_file, "find_higher_order_args: length mismatch").
find_higher_order_args(ModuleInfo, CalleeStatus, [Arg | Args],
        [CalleeArgType | CalleeArgTypes], VarTypes, RttiVarMaps,
        PredVars, ArgNo, !HOArgs) :-
    NextArg = ArgNo + 1,
    (
        % We don't specialize arguments whose declared type is polymorphic.
        % The closure they pass cannot possibly be called within the called
        % predicate, since that predicate doesn't know it's a closure
        % (without some dodgy use of type_to_univ and univ_to_type).
        map.search(PredVars, Arg, constant(ConsId, CurriedArgs)),

        % We don't specialize based on int_consts (we only keep track of them
        % to interpret calls to the procedures which extract fields from
        % typeclass_infos).
        ConsId \= int_const(_),

        ( ConsId = pred_const(_, _) ->
            % If we don't have clauses for the callee, we can't specialize
            % any higher-order arguments. We may be able to do user guided
            % type specialization.
            CalleeStatus \= status_imported(_),
            CalleeStatus \= status_external(_),
            type_is_higher_order(CalleeArgType)
        ;
            true
        )
    ->
        % Find any known higher-order arguments in the list of curried
        % arguments.
        map.apply_to_list(CurriedArgs, VarTypes, CurriedArgTypes),
        list.map(rtti_varmaps_var_info(RttiVarMaps), CurriedArgs,
            CurriedArgRttiInfo),
        ( ConsId = pred_const(ShroudedPredProcId, _) ->
            proc(PredId, _) = unshroud_pred_proc_id(ShroudedPredProcId),
            module_info_pred_info(ModuleInfo, PredId, PredInfo),
            pred_info_get_arg_types(PredInfo, CurriedCalleeArgTypes)
        ;
            CurriedCalleeArgTypes = CurriedArgTypes
        ),
        find_higher_order_args(ModuleInfo, CalleeStatus, CurriedArgs,
            CurriedCalleeArgTypes, VarTypes, RttiVarMaps,
            PredVars, 1, [], HOCurriedArgs0),
        list.reverse(HOCurriedArgs0, HOCurriedArgs),
        list.length(CurriedArgs, NumArgs),
        (
            NumArgs = list.length(HOCurriedArgs),
            \+ (
                list.member(HOCurriedArg, HOCurriedArgs),
                HOCurriedArg ^ hoa_is_constant = no
            )
        ->
            IsConst = yes
        ;
            IsConst = no
        ),
        HOArg = higher_order_arg(ConsId, ArgNo, NumArgs,
            CurriedArgs, CurriedArgTypes, CurriedArgRttiInfo,
            HOCurriedArgs, IsConst),
        list.cons(HOArg, !HOArgs)
    ;
        true
    ),
    find_higher_order_args(ModuleInfo, CalleeStatus, Args, CalleeArgTypes,
        VarTypes, RttiVarMaps, PredVars, NextArg, !HOArgs).

    % Succeeds if the type substitution for a call makes any of the
    % class constraints match an instance which was not matched before.
    %
:- pred type_subst_makes_instance_known(module_info::in,
    list(prog_constraint)::in, tvarset::in, list(tvar)::in, list(mer_type)::in,
    tvarset::in, existq_tvars::in, list(mer_type)::in) is semidet.

type_subst_makes_instance_known(ModuleInfo, CalleeUnivConstraints0, TVarSet0,
        CallerHeadTypeParams, ArgTypes, CalleeTVarSet,
        CalleeExistQVars, CalleeArgTypes0) :-
    CalleeUnivConstraints0 = [_ | _],
    tvarset_merge_renaming(TVarSet0, CalleeTVarSet, TVarSet, TypeRenaming),
    apply_variable_renaming_to_type_list(TypeRenaming, CalleeArgTypes0,
        CalleeArgTypes1),

    % Substitute the types in the callee's class constraints.
    inlining.get_type_substitution(CalleeArgTypes1, ArgTypes,
        CallerHeadTypeParams, CalleeExistQVars, TypeSubn),
    apply_variable_renaming_to_prog_constraint_list(TypeRenaming,
        CalleeUnivConstraints0, CalleeUnivConstraints1),
    apply_rec_subst_to_prog_constraint_list(TypeSubn,
        CalleeUnivConstraints1, CalleeUnivConstraints),
    assoc_list.from_corresponding_lists(CalleeUnivConstraints0,
        CalleeUnivConstraints, CalleeUnivConstraintAL),

    % Go through each constraint in turn, checking whether any instances
    % match which didn't before the substitution was applied.
    list.member(CalleeUnivConstraint0 - CalleeUnivConstraint,
        CalleeUnivConstraintAL),
    CalleeUnivConstraint0 = constraint(ClassName, ConstraintArgs0),
    list.length(ConstraintArgs0, ClassArity),
    CalleeUnivConstraint = constraint(_, ConstraintArgs),
    module_info_get_instance_table(ModuleInfo, InstanceTable),
    map.search(InstanceTable, class_id(ClassName, ClassArity), Instances),
    list.member(Instance, Instances),
    instance_matches(ConstraintArgs, Instance, _, _, TVarSet, _),
    \+ instance_matches(ConstraintArgs0, Instance, _, _, TVarSet, _).

:- type find_result
    --->    find_result_match(match)
    ;       find_result_request(request)
    ;       find_result_no_request.

:- type match
    --->    match(
                new_pred,

                maybe(int),
                            % Was the match partial, if so, how many
                            % higher_order arguments matched.

                list(prog_var),
                            % The arguments to the specialised call.
                list(mer_type)
                            % Type variables for which extra type-infos must be
                            % added to the start of the argument list.
            ).

    % WARNING - do not filter out higher-order arguments from the request
    % returned by find_matching_version, otherwise some type-infos that the
    % call specialization code is expecting to come from the curried arguments
    % of the higher-order arguments will not be present in the specialized
    % argument list.
    %
:- pred find_matching_version(higher_order_info::in,
    pred_id::in, proc_id::in, list(prog_var)::in, prog_context::in,
    list(higher_order_arg)::in, bool::in, find_result::out) is det.

find_matching_version(Info, CalledPred, CalledProc, Args0, Context,
        HigherOrderArgs, IsUserSpecProc, Result) :-
    % Args0 is the original list of arguments.
    % Args is the original list of arguments with the curried arguments
    % of known higher-order arguments added.

    ModuleInfo = Info ^ global_info ^ module_info,
    NewPreds = Info ^ global_info ^ new_preds,
    Caller = Info ^ pred_proc_id,
    PredInfo = Info ^ pred_info,
    ProcInfo = Info ^ proc_info,
    Params = Info ^ global_info ^ ho_params,

    % WARNING - do not filter out higher-order arguments after this step,
    % except when partially matching against a previously produced
    % specialization, otherwise some type-infos that the call
    % specialization code is expecting to come from the curried
    % arguments of the higher-order arguments will not be present in the
    % specialized argument list.

    get_extra_arguments(HigherOrderArgs, Args0, Args),
    compute_extra_typeinfos(Info, Args, ExtraTypeInfoTVars),

    proc_info_get_vartypes(ProcInfo, VarTypes),
    map.apply_to_list(Args0, VarTypes, CallArgTypes),
    pred_info_get_typevarset(PredInfo, TVarSet),

    Request = ho_request(Caller, proc(CalledPred, CalledProc), Args0,
        ExtraTypeInfoTVars, HigherOrderArgs, CallArgTypes,
        yes, TVarSet, IsUserSpecProc, Context),

    % Check to see if any of the specialized versions of the called pred
    % apply here.
    (
        map.search(NewPreds, proc(CalledPred, CalledProc), Versions0),
        set.to_sorted_list(Versions0, Versions),
        search_for_version(Info, Params, ModuleInfo, Request, Versions,
            no, Match)
    ->
        Result = find_result_match(Match)
    ;
        HigherOrder = Params ^ optimize_higher_order,
        TypeSpec = Params ^ type_spec,
        UserTypeSpec = Params ^ user_type_spec,
        (
            UserTypeSpec = yes,
            IsUserSpecProc = yes
        ;
            module_info_pred_info(ModuleInfo, CalledPred, CalledPredInfo),
            \+ pred_info_is_imported(CalledPredInfo),
            (
                % This handles the predicates introduced by check_typeclass.m
                % to call the class methods for a specific instance. Without
                % this, user-specified specialized versions of class methods
                % won't be called.
                UserTypeSpec = yes,
                pred_info_get_markers(CalledPredInfo, Markers),
                (
                    check_marker(Markers, marker_class_method)
                ;
                    check_marker(Markers, marker_class_instance_method)
                )
            ;
                HigherOrder = yes,
                list.member(HOArg, HigherOrderArgs),
                HOArg ^ hoa_cons_id = pred_const(_, _)
            ;
                TypeSpec = yes
            )
        )
    ->
        Result = find_result_request(Request)
    ;
        Result = find_result_no_request
    ).

    % Specializing type `T' to `list(U)' requires passing in the
    % type-info for `U'. This predicate works out which extra variables
    % to pass in given the argument list for the call.  This needs to be
    % done even if --typeinfo-liveness is not set because the type-infos
    % may be needed when specializing calls inside the specialized version.
    %
:- pred compute_extra_typeinfos(higher_order_info::in,
    list(prog_var)::in, list(tvar)::out) is det.

compute_extra_typeinfos(Info, Args, ExtraTypeInfoTVars) :-
    % Work out which type variables don't already have type-infos in the
    % list of argument types.  The list is in the order which the type
    % variables occur in the list of argument types so that the extra
    % type-info arguments for calls to imported user-guided type
    % specialization procedures can be matched against the specialized
    % version (`goal_util.extra_nonlocal_typeinfos' is not used here
    % because the type variables are returned sorted by variable number,
    % which will vary between calls).
    ProcInfo = Info ^ proc_info,
    proc_info_get_vartypes(ProcInfo, VarTypes),
    map.apply_to_list(Args, VarTypes, ArgTypes),
    type_vars_list(ArgTypes, AllTVars),
    (
        AllTVars = [],
        ExtraTypeInfoTVars = []
    ;
        AllTVars = [_ | _],
        proc_info_get_rtti_varmaps(Info ^ proc_info, RttiVarMaps),
        list.foldl(arg_contains_type_info_for_tvar(RttiVarMaps),
            Args, [], TypeInfoTVars),
        list.delete_elems(AllTVars, TypeInfoTVars, ExtraTypeInfoTVars0),
        list.remove_dups(ExtraTypeInfoTVars0, ExtraTypeInfoTVars)
    ).

:- pred arg_contains_type_info_for_tvar(rtti_varmaps::in, prog_var::in,
    list(tvar)::in, list(tvar)::out) is det.

arg_contains_type_info_for_tvar(RttiVarMaps, Var, !TVars) :-
    rtti_varmaps_var_info(RttiVarMaps, Var, VarInfo),
    (
        VarInfo = type_info_var(Type),
        ( Type = type_variable(TVar, _) ->
            !:TVars = [TVar | !.TVars]
        ;
            true
        )
    ;
        VarInfo = typeclass_info_var(Constraint),
        Constraint = constraint(_ClassName, ClassArgTypes),
        % Find out what tvars the typeclass-info contains the type-infos
        % for.
        list.filter_map(
            (pred(ClassArgType::in, ClassTVar::out) is semidet :-
                ClassArgType = type_variable(ClassTVar, _)
            ), ClassArgTypes, ClassTVars),
        list.append(ClassTVars, !TVars)
    ;
        VarInfo = non_rtti_var
    ).

:- pred construct_extra_type_infos(list(mer_type)::in,
    list(prog_var)::out, list(hlds_goal)::out,
    higher_order_info::in, higher_order_info::out) is det.

construct_extra_type_infos(Types, TypeInfoVars, TypeInfoGoals, !Info) :-
    create_poly_info(!.Info ^ global_info ^ module_info,
        !.Info ^ pred_info, !.Info ^ proc_info, PolyInfo0),
    term.context_init(Context),
    polymorphism_make_type_info_vars(Types, Context,
        TypeInfoVars, TypeInfoGoals, PolyInfo0, PolyInfo),
    poly_info_extract(PolyInfo, !.Info ^ pred_info, PredInfo,
        !.Info ^ proc_info, ProcInfo, ModuleInfo),
    !:Info = !.Info ^ pred_info := PredInfo,
    !:Info = !.Info ^ proc_info := ProcInfo,
    !:Info = !.Info ^ global_info ^ module_info := ModuleInfo.

:- pred search_for_version(higher_order_info::in, ho_params::in,
    module_info::in, request::in, list(new_pred)::in,
    maybe(match)::in, match::out) is semidet.

search_for_version(_, _, _, _, [], yes(Match), Match).
search_for_version(Info, Params, ModuleInfo, Request, [Version | Versions],
        MaybeMatch0, Match) :-
    ( version_matches(Params, ModuleInfo, Request, Version, Match1) ->
        (
            Match1 = match(_, MatchIsPartial, _, _),
            MatchIsPartial = no
        ->
            Match = Match1
        ;
            (
                MaybeMatch0 = no
            ->
                MaybeMatch2 = yes(Match1)
            ;
                % Pick the best match.
                MaybeMatch0 = yes(Match0),
                Match0 = match(_, yes(NumMatches0), _, _),
                Match1 = match(_, yes(NumMatches1), _, _)
            ->
                ( NumMatches0 > NumMatches1 ->
                    MaybeMatch2 = MaybeMatch0
                ;
                    MaybeMatch2 = yes(Match1)
                )
            ;
                unexpected(this_file, "search_for_version")
            ),
            search_for_version(Info, Params, ModuleInfo, Request,
                Versions, MaybeMatch2, Match)
        )
    ;
        search_for_version(Info, Params, ModuleInfo, Request,
            Versions, MaybeMatch0, Match)
    ).

    % Check whether the request has already been implemented by the new_pred,
    % maybe ordering the list of extra type_infos in the caller predicate
    % to match up with those in the caller.
    %
:- pred version_matches(ho_params::in, module_info::in, request::in,
    new_pred::in, match::out) is semidet.

version_matches(Params, ModuleInfo, Request, Version, Match) :-
    Match = match(Version, PartialMatch, Args, ExtraTypeInfoTypes),
    Request = ho_request(_, Callee, Args0, _, RequestHigherOrderArgs,
        CallArgTypes, _, RequestTVarSet, _, _),
    Callee = proc(CalleePredId, _),
    module_info_pred_info(ModuleInfo, CalleePredId, CalleePredInfo),
    Version = new_pred(_, _, _, _, VersionHigherOrderArgs, _,
        VersionExtraTypeInfoTVars, VersionArgTypes0, _,
        VersionTVarSet, _),
    higher_order_args_match(RequestHigherOrderArgs,
        VersionHigherOrderArgs, HigherOrderArgs, MatchIsPartial),
    (
        % Don't accept partial matches unless the predicate is imported
        % or we are only doing user-guided type specialization.
        MatchIsPartial = no,
        PartialMatch = no
    ;
        MatchIsPartial = yes,
        list.length(HigherOrderArgs, NumHOArgs),
        PartialMatch = yes(NumHOArgs),
        pred_info_get_markers(CalleePredInfo, Markers),

        % Always fully specialize calls to class methods.
        \+ check_marker(Markers, marker_class_method),
        \+ check_marker(Markers, marker_class_instance_method),
        (
            Params ^ type_spec = no
        ;
            pred_info_is_imported(CalleePredInfo)
        )
    ),

    % Rename apart type variables.
    tvarset_merge_renaming(RequestTVarSet, VersionTVarSet, _, TVarRenaming),
    apply_variable_renaming_to_type_list(TVarRenaming, VersionArgTypes0,
        VersionArgTypes),
    type_list_subsumes(VersionArgTypes, CallArgTypes, TypeSubn),

    % Work out the types of the extra type-info variables that
    % need to be passed to the specialized version.
    %
    % XXX kind inference:
    % we assume all tvars have kind `star'

    map.init(KindMap),
    apply_variable_renaming_to_tvar_kind_map(TVarRenaming, KindMap,
        RenamedKindMap),
    apply_variable_renaming_to_tvar_list(TVarRenaming,
        VersionExtraTypeInfoTVars, ExtraTypeInfoTVars0),
    apply_rec_subst_to_tvar_list(RenamedKindMap, TypeSubn, ExtraTypeInfoTVars0,
        ExtraTypeInfoTypes),
    get_extra_arguments(HigherOrderArgs, Args0, Args).

:- pred higher_order_args_match(list(higher_order_arg)::in,
    list(higher_order_arg)::in, list(higher_order_arg)::out, bool::out)
    is semidet.

higher_order_args_match([], [], [], no).
higher_order_args_match(RequestArgs, [], [], yes) :-
    RequestArgs = [_ | _],
    \+ (
        list.member(RequestArg, RequestArgs),
        RequestConsId = RequestArg ^ hoa_cons_id,
        RequestConsId = pred_const(_, _)
    ).
higher_order_args_match([RequestArg | Args1], [VersionArg | Args2],
        Args, PartialMatch) :-
    RequestArg = higher_order_arg(ConsId1, ArgNo1, _, _, _, _, _,
        RequestIsConst),
    VersionArg = higher_order_arg(ConsId2, ArgNo2, _, _, _, _, _,
        VersionIsConst),

    ( ArgNo1 = ArgNo2 ->
        ConsId1 = ConsId2,
        RequestArg = higher_order_arg(_, _, NumArgs, CurriedArgs,
            CurriedArgTypes, CurriedArgRttiInfo, HOCurriedArgs1, _),
        VersionArg = higher_order_arg(_, _, NumArgs,
            _, _, _, HOCurriedArgs2, _),
        higher_order_args_match(HOCurriedArgs1, HOCurriedArgs2,
            NewHOCurriedArgs, PartialMatch),
        higher_order_args_match(Args1, Args2, Args3, _),
        NewRequestArg = higher_order_arg(ConsId1, ArgNo1, NumArgs,
            CurriedArgs, CurriedArgTypes, CurriedArgRttiInfo,
            NewHOCurriedArgs, RequestIsConst `and` VersionIsConst),
        Args = [NewRequestArg | Args3]
    ;
        % Type-info arguments present in the request may be missing from the
        % version if we are doing user-guided type specialization. All of the
        % arguments in the version must be present in the request for a match.
        ArgNo1 < ArgNo2,

        % All the higher-order arguments must be present in the version
        % otherwise we should create a new one.
        ConsId1 \= pred_const(_, _),
        PartialMatch = yes,
        higher_order_args_match(Args1, [VersionArg | Args2], Args, _)
    ).

    % Add the curried arguments of the higher-order terms to the argument list.
    % The order here must match that generated by construct_higher_order_terms.
    %
:- pred get_extra_arguments(list(higher_order_arg)::in,
    list(prog_var)::in, list(prog_var)::out) is det.

get_extra_arguments(HOArgs, Args0, ExtraArgs ++ Args) :-
    get_extra_arguments_2(HOArgs, ExtraArgs),
    remove_const_higher_order_args(1, Args0, HOArgs, Args).

:- pred get_extra_arguments_2(list(higher_order_arg)::in, list(prog_var)::out)
    is det.

get_extra_arguments_2([], []).
get_extra_arguments_2([HOArg | HOArgs], Args) :-
    HOArg = higher_order_arg(_, _, _, CurriedArgs0, _, _, HOCurriedArgs,
        IsConst),
    (
        IsConst = yes,
        % If this argument is constant, all its sub-terms must be constant,
        % so there won't be anything more to add.
        get_extra_arguments_2(HOArgs, Args)
    ;
        IsConst = no,
        remove_const_higher_order_args(1, CurriedArgs0,
            HOCurriedArgs, CurriedArgs),
        get_extra_arguments_2(HOCurriedArgs, ExtraCurriedArgs),
        get_extra_arguments_2(HOArgs, Args1),
        list.condense([CurriedArgs, ExtraCurriedArgs, Args1], Args)
    ).

    % If the right argument of an assignment is a higher order term with a
    % known value, we need to add an entry for the left argument.
    %
:- pred maybe_add_alias(prog_var::in, prog_var::in,
    higher_order_info::in, higher_order_info::out) is det.

maybe_add_alias(LVar, RVar, !Info) :-
    ( map.search(!.Info ^ pred_vars, RVar, constant(A, B)) ->
        map.set(!.Info ^ pred_vars, LVar, constant(A, B), PredVars),
        !:Info = !.Info ^ pred_vars := PredVars
    ;
        true
    ).

:- pred update_changed_status(changed::in, changed::in, changed::out) is det.

update_changed_status(changed, _, changed).
update_changed_status(request, changed, changed).
update_changed_status(request, request, request).
update_changed_status(request, unchanged, request).
update_changed_status(unchanged, Changed, Changed).

%-----------------------------------------------------------------------------%

    % Interpret a call to `type_info_from_typeclass_info',
    % `superclass_from_typeclass_info' or
    % `instance_constraint_from_typeclass_info'.
    % This should be kept in sync with compiler/polymorphism.m,
    % library/private_builtin.m and runtime/mercury_type_info.h.
    %
:- pred interpret_typeclass_info_manipulator(typeclass_info_manipulator::in,
    list(prog_var)::in, hlds_goal_expr::in, hlds_goal_expr::out,
    higher_order_info::in, higher_order_info::out) is det.

interpret_typeclass_info_manipulator(Manipulator, Args, Goal0, Goal, !Info) :-
    ModuleInfo = !.Info ^ global_info ^ module_info,
    PredVars = !.Info ^ pred_vars,
    (
        Args = [TypeClassInfoVar, IndexVar, TypeInfoVar],
        map.search(PredVars, TypeClassInfoVar,
            constant(_TypeClassInfoConsId, TypeClassInfoArgs)),

        map.search(PredVars, IndexVar, constant(int_const(Index0), [])),

        % Extract the number of class constraints on the instance
        % from the base_typeclass_info.
        TypeClassInfoArgs = [BaseTypeClassInfoVar | OtherVars],

        map.search(PredVars, BaseTypeClassInfoVar,
            constant(base_typeclass_info_const(_, ClassId, InstanceNum, _), _))
    ->
        module_info_get_instance_table(ModuleInfo, Instances),
        map.lookup(Instances, ClassId, InstanceDefns),
        list.index1_det(InstanceDefns, InstanceNum, InstanceDefn),
        InstanceDefn = hlds_instance_defn(_,_,_,Constraints,_,_,_,_,_),
        (
            Manipulator = type_info_from_typeclass_info,
            list.length(Constraints, NumConstraints),
            Index = Index0 + NumConstraints
        ;
            Manipulator = superclass_from_typeclass_info,
            list.length(Constraints, NumConstraints),
            % Polymorphism.m adds the number of type_infos to the index.
            Index = Index0 + NumConstraints
        ;
            Manipulator = instance_constraint_from_typeclass_info,
            Index = Index0
        ),
        list.index1_det(OtherVars, Index, TypeInfoArg),
        maybe_add_alias(TypeInfoVar, TypeInfoArg, !Info),
        Uni = assign(TypeInfoVar, TypeInfoArg),
        Goal = unify(TypeInfoVar, rhs_var(TypeInfoArg), out_mode - in_mode,
            Uni, unify_context(umc_explicit, [])),
        !:Info = !.Info ^ changed := changed
    ;
        Goal = Goal0
    ).

%-----------------------------------------------------------------------------%

    % Succeed if the called pred is "unify" or "compare" and is specializable,
    % returning a specialized goal.
    %
:- pred specialize_special_pred(pred_id::in, proc_id::in, list(prog_var)::in,
    maybe(call_unify_context)::in, hlds_goal_info::in, bool::in,
    hlds_goal_expr::out, higher_order_info::in, higher_order_info::out)
    is semidet.

specialize_special_pred(CalledPred, CalledProc, Args, MaybeContext,
        OrigGoalInfo, HaveSpecialPreds, Goal, !Info) :-
    ModuleInfo = !.Info ^ global_info ^ module_info,
    ProcInfo0 = !.Info ^ proc_info,
    PredVars = !.Info ^ pred_vars,
    proc_info_get_vartypes(ProcInfo0, VarTypes),
    module_info_pred_info(ModuleInfo, CalledPred, CalledPredInfo),
    mercury_public_builtin_module = pred_info_module(CalledPredInfo),
    pred_info_module(CalledPredInfo) = mercury_public_builtin_module,
    PredName = pred_info_name(CalledPredInfo),
    PredArity = pred_info_orig_arity(CalledPredInfo),
    special_pred_name_arity(SpecialId, PredName, _, PredArity),
    special_pred_get_type(SpecialId, Args, Var),
    map.lookup(VarTypes, Var, SpecialPredType),
    SpecialPredType \= type_variable(_, _),

    % Don't specialize tuple types -- the code to unify them only exists
    % in the generic unification routine in the runtime.
    % `private_builtin.builtin_unify_tuple/2' and
    % `private_builtin.builtin_compare_tuple/3' always abort. It might be
    % worth inlining complicated unifications of small tuples (or any
    % other small type).
    SpecialPredType \= tuple_type(_, _),

    Args = [TypeInfoVar | SpecialPredArgs],
    map.search(PredVars, TypeInfoVar,
        constant(_TypeInfoConsId, TypeInfoVarArgs)),
    type_to_ctor_and_args(SpecialPredType, type_ctor(_, TypeArity), _),
    ( TypeArity = 0 ->
        TypeInfoArgs = []
    ;
        TypeInfoVarArgs = [_TypeCtorInfo | TypeInfoArgs]
    ),
    (
        \+ type_has_user_defined_equality_pred(ModuleInfo, SpecialPredType, _),
        proc_id_to_int(CalledProc, CalledProcInt),
        CalledProcInt = 0,
        (
            SpecialId = spec_pred_unify,
            SpecialPredArgs = [Arg1, Arg2],
            MaybeResult = no
        ;
            SpecialId = spec_pred_compare,
            SpecialPredArgs = [Result, Arg1, Arg2],
            MaybeResult = yes(Result)
        )
    ->
        (
            is_dummy_argument_type(ModuleInfo, SpecialPredType)
        ->
            specialize_unify_or_compare_pred_for_dummy(MaybeResult, Goal,
                !Info)
        ;
            % Look for unification or comparison applied directly to a
            % builtin or atomic type. This needs to be done separately from
            % the case for user-defined types, for two reasons.
            %
            % First, because we want to specialize such calls even if we are
            % not generating any special preds.
            %
            % Second, because the specialized code is different in the two
            % cases: here it is a call to a builtin predicate, perhaps preceded
            % by casts; there it is a call to a compiler-generated predicate.

            type_is_atomic(ModuleInfo, SpecialPredType)
        ->
            specialize_unify_or_compare_pred_for_atomic(SpecialPredType,
                MaybeResult, Arg1, Arg2, MaybeContext, OrigGoalInfo, Goal,
                !Info)
        ;
            % Look for unification or comparison applied to a no-tag type
            % wrapping a builtin or atomic type. This needs to be done to
            % optimize all the map_lookups with keys of type `term.var/1'
            % in the compiler. (:- type var(T) ---> var(int).)
            %
            % This could possibly be better handled by just inlining the
            % unification code, but the compiler doesn't have the code for
            % the comparison or in-in unification procedures for imported
            % types, and unification and comparison may be implemented in
            % C code in the runtime system.

            type_is_no_tag_type(ModuleInfo, SpecialPredType, Constructor,
                WrappedType),
            \+ type_has_user_defined_equality_pred(ModuleInfo, WrappedType, _),

            % This could be done for non-atomic types, but it would be a bit
            % more complicated because the type-info for the wrapped type
            % would need to be extracted first.
            type_is_atomic(ModuleInfo, WrappedType)
        ->
            specialize_unify_or_compare_pred_for_no_tag(WrappedType,
                Constructor, MaybeResult, Arg1, Arg2, MaybeContext,
                OrigGoalInfo, Goal, !Info)
        ;
            call_type_specific_unify_or_compare(SpecialPredType, SpecialId,
                TypeInfoArgs, SpecialPredArgs, MaybeContext, HaveSpecialPreds,
                Goal, !Info)
        )
    ;
        call_type_specific_unify_or_compare(SpecialPredType, SpecialId,
            TypeInfoArgs, SpecialPredArgs, MaybeContext, HaveSpecialPreds,
            Goal, !Info)
    ).

:- pred call_type_specific_unify_or_compare(mer_type::in, special_pred_id::in,
    list(prog_var)::in, list(prog_var)::in,
    maybe(call_unify_context)::in, bool::in, hlds_goal_expr::out,
    higher_order_info::in, higher_order_info::out) is semidet.

call_type_specific_unify_or_compare(SpecialPredType, SpecialId,
        TypeInfoArgs, SpecialPredArgs, MaybeContext, HaveSpecialPreds, Goal,
        !Info) :-
    % We can only specialize unifications and comparisons to call the
    % type-specific unify or compare predicate if we are generating
    % such predicates.
    HaveSpecialPreds = yes,
    find_special_proc(SpecialPredType, SpecialId, SymName, SpecialPredId,
        SpecialProcId, !Info),
    ( type_is_higher_order(SpecialPredType) ->
        % Builtin_*_pred are special cases which don't need the type-info
        % arguments.
        CallArgs = SpecialPredArgs
    ;
        list.append(TypeInfoArgs, SpecialPredArgs, CallArgs)
    ),
    Goal = plain_call(SpecialPredId, SpecialProcId, CallArgs, not_builtin,
        MaybeContext, SymName).

:- pred specialize_unify_or_compare_pred_for_dummy(maybe(prog_var)::in,
    hlds_goal_expr::out, higher_order_info::in, higher_order_info::out) is det.

specialize_unify_or_compare_pred_for_dummy(MaybeResult, GoalExpr, !Info) :-
    (
        MaybeResult = no,
        GoalExpr = conj(plain_conj, [])     % true
    ;
        MaybeResult = yes(ComparisonResult),
        Eq = cons(qualified(mercury_public_builtin_module, "="), 0),
        make_const_construction(ComparisonResult, Eq, Goal),
        Goal = GoalExpr - _
    ).

:- pred specialize_unify_or_compare_pred_for_atomic(mer_type::in,
    maybe(prog_var)::in, prog_var::in, prog_var::in,
    maybe(call_unify_context)::in, hlds_goal_info::in, hlds_goal_expr::out,
    higher_order_info::in, higher_order_info::out) is det.

specialize_unify_or_compare_pred_for_atomic(SpecialPredType, MaybeResult,
        Arg1, Arg2, MaybeContext, OrigGoalInfo, GoalExpr, !Info) :-
    ModuleInfo = !.Info ^ global_info ^ module_info,
    ProcInfo0 = !.Info ^ proc_info,
    (
        MaybeResult = no,
        in_mode(In),
        GoalExpr = unify(Arg1, rhs_var(Arg2), (In - In),
            simple_test(Arg1, Arg2), unify_context(umc_explicit, []))
    ;
        MaybeResult = yes(ComparisonResult),
        find_builtin_type_with_equivalent_compare(ModuleInfo,
            SpecialPredType, CompareType, NeedIntCast),
        polymorphism.get_special_proc_det(CompareType, spec_pred_compare,
            ModuleInfo, SymName, SpecialPredId, SpecialProcId),
        (
            NeedIntCast = no,
            NewCallArgs = [ComparisonResult, Arg1, Arg2],
            GoalExpr = plain_call(SpecialPredId, SpecialProcId, NewCallArgs,
                not_builtin, MaybeContext, SymName)
        ;
            NeedIntCast = yes,
            goal_info_get_context(OrigGoalInfo, Context),
            generate_unsafe_type_cast(Context, CompareType, Arg1, CastArg1,
                CastGoal1, ProcInfo0, ProcInfo1),
            generate_unsafe_type_cast(Context, CompareType, Arg2, CastArg2,
                CastGoal2, ProcInfo1, ProcInfo),
            NewCallArgs = [ComparisonResult, CastArg1, CastArg2],
            Call = plain_call(SpecialPredId, SpecialProcId, NewCallArgs,
                not_builtin, MaybeContext, SymName),
            set.list_to_set([ComparisonResult, Arg1, Arg2], NonLocals),
            instmap_delta_from_assoc_list(
                [ComparisonResult - ground(shared,none)], InstMapDelta),
            Detism = detism_det,
            goal_info_init(NonLocals, InstMapDelta, Detism, purity_pure,
                Context, GoalInfo),
            GoalExpr = conj(plain_conj,
                [CastGoal1, CastGoal2, Call - GoalInfo]),
            !:Info = !.Info ^ proc_info := ProcInfo
        )
    ).

:- pred specialize_unify_or_compare_pred_for_no_tag(mer_type::in, sym_name::in,
    maybe(prog_var)::in, prog_var::in, prog_var::in,
    maybe(call_unify_context)::in, hlds_goal_info::in, hlds_goal_expr::out,
    higher_order_info::in, higher_order_info::out) is det.

specialize_unify_or_compare_pred_for_no_tag(WrappedType, Constructor,
        MaybeResult, Arg1, Arg2, MaybeContext, OrigGoalInfo, GoalExpr,
        !Info) :-
    ModuleInfo = !.Info ^ global_info ^ module_info,
    ProcInfo0 = !.Info ^ proc_info,
    goal_info_get_context(OrigGoalInfo, Context),
    unwrap_no_tag_arg(WrappedType, Context, Constructor, Arg1,
        UnwrappedArg1, ExtractGoal1, ProcInfo0, ProcInfo1),
    unwrap_no_tag_arg(WrappedType, Context, Constructor, Arg2,
        UnwrappedArg2, ExtractGoal2, ProcInfo1, ProcInfo2),
    set.list_to_set([UnwrappedArg1, UnwrappedArg2], NonLocals0),
    (
        MaybeResult = no,
        in_mode(In),
        NonLocals = NonLocals0,
        instmap_delta_init_reachable(InstMapDelta),
        Detism = detism_semi,
        SpecialGoal = unify(UnwrappedArg1, rhs_var(UnwrappedArg2), (In - In),
            simple_test(UnwrappedArg1, UnwrappedArg2),
            unify_context(umc_explicit, [])),
        goal_info_init(NonLocals, InstMapDelta, Detism, purity_pure,
            Context, GoalInfo),
        GoalExpr = conj(plain_conj,
            [ExtractGoal1, ExtractGoal2, SpecialGoal - GoalInfo]),
        !:Info = !.Info ^ proc_info := ProcInfo2
    ;
        MaybeResult = yes(ComparisonResult),
        set.insert(NonLocals0, ComparisonResult, NonLocals),
        instmap_delta_from_assoc_list(
            [ComparisonResult - ground(shared, none)], InstMapDelta),
        Detism = detism_det,
        % Build a new call with the unwrapped arguments.
        find_builtin_type_with_equivalent_compare(ModuleInfo, WrappedType,
            CompareType, NeedIntCast),
        polymorphism.get_special_proc_det(CompareType, spec_pred_compare,
            ModuleInfo, SymName, SpecialPredId, SpecialProcId),
        (
            NeedIntCast = no,
            NewCallArgs = [ComparisonResult, UnwrappedArg1, UnwrappedArg2],
            SpecialGoal = plain_call(SpecialPredId, SpecialProcId, NewCallArgs,
                not_builtin, MaybeContext, SymName),
            goal_info_init(NonLocals, InstMapDelta, Detism, purity_pure,
                Context, GoalInfo),
            GoalExpr = conj(plain_conj, [ExtractGoal1, ExtractGoal2,
                SpecialGoal - GoalInfo]),
            !:Info = !.Info ^ proc_info := ProcInfo2
        ;
            NeedIntCast = yes,
            generate_unsafe_type_cast(Context, CompareType,
                UnwrappedArg1, CastArg1, CastGoal1, ProcInfo2, ProcInfo3),
            generate_unsafe_type_cast(Context, CompareType,
                UnwrappedArg2, CastArg2, CastGoal2, ProcInfo3, ProcInfo4),
            NewCallArgs = [ComparisonResult, CastArg1, CastArg2],
            SpecialGoal = plain_call(SpecialPredId, SpecialProcId, NewCallArgs,
                not_builtin, MaybeContext, SymName),
            goal_info_init(NonLocals, InstMapDelta, Detism, purity_pure,
                Context, GoalInfo),
            GoalExpr = conj(plain_conj,
                [ExtractGoal1, CastGoal1, ExtractGoal2, CastGoal2,
                SpecialGoal - GoalInfo]),
            !:Info = !.Info ^ proc_info := ProcInfo4
        )
    ).

:- pred find_special_proc(mer_type::in, special_pred_id::in, sym_name::out,
    pred_id::out, proc_id::out,
    higher_order_info::in, higher_order_info::out) is semidet.

find_special_proc(Type, SpecialId, SymName, PredId, ProcId, !Info) :-
    ModuleInfo0 = !.Info ^ global_info ^ module_info,
    (
        polymorphism.get_special_proc(Type, SpecialId, ModuleInfo0, SymName0,
            PredId0, ProcId0)
    ->
        SymName = SymName0,
        PredId = PredId0,
        ProcId = ProcId0
    ;
        type_to_ctor_and_args(Type, TypeCtor, _),
        special_pred_is_generated_lazily(ModuleInfo, TypeCtor),
        (
            SpecialId = spec_pred_compare,
            unify_proc.add_lazily_generated_compare_pred_decl(TypeCtor,
                PredId, ModuleInfo0, ModuleInfo),
            ProcId = hlds_pred.initial_proc_id
        ;
            SpecialId = spec_pred_index,
            % This shouldn't happen. The index predicate should only be called
            % from the compare predicate. If it is called, it shouldn't be
            % generated lazily.
            fail
        ;
            SpecialId = spec_pred_unify,

            % XXX We should only add the declaration, not the body, for the
            % unify pred, but that complicates things if mode analysis is rerun
            % after higher_order.m and requests more unification procedures.
            % In particular, it's difficult to run polymorphism on the new
            % clauses if the predicate's arguments have already had type-infos
            % added. This case shouldn't come up unless an optimization does
            % reordering which requires rescheduling a conjunction.

            unify_proc.add_lazily_generated_unify_pred(TypeCtor, PredId,
                ModuleInfo0, ModuleInfo),
            hlds_pred.in_in_unification_proc_id(ProcId)
        ),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        ModuleName = pred_info_module(PredInfo),
        Name = pred_info_name(PredInfo),
        SymName = qualified(ModuleName, Name),
        !:Info = !.Info ^ global_info ^ module_info := ModuleInfo
    ).

:- pred find_builtin_type_with_equivalent_compare(module_info::in,
    mer_type::in, mer_type::out, bool::out) is det.

find_builtin_type_with_equivalent_compare(ModuleInfo, Type, EqvType,
        NeedIntCast) :-
    TypeCategory = classify_type(ModuleInfo, Type),
    (
        TypeCategory = type_cat_int,
        EqvType = Type,
        NeedIntCast = no
    ;
        TypeCategory = type_cat_char,
        EqvType = Type,
        NeedIntCast = no
    ;
        TypeCategory = type_cat_string,
        EqvType = Type,
        NeedIntCast = no
    ;
        TypeCategory = type_cat_float,
        EqvType = Type,
        NeedIntCast = no
    ;
        TypeCategory = type_cat_dummy,
        unexpected(this_file,
            "dummy type in find_builtin_type_with_equivalent_compare")
    ;
        TypeCategory = type_cat_void,
        unexpected(this_file,
            "void type in find_builtin_type_with_equivalent_compare")
    ;
        TypeCategory = type_cat_higher_order,
        unexpected(this_file, "higher_order type in " ++
            "find_builtin_type_with_equivalent_compare")
    ;
        TypeCategory = type_cat_tuple,
        unexpected(this_file,
            "tuple type in find_builtin_type_with_equivalent_compare")
    ;
        TypeCategory = type_cat_enum,
        construct_type(type_ctor(unqualified("int"), 0), [], EqvType),
        NeedIntCast = yes
    ;
        TypeCategory = type_cat_variable,
        unexpected(this_file,
            "var type in find_builtin_type_with_equivalent_compare")
    ;
        TypeCategory = type_cat_user_ctor,
        unexpected(this_file,
            "user type in find_builtin_type_with_equivalent_compare")
    ;
        TypeCategory = type_cat_type_info,
        unexpected(this_file, "type_info type in " ++
            "find_builtin_type_with_equivalent_compare")
    ;
        TypeCategory = type_cat_type_ctor_info,
        unexpected(this_file, "type_ctor_info type in " ++
            "find_builtin_type_with_equivalent_compare")
    ;
        TypeCategory = type_cat_typeclass_info,
        unexpected(this_file, "typeclass_info type in " ++
            "find_builtin_type_with_equivalent_compare")
    ;
        TypeCategory = type_cat_base_typeclass_info,
        unexpected(this_file, "base_typeclass_info type in " ++
            "find_builtin_type_with_equivalent_compare")
    ).

:- pred generate_unsafe_type_cast(prog_context::in,
    mer_type::in, prog_var::in, prog_var::out, hlds_goal::out,
    proc_info::in, proc_info::out) is det.

generate_unsafe_type_cast(Context, ToType, Arg, CastArg, Goal, !ProcInfo) :-
    proc_info_create_var_from_type(ToType, no, CastArg, !ProcInfo),
    generate_cast(unsafe_type_cast, Arg, CastArg, Context, Goal).

:- pred unwrap_no_tag_arg(mer_type::in, prog_context::in, sym_name::in,
    prog_var::in, prog_var::out, hlds_goal::out,
    proc_info::in, proc_info::out) is det.

unwrap_no_tag_arg(WrappedType, Context, Constructor, Arg, UnwrappedArg, Goal,
        !ProcInfo) :-
    proc_info_create_var_from_type(WrappedType, no, UnwrappedArg, !ProcInfo),
    ConsId = cons(Constructor, 1),
    UniModes = [(ground(shared, none) - free) ->
        (ground(shared, none) - ground(shared, none))],
    set.list_to_set([Arg, UnwrappedArg], NonLocals),
    % This will be recomputed later.
    instmap_delta_from_assoc_list([UnwrappedArg - ground(shared, none)],
        InstMapDelta),
    goal_info_init(NonLocals, InstMapDelta, detism_det, purity_pure, Context,
        GoalInfo),
    Goal = unify(Arg, rhs_functor(ConsId, no, [UnwrappedArg]),
        in_mode - out_mode,
        deconstruct(Arg, ConsId, [UnwrappedArg], UniModes,
            cannot_fail, cannot_cgc),
        unify_context(umc_explicit, [])) - GoalInfo.

%-----------------------------------------------------------------------------%
%
% Predicates to process requests for specialization, and create any
% new predicates that are required.
%

    % Filter out requests for higher-order specialization for preds which are
    % too large. Maybe we could allow programmers to declare which predicates
    % they want specialized, as with inlining? Don't create specialized
    % versions of specialized versions, since for some fairly contrived
    % examples involving recursively building up lambda expressions
    % this can create ridiculous numbers of versions.
    %
:- pred filter_requests(list(request)::out, list(request)::out,
    higher_order_global_info::in, higher_order_global_info::out,
    io::di, io::uo) is det.

filter_requests(FilteredRequests, LoopRequests, !Info, !IO) :-
    Requests0 = set.to_sorted_list(!.Info ^ requests),
    !:Info = !.Info ^ requests := set.init,
    list.foldl3(filter_requests_2(!.Info), Requests0,
        [], FilteredRequests, [], LoopRequests, !IO).

:- pred filter_requests_2(higher_order_global_info::in, request::in,
    list(request)::in, list(request)::out,
    list(request)::in, list(request)::out, io::di, io::uo) is det.

filter_requests_2(Info, Request, !AcceptedRequests, !LoopRequests, !IO) :-
    ModuleInfo = Info ^ module_info,
    Request = ho_request(CallingPredProcId, CalledPredProcId, _, _, HOArgs,
        _, _, _, IsUserTypeSpec, Context),
    CalledPredProcId = proc(CalledPredId, _),
    module_info_pred_info(ModuleInfo, CalledPredId, PredInfo),
    globals.io_lookup_bool_option(very_verbose, VeryVerbose, !IO),
    PredModule = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    Arity = pred_info_orig_arity(PredInfo),
    pred_info_get_arg_types(PredInfo, Types),
    list.length(Types, ActualArity),
    maybe_write_request(VeryVerbose, ModuleInfo, "Request for",
        qualified(PredModule, PredName), Arity, ActualArity,
        no, HOArgs, Context, !IO),
    (
        IsUserTypeSpec = yes,
        % Ignore the size limit for user specified specializations.
        maybe_write_string(VeryVerbose,
            "%    request specialized (user-requested specialization)\n", !IO),
        list.cons(Request, !AcceptedRequests)
    ;
        IsUserTypeSpec = no,
        ( map.search(Info ^ goal_sizes, CalledPredId, GoalSize0) ->
            GoalSize = GoalSize0
        ;
            % This can happen for a specialized version.
            GoalSize = 0
        ),
        (
            GoalSize > Info ^ ho_params ^ size_limit
        ->
            maybe_write_string(VeryVerbose,
                "%    not specializing (goal too large).\n", !IO)
        ;
            higher_order_args_size(HOArgs) > Info ^ ho_params ^ arg_limit
        ->
            % If the arguments are too large, we can end up producing a
            % specialized version with massive numbers of arguments, because
            % all of the curried arguments are passed as separate arguments.
            % Without this extras/xml/xml.parse.chars.m takes forever to
            % compile.
            maybe_write_string(VeryVerbose,
                "%    not specializing (args too large).\n", !IO)
        ;
            % To ensure termination of the specialization process, the depth
            % of the higher-order arguments must strictly decrease compared
            % to parents with the same original pred_proc_id.
            VersionInfoMap = Info ^ version_info,
            (
                map.search(VersionInfoMap, CalledPredProcId, CalledVersionInfo)
            ->
                CalledVersionInfo = version_info(OrigPredProcId, _, _, _)
            ;
                OrigPredProcId = CalledPredProcId
            ),
            map.search(VersionInfoMap, CallingPredProcId, CallingVersionInfo),
            CallingVersionInfo = version_info(_, _, _, ParentVersions),
            ArgDepth = higher_order_args_depth(HOArgs),
            some [ParentVersion] (
                list.member(ParentVersion, ParentVersions),
                ParentVersion = parent_version_info(OrigPredProcId,
                    OldArgDepth),
                ArgDepth >= OldArgDepth
            )
        ->
            !:LoopRequests = [Request | !.LoopRequests],
            maybe_write_string(VeryVerbose,
                "%    not specializing (recursive specialization).\n", !IO)
        ;
            maybe_write_string(VeryVerbose,
                "%    request specialized.\n", !IO),
            list.cons(Request, !AcceptedRequests)
        )
    ).

:- pred create_new_preds(list(request)::in, list(new_pred)::in,
    list(new_pred)::out, set(pred_proc_id)::in, set(pred_proc_id)::out,
    higher_order_global_info::in, higher_order_global_info::out,
    io::di, io::uo) is det.

create_new_preds([], !NewPredList, !PredsToFix, !Info, !IO).
create_new_preds([Request | Requests], !NewPredList, !PredsToFix, !Info,
        !IO) :-
    Request = ho_request(CallingPredProcId, CalledPredProcId, _HOArgs,
        _CallArgs, _, _CallerArgTypes, _, _, _, _),
    set.insert(!.PredsToFix, CallingPredProcId, !:PredsToFix),
    ( map.search(!.Info ^ new_preds, CalledPredProcId, SpecVersions0) ->
        (
            % Check that we aren't redoing the same pred.
            % SpecVersions0 are pred_proc_ids of the specialized versions
            % of the current pred.
            set.member(Version, SpecVersions0),
            version_matches(!.Info ^ ho_params, !.Info ^ module_info,
                Request, Version, _)
        ->
            true
        ;
            create_new_pred(Request, NewPred, !Info, !IO),
            list.cons(NewPred, !NewPredList)
        )
    ;
        create_new_pred(Request, NewPred, !Info, !IO),
        list.cons(NewPred, !NewPredList)
    ),
    create_new_preds(Requests, !NewPredList, !PredsToFix, !Info, !IO).

    % If we weren't allowed to create a specialized version because the
    % loop check failed, check whether the version was created for another
    % request for which the loop check succeeded.
    %
:- pred check_loop_request(higher_order_global_info::in, request::in,
    set(pred_proc_id)::in, set(pred_proc_id)::out) is det.

check_loop_request(Info, Request, !PredsToFix) :-
    CallingPredProcId = Request ^ rq_caller,
    CalledPredProcId = Request ^ rq_callee,
    (
        map.search(Info ^ new_preds, CalledPredProcId, SpecVersions0),
        some [Version] (
            set.member(Version, SpecVersions0),
            version_matches(Info ^ ho_params, Info ^ module_info,
                Request, Version, _)
        )
    ->
        svset.insert(CallingPredProcId, !PredsToFix)
    ;
        true
    ).

    % Here we create the pred_info for the new predicate.
    %
:- pred create_new_pred(request::in, new_pred::out,
    higher_order_global_info::in, higher_order_global_info::out,
    io::di, io::uo) is det.

create_new_pred(Request, NewPred, !Info, !IO) :-
    Request = ho_request(Caller, CalledPredProc, CallArgs, ExtraTypeInfoTVars,
        HOArgs, ArgTypes, TypeInfoLiveness, CallerTVarSet,
        IsUserTypeSpec, Context),
    Caller = proc(CallerPredId, CallerProcId),
    ModuleInfo0 = !.Info ^ module_info,
    module_info_pred_proc_info(ModuleInfo0, CalledPredProc,
        PredInfo0, ProcInfo0),

    Name0 = pred_info_name(PredInfo0),
    Arity = pred_info_orig_arity(PredInfo0),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo0),
    PredModule = pred_info_module(PredInfo0),
    globals.io_lookup_bool_option(very_verbose, VeryVerbose, !IO),
    pred_info_get_arg_types(PredInfo0, ArgTVarSet, ExistQVars, Types),

    (
        IsUserTypeSpec = yes,
        % If this is a user-guided type specialisation, the new name comes from
        % the name and mode number of the requesting predicate. The mode number
        % is included because we want to avoid the creation of more than one
        % predicate with the same name if more than one mode of a predicate
        % is specialized. Since the names of e.g. deep profiling proc_static
        % structures are derived from the names of predicates, duplicate
        % predicate names lead to duplicate global variable names and hence to
        % link errors.
        PredName0 = predicate_name(ModuleInfo0, CallerPredId),
        proc_id_to_int(CallerProcId, CallerProcInt),

        % The higher_order_arg_order_version part is to avoid segmentation
        % faults or other errors when the order or number of extra arguments
        % changes. If the user does not recompile all affected code, the
        % program will not link.
        PredName = string.append_list(
            [PredName0, "_", int_to_string(CallerProcInt), "_",
            int_to_string(higher_order_arg_order_version)]),
        SymName = qualified(PredModule, PredName),
        Transform = transform_higher_order_type_specialization(CallerProcInt),
        NewProcId = CallerProcId,
        % For exported predicates the type specialization must
        % be exported.
        % For opt_imported predicates we only want to keep this
        % version if we do some other useful specialization on it.
        pred_info_get_import_status(PredInfo0, Status)
    ;
        IsUserTypeSpec = no,
        NewProcId = hlds_pred.initial_proc_id,
        IdCounter0 = !.Info ^ next_ho_id,
        counter.allocate(Id, IdCounter0, IdCounter),
        !:Info = !.Info ^ next_ho_id := IdCounter,
        string.int_to_string(Id, IdStr),
        string.append_list([Name0, "__ho", IdStr], PredName),
        SymName = qualified(PredModule, PredName),
        Transform = transform_higher_order_specialization(Id),
        Status = status_local
    ),

    list.length(Types, ActualArity),
    maybe_write_request(VeryVerbose, ModuleInfo0, "Specializing",
        qualified(PredModule, Name0), Arity, ActualArity,
        yes(PredName), HOArgs, Context, !IO),

    pred_info_get_origin(PredInfo0, OrigOrigin),
    pred_info_get_typevarset(PredInfo0, TypeVarSet),
    pred_info_get_markers(PredInfo0, MarkerList),
    pred_info_get_goal_type(PredInfo0, GoalType),
    pred_info_get_class_context(PredInfo0, ClassContext),
    varset.init(EmptyVarSet),
    map.init(EmptyVarTypes),
    map.init(EmptyTVarNameMap),
    map.init(EmptyProofs),
    map.init(EmptyConstraintMap),
    rtti_varmaps_init(EmptyRttiVarMaps),

    % This isn't looked at after here, and just clutters up HLDS dumps
    % if it's filled in.
    set_clause_list([], ClausesRep),
    ClausesInfo = clauses_info(EmptyVarSet, EmptyVarTypes,
        EmptyTVarNameMap, EmptyVarTypes, [], ClausesRep,
        EmptyRttiVarMaps, no),
    Origin = origin_transformed(Transform, OrigOrigin, CallerPredId),
    pred_info_init(PredModule, SymName, Arity, PredOrFunc, Context, Origin,
        Status, GoalType, MarkerList, Types, ArgTVarSet, ExistQVars,
        ClassContext, EmptyProofs, EmptyConstraintMap, ClausesInfo,
        NewPredInfo0),
    pred_info_set_typevarset(TypeVarSet, NewPredInfo0, NewPredInfo1),

    module_info_get_predicate_table(ModuleInfo0, PredTable0),
    predicate_table_insert(NewPredInfo1, NewPredId, PredTable0, PredTable),
    module_info_set_predicate_table(PredTable, ModuleInfo0, ModuleInfo1),

    !:Info = !.Info ^ module_info := ModuleInfo1,

    NewPred = new_pred(proc(NewPredId, NewProcId), CalledPredProc, Caller,
        SymName, HOArgs, CallArgs, ExtraTypeInfoTVars, ArgTypes,
        TypeInfoLiveness, CallerTVarSet, IsUserTypeSpec),

    add_new_pred(CalledPredProc, NewPred, !Info),

    create_new_proc(NewPred, ProcInfo0, NewPredInfo1, NewPredInfo, !Info),
    module_info_set_pred_info(NewPredId, NewPredInfo,
        !.Info ^ module_info, ModuleInfo),
    !:Info = !.Info ^ module_info := ModuleInfo.

:- pred add_new_pred(pred_proc_id::in, new_pred::in,
    higher_order_global_info::in, higher_order_global_info::out) is det.

add_new_pred(CalledPredProcId, NewPred, !Info) :-
    ( map.search(!.Info ^ new_preds, CalledPredProcId, SpecVersions0) ->
        set.insert(SpecVersions0, NewPred, SpecVersions)
    ;
        set.singleton_set(SpecVersions, NewPred)
    ),
    map.set(!.Info ^ new_preds, CalledPredProcId, SpecVersions, NewPreds),
    !:Info = !.Info ^ new_preds := NewPreds.

:- pred maybe_write_request(bool::in, module_info::in, string::in,
    sym_name::in, arity::in, arity::in, maybe(string)::in,
    list(higher_order_arg)::in, prog_context::in, io::di, io::uo) is det.

maybe_write_request(no, _, _, _, _, _, _, _, _, !IO).
maybe_write_request(yes, ModuleInfo, Msg, SymName, Arity, ActualArity,
        MaybeNewName, HOArgs, Context, !IO) :-
    OldName = sym_name_to_string(SymName),
    string.int_to_string(Arity, ArStr),
    io.write_string("% ", !IO),
    prog_out.write_context(Context, !IO),
    io.write_strings([Msg, " `", OldName, "'/", ArStr], !IO),
    (
        MaybeNewName = yes(NewName),
        io.write_string(" into ", !IO),
        io.write_string(NewName, !IO)
    ;
        MaybeNewName = no
    ),
    io.write_string(" with higher-order arguments:\n", !IO),
    NumToDrop = ActualArity - Arity,
    output_higher_order_args(ModuleInfo, NumToDrop, 0, HOArgs, !IO).

:- pred output_higher_order_args(module_info::in, int::in, int::in,
    list(higher_order_arg)::in, io::di, io::uo) is det.

output_higher_order_args(_, _, _, [], !IO).
output_higher_order_args(ModuleInfo, NumToDrop, Indent, [HOArg | HOArgs],
        !IO) :-
    HOArg = higher_order_arg(ConsId, ArgNo, NumArgs, _, _, _,
        CurriedHOArgs, IsConst),
    io.write_string("% ", !IO),
    list.duplicate(Indent + 1, "  ", Spaces),
    list.foldl(io.write_string, Spaces, !IO),
    (
        IsConst = yes,
        io.write_string("const ", !IO)
    ;
        IsConst = no
    ),
    ( ConsId = pred_const(ShroudedPredProcId, _) ->
        proc(PredId, _) = unshroud_pred_proc_id(ShroudedPredProcId),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        Name = pred_info_name(PredInfo),
        Arity = pred_info_orig_arity(PredInfo),
        % Adjust message for type_infos.
        DeclaredArgNo = ArgNo - NumToDrop,
        io.write_string("HeadVar__", !IO),
        io.write_int(DeclaredArgNo, !IO),
        io.write_string(" = `", !IO),
        io.write_string(Name, !IO),
        io.write_string("'/", !IO),
        io.write_int(Arity, !IO)
    ; ConsId = type_ctor_info_const(TypeModule, TypeName, TypeArity) ->
        io.write_string("type_ctor_info for `", !IO),
        prog_out.write_sym_name(qualified(TypeModule, TypeName), !IO),
        io.write_string("'/", !IO),
        io.write_int(TypeArity, !IO)
    ; ConsId = base_typeclass_info_const(_, ClassId, _, _) ->
        io.write_string("base_typeclass_info for `", !IO),
        ClassId = class_id(ClassName, ClassArity),
        prog_out.write_sym_name(ClassName, !IO),
        io.write_string("'/", !IO),
        io.write_int(ClassArity, !IO)
    ;
        % XXX output the type.
        io.write_string("type_info/typeclass_info ", !IO)
    ),
    io.write_string(" with ", !IO),
    io.write_int(NumArgs, !IO),
    io.write_string(" curried arguments", !IO),
    (
        CurriedHOArgs = [],
        io.nl(!IO)
    ;
        CurriedHOArgs = [_ | _],
        io.write_string(":\n", !IO),
        output_higher_order_args(ModuleInfo, 0, Indent + 1, CurriedHOArgs, !IO)
    ),
    output_higher_order_args(ModuleInfo, NumToDrop, Indent, HOArgs, !IO).

%-----------------------------------------------------------------------------%

:- pred fixup_preds(list(pred_proc_id)::in, higher_order_global_info::in,
    higher_order_global_info::out) is det.

fixup_preds(PredProcIds, !Info) :-
    MustRecompute = no,
    Requests0 = !.Info ^ requests,
    list.foldl(fixup_pred(MustRecompute), PredProcIds, !Info),
    % Any additional requests must have already been denied.
    !:Info = !.Info ^ requests := Requests0.

:- pred fixup_specialized_versions(list(new_pred)::in,
    higher_order_global_info::in, higher_order_global_info::out) is det.

fixup_specialized_versions(NewPredList, !Info) :-
    NewPredProcIds = list.map(get_np_version_ppid, NewPredList),
    % Reprocess the goals to find any new specializations made
    % possible by the specializations performed in this pass.
    MustRecompute = yes,
    list.foldl(fixup_pred(MustRecompute), NewPredProcIds, !Info).

    % Fixup calls to specialized predicates.
    %
:- pred fixup_pred(bool::in, pred_proc_id::in,
    higher_order_global_info::in, higher_order_global_info::out) is det.

fixup_pred(MustRecompute, proc(PredId, ProcId), !GlobalInfo) :-
    traverse_proc(MustRecompute, PredId, ProcId, !GlobalInfo).

%-----------------------------------------------------------------------------%

    % Build a proc_info for a specialized version.
    %
:- pred create_new_proc(new_pred::in, proc_info::in, pred_info::in,
    pred_info::out, higher_order_global_info::in,
    higher_order_global_info::out) is det.

create_new_proc(NewPred, !.NewProcInfo, !NewPredInfo, !Info) :-
    ModuleInfo = !.Info ^ module_info,

    NewPred = new_pred(NewPredProcId, OldPredProcId, CallerPredProcId, _Name,
        HOArgs0, CallArgs, ExtraTypeInfoTVars0, CallerArgTypes0, _, _, _),

    proc_info_get_headvars(!.NewProcInfo, HeadVars0),
    proc_info_get_argmodes(!.NewProcInfo, ArgModes0),
    pred_info_get_exist_quant_tvars(!.NewPredInfo, ExistQVars0),
    pred_info_get_typevarset(!.NewPredInfo, TypeVarSet0),
    pred_info_get_tvar_kinds(!.NewPredInfo, KindMap0),
    pred_info_get_arg_types(!.NewPredInfo, OriginalArgTypes0),

    CallerPredProcId = proc(CallerPredId, _),
    module_info_pred_info(ModuleInfo, CallerPredId, CallerPredInfo),
    pred_info_get_typevarset(CallerPredInfo, CallerTypeVarSet),
    pred_info_get_univ_quant_tvars(CallerPredInfo, CallerHeadParams),

    % Specialize the types of the called procedure as for inlining.
    proc_info_get_vartypes(!.NewProcInfo, VarTypes0),
    tvarset_merge_renaming(CallerTypeVarSet, TypeVarSet0, TypeVarSet,
        TypeRenaming),
    apply_variable_renaming_to_tvar_kind_map(TypeRenaming, KindMap0, KindMap),
    apply_variable_renaming_to_vartypes(TypeRenaming, VarTypes0, VarTypes1),
    apply_variable_renaming_to_type_list(TypeRenaming,
        OriginalArgTypes0, OriginalArgTypes1),

    % The real set of existentially quantified variables may be
    % smaller, but this is OK.
    apply_variable_renaming_to_tvar_list(TypeRenaming,
        ExistQVars0, ExistQVars1),

    inlining.get_type_substitution(OriginalArgTypes1, CallerArgTypes0,
        CallerHeadParams, ExistQVars1, TypeSubn),

    apply_rec_subst_to_tvar_list(KindMap, TypeSubn, ExistQVars1, ExistQTypes),
    ExistQVars = list.filter_map(
        (func(ExistQType) = ExistQVar is semidet :-
            ExistQType = type_variable(ExistQVar, _)
        ), ExistQTypes),

    apply_rec_subst_to_vartypes(TypeSubn, VarTypes1, VarTypes2),
    apply_rec_subst_to_type_list(TypeSubn,
        OriginalArgTypes1, OriginalArgTypes),
    proc_info_set_vartypes(VarTypes2, !NewProcInfo),

    % XXX kind inference: we assume vars have kind `star'.
    prog_type.var_list_to_type_list(map.init, ExtraTypeInfoTVars0,
        ExtraTypeInfoTVarTypes0),
    (
        ( map.is_empty(TypeSubn)
        ; ExistQVars = []
        )
    ->
        HOArgs = HOArgs0,
        ExtraTypeInfoTVarTypes = ExtraTypeInfoTVarTypes0,
        ExtraTypeInfoTVars = ExtraTypeInfoTVars0
    ;
        % If there are existentially quantified variables in the callee
        % we may need to bind type variables in the caller.
        list.map(substitute_higher_order_arg(TypeSubn), HOArgs0, HOArgs),

        apply_rec_subst_to_type_list(TypeSubn, ExtraTypeInfoTVarTypes0,
            ExtraTypeInfoTVarTypes),
        % The substitution should never bind any of the type variables
        % for which extra type-infos are needed, otherwise it
        % wouldn't be necessary to add them.
        (
            prog_type.type_list_to_var_list(ExtraTypeInfoTVarTypes,
                ExtraTypeInfoTVarsPrim)
        ->
            ExtraTypeInfoTVars = ExtraTypeInfoTVarsPrim
        ;
            unexpected(this_file, "create_new_proc: type var got bound")
        )
    ),

    % Add in the extra typeinfo vars.
    list.map(polymorphism.build_type_info_type,
        ExtraTypeInfoTVarTypes, ExtraTypeInfoTypes),
    proc_info_create_vars_from_types(ExtraTypeInfoTypes, ExtraTypeInfoVars,
        !NewProcInfo),

    % Add any extra type-infos or typeclass-infos we've added
    % to the typeinfo_varmap and typeclass_info_varmap.
    proc_info_get_rtti_varmaps(!.NewProcInfo, RttiVarMaps0),

    % The variable renaming doesn't rename variables in the callee.
    map.init(EmptyVarRenaming),

    apply_substitutions_to_rtti_varmaps(TypeRenaming, TypeSubn,
        EmptyVarRenaming, RttiVarMaps0, RttiVarMaps1),

    % XXX see below

    % Add entries in the typeinfo_varmap for the extra type-infos.
    list.foldl_corresponding(rtti_det_insert_type_info_type,
        ExtraTypeInfoVars, ExtraTypeInfoTVarTypes,
        RttiVarMaps1, RttiVarMaps2),
    Pred = (pred(TVar::in, Var::in, !.R::in, !:R::out) is det :-
            Locn = type_info(Var),
            rtti_set_type_info_locn(TVar, Locn, !R)
        ),
    list.foldl_corresponding(Pred, ExtraTypeInfoTVars, ExtraTypeInfoVars,
        RttiVarMaps2, RttiVarMaps),

    proc_info_set_rtti_varmaps(RttiVarMaps, !NewProcInfo),

    map.from_corresponding_lists(CallArgs, HeadVars0, VarRenaming0),

    % Construct the constant input closures within the goal
    % for the called procedure.
    map.init(PredVars0),
    construct_higher_order_terms(ModuleInfo, HeadVars0, ExtraHeadVars,
        ArgModes0, ExtraArgModes, HOArgs, !NewProcInfo,
        VarRenaming0, _, PredVars0, PredVars, ConstGoals),

    % XXX the substitutions used to be applied to the typeclass_info_varmap
    % here rather than at the XXX above.  Any new entries added in the code
    % between these two points should therefore be transformed as well?
    % The new entries come from HOArgs, which have already had TypeSubn
    % applied, but not TypeRenaming.  Perhaps this is enough?

    % Record extra information about this version.
    VersionInfoMap0 = !.Info ^ version_info,
    ArgsDepth = higher_order_args_depth(HOArgs),

    ( map.search(VersionInfoMap0, OldPredProcId, OldProcVersionInfo) ->
        OldProcVersionInfo = version_info(OrigPredProcId, _, _, _)
    ;
        OrigPredProcId = OldPredProcId
    ),

    ( map.search(VersionInfoMap0, CallerPredProcId, CallerVersionInfo) ->
        CallerVersionInfo = version_info(_, _, _, CallerParentVersions)
    ;
        CallerParentVersions = []
    ),
    ParentVersions = [parent_version_info(OrigPredProcId, ArgsDepth)
        | CallerParentVersions],

    VersionInfo = version_info(OrigPredProcId, ArgsDepth,
        PredVars, ParentVersions),
    map.det_insert(VersionInfoMap0, NewPredProcId, VersionInfo,
        VersionInfoMap),
    !:Info = !.Info ^ version_info := VersionInfoMap,

    % Fix up the argument vars, types and modes.
    in_mode(InMode),
    list.length(ExtraTypeInfoVars, NumTypeInfos),
    list.duplicate(NumTypeInfos, InMode, ExtraTypeInfoModes),

    remove_const_higher_order_args(1, HeadVars0, HOArgs, HeadVars1),
    remove_const_higher_order_args(1, ArgModes0, HOArgs, ArgModes1),
    list.condense([ExtraTypeInfoVars, ExtraHeadVars, HeadVars1], HeadVars),
    list.condense([ExtraTypeInfoModes, ExtraArgModes, ArgModes1], ArgModes),
    proc_info_set_headvars(HeadVars, !NewProcInfo),
    proc_info_set_argmodes(ArgModes, !NewProcInfo),

    proc_info_get_goal(!.NewProcInfo, Goal6),
    Goal6 = _ - GoalInfo6,
    goal_to_conj_list(Goal6, GoalList6),
    conj_list_to_goal(list.append(ConstGoals, GoalList6), GoalInfo6, Goal),
    proc_info_set_goal(Goal, !NewProcInfo),

    proc_info_get_vartypes(!.NewProcInfo, VarTypes7),
    map.apply_to_list(ExtraHeadVars, VarTypes7, ExtraHeadVarTypes0),
    remove_const_higher_order_args(1, OriginalArgTypes,
        HOArgs, ModifiedOriginalArgTypes),
    list.condense([ExtraTypeInfoTypes, ExtraHeadVarTypes0,
        ModifiedOriginalArgTypes], ArgTypes),
    pred_info_set_arg_types(TypeVarSet, ExistQVars, ArgTypes, !NewPredInfo),
    pred_info_set_typevarset(TypeVarSet, !NewPredInfo),

    % The types of the headvars in the vartypes map in the proc_info may be
    % more specific than the argument types returned by pred_info_argtypes
    % if the procedure body binds some existentially quantified type variables.
    % The types of the extra arguments added by construct_higher_order_terms
    % use the substitution computed based on the result pred_info_get_arg_types.
    % We may need to apply a substitution to the types of the new variables
    % in the vartypes in the proc_info.
    %
    % XXX We should apply this substitution to the variable types in any
    % callers of this predicate, which may introduce other opportunities
    % for specialization.
    %
    (
        ExistQVars = []
    ;
        ExistQVars = [_ | _],
        map.apply_to_list(HeadVars0, VarTypes7, OriginalHeadTypes),
        (
            type_list_subsumes(OriginalArgTypes,
                OriginalHeadTypes, ExistentialSubn)
        ->
            apply_rec_subst_to_type_list(ExistentialSubn, ExtraHeadVarTypes0,
                ExtraHeadVarTypes),
            assoc_list.from_corresponding_lists(ExtraHeadVars,
                ExtraHeadVarTypes, ExtraHeadVarsAndTypes),
            list.foldl(update_var_types, ExtraHeadVarsAndTypes,
                VarTypes7, VarTypes8),
            proc_info_set_vartypes(VarTypes8, !NewProcInfo)
        ;
            unexpected(this_file, "create_new_proc: " ++
                "type_list_subsumes failed")
        )
    ),

    % Find the new class context.
    proc_info_get_headvars(!.NewProcInfo, ArgVars),
    proc_info_get_rtti_varmaps(!.NewProcInfo, NewRttiVarMaps),
    list.map(rtti_varmaps_var_info(NewRttiVarMaps), ArgVars, ArgVarInfos),
    find_class_context(ModuleInfo, ArgVarInfos, ArgModes, [], [],
        ClassContext),
    pred_info_set_class_context(ClassContext, !NewPredInfo),

    map.init(NewProcs0),
    NewPredProcId = proc(_, NewProcId),
    map.det_insert(NewProcs0, NewProcId, !.NewProcInfo, NewProcs),
    pred_info_set_procedures(NewProcs, !NewPredInfo).

:- pred update_var_types(pair(prog_var, mer_type)::in,
    vartypes::in, vartypes::out) is det.

update_var_types(VarAndType, !Map) :-
    VarAndType = Var - Type,
    svmap.det_update(Var, Type, !Map).

    % Take an original list of headvars and arg_modes and return these
    % with curried arguments added.  The old higher-order arguments are
    % left in. They may be needed in calls which could not be
    % specialised. If not, unused_args.m can clean them up.
    %
    % Build the initial pred_vars map which records higher-order and
    % type_info constants for a call to traverse_goal.
    %
    % Build a var-var renaming from the requesting call's arguments to
    % the headvars of the specialized version.
    %
    % This predicate is recursively applied to all curried higher order
    % arguments of higher order arguments.
    %
    % Update higher_order_arg_order_version if the order or number of
    % the arguments for specialized versions changes.
    %
:- pred construct_higher_order_terms(module_info::in, list(prog_var)::in,
    list(prog_var)::out, list(mer_mode)::in, list(mer_mode)::out,
    list(higher_order_arg)::in, proc_info::in, proc_info::out,
    map(prog_var, prog_var)::in, map(prog_var, prog_var)::out,
    pred_vars::in, pred_vars::out, list(hlds_goal)::out) is det.

construct_higher_order_terms(_, _, [], _, [], [], !ProcInfo, !Renaming,
        !PredVars, []).
construct_higher_order_terms(ModuleInfo, HeadVars0, NewHeadVars, ArgModes0,
        NewArgModes, [HOArg | HOArgs], !ProcInfo, !Renaming,
        !PredVars, ConstGoals) :-
    HOArg = higher_order_arg(ConsId, Index, NumArgs, CurriedArgs,
        CurriedArgTypes, CurriedArgRttiInfo, CurriedHOArgs, IsConst),

    list.index1_det(HeadVars0, Index, LVar),
    ( ConsId = pred_const(ShroudedPredProcId, _) ->
        % Add the curried arguments to the procedure's argument list.
        proc(PredId, ProcId) =
            unshroud_pred_proc_id(ShroudedPredProcId),
        module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
            CalledPredInfo, CalledProcInfo),
        PredOrFunc = pred_info_is_pred_or_func(CalledPredInfo),
        proc_info_get_argmodes(CalledProcInfo, CalledArgModes),
        (
            list.split_list(NumArgs, CalledArgModes,
                CurriedArgModes0, NonCurriedArgModes0)
        ->
            NonCurriedArgModes = NonCurriedArgModes0,
            CurriedArgModes1 = CurriedArgModes0
        ;
            unexpected(this_file, "construct_higher_order_terms/13:" ++
                "call to list.split_list failed.")
        ),
        proc_info_interface_determinism(CalledProcInfo, ProcDetism),
        GroundInstInfo = higher_order(pred_inst_info(PredOrFunc,
            NonCurriedArgModes, ProcDetism))
    ;
        in_mode(InMode),
        GroundInstInfo = none,
        list.duplicate(NumArgs, InMode, CurriedArgModes1)
    ),

    proc_info_create_vars_from_types(CurriedArgTypes, CurriedHeadVars1,
        !ProcInfo),

    proc_info_get_rtti_varmaps(!.ProcInfo, RttiVarMaps0),
    list.foldl_corresponding(add_rtti_info, CurriedHeadVars1,
        CurriedArgRttiInfo, RttiVarMaps0, RttiVarMaps),
    proc_info_set_rtti_varmaps(RttiVarMaps, !ProcInfo),

    (
        IsConst = no,
        % Make traverse_goal pretend that the input higher-order
        % argument is built using the new arguments as its curried
        % arguments.
        svmap.det_insert(LVar, constant(ConsId, CurriedHeadVars1),
            !PredVars)
    ;
        IsConst = yes
    ),

    assoc_list.from_corresponding_lists(CurriedArgs, CurriedHeadVars1,
        CurriedRenaming),
    list.foldl(
        (pred(VarPair::in, !.Map::in, !:Map::out) is det :-
            VarPair = Var1 - Var2,
            svmap.set(Var1, Var2, !Map)
        ), CurriedRenaming, !Renaming),

    % Recursively construct the curried higher-order arguments.
    construct_higher_order_terms(ModuleInfo, CurriedHeadVars1,
        ExtraCurriedHeadVars, CurriedArgModes1, ExtraCurriedArgModes,
        CurriedHOArgs, !ProcInfo, !Renaming, !PredVars,
        CurriedConstGoals),

    % Construct the rest of the higher-order arguments.
    construct_higher_order_terms(ModuleInfo, HeadVars0, NewHeadVars1,
        ArgModes0, NewArgModes1, HOArgs, !ProcInfo,
        !Renaming, !PredVars, ConstGoals1),

    (
        IsConst = yes,
        % Build the constant inside the specialized version, so that
        % other constants which include it will be recognized as constant.
        modes_to_uni_modes(ModuleInfo, CurriedArgModes1,
            CurriedArgModes1, UniModes),
        set.list_to_set(CurriedHeadVars1, ConstNonLocals),
        ConstInst = ground(shared, GroundInstInfo),
        instmap_delta_from_assoc_list([LVar - ConstInst],
            ConstInstMapDelta),
        goal_info_init(ConstNonLocals, ConstInstMapDelta, detism_det,
            purity_pure, ConstGoalInfo),
        RHS = rhs_functor(ConsId, no, CurriedHeadVars1),
        UniMode = (free -> ConstInst) - (ConstInst -> ConstInst),
        ConstGoal = unify(LVar, RHS, UniMode,
            construct(LVar, ConsId, CurriedHeadVars1, UniModes,
                construct_dynamically, cell_is_unique, no_construct_sub_info),
            unify_context(umc_explicit, [])) - ConstGoalInfo,
        ConstGoals0 = CurriedConstGoals ++ [ConstGoal]
    ;
        IsConst = no,
        ConstGoals0 = CurriedConstGoals
    ),

    % Fix up the argument lists.
    remove_const_higher_order_args(1, CurriedHeadVars1, CurriedHOArgs,
        CurriedHeadVars),
    remove_const_higher_order_args(1, CurriedArgModes1, CurriedHOArgs,
        CurriedArgModes),
    list.condense([CurriedHeadVars, ExtraCurriedHeadVars, NewHeadVars1],
        NewHeadVars),
    list.condense([CurriedArgModes, ExtraCurriedArgModes, NewArgModes1],
        NewArgModes),
    list.append(ConstGoals0, ConstGoals1, ConstGoals).

    % Add any new type-infos or typeclass-infos to the rtti_varmaps.
    %
:- pred add_rtti_info(prog_var::in, rtti_var_info::in,
    rtti_varmaps::in, rtti_varmaps::out) is det.

add_rtti_info(Var, VarInfo, !RttiVarMaps) :-
    (
        VarInfo = type_info_var(TypeInfoType),
        rtti_det_insert_type_info_type(Var, TypeInfoType, !RttiVarMaps),
        ( TypeInfoType = type_variable(TVar, _) ->
            maybe_set_typeinfo_locn(TVar, type_info(Var), !RttiVarMaps)
        ;
            true
        )
    ;
        VarInfo = typeclass_info_var(Constraint),
        ( rtti_search_typeclass_info_var(!.RttiVarMaps, Constraint, _) ->
            true
        ;
            rtti_det_insert_typeclass_info_var(Constraint, Var, !RttiVarMaps),
            Constraint = constraint(_, ConstraintTypes),
            list.foldl2(update_type_info_locn(Var), ConstraintTypes, 1, _,
                !RttiVarMaps)
        )
    ;
        VarInfo = non_rtti_var
    ).

:- pred update_type_info_locn(prog_var::in, mer_type::in, int::in, int::out,
    rtti_varmaps::in, rtti_varmaps::out) is det.

update_type_info_locn(Var, ConstraintType, Index, Index + 1, !RttiVarMaps) :-
    ( ConstraintType = type_variable(ConstraintTVar, _) ->
        maybe_set_typeinfo_locn(ConstraintTVar,
            typeclass_info(Var, Index), !RttiVarMaps)
    ;
        true
    ).

:- pred maybe_set_typeinfo_locn(tvar::in, type_info_locn::in,
    rtti_varmaps::in, rtti_varmaps::out) is det.

maybe_set_typeinfo_locn(TVar, Locn, !RttiVarMaps) :-
    ( rtti_search_type_info_locn(!.RttiVarMaps, TVar, _) ->
        true
    ;
        rtti_det_insert_type_info_locn(TVar, Locn, !RttiVarMaps)
    ).

:- pred remove_const_higher_order_args(int::in, list(T)::in,
    list(higher_order_arg)::in, list(T)::out) is det.

remove_const_higher_order_args(_, [], _, []).
remove_const_higher_order_args(Index, [Arg | Args0], HOArgs0, Args) :-
    ( HOArgs0 = [HOArg | HOArgs] ->
        HOArg = higher_order_arg(_, HOIndex, _, _, _, _, _, IsConst),
        ( HOIndex = Index ->
            remove_const_higher_order_args(Index + 1, Args0, HOArgs, Args1),
            (
                IsConst = yes,
                Args = Args1
            ;
                IsConst = no,
                Args = [Arg | Args1]
            )
        ; HOIndex > Index ->
            remove_const_higher_order_args(Index + 1, Args0, HOArgs0, Args1),
            Args = [Arg | Args1]
        ;
            unexpected(this_file, "remove_const_higher_order_args")
        )
    ;
        Args = [Arg | Args0]
    ).

:- func higher_order_arg_order_version = int.

higher_order_arg_order_version = 1.

%-----------------------------------------------------------------------------%

    % Substitute the types in a higher_order_arg.
    %
:- pred substitute_higher_order_arg(tsubst::in, higher_order_arg::in,
    higher_order_arg::out) is det.

substitute_higher_order_arg(Subn, !HOArg) :-
    CurriedArgTypes0 = !.HOArg ^ hoa_curry_type_in_caller,
    CurriedRttiTypes0 = !.HOArg ^ hoa_curry_rtti_type,
    CurriedHOArgs0 = !.HOArg ^ hoa_known_curry_args,
    apply_rec_subst_to_type_list(Subn, CurriedArgTypes0, CurriedArgTypes),
    list.map(substitute_rtti_var_info(Subn), CurriedRttiTypes0,
        CurriedRttiTypes),
    list.map(substitute_higher_order_arg(Subn), CurriedHOArgs0, CurriedHOArgs),
    !:HOArg = !.HOArg ^ hoa_curry_type_in_caller := CurriedArgTypes,
    !:HOArg = !.HOArg ^ hoa_curry_rtti_type := CurriedRttiTypes,
    !:HOArg = !.HOArg ^ hoa_known_curry_args := CurriedHOArgs.

:- pred substitute_rtti_var_info(tsubst::in, rtti_var_info::in,
    rtti_var_info::out) is det.

substitute_rtti_var_info(Subn, type_info_var(Type0), type_info_var(Type)) :-
    apply_rec_subst_to_type(Subn, Type0, Type).
substitute_rtti_var_info(Subn, typeclass_info_var(Constraint0),
        typeclass_info_var(Constraint)) :-
    apply_rec_subst_to_prog_constraint(Subn, Constraint0, Constraint).
substitute_rtti_var_info(_, non_rtti_var, non_rtti_var).

%-----------------------------------------------------------------------------%

:- func higher_order_args_size(list(higher_order_arg)) = int.

higher_order_args_size(Args) =
    list.foldl(int.max, list.map(higher_order_arg_size, Args), 0).

:- func higher_order_arg_size(higher_order_arg) = int.

higher_order_arg_size(HOArg) =
    1 + higher_order_args_size(HOArg ^ hoa_known_curry_args).

:- func higher_order_args_depth(list(higher_order_arg)) = int.

higher_order_args_depth(Args) =
    list.foldl(int.max, list.map(higher_order_arg_depth, Args), 0).

:- func higher_order_arg_depth(higher_order_arg) = int.

higher_order_arg_depth(HOArg) =
    1 + higher_order_args_depth(HOArg ^ hoa_known_curry_args).

%-----------------------------------------------------------------------------%

    % Collect the list of prog_constraints from the list of argument
    % types. The typeclass_info for universal constraints is input,
    % output for existential constraints.
    %
:- pred find_class_context(module_info::in, list(rtti_var_info)::in,
    list(mer_mode)::in, list(prog_constraint)::in, list(prog_constraint)::in,
    prog_constraints::out) is det.

find_class_context(_, [], [], Univ0, Exist0, Constraints) :-
    list.reverse(Univ0, Univ),
    list.reverse(Exist0, Exist),
    Constraints = constraints(Univ, Exist).
find_class_context(_, [], [_ | _], _, _, _) :-
    unexpected(this_file, "mismatched list length in find_class_context/6.").
find_class_context(_, [_ | _], [], _, _, _) :-
    unexpected(this_file, "mismatched list length in find_class_context/6.").
find_class_context(ModuleInfo, [VarInfo | VarInfos], [Mode | Modes],
        !.Univ, !.Exist, Constraints) :-
    (
        VarInfo = typeclass_info_var(Constraint)
    ->
        ( mode_is_input(ModuleInfo, Mode) ->
            maybe_add_constraint(Constraint, !Univ)
        ;
            maybe_add_constraint(Constraint, !Exist)
        )
    ;
        true
    ),
    find_class_context(ModuleInfo, VarInfos, Modes, !.Univ, !.Exist,
        Constraints).

:- pred maybe_add_constraint(prog_constraint::in,
    list(prog_constraint)::in, list(prog_constraint)::out) is det.

maybe_add_constraint(Constraint, !Constraints) :-
    % Don't create duplicates.
    ( list.member(Constraint, !.Constraints) ->
        true
    ;
        list.cons(Constraint, !Constraints)
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "higher_order.m".

%-----------------------------------------------------------------------------%
:- end_module higher_order.
%-----------------------------------------------------------------------------%
