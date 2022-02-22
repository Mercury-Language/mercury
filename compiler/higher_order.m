%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
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
% Since this creates a new copy of the called procedure, I have limited the
% specialization to cases where the called procedure's goal contains less than
% 20 calls and unifications. For predicates above this size, the overhead of
% the higher order call becomes less significant while the increase in code
% size becomes significant. The limit can be changed using
% `--higher-order-size-limit'.
%
% If a specialization creates new opportunities for specialization, we will
% continue iterating the specialization process until we find no further
% opportunities, i.e. until we reach a fixpoint.
%
% The specialized version of a predicate 'foo' is named 'foo.ho<n>',
% where n is a number that uniquely identifies this specialized version.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module transform_hlds.higher_order.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred specialize_higher_order(module_info::in, module_info::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.mode_test.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.polymorphism_type_info.
:- import_module check_hlds.recompute_instmap_deltas.
:- import_module check_hlds.type_util.
:- import_module hlds.add_special_pred.
:- import_module hlds.const_struct.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_args.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module hlds.instmap.
:- import_module hlds.make_goal.
:- import_module hlds.passes_aux.
:- import_module hlds.pred_table.
:- import_module hlds.quantification.
:- import_module hlds.special_pred.
:- import_module hlds.status.
:- import_module hlds.vartypes.
:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.optimization_options.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.pred_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.set_of_var.

:- import_module assoc_list.
:- import_module bool.
:- import_module counter.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

specialize_higher_order(!ModuleInfo, !IO) :-
    % Iterate collecting requests and process them until there are no more
    % requests remaining.

    module_info_get_globals(!.ModuleInfo, Globals),
    globals.get_opt_tuple(Globals, OptTuple),
    HigherOrder = OptTuple ^ ot_opt_higher_order,
    TypeSpec = OptTuple ^ ot_spec_types,
    UserTypeSpec = OptTuple ^ ot_spec_types_user_guided,
    SizeLimit = OptTuple ^ ot_higher_order_size_limit,
    ArgLimit = OptTuple ^ ot_higher_order_arg_limit,
    Params =
        ho_params(HigherOrder, TypeSpec, UserTypeSpec, SizeLimit, ArgLimit),
    map.init(NewPredMap0),
    map.init(GoalSizes0),
    set.init(Requests0),
    map.init(VersionInfo0),
    some [!GlobalInfo] (
        !:GlobalInfo = higher_order_global_info(Requests0, NewPredMap0,
            VersionInfo0, !.ModuleInfo, GoalSizes0, Params, counter.init(1)),

        module_info_get_valid_pred_ids(!.ModuleInfo, ValidPredIds),
        module_info_get_type_spec_info(!.ModuleInfo, TypeSpecInfo),
        TypeSpecInfo = type_spec_info(_, UserSpecPredIdSet, _, _),

        % Make sure the user requested specializations are processed first,
        % since we don't want to create more versions if one of these matches.
        % We need to process these even if specialization is not being
        % performed, in case any of the specialized versions are called
        % from other modules.

        set.to_sorted_list(UserSpecPredIdSet, UserSpecPredIds),
        (
            UserSpecPredIds = [],
            NonUserSpecPredIds = ValidPredIds
        ;
            UserSpecPredIds = [_ | _],
            set.list_to_set(ValidPredIds, ValidPredIdSet),
            set.difference(ValidPredIdSet, UserSpecPredIdSet,
                NonUserSpecPredIdSet),
            set.to_sorted_list(NonUserSpecPredIdSet, NonUserSpecPredIds),

            !GlobalInfo ^ hogi_params ^ param_do_user_type_spec
                := spec_types_user_guided,
            list.foldl(get_specialization_requests, UserSpecPredIds,
                !GlobalInfo),
            process_ho_spec_requests(!GlobalInfo, !IO)
        ),

        ( if
            ( HigherOrder = opt_higher_order
            ; TypeSpec = spec_types
            ; UserTypeSpec = spec_types_user_guided
            )
        then
            % Process all other specializations until no more requests
            % are generated.
            list.foldl(get_specialization_requests, NonUserSpecPredIds,
                !GlobalInfo),
            recursively_process_ho_spec_requests(!GlobalInfo, !IO)
        else
            true
        ),

        % Remove the predicates which were used to force the production of
        % user-requested type specializations, since they are not called
        % from anywhere and are no longer needed.
        list.foldl(module_info_remove_predicate, UserSpecPredIds,
            !.GlobalInfo ^ hogi_module_info, !:ModuleInfo)
    ).

    % Process one lot of requests, returning requests for any
    % new specializations made possible by the first lot.
    %
:- pred process_ho_spec_requests(higher_order_global_info::in,
    higher_order_global_info::out, io::di, io::uo) is det.

process_ho_spec_requests(!GlobalInfo, !IO) :-
    Requests0 = set.to_sorted_list(!.GlobalInfo ^ hogi_requests),
    !GlobalInfo ^ hogi_requests := set.init,
    list.foldl3(filter_request(!.GlobalInfo), Requests0,
        [], Requests, [], LoopRequests, !IO),
    (
        Requests = []
    ;
        Requests = [_ | _],
        some [!PredProcsToFix] (
            set.init(!:PredProcsToFix),
            maybe_create_new_preds(Requests, [], NewPredList, !PredProcsToFix,
                !GlobalInfo, !IO),
            list.foldl(check_loop_request(!.GlobalInfo), LoopRequests,
                !PredProcsToFix),
            set.to_sorted_list(!.PredProcsToFix, PredProcs)
        ),
        ho_fixup_specialized_versions(NewPredList, !GlobalInfo),
        ho_fixup_preds(PredProcs, !GlobalInfo),
        (
            NewPredList = [_ | _],
            % The dependencies may have changed, so the dependency graph
            % needs to rebuilt for inlining to work properly.
            ModuleInfo0 = !.GlobalInfo ^ hogi_module_info,
            module_info_clobber_dependency_info(ModuleInfo0, ModuleInfo),
            !GlobalInfo ^ hogi_module_info := ModuleInfo
        ;
            NewPredList = []
        )
    ).

    % Process requests until there are no new requests to process.
    %
:- pred recursively_process_ho_spec_requests(higher_order_global_info::in,
    higher_order_global_info::out, io::di, io::uo) is det.

recursively_process_ho_spec_requests(!GlobalInfo, !IO) :-
    ( if set.is_empty(!.GlobalInfo ^ hogi_requests) then
        true
    else
        process_ho_spec_requests(!GlobalInfo, !IO),
        recursively_process_ho_spec_requests(!GlobalInfo, !IO)
    ).

%-----------------------------------------------------------------------------%

:- type higher_order_global_info
    --->    higher_order_global_info(
                % Requested versions.
                hogi_requests       :: set(ho_request),

                % Specialized versions for each predicate
                % not changed by ho_traverse_proc_body.
                hogi_new_pred_map   :: new_pred_map,

                % Extra information about each specialized version.
                hogi_version_info   :: map(pred_proc_id, version_info),

                hogi_module_info    :: module_info,
                hogi_goal_sizes     :: goal_sizes,
                hogi_params         :: ho_params,

                % Number identifying a specialized version.
                hogi_next_id        :: counter
            ).

    % Used while traversing goals.
    %
:- type higher_order_info
    --->    higher_order_info(
                hoi_global_info         :: higher_order_global_info,

                % Higher order variables with unique known values.
                hoi_known_var_map       :: known_var_map,

                % The pred_proc_id, pred_info and proc_info of the procedure
                % whose body is being traversed.
                hoi_pred_proc_id        :: pred_proc_id,
                hoi_pred_info           :: pred_info,
                hoi_proc_info           :: proc_info,

                hoi_changed             :: ho_changed
            ).

:- type ho_request
    --->    ho_request(
                % Calling predicate.
                rq_caller               :: pred_proc_id,

                % Called predicate.
                rq_callee               :: pred_proc_id,

                % The call's arguments.
                rq_args                 :: list(prog_var),

                % Type variables for which extra type-infos must be passed
                % from the caller if --typeinfo-liveness is set.
                rq_tvars                :: list(tvar),

                % Argument types in caller.
                rq_ho_args              :: list(higher_order_arg),
                rq_caller_types         :: list(mer_type),

                % Caller's typevarset.
                rq_caller_tvarset       :: tvarset,

                % Should the interface of the specialized procedure
                % use typeinfo liveness?
                % XXX Unfortunately, this field is not doing its job.
                % First, it is only ever set to "yes", so it is redundant.
                % Second, its value is only ever used for one thing, which
                % is to set the value of the np_typeinfo_liveness field
                % in the new_pred type, which is itself never used.
                rq_typeinfo_liveness    :: bool,

                % Is this a user-requested specialization?
                rq_user_req_spec        :: bool,

                % Context of the call which caused the request to be generated.
                rq_call_context         :: context
            ).

    % Stores cons_id, index in argument vector, number of curried arguments
    % of a higher order argument, higher-order curried arguments with known
    % values. For cons_ids other than pred_const and `type_info', the arguments
    % must be constants.
    %
:- type higher_order_arg
    --->    higher_order_arg(
                hoa_cons_id                 :: cons_id,

                % Index in argument vector.
                hoa_index                   :: int,

                % Number of curried args.
                hoa_num_curried_args        :: int,

                % Curried arguments in caller.
                hoa_curry_arg_in_caller     :: list(prog_var),

                % Curried argument types in caller.
                hoa_curry_type_in_caller    :: list(mer_type),

                % Types associated with type_infos and constraints associated
                % with typeclass_infos in the arguments.
                hoa_curry_rtti_type         :: list(rtti_var_info),

                % Higher-order curried arguments with known values.
                hoa_known_curry_args        :: list(higher_order_arg),

                % Is this higher_order_arg a constant?
                hoa_is_constant             :: bool
            ).

    % Stores the size of each predicate's goal used in the heuristic
    % to decide which preds are specialized.
    %
:- type goal_sizes == map(pred_id, int).

    % Used to hold the value of known higher order variables.
    % If a variable is not in the map, it does not have a unique known value.
    %
:- type known_var_map == map(prog_var, known_const).

:- type new_pred_map == map(pred_proc_id, set(new_pred)).

    % The list of vars is a list of the curried arguments, which must
    % be explicitly passed to the specialized predicate.
    % For cons_ids other than pred_const and `type_info', the arguments
    % must be constants. For pred_consts and type_infos, non-constant
    % arguments are passed through to any specialised version.
    %
:- type known_const
    --->    known_const(cons_id, list(prog_var)).

:- type ho_params
    --->    ho_params(
                % Propagate higher-order constants.
                param_do_higher_order_spec  :: maybe_opt_higher_order,

                % Propagate type-info constants.
                param_do_type_spec          :: maybe_spec_types,

                % User-guided type specialization.
                param_do_user_type_spec     :: maybe_spec_types_user_guided,

                % Size limit on requested version.
                param_size_limit            :: int,

                % The maximum size of the higher order arguments
                % of a specialized version.
                param_arg_limit             :: int
            ).

:- type version_info
    --->    version_info(
                % The procedure from the original program from which
                % this version was created.
                pred_proc_id,

                % Depth of the higher_order_args for this version.
                int,

                % Higher-order or constant input variables for a
                % specialised version.
                known_var_map,

                % The chain of specialized versions which caused this version
                % to be created. For each element in the list with the same
                % pred_proc_id, the depth must decrease. This ensures that
                % the specialization process must terminate.
                list(parent_version_info)
            ).

:- type parent_version_info
    --->    parent_version_info(
                % The procedure from the original program from which
                % this parent was created.
                pred_proc_id,

                % Depth of the higher_order_args for this version.
                int
            ).

:- type new_pred
    --->    new_pred(
                % version pred_proc_id
                np_version_ppid         :: pred_proc_id,

                % old pred_proc_id
                np_old_ppid             :: pred_proc_id,

                % requesting caller
                np_req_ppid             :: pred_proc_id,

                % name
                np_name                 :: sym_name,

                % specialized args
                np_spec_args            :: list(higher_order_arg),

                % Unspecialised argument vars in caller.
                np_unspec_actuals       :: list(prog_var),

                % Extra typeinfo tvars in caller.
                np_extra_act_ti_vars    :: list(tvar),

                % Unspecialised argument types in requesting caller.
                np_unspec_act_types     :: list(mer_type),

                % Caller's typevarset.
                np_call_tvarset         :: tvarset,

                % Does the interface of the specialized version use type-info
                % liveness?
                % XXX Unfortunately, this field is not doing its job;
                % its value is never used for anything.
                np_typeinfo_liveness    :: bool,

                % Is this a user-specified type specialization?
                np_is_user_spec         :: bool
            ).

    % Returned by ho_traverse_proc_body.
    %
:- type ho_changed
    --->    ho_changed      % Need to requantify goal + check other procs
    ;       ho_request      % Need to check other procs
    ;       ho_unchanged.   % Do nothing more for this predicate

:- func get_np_version_ppid(new_pred) = pred_proc_id.

get_np_version_ppid(NewPred) = NewPred ^ np_version_ppid.

%-----------------------------------------------------------------------------%

:- pred get_specialization_requests(pred_id::in,
    higher_order_global_info::in, higher_order_global_info::out) is det.

get_specialization_requests(PredId, !GlobalInfo) :-
    ModuleInfo0 = !.GlobalInfo ^ hogi_module_info,
    module_info_pred_info(ModuleInfo0, PredId, PredInfo0),
    NonImportedProcs = pred_info_valid_non_imported_procids(PredInfo0),
    (
        NonImportedProcs = []
    ;
        NonImportedProcs = [ProcId | _],
        list.foldl(ho_traverse_proc(need_not_recompute, PredId),
            NonImportedProcs, !GlobalInfo),

        ModuleInfo1 = !.GlobalInfo ^ hogi_module_info,
        module_info_proc_info(ModuleInfo1, PredId, ProcId, ProcInfo),
        proc_info_get_goal(ProcInfo, Goal),
        goal_size(Goal, GoalSize),
        GoalSizes1 = !.GlobalInfo ^ hogi_goal_sizes,
        map.set(PredId, GoalSize, GoalSizes1, GoalSizes),
        !GlobalInfo ^ hogi_goal_sizes := GoalSizes
    ).

    % This is called when the first procedure of a predicate was changed.
    % It fixes up all the other procedures, ignoring the goal_size and requests
    % that come out, since that information has already been collected.
    %
:- pred ho_traverse_proc(must_recompute::in, pred_id::in, proc_id::in,
    higher_order_global_info::in, higher_order_global_info::out) is det.

ho_traverse_proc(MustRecompute, PredId, ProcId, !GlobalInfo) :-
    map.init(KnownVarMap0),
    module_info_pred_proc_info(!.GlobalInfo ^ hogi_module_info,
        PredId, ProcId, PredInfo0, ProcInfo0),
    Info0 = higher_order_info(!.GlobalInfo, KnownVarMap0, proc(PredId, ProcId),
        PredInfo0, ProcInfo0, ho_unchanged),
    ho_traverse_proc_body(MustRecompute, Info0, Info),
    Info = higher_order_info(!:GlobalInfo, _, _, PredInfo, ProcInfo, _),
    ModuleInfo0 = !.GlobalInfo ^ hogi_module_info,
    module_info_set_pred_proc_info(PredId, ProcId, PredInfo, ProcInfo,
        ModuleInfo0, ModuleInfo),
    !GlobalInfo ^ hogi_module_info := ModuleInfo.

%-----------------------------------------------------------------------------%
%
% Goal traversal
%

:- pred ho_fixup_proc_info(must_recompute::in, hlds_goal::in,
    higher_order_info::in, higher_order_info::out) is det.

ho_fixup_proc_info(MustRecompute, !.Goal, !Info) :-
    ( if
        ( !.Info ^ hoi_changed = ho_changed
        ; MustRecompute = must_recompute
        )
    then
        % XXX The code whose effects we are now fixing up can eliminate
        % some variables from the code of the procedure. Some of those
        % variables appear in the RTTI varmaps, yet we do not delete them
        % from there. This is a bug.
        some [!ModuleInfo, !ProcInfo] (
            !:ModuleInfo = !.Info ^ hoi_global_info ^ hogi_module_info,
            !:ProcInfo   = !.Info ^ hoi_proc_info,
            proc_info_set_goal(!.Goal, !ProcInfo),
            requantify_proc_general(ordinary_nonlocals_no_lambda, !ProcInfo),
            proc_info_get_goal(!.ProcInfo, !:Goal),
            proc_info_get_initial_instmap(!.ModuleInfo, !.ProcInfo, InstMap),
            proc_info_get_vartypes(!.ProcInfo, VarTypes),
            proc_info_get_inst_varset(!.ProcInfo, InstVarSet),
            recompute_instmap_delta(do_not_recompute_atomic_instmap_deltas,
                !Goal, VarTypes, InstVarSet, InstMap, !ModuleInfo),
            proc_info_set_goal(!.Goal, !ProcInfo),
            !Info ^ hoi_proc_info := !.ProcInfo,
            !Info ^ hoi_global_info ^ hogi_module_info := !.ModuleInfo
        )
    else
        true
    ).

:- pred ho_traverse_proc_body(must_recompute::in,
    higher_order_info::in, higher_order_info::out) is det.

ho_traverse_proc_body(MustRecompute, !Info) :-
    % Lookup the initial known bindings of the variables if this procedure
    % is a specialised version.
    VersionInfoMap = !.Info ^ hoi_global_info ^ hogi_version_info,
    ( if
        map.search(VersionInfoMap, !.Info ^ hoi_pred_proc_id, VersionInfo),
        VersionInfo = version_info(_, _, KnownVarMap, _)
    then
        !Info ^ hoi_known_var_map := KnownVarMap
    else
        true
    ),
    proc_info_get_goal(!.Info ^ hoi_proc_info, Goal0),
    ho_traverse_goal(Goal0, Goal, !Info),
    ho_fixup_proc_info(MustRecompute, Goal, !Info).

    % Traverse the goal collecting higher order variables for which the value
    % is known, specialize calls, and add specialization requests to the
    % request_info structure.
    %
:- pred ho_traverse_goal(hlds_goal::in, hlds_goal::out,
    higher_order_info::in, higher_order_info::out) is det.

ho_traverse_goal(Goal0, Goal, !Info) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = conj(ConjType, Goals0),
        (
            ConjType = plain_conj,
            list.map_foldl(ho_traverse_goal, Goals0, Goals, !Info)
        ;
            ConjType = parallel_conj,
            ho_traverse_parallel_conj(Goals0, Goals, !Info)
        ),
        GoalExpr = conj(ConjType, Goals),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = disj(Goals0),
        ho_traverse_disj(Goals0, Goals, !Info),
        GoalExpr = disj(Goals),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        % A switch is treated as a disjunction.
        ho_traverse_cases(Cases0, Cases, !Info),
        GoalExpr = switch(Var, CanFail, Cases),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = generic_call(GenericCall, Args, _, _, _),
        % Check whether this call could be specialized.
        (
            GenericCall = higher_order(Var, _, _, _),
            maybe_specialize_higher_order_call(Var, Args, Goal0, Goal, !Info)
        ;
            GenericCall = class_method(Var, Method, _, _),
            maybe_specialize_method_call(Var, Method, Args, Goal0, Goal, !Info)
        ;
            ( GenericCall = event_call(_)
            ; GenericCall = cast(_)
            ),
            Goal = Goal0
        )
    ;
        GoalExpr0 = plain_call(_, _, _, _, _, _),
        % Check whether this call can be specialized.
        % XXX Due to the absence of alias tracking, passing Goal0 instead
        % of Goal1 to maybe_specialize_call would result in a mode error.
        Goal1 = hlds_goal(GoalExpr0, GoalInfo0),
        maybe_specialize_call(Goal1, Goal, !Info)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        % If-then-elses are handled as disjunctions.
        get_pre_branch_info(!.Info, PreInfo),
        ho_traverse_goal(Cond0, Cond, !Info),
        ho_traverse_goal(Then0, Then, !Info),
        get_post_branch_info_for_goal(!.Info, Then, PostThenInfo),
        set_pre_branch_info(PreInfo, !Info),
        ho_traverse_goal(Else0, Else, !Info),
        get_post_branch_info_for_goal(!.Info, Else, PostElseInfo),
        merge_post_branch_infos(PostThenInfo, PostElseInfo, PostInfo),
        set_post_branch_info(PostInfo, !Info),
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = negation(SubGoal0),
        ho_traverse_goal(SubGoal0, SubGoal, !Info),
        GoalExpr = negation(SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            Goal = Goal0
        else
            ho_traverse_goal(SubGoal0, SubGoal, !Info),
            GoalExpr = scope(Reason, SubGoal),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        )
    ;
        GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _),
        Goal = Goal0
    ;
        GoalExpr0 = unify(_, _, _, Unification0, _),
        ( if
            Unification0 = construct(_, closure_cons(_, _), _, _, _, _, _)
        then
            maybe_specialize_pred_const(Goal0, Goal, !Info)
        else
            Goal = Goal0
        ),
        ( if Goal = hlds_goal(unify(_, _, _, Unification, _), _) then
            check_unify(Unification, !Info)
        else
            true
        )
    ;
        GoalExpr0 = shorthand(_),
        % These should have been expanded out by now.
        unexpected($pred, "shorthand")
    ).

%-----------------------------------------------------------------------------%

    % To process a parallel conjunction, we process each conjunct with the
    % specialization information before the conjunct, then merge the
    % results to give the specialization information after the conjunction.
    %
:- pred ho_traverse_parallel_conj(hlds_goals::in, hlds_goals::out,
    higher_order_info::in, higher_order_info::out) is det.

ho_traverse_parallel_conj(Goals0, Goals, !Info) :-
    (
        Goals0 = [],
        unexpected($pred, "empty list")
    ;
        Goals0 = [_ | _],
        get_pre_branch_info(!.Info, PreInfo),
        ho_traverse_parallel_conj_loop(PreInfo, Goals0, Goals,
            [], PostInfos, !Info),
        merge_post_branch_infos_into_one(PostInfos, PostInfo),
        set_post_branch_info(PostInfo, !Info)
    ).

:- pred ho_traverse_parallel_conj_loop(pre_branch_info::in,
    hlds_goals::in, hlds_goals::out,
    list(post_branch_info)::in, list(post_branch_info)::out,
    higher_order_info::in, higher_order_info::out) is det.

ho_traverse_parallel_conj_loop(_, [], [], !PostInfos, !Info).
ho_traverse_parallel_conj_loop(PreInfo, [Goal0 | Goals0], [Goal | Goals],
        !PostInfos, !Info) :-
    set_pre_branch_info(PreInfo, !Info),
    ho_traverse_goal(Goal0, Goal, !Info),
    get_post_branch_info_for_goal(!.Info, Goal, GoalPostInfo),
    !:PostInfos = [GoalPostInfo | !.PostInfos],
    ho_traverse_parallel_conj_loop(PreInfo, Goals0, Goals, !PostInfos, !Info).

    % To process a disjunction, we process each disjunct with the
    % specialization information before the goal, then merge the
    % results to give the specialization information after the disjunction.
    %
:- pred ho_traverse_disj(list(hlds_goal)::in, list(hlds_goal)::out,
    higher_order_info::in, higher_order_info::out) is det.

ho_traverse_disj(Goals0, Goals, !Info) :-
    % We handle empty lists separately because merge_post_branch_infos_into_one
    % works only on nonempty lists.
    (
        Goals0 = [],
        Goals = []
    ;
        Goals0 = [_ | _],
        get_pre_branch_info(!.Info, PreInfo),
        ho_traverse_disj_loop(PreInfo, Goals0, Goals, [], PostInfos, !Info),
        merge_post_branch_infos_into_one(PostInfos, PostInfo),
        set_post_branch_info(PostInfo, !Info)
    ).

:- pred ho_traverse_disj_loop(pre_branch_info::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    list(post_branch_info)::in, list(post_branch_info)::out,
    higher_order_info::in, higher_order_info::out) is det.

ho_traverse_disj_loop(_, [], [], !PostInfos, !Info).
ho_traverse_disj_loop(PreInfo, [Goal0 | Goals0], [Goal | Goals],
        !PostInfos, !Info) :-
    set_pre_branch_info(PreInfo, !Info),
    ho_traverse_goal(Goal0, Goal, !Info),
    get_post_branch_info_for_goal(!.Info, Goal, GoalPostInfo),
    !:PostInfos = [GoalPostInfo | !.PostInfos],
    ho_traverse_disj_loop(PreInfo, Goals0, Goals, !PostInfos, !Info).

    % Switches are treated in exactly the same way as disjunctions.
    %
:- pred ho_traverse_cases(list(case)::in, list(case)::out,
    higher_order_info::in, higher_order_info::out) is det.

ho_traverse_cases(Cases0, Cases, !Info) :-
    % We handle empty lists separately because merge_post_branch_infos_into_one
    % works only on nonempty lists.
    (
        Cases0 = [],
        unexpected($pred, "empty list of cases")
    ;
        Cases0 = [_ | _],
        get_pre_branch_info(!.Info, PreInfo),
        ho_traverse_cases_loop(PreInfo, Cases0, Cases, [], PostInfos, !Info),
        merge_post_branch_infos_into_one(PostInfos, PostInfo),
        set_post_branch_info(PostInfo, !Info)
    ).

:- pred ho_traverse_cases_loop(pre_branch_info::in,
    list(case)::in, list(case)::out,
    list(post_branch_info)::in, list(post_branch_info)::out,
    higher_order_info::in, higher_order_info::out) is det.

ho_traverse_cases_loop(_, [], [], !PostInfos, !Info).
ho_traverse_cases_loop(PreInfo, [Case0 | Cases0], [Case | Cases], !PostInfos,
        !Info) :-
    set_pre_branch_info(PreInfo, !Info),
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    ho_traverse_goal(Goal0, Goal, !Info),
    Case = case(MainConsId, OtherConsIds, Goal),
    get_post_branch_info_for_goal(!.Info, Goal, GoalPostInfo),
    !:PostInfos = [GoalPostInfo | !.PostInfos],
    ho_traverse_cases_loop(PreInfo, Cases0, Cases, !PostInfos, !Info).

%-----------------------------------------------------------------------------%

:- type pre_branch_info
    --->    pre_branch_info(known_var_map).

:- type reachability
    --->    reachable
    ;       unreachable.

:- type post_branch_info
    --->    post_branch_info(known_var_map, reachability).

:- pred get_pre_branch_info(higher_order_info::in, pre_branch_info::out)
    is det.

get_pre_branch_info(Info, pre_branch_info(Info ^ hoi_known_var_map)).

:- pred set_pre_branch_info(pre_branch_info::in,
    higher_order_info::in, higher_order_info::out) is det.

set_pre_branch_info(pre_branch_info(KnownVarMap), !Info) :-
    !Info ^ hoi_known_var_map := KnownVarMap.

:- pred get_post_branch_info_for_goal(higher_order_info::in, hlds_goal::in,
    post_branch_info::out) is det.

get_post_branch_info_for_goal(HOInfo, Goal, PostBranchInfo) :-
    InstMapDelta = goal_info_get_instmap_delta(Goal ^ hg_info),
    ( if instmap_delta_is_reachable(InstMapDelta) then
        Reachability = reachable
    else
        Reachability = unreachable
    ),
    PostBranchInfo =
        post_branch_info(HOInfo ^ hoi_known_var_map, Reachability).

:- pred set_post_branch_info(post_branch_info::in,
    higher_order_info::in, higher_order_info::out) is det.

set_post_branch_info(post_branch_info(KnownVarMap, _), !Info) :-
    !Info ^ hoi_known_var_map := KnownVarMap.

    % Merge a bunch of post_branch_infos into one.
    %
:- pred merge_post_branch_infos_into_one(list(post_branch_info)::in,
    post_branch_info::out) is det.

merge_post_branch_infos_into_one(PostInfos, MergedPostInfo) :-
    (
        PostInfos = [],
        unexpected($pred, "PostInfos = []")
    ;
        PostInfos = [_ | _],
        IsReachable =
            ( pred(PostInfo::in, VarMap::out) is semidet :-
                PostInfo = post_branch_info(VarMap, reachable)
            ),
        list.filter_map(IsReachable, PostInfos, ReachableVarMaps),
        (
            ReachableVarMaps = [],
            MergedPostInfo = post_branch_info(map.init, unreachable)
        ;
            ReachableVarMaps = [HeadVarMap | TailVarMaps],
            merge_post_branch_var_maps_passes(HeadVarMap, TailVarMaps,
                MergedVarMap),
            MergedPostInfo = post_branch_info(MergedVarMap, reachable)
        )
    ).

:- pred merge_post_branch_var_maps_passes(known_var_map::in,
    list(known_var_map)::in, known_var_map::out) is det.

merge_post_branch_var_maps_passes(VarMap1, VarMaps2Plus, MergedVarMap) :-
    merge_post_branch_var_maps_pass(VarMap1, VarMaps2Plus,
        HeadMergedVarMap, TailMergedVarMaps),
    (
        TailMergedVarMaps = [],
        MergedVarMap = HeadMergedVarMap
    ;
        TailMergedVarMaps = [_ | _],
        merge_post_branch_var_maps_passes(HeadMergedVarMap, TailMergedVarMaps,
            MergedVarMap)
    ).

:- pred merge_post_branch_var_maps_pass(known_var_map::in,
    list(known_var_map)::in,
    known_var_map::out, list(known_var_map)::out) is det.

merge_post_branch_var_maps_pass(VarMap1, VarMaps2Plus,
        HeadMergedVarMap, TailMergedVarMaps) :-
    (
        VarMaps2Plus = [],
        HeadMergedVarMap = VarMap1,
        TailMergedVarMaps = []
    ;
        VarMaps2Plus = [VarMap2 | VarMaps3Plus],
        merge_post_branch_known_var_maps(VarMap1, VarMap2, HeadMergedVarMap),
        (
            VarMaps3Plus = [],
            TailMergedVarMaps = []
        ;
            VarMaps3Plus = [VarMap3 | VarMaps4Plus],
            merge_post_branch_var_maps_pass(VarMap3, VarMaps4Plus,
                HeadTailMergedVarMap, TailTailMergedVarMaps),
            TailMergedVarMaps = [HeadTailMergedVarMap | TailTailMergedVarMaps]
        )
    ).

    % Merge two the known_var_maps of post_branch_infos.
    %
    % If a variable appears in one post_branch_info, but not the other,
    % it is dropped. Such a variable is either local to the branch arm,
    % in which case no subsequent specialization opportunities exist,
    % or it does not have a unique constant value in one of the branch arms,
    % so we can't specialize it outside the branch anyway. A third possibility
    % is that the branch without the variable is unreachable. In that case
    % we include the variable in the result.
    %
:- pred merge_post_branch_known_var_maps(known_var_map::in,
    known_var_map::in, known_var_map::out) is det.

merge_post_branch_known_var_maps(VarConstMapA, VarConstMapB, VarConstMapAB) :-
    map.keys_as_set(VarConstMapA, VarsA),
    map.keys_as_set(VarConstMapB, VarsB),
    set.intersect(VarsA, VarsB, CommonVars),
    VarConstCommonMapA = map.select(VarConstMapA, CommonVars),
    VarConstCommonMapB = map.select(VarConstMapB, CommonVars),
    map.to_assoc_list(VarConstCommonMapA, VarConstCommonListA),
    map.to_assoc_list(VarConstCommonMapB, VarConstCommonListB),
    merge_common_var_const_list(VarConstCommonListA, VarConstCommonListB,
        [], VarConstCommonList),
    map.from_assoc_list(VarConstCommonList, VarConstMapAB).

:- pred merge_post_branch_infos(post_branch_info::in,
    post_branch_info::in, post_branch_info::out) is det.

merge_post_branch_infos(PostA, PostB, Post) :-
    (
        PostA = post_branch_info(VarConstMapA, reachable),
        PostB = post_branch_info(VarConstMapB, reachable),
        merge_post_branch_known_var_maps(VarConstMapA, VarConstMapB,
            VarConstMapAB),
        Post = post_branch_info(VarConstMapAB, reachable)
    ;
        PostA = post_branch_info(_, unreachable),
        PostB = post_branch_info(_, reachable),
        Post = PostB
    ;
        PostA = post_branch_info(_, reachable),
        PostB = post_branch_info(_, unreachable),
        Post = PostA
    ;
        PostA = post_branch_info(_, unreachable),
        PostB = post_branch_info(_, unreachable),
        Post = post_branch_info(map.init, unreachable)
    ).

:- pred merge_common_var_const_list(assoc_list(prog_var, known_const)::in,
    assoc_list(prog_var, known_const)::in,
    assoc_list(prog_var, known_const)::in,
    assoc_list(prog_var, known_const)::out) is det.

merge_common_var_const_list([], [], !List).
merge_common_var_const_list([], [_ | _], !MergedList) :-
    unexpected($pred, "mismatched list").
merge_common_var_const_list([_ | _], [], !MergedList) :-
    unexpected($pred, "mismatched list").
merge_common_var_const_list([VarA - ValueA | ListA], [VarB - ValueB | ListB],
        !MergedList) :-
    expect(unify(VarA, VarB), $pred, "var mismatch"),
    ( if ValueA = ValueB then
        !:MergedList = [VarA - ValueA | !.MergedList]
    else
        !:MergedList = !.MergedList
    ),
    merge_common_var_const_list(ListA, ListB, !MergedList).

%-----------------------------------------------------------------------------%

:- pred check_unify(unification::in,
    higher_order_info::in, higher_order_info::out) is det.

check_unify(Unification, !Info) :-
    (
        Unification = simple_test(_, _)
        % Testing two higher order terms for equality is not allowed.
    ;
        Unification = assign(Var1, Var2),
        maybe_add_alias(Var1, Var2, !Info)
    ;
        Unification = deconstruct(_, _, _, _, _, _)
        % Deconstructing a higher order term is not allowed.
    ;
        Unification = construct(LVar, ConsId, Args, _Modes, _, _, _),
        Params = !.Info ^ hoi_global_info ^ hogi_params,
        IsInteresting = is_interesting_cons_id(Params, ConsId),
        (
            IsInteresting = yes,
            KnownVarMap0 = !.Info ^ hoi_known_var_map,
            % A variable cannot be constructed twice.
            map.det_insert(LVar, known_const(ConsId, Args),
                KnownVarMap0, KnownVarMap),
            !Info ^ hoi_known_var_map := KnownVarMap
        ;
            IsInteresting = no
        )
    ;
        Unification = complicated_unify(_, _, _),
        unexpected($pred, "complicated unification")
    ).

:- func is_interesting_cons_id(ho_params, cons_id) = bool.

is_interesting_cons_id(Params, ConsId) = IsInteresting :-
    (
        ( ConsId = cons(_, _, _)
        ; ConsId = tuple_cons(_)
        ; ConsId = float_const(_)
        ; ConsId = char_const(_)
        ; ConsId = string_const(_)
        ; ConsId = impl_defined_const(_)
        ; ConsId = ground_term_const(_, _)
        ; ConsId = tabling_info_const(_)
        ; ConsId = deep_profiling_proc_layout(_)
        ; ConsId = table_io_entry_desc(_)
        ),
        IsInteresting = no
    ;
        ConsId = some_int_const(IntConst),
        (
            ( IntConst = uint_const(_)
            ; IntConst = int8_const(_)
            ; IntConst = uint8_const(_)
            ; IntConst = int16_const(_)
            ; IntConst = uint16_const(_)
            ; IntConst = int32_const(_)
            ; IntConst = uint32_const(_)
            ; IntConst = int64_const(_)
            ; IntConst = uint64_const(_)
            ),
            IsInteresting = no
        ;
            % We need to keep track of int_consts so we can interpret
            % calls to the builtins superclass_info_from_typeclass_info and
            % typeinfo_from_typeclass_info. We do not specialize based on
            % integers alone.
            IntConst = int_const(_),
            UserTypeSpec = Params ^ param_do_user_type_spec,
            (
                UserTypeSpec = spec_types_user_guided,
                IsInteresting = yes
            ;
                UserTypeSpec = do_not_spec_types_user_guided,
                IsInteresting = no
            )
        )
    ;
        ( ConsId = type_ctor_info_const(_, _, _)
        ; ConsId = base_typeclass_info_const(_, _, _, _)
        ; ConsId = type_info_cell_constructor(_)
        ; ConsId = typeclass_info_cell_constructor
        ; ConsId = type_info_const(_)
        ; ConsId = typeclass_info_const(_)
        ),
        UserTypeSpec = Params ^ param_do_user_type_spec,
        (
            UserTypeSpec = spec_types_user_guided,
            IsInteresting = yes
        ;
            UserTypeSpec = do_not_spec_types_user_guided,
            IsInteresting = no
        )
    ;
        ConsId = closure_cons(_, _),
        HigherOrder = Params ^ param_do_higher_order_spec,
        (
            HigherOrder = opt_higher_order,
            IsInteresting = yes
        ;
            HigherOrder = do_not_opt_higher_order,
            IsInteresting = no
        )
    ).

    % Process a higher-order call to see if it could possibly be specialized.
    %
:- pred maybe_specialize_higher_order_call(prog_var::in,
    list(prog_var)::in, hlds_goal::in, hlds_goal::out,
    higher_order_info::in, higher_order_info::out) is det.

maybe_specialize_higher_order_call(PredVar, Args, Goal0, Goal, !Info) :-
    % We can specialize calls to call/N if the closure has a known value.
    ( if
        map.search(!.Info ^ hoi_known_var_map, PredVar,
            known_const(ConsId, CurriedArgs)),
        ConsId = closure_cons(ShroudedPredProcId, _)
    then
        proc(PredId, ProcId) = unshroud_pred_proc_id(ShroudedPredProcId),
        AllArgs = CurriedArgs ++ Args,
        Goal0 = hlds_goal(_, GoalInfo),
        construct_specialized_higher_order_call(PredId, ProcId, AllArgs,
            GoalInfo, Goal, !Info)
    else
        % Non-specializable call/N.
        Goal = Goal0
    ).

    % Process a class_method_call to see if it could possibly be specialized.
    %
:- pred maybe_specialize_method_call(prog_var::in, int::in,
    list(prog_var)::in, hlds_goal::in, hlds_goal::out,
    higher_order_info::in, higher_order_info::out) is det.

maybe_specialize_method_call(TypeClassInfoVar, Method, Args, Goal0, Goal,
        !Info) :-
    Goal0 = hlds_goal(_GoalExpr0, GoalInfo0),
    ModuleInfo = !.Info ^ hoi_global_info ^ hogi_module_info,
    % We can specialize calls to class_method_call/N if the typeclass_info
    % has a known value.
    ( if
        % XXX We could duplicate this code, replacing the tests of
        % ConsId and BaseConsId with equivalent tests on const_structs.
        % However, how would we compute an equivalent of
        % InstanceConstraintArgs?
        map.search(!.Info ^ hoi_known_var_map, TypeClassInfoVar,
            known_const(ConsId, TCIArgs)),
        % A typeclass_info variable should consist of a known
        % base_typeclass_info and some argument typeclass_infos.
        ConsId = typeclass_info_cell_constructor,
        TCIArgs = [BaseTypeClassInfo | OtherTypeClassInfoArgs],
        map.search(!.Info ^ hoi_known_var_map, BaseTypeClassInfo,
            known_const(BaseConsId, _)),
        BaseConsId = base_typeclass_info_const(_, ClassId, Instance, _),

        module_info_get_instance_table(ModuleInfo, InstanceTable),
        map.lookup(InstanceTable, ClassId, InstanceList),
        list.det_index1(InstanceList, Instance, InstanceDefn),
        InstanceDefn = hlds_instance_defn(_, InstanceTypes0, _, _, _,
            InstanceConstraints, _, yes(ClassInterface), _, _),
        type_vars_list(InstanceTypes0, InstanceTvars),
        get_unconstrained_tvars(InstanceTvars,
            InstanceConstraints, UnconstrainedTVars),
        NumArgsToExtract = list.length(InstanceConstraints)
            + list.length(UnconstrainedTVars),
        list.take(NumArgsToExtract, OtherTypeClassInfoArgs,
            InstanceConstraintArgs)
    then
        list.det_index1(ClassInterface, Method, proc(PredId, ProcId)),
        AllArgs = InstanceConstraintArgs ++ Args,
        construct_specialized_higher_order_call(PredId, ProcId, AllArgs,
            GoalInfo0, Goal, !Info)
    else if
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

        CallerProcInfo0 = !.Info ^ hoi_proc_info,
        CallerPredInfo0 = !.Info ^ hoi_pred_info,
        proc_info_get_rtti_varmaps(CallerProcInfo0, CallerRttiVarMaps),
        rtti_varmaps_var_info(CallerRttiVarMaps, TypeClassInfoVar,
            typeclass_info_var(ClassConstraint)),
        ClassConstraint = constraint(ClassName, ClassArgTypes),
        list.length(ClassArgTypes, ClassArity),
        module_info_get_instance_table(ModuleInfo, InstanceTable),
        map.lookup(InstanceTable, class_id(ClassName, ClassArity), Instances),
        pred_info_get_typevarset(CallerPredInfo0, TVarSet0),
        find_matching_instance_method(Instances, Method, ClassArgTypes,
            PredId, ProcId, InstanceConstraints, UnconstrainedTVarTypes,
            TVarSet0, TVarSet)
    then
        pred_info_set_typevarset(TVarSet, CallerPredInfo0, CallerPredInfo),
        % Pull out the argument typeclass_infos.
        ( if
            InstanceConstraints = [],
            UnconstrainedTVarTypes = []
        then
            ExtraGoals = [],
            CallerProcInfo = CallerProcInfo0,
            AllArgs = Args
        else
            get_unconstrained_instance_type_infos(ModuleInfo,
                TypeClassInfoVar, UnconstrainedTVarTypes, 1,
                ArgTypeInfoGoals, ArgTypeInfoVars,
                CallerProcInfo0, CallerProcInfo1),
            FirstArgTypeclassInfo = list.length(UnconstrainedTVarTypes) + 1,
            get_arg_typeclass_infos(ModuleInfo, TypeClassInfoVar,
                InstanceConstraints, FirstArgTypeclassInfo,
                ArgTypeClassInfoGoals, ArgTypeClassInfoVars,
                CallerProcInfo1, CallerProcInfo),
            list.condense([ArgTypeInfoVars, ArgTypeClassInfoVars, Args],
                AllArgs),
            ExtraGoals = ArgTypeInfoGoals ++ ArgTypeClassInfoGoals
        ),
        !Info ^ hoi_pred_info := CallerPredInfo,
        !Info ^ hoi_proc_info := CallerProcInfo,
        construct_specialized_higher_order_call(PredId, ProcId,
            AllArgs, GoalInfo0, SpecGoal, !Info),
        conj_list_to_goal(ExtraGoals ++ [SpecGoal], GoalInfo0, Goal)
    else
        % Non-specializable class_method_call/N.
        Goal = Goal0
    ).

:- pred find_matching_instance_method(list(hlds_instance_defn)::in, int::in,
    list(mer_type)::in, pred_id::out, proc_id::out,
    list(prog_constraint)::out, list(mer_type)::out,
    tvarset::in, tvarset::out) is semidet.

find_matching_instance_method([Instance | Instances], MethodNum, ClassTypes,
        PredId, ProcId, Constraints, UnconstrainedTVarTypes, !TVarSet) :-
    ( if
        instance_matches(ClassTypes, Instance, Constraints0,
            UnconstrainedTVarTypes0, !TVarSet)
    then
        Constraints = Constraints0,
        UnconstrainedTVarTypes = UnconstrainedTVarTypes0,
        yes(MethodPredProcIds) = Instance ^ instdefn_maybe_method_ppids,
        list.det_index1(MethodPredProcIds, MethodNum, proc(PredId, ProcId))
    else
        find_matching_instance_method(Instances, MethodNum, ClassTypes,
            PredId, ProcId, Constraints, UnconstrainedTVarTypes, !TVarSet)
    ).

:- pred instance_matches(list(mer_type)::in, hlds_instance_defn::in,
    list(prog_constraint)::out, list(mer_type)::out,
    tvarset::in, tvarset::out) is semidet.

instance_matches(ClassTypes, Instance, Constraints, UnconstrainedTVarTypes,
        TVarSet0, TVarSet) :-
    Instance = hlds_instance_defn(_, InstanceTypes0, _, _, _, Constraints0,
        _, _, InstanceTVarSet, _),
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
    MakeResultType = (func(_) = typeclass_info_type),
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
    MakeResultType = build_type_info_type,
    get_typeclass_info_args(ModuleInfo, TypeClassInfoVar,
        "unconstrained_type_info_from_typeclass_info",
        MakeResultType, UnconstrainedTVarTypes,
        Index, Goals, Vars, !ProcInfo).

:- pred get_typeclass_info_args(module_info::in, prog_var::in, string::in,
    (func(T) = mer_type)::in, list(T)::in, int::in, list(hlds_goal)::out,
    list(prog_var)::out, proc_info::in, proc_info::out) is det.

get_typeclass_info_args(ModuleInfo, TypeClassInfoVar, PredName, MakeResultType,
        Args, Index, Goals, Vars, !ProcInfo) :-
    lookup_builtin_pred_proc_id(ModuleInfo, mercury_private_builtin_module,
        PredName, pf_predicate, user_arity(3), only_mode, ExtractArgPredId,
        ExtractArgProcId),
    get_typeclass_info_args_loop(TypeClassInfoVar,
        ExtractArgPredId, ExtractArgProcId,
        qualified(mercury_private_builtin_module, PredName),
        MakeResultType, Args, Index, Goals, Vars, !ProcInfo).

:- pred get_typeclass_info_args_loop(prog_var::in, pred_id::in, proc_id::in,
    sym_name::in, (func(T) = mer_type)::in,
    list(T)::in, int::in, list(hlds_goal)::out,
    list(prog_var)::out, proc_info::in, proc_info::out) is det.

get_typeclass_info_args_loop(_, _, _, _, _, [], _, [], [], !ProcInfo).
get_typeclass_info_args_loop(TypeClassInfoVar, PredId, ProcId, SymName,
        MakeResultType, [Arg | Args], Index, [IndexGoal, CallGoal | Goals],
        [ResultVar | Vars], !ProcInfo) :-
    ResultType = MakeResultType(Arg),
    proc_info_create_var_from_type(ResultType, no, ResultVar, !ProcInfo),
    MaybeContext = no,
    make_int_const_construction_alloc_in_proc(Index, no, IndexGoal, IndexVar,
        !ProcInfo),
    CallArgs = [TypeClassInfoVar, IndexVar, ResultVar],

    set_of_var.list_to_set(CallArgs, NonLocals),
    instmap_delta_init_reachable(InstMapDelta0),
    instmap_delta_insert_var(ResultVar, ground(shared, none_or_default_func),
        InstMapDelta0, InstMapDelta),
    goal_info_init(NonLocals, InstMapDelta, detism_det, purity_pure, GoalInfo),
    CallGoalExpr = plain_call(PredId, ProcId, CallArgs, not_builtin,
        MaybeContext, SymName),
    CallGoal = hlds_goal(CallGoalExpr, GoalInfo),
    get_typeclass_info_args_loop(TypeClassInfoVar, PredId, ProcId, SymName,
        MakeResultType, Args, Index + 1, Goals, Vars, !ProcInfo).

%-----------------------------------------------------------------------------%

:- pred construct_specialized_higher_order_call(pred_id::in, proc_id::in,
    list(prog_var)::in, hlds_goal_info::in, hlds_goal::out,
    higher_order_info::in, higher_order_info::out) is det.

construct_specialized_higher_order_call(PredId, ProcId, AllArgs, GoalInfo,
        hlds_goal(GoalExpr, GoalInfo), !Info) :-
    ModuleInfo = !.Info ^ hoi_global_info ^ hogi_module_info,
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ModuleName = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    SymName = qualified(ModuleName, PredName),
    proc(CallerPredId, _) = !.Info ^ hoi_pred_proc_id,
    Builtin = builtin_state(ModuleInfo, CallerPredId, PredId, ProcId),

    MaybeContext = no,
    GoalExpr1 = plain_call(PredId, ProcId, AllArgs, Builtin, MaybeContext,
        SymName),
    !Info ^ hoi_changed := ho_changed,
    maybe_specialize_call(hlds_goal(GoalExpr1, GoalInfo),
        hlds_goal(GoalExpr, _), !Info).

:- pred maybe_specialize_call(hlds_goal::in(goal_plain_call), hlds_goal::out,
    higher_order_info::in, higher_order_info::out) is det.

maybe_specialize_call(hlds_goal(GoalExpr0, GoalInfo),
        hlds_goal(GoalExpr, GoalInfo), !Info) :-
    ModuleInfo0 = !.Info ^ hoi_global_info ^ hogi_module_info,
    GoalExpr0 = plain_call(CalledPred, CalledProc, Args0, IsBuiltin,
        MaybeContext, _SymName0),
    module_info_pred_proc_info(ModuleInfo0, CalledPred, CalledProc,
        CalleePredInfo, CalleeProcInfo),
    ( if
        % Look for calls to unify/2 and compare/3 that can be specialized.
        specialize_special_pred(CalledPred, CalledProc, Args0,
            MaybeContext, GoalInfo, GoalExpr1, !Info)
    then
        GoalExpr = GoalExpr1,
        !Info ^ hoi_changed := ho_changed
    else if
        is_typeclass_info_manipulator(ModuleInfo0, CalledPred, Manipulator)
    then
        interpret_typeclass_info_manipulator(Manipulator, Args0,
            GoalExpr0, GoalExpr, !Info)
    else if
        (
            pred_info_is_imported(CalleePredInfo),
            module_info_get_type_spec_info(ModuleInfo0, TypeSpecInfo),
            TypeSpecInfo = type_spec_info(TypeSpecProcs, _, _, _),
            not set.member(proc(CalledPred, CalledProc), TypeSpecProcs)
        ;
            pred_info_is_pseudo_imported(CalleePredInfo),
            hlds_pred.in_in_unification_proc_id(CalledProc)
        ;
            pred_info_defn_has_foreign_proc(CalleePredInfo)
        )
    then
        GoalExpr = GoalExpr0
    else
        maybe_specialize_ordinary_call(can_request, CalledPred, CalledProc,
            CalleePredInfo, CalleeProcInfo, Args0, IsBuiltin, MaybeContext,
            GoalInfo, Result, !Info),
        (
            Result = specialized(ExtraTypeInfoGoals, GoalExpr1),
            goal_to_conj_list(hlds_goal(GoalExpr1, GoalInfo), GoalList1),
            GoalList = ExtraTypeInfoGoals ++ GoalList1,
            GoalExpr = conj(plain_conj, GoalList)
        ;
            Result = not_specialized,
            GoalExpr = GoalExpr0
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

maybe_specialize_pred_const(hlds_goal(GoalExpr0, GoalInfo),
        hlds_goal(GoalExpr, GoalInfo), !Info) :-
    NewPredMap = !.Info ^ hoi_global_info ^ hogi_new_pred_map,
    ModuleInfo = !.Info ^ hoi_global_info ^ hogi_module_info,
    ProcInfo0  = !.Info ^ hoi_proc_info,
    ( if
        GoalExpr0 = unify(_, _, UniMode, Unify0, Context),
        Unify0 = construct(LVar, ConsId0, Args0, _,
            HowToConstruct, CellIsUnique, SubInfo),
        (
            SubInfo = no_construct_sub_info
        ;
            SubInfo = construct_sub_info(no, no)
        ),
        ConsId0 = closure_cons(ShroudedPredProcId, EvalMethod),
        PredProcId = unshroud_pred_proc_id(ShroudedPredProcId),
        proc(PredId, ProcId) = PredProcId,
        map.contains(NewPredMap, PredProcId),
        proc_info_get_vartypes(ProcInfo0, VarTypes0),
        lookup_var_type(VarTypes0, LVar, LVarType),
        type_is_higher_order_details(LVarType, _, _, _, ArgTypes)
    then
        proc_info_create_vars_from_types(ArgTypes, UncurriedArgs,
            ProcInfo0, ProcInfo1),
        Args1 = Args0 ++ UncurriedArgs,
        !Info ^ hoi_proc_info := ProcInfo1,

        module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
            CalleePredInfo, CalleeProcInfo),

        % We don't create requests for higher-order terms because that would
        % result in duplication of effort if all uses of the constant end up
        % being specialized. For parser combinator programs it would also
        % result in huge numbers of requests with no easy way to control which
        % ones should be created.

        IsBuiltin = not_builtin,
        MaybeContext = no,
        maybe_specialize_ordinary_call(can_not_request, PredId, ProcId,
            CalleePredInfo, CalleeProcInfo, Args1, IsBuiltin, MaybeContext,
            GoalInfo, Result, !Info),
        (
            Result = specialized(ExtraTypeInfoGoals0, GoalExpr1),
            ( if
                GoalExpr1 =
                    plain_call(NewPredId0, NewProcId0, NewArgs0, _, _, _),
                list.remove_suffix(NewArgs0, UncurriedArgs, NewArgs1)
            then
                NewPredId = NewPredId0,
                NewProcId = NewProcId0,
                NewArgs = NewArgs1
            else
                unexpected($pred, "cannot get NewArgs")
            ),

            module_info_proc_info(ModuleInfo, NewPredId, NewProcId,
                NewCalleeProcInfo),
            proc_info_get_argmodes(NewCalleeProcInfo, NewCalleeArgModes),
            ( if
                list.take(list.length(NewArgs), NewCalleeArgModes,
                    CurriedArgModesPrime)
            then
                CurriedArgModes = CurriedArgModesPrime
            else
                unexpected($pred, "cannot get CurriedArgModes")
            ),
            ArgModes = list.map(mode_both_sides_to_unify_mode(ModuleInfo),
                CurriedArgModes),

            % The dummy arguments can't be used anywhere.
            ProcInfo2 = !.Info ^ hoi_proc_info,
            proc_info_get_vartypes(ProcInfo2, VarTypes2),
            delete_var_types(UncurriedArgs, VarTypes2, VarTypes),
            proc_info_set_vartypes(VarTypes, ProcInfo2, ProcInfo),
            !Info ^ hoi_proc_info := ProcInfo,

            NewPredProcId = proc(NewPredId, NewProcId),
            NewShroudedPredProcId = shroud_pred_proc_id(NewPredProcId),
            NewConsId = closure_cons(NewShroudedPredProcId, EvalMethod),
            Unify = construct(LVar, NewConsId, NewArgs, ArgModes,
                HowToConstruct, CellIsUnique, no_construct_sub_info),
            GoalExpr2 = unify(LVar,
                rhs_functor(NewConsId, is_not_exist_constr, NewArgs),
                UniMode, Unify, Context),

            % Make sure any constants in the ExtraTypeInfoGoals are recorded.
            list.map_foldl(ho_traverse_goal, ExtraTypeInfoGoals0,
                ExtraTypeInfoGoals, !Info),
            (
                ExtraTypeInfoGoals = [],
                GoalExpr = GoalExpr2
            ;
                ExtraTypeInfoGoals = [_ | _],
                GoalExpr = conj(plain_conj,
                    ExtraTypeInfoGoals ++ [hlds_goal(GoalExpr2, GoalInfo)])
            )
        ;
            Result = not_specialized,
            % The dummy arguments can't be used anywhere.
            !Info ^ hoi_proc_info := ProcInfo0,
            GoalExpr = GoalExpr0
        )
    else
        GoalExpr = GoalExpr0
    ).

:- type specialization_result
    --->    specialized(
                % Goals to construct extra type-infos.
                list(hlds_goal),

                % The specialized call.
                hlds_goal_expr
            )
    ;       not_specialized.

:- type can_request
    --->    can_request
    ;       can_not_request.

:- pred maybe_specialize_ordinary_call(can_request::in,
    pred_id::in, proc_id::in, pred_info::in, proc_info::in,
    list(prog_var)::in, builtin_state::in, maybe(call_unify_context)::in,
    hlds_goal_info::in, specialization_result::out,
    higher_order_info::in, higher_order_info::out) is det.

maybe_specialize_ordinary_call(CanRequest, CalledPred, CalledProc,
        CalleePredInfo, CalleeProcInfo, Args0, IsBuiltin,
        MaybeContext, GoalInfo, Result, !Info) :-
    ModuleInfo0 = !.Info ^ hoi_global_info ^ hogi_module_info,
    pred_info_get_status(CalleePredInfo, CalleeStatus),
    proc_info_get_vartypes(CalleeProcInfo, CalleeVarTypes),
    proc_info_get_headvars(CalleeProcInfo, CalleeHeadVars),
    lookup_var_types(CalleeVarTypes, CalleeHeadVars, CalleeArgTypes),

    CallerProcInfo0 = !.Info ^ hoi_proc_info,
    proc_info_get_vartypes(CallerProcInfo0, VarTypes),
    proc_info_get_rtti_varmaps(CallerProcInfo0, RttiVarMaps),
    find_higher_order_args(ModuleInfo0, CalleeStatus, Args0,
        CalleeArgTypes, VarTypes, RttiVarMaps, !.Info ^ hoi_known_var_map, 1,
        [], HigherOrderArgs0),

    proc(CallerPredId, _) = !.Info ^ hoi_pred_proc_id,
    module_info_get_type_spec_info(ModuleInfo0, TypeSpecInfo),
    TypeSpecInfo = type_spec_info(_, ForceVersions, _, _),
    set.is_member(CallerPredId, ForceVersions, IsUserSpecProc),
    ( if
        (
            HigherOrderArgs0 = [_ | _]
        ;
            % We should create these even if there is no specialization
            % to avoid link errors.
            IsUserSpecProc = yes
        ;
            !.Info ^ hoi_global_info ^ hogi_params ^ param_do_user_type_spec
                = spec_types_user_guided,
            lookup_var_types(VarTypes, Args0, ArgTypes),

            % Check whether any typeclass constraints now match an instance.
            pred_info_get_class_context(CalleePredInfo, CalleeClassContext),
            CalleeClassContext = constraints(CalleeUnivConstraints0, _),
            pred_info_get_typevarset(CalleePredInfo, CalleeTVarSet),
            pred_info_get_exist_quant_tvars(CalleePredInfo, CalleeExistQTVars),
            CallerPredInfo0 = !.Info ^ hoi_pred_info,
            pred_info_get_typevarset(CallerPredInfo0, TVarSet),
            pred_info_get_univ_quant_tvars(CallerPredInfo0, CallerUnivQTVars),
            type_subst_makes_instance_known(ModuleInfo0,
                CalleeUnivConstraints0, TVarSet,
                CallerUnivQTVars, ArgTypes, CalleeTVarSet,
                CalleeExistQTVars, CalleeArgTypes)
        )
    then
        list.reverse(HigherOrderArgs0, HigherOrderArgs),
        Context = goal_info_get_context(GoalInfo),
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

            Args = ExtraTypeInfoVars ++ Args1,
            CallGoal = plain_call(NewCalledPred, NewCalledProc, Args,
                IsBuiltin, MaybeContext, NewName),
            Result = specialized(ExtraTypeInfoGoals, CallGoal),
            !Info ^ hoi_changed := ho_changed
        ;
            % There is a known higher order variable in the call, so we
            % put in a request for a specialized version of the pred.
            FindResult = find_result_request(Request),
            Result = not_specialized,
            (
                CanRequest = can_request,
                Requests0 = !.Info ^ hoi_global_info ^ hogi_requests,
                Changed0 = !.Info ^ hoi_changed,
                set.insert(Request, Requests0, Requests),
                update_changed_status(Changed0, ho_request, Changed),
                !Info ^ hoi_global_info ^ hogi_requests := Requests,
                !Info ^ hoi_changed := Changed
            ;
                CanRequest = can_not_request
            )
        ;
            FindResult = find_result_no_request,
            Result = not_specialized
        )
    else
        Result = not_specialized
    ).

    % Returns a list of the higher-order arguments in a call that have
    % a known value.
    %
:- pred find_higher_order_args(module_info::in, pred_status::in,
    list(prog_var)::in, list(mer_type)::in, vartypes::in,
    rtti_varmaps::in, known_var_map::in, int::in, list(higher_order_arg)::in,
    list(higher_order_arg)::out) is det.

find_higher_order_args(_, _, [], _, _, _, _, _, !HOArgs).
find_higher_order_args(_, _, [_ | _], [], _, _, _, _, _, _) :-
    unexpected($pred, "length mismatch").
find_higher_order_args(ModuleInfo, CalleeStatus, [Arg | Args],
        [CalleeArgType | CalleeArgTypes], VarTypes, RttiVarMaps,
        KnownVarMap, ArgNo, !HOArgs) :-
    NextArg = ArgNo + 1,
    ( if
        % We don't specialize arguments whose declared type is polymorphic.
        % The closure they pass cannot possibly be called within the called
        % predicate, since that predicate doesn't know it is a closure
        % (without some dodgy use of type_to_univ and univ_to_type).
        map.search(KnownVarMap, Arg, known_const(ConsId, CurriedArgs)),

        % We don't specialize based on int_consts (we only keep track of them
        % to interpret calls to the procedures which extract fields from
        % typeclass_infos).
        ConsId \= some_int_const(int_const(_)),

        ( if ConsId = closure_cons(_, _) then
            % If we don't have clauses for the callee, we can't specialize
            % any higher-order arguments. We may be able to do user guided
            % type specialization.
            CalleeStatus \= pred_status(status_imported(_)),
            CalleeStatus \= pred_status(status_external(_)),
            type_is_higher_order(CalleeArgType)
        else
            true
        )
    then
        % Find any known higher-order arguments in the list of curried
        % arguments.
        lookup_var_types(VarTypes, CurriedArgs, CurriedArgTypes),
        list.map(rtti_varmaps_var_info(RttiVarMaps), CurriedArgs,
            CurriedArgRttiInfo),
        ( if ConsId = closure_cons(ShroudedPredProcId, _) then
            proc(PredId, _) = unshroud_pred_proc_id(ShroudedPredProcId),
            module_info_pred_info(ModuleInfo, PredId, PredInfo),
            pred_info_get_arg_types(PredInfo, CurriedCalleeArgTypes)
        else
            CurriedCalleeArgTypes = CurriedArgTypes
        ),
        find_higher_order_args(ModuleInfo, CalleeStatus, CurriedArgs,
            CurriedCalleeArgTypes, VarTypes, RttiVarMaps,
            KnownVarMap, 1, [], HOCurriedArgs0),
        list.reverse(HOCurriedArgs0, HOCurriedArgs),
        list.length(CurriedArgs, NumArgs),
        ( if
            NumArgs = list.length(HOCurriedArgs),
            not (
                list.member(HOCurriedArg, HOCurriedArgs),
                HOCurriedArg ^ hoa_is_constant = no
            )
        then
            IsConst = yes
        else
            IsConst = no
        ),
        HOArg = higher_order_arg(ConsId, ArgNo, NumArgs,
            CurriedArgs, CurriedArgTypes, CurriedArgRttiInfo,
            HOCurriedArgs, IsConst),
        list.cons(HOArg, !HOArgs)
    else
        true
    ),
    find_higher_order_args(ModuleInfo, CalleeStatus, Args, CalleeArgTypes,
        VarTypes, RttiVarMaps, KnownVarMap, NextArg, !HOArgs).

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
    compute_caller_callee_type_substitution(CalleeArgTypes1, ArgTypes,
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
    CalleeUnivConstraint0 = constraint(ClassName, ConstraintArgTypes0),
    list.length(ConstraintArgTypes0, ClassArity),
    CalleeUnivConstraint = constraint(_ClassName, ConstraintArgTypes),
    module_info_get_instance_table(ModuleInfo, InstanceTable),
    map.search(InstanceTable, class_id(ClassName, ClassArity), Instances),
    list.member(Instance, Instances),
    instance_matches(ConstraintArgTypes, Instance, _, _, TVarSet, _),
    not instance_matches(ConstraintArgTypes0, Instance, _, _, TVarSet, _).

:- type find_result
    --->    find_result_match(match)
    ;       find_result_request(ho_request)
    ;       find_result_no_request.

:- type match
    --->    match(
                new_pred,

                % Was the match partial, if so, how many higher_order arguments
                % matched.
                maybe(int),

                % The arguments to the specialised call.
                list(prog_var),

                % Type variables for which extra type-infos must be added
                % to the start of the argument list.
                list(mer_type)
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

    ModuleInfo = Info ^ hoi_global_info ^ hogi_module_info,
    NewPredMap = Info ^ hoi_global_info ^ hogi_new_pred_map,
    Caller = Info ^ hoi_pred_proc_id,
    PredInfo = Info ^ hoi_pred_info,
    ProcInfo = Info ^ hoi_proc_info,
    Params = Info ^ hoi_global_info ^ hogi_params,

    % WARNING - do not filter out higher-order arguments after this step,
    % except when partially matching against a previously produced
    % specialization, otherwise some type-infos that the call
    % specialization code is expecting to come from the curried
    % arguments of the higher-order arguments will not be present in the
    % specialized argument list.

    get_extra_arguments(HigherOrderArgs, Args0, Args),
    compute_extra_typeinfos(Info, Args, ExtraTypeInfoTVars),

    proc_info_get_vartypes(ProcInfo, VarTypes),
    lookup_var_types(VarTypes, Args0, CallArgTypes),
    pred_info_get_typevarset(PredInfo, TVarSet),

    Request = ho_request(Caller, proc(CalledPred, CalledProc), Args0,
        ExtraTypeInfoTVars, HigherOrderArgs, CallArgTypes,
        TVarSet, yes, IsUserSpecProc, Context),

    % Check to see if any of the specialized versions of the called pred
    % apply here.
    ( if
        map.search(NewPredMap, proc(CalledPred, CalledProc), Versions0),
        set.to_sorted_list(Versions0, Versions),
        search_for_version(Info, Params, ModuleInfo, Request, Versions,
            no, Match)
    then
        Result = find_result_match(Match)
    else if
        HigherOrder = Params ^ param_do_higher_order_spec,
        TypeSpec = Params ^ param_do_type_spec,
        UserTypeSpec = Params ^ param_do_user_type_spec,
        (
            UserTypeSpec = spec_types_user_guided,
            IsUserSpecProc = yes
        ;
            module_info_pred_info(ModuleInfo, CalledPred, CalledPredInfo),
            not pred_info_is_imported(CalledPredInfo),
            (
                % This handles the predicates introduced by check_typeclass.m
                % to call the class methods for a specific instance. Without
                % this, user-specified specialized versions of class methods
                % won't be called.
                UserTypeSpec = spec_types_user_guided,
                pred_info_get_markers(CalledPredInfo, Markers),
                (
                    check_marker(Markers, marker_class_method)
                ;
                    check_marker(Markers, marker_class_instance_method)
                )
            ;
                HigherOrder = opt_higher_order,
                list.member(HOArg, HigherOrderArgs),
                HOArg ^ hoa_cons_id = closure_cons(_, _)
            ;
                TypeSpec = spec_types
            )
        )
    then
        Result = find_result_request(Request)
    else
        Result = find_result_no_request
    ).

    % Specializing type `T' to `list(U)' requires passing in the
    % typeinfo for `U'. This predicate works out which extra variables
    % to pass in given the argument list for the call. This needs to be done
    % even if --typeinfo-liveness is not set because the type-infos
    % may be needed when specializing calls inside the specialized version.
    %
:- pred compute_extra_typeinfos(higher_order_info::in,
    list(prog_var)::in, list(tvar)::out) is det.

compute_extra_typeinfos(Info, Args, ExtraTypeInfoTVars) :-
    % Work out which type variables don't already have type-infos in the
    % list of argument types. The list is in the order which the type
    % variables occur in the list of argument types so that the extra
    % type-info arguments for calls to imported user-guided type
    % specialization procedures can be matched against the specialized
    % version (`goal_util.extra_nonlocal_typeinfos' is not used here
    % because the type variables are returned sorted by variable number,
    % which will vary between calls).
    ProcInfo = Info ^ hoi_proc_info,
    proc_info_get_vartypes(ProcInfo, VarTypes),
    lookup_var_types(VarTypes, Args, ArgTypes),
    type_vars_list(ArgTypes, AllTVars),
    (
        AllTVars = [],
        ExtraTypeInfoTVars = []
    ;
        AllTVars = [_ | _],
        proc_info_get_rtti_varmaps(Info ^ hoi_proc_info, RttiVarMaps),
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
        ( if Type = type_variable(TVar, _) then
            !:TVars = [TVar | !.TVars]
        else
            true
        )
    ;
        VarInfo = typeclass_info_var(Constraint),
        Constraint = constraint(_ClassName, ClassArgTypes),
        % Find out what tvars the typeclass-info contains the type-infos for.
        list.filter_map(
            ( pred(ClassArgType::in, ClassTVar::out) is semidet :-
                ClassArgType = type_variable(ClassTVar, _)
            ), ClassArgTypes, ClassTVars),
        !:TVars = ClassTVars ++ !.TVars
    ;
        VarInfo = non_rtti_var
    ).

:- pred construct_extra_type_infos(list(mer_type)::in,
    list(prog_var)::out, list(hlds_goal)::out,
    higher_order_info::in, higher_order_info::out) is det.

construct_extra_type_infos(Types, TypeInfoVars, TypeInfoGoals, !Info) :-
    ModuleInfo0 = !.Info ^ hoi_global_info ^ hogi_module_info,
    PredInfo0 = !.Info ^ hoi_pred_info,
    ProcInfo0 = !.Info ^ hoi_proc_info,
    term.context_init(Context),
    polymorphism_make_type_info_vars_raw(Types, Context,
        TypeInfoVars, TypeInfoGoals, ModuleInfo0, ModuleInfo,
        PredInfo0, PredInfo, ProcInfo0, ProcInfo),
    !Info ^ hoi_pred_info := PredInfo,
    !Info ^ hoi_proc_info := ProcInfo,
    !Info ^ hoi_global_info ^ hogi_module_info := ModuleInfo.

:- pred search_for_version(higher_order_info::in, ho_params::in,
    module_info::in, ho_request::in, list(new_pred)::in,
    maybe(match)::in, match::out) is semidet.

search_for_version(_, _, _, _, [], yes(Match), Match).
search_for_version(Info, Params, ModuleInfo, Request, [Version | Versions],
        MaybeMatch0, Match) :-
    ( if version_matches(Params, ModuleInfo, Request, Version, Match1) then
        ( if
            Match1 = match(_, MatchIsPartial, _, _),
            MatchIsPartial = no
        then
            Match = Match1
        else
            (
                MaybeMatch0 = no,
                MaybeMatch2 = yes(Match1)
            ;
                MaybeMatch0 = yes(Match0),
                ( if
                    % Pick the best match.
                    Match0 = match(_, yes(NumMatches0), _, _),
                    Match1 = match(_, yes(NumMatches1), _, _)
                then
                    ( if NumMatches0 > NumMatches1 then
                        MaybeMatch2 = MaybeMatch0
                    else
                        MaybeMatch2 = yes(Match1)
                    )
                else
                    unexpected($pred, "comparison failed")
                )
            ),
            search_for_version(Info, Params, ModuleInfo, Request,
                Versions, MaybeMatch2, Match)
        )
    else
        search_for_version(Info, Params, ModuleInfo, Request,
            Versions, MaybeMatch0, Match)
    ).

    % Check whether the request has already been implemented by the new_pred,
    % maybe ordering the list of extra type_infos in the caller predicate
    % to match up with those in the caller.
    %
:- pred version_matches(ho_params::in, module_info::in, ho_request::in,
    new_pred::in, match::out) is semidet.

version_matches(Params, ModuleInfo, Request, Version, Match) :-
    Match = match(Version, PartialMatch, Args, ExtraTypeInfoTypes),
    Request = ho_request(_, Callee, Args0, _, RequestHigherOrderArgs,
        CallArgTypes, RequestTVarSet, _, _, _),
    Callee = proc(CalleePredId, _),
    module_info_pred_info(ModuleInfo, CalleePredId, CalleePredInfo),
    Version = new_pred(_, _, _, _, VersionHigherOrderArgs, _,
        VersionExtraTypeInfoTVars, VersionArgTypes0, VersionTVarSet, _, _),
    higher_order_args_match(RequestHigherOrderArgs,
        VersionHigherOrderArgs, HigherOrderArgs, FullOrPartial),
    (
        % Don't accept partial matches unless the predicate is imported
        % or we are only doing user-guided type specialization.
        FullOrPartial = match_is_partial,
        PartialMatch = no
    ;
        FullOrPartial = match_is_full,
        list.length(HigherOrderArgs, NumHOArgs),
        PartialMatch = yes(NumHOArgs),
        pred_info_get_markers(CalleePredInfo, Markers),

        % Always fully specialize calls to class methods.
        not check_marker(Markers, marker_class_method),
        not check_marker(Markers, marker_class_instance_method),
        (
            Params ^ param_do_type_spec = do_not_spec_types
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

:- type match_is_full
    --->    match_is_full
    ;       match_is_partial.

:- pred higher_order_args_match(list(higher_order_arg)::in,
    list(higher_order_arg)::in, list(higher_order_arg)::out,
    match_is_full::out) is semidet.

higher_order_args_match([], [], [], match_is_full).
higher_order_args_match(RequestArgs, [], [], match_is_partial) :-
    RequestArgs = [_ | _],
    not (
        list.member(RequestArg, RequestArgs),
        RequestConsId = RequestArg ^ hoa_cons_id,
        RequestConsId = closure_cons(_, _)
    ).
higher_order_args_match([RequestArg | RequestArgs], [VersionArg | VersionArgs],
        Args, FullOrPartial) :-
    RequestArg = higher_order_arg(ConsId1, ArgNo1, _, _, _, _, _,
        RequestIsConst),
    VersionArg = higher_order_arg(ConsId2, ArgNo2, _, _, _, _, _,
        VersionIsConst),

    ( if ArgNo1 = ArgNo2 then
        ConsId1 = ConsId2,
        RequestArg = higher_order_arg(_, _, NumArgs, CurriedArgs,
            CurriedArgTypes, CurriedArgRttiInfo, HOCurriedRequestArgs, _),
        VersionArg = higher_order_arg(_, _, NumArgs,
            _, _, _, HOCurriedVersionArgs, _),
        higher_order_args_match(HOCurriedRequestArgs, HOCurriedVersionArgs,
            NewHOCurriedArgs, FullOrPartial),
        higher_order_args_match(RequestArgs, VersionArgs, TailArgs, _),
        NewRequestArg = higher_order_arg(ConsId1, ArgNo1, NumArgs,
            CurriedArgs, CurriedArgTypes, CurriedArgRttiInfo,
            NewHOCurriedArgs, RequestIsConst `and` VersionIsConst),
        Args = [NewRequestArg | TailArgs]
    else
        % Type-info arguments present in the request may be missing from the
        % version if we are doing user-guided type specialization. All of the
        % arguments in the version must be present in the request for a match.
        ArgNo1 < ArgNo2,

        % All the higher-order arguments must be present in the version
        % otherwise we should create a new one.
        ConsId1 \= closure_cons(_, _),
        higher_order_args_match(RequestArgs, [VersionArg | VersionArgs],
            Args, _),
        FullOrPartial = match_is_partial
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
    KnownVarMap0 = !.Info ^ hoi_known_var_map,
    ( if map.search(KnownVarMap0, RVar, KnownConst) then
        map.det_insert(LVar, KnownConst, KnownVarMap0, KnownVarMap),
        !Info ^ hoi_known_var_map := KnownVarMap
    else
        true
    ).

:- pred update_changed_status(ho_changed::in, ho_changed::in, ho_changed::out)
    is det.

update_changed_status(ho_changed, _, ho_changed).
update_changed_status(ho_request, ho_changed, ho_changed).
update_changed_status(ho_request, ho_request, ho_request).
update_changed_status(ho_request, ho_unchanged, ho_request).
update_changed_status(ho_unchanged, Changed, Changed).

%-----------------------------------------------------------------------------%

:- type typeclass_info_manipulator
    --->    type_info_from_typeclass_info
    ;       superclass_from_typeclass_info
    ;       instance_constraint_from_typeclass_info.

    % Succeed if the predicate is one of the predicates defined in
    % library/private_builtin.m to extract type_infos or typeclass_infos
    % from typeclass_infos.
    %
:- pred is_typeclass_info_manipulator(module_info::in, pred_id::in,
    typeclass_info_manipulator::out) is semidet.

is_typeclass_info_manipulator(ModuleInfo, PredId, TypeClassManipulator) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    mercury_private_builtin_module = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    (
        PredName = "type_info_from_typeclass_info",
        TypeClassManipulator = type_info_from_typeclass_info
    ;
        PredName = "superclass_from_typeclass_info",
        TypeClassManipulator = superclass_from_typeclass_info
    ;
        PredName = "instance_constraint_from_typeclass_info",
        TypeClassManipulator = instance_constraint_from_typeclass_info
    ).

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
    ModuleInfo = !.Info ^ hoi_global_info ^ hogi_module_info,
    KnownVarMap0 = !.Info ^ hoi_known_var_map,
    ( if
        Args = [TypeClassInfoVar, IndexVar, OutputVar],
        map.search(KnownVarMap0, TypeClassInfoVar,
            known_const(TypeClassInfoConsId, TypeClassInfoArgs)),
        find_typeclass_info_components(ModuleInfo, KnownVarMap0,
            TypeClassInfoConsId, TypeClassInfoArgs,
            _ModuleName, ClassId, InstanceNum, _Instance, OtherArgs),

        map.search(KnownVarMap0, IndexVar, IndexMaybeConst),
        IndexMaybeConst = known_const(some_int_const(int_const(Index0)), [])
    then
        (
            ( Manipulator = type_info_from_typeclass_info
            ; Manipulator = superclass_from_typeclass_info
            ),
            % polymorphism.m adds MR_typeclass_info_num_extra_instance_args
            % to the index.
            module_info_get_instance_table(ModuleInfo, InstanceTable),
            map.lookup(InstanceTable, ClassId, InstanceDefns),
            list.det_index1(InstanceDefns, InstanceNum, InstanceDefn),
            num_extra_instance_args(InstanceDefn, NumExtra),
            Index = Index0 + NumExtra
        ;
            Manipulator = instance_constraint_from_typeclass_info,
            Index = Index0
        ),

        (
            OtherArgs = tci_arg_vars(OtherVars),
            list.det_index1(OtherVars, Index, SelectedArg),
            maybe_add_alias(OutputVar, SelectedArg, !Info),
            UnifyMode = unify_modes_li_lf_ri_rf(free, ground_inst,
                ground_inst, ground_inst),
            Unification = assign(OutputVar, SelectedArg),
            Goal = unify(OutputVar, rhs_var(SelectedArg), UnifyMode,
                Unification, unify_context(umc_explicit, [])),

            ProcInfo0 = !.Info ^ hoi_proc_info,
            proc_info_get_rtti_varmaps(ProcInfo0, RttiVarMaps0),
            rtti_var_info_duplicate_replace(SelectedArg, OutputVar,
                RttiVarMaps0, RttiVarMaps),
            proc_info_set_rtti_varmaps(RttiVarMaps, ProcInfo0, ProcInfo),
            !Info ^ hoi_proc_info := ProcInfo,

            % Sanity check.
            proc_info_get_vartypes(ProcInfo, VarTypes),
            lookup_var_type(VarTypes, OutputVar, OutputVarType),
            lookup_var_type(VarTypes, SelectedArg, SelectedArgType),
            ( if OutputVarType = SelectedArgType then
                true
            else
                unexpected($pred, "type mismatch")
            )
        ;
            OtherArgs = tci_arg_consts(OtherConstArgs),
            list.det_index1(OtherConstArgs, Index, SelectedConstArg),
            (
                SelectedConstArg = csa_constant(SelectedConsId, _),
                SelectedConstInst = bound(shared, inst_test_results_fgtc,
                    [bound_functor(SelectedConsId, [])])
            ;
                SelectedConstArg = csa_const_struct(SelectedConstNum),
                module_info_get_const_struct_db(ModuleInfo, ConstStructDb),
                lookup_const_struct_num(ConstStructDb, SelectedConstNum,
                    SelectedConstStruct),
                SelectedConstStruct = const_struct(SelectedConstConsId, _, _,
                    SelectedConstInst, _),
                ( if
                    ( SelectedConstConsId = type_info_cell_constructor(_)
                    ; SelectedConstConsId = type_info_const(_)
                    )
                then
                    SelectedConsId = type_info_const(SelectedConstNum)
                else if
                    ( SelectedConstConsId = typeclass_info_cell_constructor
                    ; SelectedConstConsId = typeclass_info_const(_)
                    )
                then
                    SelectedConsId = typeclass_info_const(SelectedConstNum)
                else
                    unexpected($pred, "bad SelectedConstStructConsId")
                )
            ),
            map.det_insert(OutputVar, known_const(SelectedConsId, []),
                KnownVarMap0, KnownVarMap),
            !Info ^ hoi_known_var_map := KnownVarMap,

            SelectedConsIdRHS =
                rhs_functor(SelectedConsId, is_not_exist_constr, []),
            UnifyMode = unify_modes_li_lf_ri_rf(free, SelectedConstInst,
                SelectedConstInst, SelectedConstInst),
            Unification = construct(OutputVar, SelectedConsId, [], [],
                construct_dynamically, cell_is_shared, no_construct_sub_info),
            Goal = unify(OutputVar, SelectedConsIdRHS, UnifyMode,
                Unification, unify_context(umc_explicit, []))
            % XXX do we need to update the rtti varmaps?
        ),
        !Info ^ hoi_changed := ho_changed
    else
        Goal = Goal0
    ).

:- type type_class_info_args
    --->    tci_arg_vars(list(prog_var))
    ;       tci_arg_consts(list(const_struct_arg)).

:- pred find_typeclass_info_components(module_info::in, known_var_map::in,
    cons_id::in, list(prog_var)::in,
    module_name::out, class_id::out, int::out, string::out,
    type_class_info_args::out) is semidet.

find_typeclass_info_components(ModuleInfo, KnownVarMap,
        TypeClassInfoConsId, TypeClassInfoArgs,
        ModuleName, ClassId, InstanceNum, Instance, Args) :-
    (
        TypeClassInfoConsId = typeclass_info_cell_constructor,
        % Extract the number of class constraints on the instance
        % from the base_typeclass_info.
        % If we have a variable for the base typeclass info,
        % it cannot be bound to a constant structure, since
        % as far as the HLDS is concerned, a base typeclass info
        % is just a bare cons_id, and not a structure that needs a cell
        % on the heap.
        TypeClassInfoArgs = [BaseTypeClassInfoVar | OtherVars],

        map.search(KnownVarMap, BaseTypeClassInfoVar,
            BaseTypeClassInfoMaybeConst),
        BaseTypeClassInfoMaybeConst = known_const(BaseTypeClassInfoConsId, _),
        Args = tci_arg_vars(OtherVars)
    ;
        TypeClassInfoConsId = typeclass_info_const(TCIConstNum),
        TypeClassInfoArgs = [],
        module_info_get_const_struct_db(ModuleInfo, ConstStructDb),
        lookup_const_struct_num(ConstStructDb, TCIConstNum, TCIConstStruct),
        TCIConstStruct = const_struct(TCIConstConsId, TCIConstArgs, _, _, _),
        expect(unify(TCIConstConsId, typeclass_info_cell_constructor), $pred,
            "TCIConstConsId != typeclass_info_cell_constructor"),
        TCIConstArgs = [BaseTypeClassInfoConstArg | OtherConstArgs],
        BaseTypeClassInfoConstArg = csa_constant(BaseTypeClassInfoConsId, _),
        Args = tci_arg_consts(OtherConstArgs)
    ),
    BaseTypeClassInfoConsId =
        base_typeclass_info_const(ModuleName, ClassId, InstanceNum, Instance).

%-----------------------------------------------------------------------------%

    % Succeed if the called pred is "unify" or "compare" and is specializable,
    % returning a specialized goal.
    %
:- pred specialize_special_pred(pred_id::in, proc_id::in, list(prog_var)::in,
    maybe(call_unify_context)::in, hlds_goal_info::in, hlds_goal_expr::out,
    higher_order_info::in, higher_order_info::out) is semidet.

specialize_special_pred(CalledPred, CalledProc, Args, MaybeContext,
        OrigGoalInfo, Goal, !Info) :-
    ModuleInfo = !.Info ^ hoi_global_info ^ hogi_module_info,
    ProcInfo0 = !.Info ^ hoi_proc_info,
    KnownVarMap = !.Info ^ hoi_known_var_map,
    proc_info_get_vartypes(ProcInfo0, VarTypes),
    module_info_pred_info(ModuleInfo, CalledPred, CalledPredInfo),
    mercury_public_builtin_module = pred_info_module(CalledPredInfo),
    pred_info_module(CalledPredInfo) = mercury_public_builtin_module,
    PredName = pred_info_name(CalledPredInfo),
    PredArity = pred_info_orig_arity(CalledPredInfo),
    special_pred_name_arity(SpecialId, PredName, _, PredArity),
    special_pred_get_type(SpecialId, Args, Var),
    lookup_var_type(VarTypes, Var, SpecialPredType),
    SpecialPredType \= type_variable(_, _),

    % Don't specialize tuple types -- the code to unify them only exists
    % in the generic unification routine in the runtime.
    % `private_builtin.builtin_unify_tuple/2' and
    % `private_builtin.builtin_compare_tuple/3' always abort. It might be
    % worth inlining complicated unifications of small tuples (or any
    % other small type).
    SpecialPredType \= tuple_type(_, _),

    Args = [TypeInfoVar | SpecialPredArgs],
    map.search(KnownVarMap, TypeInfoVar,
        known_const(_TypeInfoConsId, TypeInfoVarArgs)),
    type_to_ctor(SpecialPredType, SpecialPredTypeCtor),
    SpecialPredTypeCtor = type_ctor(_, TypeArity),
    ( if TypeArity = 0 then
        TypeInfoArgs = []
    else
        TypeInfoVarArgs = [_TypeCtorInfo | TypeInfoArgs]
    ),
    ( if
        not type_has_user_defined_equality_pred(ModuleInfo,
            SpecialPredType, _),
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
    then
        ( if
            is_type_a_dummy(ModuleInfo, SpecialPredType) = is_dummy_type
        then
            specialize_unify_or_compare_pred_for_dummy(MaybeResult, Goal,
                !Info)
        else if
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
        then
            specialize_unify_or_compare_pred_for_atomic(SpecialPredType,
                MaybeResult, Arg1, Arg2, MaybeContext, OrigGoalInfo, Goal,
                !Info)
        else if
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
            not type_has_user_defined_equality_pred(ModuleInfo,
                WrappedType, _),

            % This could be done for non-atomic types, but it would be a bit
            % more complicated because the type-info for the wrapped type
            % would need to be extracted first.
            type_is_atomic(ModuleInfo, WrappedType)
        then
            specialize_unify_or_compare_pred_for_no_tag(SpecialPredType,
                WrappedType, Constructor, MaybeResult, Arg1, Arg2,
                MaybeContext, OrigGoalInfo, Goal, !Info)
        else
            call_type_specific_unify_or_compare(SpecialPredType, SpecialId,
                TypeInfoArgs, SpecialPredArgs, MaybeContext, Goal, !Info)
        )
    else
        call_type_specific_unify_or_compare(SpecialPredType, SpecialId,
            TypeInfoArgs, SpecialPredArgs, MaybeContext, Goal, !Info)
    ).

:- pred call_type_specific_unify_or_compare(mer_type::in, special_pred_id::in,
    list(prog_var)::in, list(prog_var)::in,
    maybe(call_unify_context)::in, hlds_goal_expr::out,
    higher_order_info::in, higher_order_info::out) is semidet.

call_type_specific_unify_or_compare(SpecialPredType, SpecialId,
        TypeInfoArgs, SpecialPredArgs, MaybeContext, Goal, !Info) :-
    % We can only specialize unifications and comparisons to call the
    % type-specific unify or compare predicate if we are generating
    % such predicates.
    type_to_ctor_det(SpecialPredType, SpecialPredTypeCtor),
    find_special_proc(SpecialPredTypeCtor, SpecialId, SymName, SpecialPredId,
        SpecialProcId, !Info),
    ( if type_is_higher_order(SpecialPredType) then
        % Builtin_*_pred are special cases which don't need the type-info
        % arguments.
        CallArgs = SpecialPredArgs
    else
        CallArgs = TypeInfoArgs ++ SpecialPredArgs
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
        Builtin = mercury_public_builtin_module,
        TypeCtor = type_ctor(qualified(Builtin, "comparison_result"), 0),
        Eq = cons(qualified(mercury_public_builtin_module, "="), 0, TypeCtor),
        make_const_construction(term.context_init, ComparisonResult, Eq, Goal),
        Goal = hlds_goal(GoalExpr, _)
    ).

:- pred specialize_unify_or_compare_pred_for_atomic(mer_type::in,
    maybe(prog_var)::in, prog_var::in, prog_var::in,
    maybe(call_unify_context)::in, hlds_goal_info::in, hlds_goal_expr::out,
    higher_order_info::in, higher_order_info::out) is det.

specialize_unify_or_compare_pred_for_atomic(SpecialPredType, MaybeResult,
        Arg1, Arg2, MaybeContext, OrigGoalInfo, GoalExpr, !Info) :-
    ModuleInfo = !.Info ^ hoi_global_info ^ hogi_module_info,
    ProcInfo0 = !.Info ^ hoi_proc_info,
    (
        MaybeResult = no,
        UnifyMode = unify_modes_li_lf_ri_rf(ground_inst, ground_inst,
            ground_inst, ground_inst),
        GoalExpr = unify(Arg1, rhs_var(Arg2), UnifyMode,
            simple_test(Arg1, Arg2), unify_context(umc_explicit, []))
    ;
        MaybeResult = yes(ComparisonResult),
        find_builtin_type_with_equivalent_compare(ModuleInfo,
            SpecialPredType, CompareType, NeedIntCast),
        type_to_ctor_det(CompareType, CompareTypeCtor),
        get_special_proc_det(ModuleInfo, CompareTypeCtor, spec_pred_compare,
            SymName, SpecialPredId, SpecialProcId),
        (
            NeedIntCast = no,
            NewCallArgs = [ComparisonResult, Arg1, Arg2],
            GoalExpr = plain_call(SpecialPredId, SpecialProcId, NewCallArgs,
                not_builtin, MaybeContext, SymName)
        ;
            NeedIntCast = yes,
            Context = goal_info_get_context(OrigGoalInfo),
            generate_unsafe_type_cast(Context, CompareType, Arg1, CastArg1,
                CastGoal1, ProcInfo0, ProcInfo1),
            generate_unsafe_type_cast(Context, CompareType, Arg2, CastArg2,
                CastGoal2, ProcInfo1, ProcInfo),
            NewCallArgs = [ComparisonResult, CastArg1, CastArg2],
            Call = plain_call(SpecialPredId, SpecialProcId, NewCallArgs,
                not_builtin, MaybeContext, SymName),
            set_of_var.list_to_set([ComparisonResult, Arg1, Arg2], NonLocals),
            InstMapDelta = instmap_delta_bind_var(ComparisonResult),
            Detism = detism_det,
            goal_info_init(NonLocals, InstMapDelta, Detism, purity_pure,
                Context, GoalInfo),
            GoalExpr = conj(plain_conj,
                [CastGoal1, CastGoal2, hlds_goal(Call, GoalInfo)]),
            !Info ^ hoi_proc_info := ProcInfo
        )
    ).

:- pred specialize_unify_or_compare_pred_for_no_tag(mer_type::in, mer_type::in,
    sym_name::in, maybe(prog_var)::in, prog_var::in, prog_var::in,
    maybe(call_unify_context)::in, hlds_goal_info::in, hlds_goal_expr::out,
    higher_order_info::in, higher_order_info::out) is det.

specialize_unify_or_compare_pred_for_no_tag(OuterType, WrappedType,
        Constructor, MaybeResult, Arg1, Arg2, MaybeContext, OrigGoalInfo,
        GoalExpr, !Info) :-
    ModuleInfo = !.Info ^ hoi_global_info ^ hogi_module_info,
    ProcInfo0 = !.Info ^ hoi_proc_info,
    Context = goal_info_get_context(OrigGoalInfo),
    unwrap_no_tag_arg(OuterType, WrappedType, Context, Constructor, Arg1,
        UnwrappedArg1, ExtractGoal1, ProcInfo0, ProcInfo1),
    unwrap_no_tag_arg(OuterType, WrappedType, Context, Constructor, Arg2,
        UnwrappedArg2, ExtractGoal2, ProcInfo1, ProcInfo2),
    set_of_var.list_to_set([UnwrappedArg1, UnwrappedArg2], NonLocals0),
    (
        MaybeResult = no,
        NonLocals = NonLocals0,
        instmap_delta_init_reachable(InstMapDelta),
        Detism = detism_semi,
        UnifyMode = unify_modes_li_lf_ri_rf(ground_inst, ground_inst,
            ground_inst, ground_inst),
        SpecialGoal = unify(UnwrappedArg1, rhs_var(UnwrappedArg2),
            UnifyMode, simple_test(UnwrappedArg1, UnwrappedArg2),
            unify_context(umc_explicit, [])),
        goal_info_init(NonLocals, InstMapDelta, Detism, purity_pure,
            Context, GoalInfo),
        GoalExpr = conj(plain_conj,
            [ExtractGoal1, ExtractGoal2, hlds_goal(SpecialGoal, GoalInfo)]),
        !Info ^ hoi_proc_info := ProcInfo2
    ;
        MaybeResult = yes(ComparisonResult),
        set_of_var.insert(ComparisonResult, NonLocals0, NonLocals),
        InstMapDelta = instmap_delta_bind_var(ComparisonResult),
        Detism = detism_det,
        % Build a new call with the unwrapped arguments.
        find_builtin_type_with_equivalent_compare(ModuleInfo, WrappedType,
            CompareType, NeedIntCast),
        type_to_ctor_det(CompareType, CompareTypeCtor),
        get_special_proc_det(ModuleInfo, CompareTypeCtor, spec_pred_compare,
            SymName, SpecialPredId, SpecialProcId),
        (
            NeedIntCast = no,
            NewCallArgs = [ComparisonResult, UnwrappedArg1, UnwrappedArg2],
            SpecialGoal = plain_call(SpecialPredId, SpecialProcId, NewCallArgs,
                not_builtin, MaybeContext, SymName),
            goal_info_init(NonLocals, InstMapDelta, Detism, purity_pure,
                Context, GoalInfo),
            GoalExpr = conj(plain_conj, [ExtractGoal1, ExtractGoal2,
                hlds_goal(SpecialGoal, GoalInfo)]),
            !Info ^ hoi_proc_info := ProcInfo2
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
                hlds_goal(SpecialGoal, GoalInfo)]),
            !Info ^ hoi_proc_info := ProcInfo4
        )
    ).

:- pred find_special_proc(type_ctor::in, special_pred_id::in, sym_name::out,
    pred_id::out, proc_id::out,
    higher_order_info::in, higher_order_info::out) is semidet.

find_special_proc(TypeCtor, SpecialId, SymName, PredId, ProcId, !Info) :-
    ModuleInfo0 = !.Info ^ hoi_global_info ^ hogi_module_info,
    ( if
        get_special_proc(ModuleInfo0, TypeCtor, SpecialId, SymName0,
            PredId0, ProcId0)
    then
        SymName = SymName0,
        PredId = PredId0,
        ProcId = ProcId0
    else
        special_pred_is_generated_lazily(ModuleInfo0, TypeCtor),
        (
            SpecialId = spec_pred_compare,
            add_lazily_generated_compare_pred_decl(TypeCtor, PredId,
                ModuleInfo0, ModuleInfo),
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

            add_lazily_generated_unify_pred(TypeCtor, PredId,
                ModuleInfo0, ModuleInfo),
            hlds_pred.in_in_unification_proc_id(ProcId)
        ),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        ModuleName = pred_info_module(PredInfo),
        Name = pred_info_name(PredInfo),
        SymName = qualified(ModuleName, Name),
        !Info ^ hoi_global_info ^ hogi_module_info := ModuleInfo
    ).

:- pred find_builtin_type_with_equivalent_compare(module_info::in,
    mer_type::in, mer_type::out, bool::out) is det.

find_builtin_type_with_equivalent_compare(ModuleInfo, Type, EqvType,
        NeedIntCast) :-
    CtorCat = classify_type(ModuleInfo, Type),
    (
        CtorCat = ctor_cat_builtin(_),
        EqvType = Type,
        NeedIntCast = no
    ;
        CtorCat = ctor_cat_enum(_),
        construct_type(type_ctor(unqualified("int"), 0), [], EqvType),
        NeedIntCast = yes
    ;
        ( CtorCat = ctor_cat_builtin_dummy
        ; CtorCat = ctor_cat_void
        ; CtorCat = ctor_cat_higher_order
        ; CtorCat = ctor_cat_tuple
        ; CtorCat = ctor_cat_variable
        ; CtorCat = ctor_cat_user(_)
        ; CtorCat = ctor_cat_system(_)
        ),
        unexpected($pred, "bad type")
    ).

:- pred generate_unsafe_type_cast(prog_context::in,
    mer_type::in, prog_var::in, prog_var::out, hlds_goal::out,
    proc_info::in, proc_info::out) is det.

generate_unsafe_type_cast(Context, ToType, Arg, CastArg, Goal, !ProcInfo) :-
    proc_info_create_var_from_type(ToType, no, CastArg, !ProcInfo),
    generate_cast(unsafe_type_cast, Arg, CastArg, Context, Goal).

:- pred unwrap_no_tag_arg(mer_type::in, mer_type::in, prog_context::in,
    sym_name::in, prog_var::in, prog_var::out, hlds_goal::out,
    proc_info::in, proc_info::out) is det.

unwrap_no_tag_arg(OuterType, WrappedType, Context, Constructor, Arg,
        UnwrappedArg, Goal, !ProcInfo) :-
    proc_info_create_var_from_type(WrappedType, no, UnwrappedArg, !ProcInfo),
    type_to_ctor_det(OuterType, OuterTypeCtor),
    ConsId = cons(Constructor, 1, OuterTypeCtor),
    Ground = ground(shared, none_or_default_func),
    UnifyModeInOut = unify_modes_li_lf_ri_rf(Ground, Ground, free, Ground),
    ArgModes = [UnifyModeInOut],
    set_of_var.list_to_set([Arg, UnwrappedArg], NonLocals),
    % This will be recomputed later.
    InstMapDelta = instmap_delta_bind_var(UnwrappedArg),
    goal_info_init(NonLocals, InstMapDelta, detism_det, purity_pure, Context,
        GoalInfo),
    Unification = deconstruct(Arg, ConsId, [UnwrappedArg], ArgModes,
        cannot_fail, cannot_cgc),
    GoalExpr = unify(Arg,
        rhs_functor(ConsId, is_not_exist_constr, [UnwrappedArg]),
        UnifyModeInOut, Unification, unify_context(umc_explicit, [])),
    Goal = hlds_goal(GoalExpr, GoalInfo).

%-----------------------------------------------------------------------------%
%
% Predicates to process requests for specialization, and create any
% new predicates that are required.
%

    % Filter out requests for higher-order specialization for preds which are
    % too large. Maybe we could allow programmers to declare which predicates
    % they want specialized, as with inlining? Don't create specialized
    % versions of specialized versions, since for some fairly contrived
    % examples involving recursively building up lambda expressions,
    % this can create ridiculous numbers of versions.
    %
:- pred filter_request(higher_order_global_info::in, ho_request::in,
    list(ho_request)::in, list(ho_request)::out,
    list(ho_request)::in, list(ho_request)::out, io::di, io::uo) is det.

filter_request(Info, Request, !AcceptedRequests, !LoopRequests, !IO) :-
    ModuleInfo = Info ^ hogi_module_info,
    Request = ho_request(CallingPredProcId, CalledPredProcId, _, _, HOArgs,
        _, _, _, IsUserTypeSpec, Context),
    CalledPredProcId = proc(CalledPredId, _),
    module_info_pred_info(ModuleInfo, CalledPredId, PredInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    PredModule = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    PredFormArity = pred_info_pred_form_arity(PredInfo),
    pred_info_get_arg_types(PredInfo, Types),
    ActualArity = arg_list_arity(Types),
    (
        VeryVerbose = no,
        MaybeProgressStream = no
    ;
        VeryVerbose = yes,
        get_progress_output_stream(ModuleInfo, ProgressStream, !IO),
        MaybeProgressStream = yes(ProgressStream),
        write_request(ProgressStream, ModuleInfo, "Request for",
            qualified(PredModule, PredName), PredFormArity, ActualArity,
            no, HOArgs, Context, !IO)
    ),
    (
        IsUserTypeSpec = yes,
        % Ignore the size limit for user specified specializations.
        maybe_write_string_to_stream(MaybeProgressStream,
            "%    request specialized (user-requested specialization)\n", !IO),
        list.cons(Request, !AcceptedRequests)
    ;
        IsUserTypeSpec = no,
        ( if map.search(Info ^ hogi_goal_sizes, CalledPredId, GoalSize0) then
            GoalSize = GoalSize0
        else
            % This can happen for a specialized version.
            GoalSize = 0
        ),
        ( if
            GoalSize > Info ^ hogi_params ^ param_size_limit
        then
            maybe_write_string_to_stream(MaybeProgressStream,
                "%    not specializing (goal too large).\n", !IO)
        else if
            higher_order_args_size(HOArgs) >
                Info ^ hogi_params ^ param_arg_limit
        then
            % If the arguments are too large, we can end up producing a
            % specialized version with massive numbers of arguments, because
            % all of the curried arguments are passed as separate arguments.
            % Without this extras/xml/xml.parse.chars.m takes forever to
            % compile.
            maybe_write_string_to_stream(MaybeProgressStream,
                "%    not specializing (args too large).\n", !IO)
        else if
            % To ensure termination of the specialization process, the depth
            % of the higher-order arguments must strictly decrease compared
            % to parents with the same original pred_proc_id.
            VersionInfoMap = Info ^ hogi_version_info,
            ( if
                map.search(VersionInfoMap, CalledPredProcId, CalledVersionInfo)
            then
                CalledVersionInfo = version_info(OrigPredProcId, _, _, _)
            else
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
        then
            !:LoopRequests = [Request | !.LoopRequests],
            maybe_write_string_to_stream(MaybeProgressStream,
                "%    not specializing (recursive specialization).\n", !IO)
        else
            maybe_write_string_to_stream(MaybeProgressStream,
                "%    request specialized.\n", !IO),
            list.cons(Request, !AcceptedRequests)
        )
    ).

:- pred maybe_create_new_preds(list(ho_request)::in, list(new_pred)::in,
    list(new_pred)::out, set(pred_proc_id)::in, set(pred_proc_id)::out,
    higher_order_global_info::in, higher_order_global_info::out,
    io::di, io::uo) is det.

maybe_create_new_preds([], !NewPreds, !PredsToFix, !Info, !IO).
maybe_create_new_preds([Request | Requests], !NewPreds, !PredsToFix,
        !Info, !IO) :-
    Request = ho_request(CallingPredProcId, CalledPredProcId, _HOArgs,
        _CallArgs, _, _CallerArgTypes, _, _, _, _),
    set.insert(CallingPredProcId, !PredsToFix),
    ( if
        % Check that we aren't redoing the same pred.
        % SpecVersions0 are pred_proc_ids of the specialized versions
        % of the current pred.
        NewPredMap = !.Info ^ hogi_new_pred_map,
        map.search(NewPredMap, CalledPredProcId, SpecVersions0),
        set.member(Version, SpecVersions0),
        version_matches(!.Info ^ hogi_params, !.Info ^ hogi_module_info,
            Request, Version, _)
    then
        true
    else
        create_new_pred(Request, NewPred, !Info, !IO),
        !:NewPreds = [NewPred | !.NewPreds]
    ),
    maybe_create_new_preds(Requests, !NewPreds, !PredsToFix, !Info, !IO).

    % If we weren't allowed to create a specialized version because the
    % loop check failed, check whether the version was created for another
    % request for which the loop check succeeded.
    %
:- pred check_loop_request(higher_order_global_info::in, ho_request::in,
    set(pred_proc_id)::in, set(pred_proc_id)::out) is det.

check_loop_request(Info, Request, !PredsToFix) :-
    CallingPredProcId = Request ^ rq_caller,
    CalledPredProcId = Request ^ rq_callee,
    ( if
        map.search(Info ^ hogi_new_pred_map, CalledPredProcId, SpecVersions0),
        some [Version] (
            set.member(Version, SpecVersions0),
            version_matches(Info ^ hogi_params, Info ^ hogi_module_info,
                Request, Version, _)
        )
    then
        set.insert(CallingPredProcId, !PredsToFix)
    else
        true
    ).

    % Here we create the pred_info for the new predicate.
    %
:- pred create_new_pred(ho_request::in, new_pred::out,
    higher_order_global_info::in, higher_order_global_info::out,
    io::di, io::uo) is det.

create_new_pred(Request, NewPred, !Info, !IO) :-
    Request = ho_request(Caller, CalledPredProc, CallArgs, ExtraTypeInfoTVars,
        HOArgs, ArgTypes, CallerTVarSet, TypeInfoLiveness,
        IsUserTypeSpec, Context),
    Caller = proc(CallerPredId, CallerProcId),
    ModuleInfo0 = !.Info ^ hogi_module_info,
    module_info_pred_proc_info(ModuleInfo0, CalledPredProc,
        PredInfo0, ProcInfo0),

    Name0 = pred_info_name(PredInfo0),
    PredFormArity = pred_info_pred_form_arity(PredInfo0),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo0),
    PredModuleName = pred_info_module(PredInfo0),
    module_info_get_globals(ModuleInfo0, Globals),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
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
        CallerPredName0 = predicate_name(ModuleInfo0, CallerPredId),
        proc_id_to_int(CallerProcId, CallerProcNum),

        % The higher_order_arg_order_version part is to avoid segmentation
        % faults or other errors when the order or number of extra arguments
        % changes. If the user does not recompile all affected code, the
        % program will not link.
        Transform = tn_higher_order_type_spec(PredOrFunc, CallerProcNum,
            higher_order_arg_order_version),
        make_transformed_pred_name(CallerPredName0, Transform, SpecName),
        OriginTransform =
            transform_higher_order_type_specialization(CallerProcNum),
        NewProcId = CallerProcId,
        % For exported predicates the type specialization must
        % be exported.
        % For opt_imported predicates we only want to keep this
        % version if we do some other useful specialization on it.
        pred_info_get_status(PredInfo0, PredStatus)
    ;
        IsUserTypeSpec = no,
        NewProcId = hlds_pred.initial_proc_id,
        IdCounter0 = !.Info ^ hogi_next_id,
        counter.allocate(Id, IdCounter0, IdCounter),
        !Info ^ hogi_next_id := IdCounter,
        Transform = tn_higher_order(PredOrFunc, Id),
        make_transformed_pred_name(Name0, Transform, SpecName),
        OriginTransform = transform_higher_order_specialization(Id),
        PredStatus = pred_status(status_local)
    ),

    (
        VeryVerbose = no
    ;
        VeryVerbose = yes,
        get_progress_output_stream(ModuleInfo0, ProgressStream, !IO),
        ActualArity = arg_list_arity(Types),
        write_request(ProgressStream, ModuleInfo0, "Specializing",
            qualified(PredModuleName, Name0), PredFormArity, ActualArity,
            yes(SpecName), HOArgs, Context, !IO)
    ),

    pred_info_get_origin(PredInfo0, OrigOrigin),
    pred_info_get_typevarset(PredInfo0, TypeVarSet),
    pred_info_get_markers(PredInfo0, MarkerList),
    pred_info_get_goal_type(PredInfo0, GoalType),
    pred_info_get_class_context(PredInfo0, ClassContext),
    pred_info_get_var_name_remap(PredInfo0, VarNameRemap),
    varset.init(EmptyVarSet),
    init_vartypes(EmptyVarTypes),
    map.init(EmptyTVarNameMap),
    map.init(EmptyProofs),
    map.init(EmptyConstraintMap),

    % This isn't looked at after here, and just clutters up HLDS dumps
    % if it's filled in.
    set_clause_list([], ClausesRep),
    EmptyHeadVars = proc_arg_vector_init(pf_predicate, []),
    ItemNumbers = init_clause_item_numbers_comp_gen,
    rtti_varmaps_init(EmptyRttiVarMaps),
    ClausesInfo = clauses_info(EmptyVarSet, EmptyTVarNameMap,
        EmptyVarTypes, EmptyVarTypes, EmptyHeadVars, ClausesRep, ItemNumbers,
        EmptyRttiVarMaps, no_foreign_lang_clauses, no_clause_syntax_errors),
    Origin = origin_transformed(OriginTransform, OrigOrigin, CallerPredId),
    CurUserDecl = maybe.no,
    pred_info_init(PredOrFunc, PredModuleName, SpecName, PredFormArity,
        Context, Origin, PredStatus, CurUserDecl, GoalType, MarkerList, Types,
        ArgTVarSet, ExistQVars, ClassContext, EmptyProofs, EmptyConstraintMap,
        ClausesInfo, VarNameRemap, NewPredInfo0),
    pred_info_set_typevarset(TypeVarSet, NewPredInfo0, NewPredInfo1),

    module_info_get_predicate_table(ModuleInfo0, PredTable0),
    predicate_table_insert(NewPredInfo1, NewPredId, PredTable0, PredTable),
    module_info_set_predicate_table(PredTable, ModuleInfo0, ModuleInfo1),

    !Info ^ hogi_module_info := ModuleInfo1,

    SpecSymName = qualified(PredModuleName, SpecName),
    NewPred = new_pred(proc(NewPredId, NewProcId), CalledPredProc, Caller,
        SpecSymName, HOArgs, CallArgs, ExtraTypeInfoTVars, ArgTypes,
        CallerTVarSet, TypeInfoLiveness, IsUserTypeSpec),

    higher_order_add_new_pred(CalledPredProc, NewPred, !Info),

    create_new_proc(NewPred, ProcInfo0, NewPredInfo1, NewPredInfo, !Info),
    ModuleInfo2 = !.Info ^ hogi_module_info,
    module_info_set_pred_info(NewPredId, NewPredInfo, ModuleInfo2, ModuleInfo),
    !Info ^ hogi_module_info := ModuleInfo.

:- pred higher_order_add_new_pred(pred_proc_id::in, new_pred::in,
    higher_order_global_info::in, higher_order_global_info::out) is det.

higher_order_add_new_pred(CalledPredProcId, NewPred, !Info) :-
    NewPredMap0 = !.Info ^ hogi_new_pred_map,
    ( if map.search(NewPredMap0, CalledPredProcId, SpecVersions0) then
        set.insert(NewPred, SpecVersions0, SpecVersions),
        map.det_update(CalledPredProcId, SpecVersions, NewPredMap0, NewPredMap)
    else
        SpecVersions = set.make_singleton_set(NewPred),
        map.det_insert(CalledPredProcId, SpecVersions, NewPredMap0, NewPredMap)
    ),
    !Info ^ hogi_new_pred_map := NewPredMap.

:- pred write_request(io.text_output_stream::in, module_info::in,
    string::in, sym_name::in, pred_form_arity::in, pred_form_arity::in,
    maybe(string)::in, list(higher_order_arg)::in, prog_context::in,
    io::di, io::uo) is det.

write_request(OutputStream, ModuleInfo, Msg,
        SymName, PredArity, ActualArity, MaybeNewName, HOArgs, Context, !IO) :-
    OldName = sym_name_to_string(SymName),
    PredArity = pred_form_arity(PredArityInt),
    ActualArity = pred_form_arity(ActualArityInt),
    io.write_string(OutputStream, "% ", !IO),
    prog_out.write_context(OutputStream, Context, !IO),
    io.format(OutputStream, "%s `%s'/%d",
        [s(Msg), s(OldName), i(PredArityInt)], !IO),
    (
        MaybeNewName = yes(NewName),
        io.format(OutputStream, " into %s", [s(NewName)], !IO)
    ;
        MaybeNewName = no
    ),
    io.write_string(OutputStream, " with higher-order arguments:\n", !IO),
    NumToDrop = ActualArityInt - PredArityInt,
    output_higher_order_args(OutputStream, ModuleInfo, NumToDrop, 0,
        HOArgs, !IO).

:- pred output_higher_order_args(io.text_output_stream::in, module_info::in,
    int::in, int::in, list(higher_order_arg)::in, io::di, io::uo) is det.

output_higher_order_args(_, _, _, _, [], !IO).
output_higher_order_args(OutputStream, ModuleInfo, NumToDrop, Indent,
        [HOArg | HOArgs], !IO) :-
    HOArg = higher_order_arg(ConsId, ArgNo, NumArgs, _, _, _,
        CurriedHOArgs, IsConst),
    io.write_string(OutputStream, "% ", !IO),
    list.duplicate(Indent + 1, "  ", Spaces),
    list.foldl(io.write_string(OutputStream), Spaces, !IO),
    (
        IsConst = yes,
        io.write_string(OutputStream, "const ", !IO)
    ;
        IsConst = no
    ),
    ( if ConsId = closure_cons(ShroudedPredProcId, _) then
        proc(PredId, _) = unshroud_pred_proc_id(ShroudedPredProcId),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        Name = pred_info_name(PredInfo),
        PredArity = pred_info_orig_arity(PredInfo),
        % Adjust message for type_infos.
        DeclaredArgNo = ArgNo - NumToDrop,
        io.format(OutputStream, "HeadVar__%d = `%s'/%d",
            [i(DeclaredArgNo), s(Name), i(PredArity)], !IO)
    else if ConsId = type_ctor_info_const(TypeModule, TypeName, TypeArity) then
        io.format(OutputStream, "type_ctor_info for `%s'/%d",
            [s(sym_name_to_escaped_string(qualified(TypeModule, TypeName))),
            i(TypeArity)], !IO)
    else if ConsId = base_typeclass_info_const(_, ClassId, _, _) then
        ClassId = class_id(ClassSymName, ClassArity),
        io.format(OutputStream, "base_typeclass_info for `%s'/%d",
            [s(sym_name_to_escaped_string(ClassSymName)), i(ClassArity)], !IO)
    else
        % XXX output the type.
        io.write_string(OutputStream, "type_info/typeclass_info ", !IO)
    ),
    io.format(OutputStream, " with %d curried arguments", [i(NumArgs)], !IO),
    (
        CurriedHOArgs = [],
        io.nl(OutputStream, !IO)
    ;
        CurriedHOArgs = [_ | _],
        io.write_string(OutputStream, ":\n", !IO),
        output_higher_order_args(OutputStream, ModuleInfo, 0, Indent + 1,
            CurriedHOArgs, !IO)
    ),
    output_higher_order_args(OutputStream, ModuleInfo, NumToDrop, Indent,
        HOArgs, !IO).

%-----------------------------------------------------------------------------%

:- type must_recompute
    --->    must_recompute
    ;       need_not_recompute.

:- pred ho_fixup_preds(list(pred_proc_id)::in, higher_order_global_info::in,
    higher_order_global_info::out) is det.

ho_fixup_preds(PredProcIds, !Info) :-
    Requests0 = !.Info ^ hogi_requests,
    list.foldl(ho_fixup_pred(need_not_recompute), PredProcIds, !Info),
    % Any additional requests must have already been denied.
    !Info ^ hogi_requests := Requests0.

:- pred ho_fixup_specialized_versions(list(new_pred)::in,
    higher_order_global_info::in, higher_order_global_info::out) is det.

ho_fixup_specialized_versions(NewPredList, !Info) :-
    NewPredProcIds = list.map(get_np_version_ppid, NewPredList),
    % Reprocess the goals to find any new specializations made
    % possible by the specializations performed in this pass.
    list.foldl(ho_fixup_pred(must_recompute), NewPredProcIds, !Info).

    % Fixup calls to specialized predicates.
    %
:- pred ho_fixup_pred(must_recompute::in, pred_proc_id::in,
    higher_order_global_info::in, higher_order_global_info::out) is det.

ho_fixup_pred(MustRecompute, proc(PredId, ProcId), !GlobalInfo) :-
    ho_traverse_proc(MustRecompute, PredId, ProcId, !GlobalInfo).

%-----------------------------------------------------------------------------%

    % Build a proc_info for a specialized version.
    %
:- pred create_new_proc(new_pred::in, proc_info::in, pred_info::in,
    pred_info::out, higher_order_global_info::in,
    higher_order_global_info::out) is det.

create_new_proc(NewPred, !.NewProcInfo, !NewPredInfo, !GlobalInfo) :-
    ModuleInfo = !.GlobalInfo ^ hogi_module_info,

    NewPred = new_pred(NewPredProcId, OldPredProcId, CallerPredProcId, _Name,
        HOArgs0, CallArgs, ExtraTypeInfoTVars0, CallerArgTypes0, _, _, _),

    proc_info_get_headvars(!.NewProcInfo, HeadVars0),
    proc_info_get_argmodes(!.NewProcInfo, ArgModes0),
    pred_info_get_exist_quant_tvars(!.NewPredInfo, ExistQVars0),
    pred_info_get_typevarset(!.NewPredInfo, TypeVarSet0),
    pred_info_get_tvar_kind_map(!.NewPredInfo, KindMap0),
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

    compute_caller_callee_type_substitution(OriginalArgTypes1, CallerArgTypes0,
        CallerHeadParams, ExistQVars1, TypeSubn),

    apply_rec_subst_to_tvar_list(KindMap, TypeSubn, ExistQVars1, ExistQTypes),
    ExistQVars = list.filter_map(
        ( func(ExistQType) = ExistQVar is semidet :-
            ExistQType = type_variable(ExistQVar, _)
        ), ExistQTypes),

    apply_rec_subst_to_vartypes(TypeSubn, VarTypes1, VarTypes2),
    apply_rec_subst_to_type_list(TypeSubn,
        OriginalArgTypes1, OriginalArgTypes),
    proc_info_set_vartypes(VarTypes2, !NewProcInfo),

    % XXX kind inference: we assume vars have kind `star'.
    prog_type.var_list_to_type_list(map.init, ExtraTypeInfoTVars0,
        ExtraTypeInfoTVarTypes0),
    ( if
        ( map.is_empty(TypeSubn)
        ; ExistQVars = []
        )
    then
        HOArgs = HOArgs0,
        ExtraTypeInfoTVarTypes = ExtraTypeInfoTVarTypes0,
        ExtraTypeInfoTVars = ExtraTypeInfoTVars0
    else
        % If there are existentially quantified variables in the callee
        % we may need to bind type variables in the caller.
        list.map(substitute_higher_order_arg(TypeSubn), HOArgs0, HOArgs),

        apply_rec_subst_to_type_list(TypeSubn, ExtraTypeInfoTVarTypes0,
            ExtraTypeInfoTVarTypes),
        % The substitution should never bind any of the type variables
        % for which extra type-infos are needed, otherwise it
        % wouldn't be necessary to add them.
        ( if
            prog_type.type_list_to_var_list(ExtraTypeInfoTVarTypes,
                ExtraTypeInfoTVarsPrim)
        then
            ExtraTypeInfoTVars = ExtraTypeInfoTVarsPrim
        else
            unexpected($pred, "type var got bound")
        )
    ),

    % Add in the extra typeinfo vars.
    ExtraTypeInfoTypes =
        list.map(build_type_info_type, ExtraTypeInfoTVarTypes),
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
    SetTypeInfoVarLocn =
        ( pred(TVar::in, Var::in, !.R::in, !:R::out) is det :-
            Locn = type_info(Var),
            rtti_set_type_info_locn(TVar, Locn, !R)
        ),
    list.foldl_corresponding(SetTypeInfoVarLocn,
        ExtraTypeInfoTVars, ExtraTypeInfoVars, RttiVarMaps2, RttiVarMaps),

    proc_info_set_rtti_varmaps(RttiVarMaps, !NewProcInfo),

    map.from_corresponding_lists(CallArgs, HeadVars0, VarRenaming0),

    % Construct the constant input closures within the goal
    % for the called procedure.
    map.init(KnownVarMap0),
    construct_higher_order_terms(ModuleInfo, HeadVars0, ExtraHeadVars,
        ArgModes0, ExtraArgModes, HOArgs, !NewProcInfo,
        VarRenaming0, _, KnownVarMap0, KnownVarMap, ConstGoals),

    % XXX The substitutions used to be applied to the typeclass_info_varmap
    % here rather than at the XXX above. Any new entries added in the code
    % between these two points should therefore be transformed as well?
    % The new entries come from HOArgs, which have already had TypeSubn
    % applied, but not TypeRenaming. Perhaps this is enough?

    % Record extra information about this version.
    VersionInfoMap0 = !.GlobalInfo ^ hogi_version_info,
    ArgsDepth = higher_order_args_depth(HOArgs),

    ( if map.search(VersionInfoMap0, OldPredProcId, OldProcVersionInfo) then
        OldProcVersionInfo = version_info(OrigPredProcId, _, _, _)
    else
        OrigPredProcId = OldPredProcId
    ),

    ( if map.search(VersionInfoMap0, CallerPredProcId, CallerVersionInfo) then
        CallerVersionInfo = version_info(_, _, _, CallerParentVersions)
    else
        CallerParentVersions = []
    ),
    ParentVersions = [parent_version_info(OrigPredProcId, ArgsDepth)
        | CallerParentVersions],

    VersionInfo = version_info(OrigPredProcId, ArgsDepth,
        KnownVarMap, ParentVersions),
    map.det_insert(NewPredProcId, VersionInfo,
        VersionInfoMap0, VersionInfoMap),
    !GlobalInfo ^ hogi_version_info := VersionInfoMap,

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
    Goal6 = hlds_goal(_, GoalInfo6),
    goal_to_conj_list(Goal6, GoalList6),
    conj_list_to_goal(ConstGoals ++ GoalList6, GoalInfo6, Goal),
    proc_info_set_goal(Goal, !NewProcInfo),

    % Remove any imported structure sharing and reuse information for the
    % original procedure as they won't be (directly) applicable.
    proc_info_reset_imported_structure_sharing(!NewProcInfo),
    proc_info_reset_imported_structure_reuse(!NewProcInfo),

    proc_info_get_vartypes(!.NewProcInfo, VarTypes7),
    lookup_var_types(VarTypes7, ExtraHeadVars, ExtraHeadVarTypes0),
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
    % use the substitution computed based on the result
    % pred_info_get_arg_types. We may need to apply a substitution
    % to the types of the new variables in the vartypes in the proc_info.
    %
    % XXX We should apply this substitution to the variable types in any
    % callers of this predicate, which may introduce other opportunities
    % for specialization.

    (
        ExistQVars = []
    ;
        ExistQVars = [_ | _],
        lookup_var_types(VarTypes7, HeadVars0, OriginalHeadTypes),
        type_list_subsumes_det(OriginalArgTypes, OriginalHeadTypes,
            ExistentialSubn),
        apply_rec_subst_to_type_list(ExistentialSubn, ExtraHeadVarTypes0,
            ExtraHeadVarTypes),
        assoc_list.from_corresponding_lists(ExtraHeadVars,
            ExtraHeadVarTypes, ExtraHeadVarsAndTypes),
        list.foldl(update_var_types, ExtraHeadVarsAndTypes,
            VarTypes7, VarTypes8),
        proc_info_set_vartypes(VarTypes8, !NewProcInfo)
    ),

    % Find the new class context.
    proc_info_get_headvars(!.NewProcInfo, ArgVars),
    proc_info_get_rtti_varmaps(!.NewProcInfo, NewRttiVarMaps),
    list.map(rtti_varmaps_var_info(NewRttiVarMaps), ArgVars, ArgVarInfos),
    find_class_context(ModuleInfo, ArgVarInfos, ArgModes, [], [],
        ClassContext),
    pred_info_set_class_context(ClassContext, !NewPredInfo),

    NewPredProcId = proc(_, NewProcId),
    NewProcs = map.singleton(NewProcId, !.NewProcInfo),
    pred_info_set_proc_table(NewProcs, !NewPredInfo).

:- pred update_var_types(pair(prog_var, mer_type)::in,
    vartypes::in, vartypes::out) is det.

update_var_types(VarAndType, !VarTypes) :-
    VarAndType = Var - Type,
    update_var_type(Var, Type, !VarTypes).

    % Take an original list of headvars and arg_modes and return these
    % with curried arguments added. The old higher-order arguments are
    % left in. They may be needed in calls which could not be
    % specialised. If not, unused_args.m can clean them up.
    %
    % Build the initial known_var_map which records higher-order and
    % type_info constants for a call to ho_traverse_proc_body.
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
    known_var_map::in, known_var_map::out, list(hlds_goal)::out) is det.

construct_higher_order_terms(_, _, [], _, [], [], !ProcInfo, !Renaming,
        !KnownVarMap, []).
construct_higher_order_terms(ModuleInfo, HeadVars0, NewHeadVars, ArgModes0,
        NewArgModes, [HOArg | HOArgs], !ProcInfo, !Renaming,
        !KnownVarMap, ConstGoals) :-
    HOArg = higher_order_arg(ConsId, Index, NumArgs, CurriedArgs,
        CurriedArgTypes, CurriedArgRttiInfo, CurriedHOArgs, IsConst),

    list.det_index1(HeadVars0, Index, LVar),
    ( if ConsId = closure_cons(ShroudedPredProcId, _) then
        % Add the curried arguments to the procedure's argument list.
        proc(PredId, ProcId) =
            unshroud_pred_proc_id(ShroudedPredProcId),
        module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
            CalledPredInfo, CalledProcInfo),
        PredOrFunc = pred_info_is_pred_or_func(CalledPredInfo),
        proc_info_get_argmodes(CalledProcInfo, CalledArgModes),
        list.det_split_list(NumArgs, CalledArgModes,
            CurriedArgModes1, NonCurriedArgModes),
        proc_info_interface_determinism(CalledProcInfo, ProcDetism),
        GroundInstInfo = higher_order(pred_inst_info(PredOrFunc,
            NonCurriedArgModes, arg_reg_types_unset, ProcDetism))
    else
        in_mode(InMode),
        GroundInstInfo = none_or_default_func,
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
        % Make ho_traverse_proc_body pretend that the input higher-order
        % argument is built using the new arguments as its curried arguments.
        map.det_insert(LVar, known_const(ConsId, CurriedHeadVars1),
            !KnownVarMap)
    ;
        IsConst = yes
    ),

    assoc_list.from_corresponding_lists(CurriedArgs, CurriedHeadVars1,
        CurriedRenaming),
    list.foldl(
        ( pred(VarPair::in, !.Map::in, !:Map::out) is det :-
            VarPair = Var1 - Var2,
            map.set(Var1, Var2, !Map)
        ), CurriedRenaming, !Renaming),

    % Recursively construct the curried higher-order arguments.
    construct_higher_order_terms(ModuleInfo, CurriedHeadVars1,
        ExtraCurriedHeadVars, CurriedArgModes1, ExtraCurriedArgModes,
        CurriedHOArgs, !ProcInfo, !Renaming, !KnownVarMap,
        CurriedConstGoals),

    % Construct the rest of the higher-order arguments.
    construct_higher_order_terms(ModuleInfo, HeadVars0, NewHeadVars1,
        ArgModes0, NewArgModes1, HOArgs, !ProcInfo,
        !Renaming, !KnownVarMap, ConstGoals1),

    (
        IsConst = yes,
        % Build the constant inside the specialized version, so that
        % other constants which include it will be recognized as constant.
        ArgModes = list.map(mode_both_sides_to_unify_mode(ModuleInfo),
            CurriedArgModes1),
        set_of_var.list_to_set(CurriedHeadVars1, ConstNonLocals),
        ConstInst = ground(shared, GroundInstInfo),
        ConstInstMapDelta = instmap_delta_from_assoc_list([LVar - ConstInst]),
        goal_info_init(ConstNonLocals, ConstInstMapDelta, detism_det,
            purity_pure, ConstGoalInfo),
        RHS = rhs_functor(ConsId, is_not_exist_constr, CurriedHeadVars1),
        UnifyMode = unify_modes_li_lf_ri_rf(free, ConstInst,
            ConstInst, ConstInst),
        ConstGoalExpr = unify(LVar, RHS, UnifyMode,
            construct(LVar, ConsId, CurriedHeadVars1, ArgModes,
                construct_dynamically, cell_is_unique, no_construct_sub_info),
            unify_context(umc_explicit, [])),
        ConstGoal = hlds_goal(ConstGoalExpr, ConstGoalInfo),
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
    ConstGoals = ConstGoals0 ++ ConstGoals1.

    % Add any new type-infos or typeclass-infos to the rtti_varmaps.
    %
:- pred add_rtti_info(prog_var::in, rtti_var_info::in,
    rtti_varmaps::in, rtti_varmaps::out) is det.

add_rtti_info(Var, VarInfo, !RttiVarMaps) :-
    (
        VarInfo = type_info_var(TypeInfoType),
        rtti_det_insert_type_info_type(Var, TypeInfoType, !RttiVarMaps),
        ( if TypeInfoType = type_variable(TVar, _) then
            maybe_set_typeinfo_locn(TVar, type_info(Var), !RttiVarMaps)
        else
            true
        )
    ;
        VarInfo = typeclass_info_var(Constraint),
        ( if rtti_search_typeclass_info_var(!.RttiVarMaps, Constraint, _) then
            true
        else
            rtti_det_insert_typeclass_info_var(Constraint, Var, !RttiVarMaps),
            Constraint = constraint(_ClassName, ConstraintArgTypes),
            list.foldl2(update_type_info_locn(Var), ConstraintArgTypes, 1, _,
                !RttiVarMaps)
        )
    ;
        VarInfo = non_rtti_var
    ).

:- pred update_type_info_locn(prog_var::in, mer_type::in, int::in, int::out,
    rtti_varmaps::in, rtti_varmaps::out) is det.

update_type_info_locn(Var, ConstraintType, Index, Index + 1, !RttiVarMaps) :-
    (
        ConstraintType = type_variable(ConstraintTVar, _),
        maybe_set_typeinfo_locn(ConstraintTVar,
            typeclass_info(Var, Index), !RttiVarMaps)
    ;
        ( ConstraintType = defined_type(_, _, _)
        ; ConstraintType = builtin_type(_)
        ; ConstraintType = tuple_type(_, _)
        ; ConstraintType = higher_order_type(_, _, _, _, _)
        ; ConstraintType = apply_n_type(_, _, _)
        ; ConstraintType = kinded_type(_, _)
        )
    ).

:- pred maybe_set_typeinfo_locn(tvar::in, type_info_locn::in,
    rtti_varmaps::in, rtti_varmaps::out) is det.

maybe_set_typeinfo_locn(TVar, Locn, !RttiVarMaps) :-
    ( if rtti_search_type_info_locn(!.RttiVarMaps, TVar, _) then
        true
    else
        rtti_det_insert_type_info_locn(TVar, Locn, !RttiVarMaps)
    ).

:- pred remove_const_higher_order_args(int::in, list(T)::in,
    list(higher_order_arg)::in, list(T)::out) is det.

remove_const_higher_order_args(_, [], _, []).
remove_const_higher_order_args(Index, [Arg | Args0], HOArgs0, Args) :-
    (
        HOArgs0 = [HOArg | HOArgs],
        HOArg = higher_order_arg(_, HOIndex, _, _, _, _, _, IsConst),
        ( if HOIndex = Index then
            remove_const_higher_order_args(Index + 1, Args0, HOArgs, Args1),
            (
                IsConst = yes,
                Args = Args1
            ;
                IsConst = no,
                Args = [Arg | Args1]
            )
        else if HOIndex > Index then
            remove_const_higher_order_args(Index + 1, Args0, HOArgs0, Args1),
            Args = [Arg | Args1]
        else
            unexpected($pred, "unordered indexes")
        )
    ;
        HOArgs0 = [],
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
    !HOArg ^ hoa_curry_type_in_caller := CurriedArgTypes,
    !HOArg ^ hoa_curry_rtti_type := CurriedRttiTypes,
    !HOArg ^ hoa_known_curry_args := CurriedHOArgs.

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

find_class_context(_, [], [], !.RevUniv, !.RevExist, Constraints) :-
    list.reverse(!.RevUniv, Univ),
    list.reverse(!.RevExist, Exist),
    Constraints = constraints(Univ, Exist).
find_class_context(_, [], [_ | _], _, _, _) :-
    unexpected($pred, "mismatched list length").
find_class_context(_, [_ | _], [], _, _, _) :-
    unexpected($pred, "mismatched list length").
find_class_context(ModuleInfo, [VarInfo | VarInfos], [Mode | Modes],
        !.RevUniv, !.RevExist, Constraints) :-
    (
        VarInfo = typeclass_info_var(Constraint),
        ( if mode_is_input(ModuleInfo, Mode) then
            maybe_add_constraint(Constraint, !RevUniv)
        else
            maybe_add_constraint(Constraint, !RevExist)
        )
    ;
        VarInfo = type_info_var(_)
    ;
        VarInfo = non_rtti_var
    ),
    find_class_context(ModuleInfo, VarInfos, Modes, !.RevUniv, !.RevExist,
        Constraints).

:- pred maybe_add_constraint(prog_constraint::in,
    list(prog_constraint)::in, list(prog_constraint)::out) is det.

maybe_add_constraint(Constraint, !RevConstraints) :-
    % Don't create duplicates.
    ( if list.member(Constraint, !.RevConstraints) then
        true
    else
        !:RevConstraints = [Constraint | !.RevConstraints]
    ).

%-----------------------------------------------------------------------------%

:- func mode_both_sides_to_unify_mode(module_info, mer_mode) = unify_mode.

mode_both_sides_to_unify_mode(ModuleInfo, Mode) = UnifyMode :-
    mode_get_insts(ModuleInfo, Mode, InitInst, FinalInst),
    UnifyMode = unify_modes_li_lf_ri_rf(InitInst, FinalInst,
        InitInst, FinalInst).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.higher_order.
%-----------------------------------------------------------------------------%
