%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% Copyright (C) 2014-2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module transform_hlds.higher_order.higher_order_global_info.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module libs.
:- import_module libs.optimization_options.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module counter.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module set.
:- import_module term_context.
:- import_module varset.

%---------------------------------------------------------------------------%

:- type higher_order_global_info.

:- func init_higher_order_global_info(ho_params, module_info) =
    higher_order_global_info.

:- func hogi_get_module_info(higher_order_global_info) = module_info.
:- func hogi_get_params(higher_order_global_info) = ho_params.
:- func hogi_get_goal_size_map(higher_order_global_info) = goal_size_map.
:- func hogi_get_new_pred_map(higher_order_global_info) = new_pred_map.
:- func hogi_get_version_info_map(higher_order_global_info) = version_info_map.

:- pred hogi_set_module_info(module_info::in,
    higher_order_global_info::in, higher_order_global_info::out) is det.
:- pred hogi_set_new_pred_map(new_pred_map::in,
    higher_order_global_info::in, higher_order_global_info::out) is det.
:- pred hogi_set_version_info_map(version_info_map::in,
    higher_order_global_info::in, higher_order_global_info::out) is det.

:- pred hogi_add_goal_size(pred_id::in, int::in,
    higher_order_global_info::in, higher_order_global_info::out) is det.
:- pred hogi_add_version(pred_proc_id::in, version_info::in,
    higher_order_global_info::in, higher_order_global_info::out) is det.
:- pred hogi_allocate_id(int::out,
    higher_order_global_info::in, higher_order_global_info::out) is det.

%---------------------%

:- type ho_params
    --->    ho_params(
                % Propagate higher-order constants.
                param_do_higher_order_spec  :: maybe_opt_higher_order,

                % Propagate typeinfo constants.
                param_do_type_spec          :: maybe_spec_types,

                % User-guided type specialization.
                param_do_user_type_spec     :: maybe_spec_types_user_guided,

                % Size limit on requested version.
                param_size_limit            :: int,

                % The maximum size of the higher order arguments
                % of a specialized version.
                param_arg_limit             :: int
            ).

%---------------------%

    % Stores the size of each predicate's goal. Used in the heuristic
    % to decide which preds are specialized.
    %
:- type goal_size_map == map(pred_id, int).

%---------------------%

:- type new_pred_map == map(pred_proc_id, set(new_pred)).

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

                % Unspecialised argument vars in caller, and their types.
                np_unspec_actuals       :: assoc_list(prog_var, mer_type),

                % Extra typeinfo tvars in caller.
                np_extra_act_ti_vars    :: list(tvar),

                % Caller's typevarset.
                np_call_tvarset         :: tvarset,

                % Is this a user-specified type specialization?
                np_is_user_spec         :: ho_request_kind
            ).

%---------------------%

:- type version_info_map == map(pred_proc_id, version_info).

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

%---------------------------------------------------------------------------%

    % Used to hold the value of known higher order variables.
    % If a variable is not in the map, it does not have a unique known value.
    %
:- type known_var_map == map(prog_var, known_const).

    % The list of vars is a list of the curried arguments, which must be
    % explicitly passed to the specialized predicate. For cons_ids other than
    % pred_const and `type_info', the arguments must be constants.
    % For pred_consts and type_infos, non-constant arguments are passed through
    % to any specialised version.
    %
:- type known_const
    --->    known_const(cons_id, list(prog_var)).

%---------------------------------------------------------------------------%

:- type ho_request
    --->    ho_request(
                % Calling predicate.
                rq_caller               :: pred_proc_id,

                % Called predicate.
                rq_callee               :: pred_proc_id,

                % The call's arguments, and their types.
                rq_args                 :: assoc_list(prog_var, mer_type),

                % Type variables for which extra typeinfos must be passed
                % from the caller if --typeinfo-liveness is set.
                rq_tvars                :: list(tvar),

                % Argument types in caller, other than the ones in rq_args.
                rq_ho_args              :: list(higher_order_arg),

                % Caller's typevarset.
                rq_caller_tvarset       :: tvarset,

                % Is this a user-requested specialization?
                rq_request_kind         :: ho_request_kind,

                % Context of the call which caused the request to be generated.
                rq_call_context         :: prog_context
            ).

:- type ho_request_kind
    --->    non_user_type_spec
    ;       user_type_spec.

%---------------------------------------------------------------------------%

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
                hoa_is_constant             :: is_arg_const
            ).

:- type is_arg_const
    --->    arg_is_not_const
    ;       arg_is_const.

:- pred all_higher_order_args_are_constant(list(higher_order_arg)::in)
    is semidet.

:- func both_constants(is_arg_const, is_arg_const) = is_arg_const.

%---------------------------------------------------------------------------%

:- type must_recompute
    --->    must_recompute
    ;       need_not_recompute.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% General utilities.
%

%---------------------------------------------------------------------------%

:- type match
    --->    match(
                new_pred,
                match_completeness,

                % The arguments to the specialised call.
                list(prog_var),

                % Type variables for which extra typeinfos must be added
                % to the start of the argument list.
                list(mer_type)
            ).

    % Was the match complete or partial? If partial, how many higher_order
    % arguments matched?
:- type match_completeness
    --->    complete_match
    ;       partial_match(int).

    % Check whether the request has already been implemented by the new_pred,
    % maybe ordering the list of extra type_infos in the caller predicate
    % to match up with those in the caller.
    %
:- pred version_matches(ho_params::in, module_info::in, ho_request::in,
    new_pred::in, match::out) is semidet.

%---------------------%

    % Add the curried arguments of the higher-order terms to the argument list.
    % The order here must match that generated by construct_higher_order_terms.
    %
:- pred get_extra_arguments(list(higher_order_arg)::in,
    list(prog_var)::in, list(prog_var)::out) is det.

%---------------------%

    % remove_const_higher_order_args(HOArgs, Args0, Args):
    %
    % Look through every ho arg in HOArgs. For every ho arg that is constant,
    % remove the corresponding arg from Args0, returning the result as Args.
    %
:- pred remove_const_higher_order_args(list(higher_order_arg)::in,
    list(T)::in, list(T)::out) is det.

%---------------------%

:- func mode_to_unify_mode(module_info, mer_mode) = unify_mode.

%---------------------%

:- func higher_order_max_args_size(list(higher_order_arg)) = int.
:- func higher_order_arg_size(higher_order_arg) = int.
:- func higher_order_max_args_depth(list(higher_order_arg)) = int.
:- func higher_order_arg_depth(higher_order_arg) = int.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.mode_util.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_type_unify.

:- import_module int.
:- import_module require.

%---------------------------------------------------------------------------%

:- type higher_order_global_info
    --->    higher_order_global_info(
                % This field is read-only. All the other fields
                % are read-write.
                hogi_params             :: ho_params,

                hogi_module_info        :: module_info,
                hogi_goal_size_map      :: goal_size_map,

                % Specialized versions for each predicate
                % not changed by ho_traverse_proc_body.
                hogi_new_pred_map       :: new_pred_map,

                % Extra information about each specialized version.
                hogi_version_info_map   :: version_info_map,

                % A counter for allocating sequence numbers, each of which
                % identifies one specialized predicate.
                hogi_next_id_counter    :: counter
            ).

%---------------------%

init_higher_order_global_info(Params, ModuleInfo) = Info :-
    map.init(GoalSizeMap),
    map.init(NewPredMap),
    map.init(VersionInfoMap),
    NextIdCounter = counter.init(1),
    Info = higher_order_global_info(Params, ModuleInfo, GoalSizeMap,
        NewPredMap, VersionInfoMap, NextIdCounter).

%---------------------%

:- func hogi_get_next_id_counter(higher_order_global_info) = counter.

hogi_get_module_info(Info) = X :-
    X = Info ^ hogi_module_info.
hogi_get_params(Info) = X :-
    X = Info ^ hogi_params.
hogi_get_goal_size_map(Info) = X :-
    X = Info ^ hogi_goal_size_map.
hogi_get_new_pred_map(Info) = X :-
    X = Info ^ hogi_new_pred_map.
hogi_get_version_info_map(Info) = X :-
    X = Info ^ hogi_version_info_map.
hogi_get_next_id_counter(Info) = X :-
    X = Info ^ hogi_next_id_counter.

%---------------------%

:- pred hogi_set_goal_size_map(goal_size_map::in,
    higher_order_global_info::in, higher_order_global_info::out) is det.
:- pred hogi_set_next_id_counter(counter::in,
    higher_order_global_info::in, higher_order_global_info::out) is det.

hogi_set_module_info(X, !Info) :-
    !Info ^ hogi_module_info := X.
hogi_set_goal_size_map(X, !Info) :-
    !Info ^ hogi_goal_size_map := X.
hogi_set_new_pred_map(X, !Info) :-
    !Info ^ hogi_new_pred_map := X.
hogi_set_version_info_map(X, !Info) :-
    !Info ^ hogi_version_info_map := X.
hogi_set_next_id_counter(X, !Info) :-
    !Info ^ hogi_next_id_counter := X.

%---------------------%

hogi_add_goal_size(PredId, GoalSize, !Info) :-
    GoalSizeMap0 = hogi_get_goal_size_map(!.Info),
    map.set(PredId, GoalSize, GoalSizeMap0, GoalSizeMap),
    hogi_set_goal_size_map(GoalSizeMap, !Info).

hogi_add_version(PredProcId, Version, !Info) :-
    VersionInfoMap0 = hogi_get_version_info_map(!.Info),
    map.det_insert(PredProcId, Version, VersionInfoMap0, VersionInfoMap),
    hogi_set_version_info_map(VersionInfoMap, !Info).

hogi_allocate_id(Id, !Info) :-
    Counter0 = hogi_get_next_id_counter(!.Info),
    counter.allocate(Id, Counter0, Counter),
    hogi_set_next_id_counter(Counter, !Info).

%---------------------------------------------------------------------------%

all_higher_order_args_are_constant([]).
all_higher_order_args_are_constant([Arg | Args]) :-
    Arg ^ hoa_is_constant = arg_is_const,
    all_higher_order_args_are_constant(Args).

both_constants(IsConstA, IsConstB) = IsConst :-
    ( if IsConstA = arg_is_const, IsConstB = arg_is_const then
        IsConst = arg_is_const
    else
        IsConst = arg_is_not_const
    ).

%---------------------------------------------------------------------------%

version_matches(Params, ModuleInfo, Request, Version, Match) :-
    Request = ho_request(_, CalleePredProcId, ArgsTypes0, _,
        RequestHigherOrderArgs, RequestTVarSet, _, _),
    CalleePredProcId = proc(CalleePredId, _),
    module_info_pred_info(ModuleInfo, CalleePredId, CalleePredInfo),
    Version = new_pred(_, _, _, _, VersionHigherOrderArgs, VersionArgsTypes0,
        VersionExtraTypeInfoTVars, VersionTVarSet, _),
    higher_order_args_match(RequestHigherOrderArgs,
        VersionHigherOrderArgs, HigherOrderArgs, FullOrPartial),
    (
        FullOrPartial = match_is_partial,
        % Don't accept partial matches unless the predicate is imported
        % or we are only doing user-guided type specialization.
        MatchCompleteness = complete_match
    ;
        FullOrPartial = match_is_full,
        pred_info_get_markers(CalleePredInfo, Markers),

        % Always fully specialize calls to class methods.
        % XXX This block of code was added by Simon on 2003 May 17, but
        % - the code seems to *prevent* the specialization of class methods
        %   in the match_is_full case, and
        % - it seems to have nothing to do with the log message of that diff.
        not check_marker(Markers, marker_class_method),
        not check_marker(Markers, marker_class_instance_method),

        (
            Params ^ param_do_type_spec = do_not_spec_types
        ;
            pred_info_is_imported(CalleePredInfo)
        ),

        list.length(HigherOrderArgs, NumHOArgs),
        MatchCompleteness = partial_match(NumHOArgs)
    ),

    % Rename apart type variables.
    tvarset_merge_renaming(RequestTVarSet, VersionTVarSet, _, TVarRenaming),
    assoc_list.values(VersionArgsTypes0, VersionArgTypes0),
    apply_variable_renaming_to_type_list(TVarRenaming,
        VersionArgTypes0, VersionArgTypes),
    assoc_list.keys_and_values(ArgsTypes0, Args0, ArgTypes),

    type_list_subsumes(VersionArgTypes, ArgTypes, TypeSubn),

    require_det (
        % We know the version matches; work out the details.
        % Specifically, work out the types of the extra typeinfo variables
        % that we need to pass to the specialized version.
        %
        % XXX kind inference:
        % we assume all tvars have kind `star'

        map.init(KindMap),
        apply_variable_renaming_to_tvar_kind_map(TVarRenaming, KindMap,
            RenamedKindMap),
        apply_variable_renaming_to_tvar_list(TVarRenaming,
            VersionExtraTypeInfoTVars, ExtraTypeInfoTVars0),
        apply_rec_subst_to_tvar_list(RenamedKindMap, TypeSubn,
            ExtraTypeInfoTVars0, ExtraTypeInfoTypes),
        get_extra_arguments(HigherOrderArgs, Args0, Args),

        Match = match(Version, MatchCompleteness, Args, ExtraTypeInfoTypes)
    ).

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
        some [RequestArg] (
            list.member(RequestArg, RequestArgs),
            RequestArg ^ hoa_cons_id = closure_cons(_, _)
        )
    ).
higher_order_args_match([RequestArg | RequestArgs], [VersionArg | VersionArgs],
        Args, FullOrPartial) :-
    RequestArg = higher_order_arg(ConsIdR, ArgNoR, _, _, _, _, _,
        RequestIsConst),
    VersionArg = higher_order_arg(ConsIdV, ArgNoV, _, _, _, _, _,
        VersionIsConst),

    ( if ArgNoR = ArgNoV then
        ConsIdR = ConsIdV,
        RequestArg = higher_order_arg(_, _, NumArgs,
            CurriedArgs, CurriedArgTypes, CurriedArgRttiInfo,
            HOCurriedRequestArgs, _),
        VersionArg = higher_order_arg(_, _, NumArgs, _, _, _,
            HOCurriedVersionArgs, _),
        higher_order_args_match(HOCurriedRequestArgs, HOCurriedVersionArgs,
            NewHOCurriedArgs, FullOrPartial),
        higher_order_args_match(RequestArgs, VersionArgs, TailArgs, _),
        NewRequestArg = higher_order_arg(ConsIdR, ArgNoR, NumArgs,
            CurriedArgs, CurriedArgTypes, CurriedArgRttiInfo,
            NewHOCurriedArgs, both_constants(RequestIsConst, VersionIsConst)),
        Args = [NewRequestArg | TailArgs]
    else
        % Typeinfo arguments that are present in the request may be
        % missing from the version, if we are doing user-guided type
        % specialization. All of the arguments in the version must be present
        % in the request for a match.
        ArgNoR < ArgNoV,

        % All the higher-order arguments in the request must be present
        % in the version for a match. If there is no such version, we should
        % create a new one.
        ConsIdR \= closure_cons(_, _),
        higher_order_args_match(RequestArgs, [VersionArg | VersionArgs],
            Args, _),
        FullOrPartial = match_is_partial
    ).

%---------------------------------------------------------------------------%

get_extra_arguments(HOArgs, Args0, ExtraArgs ++ Args) :-
    get_extra_arguments_2(HOArgs, ExtraArgs),
    remove_const_higher_order_args(HOArgs, Args0, Args).

:- pred get_extra_arguments_2(list(higher_order_arg)::in, list(prog_var)::out)
    is det.

get_extra_arguments_2([], []).
get_extra_arguments_2([HOArg | HOArgs], Args) :-
    HOArg = higher_order_arg(_, _, _, CurriedArgs0, _, _, HOCurriedArgs,
        IsConst),
    (
        IsConst = arg_is_const,
        % If this argument is constant, all its subterms must be constant,
        % so there won't be anything more to add.
        get_extra_arguments_2(HOArgs, Args)
    ;
        IsConst = arg_is_not_const,
        remove_const_higher_order_args(HOCurriedArgs,
            CurriedArgs0, CurriedArgs),
        get_extra_arguments_2(HOCurriedArgs, ExtraCurriedArgs),
        get_extra_arguments_2(HOArgs, Args1),
        list.condense([CurriedArgs, ExtraCurriedArgs, Args1], Args)
    ).

%---------------------------------------------------------------------------%

remove_const_higher_order_args(HOArgs, Args0, Args) :-
    remove_const_higher_order_args_loop(1, HOArgs, Args0, Args).

:- pred remove_const_higher_order_args_loop(int::in,
    list(higher_order_arg)::in, list(T)::in, list(T)::out) is det.

remove_const_higher_order_args_loop(_, _, [], []).
remove_const_higher_order_args_loop(Index, HOArgs,
        [HeadArg0 | TailArgs0], Args) :-
    (
        HOArgs = [HeadHOArg | TailHOArgs],
        HeadHOArg =
            higher_order_arg(_, HeadHOIndex, _, _, _, _, _, HeadIsConst),
        ( if HeadHOIndex = Index then
            remove_const_higher_order_args_loop(Index + 1, TailHOArgs,
                TailArgs0, TailArgs),
            (
                HeadIsConst = arg_is_const,
                Args = TailArgs
            ;
                HeadIsConst = arg_is_not_const,
                Args = [HeadArg0 | TailArgs]
            )
        else if HeadHOIndex > Index then
            remove_const_higher_order_args_loop(Index + 1, HOArgs,
                TailArgs0, TailArgs),
            Args = [HeadArg0 | TailArgs]
        else
            unexpected($pred, "unordered indexes")
        )
    ;
        HOArgs = [],
        Args = [HeadArg0 | TailArgs0]
    ).

%---------------------------------------------------------------------------%

mode_to_unify_mode(ModuleInfo, Mode) = UnifyMode :-
    mode_get_insts(ModuleInfo, Mode, InitInst, FinalInst),
    UnifyMode = unify_modes_li_lf_ri_rf(InitInst, FinalInst,
        InitInst, FinalInst).

%---------------------------------------------------------------------------%

higher_order_max_args_size(Args) =
    list.foldl(int.max, list.map(higher_order_arg_size, Args), 0).

higher_order_arg_size(HOArg) =
    1 + higher_order_total_args_size(HOArg ^ hoa_known_curry_args).

:- func higher_order_total_args_size(list(higher_order_arg)) = int.

higher_order_total_args_size(Args) =
    list.foldl(int.plus, list.map(higher_order_arg_size, Args), 0).

%---------------------%

higher_order_max_args_depth(Args) =
    list.foldl(int.max, list.map(higher_order_arg_depth, Args), 0).

higher_order_arg_depth(HOArg) =
    1 + higher_order_max_args_depth(HOArg ^ hoa_known_curry_args).

%---------------------------------------------------------------------------%
:- end_module transform_hlds.higher_order.higher_order_global_info.
%---------------------------------------------------------------------------%
