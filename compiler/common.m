%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Original author: squirrel (Jane Anna Langley).
% Some bugs fixed by fjh.
% Extensive revision by zs.
% More revision by stayl.
%
% This module attempts to optimise out instances where a variable is
% decomposed and then soon after reconstructed from the parts. If possible
% we would like to "short-circuit" this process.
% It also optimizes deconstructions of known cells, replacing them with
% assignments to the arguments where this is guaranteed to not increase
% the number of stack slots required by the goal.
% Repeated calls to predicates with the same input arguments are replaced by
% assigments and warnings are returned.
%
% IMPORTANT: This module does a small subset of the job of compile-time
% garbage collection, but it does so without paying attention to uniqueness
% information, since the compiler does not yet have such information.
% Once we implement ctgc, the assumptions made by this module will have
% to be revisited.
%
%---------------------------------------------------------------------------%

:- module check_hlds__common.
:- interface.

:- import_module check_hlds__simplify.
:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_pred.
:- import_module parse_tree__prog_data.

:- import_module list.

    % If we find a deconstruction or a construction we cannot optimize,
    % record the details of the memory cell in CommonInfo.
    %
    % If we find a construction that constructs a cell identical to one
    % we have seen before, replace the construction with an assignment
    % from the variable unified with that cell.
    %
:- pred common__optimise_unification(unification::in, prog_var::in,
    unify_rhs::in, unify_mode::in, unify_context::in,
    hlds_goal_expr::in, hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out,
    simplify_info::in, simplify_info::out) is det.

    % Check whether this call has been seen before and is replaceable, if
    % so produce assignment unification for the non-local output variables,
    % and give a warning.
    % A call is considered replaceable if it has no uniquely moded outputs
    % and no destructive inputs.
    % It is the caller's responsibility to check that the call is pure.
    %
:- pred common__optimise_call(pred_id::in, proc_id::in, list(prog_var)::in,
    hlds_goal_info::in, hlds_goal_expr::in, hlds_goal_expr::out,
    simplify_info::in, simplify_info::out) is det.

:- pred common__optimise_higher_order_call(prog_var::in, list(prog_var)::in,
    list(mode)::in, determinism::in, hlds_goal_info::in,
    hlds_goal_expr::in, hlds_goal_expr::out,
    simplify_info::in, simplify_info::out) is det.

    % Succeeds if the two variables are equivalent according to the specified
    % equivalence class.
    %
:- pred common__vars_are_equivalent(prog_var::in, prog_var::in,
    common_info::in) is semidet.

    % Assorted stuff used here that simplify.m doesn't need to know about.
    %
:- type common_info.
:- func common_info_init = common_info.

    % Clear the list of structs seen since the last stack flush.
    %
:- pred common_info_clear_structs(common_info::in, common_info::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds__det_report.
:- import_module check_hlds__det_util.
:- import_module check_hlds__inst_match.
:- import_module check_hlds__mode_util.
:- import_module check_hlds__polymorphism.
:- import_module check_hlds__type_util.
:- import_module hlds__goal_util.
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_module.
:- import_module hlds__instmap.
:- import_module hlds__quantification.
:- import_module libs__globals.
:- import_module libs__options.
:- import_module parse_tree__error_util.
:- import_module parse_tree__prog_util.
:- import_module parse_tree__prog_type.
:- import_module transform_hlds__pd_cost.

:- import_module bool.
:- import_module eqvclass.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module string.
:- import_module sveqvclass.
:- import_module svmap.
:- import_module term.

    % The var_eqv field records information about which sets of variables
    % are known to be equivalent, usually because they have been unified.
    % This is useful when eliminating duplicate unifications and when
    % eliminating duplicate calls.
    %
    % The all_structs and since_call_structs fields record information
    % about the memory cells available for reuse. The all_structs field
    % has info about all the cells available at the current program point.
    % The since_call_structs field contains info about the subset of these
    % cells that have been seen since the last stack flush, which is
    % usually a call.
    %
    % The reason why we make the distinction between structs seen before
    % the last call and structs seen after is best explained by these two
    % program fragments:
    %
    % fragment 1:
    %   X => f(A1, A2, A3, A4),
    %   X => f(B1, B2, B3, B4),
    %
    % fragment 2:
    %   X => f(A1, A2, A3, A4),
    %   p(...),
    %   X => f(B1, B2, B3, B4),
    %
    % In fragment 1, we want to replace the second deconstruction with
    % the assignments B1 = A1, ... B4 = A4, since this can avoid the
    % second check of X's function symbol. (If the inst of X at the start
    % of the second unification is `bound(f(...))', we can dispense with
    % this test anyway, but if the two unifications are brought together
    % by inlining, then X's inst then may simply be `ground'.)
    %
    % In fragment 2, we don't want make the same transformation, because
    % doing so would require storing A1 ... A4 across the call instead of
    % just X.
    %
    % If the second unification were a construction instead of a
    % deconstruction, we want to make the transformation in both cases,
    % because the heap allocation we thus avoid is quite expensive,
    % and because it actually reduces the number of stack slots we need
    % across the call (X instead of A1 .. A4). The exception is
    % constructions using function symbols of arity zero, which we
    % never need to eliminate. We process unifications with constants
    % only to update our information about variable equivalences: after
    % X = c and Y = c, X and Y are equivalent.
    %
    % The seen_calls field records which calls we have seen, which we use
    % to eliminate duplicate calls.

:- type common_info
    --->    common_info(
                var_eqv                 :: eqvclass(prog_var),
                all_structs             :: struct_map,
                since_call_structs      :: struct_map,
                seen_calls              :: seen_calls
            ).

    % A struct_map maps a principal type constructor and a cons_id of that
    % type to information about cells involving that cons_id.
    %
    % The reason why we need the principal type constructors is that two
    % syntactially identical structures have compatible representations
    % if and only if their principal type constructors are the same.
    % For example, if we have
    %
    %   :- type maybe_err(T) --> ok(T) ; err(string).
    %
    %   :- pred p(maybe_err(foo)::in, maybe_err(bar)::out) is semidet.
    %   p(err(X), err(X)).
    %
    % then we want to reuse the `err(X)' in the first arg rather than
    % constructing a new copy of it for the second arg.
    % The two occurrences of `err(X)' have types `maybe_err(int)'
    % and `maybe(float)', but we know that they have the same
    % representation.
    %
    % We put the cons_id first in the pair because there are more cons_ids
    % than type constructors, and hence comparisons involving cons_ids are
    % more likely to fail. This should ensure that failed comparisons in map
    % searches fail as soon as possible.

:- type cons_id_map  ==  map(cons_id, structures).
:- type struct_map  ==  map(type_ctor, cons_id_map).

    % Given a unification X = f(Y1, ... Yn), we record its availability for
    % reuse by creating structure(X, [Y1, ... Yn]), and putting it at the
    % front of the list of structures for the entry for f and X's type_ctor.

:- type structures == list(structure).
:- type structure
    --->    structure(prog_var, list(prog_var)).

:- type seen_calls  ==  map(seen_call_id, list(call_args)).

:- type call_args
    --->    call_args(
                prog_context,       % The context of the call, for use in
                                    % warnings about % duplicate calls.
                list(prog_var),     % The input arguments. For higher-order
                                    % calls, the closure is the first input
                                    % argument.
                list(prog_var)      % The output arguments.
            ).

%---------------------------------------------------------------------------%

common_info_init = CommonInfo :-
    eqvclass__init(VarEqv0),
    map__init(StructMap0),
    map__init(SeenCalls0),
    CommonInfo = common_info(VarEqv0, StructMap0, StructMap0, SeenCalls0).

common_info_clear_structs(!Info) :-
    !:Info = !.Info ^ since_call_structs := map__init.

%---------------------------------------------------------------------------%

common__optimise_unification(Unification0, _Left0, _Right0, Mode, _Context,
        Goal0, Goal, GoalInfo0, GoalInfo, !Info) :-
    (
        Unification0 = construct(Var, ConsId, ArgVars, _, _, _, SubInfo),
        (
            SubInfo = construct_sub_info(MaybeTakeAddr, _),
            MaybeTakeAddr = yes(_)
        ->
            Goal = Goal0,
            GoalInfo = GoalInfo0
        ;
            common__optimise_construct(Var, ConsId, ArgVars, Mode,
                Goal0, Goal, GoalInfo0, GoalInfo, !Info)
        )
    ;
        Unification0 = deconstruct(Var, ConsId, ArgVars, UniModes, CanFail, _),
        common__optimise_deconstruct(Var, ConsId, ArgVars, UniModes, CanFail,
            Mode, Goal0, Goal, GoalInfo0, GoalInfo, !Info)
    ;
        Unification0 = assign(Var1, Var2),
        common__record_equivalence(Var1, Var2, !Info),
        Goal = Goal0,
        GoalInfo = GoalInfo0
    ;
        Unification0 = simple_test(Var1, Var2),
        common__record_equivalence(Var1, Var2, !Info),
        Goal = Goal0,
        GoalInfo = GoalInfo0
    ;
        Unification0 = complicated_unify(_, _, _),
        Goal = Goal0,
        GoalInfo = GoalInfo0
    ).

:- pred common__optimise_construct(prog_var::in, cons_id::in,
    list(prog_var)::in, unify_mode::in,
    hlds_goal_expr::in, hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out,
    simplify_info::in, simplify_info::out) is det.

common__optimise_construct(Var, ConsId, ArgVars, Mode, Goal0, Goal,
        GoalInfo0, GoalInfo, !Info) :-
    Mode = LVarMode - _,
    simplify_info_get_module_info(!.Info, ModuleInfo),
    mode_get_insts(ModuleInfo, LVarMode, _, Inst),
    (
            % Don't optimise partially instantiated deconstruction
            % unifications, because it's tricky to work out how to mode
            % the replacement asssignment unifications. In the vast
            % majority of cases, the variable is ground.
        \+ inst_is_ground(ModuleInfo, Inst)
    ->
        Goal = Goal0,
        GoalInfo = GoalInfo0
    ;
        TypeCtor = lookup_var_type_ctor(!.Info, Var),
        simplify_info_get_common_info(!.Info, CommonInfo0),
        VarEqv0 = CommonInfo0 ^ var_eqv,
        list__map_foldl(eqvclass__ensure_element_partition_id,
            ArgVars, ArgVarIds, VarEqv0, VarEqv1),
        AllStructMap0 = CommonInfo0 ^ all_structs,
        (
            % common__generate_assign assumes that the output variable
            % is in the instmap_delta, which will not be true if the
            % variable is local to the unification. The optimization
            % is pointless in that case.
            goal_info_get_instmap_delta(GoalInfo0, InstMapDelta),
            instmap_delta_search_var(InstMapDelta, Var, _),

            map__search(AllStructMap0, TypeCtor, ConsIdMap0),
            map__search(ConsIdMap0, ConsId, Structs),
            find_matching_cell_construct(Structs, VarEqv1, ArgVarIds,
                OldStruct)
        ->
            OldStruct = structure(OldVar, _),
            sveqvclass__ensure_equivalence(Var, OldVar, VarEqv1, VarEqv),
            CommonInfo = CommonInfo0 ^ var_eqv := VarEqv,
            simplify_info_set_common_info(CommonInfo, !Info),
            (
                ArgVars = [],
                % Constants don't use memory, so there's no point in
                % optimizing away their construction; in fact, doing so
                % could cause more stack usage.
                Goal = Goal0,
                GoalInfo = GoalInfo0
            ;
                ArgVars = [_ | _],
                UniMode = ((free - Inst) -> (Inst - Inst)),
                common__generate_assign(Var, OldVar, UniMode, GoalInfo0,
                    Goal - GoalInfo, !Info),
                simplify_info_set_requantify(!Info),
                pd_cost__goal(Goal0 - GoalInfo0, Cost),
                simplify_info_incr_cost_delta(Cost, !Info)
            )
        ;
            Goal = Goal0,
            GoalInfo = GoalInfo0,
            Struct = structure(Var, ArgVars),
            record_cell_in_maps(TypeCtor, ConsId, Struct, VarEqv1, !Info)
        )
    ).

:- pred common__optimise_deconstruct(prog_var::in, cons_id::in,
    list(prog_var)::in, list(uni_mode)::in, can_fail::in, unify_mode::in,
    hlds_goal_expr::in, hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out,
    simplify_info::in, simplify_info::out) is det.

common__optimise_deconstruct(Var, ConsId, ArgVars, UniModes, CanFail, Mode,
        Goal0, Goal, GoalInfo0, GoalInfo, !Info) :-
    simplify_info_get_module_info(!.Info, ModuleInfo),
    (
            % Don't optimise partially instantiated deconstruction
            % unifications, because it's tricky to work out how to mode
            % the replacement asssignment unifications. In the vast
            % majority of cases, the variable is ground.
        Mode = LVarMode - _,
        mode_get_insts(ModuleInfo, LVarMode, Inst0, _),
        \+ inst_is_ground(ModuleInfo, Inst0)
    ->
        Goal = Goal0
    ;
        TypeCtor = lookup_var_type_ctor(!.Info, Var),
        simplify_info_get_common_info(!.Info, CommonInfo0),
        VarEqv0 = CommonInfo0 ^ var_eqv,
        eqvclass__ensure_element_partition_id(Var, VarId, VarEqv0, VarEqv1),
        SinceCallStructMap0 = CommonInfo0 ^ since_call_structs,
        (
            % Do not delete deconstruction unifications inserted by
            % stack_opt.m, which has done a more comprehensive cost
            % analysis than common.m can do.
            \+ goal_info_has_feature(GoalInfo, stack_opt),
            \+ goal_info_has_feature(GoalInfo, tuple_opt),

            map__search(SinceCallStructMap0, TypeCtor, ConsIdMap0),
            map__search(ConsIdMap0, ConsId, Structs),
            find_matching_cell_deconstruct(Structs, VarEqv1, VarId, OldStruct)
        ->
            OldStruct = structure(_, OldArgVars),
            eqvclass__ensure_corresponding_equivalences(ArgVars,
                OldArgVars, VarEqv1, VarEqv),
            CommonInfo = CommonInfo0 ^ var_eqv := VarEqv,
            simplify_info_set_common_info(CommonInfo, !Info),
            common__create_output_unifications(GoalInfo0, ArgVars, OldArgVars,
                UniModes, Goals, !Info),
            Goal = conj(Goals),
            pd_cost__goal(Goal0 - GoalInfo0, Cost),
            simplify_info_incr_cost_delta(Cost, !Info),
            simplify_info_set_requantify(!Info),
            (
                CanFail = can_fail,
                simplify_info_set_rerun_det(!Info)
            ;
                CanFail = cannot_fail
            )
        ;
            Goal = Goal0,
            Struct = structure(Var, ArgVars),
            record_cell_in_maps(TypeCtor, ConsId, Struct, VarEqv1, !Info)
        )
    ),
    GoalInfo = GoalInfo0.

:- func lookup_var_type_ctor(simplify_info, prog_var) = type_ctor.

lookup_var_type_ctor(Info, Var) = TypeCtor :-
    simplify_info_get_var_types(Info, VarTypes),
    map__lookup(VarTypes, Var, Type),
    ( type_to_ctor_and_args(Type, TypeCtorPrime, _) ->
        TypeCtor = TypeCtorPrime
    ;
        % If we unify a variable with a function symbol, we *must* know
        % what the principal type constructor of its type is.
        error("lookup_var_type_ctor: cannot find type_ctor")
    ).

%---------------------------------------------------------------------------%

:- pred find_matching_cell_construct(structures::in, eqvclass(prog_var)::in,
    list(partition_id)::in, structure::out) is semidet.

find_matching_cell_construct([Struct | Structs], VarEqv, ArgVarIds, Match) :-
    Struct = structure(_Var, Vars),
    ( common__ids_vars_match(ArgVarIds, Vars, VarEqv) ->
        Match = Struct
    ;
        find_matching_cell_construct(Structs, VarEqv, ArgVarIds, Match)
    ).

:- pred find_matching_cell_deconstruct(structures::in, eqvclass(prog_var)::in,
    partition_id::in, structure::out) is semidet.

find_matching_cell_deconstruct([Struct | Structs], VarEqv, VarId, Match) :-
    Struct = structure(Var, _Vars),
    ( common__id_var_match(VarId, Var, VarEqv) ->
        Match = Struct
    ;
        find_matching_cell_deconstruct(Structs, VarEqv, VarId, Match)
    ).

:- pred common__ids_vars_match(list(partition_id)::in, list(prog_var)::in,
    eqvclass(prog_var)::in) is semidet.

common__ids_vars_match([], [], _VarEqv).
common__ids_vars_match([Id | Ids], [Var | Vars], VarEqv) :-
    common__id_var_match(Id, Var, VarEqv),
    common__ids_vars_match(Ids, Vars, VarEqv).

:- pragma inline(common__id_var_match/3).
:- pred common__id_var_match(partition_id::in, prog_var::in,
    eqvclass(prog_var)::in) is semidet.

common__id_var_match(Id, Var, VarEqv) :-
    eqvclass__partition_id(VarEqv, Var, VarId),
    Id = VarId.

%---------------------------------------------------------------------------%

:- pred record_cell_in_maps(type_ctor::in, cons_id::in, structure::in,
    eqvclass(prog_var)::in, simplify_info::in, simplify_info::out) is det.

record_cell_in_maps(TypeCtor, ConsId, Struct, VarEqv, !Info) :-
    some [!CommonInfo] (
        simplify_info_get_common_info(!.Info, !:CommonInfo),
        AllStructMap0 = !.CommonInfo ^ all_structs,
        SinceCallStructMap0 = !.CommonInfo ^ since_call_structs,
        common__do_record_cell_in_struct_map(TypeCtor, ConsId, Struct,
            AllStructMap0, AllStructMap),
        common__do_record_cell_in_struct_map(TypeCtor, ConsId, Struct,
            SinceCallStructMap0, SinceCallStructMap),
        !:CommonInfo = !.CommonInfo ^ var_eqv := VarEqv,
        !:CommonInfo = !.CommonInfo ^ all_structs := AllStructMap,
        !:CommonInfo = !.CommonInfo ^ since_call_structs := SinceCallStructMap,
        simplify_info_set_common_info(!.CommonInfo, !Info)
    ).

:- pred common__do_record_cell_in_struct_map(type_ctor::in, cons_id::in,
    structure::in, struct_map::in, struct_map::out) is det.

common__do_record_cell_in_struct_map(TypeCtor, ConsId, Struct, !StructMap) :-
    ( map__search(!.StructMap, TypeCtor, ConsIdMap0) ->
        ( map__search(ConsIdMap0, ConsId, Structs0) ->
            Structs = [Struct | Structs0],
            map__det_update(ConsIdMap0, ConsId, Structs, ConsIdMap)
        ;
            map__det_insert(ConsIdMap0, ConsId, [Struct], ConsIdMap)
        ),
        svmap__det_update(TypeCtor, ConsIdMap, !StructMap)
    ;
        map__det_insert(map__init, ConsId, [Struct], ConsIdMap),
        svmap__det_insert(TypeCtor, ConsIdMap, !StructMap)
    ).

%---------------------------------------------------------------------------%

:- pred common__record_equivalence(prog_var::in, prog_var::in,
    simplify_info::in, simplify_info::out) is det.

common__record_equivalence(Var1, Var2, !Info) :-
    simplify_info_get_common_info(!.Info, CommonInfo0),
    VarEqv0 = CommonInfo0 ^ var_eqv,
    eqvclass__ensure_equivalence(VarEqv0, Var1, Var2, VarEqv),
    CommonInfo = CommonInfo0 ^ var_eqv := VarEqv,
    simplify_info_set_common_info(CommonInfo, !Info).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

common__optimise_call(PredId, ProcId, Args, GoalInfo, Goal0, Goal, !Info) :-
    (
        goal_info_get_determinism(GoalInfo, Det),
        common__check_call_detism(Det),
        simplify_info_get_var_types(!.Info, VarTypes),
        simplify_info_get_module_info(!.Info, ModuleInfo),
        module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
        proc_info_argmodes(ProcInfo, ArgModes),
        common__partition_call_args(VarTypes, ModuleInfo, ArgModes, Args,
            InputArgs, OutputArgs, OutputModes)
    ->
        common__optimise_call_2(seen_call(PredId, ProcId), InputArgs,
            OutputArgs, OutputModes, GoalInfo, Goal0, Goal, !Info)
    ;
        Goal = Goal0
    ).

common__optimise_higher_order_call(Closure, Args, Modes, Det, GoalInfo,
        Goal0, Goal, !Info) :-
    (
        common__check_call_detism(Det),
        simplify_info_get_var_types(!.Info, VarTypes),
        simplify_info_get_module_info(!.Info, ModuleInfo),
        common__partition_call_args(VarTypes, ModuleInfo, Modes, Args,
            InputArgs, OutputArgs, OutputModes)
    ->
        common__optimise_call_2(higher_order_call, [Closure | InputArgs],
            OutputArgs, OutputModes, GoalInfo, Goal0, Goal, !Info)
    ;
        Goal = Goal0
    ).

:- pred common__check_call_detism(determinism::in) is semidet.

common__check_call_detism(Det) :-
    determinism_components(Det, _, SolnCount),
    % Replacing nondet or mulidet calls would cause
    % loss of solutions.
    ( SolnCount = at_most_one
    ; SolnCount = at_most_many_cc
    ).

:- pred common__optimise_call_2(seen_call_id::in, list(prog_var)::in,
    list(prog_var)::in, list(mode)::in, hlds_goal_info::in,
    hlds_goal_expr::in, hlds_goal_expr::out,
    simplify_info::in, simplify_info::out) is det.

common__optimise_call_2(SeenCall, InputArgs, OutputArgs, Modes, GoalInfo,
        Goal0, Goal, !Info) :-
    simplify_info_get_common_info(!.Info, CommonInfo0),
    Eqv0 = CommonInfo0 ^ var_eqv,
    SeenCalls0 = CommonInfo0 ^ seen_calls,
    ( map__search(SeenCalls0, SeenCall, SeenCallsList0) ->
        (
            common__find_previous_call(SeenCallsList0, InputArgs, Eqv0,
                OutputArgs2, PrevContext)
        ->
            simplify_info_get_module_info(!.Info, ModuleInfo),
            modes_to_uni_modes(ModuleInfo, Modes, Modes, UniModes),
            common__create_output_unifications(GoalInfo,
                OutputArgs, OutputArgs2, UniModes, Goals, !Info),
            Goal = conj(Goals),
            simplify_info_get_var_types(!.Info, VarTypes),
            (
                simplify_do_warn_calls(!.Info),
                % Don't warn for cases such as:
                % set__init(Set1 : set(int)),
                % set__init(Set2 : set(float)).
                map__apply_to_list(OutputArgs, VarTypes, OutputArgTypes1),
                map__apply_to_list(OutputArgs2, VarTypes, OutputArgTypes2),
                common__types_match_exactly_list(
                    OutputArgTypes1, OutputArgTypes2)
            ->
                goal_info_get_context(GoalInfo, Context),
                simplify_info_do_add_msg(
                    duplicate_call(SeenCall, PrevContext, Context), !Info)
            ;
                true
            ),
            CommonInfo = CommonInfo0,
            pd_cost__goal(Goal0 - GoalInfo, Cost),
            simplify_info_incr_cost_delta(Cost, !Info),
            simplify_info_set_requantify(!Info),
            goal_info_get_determinism(GoalInfo, Detism0),
            ( Detism0 \= det ->
                simplify_info_set_rerun_det(!Info)
            ;
                true
            )
        ;
            goal_info_get_context(GoalInfo, Context),
            ThisCall = call_args(Context, InputArgs, OutputArgs),
            map__det_update(SeenCalls0, SeenCall,
                [ThisCall | SeenCallsList0], SeenCalls),
            CommonInfo = CommonInfo0 ^ seen_calls := SeenCalls,
            Goal = Goal0
        )
    ;
        goal_info_get_context(GoalInfo, Context),
        ThisCall = call_args(Context, InputArgs, OutputArgs),
        map__det_insert(SeenCalls0, SeenCall, [ThisCall], SeenCalls),
        CommonInfo = CommonInfo0 ^ seen_calls := SeenCalls,
        Goal = Goal0
    ),
    simplify_info_set_common_info(CommonInfo, !Info).

%---------------------------------------------------------------------------%

    % Partition the arguments of a call into inputs and outputs,
    % failing if any of the outputs have a unique component
    % or if any of the outputs contain any `any' insts.
:- pred common__partition_call_args(vartypes::in, module_info::in,
    list(mode)::in, list(prog_var)::in, list(prog_var)::out,
    list(prog_var)::out, list(mode)::out) is semidet.

common__partition_call_args(_, _, [], [_ | _], _, _, _) :-
    error("common__partition_call_args").
common__partition_call_args(_, _, [_ | _], [], _, _, _) :-
    error("common__partition_call_args").
common__partition_call_args(_, _, [], [], [], [], []).
common__partition_call_args(VarTypes, ModuleInfo, [ArgMode | ArgModes],
        [Arg | Args], InputArgs, OutputArgs, OutputModes) :-
    common__partition_call_args(VarTypes, ModuleInfo, ArgModes, Args,
        InputArgs1, OutputArgs1, OutputModes1),
    mode_get_insts(ModuleInfo, ArgMode, InitialInst, FinalInst),
    map__lookup(VarTypes, Arg, Type),
    ( inst_matches_binding(InitialInst, FinalInst, Type, ModuleInfo) ->
        InputArgs = [Arg | InputArgs1],
        OutputArgs = OutputArgs1,
        OutputModes = OutputModes1
    ;
        % Calls with partly unique outputs cannot be replaced,
        % since a unique copy of the outputs must be produced.
        inst_is_not_partly_unique(ModuleInfo, FinalInst),

        % Don't optimize calls whose outputs include any
        % `any' insts, since that would create false aliasing
        % between the different variables.
        % (inst_matches_binding applied to identical insts
        % fails only for `any' insts.)
        inst_matches_binding(FinalInst, FinalInst, Type, ModuleInfo),

        % Don't optimize calls where a partially instantiated
        % variable is further instantiated. That case is difficult
        % to test properly because mode analysis currently
        % rejects most potential test cases.
        inst_is_free(ModuleInfo, InitialInst),

        InputArgs = InputArgs1,
        OutputArgs = [Arg | OutputArgs1],
        OutputModes = [ArgMode | OutputModes1]
    ).

%---------------------------------------------------------------------------%

:- pred common__find_previous_call(list(call_args)::in, list(prog_var)::in,
    eqvclass(prog_var)::in, list(prog_var)::out,
    prog_context::out) is semidet.

common__find_previous_call([SeenCall | SeenCalls], InputArgs,
        Eqv, OutputArgs2, PrevContext) :-
    SeenCall = call_args(PrevContext, InputArgs1, OutputArgs1),
    ( common__var_lists_are_equiv(InputArgs, InputArgs1, Eqv) ->
        OutputArgs2 = OutputArgs1
    ;
        common__find_previous_call(SeenCalls, InputArgs, Eqv,
            OutputArgs2, PrevContext)
    ).

%---------------------------------------------------------------------------%

    % succeeds if the two lists of variables are equivalent
    % according to the specified equivalence class.
:- pred common__var_lists_are_equiv(list(prog_var)::in, list(prog_var)::in,
    eqvclass(prog_var)::in) is semidet.

common__var_lists_are_equiv([], [], _VarEqv).
common__var_lists_are_equiv([X | Xs], [Y | Ys], VarEqv) :-
    common__vars_are_equiv(X, Y, VarEqv),
    common__var_lists_are_equiv(Xs, Ys, VarEqv).

common__vars_are_equivalent(X, Y, CommonInfo) :-
    EqvVars = CommonInfo ^ var_eqv,
    common__vars_are_equiv(X, Y, EqvVars).

    % succeeds if the two variables are equivalent
    % according to the specified equivalence class.
:- pred common__vars_are_equiv(prog_var::in, prog_var::in,
    eqvclass(prog_var)::in) is semidet.

common__vars_are_equiv(X, Y, VarEqv) :-
    (
        X = Y
    ;
        eqvclass__partition_id(VarEqv, X, Id),
        eqvclass__partition_id(VarEqv, Y, Id)
    ).

%---------------------------------------------------------------------------%

:- pred common__create_output_unifications(hlds_goal_info::in,
    list(prog_var)::in, list(prog_var)::in, list(uni_mode)::in,
    list(hlds_goal)::out, simplify_info::in,
    simplify_info::out) is det.

    % Create unifications to assign the vars in OutputArgs from
    % the corresponding var in OldOutputArgs.
    % This needs to be done even if OutputArg is not a nonlocal in
    % the original goal because later goals in the conjunction may
    % match against the cell and need all the output arguments.
    % The unneeded assignments will be removed later.

common__create_output_unifications(GoalInfo, OutputArgs, OldOutputArgs,
        UniModes, Goals, !Info) :-
    (
        OutputArgs = [OutputArg | OutputArgsTail],
        OldOutputArgs = [OldOutputArg | OldOutputArgsTail],
        UniModes = [UniMode | UniModesTail]
    ->
        (
            % This can happen if the first cell was created
            % with a partially instantiated deconstruction.
            OutputArg \= OldOutputArg
        ->
            common__generate_assign(OutputArg, OldOutputArg, UniMode, GoalInfo,
                Goal, !Info),
            common__create_output_unifications(GoalInfo,
                OutputArgsTail, OldOutputArgsTail, UniModesTail,
                GoalsTail, !Info),
            Goals = [Goal | GoalsTail]
        ;
            common__create_output_unifications(GoalInfo,
                OutputArgsTail, OldOutputArgsTail,
                UniModesTail, Goals, !Info)
        )
    ;
        OutputArgs = [],
        OldOutputArgs = [],
        UniModes = []
    ->
        Goals = []
    ;
        error("common__create_output_unifications: mode mismatch")
    ).

%---------------------------------------------------------------------------%

:- pred common__generate_assign(prog_var::in, prog_var::in, uni_mode::in,
    hlds_goal_info::in, hlds_goal::out, simplify_info::in, simplify_info::out)
    is det.

common__generate_assign(ToVar, FromVar, UniMode, _, Goal, !Info) :-
    apply_induced_tsubst(ToVar, FromVar, !Info),
    simplify_info_get_var_types(!.Info, VarTypes),
    map__lookup(VarTypes, ToVar, ToVarType),
    map__lookup(VarTypes, FromVar, FromVarType),

    set__list_to_set([ToVar, FromVar], NonLocals),
    UniMode = ((_ - ToVarInst0) -> (_ - ToVarInst)),
    ( common__types_match_exactly(ToVarType, FromVarType) ->
        UnifyMode = (ToVarInst0 -> ToVarInst) - (ToVarInst -> ToVarInst),
        UnifyContext = unify_context(explicit, []),
        GoalExpr = unify(ToVar, var(FromVar), UnifyMode,
            assign(ToVar, FromVar), UnifyContext)
    ;
        % If the cells we are optimizing don't have exactly the same
        % type, we insert explicit type casts to ensure type
        % correctness. This avoids problems with HLDS optimizations
        % such as inlining which expect the HLDS to be well-typed.
        % Unfortunately this loses information for other optimizations,
        % since the call to the type cast hides the equivalence of
        % the input and output.
        Modes = [(ToVarInst -> ToVarInst), (free -> ToVarInst)],
        GoalExpr = generic_call(cast(unsafe_type_cast), [FromVar, ToVar],
            Modes, det)
    ),

    % `ToVar' may not appear in the original instmap_delta,
    % so we can't just use instmap_delta_restrict on the
    % original instmap_delta here.
    instmap_delta_from_assoc_list([ToVar - ToVarInst], InstMapDelta),

    goal_info_init(NonLocals, InstMapDelta, det, pure, GoalInfo),
    Goal = GoalExpr - GoalInfo,
    common__record_equivalence(ToVar, FromVar, !Info).

:- pred common__types_match_exactly((type)::in, (type)::in) is semidet.

common__types_match_exactly(variable(TVar, _), variable(TVar, _)).
common__types_match_exactly(defined(Name, As, _), defined(Name, Bs, _)) :-
    common__types_match_exactly_list(As, Bs).
common__types_match_exactly(builtin(BuiltinType), builtin(BuiltinType)).
common__types_match_exactly(higher_order(As, AR, P, E),
        higher_order(Bs, BR, P, E)) :-
    common__types_match_exactly_list(As, Bs),
    (
        AR = yes(A),
        BR = yes(B),
        common__types_match_exactly(A, B)
    ;
        AR = no,
        BR = no
    ).
common__types_match_exactly(tuple(As, _), tuple(Bs, _)) :-
    common__types_match_exactly_list(As, Bs).
common__types_match_exactly(apply_n(TVar, As, _), apply_n(TVar, Bs, _)) :-
    common__types_match_exactly_list(As, Bs).
common__types_match_exactly(kinded(_, _), _) :-
    unexpected(this_file, "kind annotation").

:- pred common__types_match_exactly_list(list(type)::in, list(type)::in)
    is semidet.

common__types_match_exactly_list([], []).
common__types_match_exactly_list([Type1 | Types1], [Type2 | Types2]) :-
    common__types_match_exactly(Type1, Type2),
    common__types_match_exactly_list(Types1, Types2).

%---------------------------------------------------------------------------%

    % Two existentially quantified type variables may become aliased if two
    % calls or two deconstructions are merged together.  We detect this
    % situation here and apply the appropriate tsubst to the vartypes and
    % rtti_varmaps.  This allows us to avoid an unsafe cast, and also may
    % allow more opportunities for simplification.
    %
    % Note that this relies on the assignments for type_infos and
    % typeclass_infos to be generated before other arguments with these
    % existential types are processed.  In other words, the arguments of
    % calls and deconstructions must be processed in left to right order.
    %
:- pred apply_induced_tsubst(prog_var::in, prog_var::in, simplify_info::in,
    simplify_info::out) is det.

apply_induced_tsubst(ToVar, FromVar, !Info) :-
    simplify_info_get_rtti_varmaps(!.Info, RttiVarMaps0),
    rtti_varmaps_var_info(RttiVarMaps0, FromVar, FromVarRttiInfo),
    rtti_varmaps_var_info(RttiVarMaps0, ToVar, ToVarRttiInfo),
    (
        calculate_induced_tsubst(ToVarRttiInfo, FromVarRttiInfo, TSubst)
    ->
        ( map__is_empty(TSubst) ->
            true
        ;
            simplify_info_apply_type_substitution(TSubst, !Info)
        )
    ;
            % Update the rtti_varmaps with new information if only one of the
            % variables has rtti_var_info recorded.  This can happen if a new
            % variable has been introduced, eg in quantification, without
            % being recorded in the rtti_varmaps.
            %
        FromVarRttiInfo = non_rtti_var
    ->
        rtti_var_info_duplicate(ToVar, FromVar, RttiVarMaps0, RttiVarMaps),
        simplify_info_set_rtti_varmaps(RttiVarMaps, !Info)
    ;
        ToVarRttiInfo = non_rtti_var
    ->
        rtti_var_info_duplicate(FromVar, ToVar, RttiVarMaps0, RttiVarMaps),
        simplify_info_set_rtti_varmaps(RttiVarMaps, !Info)
    ;
            % calculate_induced_tsubst failed for a different reason, either
            % because unification failed or because one variable was a
            % type_info and the other was a typeclass_info.
            %
        unexpected(this_file, "apply_induced_tsubst: inconsistent info")
    ).

    % Calculate the induced substitution by unifying the types or constraints,
    % if they exist.  Fail if given non-matching rtti_var_infos.
    %
:- pred calculate_induced_tsubst(rtti_var_info::in, rtti_var_info::in,
    tsubst::out) is semidet.

calculate_induced_tsubst(ToVarRttiInfo, FromVarRttiInfo, TSubst) :-
    (
        FromVarRttiInfo = type_info_var(FromVarTypeInfoType),
        ToVarRttiInfo = type_info_var(ToVarTypeInfoType),
        type_unify(FromVarTypeInfoType, ToVarTypeInfoType, [],
                map__init, TSubst)
    ;
        FromVarRttiInfo = typeclass_info_var(FromVarConstraint),
        ToVarRttiInfo = typeclass_info_var(ToVarConstraint),
        FromVarConstraint = constraint(Name, FromArgs),
        ToVarConstraint = constraint(Name, ToArgs),
        type_unify_list(FromArgs, ToArgs, [], map__init, TSubst)
    ;
        FromVarRttiInfo = non_rtti_var,
        ToVarRttiInfo = non_rtti_var,
        map__init(TSubst)
    ).

%---------------------------------------------------------------------------%

:- func this_file = string.

this_file = "common.m".

%---------------------------------------------------------------------------%
:- end_module check_hlds__common.
%---------------------------------------------------------------------------%
