%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: interval.m.
% Author: zs.
%
% This module contains a predicate to build up interval information for a
% procedure; in particular the start and end points of intervals and the set
% of variables needed in that interval. It also contains a procedure to
% insert deconstruction unifications into a goal, given a map of insertions
% to make after particular anchors. More detailed information is in
% stack_opt.m, from where this code was extracted.
%
% A description of intervals is in the paper "Using the heap to eliminate
% stack accesses" by Zoltan Somogyi and Peter Stuckey:
%
%    Definition 3: An interval is a sequence of atomic goals delimited by a
%    left-right pair of anchors, satisfying the property that if forward
%    execution starts at the left anchor and continues without encountering
%    failure (which would initiate backtracking, i.e. backward execution),
%    the next anchor it reaches is the right anchor of the pair. We consider
%    a call to be part of the atomic goals of the interval only if
%    the call site is the right anchor of the interval, not the left anchor.
%
%-----------------------------------------------------------------------------%

:- module backend_libs.interval.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.vartypes.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module counter.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.

%-----------------------------------------------------------------------------%

:- type save_point_type
    --->    save_point_call_site
    ;       save_point_resume_point.

:- type save_point
    --->    save_point(
                save_point_type,
                goal_id
            ).

:- type branch_construct
    --->    branch_ite
    ;       branch_disj
    ;       branch_switch
    ;       branch_neg
    ;       branch_par_conj.

:- type resume_save_status
    --->    has_resume_save
    ;       has_no_resume_save.

:- type anchor
    --->    anchor_proc_start
    ;       anchor_proc_end
    ;       anchor_branch_start(branch_construct, goal_id)
    ;       anchor_cond_then(goal_id)
    ;       anchor_branch_end(branch_construct, goal_id)
    ;       anchor_call_site(goal_id).

:- type interval_id
    --->    interval_id(int).

:- type branch_end_info
    --->    branch_end_info(
                flushed_after_branch    :: set_of_progvar,
                accessed_after_branch   :: set_of_progvar,
                interval_after_branch   :: interval_id
            ).

:- type insert_spec
    --->    insert_spec(
                hlds_goal,
                set_of_progvar
            ).

:- type insert_map      ==  map(anchor, list(insert_spec)).

:- type anchor_follow_info
    --->    anchor_follow_info(
                set_of_progvar,
                set(interval_id)
            ).

:- type interval_params
    --->    interval_params(
                ip_module_info          :: module_info,
                ip_var_types            :: vartypes,
                ip_at_most_zero_calls   :: bool
            ).

:- type interval_info
    --->    interval_info(
                ii_interval_params      :: interval_params,
                ii_flushed_later        :: set_of_progvar,
                ii_accessed_later       :: set_of_progvar,
                ii_branch_resume_map    :: map(goal_id, resume_save_status),
                ii_branch_end_map       :: map(goal_id, branch_end_info),
                ii_cond_end_map         :: map(goal_id, interval_id),
                ii_cur_interval         :: interval_id,
                ii_interval_counter     :: counter,
                ii_open_intervals       :: set(interval_id),
                ii_anchor_follow_map    :: map(anchor, anchor_follow_info),
                ii_model_non_anchors    :: set(anchor),
                ii_interval_start       :: map(interval_id, anchor),
                ii_interval_end         :: map(interval_id, anchor),
                ii_interval_succ        :: map(interval_id, list(interval_id)),
                ii_interval_vars        :: map(interval_id, set_of_progvar),
                ii_interval_delvars     :: map(interval_id,
                                            list(set_of_progvar))
            ).

:- type maybe_needs_flush
    --->    needs_flush
    ;       doesnt_need_flush.

:- typeclass build_interval_info_acc(T) where [
    pred use_cell(prog_var::in, list(prog_var)::in, cons_id::in,
        hlds_goal::in, interval_info::in, interval_info::out, T::in, T::out)
        is det
].

:- pred build_interval_info_in_goal(hlds_goal::in, interval_info::in,
    interval_info::out, T::in, T::out) is det <= build_interval_info_acc(T).

:- pred record_interval_vars(interval_id::in, list(prog_var)::in,
    interval_info::in, interval_info::out) is det.

:- pred delete_interval_vars(interval_id::in, set_of_progvar::in,
    set_of_progvar::out, interval_info::in, interval_info::out) is det.

:- type rename_map  ==  map(prog_var, prog_var).

:- pred record_decisions_in_goal(hlds_goal::in, hlds_goal::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    rename_map::in, rename_map::out, insert_map::in,
    maybe(goal_feature)::in) is det.

:- pred make_inserted_goal(prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out, rename_map::in, rename_map::out,
    insert_spec::in, maybe(goal_feature)::in, hlds_goal::out) is det.

    % The final RenameMap may ask for some of the head variables to be renamed.
    % Doing so is inconvenient, e.g. because the debugger wants head variables
    % to have names of a fixed form. Instead, we exploit the fact that the
    % transformation does not care about actual variable names or even numbers;
    % all it cares about wrt renaming is that the variables it has renamed
    % apart should stay renamed apart. We therefore swap the roles of the
    % original and the renamed variable in the goal representing the procedure
    % body. The resulting procedure definition will be isomorphic to the one
    % we would have get by applying the original renaming to the headvars.
    %
:- pred apply_headvar_correction(set_of_progvar::in, rename_map::in,
    hlds_goal::in, hlds_goal::out) is det.

:- pred dump_interval_info(io.text_output_stream::in, interval_info::in,
    io::di, io::uo) is det.

:- func int_list_to_string(list(int)) = string.

:- func interval_id_to_int(interval_id) = int.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.inst_test.
:- import_module check_hlds.mode_util.
:- import_module hlds.arg_info.
:- import_module hlds.code_model.
:- import_module hlds.hlds_llds.
:- import_module hlds.instmap.
:- import_module ll_backend.
:- import_module ll_backend.call_gen.
:- import_module parse_tree.prog_rename.

:- import_module assoc_list.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

build_interval_info_in_goal(hlds_goal(GoalExpr, GoalInfo), !IntervalInfo,
        !Acc) :-
    (
        GoalExpr = conj(ConjType, Goals),
        build_interval_info_in_conj(Goals, ConjType, !IntervalInfo, !Acc)
    ;
        GoalExpr = disj(Goals),
        (
            Goals = [FirstDisjunct | _],
            reached_branch_end(GoalInfo, yes(FirstDisjunct), branch_disj,
                StartAnchor, EndAnchor, BeforeId, AfterId,
                MaybeResumeVars, !IntervalInfo, !Acc),
            build_interval_info_in_disj(Goals, doesnt_need_flush,
                StartAnchor, EndAnchor, BeforeId, AfterId,
                OpenIntervals, !IntervalInfo, !Acc),
            leave_branch_start(branch_disj, StartAnchor, BeforeId,
                MaybeResumeVars, OpenIntervals, !IntervalInfo)
        ;
            Goals = [],
            % We could reset the set of variables in the current interval
            % to the empty set, since any variable accesses after a fail
            % goal (which is what an empty disjunction represent) will not
            % be executed at runtime. However, simplify should have removed
            % any goals in the current branch from after the fail, so the
            % set of variables in the current interval will already be
            % the empty set.
            no_open_intervals(!IntervalInfo)
        )
    ;
        GoalExpr = switch(Var, _Det, Cases),
        reached_branch_end(GoalInfo, no, branch_switch,
            StartAnchor, EndAnchor, BeforeId, AfterId, MaybeResumeVars,
            !IntervalInfo, !Acc),
        build_interval_info_in_cases(Cases, StartAnchor, EndAnchor,
            BeforeId, AfterId, OpenIntervalsList, !IntervalInfo, !Acc),
        OpenIntervals = set.union_list(OpenIntervalsList),
        leave_branch_start(branch_switch, StartAnchor, BeforeId,
            MaybeResumeVars, OpenIntervals, !IntervalInfo),
        require_in_regs([Var], !IntervalInfo),
        require_access([Var], !IntervalInfo)
    ;
        GoalExpr = negation(SubGoal),
        reached_branch_end(GoalInfo, yes(SubGoal), branch_neg,
            StartAnchor, EndAnchor, BeforeId, AfterId, MaybeResumeVars,
            !IntervalInfo, !Acc),
        enter_branch_tail(EndAnchor, AfterId, !IntervalInfo),
        build_interval_info_in_goal(SubGoal, !IntervalInfo, !Acc),
        reached_branch_start(needs_flush, StartAnchor, BeforeId,
            OpenIntervals, !IntervalInfo, !Acc),
        leave_branch_start(branch_neg, StartAnchor, BeforeId, MaybeResumeVars,
            OpenIntervals, !IntervalInfo)
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        reached_branch_end(GoalInfo, yes(Cond), branch_ite,
            StartAnchor, EndAnchor, BeforeId, AfterId, MaybeResumeVars,
            !IntervalInfo, !Acc),
        enter_branch_tail(EndAnchor, AfterId, !IntervalInfo),
        build_interval_info_in_goal(Then, !IntervalInfo, !Acc),
        reached_cond_then(GoalInfo, !IntervalInfo),
        build_interval_info_in_goal(Cond, !IntervalInfo, !Acc),
        reached_branch_start(doesnt_need_flush, StartAnchor, BeforeId,
            CondOpenIntervals, !IntervalInfo, !Acc),
        enter_branch_tail(EndAnchor, AfterId, !IntervalInfo),
        build_interval_info_in_goal(Else, !IntervalInfo, !Acc),
        reached_branch_start(needs_flush, StartAnchor, BeforeId,
            _ElseOpenIntervals, !IntervalInfo, !Acc),
        leave_branch_start(branch_ite, StartAnchor, BeforeId, MaybeResumeVars,
            CondOpenIntervals, !IntervalInfo)
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( if
            Reason = from_ground_term(TermVar, from_ground_term_construct)
        then
            % We treat this scope as a construction unification that unifies
            % TermVar with a single big variable-free term, since this is what
            % the generated code will do.
            require_access([TermVar], !IntervalInfo)
        else
            % XXX We could treat from_ground_term_deconstruct scopes specially
            % as well.
            build_interval_info_in_goal(SubGoal, !IntervalInfo, !Acc)
        )
    ;
        GoalExpr = generic_call(GenericCall, ArgVars, ArgModes, MaybeArgRegs,
            _Detism),
        goal_info_get_maybe_need_across_call(GoalInfo, MaybeNeedAcrossCall),
        IntParams = !.IntervalInfo ^ ii_interval_params,
        ModuleInfo = IntParams ^ ip_module_info,
        VarTypes = IntParams ^ ip_var_types,
        lookup_var_types(VarTypes, ArgVars, ArgTypes),
        arg_info.generic_call_arg_reg_types(ModuleInfo, VarTypes, GenericCall,
            ArgVars, MaybeArgRegs, ArgRegTypes),
        arg_info.compute_in_and_out_vars_sep_regs(ModuleInfo, ArgVars,
            ArgModes, ArgTypes, ArgRegTypes, InputArgsR, InputArgsF,
            _OutputArgsR, _OutputArgsF),
        list.append(InputArgsR, InputArgsF, InputArgs),

        % Casts are generated inline.
        (
            GenericCall = cast(_),
            require_in_regs(InputArgs, !IntervalInfo),
            require_access(InputArgs, !IntervalInfo)
        ;
            ( GenericCall = higher_order(_, _, _, _)
            ; GenericCall = class_method(_, _, _, _)
            ; GenericCall = event_call(_)
            ; GenericCall = subtype_coerce  % XXX SUBTYPE revisit this later
            ),
            module_info_get_globals(ModuleInfo, Globals),
            call_gen.generic_call_info(Globals, GenericCall,
                length(InputArgsR), length(InputArgsF), _,
                GenericVarsArgInfos, _, _),
            assoc_list.keys(GenericVarsArgInfos, GenericVars),
            list.append(GenericVars, InputArgs, Inputs),
            build_interval_info_at_call(Inputs, MaybeNeedAcrossCall, GoalInfo,
                !IntervalInfo, !Acc)
        )
    ;
        GoalExpr = plain_call(PredId, ProcId, ArgVars, Builtin, _, _),
        IntParams = !.IntervalInfo ^ ii_interval_params,
        ModuleInfo = IntParams ^ ip_module_info,
        module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
            _PredInfo, ProcInfo),
        VarTypes = IntParams ^ ip_var_types,
        arg_info.partition_proc_call_args(ProcInfo, VarTypes,
            ModuleInfo, ArgVars, InputArgs, _, _),
        set.to_sorted_list(InputArgs, Inputs),
        (
            Builtin = inline_builtin,
            require_in_regs(Inputs, !IntervalInfo),
            require_access(Inputs, !IntervalInfo)
        ;
            Builtin = not_builtin,
            goal_info_get_maybe_need_across_call(GoalInfo,
                MaybeNeedAcrossCall),
            build_interval_info_at_call(Inputs, MaybeNeedAcrossCall, GoalInfo,
                !IntervalInfo, !Acc)
        )
    ;
        GoalExpr = call_foreign_proc(_Attributes, PredId, ProcId,
            Args, ExtraArgs, _MaybeTraceRuntimeCond, _PragmaCode),
        IntParams = !.IntervalInfo ^ ii_interval_params,
        ModuleInfo = IntParams ^ ip_module_info,
        module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
            _PredInfo, ProcInfo),
        VarTypes = IntParams ^ ip_var_types,
        ArgVars = list.map(foreign_arg_var, Args),
        ExtraVars = list.map(foreign_arg_var, ExtraArgs),
        arg_info.partition_proc_call_args(ProcInfo, VarTypes,
            ModuleInfo, ArgVars, InputArgVarSet, _, _),
        set.to_sorted_list(InputArgVarSet, InputArgVars),
        list.append(InputArgVars, ExtraVars, InputVars),
        ( if
            goal_info_maybe_get_maybe_need_across_call(GoalInfo,
                MaybeNeedAcrossCall),
            MaybeNeedAcrossCall = yes(_)
        then
            build_interval_info_at_call(InputVars, MaybeNeedAcrossCall,
                GoalInfo, !IntervalInfo, !Acc)
        else
            require_in_regs(InputVars, !IntervalInfo),
            require_access(InputVars, !IntervalInfo)
        )
    ;
        GoalExpr = unify(_, _, _, Unification, _),
        (
            Unification = construct(CellVar, _ConsId, ArgVars, _,
                HowToConstruct, _, _),
            (
                HowToConstruct = reuse_cell(_),
                unexpected($pred, "reuse")
            ;
                % XXX Temporary for the time being.
                HowToConstruct = construct_in_region(_),
                unexpected($pred, "construct in region")
            ;
                ( HowToConstruct = construct_statically
                ; HowToConstruct = construct_dynamically
                )
            ),
            require_in_regs(ArgVars, !IntervalInfo),
            require_access([CellVar | ArgVars], !IntervalInfo)
            % use_cell(CellVar, ArgVars, ConsId, GoalExpr - GoalInfo,
            %   !IntervalInfo)
            % We cannot use such cells, because some of the ArgVars
            % may need to be saved on the stack before this construction.
        ;
            Unification = deconstruct(CellVar, ConsId, ArgVars, ArgModes,
                _, _),
            IntParams = !.IntervalInfo ^ ii_interval_params,
            ModuleInfo = IntParams ^ ip_module_info,
            ( if shared_left_to_right_deconstruct(ModuleInfo, ArgModes) then
                Goal = hlds_goal(GoalExpr, GoalInfo),
                use_cell(CellVar, ArgVars, ConsId, Goal, !IntervalInfo, !Acc)
            else
                true
            ),
            require_in_regs([CellVar], !IntervalInfo),
            require_access([CellVar | ArgVars], !IntervalInfo)
        ;
            Unification = assign(ToVar, FromVar),
            require_in_regs([FromVar], !IntervalInfo),
            require_access([FromVar, ToVar], !IntervalInfo)
        ;
            Unification = simple_test(Var1, Var2),
            require_in_regs([Var1, Var2], !IntervalInfo),
            require_access([Var1, Var2], !IntervalInfo)
        ;
            Unification = complicated_unify(_, _, _),
            unexpected($pred, "complicated_unify")
        )
    ;
        GoalExpr = shorthand(_),
        % These should have been expanded out by now.
        unexpected($pred, "shorthand")
    ).

:- pred shared_left_to_right_deconstruct(module_info::in,
    list(unify_mode)::in) is semidet.

shared_left_to_right_deconstruct(_, []).
shared_left_to_right_deconstruct(ModuleInfo, [ArgMode | ArgsModes]) :-
    ArgMode = unify_modes_li_lf_ri_rf(CellInitInst, CellFinalInst,
        ArgInitInst, ArgFinalInst),
    init_inst_is_fully_input(ModuleInfo, CellInitInst),
    init_final_insts_is_output(ModuleInfo, ArgInitInst, ArgFinalInst),
    inst_is_not_partly_unique(ModuleInfo, CellFinalInst),
    inst_is_not_partly_unique(ModuleInfo, ArgFinalInst),
    shared_left_to_right_deconstruct(ModuleInfo, ArgsModes).

%-----------------------------------------------------------------------------%

:- pred build_interval_info_at_call(list(prog_var)::in,
    maybe(need_across_call)::in, hlds_goal_info::in,
    interval_info::in, interval_info::out, T::in, T::out) is det
    <= build_interval_info_acc(T).

build_interval_info_at_call(Inputs, MaybeNeedAcrossCall, GoalInfo,
        !IntervalInfo, !Acc) :-
    (
        MaybeNeedAcrossCall = yes(NeedAcrossCall),
        NeedAcrossCall = need_across_call(ForwardVars, ResumeVars,
            NondetLiveVars),
        VarsOnStack0 = set_of_var.union_list([ForwardVars, ResumeVars,
            NondetLiveVars]),
        GoalId = goal_info_get_goal_id(GoalInfo),
        CallAnchor = anchor_call_site(GoalId),
        get_cur_interval(AfterCallId, !.IntervalInfo),
        new_interval_id(BeforeCallId, !IntervalInfo),
        record_interval_start(AfterCallId, CallAnchor, !IntervalInfo),
        record_interval_end(BeforeCallId, CallAnchor, !IntervalInfo),
        InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
        IntParams = !.IntervalInfo ^ ii_interval_params,
        ( if
            ( instmap_delta_is_reachable(InstMapDelta)
            ; IntParams ^ ip_at_most_zero_calls = no
            )
        then
            record_interval_succ(BeforeCallId, AfterCallId, !IntervalInfo),
            VarsOnStack = VarsOnStack0
        else
            % If the call cannot succeed, then execution cannot
            % get from BeforeCallId to AfterCallId.
            record_interval_no_succ(BeforeCallId, !IntervalInfo),
            VarsOnStack = set_of_var.init
        ),
        set_cur_interval(BeforeCallId, !IntervalInfo),
        assign_open_intervals_to_anchor(CallAnchor, !IntervalInfo),
        CodeModel = goal_info_get_code_model(GoalInfo),
        (
            CodeModel = model_non,
            record_model_non_anchor(CallAnchor, !IntervalInfo)
        ;
            ( CodeModel = model_det
            ; CodeModel = model_semi
            )
        ),
        one_open_interval(BeforeCallId, !IntervalInfo),
        require_flushed(VarsOnStack, !IntervalInfo),
        require_in_regs(Inputs, !IntervalInfo),
        require_access(Inputs, !IntervalInfo)
    ;
        MaybeNeedAcrossCall = no,
        unexpected($pred, "no need across call")
    ).

%-----------------------------------------------------------------------------%

:- pred build_interval_info_in_conj(list(hlds_goal)::in, conj_type::in,
    interval_info::in, interval_info::out, T::in, T::out) is det
    <= build_interval_info_acc(T).

build_interval_info_in_conj([], _, !IntervalInfo, !Acc).
build_interval_info_in_conj([Goal | Goals], ConjType, !IntervalInfo, !Acc) :-
    % XXX zs: I am not sure that passing interval_info from the first goal
    % to the rest is OK when ConjType = parallel_conj. Maybe we should pass
    % the initial interval_info to all the conjuncts, and then merge the
    % resulting interval_infos.
    build_interval_info_in_conj(Goals, ConjType, !IntervalInfo, !Acc),
    build_interval_info_in_goal(Goal, !IntervalInfo, !Acc).

:- pred build_interval_info_in_disj(list(hlds_goal)::in, maybe_needs_flush::in,
    anchor::in, anchor::in, interval_id::in, interval_id::in,
    set(interval_id)::out, interval_info::in, interval_info::out,
    T::in, T::out) is det <= build_interval_info_acc(T).

build_interval_info_in_disj([], _, _, _, _, _, set.init, !IntervalInfo, !Acc).
build_interval_info_in_disj([Goal | Goals], MaybeNeedsFlush,
        StartAnchor, EndAnchor, BeforeId, AfterId, OpenIntervals,
        !IntervalInfo, !Acc) :-
    enter_branch_tail(EndAnchor, AfterId, !IntervalInfo),
    build_interval_info_in_goal(Goal, !IntervalInfo, !Acc),
    reached_branch_start(MaybeNeedsFlush, StartAnchor, BeforeId,
        OpenIntervals, !IntervalInfo, !Acc),
    build_interval_info_in_disj(Goals, needs_flush, StartAnchor, EndAnchor,
        BeforeId, AfterId, _OpenIntervals, !IntervalInfo, !Acc).

:- pred build_interval_info_in_cases(list(case)::in,
    anchor::in, anchor::in, interval_id::in, interval_id::in,
    list(set(interval_id))::out, interval_info::in, interval_info::out,
    T::in, T::out) is det <= build_interval_info_acc(T).

build_interval_info_in_cases([], _, _, _, _, [], !IntervalInfo, !Acc).
build_interval_info_in_cases([Case | Cases],
        StartAnchor, EndAnchor, BeforeId, AfterId,
        [OpenIntervals | OpenIntervalsList], !IntervalInfo, !Acc) :-
    Case = case(_MainConsId, _OtherConsIds, Goal),
    enter_branch_tail(EndAnchor, AfterId, !IntervalInfo),
    build_interval_info_in_goal(Goal, !IntervalInfo, !Acc),
    reached_branch_start(doesnt_need_flush, StartAnchor, BeforeId,
        OpenIntervals, !IntervalInfo, !Acc),
    build_interval_info_in_cases(Cases, StartAnchor, EndAnchor,
        BeforeId, AfterId, OpenIntervalsList, !IntervalInfo, !Acc).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred reached_branch_end(hlds_goal_info::in, maybe(hlds_goal)::in,
    branch_construct::in, anchor::out, anchor::out,
    interval_id::out, interval_id::out, maybe(set_of_progvar)::out,
    interval_info::in, interval_info::out, T::in, T::out) is det
    <= build_interval_info_acc(T).

reached_branch_end(GoalInfo, MaybeResumeGoal, Construct,
        StartAnchor, EndAnchor, BeforeIntervalId, AfterIntervalId,
        MaybeResumeVars, !IntervalInfo, !Acc) :-
    GoalId = goal_info_get_goal_id(GoalInfo),
    record_branch_end_info(GoalId, !IntervalInfo),
    ( if
        MaybeResumeGoal = yes(hlds_goal(_ResumeGoalExpr, ResumeGoalInfo)),
        goal_info_maybe_get_resume_point(ResumeGoalInfo, ResumePoint),
        ResumePoint = resume_point(ResumeVars, ResumeLocs),
        ResumeLocs \= resume_locs_orig_only
    then
        HasResumeSave = has_resume_save,
        MaybeResumeVars = yes(ResumeVars)
    else
        HasResumeSave = has_no_resume_save,
        MaybeResumeVars = no
    ),
    record_branch_resume(GoalId, HasResumeSave, !IntervalInfo),
    ( if goal_info_maybe_get_store_map(GoalInfo, StoreMap) then
        map.sorted_keys(StoreMap, StoreMapVarList),
        StoreMapVars = set_of_var.sorted_list_to_set(StoreMapVarList),
        require_flushed(StoreMapVars, !IntervalInfo)
    else
        unexpected($pred, "no store map")
    ),
    EndAnchor = anchor_branch_end(Construct, GoalId),
    StartAnchor = anchor_branch_start(Construct, GoalId),
    assign_open_intervals_to_anchor(EndAnchor, !IntervalInfo),
    CodeModel = goal_info_get_code_model(GoalInfo),
    (
        CodeModel = model_non,
        record_model_non_anchor(EndAnchor, !IntervalInfo)
    ;
        ( CodeModel = model_det
        ; CodeModel = model_semi
        )
    ),
    no_open_intervals(!IntervalInfo),
    get_cur_interval(AfterIntervalId, !.IntervalInfo),
    record_interval_start(AfterIntervalId, EndAnchor, !IntervalInfo),
    new_interval_id(BeforeIntervalId, !IntervalInfo).

:- pred enter_branch_tail(anchor::in, interval_id::in,
    interval_info::in, interval_info::out) is det.

enter_branch_tail(EndAnchor, AfterId, !IntervalInfo) :-
    new_interval_id(BranchTailId, !IntervalInfo),
    record_interval_end(BranchTailId, EndAnchor, !IntervalInfo),
    record_interval_succ(BranchTailId, AfterId, !IntervalInfo),
    set_cur_interval(BranchTailId, !IntervalInfo),
    one_open_interval(BranchTailId, !IntervalInfo).

:- pred reached_branch_start(maybe_needs_flush::in, anchor::in,
    interval_id::in, set(interval_id)::out, interval_info::in,
    interval_info::out, T::in, T::out) is det <= build_interval_info_acc(T).

reached_branch_start(MaybeNeedsFlush, StartAnchor, BeforeId, OpenIntervals,
        !IntervalInfo, !Acc) :-
    get_cur_interval(BranchStartId, !.IntervalInfo),
    record_interval_start(BranchStartId, StartAnchor, !IntervalInfo),
    record_interval_succ(BeforeId, BranchStartId, !IntervalInfo),
    get_open_intervals(!.IntervalInfo, OpenIntervals),
    (
        MaybeNeedsFlush = doesnt_need_flush
    ;
        MaybeNeedsFlush = needs_flush,
        assign_open_intervals_to_anchor(StartAnchor, !IntervalInfo)
    ).

:- pred reached_cond_then(hlds_goal_info::in,
    interval_info::in, interval_info::out) is det.

reached_cond_then(GoalInfo, !IntervalInfo) :-
    GoalId = goal_info_get_goal_id(GoalInfo),
    record_cond_end(GoalId, !IntervalInfo),
    get_cur_interval(ThenStartId, !.IntervalInfo),
    record_interval_start(ThenStartId, CondThenAnchor, !IntervalInfo),
    new_interval_id(CondTailId, !IntervalInfo),
    CondThenAnchor = anchor_cond_then(GoalId),
    record_interval_end(CondTailId, CondThenAnchor, !IntervalInfo),
    record_interval_succ(CondTailId, ThenStartId, !IntervalInfo),
    set_cur_interval(CondTailId, !IntervalInfo),
    get_open_intervals(!.IntervalInfo, OpenIntervals0),
    set.insert(CondTailId, OpenIntervals0, OpenIntervals),
    set_open_intervals(OpenIntervals, !IntervalInfo).

:- pred leave_branch_start(branch_construct::in, anchor::in, interval_id::in,
    maybe(set_of_progvar)::in, set(interval_id)::in,
    interval_info::in, interval_info::out) is det.

leave_branch_start(_BranchConstruct, StartArchor, BeforeId, MaybeResumeVars,
        OpenIntervals, !IntervalInfo) :-
    record_interval_end(BeforeId, StartArchor, !IntervalInfo),
    (
        MaybeResumeVars = yes(ResumeVars),
        require_flushed(ResumeVars, !IntervalInfo)
    ;
        MaybeResumeVars = no
    ),
    set_cur_interval(BeforeId, !IntervalInfo),
    set_open_intervals(OpenIntervals, !IntervalInfo).

:- pred get_open_intervals(interval_info::in, set(interval_id)::out) is det.

get_open_intervals(IntervalInfo, OpenIntervals) :-
    OpenIntervals = IntervalInfo ^ ii_open_intervals.

:- pred set_open_intervals(set(interval_id)::in,
    interval_info::in, interval_info::out) is det.

set_open_intervals(OpenIntervals, !IntervalInfo) :-
    !IntervalInfo ^ ii_open_intervals := OpenIntervals.

:- pred no_open_intervals(interval_info::in, interval_info::out) is det.

no_open_intervals(!IntervalInfo) :-
    !IntervalInfo ^ ii_open_intervals := set.init.

:- pred one_open_interval(interval_id::in, interval_info::in,
    interval_info::out) is det.

one_open_interval(IntervalId, !IntervalInfo) :-
    !IntervalInfo ^ ii_open_intervals := set.make_singleton_set(IntervalId).

:- pred assign_open_intervals_to_anchor(anchor::in,
    interval_info::in, interval_info::out) is det.

assign_open_intervals_to_anchor(Anchor, !IntervalInfo) :-
    AnchorFollowMap0 = !.IntervalInfo ^ ii_anchor_follow_map,
    IntervalVarMap = !.IntervalInfo ^ ii_interval_vars,
    CurOpenIntervals = !.IntervalInfo ^ ii_open_intervals,
    set.fold(gather_interval_vars(IntervalVarMap), CurOpenIntervals,
        set_of_var.init, CurOpenIntervalVars),
    ( if map.search(AnchorFollowMap0, Anchor, AnchorFollowInfo0) then
        AnchorFollowInfo0 =
            anchor_follow_info(OpenIntervalVars0, OpenIntervals0),
        OpenIntervalVars =
            set_of_var.union(OpenIntervalVars0, CurOpenIntervalVars),
        OpenIntervals = set.union(OpenIntervals0, CurOpenIntervals),
        AnchorFollowInfo =
            anchor_follow_info(OpenIntervalVars, OpenIntervals),
        map.det_update(Anchor, AnchorFollowInfo,
            AnchorFollowMap0, AnchorFollowMap)
    else
        AnchorFollowInfo =
            anchor_follow_info(CurOpenIntervalVars, CurOpenIntervals),
        map.det_insert(Anchor, AnchorFollowInfo,
            AnchorFollowMap0, AnchorFollowMap)
    ),
    !IntervalInfo ^ ii_anchor_follow_map := AnchorFollowMap.

:- pred gather_interval_vars(map(interval_id, set_of_progvar)::in,
    interval_id::in, set_of_progvar::in, set_of_progvar::out) is det.

gather_interval_vars(IntervalVarMap, IntervalId, !OpenIntervalVars) :-
    map.lookup(IntervalVarMap, IntervalId, IntervalVars),
    !:OpenIntervalVars = set_of_var.union(!.OpenIntervalVars, IntervalVars).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred get_cur_interval(interval_id::out, interval_info::in) is det.

get_cur_interval(IntervalInfo ^ ii_cur_interval, IntervalInfo).

:- pred set_cur_interval(interval_id::in,
    interval_info::in, interval_info::out) is det.

set_cur_interval(CurInterval, !IntervalInfo) :-
    !IntervalInfo ^ ii_cur_interval := CurInterval.

:- pred new_interval_id(interval_id::out,
    interval_info::in, interval_info::out) is det.

new_interval_id(Id, !IntervalInfo) :-
    Counter0 = !.IntervalInfo ^ ii_interval_counter,
    IntervalVars0 = !.IntervalInfo ^ ii_interval_vars,
    counter.allocate(Num, Counter0, Counter),
    Id = interval_id(Num),
    map.det_insert(Id, set_of_var.init, IntervalVars0, IntervalVars),
    !IntervalInfo ^ ii_interval_counter := Counter,
    !IntervalInfo ^ ii_interval_vars := IntervalVars.

:- pred record_branch_end_info(goal_id::in,
    interval_info::in, interval_info::out) is det.

record_branch_end_info(GoalId, !IntervalInfo) :-
    FlushedLater = !.IntervalInfo ^ ii_flushed_later,
    AccessedLater = !.IntervalInfo ^ ii_accessed_later,
    CurInterval = !.IntervalInfo ^ ii_cur_interval,
    BranchEndMap0 = !.IntervalInfo ^ ii_branch_end_map,
    BranchEndInfo = branch_end_info(FlushedLater, AccessedLater, CurInterval),
    map.det_insert(GoalId, BranchEndInfo, BranchEndMap0, BranchEndMap),
    !IntervalInfo ^ ii_branch_end_map := BranchEndMap.

:- pred record_cond_end(goal_id::in, interval_info::in, interval_info::out)
    is det.

record_cond_end(GoalId, !IntervalInfo) :-
    CurInterval = !.IntervalInfo ^ ii_cur_interval,
    CondEndMap0 = !.IntervalInfo ^ ii_cond_end_map,
    map.det_insert(GoalId, CurInterval, CondEndMap0, CondEndMap),
    !IntervalInfo ^ ii_cond_end_map := CondEndMap.

:- pred record_interval_end(interval_id::in, anchor::in,
    interval_info::in, interval_info::out) is det.

record_interval_end(Id, End, !IntervalInfo) :-
    EndMap0 = !.IntervalInfo ^ ii_interval_end,
    map.det_insert(Id, End, EndMap0, EndMap),
    !IntervalInfo ^ ii_interval_end := EndMap.

:- pred record_interval_start(interval_id::in, anchor::in,
    interval_info::in, interval_info::out) is det.

record_interval_start(Id, Start, !IntervalInfo) :-
    StartMap0 = !.IntervalInfo ^ ii_interval_start,
    map.det_insert(Id, Start, StartMap0, StartMap),
    !IntervalInfo ^ ii_interval_start := StartMap.

:- pred record_interval_succ(interval_id::in, interval_id::in,
    interval_info::in, interval_info::out) is det.

record_interval_succ(Id, Succ, !IntervalInfo) :-
    SuccMap0 = !.IntervalInfo ^ ii_interval_succ,
    ( if map.search(SuccMap0, Id, Succ0) then
        map.det_update(Id, [Succ | Succ0], SuccMap0, SuccMap)
    else
        map.det_insert(Id, [Succ], SuccMap0, SuccMap)
    ),
    !IntervalInfo ^ ii_interval_succ := SuccMap.

:- pred record_interval_no_succ(interval_id::in,
    interval_info::in, interval_info::out) is det.

record_interval_no_succ(Id, !IntervalInfo) :-
    SuccMap0 = !.IntervalInfo ^ ii_interval_succ,
    ( if map.search(SuccMap0, Id, _Succ0) then
        unexpected($pred, "already in succ map")
    else
        map.det_insert(Id, [], SuccMap0, SuccMap)
    ),
    !IntervalInfo ^ ii_interval_succ := SuccMap.

record_interval_vars(Id, NewVars, !IntervalInfo) :-
    VarsMap0 = !.IntervalInfo ^ ii_interval_vars,
    ( if map.search(VarsMap0, Id, Vars0) then
        set_of_var.insert_list(NewVars, Vars0, Vars),
        map.det_update(Id, Vars, VarsMap0, VarsMap)
    else
        Vars = set_of_var.list_to_set(NewVars),
        map.det_insert(Id, Vars, VarsMap0, VarsMap)
    ),
    !IntervalInfo ^ ii_interval_vars := VarsMap.

delete_interval_vars(Id, ToDeleteVars, DeletedVars, !IntervalInfo) :-
    VarsMap0 = !.IntervalInfo ^ ii_interval_vars,
    map.lookup(VarsMap0, Id, Vars0),
    DeletedVars = set_of_var.intersect(Vars0, ToDeleteVars),
    Vars = set_of_var.difference(Vars0, DeletedVars),
    map.det_update(Id, Vars, VarsMap0, VarsMap),
    !IntervalInfo ^ ii_interval_vars := VarsMap,

    % The deletions are recorded only for debugging. The algorithm itself
    % does not need this information to be recorded.
    DeleteMap0 = !.IntervalInfo ^ ii_interval_delvars,
    ( if map.search(DeleteMap0, Id, Deletions0) then
        Deletions = [DeletedVars | Deletions0],
        map.det_update(Id, Deletions, DeleteMap0, DeleteMap)
    else
        Deletions = [DeletedVars],
        map.det_insert(Id, Deletions, DeleteMap0, DeleteMap)
    ),
    !IntervalInfo ^ ii_interval_delvars := DeleteMap.

:- pred require_in_regs(list(prog_var)::in, interval_info::in,
    interval_info::out) is det.

require_in_regs(Vars, !IntervalInfo) :-
    CurIntervalId = !.IntervalInfo ^ ii_cur_interval,
    record_interval_vars(CurIntervalId, Vars, !IntervalInfo).

:- pred require_flushed(set_of_progvar::in,
    interval_info::in, interval_info::out) is det.

require_flushed(Vars, !IntervalInfo) :-
    FlushedLater0 = !.IntervalInfo ^ ii_flushed_later,
    FlushedLater = set_of_var.union(FlushedLater0, Vars),
    !IntervalInfo ^ ii_flushed_later := FlushedLater.

:- pred require_access(list(prog_var)::in,
    interval_info::in, interval_info::out) is det.

require_access(Vars, !IntervalInfo) :-
    AccessedLater0 = !.IntervalInfo ^ ii_accessed_later,
    set_of_var.insert_list(Vars, AccessedLater0, AccessedLater),
    !IntervalInfo ^ ii_accessed_later := AccessedLater.

:- pred record_branch_resume(goal_id::in, resume_save_status::in,
    interval_info::in, interval_info::out) is det.

record_branch_resume(GoalId, ResumeSaveStatus, !IntervalInfo) :-
    BranchResumeMap0 = !.IntervalInfo ^ ii_branch_resume_map,
    map.det_insert(GoalId, ResumeSaveStatus,
        BranchResumeMap0, BranchResumeMap),
    !IntervalInfo ^ ii_branch_resume_map := BranchResumeMap.

:- pred record_model_non_anchor(anchor::in, interval_info::in,
    interval_info::out) is det.

record_model_non_anchor(Anchor, !IntervalInfo) :-
    ModelNonAnchors0 = !.IntervalInfo ^ ii_model_non_anchors,
    set.insert(Anchor, ModelNonAnchors0, ModelNonAnchors),
    !IntervalInfo ^ ii_model_non_anchors := ModelNonAnchors.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type interval_var_info
    --->    interval_var_info(
                ivi_varset      :: prog_varset,
                ivi_vartypes    :: vartypes
            ).

record_decisions_in_goal(!Goal, VarSet0, VarSet, VarTypes0, VarTypes,
        !VarRename, InsertMap, MaybeFeature) :-
    Info0 = interval_var_info(VarSet0, VarTypes0),
    record_decisions_in_goal(!Goal, Info0, Info, !VarRename,
        InsertMap, MaybeFeature),
    Info = interval_var_info(VarSet, VarTypes).

:- pred record_decisions_in_goal(hlds_goal::in, hlds_goal::out,
    interval_var_info::in, interval_var_info::out,
    rename_map::in, rename_map::out, insert_map::in, maybe(goal_feature)::in)
    is det.

record_decisions_in_goal(Goal0, Goal, !VarInfo, !VarRename, InsertMap,
        MaybeFeature) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = conj(ConjType, Goals0),
        record_decisions_in_conj(Goals0, Goals, !VarInfo, !VarRename,
            ConjType, InsertMap, MaybeFeature),
        GoalExpr = conj(ConjType, Goals),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = disj(Goals0),
        construct_anchors(branch_disj, Goal0, StartAnchor, EndAnchor),
        (
            Goals0 = [FirstGoal0 | LaterGoals0],
            record_decisions_in_goal(FirstGoal0, FirstGoal, !VarInfo,
                !.VarRename, _, InsertMap, MaybeFeature),
            lookup_inserts(InsertMap, StartAnchor, StartInserts),
            record_decisions_in_disj(LaterGoals0, LaterGoals,
                !VarInfo, !.VarRename, StartInserts, InsertMap, MaybeFeature),
            Goals = [FirstGoal | LaterGoals],
            Goal1 = hlds_goal(disj(Goals), GoalInfo0),
            lookup_inserts(InsertMap, EndAnchor, Inserts),
            insert_goals_after(Goal1, Goal, !VarInfo, !:VarRename, Inserts,
                MaybeFeature)
        ;
            Goals0 = [],
            GoalExpr = disj(Goals0),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        )
    ;
        GoalExpr0 = switch(Var0, Det, Cases0),
        record_decisions_in_cases(Cases0, Cases, !VarInfo, !.VarRename,
            InsertMap, MaybeFeature),
        rename_var(need_not_rename, !.VarRename, Var0, Var),
        Goal1 = hlds_goal(switch(Var, Det, Cases), GoalInfo0),
        construct_anchors(branch_switch, Goal0, _StartAnchor, EndAnchor),
        lookup_inserts(InsertMap, EndAnchor, Inserts),
        insert_goals_after(Goal1, Goal, !VarInfo, !:VarRename, Inserts,
            MaybeFeature)
    ;
        GoalExpr0 = negation(NegGoal0),
        record_decisions_in_goal(NegGoal0, NegGoal, !VarInfo, !.VarRename, _,
            InsertMap, MaybeFeature),
        Goal1 = hlds_goal(negation(NegGoal), GoalInfo0),
        construct_anchors(branch_neg, Goal0, _StartAnchor, EndAnchor),
        lookup_inserts(InsertMap, EndAnchor, Inserts),
        % XXX
        insert_goals_after(Goal1, Goal, !VarInfo, !:VarRename, Inserts,
            MaybeFeature)
    ;
        GoalExpr0 = if_then_else(Vars0, Cond0, Then0, Else0),
        construct_anchors(branch_ite, Goal0, StartAnchor, EndAnchor),
        rename_var_list(need_not_rename, !.VarRename, Vars0, Vars),
        record_decisions_in_goal(Cond0, Cond, !VarInfo, !VarRename, InsertMap,
            MaybeFeature),
        record_decisions_in_goal(Then0, Then, !VarInfo, !.VarRename, _,
            InsertMap, MaybeFeature),
        lookup_inserts(InsertMap, StartAnchor, StartInserts),
        make_inserted_goals(!VarInfo, map.init, VarRenameElse,
            StartInserts, MaybeFeature, StartInsertGoals),
        record_decisions_in_goal(Else0, Else1, !VarInfo, VarRenameElse, _,
            InsertMap, MaybeFeature),
        Else0 = hlds_goal(_, ElseGoalInfo0),
        conj_list_to_goal(list.append(StartInsertGoals, [Else1]),
            ElseGoalInfo0, Else),
        Goal1 = hlds_goal(if_then_else(Vars, Cond, Then, Else), GoalInfo0),
        lookup_inserts(InsertMap, EndAnchor, EndInserts),
        insert_goals_after(Goal1, Goal, !VarInfo, !:VarRename, EndInserts,
            MaybeFeature)
    ;
        GoalExpr0 = scope(Reason0, SubGoal0),
        (
            Reason0 = exist_quant(Vars0),
            rename_var_list(need_not_rename, !.VarRename, Vars0, Vars),
            Reason = exist_quant(Vars)
        ;
            Reason0 = from_ground_term(Var0, Kind),
            rename_var(need_not_rename, !.VarRename, Var0, Var),
            Reason = from_ground_term(Var, Kind)
        ;
            ( Reason0 = disable_warnings(_, _)
            ; Reason0 = promise_purity(_)
            ; Reason0 = promise_solutions(_, _)
            ; Reason0 = require_detism(_)
            ; Reason0 = require_complete_switch(_)
            ; Reason0 = require_switch_arms_detism(_, _)
            ; Reason0 = commit(_)
            ; Reason0 = barrier(_)
            ; Reason0 = trace_goal(_, _, _, _, _)
            ; Reason0 = loop_control(_, _, _)
            ),
            Reason = Reason0
        ),
        ( if Reason = from_ground_term(_, from_ground_term_construct) then
            % There won't be any decisions to record.
            Goal = Goal0
        else
            % XXX We could treat from_ground_term_deconstruct scopes specially
            % as well.
            record_decisions_in_goal(SubGoal0, SubGoal, !VarInfo, !VarRename,
                InsertMap, MaybeFeature),
            GoalExpr = scope(Reason, SubGoal),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        )
    ;
        GoalExpr0 = generic_call(GenericCall, _, _, _, _),
        % Casts are generated inline.
        (
            ( GenericCall = cast(_)
            ; GenericCall = subtype_coerce
            ),
            MustHaveMap = no
        ;
            ( GenericCall = higher_order(_, _, _, _)
            ; GenericCall = class_method(_, _, _, _)
            ; GenericCall = event_call(_)
            ),
            MustHaveMap = yes
        ),
        record_decisions_at_call_site(Goal0, Goal, !VarInfo, !VarRename,
            MustHaveMap, InsertMap, MaybeFeature)
    ;
        GoalExpr0 = plain_call(_, _, _, Builtin, _, _),
        (
            Builtin = inline_builtin,
            MustHaveMap = no
        ;
            Builtin = not_builtin,
            MustHaveMap = yes
        ),
        record_decisions_at_call_site(Goal0, Goal, !VarInfo, !VarRename,
            MustHaveMap, InsertMap, MaybeFeature)
    ;
        GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _),
        record_decisions_at_call_site(Goal0, Goal, !VarInfo,
            !VarRename, no, InsertMap, MaybeFeature)
    ;
        GoalExpr0 = unify(_, _, _, _, _),
        rename_some_vars_in_goal(!.VarRename, Goal0, Goal)
    ;
        GoalExpr0 = shorthand(_),
        unexpected($pred, "shorthand")
    ).

%-----------------------------------------------------------------------------%

:- pred lookup_inserts(insert_map::in, anchor::in, list(insert_spec)::out)
    is det.

lookup_inserts(InsertMap, Anchor, Inserts) :-
    ( if map.search(InsertMap, Anchor, InsertsPrime) then
        Inserts = InsertsPrime
    else
        Inserts = []
    ).

:- pred insert_goals_after(hlds_goal::in, hlds_goal::out,
    interval_var_info::in, interval_var_info::out, rename_map::out,
    list(insert_spec)::in, maybe(goal_feature)::in) is det.

insert_goals_after(BranchesGoal, Goal, !VarInfo, VarRename, Inserts,
        MaybeFeature) :-
    make_inserted_goals(!VarInfo, map.init, VarRename, Inserts, MaybeFeature,
        InsertGoals),
    BranchesGoal = hlds_goal(_, BranchesGoalInfo),
    conj_list_to_goal([BranchesGoal | InsertGoals], BranchesGoalInfo, Goal).

:- pred make_inserted_goals(interval_var_info::in, interval_var_info::out,
    rename_map::in, rename_map::out, list(insert_spec)::in,
    maybe(goal_feature)::in, list(hlds_goal)::out) is det.

make_inserted_goals(!VarInfo, !VarRename, [], _MaybeFeature, []).
make_inserted_goals(!VarInfo, !VarRename, [Spec | Specs], MaybeFeature,
        [Goal | Goals]) :-
    make_inserted_goal(!VarInfo, !VarRename, Spec, MaybeFeature, Goal),
    make_inserted_goals(!VarInfo, !VarRename, Specs, MaybeFeature, Goals).

:- pred make_inserted_goal(interval_var_info::in, interval_var_info::out,
    rename_map::in, rename_map::out, insert_spec::in,
    maybe(goal_feature)::in, hlds_goal::out) is det.

make_inserted_goal(!VarInfo, !VarRename, Spec, MaybeFeature, Goal) :-
    Spec = insert_spec(Goal0, VarsToExtract),
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    ( if
        GoalExpr0 = unify(_, _, _, Unification0, _),
        Unification0 = deconstruct(_, _, ArgVars, _, _, _)
    then
        Unification1 = Unification0 ^ deconstruct_can_fail := cannot_fail,
        GoalExpr1 = GoalExpr0 ^ unify_kind := Unification1,
        goal_info_set_determinism(detism_det, GoalInfo0, GoalInfo1),
        (
            MaybeFeature = yes(Feature),
            goal_info_add_feature(Feature, GoalInfo1, GoalInfo2)
        ;
            MaybeFeature = no,
            GoalInfo2 = GoalInfo1
        ),
        Goal2 = hlds_goal(GoalExpr1, GoalInfo2),
        !.VarInfo = interval_var_info(VarSet0, VarTypes0),
        create_shadow_vars(ArgVars, VarsToExtract, VarSet0, VarSet,
            VarTypes0, VarTypes, map.init, NewRename, map.init, VoidRename),
        !:VarInfo = interval_var_info(VarSet, VarTypes),
        map.old_merge(!.VarRename, NewRename, !:VarRename),
        % We rename the original goal.
        rename_some_vars_in_goal(!.VarRename, Goal2, Goal3),
        rename_some_vars_in_goal(VoidRename, Goal3, Goal)
    else
        unexpected($pred, "not a deconstruct")
    ).

make_inserted_goal(VarSet0, VarSet, VarTypes0, VarTypes, !RenameMap,
        InsertSpec, MaybeFeature, Goal) :-
    Info0 = interval_var_info(VarSet0, VarTypes0),
    make_inserted_goal(Info0, Info, !RenameMap, InsertSpec,
        MaybeFeature, Goal),
    Info = interval_var_info(VarSet, VarTypes).

:- pred create_shadow_vars(list(prog_var)::in, set_of_progvar::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    rename_map::in, rename_map::out, rename_map::in, rename_map::out)
    is det.

create_shadow_vars([], _, !VarSet, !VarTypes, !VarRename, !VoidRename).
create_shadow_vars([Arg | Args], VarsToExtract, !VarSet, !VarTypes,
        !VarRename, !VoidRename) :-
    create_shadow_var(Arg, VarsToExtract, !VarSet, !VarTypes,
        !VarRename, !VoidRename),
    create_shadow_vars(Args, VarsToExtract, !VarSet, !VarTypes,
        !VarRename, !VoidRename).

:- pred create_shadow_var(prog_var::in, set_of_progvar::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    rename_map::in, rename_map::out, rename_map::in, rename_map::out) is det.

create_shadow_var(Arg, VarsToExtract, !VarSet, !VarTypes,
        !VarRename, !VoidRename) :-
    varset.lookup_name(!.VarSet, Arg, Name),
    varset.new_named_var(Name, Shadow, !VarSet),
    lookup_var_type(!.VarTypes, Arg, Type),
    add_var_type(Shadow, Type, !VarTypes),
    ( if set_of_var.member(VarsToExtract, Arg) then
        map.det_insert(Arg, Shadow, !VarRename)
    else
        map.det_insert(Arg, Shadow, !VoidRename)
    ).

%-----------------------------------------------------------------------------%

:- pred record_decisions_at_call_site(hlds_goal::in, hlds_goal::out,
    interval_var_info::in, interval_var_info::out,
    rename_map::in, rename_map::out, bool::in, insert_map::in,
    maybe(goal_feature)::in) is det.

record_decisions_at_call_site(Goal0, Goal, !VarInfo, !VarRename,
        MustHaveMap, InsertMap, MaybeFeature) :-
    Goal0 = hlds_goal(_, GoalInfo0),
    rename_some_vars_in_goal(!.VarRename, Goal0, Goal1),
    ( if
        goal_info_maybe_get_maybe_need_across_call(GoalInfo0,
            MaybeNeedAcrossCall),
        MaybeNeedAcrossCall = yes(_NeedAcrossCall)
    then
        GoalId = goal_info_get_goal_id(GoalInfo0),
        Anchor = anchor_call_site(GoalId),
        lookup_inserts(InsertMap, Anchor, Inserts),
        insert_goals_after(Goal1, Goal, !VarInfo, !:VarRename, Inserts,
            MaybeFeature)
    else
        (
            MustHaveMap = no,
            Goal = Goal1
        ;
            MustHaveMap = yes,
            unexpected($pred, "no save map")
        )
    ).

%-----------------------------------------------------------------------------%

:- pred record_decisions_in_conj(list(hlds_goal)::in, list(hlds_goal)::out,
    interval_var_info::in, interval_var_info::out,
    rename_map::in, rename_map::out, conj_type::in, insert_map::in,
    maybe(goal_feature)::in) is det.

record_decisions_in_conj([], [], !VarInfo, !VarRename, _, _, _).
record_decisions_in_conj([Goal0 | Goals0], Goals, !VarInfo, !VarRename,
        ConjType, InsertMap, MaybeFeature) :-
    record_decisions_in_goal(Goal0, Goal, !VarInfo, !VarRename,
        InsertMap, MaybeFeature),
    record_decisions_in_conj(Goals0, TailGoals, !VarInfo, !VarRename,
        ConjType, InsertMap, MaybeFeature),
    ( if
        Goal = hlds_goal(conj(InnerConjType, SubGoals), _),
        ConjType = InnerConjType
    then
        Goals = SubGoals ++ TailGoals
    else
        Goals = [Goal | TailGoals]
    ).

:- pred record_decisions_in_disj(list(hlds_goal)::in, list(hlds_goal)::out,
    interval_var_info::in, interval_var_info::out,
    rename_map::in, list(insert_spec)::in, insert_map::in,
    maybe(goal_feature)::in) is det.

record_decisions_in_disj([], [], !VarInfo, _, _, _, _).
record_decisions_in_disj([Goal0 | Goals0], [Goal | Goals], !VarInfo,
        VarRename0, Inserts, InsertMap, MaybeFeature) :-
    make_inserted_goals(!VarInfo, map.init, VarRename1,
        Inserts, MaybeFeature, InsertGoals),
    Goal0 = hlds_goal(_, GoalInfo0),
    record_decisions_in_goal(Goal0, Goal1, !VarInfo, VarRename1, _,
        InsertMap, MaybeFeature),
    conj_list_to_goal(list.append(InsertGoals, [Goal1]), GoalInfo0, Goal),
    record_decisions_in_disj(Goals0, Goals, !VarInfo, VarRename0,
        Inserts, InsertMap, MaybeFeature).

:- pred record_decisions_in_cases(list(case)::in, list(case)::out,
    interval_var_info::in, interval_var_info::out,
    rename_map::in, insert_map::in, maybe(goal_feature)::in) is det.

record_decisions_in_cases([], [], !VarInfo, _, _, _).
record_decisions_in_cases([Case0 | Cases0], [Case | Cases],
        !VarInfo, VarRename0, InsertMap, MaybeFeature) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    record_decisions_in_goal(Goal0, Goal, !VarInfo, VarRename0, _,
        InsertMap, MaybeFeature),
    Case = case(MainConsId, OtherConsIds, Goal),
    record_decisions_in_cases(Cases0, Cases, !VarInfo, VarRename0,
        InsertMap, MaybeFeature).

%-----------------------------------------------------------------------------%

apply_headvar_correction(HeadVarSet, RenameMap, Goal0, Goal) :-
    HeadVars = set_of_var.to_sorted_list(HeadVarSet),
    build_headvar_subst(HeadVars, RenameMap, map.init, Subst),
    ( if map.is_empty(Subst) then
        Goal = Goal0
    else
        rename_some_vars_in_goal(Subst, Goal0, Goal)
    ).

:- pred build_headvar_subst(list(prog_var)::in, rename_map::in,
    map(prog_var, prog_var)::in, map(prog_var, prog_var)::out) is det.

build_headvar_subst([], _RenameMap, !Subst).
build_headvar_subst([HeadVar | HeadVars], RenameMap, !Subst) :-
    ( if map.search(RenameMap, HeadVar, Replacement) then
        map.det_insert(Replacement, HeadVar, !Subst),
        map.det_insert(HeadVar, Replacement, !Subst)
    else
        true
    ),
    build_headvar_subst(HeadVars, RenameMap, !Subst).

%-----------------------------------------------------------------------------%

:- pred construct_anchors(branch_construct::in, hlds_goal::in,
    anchor::out, anchor::out) is det.

construct_anchors(Construct, Goal, StartAnchor, EndAnchor) :-
    Goal = hlds_goal(_, GoalInfo),
    GoalId = goal_info_get_goal_id(GoalInfo),
    StartAnchor = anchor_branch_start(Construct, GoalId),
    EndAnchor = anchor_branch_end(Construct, GoalId).

%-----------------------------------------------------------------------------%

% For debugging purposes.

dump_interval_info(Stream, IntervalInfo, !IO) :-
    map.keys(IntervalInfo ^ ii_interval_start, StartIds),
    map.keys(IntervalInfo ^ ii_interval_end, EndIds),
    map.keys(IntervalInfo ^ ii_interval_vars, VarsIds),
    map.keys(IntervalInfo ^ ii_interval_succ, SuccIds),
    list.condense([StartIds, EndIds, VarsIds, SuccIds], IntervalIds0),
    list.sort_and_remove_dups(IntervalIds0, IntervalIds),
    io.write_string(Stream, "INTERVALS:\n", !IO),
    list.foldl(dump_interval_info_id(Stream, IntervalInfo), IntervalIds, !IO),

    map.to_assoc_list(IntervalInfo ^ ii_anchor_follow_map, AnchorFollows),
    io.write_string(Stream, "\nANCHOR FOLLOW:\n", !IO),
    list.foldl(dump_anchor_follow(Stream), AnchorFollows, !IO).

:- pred dump_interval_info_id(io.text_output_stream::in, interval_info::in,
    interval_id::in, io::di, io::uo) is det.

dump_interval_info_id(Stream, IntervalInfo, IntervalId, !IO) :-
    io.format(Stream, "\ninterval %d:",
        [i(interval_id_to_int(IntervalId))], !IO),
    ( if map.search(IntervalInfo ^ ii_interval_succ, IntervalId, SuccIds) then
        SuccNums = list.map(interval_id_to_int, SuccIds),
        io.format(Stream, "succ [%s]\n",
            [s(int_list_to_string(SuccNums))], !IO)
    else
        io.write_string(Stream, "no succ\n", !IO)
    ),
    ( if map.search(IntervalInfo ^ ii_interval_start, IntervalId, Start) then
        io.write_string(Stream, "start ", !IO),
        io.write_line(Stream, Start, !IO)
    else
        io.write_string(Stream, "no start\n", !IO)
    ),
    ( if map.search(IntervalInfo ^ ii_interval_end, IntervalId, End) then
        io.write_string(Stream, "end ", !IO),
        io.write_line(Stream, End, !IO)
    else
        io.write_string(Stream, "no end\n", !IO)
    ),
    ( if map.search(IntervalInfo ^ ii_interval_vars, IntervalId, Vars) then
        list.map(term.var_to_int, set_of_var.to_sorted_list(Vars), VarNums),
        io.format(Stream, "vars [%s]\n",
            [s(int_list_to_string(VarNums))], !IO)
    else
        io.write_string(Stream, "no vars\n", !IO)
    ),
    ( if
        map.search(IntervalInfo ^ ii_interval_delvars, IntervalId, Deletions)
    then
        io.write_string(Stream, "deletions", !IO),
        list.foldl(dump_deletion(Stream), Deletions, !IO),
        io.write_string(Stream, "\n", !IO)
    else
        true
    ).

:- pred dump_deletion(io.text_output_stream::in, set_of_progvar::in,
    io::di, io::uo) is det.

dump_deletion(Stream, Vars, !IO) :-
    list.map(term.var_to_int, set_of_var.to_sorted_list(Vars), VarNums),
    io.format(Stream, " [%s]", [s(int_list_to_string(VarNums))], !IO).

:- pred dump_anchor_follow(io.text_output_stream::in,
    pair(anchor, anchor_follow_info)::in, io::di, io::uo) is det.

dump_anchor_follow(Stream, Anchor - AnchorFollowInfo, !IO) :-
    AnchorFollowInfo = anchor_follow_info(Vars, Intervals),
    list.map(term.var_to_int, set_of_var.to_sorted_list(Vars), VarNums),
    set.to_sorted_list(Intervals, IntervalList),
    IntervalIntList = list.map(interval_id_to_int, IntervalList),
    io.write_string(Stream, "\n", !IO),
    io.write(Stream, Anchor, !IO),
    io.write_string(Stream, " =>\n", !IO),
    io.format(Stream, "vars [%s]\n",
        [s(int_list_to_string(VarNums))], !IO),
    io.format(Stream, "intervals: %s\n",
        [s(int_list_to_string(IntervalIntList))], !IO).

int_list_to_string(Ints) = IntsStr :-
    IntStrs = list.map(string.int_to_string, Ints),
    IntsStr = string.join_list(", ", IntStrs).

interval_id_to_int(interval_id(Num)) = Num.

%-----------------------------------------------------------------------------%
:- end_module backend_libs.interval.
%-----------------------------------------------------------------------------%
