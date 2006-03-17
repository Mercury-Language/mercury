%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: unique_modes.m.
% Main author: fjh

% This module checks that variables with a unique mode (as opposed to a
% mostly-unique mode) really are unique, and not nondet live - i.e. that
% they cannot be referenced on backtracking. (Actually the term "nondet live"
% is a bit of a misnomer, because really we are just interested in whether
% something can be referenced on backtracking, and this can occur after
% backtracking in semidet code too, not just in nondet code.)
%
% Basically we just traverse each goal, keeping track of which variables are
% nondet live. At each procedure call, we check that any arguments whose
% initial insts are required to be unique are not nondet live. If they are,
% we first try selecting a different mode of the same predicate, and if that
% fails, then we report an error message.
%
% Variables can become nondet live in several places: in negations, in the
% conditions of if-then-elses, in disjunctions, and at nondet calls. These are
% the only places at which we can resume execution after backtracking.
%
% XXX We currently make the conservative assumption that any non-local variable
% in a disjunction or nondet call is nondet-live - and stays nondet-live.

%-----------------------------------------------------------------------------%

:- module check_hlds.unique_modes.
:- interface.

:- import_module check_hlds.mode_info.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module io.
:- import_module set.

%-----------------------------------------------------------------------------%

    % Check every predicate in a module.
    %
:- pred check_module(module_info::in, module_info::out, io::di, io::uo) is det.

    % Just check a single procedure.
    %
:- pred check_proc(proc_id::in, pred_id::in, module_info::in, module_info::out,
    bool::out, io::di, io::uo) is det.

    % Just check a single goal.
    %
:- pred check_goal(hlds_goal::in, hlds_goal::out,
    mode_info::in, mode_info::out, io::di, io::uo) is det.

    % Prepare for checking a disjunct in a disjunction.
    %
:- pred prepare_for_disjunct(hlds_goal::in, determinism::in, set(prog_var)::in,
    mode_info::in, mode_info::out) is det.

    % Make all nondet-live variables whose current inst
    % is `unique' become `mostly_unique'.
    %
:- pred make_all_nondet_live_vars_mostly_uniq(mode_info::in, mode_info::out)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_match.
:- import_module check_hlds.inst_util.
:- import_module check_hlds.modecheck_call.
:- import_module check_hlds.modecheck_unify.
:- import_module check_hlds.mode_debug.
:- import_module check_hlds.mode_errors.
:- import_module check_hlds.modes.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.unify_proc.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_out.
:- import_module hlds.instmap.
:- import_module hlds.passes_aux.
:- import_module libs.compiler_util.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_out.

:- import_module assoc_list.
:- import_module bag.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module std_util.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

check_module(!ModuleInfo, !IO) :-
    check_pred_modes(check_unique_modes, may_change_called_proc, !ModuleInfo,
        _UnsafeToContinue, !IO).

check_proc(ProcId, PredId, !ModuleInfo, Changed, !IO) :-
    modecheck_proc_general(ProcId, PredId, check_unique_modes,
        may_change_called_proc, !ModuleInfo, NumErrors, Changed, !IO),
    ( NumErrors \= 0 ->
        io.set_exit_status(1, !IO)
    ;
        true
    ).

check_goal(Goal0, Goal, !ModeInfo, !IO) :-
    Goal0 = GoalExpr0 - GoalInfo0,
    goal_info_get_context(GoalInfo0, Context),
    term.context_init(EmptyContext),
    ( Context = EmptyContext ->
        true
    ;
        mode_info_set_context(Context, !ModeInfo)
    ),

    mode_info_get_instmap(!.ModeInfo, InstMap0),
    % Grab the original bag of nondet-live vars.
    mode_info_get_nondet_live_vars(!.ModeInfo, NondetLiveVars0),

    % If the goal is not nondet, then nothing is nondet-live, so reset the bag
    % of nondet-live vars to be empty.
    goal_info_get_determinism(GoalInfo0, Detism),
    ( determinism_components(Detism, _, at_most_many) ->
        true
    ;
        mode_info_set_nondet_live_vars(bag.init, !ModeInfo)
    ),

    check_goal_2(GoalExpr0, GoalInfo0, GoalExpr, !ModeInfo, !IO),
    % Restore the original bag of nondet-live vars.
    mode_info_set_nondet_live_vars(NondetLiveVars0, !ModeInfo),

    % Grab the final instmap, compute the change in insts over this goal,
    % and save that instmap_delta in the goal_info.
    compute_goal_instmap_delta(InstMap0, GoalExpr, GoalInfo0, GoalInfo,
        !ModeInfo),
    Goal = GoalExpr - GoalInfo.

make_all_nondet_live_vars_mostly_uniq(ModeInfo0, ModeInfo) :-
    mode_info_get_instmap(ModeInfo0, FullInstMap0),
    ( instmap.is_reachable(FullInstMap0) ->
        instmap.vars_list(FullInstMap0, AllVars),
        select_nondet_live_vars(AllVars, ModeInfo0, NondetLiveVars),
        make_var_list_mostly_uniq(NondetLiveVars, ModeInfo0, ModeInfo)
    ;
        ModeInfo = ModeInfo0
    ).

:- pred select_live_vars(list(prog_var)::in, mode_info::in,
    list(prog_var)::out) is det.

select_live_vars([], _, []).
select_live_vars([Var|Vars], ModeInfo, LiveVars) :-
    ( mode_info_var_is_live(ModeInfo, Var, live) ->
        select_live_vars(Vars, ModeInfo, LiveVars1),
        LiveVars = [Var | LiveVars1]
    ;
        select_live_vars(Vars, ModeInfo, LiveVars)
    ).

:- pred select_nondet_live_vars(list(prog_var)::in, mode_info::in,
    list(prog_var)::out) is det.

select_nondet_live_vars([], _, []).
select_nondet_live_vars([Var|Vars], ModeInfo, NondetLiveVars) :-
    ( mode_info_var_is_nondet_live(ModeInfo, Var, live) ->
        select_nondet_live_vars(Vars, ModeInfo, NondetLiveVars1),
        NondetLiveVars = [Var | NondetLiveVars1]
    ;
        select_nondet_live_vars(Vars, ModeInfo, NondetLiveVars)
    ).

    % Given a list of variables, a delta instmap, and a mode_info, select all
    % the variables whose inst changed in the delta instmap (other than
    % changes which just add information, e.g. `ground -> bound(42)'.)
    %
:- pred select_changed_inst_vars(list(prog_var)::in, instmap_delta::in,
    mode_info::in, list(prog_var)::out) is det.

select_changed_inst_vars([], _DeltaInstMap, _ModeInfo, []).
select_changed_inst_vars([Var | Vars], DeltaInstMap, ModeInfo, ChangedVars) :-
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    mode_info_get_instmap(ModeInfo, InstMap0),
    instmap.lookup_var(InstMap0, Var, Inst0),
    mode_info_get_var_types(ModeInfo, VarTypes),
    map.lookup(VarTypes, Var, Type),
    (
        instmap_delta_is_reachable(DeltaInstMap),
        instmap_delta_search_var(DeltaInstMap, Var, Inst),
        \+ inst_matches_final(Inst, Inst0, Type, ModuleInfo)
    ->
        select_changed_inst_vars(Vars, DeltaInstMap, ModeInfo, ChangedVars1),
        ChangedVars = [Var | ChangedVars1]
    ;
        select_changed_inst_vars(Vars, DeltaInstMap, ModeInfo, ChangedVars)
    ).

:- pred make_var_list_mostly_uniq(list(prog_var)::in,
    mode_info::in, mode_info::out) is det.

make_var_list_mostly_uniq([], !ModeInfo).
make_var_list_mostly_uniq([Var | Vars], !ModeInfo) :-
    make_var_mostly_uniq(Var, !ModeInfo),
    make_var_list_mostly_uniq(Vars, !ModeInfo).

:- pred make_var_mostly_uniq(prog_var::in,
    mode_info::in, mode_info::out) is det.

make_var_mostly_uniq(Var, !ModeInfo) :-
    mode_info_get_instmap(!.ModeInfo, InstMap0),
    mode_info_get_module_info(!.ModeInfo, ModuleInfo0),
    (
        % Only variables which are `unique' need to be changed.
        instmap.is_reachable(InstMap0),
        instmap.vars_list(InstMap0, Vars),
        list.member(Var, Vars),
        instmap.lookup_var(InstMap0, Var, Inst0),
        inst_expand(ModuleInfo0, Inst0, Inst1),
        ( Inst1 = ground(unique, _)
        ; Inst1 = bound(unique, _)
        ; Inst1 = any(unique)
        )
    ->
        make_mostly_uniq_inst(Inst0, Inst, ModuleInfo0, ModuleInfo),
        mode_info_set_module_info(ModuleInfo, !ModeInfo),
        instmap.set(Var, Inst, InstMap0, InstMap),
        mode_info_set_instmap(InstMap, !ModeInfo)
    ;
        true
    ).

:- pred check_goal_2(hlds_goal_expr::in, hlds_goal_info::in,
    hlds_goal_expr::out, mode_info::in, mode_info::out,
    io::di, io::uo) is det.

check_goal_2(conj(ConjType, List0), GoalInfo0, conj(ConjType, List),
        !ModeInfo, !IO) :-
    (
        ConjType = plain_conj,
        mode_checkpoint(enter, "conj", !ModeInfo, !IO),
        (
            List0 = [],
            % For efficiency, optimize common case.
            List = []
        ;
            List0 = [_ | _],
            mode_info_add_goals_live_vars(List0, !ModeInfo),
            check_conj(List0, List, !ModeInfo, !IO)
        ),
        mode_checkpoint(exit, "conj", !ModeInfo, !IO)
    ;
        ConjType = parallel_conj,
        mode_checkpoint(enter, "par_conj", !ModeInfo, !IO),
        goal_info_get_nonlocals(GoalInfo0, NonLocals),
        mode_info_add_live_vars(NonLocals, !ModeInfo),
        % Build a multiset of the nonlocals of the conjuncts so that we can
        % figure out which variables must be made shared at the start of the
        % parallel conjunction.
        make_par_conj_nonlocal_multiset(List0, NonLocalsBag),
        check_par_conj(List0, NonLocalsBag, List, InstMapList, !ModeInfo, !IO),
        instmap.unify(NonLocals, InstMapList, !ModeInfo),
        mode_info_remove_live_vars(NonLocals, !ModeInfo),
        mode_checkpoint(exit, "par_conj", !ModeInfo, !IO)
    ).

check_goal_2(disj(List0), GoalInfo0, disj(List), !ModeInfo, !IO) :-
    mode_checkpoint(enter, "disj", !ModeInfo, !IO),
    (
        List0 = [],
        List = [],
        instmap.init_unreachable(InstMap),
        mode_info_set_instmap(InstMap, !ModeInfo)
    ;
        List0 = [_ | _],
        % If the disjunction creates a choice point (i.e. is model_non), then
        % mark all the variables which are live at the start of the disjunction
        % and whose inst is `unique' as instead being only `mostly_unique',
        % since those variables may be needed again after we backtrack to that
        % choice point and resume forward execution again.
        %
        % Note: for model_det or model_semi disjunctions, we may do some
        % "shallow" backtracking from semidet disjuncts. But we handle that
        % separately for each disjunct, in check_disj.

        goal_info_get_nonlocals(GoalInfo0, NonLocals),
        goal_info_get_determinism(GoalInfo0, Determinism),
        % does this disjunction create a choice point?
        ( determinism_components(Determinism, _, at_most_many) ->
            mode_info_add_live_vars(NonLocals, !ModeInfo),
            make_all_nondet_live_vars_mostly_uniq(!ModeInfo),
            mode_info_remove_live_vars(NonLocals, !ModeInfo)
        ;
            true
        ),

        % Now just modecheck each disjunct in turn, and then
        % merge the resulting instmaps.
        check_disj(List0, Determinism, NonLocals, List, InstMapList,
            !ModeInfo, !IO),
        instmap.merge(NonLocals, InstMapList, disj, !ModeInfo)
    ),
    mode_checkpoint(exit, "disj", !ModeInfo, !IO).

check_goal_2(if_then_else(Vars, Cond0, Then0, Else0), GoalInfo0, Goal,
        !ModeInfo, !IO) :-
    mode_checkpoint(enter, "if-then-else", !ModeInfo, !IO),
    goal_info_get_nonlocals(GoalInfo0, NonLocals),
    goal_get_nonlocals(Cond0, Cond_Vars),
    goal_get_nonlocals(Then0, Then_Vars),
    goal_get_nonlocals(Else0, Else_Vars),
    mode_info_get_instmap(!.ModeInfo, InstMap0),
    mode_info_lock_vars(if_then_else, NonLocals, !ModeInfo),

    % At this point, we should set the inst of any `unique' variables which
    % occur in the condition and which are live to `mostly_unique'. However,
    % if a variable's inst was unchanged over the condition (i.e. it remains
    % `unique' on exit from the condition), then it is safe to leave it as
    % `unique' on entry to the condition. The only case we need to set it
    % to `mostly_unique' is if the condition would clobber it.
    %
    % XXX Actually that is not true; the code below does the wrong thing
    % for examples such as this one:
    %
    % :- mode p(di).
    % p(Var) :-
    %   (if
    %       (if semidet_succeed then
    %           clobber(Var), fail
    %       else
    %           true
    %       )
    %   then
    %       blah
    %   else
    %       use(Var)
    %   ).

    mode_info_add_live_vars(Else_Vars, !ModeInfo),
    set.to_sorted_list(Cond_Vars, Cond_Vars_List),
    select_live_vars(Cond_Vars_List, !.ModeInfo, Cond_Live_Vars),
    Cond0 = _ - Cond0_GoalInfo,
    goal_info_get_instmap_delta(Cond0_GoalInfo, Cond0_DeltaInstMap),
    select_changed_inst_vars(Cond_Live_Vars, Cond0_DeltaInstMap, !.ModeInfo,
        ChangedVars),
    make_var_list_mostly_uniq(ChangedVars, !ModeInfo),
    mode_info_remove_live_vars(Else_Vars, !ModeInfo),

    mode_info_add_live_vars(Then_Vars, !ModeInfo),
    check_goal(Cond0, Cond, !ModeInfo, !IO),
    mode_info_remove_live_vars(Then_Vars, !ModeInfo),
    mode_info_unlock_vars(if_then_else, NonLocals, !ModeInfo),
    mode_info_get_instmap(!.ModeInfo, InstMapCond),
    ( instmap.is_reachable(InstMapCond) ->
        check_goal(Then0, Then, !ModeInfo, !IO),
        mode_info_get_instmap(!.ModeInfo, InstMapThen)
    ;
        % We should not mode-analyse the goal, since it is unreachable.
        % Instead we optimize the goal away, so that later passes
        % won't complain about it not having unique mode information.
        Then = true_goal,
        InstMapThen = InstMapCond
    ),
    mode_info_set_instmap(InstMap0, !ModeInfo),
    check_goal(Else0, Else, !ModeInfo, !IO),
    mode_info_get_instmap(!.ModeInfo, InstMapElse),
    mode_info_set_instmap(InstMap0, !ModeInfo),
    instmap.merge(NonLocals, [InstMapThen, InstMapElse], if_then_else,
        !ModeInfo),
    Goal = if_then_else(Vars, Cond, Then, Else),
    mode_checkpoint(exit, "if-then-else", !ModeInfo, !IO).

check_goal_2(not(SubGoal0), GoalInfo0, not(SubGoal), !ModeInfo, !IO) :-
    mode_checkpoint(enter, "not", !ModeInfo, !IO),
    mode_info_get_instmap(!.ModeInfo, InstMap0),

    % We need to mark all the variables which are live after the negation
    % as nondet-live for the negated goal, since if the negated goal fails,
    % then the negation will succeed, and so these variables can be accessed
    % again after backtracking.
    goal_info_get_nonlocals(GoalInfo0, NonLocals),
    set.to_sorted_list(NonLocals, NonLocalsList),
    select_live_vars(NonLocalsList, !.ModeInfo, LiveNonLocals),
    make_var_list_mostly_uniq(LiveNonLocals, !ModeInfo),

    % But nothing is forward-live for the negated goal, since if the goal
    % succeeds then execution will immediately backtrack. So we need to set
    % the live variables set to empty here.
    mode_info_get_live_vars(!.ModeInfo, LiveVars0),
    mode_info_set_live_vars(bag.init, !ModeInfo),

    % We need to lock the non-local variables, to ensure that the negation
    % does not bind them.
    mode_info_lock_vars(negation, NonLocals, !ModeInfo),
    check_goal(SubGoal0, SubGoal, !ModeInfo, !IO),
    mode_info_unlock_vars(negation, NonLocals, !ModeInfo),
    mode_info_set_live_vars(LiveVars0, !ModeInfo),
    mode_info_set_instmap(InstMap0, !ModeInfo),
    mode_checkpoint(exit, "not", !ModeInfo, !IO).

check_goal_2(scope(Reason, SubGoal0), _, scope(Reason, SubGoal),
        !ModeInfo, !IO) :-
    mode_checkpoint(enter, "some", !ModeInfo, !IO),
    check_goal(SubGoal0, SubGoal, !ModeInfo, !IO),
    mode_checkpoint(exit, "some", !ModeInfo, !IO).

check_goal_2(generic_call(GenericCall, Args, Modes, Det), _GoalInfo0, Goal,
        !ModeInfo, !IO) :-
    mode_checkpoint(enter, "generic_call", !ModeInfo, !IO),
    hlds_goal.generic_call_id(GenericCall, CallId),
    mode_info_set_call_context(call(CallId), !ModeInfo),
    ( determinism_components(Det, _, at_most_zero) ->
        NeverSucceeds = yes
    ;
        NeverSucceeds = no
    ),
    (
        GenericCall = higher_order(_, _, _, _),
        ArgOffset = 1
    ;
        % Class method calls are introduced by the compiler
        % and should be mode correct.
        GenericCall = class_method(_, _, _, _),
        ArgOffset = 0
    ;
        % Casts are introduced by the compiler and should be mode correct.
        GenericCall = cast(_),
        ArgOffset = 0
    ),
    check_call_modes(Args, Modes, ArgOffset, Det, NeverSucceeds, !ModeInfo),
    Goal = generic_call(GenericCall, Args, Modes, Det),
    mode_info_unset_call_context(!ModeInfo),
    mode_checkpoint(exit, "generic_call", !ModeInfo, !IO).

check_goal_2(call(PredId, ProcId0, Args, Builtin, CallContext,
        PredName), GoalInfo0, Goal, !ModeInfo, !IO) :-
    sym_name_to_string(PredName, PredNameString),
    string.append("call ", PredNameString, CallString),
    mode_checkpoint(enter, CallString, !ModeInfo, !IO),
    mode_info_get_call_id(!.ModeInfo, PredId, CallId),
    mode_info_set_call_context(call(call(CallId)), !ModeInfo),
    check_call(PredId, ProcId0, Args, GoalInfo0, ProcId, !ModeInfo),
    Goal = call(PredId, ProcId, Args, Builtin, CallContext, PredName),
    mode_info_unset_call_context(!ModeInfo),
    mode_checkpoint(exit, "call", !ModeInfo, !IO).

check_goal_2(unify(LHS0, RHS0, _, UnifyInfo0, UnifyContext), GoalInfo0, Goal,
        !ModeInfo, !IO) :-
    mode_checkpoint(enter, "unify", !ModeInfo, !IO),
    mode_info_set_call_context(unify(UnifyContext), !ModeInfo),
    modecheck_unification(LHS0, RHS0, UnifyInfo0, UnifyContext, GoalInfo0,
        Goal, !ModeInfo, !IO),
    mode_info_unset_call_context(!ModeInfo),
    mode_checkpoint(exit, "unify", !ModeInfo, !IO).

check_goal_2(switch(Var, CanFail, Cases0), GoalInfo0,
        switch(Var, CanFail, Cases), !ModeInfo, !IO) :-
    mode_checkpoint(enter, "switch", !ModeInfo, !IO),
    (
        Cases0 = [],
        Cases = [],
        instmap.init_unreachable(InstMap),
        mode_info_set_instmap(InstMap, !ModeInfo)
    ;
        Cases0 = [_ | _],
        goal_info_get_nonlocals(GoalInfo0, NonLocals),
        check_case_list(Cases0, Var, Cases, InstMapList, !ModeInfo, !IO),
        instmap.merge(NonLocals, InstMapList, disj, !ModeInfo)
    ),
    mode_checkpoint(exit, "switch", !ModeInfo, !IO).

check_goal_2(foreign_proc(Attributes, PredId, ProcId0, Args, ExtraArgs,
        PragmaCode), GoalInfo0, Goal, !ModeInfo, !IO) :-
    % To modecheck a pragma_c_code, we just modecheck the proc for
    % which it is the goal.
    mode_checkpoint(enter, "foreign_proc", !ModeInfo, !IO),
    mode_info_get_call_id(!.ModeInfo, PredId, CallId),
    mode_info_set_call_context(call(call(CallId)), !ModeInfo),
    ArgVars = list.map(foreign_arg_var, Args),
    check_call(PredId, ProcId0, ArgVars, GoalInfo0, ProcId, !ModeInfo),
    Goal = foreign_proc(Attributes, PredId, ProcId, Args, ExtraArgs,
        PragmaCode),
    mode_info_unset_call_context(!ModeInfo),
    mode_checkpoint(exit, "foreign_proc", !ModeInfo, !IO).

check_goal_2(shorthand(_), _, _, !ModeInfo, !IO) :-
    % These should have been expanded out by now.
    unexpected(this_file, "check_goal_2: unexpected shorthand").

:- pred check_call(pred_id::in, proc_id::in, list(prog_var)::in,
    hlds_goal_info::in, proc_id::out, mode_info::in, mode_info::out) is det.

check_call(PredId, ProcId0, ArgVars, GoalInfo, ProcId, !ModeInfo) :-
    % Set the error list to empty for use below
    % (saving the old error list and instmap in variables).
    mode_info_get_errors(!.ModeInfo, OldErrors),
    mode_info_get_instmap(!.ModeInfo, InstMap0),
    mode_info_set_errors([], !ModeInfo),

    % First off, try using the existing mode.
    mode_info_get_module_info(!.ModeInfo, ModuleInfo),
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId0,
            PredInfo, ProcInfo),
    compute_arg_offset(PredInfo, ArgOffset),
    proc_info_argmodes(ProcInfo, ProcArgModes0),
    proc_info_interface_determinism(ProcInfo, InterfaceDeterminism),
    proc_info_never_succeeds(ProcInfo, NeverSucceeds),
    check_call_modes(ArgVars, ProcArgModes0, ArgOffset, InterfaceDeterminism,
        NeverSucceeds, !ModeInfo),
    ModeErrors = ProcInfo ^ mode_errors,
    (
        ModeErrors = [_ | _],
        % mode error in callee for this mode
        WaitingVars = set.list_to_set(ArgVars),
        mode_info_get_instmap(!.ModeInfo, InstMap),
        instmap.lookup_vars(ArgVars, InstMap, ArgInsts),
        mode_info_error(WaitingVars,
            mode_error_in_callee(ArgVars, ArgInsts, PredId, ProcId0,
                ProcInfo ^ mode_errors),
            !ModeInfo)
    ;
        ModeErrors = []
    ),

    % See whether or not that worked (and restore the old error list).
    mode_info_get_errors(!.ModeInfo, Errors),
    mode_info_set_errors(OldErrors, !ModeInfo),
    mode_info_get_may_change_called_proc(!.ModeInfo, MayChangeCalledProc),
    ( Errors = [] ->
        ProcId = ProcId0
    ; MayChangeCalledProc = may_not_change_called_proc ->
        % We're not allowed to try a different procedure here, so just return
        % all the errors.
        ProcId = ProcId0,
        list.append(OldErrors, Errors, AllErrors),
        mode_info_set_errors(AllErrors, !ModeInfo)
    ;
        % If it didn't work, restore the original instmap, and then call
        % modecheck_call_pred. That will try all the modes, and will infer
        % new ones if necessary.
        %
        % We set the declared determinism for newly inferred modes to be the
        % same as the determinism inferred for the existing mode selected by
        % ordinary (non-unique) mode analysis. This means that determinism
        % analysis will report an error if the determinism changes as a result
        % of unique mode analysis.  That is OK, because uniqueness should not
        % affect determinism.
        mode_info_set_instmap(InstMap0, !ModeInfo),
        proc_info_inferred_determinism(ProcInfo, Determinism),
        modecheck_call_pred(PredId, yes(Determinism), ProcId0, ProcId,
            ArgVars, NewArgVars, GoalInfo, ExtraGoals, !ModeInfo),

        (
            NewArgVars = ArgVars,
            ExtraGoals = no_extra_goals
        ->
            true
        ;
            % This shouldn't happen, since modes.m should do all the handling
            % of implied modes
            % XXX it might happen, though, if the user
            % XXX writes strange code; we should report
            % XXX a proper error here
            unexpected(this_file, "call to implied mode?")
        )
    ).

    % To check a call, we just look up the required initial insts for the
    % arguments of the call, and then check for each argument if the variable
    % is nondet-live and the required initial inst was unique.
    %
:- pred check_call_modes(list(prog_var)::in, list(mer_mode)::in, int::in,
    determinism::in, bool::in, mode_info::in, mode_info::out) is det.

check_call_modes(ArgVars, ProcArgModes, ArgOffset, Determinism, NeverSucceeds,
        !ModeInfo) :-
    mode_info_get_module_info(!.ModeInfo, ModuleInfo),
    mode_list_get_initial_insts(ModuleInfo, ProcArgModes, InitialInsts),
    NeedExactMatch = no,
    modecheck_var_has_inst_list(ArgVars, InitialInsts,
        NeedExactMatch, ArgOffset, InstVarSub, !ModeInfo),
    mode_list_get_final_insts(ModuleInfo, ProcArgModes, FinalInsts0),
    inst_list_apply_substitution(InstVarSub, FinalInsts0, FinalInsts),
    modecheck_set_var_inst_list(ArgVars, InitialInsts, FinalInsts,
        ArgOffset, NewArgVars, ExtraGoals, !ModeInfo),
    (
        NewArgVars = ArgVars,
        ExtraGoals = no_extra_goals
    ->
        true
    ;
        % This shouldn't happen, since modes.m should do all the handling
        % of implied modes.
        unexpected(this_file, "call to implied mode?")
    ),
    (
        NeverSucceeds = yes,
        instmap.init_unreachable(InstMap),
        mode_info_set_instmap(InstMap, !ModeInfo)
    ;
        NeverSucceeds = no,
        % Check whether we are at a call to a nondet predicate.
        % If so, mark all the currently nondet-live variables
        % whose inst is `unique' as instead being only `mostly_unique'.
        ( determinism_components(Determinism, _, at_most_many) ->
            make_all_nondet_live_vars_mostly_uniq(!ModeInfo)
        ;
            true
        )
    ).

%-----------------------------------------------------------------------------%

:- pred check_conj(list(hlds_goal)::in, list(hlds_goal)::out,
    mode_info::in, mode_info::out, io::di, io::uo) is det.

    % Just process each conjunct in turn.  Note that we don't do any
    % reordering of conjuncts here, although we do flatten conjunctions.
    %
check_conj([], [], !ModeInfo, !IO).
check_conj([Goal0 | Goals0], Goals, !ModeInfo, !IO) :-
    (
        Goal0 = conj(plain_conj, ConjGoals) - _
    ->
        list.append(ConjGoals, Goals0, Goals1),
        check_conj(Goals1, Goals, !ModeInfo, !IO)
    ;
        check_conj_2(Goal0, Goals0, Goals, !ModeInfo, !IO)
    ).

:- pred check_conj_2(hlds_goal::in, list(hlds_goal)::in, list(hlds_goal)::out,
    mode_info::in, mode_info::out, io::di, io::uo) is det.

check_conj_2(Goal0, Goals0, [Goal | Goals], !ModeInfo, !IO) :-
    goal_get_nonlocals(Goal0, NonLocals),
    mode_info_remove_live_vars(NonLocals, !ModeInfo),
    check_goal(Goal0, Goal, !ModeInfo, !IO),
    mode_info_get_instmap(!.ModeInfo, InstMap),
    ( instmap.is_unreachable(InstMap) ->
        % We should not mode-analyse the remaining goals, since they are
        % unreachable. Instead we optimize them away, so that later passes
        % won't complain about them not having unique mode information.
        mode_info_remove_goals_live_vars(Goals0, !ModeInfo),
        Goals = []
    ;
        check_conj(Goals0, Goals, !ModeInfo, !IO)
    ).

%-----------------------------------------------------------------------------%

    % Make_par_conj_nonlocal_multiset builds a multiset (bag) of all
    % the nonlocals of the conjuncts.
    %
:- pred make_par_conj_nonlocal_multiset(list(hlds_goal)::in,
    bag(prog_var)::out) is det.

make_par_conj_nonlocal_multiset([], Empty) :-
    bag.init(Empty).
make_par_conj_nonlocal_multiset([Goal | Goals], NonLocalsMultiSet) :-
    make_par_conj_nonlocal_multiset(Goals, NonLocalsMultiSet0),
    goal_get_nonlocals(Goal, NonLocals),
    set.to_sorted_list(NonLocals, NonLocalsList),
    bag.from_list(NonLocalsList, NonLocalsMultiSet1),
    bag.union(NonLocalsMultiSet0, NonLocalsMultiSet1, NonLocalsMultiSet).

    % To unique-modecheck a parallel conjunction, we find the variables
    % that are nonlocal to more than one conjunct and make them shared,
    % then we unique-modecheck the conjuncts.
    %
    % The variables that occur in more than one conjunct must be shared
    % because otherwise it would be possible to make them become clobbered
    % which would introduce an implicit dependency between the conjuncts
    % which we do not allow.
    %
:- pred check_par_conj(list(hlds_goal)::in, bag(prog_var)::in,
    list(hlds_goal)::out, list(pair(instmap, set(prog_var)))::out,
    mode_info::in, mode_info::out, io::di, io::uo) is det.

check_par_conj(Goals0, NonLocalVarsBag, Goals, Instmaps, !ModeInfo, !IO) :-
    check_par_conj_0(NonLocalVarsBag, !ModeInfo),
    check_par_conj_1(Goals0, Goals, Instmaps, !ModeInfo, !IO).

    % Figure out which variables occur in more than one conjunct and make them
    % shared.
    %
:- pred check_par_conj_0(bag(prog_var)::in, mode_info::in, mode_info::out)
    is det.

check_par_conj_0(NonLocalVarsBag, !ModeInfo) :-
    bag.to_assoc_list(NonLocalVarsBag, NonLocalVarsList),
    list.filter_map((pred(Pair::in, Var::out) is semidet :-
        Pair = Var - Multiplicity,
        Multiplicity > 1
    ), NonLocalVarsList, SharedList),
    mode_info_get_instmap(!.ModeInfo, InstMap0),
    instmap.lookup_vars(SharedList, InstMap0, VarInsts),
    mode_info_get_module_info(!.ModeInfo, ModuleInfo0),
    make_shared_inst_list(VarInsts, SharedVarInsts,
        ModuleInfo0, ModuleInfo1),
    mode_info_set_module_info(ModuleInfo1, !ModeInfo),
    instmap.set_vars(SharedList, SharedVarInsts, InstMap0, InstMap1),
    mode_info_set_instmap(InstMap1, !ModeInfo).

    % Just process each conjunct in turn. Because we have already done
    % modechecking, we know that there are no attempts to bind a variable in
    % multiple parallel conjuncts, so we don't need to lock/unlock variables.
    %
:- pred check_par_conj_1(list(hlds_goal)::in,
    list(hlds_goal)::out, list(pair(instmap, set(prog_var)))::out,
    mode_info::in, mode_info::out, io::di, io::uo) is det.

check_par_conj_1([], [], [], !ModeInfo, !IO).
check_par_conj_1([Goal0 | Goals0], [Goal | Goals],
        [InstMap - NonLocals | InstMaps], !ModeInfo, !IO) :-
    goal_get_nonlocals(Goal0, NonLocals),
    mode_info_get_instmap(!.ModeInfo, InstMap0),
    check_goal(Goal0, Goal, !ModeInfo, !IO),
    mode_info_get_instmap(!.ModeInfo, InstMap),
    mode_info_set_instmap(InstMap0, !ModeInfo),
    check_par_conj_1(Goals0, Goals, InstMaps, !ModeInfo, !IO).

%-----------------------------------------------------------------------------%

    % Process each of the disjunctions in turn, making sure to restore the
    % original instmap before processing the next one. Collect up a list
    % of the resulting instmaps.
    %
:- pred check_disj(list(hlds_goal)::in, determinism::in, set(prog_var)::in,
    list(hlds_goal)::out, list(instmap)::out, mode_info::in, mode_info::out,
    io::di, io::uo) is det.

check_disj([], _, _, [], [], !ModeInfo, !IO).
check_disj([Goal0 | Goals0], DisjDetism, DisjNonLocals, [Goal | Goals],
        [InstMap | InstMaps], !ModeInfo, !IO) :-
    mode_info_get_instmap(!.ModeInfo, InstMap0),
    % If you modify this code, you may also need to modify
    % unique_modecheck_clause_disj or the code that calls it.

    prepare_for_disjunct(Goal0, DisjDetism, DisjNonLocals, !ModeInfo),
    check_goal(Goal0, Goal, !ModeInfo, !IO),
    mode_info_get_instmap(!.ModeInfo, InstMap),
    mode_info_set_instmap(InstMap0, !ModeInfo),
    check_disj(Goals0, DisjDetism, DisjNonLocals, Goals, InstMaps,
        !ModeInfo, !IO).

prepare_for_disjunct(Goal0, DisjDetism, DisjNonLocals, !ModeInfo) :-
    (
        % If the disjunction was model_nondet, then we already marked all the
        % non-locals as only being mostly-unique, so we don't need to do
        % anything special here...
        \+ determinism_components(DisjDetism, _, at_most_many),

        % ... but for model_semi or model_det disjunctions, if the _disjunct_
        % can fail, then we still might backtrack to another disjunct, so again
        % in that case we need to mark all the non-locals as being only
        % mostly-unique rather than unique.
        Goal0 = _ - GoalInfo0,
        goal_info_get_determinism(GoalInfo0, Determinism),
        determinism_components(Determinism, CanFail, _),
        CanFail = can_fail
    ->
        mode_info_add_live_vars(DisjNonLocals, !ModeInfo),
        make_all_nondet_live_vars_mostly_uniq(!ModeInfo),
        mode_info_remove_live_vars(DisjNonLocals, !ModeInfo)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

:- pred check_case_list(list(case)::in, prog_var::in, list(case)::out,
    list(instmap)::out, mode_info::in, mode_info::out, io::di, io::uo) is det.

check_case_list([], _Var, [], [], !ModeInfo, !IO).
check_case_list([Case0 | Cases0], Var, [Case | Cases], [InstMap | InstMaps],
        !ModeInfo, !IO) :-
    Case0 = case(ConsId, Goal0),
    Case = case(ConsId, Goal),
    mode_info_get_instmap(!.ModeInfo, InstMap0),

    % If you modify this code, you may also need to modify
    % unique_modecheck_clause_switch or the code that calls it.

    % Record the fact that Var was bound to ConsId in the instmap before
    % processing this case.
    modecheck_functor_test(Var, ConsId, !ModeInfo),

    mode_info_get_instmap(!.ModeInfo, InstMap1),
    ( instmap.is_reachable(InstMap1) ->
        check_goal(Goal0, Goal1, !ModeInfo, !IO)
    ;
        % We should not mode-analyse the goal, since it is unreachable.
        % Instead we optimize the goal away, so that later passes
        % won't complain about it not having unique mode information.
        Goal1 = true_goal
    ),

    mode_info_get_instmap(!.ModeInfo, InstMap),
    fixup_switch_var(Var, InstMap0, InstMap, Goal1, Goal),

    mode_info_set_instmap(InstMap0, !ModeInfo),
    check_case_list(Cases0, Var, Cases, InstMaps, !ModeInfo, !IO).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "unique_modes.m".

%-----------------------------------------------------------------------------%
:- end_module unique_modes.
%-----------------------------------------------------------------------------%
