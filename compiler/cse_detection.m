%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1995-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Common subexpression detection - hoist common subexpression goals out of
% branched structures. This can enable us to find more indexing opportunities
% and hence can make the code more deterministic.
% This code is switched on/off with the `--common-goal' option.
%
% Main author: zs.
% Much of the code is based on switch_detection.m by fjh.
%
%-----------------------------------------------------------------------------%

:- module check_hlds__cse_detection.

:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

:- import_module io.

:- pred detect_cse(module_info::in, module_info::out, io::di, io::uo) is det.

:- pred detect_cse_in_proc(proc_id::in, pred_id::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.det_util.
:- import_module check_hlds.inst_match.
:- import_module check_hlds.modes.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.switch_detection.
:- import_module check_hlds.switch_detection.
:- import_module check_hlds.type_util.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_out.
:- import_module hlds.instmap.
:- import_module hlds.quantification.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module multi_map.
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module string.
:- import_module svmap.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

detect_cse(!ModuleInfo, !IO) :-
    % Traverse the module structure, calling `detect_cse_in_goal'
    % for each procedure body.
    module_info_predids(!.ModuleInfo, PredIds),
    detect_cse_in_preds(PredIds, !ModuleInfo, !IO).

:- pred detect_cse_in_preds(list(pred_id)::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

detect_cse_in_preds([], !ModuleInfo, !IO).
detect_cse_in_preds([PredId | PredIds], !ModuleInfo, !IO) :-
    module_info_preds(!.ModuleInfo, PredTable),
    map__lookup(PredTable, PredId, PredInfo),
    detect_cse_in_pred(PredId, PredInfo, !ModuleInfo, !IO),
    detect_cse_in_preds(PredIds, !ModuleInfo, !IO).

:- pred detect_cse_in_pred(pred_id::in, pred_info::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

detect_cse_in_pred(PredId, PredInfo0, !ModuleInfo, !IO) :-
    ProcIds = pred_info_non_imported_procids(PredInfo0),
    detect_cse_in_procs(ProcIds, PredId, !ModuleInfo, !IO).

:- pred detect_cse_in_procs(list(proc_id)::in, pred_id::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

detect_cse_in_procs([], _PredId, !ModuleInfo, !IO).
detect_cse_in_procs([ProcId | ProcIds], PredId, !ModuleInfo, !IO) :-
    detect_cse_in_proc(ProcId, PredId, !ModuleInfo, !IO),
    detect_cse_in_procs(ProcIds, PredId, !ModuleInfo, !IO).

detect_cse_in_proc(ProcId, PredId, !ModuleInfo, !IO) :-
    detect_cse_in_proc_2(ProcId, PredId, Redo, !ModuleInfo),
    globals__io_lookup_bool_option(detailed_statistics, Statistics, !IO),
    maybe_report_stats(Statistics, !IO),
    (
        Redo = no
    ;
        Redo = yes,
        globals__io_lookup_bool_option(very_verbose, VeryVerbose, !IO),
        (
            VeryVerbose = yes,
            io__write_string("% Repeating mode check for ", !IO),
            hlds_out__write_pred_id(!.ModuleInfo, PredId, !IO),
            io__write_string("\n", !IO)
        ;
            VeryVerbose = no
        ),
        modecheck_proc(ProcId, PredId, !ModuleInfo, Errs, _Changed, !IO),
        maybe_report_stats(Statistics, !IO),
        ( Errs > 0 ->
            unexpected(this_file, "mode check fails when repeated")
        ;
            true
        ),
        (
            VeryVerbose = yes,
            io__write_string("% Repeating switch detection for ", !IO),
            hlds_out__write_pred_id(!.ModuleInfo, PredId, !IO),
            io__write_string("\n", !IO)
        ;
            VeryVerbose = no
        ),
        detect_switches_in_proc(ProcId, PredId, !ModuleInfo),
        maybe_report_stats(Statistics, !IO),
        (
            VeryVerbose = yes,
            io__write_string("% Repeating common " ++
                "deconstruction detection for ", !IO),
            hlds_out__write_pred_id(!.ModuleInfo, PredId, !IO),
            io__write_string("\n", !IO)
        ;
            VeryVerbose = no
        ),
        detect_cse_in_proc(ProcId, PredId, !ModuleInfo, !IO)
    ).

:- type cse_info
    --->    cse_info(
                varset          :: prog_varset,
                vartypes        :: vartypes,
                rtti_varmaps    :: rtti_varmaps,
                module_info     :: module_info
            ).

:- pred detect_cse_in_proc_2(proc_id::in, pred_id::in, bool::out,
    module_info::in, module_info::out) is det.

detect_cse_in_proc_2(ProcId, PredId, Redo, ModuleInfo0, ModuleInfo) :-
    module_info_preds(ModuleInfo0, PredTable0),
    map__lookup(PredTable0, PredId, PredInfo0),
    pred_info_procedures(PredInfo0, ProcTable0),
    map__lookup(ProcTable0, ProcId, ProcInfo0),

    % To process each ProcInfo, we get the goal, initialize the instmap
    % based on the modes of the head vars, and pass these to
    % `detect_cse_in_goal'.

    proc_info_goal(ProcInfo0, Goal0),
    proc_info_get_initial_instmap(ProcInfo0, ModuleInfo0, InstMap0),
    proc_info_varset(ProcInfo0, Varset0),
    proc_info_vartypes(ProcInfo0, VarTypes0),
    proc_info_rtti_varmaps(ProcInfo0, RttiVarMaps0),
    CseInfo0 = cse_info(Varset0, VarTypes0, RttiVarMaps0, ModuleInfo0),
    detect_cse_in_goal(Goal0, InstMap0, CseInfo0, CseInfo, Redo, Goal1),

    (
        Redo = no,
        ModuleInfo = ModuleInfo0
    ;
        Redo = yes,

        % ModuleInfo should not be changed by detect_cse_in_goal.
        CseInfo = cse_info(VarSet1, VarTypes1, RttiVarMaps, _),
        proc_info_headvars(ProcInfo0, HeadVars),

        implicitly_quantify_clause_body(HeadVars, _Warnings,
            Goal1, Goal, VarSet1, VarSet, VarTypes1, VarTypes),

        proc_info_set_goal(Goal, ProcInfo0, ProcInfo1),
        proc_info_set_varset(VarSet, ProcInfo1, ProcInfo2),
        proc_info_set_vartypes(VarTypes, ProcInfo2, ProcInfo3),
        proc_info_set_rtti_varmaps(RttiVarMaps, ProcInfo3, ProcInfo),

        map__det_update(ProcTable0, ProcId, ProcInfo, ProcTable),
        pred_info_set_procedures(ProcTable, PredInfo0, PredInfo),
        map__det_update(PredTable0, PredId, PredInfo, PredTable),
        module_info_set_preds(PredTable, ModuleInfo0, ModuleInfo)
    ).

%-----------------------------------------------------------------------------%

    % Given a goal, and the instmap on entry to that goal,
    % find disjunctions that contain common subexpressions
    % and hoist these out of the disjunction. At the moment
    % we only look for cses that are deconstruction unifications.
    %
:- pred detect_cse_in_goal(hlds_goal::in, instmap::in, cse_info::in,
    cse_info::out, bool::out, hlds_goal::out) is det.

detect_cse_in_goal(Goal0, InstMap0, !CseInfo, Redo, Goal) :-
    detect_cse_in_goal_1(Goal0, InstMap0, !CseInfo, Redo, Goal, _InstMap).

    % This version is the same as the above except that it returns
    % the resulting instmap on exit from the goal, which is
    % computed by applying the instmap delta specified in the
    % goal's goalinfo.
    %
:- pred detect_cse_in_goal_1(hlds_goal::in, instmap::in, cse_info::in,
    cse_info::out, bool::out, hlds_goal::out, instmap::out) is det.

detect_cse_in_goal_1(Goal0 - GoalInfo, InstMap0, !CseInfo, Redo,
        Goal - GoalInfo, InstMap) :-
    detect_cse_in_goal_2(Goal0, GoalInfo, InstMap0, !CseInfo, Redo, Goal),
    goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
    instmap__apply_instmap_delta(InstMap0, InstMapDelta, InstMap).

    % Here we process each of the different sorts of goals.
    %
:- pred detect_cse_in_goal_2(hlds_goal_expr::in, hlds_goal_info::in,
    instmap::in, cse_info::in, cse_info::out, bool::out,
    hlds_goal_expr::out) is det.

detect_cse_in_goal_2(Goal @ foreign_proc(_, _, _, _, _, _), _, _, !CseInfo,
        no, Goal).
detect_cse_in_goal_2(Goal @ generic_call(_, _, _, _), _, _, !CseInfo,
        no, Goal).
detect_cse_in_goal_2(Goal @ call(_, _, _, _, _, _), _, _, !CseInfo, no, Goal).
detect_cse_in_goal_2(unify(LHS, RHS0, Mode, Unify,  UnifyContext), _, InstMap0,
        !CseInfo, Redo, unify(LHS, RHS, Mode,Unify, UnifyContext)) :-
    (
        RHS0 = lambda_goal(Purity, PredOrFunc, EvalMethod, FixModes,
            NonLocalVars, Vars, Modes, Det, Goal0)
    ->
        ModuleInfo = !.CseInfo ^ module_info,
        instmap__pre_lambda_update(ModuleInfo, Vars, Modes, InstMap0, InstMap),
        detect_cse_in_goal(Goal0, InstMap, !CseInfo, Redo, Goal),
        RHS = lambda_goal(Purity, PredOrFunc, EvalMethod, FixModes,
            NonLocalVars, Vars, Modes, Det, Goal)
    ;
        RHS = RHS0,
        Redo = no
    ).
detect_cse_in_goal_2(not(Goal0), _GoalInfo, InstMap, !CseInfo, Redo,
        not(Goal)) :-
    detect_cse_in_goal(Goal0, InstMap, !CseInfo, Redo, Goal).
detect_cse_in_goal_2(scope(Reason, Goal0), _GoalInfo, InstMap,
        !CseInfo, Redo, scope(Reason, Goal)) :-
    detect_cse_in_goal(Goal0, InstMap, !CseInfo, Redo, Goal).
detect_cse_in_goal_2(conj(Goals0), _GoalInfo, InstMap, !CseInfo, Redo,
        conj(Goals)) :-
    detect_cse_in_conj(Goals0, InstMap, !CseInfo, Redo, Goals).
detect_cse_in_goal_2(par_conj(Goals0), _, InstMap, !CseInfo, Redo,
        par_conj(Goals)) :-
    detect_cse_in_par_conj(Goals0, InstMap, !CseInfo, Redo, Goals).
detect_cse_in_goal_2(disj(Goals0), GoalInfo, InstMap, !CseInfo, Redo, Goal) :-
    ( Goals0 = [] ->
        Redo = no,
        Goal = disj([])
    ;
        goal_info_get_nonlocals(GoalInfo, NonLocals),
        set__to_sorted_list(NonLocals, NonLocalsList),
        detect_cse_in_disj(NonLocalsList, Goals0, GoalInfo,
            InstMap, !CseInfo, Redo, Goal)
    ).
detect_cse_in_goal_2(switch(Var, CanFail, Cases0), GoalInfo, InstMap,
        !CseInfo, Redo, Goal) :-
    goal_info_get_nonlocals(GoalInfo, NonLocals),
    set__to_sorted_list(NonLocals, NonLocalsList),
    detect_cse_in_cases(NonLocalsList, Var, CanFail, Cases0, GoalInfo,
        InstMap, !CseInfo, Redo, Goal).
detect_cse_in_goal_2(if_then_else(Vars, Cond0, Then0, Else0), GoalInfo,
        InstMap, !CseInfo, Redo, Goal) :-
    goal_info_get_nonlocals(GoalInfo, NonLocals),
    set__to_sorted_list(NonLocals, NonLocalsList),
    detect_cse_in_ite(NonLocalsList, Vars, Cond0, Then0, Else0, GoalInfo,
        InstMap, !CseInfo, Redo, Goal).

detect_cse_in_goal_2(shorthand(_), _, _, _, _, _, _) :-
    % These should have been expanded out by now.
    unexpected(this_file, "detect_cse_in_goal_2: unexpected shorthand").

%-----------------------------------------------------------------------------%

:- pred detect_cse_in_conj(list(hlds_goal)::in, instmap::in, cse_info::in,
    cse_info::out, bool::out, list(hlds_goal)::out) is det.

detect_cse_in_conj([], _InstMap, !CseInfo, no, []).
detect_cse_in_conj([Goal0 | Goals0], InstMap0, !CseInfo, Redo, Goals) :-
    detect_cse_in_goal_1(Goal0, InstMap0, !CseInfo, Redo1, Goal1, InstMap1),
    detect_cse_in_conj(Goals0, InstMap1, !CseInfo, Redo2, Goals1),
    ( Goal1 = conj(ConjGoals) - _ ->
        list__append(ConjGoals, Goals1, Goals)
    ;
        Goals = [Goal1 | Goals1]
    ),
    bool__or(Redo1, Redo2, Redo).

%-----------------------------------------------------------------------------%

:- pred detect_cse_in_par_conj(list(hlds_goal)::in, instmap::in, cse_info::in,
    cse_info::out, bool::out, list(hlds_goal)::out) is det.

detect_cse_in_par_conj([], _InstMap, !CseInfo, no, []).
detect_cse_in_par_conj([Goal0 | Goals0], InstMap0, !CseInfo, Redo,
        [Goal | Goals]) :-
    detect_cse_in_goal(Goal0, InstMap0, !CseInfo, Redo1, Goal),
    detect_cse_in_par_conj(Goals0, InstMap0, !CseInfo, Redo2, Goals),
    bool__or(Redo1, Redo2, Redo).

%-----------------------------------------------------------------------------%

    % These are the interesting bits - we've found a non-empty branched
    % structure, and we've got a list of the non-local variables of that
    % structure. Now for each non-local variable, we check whether each
    % branch matches that variable against the same functor.
    %
:- pred detect_cse_in_disj(list(prog_var)::in, list(hlds_goal)::in,
    hlds_goal_info::in, instmap::in, cse_info::in,
    cse_info::out, bool::out, hlds_goal_expr::out) is det.

detect_cse_in_disj([], Goals0, _, InstMap, !CseInfo, Redo, disj(Goals)) :-
    detect_cse_in_disj_2(Goals0, InstMap, !CseInfo, Redo, Goals).
detect_cse_in_disj([Var | Vars], Goals0, GoalInfo0, InstMap,
        !CseInfo, Redo, Goal) :-
    (
        instmap__lookup_var(InstMap, Var, VarInst0),
        ModuleInfo = !.CseInfo ^ module_info,
        % XXX we only need inst_is_bound, but leave this as it is
        % until mode analysis can handle aliasing between free
        % variables.
        inst_is_ground_or_any(ModuleInfo, VarInst0),
        common_deconstruct(Goals0, Var, !CseInfo, Unify,
            FirstOldNew, LaterOldNew, Goals)
    ->
        maybe_update_existential_data_structures(Unify,
            FirstOldNew, LaterOldNew, !CseInfo),
        Goal = conj([Unify, disj(Goals) - GoalInfo0]),
        Redo = yes
    ;
        detect_cse_in_disj(Vars, Goals0, GoalInfo0, InstMap,
            !CseInfo, Redo, Goal)
    ).

:- pred detect_cse_in_disj_2(list(hlds_goal)::in, instmap::in, cse_info::in,
    cse_info::out, bool::out, list(hlds_goal)::out) is det.

detect_cse_in_disj_2([], _InstMap, !CseInfo, no, []).
detect_cse_in_disj_2([Goal0 | Goals0], InstMap0, !CseInfo, Redo,
        [Goal | Goals]) :-
    detect_cse_in_goal(Goal0, InstMap0, !CseInfo, Redo1, Goal),
    detect_cse_in_disj_2(Goals0, InstMap0, !CseInfo, Redo2, Goals),
    bool__or(Redo1, Redo2, Redo).

:- pred detect_cse_in_cases(list(prog_var)::in, prog_var::in, can_fail::in,
    list(case)::in, hlds_goal_info::in, instmap::in,
    cse_info::in, cse_info::out, bool::out, hlds_goal_expr::out) is det.

detect_cse_in_cases([], SwitchVar, CanFail, Cases0, _GoalInfo, InstMap,
        !CseInfo, Redo, switch(SwitchVar, CanFail, Cases)) :-
    detect_cse_in_cases_2(Cases0, InstMap, !CseInfo, Redo, Cases).
detect_cse_in_cases([Var | Vars], SwitchVar, CanFail, Cases0, GoalInfo,
        InstMap, !CseInfo, Redo, Goal) :-
    (
        Var \= SwitchVar,
        instmap__lookup_var(InstMap, Var, VarInst0),
        ModuleInfo = !.CseInfo ^ module_info,
        % XXX We only need inst_is_bound, but leave this as it is until
        % mode analysis can handle aliasing between free variables.
        inst_is_ground_or_any(ModuleInfo, VarInst0),
        common_deconstruct_cases(Cases0, Var, !CseInfo,
            Unify, FirstOldNew, LaterOldNew, Cases)
    ->
        maybe_update_existential_data_structures(Unify,
            FirstOldNew, LaterOldNew, !CseInfo),
        Goal = conj([Unify, switch(SwitchVar, CanFail, Cases) - GoalInfo]),
        Redo = yes
    ;
        detect_cse_in_cases(Vars, SwitchVar, CanFail, Cases0, GoalInfo,
            InstMap, !CseInfo, Redo, Goal)
    ).

:- pred detect_cse_in_cases_2(list(case)::in, instmap::in, cse_info::in,
    cse_info::out, bool::out, list(case)::out) is det.

detect_cse_in_cases_2([], _, !CseInfo, no, []).
detect_cse_in_cases_2([Case0 | Cases0], InstMap, !CseInfo, Redo,
        [Case | Cases]) :-
    Case0 = case(Functor, Goal0),
    detect_cse_in_goal(Goal0, InstMap, !CseInfo, Redo1, Goal),
    Case = case(Functor, Goal),
    detect_cse_in_cases_2(Cases0, InstMap, !CseInfo, Redo2, Cases),
    bool__or(Redo1, Redo2, Redo).

:- pred detect_cse_in_ite(list(prog_var)::in, list(prog_var)::in,
    hlds_goal::in, hlds_goal::in, hlds_goal::in, hlds_goal_info::in,
    instmap::in, cse_info::in, cse_info::out, bool::out,
    hlds_goal_expr::out) is det.

detect_cse_in_ite([], IfVars, Cond0, Then0, Else0, _, InstMap, !CseInfo,
        Redo, if_then_else(IfVars, Cond, Then, Else)) :-
    detect_cse_in_ite_2(Cond0, Then0, Else0, InstMap, !CseInfo, Redo,
        Cond, Then, Else).
detect_cse_in_ite([Var | Vars], IfVars, Cond0, Then0, Else0, GoalInfo,
        InstMap, !CseInfo, Redo, Goal) :-
    (
        ModuleInfo = !.CseInfo ^ module_info,
        instmap__lookup_var(InstMap, Var, VarInst0),
        % XXX We only need inst_is_bound, but leave this as it is until
        % mode analysis can handle aliasing between free variables.
        inst_is_ground_or_any(ModuleInfo, VarInst0),
        common_deconstruct([Then0, Else0], Var, !CseInfo,
            Unify, FirstOldNew, LaterOldNew, Goals),
        Goals = [Then, Else]
    ->
        maybe_update_existential_data_structures(Unify,
            FirstOldNew, LaterOldNew, !CseInfo),
        Goal = conj([Unify, if_then_else(IfVars, Cond0, Then, Else)
            - GoalInfo]),
        Redo = yes
    ;
        detect_cse_in_ite(Vars, IfVars, Cond0, Then0, Else0, GoalInfo,
            InstMap, !CseInfo, Redo, Goal)
    ).

:- pred detect_cse_in_ite_2(hlds_goal::in, hlds_goal::in, hlds_goal::in,
    instmap::in, cse_info::in, cse_info::out, bool::out,
    hlds_goal::out, hlds_goal::out, hlds_goal::out) is det.

detect_cse_in_ite_2(Cond0, Then0, Else0, InstMap0, !CseInfo, Redo,
        Cond, Then, Else) :-
    detect_cse_in_goal_1(Cond0, InstMap0, !CseInfo, Redo1, Cond, InstMap1),
    detect_cse_in_goal(Then0, InstMap1, !CseInfo, Redo2, Then),
    detect_cse_in_goal(Else0, InstMap0, !CseInfo, Redo3, Else),
    bool__or(Redo1, Redo2, Redo12),
    bool__or(Redo12, Redo3, Redo).

%-----------------------------------------------------------------------------%

    % common_deconstruct(Goals0, Var, !CseInfo, Unify, Goals):
    % input vars:
    %   Goals0 is a list of parallel goals in a branched structure
    %   (disjunction, if-then-else, or switch).
    %   Var is the variable we are looking for a common deconstruction on.
    %   !.CseInfo contains the original varset and type map.
    % output vars:
    %   !:CseInfo has a varset and a type map reflecting the new variables
    %   we have introduced.
    %   Goals is the modified version of Goals0 after the common deconstruction
    %   has been hoisted out, with the new variables as the functor arguments.
    %   Unify is the unification that was hoisted out.
    %
:- pred common_deconstruct(list(hlds_goal)::in, prog_var::in, cse_info::in,
    cse_info::out, hlds_goal::out, assoc_list(prog_var)::out,
    list(assoc_list(prog_var))::out, list(hlds_goal)::out) is semidet.

common_deconstruct(Goals0, Var, !CseInfo, Unify, FirstOldNew, LaterOldNew,
        Goals) :-
    common_deconstruct_2(Goals0, Var, before_candidate,
        have_candidate(Unify, FirstOldNew, LaterOldNew), !CseInfo, Goals),
    LaterOldNew = [_ | _].

:- pred common_deconstruct_2(list(hlds_goal)::in, prog_var::in,
    cse_state::in, cse_state::out, cse_info::in, cse_info::out,
    list(hlds_goal)::out) is semidet.

common_deconstruct_2([], _Var, !CseState, !CseInfo, []).
common_deconstruct_2([Goal0 | Goals0], Var, !CseState, !CseInfo,
        [Goal | Goals]) :-
    find_bind_var(Var, find_bind_var_for_cse_in_deconstruct, Goal0, Goal,
        !CseState, !CseInfo, yes),
    !.CseState = have_candidate(_, _, _),
    common_deconstruct_2(Goals0, Var, !CseState, !CseInfo, Goals).

%-----------------------------------------------------------------------------%

:- pred common_deconstruct_cases(list(case)::in, prog_var::in,
    cse_info::in, cse_info::out, hlds_goal::out, assoc_list(prog_var)::out,
    list(assoc_list(prog_var))::out, list(case)::out) is semidet.

common_deconstruct_cases(Cases0, Var, !CseInfo, Unify,
        FirstOldNew, LaterOldNew, Cases) :-
    common_deconstruct_cases_2(Cases0, Var, before_candidate,
        have_candidate(Unify, FirstOldNew, LaterOldNew), !CseInfo, Cases),
    LaterOldNew = [_ | _].

:- pred common_deconstruct_cases_2(list(case)::in, prog_var::in,
    cse_state::in, cse_state::out, cse_info::in, cse_info::out,
    list(case)::out) is semidet.

common_deconstruct_cases_2([], _Var, !CseState, !CseInfo, []).
common_deconstruct_cases_2([case(ConsId, Goal0) | Cases0], Var,
        !CseState, !CseInfo, [case(ConsId, Goal) | Cases]) :-
    find_bind_var(Var, find_bind_var_for_cse_in_deconstruct, Goal0, Goal,
        !CseState, !CseInfo, yes),
    !.CseState = have_candidate(_, _, _),
    common_deconstruct_cases_2(Cases0, Var, !CseState, !CseInfo, Cases).

%-----------------------------------------------------------------------------%

    % This data structure represents the state of the search for
    % deconstructions in all the branches of a branched control structure
    % that deconstruct a given variable with the same functor.
    % Initially, we don't know what unification we will hoist out, so the
    % state is before_candidate. When we find a unification we want to
    % hoist out, this fixes the functor, and the state is have_candidate.
    % If we find that some branches unify that variable with some other
    % functor, we have multiple_candidates, which means that we don't hoist
    % out any of them. (Although our caller may try again with another
    % variable.)
    %
    % The goal field contains the unification we are proposing to put
    % before the branched control structure. The first_old_new field
    % gives the mapping from argument variables in the old unification
    % in the first branch to the freshly created variables in the goal
    % being hoisted before the branched control structure. The
    % later_old_new field contains the same information for the second
    % and later branches.
:- type cse_state
    --->    before_candidate
    ;       have_candidate(
                goal            ::  hlds_goal,
                first_old_new   ::  assoc_list(prog_var),
                later_old_new   ::  list(assoc_list(prog_var))
            )
    ;       multiple_candidates.

:- pred find_bind_var_for_cse_in_deconstruct(prog_var::in, hlds_goal::in,
    list(hlds_goal)::out, cse_state::in, cse_state::out,
    cse_info::in, cse_info::out) is det.

find_bind_var_for_cse_in_deconstruct(Var, Goal0, Goals,
        !CseState, !CseInfo) :-
    (
        !.CseState = before_candidate,
        construct_common_unify(Var, Goal0, !CseInfo, OldNewVars,
            HoistedGoal, Goals),
        !:CseState = have_candidate(HoistedGoal, OldNewVars, [])
    ;
        !.CseState = have_candidate(HoistedGoal,
            FirstOldNewVars, LaterOldNewVars0),
        Goal0 = _ - GoalInfo,
        goal_info_get_context(GoalInfo, Context),
        (
            find_similar_deconstruct(HoistedGoal,
                Goal0, Context, OldNewVars, Goals0)
        ->
            Goals = Goals0,
            LaterOldNewVars = [OldNewVars | LaterOldNewVars0],
            !:CseState = have_candidate(HoistedGoal,
                FirstOldNewVars, LaterOldNewVars)
        ;
            Goals = [Goal0],
            !:CseState = multiple_candidates
        )
    ;
        !.CseState = multiple_candidates,
        Goals = [Goal0],
        !:CseState = multiple_candidates
    ).

:- pred construct_common_unify(prog_var::in, hlds_goal::in,
    cse_info::in, cse_info::out, assoc_list(prog_var)::out,
    hlds_goal::out, list(hlds_goal)::out) is det.

construct_common_unify(Var, GoalExpr0 - GoalInfo, !CseInfo, OldNewVars,
        HoistedGoal, Replacements) :-
    (
        GoalExpr0 = unify(_, Term, Umode, Unif0, Ucontext),
        Unif0 = deconstruct(_, Consid, Args, Submodes, CanFail, CanCGC)
    ->
        Unif = deconstruct(Var, Consid, Args, Submodes, CanFail, CanCGC),
        ( Term = functor(_, _, _) ->
            GoalExpr1 = unify(Var, Term, Umode, Unif, Ucontext)
        ;
            unexpected(this_file,
                "non-functor unify in construct_common_unify")
        ),
        goal_info_get_context(GoalInfo, Context),
        create_parallel_subterms(Args, Context, Ucontext,
            !CseInfo, OldNewVars, Replacements),
        map__from_assoc_list(OldNewVars, Sub),
        goal_util__rename_vars_in_goal(Sub,
            GoalExpr1 - GoalInfo, HoistedGoal)
    ;
        unexpected(this_file, "non-unify goal in construct_common_unify")
    ).

:- pred create_parallel_subterms(list(prog_var)::in, prog_context::in,
    unify_context::in, cse_info::in, cse_info::out,
    assoc_list(prog_var)::out, list(hlds_goal)::out) is det.

create_parallel_subterms([], _, _, !CseInfo, [], []).
create_parallel_subterms([OFV | OFV0], Context, UnifyContext, !CseInfo,
        OldNewVars, Replacements) :-
    create_parallel_subterms(OFV0, Context, UnifyContext, !CseInfo,
        OldNewVars1, Replacements1),
    create_parallel_subterm(OFV, Context, UnifyContext, !CseInfo,
        OldNewVars1, OldNewVars, Goal),
    Replacements = [Goal | Replacements1].

:- pred create_parallel_subterm(prog_var::in, prog_context::in,
    unify_context::in, cse_info::in, cse_info::out,
    assoc_list(prog_var)::in, assoc_list(prog_var)::out,
    hlds_goal::out) is det.

create_parallel_subterm(OFV, Context, UnifyContext, !CseInfo, !OldNewVar,
        Goal) :-
    VarSet0 = !.CseInfo ^ varset,
    VarTypes0 = !.CseInfo ^ vartypes,
    varset__new_var(VarSet0, NFV, VarSet),
    map__lookup(VarTypes0, OFV, Type),
    map__det_insert(VarTypes0, NFV, Type, VarTypes),
    !:OldNewVar = [OFV - NFV | !.OldNewVar],
    UnifyContext = unify_context(MainCtxt, SubCtxt),
    % It is ok to create complicated unifications here, because we rerun
    % mode analysis on the resulting goal. It would be nicer to generate
    % the right assignment unification directly, but that would require keeping
    % track of the inst of OFV.
    create_atomic_complicated_unification(OFV, var(NFV),
        Context, MainCtxt, SubCtxt, Goal),
    !:CseInfo = !.CseInfo ^ varset := VarSet,
    !:CseInfo = !.CseInfo ^ vartypes := VarTypes.

%-----------------------------------------------------------------------------%

:- pred find_similar_deconstruct(hlds_goal::in, hlds_goal::in,
    prog_context::in, assoc_list(prog_var)::out, list(hlds_goal)::out)
    is semidet.

find_similar_deconstruct(HoistedUnifyGoal, OldUnifyGoal, Context,
        OldHoistedVars, Replacements) :-
    (
        HoistedUnifyGoal = unify(_, _, _, HoistedUnifyInfo, OC) - _,
        HoistedUnifyInfo = deconstruct(_, HoistedFunctor,
            HoistedVars, _, _, _),
        OldUnifyGoal = unify(_, _, _, OldUnifyInfo, _NC) - _,
        OldUnifyInfo = deconstruct(_, OldFunctor, OldVars, _, _, _)
    ->
        HoistedFunctor = OldFunctor,
        list__length(HoistedVars, HoistedVarsCount),
        list__length(OldVars, OldVarsCount),
        HoistedVarsCount = OldVarsCount,
        assoc_list__from_corresponding_lists(OldVars, HoistedVars,
            OldHoistedVars),
        pair_subterms(OldHoistedVars, Context, OC, Replacements)
    ;
        unexpected(this_file,
            "find_similar_deconstruct: non-deconstruct unify")
    ).

:- pred pair_subterms(assoc_list(prog_var)::in, prog_context::in,
    unify_context::in, list(hlds_goal)::out) is det.

pair_subterms([], _Context, _UnifyContext, []).
pair_subterms([OldVar - HoistedVar | OldHoistedVars], Context, UnifyContext,
        Replacements) :-
    pair_subterms(OldHoistedVars, Context, UnifyContext, Replacements1),
    ( OldVar = HoistedVar ->
        Replacements = Replacements1
    ;
        UnifyContext = unify_context(MainCtxt, SubCtxt),
        % It is ok to create complicated unifications here, because we rerun
        % mode analysis on the resulting goal. It would be nicer to generate
        % the right assignment unification directly, but that would require
        % keeping track of the inst of OldVar.
        create_atomic_complicated_unification(HoistedVar, var(OldVar),
            Context, MainCtxt, SubCtxt, Goal),
        Replacements = [Goal | Replacements1]
    ).

%-----------------------------------------------------------------------------%

% This section handles the case where the functor involved in the
% common subexpression contains existentially typed arguments,
% whether or not they are constrained to belong to a typeclass.
% In such cases, what the compiler used to consider several distinct
% types (the types of say the first the existentially typed argument
% in the deconstructions in the different branches) become one (in this
% case, the type of the first existentially typed argument in the
% hoisted out deconstruction). The prog_vars describing the types
% of the existentially typed arguments (i.e. containing their
% typeinfos) change as well, from being some of the variables in
% in the original deconstructions to being the corresponding variables
% in the hoisted out deconstruction.
%
% As an example, consider a disjunction such as
%
%   (
%       HeadVar__2_2 = x:u(TypeClassInfo_for_v_8, V_4),
%       ...
%   ;
%       HeadVar__2_2 = x:u(TypeClassInfo_for_v_14, V_6)
%       ...
%   )
%
% The main part of cse_detection will replace this with
%
%   HeadVar__2_2 = x:u(V_17, V_16)
%   (
%       TypeClassInfo_for_v_8 = V_17,
%       V_4 = V_16,
%       ...
%   ;
%       TypeClassInfo_for_v_14 = V_17,
%       V_6 = V_16,
%       ...
%   )
%
% However, this is not enough. Since TypeClassInfo_for_v_8 and
% TypeClassInfo_for_v_14 may (and probably will) be eliminated later,
% it is imperative that the data structures in the proc_info that refer
% to them be updated to eliminate references to those variables.
% Those data structures may originally contain something like this:
%
% type_info varmap:
% T_1 (number 1) -> typeclass_info(TypeClassInfo_for_v_8, 1)
% T_3 (number 3) -> typeclass_info(TypeClassInfo_for_v_14, 1)
% typeclass_info varmap:
% x:v(T_1) -> TypeClassInfo_for_v_8
% x:v(T_3) -> TypeClassInfo_for_v_14
% variable types map:
% V_4 (number 4) :: T_1
% V_6 (number 6) :: T_3
%
% They must be updated like this:
%
% type_info varmap:
% T_1 (number 1) -> typeclass_info(V_17, 1)
% typeclass_info varmap:
% x:v(T_1) -> V_17
% variable types map:
% V_4 (number 4) :: T_1
% V_6 (number 6) :: T_1

:- pred maybe_update_existential_data_structures(hlds_goal::in,
    assoc_list(prog_var)::in, list(assoc_list(prog_var))::in,
    cse_info::in, cse_info::out) is det.

maybe_update_existential_data_structures(Unify, FirstOldNew, LaterOldNew,
        !CseInfo) :-
    (
        Unify = unify(_, _, _, UnifyInfo, _) - _,
        UnifyInfo = deconstruct(Var, ConsId, _, _, _, _),
        ModuleInfo = !.CseInfo ^ module_info,
        VarTypes = !.CseInfo ^ vartypes,
        map__lookup(VarTypes, Var, Type),
        type_util__is_existq_cons(ModuleInfo, Type, ConsId)
    ->
        update_existential_data_structures(FirstOldNew, LaterOldNew, !CseInfo)
    ;
        true
    ).

:- pred update_existential_data_structures(
    assoc_list(prog_var)::in, list(assoc_list(prog_var))::in,
    cse_info::in, cse_info::out) is det.

update_existential_data_structures(FirstOldNew, LaterOldNews, !CseInfo) :-
    list__condense(LaterOldNews, LaterOldNew),
    map__from_assoc_list(FirstOldNew, FirstOldNewMap),
    map__from_assoc_list(LaterOldNew, LaterOldNewMap),

    RttiVarMaps0 = !.CseInfo ^ rtti_varmaps,
    VarTypes0 = !.CseInfo ^ vartypes,

    % Build a map for all locations in the rtti_varmaps that are changed
    % by the application of FirstOldNewMap. The keys of this map are the
    % new locations, and the values are the tvars (from the first branch)
    % that have had their locations moved.
    %
    rtti_varmaps_tvars(RttiVarMaps0, TvarsList),
    list__foldl(find_type_info_locn_tvar_map(RttiVarMaps0, FirstOldNewMap),
        TvarsList, map__init, NewTvarMap),

    % Traverse TVarsList again, this time looking for locations in later
    % branches that merge with locations in the first branch. When we find one,
    % add a type substitution which represents the type variables that were
    % merged.
    %
    list__foldl(find_merged_tvars(RttiVarMaps0, LaterOldNewMap, NewTvarMap),
        TvarsList, map__init, Renaming),

    % Apply the full old->new map and the type substitution to the
    % rtti_varmaps, and apply the type substitution to the vartypes.
    %
    list__append(FirstOldNew, LaterOldNew, OldNew),
    map__from_assoc_list(OldNew, OldNewMap),
    apply_substitutions_to_rtti_varmaps(Renaming, map__init, OldNewMap,
        RttiVarMaps0, RttiVarMaps),
    map__map_values(apply_tvar_rename(Renaming), VarTypes0, VarTypes),

    !:CseInfo = !.CseInfo ^ rtti_varmaps := RttiVarMaps,
    !:CseInfo = !.CseInfo ^ vartypes := VarTypes.

:- pred apply_tvar_rename(tvar_renaming::in, prog_var::in,
    mer_type::in, mer_type::out) is det.

apply_tvar_rename(Renaming, _Var, Type0, Type) :-
    apply_variable_renaming_to_type(Renaming, Type0, Type).

:- pred find_type_info_locn_tvar_map(rtti_varmaps::in,
    map(prog_var, prog_var)::in, tvar::in,
    map(type_info_locn, tvar)::in, map(type_info_locn, tvar)::out) is det.

find_type_info_locn_tvar_map(RttiVarMaps, FirstOldNewMap, Tvar, !NewTvarMap) :-
    rtti_lookup_type_info_locn(RttiVarMaps, Tvar, TypeInfoLocn0),
    type_info_locn_var(TypeInfoLocn0, Old),
    ( map__search(FirstOldNewMap, Old, New) ->
        type_info_locn_set_var(New, TypeInfoLocn0, TypeInfoLocn),
        svmap__det_insert(TypeInfoLocn, Tvar, !NewTvarMap)
    ;
        true
    ).

:- pred find_merged_tvars(rtti_varmaps::in, map(prog_var, prog_var)::in,
    map(type_info_locn, tvar)::in, tvar::in,
    tvar_renaming::in, tvar_renaming::out) is det.

find_merged_tvars(RttiVarMaps, LaterOldNewMap, NewTvarMap, Tvar, !Renaming) :-
    rtti_lookup_type_info_locn(RttiVarMaps, Tvar, TypeInfoLocn0),
    type_info_locn_var(TypeInfoLocn0, Old),
    ( map__search(LaterOldNewMap, Old, New) ->
        type_info_locn_set_var(New, TypeInfoLocn0, TypeInfoLocn),
        map__lookup(NewTvarMap, TypeInfoLocn, NewTvar),
        ( NewTvar = Tvar ->
            true
        ;
            svmap__det_insert(Tvar, NewTvar, !Renaming)
        )
    ;
        true
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "cse_detection.m".

%-----------------------------------------------------------------------------%
