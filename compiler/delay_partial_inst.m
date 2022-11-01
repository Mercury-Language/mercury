%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2007-2012 The University of Melbourne.
% Copyright (C) 2014-2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: delay_partial_inst.m.
% Author: wangp.
%
% This module runs just after mode analysis on mode-correct procedures and
% tries to transform procedures to avoid intermediate partially instantiated
% data structures. The original impetus for this pass was the Erlang backend,
% which could not handle partially instantiated data structures at all,
% because, due to all Erlang data structures being immutable, it could not
% further instantiate any originally partially instantiated data structures.
% However, even on other backends, constructing a ground data structure
% directly is simpler and more efficient than creating a partially instantiated
% data structure and then filling it in.
%
% There are two situations we look for. The first is an implied mode call
% such as
%
%       p(f(_, _))
%
% which after mode checking looks like this:
%
%       X := f(V_1, V_2),       % partially instantiated
%       p(Y),
%       X ?= Y
%
% We transform it to this more obvious sequence which doesn't need the
% partially instantiated data structure:
%
%       p(Y),
%       Y ?= f(_, _)
%
% The other situation occurs when the user writes code that constructs data
% structures with free variables, e.g.
%
%       :- type t
%           --->    t(
%                       a :: int,
%                       b :: int
%                   ).
%
%       F ^ a = 1,
%       F ^ b = 2
%
% After mode checking we get:
%
%       V_1 = 1,
%       F := t(V_1, V_2),       % ground, free
%       V_3 = 2,
%       F => t(V_4, V_3)        % ground, ground
%
% Whereas we would like to see this:
%
%       V_1 = 1,
%       V_2 = 2,
%       F := t(V_1, V_2)
%
%-----------------------------------------------------------------------------%
%
% ALGORITHM
%
% The idea is to remove unifications that produce partially instantiated data
% structures (as the mode checker can't be counted on to move these), and keep
% track of variables which are bound to top-level functors with free arguments.
% In place of the unifications we remove, we insert the unifications for the
% sub-components which are ground. Only once the variable is ground, because
% all its sub-components are ground, do we create the top-level memory cell.
%
% The algorithm makes a single forward pass over each procedure. When we see
% a unification that binds a variable V to a functor f/n with at least one
% free argument, we add an entry to the "construction map" and delete the
% unification. The construction map records that V was bound to f/n.
% We also create new "canonical" variables for each of the arguments.
%
% When we later see a deconstruction unification of V we first unify each
% argument in the deconstruction with its corresponding "canonical" variable.
% This way we can always use the canonical variables when it comes time to
% reconstruct V, so we don't need to keep track of aliases. If the mode of the
% deconstruction unification indicates that V should be ground at the end
% of the deconstruction, we insert a construction unification using the
% canonical variables, in place of the deconstruction, and delete V's entry
% from the construction map now. Otherwise, if V is not ground, we just delete
% the deconstruction unification.
%
% To handle the problem with implied mode calls, we look for complicated
% `can_fail' unifications that have V on the left-hand side. We transform them
% as in the example above, i.e. instead of unifying a ground variable G with a
% partially instantiated V, we unify G with the functor that V is bound to.
%
% After transforming all the procedures, we requantify and rerun mode analysis,
% which should do the rest.
%
% This algorithm can't handle everything that the mode checker allows, however
% most code written in practice should be okay. Here is an example of code we
% cannot handle:
%
%   foo(Xs) :-
%       ( Xs = []
%       ; Xs = [1 | _]
%       ),
%       ( Xs = []
%       ; Xs = [_ | []]
%       ).
%
%-----------------------------------------------------------------------------%

:- module check_hlds.delay_partial_inst.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

:- pred delay_partial_inst_preds(io.text_output_stream::in,
    list(pred_id)::in, list(pred_id)::out,
    module_info::in, module_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_test.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_goal.
:- import_module hlds.instmap.
:- import_module hlds.make_goal.
:- import_module hlds.passes_aux.
:- import_module hlds.quantification.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_rename.
:- import_module parse_tree.var_db.
:- import_module parse_tree.var_table.

:- import_module assoc_list.
:- import_module bool.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%

:- type delay_partial_inst_info
    --->    delay_partial_inst_info(
                % Read-only.
                dpi_module_info :: module_info,

                % Read-write.
                dpi_var_table   :: var_table,
                dpi_changed     :: bool
            ).

    % A map from the variable to the functor to which it is bound, which maps
    % to the canonical variables assigned for that functor.
    %
    % We can actually only handle the case when a variable is definitely bound
    % to a single functor. If different disjuncts bind a variable to different
    % functors, then our algorithm won't work. So why do we use a single map
    % from the variable to (cons_id, canon_vars)? To handle code like this,
    % which can result from a reasonable predicate definition.
    %
    %   ( X := f
    %   ; X := g
    %   ; X := h(_), fail
    %   ; X := i(_), fail
    %   )
    %
    % We don't want to abort as soon as we see that "X := i(_)" is incompatible
    % with "X := h(_)". We *will* abort later if we need to look up the sole
    % functor that X could be bound to, and find that there are multiple
    % choices.
    %
:- type construct_map == map(prog_var, canon_vars_map).

:- type canon_vars_map == map(cons_id, prog_vars).

%-----------------------------------------------------------------------------%

delay_partial_inst_preds(ProgressStream, PredIds, ChangedPredIds,
        !ModuleInfo) :-
    delay_partial_inst_preds_acc(ProgressStream, PredIds,
        [], RevChangedPredIds, !ModuleInfo),
    list.reverse(RevChangedPredIds, ChangedPredIds).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred delay_partial_inst_preds_acc(io.text_output_stream::in,
    list(pred_id)::in, list(pred_id)::in, list(pred_id)::out,
    module_info::in, module_info::out) is det.

delay_partial_inst_preds_acc(_, [], !RevChangedPredIds, !ModuleInfo).
delay_partial_inst_preds_acc(ProgressStream, [PredId | PredIds],
        !RevChangedPredIds, !ModuleInfo) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    pred_info_get_proc_table(PredInfo0, ProcTable0),
    ProcIds = pred_info_valid_non_imported_procids(PredInfo0),
    list.foldl(
        delay_partial_inst_proc(ProgressStream, !.ModuleInfo, PredId,
            ProcTable0),
        ProcIds, [], ChangedProcs),
    (
        ChangedProcs = [_ | _],
        map.set_from_assoc_list(ChangedProcs, ProcTable0, ProcTable),
        pred_info_set_proc_table(ProcTable, PredInfo0, PredInfo),
        module_info_set_pred_info(PredId, PredInfo, !ModuleInfo),
        !:RevChangedPredIds = [PredId | !.RevChangedPredIds]
    ;
        ChangedProcs = []
    ),
    delay_partial_inst_preds_acc(ProgressStream, PredIds,
        !RevChangedPredIds, !ModuleInfo).

:- pred delay_partial_inst_proc(io.text_output_stream::in, module_info::in,
    pred_id::in, proc_table::in, proc_id::in,
    assoc_list(proc_id, proc_info)::in, assoc_list(proc_id, proc_info)::out)
    is det.

delay_partial_inst_proc(ProgressStream, ModuleInfo, PredId, ProcTable, ProcId,
        !ChangedProcs) :-
    trace [io(!IO)] (
        maybe_write_proc_progress_message(ProgressStream, ModuleInfo,
            "Delaying partial instantiations in", proc(PredId, ProcId), !IO)
    ),
    some [!ProcInfo] (
        map.lookup(ProcTable, ProcId, !:ProcInfo),
        proc_info_get_var_table(!.ProcInfo, VarTable0),
        proc_info_get_initial_instmap(ModuleInfo, !.ProcInfo, InstMap0),
        proc_info_get_goal(!.ProcInfo, Goal0),

        Changed0 = no,
        DelayInfo0 = delay_partial_inst_info(ModuleInfo, VarTable0, Changed0),
        delay_partial_inst_in_goal(InstMap0, Goal0, Goal,
            map.init, _ConstructMap, DelayInfo0, DelayInfo),
        DelayInfo = delay_partial_inst_info(_, VarTable, Changed),

        (
            Changed = yes,
            proc_info_set_goal(Goal, !ProcInfo),
            proc_info_set_var_table(VarTable, !ProcInfo),
            requantify_proc_general(ord_nl_maybe_lambda, !ProcInfo),
            !:ChangedProcs = [ProcId - !.ProcInfo | !.ChangedProcs],

            trace [compiletime(flag("debug_delay_partial_inst")), io(!IO)] (
                io.output_stream(Stream, !IO),
                io.write_string(Stream,
                    "predicate body BEFORE delay_partial_inst:\n", !IO),
                dump_goal_nl(Stream, ModuleInfo, vns_var_table(VarTable0),
                    Goal0, !IO),
                io.write_string(Stream,
                    "predicate body AFTER delay_partial_inst:\n", !IO),
                dump_goal_nl(Stream, ModuleInfo, vns_var_table(VarTable),
                    Goal, !IO)
            )
        ;
            Changed = no
        )
    ).

%-----------------------------------------------------------------------------%

:- pred delay_partial_inst_in_goal(instmap::in, hlds_goal::in, hlds_goal::out,
    construct_map::in, construct_map::out,
    delay_partial_inst_info::in, delay_partial_inst_info::out) is det.

delay_partial_inst_in_goal(InstMap0, Goal0, Goal, !ConstructMap, !DelayInfo) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = conj(ConjType, Goals0),
        delay_partial_inst_in_conj(InstMap0, Goals0, Goals, !ConstructMap,
            !DelayInfo),
        Goal = hlds_goal(conj(ConjType, Goals), GoalInfo0)
    ;
        GoalExpr0 = disj(Goals0),
        delay_partial_inst_in_disj(InstMap0, Goals0, Goals, !ConstructMap,
            !DelayInfo),
        Goal = hlds_goal(disj(Goals), GoalInfo0)
    ;
        GoalExpr0 = negation(NegGoal0),
        delay_partial_inst_in_goal(InstMap0, NegGoal0, NegGoal,
            !.ConstructMap, _, !DelayInfo),
        Goal = hlds_goal(negation(NegGoal), GoalInfo0)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        delay_partial_inst_in_cases(InstMap0, Cases0, Cases, !ConstructMap,
            !DelayInfo),
        Goal = hlds_goal(switch(Var, CanFail, Cases), GoalInfo0)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        update_instmap(Cond0, InstMap0, InstMapThen),
        delay_partial_inst_in_goal(InstMap0, Cond0, Cond, !ConstructMap,
            !DelayInfo),
        delay_partial_inst_in_goal(InstMapThen, Then0, Then, !ConstructMap,
            !DelayInfo),
        delay_partial_inst_in_goal(InstMap0, Else0, Else, !ConstructMap,
            !DelayInfo),
        Goal = hlds_goal(if_then_else(Vars, Cond, Then, Else), GoalInfo0)
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
            delay_partial_inst_in_goal(InstMap0, SubGoal0, SubGoal,
                !.ConstructMap, _, !DelayInfo),
            Goal = hlds_goal(scope(Reason, SubGoal), GoalInfo0)
        )
    ;
        GoalExpr0 = unify(LHS, RHS0, Mode, Unify, Context),
        (
            Unify = construct(_Var, ConsId, _Args, ArgModes, _, _, _),
            ( if
                % Is this construction of the form
                %   V = f(A1, A2, A3, ...)
                % and at least one of the arguments is free?
                %
                ( ConsId = cons(_, _, _)
                ; ConsId = tuple_cons(_)
                ),
                ModuleInfo = !.DelayInfo ^ dpi_module_info,
                some [ArgMode] (
                    list.member(ArgMode, ArgModes),
                    ArgMode = unify_modes_li_lf_ri_rf(_, _, _, RHSFinalInst),
                    inst_is_free(ModuleInfo, RHSFinalInst)
                )
            then
                delay_partial_inst_in_partial_construct(GoalInfo0, Unify, Goal,
                    !ConstructMap, !DelayInfo)
            else
                (
                    % Tranform lambda goals as well. Non-local variables in
                    % lambda goals must be any or ground so we don't carry the
                    % construct map into the lambda goal.
                    RHS0 = rhs_lambda_goal(Purity, Groundness, PredOrFunc,
                        EvalMethod, NonLocals, LambdaArgVarsModes, Detism,
                        LambdaGoal0),
                    delay_partial_inst_in_goal(InstMap0,
                        LambdaGoal0, LambdaGoal, map.init, _ConstructMap,
                        !DelayInfo),
                    RHS = rhs_lambda_goal(Purity, Groundness, PredOrFunc,
                        EvalMethod, NonLocals, LambdaArgVarsModes, Detism,
                        LambdaGoal),
                    GoalExpr = unify(LHS, RHS, Mode, Unify, Context),
                    Goal = hlds_goal(GoalExpr, GoalInfo0)
                ;
                    ( RHS0 = rhs_var(_)
                    ; RHS0 = rhs_functor(_, _, _)
                    ),
                    Goal = Goal0
                )
            )
        ;
            Unify = deconstruct(_Var, _ConsId, _Args, _ArgModes,
                _CanFail, _CanCGC),
            delay_partial_inst_in_deconstruct(Goal0, Mode, Unify, Goal,
                !ConstructMap, !DelayInfo)
        ;
            Unify = complicated_unify(_Mode, _CanFail, _TypeInfos),
            delay_partial_inst_in_complicated_unify(Goal0, LHS, RHS0,
                Unify, Goal, !ConstructMap, !DelayInfo)
        ;
            ( Unify = assign(_, _)
            ; Unify = simple_test(_, _)
            ),
            Goal = Goal0
        )
    ;
        ( GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        Goal = Goal0
    ;
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal0, OrElseGoals0, OrElseInners),
            % XXX Is it ok to ignore the updated ConstructMaps,
            % and if yes, why? This should be documented.
            delay_partial_inst_in_goal(InstMap0, MainGoal0, MainGoal,
                !.ConstructMap, _, !DelayInfo),
            delay_partial_inst_in_disj(InstMap0, OrElseGoals0, OrElseGoals,
                !.ConstructMap, _, !DelayInfo),
            ShortHand = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal, OrElseGoals, OrElseInners),
            GoalExpr = shorthand(ShortHand),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        ;
            ShortHand0 = try_goal(MaybeIO, ResultVar, SubGoal0),
            delay_partial_inst_in_goal(InstMap0, SubGoal0, SubGoal,
                !ConstructMap, !DelayInfo),
            ShortHand = try_goal(MaybeIO, ResultVar, SubGoal),
            GoalExpr = shorthand(ShortHand),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        ;
            ShortHand0 = bi_implication(_, _),
            % These should have been expanded out by now.
            unexpected($pred, "bi_implication")
        )
    ).

%-----------------------------------------------------------------------------%
%
% Handle compound goals.
%

:- pred delay_partial_inst_in_conj(instmap::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    construct_map::in, construct_map::out,
    delay_partial_inst_info::in, delay_partial_inst_info::out) is det.

delay_partial_inst_in_conj(_, [], [], !ConstructMap, !DelayInfo).
delay_partial_inst_in_conj(InstMap0, [HeadGoal0 | TailGoals0], Goals,
        !ConstructMap, !DelayInfo) :-
    delay_partial_inst_in_goal(InstMap0, HeadGoal0, HeadGoal, !ConstructMap,
        !DelayInfo),
    update_instmap(HeadGoal0, InstMap0, InstMap1),
    delay_partial_inst_in_conj(InstMap1, TailGoals0, TailGoals, !ConstructMap,
        !DelayInfo),
    goal_to_conj_list(HeadGoal, HeadGoals),
    Goals = HeadGoals ++ TailGoals.

:- pred delay_partial_inst_in_disj(instmap::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    construct_map::in, construct_map::out,
    delay_partial_inst_info::in, delay_partial_inst_info::out) is det.

delay_partial_inst_in_disj(_, [], [], !ConstructMap, !DelayInfo).
delay_partial_inst_in_disj(InstMap0, [Goal0 | Goals0], [Goal | Goals],
        !ConstructMap, !DelayInfo) :-
    % Each time that a variable X is bound to a partially instantiated term
    % with functor f/n somewhere in the disjunction, we want the same set of
    % "canonical" variables to name the individual arguments of f/n.
    % That is why we thread the construct map through the disjunctions,
    % so we don't end up with different canonical variables per disjunct.
    %
    % XXX we depend on the fact that (it seems) after mode checking a
    % variable won't become ground in each of the disjuncts, but rather
    % will become ground after the disjunction as a whole. Otherwise
    % entries could be removed from the construct map in earlier disjuncts
    % that should be visible in later disjuncts. To lift this assumption we
    % would need to use separate construct maps per disjunct, merge them
    % afterwards and renaming variables so that there is only one set of
    % canonical variables.
    %
    delay_partial_inst_in_goal(InstMap0, Goal0, Goal, !ConstructMap,
        !DelayInfo),
    delay_partial_inst_in_disj(InstMap0, Goals0, Goals, !ConstructMap,
        !DelayInfo).

:- pred delay_partial_inst_in_cases(instmap::in,
    list(case)::in, list(case)::out, construct_map::in, construct_map::out,
    delay_partial_inst_info::in, delay_partial_inst_info::out) is det.

delay_partial_inst_in_cases(_, [], [], !ConstructMap, !DelayInfo).
delay_partial_inst_in_cases(InstMap0, [Case0 | Cases0], [Case | Cases],
        !ConstructMap, !DelayInfo) :-
    % See comment in delay_partial_inst_in_goals.
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    delay_partial_inst_in_goal(InstMap0, Goal0, Goal, !ConstructMap,
        !DelayInfo),
    Case = case(MainConsId, OtherConsIds, Goal),
    delay_partial_inst_in_cases(InstMap0, Cases0, Cases, !ConstructMap,
        !DelayInfo).

%-----------------------------------------------------------------------------%
%
% Handle unifications that construct partially instantated terms.
%

:- pred delay_partial_inst_in_partial_construct(hlds_goal_info::in,
    unification::in(unification_construct), hlds_goal::out,
    construct_map::in, construct_map::out,
    delay_partial_inst_info::in, delay_partial_inst_info::out) is det.

delay_partial_inst_in_partial_construct(GoalInfo0, Unify, Goal,
        !ConstructMap, !DelayInfo) :-
    Unify = construct(Var, ConsId, Args, ArgModes, _, _, _),
    % Add an entry for Var to the construct map if it doesn't exist
    % already, otherwise look up the canonical variables.
    ( if
        map.search(!.ConstructMap, Var, CanonVarsMap0),
        map.search(CanonVarsMap0, ConsId, CanonVars0)
    then
        CanonVars = CanonVars0
    else
        create_canonical_variables(Args, CanonVars, !DelayInfo),
        add_to_construct_map(Var, ConsId, CanonVars, !ConstructMap)
    ),

    % Unify the canonical variables and corresponding ground
    % arguments (if any).
    ModuleInfo = !.DelayInfo ^ dpi_module_info,
    ProgContext = goal_info_get_context(GoalInfo0),
    SubUnifyGoals = list.filter_map_corresponding3(
        maybe_unify_var_with_ground_var(ModuleInfo, ProgContext),
        CanonVars, Args, ArgModes),
    conj_list_to_goal(SubUnifyGoals, GoalInfo0, Goal),

    % Mark the procedure as changed.
    !DelayInfo ^ dpi_changed := yes.

:- pred create_canonical_variables(prog_vars::in, prog_vars::out,
    delay_partial_inst_info::in, delay_partial_inst_info::out) is det.

create_canonical_variables(OrigVars, CanonVars, !DelayInfo) :-
    VarTable0 = !.DelayInfo ^ dpi_var_table,
    clone_variables(OrigVars, VarTable0, VarTable0, VarTable,
        map.init, Renaming),
    rename_var_list(must_rename, Renaming, OrigVars, CanonVars),
    !DelayInfo ^ dpi_var_table := VarTable.

:- pred add_to_construct_map(prog_var::in, cons_id::in, prog_vars::in,
    construct_map::in, construct_map::out) is det.

add_to_construct_map(Var, ConsId, CanonVars, !ConstructMap) :-
    ( if map.search(!.ConstructMap, Var, ConsIdMap0) then
        ConsIdMap1 = ConsIdMap0
    else
        ConsIdMap1 = map.init
    ),
    map.det_insert(ConsId, CanonVars, ConsIdMap1, ConsIdMap),
    map.set(Var, ConsIdMap, !ConstructMap).

%-----------------------------------------------------------------------------%
%
% Handle deconstructions. In some of these, information flows from right to
% left, i.e. from some of the function symbol's argument variables to the
% LHS variable, which previously must have been bound to a partially
% instantiated term.
%

:- pred delay_partial_inst_in_deconstruct(hlds_goal::in,
    unify_mode::in, unification::in(unification_deconstruct), hlds_goal::out,
    construct_map::in, construct_map::out,
    delay_partial_inst_info::in, delay_partial_inst_info::out) is det.

delay_partial_inst_in_deconstruct(Goal0, UnifyMode, Unify, Goal,
        !ConstructMap, !DelayInfo) :-
    Unify = deconstruct(Var, ConsId, Args, ArgModes, _CanFail, _CanCGC),
    ( if
        map.search(!.ConstructMap, Var, CanonVarsMap0),
        map.search(CanonVarsMap0, ConsId, CanonArgs)
    then
        % Unify each ground argument with the corresponding canonical
        % variable.
        ModuleInfo = !.DelayInfo ^ dpi_module_info,
        ProgContext = goal_info_get_context(GoalInfo0),
        SubUnifyGoals = list.filter_map_corresponding3(
            maybe_unify_var_with_ground_var(ModuleInfo, ProgContext),
            CanonArgs, Args, ArgModes),

        % Construct Var if it should be ground now.
        UnifyMode = unify_modes_li_lf_ri_rf(_, LHSFinalInst, _, _),
        ( if inst_is_ground(ModuleInfo, LHSFinalInst) then
            construct_functor(Var, ConsId, CanonArgs, ConstructGoal),

            % Delete the variable on the LHS from the construct map
            % since it has been constructed.
            map.delete(ConsId, CanonVarsMap0, CanonVarsMap),
            map.det_update(Var, CanonVarsMap, !ConstructMap),

            ConjList = SubUnifyGoals ++ [ConstructGoal]
        else
            ConjList = SubUnifyGoals
        ),
        Goal0 = hlds_goal(_, GoalInfo0),
        conj_list_to_goal(ConjList, GoalInfo0, Goal)
    else
        Goal = Goal0
    ).

%-----------------------------------------------------------------------------%
%
% Utility predicate used by handling of both partial constructions and
% deconstructions.
%

:- func maybe_unify_var_with_ground_var(module_info::in, prog_context::in,
    prog_var::in, prog_var::in, unify_mode::in) = (hlds_goal::out) is semidet.

maybe_unify_var_with_ground_var(ModuleInfo, Context, LHSVar, RHSVar, UnifyMode)
        = Goal :-
    UnifyMode = unify_modes_li_lf_ri_rf(_, _, RHSInitInst, _),
    inst_is_ground(ModuleInfo, RHSInitInst),
    create_pure_atomic_complicated_unification(LHSVar, rhs_var(RHSVar),
        Context, umc_implicit("delay_partial_inst"), [], Goal).

%-----------------------------------------------------------------------------%
%
% Handle complicated test unifications.
%

:- pred delay_partial_inst_in_complicated_unify(hlds_goal::in,
    prog_var::in, unify_rhs::in,
    unification::in(unification_complicated_unify), hlds_goal::out,
    construct_map::in, construct_map::out,
    delay_partial_inst_info::in, delay_partial_inst_info::out) is det.

delay_partial_inst_in_complicated_unify(Goal0, LHS, RHS0, Unify, Goal,
        !ConstructMap, !DelayInfo) :-
    Unify = complicated_unify(_Mode, CanFail, _TypeInfos),
    % Deal with tests generated for calls to implied modes.
    %
    %       LHS := f(_),
    %       p(RHS),
    %       LHS ?= RHS
    %   ===>
    %       p(RHS),
    %       RHS ?= f(_),
    %       LHS := RHS
    %
    % XXX I have not seen a case where the LHS and RHS are swapped
    % but we should handle that if it comes up.
    ( if
        CanFail = can_fail,
        RHS0 = rhs_var(RHSVar),
        get_sole_cons_id_and_canon_vars(!.ConstructMap, LHS, ConsId,
            CanonArgs)
    then
        Goal0 = hlds_goal(_, GoalInfo0),
        ProgContext = goal_info_get_context(GoalInfo0),
        create_pure_atomic_complicated_unification(RHSVar,
            rhs_functor(ConsId, is_not_exist_constr, CanonArgs),
            ProgContext, umc_explicit, [], TestGoal),
        create_pure_atomic_complicated_unification(LHS, RHS0,
            ProgContext, umc_implicit("delay_partial_inst"), [],
            AssignGoal),
        conjoin_goals(TestGoal, AssignGoal, Goal)
    else
        Goal = Goal0
    ).

:- pred get_sole_cons_id_and_canon_vars(construct_map::in, prog_var::in,
    cons_id::out, prog_vars::out) is semidet.

get_sole_cons_id_and_canon_vars(ConstructMap, Var, ConsId, CanonVars) :-
    map.search(ConstructMap, Var, CanonVarsMap),
    List = map.to_assoc_list(CanonVarsMap),
    (
        List = [],
        fail
    ;
        List = [ConsId - CanonVars | Rest],
        (
            Rest = []
        ;
            Rest = [_ | _],
            % This algorithm does not work if a variable could be bound to
            % multiple functors when we try to do a tag test against it.
            % XXX report a nicer error message
            sorry($pred,
                "delaying partial instantiations when variable could be " ++
                "bound to multiple functors")
        )
    ).

%-----------------------------------------------------------------------------%
:- end_module check_hlds.delay_partial_inst.
%-----------------------------------------------------------------------------%
