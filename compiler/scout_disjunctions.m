%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2025-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: scout_disjunctions.m.
% Author: zs.
%
% This module is part of the switch detection pass, and can be considered
% its pre-pass. It does a bottom-up traversal of an entire procedure body,
% and builds up a database about which variables are deconstructed
% (directly, or through an aliased variable) in each disjunct
% of each disjunction. This database is intended to both simplify
% and speed up the work of the main top-down switch detection algorithm
% in switch_detection.m.
%
%---------------------------------------------------------------------------%

:- module check_hlds.scout_disjunctions.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module char.
:- import_module map.
:- import_module one_or_more.
:- import_module set.

%---------------------------------------------------------------------------%

    % The set of cons_id kinds that we consider creating switch arms for.
:- type switchable_cons_id =< cons_id
    --->    du_data_ctor(du_ctor)
    ;       some_int_const(some_int_const)
    ;       float_const(float)
    ;       char_const(char)
    ;       string_const(string).

    % We should be able to coerce sets of switchable_cons_ids to cons_ids,
    % but we cannot. We use this until we can do so.
    %
:- func switchable_cons_id_to_cons_id(switchable_cons_id) = cons_id.

%---------------------------------------------------------------------------%

    % The type whose values we use to identify a disjunction.
    % The goal specified by the given goal_id will be a disj(...) goal.
:- type disjunction_id
    --->    disjunction_id(goal_id).

    % The type whose values we use to identify a disjunct in a disjunction.
    % The goal specified by the given goal_id will have a disj(...) goal
    % as its immediate parent.
:- type disjunct_id
    --->    disjunct_id(goal_id).

%---------------------%

    % Maps the id of a disjunction to information about that disjunction.
:- type disjunction_info_map == map(disjunction_id, disjunction_info).

    % Map the id of a disjunct to information about that disjunct.
:- type disjunct_info_map == map(disjunct_id, disjunct_info).

%---------------------%

    % The information that scouting finds about a disjunction.
    % There should be one of these in the disjunction_info_map
    % for every disjunction in the procedure body.
:- type disjunction_info
    --->    disjunction_info(
                % The list of disjuncts in the disjunction.
                % This field is not yet used.
                dni_arms                    :: one_or_more(disjunct_id_info),

                % This field is the main product of the scouting pass.
                % The map will contain an entry for every variable
                % that is deconstructed in the zone of every disjunct.
                % Such a deconstruction can occur directly in the disjunct,
                % or it can occur in smaller disjunctions inside it, nested
                % at an any depth.
                dni_summary_map             :: all_arms_summary_map
            ).

    % This type is used only for the dni_summary_map field.
    % Please see its documentation.
:- type all_arms_summary_map == map(prog_var, var_all_arms_summary).

:- type var_all_arms_summary
    --->    var_all_arms_summary(
                % The set of cons_ids that disjuncts in this disjunction
                % unify the associated variable with in the zone, either
                % in the disjunct directly, or in a subdisjunction
                % (which may be arbitrarily deeply nested).
                %
                % (The associated variable is the key in the
                % all_arms_summary_map for this value.)
                set(switchable_cons_id),

                % If the associated variable is deconstructed to one of
                % the above cons_ids in *more than one* disjunct, then
                % turning the overall disjunction into a switch would
                % require making the switch arm for that cons_id into
                % a subdisjunction. Is there such a cons_id?
                is_sub_disj_needed
            ).

:- type is_sub_disj_needed
    --->    sub_disj_is_not_needed
    ;       sub_disj_is_needed.

:- type disjunct_id_info.
:- type disjunct_info.

%---------------------------------------------------------------------------%

:- pred scout_disjunctions_in_proc(module_info::in,
    proc_info::in, proc_info::out, disjunction_info_map::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.goal_path.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_markers.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_goal.
:- import_module hlds.hlds_proc_util.
:- import_module hlds.instmap.
:- import_module parse_tree.parse_tree_out_cons_id.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.var_db.
:- import_module parse_tree.var_table.

:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module term_context.
:- import_module term_subst.
:- import_module term_unify.
:- import_module varset.

%---------------------------------------------------------------------------%

switchable_cons_id_to_cons_id(ConsId) = coerce(ConsId).

%---------------------------------------------------------------------------%

% The data structures constructed by the code of this module
% for use by the main traversal in switch_detection.m.
%
% The result of scouting is information about the terrain ahead, which
% in this case means information about deconstruction unifications
% and disjunctions that the main traversal has not yet seen.
%
% At the moment, we use scouting results at only one point
% in the main traversal. However, this may change in the future.

:- type scout_disj_info
    --->    scout_disj_info(
                % Conceptually, both of these are read-only, though in
                % actuality, we update module_info when we handle cases
                % inside switches.
                scdi_module_info            :: module_info,
                scdi_var_table              :: var_table,

                % These are the data structures we are constructing.
                % The one we really want is the disjunction_info_map;
                % we build the disjunct_info_map as a stepping stone to it.
                scdi_disjunction_info_map   :: disjunction_info_map,
                scdi_disjunct_info_map      :: disjunct_info_map
            ).

%---------------------%

    % Values of this type contain summary information about one disjunct
    % of a disjunction. Their sole intended use is as an input for the
    % construction of var_all_arms_summary structures.
:- type one_arm_summary_map == map(prog_var, var_one_arm_summary).

:- type var_one_arm_summary
    --->    voas_deconstruct(deconstruct_info)
            % The disjunct deconstructs the associated variable directly
            % in its zone. The argument gives the specifics of the
            % deconstruction.
    ;       voas_sub_disjunction(var_all_arms_summary).
            % The disjunct does not deconstruct the associated variable
            % directly in its zone, but it does contain a subdisjunction
            % in the zone which does so, either directly or indirectly.

:- type maybe_in_zone
    --->    in_zone(disjunct_id)
            % We are in one of the disjuncts of a disjunction; the argument
            % specifies the disjunct. And we are within the initial sequence
            % of unifications within that disjunct. (We treat calls from the
            % clause head as unifications for this purpose.)
            %
            % As soon as we leave this initial part of a disjunct,
            % we switch to not_in_zone. The only deconstruction unifications
            % we consider for switch detection are the ones that occur
            % "in the zone".
            %
            % We originally adopted this rule to reduce the cost (in compile
            % time) of searching for deconstruction unifications that denote
            % switch arms. However, converting such a deconstruction
            % unification into the test for a switch arm can also change
            % the order execution of the disjunct's conjuncts, and restricting
            % the reordering to happen only among unifications eliminates
            % any concerns about changing the operational semantics of the
            % procedure in terms of exceptions being raised or nontermination
            % being introduced. (Function calls from clause heads do not have
            % a clearly specified order with respect to goals in the clause
            % body, which is why we allow reordering with respect to them.)
            %
            % The effect on compile times is no longer meaningful, but the
            % effect on operational semantics is still relevant.
            %
            % There is also an ergonomic argument here: requiring unifications
            % that effectively serve as case constants in C switch statements
            % to be near the start of their switch arms keeps code readable,
            % compared to a hypothetical alternative arrangement in which
            % we allow unifications from the ends of possibly-long disjuncts
            % to provide the cons_id that identifies a switch arm.
    ;       not_in_zone.

:- type disjunct_id_info
    --->    disjunct_id_info(disjunct_id, disjunct_info).

    % The information that scouting finds about a disjunct.
    % There should be one of these in the disjunct_info_map
    % for every disjunct in the procedure body.
:- type disjunct_info
    --->    disjunct_info(
                di_iz_deconstruct_map       :: in_zone_deconstruct_map,
                % We record info about at most one disjunction, since
                % a disjunction ends the zone.
                di_iz_sub_disjunctions      :: maybe(disjunction_id)
            ).

    % Note that we map not just the deconstructed variable
    % to a deconstruct_info, but also all variables equivalent to it.
:- type in_zone_deconstruct_map == map(prog_var, deconstruct_info).

:- type deconstruct_info
    --->    deconstruct_info(
                % This goal ...
                goal_id,

                % ... deconstructs this variable ...
                prog_var,

                % ... which is part of this equivalence class ...
                set(prog_var),

                % ... with this cons_id.
                switchable_cons_id
            ).

%---------------------------------------------------------------------------%

scout_disjunctions_in_proc(ModuleInfo, !ProcInfo, DisjunctionInfoMap) :-
    % XXX TODO Deforest with scout_disjunctions_in_goal.
    fill_goal_id_slots_in_proc(ModuleInfo, _ContainingGoalMap, !ProcInfo),

    SubstDb0 = init_subst_db,
    proc_info_get_var_table(!.ProcInfo, VarTable),
    map.init(DisjunctionInfoMap0),
    map.init(DisjunctInfoMap0),
    ScoutInfo0 = scout_disj_info(ModuleInfo, VarTable,
        DisjunctionInfoMap0, DisjunctInfoMap0),
    proc_info_get_goal(!.ProcInfo, Goal),
    proc_info_get_initial_instmap(ModuleInfo, !.ProcInfo, InstMap0),
    scout_disjunctions_in_goal(Goal, InstMap0, _InstMap,
        not_in_zone, _InZone, SubstDb0, _SubstDb, ScoutInfo0, ScoutInfo),
    ScoutInfo = scout_disj_info(_, _, DisjunctionInfoMap, _),
    trace [
        compile_time(flag("scout-disjunctions")),
        runtime(env("SCOUT_DISJUNCTIONS")),
        io(!IO)
    ] (
        io.stderr_stream(StrErr, !IO),
        varset.init(TVarSet),
        proc_info_get_inst_varset(!.ProcInfo, InstVarSet),
        io.write_string(StrErr, "\nPROC BODY\n", !IO),
        dump_goal_nl(StrErr, ModuleInfo, vns_var_table(VarTable),
            TVarSet, InstVarSet, Goal, !IO),
        DisjunctionInfoMapStr =
            disjunction_info_map_to_string(VarTable, DisjunctionInfoMap),
        io.write_string(StrErr, DisjunctionInfoMapStr, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred scout_disjunctions_in_goal(hlds_goal::in, instmap::in, instmap::out,
    maybe_in_zone::in, maybe_in_zone::out, subst_db::in, subst_db::out,
    scout_disj_info::in, scout_disj_info::out) is det.

scout_disjunctions_in_goal(Goal, InstMap0, InstMap,
        !InZone, !SubstDb, !ScoutInfo) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    (
        GoalExpr = unify(_, _, _, _, _),
        scout_disjunctions_in_unify_expr(GoalExpr, GoalInfo, InstMap0,
            !.InZone, !SubstDb, !ScoutInfo)
    ;
        ( GoalExpr = generic_call(_, _, _, _, _)
        ; GoalExpr = plain_call(_, _, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        ( if goal_info_has_feature(GoalInfo, feature_from_head) then
            true
        else
            !:InZone = not_in_zone
        )
    ;
        GoalExpr = conj(ConjType, Conjuncts),
        (
            ConjType = plain_conj,
            scout_disjunctions_in_conjuncts(Conjuncts, InstMap0,
                !InZone, !SubstDb, !ScoutInfo)
        ;
            ConjType = parallel_conj,
            (
                Conjuncts = []
            ;
                Conjuncts = [HeadConjunct | TailConjuncts],
                % The first parallel conjunct can be in the zone;
                % any later conjuncts cannot be in the zone.
                scout_disjunctions_in_goal(HeadConjunct, InstMap0, InstMap1,
                    !.InZone, _, !SubstDb, !ScoutInfo),
                scout_disjunctions_in_conjuncts(TailConjuncts, InstMap1,
                    not_in_zone, _, !SubstDb, !ScoutInfo),
                !:InZone = not_in_zone
            )
        )
    ;
        GoalExpr = disj(Disjuncts),
        (
            Disjuncts = []
        ;
            Disjuncts = [HeadDisjunct | TailDisjuncts],
            scout_disjunctions_in_disjuncts(HeadDisjunct, TailDisjuncts,
                InstMap0, !.SubstDb,
                HeadDisjunctIdInfo, TailDisjunctIdInfos, !ScoutInfo),

            OoMDisjunctIdsInfos =
                one_or_more(HeadDisjunctIdInfo, TailDisjunctIdInfos),
            construct_scout_disjunction_info(!.ScoutInfo, OoMDisjunctIdsInfos,
                DisjunctionInfo),

            DisjunctionGoalId = goal_info_get_goal_id(GoalInfo),
            DisjunctionId = disjunction_id(DisjunctionGoalId),
            DisjunctionInfoMap0 = !.ScoutInfo ^ scdi_disjunction_info_map,
            map.det_insert(DisjunctionId, DisjunctionInfo,
                DisjunctionInfoMap0, DisjunctionInfoMap),
            !ScoutInfo ^ scdi_disjunction_info_map := DisjunctionInfoMap,

            (
                !.InZone = in_zone(DisjunctId),
                DisjunctInfoMap0 = !.ScoutInfo ^ scdi_disjunct_info_map,
                map.lookup(DisjunctInfoMap0, DisjunctId, DisjunctInfo0),
                DisjunctInfo0 =
                    disjunct_info(DeconstructMap0, SubDisjunctions0),
                expect(unify(SubDisjunctions0, no), $pred,
                    "SubDisjunctions0 != no"),
                SubDisjunctions = yes(DisjunctionId),
                DisjunctInfo = disjunct_info(DeconstructMap0, SubDisjunctions),
                map.det_update(DisjunctId, DisjunctInfo,
                    DisjunctInfoMap0, DisjunctInfoMap),
                !ScoutInfo ^ scdi_disjunct_info_map := DisjunctInfoMap
            ;
                !.InZone = not_in_zone
            ),
            !:InZone = not_in_zone
        )
    ;
        GoalExpr = switch(Var, _CanFail, Cases),
        scout_disjunctions_in_cases(Var, Cases, InstMap0,
            !.SubstDb, !ScoutInfo),
        !:InZone = not_in_zone
    ;
        GoalExpr = if_then_else(_Vars, Cond, Then, Else),
        scout_disjunctions_in_goal(Cond, InstMap0, InstMap1,
            not_in_zone, _, !.SubstDb, SubstDbCond, !ScoutInfo),
        scout_disjunctions_in_goal(Then, InstMap1, _,
            not_in_zone, _, SubstDbCond, _, !ScoutInfo),
        scout_disjunctions_in_goal(Else, InstMap0, _,
            not_in_zone, _, !.SubstDb, _, !ScoutInfo),
        !:InZone = not_in_zone
    ;
        GoalExpr = negation(SubGoal),
        scout_disjunctions_in_goal(SubGoal, InstMap0, _,
            not_in_zone, _, !.SubstDb, _, !ScoutInfo),
        !:InZone = not_in_zone
    ;
        GoalExpr = scope(Reason, SubGoal),
        (
            Reason = from_ground_term(_, FgtKind),
            (
                FgtKind = from_ground_term_deconstruct,
                SubGoal = hlds_goal(SubGoalExpr, _),
                ( if
                    SubGoalExpr = conj(plain_conj, [HeadSubGoal | _]),
                    HeadSubGoal = hlds_goal(HeadSubGoalExpr, HeadSubGoalInfo),
                    HeadSubGoalExpr = unify(_, _, _, _, _)
                then
                    scout_disjunctions_in_unify_expr(HeadSubGoalExpr,
                        HeadSubGoalInfo, InstMap0, !.InZone,
                        !SubstDb, !ScoutInfo)
                    % Ignore the goals after HeadSubGoal; nothing in them
                    % could interest us, since none of the variables
                    % they deconstruct are visible from outside this scope.
                else
                    unexpected($pred, "unexpected goal in fgt scope")
                )
            ;
                ( FgtKind = from_ground_term_initial
                ; FgtKind = from_ground_term_construct
                ; FgtKind = from_ground_term_other
                )
                % Ignore the scope; nothing in it could interest us.
            )
        ;
            ( Reason = exist_quant(_, _)
            ; Reason = disable_warnings(_, _)
            ; Reason = promise_solutions(_, _)
            ; Reason = promise_purity(_)
            ; Reason = require_detism(_)
            ; Reason = commit(_)
            ; Reason = barrier(_)
            ; Reason = trace_goal(_, _, _, _, _)
            ; Reason = loop_control(_, _, _)
            ),
            scout_disjunctions_in_goal(SubGoal, InstMap0, _,
                !.InZone, _, !.SubstDb, _, !ScoutInfo)
        ;
            ( Reason = require_complete_switch(_RequiredVar)
            ; Reason = require_switch_arms_detism(_RequiredVar, _)
            ),
            scout_disjunctions_in_goal(SubGoal, InstMap0, _,
                not_in_zone, !:InZone, !.SubstDb, _, !ScoutInfo),
            expect(unify(!.InZone, not_in_zone), $pred,
                "in_zone after switch-related reason")
        )
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ShortHand = atomic_goal(_GoalType, _Outer, _Inner,
                _MaybeOutputVars, MainGoal, OrElseGoals, _OrElseInners),
            scout_disjunctions_in_goal(MainGoal, InstMap0, _,
                not_in_zone, _, !.SubstDb, _, !ScoutInfo),
            scout_disjunctions_in_orelse_goals(OrElseGoals, InstMap0,
                !.SubstDb, !ScoutInfo),
            !:InZone = not_in_zone
        ;
            ShortHand = try_goal(_MaybeIO, _ResultVar, SubGoal),
            scout_disjunctions_in_goal(SubGoal, InstMap0, _,
                not_in_zone, _, !.SubstDb, _, !ScoutInfo),
            !:InZone = not_in_zone
        ;
            ShortHand = bi_implication(_, _),
            % These should have been expanded out by now.
            unexpected($pred, "bi_implication")
        )
    ),
    apply_goal_instmap_delta(Goal, InstMap0, InstMap).

:- pred scout_disjunctions_in_unify_expr(hlds_goal_expr::in(goal_expr_unify),
    hlds_goal_info::in, instmap::in, maybe_in_zone::in,
    subst_db::in, subst_db::out,
    scout_disj_info::in, scout_disj_info::out) is det.

scout_disjunctions_in_unify_expr(GoalExpr, GoalInfo, InstMap0,
        InZone0, !SubstDb, !ScoutInfo) :-
    GoalExpr = unify(XVar, RHS, _, Unification, _),
    % For both rhs_var and rhs_functor, we record the effect of the
    % unification on the substitution database even when we are
    % outside the zone. This extra info won't help us find more
    % aliases for in-zone deconstructions in this disjunct (since there
    % aren't any more past the end of the zone), but the extra information
    % in the substitution database may help us find more aliases inside
    % nested disjunctions.
    (
        RHS = rhs_lambda_goal(_, _, _, _, VarsModes, _, LambdaGoal),
        % We need to insert the initial insts for the lambda variables
        % into the instmap before processing the lambda goal.
        ModuleInfo = !.ScoutInfo ^ scdi_module_info,
        instmap.pre_lambda_update(ModuleInfo, VarsModes, InstMap0, InstMap1),
        % LambdaGoal may be in_zone from the point of view of the code
        % outside this unification, but the proper perspective for
        % this call is the code *inside* the lambda goal. And from that
        % point of view, LambdaGoal is not inside *any* disjunction.
        scout_disjunctions_in_goal(LambdaGoal, InstMap1, _,
            not_in_zone, _, !.SubstDb, _, !ScoutInfo)
    ;
        RHS = rhs_var(YVar),
        record_var_var_unify(XVar, YVar, !SubstDb)
    ;
        RHS = rhs_functor(ConsId, _IsExistConstr, YVars),
        (
            ( ConsId = du_data_ctor(_)
            ; ConsId = some_int_const(_)
            ; ConsId = float_const(_)
            ; ConsId = char_const(_)
            ; ConsId = string_const(_)
            ),
            (
                Unification = assign(_, _),
                unexpected($pred, "assign")
            ;
                Unification = simple_test(_, _),
                unexpected($pred, "simple_test")
            ;
                Unification = construct(_, _, _, _, _, _, _)
            ;
                Unification = deconstruct(_, _, _, _, _, _),
                (
                    InZone0 = in_zone(DisjunctId),
                    GoalId = goal_info_get_goal_id(GoalInfo),
                    record_deconstruct(GoalId, XVar, coerce(ConsId),
                        !.SubstDb, DisjunctId, !ScoutInfo)
                ;
                    InZone0 = not_in_zone
                )
            ;
                Unification = complicated_unify(_, _, _),
                unexpected($pred, "complicated_unify")
            ),
            record_var_functor_unify(XVar, ConsId, YVars, !SubstDb)
        ;
            ( ConsId = tuple_cons(_)
            ; ConsId = closure_cons(_)
            ; ConsId = impl_defined_const(_)
            ; ConsId = type_ctor_info_const(_, _, _)
            ; ConsId = base_typeclass_info_const(_, _, _, _)
            ; ConsId = type_info_cell_constructor(_)
            ; ConsId = typeclass_info_cell_constructor
            ; ConsId = type_info_const(_)
            ; ConsId = typeclass_info_const(_)
            ; ConsId = ground_term_const(_, _)
            ; ConsId = tabling_info_const(_)
            ; ConsId = table_io_entry_desc(_)
            ; ConsId = deep_profiling_proc_layout(_)
            )
        )
    ).

:- pred record_deconstruct(goal_id::in, prog_var::in, switchable_cons_id::in,
    subst_db::in, disjunct_id::in,
    scout_disj_info::in, scout_disj_info::out) is det.

record_deconstruct(GoalId, XVar, ConsId, SubstDb, DisjunctId, !ScoutInfo) :-
    get_equivalent_vars(SubstDb, XVar, XEqvVars),
    DeconstructInfo = deconstruct_info(GoalId, XVar, XEqvVars, ConsId),

    DisjunctInfoMap0 = !.ScoutInfo ^ scdi_disjunct_info_map,
    map.lookup(DisjunctInfoMap0, DisjunctId, DisjunctInfo0),
    DisjunctInfo0 = disjunct_info(DeconstructMap0, SubDisjunctions0),
    set.foldl(maybe_add_deconstruct(DeconstructInfo), XEqvVars,
        DeconstructMap0, DeconstructMap),
    DisjunctInfo = disjunct_info(DeconstructMap, SubDisjunctions0),
    map.det_update(DisjunctId, DisjunctInfo,
        DisjunctInfoMap0, DisjunctInfoMap),
    !ScoutInfo ^ scdi_disjunct_info_map := DisjunctInfoMap.

:- pred maybe_add_deconstruct(deconstruct_info::in, prog_var::in,
    in_zone_deconstruct_map::in, in_zone_deconstruct_map::out) is det.

maybe_add_deconstruct(DeconstructInfo, XEqvVar, !DeconstructMap) :-
    map.search_insert(XEqvVar, DeconstructInfo, _, !DeconstructMap).

%---------------------------------------------------------------------------%

:- pred scout_disjunctions_in_conjuncts(list(hlds_goal)::in, instmap::in,
    maybe_in_zone::in, maybe_in_zone::out, subst_db::in, subst_db::out,
    scout_disj_info::in, scout_disj_info::out) is det.

scout_disjunctions_in_conjuncts([], _InstMap0,
        !InZone, !SubstDb, !ScoutInfo).
scout_disjunctions_in_conjuncts([Conjunct | Conjuncts], InstMap0,
        !InZone, !SubstDb, !ScoutInfo) :-
    scout_disjunctions_in_goal(Conjunct, InstMap0, InstMap1,
        !InZone, !SubstDb, !ScoutInfo),
    scout_disjunctions_in_conjuncts(Conjuncts, InstMap1,
        !InZone, !SubstDb, !ScoutInfo).

%---------------------------------------------------------------------------%

:- pred scout_disjunctions_in_disjuncts(hlds_goal::in, list(hlds_goal)::in,
    instmap::in, subst_db::in,
    disjunct_id_info::out, list(disjunct_id_info)::out,
    scout_disj_info::in, scout_disj_info::out) is det.

scout_disjunctions_in_disjuncts(HeadDisjunct, TailDisjuncts, InstMap0,
        SubstDb0, HeadDisjunctIdInfo, TailDisjunctIdInfos, !ScoutInfo) :-
    % We pass in_zone here because a deconstruction unification
    % near the start of Disjunct could make Disjunct one arm of a switch.
    HeadDisjunct = hlds_goal(_, HeadDisjunctGoalInfo),
    HeadDisjunctGoalId = goal_info_get_goal_id(HeadDisjunctGoalInfo),
    HeadDisjunctId = disjunct_id(HeadDisjunctGoalId),
    DisjunctInfoMap0 = !.ScoutInfo ^ scdi_disjunct_info_map,
    HeadDisjunctInfo0 = disjunct_info(map.init, no),
    map.det_insert(HeadDisjunctId, HeadDisjunctInfo0,
        DisjunctInfoMap0, DisjunctInfoMap1),
    !ScoutInfo ^ scdi_disjunct_info_map := DisjunctInfoMap1,

    scout_disjunctions_in_goal(HeadDisjunct, InstMap0, _,
        in_zone(HeadDisjunctId), _, SubstDb0, _, !ScoutInfo),
    DisjunctInfoMap = !.ScoutInfo ^ scdi_disjunct_info_map,
    map.lookup(DisjunctInfoMap, HeadDisjunctId, HeadDisjunctInfo),
    HeadDisjunctIdInfo = disjunct_id_info(HeadDisjunctId, HeadDisjunctInfo),
    (
        TailDisjuncts = [],
        TailDisjunctIdInfos = []
    ;
        TailDisjuncts = [HeadTailDisjunct | TailTailDisjuncts],
        scout_disjunctions_in_disjuncts(HeadTailDisjunct, TailTailDisjuncts,
            InstMap0, SubstDb0,
            HeadTailDisjunctIdInfo, TailTailDisjunctIdInfos, !ScoutInfo),
        TailDisjunctIdInfos =
            [HeadTailDisjunctIdInfo | TailTailDisjunctIdInfos]
    ).

%---------------------------------------------------------------------------%

:- pred scout_disjunctions_in_orelse_goals(list(hlds_goal)::in,
    instmap::in, subst_db::in,
    scout_disj_info::in, scout_disj_info::out) is det.

scout_disjunctions_in_orelse_goals([], _InstMap0, _Subst0, !ScoutInfo).
scout_disjunctions_in_orelse_goals([OrElseGoal | OrElseGoals], InstMap0,
        Subst0, !ScoutInfo) :-
    % We pass not_in_zone here because a deconstruction unification
    % near the start of OrElseGoal *cannot* OrElseGoal an arm of a switch.
    scout_disjunctions_in_goal(OrElseGoal, InstMap0, _,
        not_in_zone, _, Subst0, _, !ScoutInfo),
    scout_disjunctions_in_orelse_goals(OrElseGoals, InstMap0,
        Subst0, !ScoutInfo).

%---------------------------------------------------------------------------%

:- pred scout_disjunctions_in_cases(prog_var::in, list(case)::in,
    instmap::in, subst_db::in,
    scout_disj_info::in, scout_disj_info::out) is det.

scout_disjunctions_in_cases(_Var, [], _InstMap0, _SubstDb0, !ScoutInfo).
scout_disjunctions_in_cases(Var, [Case | Cases], InstMap0,
        SubstDb0, !ScoutInfo) :-
    Case = case(MainConsId, OtherConsIds, Goal),
    VarTable = !.ScoutInfo ^ scdi_var_table,
    lookup_var_type(VarTable, Var, VarType),
    ModuleInfo0 = !.ScoutInfo ^ scdi_module_info,
    bind_var_to_functors(Var, VarType, MainConsId, OtherConsIds,
        InstMap0, InstMap1, ModuleInfo0, ModuleInfo),
    !ScoutInfo ^ scdi_module_info := ModuleInfo,

    scout_disjunctions_in_goal(Goal, InstMap1, _,
        not_in_zone, _, SubstDb0, _, !ScoutInfo),
    scout_disjunctions_in_cases(Var, Cases, InstMap0, SubstDb0, !ScoutInfo).

%---------------------------------------------------------------------------%

:- pred construct_scout_disjunction_info(scout_disj_info::in,
    one_or_more(disjunct_id_info)::in, disjunction_info::out) is det.

construct_scout_disjunction_info(ScoutInfo, OoMDisjunctIdsInfos,
        DisjunctionInfo) :-
    OoMDisjunctIdsInfos =
        one_or_more(HeadDisjunctIdInfo, TailDisjunctIdInfos),

    disjunct_id_info_to_one_arm_summary(ScoutInfo,
        HeadDisjunctIdInfo, HeadOneArmMap),
    list.map(disjunct_id_info_to_one_arm_summary(ScoutInfo),
        TailDisjunctIdInfos, TailOneArmMaps),
    summarize_all_one_arms(HeadOneArmMap, TailOneArmMaps, AllArmsMap),

    DisjunctionInfo = disjunction_info(OoMDisjunctIdsInfos, AllArmsMap).

%---------------------%

:- pred disjunct_id_info_to_one_arm_summary(scout_disj_info::in,
    disjunct_id_info::in, map(prog_var, var_one_arm_summary)::out) is det.

disjunct_id_info_to_one_arm_summary(ScoutInfo, DisjunctIdInfo, OneArmMap) :-
    DisjunctIdInfo = disjunct_id_info(_DisjunctId, DisjunctInfo),
    DisjunctInfo = disjunct_info(DeconstructMap, MaybeSubDisjunction),
    map.map_values_only(in_zone_deconstruct_to_one_arm_summary,
        DeconstructMap, OneArmMap0),
    (
        MaybeSubDisjunction = no,
        OneArmMap = OneArmMap0
    ;
        MaybeSubDisjunction = yes(SubDisjunctionId),
        DisjunctionInfoMap = ScoutInfo ^ scdi_disjunction_info_map,
        map.lookup(DisjunctionInfoMap, SubDisjunctionId, SubDisjunctionInfo),
        SubDisjunctionInfo = disjunction_info(_, SubAllArmsMap),
        % If a variable already occurs in !.OneArmMap, then it must have been
        % added from DeconstructMap, meaning it must have been deconstructed
        % in the zone. Since the disjunction identified by SubDisjunctionId
        % would end the zone, we can ignore any reference to deconstructions
        % of such variables in SubAllArmsMap, because
        %
        % - if the referenced deconstructions's cons_id is the same cons_id
        %   assigned to the variable in OneArmMap0, then that reference
        %   is redundant, while
        %
        % - if the referenced deconstructions's cons_id is NOT the same
        %   cons_id as assigned to the variable in OneArmMap0, then that
        %   unification cannot possibly succeed, making the arm in which
        %   it occurs a dead arm. (One reason why we process deconstructions
        %   only in the zone of initial goals in each disjunct is to allow us
        %   to delete such dead arms without changing the operational
        %   semantics of the predicate body.)
        map.foldl(acc_sub_disjunction_summary, SubAllArmsMap,
            OneArmMap0, OneArmMap)
    ).

:- pred in_zone_deconstruct_to_one_arm_summary(deconstruct_info::in,
    var_one_arm_summary::out) is det.

in_zone_deconstruct_to_one_arm_summary(DeconstructInfo, OneArm) :-
    OneArm = voas_deconstruct(DeconstructInfo).

:- pred acc_sub_disjunction_summary(prog_var::in, var_all_arms_summary::in,
    one_arm_summary_map::in, one_arm_summary_map::out) is det.

acc_sub_disjunction_summary(Var, SubDisjAllArms, !OneArmMap) :-
    map.search_insert(Var, voas_sub_disjunction(SubDisjAllArms), _OldOneArm,
        !OneArmMap).

%---------------------%

:- pred summarize_all_one_arms(
    one_arm_summary_map::in, list(one_arm_summary_map)::in,
    all_arms_summary_map::out) is det.

summarize_all_one_arms(HeadOneArmMap, TailOneArmMaps, !:AllArmsMap) :-
    map.init(!:AllArmsMap),
    map.foldl(maybe_acc_all_arm_for_var(TailOneArmMaps),
        HeadOneArmMap, !AllArmsMap).

:- pred maybe_acc_all_arm_for_var(list(one_arm_summary_map)::in,
    prog_var::in, var_one_arm_summary::in,
    all_arms_summary_map::in, all_arms_summary_map::out) is det.

maybe_acc_all_arm_for_var(TailOneArmMaps, Var, HeadArmSummary, !AllArmsMap) :-
    ( if
        find_var_one_arm_summaries(Var, TailOneArmMaps,
            [], RevTailArmSummaries)
    then
        (
            HeadArmSummary = voas_deconstruct(DeconstructInfo),
            DeconstructInfo = deconstruct_info(_, _, _, ConsId),
            ConsIdSet = set.make_singleton_set(ConsId),
            AllArmsSummary0 =
                var_all_arms_summary(ConsIdSet, sub_disj_is_not_needed)
        ;
            HeadArmSummary = voas_sub_disjunction(AllArmsSummary0)
        ),
        % The order in which we add the tail arms summaries does not matter.
        list.foldl(add_arm_to_all_arms_summary, RevTailArmSummaries,
            AllArmsSummary0, AllArmsSummary),
        map.det_insert(Var, AllArmsSummary, !AllArmsMap)
    else
        true
    ).

:- pred find_var_one_arm_summaries(prog_var::in,
    list(one_arm_summary_map)::in,
    list(var_one_arm_summary)::in, list(var_one_arm_summary)::out) is semidet.

find_var_one_arm_summaries(_Var, [], !ArmSummaries).
find_var_one_arm_summaries(Var, [ArmSummaryMap | ArmSummaryMaps],
        !ArmSummaries) :-
    map.search(ArmSummaryMap, Var, ArmSummary),
    !:ArmSummaries = [ArmSummary | !.ArmSummaries],
    find_var_one_arm_summaries(Var, ArmSummaryMaps, !ArmSummaries).

:- pred add_arm_to_all_arms_summary(var_one_arm_summary::in,
    var_all_arms_summary::in, var_all_arms_summary::out) is det.

add_arm_to_all_arms_summary(OneArmSummary, !AllArmsSummary) :-
    !.AllArmsSummary = var_all_arms_summary(ConsIdSet0, SubDisjNeeded0),
    (
        OneArmSummary = voas_deconstruct(DeconstructInfo),
        DeconstructInfo = deconstruct_info(_, _, _, ConsId),
        % Was ConsId already in ConsIdSet0?
        ( if set.insert_new(ConsId, ConsIdSet0, ConsIdSetPrime) then
            % No, it was not.
            ConsIdSet = ConsIdSetPrime,
            SubDisjNeeded = SubDisjNeeded0
        else
            % Yes, it was. Adding it to ConsIdSet0 would leave it unchanged.
            ConsIdSet = ConsIdSet0,
            % If the variable whose summaries we are now processing
            % is selected as the switched-on variable, then its case for
            % ConsId would need to include a disjunction containing at least
            % the arm that first added ConsId to ConsIdSet0, and this arm.
            SubDisjNeeded = sub_disj_is_needed
        )
    ;
        OneArmSummary = voas_sub_disjunction(SubAllArmsSummary),
        SubAllArmsSummary =
            var_all_arms_summary(SubConsIdSet, SubSubDisjNeeded),
        set.union(SubConsIdSet, ConsIdSet0, ConsIdSet),
        ( if
            SubDisjNeeded0 = sub_disj_is_not_needed,
            SubSubDisjNeeded = sub_disj_is_not_needed
        then
            set.intersect(SubConsIdSet, ConsIdSet0, IntersectSet),
            ( if set.is_empty(IntersectSet) then
                SubDisjNeeded = sub_disj_is_not_needed
            else
                SubDisjNeeded = sub_disj_is_needed
            )
        else
            SubDisjNeeded = sub_disj_is_needed
        )
    ),
    !:AllArmsSummary = var_all_arms_summary(ConsIdSet, SubDisjNeeded).

%---------------------------------------------------------------------------%
%
% These functions are intended to be used only for debugging the compiler.
%

:- func disjunction_info_map_to_string(var_table, disjunction_info_map)
    = string.

disjunction_info_map_to_string(VarTable, DisjunctionInfoMap) = Str :-
    HeaderStr = "\nDISJUNCTION INFO MAP\n",
    EndHeaderStr = "END DISJUNCTION INFO MAP\n",
    map.to_sorted_assoc_list(DisjunctionInfoMap, DisjunctionIdsInfos),
    DisjunctionIdInfoStrs =
        list.map(disjunction_id_info_to_string(VarTable), DisjunctionIdsInfos),
    string.append_list(
        [HeaderStr | DisjunctionIdInfoStrs] ++ [EndHeaderStr], Str).

:- func disjunction_id_info_to_string(var_table,
    pair(disjunction_id, disjunction_info)) = string.

disjunction_id_info_to_string(VarTable, DisjunctionId - DisjunctionInfo)
        = Str :-
    DisjunctionIdStr = string.string(DisjunctionId),
    string.format("\n%s\n", [s(DisjunctionIdStr)], HeaderStr),
    DisjunctionInfo = disjunction_info(_, AllArmsMap),
    map.to_sorted_assoc_list(AllArmsMap, AllArmsEntries),
    AllArmsEntryStrs =
        list.map(var_all_arms_summary_to_string(VarTable), AllArmsEntries),
    string.append_list([HeaderStr | AllArmsEntryStrs], Str).

:- func var_all_arms_summary_to_string(var_table,
    pair(prog_var, var_all_arms_summary)) = string.

var_all_arms_summary_to_string(VarTable, Var - AllArmsSummary) = Str :-
    VarStr = mercury_var_to_string(VarTable, print_name_and_num, Var),
    AllArmsSummary = var_all_arms_summary(ConsIdSet0, SubDisj),
    ConsIdSet = set.map(switchable_cons_id_to_cons_id, ConsIdSet0),
    ConsIdStrSet = set.map(cons_id_and_arity_to_string, ConsIdSet),
    set.to_sorted_list(ConsIdStrSet, ConsIdStrs),
    ConsIdsStr = string.string(ConsIdStrs),
    SubDisjStr = string.string(SubDisj),
    string.format("%s -> var_all_arms_summary(%s, %s)\n",
        [s(VarStr), s(ConsIdsStr), s(SubDisjStr)], Str).

%---------------------------------------------------------------------------%
%
% We use the "substitution database" to figure out the set of variables
% that a given variable is an alias for.
%
% The occurrence of the deconstruction unification X = f(...) in an arm
% of a disjunction can be used to support turning that disjunct into
% an arm of a switch on X, but also into an arm of a switch on Y,
% if at the program point of that deconstruction, X and Y are known
% to be aliases. This can happen not just if we saw a unification X = Y,
% but also if we saw e.g. X = Z and Z = Y.
%

:- type subst_db
    --->    subst_db(
                % The set of variables we have seen in unifications
                % at the current point of the traversal. Only variables
                % in this set can possibly be such aliases at the current
                % program point.
                set(prog_var),

                % The substitution representing the relationships (if any)
                % between those variables.
                prog_substitution
            ).

:- func init_subst_db = subst_db.

init_subst_db = subst_db(set.init, map.init).

:- pred record_var_var_unify(prog_var::in, prog_var::in,
    subst_db::in, subst_db::out) is det.

record_var_var_unify(XVar, YVar, !SubstDb) :-
    !.SubstDb = subst_db(SeenVars0, Subst0),
    set.insert(XVar, SeenVars0, SeenVars1),
    set.insert(YVar, SeenVars1, SeenVars),
    XTerm = term.variable(XVar, dummy_context),
    YTerm = term.variable(YVar, dummy_context),
    ( if unify_terms(XTerm, YTerm, Subst0, Subst1) then
        Subst = Subst1
    else
        % The unification must fail - just ignore it.
        Subst = Subst0
    ),
    !:SubstDb = subst_db(SeenVars, Subst).

:- pred record_var_functor_unify(prog_var::in,
    cons_id::in(switchable_cons_id), list(prog_var)::in,
    subst_db::in, subst_db::out) is det.

record_var_functor_unify(XVar, ConsId, YVars, !SubstDb) :-
    !.SubstDb = subst_db(SeenVars0, Subst0),
    set.insert(XVar, SeenVars0, SeenVars1),
    set.insert_list(YVars, SeenVars1, SeenVars),
    XTerm = term.variable(XVar, dummy_context),
    term_subst.var_list_to_term_list(YVars, YVarTerms),
    cons_id_and_args_to_term(ConsId, YVarTerms, YTerm),
    ( if unify_terms(XTerm, YTerm, Subst0, Subst1) then
        Subst = Subst1
    else
        % The unification must fail - just ignore it.
        Subst = Subst0
    ),
    !:SubstDb = subst_db(SeenVars, Subst).

:- pred get_equivalent_vars(subst_db::in, prog_var::in,
    set(prog_var)::out) is det.

get_equivalent_vars(SubstDb, Var, EqvVarsSet) :-
    SubstDb = subst_db(SeenVars, Subst),
    term_subst.apply_rec_substitution_in_term(Subst,
        term.variable(Var, dummy_context), VarSubstTerm),
    list.foldl(acc_var_if_equivalent(Subst, VarSubstTerm),
        [Var | set.to_sorted_list(SeenVars)], [], EqvVars),
    set.list_to_set(EqvVars, EqvVarsSet).

:- pred acc_var_if_equivalent(prog_substitution::in, prog_term::in,
    prog_var::in, list(prog_var)::in, list(prog_var)::out) is det.

acc_var_if_equivalent(Subst, VarSubstTerm, SeenVar, !EqvVars) :-
    term_subst.apply_rec_substitution_in_term(Subst,
        term.variable(SeenVar, dummy_context), SeenVarSubstTerm),
    % Are Var in our caller (the variable being deconstructed) and SeenVar
    % - mapped to the same term by Subst, and
    % - is this term a variable?
    ( if
        VarSubstTerm = term.variable(X, _),
        SeenVarSubstTerm = term.variable(X, _)
    then
        !:EqvVars = [SeenVar | !.EqvVars]
    else
        true
    ).

%---------------------------------------------------------------------------%
:- end_module check_hlds.scout_disjunctions.
%---------------------------------------------------------------------------%
