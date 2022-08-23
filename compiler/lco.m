%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% Copyright (C) 2015, 2017 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: lco.m.
% Author: zs.
% Modifications by wangp.
%
% Transform predicates with calls that are tail recursive modulo construction
% where (1) all recursive calls have the same args participating in the "modulo
% construction" part and (2) all the other output args are returned in the same
% registers in all recursive calls as expected by the head.
%
%   p(In1, ... InN, Out1, ... OutM) :-
%       Out1 = ground
%   p(In1, ... InN, Out1, ... OutM) :-
%       ...
%       p(In1, ... InN, Mid1, Out2... OutM)
%       Out1 = f1(...Mid1...)
%
% The definition of append fits this pattern:
%
% app(list(T)::in, list(T)::in, list(T)::out)
% app(A, B, C) :-
%   (
%       A == [],
%       C := B
%   ;
%       A => [H | T],
%       app(T, B, NT),
%       C <= [H | NT]
%   )
%
%---------------------------------------------------------------------------%
%
% TRANSFORMATION FOR LOW-LEVEL DATA
%
% Concrete example of what the original predicate and its return-via-memory
% variant should look like for append, in grades for which it is possible to
% take the address of a field:
%
% app(list(T)::in, list(T)::in, list(T)::out)
% app(A, B, C) :-
%   (
%       A == [],
%       C := B
%   ;
%       A => [H | T],
%       C <= [H | _HT] capture &HT in AddrHT
%       app'(T, B, AddrHT)
%   )
%
% app'(list(T)::in, list(T)::in, store_at_ref_type(T)::in)
% app'(A, B, AddrC) :-
%   (
%       A == [],
%       C := B,
%       store_at_ref_impure(AddrC, C)
%   ;
%       A => [H | T],
%       C <= [H | _HT] capture &HT in AddrHT
%       store_at_ref_impure(AddrC, C)
%       app'(T, B, AddrHT)
%   )
%
% The transformation done on the original predicate is to take recursive calls
% followed by construction unifications that use outputs of the recursive calls
% (each being used just once) and
%
% 1 move the constructions from after the recursive call to before, and attach
%   a feature to them that tells the code generator to not define a given list
%   of fields, but to capture their addresses in the related variable instead,
%
% 2 make the call go to the variant, and pass the address variables (e.g.
%   AddrHT) as inputs instead of the original variables (e.g. HT) as outputs.
%
% The variant predicate is based on the transformed version of the original
% predicate, but it has a further transformation performed on it. This further
% transformation
%
% 3 replaces the output arguments with input arguments of type
%   store_at_ref_type(T), where T is type of the field pointed to, and
%
% 4 calls to other procedures in the same SCC that are tail calls module
%   constructors are replaced by calls to variants where possible, and
%
% 5 follows each primitive goal that binds one of the output arguments
%   with a store to the memory location indicated by the corresponding pointer.
%
%   p(In1, ... InN, Out1, ... OutM) :-
%       Out1 = ground
%   p(In1, ... InN, Out1, ... OutM) :-
%       ...
%       Out1 = f1(...Mid1...)
%           capture addr of Mid1 in AddrMid1
%       p'(In1, ... InN, AddrMid1, Out2... OutM)
%
%   p'(In1, ... InN, RefOut1, ... OutM) :-
%       Out1 = ground,
%       store_at_ref_impure(RefOut1, Out1)
%   p'(In1, ... InN, RefOut1, Out2... OutM) :-
%       ...
%       Out1 = f1(...Mid1...)
%           capture addr of Mid1 in AddrMid1
%       store_at_ref_impure(RefOut1, Out1)
%       p'(In1, ... InN, AddrMid1, Out2... OutM)
%
%---------------------------------------------------------------------------%
%
% TRANSFORMATION FOR HIGH-LEVEL DATA
%
% In grades where it is impossible to take the address of a field (we assume
% this is so when using --highlevel-data), the transformed procedures are
% passed partially instantiated cells, whose holes need to be filled.
% The append example looks like:
%
% app(list(T)::in, list(T)::in, list(T)::out)
% app(A, B, C) :-
%   (
%       A == [],
%       C := B
%   ;
%       A => [H | T],
%       C <= [H | _],   % with hole
%       app'(T, B, C)
%   )
%
% app'(list(T)::in, list(T)::in, T::in(bound('[|]'(ground, free))))
% app'(A, B, C) :-
%   (
%       A == [],
%       C => [_ | []]   % fill in hole
%   ;
%       A => [H | T],
%       C <= [H | _HT], % bind C to AddrC
%       app'(T, B, AddrC)
%   )
%
% The differences are:
%
% 1 The output arguments become partially instantiated input arguments
%   instead of store_at_ref_type(T) arguments.
%
% 2 The holes in the output arguments are filled in with unifications
%   instead of a store_at_ref_impure builtin.
%
% 3 Variant procedures need to know the functor and position of the argument in
%   the partially instantiated structures, so many more variants could be
%   produced. The number of variants is capped.
%
%---------------------------------------------------------------------------%

:- module transform_hlds.lco.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.

:- pred lco_modulo_constructors(module_info::in, module_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.inst_lookup.
:- import_module check_hlds.inst_test.
:- import_module check_hlds.mode_top_functor.
:- import_module check_hlds.recompute_instmap_deltas.
:- import_module check_hlds.type_util.
:- import_module hlds.arg_info.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_dependency_graph.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module hlds.passes_aux.
:- import_module hlds.pred_name.
:- import_module hlds.pred_table.
:- import_module hlds.quantification.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.dependency_graph.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_rename.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_db.
:- import_module parse_tree.var_table.

:- import_module assoc_list.
:- import_module bag.
:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module multi_map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term_context.
:- import_module varset.

%---------------------------------------------------------------------------%

:- type variant_id
    --->    variant_id(
                list(variant_arg),  % The output arguments returned in memory.
                pred_proc_id,       % The id of the variant.
                sym_name            % The sym_name of the variant predicate.
            ).

:- type variant_arg
    --->    variant_arg(
                % Position of the output argument. The first output argument
                % is 1, the second is 2, and so on, without counting input
                % arguments.
                va_pos      :: int,

                % For low-level data, this field should be `no'.
                % For high-level data, this field should be `yes(FieldId)'
                % where FieldId indicates the functor that the argument
                % will be bound to, and the argument of that functor
                % which is to be filled.
                va_field    :: maybe(field_id)
            ).

    % Materialize this automatically defined field access function,
    % so we can pass it to higher order code.
    %
:- func va_pos(variant_arg) = int.

:- type field_id
    --->    field_id(
                fi_type     :: mer_type,
                fi_cons_id  :: cons_id,
                fi_arg      :: int
            ).

:- type variant_map == multi_map(pred_proc_id, variant_id).

:- type lco_is_permitted_on_scc
    --->    lco_is_not_permitted_on_scc
    ;       lco_is_permitted_on_scc.

:- type proc_changed
    --->    proc_not_changed
    ;       proc_changed.

:- type allow_float_addr
    --->    do_not_allow_float_addr
    ;       allow_float_addr.

:- type lco_info
    --->    lco_info(
                lco_module_info         :: module_info,
                lco_cur_scc_variants    :: variant_map,
                lco_var_table           :: var_table,
                lco_permitted           :: lco_is_permitted_on_scc,
                lco_changed             :: proc_changed
            ).

:- type lco_const_info
    --->    lco_const_info(
                lci_lower_scc_variants  :: variant_map,
                lci_cur_scc             :: set(pred_proc_id),
                lci_cur_proc_id         :: pred_proc_id,
                lci_cur_proc_pred       :: pred_info,
                lci_cur_proc_proc       :: proc_info,
                lci_cur_proc_outputs    :: list(prog_var),
                lci_cur_proc_detism     :: determinism,
                lci_allow_float_addr    :: allow_float_addr,
                lci_highlevel_data      :: bool
            ).

:- type var_to_target == assoc_list(prog_var, store_target).

:- type store_target
    --->    store_target(
                prog_var,
                maybe(field_id)
            ).

%---------------------------------------------------------------------------%

lco_modulo_constructors(!ModuleInfo) :-
    module_info_rebuild_dependency_info(!ModuleInfo, DepInfo),
    SCCs = dependency_info_get_bottom_up_sccs(DepInfo),
    list.foldl2(lco_scc, SCCs, map.init, _, !ModuleInfo).

:- pred lco_scc(set(pred_proc_id)::in, variant_map::in, variant_map::out,
    module_info::in, module_info::out) is det.

lco_scc(SCC, !VariantMap, !ModuleInfo) :-
    % XXX did we forget to add CurSCCVariants to !VariantMap?
    ModuleInfo0 = !.ModuleInfo,
    set.foldl4(lco_proc_if_permitted(!.VariantMap, SCC), SCC, !ModuleInfo,
        map.init, CurSCCVariantMap, map.init, CurSCCUpdateMap,
        lco_is_permitted_on_scc, Permitted),
    multi_map.to_flat_assoc_list(CurSCCVariantMap, CurSCCVariants),
    map.to_assoc_list(CurSCCUpdateMap, CurSCCUpdates),
    ( if
        Permitted = lco_is_permitted_on_scc,
        CurSCCUpdates = [_ | _]
    then
        trace [compile_time(flag("lco_log_updates")), io(!IO)] (
            io.open_append("/tmp/LCO_LOG", OpenResult, !IO),
            (
                OpenResult = ok(Stream),
                io.write_string(Stream, "updating scc:\n", !IO),
                list.foldl(lco_log_update(Stream, !.ModuleInfo),
                    CurSCCUpdates, !IO),
                io.close_output(Stream, !IO)
            ;
                OpenResult = error(_)
            )
        ),
        list.foldl(store_updated_procs_in_module_info, CurSCCUpdates,
            !ModuleInfo),
        list.foldl(update_variant_pred_info(CurSCCVariantMap), CurSCCVariants,
            !ModuleInfo)
    else
        !:ModuleInfo = ModuleInfo0
    ).

:- pred lco_log_update(io.output_stream::in, module_info::in,
    pair(pred_proc_id, proc_info)::in, io::di, io::uo) is det.

lco_log_update(Stream, ModuleInfo, PredProcId - _NewProcInfo, !IO) :-
    PredProcId = proc(PredId, ProcId),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_module_name(PredInfo, ModuleName),
    pred_info_get_name(PredInfo, PredName),
    SymNameStr = sym_name_to_string(qualified(ModuleName, PredName)),
    ProcIdInt = proc_id_to_int(ProcId),
    io.format(Stream, "    %s/%d\n", [s(SymNameStr), i(ProcIdInt)], !IO).

%---------------------------------------------------------------------------%

:- pred lco_proc_if_permitted(variant_map::in, scc::in, pred_proc_id::in,
    module_info::in, module_info::out, variant_map::in, variant_map::out,
    map(pred_proc_id, proc_info)::in, map(pred_proc_id, proc_info)::out,
    lco_is_permitted_on_scc::in, lco_is_permitted_on_scc::out) is det.

lco_proc_if_permitted(LowerSCCVariants, SCC, CurProc,
        !ModuleInfo, !CurSCCVariants, !CurSCCUpdates, !Permitted) :-
    (
        !.Permitted = lco_is_not_permitted_on_scc
    ;
        !.Permitted = lco_is_permitted_on_scc,
        CurProc = proc(PredId, ProcId),
        module_info_pred_proc_info(!.ModuleInfo, PredId, ProcId,
            PredInfo, ProcInfo0),
        pred_info_get_status(PredInfo, PredStatus),
        DefInThisModule = pred_status_defined_in_this_module(PredStatus),
        proc_info_get_inferred_determinism(ProcInfo0, Detism),
        ( if
            DefInThisModule = yes,
            acceptable_detism_for_lco(Detism) = yes
        then
            lco_proc(LowerSCCVariants, SCC, CurProc, PredInfo, ProcInfo0,
                !ModuleInfo, !CurSCCVariants, !CurSCCUpdates, !:Permitted)
        else
            !:Permitted = lco_is_not_permitted_on_scc
        )
    ).

:- pred lco_proc(variant_map::in, scc::in, pred_proc_id::in,
    pred_info::in, proc_info::in,
    module_info::in, module_info::out, variant_map::in, variant_map::out,
    map(pred_proc_id, proc_info)::in, map(pred_proc_id, proc_info)::out,
    lco_is_permitted_on_scc::out) is det.

lco_proc(LowerSCCVariants, SCC, CurProc, PredInfo, ProcInfo0,
        !ModuleInfo, !CurSCCVariants, !CurSCCUpdates, !:Permitted) :-
    trace [compiletime(flag("lco")), io(!IO)] (
        get_debug_output_stream(!.ModuleInfo, DebugStream, !IO),
        io.write_string(DebugStream, "\nlco_proc ", !IO),
        io.write_line(DebugStream, CurProc, !IO),
        io.flush_output(DebugStream, !IO)
    ),
    proc_info_get_var_table(ProcInfo0, VarTable0),
    proc_info_get_headvars(ProcInfo0, HeadVars),
    proc_info_get_argmodes(ProcInfo0, ArgModes),
    arg_info.compute_in_and_out_vars(!.ModuleInfo, VarTable0,
        HeadVars, ArgModes, _InputHeadVars, OutputHeadVars),
    proc_info_get_inferred_determinism(ProcInfo0, CurProcDetism),
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.get_target(Globals, Target),
    HighLevelData = compilation_target_high_level_data(Target),
    globals.lookup_bool_option(Globals, unboxed_float, UnboxedFloat),
    (
        UnboxedFloat = no,
        % In both C backends, we can and do store doubles across two words
        % of a cell when sizeof(double) > sizeof(void *). However we are
        % not yet ready to take the address of double-word fields
        % nor to assign to them.
        AllowFloatAddr = do_not_allow_float_addr
    ;
        UnboxedFloat = yes,
        AllowFloatAddr = allow_float_addr
    ),
    ConstInfo = lco_const_info(LowerSCCVariants, SCC,
        CurProc, PredInfo, ProcInfo0, OutputHeadVars, CurProcDetism,
        AllowFloatAddr, HighLevelData),
    Info0 = lco_info(!.ModuleInfo, !.CurSCCVariants, VarTable0,
        lco_is_permitted_on_scc, proc_not_changed),
    proc_info_get_goal(ProcInfo0, Goal0),
    lco_in_goal(Goal0, Goal, Info0, Info, ConstInfo),
    Info = lco_info(!:ModuleInfo, !:CurSCCVariants, VarTable,
        !:Permitted, Changed),
    ( if
        !.Permitted = lco_is_permitted_on_scc,
        Changed = proc_changed
    then
        trace [compiletime(flag("lco")), io(!IO)] (
            get_lco_debug_output_stream(Info, DebugStream, !IO),
            io.write_string(DebugStream, "\ngoal before lco:\n", !IO),
            dump_goal_nl(DebugStream, !.ModuleInfo, vns_var_table(VarTable),
                Goal0, !IO),
            io.write_string(DebugStream, "\ngoal after lco:\n", !IO),
            dump_goal_nl(DebugStream, !.ModuleInfo, vns_var_table(VarTable),
                Goal, !IO)
        ),
        some [!ProcInfo] (
            !:ProcInfo = ProcInfo0,
            proc_info_set_var_table(VarTable, !ProcInfo),
            proc_info_set_goal(Goal, !ProcInfo),
            % See the comment in transform_call_and_unifies for why these
            % are needed.
            requantify_proc_general(ord_nl_no_lambda, !ProcInfo),
            recompute_instmap_delta_proc(recomp_atomics,
                !ProcInfo, !ModuleInfo),
            map.det_insert(CurProc, !.ProcInfo, !CurSCCUpdates)
        )
    else
        true
    ).

    % Procedures which can succeed more than once can't do proper tail calls,
    % and procedures that cannot succeed at all should not be optimized
    % for time.
    %
:- func acceptable_detism_for_lco(determinism) = bool.

acceptable_detism_for_lco(detism_det) = yes.
acceptable_detism_for_lco(detism_semi) = yes.
acceptable_detism_for_lco(detism_cc_multi) = yes.
acceptable_detism_for_lco(detism_cc_non) = yes.
acceptable_detism_for_lco(detism_multi) = no.
acceptable_detism_for_lco(detism_non) = no.
acceptable_detism_for_lco(detism_failure) = no.
acceptable_detism_for_lco(detism_erroneous) = no.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred lco_in_goal(hlds_goal::in, hlds_goal::out, lco_info::in, lco_info::out,
    lco_const_info::in) is det.

lco_in_goal(Goal0, Goal, !Info, ConstInfo) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo),
    (
        GoalExpr0 = conj(ConjType, Goals0),
        (
            ConjType = plain_conj,
            lco_in_conj(Goals0, MaybeGoals, !Info, ConstInfo),
            (
                MaybeGoals = yes(Goals),
                GoalExpr = conj(plain_conj, Goals)
            ;
                MaybeGoals = no,
                % If the top-level conjunction doesn't end with some
                % unifications we can move before a recursive call,
                % maybe it ends with a switch or if-then-else, some of whose
                % arms fit that pattern.
                ( if list.split_last(Goals0, AllButLast, Last0) then
                    lco_in_goal(Last0, Last, !Info, ConstInfo),
                    GoalExpr = conj(plain_conj, AllButLast ++ [Last])
                else
                    GoalExpr = GoalExpr0
                )
            )
        ;
            ConjType = parallel_conj,
            GoalExpr = GoalExpr0,
            !Info ^ lco_permitted := lco_is_not_permitted_on_scc
        )
    ;
        GoalExpr0 = disj(Goals0),
        % There is no point in looking for tail calls in the non-last
        % disjuncts.
        ( if list.split_last(Goals0, AllButLast, Last0) then
            lco_in_goal(Last0, Last, !Info, ConstInfo),
            GoalExpr = disj(AllButLast ++ [Last])
        else
            GoalExpr = GoalExpr0
        )
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        lco_in_cases(Cases0, Cases, !Info, ConstInfo),
        GoalExpr = switch(Var, CanFail, Cases)
    ;
        GoalExpr0 = if_then_else(Vars, Cond, Then0, Else0),
        lco_in_goal(Then0, Then, !Info, ConstInfo),
        lco_in_goal(Else0, Else, !Info, ConstInfo),
        GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            GoalExpr = GoalExpr0
        else
            lco_in_goal(SubGoal0, SubGoal, !Info, ConstInfo),
            GoalExpr = scope(Reason, SubGoal)
        )
    ;
        ( GoalExpr0 = negation(_)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = unify(_, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _,  _, _, _)
        ),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = shorthand(_),
        % These should have been expanded out by now.
        unexpected($pred, "shorthand")
    ),
    Goal = hlds_goal(GoalExpr, GoalInfo).

%---------------------------------------------------------------------------%

:- pred lco_in_cases(list(case)::in, list(case)::out,
    lco_info::in, lco_info::out, lco_const_info::in) is det.

lco_in_cases([], [], !Info, _ConstInfo).
lco_in_cases([Case0 | Cases0], [Case | Cases], !Info, ConstInfo) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    lco_in_goal(Goal0, Goal, !Info, ConstInfo),
    Case = case(MainConsId, OtherConsIds, Goal),
    lco_in_cases(Cases0, Cases, !Info, ConstInfo).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % lco_in_conj(Goals0, MaybeGoals, !Info, ConstInfo)
    %
    % Given a conjunction whose structure is:
    %
    %   zero or more arbitrary goals
    %   recursive call that could be a last call modulo constructors
    %   one or more moveable goals
    %
    % move the moveable goals before the call.
    % If successful, MaybeGoals is yes(Goals) with the goals in the new order.
    % Otherwise MaybeGoals is no.
    %
:- pred lco_in_conj(list(hlds_goal)::in, maybe(list(hlds_goal))::out,
    lco_info::in, lco_info::out, lco_const_info::in) is det.

lco_in_conj(Goals0, MaybeGoals, !Info, ConstInfo) :-
    list.reverse(Goals0, RevGoals0),
    ( if
        divide_rev_conj(!.Info, ConstInfo, RevGoals0, [], AfterGoals,
            RecGoal, RecOutArgs, RevBeforeGoals),
        AfterGoals = [_ | _],
        set_of_var.list_to_set(RecOutArgs, DelayForVars0),
        list.foldl3(partition_dependent_goal(!.Info, ConstInfo), AfterGoals,
            [], RevAfterDependentGoals,
            [], RevAfterNonDependentGoals,
            DelayForVars0, DelayForVars),
        list.foldl2(acceptable_construct_unification(ConstInfo, DelayForVars),
            RevAfterDependentGoals, bag.init, UnifyInputVars, !Info)
    then
        list.reverse(RevAfterDependentGoals, UnifyGoals),
        transform_call_and_unifies(RecGoal, RecOutArgs,
            UnifyGoals, UnifyInputVars, MaybeGoals1, !Info, ConstInfo),
        (
            MaybeGoals1 = yes(UpdatedRecAndUnifies),
            Goals = list.reverse(RevBeforeGoals)
                ++ list.reverse(RevAfterNonDependentGoals)
                ++ UpdatedRecAndUnifies,
            MaybeGoals = yes(Goals)
        ;
            MaybeGoals1 = no,
            MaybeGoals = no
        )
    else
        MaybeGoals = no
    ).

    % Divide a conjunction into
    % - a list of goals before the rightmost recursive call
    % - the recursive call itself
    % - the goals following the recursive call which could potentially be
    %   moved before the recursive call, using the LCMC transform if necessary.
    %
    % invariant:
    %   reverse(RevGoals0) ++ AfterGoals0
    % = reverse(RevBeforeGoals) ++ [RecGoal] ++ AfterGoals
    %
:- pred divide_rev_conj(lco_info::in, lco_const_info::in, list(hlds_goal)::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    hlds_goal::out, list(prog_var)::out, list(hlds_goal)::out) is semidet.

divide_rev_conj(Info, ConstInfo, RevGoals0, !AfterGoals, RecGoal, RecOutArgs,
        RevBeforeGoals) :-
    (
        RevGoals0 = [],
        % No recursive call found.
        fail
    ;
        RevGoals0 = [RevGoal | RevGoalsTail],
        ( if
            potentially_transformable_recursive_call(Info, ConstInfo, RevGoal,
                OutArgs)
        then
            RecGoal = RevGoal,
            RecOutArgs = OutArgs,
            RevBeforeGoals = RevGoalsTail
        else if
            potentially_moveable_goal(RevGoal)
        then
            !:AfterGoals = [RevGoal | !.AfterGoals],
            divide_rev_conj(Info, ConstInfo, RevGoalsTail, !AfterGoals,
                RecGoal, RecOutArgs, RevBeforeGoals)
        else
            fail
        )
    ).

:- pred potentially_transformable_recursive_call(lco_info::in,
    lco_const_info::in, hlds_goal::in, list(prog_var)::out) is semidet.

potentially_transformable_recursive_call(Info, ConstInfo, Goal, OutArgs) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    GoalExpr = plain_call(PredId, ProcId, Args, _Builtin, _UnifyContext,
        _SymName),
    set.member(proc(PredId, ProcId), ConstInfo ^ lci_cur_scc),
    goal_info_get_determinism(GoalInfo) = ConstInfo ^ lci_cur_proc_detism,

    ModuleInfo = Info ^ lco_module_info,
    ProcInfo = ConstInfo ^ lci_cur_proc_proc,
    proc_info_get_var_table(ProcInfo, VarTable),

    module_info_proc_info(ModuleInfo, PredId, ProcId, CalleeProcInfo),
    proc_info_get_argmodes(CalleeProcInfo, CalleeArgModes),
    classify_proc_call_args(ModuleInfo, VarTable, Args, CalleeArgModes,
        _InArgs, OutArgs, UnusedArgs),
    UnusedArgs = [],

    trace [compiletime(flag("lco")), io(!IO)] (
        get_lco_debug_output_stream(Info, DebugStream, !IO),
        io.write_string(DebugStream, "call output args: ", !IO),
        io.write_line(DebugStream, OutArgs, !IO)
    ),
    list.length(OutArgs, NumOutArgs),
    CurrProcOutArgs = ConstInfo ^ lci_cur_proc_outputs,
    list.length(CurrProcOutArgs, NumCurrProcOutArgs),
    NumOutArgs = NumCurrProcOutArgs.

    % A goal is potentially moveable before a recursive call if it is det, and
    % guaranteed neither to throw an exception nor loop forever (subject to
    % --no-reorder-conj). It is actually moveable if it does not depend on the
    % output of the recursive call.
    %
    % For now we only move unification goals and goals which construct ground
    % terms.
    %
:- pred potentially_moveable_goal(hlds_goal::in) is semidet.

potentially_moveable_goal(Goal) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    goal_info_get_determinism(GoalInfo) = detism_det,
    require_complete_switch [GoalExpr]
    (
        GoalExpr = unify(_, _, _, _, _)
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( if Reason = from_ground_term(_, _) then
            true
        else
            potentially_moveable_goal(SubGoal)
        )
    ;
        ( GoalExpr = plain_call(_, _, _, _, _, _)
        ; GoalExpr = generic_call(_, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ; GoalExpr = conj(_, _)
        ; GoalExpr = disj(_)
        ; GoalExpr = switch(_, _, _)
        ; GoalExpr = negation(_)
        ; GoalExpr = if_then_else(_, _, _, _)
        ),
        fail
    ;
        GoalExpr = shorthand(_),
        unexpected($pred, "shorthand")
    ).

    % Partition a goal which follows a recursive call goal into those goals
    % which depend directly or indirectly on an output of the recursive call,
    % and those goals which don't.
    %
:- pred partition_dependent_goal(lco_info::in, lco_const_info::in,
    hlds_goal::in, list(hlds_goal)::in, list(hlds_goal)::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    set_of_progvar::in, set_of_progvar::out) is det.

partition_dependent_goal(_Info, _ConstInfo, Goal,
        !RevDependentGoals, !RevNonDependentGoals, !DelayForVars) :-
    Goal = hlds_goal(_GoalExpr, GoalInfo),
    goal_vars(Goal, GoalVars),
    set_of_var.intersect(!.DelayForVars, GoalVars, Intersection),
    ( if set_of_var.is_empty(Intersection) then
        !:RevNonDependentGoals = [Goal | !.RevNonDependentGoals]
    else
        !:RevDependentGoals = [Goal | !.RevDependentGoals],
        % Expand the set of variables for which we must delay goals.
        InstmapDelta = goal_info_get_instmap_delta(GoalInfo),
        instmap_delta_changed_vars(InstmapDelta, ChangedVars),
        set_of_var.union(ChangedVars, !DelayForVars)
    ).

%---------------------------------------------------------------------------%

:- pred acceptable_construct_unification(lco_const_info::in,
    set_of_progvar::in, hlds_goal::in,
    bag(prog_var)::in, bag(prog_var)::out, lco_info::in, lco_info::out)
    is semidet.

acceptable_construct_unification(ConstInfo, DelayForVars, Goal,
        !UnifyInputVars, !Info) :-
    Goal = hlds_goal(GoalExpr, _GoalInfo),
    GoalExpr = unify(_, _, _, Unification, _),
    Unification = construct(ConstructedVar, ConsId, ConstructArgVars,
        ArgModes, _, _, SubInfo),
    (
        SubInfo = no_construct_sub_info
    ;
        SubInfo = construct_sub_info(MaybeTakeAddrs, _),
        MaybeTakeAddrs = no
    ),
    ModuleInfo = !.Info ^ lco_module_info,
    all_true(acceptable_construct_mode(ModuleInfo), ArgModes),
    get_cons_repn_defn(ModuleInfo, ConsId, CtorRepn),
    ConsTag = CtorRepn ^ cr_tag,
    % Our optimization is inapplicable to constants, and its implementation
    % in the code generator can handle only some kinds of functors with args.
    % For example, it does not make sense to take the address of the field
    % of a function symbol of a `notag' type. The only functors it can handle
    % are the ones whose representation is remote_args_tag().
    ConsTag = remote_args_tag(_),
    % If the construction unification has any existential constraints,
    % then ConstructArgVars will have more elements than the number of
    % arguments in ConsRepn ^ cr_args (the extra variables will be the
    % type_infos and/or typeclass_infos providing information about
    % those constraints). This will cause all_delayed_arg_vars_are_full_words
    % to fail.
    %
    % We could work around that fact, but existentially constrained
    % construction unifications are so rare that there is no point.
    all_delayed_arg_vars_are_full_words(ConstructArgVars, CtorRepn ^ cr_args,
        DelayForVars),
    require_det (
        UnifyInputVars0 = !.UnifyInputVars,
        bag.delete(ConstructedVar, !UnifyInputVars),
        bag.insert_list(ConstructArgVars, !UnifyInputVars),
        trace [compiletime(flag("lco")), io(!IO)] (
            ProcInfo = ConstInfo ^ lci_cur_proc_proc,
            proc_info_get_var_table(ProcInfo, VarTable),
            ConstructedVarStr = mercury_var_to_string(VarTable,
                print_name_and_num, ConstructedVar),
            ConsIdStr = mercury_cons_id_to_string(output_debug,
                does_not_need_brackets, ConsId),
            ConstructArgVarStrs = list.map(
                mercury_var_to_string(VarTable, print_name_and_num),
                ConstructArgVars),
            ConstructArgVarsStr = string.join_list(", ", ConstructArgVarStrs),
            get_debug_output_stream(ModuleInfo, DebugStream, !IO),
            io.format(DebugStream,
                "processing unification %s <= %s(%s)\n",
                [s(ConstructedVarStr), s(ConsIdStr), s(ConstructArgVarsStr)],
                !IO),

            bag.to_assoc_list(UnifyInputVars0, InitialUnifyInputVars),
            bag.to_assoc_list(!.UnifyInputVars, UpdatedUnifyInputVars),
            io.write_string(DebugStream, "initial UnifyInputVars: ", !IO),
            io.write_line(DebugStream, InitialUnifyInputVars, !IO),
            io.write_string(DebugStream, "updated UnifyInputVars: ", !IO),
            io.write_line(DebugStream, UpdatedUnifyInputVars, !IO)
        )
    ).

:- pred all_delayed_arg_vars_are_full_words(list(prog_var)::in,
    list(constructor_arg_repn)::in, set_of_progvar::in) is semidet.

all_delayed_arg_vars_are_full_words([], [], _).
all_delayed_arg_vars_are_full_words([ArgVar | ArgVars], [ArgRepn | ArgRepns],
        DelayForVars) :-
    ArgWidth = ArgRepn ^ car_pos_width,
    (
        ArgWidth = apw_full(_, _)
    ;
        ArgWidth = apw_double(_, _, _),
        % XXX I (zs) am not sure whether this works. Until I am,
        % failing here plays things safe.
        fail
    ;
        ( ArgWidth = apw_none_nowhere
        ; ArgWidth = apw_none_shifted(_, _)
        ),
        % XXX We *could* allow lco to apply to fill in the values of
        % dummy variables by passing a dummy pointer to some static
        % variable we never read. However, it would be *far* simpler
        % to just fill in the one possible value before the recursive call.
        % XXX We don't do that yet either. Since code that fills in
        % the value of a variable of a dummy type after recursive call
        % is as rare as hen's teeth, this is not a great loss.
        fail
    ;
        ( ArgWidth = apw_partial_first(_, _, _, _, _, _)
        ; ArgWidth = apw_partial_shifted(_, _, _, _, _, _)
        ),
        % It is ok for the cell to have subword arguments (packed two or more
        % into a single word) IF AND ONLY IF we don't try to take
        % their addresses.
        not set_of_var.member(DelayForVars, ArgVar)
    ),
    all_delayed_arg_vars_are_full_words(ArgVars, ArgRepns, DelayForVars).

:- pred transform_call_and_unifies(hlds_goal::in, list(prog_var)::in,
    list(hlds_goal)::in, bag(prog_var)::in, maybe(list(hlds_goal))::out,
    lco_info::in, lco_info::out, lco_const_info::in) is det.

transform_call_and_unifies(CallGoal, CallOutArgVars, UnifyGoals,
        UnifyInputVars, MaybeGoals, !Info, ConstInfo) :-
    CallGoal = hlds_goal(CallGoalExpr, CallGoalInfo),
    ModuleInfo = !.Info ^ lco_module_info,
    CurProcInfo = ConstInfo ^ lci_cur_proc_proc,
    ( if
        CallGoalExpr = plain_call(PredId, ProcId, ArgVars, Builtin,
            UnifyContext, _SymName),
        CurrProcOutArgs = ConstInfo ^ lci_cur_proc_outputs,
        assoc_list.from_corresponding_lists(CallOutArgVars, CurrProcOutArgs,
            CallHeadPairs),
        find_args_to_pass_by_addr(ConstInfo, UnifyInputVars, CallHeadPairs,
            1, Mismatches, UpdatedCallOutArgs, map.init, Subst, !Info),
        trace [compiletime(flag("lco")), io(!IO)] (
            get_debug_output_stream(ModuleInfo, DebugStream, !IO),
            io.write_string(DebugStream, "find_args_to_pass_by_addr:\n", !IO),
            io.write_string(DebugStream, "call head pairs: ", !IO),
            io.write_line(DebugStream, CallHeadPairs, !IO),
            io.write_string(DebugStream, "mismatches: ", !IO),
            io.write_line(DebugStream, Mismatches, !IO),
            io.write_string(DebugStream, "updated call out args: ", !IO),
            io.write_line(DebugStream, UpdatedCallOutArgs, !IO),
            io.write_string(DebugStream, "substitution: ", !IO),
            map.to_assoc_list(Subst, SubstAL),
            io.write_line(DebugStream, SubstAL, !IO),
            io.nl(DebugStream, !IO)
        ),
        % If there are no mismatches, we would create an identical "variant".
        % Such cases should be optimized using other means.
        Mismatches = [_ | _],
        assoc_list.values(Mismatches, MismatchedCallArgs),
        % The variants we create return each output in only one place in
        % memory.
        all_true(occurs_once(UnifyInputVars), MismatchedCallArgs),

        list.map_foldl2(update_construct(ConstInfo, Subst),
            UnifyGoals, UpdatedUnifyGoals, map.init, AddrFieldIds, !Info),
        trace [compiletime(flag("lco")), io(!IO)] (
            get_debug_output_stream(ModuleInfo, DebugStream, !IO),
            VarTable = !.Info ^ lco_var_table,
            VarNameSrc = vns_var_table(VarTable),
            io.write_string(DebugStream, "original unifies:\n", !IO),
            list.foldl(dump_goal_nl(DebugStream, ModuleInfo, VarNameSrc),
                UnifyGoals, !IO),
            io.write_string(DebugStream, "updated unifies:\n", !IO),
            list.foldl(dump_goal_nl(DebugStream, ModuleInfo, VarNameSrc),
                UpdatedUnifyGoals, !IO),
            io.write_string(DebugStream, "addr field ids:\n", !IO),
            map.to_assoc_list(AddrFieldIds, AddrFieldIdsAL),
            io.write_line(DebugStream, AddrFieldIdsAL, !IO)
        ),
        HighLevelData = ConstInfo ^ lci_highlevel_data,
        make_variant_args(HighLevelData, AddrFieldIds, Mismatches,
            VariantArgs),
        ensure_variant_exists(PredId, ProcId, VariantArgs,
            VariantPredProcId, VariantSymName, !Info)
    then
        proc_info_get_var_table(CurProcInfo, CurProcVarTable),
        module_info_proc_info(ModuleInfo, PredId, ProcId, CalleeProcInfo),
        proc_info_get_argmodes(CalleeProcInfo, CalleeModes),
        update_call_args(ModuleInfo, CurProcVarTable, CalleeModes, ArgVars,
            UpdatedCallOutArgs, UpdatedArgs),
        VariantPredProcId = proc(VariantPredId, VariantProcId),
        UpdatedGoalExpr = plain_call(VariantPredId, VariantProcId,
            UpdatedArgs, Builtin, UnifyContext, VariantSymName),
        OrigCallPurity = goal_info_get_purity(CallGoalInfo),
        % If the original call was pure, then taking away its output
        % allows the simplify pass to just delete it from the procedure.
        % To prevent this, we mark the call as impure. However, we don't
        % want this to make the whole procedure body impure, so we wrap
        % a promise_purity scope around the pre-call unifications and the call
        % that together do what the call and the post-call unifications
        % originally did, and thus has the same purity. Since the unifications
        % are always pure, this purity is the call's original purity.
        goal_info_set_purity(purity_impure, CallGoalInfo, UpdatedGoalInfo),
        UpdatedGoal = hlds_goal(UpdatedGoalExpr, UpdatedGoalInfo),
        Goals = UpdatedUnifyGoals ++ [UpdatedGoal],
        ( if OrigCallPurity = purity_impure then
            % The promise_purity scope would be redundant.
            MaybeGoals = yes(Goals)
        else
            % Copying the nonlocals and instmap delta fields of ConjGoal and
            % PromiseGoal from the corresponding fields of CallGoal is
            % definitely wrong, since the conjunction binds the variables
            % that in the original code were bound by the post-call
            % unifications. We *could* cobble together the right values
            % of both fields from the corresponding fields of CallGoal and
            % UnifyGoals, but letting quantification and mode analysis do
            % the job lets us avoid duplicating their here.
            %
            % The other fields of goal_infos are either ok to copy
            % (such as determinism), or irrelevant (such as goal id).
            % The most complex case is goal features. However, at the moment
            % I (zs) believe that all the possible values of the goal_features
            % type fall into one of four categories:
            %
            % - Those that are never attached to calls.
            % - Those that are never attached to anything before the lco pass.
            % - Those that are never looked at after the lco pass.
            % - Those that are ok to copy to the conjunction and the scope.
            %
            % None need to be deleted from the conjunction or the scope.
            ConjGoalExpr = conj(plain_conj, Goals),
            ConjGoal = hlds_goal(ConjGoalExpr, UpdatedGoalInfo),
            PromiseGoalExpr = scope(promise_purity(OrigCallPurity), ConjGoal),
            PromiseGoal = hlds_goal(PromiseGoalExpr, UpdatedGoalInfo),
            MaybeGoals = yes([PromiseGoal])
        ),
        !Info ^ lco_changed := proc_changed
    else
        % The reversed conjunction does not follow the pattern we are looking
        % for, so we cannot optimize it.
        MaybeGoals = no
    ).

:- pred occurs_once(bag(prog_var)::in, prog_var::in) is semidet.

occurs_once(Bag, Var) :-
    bag.count_value(Bag, Var, 1).

:- pred update_call_args(module_info::in, var_table::in, list(mer_mode)::in,
    list(prog_var)::in, list(prog_var)::in, list(prog_var)::out) is det.

update_call_args(_ModuleInfo, _VarTable, [], [], UpdatedCallOutArgVars, []) :-
    expect(unify(UpdatedCallOutArgVars, []), $pred,
        "updating nonexistent arg").
update_call_args(_ModuleInfo, _VarTable, [], [_ | _], _, _) :-
    unexpected($pred, "mismatched lists").
update_call_args(_ModuleInfo, _VarTable, [_ | _], [], _, _) :-
    unexpected($pred, "mismatched lists").
update_call_args(ModuleInfo, VarTable, [CalleeMode | CalleeModes],
        [ArgVar | ArgVars], !.UpdatedCallOutArgVars, !:UpdatedArgVars) :-
    lookup_var_type(VarTable, ArgVar, CalleeType),
    mode_to_top_functor_mode(ModuleInfo, CalleeMode, CalleeType,
        TopFunctorMode),
    (
        TopFunctorMode = top_in,
        update_call_args(ModuleInfo, VarTable, CalleeModes, ArgVars,
            !.UpdatedCallOutArgVars, !:UpdatedArgVars),
        !:UpdatedArgVars = [ArgVar | !.UpdatedArgVars]
    ;
        TopFunctorMode = top_out,
        (
            !.UpdatedCallOutArgVars = [UpdatedArgVar | !:UpdatedCallOutArgVars]
        ;
            !.UpdatedCallOutArgVars = [],
            unexpected($pred, "no UpdatedCallOutArgs")
        ),
        update_call_args(ModuleInfo, VarTable, CalleeModes, ArgVars,
            !.UpdatedCallOutArgVars, !:UpdatedArgVars),
        !:UpdatedArgVars = [UpdatedArgVar | !.UpdatedArgVars]
    ;
        TopFunctorMode = top_unused,
        unexpected($pred, "top_unused")
    ).

%---------------------------------------------------------------------------%

:- pred classify_proc_call_args(module_info::in, var_table::in,
    list(prog_var)::in, list(mer_mode)::in,
    list(prog_var)::out, list(prog_var)::out, list(prog_var)::out) is det.

classify_proc_call_args(_ModuleInfo, _VarTable, [], [], [], [], []).
classify_proc_call_args(_ModuleInfo, _VarTable, [], [_ | _], _, _, _) :-
    unexpected($pred, "mismatched lists").
classify_proc_call_args(_ModuleInfo, _VarTable, [_ | _], [], _, _, _) :-
    unexpected($pred, "mismatched lists").
classify_proc_call_args(ModuleInfo, VarTable, [Arg | Args],
        [CalleeMode | CalleeModes], !:InArgs, !:OutArgs, !:UnusedArgs) :-
    classify_proc_call_args(ModuleInfo, VarTable, Args, CalleeModes,
        !:InArgs, !:OutArgs, !:UnusedArgs),
    lookup_var_type(VarTable, Arg, CalleeType),
    mode_to_top_functor_mode(ModuleInfo, CalleeMode, CalleeType,
        TopFunctorMode),
    (
        TopFunctorMode = top_in,
        !:InArgs = [Arg | !.InArgs]
    ;
        TopFunctorMode = top_out,
        !:OutArgs = [Arg | !.OutArgs]
    ;
        TopFunctorMode = top_unused,
        !:UnusedArgs = [Arg | !.UnusedArgs]
    ).

%---------------------------------------------------------------------------%

:- pred find_args_to_pass_by_addr(lco_const_info::in, bag(prog_var)::in,
    assoc_list(prog_var, prog_var)::in, int::in,
    assoc_list(int, prog_var)::out, list(prog_var)::out,
    map(prog_var, prog_var)::in, map(prog_var, prog_var)::out,
    lco_info::in, lco_info::out) is det.

find_args_to_pass_by_addr(_ConstInfo, _, [], _, [], [], !Subst, !Info).
find_args_to_pass_by_addr(ConstInfo, UnifyInputVars,
        [CallArg - HeadArg | CallHeadArgs], ArgNum, Mismatches,
        [UpdatedCallArg | UpdatedCallArgs], !Subst, !Info) :-
    find_args_to_pass_by_addr(ConstInfo, UnifyInputVars, CallHeadArgs,
        ArgNum + 1, MismatchesTail, UpdatedCallArgs, !Subst, !Info),
    ( if
        ConstInfo ^ lci_allow_float_addr = do_not_allow_float_addr,
        lookup_var_type(!.Info ^ lco_var_table, CallArg, CallArgType),
        type_to_ctor(CallArgType, CallArgTypeCtor),
        CallArgTypeCtor = type_ctor(unqualified("float"), 0)
    then
        !Info ^ lco_permitted := lco_is_not_permitted_on_scc
    else
        true
    ),
    ( if CallArg = HeadArg then
        UpdatedCallArg = CallArg,
        Mismatches = MismatchesTail,
        ( if bag.member(HeadArg, UnifyInputVars) then
            % This is a fix for Mantis bug 103. If CallArg is both a head
            % variable AND used in a unification that we are trying to move
            % before the recursive call, like this:
            %
            %   rec_call(..., HV2, X),
            %   HV3 = functor(HV2, X)
            %
            % then each call through this recursive call site is next to a
            % unification that creates a cell with a field (in this case
            % the first field) that needs to be set to the value of HV2
            % finally computed by a base case. After the Nth recursive call,
            % there will be N cells. There is no way we can pass N addresses
            % to the base case without making the transformed code pass a
            % LIST of addresses in place of HV2 in the argument list.
            % That would be a different transformation than what lco.m does,
            % and it is far from clear that that transformation would be a
            % good idea, since it doubles the number of loop iterations
            % we would need to execute (N recursive calls, and N iterations
            % of a loop to fill in the fields).
            %
            % Instead, we just disable the application of the lco
            % transformation to this call.

            !Info ^ lco_permitted := lco_is_not_permitted_on_scc
        else
            true
        )
    else
        make_address_var(ConstInfo, CallArg, UpdatedCallArg, !Info),
        Mismatches = [ArgNum - CallArg | MismatchesTail],
        map.det_insert(CallArg, UpdatedCallArg, !Subst)
    ).

:- pred make_address_var(lco_const_info::in, prog_var::in, prog_var::out,
    lco_info::in, lco_info::out) is det.

make_address_var(ConstInfo, Var, AddrVar, !Info) :-
    VarTable0 = !.Info ^ lco_var_table,
    lookup_var_entry(VarTable0, Var, VarEntry),
    Name = var_entry_name_default(Var, VarEntry, "SCCcallarg"),
    VarEntry = vte(_, VarType, _VarTypeIsDummy),
    HighLevelData = ConstInfo ^ lci_highlevel_data,
    (
        HighLevelData = no,
        AddrVarType = make_ref_type(VarType)
    ;
        HighLevelData = yes,
        % We set the actual type later when it is more convenient.
        AddrVarType = void_type
    ),
    AddrName = "Addr" ++ Name,
    AddrVarEntry = vte(AddrName, AddrVarType, is_not_dummy_type),
    add_var_entry(AddrVarEntry, AddrVar, VarTable0, VarTable),
    !Info ^ lco_var_table := VarTable.

:- func make_ref_type(mer_type) = mer_type.

make_ref_type(FieldType) = PtrType :-
    RefTypeName = qualified(mercury_private_builtin_module,
        "store_at_ref_type"),
    PtrType = defined_type(RefTypeName, [FieldType], kind_star).

%---------------------------------------------------------------------------%

:- pred make_variant_args(bool::in, map(prog_var, field_id)::in,
    assoc_list(int, prog_var)::in, list(variant_arg)::out) is det.

make_variant_args(HighLevelData, AddrVarFieldIds, Mismatches, VariantArgs) :-
    (
        HighLevelData = no,
        MakeArg = (func(Pos - _Var) = variant_arg(Pos, no))
    ;
        HighLevelData = yes,
        MakeArg =
            ( func(Pos - Var) = variant_arg(Pos, yes(FieldId)) :-
                map.lookup(AddrVarFieldIds, Var, FieldId)
            )
    ),
    VariantArgs = list.map(MakeArg, Mismatches).

:- pred ensure_variant_exists(pred_id::in, proc_id::in, list(variant_arg)::in,
    pred_proc_id::out, sym_name::out, lco_info::in, lco_info::out) is semidet.

ensure_variant_exists(PredId, ProcId, AddrOutArgs, VariantPredProcId,
        VariantSymName, !Info) :-
    PredProcId = proc(PredId, ProcId),
    CurSCCVariants0 = !.Info ^ lco_cur_scc_variants,
    ( if
        multi_map.search(CurSCCVariants0, PredProcId, ExistingVariantIds),
        match_existing_variant(ExistingVariantIds, AddrOutArgs,
            ExistingVariantId)
    then
        ExistingVariantId = variant_id(_, VariantPredProcId, VariantSymName)
    else
        ( if
            multi_map.search(CurSCCVariants0, PredProcId, ExistingVariants)
        then
            VariantNumber = list.length(ExistingVariants) + 1
        else
            VariantNumber = 1
        ),
        VariantNumber =< max_variants_per_proc,

        ModuleInfo0 = !.Info ^ lco_module_info,
        module_info_get_name(ModuleInfo0, ModuleName),
        module_info_pred_info(ModuleInfo0, PredId, PredInfo),
        PredOrFunc = pred_info_is_pred_or_func(PredInfo),
        pred_info_get_name(PredInfo, PredName),
        Transform = tn_last_call_modulo_cons(PredOrFunc, VariantNumber),
        % Even if PredInfo describes a predicate opt-imported from another
        % module, the variant we construct is defined in *this* module.
        make_transformed_pred_name(PredName, Transform, VariantName),

        some [!VariantPredInfo] (
            !:VariantPredInfo = PredInfo,
            VariantSymName = qualified(ModuleName, VariantName),
            pred_info_set_module_name(ModuleName, !VariantPredInfo),
            pred_info_set_name(VariantName, !VariantPredInfo),
            pred_info_set_is_pred_or_func(pf_predicate, !VariantPredInfo),

            pred_info_get_origin(PredInfo, Origin0),
            AddrOutArgNums = list.map(va_pos, AddrOutArgs),
            ProcTransform = proc_transform_lcmc(VariantNumber, AddrOutArgNums),
            Origin = origin_proc_transform(ProcTransform, Origin0,
                PredId, ProcId),
            pred_info_set_origin(Origin, !VariantPredInfo),

            % We throw away any other procs in the variant predicate, because
            % we create a separate predicate for each variant.
            %
            % update_variant_pred_info will update the proc_info
            % after transforming it.
            pred_info_get_proc_table(PredInfo, ProcTable),
            map.lookup(ProcTable, ProcId, ProcInfo),
            VariantProcTable = map.singleton(ProcId, ProcInfo),
            pred_info_set_proc_table(VariantProcTable, !VariantPredInfo),

            module_info_get_predicate_table(ModuleInfo0, PredTable0),
            predicate_table_insert(!.VariantPredInfo, VariantPredId,
                PredTable0, PredTable),
            module_info_set_predicate_table(PredTable,
                ModuleInfo0, ModuleInfo)
        ),

        VariantPredProcId = proc(VariantPredId, ProcId),
        !Info ^ lco_module_info := ModuleInfo,

        NewVariant =
            variant_id(AddrOutArgs, VariantPredProcId, VariantSymName),
        multi_map.set(PredProcId, NewVariant, CurSCCVariants0, CurSCCVariants),
        !Info ^ lco_cur_scc_variants := CurSCCVariants
    ).

:- pred match_existing_variant(list(variant_id)::in, list(variant_arg)::in,
    variant_id::out) is semidet.

match_existing_variant([VariantId0 | VariantIds], AddrArgNums, VariantId) :-
    ( if VariantId0 = variant_id(AddrArgNums, _, _) then
        VariantId = VariantId0
    else
        match_existing_variant(VariantIds, AddrArgNums, VariantId)
    ).

:- func max_variants_per_proc = int.

max_variants_per_proc = 4.

%---------------------------------------------------------------------------%

:- pred update_construct(lco_const_info::in, map(prog_var, prog_var)::in,
    hlds_goal::in, hlds_goal::out,
    map(prog_var, field_id)::in, map(prog_var, field_id)::out,
    lco_info::in, lco_info::out) is det.

update_construct(ConstInfo, Subst, Goal0, Goal, !AddrVarFieldIds, !Info) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    ( if
        GoalExpr0 = unify(LHS, RHS0, Mode, Unification0, UnifyContext),
        Unification0 = construct(Var, ConsId, ArgVars, ArgModes,
            How, IsUnique, SubInfo0),
        (
            SubInfo0 = no_construct_sub_info,
            TermSizeSlot = no
        ;
            SubInfo0 = construct_sub_info(no, TermSizeSlot)
        )
    then
        % For high-level data, we should not be using the `take_address_fields'
        % feature in `construct_sub_info', but simply assign the new cell to
        % each of the variables that takes its address. But as support for
        % partial instantiation is incomplete, instmaps for the assignments are
        % likely to be recomputed incorrectly.
        HighLevelData = ConstInfo ^ lci_highlevel_data,
        VarTable0 = !.Info ^ lco_var_table,
        lookup_var_entry(VarTable0, Var, VarEntry),
        VarEntry = vte(_, VarType, IsDummy),
        InstMapDelta0 = goal_info_get_instmap_delta(GoalInfo0),
        update_construct_args(Subst, HighLevelData, VarType, IsDummy,
            ConsId, 1, ArgVars, UpdatedArgVars, AddrFields,
            InstMapDelta0, InstMapDelta, !AddrVarFieldIds,
            VarTable0, VarTable),
        !Info ^ lco_var_table := VarTable,
        (
            AddrFields = [],
            Goal = Goal0
        ;
            AddrFields = [_ | _],
            SubInfo = construct_sub_info(yes(AddrFields), TermSizeSlot),
            Unification = construct(Var, ConsId, UpdatedArgVars, ArgModes,
                How, IsUnique, SubInfo),
            % We must update RHS because quantification gets the set of
            % variables in the unification from there, not from Unification.
            (
                RHS0 = rhs_var(_),
                unexpected($pred, "var RHS")
            ;
                RHS0 = rhs_functor(RHSConsId, IsExistConstr, RHSVars0),
                expect(unify(ConsId, RHSConsId), $pred, "cons_id mismatch"),
                rename_var_list(need_not_rename, Subst, RHSVars0, RHSVars),
                RHS = rhs_functor(RHSConsId, IsExistConstr, RHSVars)
            ;
                RHS0 = rhs_lambda_goal(_, _, _, _, _, _, _, _),
                unexpected($pred, "lambda RHS")
            ),
            GoalExpr = unify(LHS, RHS, Mode, Unification, UnifyContext),

            % For high-level data, there is a lie in this instmap_delta: the
            % new cell is not yet ground, although it will become ground after
            % the call that follows the construction.
            goal_info_set_instmap_delta(InstMapDelta, GoalInfo0, GoalInfo),
            Goal = hlds_goal(GoalExpr, GoalInfo)
        )
    else
        unexpected($pred, "not construct")
    ).

:- pred update_construct_args(map(prog_var, prog_var)::in, bool::in,
    mer_type::in, is_dummy_type::in, cons_id::in, int::in, list(prog_var)::in,
    list(prog_var)::out, list(int)::out, instmap_delta::in, instmap_delta::out,
    map(prog_var, field_id)::in, map(prog_var, field_id)::out,
    var_table::in, var_table::out) is det.

update_construct_args(_, _, _, _, _, _, [], [], [],
        !InstMapDelta, !AddrFieldIds, !VarTable).
update_construct_args(Subst, HighLevelData, VarType, IsDummyType,
        ConsId, ArgNum, [OrigVar | OrigVars], [UpdatedVar | UpdatedVars],
        AddrArgs, !InstMapDelta, !AddrFieldIds, !VarTable) :-
    update_construct_args(Subst, HighLevelData, VarType, IsDummyType,
        ConsId, ArgNum + 1, OrigVars, UpdatedVars, AddrArgsTail,
        !InstMapDelta, !AddrFieldIds, !VarTable),
    ( if map.search(Subst, OrigVar, AddrVar) then
        UpdatedVar = AddrVar,
        (
            HighLevelData = no,
            FinalInst = ground_inst
        ;
            HighLevelData = yes,
            BoundInst = bound_inst_with_free_arg(ConsId, ArgNum),
            FinalInst = bound(shared, inst_test_no_results, [BoundInst]),
            % We didn't do this when we initially created the variable.
            lookup_var_entry(!.VarTable, AddrVar, AddrVarEntry0),
            AddrVarEntry0 = vte(AddrVarName, _, _),
            % XXX Why is it that VarType, and its IsDummyType companion,
            % do not depend on which field we are taking the address of?
            AddrVarEntry = vte(AddrVarName, VarType, IsDummyType),
            update_var_entry(AddrVar, AddrVarEntry, !VarTable)
        ),
        instmap_delta_set_var(AddrVar, FinalInst, !InstMapDelta),
        FieldId = field_id(VarType, ConsId, ArgNum),
        map.det_insert(OrigVar, FieldId, !AddrFieldIds),
        AddrArgs = [ArgNum | AddrArgsTail]
    else
        UpdatedVar = OrigVar,
        AddrArgs = AddrArgsTail
    ).

:- func bound_inst_with_free_arg(cons_id, int) = bound_inst.

bound_inst_with_free_arg(ConsId, FreeArg) = Inst :-
    Arity = cons_id_arity(ConsId),
    list.duplicate(Arity, ground_inst, ArgInsts0),
    list.det_replace_nth(ArgInsts0, FreeArg, free_inst, ArgInsts),
    Inst = bound_functor(ConsId, ArgInsts).

%---------------------------------------------------------------------------%

:- pred acceptable_construct_mode(module_info::in, unify_mode::in) is semidet.

acceptable_construct_mode(ModuleInfo, UnifyMode) :-
    UnifyMode = unify_modes_li_lf_ri_rf(InitInstX, FinalInstX,
        InitInstY, FinalInstY),
    inst_is_free(ModuleInfo, InitInstX),
    inst_is_ground(ModuleInfo, InitInstY),
    inst_is_ground(ModuleInfo, FinalInstX),
    inst_is_ground(ModuleInfo, FinalInstY).

%---------------------------------------------------------------------------%

:- pred store_updated_procs_in_module_info(pair(pred_proc_id, proc_info)::in,
    module_info::in, module_info::out) is det.

store_updated_procs_in_module_info(PredProcId - NewProcInfo, !ModuleInfo) :-
    PredProcId = proc(PredId, ProcId),
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    pred_info_get_proc_table(PredInfo0, Procs0),
    map.det_update(ProcId, NewProcInfo, Procs0, Procs),
    pred_info_set_proc_table(Procs, PredInfo0, PredInfo),
    module_info_set_pred_info(PredId, PredInfo, !ModuleInfo).

%---------------------------------------------------------------------------%

:- pred update_variant_pred_info(variant_map::in,
    pair(pred_proc_id, variant_id)::in,
    module_info::in, module_info::out) is det.

update_variant_pred_info(VariantMap, PredProcId - VariantId, !ModuleInfo) :-
    trace [compiletime(flag("lco")), io(!IO)] (
        get_debug_output_stream(!.ModuleInfo, DebugStream, !IO),
        io.write_string(DebugStream, "\nupdate_variant_pred_info ", !IO),
        io.write_line(DebugStream, PredProcId, !IO),
        io.write_line(DebugStream, VariantId, !IO)
    ),
    VariantId = variant_id(AddrOutArgs, VariantPredProcId, _VariantSymName),
    VariantPredProcId = proc(VariantPredId, VariantProcId),
    PredProcId = proc(PredId, ProcId),

    module_info_pred_proc_info(!.ModuleInfo, PredId, ProcId,
        _PredInfo, ProcInfo),
    lco_transform_variant_proc(VariantMap, AddrOutArgs, ProcInfo,
        VariantProcInfo, !ModuleInfo),

    proc_info_get_headvars(VariantProcInfo, HeadVars),
    proc_info_get_var_table(VariantProcInfo, VarTable),
    lookup_var_types(VarTable, HeadVars, ArgTypes),

    some [!VariantPredInfo] (
        module_info_pred_info(!.ModuleInfo, VariantPredId, !:VariantPredInfo),
        % Put the updated arg type information in the VariantProcInfo
        % we just constructed into !VariantPredInfo.
        pred_info_get_arg_types(!.VariantPredInfo,
            TVarSet, ExistQVars, _ArgTypes0),
        pred_info_set_arg_types(TVarSet, ExistQVars, ArgTypes,
            !VariantPredInfo),
        % Put the VariantProcInfo we just constructed into !VariantPredInfo.
        VariantProcs = map.singleton(VariantProcId, VariantProcInfo),
        pred_info_set_proc_table(VariantProcs, !VariantPredInfo),
        module_info_set_pred_info(VariantPredId, !.VariantPredInfo,
            !ModuleInfo)
    ).

%---------------------------------------------------------------------------%

:- pred lco_transform_variant_proc(variant_map::in, list(variant_arg)::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out) is det.

lco_transform_variant_proc(VariantMap, AddrOutArgs, ProcInfo,
        !:VariantProcInfo, !ModuleInfo) :-
    !:VariantProcInfo = ProcInfo,
    proc_info_get_var_table(ProcInfo, VarTable0),
    proc_info_get_headvars(ProcInfo, HeadVars0),
    proc_info_get_argmodes(ProcInfo, ArgModes0),
    make_addr_vars(!.ModuleInfo, 1, HeadVars0, HeadVars, ArgModes0, ArgModes,
        AddrOutArgs, VarToAddr, VarTable0, VarTable),
    proc_info_set_headvars(HeadVars, !VariantProcInfo),
    proc_info_set_argmodes(ArgModes, !VariantProcInfo),
    proc_info_set_var_table(VarTable, !VariantProcInfo),

    proc_info_get_initial_instmap(!.ModuleInfo, ProcInfo, InstMap0),
    proc_info_get_goal(ProcInfo, Goal0),
    lco_transform_variant_goal(!.ModuleInfo,
        groundings_and_lco_calls(set_of_var.init), VariantMap,
        VarToAddr, InstMap0, Goal0, Goal, Changed, !VariantProcInfo),
    trace [compiletime(flag("lco")), io(!IO)] (
        get_debug_output_stream(!.ModuleInfo, DebugStream, !IO),
        io.write_string(DebugStream, "\nlco_transform_variant_proc\n", !IO),
        (
            Changed = no,
            io.write_string(DebugStream, "unchanged\n", !IO)
        ;
            Changed = yes,
            io.write_string(DebugStream, "goal before:\n", !IO),
            dump_goal_nl(DebugStream, !.ModuleInfo, vns_var_table(VarTable),
                Goal0, !IO),
            io.write_string(DebugStream, "\ngoal after:\n", !IO),
            dump_goal_nl(DebugStream, !.ModuleInfo, vns_var_table(VarTable),
                Goal, !IO)
        )
    ),
    proc_info_set_goal(Goal, !VariantProcInfo),
    % We changed the scopes of the headvars we now return via pointers.
    requantify_proc_general(ord_nl_no_lambda, !VariantProcInfo),

    % The high-level data transformation requires instmap deltas
    % to be recomputed.
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.get_target(Globals, Target),
    (
        Target = target_c
    ;
        ( Target = target_java
        ; Target = target_csharp
        ),
        recompute_instmap_delta_proc(no_recomp_atomics,
            !VariantProcInfo, !ModuleInfo)
    ).

:- pred make_addr_vars(module_info::in, int::in,
    list(prog_var)::in, list(prog_var)::out,
    list(mer_mode)::in, list(mer_mode)::out,
    list(variant_arg)::in, var_to_target::out,
    var_table::in, var_table::out) is det.

make_addr_vars(_, _, [], [], [], [], AddrOutArgs, [], !VarTable) :-
    expect(unify(AddrOutArgs, []), $pred, "AddrOutArgs != []").
make_addr_vars(_, _, [], _, [_ | _], _, _, _, !VarTable) :-
    unexpected($pred, "mismatched lists").
make_addr_vars(_, _, [_ | _], _, [], _, _, _, !VarTable) :-
    unexpected($pred, "mismatched lists").
make_addr_vars(ModuleInfo, NextOutArgNum,
        [HeadVar0 | HeadVars0], [HeadVar | HeadVars],
        [Mode0 | Modes0], [Mode | Modes],
        !.AddrOutArgs, VarToAddr, !VarTable) :-
    lookup_var_entry(!.VarTable, HeadVar0, HeadVarEntry0),
    HeadVarType = HeadVarEntry0 ^ vte_type,
    mode_to_top_functor_mode(ModuleInfo, Mode0, HeadVarType, TopFunctorMode),
    (
        TopFunctorMode = top_in,
        HeadVar = HeadVar0,
        Mode = Mode0,
        make_addr_vars(ModuleInfo, NextOutArgNum,
            HeadVars0, HeadVars, Modes0, Modes,
            !.AddrOutArgs, VarToAddr, !VarTable)
    ;
        TopFunctorMode = top_out,
        ( if
            !.AddrOutArgs = [AddrOutArg | !:AddrOutArgs],
            AddrOutArg = variant_arg(NextOutArgNum, MaybeFieldId)
        then
            HeadVarName = var_entry_name(HeadVar0, HeadVarEntry0),
            AddrVarName = "AddrOf" ++ HeadVarName,
            (
                MaybeFieldId = no,
                % For low-level data we replace the output argument with a
                % store_at_ref_type(T) input argument.
                AddrVarType = make_ref_type(HeadVarType),
                AddrVarTypeIsDummy = is_not_dummy_type,
                Mode = in_mode
            ;
                MaybeFieldId = yes(field_id(AddrVarType, ConsId, ArgNum)),
                % For high-level data we replace the output argument with a
                % partially instantiated structure. The structure has one
                % argument left unfilled.
                AddrVarTypeIsDummy = is_type_a_dummy(ModuleInfo, AddrVarType),
                BoundInst = bound_inst_with_free_arg(ConsId, ArgNum),
                InitialInst = bound(shared, inst_test_no_results, [BoundInst]),
                Mode = from_to_mode(InitialInst, ground_inst)
            ),
            AddrVarEntry = vte(AddrVarName, AddrVarType, AddrVarTypeIsDummy),
            add_var_entry(AddrVarEntry, AddrVar, !VarTable),
            HeadVar = AddrVar,
            make_addr_vars(ModuleInfo, NextOutArgNum + 1,
                HeadVars0, HeadVars, Modes0, Modes,
                !.AddrOutArgs, VarToAddrTail, !VarTable),
            VarToAddrHead = HeadVar0 - store_target(AddrVar, MaybeFieldId),
            VarToAddr = [VarToAddrHead | VarToAddrTail]
        else
            HeadVar = HeadVar0,
            Mode = Mode0,
            make_addr_vars(ModuleInfo, NextOutArgNum + 1,
                HeadVars0, HeadVars, Modes0, Modes,
                !.AddrOutArgs, VarToAddr, !VarTable)
        )
    ;
        TopFunctorMode = top_unused,
        unexpected($pred, "top_unused")
    ).

    % The lco_transform_variant_* predicates can perform two transformations.
    %
    % The first transformation is done by lco_transform_variant_atomic_goal.
    % It takes atomic goals that ground one or more of the output arguments
    % of the current procedure that we are supposed to return not by the usual
    % route, but by filling in a memory cell whose address we are given.
    % We have to perform this transformation whereever it is applicable
    % in the body of the procedure.
    %
    % The second transformation is done by lco_transform_variant_plain_call.
    % It checks whether the callee of a plain call goal has an lco-optimized
    % variant that is applicable to the call site, and if yes, calls that
    % variant instead of the original predicate. In the usual case where
    % the callee is recursive, this is basically a short-circuiting operation:
    % instead of calling first (say) p, which then calls lco-optimized-p
    % which then calls itself, we call lco-optimized-p directly. The main
    % benefit here is that the short-circuited code has a smaller footprint
    % in the instruction cache, and will thus probably get fewer misses there.
    %
    % However, the second transformation replaces code that would record
    % the value of the lco-optimized output variables of the callee in the
    % current procedure's stack frame with code that puts the values of those
    % variables in fields in heap cells. Any attempt by later code to look up
    % the values of these variables would thus result in a compiler crash
    % in the code generator (Mantis bug #539). We can therefore apply this
    % tranformation (named lco_calls in this type) ONLY if the affected
    % variables are NOT needed by later code. The argument of the second
    % function symbol here gives the set of variables whose values *are*
    % needed by later code.
    %
:- type variant_transforms
    --->    groundings_only
    ;       groundings_and_lco_calls(set_of_progvar).

:- pred lco_transform_variant_goal(module_info::in, variant_transforms::in,
    variant_map::in, var_to_target::in, instmap::in,
    hlds_goal::in, hlds_goal::out, bool::out,
    proc_info::in, proc_info::out) is det.

lco_transform_variant_goal(ModuleInfo, Transforms, VariantMap, VarToAddr,
        InstMap0, Goal0, Goal, Changed, !ProcInfo) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = conj(ConjType, Goals0),
        (
            ConjType = plain_conj,
            % We want to process the conjuncts of the conjunction backwards,
            % from last conjunct to first conjunct, so that when we process
            % a conjunct, we know what variables the conjuncts to its right
            % need as inputs. (This info is needed for the fix of Mantis
            % bug #539.)
            %
            % However, processing each goal requires its initial instmap,
            % and we can apply instmaps only from front to back. So we get
            % rev_conj_and_attach_init_instmaps to record the initial instmap
            % of each conjunct with that conjunct, and at the same time
            % reverse the list. We then give this reversed, instmap-annotated
            % list to lco_transform_variant_rev_conj.
            rev_conj_and_attach_init_instmaps(InstMap0, Goals0,
                [], RevGoalIMs0),
            lco_transform_variant_rev_conj(ModuleInfo, Transforms, VariantMap,
                VarToAddr, RevGoalIMs0, cord.init, GoalsCord, Changed,
                !ProcInfo),
            GoalExpr = conj(ConjType, cord.list(GoalsCord)),
            GoalInfo = GoalInfo0
        ;
            ConjType = parallel_conj,
            unexpected($pred, "parallel_conj")
        )
    ;
        GoalExpr0 = disj(Goals0),
        list.map2_foldl(
            lco_transform_variant_goal(ModuleInfo, Transforms, VariantMap,
                VarToAddr, InstMap0),
            Goals0, Goals, DisjsChanged, !ProcInfo),
        Changed = bool.or_list(DisjsChanged),
        GoalExpr = disj(Goals),
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        list.map2_foldl(
            lco_transform_variant_case(ModuleInfo, Transforms, VariantMap,
                VarToAddr, InstMap0),
            Cases0, Cases, CasesChanged, !ProcInfo),
        Changed = bool.or_list(CasesChanged),
        GoalExpr = switch(Var, CanFail, Cases),
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = if_then_else(Vars, Cond, Then0, Else0),
        update_instmap(Cond, InstMap0, InstMap1),
        lco_transform_variant_goal(ModuleInfo, Transforms, VariantMap,
            VarToAddr, InstMap1, Then0, Then, ThenChanged, !ProcInfo),
        lco_transform_variant_goal(ModuleInfo, Transforms, VariantMap,
            VarToAddr, InstMap0, Else0, Else, ElseChanged, !ProcInfo),
        Changed = bool.or(ThenChanged, ElseChanged),
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        ( if
            Reason = from_ground_term(FGTVar, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            ( if
                member(FGTVarPair, VarToAddr),
                FGTVarPair = FGTVar - _
            then
                % We can treat such a scope as an atomic goal, since all we
                % care about is with either is that it makes the variable
                % we're interested in ground.
                lco_transform_variant_atomic_goal(ModuleInfo, VarToAddr,
                    InstMap0, GoalInfo0, GoalExpr0, GoalExpr, Changed,
                    !ProcInfo)
            else
                GoalExpr = GoalExpr0,
                Changed = no
            )
        else
            lco_transform_variant_goal(ModuleInfo, Transforms, VariantMap,
                VarToAddr, InstMap0, SubGoal0, SubGoal, Changed, !ProcInfo),
            GoalExpr = scope(Reason, SubGoal)
        ),
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = negation(_),
        GoalExpr = GoalExpr0,
        GoalInfo = GoalInfo0,
        Changed = no
    ;
        GoalExpr0 = generic_call(_, _, _, _, _),
        lco_transform_variant_atomic_goal(ModuleInfo, VarToAddr,
            InstMap0, GoalInfo0, GoalExpr0, GoalExpr, Changed, !ProcInfo),
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = plain_call(_, _, _, _, _, _),
        lco_transform_variant_plain_call(ModuleInfo, Transforms, VariantMap,
            VarToAddr, InstMap0, GoalExpr0, GoalExpr,
            GoalInfo0, GoalInfo, Changed, !ProcInfo)
    ;
        GoalExpr0 = unify(_, _, _, _, _),
        lco_transform_variant_atomic_goal(ModuleInfo, VarToAddr,
            InstMap0, GoalInfo0, GoalExpr0, GoalExpr, Changed, !ProcInfo),
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = call_foreign_proc(_, _, _, _,  _, _, _),
        lco_transform_variant_atomic_goal(ModuleInfo, VarToAddr,
            InstMap0, GoalInfo0, GoalExpr0, GoalExpr, Changed, !ProcInfo),
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = shorthand(_),
        % These should have been expanded out by now.
        unexpected($pred, "shorthand")
    ),
    (
        Changed = yes,
        % This is not actually necessary for the transformation used for
        % high-level data.
        goal_info_set_purity(purity_impure, GoalInfo, GoalInfoImpure),
        Goal = hlds_goal(GoalExpr, GoalInfoImpure)
    ;
        Changed = no,
        Goal = Goal0
    ).

:- type goal_and_init_instmap
    --->    goal_and_init_instmap(hlds_goal, instmap).

:- pred rev_conj_and_attach_init_instmaps(instmap::in, list(hlds_goal)::in,
    list(goal_and_init_instmap)::in, list(goal_and_init_instmap)::out) is det.

rev_conj_and_attach_init_instmaps(_, [], !RevGoalIMs).
rev_conj_and_attach_init_instmaps(InstMap0, [Goal | Goals], !RevGoalIMs) :-
    GoalIM = goal_and_init_instmap(Goal, InstMap0),
    !:RevGoalIMs = [GoalIM | !.RevGoalIMs],
    update_instmap(Goal, InstMap0, InstMap1),
    rev_conj_and_attach_init_instmaps(InstMap1, Goals, !RevGoalIMs).

:- pred lco_transform_variant_rev_conj(module_info::in, variant_transforms::in,
    variant_map::in, var_to_target::in,
    list(goal_and_init_instmap)::in, cord(hlds_goal)::in, cord(hlds_goal)::out,
    bool::out, proc_info::in, proc_info::out) is det.

lco_transform_variant_rev_conj(_, _, _, _, [], !Conjuncts, no, !ProcInfo).
lco_transform_variant_rev_conj(ModuleInfo, Transforms0, VariantMap, VarToAddr,
        [RevGoalIM0 | RevGoalIMs0], !Conjuncts, Changed, !ProcInfo) :-
    RevGoalIM0 = goal_and_init_instmap(RevGoal0, RevGoalInitInstMap),
    lco_transform_variant_goal(ModuleInfo, Transforms0, VariantMap, VarToAddr,
        RevGoalInitInstMap, RevGoal0, RevGoal, HeadChanged, !ProcInfo),
    RevGoal = hlds_goal(RevGoalExpr, RevGoalInfo),
    (
        Transforms0 = groundings_only,
        Transforms1 = groundings_only
    ;
        Transforms0 = groundings_and_lco_calls(NeededLaterVars0),
        % XXX The value of Transforms starts out as groundings_and_lco_calls,
        % and this is the only logical place where it could transition to
        % groundings_only. We could e.g. switch to groundings_only
        % when we encounter nonrecursive calls in our backward traversal.
        %
        % However, if we did that, then lco_transform_variant_plain_call's
        % handling of groundings_and_lco_calls would not get tested
        % when compiling tests/valid/bug539.m. Also, not switching
        % to groundings_only here should yield very slightly faster code,
        % as explained in the comment on the variant_transforms type.
        get_input_vars_needed_by_goal(RevGoalInitInstMap, RevGoalInfo,
            NeededVars),
        set_of_var.union(NeededVars, NeededLaterVars0, NeededLaterVars1),
        Transforms1 = groundings_and_lco_calls(NeededLaterVars1)
    ),
    lco_transform_variant_rev_conj(ModuleInfo, Transforms1, VariantMap,
        VarToAddr, RevGoalIMs0, !Conjuncts, TailChanged, !ProcInfo),
    Changed = bool.or(HeadChanged, TailChanged),
    ( if RevGoalExpr = conj(plain_conj, RevGoalSubConjuncts) then
        !:Conjuncts = !.Conjuncts ++ cord.from_list(RevGoalSubConjuncts)
    else
        cord.snoc(RevGoal, !Conjuncts)
    ).

:- pred get_input_vars_needed_by_goal(instmap::in, hlds_goal_info::in,
    set_of_progvar::out) is det.

get_input_vars_needed_by_goal(InstMap0, GoalInfo, NeededVars) :-
    % A goal may need the value of a variable from preceding code if
    % - the variable already has an inst before the goal, and
    % - the goal actually refers to the variable.
    %
    % Technically, we want only the vars in InstMap0 whose inst is more
    % instantiated than just "free". However, the vast majority of variables
    % are bound by their first occurrence, so they already more instantiated
    % than "free" when they are first put into an instmap. We can afford
    % to be too-conservative with the remainder. (Including variables
    % in InstMapVars even when they are free errs on the side of caution.)
    instmap_vars(InstMap0, InstMapVars),
    NonLocals = goal_info_get_nonlocals(GoalInfo),
    NeededVars = set_of_var.intersect(InstMapVars, NonLocals).

:- pred lco_transform_variant_case(module_info::in, variant_transforms::in,
    variant_map::in, var_to_target::in, instmap::in,
    case::in, case::out, bool::out, proc_info::in, proc_info::out) is det.

lco_transform_variant_case(ModuleInfo, Transforms, VariantMap, VarToAddr,
        InstMap0, Case0, Case, Changed, !ProcInfo) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    lco_transform_variant_goal(ModuleInfo, Transforms, VariantMap, VarToAddr,
        InstMap0, Goal0, Goal, Changed, !ProcInfo),
    Case = case(MainConsId, OtherConsIds, Goal).

:- pred lco_transform_variant_plain_call(module_info::in,
    variant_transforms::in, variant_map::in, var_to_target::in, instmap::in,
    hlds_goal_expr::in(goal_expr_plain_call), hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out, bool::out,
    proc_info::in, proc_info::out) is det.

lco_transform_variant_plain_call(ModuleInfo, Transforms, VariantMap, VarToAddr,
        InstMap0, GoalExpr0, GoalExpr, GoalInfo0, GoalInfo, Changed,
        !ProcInfo) :-
    update_instmap_goal_info(GoalInfo0, InstMap0, InstMap1),
    list.filter(is_grounding(ModuleInfo, InstMap0, InstMap1),
        VarToAddr, GroundingVarToAddr),
    (
        GroundingVarToAddr = [],
        GoalExpr = GoalExpr0,
        GoalInfo = GoalInfo0,
        Changed = no
    ;
        GroundingVarToAddr = [_ | _],
        % Check if there is a variant of the called procedure where we can pass
        % an address variable in place of each variable that would be ground by
        % the call.
        GoalExpr0 = plain_call(CallPredId, CallProcId, ArgVars, Builtin,
            UnifyContext, _SymName),
        CallPredProcId = proc(CallPredId, CallProcId),
        module_info_proc_info(ModuleInfo, CallPredId, CallProcId,
            CalleeProcInfo),
        proc_info_get_var_table(!.ProcInfo, VarTable),
        proc_info_get_argmodes(CalleeProcInfo, CalleeArgModes),
        ( if
            multi_map.search(VariantMap, CallPredProcId, ExistingVariantIds),
            classify_proc_call_args(ModuleInfo, VarTable,
                ArgVars, CalleeArgModes,
                _InArgVars, OutArgVars, _UnusedArgVars),
            grounding_to_variant_args(GroundingVarToAddr, 1, OutArgVars, Subst,
                VariantArgVars, VariantArgs),
            match_existing_variant(ExistingVariantIds, VariantArgs,
                ExistingVariantId),

            % The need for the test done by the rest of this condition
            % is explained by the comment on the variant_transforms type.
            Transforms = groundings_and_lco_calls(NeededVarsSet),
            set_of_var.list_to_set(VariantArgVars, VariantArgVarSet),
            set_of_var.intersect(NeededVarsSet, VariantArgVarSet,
                NeededOutArgVarsSet),
            set_of_var.is_empty(NeededOutArgVarsSet)
        then
            rename_var_list(need_not_rename, Subst, ArgVars, CallArgVars),
            ExistingVariantId = variant_id(_, VariantPredProcId,
                VariantSymName),
            VariantPredProcId = proc(VariantPredId, VariantProcId),
            GoalExpr = plain_call(VariantPredId, VariantProcId, CallArgVars,
                Builtin, UnifyContext, VariantSymName),

            module_info_get_globals(ModuleInfo, Globals),
            globals.get_target(Globals, Target),
            (
                Target = target_c,
                GoalInfo = GoalInfo0
            ;
                ( Target = target_java
                ; Target = target_csharp
                ),
                % The partially instantiated cells will be ground
                % after the call.
                list.map(pair.fst, GroundingVarToAddr, GroundVars),
                map.apply_to_list(GroundVars, Subst, AddrVars),
                InstMapDelta0 = goal_info_get_instmap_delta(GoalInfo0),
                instmap_delta_set_vars_same(ground_inst, AddrVars,
                    InstMapDelta0, InstMapDelta),
                goal_info_set_instmap_delta(InstMapDelta, GoalInfo0, GoalInfo)
            ),
            Changed = yes
        else
            lco_transform_variant_atomic_goal(ModuleInfo, VarToAddr, InstMap0,
                GoalInfo0, GoalExpr0, GoalExpr, Changed, !ProcInfo),
            GoalInfo = GoalInfo0
        )
    ).

:- pred lco_transform_variant_atomic_goal(module_info::in, var_to_target::in,
    instmap::in, hlds_goal_info::in, hlds_goal_expr::in, hlds_goal_expr::out,
    bool::out, proc_info::in, proc_info::out) is det.

lco_transform_variant_atomic_goal(ModuleInfo, VarToAddr, InstMap0,
        GoalInfo, GoalExpr0, GoalExpr, Changed, !ProcInfo) :-
    update_instmap_goal_info(GoalInfo, InstMap0, InstMap1),
    list.filter(is_grounding(ModuleInfo, InstMap0, InstMap1),
        VarToAddr, GroundingVarToAddr),
    (
        GroundingVarToAddr = [],
        GoalExpr = GoalExpr0,
        Changed = no
    ;
        GroundingVarToAddr = [_ | _],
        list.map_foldl(make_store_goal(ModuleInfo, InstMap1),
            GroundingVarToAddr, StoreGoals, !ProcInfo),
        GoalExpr = conj(plain_conj,
            [hlds_goal(GoalExpr0, GoalInfo) | StoreGoals]),
        Changed = yes
    ).

:- pred grounding_to_variant_args(assoc_list(prog_var, store_target)::in,
    int::in, list(prog_var)::in, prog_var_renaming::out,
    list(prog_var)::out, list(variant_arg)::out) is det.

grounding_to_variant_args(GroundingVarToAddr, OutArgNum, OutArgVars, Subst,
        VariantArgVars, VariantArgs) :-
    (
        OutArgVars = [],
        Subst = map.init,
        VariantArgVars = [],
        VariantArgs = []
    ;
        OutArgVars = [OutArgVar | OutArgVarsTail],
        grounding_to_variant_args(GroundingVarToAddr, OutArgNum + 1,
            OutArgVarsTail, Subst0, VariantArgVarsTail, VariantArgsTail),
        ( if assoc_list.search(GroundingVarToAddr, OutArgVar, StoreTarget) then
            StoreTarget = store_target(StoreArgVar, MaybeFieldId),
            map.det_insert(OutArgVar, StoreArgVar, Subst0, Subst),
            VariantArg = variant_arg(OutArgNum, MaybeFieldId),
            VariantArgVars = [OutArgVar | VariantArgVarsTail],
            VariantArgs = [VariantArg | VariantArgsTail]
        else
            Subst = Subst0,
            VariantArgVars = VariantArgVarsTail,
            VariantArgs = VariantArgsTail
        )
    ).

:- pred make_store_goal(module_info::in, instmap::in,
    pair(prog_var, store_target)::in, hlds_goal::out,
    proc_info::in, proc_info::out) is det.

make_store_goal(ModuleInfo, InstMap, GroundVar - StoreTarget, Goal,
        !ProcInfo) :-
    StoreTarget = store_target(AddrVar, MaybeFieldId),
    (
        % Low-level data.
        MaybeFieldId = no,
        generate_plain_call(ModuleInfo, pf_predicate,
            mercury_private_builtin_module, "store_at_ref_impure",
            [], [AddrVar, GroundVar], instmap_delta_bind_vars([]), only_mode,
            detism_det, purity_impure, [], dummy_context, Goal)
    ;
        % High-level data.
        MaybeFieldId = yes(field_id(AddrVarType, ConsId, ArgNum)),
        get_cons_id_arg_types(ModuleInfo, AddrVarType, ConsId, ArgTypes),
        make_unification_args(ModuleInfo, GroundVar, ArgNum, 1, ArgTypes,
            ArgVars, ArgModes, !ProcInfo),

        RHS = rhs_functor(ConsId, is_not_exist_constr, ArgVars),

        instmap_lookup_var(InstMap, AddrVar, AddrVarInst0),
        inst_expand(ModuleInfo, AddrVarInst0, AddrVarInst),
        UnifyMode = unify_modes_li_lf_ri_rf(AddrVarInst, ground_inst,
            ground_inst, ground_inst),

        Unification = deconstruct(AddrVar, ConsId, ArgVars, ArgModes,
            cannot_fail, cannot_cgc),
        UnifyContext = unify_context(umc_implicit("lcmc"), []),

        GoalExpr = unify(AddrVar, RHS, UnifyMode, Unification, UnifyContext),

        goal_info_init(GoalInfo0),
        goal_info_set_determinism(detism_det, GoalInfo0, GoalInfo1),
        goal_info_set_instmap_delta(instmap_delta_bind_var(AddrVar),
            GoalInfo1, GoalInfo),

        Goal = hlds_goal(GoalExpr, GoalInfo)
    ).

:- pred make_unification_args(module_info::in, prog_var::in, int::in, int::in,
    list(mer_type)::in, list(prog_var)::out, list(unify_mode)::out,
    proc_info::in, proc_info::out) is det.

make_unification_args(ModuleInfo, GroundVar, TargetArgNum, CurArgNum, ArgTypes,
        ArgVars, ArgModes, !ProcInfo) :-
    (
        ArgTypes = [],
        ArgVars = [],
        ArgModes = []
    ;
        ArgTypes = [ArgType | ArgTypesTail],
        make_unification_args(ModuleInfo, GroundVar, TargetArgNum,
            CurArgNum + 1, ArgTypesTail, ArgVarsTail, ArgModesTail, !ProcInfo),
        make_unification_arg(ModuleInfo, GroundVar, TargetArgNum,
            CurArgNum, ArgType, Var, ArgMode, !ProcInfo),
        ArgVars = [Var | ArgVarsTail],
        ArgModes = [ArgMode | ArgModesTail]
    ).

:- pred make_unification_arg(module_info::in, prog_var::in, int::in, int::in,
    mer_type::in, prog_var::out, unify_mode::out,
    proc_info::in, proc_info::out) is det.

make_unification_arg(ModuleInfo, GroundVar, TargetArgNum, CurArgNum,
        ArgType, Var, UnifyMode, !ProcInfo) :-
    ( if CurArgNum = TargetArgNum then
        Var = GroundVar,
        UnifyMode = unify_modes_li_lf_ri_rf(free_inst, ground_inst,
            ground_inst, ground_inst)
    else
        % Bind other arguments to fresh variables.
        ArgTypeIsDummy = is_type_a_dummy(ModuleInfo, ArgType),
        proc_info_create_var_from_type("", ArgType, ArgTypeIsDummy,
            Var, !ProcInfo),
        UnifyMode = unify_modes_li_lf_ri_rf(ground_inst, ground_inst,
            free_inst, ground_inst)
    ).

%---------------------------------------------------------------------------%

:- pred is_grounding(module_info::in, instmap::in, instmap::in,
    pair(prog_var, store_target)::in) is semidet.

is_grounding(ModuleInfo, InstMap0, InstMap, Var - _StoreTarget) :-
    instmap_lookup_var(InstMap0, Var, Inst0),
    not inst_is_ground(ModuleInfo, Inst0),
    instmap_is_reachable(InstMap),
    instmap_lookup_var(InstMap, Var, Inst),
    inst_is_ground(ModuleInfo, Inst).

%---------------------------------------------------------------------------%

:- pred get_lco_debug_output_stream(lco_info::in, io.text_output_stream::out,
    io::di, io::uo) is det.

get_lco_debug_output_stream(Info, DebugStream, !IO) :-
    ModuleInfo = Info ^ lco_module_info,
    get_debug_output_stream(ModuleInfo, DebugStream, !IO).

%---------------------------------------------------------------------------%
:- end_module transform_hlds.lco.
%---------------------------------------------------------------------------%
