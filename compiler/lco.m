%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% Copyright (C) 2015, 2017 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
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
%-----------------------------------------------------------------------------%
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
%       store_at_ref(AddrC, C)
%   ;
%       A => [H | T],
%       C <= [H | _HT] capture &HT in AddrHT
%       store_at_ref(AddrC, C)
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
% 4 calls to other procedures in the same SCC are replaced by calls to
%   variants where possible, and
%
% 5 follows each primitive goal that binds one of the output arguments
%   with a store to the memory location indicated by the corresponding pointer.
%
%   p(In1, ... InN, Out1, ... OutM) :-
%       Out1 = ground
%   p(In1, ... InN, Out1, ... OutM) :-
%       ...
%       Out1 = f1(...Mid1...)
%           capture addr of Mid1 in Addr1
%       p'(In1, ... InN, Addr1, Out2... OutM)
%
%   p'(In1, ... InN, Ref1, ... OutM) :-
%       Out1 = ground,
%       store_at_ref(Ref1, Out1)
%   p'(In1, ... InN, Ref1, Out2... OutM) :-
%       ...
%       Out1 = f1(...Mid1...)
%           capture addr of Mid1 in Addr1
%       store_at_ref(Ref1, Out1)
%       p'(In1, ... InN, Addr1, Out2... OutM)
%
%-----------------------------------------------------------------------------%
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
%   instead of a store_at_ref builtin.
%
% 3 Variant procedures need to know the functor and position of the argument in
%   the partially instantiated structures, so many more variants could be
%   produced. The number of variants is capped.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.lco.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.

:- pred lco_modulo_constructors(module_info::in, module_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.inst_test.
:- import_module check_hlds.inst_util.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.arg_info.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_code_util.
:- import_module hlds.hlds_dependency_graph.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module hlds.pred_table.
:- import_module hlds.quantification.
:- import_module hlds.status.
:- import_module hlds.vartypes.
:- import_module libs.
:- import_module libs.dependency_graph.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_rename.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.

:- import_module assoc_list.
:- import_module bag.
:- import_module bool.
:- import_module int.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module multi_map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

:- type variant_id
    --->    variant_id(
                list(variant_arg),  % The output arguments returned in memory.
                pred_proc_id,       % The id of the variant.
                string              % The name of the variant predicate.
            ).

:- type variant_arg
    --->    variant_arg(
                % Position of the output argument. The first output argument
                % is 1, the second is 2, and so on, without counting input
                % arguments.
                va_pos      :: int,

                % For low-level data this is `no'.
                % For high-level data this is `yes(FieldId)' where FieldId
                % indicates the functor that the argument will be bound to, and
                % the argument of that functor which is to be filled.
                va_field    :: maybe(field_id)
            ).

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
                lco_var_set             :: prog_varset,
                lco_var_types           :: vartypes,
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

%-----------------------------------------------------------------------------%

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
        list.foldl(lco_process_proc_update, CurSCCUpdates, !ModuleInfo),
        list.foldl(lco_process_proc_variant(CurSCCVariantMap), CurSCCVariants,
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

%-----------------------------------------------------------------------------%

:- pred lco_process_proc_update(pair(pred_proc_id, proc_info)::in,
    module_info::in, module_info::out) is det.

lco_process_proc_update(PredProcId - NewProcInfo, !ModuleInfo) :-
    PredProcId = proc(PredId, ProcId),
    module_info_get_preds(!.ModuleInfo, PredTable0),
    map.lookup(PredTable0, PredId, PredInfo0),
    pred_info_get_proc_table(PredInfo0, Procs0),
    map.det_update(ProcId, NewProcInfo, Procs0, Procs),
    pred_info_set_proc_table(Procs, PredInfo0, PredInfo),
    map.det_update(PredId, PredInfo, PredTable0, PredTable),
    module_info_set_preds(PredTable, !ModuleInfo).

:- pred lco_process_proc_variant(variant_map::in,
    pair(pred_proc_id, variant_id)::in,
    module_info::in, module_info::out) is det.

lco_process_proc_variant(VariantMap, PredProcId - VariantId, !ModuleInfo) :-
    VariantId = variant_id(AddrOutArgs, VariantPredProcId, VariantName),
    VariantPredProcId = proc(VariantPredId, VariantProcId),
    PredProcId = proc(PredId, ProcId),

    module_info_pred_proc_info(!.ModuleInfo, PredId, ProcId,
        _PredInfo, ProcInfo),
    lco_transform_variant_proc(VariantMap, AddrOutArgs, ProcInfo,
        VariantProcInfo, !ModuleInfo),

    proc_info_get_headvars(VariantProcInfo, HeadVars),
    proc_info_get_vartypes(VariantProcInfo, VarTypes),
    lookup_var_types(VarTypes, HeadVars, ArgTypes),

    some [!VariantPredInfo, !PredTable] (
        module_info_get_preds(!.ModuleInfo, !:PredTable),
        map.lookup(!.PredTable, VariantPredId, !:VariantPredInfo),
        pred_info_set_name(VariantName, !VariantPredInfo),
        pred_info_set_is_pred_or_func(pf_predicate, !VariantPredInfo),

        % Update the argument types for the variant's pred_info.
        pred_info_get_arg_types(!.VariantPredInfo, TVarSet, ExistQVars,
            _ArgTypes0),
        pred_info_set_arg_types(TVarSet, ExistQVars, ArgTypes,
            !VariantPredInfo),

        pred_info_get_origin(!.VariantPredInfo, Origin0),
        AddrOutArgPosns = list.map(va_pos, AddrOutArgs),
        Transform = transform_return_via_ptr(ProcId, AddrOutArgPosns),
        Origin = origin_transformed(Transform, Origin0, PredId),
        pred_info_set_origin(Origin, !VariantPredInfo),

        % We throw away any other procs in the variant predicate, because
        % we create a separate predicate for each variant.
        VariantProcs = map.singleton(VariantProcId, VariantProcInfo),
        pred_info_set_proc_table(VariantProcs, !VariantPredInfo),
        map.det_update(VariantPredId, !.VariantPredInfo, !PredTable),
        module_info_set_preds(!.PredTable, !ModuleInfo)
    ).

:- func va_pos(variant_arg) = int.

%-----------------------------------------------------------------------------%

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
            ( DefInThisModule = no
            ; not acceptable_detism_for_lco(Detism)
            )
        then
            !:Permitted = lco_is_not_permitted_on_scc
        else
            lco_proc(LowerSCCVariants, SCC, CurProc, PredInfo, ProcInfo0,
                !ModuleInfo, !CurSCCVariants, !CurSCCUpdates, !:Permitted)
        )
    ).

:- pred lco_proc(variant_map::in, scc::in, pred_proc_id::in,
    pred_info::in, proc_info::in,
    module_info::in, module_info::out, variant_map::in, variant_map::out,
    map(pred_proc_id, proc_info)::in, map(pred_proc_id, proc_info)::out,
    lco_is_permitted_on_scc::out) is det.

lco_proc(LowerSCCVariants, SCC, CurProc, PredInfo, ProcInfo0,
        !ModuleInfo, !CurSCCVariants, !CurSCCUpdates, !:Permitted) :-
    proc_info_get_varset(ProcInfo0, VarSet0),
    proc_info_get_vartypes(ProcInfo0, VarTypes0),
    proc_info_get_headvars(ProcInfo0, HeadVars),
    proc_info_get_argmodes(ProcInfo0, ArgModes),
    lookup_var_types(VarTypes0, HeadVars, ArgTypes),
    arg_info.compute_in_and_out_vars(!.ModuleInfo, HeadVars,
        ArgModes, ArgTypes, _InputHeadVars, OutputHeadVars),
    proc_info_get_inferred_determinism(ProcInfo0, CurProcDetism),
    globals.lookup_bool_option(Globals, highlevel_data, HighLevelData),
    module_info_get_globals(!.ModuleInfo, Globals),
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
    Info0 = lco_info(!.ModuleInfo, !.CurSCCVariants, VarSet0, VarTypes0,
        lco_is_permitted_on_scc, proc_not_changed),
    proc_info_get_goal(ProcInfo0, Goal0),
    lco_in_goal(Goal0, Goal, Info0, Info, ConstInfo),
    Info = lco_info(!:ModuleInfo, !:CurSCCVariants, VarSet, VarTypes,
        !:Permitted, Changed),
    ( if
        !.Permitted = lco_is_permitted_on_scc,
        Changed = proc_changed
    then
        trace [compiletime(flag("lco")), io(!IO)] (
            io.write_string("\ngoal before lco:\n", !IO),
            dump_goal(!.ModuleInfo, VarSet, Goal0, !IO),
            io.nl(!IO),
            io.write_string("\ngoal after lco:\n", !IO),
            dump_goal(!.ModuleInfo, VarSet, Goal, !IO),
            io.nl(!IO)
        ),
        some [!ProcInfo] (
            !:ProcInfo = ProcInfo0,
            proc_info_set_varset(VarSet, !ProcInfo),
            proc_info_set_vartypes(VarTypes, !ProcInfo),
            proc_info_set_goal(Goal, !ProcInfo),
            % See the comment in transform_call_and_unifies for why these
            % are needed.
            requantify_proc_general(ordinary_nonlocals_no_lambda, !ProcInfo),
            recompute_instmap_delta_proc(recompute_atomic_instmap_deltas,
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
:- pred acceptable_detism_for_lco(determinism::in) is semidet.

acceptable_detism_for_lco(detism_det).
acceptable_detism_for_lco(detism_semi).
acceptable_detism_for_lco(detism_cc_multi).
acceptable_detism_for_lco(detism_cc_non).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

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
        unexpected($module, $pred, "shorthand")
    ),
    Goal = hlds_goal(GoalExpr, GoalInfo).

%-----------------------------------------------------------------------------%

:- pred lco_in_disj(list(hlds_goal)::in, list(hlds_goal)::out,
    lco_info::in, lco_info::out, lco_const_info::in) is det.

lco_in_disj([], [], !Info, _ConstInfo).
lco_in_disj([Goal0 | Goals0], [Goal | Goals], !Info, ConstInfo) :-
    lco_in_goal(Goal0, Goal, !Info, ConstInfo),
    lco_in_disj(Goals0, Goals, !Info, ConstInfo).

:- pred lco_in_cases(list(case)::in, list(case)::out,
    lco_info::in, lco_info::out, lco_const_info::in) is det.

lco_in_cases([], [], !Info, _ConstInfo).
lco_in_cases([Case0 | Cases0], [Case | Cases], !Info, ConstInfo) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    lco_in_goal(Goal0, Goal, !Info, ConstInfo),
    Case = case(MainConsId, OtherConsIds, Goal),
    lco_in_cases(Cases0, Cases, !Info, ConstInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

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
            DelayForVars0, _DelayForVars),
        list.foldl2(acceptable_construct_unification,
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
            cons(RevGoal, !AfterGoals),
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
    proc_info_get_vartypes(ProcInfo, VarTypes),

    module_info_proc_info(ModuleInfo, PredId, ProcId, CalleeProcInfo),
    proc_info_get_argmodes(CalleeProcInfo, CalleeArgModes),
    classify_proc_call_args(ModuleInfo, VarTypes, Args, CalleeArgModes,
        _InArgs, OutArgs, UnusedArgs),
    UnusedArgs = [],

    trace [compiletime(flag("lco")), io(!IO)] (
        io.write_string("call output args: ", !IO),
        io.write(OutArgs, !IO),
        io.nl(!IO)
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
        unexpected($module, $pred, "shorthand")
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
        cons(Goal, !RevNonDependentGoals)
    else
        cons(Goal, !RevDependentGoals),
        % Expand the set of variables for which we must delay goals.
        InstmapDelta = goal_info_get_instmap_delta(GoalInfo),
        instmap_delta_changed_vars(InstmapDelta, ChangedVars),
        set_of_var.union(ChangedVars, !DelayForVars)
    ).

%-----------------------------------------------------------------------------%

:- pred acceptable_construct_unification(hlds_goal::in, bag(prog_var)::in,
    bag(prog_var)::out, lco_info::in, lco_info::out) is semidet.

acceptable_construct_unification(Goal, !UnifyInputVars, !Info) :-
    Goal = hlds_goal(GoalExpr, _GoalInfo),
    GoalExpr = unify(_, _, _, Unification, _),
    Unification = construct(ConstructedVar, ConsId, ConstructArgs,
        ArgModes, _, _, SubInfo),
    (
        SubInfo = no_construct_sub_info
    ;
        SubInfo = construct_sub_info(no, _)
    ),
    ModuleInfo = !.Info ^ lco_module_info,
    all_true(acceptable_construct_mode(ModuleInfo), ArgModes),
    ConsTag = cons_id_to_tag(ModuleInfo, ConsId),
    % The code generator can't handle some kinds of tags. For example, it does
    % not make sense to take the address of the field of a function symbol of a
    % `notag' type. These are the kinds it CAN handle.
    (
        ConsTag = single_functor_tag
    ;
        ConsTag = unshared_tag(_)
    ;
        ConsTag = shared_remote_tag(_, _)
    ),
    require_det (
        trace [compiletime(flag("lco")), io(!IO)] (
            io.write_string("processing unification ", !IO),
            io.write(ConstructedVar, !IO),
            io.write_string(" <= ", !IO),
            io.write(ConsId, !IO),
            io.write_string("(", !IO),
            io.write(ConstructArgs, !IO),
            io.write_string(")\n", !IO)
        ),
        trace [compiletime(flag("lco")), io(!IO)] (
            io.write_string("initial UnifyInputVars: ", !IO),
            io.write(!.UnifyInputVars, !IO),
            io.nl(!IO)
        ),
        bag.delete(ConstructedVar, !UnifyInputVars),
        bag.insert_list(ConstructArgs, !UnifyInputVars),
        trace [compiletime(flag("lco")), io(!IO)] (
            io.write_string("updated UnifyInputVars: ", !IO),
            io.write(!.UnifyInputVars, !IO),
            io.nl(!IO)
        )
    ).

:- pred transform_call_and_unifies(hlds_goal::in, list(prog_var)::in,
    list(hlds_goal)::in, bag(prog_var)::in, maybe(list(hlds_goal))::out,
    lco_info::in, lco_info::out, lco_const_info::in) is det.

transform_call_and_unifies(CallGoal, CallOutArgs, UnifyGoals, UnifyInputVars,
        MaybeGoals, !Info, ConstInfo) :-
    CallGoal = hlds_goal(CallGoalExpr, CallGoalInfo),
    ModuleInfo = !.Info ^ lco_module_info,
    ProcInfo = ConstInfo ^ lci_cur_proc_proc,
    proc_info_get_vartypes(ProcInfo, VarTypes),
    ( if
        CallGoalExpr = plain_call(PredId, ProcId, Args, Builtin, UnifyContext,
            SymName),
        CurrProcOutArgs = ConstInfo ^ lci_cur_proc_outputs,
        assoc_list.from_corresponding_lists(CallOutArgs, CurrProcOutArgs,
            CallHeadPairs),
        find_args_to_pass_by_addr(ConstInfo, UnifyInputVars, CallHeadPairs,
            1, Mismatches, UpdatedCallOutArgs, map.init, Subst, !Info),
        trace [compiletime(flag("lco")), io(!IO)] (
            io.write_string("find_args_to_pass_by_addr:\n", !IO),
            io.write_string("call head pairs: ", !IO),
            io.write(CallHeadPairs, !IO),
            io.nl(!IO),
            io.write_string("mismatches: ", !IO),
            io.write(Mismatches, !IO),
            io.nl(!IO),
            io.write_string("updated call out args: ", !IO),
            io.write(UpdatedCallOutArgs, !IO),
            io.nl(!IO),
            io.write_string("substitution: ", !IO),
            io.write(Subst, !IO),
            io.nl(!IO),
            io.nl(!IO)
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
            VarSet = !.Info ^ lco_var_set,
            io.write_string("original unifies:\n", !IO),
            io.write_list(UnifyGoals, "\n",
                dump_goal(ModuleInfo, VarSet), !IO),
            io.nl(!IO),
            io.write_string("updated unifies:\n", !IO),
            io.write_list(UpdatedUnifyGoals, "\n",
                dump_goal(ModuleInfo, VarSet), !IO),
            io.nl(!IO),
            io.write_string("addr field ids:\n", !IO),
            io.write(AddrFieldIds, !IO),
            io.nl(!IO)
        ),
        HighLevelData = ConstInfo ^ lci_highlevel_data,
        make_variant_args(HighLevelData, AddrFieldIds, Mismatches,
            VariantArgs),
        ensure_variant_exists(PredId, ProcId, VariantArgs,
            VariantPredProcId, SymName, VariantSymName, !Info)
    then
        module_info_proc_info(ModuleInfo, PredId, ProcId, CalleeProcInfo),
        proc_info_get_argmodes(CalleeProcInfo, CalleeModes),
        update_call_args(ModuleInfo, VarTypes, CalleeModes, Args,
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

:- pred update_call_args(module_info::in, vartypes::in, list(mer_mode)::in,
    list(prog_var)::in, list(prog_var)::in, list(prog_var)::out) is det.

update_call_args(_ModuleInfo, _VarTypes, [], [], UpdatedCallOutArgs, []) :-
    expect(unify(UpdatedCallOutArgs, []), $module, $pred,
        "updating nonexistent arg").
update_call_args(_ModuleInfo, _VarTypes, [], [_ | _], _, _) :-
    unexpected($module, $pred, "mismatched lists").
update_call_args(_ModuleInfo, _VarTypes, [_ | _], [], _, _) :-
    unexpected($module, $pred, "mismatched lists").
update_call_args(ModuleInfo, VarTypes, [CalleeMode | CalleeModes],
        [Arg | Args], !.UpdatedCallOutArgs, !:UpdatedArgs) :-
    lookup_var_type(VarTypes, Arg, CalleeType),
    mode_to_top_functor_mode(ModuleInfo, CalleeMode, CalleeType,
        TopFunctorMode),
    (
        TopFunctorMode = top_in,
        update_call_args(ModuleInfo, VarTypes, CalleeModes, Args,
            !.UpdatedCallOutArgs, !:UpdatedArgs),
        !:UpdatedArgs = [Arg | !.UpdatedArgs]
    ;
        TopFunctorMode = top_out,
        (
            !.UpdatedCallOutArgs = [UpdatedArg | !:UpdatedCallOutArgs]
        ;
            !.UpdatedCallOutArgs = [],
            unexpected($module, $pred, "no UpdatedCallOutArgs")
        ),
        update_call_args(ModuleInfo, VarTypes, CalleeModes, Args,
            !.UpdatedCallOutArgs, !:UpdatedArgs),
        !:UpdatedArgs = [UpdatedArg | !.UpdatedArgs]
    ;
        TopFunctorMode = top_unused,
        unexpected($module, $pred, "top_unused")
    ).

%-----------------------------------------------------------------------------%

:- pred classify_proc_call_args(module_info::in, vartypes::in,
    list(prog_var)::in, list(mer_mode)::in,
    list(prog_var)::out, list(prog_var)::out, list(prog_var)::out) is det.

classify_proc_call_args(_ModuleInfo, _VarTypes, [], [], [], [], []).
classify_proc_call_args(_ModuleInfo, _VarTypes, [], [_ | _], _, _, _) :-
    unexpected($module, $pred, "mismatched lists").
classify_proc_call_args(_ModuleInfo, _VarTypes, [_ | _], [], _, _, _) :-
    unexpected($module, $pred, "mismatched lists").
classify_proc_call_args(ModuleInfo, VarTypes, [Arg | Args],
        [CalleeMode | CalleeModes], !:InArgs, !:OutArgs, !:UnusedArgs) :-
    classify_proc_call_args(ModuleInfo, VarTypes, Args, CalleeModes,
        !:InArgs, !:OutArgs, !:UnusedArgs),
    lookup_var_type(VarTypes, Arg, CalleeType),
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

%-----------------------------------------------------------------------------%

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
        lookup_var_type(!.Info ^ lco_var_types, CallArg, CallArgType),
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
    VarSet0 = !.Info ^ lco_var_set,
    VarTypes0 = !.Info ^ lco_var_types,
    varset.lookup_name(VarSet0, Var, "SCCcallarg", Name),
    AddrName = "Addr" ++ Name,
    varset.new_named_var(AddrName, AddrVar, VarSet0, VarSet),
    HighLevelData = ConstInfo ^ lci_highlevel_data,
    (
        HighLevelData = no,
        lookup_var_type(VarTypes0, Var, FieldType),
        AddrVarType = make_ref_type(FieldType),
        add_var_type(AddrVar, AddrVarType, VarTypes0, VarTypes)
    ;
        HighLevelData = yes,
        % We set the type later when it is more convenient.
        VarTypes = VarTypes0
    ),
    !Info ^ lco_var_set := VarSet,
    !Info ^ lco_var_types := VarTypes.

:- func make_ref_type(mer_type) = mer_type.

make_ref_type(FieldType) = PtrType :-
    RefTypeName = qualified(mercury_private_builtin_module,
        "store_at_ref_type"),
    PtrType = defined_type(RefTypeName, [FieldType], kind_star).

%-----------------------------------------------------------------------------%

:- pred make_variant_args(bool::in, map(prog_var, field_id)::in,
    assoc_list(int, prog_var)::in, list(variant_arg)::out) is det.

make_variant_args(HighLevelData, AddrVarFieldIds, Mismatches, VariantArgs) :-
    (
        HighLevelData = no,
        MakeArg = (func(Pos - _Var) = variant_arg(Pos, no))
    ;
        HighLevelData = yes,
        MakeArg = (func(Pos - Var) = variant_arg(Pos, yes(FieldId)) :-
            map.lookup(AddrVarFieldIds, Var, FieldId)
        )
    ),
    VariantArgs = list.map(MakeArg, Mismatches).

:- pred ensure_variant_exists(pred_id::in, proc_id::in, list(variant_arg)::in,
    pred_proc_id::out, sym_name::in, sym_name::out,
    lco_info::in, lco_info::out) is semidet.

ensure_variant_exists(PredId, ProcId, AddrArgNums, VariantPredProcId,
        SymName, VariantSymName, !Info) :-
    PredProcId = proc(PredId, ProcId),
    CurSCCVariants0 = !.Info ^ lco_cur_scc_variants,
    ( if
        multi_map.search(CurSCCVariants0, PredProcId, ExistingVariants),
        match_existing_variant(ExistingVariants, AddrArgNums, ExistingVariant)
    then
        get_variant_id_and_name(ExistingVariant, SymName, VariantPredProcId,
            VariantSymName)
    else
        ModuleInfo0 = !.Info ^ lco_module_info,
        clone_pred_proc(PredId, ClonePredId, PredOrFunc,
            ModuleInfo0, ModuleInfo),
        VariantPredProcId = proc(ClonePredId, ProcId),
        !Info ^ lco_module_info := ModuleInfo,
        ( if
            multi_map.search(CurSCCVariants0, PredProcId, ExistingVariants)
        then
            VariantNumber = list.length(ExistingVariants) + 1
        else
            VariantNumber = 1
        ),
        VariantNumber =< max_variants_per_proc,
        (
            SymName = unqualified(Name),
            create_variant_name(PredOrFunc, VariantNumber, Name, VariantName),
            VariantSymName = unqualified(VariantName)
        ;
            SymName = qualified(ModuleName, Name),
            create_variant_name(PredOrFunc, VariantNumber, Name, VariantName),
            VariantSymName = qualified(ModuleName, VariantName)
        ),
        NewVariant = variant_id(AddrArgNums, VariantPredProcId, VariantName),
        multi_map.set(PredProcId, NewVariant, CurSCCVariants0, CurSCCVariants),
        !Info ^ lco_cur_scc_variants := CurSCCVariants
    ).

:- pred match_existing_variant(list(variant_id)::in, list(variant_arg)::in,
    variant_id::out) is semidet.

match_existing_variant([Variant0 | Variants], AddrArgNums, Variant) :-
    ( if Variant0 = variant_id(AddrArgNums, _, _) then
        Variant = Variant0
    else
        match_existing_variant(Variants, AddrArgNums, Variant)
    ).

:- pred get_variant_id_and_name(variant_id::in, sym_name::in,
    pred_proc_id::out, sym_name::out) is det.

get_variant_id_and_name(VariantId, SymName, PredProcId, VariantSymName) :-
    VariantId = variant_id(_, PredProcId, VariantName),
    (
        SymName = unqualified(_Name),
        VariantSymName = unqualified(VariantName)
    ;
        SymName = qualified(ModuleName, _Name),
        VariantSymName = qualified(ModuleName, VariantName)
    ).

:- func max_variants_per_proc = int.

max_variants_per_proc = 4.

:- pred clone_pred_proc(pred_id::in, pred_id::out, pred_or_func::out,
    module_info::in, module_info::out) is det.

clone_pred_proc(PredId, ClonePredId, PredOrFunc, !ModuleInfo) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    module_info_get_predicate_table(!.ModuleInfo, PredTable0),
    predicate_table_insert(PredInfo, ClonePredId, PredTable0, PredTable),
    module_info_set_predicate_table(PredTable, !ModuleInfo).

:- pred create_variant_name(pred_or_func::in, int::in, string::in,
    string::out) is det.

create_variant_name(PredOrFunc, VariantNumber, OrigName, VariantName) :-
    (
        PredOrFunc = pf_function,
        Prefix = "LCMCfn_"
    ;
        PredOrFunc = pf_predicate,
        Prefix = "LCMCpr_"
    ),
    VariantName = Prefix ++ OrigName ++ "_" ++ int_to_string(VariantNumber).

%-----------------------------------------------------------------------------%

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
        VarTypes0 = !.Info ^ lco_var_types,
        lookup_var_type(VarTypes0, Var, VarType),
        InstMapDelta0 = goal_info_get_instmap_delta(GoalInfo0),
        update_construct_args(Subst, HighLevelData, VarType, ConsId, 1,
            ArgVars, UpdatedArgVars, AddrFields, InstMapDelta0, InstMapDelta,
            !AddrVarFieldIds, VarTypes0, VarTypes),
        !Info ^ lco_var_types := VarTypes,
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
                unexpected($module, $pred, "var RHS")
            ;
                RHS0 = rhs_functor(RHSConsId, IsExistConstr, RHSVars0),
                expect(unify(ConsId, RHSConsId), $module,
                    "update_construct: cons_id mismatch"),
                rename_var_list(need_not_rename, Subst, RHSVars0, RHSVars),
                RHS = rhs_functor(RHSConsId, IsExistConstr, RHSVars)
            ;
                RHS0 = rhs_lambda_goal(_, _, _, _, _, _, _, _, _),
                unexpected($module, $pred, "lambda RHS")
            ),
            GoalExpr = unify(LHS, RHS, Mode, Unification, UnifyContext),

            % For high-level data, there is a lie in this instmap_delta: the
            % new cell is not yet ground, although it will become ground after
            % the call that follows the construction.
            goal_info_set_instmap_delta(InstMapDelta, GoalInfo0, GoalInfo),
            Goal = hlds_goal(GoalExpr, GoalInfo)
        )
    else
        unexpected($module, $pred, "not construct")
    ).

:- pred update_construct_args(map(prog_var, prog_var)::in, bool::in,
    mer_type::in, cons_id::in, int::in, list(prog_var)::in,
    list(prog_var)::out, list(int)::out, instmap_delta::in, instmap_delta::out,
    map(prog_var, field_id)::in, map(prog_var, field_id)::out,
    vartypes::in, vartypes::out) is det.

update_construct_args(_, _, _, _, _, [], [], [],
        !InstMapDelta, !AddrFieldIds, !VarTypes).
update_construct_args(Subst, HighLevelData, VarType, ConsId, ArgNum,
        [OrigVar | OrigVars], [UpdatedVar | UpdatedVars], AddrArgs,
        !InstMapDelta, !AddrFieldIds, !VarTypes) :-
    update_construct_args(Subst, HighLevelData, VarType, ConsId, ArgNum + 1,
        OrigVars, UpdatedVars, AddrArgsTail, !InstMapDelta, !AddrFieldIds,
        !VarTypes),
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
            add_var_type(AddrVar, VarType, !VarTypes)
        ),
        instmap_delta_set_var(AddrVar, FinalInst, !InstMapDelta),
        map.det_insert(OrigVar, field_id(VarType, ConsId, ArgNum),
            !AddrFieldIds),
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

%-----------------------------------------------------------------------------%

:- pred acceptable_construct_mode(module_info::in, unify_mode::in) is semidet.

acceptable_construct_mode(ModuleInfo, UnifyMode) :-
    UnifyMode = unify_modes_lhs_rhs(
        from_to_insts(InitInstX, FinalInstX),
        from_to_insts(InitInstY, FinalInstY)),
    inst_is_free(ModuleInfo, InitInstX),
    inst_is_ground(ModuleInfo, InitInstY),
    inst_is_ground(ModuleInfo, FinalInstX),
    inst_is_ground(ModuleInfo, FinalInstY).

:- pred occurs_once(bag(prog_var)::in, prog_var::in) is semidet.

occurs_once(Bag, Var) :-
    bag.count_value(Bag, Var, 1).

%-----------------------------------------------------------------------------%

:- pred lco_transform_variant_proc(variant_map::in, list(variant_arg)::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out) is det.

lco_transform_variant_proc(VariantMap, AddrOutArgs, ProcInfo,
        !:VariantProcInfo, !ModuleInfo) :-
    !:VariantProcInfo = ProcInfo,
    proc_info_get_varset(ProcInfo, VarSet0),
    proc_info_get_vartypes(ProcInfo, VarTypes0),
    proc_info_get_headvars(ProcInfo, HeadVars0),
    proc_info_get_argmodes(ProcInfo, ArgModes0),
    make_addr_vars(HeadVars0, ArgModes0, HeadVars, ArgModes,
        AddrOutArgs, 1, !.ModuleInfo, VarToAddr,
        VarSet0, VarSet, VarTypes0, VarTypes),
    proc_info_set_headvars(HeadVars, !VariantProcInfo),
    proc_info_set_argmodes(ArgModes, !VariantProcInfo),
    proc_info_set_varset(VarSet, !VariantProcInfo),
    proc_info_set_vartypes(VarTypes, !VariantProcInfo),

    proc_info_get_initial_instmap(ProcInfo, !.ModuleInfo, InstMap0),
    proc_info_get_goal(ProcInfo, Goal0),
    lco_transform_variant_goal(!.ModuleInfo, VariantMap, VarToAddr, InstMap0,
        Goal0, Goal, _Changed, !VariantProcInfo),
    proc_info_set_goal(Goal, !VariantProcInfo),
    % We changed the scopes of the headvars we now return via pointers.
    requantify_proc_general(ordinary_nonlocals_no_lambda, !VariantProcInfo),

    % The high-level data transformation requires instmap deltas to be
    % recomputed.
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, highlevel_data, HighLevelData),
    (
        HighLevelData = yes,
        recompute_instmap_delta_proc(do_not_recompute_atomic_instmap_deltas,
            !VariantProcInfo, !ModuleInfo)
    ;
        HighLevelData = no
    ).

:- pred make_addr_vars(list(prog_var)::in, list(mer_mode)::in,
    list(prog_var)::out, list(mer_mode)::out, list(variant_arg)::in,
    int::in, module_info::in, var_to_target::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

make_addr_vars([], [], [], [], AddrOutArgs, _, _, [],
        !VarSet, !VarTypes) :-
    expect(unify(AddrOutArgs, []), $module,
        "make_addr_vars: AddrOutArgs != []").
make_addr_vars([], [_ | _], _, _, _, _, _, _, !VarSet, !VarTypes) :-
    unexpected($module, $pred, "mismatched lists").
make_addr_vars([_ | _], [], _, _, _, _, _, _, !VarSet, !VarTypes) :-
    unexpected($module, $pred, "mismatched lists").
make_addr_vars([HeadVar0 | HeadVars0], [Mode0 | Modes0],
        [HeadVar | HeadVars], [Mode | Modes], !.AddrOutArgs,
        NextOutArgNum, ModuleInfo, VarToAddr, !VarSet, !VarTypes) :-
    lookup_var_type(!.VarTypes, HeadVar0, HeadVarType),
    mode_to_top_functor_mode(ModuleInfo, Mode0, HeadVarType, TopFunctorMode),
    (
        TopFunctorMode = top_in,
        HeadVar = HeadVar0,
        Mode = Mode0,
        make_addr_vars(HeadVars0, Modes0, HeadVars, Modes, !.AddrOutArgs,
            NextOutArgNum, ModuleInfo, VarToAddr, !VarSet, !VarTypes)
    ;
        TopFunctorMode = top_out,
        ( if
            !.AddrOutArgs = [AddrOutArg | !:AddrOutArgs],
            AddrOutArg = variant_arg(NextOutArgNum, MaybeFieldId)
        then
            varset.lookup_name(!.VarSet, HeadVar0, Name),
            AddrName = "AddrOf" ++ Name,
            varset.new_named_var(AddrName, AddrVar, !VarSet),
            HeadVar = AddrVar,
            lookup_var_type(!.VarTypes, HeadVar0, OldType),
            (
                MaybeFieldId = no,
                % For low-level data we replace the output argument with a
                % store_at_ref_type(T) input argument.
                add_var_type(AddrVar, make_ref_type(OldType), !VarTypes),
                Mode = in_mode
            ;
                MaybeFieldId = yes(field_id(AddrVarType, ConsId, ArgNum)),
                % For high-level data we replace the output argument with a
                % partially instantiated structure. The structure has one
                % argument left unfilled.
                add_var_type(AddrVar, AddrVarType, !VarTypes),
                BoundInst = bound_inst_with_free_arg(ConsId, ArgNum),
                InitialInst = bound(shared, inst_test_no_results, [BoundInst]),
                Mode = from_to_mode(InitialInst, ground_inst)
            ),
            make_addr_vars(HeadVars0, Modes0, HeadVars, Modes,
                !.AddrOutArgs, NextOutArgNum + 1, ModuleInfo,
                VarToAddrTail, !VarSet, !VarTypes),
            VarToAddrHead = HeadVar0 - store_target(AddrVar, MaybeFieldId),
            VarToAddr = [VarToAddrHead | VarToAddrTail]
        else
            HeadVar = HeadVar0,
            Mode = Mode0,
            make_addr_vars(HeadVars0, Modes0, HeadVars, Modes,
                !.AddrOutArgs, NextOutArgNum + 1, ModuleInfo,
                VarToAddr, !VarSet, !VarTypes)
        )
    ;
        TopFunctorMode = top_unused,
        unexpected($module, $pred, "top_unused")
    ).

:- pred lco_transform_variant_goal(module_info::in, variant_map::in,
    var_to_target::in, instmap::in, hlds_goal::in, hlds_goal::out, bool::out,
    proc_info::in, proc_info::out) is det.

lco_transform_variant_goal(ModuleInfo, VariantMap, VarToAddr, InstMap0,
        Goal0, Goal, Changed, !ProcInfo) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = conj(ConjType, Goals0),
        (
            ConjType = plain_conj,
            lco_transform_variant_conj(ModuleInfo, VariantMap, VarToAddr,
                InstMap0, Goals0, Goals, Changed, !ProcInfo),
            GoalExpr = conj(ConjType, Goals),
            GoalInfo = GoalInfo0
        ;
            ConjType = parallel_conj,
            unexpected($module, $pred, "parallel_conj")
        )
    ;
        GoalExpr0 = disj(Goals0),
        list.map2_foldl(
            lco_transform_variant_goal(ModuleInfo, VariantMap, VarToAddr,
                InstMap0),
            Goals0, Goals, DisjsChanged, !ProcInfo),
        Changed = bool.or_list(DisjsChanged),
        GoalExpr = disj(Goals),
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        list.map2_foldl(
            lco_transform_variant_case(ModuleInfo, VariantMap, VarToAddr,
                InstMap0),
            Cases0, Cases, CasesChanged, !ProcInfo),
        Changed = bool.or_list(CasesChanged),
        GoalExpr = switch(Var, CanFail, Cases),
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = if_then_else(Vars, Cond, Then0, Else0),
        update_instmap(Cond, InstMap0, InstMap1),
        lco_transform_variant_goal(ModuleInfo, VariantMap, VarToAddr, InstMap1,
            Then0, Then, ThenChanged, !ProcInfo),
        lco_transform_variant_goal(ModuleInfo, VariantMap, VarToAddr, InstMap0,
            Else0, Else, ElseChanged, !ProcInfo),
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
            lco_transform_variant_goal(ModuleInfo, VariantMap, VarToAddr,
                InstMap0, SubGoal0, SubGoal, Changed, !ProcInfo),
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
        lco_transform_variant_atomic_goal(ModuleInfo, VarToAddr, InstMap0,
            GoalInfo0, GoalExpr0, GoalExpr, Changed, !ProcInfo),
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = plain_call(_, _, _, _, _, _),
        lco_transform_variant_plain_call(ModuleInfo, VariantMap, VarToAddr,
            InstMap0, GoalExpr0, GoalExpr, GoalInfo0, GoalInfo, Changed,
            !ProcInfo)
    ;
        GoalExpr0 = unify(_, _, _, _, _),
        lco_transform_variant_atomic_goal(ModuleInfo, VarToAddr, InstMap0,
            GoalInfo0, GoalExpr0, GoalExpr, Changed, !ProcInfo),
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = call_foreign_proc(_, _, _, _,  _, _, _),
        lco_transform_variant_atomic_goal(ModuleInfo, VarToAddr, InstMap0,
            GoalInfo0, GoalExpr0, GoalExpr, Changed, !ProcInfo),
        GoalInfo = GoalInfo0
    ;
        GoalExpr0 = shorthand(_),
        % These should have been expanded out by now.
        unexpected($module, $pred, "shorthand")
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

:- pred lco_transform_variant_conj(module_info::in, variant_map::in,
    var_to_target::in, instmap::in, list(hlds_goal)::in, list(hlds_goal)::out,
    bool::out, proc_info::in, proc_info::out) is det.

lco_transform_variant_conj(_, _, _, _, [], [], no, !ProcInfo).
lco_transform_variant_conj(ModuleInfo, VariantMap, VarToAddr, InstMap0,
        [Goal0 | Goals0], Conj, Changed, !ProcInfo) :-
    lco_transform_variant_goal(ModuleInfo, VariantMap, VarToAddr, InstMap0,
        Goal0, Goal, HeadChanged, !ProcInfo),
    update_instmap(Goal0, InstMap0, InstMap1),
    lco_transform_variant_conj(ModuleInfo, VariantMap, VarToAddr, InstMap1,
        Goals0, Goals, TailChanged, !ProcInfo),
    Changed = bool.or(HeadChanged, TailChanged),
    ( if Goal = hlds_goal(conj(plain_conj, SubConj), _) then
        Conj = SubConj ++ Goals
    else
        Conj = [Goal | Goals]
    ).

:- pred lco_transform_variant_case(module_info::in, variant_map::in,
    var_to_target::in, instmap::in, case::in, case::out, bool::out,
    proc_info::in, proc_info::out) is det.

lco_transform_variant_case(ModuleInfo, VariantMap, VarToAddr, InstMap0,
        Case0, Case, Changed, !ProcInfo) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    lco_transform_variant_goal(ModuleInfo, VariantMap, VarToAddr, InstMap0,
        Goal0, Goal, Changed, !ProcInfo),
    Case = case(MainConsId, OtherConsIds, Goal).

:- pred lco_transform_variant_atomic_goal(module_info::in,
    var_to_target::in, instmap::in, hlds_goal_info::in,
    hlds_goal_expr::in, hlds_goal_expr::out, bool::out,
    proc_info::in, proc_info::out) is det.

lco_transform_variant_atomic_goal(ModuleInfo, VarToAddr, InstMap0,
        GoalInfo, GoalExpr0, GoalExpr, Changed, !ProcInfo) :-
    update_instmap(hlds_goal(GoalExpr0, GoalInfo), InstMap0, InstMap1),
    list.filter(is_grounding(ModuleInfo, InstMap0, InstMap1), VarToAddr,
        GroundingVarToAddr),
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

:- pred lco_transform_variant_plain_call(module_info::in, variant_map::in,
    var_to_target::in, instmap::in,
    hlds_goal_expr::in(goal_expr_plain_call), hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out, bool::out,
    proc_info::in, proc_info::out) is det.

lco_transform_variant_plain_call(ModuleInfo, VariantMap, VarToAddr, InstMap0,
        GoalExpr0, GoalExpr, GoalInfo0, GoalInfo, Changed, !ProcInfo) :-
    update_instmap(hlds_goal(GoalExpr0, GoalInfo0), InstMap0, InstMap1),
    list.filter(is_grounding(ModuleInfo, InstMap0, InstMap1), VarToAddr,
        GroundingVarToAddr),
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
        GoalExpr0 = plain_call(CallPredId, CallProcId, Args, Builtin,
            UnifyContext, SymName),
        CallPredProcId = proc(CallPredId, CallProcId),
        module_info_proc_info(ModuleInfo, CallPredId, CallProcId,
            CalleeProcInfo),
        proc_info_get_vartypes(!.ProcInfo, VarTypes),
        proc_info_get_argmodes(CalleeProcInfo, CalleeArgModes),
        ( if
            multi_map.search(VariantMap, CallPredProcId, ExistingVariants),
            classify_proc_call_args(ModuleInfo, VarTypes, Args, CalleeArgModes,
                _InArgs, OutArgs, _UnusedArgs),
            grounding_to_variant_args(GroundingVarToAddr, 1, OutArgs, Subst,
                VariantArgs),
            match_existing_variant(ExistingVariants, VariantArgs,
                ExistingVariant)
        then
            rename_var_list(need_not_rename, Subst, Args, CallArgs),
            get_variant_id_and_name(ExistingVariant, SymName,
                proc(VariantPredId, VariantProcId), VariantSymName),
            GoalExpr = plain_call(VariantPredId, VariantProcId, CallArgs,
                Builtin, UnifyContext, VariantSymName),

            module_info_get_globals(ModuleInfo, Globals),
            globals.lookup_bool_option(Globals, highlevel_data, HighLevelData),
            (
                HighLevelData = no,
                GoalInfo = GoalInfo0
            ;
                HighLevelData = yes,
                % The partially instantiated cells will be ground after the
                % call.
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

:- pred is_grounding(module_info::in, instmap::in, instmap::in,
    pair(prog_var, store_target)::in) is semidet.

is_grounding(ModuleInfo, InstMap0, InstMap, Var - _StoreTarget) :-
    instmap_lookup_var(InstMap0, Var, Inst0),
    not inst_is_ground(ModuleInfo, Inst0),
    instmap_is_reachable(InstMap),
    instmap_lookup_var(InstMap, Var, Inst),
    inst_is_ground(ModuleInfo, Inst).

:- pred grounding_to_variant_args(assoc_list(prog_var, store_target)::in,
    int::in, list(prog_var)::in, prog_var_renaming::out,
    list(variant_arg)::out) is det.

grounding_to_variant_args(GroundingVarToAddr, OutArgNum, OutArgs, Subst,
        VariantArgs) :-
    (
        OutArgs = [],
        Subst = map.init,
        VariantArgs = []
    ;
        OutArgs = [OutArg | OutArgsTail],
        grounding_to_variant_args(GroundingVarToAddr, OutArgNum + 1,
            OutArgsTail, Subst0, VariantArgsTail),
        ( if assoc_list.search(GroundingVarToAddr, OutArg, StoreTarget) then
            StoreTarget = store_target(StoreArg, MaybeFieldId),
            map.det_insert(OutArg, StoreArg, Subst0, Subst),
            VariantArg = variant_arg(OutArgNum, MaybeFieldId),
            VariantArgs = [VariantArg | VariantArgsTail]
        else
            Subst = Subst0,
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
        generate_simple_call(mercury_private_builtin_module,
            "store_at_ref_impure",
            pf_predicate, only_mode, detism_det, purity_impure,
            [AddrVar, GroundVar],
            [], instmap_delta_bind_vars([]), ModuleInfo, term.context_init,
            Goal)
    ;
        % High-level data.
        MaybeFieldId = yes(field_id(AddrVarType, ConsId, ArgNum)),
        get_cons_id_arg_types(ModuleInfo, AddrVarType, ConsId, ArgTypes),
        make_unification_args(GroundVar, ArgNum, 1, ArgTypes,
            ArgVars, ArgModes, !ProcInfo),

        RHS = rhs_functor(ConsId, is_not_exist_constr, ArgVars),

        instmap_lookup_var(InstMap, AddrVar, AddrVarInst0),
        inst_expand(ModuleInfo, AddrVarInst0, AddrVarInst),
        UnifyMode = unify_modes_lhs_rhs(
            from_to_insts(AddrVarInst, ground_inst),
            from_to_insts(ground_inst, ground_inst)),

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

:- pred make_unification_args(prog_var::in, int::in, int::in,
    list(mer_type)::in, list(prog_var)::out, list(unify_mode)::out,
    proc_info::in, proc_info::out) is det.

make_unification_args(GroundVar, TargetArgNum, CurArgNum, ArgTypes,
        ArgVars, ArgModes, !ProcInfo) :-
    (
        ArgTypes = [],
        ArgVars = [],
        ArgModes = []
    ;
        ArgTypes = [ArgType | ArgTypesTail],
        make_unification_args(GroundVar, TargetArgNum, CurArgNum + 1,
            ArgTypesTail, ArgVarsTail, ArgModesTail, !ProcInfo),
        make_unification_arg(GroundVar, TargetArgNum, CurArgNum,
            ArgType, Var, ArgMode, !ProcInfo),
        ArgVars = [Var | ArgVarsTail],
        ArgModes = [ArgMode | ArgModesTail]
    ).

:- pred make_unification_arg(prog_var::in, int::in, int::in, mer_type::in,
    prog_var::out, unify_mode::out, proc_info::in, proc_info::out) is det.

make_unification_arg(GroundVar, TargetArgNum, CurArgNum, ArgType,
        Var, UnifyMode, !ProcInfo) :-
    ( if CurArgNum = TargetArgNum then
        Var = GroundVar,
        UnifyMode = unify_modes_lhs_rhs(
            from_to_insts(free_inst, ground_inst),
            from_to_insts(ground_inst, ground_inst))
    else
        % Bind other arguments to fresh variables.
        proc_info_create_var_from_type(ArgType, no, Var, !ProcInfo),
        UnifyMode = unify_modes_lhs_rhs(
            from_to_insts(ground_inst, ground_inst),
            from_to_insts(free_inst, ground_inst))
    ).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.lco.
%-----------------------------------------------------------------------------%
