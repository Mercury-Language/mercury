%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: lco.m.
% Author: zs.
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
% Concrete example of what the original predicate and its return-via-memory
% variant should look like for append:
% 
% app(list(T)::in, list(T)::in, list(T)::out)
% app(A, B, C) :-
%   (
%       A == [],
%       C := B
%   ;
%       A => [H | T],
%       C <= [H | _NT] capture &HT in AddrHT
%       app'(T, B, AddrHT)
%   )
% 
% app'(list(T)::in, list(T)::in, store_by_ref_type(T)::in)
% app'(A, B, AddrC) :-
%   (
%       A == [],
%       C := B,
%       store_at_ref(AddrC, C)
%   ;
%       A => [H | T],
%       C <= [H | _NT] capture &HT in AddrHT
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
%   store_by_ref_type(T), where T is type of the field pointed to, and
% 
% 4 follows each primitive goal that binds one of the output arguments
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

:- module transform_hlds.lco.
:- interface.

:- import_module hlds.hlds_module.

:- pred lco_modulo_constructors(module_info::in, module_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_match.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.arg_info.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_code_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module hlds.pred_table.
:- import_module hlds.quantification.
:- import_module libs.compiler_util.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_mode.
:- import_module transform_hlds.dependency_graph.

:- import_module assoc_list.
:- import_module bag.
:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module set.
:- import_module std_util.
:- import_module string.
:- import_module svbag.
:- import_module svmap.
:- import_module svvarset.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

:- type variant_id
    --->    variant_id(
                list(int),      % Positions of output arguments returned in
                                % memory.
                pred_proc_id    % The id of the variant.
            ).

:- type variant_map == map(pred_proc_id, variant_id).

:- type permitted
    --->    permitted
    ;       not_permitted.

:- type changed
    --->    changed
    ;       not_changed.

:- type lco_info
    --->    lco_info(
                module_info         :: module_info,
                cur_scc_variants    :: variant_map,
                var_set             :: prog_varset,
                var_types           :: vartypes,
                permitted           :: permitted,
                changed             :: changed
            ).

:- type lco_const_info
    --->    lco_const_info(
                lower_scc_variants  :: variant_map,
                cur_scc             :: set(pred_proc_id),
                cur_proc_id         :: pred_proc_id,
                cur_proc_pred       :: pred_info,
                cur_proc_proc       :: proc_info,
                cur_proc_outputs    :: list(prog_var),
                cur_proc_detism     :: determinism
            ).

%-----------------------------------------------------------------------------%

lco_modulo_constructors(!ModuleInfo) :-
    module_info_rebuild_dependency_info(!ModuleInfo, DepInfo),
    hlds_dependency_info_get_dependency_ordering(DepInfo, SCCs),
    list.foldl2(lco_scc, SCCs, map.init, _, !ModuleInfo).

:- pred lco_scc(list(pred_proc_id)::in, variant_map::in, variant_map::out,
    module_info::in, module_info::out) is det.

lco_scc(SCC, !VariantMap, !ModuleInfo) :-
    ModuleInfo0 = !.ModuleInfo,
    list.foldl4(lco_proc(!.VariantMap, SCC), SCC, !ModuleInfo,
        map.init, CurSCCVariantMap, map.init, CurSCCUpdateMap,
        permitted, Permitted),
    map.to_assoc_list(CurSCCVariantMap, CurSCCVariants),
    map.to_assoc_list(CurSCCUpdateMap, CurSCCUpdates),
    (
        Permitted = permitted,
        CurSCCUpdates = [_ | _]
    ->
        list.foldl(process_proc_update, CurSCCUpdates, !ModuleInfo),
        list.foldl(process_proc_variant, CurSCCVariants, !ModuleInfo)
    ;
        !:ModuleInfo = ModuleInfo0
    ).

%-----------------------------------------------------------------------------%

:- pred process_proc_update(pair(pred_proc_id, proc_info)::in,
    module_info::in, module_info::out) is det.

process_proc_update(PredProcId - NewProcInfo, !ModuleInfo) :-
    PredProcId = proc(PredId, ProcId),

    module_info_preds(!.ModuleInfo, Preds0),
    map.lookup(Preds0, PredId, PredInfo0),
    pred_info_procedures(PredInfo0, Procs0),
    map.det_update(Procs0, ProcId, NewProcInfo, Procs),
    pred_info_set_procedures(Procs, PredInfo0, PredInfo),
    map.det_update(Preds0, PredId, PredInfo, Preds),
    module_info_set_preds(Preds, !ModuleInfo).

:- pred process_proc_variant(pair(pred_proc_id, variant_id)::in,
    module_info::in, module_info::out) is det.

process_proc_variant(PredProcId - VariantId, !ModuleInfo) :-
    VariantId = variant_id(AddrOutArgPosns, VariantPredProcId),
    VariantPredProcId = proc(VariantPredId, VariantProcId),
    PredProcId = proc(PredId, ProcId),

    module_info_pred_proc_info(!.ModuleInfo, PredId, ProcId,
        _PredInfo, ProcInfo),
    transform_variant_proc(!.ModuleInfo, AddrOutArgPosns,
        ProcInfo, VariantProcInfo),

    some [!VariantPredInfo] (
        module_info_preds(!.ModuleInfo, Preds0),
        map.lookup(Preds0, VariantPredId, !:VariantPredInfo),
        Name0 = pred_info_name(!.VariantPredInfo),
        pred_info_get_origin(!.VariantPredInfo, Origin0),
        create_variant_name(AddrOutArgPosns, Name0, Name),
        Transform = return_via_ptr(ProcId, AddrOutArgPosns),
        Origin = transformed(Transform, Origin0, PredId),
        pred_info_set_name(Name, !VariantPredInfo),
        pred_info_set_origin(Origin, !VariantPredInfo),

        % We throw away any other procs in the variant predicate, because
        % we create a separate predicate for each variant.
        map.det_insert(map.init, VariantProcId, VariantProcInfo,
            VariantProcs),
        pred_info_set_procedures(VariantProcs, !VariantPredInfo),
        map.det_update(Preds0, VariantPredId, !.VariantPredInfo, Preds),
        module_info_set_preds(Preds, !ModuleInfo)
    ).

:- pred create_variant_name(list(int)::in, string::in, string::out) is det.

create_variant_name([], !Name) :-
    !:Name = "LCMC_" ++ !.Name.
create_variant_name([ArgPos | ArgPoss], !Name) :-
    !:Name = !.Name ++ "_" ++ int_to_string(ArgPos),
    create_variant_name(ArgPoss, !Name).

%-----------------------------------------------------------------------------%

:- pred lco_proc(variant_map::in, list(pred_proc_id)::in,
    pred_proc_id::in, module_info::in, module_info::out,
    variant_map::in, variant_map::out,
    map(pred_proc_id, proc_info)::in, map(pred_proc_id, proc_info)::out,
    permitted::in, permitted::out) is det.

lco_proc(LowerSCCVariants, SCC, CurProc, !ModuleInfo, !CurSCCVariants,
        !CurSCCUpdates, !Permitted) :-
    (
        !.Permitted = not_permitted
    ;
        !.Permitted = permitted,
        CurProc = proc(PredId, ProcId),
        module_info_pred_proc_info(!.ModuleInfo, PredId, ProcId,
            PredInfo, ProcInfo0),
        pred_info_import_status(PredInfo, Status),
        status_defined_in_this_module(Status, DefInThisModule),
        proc_info_inferred_determinism(ProcInfo0, Detism),
        (
            ( DefInThisModule = no
            ; not acceptable_detism_for_lco(Detism)
            )
        ->
            !:Permitted = not_permitted
        ;
            proc_info_varset(ProcInfo0, VarSet0),
            proc_info_vartypes(ProcInfo0, VarTypes0),
            proc_info_headvars(ProcInfo0, HeadVars),
            proc_info_argmodes(ProcInfo0, ArgModes),
            list.map(map.lookup(VarTypes0), HeadVars, ArgTypes),
            arg_info.compute_in_and_out_vars(!.ModuleInfo, HeadVars,
                ArgModes, ArgTypes, _InputHeadVars, OutputHeadVars),
            proc_info_inferred_determinism(ProcInfo0, CurProcDetism),
            ConstInfo = lco_const_info(LowerSCCVariants, list_to_set(SCC),
                CurProc, PredInfo, ProcInfo0, OutputHeadVars, CurProcDetism),
            Info0 = lco_info(!.ModuleInfo, !.CurSCCVariants,
                VarSet0, VarTypes0, permitted, not_changed),
            proc_info_goal(ProcInfo0, Goal0),
            lco_in_goal(Goal0, Goal, Info0, Info, ConstInfo),
            Info = lco_info(!:ModuleInfo, !:CurSCCVariants, VarSet, VarTypes,
                !:Permitted, Changed),
            (
                !.Permitted = permitted,
                Changed = changed
            ->
                some [!ProcInfo] (
                    !:ProcInfo = ProcInfo0,
                    proc_info_set_varset(VarSet, !ProcInfo),
                    proc_info_set_vartypes(VarTypes, !ProcInfo),
                    proc_info_set_goal(Goal, !ProcInfo),
                    requantify_proc(!ProcInfo),
                    svmap.det_insert(CurProc, !.ProcInfo, !CurSCCUpdates)
                )
            ;
                true
            )
        )
    ).

    % Procedures which can succeed more than once can't do proper tail calls,
    % and procedures that cannot succeed at all should not be optimized
    % for time.
    %
:- pred acceptable_detism_for_lco(determinism::in) is semidet.

acceptable_detism_for_lco(det).
acceptable_detism_for_lco(semidet).
acceptable_detism_for_lco(cc_multidet).
acceptable_detism_for_lco(cc_nondet).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred lco_in_goal(hlds_goal::in, hlds_goal::out, lco_info::in, lco_info::out,
    lco_const_info::in) is det.

lco_in_goal(Goal0 - GoalInfo, Goal - GoalInfo, !Info, ConstInfo) :-
    (
        Goal0 = conj(ConjType, Goals0),
        (
            ConjType = plain_conj,
            lco_in_conj(list.reverse(Goals0), [], bag.init, MaybeGoals,
                !Info, ConstInfo),
            (
                MaybeGoals = yes(Goals),
                Goal = conj(plain_conj, Goals)
            ;
                MaybeGoals = no,
                % If the top-level conjunction doesn't end with some
                % unifications we can move before a recursive call,
                % maybe it ends with a switch or if-then-else, some of whose
                % arms fit that pattern.
                ( list.split_last(Goals0, AllButLast, Last0) ->
                    lco_in_goal(Last0, Last, !Info, ConstInfo),
                    Goal = conj(plain_conj, AllButLast ++ [Last])
                ;
                    Goal = Goal0
                )
            )
        ;
            ConjType = parallel_conj,
            Goal = Goal0,
            !:Info = !.Info ^ permitted := not_permitted
        )
    ;
        Goal0 = disj(Goals0),
        % There is no point in looking for tail calls in the non-last
        % disjuncts.
        ( list.split_last(Goals0, AllButLast, Last0) ->
            lco_in_goal(Last0, Last, !Info, ConstInfo),
            Goal = disj(AllButLast ++ [Last])
        ;
            Goal = Goal0
        )
    ;
        Goal0 = switch(Var, CanFail, Cases0),
        lco_in_cases(Cases0, Cases, !Info, ConstInfo),
        Goal = switch(Var, CanFail, Cases)
    ;
        Goal0 = if_then_else(Vars, Cond, Then0, Else0),
        lco_in_goal(Then0, Then, !Info, ConstInfo),
        lco_in_goal(Else0, Else, !Info, ConstInfo),
        Goal = if_then_else(Vars, Cond, Then, Else)
    ;
        Goal0 = scope(Reason, SubGoal0),
        lco_in_goal(SubGoal0, SubGoal, !Info, ConstInfo),
        Goal = scope(Reason, SubGoal)
    ;
        Goal0 = not(_),
        Goal = Goal0
    ;
        Goal0 = generic_call(_, _, _, _),
        Goal = Goal0
    ;
        Goal0 = call(_, _, _, _, _, _),
        Goal = Goal0
    ;
        Goal0 = unify(_, _, _, _, _),
        Goal = Goal0
    ;
        Goal0 = foreign_proc(_, _,_,  _, _, _),
        Goal = Goal0
    ;
        Goal0 = shorthand(_),
        unexpected(this_file, "lco_in_goal: shorthand")
    ).

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
lco_in_cases([case(Cons, Goal0) | Cases0], [case(Cons, Goal) | Cases],
        !Info, ConstInfo) :-
    lco_in_goal(Goal0, Goal, !Info, ConstInfo),
    lco_in_cases(Cases0, Cases, !Info, ConstInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % lco_in_conj(RevGoals, Unifies, ModuleInfo, Goals)
    %
    % Given a conjunction whose structure is:
    %
    %   zero or more arbitrary goals
    %   recursive call that could be a last call modulo constructors
    %   one or more construction unifications
    %
    % move the construction unifications before the call.
    %
    % We traverse the conjunction backwards (the caller has reversed the list).
    % RevGoals is the list of remaining goals in the reversed conjunction list.
    % RevUnifies is the list of assignments and constructions delayed by any
    % previous recursive invocations of lco_in_conj.
    %
    % invariant: append(reverse(RevGoals), Unifies) = original conjunction
    %
:- pred lco_in_conj(list(hlds_goal)::in, list(hlds_goal)::in,
    bag(prog_var)::in, maybe(list(hlds_goal))::out,
    lco_info::in, lco_info::out, lco_const_info::in) is det.

lco_in_conj([], _Unifies, _UnifyInputVars, no, !Info, _ConstInfo).
lco_in_conj([RevGoal | RevGoals], !.Unifies, !.UnifyInputVars, MaybeGoals,
        !Info, ConstInfo) :-
    RevGoal = RevGoalExpr - RevGoalInfo,
    ModuleInfo = !.Info ^ module_info,
    ProcInfo = ConstInfo ^ cur_proc_proc,
    proc_info_vartypes(ProcInfo, VarTypes),
    (
        RevGoalExpr = unify(_, _, _, Unification, _),
        Unification = construct(ConstructedVar, ConsId, ConstructArgs,
            ArgUniModes, _, _, SubInfo),
        (
            SubInfo = no_construct_sub_info
        ;
            SubInfo = construct_sub_info(no, _)
        ),
        all_true(acceptable_construct_mode(ModuleInfo), ArgUniModes),
        map.lookup(VarTypes, ConstructedVar, ConstructedType),
        ConsTag = cons_id_to_tag(ConsId, ConstructedType, ModuleInfo),
        % The code generator can't handle the other tags. For example, it
        % doesn't make sense to the address of the field of a function symbol
        % of a `notag' type.
        (
            ConsTag = unshared_tag(_)
        ;
            ConsTag = shared_remote_tag(_, _)
        )
    ->
        svbag.delete(ConstructedVar, !UnifyInputVars),
        svbag.insert_list(ConstructArgs, !UnifyInputVars),
        !:Unifies = [RevGoal | !.Unifies],
        lco_in_conj(RevGoals, !.Unifies, !.UnifyInputVars, MaybeGoals,
            !Info, ConstInfo)
    ;
        RevGoalExpr = call(PredId, ProcId, Args, Builtin, UnifyContext,
            SymName),
        set.member(proc(PredId, ProcId), ConstInfo ^ cur_scc),
        goal_info_get_determinism(RevGoalInfo, RevGoalDetism),
        RevGoalDetism = ConstInfo ^ cur_proc_detism,

        module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
            _CalleePredInfo, CalleeProcInfo),
        proc_info_argmodes(CalleeProcInfo, CalleeArgModes),
        classify_proc_call_args(ModuleInfo, VarTypes, Args, CalleeArgModes,
            _InArgs, OutArgs, UnusedArgs),
        UnusedArgs = [],
        list.length(OutArgs, NumOutArgs),
        CurrProcOutArgs = ConstInfo ^ cur_proc_outputs,
        list.length(CurrProcOutArgs, NumCurrProcOutArgs),
        NumOutArgs = NumCurrProcOutArgs,

        assoc_list.from_corresponding_lists(OutArgs, CurrProcOutArgs,
            CallHeadPairs),
        find_args_to_pass_by_addr(CallHeadPairs, 1, Mismatches,
            UpdatedCallOutArgs, map.init, Subst, !Info),
        % If there are no mismatches, we would create an identical "variant".
        % Such cases should be optimized using other means.
        Mismatches = [_ | _],
        assoc_list.values(Mismatches, MismatchedCallArgs),
        % The variants we create return each output in only one place in
        % memory.
        all_true(occurs_once(!.UnifyInputVars), MismatchedCallArgs),
        ensure_variant_exists(PredId, ProcId, assoc_list.keys(Mismatches),
            VariantPredProcId, !Info)
    ->
        list.map(update_construct(Subst), !.Unifies, UpdatedUnifies),
        proc_info_argmodes(CalleeProcInfo, CalleeModes),
        update_call_args(ModuleInfo, VarTypes, CalleeModes, Args,
            UpdatedCallOutArgs, UpdatedArgs),
        VariantPredProcId = proc(VariantPredId, VariantProcId),
        UpdatedGoalExpr = call(VariantPredId, VariantProcId, UpdatedArgs,
            Builtin, UnifyContext, SymName),
        UpdatedGoalInfo = RevGoalInfo,
        UpdatedGoal = UpdatedGoalExpr - UpdatedGoalInfo,
        Goals = list.reverse(RevGoals) ++ UpdatedUnifies ++ [UpdatedGoal],
        MaybeGoals = yes(Goals),
        !:Info = !.Info ^ changed := changed
    ;
        % The reversed conjunction does not follow the pattern we are looking
        % for, so we cannot optimize it.
        MaybeGoals = no
    ).

:- pred update_call_args(module_info::in, vartypes::in, list(mer_mode)::in,
    list(prog_var)::in, list(prog_var)::in, list(prog_var)::out) is det.

update_call_args(_ModuleInfo, _VarTypes, [], [], UpdatedCallOutArgs, []) :-
    expect(unify(UpdatedCallOutArgs, []), this_file,
        "update_call_args: updating nonexistent arg").
update_call_args(_ModuleInfo, _VarTypes, [], [_ | _], _, _) :-
    unexpected(this_file, "update_call_args: mismatches lists").
update_call_args(_ModuleInfo, _VarTypes, [_ | _], [], _, _) :-
    unexpected(this_file, "update_call_args: mismatches lists").
update_call_args(ModuleInfo, VarTypes, [CalleeMode | CalleeModes],
        [Arg | Args], !.UpdatedCallOutArgs, !:UpdatedArgs) :-
    map.lookup(VarTypes, Arg, CalleeType),
    mode_to_arg_mode(ModuleInfo, CalleeMode, CalleeType, ArgMode),
    (
        ArgMode = top_in,
        update_call_args(ModuleInfo, VarTypes, CalleeModes, Args,
            !.UpdatedCallOutArgs, !:UpdatedArgs),
        !:UpdatedArgs = [Arg | !.UpdatedArgs]
    ;
        ArgMode = top_out,
        (
            !.UpdatedCallOutArgs = [UpdatedArg | !:UpdatedCallOutArgs]
        ;
            !.UpdatedCallOutArgs = [],
            unexpected(this_file, "update_call_args: no UpdatedCallOutArgs")
        ),
        update_call_args(ModuleInfo, VarTypes, CalleeModes, Args,
            !.UpdatedCallOutArgs, !:UpdatedArgs),
        !:UpdatedArgs = [UpdatedArg | !.UpdatedArgs]
    ;
        ArgMode = top_unused,
        unexpected(this_file, "update_call_args: top_unused")
    ).

%-----------------------------------------------------------------------------%

:- pred classify_proc_call_args(module_info::in, vartypes::in,
    list(prog_var)::in, list(mer_mode)::in,
    list(prog_var)::out, list(prog_var)::out, list(prog_var)::out) is det.

classify_proc_call_args(_ModuleInfo, _VarTypes, [], [], [], [], []).
classify_proc_call_args(_ModuleInfo, _VarTypes, [], [_ | _], _, _, _) :-
    unexpected(this_file, "classify_proc_call_args: mismatches lists").
classify_proc_call_args(_ModuleInfo, _VarTypes, [_ | _], [], _, _, _) :-
    unexpected(this_file, "classify_proc_call_args: mismatches lists").
classify_proc_call_args(ModuleInfo, VarTypes, [Arg | Args],
        [CalleeMode | CalleeModes], !:InArgs, !:OutArgs, !:UnusedArgs) :-
    classify_proc_call_args(ModuleInfo, VarTypes, Args, CalleeModes,
        !:InArgs, !:OutArgs, !:UnusedArgs),
    map.lookup(VarTypes, Arg, CalleeType),
    mode_to_arg_mode(ModuleInfo, CalleeMode, CalleeType, ArgMode),
    (
        ArgMode = top_in,
        !:InArgs = [Arg | !.InArgs]
    ;
        ArgMode = top_out,
        !:OutArgs = [Arg | !.OutArgs]
    ;
        ArgMode = top_unused,
        !:UnusedArgs = [Arg | !.UnusedArgs]
    ).

%-----------------------------------------------------------------------------%

:- pred find_args_to_pass_by_addr(assoc_list(prog_var, prog_var)::in, int::in,
    assoc_list(int, prog_var)::out, list(prog_var)::out,
    map(prog_var, prog_var)::in, map(prog_var, prog_var)::out,
    lco_info::in, lco_info::out) is det.

find_args_to_pass_by_addr([], _, [], [], !Subst, !Info).
find_args_to_pass_by_addr([CallArg - HeadArg | CallHeadArgs], ArgNum,
        Mismatches, [UpdatedCallArg | UpdatedCallArgs], !Subst, !Info) :-
    find_args_to_pass_by_addr(CallHeadArgs, ArgNum + 1, MismatchesTail,
        UpdatedCallArgs, !Subst, !Info),
    ( CallArg = HeadArg ->
        UpdatedCallArg = CallArg,
        Mismatches = MismatchesTail
    ;
        make_address_var(CallArg, UpdatedCallArg, !Info),
        Mismatches = [ArgNum - CallArg | MismatchesTail],
        svmap.det_insert(CallArg, UpdatedCallArg, !Subst)
    ).

:- pred make_address_var(prog_var::in, prog_var::out,
    lco_info::in, lco_info::out) is det.

make_address_var(Var, AddrVar, !Info) :-
    VarSet0 = !.Info ^ var_set,
    VarTypes0 = !.Info ^ var_types,
    varset.lookup_name(VarSet0, Var, "SCCcallarg", Name),
    AddrName = "Addr" ++ Name,
    varset.new_named_var(VarSet0, AddrName, AddrVar, VarSet),
    map.lookup(VarTypes0, Var, FieldType),
    map.det_insert(VarTypes0, AddrVar, make_ref_type(FieldType), VarTypes),
    !:Info = !.Info ^ var_set := VarSet,
    !:Info = !.Info ^ var_types := VarTypes.

:- func make_ref_type(mer_type) = mer_type.

make_ref_type(FieldType) = PtrType :-
    RefTypeName = qualified(mercury_private_builtin_module,
        "store_by_ref_type"),
    PtrType = defined(RefTypeName, [FieldType], star).

%-----------------------------------------------------------------------------%

:- pred ensure_variant_exists(pred_id::in, proc_id::in, list(int)::in,
    pred_proc_id::out, lco_info::in, lco_info::out) is semidet.

ensure_variant_exists(PredId, ProcId, AddrArgNums, VariantPredProcId, !Info) :-
    CurSCCVariants0 = !.Info ^ cur_scc_variants,
    ( map.search(CurSCCVariants0, proc(PredId, ProcId), ExistingVariant) ->
        ExistingVariant = variant_id(ExistingAddrArgNums, VariantPredProcId),
        AddrArgNums = ExistingAddrArgNums
    ;
        ModuleInfo0 = !.Info ^ module_info,
        clone_pred_proc(PredId, ClonePredId, ModuleInfo0, ModuleInfo),
        VariantPredProcId = proc(ClonePredId, ProcId),
        !:Info = !.Info ^ module_info := ModuleInfo,
        NewVariant = variant_id(AddrArgNums, VariantPredProcId),
        map.det_insert(CurSCCVariants0, proc(PredId, ProcId), NewVariant,
            CurSCCVariants),
        !:Info = !.Info ^ cur_scc_variants := CurSCCVariants
    ).

:- pred clone_pred_proc(pred_id::in, pred_id::out,
    module_info::in, module_info::out) is det.

clone_pred_proc(PredId, ClonePredId, !ModuleInfo) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    module_info_get_predicate_table(!.ModuleInfo, PredTable0),
    predicate_table_insert(PredInfo, ClonePredId, PredTable0, PredTable),
    module_info_set_predicate_table(PredTable, !ModuleInfo).

%-----------------------------------------------------------------------------%

:- pred update_construct(map(prog_var, prog_var)::in,
    hlds_goal::in, hlds_goal::out) is det.

update_construct(Subst, Goal0, Goal) :-
    Goal0 = GoalExpr0 - GoalInfo0,
    (
        GoalExpr0 = unify(LHS, RHS0, Mode, Unification0, UnifyContext),
        Unification0 = construct(Var, ConsId, ArgVars, UniModes,
            How, IsUnique, SubInfo0),
        (
            SubInfo0 = no_construct_sub_info,
            TermSizeSlot = no
        ;
            SubInfo0 = construct_sub_info(no, TermSizeSlot)
        )
    ->
        goal_info_get_instmap_delta(GoalInfo0, InstMapDelta0),
        update_construct_args(Subst, 1, ArgVars, UpdatedArgVars, AddrFields,
            InstMapDelta0, InstMapDelta),
        (
            AddrFields = [],
            Goal = Goal0
        ;
            AddrFields = [_ | _],
            SubInfo = construct_sub_info(yes(AddrFields), TermSizeSlot),
            Unification = construct(Var, ConsId, UpdatedArgVars, UniModes,
                How, IsUnique, SubInfo),
            % We must update RHS because quantification gets the set of
            % variables in the unification from there, not from Unification.
            (
                RHS0 = var(_),
                unexpected(this_file, "update_construct: var RHS")
            ;
                RHS0 = functor(RHSConsId, IsExistConstr, RHSVars0),
                expect(unify(ConsId, RHSConsId), this_file,
                    "update_construct: cons_id mismatch"),
                rename_var_list(no, Subst, RHSVars0, RHSVars),
                RHS = functor(RHSConsId, IsExistConstr, RHSVars)
            ;
                RHS0 = lambda_goal(_, _, _, _, _, _, _, _),
                unexpected(this_file, "update_construct: lambda RHS")
            ),
            GoalExpr = unify(LHS, RHS, Mode, Unification, UnifyContext),

            goal_info_set_instmap_delta(InstMapDelta, GoalInfo0, GoalInfo),
            Goal = GoalExpr - GoalInfo
        )
    ;
        unexpected(this_file, "update_construct: not construct")
    ).

:- pred update_construct_args(map(prog_var, prog_var)::in, int::in,
    list(prog_var)::in, list(prog_var)::out, list(int)::out,
    instmap_delta::in, instmap_delta::out) is det.

update_construct_args(_, _, [], [], [], !InstMapDelta).
update_construct_args(Subst, ArgNum, [OrigVar | OrigVars],
        [UpdatedVar | UpdatedVars], AddrArgs, !InstMapDelta) :-
    update_construct_args(Subst, ArgNum + 1, OrigVars, UpdatedVars,
        AddrArgsTail, !InstMapDelta),
    ( map.search(Subst, OrigVar, AddrVar) ->
        UpdatedVar = AddrVar,
        instmap_delta_set(AddrVar, ground(shared, none), !InstMapDelta),
        AddrArgs = [ArgNum | AddrArgsTail]
    ;
        UpdatedVar = OrigVar,
        AddrArgs = AddrArgsTail
    ).

%-----------------------------------------------------------------------------%

:- pred acceptable_construct_mode(module_info::in, uni_mode::in) is semidet.

acceptable_construct_mode(ModuleInfo, UniMode) :-
    UniMode = ((InitInstX - InitInstY) -> (FinalInstX - FinalInstY)),
    inst_is_free(ModuleInfo, InitInstX),
    inst_is_ground(ModuleInfo, InitInstY),
    inst_is_ground(ModuleInfo, FinalInstX),
    inst_is_ground(ModuleInfo, FinalInstY).

:- pred occurs_once(bag(prog_var)::in, prog_var::in) is semidet.

occurs_once(Bag, Var) :-
    bag.count_value(Bag, Var, 1).

%-----------------------------------------------------------------------------%

:- pred transform_variant_proc(module_info::in, list(int)::in,
    proc_info::in, proc_info::out) is det.

transform_variant_proc(ModuleInfo, AddrOutArgPosns, ProcInfo,
        !:VariantProcInfo) :-
    !:VariantProcInfo = ProcInfo,
    proc_info_varset(ProcInfo, VarSet0),
    proc_info_vartypes(ProcInfo, VarTypes0),
    proc_info_headvars(ProcInfo, HeadVars0),
    proc_info_argmodes(ProcInfo, ArgModes0),
    make_addr_vars(HeadVars0, ArgModes0, HeadVars, ArgModes, 
        AddrOutArgPosns, 1, ModuleInfo, VarToAddr,
        VarSet0, VarSet, VarTypes0, VarTypes),
    proc_info_set_headvars(HeadVars, !VariantProcInfo),
    proc_info_set_argmodes(ArgModes, !VariantProcInfo),
    proc_info_set_varset(VarSet, !VariantProcInfo),
    proc_info_set_vartypes(VarTypes, !VariantProcInfo),

    proc_info_get_initial_instmap(ProcInfo, ModuleInfo, InstMap0),
    proc_info_goal(ProcInfo, Goal0),
    transform_variant_goal(ModuleInfo, VarToAddr, InstMap0, Goal0, Goal),
    proc_info_set_goal(Goal, !VariantProcInfo),
    % We changed the scopes of the headvars we now return via pointers.
    requantify_proc(!VariantProcInfo).

:- pred make_addr_vars(list(prog_var)::in, list(mer_mode)::in,
    list(prog_var)::out, list(mer_mode)::out, list(int)::in,
    int::in, module_info::in, assoc_list(prog_var)::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

make_addr_vars([], [], [], [], AddrOutArgPosns, _, _, [],
        !VarSet, !VarTypes) :-
    expect(unify(AddrOutArgPosns, []), this_file,
        "make_addr_vars: AddrOutArgPosns != []").
make_addr_vars([], [_ | _], _, _, _, _, _, _, !VarSet, !VarTypes) :-
    unexpected(this_file, "make_addr_vars: mismatched lists").
make_addr_vars([_ | _], [], _, _, _, _, _, _, !VarSet, !VarTypes) :-
    unexpected(this_file, "make_addr_vars: mismatched lists").
make_addr_vars([HeadVar0 | HeadVars0], [Mode0 | Modes0],
        [HeadVar | HeadVars], [Mode | Modes], !.AddrOutArgPosns,
        NextOutArgNum, ModuleInfo, VarToAddr, !VarSet, !VarTypes) :-
    map.lookup(!.VarTypes, HeadVar0, HeadVarType),
    mode_to_arg_mode(ModuleInfo, Mode0, HeadVarType, ArgMode),
    (
        ArgMode = top_in,
        HeadVar = HeadVar0,
        Mode = Mode0,
        make_addr_vars(HeadVars0, Modes0, HeadVars, Modes, !.AddrOutArgPosns,
            NextOutArgNum, ModuleInfo, VarToAddr, !VarSet, !VarTypes)
    ;
        ArgMode = top_out,
        ( !.AddrOutArgPosns = [NextOutArgNum | !:AddrOutArgPosns] ->
            varset.lookup_name(!.VarSet, HeadVar0, Name),
            AddrName = "AddrOf" ++ Name,
            svvarset.new_named_var(AddrName, AddrVar, !VarSet),
            map.lookup(!.VarTypes, HeadVar0, OldType),
            svmap.det_insert(AddrVar, make_ref_type(OldType), !VarTypes),
            HeadVar = AddrVar,
            Mode = in_mode,
            make_addr_vars(HeadVars0, Modes0, HeadVars, Modes,
                !.AddrOutArgPosns, NextOutArgNum + 1, ModuleInfo,
                VarToAddrTail, !VarSet, !VarTypes),
            VarToAddr = [HeadVar0 - AddrVar | VarToAddrTail]
        ;
            HeadVar = HeadVar0,
            Mode = Mode0,
            make_addr_vars(HeadVars0, Modes0, HeadVars, Modes,
                !.AddrOutArgPosns, NextOutArgNum + 1, ModuleInfo,
                VarToAddr, !VarSet, !VarTypes)
        )
    ;
        ArgMode = top_unused,
        unexpected(this_file, "make_addr_vars: top_unused")
    ).

:- pred transform_variant_goal(module_info::in, assoc_list(prog_var)::in,
    instmap::in, hlds_goal::in, hlds_goal::out) is det.

transform_variant_goal(ModuleInfo, VarToAddr, InstMap0,
        GoalExpr0 - GoalInfo, GoalExpr - GoalInfo) :-
    (
        GoalExpr0 = conj(ConjType, Goals0),
        ( ConjType = parallel_conj ->
            unexpected(this_file, "transform_variant_goal: parallel_conj")
        ;
            transform_variant_conj(ModuleInfo, VarToAddr, InstMap0,
                Goals0, Goals),
            GoalExpr = conj(ConjType, Goals)
        )
    ;
        GoalExpr0 = disj(Goals0),
        list.map(transform_variant_goal(ModuleInfo, VarToAddr, InstMap0),
            Goals0, Goals),
        GoalExpr = disj(Goals)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        list.map(transform_variant_case(ModuleInfo, VarToAddr, InstMap0),
            Cases0, Cases),
        GoalExpr = switch(Var, CanFail, Cases)
    ;
        GoalExpr0 = if_then_else(Vars, Cond, Then0, Else0),
        update_instmap(Cond, InstMap0, InstMap1),
        transform_variant_goal(ModuleInfo, VarToAddr, InstMap1, Then0, Then),
        transform_variant_goal(ModuleInfo, VarToAddr, InstMap0, Else0, Else),
        GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        transform_variant_goal(ModuleInfo, VarToAddr, InstMap0,
            SubGoal0, SubGoal),
        GoalExpr = scope(Reason, SubGoal)
    ;
        GoalExpr0 = not(_),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = generic_call(_, _, _, _),
        transform_variant_atomic_goal(ModuleInfo, VarToAddr, InstMap0,
            GoalInfo, GoalExpr0, GoalExpr)
    ;
        GoalExpr0 = call(_, _, _, _, _, _),
        % XXX We could handle recursive calls better.
        transform_variant_atomic_goal(ModuleInfo, VarToAddr, InstMap0,
            GoalInfo, GoalExpr0, GoalExpr)
    ;
        GoalExpr0 = unify(_, _, _, _, _),
        transform_variant_atomic_goal(ModuleInfo, VarToAddr, InstMap0,
            GoalInfo, GoalExpr0, GoalExpr)
    ;
        GoalExpr0 = foreign_proc(_, _,_,  _, _, _),
        transform_variant_atomic_goal(ModuleInfo, VarToAddr, InstMap0,
            GoalInfo, GoalExpr0, GoalExpr)
    ;
        GoalExpr0 = shorthand(_),
        unexpected(this_file, "transform_variant_goal: shorthand")
    ).

:- pred transform_variant_conj(module_info::in, assoc_list(prog_var)::in,
    instmap::in, list(hlds_goal)::in, list(hlds_goal)::out) is det.

transform_variant_conj(_, _, _, [], []).
transform_variant_conj(ModuleInfo, VarToAddr, InstMap0, [Goal0 | Goals0],
        Conj) :-
    transform_variant_goal(ModuleInfo, VarToAddr, InstMap0, Goal0, Goal),
    update_instmap(Goal0, InstMap0, InstMap1),
    transform_variant_conj(ModuleInfo, VarToAddr, InstMap1, Goals0, Goals),
    ( Goal = conj(plain_conj, SubConj) - _ ->
        Conj = SubConj ++ Goals
    ;
        Conj = [Goal | Goals]
    ).

:- pred transform_variant_case(module_info::in,
    assoc_list(prog_var)::in, instmap::in, case::in, case::out) is det.

transform_variant_case(ModuleInfo, VarToAddr, InstMap0, case(ConsId, Goal0),
        case(ConsId, Goal)) :-
    transform_variant_goal(ModuleInfo, VarToAddr, InstMap0, Goal0, Goal).

:- pred transform_variant_atomic_goal(module_info::in,
    assoc_list(prog_var)::in, instmap::in, hlds_goal_info::in,
    hlds_goal_expr::in, hlds_goal_expr::out) is det.

transform_variant_atomic_goal(ModuleInfo, VarToAddr, InstMap0, GoalInfo,
        GoalExpr0, GoalExpr) :-
    update_instmap(GoalExpr0 - GoalInfo, InstMap0, InstMap1),
    list.filter(is_grounding(ModuleInfo, InstMap0, InstMap1), VarToAddr,
        GroundingVarToAddr),
    (
        GroundingVarToAddr = [],
        GoalExpr = GoalExpr0
    ;
        GroundingVarToAddr = [_ | _],
        list.map(make_store_goal(ModuleInfo), GroundingVarToAddr, StoreGoals),
        GoalExpr = conj(plain_conj, [GoalExpr0 - GoalInfo | StoreGoals])
    ).

:- pred is_grounding(module_info::in, instmap::in, instmap::in,
    pair(prog_var)::in) is semidet.

is_grounding(ModuleInfo, InstMap0, InstMap, Var - _AddrVar) :-
    instmap.lookup_var(InstMap0, Var, Inst0),
    not inst_is_ground(ModuleInfo, Inst0),
    instmap.lookup_var(InstMap, Var, Inst),
    inst_is_ground(ModuleInfo, Inst).

:- pred make_store_goal(module_info::in, pair(prog_var)::in,
    hlds_goal::out) is det.

make_store_goal(ModuleInfo, Var - AddrVar, Goal) :-
    generate_simple_call(mercury_private_builtin_module, "store_at_ref",
        predicate, only_mode, det, [AddrVar, Var], [impure_goal], [],
        ModuleInfo, term.context_init, Goal).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "lco.m".

%-----------------------------------------------------------------------------%
:- end_module lco.
%-----------------------------------------------------------------------------%
