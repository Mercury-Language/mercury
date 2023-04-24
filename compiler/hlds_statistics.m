%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2010-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: hlds_statistics.m.
% Author: zs.
%
% Write out statistics about various aspects of the HLDS.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module hlds.hlds_statistics.
:- interface.

:- import_module hlds.hlds_module.

:- import_module io.

    % Write out size statistics about each procedure in the module.
    % The statistics state the size of the procedure's body (the number of
    % subgoals of each possible kind) and the number of its variables.
    %
:- pred write_proc_stats_for_module(io.text_output_stream::in, string::in,
    module_info::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.var_table.
:- import_module parse_tree.write_error_spec.

:- import_module assoc_list.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module set_tree234.
:- import_module string.
:- import_module term.

%-----------------------------------------------------------------------------%

write_proc_stats_for_module(OutStream, Msg, ModuleInfo, !IO) :-
    module_info_get_name(ModuleInfo, ModuleSymName),
    ModuleName = sym_name_to_string(ModuleSymName),
    io.format(OutStream, "MODULE %s\n", [s(ModuleName)], !IO),

    module_info_get_pred_id_table(ModuleInfo, PredIdTable),
    map.to_assoc_list(PredIdTable, PredIdsInfos),
    list.foldl(write_proc_stats_for_pred(OutStream, Msg, ModuleInfo),
        PredIdsInfos, !IO).

:- pred write_proc_stats_for_pred(io.text_output_stream::in, string::in,
    module_info::in, pair(pred_id, pred_info)::in, io::di, io::uo) is det.

write_proc_stats_for_pred(OutStream, Msg, ModuleInfo, PredId - PredInfo,
        !IO) :-
    ( if
        ( pred_info_is_imported(PredInfo)
        ; is_unify_index_or_compare_pred(PredInfo)
        )
    then
        true
    else
        pred_info_get_proc_table(PredInfo, ProcTable),
        map.to_assoc_list(ProcTable, Procs),
        list.foldl(
            write_proc_stats_for_proc(OutStream, Msg, ModuleInfo, PredId),
            Procs, !IO)
    ).

:- pred write_proc_stats_for_proc(io.text_output_stream::in, string::in,
    module_info::in, pred_id::in, pair(proc_id, proc_info)::in,
    io::di, io::uo) is det.

write_proc_stats_for_proc(OutStream, Msg, ModuleInfo,
        PredId, ProcId - ProcInfo, !IO) :-
    NamePieces = describe_one_proc_name(ModuleInfo, should_not_module_qualify,
        proc(PredId, ProcId)),
    Name = error_pieces_to_string(NamePieces),

    proc_info_get_goal(ProcInfo, Goal),
    UsedVars0 = set_tree234.init,
    Stats0 = init_proc_stats,
    accumulate_proc_stats_in_goal(Goal, UsedVars0, UsedVars, Stats0, Stats),

    proc_info_get_var_table(ProcInfo, VarTable),
    do_write_proc_stats(OutStream, Msg, Name, PredId, ProcId, Stats,
        UsedVars, VarTable, !IO).

%-----------------------------------------------------------------------------%

:- pred accumulate_proc_stats_in_goal(hlds_goal::in,
    set_tree234(prog_var)::in, set_tree234(prog_var)::out,
    proc_stats::in, proc_stats::out) is det.

accumulate_proc_stats_in_goal(Goal, !UsedVars, !Stats) :-
    Goal = hlds_goal(GoalExpr, _GoalInfo),
    (
        GoalExpr = unify(LHS, RHS, _, Uni, _),
        (
            Uni = construct(CellVar, _, ArgVars, _, _, _, _),
            set_tree234.insert(CellVar, !UsedVars),
            set_tree234.insert_list(ArgVars, !UsedVars),
            !Stats ^ ps_unify_constructs := !.Stats ^ ps_unify_constructs + 1
        ;
            Uni = deconstruct(CellVar, _, ArgVars, _, _, _),
            set_tree234.insert(CellVar, !UsedVars),
            set_tree234.insert_list(ArgVars, !UsedVars),
            !Stats ^ ps_unify_deconstructs :=
                !.Stats ^ ps_unify_deconstructs + 1
        ;
            Uni = assign(ToVar, FromVar),
            set_tree234.insert(ToVar, !UsedVars),
            set_tree234.insert(FromVar, !UsedVars),
            !Stats ^ ps_unify_assigns := !.Stats ^ ps_unify_assigns + 1
        ;
            Uni = simple_test(VarA, VarB),
            set_tree234.insert(VarA, !UsedVars),
            set_tree234.insert(VarB, !UsedVars),
            !Stats ^ ps_unify_tests := !.Stats ^ ps_unify_tests + 1
        ;
            Uni = complicated_unify(_, _, _),
            set_tree234.insert(LHS, !UsedVars),
            (
                RHS = rhs_var(RHSVar),
                set_tree234.insert(RHSVar, !UsedVars),
                !Stats ^ ps_unify_complicateds :=
                    !.Stats ^ ps_unify_complicateds + 1
            ;
                RHS = rhs_functor(_, _, RHSVars),
                set_tree234.insert_list(RHSVars, !UsedVars),
                !Stats ^ ps_unify_complicateds :=
                    !.Stats ^ ps_unify_complicateds + 1
            ;
                RHS = rhs_lambda_goal(_, _, _, _, NonLocals, ArgVarsModes,
                    _, LambdaGoal),
                assoc_list.keys(ArgVarsModes, ArgVars),
                set_tree234.insert_list(NonLocals, !UsedVars),
                set_tree234.insert_list(ArgVars, !UsedVars),
                !Stats ^ ps_unify_complicateds :=
                    !.Stats ^ ps_unify_complicateds + 1,
                accumulate_proc_stats_in_goal(LambdaGoal, !UsedVars, !Stats)
            )
        )
    ;
        GoalExpr = plain_call(_, _, ArgVars, _, _, _),
        set_tree234.insert_list(ArgVars, !UsedVars),
        !Stats ^ ps_plain_calls := !.Stats ^ ps_plain_calls + 1
    ;
        GoalExpr = call_foreign_proc(_, _, _, Args, ExtraArgs, _, _),
        ArgVars = list.map(foreign_arg_var, Args),
        ExtraArgVars = list.map(foreign_arg_var, ExtraArgs),
        set_tree234.insert_list(ArgVars, !UsedVars),
        set_tree234.insert_list(ExtraArgVars, !UsedVars),
        !Stats ^ ps_foreign_calls := !.Stats ^ ps_foreign_calls + 1
    ;
        GoalExpr = generic_call(CallKind, ArgVars, _, _, _),
        set_tree234.insert_list(ArgVars, !UsedVars),
        (
            CallKind = higher_order(HOVar, _, _, _),
            set_tree234.insert(HOVar, !UsedVars),
            !Stats ^ ps_ho_calls := !.Stats ^ ps_ho_calls + 1
        ;
            CallKind = class_method(TCIVar, _, _, _),
            set_tree234.insert(TCIVar, !UsedVars),
            !Stats ^ ps_method_calls := !.Stats ^ ps_method_calls + 1
        ;
            CallKind = event_call(_),
            !Stats ^ ps_event_calls := !.Stats ^ ps_event_calls + 1
        ;
            CallKind = cast(_),
            !Stats ^ ps_casts := !.Stats ^ ps_casts + 1
        )
    ;
        GoalExpr = conj(ConjType, Conjs),
        (
            ConjType = plain_conj,
            !Stats ^ ps_plain_conjs := !.Stats ^ ps_plain_conjs + 1,
            accumulate_proc_stats_in_plain_conj(Conjs, !UsedVars, !Stats)
        ;
            ConjType = parallel_conj,
            !Stats ^ ps_parallel_conjs := !.Stats ^ ps_parallel_conjs + 1,
            accumulate_proc_stats_in_parallel_conj(Conjs, !UsedVars, !Stats)
        )
    ;
        GoalExpr = disj(Disjs),
        !Stats ^ ps_disjs := !.Stats ^ ps_disjs + 1,
        accumulate_proc_stats_in_disj(Disjs, !UsedVars, !Stats)
    ;
        GoalExpr = switch(SwitchVar, _, Cases),
        set_tree234.insert(SwitchVar, !UsedVars),
        !Stats ^ ps_switches := !.Stats ^ ps_switches + 1,
        accumulate_proc_stats_in_switch(Cases, !UsedVars, !Stats)
    ;
        GoalExpr = if_then_else(_, CondGoal, ThenGoal, ElseGoal),
        !Stats ^ ps_ites := !.Stats ^ ps_ites + 1,
        accumulate_proc_stats_in_goal(CondGoal, !UsedVars, !Stats),
        accumulate_proc_stats_in_goal(ThenGoal, !UsedVars, !Stats),
        accumulate_proc_stats_in_goal(ElseGoal, !UsedVars, !Stats)
    ;
        GoalExpr = negation(SubGoal),
        !Stats ^ ps_negations := !.Stats ^ ps_negations + 1,
        accumulate_proc_stats_in_goal(SubGoal, !UsedVars, !Stats)
    ;
        GoalExpr = scope(_, SubGoal),
        !Stats ^ ps_scopes := !.Stats ^ ps_scopes + 1,
        accumulate_proc_stats_in_goal(SubGoal, !UsedVars, !Stats)
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ShortHand = bi_implication(GoalA, GoalB),
            !Stats ^ ps_bi_implications := !.Stats ^ ps_bi_implications + 1,
            accumulate_proc_stats_in_goal(GoalA, !UsedVars, !Stats),
            accumulate_proc_stats_in_goal(GoalB, !UsedVars, !Stats)
        ;
            ShortHand = atomic_goal(_, _, _, _, MainGoal, OrElseGoals, _),
            !Stats ^ ps_atomic_goals := !.Stats ^ ps_atomic_goals + 1,
            accumulate_proc_stats_in_goal(MainGoal, !UsedVars, !Stats),
            list.foldl2(accumulate_proc_stats_in_goal, OrElseGoals,
                !UsedVars, !Stats)
        ;
            ShortHand = try_goal(_, _, SubGoal),
            !Stats ^ ps_try_goals := !.Stats ^ ps_try_goals + 1,
            accumulate_proc_stats_in_goal(SubGoal, !UsedVars, !Stats)
        )
    ).

%-----------------------------------------------------------------------------%

:- pred accumulate_proc_stats_in_plain_conj(list(hlds_goal)::in,
    set_tree234(prog_var)::in, set_tree234(prog_var)::out,
    proc_stats::in, proc_stats::out) is det.

accumulate_proc_stats_in_plain_conj([], !UsedVars, !Stats).
accumulate_proc_stats_in_plain_conj([Goal | Goals], !UsedVars, !Stats) :-
    !Stats ^ ps_plain_conjuncts := !.Stats ^ ps_plain_conjuncts + 1,
    accumulate_proc_stats_in_goal(Goal, !UsedVars, !Stats),
    accumulate_proc_stats_in_plain_conj(Goals, !UsedVars, !Stats).

:- pred accumulate_proc_stats_in_parallel_conj(list(hlds_goal)::in,
    set_tree234(prog_var)::in, set_tree234(prog_var)::out,
    proc_stats::in, proc_stats::out) is det.

accumulate_proc_stats_in_parallel_conj([], !UsedVars, !Stats).
accumulate_proc_stats_in_parallel_conj([Goal | Goals], !UsedVars, !Stats) :-
    !Stats ^ ps_parallel_conjuncts := !.Stats ^ ps_parallel_conjuncts + 1,
    accumulate_proc_stats_in_goal(Goal, !UsedVars, !Stats),
    accumulate_proc_stats_in_parallel_conj(Goals, !UsedVars, !Stats).

:- pred accumulate_proc_stats_in_disj(list(hlds_goal)::in,
    set_tree234(prog_var)::in, set_tree234(prog_var)::out,
    proc_stats::in, proc_stats::out) is det.

accumulate_proc_stats_in_disj([], !UsedVars, !Stats).
accumulate_proc_stats_in_disj([Goal | Goals], !UsedVars, !Stats) :-
    !Stats ^ ps_disjuncts := !.Stats ^ ps_disjuncts + 1,
    accumulate_proc_stats_in_goal(Goal, !UsedVars, !Stats),
    accumulate_proc_stats_in_disj(Goals, !UsedVars, !Stats).

:- pred accumulate_proc_stats_in_switch(list(case)::in,
    set_tree234(prog_var)::in, set_tree234(prog_var)::out,
    proc_stats::in, proc_stats::out) is det.

accumulate_proc_stats_in_switch([], !UsedVars, !Stats).
accumulate_proc_stats_in_switch([Case | Cases], !UsedVars, !Stats) :-
    !Stats ^ ps_switch_arms := !.Stats ^ ps_switch_arms + 1,
    Case = case(_, _, Goal),
    accumulate_proc_stats_in_goal(Goal, !UsedVars, !Stats),
    accumulate_proc_stats_in_switch(Cases, !UsedVars, !Stats).

%-----------------------------------------------------------------------------%

:- type proc_stats
    --->    proc_stats(
                ps_unify_constructs     :: int,
                ps_unify_deconstructs   :: int,
                ps_unify_assigns        :: int,
                ps_unify_tests          :: int,
                ps_unify_complicateds   :: int,

                ps_plain_calls          :: int,

                ps_foreign_calls        :: int,

                ps_ho_calls             :: int,
                ps_method_calls         :: int,
                ps_event_calls          :: int,
                ps_casts                :: int,

                ps_plain_conjs          :: int,
                ps_plain_conjuncts      :: int,
                ps_parallel_conjs       :: int,
                ps_parallel_conjuncts   :: int,

                ps_disjs                :: int,
                ps_disjuncts            :: int,

                ps_switches             :: int,
                ps_switch_arms          :: int,

                ps_ites                 :: int,

                ps_negations            :: int,

                ps_scopes               :: int,

                ps_bi_implications      :: int,
                ps_atomic_goals         :: int,
                ps_try_goals            :: int
            ).

:- func init_proc_stats = proc_stats.

init_proc_stats = Stats :-
    Stats = proc_stats(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0).

:- pred write_proc_stat_components(io.text_output_stream::in, string::in,
    string::in, pred_id::in, proc_id::in, proc_stats::in, io::di, io::uo)
    is det.

write_proc_stat_components(OutStream, Msg, Name, PredId, ProcId, Stats, !IO) :-
    Stats = proc_stats(UnifyConstructs, UnifyDeconstructs,
        UnifyAssigns, UnifyTests, UnifyComplicateds,
        PlainCalls, ForeignCalls, HOCalls, MethodCalls, EventCalls, Casts,
        PlainConjs, PlainConjuncts, ParallelConjs, ParallelConjuncts,
        Disjs, Disjuncts, Switches, SwitchArms,
        IfThenElses, Negations, Scopes, BiImplications, AtomicGoals, TryGoals),

    Total =
        UnifyConstructs + UnifyDeconstructs +
        UnifyAssigns + UnifyTests + UnifyComplicateds +
        PlainCalls + ForeignCalls +
        HOCalls + MethodCalls + EventCalls + Casts +
        PlainConjs + ParallelConjs +
        Disjs + Switches +
        IfThenElses + Negations + Scopes +
        BiImplications + AtomicGoals + TryGoals,

    output_proc_stat_component(OutStream, Msg, Name, PredId, ProcId,
        "unify_contructs", UnifyConstructs, !IO),
    output_proc_stat_component(OutStream, Msg, Name, PredId, ProcId,
        "unify_decontructs", UnifyDeconstructs, !IO),
    output_proc_stat_component(OutStream, Msg, Name, PredId, ProcId,
        "unify_assigns", UnifyAssigns, !IO),
    output_proc_stat_component(OutStream, Msg, Name, PredId, ProcId,
        "unify_tests", UnifyTests, !IO),
    output_proc_stat_component(OutStream, Msg, Name, PredId, ProcId,
        "unify_complicateds", UnifyComplicateds, !IO),

    output_proc_stat_component(OutStream, Msg, Name, PredId, ProcId,
        "plain_calls", PlainCalls, !IO),
    output_proc_stat_component(OutStream, Msg, Name, PredId, ProcId,
        "foreign_calls", ForeignCalls, !IO),
    output_proc_stat_component(OutStream, Msg, Name, PredId, ProcId,
        "ho_calls", HOCalls, !IO),
    output_proc_stat_component(OutStream, Msg, Name, PredId, ProcId,
        "method_calls", MethodCalls, !IO),
    output_proc_stat_component(OutStream, Msg, Name, PredId, ProcId,
        "event_calls", EventCalls, !IO),
    output_proc_stat_component(OutStream, Msg, Name, PredId, ProcId,
        "casts", Casts, !IO),

    output_proc_stat_component(OutStream, Msg, Name, PredId, ProcId,
        "plain_conjs", PlainConjs, !IO),
    output_proc_stat_component(OutStream, Msg, Name, PredId, ProcId,
        "plain_conjuncts", PlainConjuncts, !IO),
    output_proc_stat_component(OutStream, Msg, Name, PredId, ProcId,
        "parallel_conjs", ParallelConjs, !IO),
    output_proc_stat_component(OutStream, Msg, Name, PredId, ProcId,
        "parallel_conjuncts", ParallelConjuncts, !IO),

    output_proc_stat_component(OutStream, Msg, Name, PredId, ProcId,
        "disjs", Disjs, !IO),
    output_proc_stat_component(OutStream, Msg, Name, PredId, ProcId,
        "disjunctions", Disjuncts, !IO),
    output_proc_stat_component(OutStream, Msg, Name, PredId, ProcId,
        "switches", Switches, !IO),
    output_proc_stat_component(OutStream, Msg, Name, PredId, ProcId,
        "switch_arms", SwitchArms, !IO),

    output_proc_stat_component(OutStream, Msg, Name, PredId, ProcId,
        "if_then_elses", IfThenElses, !IO),
    output_proc_stat_component(OutStream, Msg, Name, PredId, ProcId,
        "negations", Negations, !IO),
    output_proc_stat_component(OutStream, Msg, Name, PredId, ProcId,
        "scopes", Scopes, !IO),
    output_proc_stat_component(OutStream, Msg, Name, PredId, ProcId,
        "bi_implications", BiImplications, !IO),
    output_proc_stat_component(OutStream, Msg, Name, PredId, ProcId,
        "atomic_goals", AtomicGoals, !IO),
    output_proc_stat_component(OutStream, Msg, Name, PredId, ProcId,
        "try_goals", TryGoals, !IO),

    output_proc_stat_component(OutStream, Msg, Name, PredId, ProcId,
        "total_size", Total, !IO).

%-----------------------------------------------------------------------------%

:- pred do_write_proc_stats(io.text_output_stream::in,
    string::in, string::in, pred_id::in, proc_id::in,
    proc_stats::in, set_tree234(prog_var)::in, var_table::in,
    io::di, io::uo) is det.

do_write_proc_stats(OutStream, Msg, Name, PredId, ProcId,
        Stats, UsedVars, VarTable, !IO) :-
    PredIdInt = pred_id_to_int(PredId),
    ProcIdInt = proc_id_to_int(ProcId),
    io.format(OutStream, "PROC %d %d %s\n",
        [i(PredIdInt), i(ProcIdInt), s(Name)], !IO),

    write_proc_stat_components(OutStream, Msg, Name, PredId, ProcId, Stats,
        !IO),

    var_table_count(VarTable, VarTableCount),
    NumUsedVars = set_tree234.count(UsedVars),
    io.format(OutStream, "VARS %d %d\n",
        [i(VarTableCount), i(NumUsedVars)], !IO).

:- pred output_proc_stat_component(io.text_output_stream::in,
    string::in, string::in, pred_id::in, proc_id::in,
    string::in, int::in, io::di, io::uo) is det.

output_proc_stat_component(OutStream, _Msg, _Name, _PredId, _ProcId,
        ComponentName, ComponentCount, !IO) :-
    ( if ComponentCount > 0 then
        io.format(OutStream, "GOAL %s: %d\n",
            [s(ComponentName), i(ComponentCount)], !IO)
    else
        true
    ).

%-----------------------------------------------------------------------------%
:- end_module hlds.hlds_statistics.
%-----------------------------------------------------------------------------%
