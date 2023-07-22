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

    % Count the number of insts using each function symbol of the mer_inst
    % type in all the procedures of the given module, and write out a report
    % containing these counts to the given stream.
    %
:- pred write_inst_stats_for_module(io.text_output_stream::in,
    module_info::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_inst_mode.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
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

%-----------------------------------------------------------------------------%

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
%-----------------------------------------------------------------------------%

write_inst_stats_for_module(OutStream, ModuleInfo, !IO) :-
    module_info_get_name(ModuleInfo, ModuleSymName),
    ModuleName = sym_name_to_string(ModuleSymName),

    module_info_get_pred_id_table(ModuleInfo, PredIdTable),
    map.values(PredIdTable, PredInfos),

    StatsInProcs0 = init_inst_stats,
    list.foldl(acc_inst_stats_in_pred, PredInfos, StatsInProcs0, StatsInProcs),

    module_info_get_inst_table(ModuleInfo, InstTable),
    % User insts are never numerous enough to be worth collecting stats about.
    % inst_table_get_user_insts(InstTable, UserInsts),
    inst_table_get_unify_insts(InstTable, UnifyInstsTable),
    inst_table_get_merge_insts(InstTable, MergeInstsTable),
    inst_table_get_ground_insts(InstTable, GroundInstsTable),
    inst_table_get_any_insts(InstTable, AnyInstsTable),
    inst_table_get_shared_insts(InstTable, SharedInstsTable),
    inst_table_get_mostly_uniq_insts(InstTable, MostlyUniqInstsTable),

    unify_insts_to_sorted_pairs(UnifyInstsTable, UnifyInsts),
    merge_insts_to_sorted_pairs(MergeInstsTable, MergeInsts),
    ground_insts_to_sorted_pairs(GroundInstsTable, GroundInsts),
    any_insts_to_sorted_pairs(AnyInstsTable, AnyInsts),
    shared_insts_to_sorted_pairs(SharedInstsTable, SharedInsts),
    mostly_uniq_insts_to_sorted_pairs(MostlyUniqInstsTable, MostlyUniqInsts),

    StatsInTables0 = init_inst_stats,
    list.foldl(acc_inst_stats_in_unify_inst, UnifyInsts,
        StatsInTables0, StatsInTables1),
    list.foldl(acc_inst_stats_in_merge_inst, MergeInsts,
        StatsInTables1, StatsInTables2),
    list.foldl(acc_inst_stats_in_ground_inst, GroundInsts,
        StatsInTables2, StatsInTables3),
    list.foldl(acc_inst_stats_in_any_inst, AnyInsts,
        StatsInTables3, StatsInTables4),
    list.foldl(acc_inst_stats_in_named_inst, SharedInsts,
        StatsInTables4, StatsInTables5),
    list.foldl(acc_inst_stats_in_named_inst, MostlyUniqInsts,
        StatsInTables5, StatsInTables),

    io.format(OutStream, "INST_STATS FOR MODULE %s\n", [s(ModuleName)], !IO),
    write_inst_stats(OutStream, "proc", StatsInProcs, !IO),
    write_inst_stats(OutStream, "table", StatsInTables, !IO).

:- pred write_inst_stats(io.text_output_stream::in, string::in,
    inst_stats::in, io::di, io::uo) is det.

write_inst_stats(OutStream, KindStr, Stats, !IO) :-
    Stats = inst_stats(Free, Any, Bound, Ground, NotReached, Var,
        Constrained, Defined),

    io.format(OutStream, "%-5s %-15s %20d\n",
        [s(KindStr), s("free"), i(Free)], !IO),
    io.format(OutStream, "%-5s %-15s %20d\n",
        [s(KindStr), s("any"), i(Any)], !IO),
    io.format(OutStream, "%-5s %-15s %20d\n",
        [s(KindStr), s("bound"), i(Bound)], !IO),
    io.format(OutStream, "%-5s %-15s %20d\n",
        [s(KindStr), s("ground"), i(Ground)], !IO),
    io.format(OutStream, "%-5s %-15s %20d\n",
        [s(KindStr), s("not_reached"), i(NotReached)], !IO),
    io.format(OutStream, "%-5s %-15s %20d\n",
        [s(KindStr), s("inst_var"), i(Var)], !IO),
    io.format(OutStream, "%-5s %-15s %20d\n",
        [s(KindStr), s("constrained"), i(Constrained)], !IO),
    io.format(OutStream, "%-5s %-15s %20d\n",
        [s(KindStr), s("defined"), i(Defined)], !IO).

%-----------------------------------------------------------------------------%

:- type inst_stats
    --->    inst_stats(
                is_free0        :: int,
                is_any          :: int,
                is_bound        :: int,
                is_ground       :: int,
                is_notreached   :: int,
                is_var          :: int,
                is_constrained  :: int,
                is_defined      :: int
            ).

:- func init_inst_stats = inst_stats.

init_inst_stats = inst_stats(0, 0, 0, 0, 0, 0, 0, 0).

%-----------------------------------------------------------------------------%

:- pred acc_inst_stats_in_pred(pred_info::in, inst_stats::in, inst_stats::out)
    is det.

acc_inst_stats_in_pred(PredInfo, !Stats) :-
    pred_info_get_proc_table(PredInfo, ProcTable),
    map.foldl_values(acc_inst_stats_in_proc, ProcTable, !Stats).

:- pred acc_inst_stats_in_proc(proc_info::in, inst_stats::in, inst_stats::out)
    is det.

acc_inst_stats_in_proc(ProcInfo, !Stats) :-
    proc_info_get_argmodes(ProcInfo, ArgModes),
    list.foldl(acc_inst_stats_in_mode, ArgModes, !Stats),
    proc_info_get_goal(ProcInfo, Goal),
    acc_inst_stats_in_goal(Goal, !Stats).

:- pred acc_inst_stats_in_goal(hlds_goal::in, inst_stats::in, inst_stats::out)
    is det.

acc_inst_stats_in_goal(Goal, !Stats) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
    instmap_delta_to_assoc_list(InstMapDelta, InstMapDeltaAL),
    assoc_list.values(InstMapDeltaAL, InstMapDeltaInsts),
    list.foldl(acc_inst_stats_in_inst, InstMapDeltaInsts, !Stats),
    (
        GoalExpr = unify(_LHS, _RHS, UnifyMode, _Kind, _Context),
        UnifyMode = unify_modes_li_lf_ri_rf(InstLI, InstLF, InstRI, InstRF),
        acc_inst_stats_in_inst(InstLI, !Stats),
        acc_inst_stats_in_inst(InstLF, !Stats),
        acc_inst_stats_in_inst(InstRI, !Stats),
        acc_inst_stats_in_inst(InstRF, !Stats)
    ;
        GoalExpr = plain_call(_PredId, _ProcId, _ArgVars, _Builtin,
            _UnifyContext, _SymName)
    ;
        GoalExpr = generic_call(_GCall, _ArgVars, Modes, _RegTypes, _Det),
        list.foldl(acc_inst_stats_in_mode, Modes, !Stats)
    ;
        GoalExpr = call_foreign_proc(_Attrs, _PredId, _ProcId,
            _Args, _ExtraArgs, _TraceCond, _Impl)
    ;
        ( GoalExpr = conj(_ConjType, SubGoals)
        ; GoalExpr = disj(SubGoals)
        ),
        list.foldl(acc_inst_stats_in_goal, SubGoals, !Stats)
    ;
        GoalExpr = switch(_Var, _CanFail, Cases),
        list.foldl(acc_inst_stats_in_case, Cases, !Stats)
    ;
        ( GoalExpr = negation(SubGoal)
        ; GoalExpr = scope(_Reason, SubGoal)
        ),
        acc_inst_stats_in_goal(SubGoal, !Stats)
    ;
        GoalExpr = if_then_else(_Vars, Cond, Then, Else),
        acc_inst_stats_in_goal(Cond, !Stats),
        acc_inst_stats_in_goal(Then, !Stats),
        acc_inst_stats_in_goal(Else, !Stats)
    ;
        GoalExpr = shorthand(Shorthand),
        (
            Shorthand = bi_implication(SubGoalA, SubGoalB),
            acc_inst_stats_in_goal(SubGoalA, !Stats),
            acc_inst_stats_in_goal(SubGoalB, !Stats)
        ;
            Shorthand = atomic_goal(_Type, _Outer, _Inner, _Outputs,
                MainGoal, OrElseGoals, _OrElseInners),
            acc_inst_stats_in_goal(MainGoal, !Stats),
            list.foldl(acc_inst_stats_in_goal, OrElseGoals, !Stats)
        ;
            Shorthand = try_goal(_MaybeIO, _ResultVar, SubGoal),
            acc_inst_stats_in_goal(SubGoal, !Stats)
        )
    ).

:- pred acc_inst_stats_in_case(case::in, inst_stats::in, inst_stats::out)
    is det.

acc_inst_stats_in_case(Case, !Stats) :-
    Case = case(_MainConsId, _OtherConsIds, Goal),
    acc_inst_stats_in_goal(Goal, !Stats).

%-----------------------------------------------------------------------------%

:- pred acc_inst_stats_in_unify_inst(pair(unify_inst_info, maybe_inst_det)::in,
    inst_stats::in, inst_stats::out) is det.

acc_inst_stats_in_unify_inst(Pair, !Stats) :-
    Pair = UnifyInstInfo - MaybeInstDet,
    UnifyInstInfo = unify_inst_info(_IsLive, _IsReal, InstA, InstB),
    acc_inst_stats_in_inst(InstA, !Stats),
    acc_inst_stats_in_inst(InstB, !Stats),
    (
        MaybeInstDet = inst_det_unknown
    ;
        MaybeInstDet = inst_det_known(Inst, _Det),
        acc_inst_stats_in_inst(Inst, !Stats)
    ).

:- pred acc_inst_stats_in_merge_inst(pair(merge_inst_info, maybe_inst)::in,
    inst_stats::in, inst_stats::out) is det.

acc_inst_stats_in_merge_inst(Pair, !Stats) :-
    Pair = MergeInstInfo - MaybeInst,
    MergeInstInfo = merge_inst_info(InstA, InstB),
    acc_inst_stats_in_inst(InstA, !Stats),
    acc_inst_stats_in_inst(InstB, !Stats),
    (
        MaybeInst = inst_unknown
    ;
        MaybeInst = inst_known(Inst),
        acc_inst_stats_in_inst(Inst, !Stats)
    ).

:- pred acc_inst_stats_in_ground_inst(
    pair(ground_inst_info, maybe_inst_det)::in,
    inst_stats::in, inst_stats::out) is det.

acc_inst_stats_in_ground_inst(Pair, !Stats) :-
    Pair = GroundInstInfo - MaybeInstDet,
    GroundInstInfo = ground_inst_info(_Name, _Uniq, _IsLive, _IsReal),
    (
        MaybeInstDet = inst_det_unknown
    ;
        MaybeInstDet = inst_det_known(Inst, _Det),
        acc_inst_stats_in_inst(Inst, !Stats)
    ).

:- pred acc_inst_stats_in_any_inst(pair(any_inst_info, maybe_inst_det)::in,
    inst_stats::in, inst_stats::out) is det.

acc_inst_stats_in_any_inst(Pair, !Stats) :-
    Pair = AnyInstInfo - MaybeInstDet,
    AnyInstInfo = any_inst_info(_Name, _Uniq, _IsLive, _IsReal),
    (
        MaybeInstDet = inst_det_unknown
    ;
        MaybeInstDet = inst_det_known(Inst, _Det),
        acc_inst_stats_in_inst(Inst, !Stats)
    ).

:- pred acc_inst_stats_in_named_inst(pair(inst_name, maybe_inst)::in,
    inst_stats::in, inst_stats::out) is det.

acc_inst_stats_in_named_inst(Pair, !Stats) :-
    Pair = _InstName - MaybeInst,
    (
        MaybeInst = inst_unknown
    ;
        MaybeInst = inst_known(Inst),
        acc_inst_stats_in_inst(Inst, !Stats)
    ).

%-----------------------------------------------------------------------------%

:- pred acc_inst_stats_in_inst(mer_inst::in, inst_stats::in, inst_stats::out)
    is det.

acc_inst_stats_in_inst(Inst, !Stats) :-
    (
        Inst = free,
        !Stats ^ is_free0 := !.Stats ^ is_free0 + 1
    ;
        Inst = any(_Uniq, HOInstInfo),
        !Stats ^ is_any := !.Stats ^ is_any + 1,
        acc_inst_stats_in_ho_inst_info(HOInstInfo, !Stats)
    ;
        Inst = bound(_Uniq, _TestResults, BoundInsts),
        !Stats ^ is_bound := !.Stats ^ is_bound + 1,
        list.foldl(acc_inst_stats_in_bound_inst, BoundInsts, !Stats)
    ;
        Inst = ground(_Uniq, HOInstInfo),
        !Stats ^ is_ground := !.Stats ^ is_ground + 1,
        acc_inst_stats_in_ho_inst_info(HOInstInfo, !Stats)
    ;
        Inst = not_reached,
        !Stats ^ is_notreached := !.Stats ^ is_notreached + 1
    ;
        Inst = inst_var(_Var),
        !Stats ^ is_var := !.Stats ^ is_var + 1
    ;
        Inst = constrained_inst_vars(_Vars, SubInst),
        !Stats ^ is_constrained := !.Stats ^ is_constrained + 1,
        acc_inst_stats_in_inst(SubInst, !Stats)
    ;
        Inst = defined_inst(_Name),
        !Stats ^ is_defined := !.Stats ^ is_defined + 1
    ).

:- pred acc_inst_stats_in_bound_inst(bound_inst::in,
    inst_stats::in, inst_stats::out) is det.

acc_inst_stats_in_bound_inst(BoundInst, !Stats) :-
    BoundInst = bound_functor(_ConsId, ArgInsts),
    list.foldl(acc_inst_stats_in_inst, ArgInsts, !Stats).

:- pred acc_inst_stats_in_ho_inst_info(ho_inst_info::in,
    inst_stats::in, inst_stats::out) is det.

acc_inst_stats_in_ho_inst_info(HOInstInfo, !Stats) :-
    (
        HOInstInfo = higher_order(PredInstInfo),
        PredInstInfo = pred_inst_info(_PredOrFunc, Modes, _RegTypes, _Det),
        list.foldl(acc_inst_stats_in_mode, Modes, !Stats)
    ;
        HOInstInfo = none_or_default_func
    ).

:- pred acc_inst_stats_in_mode(mer_mode::in,
    inst_stats::in, inst_stats::out) is det.

acc_inst_stats_in_mode(Mode, !Stats) :-
    (
        Mode = from_to_mode(InitInst, FinalInst),
        acc_inst_stats_in_inst(InitInst, !Stats),
        acc_inst_stats_in_inst(FinalInst, !Stats)
    ;
        Mode = user_defined_mode(_SymName, ArgInsts),
        list.foldl(acc_inst_stats_in_inst, ArgInsts, !Stats)
    ).

%-----------------------------------------------------------------------------%
:- end_module hlds.hlds_statistics.
%-----------------------------------------------------------------------------%
