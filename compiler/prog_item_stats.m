%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: prog_item_stats.m.
%
% This module has facilities for gathering and printing statistics
% about how often different kinds of items and goals occur in parse trees.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.prog_item_stats.
:- interface.

:- import_module parse_tree.prog_item.

:- import_module io.

%-----------------------------------------------------------------------------%

    % Gather statistics about the given augmented compilation unit,
    % and write them to the given stream.
    %
:- pred gather_and_write_item_stats(io.output_stream::in,
    aug_compilation_unit::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.maybe_error.

:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module string.

%-----------------------------------------------------------------------------%

gather_and_write_item_stats(Stream, AugCompUnit, !IO) :-
    gather_stats_in_aug_comp_unit(AugCompUnit, CompUnitStats),
    write_comp_unit_stats(Stream, AugCompUnit ^ aci_module_name,
        CompUnitStats, !IO).

%-----------------------------------------------------------------------------%

:- type item_stats
    --->    item_stats(
                % Each field contains the number of occurrences
                % of the named kind of item.
                %
                % We gather separate stats for some but not all kinds of
                % pragmas. For the ones we don't track individually, we
                % do record whether they are of a kind that is currently
                % processed in pass 2 or pass 3 by make_hlds_passes.m.
                item_num_clauses                    :: int,
                item_num_type_defn                  :: int,
                item_num_inst_defn                  :: int,
                item_num_mode_defn                  :: int,
                item_num_pred_decl                  :: int,
                item_num_mode_decl                  :: int,
                item_num_fim                        :: int,
                item_num_pragma_term                :: int,
                item_num_pragma_term2               :: int,
                item_num_pragma_exceptions          :: int,
                item_num_pragma_trailing            :: int,
                item_num_pragma_mm_tabling          :: int,
                item_num_pragma_other_pass_2        :: int,
                item_num_pragma_other_pass_3        :: int,
                item_num_promise                    :: int,
                item_num_typeclass                  :: int,
                item_num_instance                   :: int,
                item_num_initialise                 :: int,
                item_num_finalise                   :: int,
                item_num_mutable                    :: int,
                item_num_type_repn                  :: int
            ).

:- type goal_stats
    --->    goal_stats(
                % Each field contains the number of occurrences
                % of the named kind of goal.
                goal_num_conj                       :: int,
                goal_num_par_conj                   :: int,
                goal_num_true                       :: int,
                goal_num_disj                       :: int,
                goal_num_fail                       :: int,
                goal_num_some                       :: int,
                goal_num_all                        :: int,
                goal_num_some_state_vars            :: int,
                goal_num_all_state_vars             :: int,
                goal_num_promise_purity             :: int,
                goal_num_promise_eqv_solns          :: int,
                goal_num_promise_eqv_sets           :: int,
                goal_num_promise_arbitrary          :: int,
                goal_num_req_detism                 :: int,
                goal_num_req_compl_switch           :: int,
                goal_num_req_arm_detism             :: int,
                goal_num_disable_warnings           :: int,
                goal_num_trace                      :: int,
                goal_num_atomic                     :: int,
                goal_num_try                        :: int,
                goal_num_implies                    :: int,
                goal_num_equivalent                 :: int,
                goal_num_not                        :: int,
                goal_num_if_then_else               :: int,
                goal_num_event                      :: int,
                goal_num_call                       :: int,
                goal_num_unify                      :: int
            ).

:- type section_stats
    --->    section_stats(item_stats, goal_stats).

:- type comp_unit_stats == map(string, section_stats).

%-----------------------------------------------------------------------------%

    % Initialize an item_stats structure.
    %
:- func init_item_stats = item_stats.

init_item_stats =
    item_stats(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0).

    % Initialize a goal_stats structure.
    %
:- func init_goal_stats = goal_stats.

init_goal_stats =
    goal_stats(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0).

%-----------------------------------------------------------------------------%

    % Update an item_stats structure based on the contents of the given
    % item block.
    %
:- pred gather_stats_in_aug_comp_unit(aug_compilation_unit::in,
    comp_unit_stats::out) is det.

gather_stats_in_aug_comp_unit(AugCompUnit, !:CompUnitStats) :-
    AugCompUnit = aug_compilation_unit(_ModuleName, _ModuleNameContext,
        _ModuleVersionNumbers, SrcItemBlocks,
        DirectIntItemBlocks, IndirectIntItemBlocks,
        OptItemBlocks, IntForOptItemBlocks),

    map.init(!:CompUnitStats),
    gather_stats_in_item_blocks(section_name_src, SrcItemBlocks,
        !CompUnitStats),
    gather_stats_in_item_blocks(section_name_int, DirectIntItemBlocks,
        !CompUnitStats),
    gather_stats_in_item_blocks(section_name_int, IndirectIntItemBlocks,
        !CompUnitStats),
    gather_stats_in_item_blocks(section_name_int_for_opt, IntForOptItemBlocks,
        !CompUnitStats),
    gather_stats_in_item_blocks(section_name_opt, OptItemBlocks,
        !CompUnitStats).

:- func section_name_src(src_module_section) = string.
section_name_src(sms_interface) = "src_int".
section_name_src(sms_implementation) = "src_impl".
section_name_src(sms_impl_but_exported_to_submodules) = "src_impl_sub".

:- func section_name_int(int_module_section) = string.
section_name_int(ims_imported_or_used(_, _, _, iou_imported)) = "int_imported".
section_name_int(ims_imported_or_used(_, _, _, iou_used)) = "int_used".
section_name_int(ims_imported_or_used(_, _, _, iou_used_and_imported)) =
    "int_used_and_imported".
section_name_int(ims_abstract_imported(_, _)) = "int_abstract_imported".

:- func section_name_opt(opt_module_section) = string.
section_name_opt(oms_opt_imported(_, _)) = "opt_imported".

:- func section_name_int_for_opt(int_for_opt_module_section) = string.
section_name_int_for_opt(ioms_opt_imported(_, _)) = "int_for_opt_imported".

%-----------------------------------------------------------------------------%

    % Update an item_stats structure based on the contents of the given
    % item block.
    %
:- pred gather_stats_in_item_blocks((func(MS) = string)::in,
    list(item_block(MS))::in,
    comp_unit_stats::in, comp_unit_stats::out) is det.

gather_stats_in_item_blocks(_, [], !CompUnitStats).
gather_stats_in_item_blocks(SectionFunc, [ItemBlock | ItemBlocks],
        !CompUnitStats) :-
    ItemBlock = item_block(_, Section, _, _, _, Items),
    SectionName = SectionFunc(Section),
    ( if map.search(!.CompUnitStats, SectionName, SectionStats0) then
        SectionStats0 = section_stats(ItemStats0, GoalStats0),
        gather_stats_in_items(Items,
            ItemStats0, ItemStats, GoalStats0, GoalStats),
        SectionStats = section_stats(ItemStats, GoalStats),
        map.det_update(SectionName, SectionStats, !CompUnitStats)
    else
        gather_stats_in_items(Items,
            init_item_stats, ItemStats, init_goal_stats, GoalStats),
        SectionStats = section_stats(ItemStats, GoalStats),
        map.det_insert(SectionName, SectionStats, !CompUnitStats)
    ),
    gather_stats_in_item_blocks(SectionFunc, ItemBlocks, !CompUnitStats).

:- pred gather_stats_in_items(list(item)::in,
    item_stats::in, item_stats::out, goal_stats::in, goal_stats::out) is det.

gather_stats_in_items([], !ItemStats, !GoalStats).
gather_stats_in_items([Item | Items], !ItemStats, !GoalStats) :-
    gather_stats_in_item(Item, !ItemStats, !GoalStats),
    gather_stats_in_items(Items, !ItemStats, !GoalStats).

:- pred gather_stats_in_item(item::in,
    item_stats::in, item_stats::out,
    goal_stats::in, goal_stats::out) is det.

gather_stats_in_item(Item, !ItemStats, !GoalStats) :-
    (
        Item = item_clause(ItemClauseInfo),
        !ItemStats ^ item_num_clauses := !.ItemStats ^ item_num_clauses + 1,

        ItemClauseInfo = item_clause_info(_, _, _, _, _, MaybeGoal, _, _),
        (
            MaybeGoal = ok1(Goal),
            gather_stats_in_goal(Goal, !GoalStats)
        ;
            MaybeGoal = error1(_)
        )
    ;
        Item = item_type_defn(_),
        !ItemStats ^ item_num_type_defn := !.ItemStats ^ item_num_type_defn + 1
    ;
        Item = item_inst_defn(_),
        !ItemStats ^ item_num_inst_defn := !.ItemStats ^ item_num_inst_defn + 1
    ;
        Item = item_mode_defn(_),
        !ItemStats ^ item_num_mode_defn := !.ItemStats ^ item_num_mode_defn + 1
    ;
        Item = item_pred_decl(_),
        !ItemStats ^ item_num_pred_decl := !.ItemStats ^ item_num_pred_decl + 1
    ;
        Item = item_mode_decl(_),
        !ItemStats ^ item_num_mode_decl := !.ItemStats ^ item_num_mode_decl + 1
    ;
        Item = item_pragma(ItemPragmaInfo),
        gather_stats_in_item_pragma(ItemPragmaInfo, !ItemStats)
    ;
        Item = item_promise(_),
        !ItemStats ^ item_num_promise := !.ItemStats ^ item_num_promise + 1
    ;
        Item = item_typeclass(_),
        !ItemStats ^ item_num_typeclass := !.ItemStats ^ item_num_typeclass + 1
    ;
        Item = item_instance(_),
        !ItemStats ^ item_num_instance := !.ItemStats ^ item_num_instance + 1
    ;
        Item = item_initialise(_),
        !ItemStats ^ item_num_initialise :=
            !.ItemStats ^ item_num_initialise + 1
    ;
        Item = item_finalise(_),
        !ItemStats ^ item_num_finalise := !.ItemStats ^ item_num_finalise + 1
    ;
        Item = item_mutable(_),
        !ItemStats ^ item_num_mutable := !.ItemStats ^ item_num_mutable + 1
    ;
        Item = item_foreign_import_module(_),
        !ItemStats ^ item_num_fim := !.ItemStats ^ item_num_fim + 1
    ;
        Item = item_type_repn(_),
        !ItemStats ^ item_num_type_repn := !.ItemStats ^ item_num_type_repn + 1
    ).

:- pred gather_stats_in_item_pragma(item_pragma_info::in,
    item_stats::in, item_stats::out) is det.

gather_stats_in_item_pragma(ItemPragmaInfo, !ItemStats) :-
    ItemPragmaInfo = item_pragma_info(PragmaType, _, _, _),
    (
        PragmaType = pragma_termination_info(_),
        !ItemStats ^ item_num_pragma_term :=
            !.ItemStats ^ item_num_pragma_term + 1
    ;
        PragmaType = pragma_termination2_info(_),
        !ItemStats ^ item_num_pragma_term2 :=
            !.ItemStats ^ item_num_pragma_term2 + 1
    ;
        PragmaType = pragma_exceptions(_),
        !ItemStats ^ item_num_pragma_exceptions :=
            !.ItemStats ^ item_num_pragma_exceptions + 1
    ;
        PragmaType = pragma_trailing_info(_),
        !ItemStats ^ item_num_pragma_trailing :=
            !.ItemStats ^ item_num_pragma_trailing + 1
    ;
        PragmaType = pragma_mm_tabling_info(_),
        !ItemStats ^ item_num_pragma_mm_tabling :=
            !.ItemStats ^ item_num_pragma_mm_tabling + 1
    ;
        ( PragmaType = pragma_foreign_decl(_)
        ; PragmaType = pragma_foreign_code(_)
        ; PragmaType = pragma_foreign_export_enum(_)
        ; PragmaType = pragma_foreign_enum(_)
        ; PragmaType = pragma_external_proc(_)
        ; PragmaType = pragma_inline(_)
        ; PragmaType = pragma_no_inline(_)
        ; PragmaType = pragma_consider_used(_)
        ; PragmaType = pragma_unused_args(_)
        ; PragmaType = pragma_obsolete(_)
        ; PragmaType = pragma_no_detism_warning(_)
        ; PragmaType = pragma_promise_eqv_clauses(_)
        ; PragmaType = pragma_promise_pure(_)
        ; PragmaType = pragma_promise_semipure(_)
        ; PragmaType = pragma_terminates(_)
        ; PragmaType = pragma_does_not_terminate(_)
        ; PragmaType = pragma_check_termination(_)
        ; PragmaType = pragma_mode_check_clauses(_)
        ; PragmaType = pragma_require_feature_set(_)
        ; PragmaType = pragma_require_tail_recursion(_)
        ),
        !ItemStats ^ item_num_pragma_other_pass_2 :=
            !.ItemStats ^ item_num_pragma_other_pass_2 + 1
    ;
        ( PragmaType = pragma_foreign_proc(_)
        ; PragmaType = pragma_type_spec(_)
        ; PragmaType = pragma_tabled(_)
        ; PragmaType = pragma_fact_table(_)
        ; PragmaType = pragma_oisu(_)
        ; PragmaType = pragma_foreign_proc_export(_)
        ; PragmaType = pragma_structure_sharing(_)
        ; PragmaType = pragma_structure_reuse(_)
        ),
        !ItemStats ^ item_num_pragma_other_pass_3 :=
            !.ItemStats ^ item_num_pragma_other_pass_3 + 1
    ).

%-----------------------------------------------------------------------------%

:- pred gather_stats_in_goals(list(goal)::in,
    goal_stats::in, goal_stats::out) is det.

gather_stats_in_goals([], !GoalStats).
gather_stats_in_goals([Goal | Goals], !GoalStats) :-
    gather_stats_in_goal(Goal, !GoalStats),
    gather_stats_in_goals(Goals, !GoalStats).

:- pred gather_stats_in_goal(goal::in, goal_stats::in, goal_stats::out) is det.

gather_stats_in_goal(Goal, !GoalStats) :-
    (
        Goal = conj_expr(_, SubGoalA, SubGoalB),
        !GoalStats ^ goal_num_conj := !.GoalStats ^ goal_num_conj + 1,
        gather_stats_in_goal(SubGoalA, !GoalStats),
        gather_stats_in_goal(SubGoalB, !GoalStats)
    ;
        Goal = par_conj_expr(_, SubGoalA, SubGoalB),
        !GoalStats ^ goal_num_par_conj := !.GoalStats ^ goal_num_par_conj + 1,
        gather_stats_in_goal(SubGoalA, !GoalStats),
        gather_stats_in_goal(SubGoalB, !GoalStats)
    ;
        Goal = true_expr(_),
        !GoalStats ^ goal_num_true := !.GoalStats ^ goal_num_true + 1
    ;
        Goal = disj_expr(_, SubGoalA, SubGoalB),
        !GoalStats ^ goal_num_disj := !.GoalStats ^ goal_num_disj + 1,
        gather_stats_in_goal(SubGoalA, !GoalStats),
        gather_stats_in_goal(SubGoalB, !GoalStats)
    ;
        Goal = fail_expr(_),
        !GoalStats ^ goal_num_fail := !.GoalStats ^ goal_num_fail + 1
    ;
        Goal = quant_expr(QuantType, QuantVarsKind, _, _, SubGoal),
        (
            QuantType = quant_some,
            QuantVarsKind = quant_ordinary_vars,
            !GoalStats ^ goal_num_some := !.GoalStats ^ goal_num_some + 1
        ;
            QuantType = quant_some,
            QuantVarsKind = quant_state_vars,
            !GoalStats ^ goal_num_some_state_vars :=
                !.GoalStats ^ goal_num_some_state_vars + 1
        ;
            QuantType = quant_all,
            QuantVarsKind = quant_ordinary_vars,
            !GoalStats ^ goal_num_all := !.GoalStats ^ goal_num_all + 1
        ;
            QuantType = quant_all,
            QuantVarsKind = quant_state_vars,
            !GoalStats ^ goal_num_all_state_vars :=
                !.GoalStats ^ goal_num_all_state_vars + 1
        ),
        gather_stats_in_goal(SubGoal, !GoalStats)
    ;
        Goal = promise_purity_expr(_, _, SubGoal),
        !GoalStats ^ goal_num_promise_purity :=
            !.GoalStats ^ goal_num_promise_purity + 1,
        gather_stats_in_goal(SubGoal, !GoalStats)
    ;
        Goal = promise_equivalent_solutions_expr(_, _, _, _, _, SubGoal),
        !GoalStats ^ goal_num_promise_eqv_solns :=
            !.GoalStats ^ goal_num_promise_eqv_solns + 1,
        gather_stats_in_goal(SubGoal, !GoalStats)
    ;
        Goal = promise_equivalent_solution_sets_expr(_, _, _, _, _, SubGoal),
        !GoalStats ^ goal_num_promise_eqv_sets :=
            !.GoalStats ^ goal_num_promise_eqv_sets + 1,
        gather_stats_in_goal(SubGoal, !GoalStats)
    ;
        Goal = promise_equivalent_solution_arbitrary_expr(_, _, _, _, _,
            SubGoal),
        !GoalStats ^ goal_num_promise_arbitrary :=
            !.GoalStats ^ goal_num_promise_arbitrary + 1,
        gather_stats_in_goal(SubGoal, !GoalStats)
    ;
        Goal = require_detism_expr(_, _, SubGoal),
        !GoalStats ^ goal_num_req_detism :=
            !.GoalStats ^ goal_num_req_detism + 1,
        gather_stats_in_goal(SubGoal, !GoalStats)
    ;
        Goal = require_complete_switch_expr(_, _, SubGoal),
        !GoalStats ^ goal_num_req_compl_switch :=
            !.GoalStats ^ goal_num_req_compl_switch + 1,
        gather_stats_in_goal(SubGoal, !GoalStats)
    ;
        Goal = require_switch_arms_detism_expr(_, _, _, SubGoal),
        !GoalStats ^ goal_num_req_arm_detism :=
            !.GoalStats ^ goal_num_req_arm_detism + 1,
        gather_stats_in_goal(SubGoal, !GoalStats)
    ;
        Goal = disable_warnings_expr(_, _, _, SubGoal),
        !GoalStats ^ goal_num_disable_warnings :=
            !.GoalStats ^ goal_num_disable_warnings + 1,
        gather_stats_in_goal(SubGoal, !GoalStats)
    ;
        Goal = trace_expr(_, _, _, _, _, SubGoal),
        !GoalStats ^ goal_num_trace := !.GoalStats ^ goal_num_trace + 1,
        gather_stats_in_goal(SubGoal, !GoalStats)
    ;
        Goal = atomic_expr(_, _, _, _, MainGoal, OrElseGoals),
        !GoalStats ^ goal_num_atomic := !.GoalStats ^ goal_num_atomic+ 1,
        gather_stats_in_goal(MainGoal, !GoalStats),
        gather_stats_in_goals(OrElseGoals, !GoalStats)
    ;
        Goal = try_expr(_, _, MainGoal, ThenGoal, MaybeElseGoal,
            Catches, MaybeCatchAny),
        !GoalStats ^ goal_num_try := !.GoalStats ^ goal_num_try + 1,
        gather_stats_in_goal(MainGoal, !GoalStats),
        gather_stats_in_goal(ThenGoal, !GoalStats),
        (
            MaybeElseGoal = yes(ElseGoal),
            gather_stats_in_goal(ElseGoal, !GoalStats)
        ;
            MaybeElseGoal = no
        ),
        gather_stats_in_catch_exprs(Catches, !GoalStats),
        (
            MaybeCatchAny = yes(catch_any_expr(_, CatchAnyGoal)),
            gather_stats_in_goal(CatchAnyGoal, !GoalStats)
        ;
            MaybeCatchAny = no
        )
    ;
        Goal = implies_expr(_, SubGoalA, SubGoalB),
        !GoalStats ^ goal_num_implies := !.GoalStats ^ goal_num_implies + 1,
        gather_stats_in_goal(SubGoalA, !GoalStats),
        gather_stats_in_goal(SubGoalB, !GoalStats)
    ;
        Goal = equivalent_expr(_, SubGoalA, SubGoalB),
        !GoalStats ^ goal_num_equivalent :=
            !.GoalStats ^ goal_num_equivalent + 1,
        gather_stats_in_goal(SubGoalA, !GoalStats),
        gather_stats_in_goal(SubGoalB, !GoalStats)
    ;
        Goal = not_expr(_, SubGoal),
        !GoalStats ^ goal_num_not := !.GoalStats ^ goal_num_not + 1,
        gather_stats_in_goal(SubGoal, !GoalStats)
    ;
        Goal = if_then_else_expr(_, _, _, CondGoal, ThenGoal, ElseGoal),
        !GoalStats ^ goal_num_if_then_else :=
            !.GoalStats ^ goal_num_if_then_else + 1,
        gather_stats_in_goal(CondGoal, !GoalStats),
        gather_stats_in_goal(ThenGoal, !GoalStats),
        gather_stats_in_goal(ElseGoal, !GoalStats)
    ;
        Goal = event_expr(_, _, _),
        !GoalStats ^ goal_num_event := !.GoalStats ^ goal_num_event + 1
    ;
        Goal = call_expr(_, _, _, _),
        !GoalStats ^ goal_num_call := !.GoalStats ^ goal_num_call + 1
    ;
        Goal = unify_expr(_, _, _, _),
        !GoalStats ^ goal_num_unify := !.GoalStats ^ goal_num_unify + 1
    ).

:- pred gather_stats_in_catch_exprs(list(catch_expr)::in,
    goal_stats::in, goal_stats::out) is det.

gather_stats_in_catch_exprs([], !GoalStats).
gather_stats_in_catch_exprs([CatchExpr | CatchExprs], !GoalStats) :-
    CatchExpr = catch_expr(_, Goal),
    gather_stats_in_goal(Goal, !GoalStats),
    gather_stats_in_catch_exprs(CatchExprs, !GoalStats).

%-----------------------------------------------------------------------------%

:- pred write_comp_unit_stats(io.output_stream::in, module_name::in,
    comp_unit_stats::in, io::di, io::uo) is det.

write_comp_unit_stats(Stream, ModuleName, CompUnitStats, !IO) :-
    io.format(Stream, "MODULE %s\n", [s(sym_name_to_string(ModuleName))], !IO),
    map.to_assoc_list(CompUnitStats, SectionStatPairs),
    list.foldl(write_section_stats(Stream), SectionStatPairs, !IO).

:- pred write_section_stats(io.output_stream::in,
    pair(string, section_stats)::in, io::di, io::uo) is det.

write_section_stats(Stream, SectionName - SectionStats, !IO) :-
    SectionStats = section_stats(ItemStats, GoalStats),
    write_item_stats(Stream, SectionName, ItemStats, !IO),
    write_goal_stats(Stream, SectionName, GoalStats, !IO).

:- pred write_item_stats(io.output_stream::in, string::in, item_stats::in,
    io::di, io::uo) is det.

write_item_stats(Stream, SectionName, ItemStats, !IO) :-
    ItemStats = item_stats(Clause, TypeDefn, InstDefn, ModeDefn,
        PredDecl, ModeDecl, FIM, PragmaTerm, PragmaTerm2,
        PragmaExcp, PragmaTrail, PragmaMM, PragmaPass2, PragmaPass3,
        Promise, Typeclasse, Instance, Initialise, Finalise, Mutable,
        TypeRepn),
    write_one_stat(Stream, SectionName, "item_clause", Clause, !IO),
    write_one_stat(Stream, SectionName, "item_type_defn", TypeDefn, !IO),
    write_one_stat(Stream, SectionName, "item_inst_defn", InstDefn, !IO),
    write_one_stat(Stream, SectionName, "item_mode_defn", ModeDefn, !IO),
    write_one_stat(Stream, SectionName, "item_pred_decl", PredDecl, !IO),
    write_one_stat(Stream, SectionName, "item_mode_decl", ModeDecl, !IO),
    write_one_stat(Stream, SectionName, "item_fim", FIM, !IO),
    write_one_stat(Stream, SectionName, "item_pragma_term", PragmaTerm, !IO),
    write_one_stat(Stream, SectionName, "item_pragma_term2", PragmaTerm2, !IO),
    write_one_stat(Stream, SectionName, "item_pragma_excp", PragmaExcp, !IO),
    write_one_stat(Stream, SectionName, "item_pragma_trail", PragmaTrail, !IO),
    write_one_stat(Stream, SectionName, "item_pragma_mm", PragmaMM, !IO),
    write_one_stat(Stream, SectionName, "item_pragma_pass2", PragmaPass2, !IO),
    write_one_stat(Stream, SectionName, "item_pragma_pass3", PragmaPass3, !IO),
    write_one_stat(Stream, SectionName, "item_promise", Promise, !IO),
    write_one_stat(Stream, SectionName, "item_typeclass", Typeclasse, !IO),
    write_one_stat(Stream, SectionName, "item_instance", Instance, !IO),
    write_one_stat(Stream, SectionName, "item_promise", Promise, !IO),
    write_one_stat(Stream, SectionName, "item_initialise", Initialise, !IO),
    write_one_stat(Stream, SectionName, "item_finalise", Finalise, !IO),
    write_one_stat(Stream, SectionName, "item_mutable", Mutable, !IO),
    write_one_stat(Stream, SectionName, "item_type_repn", TypeRepn, !IO).

:- pred write_goal_stats(io.output_stream::in, string::in, goal_stats::in,
    io::di, io::uo) is det.

write_goal_stats(Stream, SectionName, GoalStats, !IO) :-
    GoalStats = goal_stats(Conj, ParConj, True, Disj, Fail,
        Some, All, SomeStateVars, AllStateVars,
        PromisePurity, PromiseEqvSolns, PromiseEqvSolnSets, PromiseArbitrary,
        ReqDetism, ReqComplSwitch, ReqSwitchArmDetism, DisableWarnings,
        Trace, Atomic, Try, Implies, Equivalent, Not, IfThenElse,
        Event, Call, Unify),
    write_one_stat(Stream, SectionName, "goal_conj", Conj, !IO),
    write_one_stat(Stream, SectionName, "goal_par_conj", ParConj, !IO),
    write_one_stat(Stream, SectionName, "goal_true", True, !IO),
    write_one_stat(Stream, SectionName, "goal_disj", Disj, !IO),
    write_one_stat(Stream, SectionName, "goal_fail", Fail, !IO),
    write_one_stat(Stream, SectionName, "goal_some", Some, !IO),
    write_one_stat(Stream, SectionName, "goal_all", All, !IO),
    write_one_stat(Stream, SectionName, "goal_some_state_vars",
        SomeStateVars, !IO),
    write_one_stat(Stream, SectionName, "goal_all_state_vars",
        AllStateVars, !IO),
    write_one_stat(Stream, SectionName, "goal_pro_purity", PromisePurity, !IO),
    write_one_stat(Stream, SectionName, "goal_pro_eqv_solns",
        PromiseEqvSolns, !IO),
    write_one_stat(Stream, SectionName, "goal_pro_eqv_soln_sets",
        PromiseEqvSolnSets, !IO),
    write_one_stat(Stream, SectionName, "goal_pro_arbitrary",
        PromiseArbitrary, !IO),
    write_one_stat(Stream, SectionName, "goal_req_detism", ReqDetism, !IO),
    write_one_stat(Stream, SectionName, "goal_req_compl_switch",
        ReqComplSwitch, !IO),
    write_one_stat(Stream, SectionName, "goal_req_arm_detism",
        ReqSwitchArmDetism, !IO),
    write_one_stat(Stream, SectionName, "goal_disable_warnings",
        DisableWarnings, !IO),
    write_one_stat(Stream, SectionName, "goal_trace", Trace, !IO),
    write_one_stat(Stream, SectionName, "goal_atomic", Atomic, !IO),
    write_one_stat(Stream, SectionName, "goal_try", Try, !IO),
    write_one_stat(Stream, SectionName, "goal_implies", Implies, !IO),
    write_one_stat(Stream, SectionName, "goal_equivalent", Equivalent, !IO),
    write_one_stat(Stream, SectionName, "goal_not", Not, !IO),
    write_one_stat(Stream, SectionName, "goal_if_then_else", IfThenElse, !IO),
    write_one_stat(Stream, SectionName, "goal_event", Event, !IO),
    write_one_stat(Stream, SectionName, "goal_call", Call, !IO),
    write_one_stat(Stream, SectionName, "goal_unify", Unify, !IO).

:- pred write_one_stat(io.output_stream::in, string::in, string::in, int::in,
    io::di, io::uo) is det.

write_one_stat(Stream, SectionName, StatName, StatNum, !IO) :-
    io.write_string(Stream, SectionName, !IO),
    io.write_string(Stream, " ", !IO),
    io.write_string(Stream, StatName, !IO),
    io.write_string(Stream, " ", !IO),
    io.write_int(Stream, StatNum, !IO),
    io.nl(Stream, !IO),
    io.flush_output(Stream, !IO).

%-----------------------------------------------------------------------------%
:- end_module parse_tree.prog_item_stats.
%-----------------------------------------------------------------------------%
