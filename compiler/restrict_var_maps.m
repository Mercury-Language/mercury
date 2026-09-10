%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2009-2012 The University of Melbourne.
% Copyright (C) 2013-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: restrict_var_maps.m.
% Main author: zs.
%
% This module has the job of restricting a procedure's var_tables and
% rtti_varmaps structures to just the variables occurring in a its
% argument list and body goal.
%
%---------------------------------------------------------------------------%

:- module hlds.restrict_var_maps.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_rtti.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.var_table.

:- import_module list.

%---------------------------------------------------------------------------%

    % The proc_info has several maps that refer to variables. After lambda
    % expansion, both the newly created procedures and the original procedure
    % that they were carved out of have duplicate copies of these maps.
    % This duplication is a problem because later passes (in particular,
    % the equiv_types_hlds pass) iterate over the entries in these maps,
    % and if an entry is duplicated N times, they have to process it N times.
    % The task of this predicate is to eliminate unnecessary entries
    % from the var_table, and this requires also eliminating them from
    % the rtti_varmaps.
    %
    % We could in theory restrict the tvarset and the inst_varset as well,
    % but since we don't iterate over those sets, there is (as yet) no need
    % for this.
    %
:- pred restrict_var_maps(list(prog_var)::in, hlds_goal::in,
    var_table::in, var_table::out, rtti_varmaps::in, rtti_varmaps::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.pred_proc_id.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.

:- import_module array.
:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module term.

%---------------------------------------------------------------------------%

restrict_var_maps(HeadVars, Goal, !VarTable, !RttiVarMaps) :-
    % This code was added to the compiler (in what was then lambda.m)
    % in 2009 in commit 62d7496a7e8cb11403b55433309edf8efb44c6b2
    % with the aim of eliminating some horrible worst-case behavior.
    %
    % The reason for the use here of an array of bools to represent
    % whether our knowledge of which variables are used and which are not,
    % instead of simply a set_of_var, is the fact that for some severe
    % stress-test inputs, we judged the extra performance of this data
    % structure compared to sparse bitsets to be worthwhile.

    var_table_max_var_num(!.VarTable, MaxVarNum),
    % Variable numbers go from 1 to MaxVarNum. Reserve array slots
    % from 0 to MaxVarNum, since wasting the space of one array element
    % is preferable to having to do a subtraction on every array lookup.
    array.init(MaxVarNum + 1, no, VarUses0),
    mark_vars_as_used(HeadVars, VarUses0, VarUses1),
    find_used_vars_in_goal(Goal, VarUses1, VarUses),

    var_table_to_sorted_assoc_list(!.VarTable, VarTableEntries0),
    filter_var_table_entries(VarTableEntries0, VarUses,
        [], RevVarTableEntries),
    var_table_from_rev_sorted_assoc_list(RevVarTableEntries, !:VarTable),

    restrict_rtti_varmaps(VarUses, !RttiVarMaps).

:- pred find_used_vars_in_goal(hlds_goal::in,
    array(bool)::array_di, array(bool)::array_uo) is det.

find_used_vars_in_goal(Goal, !VarUses) :-
    Goal = hlds_goal(GoalExpr, _GoalInfo),
    (
        GoalExpr = unify(LHSVar, RHS, _, Unif, _),
        mark_var_as_used(LHSVar, !VarUses),
        (
            Unif = construct(_, _, _, _, CellToReuse, _, _),
            ( if CellToReuse = reuse_cell(cell_to_reuse(ReuseVar, _, _)) then
                mark_var_as_used(ReuseVar, !VarUses)
            else
                true
            )
        ;
            ( Unif = deconstruct(_, _, _, _, _, _)
            ; Unif = assign(_, _)
            ; Unif = simple_test(_, _)
            ; Unif = complicated_unify(_, _, _)
            )
        ),
        find_used_vars_in_unify_rhs(RHS, !VarUses)
    ;
        GoalExpr = generic_call(GenericCall, ArgVars, _, _, _),
        (
            GenericCall = higher_order(Var, _, _, _, _),
            mark_var_as_used(Var, !VarUses)
        ;
            GenericCall = class_method(Var, _, _, _),
            mark_var_as_used(Var, !VarUses)
        ;
            GenericCall = event_call(_)
        ;
            GenericCall = cast(_)
        ),
        mark_vars_as_used(ArgVars, !VarUses)
    ;
        GoalExpr = plain_call(_, _, ArgVars, _, MaybeCallUnifyContext, _),
        mark_vars_as_used(ArgVars, !VarUses),
        (
            MaybeCallUnifyContext = no
        ;
            MaybeCallUnifyContext = yes(CallUnifyContext),
            CallUnifyContext = call_unify_context(Var, RHS, _UC),
            mark_var_as_used(Var, !VarUses),
            find_used_vars_in_unify_rhs(RHS, !VarUses)
        )
    ;
        ( GoalExpr = conj(_, Goals)
        ; GoalExpr = disj(Goals)
        ),
        find_used_vars_in_goals(Goals, !VarUses)
    ;
        GoalExpr = switch(Var, _Det, Cases),
        mark_var_as_used(Var, !VarUses),
        find_used_vars_in_cases(Cases, !VarUses)
    ;
        GoalExpr = scope(Reason, SubGoal),
        (
            Reason = exist_quant(Vars, _),
            mark_vars_as_used(Vars, !VarUses)
        ;
            Reason = promise_solutions(Vars, _),
            mark_vars_as_used(Vars, !VarUses)
        ;
            Reason = from_ground_term(Var, _),
            mark_var_as_used(Var, !VarUses)
        ;
            Reason = loop_control(LCVar, LCSVar, _),
            mark_var_as_used(LCVar, !VarUses),
            mark_var_as_used(LCSVar, !VarUses)
        ;
            ( Reason = disable_warnings(_, _)
            ; Reason = promise_purity(_)
            ; Reason = barrier(_)
            ; Reason = commit(_)
            ; Reason = trace_goal(_, _, _, _, _)
            )
            % Do nothing.
        ;
            ( Reason = require_detism(_)
            ; Reason = require_complete_switch(_)
            ; Reason = require_switch_arms_detism(_, _)
            ),
            % These scopes should have been deleted by now.
            unexpected($pred, "unexpected scope")
        ),
        find_used_vars_in_goal(SubGoal, !VarUses)
    ;
        GoalExpr = negation(SubGoal),
        find_used_vars_in_goal(SubGoal, !VarUses)
    ;
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        mark_vars_as_used(Vars, !VarUses),
        find_used_vars_in_goal(Cond, !VarUses),
        find_used_vars_in_goal(Then, !VarUses),
        find_used_vars_in_goal(Else, !VarUses)
    ;
        GoalExpr = call_foreign_proc(_, _, _, Args, ExtraArgs, _, _),
        ArgVars = list.map(foreign_arg_var, Args),
        ExtraVars = list.map(foreign_arg_var, ExtraArgs),
        mark_vars_as_used(ArgVars, !VarUses),
        mark_vars_as_used(ExtraVars, !VarUses)
    ;
        GoalExpr = shorthand(Shorthand),
        (
            Shorthand = atomic_goal(_, Outer, Inner, MaybeOutputVars,
                MainGoal, OrElseGoals, _),
            Outer = atomic_interface_vars(OuterDI, OuterUO),
            mark_var_as_used(OuterDI, !VarUses),
            mark_var_as_used(OuterUO, !VarUses),
            Inner = atomic_interface_vars(InnerDI, InnerUO),
            mark_var_as_used(InnerDI, !VarUses),
            mark_var_as_used(InnerUO, !VarUses),
            (
                MaybeOutputVars = no
            ;
                MaybeOutputVars = yes(OutputVars),
                mark_vars_as_used(OutputVars, !VarUses)
            ),
            find_used_vars_in_goal(MainGoal, !VarUses),
            find_used_vars_in_goals(OrElseGoals, !VarUses)
        ;
            Shorthand = try_goal(_, _, SubGoal),
            % The IO and Result variables would be in SubGoal.
            find_used_vars_in_goal(SubGoal, !VarUses)
        ;
            Shorthand = bi_implication(LeftGoal, RightGoal),
            find_used_vars_in_goal(LeftGoal, !VarUses),
            find_used_vars_in_goal(RightGoal, !VarUses)
        )
    ).

:- pred find_used_vars_in_goals(list(hlds_goal)::in,
    array(bool)::array_di, array(bool)::array_uo) is det.

find_used_vars_in_goals([], !VarUses).
find_used_vars_in_goals([Goal | Goals], !VarUses) :-
    find_used_vars_in_goal(Goal, !VarUses),
    find_used_vars_in_goals(Goals, !VarUses).

:- pred find_used_vars_in_cases(list(case)::in,
    array(bool)::array_di, array(bool)::array_uo) is det.

find_used_vars_in_cases([], !VarUses).
find_used_vars_in_cases([Case | Cases], !VarUses) :-
    Case = case(_, _, Goal),
    find_used_vars_in_goal(Goal, !VarUses),
    find_used_vars_in_cases(Cases, !VarUses).

:- pred find_used_vars_in_unify_rhs(unify_rhs::in,
    array(bool)::array_di, array(bool)::array_uo) is det.

find_used_vars_in_unify_rhs(RHS, !VarUses) :-
    (
        RHS = rhs_var(RHSVar),
        mark_var_as_used(RHSVar, !VarUses)
    ;
        RHS = rhs_functor(_, _, ArgVars),
        mark_vars_as_used(ArgVars, !VarUses)
    ;
        RHS = rhs_lambda_goal(_, _, _, NonLocals, ArgVarsModes, _, LambdaGoal),
        assoc_list.keys(ArgVarsModes, ArgVars),
        mark_vars_as_used(NonLocals, !VarUses),
        mark_vars_as_used(ArgVars, !VarUses),
        find_used_vars_in_goal(LambdaGoal, !VarUses)
    ).

:- pred mark_var_as_used(prog_var::in,
    array(bool)::array_di, array(bool)::array_uo) is det.
:- pragma inline(pred(mark_var_as_used/3)).

mark_var_as_used(Var, !VarUses) :-
    array.set(var_to_int(Var), yes, !VarUses).

:- pred mark_vars_as_used(list(prog_var)::in,
    array(bool)::array_di, array(bool)::array_uo) is det.

mark_vars_as_used([], !VarUses).
mark_vars_as_used([Var | Vars], !VarUses) :-
    mark_var_as_used(Var, !VarUses),
    mark_vars_as_used(Vars, !VarUses).

:- pred filter_var_table_entries(assoc_list(prog_var, var_table_entry)::in,
    array(bool)::in,
    assoc_list(prog_var, var_table_entry)::in,
    assoc_list(prog_var, var_table_entry)::out) is det.

filter_var_table_entries([], _VarUses, !RevVarsEntries).
filter_var_table_entries([VarEntry | VarsEntries], VarUses, !RevVarsEntries) :-
    VarEntry = Var - _Entry,
    VarNum = var_to_int(Var),
    array.unsafe_lookup(VarUses, VarNum, Used),
    (
        Used = yes,
        !:RevVarsEntries = [VarEntry | !.RevVarsEntries]
    ;
        Used = no
    ),
    filter_var_table_entries(VarsEntries, VarUses, !RevVarsEntries).

%---------------------------------------------------------------------------%
:- end_module hlds.restrict_var_maps.
%---------------------------------------------------------------------------%
