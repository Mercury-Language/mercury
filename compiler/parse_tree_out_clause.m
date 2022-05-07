%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module converts the parse tree structure representations of clauses
% back into Mercury source text.
%
%---------------------------------------------------------------------------%

:- module parse_tree.parse_tree_out_clause.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

:- pred mercury_output_item_clause(merc_out_info::in,
    io.text_output_stream::in, item_clause_info::in, io::di, io::uo) is det.

:- pred output_instance_method_clause(sym_name::in, item_clause_info::in,
    io.text_output_stream::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred mercury_output_goal(io.text_output_stream::in, prog_varset::in,
    int::in, goal::in, io::di, io::uo) is det.

:- pred write_goal_warnings(io.text_output_stream::in, goal_warning::in,
    list(goal_warning)::in, io::di, io::uo) is det.

:- pred mercury_output_trace_expr(io.text_output_stream::in,
    pred(T, io.text_output_stream, io, io)::in(pred(in, in, di, uo) is det),
    trace_expr(T)::in, io::di, io::uo) is det.

:- pred mercury_output_trace_compiletime(trace_compiletime::in,
    io.text_output_stream::in, io::di, io::uo) is det.

:- pred mercury_output_trace_runtime(trace_runtime::in,
    io.text_output_stream::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.prim_data.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.

:- import_module bool.
:- import_module int.
:- import_module maybe.
:- import_module string.
:- import_module term.
:- import_module term_io.
:- import_module varset.

%---------------------------------------------------------------------------%

mercury_output_item_clause(Info, Stream, ItemClause, !IO) :-
    ItemClause = item_clause_info(PredOrFunc, PredName0, ArgTerms,
        VarSet, MaybeBodyGoal, Context, _SeqNum),
    get_clause_body_goal(MaybeBodyGoal, BodyGoal),
    maybe_unqualify_sym_name(Info, PredName0, PredName),
    maybe_output_line_number(Info, Context, Stream, !IO),
    (
        PredOrFunc = pf_predicate,
        mercury_output_pred_clause(Stream, VarSet, PredName,
            ArgTerms, BodyGoal, !IO)
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(ArgTerms, FuncArgTerms, ResultTerm),
        mercury_output_func_clause(Stream, VarSet, PredName,
            FuncArgTerms, ResultTerm, BodyGoal, !IO)
    ),
    io.write_string(Stream, ".\n", !IO).

output_instance_method_clause(MethodName, ItemClause, Stream, !IO) :-
    ItemClause = item_clause_info(PredOrFunc, _PredName, ArgTerms,
        VarSet, MaybeBodyGoal, _Context, _SeqNum),
    get_clause_body_goal(MaybeBodyGoal, BodyGoal),
    (
        PredOrFunc = pf_predicate,
        mercury_output_pred_clause(Stream, VarSet, MethodName,
            ArgTerms, BodyGoal, !IO)
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(ArgTerms, FuncArgTerms, ResultTerm),
        mercury_output_func_clause(Stream, VarSet, MethodName,
            FuncArgTerms, ResultTerm, BodyGoal, !IO)
    ).

:- pred get_clause_body_goal(maybe2(goal, T)::in, goal::out) is det.

get_clause_body_goal(MaybeBodyGoal, BodyGoal) :-
    (
        MaybeBodyGoal = ok2(BodyGoal, _WarningSpecs)
    ;
        MaybeBodyGoal = error2(_),
        PredName = unqualified("there_was_a_syntax_error"),
        BodyGoal = call_expr(term.context_init, PredName, [], purity_pure)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_output_pred_clause(io.text_output_stream::in, prog_varset::in,
    sym_name::in, list(prog_term)::in, goal::in, io::di, io::uo) is det.

mercury_output_pred_clause(Stream, VarSet, PredName, Args, Body, !IO) :-
    mercury_output_sym_name(PredName, Stream, !IO),
    (
        Args = [HeadArg | TailArgs],
        io.write_string(Stream, "(", !IO),
        mercury_format_comma_separated_terms(VarSet, print_name_only,
            HeadArg, TailArgs, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        Args = []
    ),
    ( if Body = true_expr(_) then
        true
    else
        io.write_string(Stream, " :-\n\t", !IO),
        mercury_output_goal(Stream, VarSet, 1, Body, !IO)
    ).

:- pred mercury_output_func_clause(io.text_output_stream::in, prog_varset::in,
    sym_name::in, list(prog_term)::in, prog_term::in, goal::in,
    io::di, io::uo) is det.

mercury_output_func_clause(Stream, VarSet, PredName, Args, Result,
        Body, !IO) :-
    mercury_output_sym_name(PredName, Stream, !IO),
    (
        Args = [HeadArg | TailArgs],
        io.write_string(Stream, "(", !IO),
        mercury_format_comma_separated_terms(VarSet, print_name_only,
            HeadArg, TailArgs, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        Args = []
    ),
    io.write_string(Stream, " = ", !IO),
    ( if Body = true_expr(_) then
        mercury_format_term_nq(VarSet, print_name_only, next_to_graphic_token,
            Result, Stream, !IO)
    else
        mercury_format_term(VarSet, print_name_only, Result, Stream, !IO),
        io.write_string(Stream, " :-\n\t", !IO),
        mercury_output_goal(Stream, VarSet, 1, Body, !IO)
    ).

%---------------------------------------------------------------------------%

mercury_output_goal(Stream, VarSet, Indent, Goal, !IO) :-
    (
        Goal = fail_expr(_),
        io.write_string(Stream, "fail", !IO)
    ;
        Goal = true_expr(_),
        io.write_string(Stream, "true", !IO)
    ;
        Goal = implies_expr(_, SubGoalA, SubGoalB),
        Indent1 = Indent + 1,
        io.write_string(Stream, "(", !IO),
        mercury_output_newline(Indent1, Stream, !IO),
        mercury_output_connected_goal(Stream, VarSet, Indent1, SubGoalA, !IO),
        mercury_output_newline(Indent, Stream, !IO),
        io.write_string(Stream, "=>", !IO),
        mercury_output_newline(Indent1, Stream, !IO),
        mercury_output_connected_goal(Stream, VarSet, Indent1, SubGoalB, !IO),
        mercury_output_newline(Indent, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        Goal = equivalent_expr(_, SubGoalA, SubGoalB),
        Indent1 = Indent + 1,
        io.write_string(Stream, "(", !IO),
        mercury_output_newline(Indent1, Stream, !IO),
        mercury_output_connected_goal(Stream, VarSet, Indent1, SubGoalA, !IO),
        mercury_output_newline(Indent, Stream, !IO),
        io.write_string(Stream, "<=>", !IO),
        mercury_output_newline(Indent1, Stream, !IO),
        mercury_output_connected_goal(Stream, VarSet, Indent1, SubGoalB, !IO),
        mercury_output_newline(Indent, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        Goal = quant_expr(QuantType, QuantVarsKind, _, Vars, SubGoal),
        (
            Vars = [],
            mercury_output_goal(Stream, VarSet, Indent, SubGoal, !IO)
        ;
            Vars = [_ | _],
            (
                QuantType = quant_some,
                io.write_string(Stream, "some", !IO)
            ;
                QuantType = quant_all,
                io.write_string(Stream, "all", !IO)
            ),
            io.write_string(Stream, "[", !IO),
            (
                QuantVarsKind = quant_ordinary_vars,
                mercury_output_vars(VarSet, print_name_only, Vars, Stream, !IO)
            ;
                QuantVarsKind = quant_state_vars,
                mercury_output_state_vars(VarSet, print_name_only, Vars,
                    Stream, !IO)
            ),
            io.write_string(Stream, "] (", !IO),
            Indent1 = Indent + 1,
            mercury_output_newline(Indent1, Stream, !IO),
            mercury_output_goal(Stream, VarSet, Indent1, SubGoal, !IO),
            mercury_output_newline(Indent, Stream, !IO),
            io.write_string(Stream, ")", !IO)
        )
    ;
        Goal = promise_equivalent_solutions_expr(_, Vars, StateVars,
            DotSVars, ColonSVars, SubGoal),
        mercury_output_promise_eqv_solutions_goal(Stream, VarSet, Indent,
            Vars, StateVars, DotSVars, ColonSVars, SubGoal,
            "promise_equivalent_solutions", !IO)
    ;
        Goal = promise_equivalent_solution_sets_expr(_, Vars, StateVars,
            DotSVars, ColonSVars, SubGoal),
        mercury_output_promise_eqv_solutions_goal(Stream, VarSet, Indent,
            Vars, StateVars, DotSVars, ColonSVars, SubGoal,
            "promise_equivalent_solution_sets", !IO)
    ;
        Goal = promise_equivalent_solution_arbitrary_expr(_, Vars, StateVars,
            DotSVars, ColonSVars, SubGoal),
        mercury_output_promise_eqv_solutions_goal(Stream, VarSet, Indent,
            Vars, StateVars, DotSVars, ColonSVars, SubGoal, "arbitrary", !IO)
    ;
        Goal = promise_purity_expr(_, Purity, SubGoal),
        (
            Purity = purity_pure,
            io.write_string(Stream, "promise_pure (", !IO)
        ;
            Purity = purity_semipure,
            io.write_string(Stream, "promise_semipure (", !IO)
        ;
            Purity = purity_impure,
            io.write_string(Stream, "promise_impure (", !IO)
        ),
        Indent1 = Indent + 1,
        mercury_output_newline(Indent1, Stream, !IO),
        mercury_output_goal(Stream, VarSet, Indent1, SubGoal, !IO),
        mercury_output_newline(Indent, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        Goal = require_detism_expr(_, Detism, SubGoal),
        (
            Detism = detism_det,
            io.write_string(Stream, "require_det", !IO)
        ;
            Detism = detism_semi,
            io.write_string(Stream, "require_semidet", !IO)
        ;
            Detism = detism_multi,
            io.write_string(Stream, "require_multi", !IO)
        ;
            Detism = detism_non,
            io.write_string(Stream, "require_nondet", !IO)
        ;
            Detism = detism_cc_multi,
            io.write_string(Stream, "require_cc_multi", !IO)
        ;
            Detism = detism_cc_non,
            io.write_string(Stream, "require_cc_nondet", !IO)
        ;
            Detism = detism_erroneous,
            io.write_string(Stream, "require_erroneous", !IO)
        ;
            Detism = detism_failure,
            io.write_string(Stream, "require_failure", !IO)
        ),
        io.write_string(Stream, " (", !IO),
        Indent1 = Indent + 1,
        mercury_output_newline(Indent1, Stream, !IO),
        mercury_output_goal(Stream, VarSet, Indent1, SubGoal, !IO),
        mercury_output_newline(Indent, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        Goal = require_complete_switch_expr(_, Var, SubGoal),
        io.write_string(Stream, "require_complete_switch [", !IO),
        mercury_output_plain_or_dot_var(Stream, VarSet, print_name_only, Var,
            !IO),
        io.write_string(Stream, "] (", !IO),
        Indent1 = Indent + 1,
        mercury_output_newline(Indent1, Stream, !IO),
        mercury_output_goal(Stream, VarSet, Indent1, SubGoal, !IO),
        mercury_output_newline(Indent, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        Goal = require_switch_arms_detism_expr(_, Var, Detism, SubGoal),
        (
            Detism = detism_det,
            io.write_string(Stream, "require_switch_arms_det", !IO)
        ;
            Detism = detism_semi,
            io.write_string(Stream, "require_switch_arms_semidet", !IO)
        ;
            Detism = detism_multi,
            io.write_string(Stream, "require_switch_arms_multi", !IO)
        ;
            Detism = detism_non,
            io.write_string(Stream, "require_switch_arms_nondet", !IO)
        ;
            Detism = detism_cc_multi,
            io.write_string(Stream, "require_switch_arms_cc_multi", !IO)
        ;
            Detism = detism_cc_non,
            io.write_string(Stream, "require_switch_arms_cc_nondet", !IO)
        ;
            Detism = detism_erroneous,
            io.write_string(Stream, "require_switch_arms_erroneous", !IO)
        ;
            Detism = detism_failure,
            io.write_string(Stream, "require_switch_arms_failure", !IO)
        ),
        io.write_string(Stream, " [", !IO),
        mercury_output_plain_or_dot_var(Stream, VarSet, print_name_only,
            Var, !IO),
        io.write_string(Stream, "] (", !IO),
        Indent1 = Indent + 1,
        mercury_output_newline(Indent1, Stream, !IO),
        mercury_output_goal(Stream, VarSet, Indent1, SubGoal, !IO),
        mercury_output_newline(Indent, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        Goal = disable_warnings_expr(_, HeadWarning, TailWarnings, SubGoal),
        io.write_string(Stream, "disable_warnings [", !IO),
        write_goal_warnings(Stream, HeadWarning, TailWarnings, !IO),
        io.write_string(Stream, "] (", !IO),
        Indent1 = Indent + 1,
        mercury_output_newline(Indent1, Stream, !IO),
        mercury_output_goal(Stream, VarSet, Indent1, SubGoal, !IO),
        mercury_output_newline(Indent, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        Goal = atomic_expr(_, Outer, Inner, _, MainGoal, OrElseGoals),
        io.write_string(Stream, "atomic [outer(", !IO),
        (
            Outer = atomic_state_var(OVar),
            io.write_string(Stream, "!", !IO),
            mercury_output_var(VarSet, print_name_only, OVar, Stream, !IO)
        ;
            Outer = atomic_var_pair(OuterDI, OuterUO),
            mercury_output_var(VarSet, print_name_only, OuterDI, Stream, !IO),
            io.write_string(Stream, ", ", !IO),
            mercury_output_var(VarSet, print_name_only, OuterUO, Stream, !IO)
        ),
        io.write_string(Stream, "), inner(", !IO),
        (
            Inner = atomic_state_var(IVar),
            io.write_string(Stream, "!", !IO),
            mercury_output_var(VarSet, print_name_only, IVar, Stream, !IO)
        ;
            Inner = atomic_var_pair(InnerDI, InnerUO),
            mercury_output_var(VarSet, print_name_only, InnerDI, Stream, !IO),
            io.write_string(Stream, ", ", !IO),
            mercury_output_var(VarSet, print_name_only, InnerUO, Stream, !IO)
        ),
        io.write_string(Stream, ")] (", !IO),

        Indent1 = Indent + 1,
        mercury_output_newline(Indent1, Stream, !IO),
        mercury_output_orelse_goals(Stream, VarSet, Indent1,
            [MainGoal | OrElseGoals], !IO),
        mercury_output_newline(Indent, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        Goal = trace_expr(_, MaybeCompileTime, MaybeRunTime, MaybeIO,
            MutableVars, SubGoal),
        mercury_output_newline(Indent, Stream, !IO),
        io.write_string(Stream, "trace [", !IO),
        some [!NeedComma] (
            !:NeedComma = no,
            (
                MaybeCompileTime = yes(CompileTime),
                mercury_output_trace_expr(Stream,
                    mercury_output_trace_compiletime,
                    CompileTime, !IO),
                !:NeedComma = yes
            ;
                MaybeCompileTime = no
            ),
            (
                MaybeRunTime = yes(RunTime),
                mercury_output_comma_if_needed(Stream, !.NeedComma, !IO),
                mercury_output_trace_expr(Stream, mercury_output_trace_runtime,
                    RunTime, !IO),
                !:NeedComma = yes
            ;
                MaybeRunTime = no
            ),
            (
                MaybeIO = yes(IOStateVar),
                mercury_output_comma_if_needed(Stream, !.NeedComma, !IO),
                io.write_string(Stream, "io(!", !IO),
                mercury_output_var(VarSet, print_name_only, IOStateVar,
                    Stream, !IO),
                io.write_string(Stream, ")", !IO),
                !:NeedComma = yes
            ;
                MaybeIO = no
            ),
            list.foldl2(
                mercury_output_trace_mutable_var_and_comma(Stream, VarSet,
                    print_name_only),
                MutableVars, !.NeedComma, _, !IO)
        ),
        io.write_string(Stream, "]", !IO),
        mercury_output_newline(Indent + 1, Stream, !IO),
        mercury_output_goal(Stream, VarSet, Indent + 1, SubGoal, !IO),
        mercury_output_newline(Indent, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        Goal = try_expr(_, MaybeIO, SubGoal, Then, MaybeElse, Catches,
            MaybeCatchAny),
        io.write_string(Stream, "(try [", !IO),
        (
            MaybeIO = yes(IOStateVar),
            io.write_string(Stream, "io(!", !IO),
            mercury_output_var(VarSet, print_name_only, IOStateVar,
                Stream, !IO),
            io.write_string(Stream, ")", !IO)
        ;
            MaybeIO = no
        ),
        io.write_string(Stream, "] (", !IO),
        Indent1 = Indent + 1,
        mercury_output_newline(Indent1, Stream, !IO),
        mercury_output_goal(Stream, VarSet, Indent1, SubGoal, !IO),
        mercury_output_newline(Indent, Stream, !IO),
        io.write_string(Stream, ")", !IO),
        mercury_output_newline(Indent, Stream, !IO),
        io.write_string(Stream, "then", !IO),
        mercury_output_newline(Indent1, Stream, !IO),
        mercury_output_goal(Stream, VarSet, Indent1, Then, !IO),
        mercury_output_newline(Indent, Stream, !IO),
        (
            MaybeElse = yes(Else),
            io.write_string(Stream, "else", !IO),
            mercury_output_newline(Indent1, Stream, !IO),
            mercury_output_goal(Stream, VarSet, Indent1, Else, !IO)
        ;
            MaybeElse = no
        ),
        list.foldl(mercury_output_catch(Stream, VarSet, Indent), Catches, !IO),
        (
            MaybeCatchAny = yes(catch_any_expr(CatchAnyVar, CatchAnyGoal)),
            io.write_string(Stream, "catch_any ", !IO),
            mercury_output_var(VarSet, print_name_only, CatchAnyVar,
                Stream, !IO),
            io.write_string(Stream, " ->", !IO),
            mercury_output_newline(Indent1, Stream, !IO),
            mercury_output_goal(Stream, VarSet, Indent1, CatchAnyGoal, !IO)
        ;
            MaybeCatchAny = no
        ),
        mercury_output_newline(Indent, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        Goal = if_then_else_expr(_, Vars, StateVars, Cond, Then, Else),
        io.write_string(Stream, "(if", !IO),
        mercury_output_some(Stream, VarSet, Vars, StateVars, !IO),
        Indent1 = Indent + 1,
        mercury_output_newline(Indent1, Stream, !IO),
        mercury_output_goal(Stream, VarSet, Indent1, Cond, !IO),
        mercury_output_newline(Indent, Stream, !IO),
        io.write_string(Stream, "then", !IO),
        mercury_output_newline(Indent1, Stream, !IO),
        mercury_output_goal(Stream, VarSet, Indent1, Then, !IO),
        mercury_output_newline(Indent, Stream, !IO),
        io.write_string(Stream, "else", !IO),
        mercury_output_newline(Indent1, Stream, !IO),
        mercury_output_goal(Stream, VarSet, Indent1, Else, !IO),
        mercury_output_newline(Indent, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        Goal = not_expr(_, SubGoal),
        io.write_string(Stream, "\\+ (", !IO),
        Indent1 = Indent + 1,
        mercury_output_newline(Indent1, Stream, !IO),
        mercury_output_goal(Stream, VarSet, Indent1, SubGoal, !IO),
        mercury_output_newline(Indent, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        Goal = conj_expr(_, SubGoalA, SubGoalsB),
        mercury_output_conj(Stream, VarSet, Indent, SubGoalA, SubGoalsB, !IO)
    ;
        Goal = par_conj_expr(_, SubGoalA, SubGoalsB),
        io.write_string(Stream, "(\n", !IO),
        mercury_output_par_conj(Stream, VarSet, Indent,
            SubGoalA, SubGoalsB, !IO),
        mercury_output_newline(Indent, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        Goal = disj_expr(_, Disjunct1, Disjunct2, Disjuncts),
        NonFirstDisjuncts = [Disjunct2 | Disjuncts],
        io.write_string(Stream, "(", !IO),
        Indent1 = Indent + 1,
        mercury_output_newline(Indent1, Stream, !IO),
        mercury_output_goal(Stream, VarSet, Indent1, Disjunct1, !IO),
        mercury_output_disj(Stream, VarSet, Indent, NonFirstDisjuncts, !IO),
        mercury_output_newline(Indent, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        Goal = event_expr(_, Name, Terms),
        io.write_string(Stream, "event ", !IO),
        mercury_output_call(Stream, VarSet, unqualified(Name), Terms, !IO)
    ;
        Goal = call_expr(_, Name, Terms, Purity),
        add_purity_prefix(Purity, Stream, !IO),
        mercury_output_call(Stream, VarSet, Name, Terms, !IO)
    ;
        Goal = unify_expr(_, TermA, TermB, Purity),
        add_purity_prefix(Purity, Stream, !IO),
        mercury_output_term(VarSet, print_name_only, TermA, Stream, !IO),
        io.write_string(Stream, " = ", !IO),
        mercury_output_term_nq(VarSet, print_name_only, next_to_graphic_token,
            TermB, Stream, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_output_connected_goal(io.text_output_stream::in,
    prog_varset::in, int::in, goal::in, io::di, io::uo) is det.

mercury_output_connected_goal(Stream, VarSet, Indent, Goal, !IO) :-
    (
        ( Goal = fail_expr(_)
        ; Goal = true_expr(_)
        ; Goal = implies_expr(_, _, _)
        ; Goal = equivalent_expr(_, _, _)
        ; Goal = try_expr(_, _, _, _, _, _, _)
        ; Goal = if_then_else_expr(_, _, _, _, _, _)
        ; Goal = not_expr(_, _)
        ; Goal = par_conj_expr(_, _, _)
        ; Goal = disj_expr(_, _, _, _)
        ; Goal = event_expr(_, _, _)
        ; Goal = call_expr(_, _, _, _)
        ; Goal = unify_expr(_, _, _, _)
        ),
        mercury_output_goal(Stream, VarSet, Indent, Goal, !IO)
    ;
        ( Goal = quant_expr(_, _, _, _, _)
        ; Goal = promise_equivalent_solutions_expr(_, _, _, _, _, _)
        ; Goal = promise_equivalent_solution_sets_expr(_, _, _, _, _, _)
        ; Goal = promise_equivalent_solution_arbitrary_expr(_, _, _, _, _, _)
        ; Goal = promise_purity_expr(_, _, _)
        ; Goal = require_detism_expr(_, _, _)
        ; Goal = require_complete_switch_expr(_, _, _)
        ; Goal = require_switch_arms_detism_expr(_, _, _, _)
        ; Goal = disable_warnings_expr(_, _, _, _)
        ; Goal = conj_expr(_, _, _)
        ; Goal = atomic_expr(_, _, _, _, _, _)
        ; Goal = trace_expr(_, _, _, _, _, _)
        ),
        io.write_string(Stream, "(", !IO),
        Indent1 = Indent + 1,
        mercury_output_newline(Indent1, Stream, !IO),
        mercury_output_goal(Stream, VarSet, Indent1, Goal, !IO),
        mercury_output_newline(Indent, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_output_plain_or_dot_var(io.text_output_stream::in,
    prog_varset::in, var_name_print::in, plain_or_dot_var::in,
    io::di, io::uo) is det.

mercury_output_plain_or_dot_var(Stream, VarSet, VarNamePrint, PODVar, !IO) :-
    (
        PODVar = podv_plain(Var),
        mercury_output_var(VarSet, VarNamePrint, Var, Stream, !IO)
    ;
        PODVar = podv_dot(Var),
        io.write_string(Stream, "!.", !IO),
        mercury_output_var(VarSet, VarNamePrint, Var, Stream, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_output_call(io.text_output_stream::in, prog_varset::in,
    sym_name::in, list(prog_term)::in, io::di, io::uo) is det.

mercury_output_call(Stream, VarSet, SymName, Term, !IO) :-
    (
        SymName = qualified(ModuleName, PredName),
        mercury_output_bracketed_sym_name_ngt(next_to_graphic_token,
            ModuleName, Stream, !IO),
        io.write_string(Stream, ".", !IO),
        term.context_init(Context0),
        SubTerm = term.functor(term.atom(PredName), Term, Context0),
        mercury_output_term_nq(VarSet, print_name_only, next_to_graphic_token,
            SubTerm, Stream, !IO)
    ;
        SymName = unqualified(PredName),
        term.context_init(Context0),
        SubTerm = term.functor(term.atom(PredName), Term, Context0),
        mercury_output_term_nq(VarSet, print_name_only, next_to_graphic_token,
            SubTerm, Stream, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_output_disj(io.text_output_stream::in, prog_varset::in,
    int::in, list(goal)::in, io::di, io::uo) is det.

mercury_output_disj(_Stream, _VarSet, _Indent, [], !IO).
mercury_output_disj(Stream, VarSet, Indent, [Disjunct | Disjuncts], !IO) :-
    mercury_output_newline(Indent, Stream, !IO),
    io.write_string(Stream, ";", !IO),
    Indent1 = Indent + 1,
    mercury_output_newline(Indent1, Stream, !IO),
    mercury_output_goal(Stream, VarSet, Indent1, Disjunct, !IO),
    mercury_output_disj(Stream, VarSet, Indent, Disjuncts, !IO).

%---------------------------------------------------------------------------%

:- pred mercury_output_conj(io.text_output_stream::in, prog_varset::in,
    int::in, goal::in, list(goal)::in, io::di, io::uo) is det.

mercury_output_conj(Stream, VarSet, Indent, GoalA, GoalsB, !IO) :-
    mercury_output_goal(Stream, VarSet, Indent, GoalA, !IO),
    (
        GoalsB = []
    ;
        GoalsB = [GoalB | GoalsC],
        io.write_string(Stream, ",", !IO),
        mercury_output_newline(Indent, Stream, !IO),
        mercury_output_conj(Stream, VarSet, Indent, GoalB, GoalsC, !IO)
    ).

:- pred mercury_output_par_conj(io.text_output_stream::in, prog_varset::in,
    int::in, goal::in, list(goal)::in, io::di, io::uo) is det.

mercury_output_par_conj(Stream, VarSet, Indent, GoalA, GoalsB, !IO) :-
    Indent1 = Indent + 1,
    mercury_format_tabs(Indent1, Stream, !IO),
    mercury_output_goal(Stream, VarSet, Indent1, GoalA, !IO),
    (
        GoalsB = []
    ;
        GoalsB = [GoalB | GoalsC],
        mercury_output_newline(Indent, Stream, !IO),
        io.write_string(Stream, "&\n", !IO),
        mercury_output_par_conj(Stream, VarSet, Indent, GoalB, GoalsC, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_output_orelse_goals(io.text_output_stream::in,
    prog_varset::in, int::in, list(goal)::in, io::di, io::uo) is det.

mercury_output_orelse_goals(Stream, VarSet, Indent, Goals, !IO) :-
    (
        Goals = []
    ;
        Goals = [HeadGoal | TailGoals],
        (
            TailGoals = [],
            mercury_output_goal(Stream, VarSet, Indent + 1, HeadGoal, !IO)
        ;
            TailGoals = [_|_],
            mercury_output_goal(Stream, VarSet, Indent + 1, HeadGoal, !IO),
            mercury_output_newline(Indent, Stream, !IO),
            io.write_string(Stream, "orelse", !IO),
            mercury_output_newline(Indent, Stream, !IO),
            mercury_output_orelse_goals(Stream, VarSet, Indent, TailGoals, !IO)
        )
    ).

%---------------------------------------------------------------------------%

:- pred mercury_output_some(io.text_output_stream::in, varset(T)::in,
    list(var(T))::in, list(var(T))::in, io::di, io::uo) is det.

mercury_output_some(Stream, VarSet, Vars, StateVars, !IO) :-
    ( if
        ( Vars = [_ | _]
        ; StateVars = [_ | _]
        )
    then
        io.write_string(Stream, " some [", !IO),
        mercury_output_vars(VarSet, print_name_only, Vars, Stream, !IO),
        ( if
            Vars = [_ | _],
            StateVars = [_ | _]
        then
            io.write_string(Stream, ", ", !IO),
            % XXX BUG: we should print StateVars even if Vars = [].
            mercury_output_state_vars(VarSet, print_name_only, StateVars,
                Stream, !IO)
        else
            true
        ),
        io.write_string(Stream, "]", !IO)
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred mercury_output_promise_eqv_solutions_goal(io.text_output_stream::in,
    prog_varset::in, int::in, list(prog_var)::in, list(prog_var)::in,
    list(prog_var)::in, list(prog_var)::in, goal::in, string::in,
    io::di, io::uo) is det.

mercury_output_promise_eqv_solutions_goal(Stream, VarSet, Indent,
        Vars, StateVars, DotSVars, ColonSVars, Goal, Keyword, !IO) :-
    ( if
        Vars = [],
        StateVars = [],
        DotSVars = [],
        ColonSVars = []
    then
        % This should have been caught be parse_goal when reading in
        % the term, but there is no point in aborting here.
        mercury_output_goal(Stream, VarSet, Indent, Goal, !IO)
    else
        io.write_string(Stream, Keyword, !IO),
        io.write_string(Stream, " [", !IO),
        mercury_output_vars(VarSet, print_name_only, Vars, Stream, !IO),
        ( if
            Vars = [_ | _],
            StateVars = [_ | _]
        then
            io.write_string(Stream, ", ", !IO)
        else
            true
        ),
        mercury_output_state_vars_using_prefix(Stream, VarSet, print_name_only,
            "!", StateVars, !IO),
        ( if
            ( Vars = [_ | _]
            ; StateVars = [_ | _]
            ),
            DotSVars = [_ | _]
        then
            io.write_string(Stream, ", ", !IO)
        else
            true
        ),
        mercury_output_state_vars_using_prefix(Stream, VarSet, print_name_only,
            "!.", DotSVars, !IO),
        ( if
            ( Vars = [_ | _]
            ; StateVars = [_ | _]
            ; DotSVars = [_ | _]
            ),
            ColonSVars = [_ | _]
        then
            io.write_string(Stream, ", ", !IO)
        else
            true
        ),
        mercury_output_state_vars_using_prefix(Stream, VarSet, print_name_only,
            "!:", ColonSVars, !IO),
        io.write_string(Stream, "] (", !IO),
        Indent1 = Indent + 1,
        mercury_output_newline(Indent1, Stream, !IO),
        mercury_output_goal(Stream, VarSet, Indent1, Goal, !IO),
        mercury_output_newline(Indent, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ).

:- pred mercury_output_state_vars_using_prefix(io.text_output_stream::in,
    prog_varset::in, var_name_print::in, string::in, list(prog_var)::in,
    io::di, io::uo) is det.

mercury_output_state_vars_using_prefix(_, _, _, _, [], !IO).
mercury_output_state_vars_using_prefix(Stream, VarSet, VarNamePrint, BangPrefix,
        [SVar | SVars], !IO) :-
    io.write_string(Stream, BangPrefix, !IO),
    mercury_format_var(VarSet, VarNamePrint, SVar, Stream, !IO),
    (
        SVars = [_ | _],
        io.write_string(Stream, ", ", !IO),
        mercury_output_state_vars_using_prefix(Stream, VarSet, VarNamePrint,
            BangPrefix, SVars, !IO)
    ;
        SVars = []
    ).

%---------------------------------------------------------------------------%

write_goal_warnings(Stream, HeadWarning, TailWarnings, !IO) :-
    io.write_string(Stream, goal_warning_to_string(HeadWarning), !IO),
    (
        TailWarnings = []
    ;
        TailWarnings = [HeadTailWarning | TailTailWarnings],
        io.write_string(Stream, ", ", !IO),
        write_goal_warnings(Stream, HeadTailWarning, TailTailWarnings, !IO)
    ).

%---------------------------------------------------------------------------%

mercury_output_trace_expr(Stream, PrintBase, TraceExpr, !IO) :-
    (
        TraceExpr = trace_base(Base),
        PrintBase(Base, Stream, !IO)
    ;
        TraceExpr = trace_not(TraceExprA),
        io.write_string(Stream, "not(", !IO),
        mercury_output_trace_expr(Stream, PrintBase, TraceExprA, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        TraceExpr = trace_op(trace_or, TraceExprA, TraceExprB),
        io.write_string(Stream, "(", !IO),
        mercury_output_trace_expr(Stream, PrintBase, TraceExprA, !IO),
        io.write_string(Stream, ") or (", !IO),
        mercury_output_trace_expr(Stream, PrintBase, TraceExprB, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        TraceExpr = trace_op(trace_and, TraceExprA, TraceExprB),
        mercury_output_trace_expr(Stream, PrintBase, TraceExprA, !IO),
        io.write_string(Stream, " and ", !IO),
        mercury_output_trace_expr(Stream, PrintBase, TraceExprB, !IO)
    ).

mercury_output_trace_compiletime(CompileTime, Stream, !IO) :-
    (
        CompileTime = trace_flag(FlagName),
        io.format(Stream, "flag(%s)", [s(quoted_string(FlagName))], !IO)
    ;
        CompileTime = trace_grade(Grade),
        parse_trace_grade_name(GradeName, Grade),
        io.format(Stream, "grade(%s)", [s(GradeName)], !IO)
    ;
        CompileTime = trace_trace_level(Level),
        (
            Level = trace_level_shallow,
            LevelStr = "shallow"
        ;
            Level = trace_level_deep,
            LevelStr = "deep"
        ),
        io.format(Stream, "tracelevel(%s)", [s(LevelStr)], !IO)
    ).

mercury_output_trace_runtime(trace_envvar(EnvVarName), Stream, !IO) :-
    io.write_string(Stream, "env(", !IO),
    term_io.quote_string(Stream, EnvVarName, !IO),
    io.write_string(Stream, ")", !IO).

%---------------------------------------------------------------------------%

:- pred mercury_output_trace_mutable_var(io.text_output_stream::in,
    prog_varset::in, var_name_print::in, trace_mutable_var::in,
    io::di, io::uo) is det.

mercury_output_trace_mutable_var(Stream, VarSet, VarNamePrint, MutableVar,
        !IO) :-
    MutableVar = trace_mutable_var(MutableName, StateVar),
    StateVarStr = mercury_var_to_string(VarSet, VarNamePrint, StateVar),
    io.format(Stream, "state(%s, %s)",
        [s(MutableName), s(StateVarStr)], !IO).

:- pred mercury_output_trace_mutable_var_and_comma(io.text_output_stream::in,
    prog_varset::in, var_name_print::in, trace_mutable_var::in,
    bool::in, bool::out, io::di, io::uo) is det.

mercury_output_trace_mutable_var_and_comma(Stream, VarSet, VarNamePrint,
        MutableVar, !NeedComma, !IO) :-
    mercury_output_comma_if_needed(Stream, !.NeedComma, !IO),
    !:NeedComma = yes,
    mercury_output_trace_mutable_var(Stream, VarSet, VarNamePrint,
        MutableVar, !IO).

%---------------------------------------------------------------------------%

:- pred mercury_output_catch(io.text_output_stream::in, prog_varset::in,
    int::in, catch_expr::in, io::di, io::uo) is det.

mercury_output_catch(Stream, VarSet, Indent, catch_expr(Pattern, Goal), !IO) :-
    io.write_string(Stream, "catch ", !IO),
    mercury_output_term(VarSet, print_name_only, Pattern, Stream, !IO),
    io.write_string(Stream, " ->", !IO),
    mercury_output_newline(Indent + 1, Stream, !IO),
    mercury_output_goal(Stream, VarSet, Indent + 1, Goal, !IO),
    mercury_output_newline(Indent, Stream, !IO).

%---------------------------------------------------------------------------%

:- pred mercury_output_comma_if_needed(io.text_output_stream::in, bool::in,
    io::di, io::uo) is det.

mercury_output_comma_if_needed(_, no, !IO).
mercury_output_comma_if_needed(Stream, yes, !IO) :-
    io.write_string(Stream, ", ", !IO).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_tree_out_clause.
%---------------------------------------------------------------------------%
