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

:- import_module list.

%---------------------------------------------------------------------------%

:- pred mercury_format_item_clause(merc_out_info::in,
    S::in, item_clause_info::in, U::di, U::uo) is det <= pt_output(S, U).

:- pred mercury_format_instance_method_clause(sym_name::in,
    item_clause_info::in, S::in, U::di, U::uo) is det <= pt_output(S, U).

%---------------------------------------------------------------------------%

:- pred mercury_format_goal(S::in, prog_varset::in,
    int::in, goal::in, U::di, U::uo) is det <= pt_output(S, U).

:- pred mercury_format_goal_warnings(S::in,
    goal_warning::in, list(goal_warning)::in,
    U::di, U::uo) is det <= pt_output(S, U).

:- pred mercury_format_trace_expr(S::in,
    pred(T, S, U, U)::in(pred(in, in, di, uo) is det),
    trace_expr(T)::in, U::di, U::uo) is det <= pt_output(S, U).

:- pred mercury_format_trace_compiletime(trace_compiletime::in, S::in,
    U::di, U::uo) is det <= pt_output(S, U).

:- pred mercury_format_trace_runtime(trace_runtime::in, S::in,
    U::di, U::uo) is det <= pt_output(S, U).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.prim_data.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.parse_tree_out_sym_name.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_util.

:- import_module bool.
:- import_module int.
:- import_module maybe.
:- import_module string.
:- import_module term.
:- import_module term_context.
:- import_module term_io.
:- import_module varset.

%---------------------------------------------------------------------------%

mercury_format_item_clause(Info, S, ItemClause, !U) :-
    ItemClause = item_clause_info(PredOrFunc, PredName0, ArgTerms,
        VarSet, MaybeBodyGoal, Context, _SeqNum),
    get_clause_body_goal(MaybeBodyGoal, BodyGoal),
    maybe_unqualify_sym_name(Info, PredName0, PredName),
    maybe_format_line_number(Info, Context, S, !U),
    (
        PredOrFunc = pf_predicate,
        mercury_format_pred_clause(S, VarSet, PredName,
            ArgTerms, BodyGoal, !U)
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(ArgTerms, FuncArgTerms, ResultTerm),
        mercury_format_func_clause(S, VarSet, PredName,
            FuncArgTerms, ResultTerm, BodyGoal, !U)
    ),
    add_string(".\n", S, !U).

mercury_format_instance_method_clause(MethodName, ItemClause, S, !U) :-
    ItemClause = item_clause_info(PredOrFunc, _PredName, ArgTerms,
        VarSet, MaybeBodyGoal, _Context, _SeqNum),
    get_clause_body_goal(MaybeBodyGoal, BodyGoal),
    (
        PredOrFunc = pf_predicate,
        mercury_format_pred_clause(S, VarSet, MethodName,
            ArgTerms, BodyGoal, !U)
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(ArgTerms, FuncArgTerms, ResultTerm),
        mercury_format_func_clause(S, VarSet, MethodName,
            FuncArgTerms, ResultTerm, BodyGoal, !U)
    ).

:- pred get_clause_body_goal(maybe2(goal, T)::in, goal::out) is det.

get_clause_body_goal(MaybeBodyGoal, BodyGoal) :-
    (
        MaybeBodyGoal = ok2(BodyGoal, _WarningSpecs)
    ;
        MaybeBodyGoal = error2(_),
        PredName = unqualified("there_was_a_syntax_error"),
        BodyGoal = call_expr(dummy_context, PredName, [], purity_pure)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_format_pred_clause(S::in, prog_varset::in,
    sym_name::in, list(prog_term)::in, goal::in, U::di, U::uo) is det
    <= pt_output(S, U).

mercury_format_pred_clause(S, VarSet, PredName, Args, Body, !U) :-
    mercury_format_sym_name(PredName, S, !U),
    (
        Args = [HeadArg | TailArgs],
        add_string("(", S, !U),
        mercury_format_comma_separated_terms_vs(VarSet, print_name_only,
            HeadArg, TailArgs, S, !U),
        add_string(")", S, !U)
    ;
        Args = []
    ),
    ( if Body = true_expr(_) then
        true
    else
        add_string(" :-\n\t", S, !U),
        mercury_format_goal(S, VarSet, 1, Body, !U)
    ).

:- pred mercury_format_func_clause(S::in, prog_varset::in,
    sym_name::in, list(prog_term)::in, prog_term::in, goal::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_func_clause(S, VarSet, PredName, Args, Result, Body, !U) :-
    mercury_format_sym_name(PredName, S, !U),
    (
        Args = [HeadArg | TailArgs],
        add_string("(", S, !U),
        mercury_format_comma_separated_terms_vs(VarSet, print_name_only,
            HeadArg, TailArgs, S, !U),
        add_string(")", S, !U)
    ;
        Args = []
    ),
    add_string(" = ", S, !U),
    ( if Body = true_expr(_) then
        mercury_format_term_nq_vs(VarSet, print_name_only,
            next_to_graphic_token, Result, S, !U)
    else
        mercury_format_term_vs(VarSet, print_name_only, Result, S, !U),
        add_string(" :-\n\t", S, !U),
        mercury_format_goal(S, VarSet, 1, Body, !U)
    ).

%---------------------------------------------------------------------------%

mercury_format_goal(S, VarSet, Indent, Goal, !U) :-
    (
        Goal = fail_expr(_),
        add_string("fail", S, !U)
    ;
        Goal = true_expr(_),
        add_string("true", S, !U)
    ;
        Goal = implies_expr(_, SubGoalA, SubGoalB),
        Indent1 = Indent + 1,
        add_string("(", S, !U),
        mercury_format_newline(Indent1, S, !U),
        mercury_format_connected_goal(S, VarSet, Indent1, SubGoalA, !U),
        mercury_format_newline(Indent, S, !U),
        add_string("=>", S, !U),
        mercury_format_newline(Indent1, S, !U),
        mercury_format_connected_goal(S, VarSet, Indent1, SubGoalB, !U),
        mercury_format_newline(Indent, S, !U),
        add_string(")", S, !U)
    ;
        Goal = equivalent_expr(_, SubGoalA, SubGoalB),
        Indent1 = Indent + 1,
        add_string("(", S, !U),
        mercury_format_newline(Indent1, S, !U),
        mercury_format_connected_goal(S, VarSet, Indent1, SubGoalA, !U),
        mercury_format_newline(Indent, S, !U),
        add_string("<=>", S, !U),
        mercury_format_newline(Indent1, S, !U),
        mercury_format_connected_goal(S, VarSet, Indent1, SubGoalB, !U),
        mercury_format_newline(Indent, S, !U),
        add_string(")", S, !U)
    ;
        Goal = quant_expr(QuantType, QuantVarsKind, _, Vars, SubGoal),
        (
            Vars = [],
            mercury_format_goal(S, VarSet, Indent, SubGoal, !U)
        ;
            Vars = [_ | _],
            (
                QuantType = quant_some,
                add_string("some", S, !U)
            ;
                QuantType = quant_all,
                add_string("all", S, !U)
            ),
            add_string("[", S, !U),
            (
                QuantVarsKind = quant_ordinary_vars,
                mercury_format_vars_vs(VarSet, print_name_only, Vars, S, !U)
            ;
                QuantVarsKind = quant_state_vars,
                mercury_format_state_vars(VarSet, print_name_only, Vars, S, !U)
            ),
            add_string("] (", S, !U),
            Indent1 = Indent + 1,
            mercury_format_newline(Indent1, S, !U),
            mercury_format_goal(S, VarSet, Indent1, SubGoal, !U),
            mercury_format_newline(Indent, S, !U),
            add_string(")", S, !U)
        )
    ;
        Goal = promise_equivalent_solutions_expr(_, Vars, StateVars,
            DotSVars, ColonSVars, SubGoal),
        mercury_format_promise_eqv_solutions_goal(S, VarSet, Indent,
            Vars, StateVars, DotSVars, ColonSVars, SubGoal,
            "promise_equivalent_solutions", !U)
    ;
        Goal = promise_equivalent_solution_sets_expr(_, Vars, StateVars,
            DotSVars, ColonSVars, SubGoal),
        mercury_format_promise_eqv_solutions_goal(S, VarSet, Indent,
            Vars, StateVars, DotSVars, ColonSVars, SubGoal,
            "promise_equivalent_solution_sets", !U)
    ;
        Goal = promise_equivalent_solution_arbitrary_expr(_, Vars, StateVars,
            DotSVars, ColonSVars, SubGoal),
        mercury_format_promise_eqv_solutions_goal(S, VarSet, Indent,
            Vars, StateVars, DotSVars, ColonSVars, SubGoal, "arbitrary", !U)
    ;
        Goal = promise_purity_expr(_, Purity, SubGoal),
        ( Purity = purity_pure,     PurityStr = "promise_pure"
        ; Purity = purity_semipure, PurityStr = "promise_semipure"
        ; Purity = purity_impure,   PurityStr = "promise_impure"
        ),
        Indent1 = Indent + 1,
        add_string(PurityStr, S, !U),
        add_string(" (", S, !U),
        mercury_format_newline(Indent1, S, !U),
        mercury_format_goal(S, VarSet, Indent1, SubGoal, !U),
        mercury_format_newline(Indent, S, !U),
        add_string(")", S, !U)
    ;
        Goal = require_detism_expr(_, Detism, SubGoal),
        ( Detism = detism_det,       DetismStr = "require_det"
        ; Detism = detism_semi,      DetismStr = "require_semidet"
        ; Detism = detism_multi,     DetismStr = "require_multi"
        ; Detism = detism_non,       DetismStr = "require_nondet"
        ; Detism = detism_cc_multi,  DetismStr = "require_cc_multi"
        ; Detism = detism_cc_non,    DetismStr = "require_cc_nondet"
        ; Detism = detism_erroneous, DetismStr = "require_erroneous"
        ; Detism = detism_failure,   DetismStr = "require_failure"
        ),
        add_string(DetismStr, S, !U),
        add_string(" (", S, !U),
        Indent1 = Indent + 1,
        mercury_format_newline(Indent1, S, !U),
        mercury_format_goal(S, VarSet, Indent1, SubGoal, !U),
        mercury_format_newline(Indent, S, !U),
        add_string(")", S, !U)
    ;
        Goal = require_complete_switch_expr(_, Var, SubGoal),
        add_string("require_complete_switch [", S, !U),
        mercury_format_plain_or_dot_var(S, VarSet, print_name_only, Var, !U),
        add_string("] (", S, !U),
        Indent1 = Indent + 1,
        mercury_format_newline(Indent1, S, !U),
        mercury_format_goal(S, VarSet, Indent1, SubGoal, !U),
        mercury_format_newline(Indent, S, !U),
        add_string(")", S, !U)
    ;
        Goal = require_switch_arms_detism_expr(_, Var, Detism, SubGoal),
        ( Detism = detism_det,       ReqStr = "require_switch_arms_det"
        ; Detism = detism_semi,      ReqStr = "require_switch_arms_semidet"
        ; Detism = detism_multi,     ReqStr = "require_switch_arms_multi"
        ; Detism = detism_non,       ReqStr = "require_switch_arms_nondet"
        ; Detism = detism_cc_multi,  ReqStr = "require_switch_arms_cc_multi"
        ; Detism = detism_cc_non,    ReqStr = "require_switch_arms_cc_nondet"
        ; Detism = detism_erroneous, ReqStr = "require_switch_arms_erroneous"
        ; Detism = detism_failure,   ReqStr = "require_switch_arms_failure"
        ),
        add_string(ReqStr, S, !U),
        add_string(" [", S, !U),
        mercury_format_plain_or_dot_var(S, VarSet, print_name_only, Var, !U),
        add_string("] (", S, !U),
        Indent1 = Indent + 1,
        mercury_format_newline(Indent1, S, !U),
        mercury_format_goal(S, VarSet, Indent1, SubGoal, !U),
        mercury_format_newline(Indent, S, !U),
        add_string(")", S, !U)
    ;
        Goal = disable_warnings_expr(_, HeadWarning, TailWarnings, SubGoal),
        add_string("disable_warnings [", S, !U),
        mercury_format_goal_warnings(S, HeadWarning, TailWarnings, !U),
        add_string("] (", S, !U),
        Indent1 = Indent + 1,
        mercury_format_newline(Indent1, S, !U),
        mercury_format_goal(S, VarSet, Indent1, SubGoal, !U),
        mercury_format_newline(Indent, S, !U),
        add_string(")", S, !U)
    ;
        Goal = atomic_expr(_, Outer, Inner, _, MainGoal, OrElseGoals),
        add_string("atomic [outer(", S, !U),
        (
            Outer = atomic_state_var(OVar),
            add_string("!", S, !U),
            mercury_format_var_vs(VarSet, print_name_only, OVar, S, !U)
        ;
            Outer = atomic_var_pair(OuterDI, OuterUO),
            mercury_format_var_vs(VarSet, print_name_only, OuterDI, S, !U),
            add_string(", ", S, !U),
            mercury_format_var_vs(VarSet, print_name_only, OuterUO, S, !U)
        ),
        add_string("), inner(", S, !U),
        (
            Inner = atomic_state_var(IVar),
            add_string("!", S, !U),
            mercury_format_var_vs(VarSet, print_name_only, IVar, S, !U)
        ;
            Inner = atomic_var_pair(InnerDI, InnerUO),
            mercury_format_var_vs(VarSet, print_name_only, InnerDI, S, !U),
            add_string(", ", S, !U),
            mercury_format_var_vs(VarSet, print_name_only, InnerUO, S, !U)
        ),
        add_string(")] (", S, !U),

        Indent1 = Indent + 1,
        mercury_format_newline(Indent1, S, !U),
        mercury_format_orelse_goals(S, VarSet, Indent1,
            [MainGoal | OrElseGoals], !U),
        mercury_format_newline(Indent, S, !U),
        add_string(")", S, !U)
    ;
        Goal = trace_expr(_, MaybeCompileTime, MaybeRunTime, MaybeIO,
            MutableVars, SubGoal),
        mercury_format_newline(Indent, S, !U),
        add_string("trace [", S, !U),
        some [!NeedComma] (
            !:NeedComma = no,
            (
                MaybeCompileTime = yes(CompileTime),
                mercury_format_trace_expr(S,
                    mercury_format_trace_compiletime,
                    CompileTime, !U),
                !:NeedComma = yes
            ;
                MaybeCompileTime = no
            ),
            (
                MaybeRunTime = yes(RunTime),
                mercury_format_comma_if_needed(S, !.NeedComma, !U),
                mercury_format_trace_expr(S, mercury_format_trace_runtime,
                    RunTime, !U),
                !:NeedComma = yes
            ;
                MaybeRunTime = no
            ),
            (
                MaybeIO = yes(IOStateVar),
                mercury_format_comma_if_needed(S, !.NeedComma, !U),
                add_string("io(!", S, !U),
                mercury_format_var_vs(VarSet, print_name_only, IOStateVar,
                    S, !U),
                add_string(")", S, !U),
                !:NeedComma = yes
            ;
                MaybeIO = no
            ),
            list.foldl2(
                mercury_format_trace_mutable_var_and_comma(S, VarSet,
                    print_name_only),
                MutableVars, !.NeedComma, _, !U)
        ),
        add_string("]", S, !U),
        mercury_format_newline(Indent + 1, S, !U),
        mercury_format_goal(S, VarSet, Indent + 1, SubGoal, !U),
        mercury_format_newline(Indent, S, !U),
        add_string(")", S, !U)
    ;
        Goal = try_expr(_, MaybeIO, SubGoal, Then, MaybeElse, Catches,
            MaybeCatchAny),
        add_string("(try [", S, !U),
        (
            MaybeIO = yes(IOStateVar),
            add_string("io(!", S, !U),
            mercury_format_var_vs(VarSet, print_name_only, IOStateVar, S, !U),
            add_string(")", S, !U)
        ;
            MaybeIO = no
        ),
        add_string("] (", S, !U),
        Indent1 = Indent + 1,
        mercury_format_newline(Indent1, S, !U),
        mercury_format_goal(S, VarSet, Indent1, SubGoal, !U),
        mercury_format_newline(Indent, S, !U),
        add_string(")", S, !U),
        mercury_format_newline(Indent, S, !U),
        add_string("then", S, !U),
        mercury_format_newline(Indent1, S, !U),
        mercury_format_goal(S, VarSet, Indent1, Then, !U),
        mercury_format_newline(Indent, S, !U),
        (
            MaybeElse = yes(Else),
            add_string("else", S, !U),
            mercury_format_newline(Indent1, S, !U),
            mercury_format_goal(S, VarSet, Indent1, Else, !U)
        ;
            MaybeElse = no
        ),
        list.foldl(mercury_format_catch(S, VarSet, Indent), Catches, !U),
        (
            MaybeCatchAny = yes(catch_any_expr(CatchAnyVar, CatchAnyGoal)),
            add_string("catch_any ", S, !U),
            mercury_format_var_vs(VarSet, print_name_only, CatchAnyVar, S, !U),
            add_string(" ->", S, !U),
            mercury_format_newline(Indent1, S, !U),
            mercury_format_goal(S, VarSet, Indent1, CatchAnyGoal, !U)
        ;
            MaybeCatchAny = no
        ),
        mercury_format_newline(Indent, S, !U),
        add_string(")", S, !U)
    ;
        Goal = if_then_else_expr(_, Vars, StateVars, Cond, Then, Else),
        add_string("(if", S, !U),
        mercury_format_some(S, VarSet, Vars, StateVars, !U),
        Indent1 = Indent + 1,
        mercury_format_newline(Indent1, S, !U),
        mercury_format_goal(S, VarSet, Indent1, Cond, !U),
        mercury_format_newline(Indent, S, !U),
        add_string("then", S, !U),
        mercury_format_newline(Indent1, S, !U),
        mercury_format_goal(S, VarSet, Indent1, Then, !U),
        mercury_format_newline(Indent, S, !U),
        add_string("else", S, !U),
        mercury_format_newline(Indent1, S, !U),
        mercury_format_goal(S, VarSet, Indent1, Else, !U),
        mercury_format_newline(Indent, S, !U),
        add_string(")", S, !U)
    ;
        Goal = not_expr(_, SubGoal),
        add_string("\\+ (", S, !U),
        Indent1 = Indent + 1,
        mercury_format_newline(Indent1, S, !U),
        mercury_format_goal(S, VarSet, Indent1, SubGoal, !U),
        mercury_format_newline(Indent, S, !U),
        add_string(")", S, !U)
    ;
        Goal = conj_expr(_, SubGoalA, SubGoalsB),
        mercury_format_conj(S, VarSet, Indent, SubGoalA, SubGoalsB, !U)
    ;
        Goal = par_conj_expr(_, SubGoalA, SubGoalsB),
        add_string("(\n", S, !U),
        mercury_format_par_conj(S, VarSet, Indent, SubGoalA, SubGoalsB, !U),
        mercury_format_newline(Indent, S, !U),
        add_string(")", S, !U)
    ;
        Goal = disj_expr(_, Disjunct1, Disjunct2, Disjuncts),
        NonFirstDisjuncts = [Disjunct2 | Disjuncts],
        add_string("(", S, !U),
        Indent1 = Indent + 1,
        mercury_format_newline(Indent1, S, !U),
        mercury_format_goal(S, VarSet, Indent1, Disjunct1, !U),
        mercury_format_disj(S, VarSet, Indent, NonFirstDisjuncts, !U),
        mercury_format_newline(Indent, S, !U),
        add_string(")", S, !U)
    ;
        Goal = event_expr(_, Name, Terms),
        add_string("event ", S, !U),
        mercury_format_call(S, VarSet, unqualified(Name), Terms, !U)
    ;
        Goal = call_expr(_, Name, Terms, Purity),
        add_purity_prefix(Purity, S, !U),
        mercury_format_call(S, VarSet, Name, Terms, !U)
    ;
        Goal = unify_expr(_, TermA, TermB, Purity),
        add_purity_prefix(Purity, S, !U),
        mercury_format_term_vs(VarSet, print_name_only, TermA, S, !U),
        add_string(" = ", S, !U),
        mercury_format_term_nq_vs(VarSet, print_name_only,
            next_to_graphic_token, TermB, S, !U)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_format_connected_goal(S::in, prog_varset::in, int::in,
    goal::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_connected_goal(S, VarSet, Indent, Goal, !U) :-
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
        mercury_format_goal(S, VarSet, Indent, Goal, !U)
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
        Indent1 = Indent + 1,
        add_string("(", S, !U),
        mercury_format_newline(Indent1, S, !U),
        mercury_format_goal(S, VarSet, Indent1, Goal, !U),
        mercury_format_newline(Indent, S, !U),
        add_string(")", S, !U)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_format_plain_or_dot_var(S::in, prog_varset::in,
    var_name_print::in, plain_or_dot_var::in, U::di, U::uo) is det
    <= pt_output(S, U).

mercury_format_plain_or_dot_var(S, VarSet, VarNamePrint, PODVar, !U) :-
    (
        PODVar = podv_plain(Var),
        mercury_format_var_vs(VarSet, VarNamePrint, Var, S, !U)
    ;
        PODVar = podv_dot(Var),
        add_string("!.", S, !U),
        mercury_format_var_vs(VarSet, VarNamePrint, Var, S, !U)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_format_call(S::in, prog_varset::in,
    sym_name::in, list(prog_term)::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_call(S, VarSet, SymName, Term, !U) :-
    (
        SymName = qualified(ModuleName, PredName),
        mercury_format_bracketed_sym_name_ngt(next_to_graphic_token,
            ModuleName, S, !U),
        add_string(".", S, !U),
        SubTerm = term.functor(term.atom(PredName), Term, dummy_context),
        mercury_format_term_nq_vs(VarSet, print_name_only,
            next_to_graphic_token, SubTerm, S, !U)
    ;
        SymName = unqualified(PredName),
        SubTerm = term.functor(term.atom(PredName), Term, dummy_context),
        mercury_format_term_nq_vs(VarSet, print_name_only,
            next_to_graphic_token, SubTerm, S, !U)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_format_disj(S::in, prog_varset::in,
    int::in, list(goal)::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_disj(_S, _VarSet, _Indent, [], !U).
mercury_format_disj(S, VarSet, Indent, [Disjunct | Disjuncts], !U) :-
    mercury_format_newline(Indent, S, !U),
    add_string(";", S, !U),
    Indent1 = Indent + 1,
    mercury_format_newline(Indent1, S, !U),
    mercury_format_goal(S, VarSet, Indent1, Disjunct, !U),
    mercury_format_disj(S, VarSet, Indent, Disjuncts, !U).

%---------------------------------------------------------------------------%

:- pred mercury_format_conj(S::in, prog_varset::in,
    int::in, goal::in, list(goal)::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_conj(S, VarSet, Indent, GoalA, GoalsB, !U) :-
    mercury_format_goal(S, VarSet, Indent, GoalA, !U),
    (
        GoalsB = []
    ;
        GoalsB = [GoalB | GoalsC],
        add_string(",", S, !U),
        mercury_format_newline(Indent, S, !U),
        mercury_format_conj(S, VarSet, Indent, GoalB, GoalsC, !U)
    ).

:- pred mercury_format_par_conj(S::in, prog_varset::in,
    int::in, goal::in, list(goal)::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_par_conj(S, VarSet, Indent, GoalA, GoalsB, !U) :-
    Indent1 = Indent + 1,
    mercury_format_tabs(Indent1, S, !U),
    mercury_format_goal(S, VarSet, Indent1, GoalA, !U),
    (
        GoalsB = []
    ;
        GoalsB = [GoalB | GoalsC],
        mercury_format_newline(Indent, S, !U),
        add_string("&\n", S, !U),
        mercury_format_par_conj(S, VarSet, Indent, GoalB, GoalsC, !U)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_format_orelse_goals(S::in, prog_varset::in,
    int::in, list(goal)::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_orelse_goals(S, VarSet, Indent, Goals, !U) :-
    (
        Goals = []
    ;
        Goals = [HeadGoal | TailGoals],
        (
            TailGoals = [],
            mercury_format_goal(S, VarSet, Indent + 1, HeadGoal, !U)
        ;
            TailGoals = [_|_],
            mercury_format_goal(S, VarSet, Indent + 1, HeadGoal, !U),
            mercury_format_newline(Indent, S, !U),
            add_string("orelse", S, !U),
            mercury_format_newline(Indent, S, !U),
            mercury_format_orelse_goals(S, VarSet, Indent, TailGoals, !U)
        )
    ).

%---------------------------------------------------------------------------%

:- pred mercury_format_some(S::in, varset(T)::in,
    list(var(T))::in, list(var(T))::in, U::di, U::uo) is det
    <= pt_output(S, U).

mercury_format_some(S, VarSet, Vars, StateVars, !U) :-
    ( if
        ( Vars = [_ | _]
        ; StateVars = [_ | _]
        )
    then
        add_string(" some [", S, !U),
        mercury_format_vars_vs(VarSet, print_name_only, Vars, S, !U),
        ( if
            Vars = [_ | _],
            StateVars = [_ | _]
        then
            add_string(", ", S, !U),
            % XXX BUG: we should print StateVars even if Vars = [].
            mercury_format_state_vars(VarSet, print_name_only, StateVars,
                S, !U)
        else
            true
        ),
        add_string("]", S, !U)
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred mercury_format_promise_eqv_solutions_goal(S::in, prog_varset::in,
    int::in, list(prog_var)::in, list(prog_var)::in,
    list(prog_var)::in, list(prog_var)::in, goal::in, string::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_promise_eqv_solutions_goal(S, VarSet, Indent,
        Vars, StateVars, DotSVars, ColonSVars, Goal, Keyword, !U) :-
    ( if
        Vars = [],
        StateVars = [],
        DotSVars = [],
        ColonSVars = []
    then
        % This should have been caught be parse_goal when reading in the term,
        % but there is no point in aborting here.
        mercury_format_goal(S, VarSet, Indent, Goal, !U)
    else
        add_string(Keyword, S, !U),
        add_string(" [", S, !U),
        mercury_format_vars_vs(VarSet, print_name_only, Vars, S, !U),
        ( if
            Vars = [_ | _],
            StateVars = [_ | _]
        then
            add_string(", ", S, !U)
        else
            true
        ),
        mercury_format_state_vars_using_prefix(S, VarSet, print_name_only,
            "!", StateVars, !U),
        ( if
            ( Vars = [_ | _]
            ; StateVars = [_ | _]
            ),
            DotSVars = [_ | _]
        then
            add_string(", ", S, !U)
        else
            true
        ),
        mercury_format_state_vars_using_prefix(S, VarSet, print_name_only,
            "!.", DotSVars, !U),
        ( if
            ( Vars = [_ | _]
            ; StateVars = [_ | _]
            ; DotSVars = [_ | _]
            ),
            ColonSVars = [_ | _]
        then
            add_string(", ", S, !U)
        else
            true
        ),
        mercury_format_state_vars_using_prefix(S, VarSet, print_name_only,
            "!:", ColonSVars, !U),
        add_string("] (", S, !U),
        Indent1 = Indent + 1,
        mercury_format_newline(Indent1, S, !U),
        mercury_format_goal(S, VarSet, Indent1, Goal, !U),
        mercury_format_newline(Indent, S, !U),
        add_string(")", S, !U)
    ).

:- pred mercury_format_state_vars_using_prefix(S::in, prog_varset::in,
    var_name_print::in, string::in, list(prog_var)::in, U::di, U::uo) is det
    <= pt_output(S, U).

mercury_format_state_vars_using_prefix(_, _, _, _, [], !U).
mercury_format_state_vars_using_prefix(S, VarSet, VarNamePrint,
        BangPrefix, [SVar | SVars], !U) :-
    add_string(BangPrefix, S, !U),
    mercury_format_var_vs(VarSet, VarNamePrint, SVar, S, !U),
    (
        SVars = [_ | _],
        add_string(", ", S, !U),
        mercury_format_state_vars_using_prefix(S, VarSet, VarNamePrint,
            BangPrefix, SVars, !U)
    ;
        SVars = []
    ).

%---------------------------------------------------------------------------%

mercury_format_goal_warnings(S, HeadWarning, TailWarnings, !U) :-
    add_string(goal_warning_to_string(HeadWarning), S, !U),
    (
        TailWarnings = []
    ;
        TailWarnings = [HeadTailWarning | TailTailWarnings],
        add_string(", ", S, !U),
        mercury_format_goal_warnings(S, HeadTailWarning, TailTailWarnings, !U)
    ).

%---------------------------------------------------------------------------%

mercury_format_trace_expr(S, PrintBase, TraceExpr, !U) :-
    (
        TraceExpr = trace_base(Base),
        PrintBase(Base, S, !U)
    ;
        TraceExpr = trace_not(TraceExprA),
        add_string("not(", S, !U),
        mercury_format_trace_expr(S, PrintBase, TraceExprA, !U),
        add_string(")", S, !U)
    ;
        TraceExpr = trace_op(trace_or, TraceExprA, TraceExprB),
        add_string("(", S, !U),
        mercury_format_trace_expr(S, PrintBase, TraceExprA, !U),
        add_string(") or (", S, !U),
        mercury_format_trace_expr(S, PrintBase, TraceExprB, !U),
        add_string(")", S, !U)
    ;
        TraceExpr = trace_op(trace_and, TraceExprA, TraceExprB),
        mercury_format_trace_expr(S, PrintBase, TraceExprA, !U),
        add_string(" and ", S, !U),
        mercury_format_trace_expr(S, PrintBase, TraceExprB, !U)
    ).

mercury_format_trace_compiletime(CompileTime, S, !U) :-
    (
        CompileTime = trace_flag(FlagName),
        string.format("flag(%s)", [s(quoted_string(FlagName))], CondStr)
    ;
        CompileTime = trace_grade(Grade),
        parse_trace_grade_name(GradeName, Grade),
        string.format("grade(%s)", [s(GradeName)], CondStr)
    ;
        CompileTime = trace_trace_level(Level),
        (
            Level = trace_level_shallow,
            LevelStr = "shallow"
        ;
            Level = trace_level_deep,
            LevelStr = "deep"
        ),
        string.format("tracelevel(%s)", [s(LevelStr)], CondStr)
    ),
    add_string(CondStr, S, !U).

mercury_format_trace_runtime(trace_envvar(EnvVarName), S, !U) :-
    add_string("env(", S, !U),
    term_io.format_quoted_string(S, EnvVarName, !U),
    add_string(")", S, !U).

%---------------------------------------------------------------------------%

:- pred mercury_format_trace_mutable_var(S::in, prog_varset::in,
    var_name_print::in, trace_mutable_var::in, U::di, U::uo) is det
    <= pt_output(S, U).

mercury_format_trace_mutable_var(S, VarSet, VarNamePrint, MutableVar, !U) :-
    MutableVar = trace_mutable_var(MutableName, StateVar),
    StateVarStr = mercury_var_to_string_vs(VarSet, VarNamePrint, StateVar),
    string.format("state(%s, %s)", [s(MutableName), s(StateVarStr)], StateStr),
    add_string(StateStr, S, !U).

:- pred mercury_format_trace_mutable_var_and_comma(S::in, prog_varset::in,
    var_name_print::in, trace_mutable_var::in, bool::in, bool::out,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_trace_mutable_var_and_comma(S, VarSet, VarNamePrint, MutableVar,
        !NeedComma, !U) :-
    mercury_format_comma_if_needed(S, !.NeedComma, !U),
    !:NeedComma = yes,
    mercury_format_trace_mutable_var(S, VarSet, VarNamePrint, MutableVar, !U).

%---------------------------------------------------------------------------%

:- pred mercury_format_catch(S::in, prog_varset::in, int::in, catch_expr::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_catch(S, VarSet, Indent, catch_expr(Pattern, Goal), !U) :-
    add_string("catch ", S, !U),
    mercury_format_term_vs(VarSet, print_name_only, Pattern, S, !U),
    add_string(" ->", S, !U),
    mercury_format_newline(Indent + 1, S, !U),
    mercury_format_goal(S, VarSet, Indent + 1, Goal, !U),
    mercury_format_newline(Indent, S, !U).

%---------------------------------------------------------------------------%

:- pred mercury_format_comma_if_needed(S::in, bool::in, U::di, U::uo) is det
    <= pt_output(S, U).

mercury_format_comma_if_needed(_, no, !U).
mercury_format_comma_if_needed(S, yes, !U) :-
    add_string(", ", S, !U).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_tree_out_clause.
%---------------------------------------------------------------------------%
