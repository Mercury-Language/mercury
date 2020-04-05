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

:- pred mercury_output_item_clause(merc_out_info::in, item_clause_info::in,
    io::di, io::uo) is det.

:- pred output_instance_method_clause(sym_name::in, item_clause_info::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred mercury_output_goal(prog_varset::in, int::in, goal::in,
    io::di, io::uo) is det.

:- pred write_goal_warnings(goal_warning::in, list(goal_warning)::in,
    io::di, io::uo) is det.

:- pred mercury_output_trace_expr(pred(T, io, io)::in(pred(in, di, uo) is det),
    trace_expr(T)::in, io::di, io::uo) is det.

:- pred mercury_output_trace_compiletime(trace_compiletime::in,
    io::di, io::uo) is det.

:- pred mercury_output_trace_runtime(trace_runtime::in,
    io::di, io::uo) is det.

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
:- import_module term.
:- import_module term_io.
:- import_module varset.

%---------------------------------------------------------------------------%

mercury_output_item_clause(Info, ItemClause, !IO) :-
    ItemClause = item_clause_info(PredName0, PredOrFunc, ArgTerms, _MaybeAttrs,
        VarSet, MaybeBodyGoal, Context, _SeqNum),
    get_clause_body_goal(MaybeBodyGoal, BodyGoal),
    maybe_unqualify_sym_name(Info, PredName0, PredName),
    maybe_output_line_number(Info, Context, !IO),
    (
        PredOrFunc = pf_predicate,
        mercury_output_pred_clause(VarSet, PredName, ArgTerms, BodyGoal, !IO)
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(ArgTerms, FuncArgTerms, ResultTerm),
        mercury_output_func_clause(VarSet, PredName, FuncArgTerms, ResultTerm,
            BodyGoal, !IO)
    ),
    io.write_string(".\n", !IO).

output_instance_method_clause(MethodName, ItemClause, !IO) :-
    ItemClause = item_clause_info(_PredName, PredOrFunc, ArgTerms, _MaybeAttrs,
        VarSet, MaybeBodyGoal, _Context, _SeqNum),
    get_clause_body_goal(MaybeBodyGoal, BodyGoal),
    (
        PredOrFunc = pf_predicate,
        mercury_output_pred_clause(VarSet, MethodName, ArgTerms, BodyGoal, !IO)
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(ArgTerms, FuncArgTerms, ResultTerm),
        mercury_output_func_clause(VarSet, MethodName,
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

:- pred mercury_output_pred_clause(prog_varset::in, sym_name::in,
    list(prog_term)::in, goal::in, io::di, io::uo) is det.

mercury_output_pred_clause(VarSet, PredName, Args, Body, !IO) :-
    mercury_output_sym_name(PredName, !IO),
    (
        Args = [HeadArg | TailArgs],
        io.write_string("(", !IO),
        mercury_format_comma_separated_terms(VarSet, print_name_only,
            HeadArg, TailArgs, !IO),
        io.write_string(")", !IO)
    ;
        Args = []
    ),
    ( if Body = true_expr(_) then
        true
    else
        io.write_string(" :-\n\t", !IO),
        mercury_output_goal(VarSet, 1, Body, !IO)
    ).

:- pred mercury_output_func_clause(prog_varset::in, sym_name::in,
    list(prog_term)::in, prog_term::in, goal::in, io::di, io::uo) is det.

mercury_output_func_clause(VarSet, PredName, Args, Result, Body, !IO) :-
    mercury_output_sym_name(PredName, !IO),
    (
        Args = [HeadArg | TailArgs],
        io.write_string("(", !IO),
        mercury_format_comma_separated_terms(VarSet, print_name_only,
            HeadArg, TailArgs, !IO),
        io.write_string(")", !IO)
    ;
        Args = []
    ),
    io.write_string(" = ", !IO),
    ( if Body = true_expr(_) then
        mercury_format_term_nq(VarSet, print_name_only, next_to_graphic_token,
            Result, !IO)
    else
        mercury_format_term(VarSet, print_name_only, Result, !IO),
        io.write_string(" :-\n\t", !IO),
        mercury_output_goal(VarSet, 1, Body, !IO)
    ).

%---------------------------------------------------------------------------%

mercury_output_goal(VarSet, Indent, Goal, !IO) :-
    (
        Goal = fail_expr(_),
        io.write_string("fail", !IO)
    ;
        Goal = true_expr(_),
        io.write_string("true", !IO)
    ;
        Goal = implies_expr(_, SubGoalA, SubGoalB),
        Indent1 = Indent + 1,
        io.write_string("(", !IO),
        mercury_output_newline(Indent1, !IO),
        mercury_output_connected_goal(VarSet, Indent1, SubGoalA, !IO),
        mercury_output_newline(Indent, !IO),
        io.write_string("=>", !IO),
        mercury_output_newline(Indent1, !IO),
        mercury_output_connected_goal(VarSet, Indent1, SubGoalB, !IO),
        mercury_output_newline(Indent, !IO),
        io.write_string(")", !IO)
    ;
        Goal = equivalent_expr(_, SubGoalA, SubGoalB),
        Indent1 = Indent + 1,
        io.write_string("(", !IO),
        mercury_output_newline(Indent1, !IO),
        mercury_output_connected_goal(VarSet, Indent1, SubGoalA, !IO),
        mercury_output_newline(Indent, !IO),
        io.write_string("<=>", !IO),
        mercury_output_newline(Indent1, !IO),
        mercury_output_connected_goal(VarSet, Indent1, SubGoalB, !IO),
        mercury_output_newline(Indent, !IO),
        io.write_string(")", !IO)
    ;
        Goal = quant_expr(QuantType, QuantVarsKind, _, Vars, SubGoal),
        (
            Vars = [],
            mercury_output_goal(VarSet, Indent, SubGoal, !IO)
        ;
            Vars = [_ | _],
            (
                QuantType = quant_some,
                io.write_string("some", !IO)
            ;
                QuantType = quant_all,
                io.write_string("all", !IO)
            ),
            io.write_string("[", !IO),
            (
                QuantVarsKind = quant_ordinary_vars,
                mercury_output_vars(VarSet, print_name_only, Vars, !IO)
            ;
                QuantVarsKind = quant_state_vars,
                mercury_output_state_vars(VarSet, print_name_only, Vars, !IO)
            ),
            io.write_string("] (", !IO),
            Indent1 = Indent + 1,
            mercury_output_newline(Indent1, !IO),
            mercury_output_goal(VarSet, Indent1, SubGoal, !IO),
            mercury_output_newline(Indent, !IO),
            io.write_string(")", !IO)
        )
    ;
        Goal = promise_equivalent_solutions_expr(_, Vars, StateVars,
            DotSVars, ColonSVars, SubGoal),
        mercury_output_promise_eqv_solutions_goal(VarSet, Indent,
            Vars, StateVars, DotSVars, ColonSVars, SubGoal,
            "promise_equivalent_solutions", !IO)
    ;
        Goal = promise_equivalent_solution_sets_expr(_, Vars, StateVars,
            DotSVars, ColonSVars, SubGoal),
        mercury_output_promise_eqv_solutions_goal(VarSet, Indent,
            Vars, StateVars, DotSVars, ColonSVars, SubGoal,
            "promise_equivalent_solution_sets", !IO)
    ;
        Goal = promise_equivalent_solution_arbitrary_expr(_, Vars, StateVars,
            DotSVars, ColonSVars, SubGoal),
        mercury_output_promise_eqv_solutions_goal(VarSet, Indent,
            Vars, StateVars, DotSVars, ColonSVars, SubGoal, "arbitrary", !IO)
    ;
        Goal = promise_purity_expr(_, Purity, SubGoal),
        (
            Purity = purity_pure,
            io.write_string("promise_pure (", !IO)
        ;
            Purity = purity_semipure,
            io.write_string("promise_semipure (", !IO)
        ;
            Purity = purity_impure,
            io.write_string("promise_impure (", !IO)
        ),
        Indent1 = Indent + 1,
        mercury_output_newline(Indent1, !IO),
        mercury_output_goal(VarSet, Indent1, SubGoal, !IO),
        mercury_output_newline(Indent, !IO),
        io.write_string(")", !IO)
    ;
        Goal = require_detism_expr(_, Detism, SubGoal),
        (
            Detism = detism_det,
            io.write_string("require_det", !IO)
        ;
            Detism = detism_semi,
            io.write_string("require_semidet", !IO)
        ;
            Detism = detism_multi,
            io.write_string("require_multi", !IO)
        ;
            Detism = detism_non,
            io.write_string("require_nondet", !IO)
        ;
            Detism = detism_cc_multi,
            io.write_string("require_cc_multi", !IO)
        ;
            Detism = detism_cc_non,
            io.write_string("require_cc_nondet", !IO)
        ;
            Detism = detism_erroneous,
            io.write_string("require_erroneous", !IO)
        ;
            Detism = detism_failure,
            io.write_string("require_failure", !IO)
        ),
        io.write_string(" (", !IO),
        Indent1 = Indent + 1,
        mercury_output_newline(Indent1, !IO),
        mercury_output_goal(VarSet, Indent1, SubGoal, !IO),
        mercury_output_newline(Indent, !IO),
        io.write_string(")", !IO)
    ;
        Goal = require_complete_switch_expr(_, Var, SubGoal),
        io.write_string("require_complete_switch [", !IO),
        mercury_output_plain_or_dot_var(VarSet, print_name_only, Var, !IO),
        io.write_string("] (", !IO),
        Indent1 = Indent + 1,
        mercury_output_newline(Indent1, !IO),
        mercury_output_goal(VarSet, Indent1, SubGoal, !IO),
        mercury_output_newline(Indent, !IO),
        io.write_string(")", !IO)
    ;
        Goal = require_switch_arms_detism_expr(_, Var, Detism, SubGoal),
        (
            Detism = detism_det,
            io.write_string("require_switch_arms_det", !IO)
        ;
            Detism = detism_semi,
            io.write_string("require_switch_arms_semidet", !IO)
        ;
            Detism = detism_multi,
            io.write_string("require_switch_arms_multi", !IO)
        ;
            Detism = detism_non,
            io.write_string("require_switch_arms_nondet", !IO)
        ;
            Detism = detism_cc_multi,
            io.write_string("require_switch_arms_cc_multi", !IO)
        ;
            Detism = detism_cc_non,
            io.write_string("require_switch_arms_cc_nondet", !IO)
        ;
            Detism = detism_erroneous,
            io.write_string("require_switch_arms_erroneous", !IO)
        ;
            Detism = detism_failure,
            io.write_string("require_switch_arms_failure", !IO)
        ),
        io.write_string(" [", !IO),
        mercury_output_plain_or_dot_var(VarSet, print_name_only, Var, !IO),
        io.write_string("] (", !IO),
        Indent1 = Indent + 1,
        mercury_output_newline(Indent1, !IO),
        mercury_output_goal(VarSet, Indent1, SubGoal, !IO),
        mercury_output_newline(Indent, !IO),
        io.write_string(")", !IO)
    ;
        Goal = disable_warnings_expr(_, HeadWarning, TailWarnings, SubGoal),
        io.write_string("disable_warnings [", !IO),
        write_goal_warnings(HeadWarning, TailWarnings, !IO),
        io.write_string("] (", !IO),
        Indent1 = Indent + 1,
        mercury_output_newline(Indent1, !IO),
        mercury_output_goal(VarSet, Indent1, SubGoal, !IO),
        mercury_output_newline(Indent, !IO),
        io.write_string(")", !IO)
    ;
        Goal = atomic_expr(_, Outer, Inner, _, MainGoal, OrElseGoals),
        io.write_string("atomic [outer(", !IO),
        (
            Outer = atomic_state_var(OVar),
            io.write_string("!", !IO),
            mercury_output_var(VarSet, print_name_only, OVar, !IO)
        ;
            Outer = atomic_var_pair(OuterDI, OuterUO),
            mercury_output_var(VarSet, print_name_only, OuterDI, !IO),
            io.write_string(", ", !IO),
            mercury_output_var(VarSet, print_name_only, OuterUO, !IO)
        ),
        io.write_string("), inner(", !IO),
        (
            Inner = atomic_state_var(IVar),
            io.write_string("!", !IO),
            mercury_output_var(VarSet, print_name_only, IVar, !IO)
        ;
            Inner = atomic_var_pair(InnerDI, InnerUO),
            mercury_output_var(VarSet, print_name_only, InnerDI, !IO),
            io.write_string(", ", !IO),
            mercury_output_var(VarSet, print_name_only, InnerUO, !IO)
        ),
        io.write_string(")] (", !IO),

        Indent1 = Indent + 1,
        mercury_output_newline(Indent1, !IO),
        mercury_output_orelse_goals(VarSet, Indent1, [MainGoal | OrElseGoals],
            !IO),
        mercury_output_newline(Indent, !IO),
        io.write_string(")", !IO)
    ;
        Goal = trace_expr(_, MaybeCompileTime, MaybeRunTime, MaybeIO,
            MutableVars, SubGoal),
        mercury_output_newline(Indent, !IO),
        io.write_string("trace [", !IO),
        some [!NeedComma] (
            !:NeedComma = no,
            (
                MaybeCompileTime = yes(CompileTime),
                mercury_output_trace_expr(mercury_output_trace_compiletime,
                    CompileTime, !IO),
                !:NeedComma = yes
            ;
                MaybeCompileTime = no
            ),
            (
                MaybeRunTime = yes(RunTime),
                mercury_output_comma_if_needed(!.NeedComma, !IO),
                mercury_output_trace_expr(mercury_output_trace_runtime,
                    RunTime, !IO),
                !:NeedComma = yes
            ;
                MaybeRunTime = no
            ),
            (
                MaybeIO = yes(IOStateVar),
                mercury_output_comma_if_needed(!.NeedComma, !IO),
                io.write_string("io(!", !IO),
                mercury_output_var(VarSet, print_name_only, IOStateVar, !IO),
                io.write_string(")", !IO),
                !:NeedComma = yes
            ;
                MaybeIO = no
            ),
            list.foldl2(
                mercury_output_trace_mutable_var_and_comma(VarSet,
                    print_name_only),
                MutableVars, !.NeedComma, _, !IO)
        ),
        io.write_string("]", !IO),
        mercury_output_newline(Indent + 1, !IO),
        mercury_output_goal(VarSet, Indent + 1, SubGoal, !IO),
        mercury_output_newline(Indent, !IO),
        io.write_string(")", !IO)
    ;
        Goal = try_expr(_, MaybeIO, SubGoal, Then, MaybeElse, Catches,
            MaybeCatchAny),
        io.write_string("(try [", !IO),
        (
            MaybeIO = yes(IOStateVar),
            io.write_string("io(!", !IO),
            mercury_output_var(VarSet, print_name_only, IOStateVar, !IO),
            io.write_string(")", !IO)
        ;
            MaybeIO = no
        ),
        io.write_string("] (", !IO),
        Indent1 = Indent + 1,
        mercury_output_newline(Indent1, !IO),
        mercury_output_goal(VarSet, Indent1, SubGoal, !IO),
        mercury_output_newline(Indent, !IO),
        io.write_string(")", !IO),
        mercury_output_newline(Indent, !IO),
        io.write_string("then", !IO),
        mercury_output_newline(Indent1, !IO),
        mercury_output_goal(VarSet, Indent1, Then, !IO),
        mercury_output_newline(Indent, !IO),
        (
            MaybeElse = yes(Else),
            io.write_string("else", !IO),
            mercury_output_newline(Indent1, !IO),
            mercury_output_goal(VarSet, Indent1, Else, !IO)
        ;
            MaybeElse = no
        ),
        list.foldl(mercury_output_catch(VarSet, Indent), Catches, !IO),
        (
            MaybeCatchAny = yes(catch_any_expr(CatchAnyVar, CatchAnyGoal)),
            io.write_string("catch_any ", !IO),
            mercury_output_var(VarSet, print_name_only, CatchAnyVar, !IO),
            io.write_string(" ->", !IO),
            mercury_output_newline(Indent1, !IO),
            mercury_output_goal(VarSet, Indent1, CatchAnyGoal, !IO)
        ;
            MaybeCatchAny = no
        ),
        mercury_output_newline(Indent, !IO),
        io.write_string(")", !IO)
    ;
        Goal = if_then_else_expr(_, Vars, StateVars, Cond, Then, Else),
        io.write_string("(if", !IO),
        mercury_output_some(VarSet, Vars, StateVars, !IO),
        Indent1 = Indent + 1,
        mercury_output_newline(Indent1, !IO),
        mercury_output_goal(VarSet, Indent1, Cond, !IO),
        mercury_output_newline(Indent, !IO),
        io.write_string("then", !IO),
        mercury_output_newline(Indent1, !IO),
        mercury_output_goal(VarSet, Indent1, Then, !IO),
        mercury_output_newline(Indent, !IO),
        io.write_string("else", !IO),
        mercury_output_newline(Indent1, !IO),
        mercury_output_goal(VarSet, Indent1, Else, !IO),
        mercury_output_newline(Indent, !IO),
        io.write_string(")", !IO)
    ;
        Goal = not_expr(_, SubGoal),
        io.write_string("\\+ (", !IO),
        Indent1 = Indent + 1,
        mercury_output_newline(Indent1, !IO),
        mercury_output_goal(VarSet, Indent1, SubGoal, !IO),
        mercury_output_newline(Indent, !IO),
        io.write_string(")", !IO)
    ;
        Goal = conj_expr(_, SubGoalA, SubGoalB),
        mercury_output_goal(VarSet, Indent, SubGoalA, !IO),
        io.write_string(",", !IO),
        mercury_output_newline(Indent, !IO),
        mercury_output_goal(VarSet, Indent, SubGoalB, !IO)
    ;
        Goal = par_conj_expr(_, SubGoalA, SubGoalB),
        io.write_string("(", !IO),
        Indent1 = Indent + 1,
        mercury_output_newline(Indent1, !IO),
        mercury_output_goal(VarSet, Indent1, SubGoalA, !IO),
        mercury_output_par_conj(VarSet, Indent, SubGoalB, !IO),
        mercury_output_newline(Indent, !IO),
        io.write_string(")", !IO)
    ;
        Goal = disj_expr(_, SubGoalA, SubGoalB),
        io.write_string("(", !IO),
        Indent1 = Indent + 1,
        mercury_output_newline(Indent1, !IO),
        mercury_output_goal(VarSet, Indent1, SubGoalA, !IO),
        mercury_output_disj(VarSet, Indent, SubGoalB, !IO),
        mercury_output_newline(Indent, !IO),
        io.write_string(")", !IO)
    ;
        Goal = event_expr(_, Name, Terms),
        io.write_string("event ", !IO),
        mercury_output_call(VarSet, unqualified(Name), Terms, !IO)
    ;
        Goal = call_expr(_, Name, Terms, Purity),
        write_purity_prefix(Purity, !IO),
        mercury_output_call(VarSet, Name, Terms, !IO)
    ;
        Goal = unify_expr(_, TermA, TermB, Purity),
        write_purity_prefix(Purity, !IO),
        mercury_output_term(VarSet, print_name_only, TermA, !IO),
        io.write_string(" = ", !IO),
        mercury_output_term_nq(VarSet, print_name_only, next_to_graphic_token,
            TermB, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_output_connected_goal(prog_varset::in, int::in, goal::in,
    io::di, io::uo) is det.

mercury_output_connected_goal(VarSet, Indent, Goal, !IO) :-
    (
        ( Goal = fail_expr(_)
        ; Goal = true_expr(_)
        ; Goal = implies_expr(_, _, _)
        ; Goal = equivalent_expr(_, _, _)
        ; Goal = try_expr(_, _, _, _, _, _, _)
        ; Goal = if_then_else_expr(_, _, _, _, _, _)
        ; Goal = not_expr(_, _)
        ; Goal = par_conj_expr(_, _, _)
        ; Goal = disj_expr(_, _, _)
        ; Goal = event_expr(_, _, _)
        ; Goal = call_expr(_, _, _, _)
        ; Goal = unify_expr(_, _, _, _)
        ),
        mercury_output_goal(VarSet, Indent, Goal, !IO)
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
        io.write_string("(", !IO),
        Indent1 = Indent + 1,
        mercury_output_newline(Indent1, !IO),
        mercury_output_goal(VarSet, Indent1, Goal, !IO),
        mercury_output_newline(Indent, !IO),
        io.write_string(")", !IO)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_output_plain_or_dot_var(prog_varset::in, var_name_print::in,
    plain_or_dot_var::in, io::di, io::uo) is det.

mercury_output_plain_or_dot_var(VarSet, VarNamePrint, PODVar, !IO) :-
    (
        PODVar = podv_plain(Var),
        mercury_output_var(VarSet, VarNamePrint, Var, !IO)
    ;
        PODVar = podv_dot(Var),
        io.write_string("!.", !IO),
        mercury_output_var(VarSet, VarNamePrint, Var, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_output_call(prog_varset::in, sym_name::in, list(prog_term)::in,
    io::di, io::uo) is det.

mercury_output_call(VarSet, SymName, Term, !IO) :-
    (
        SymName = qualified(ModuleName, PredName),
        mercury_output_bracketed_sym_name_ngt(next_to_graphic_token,
            ModuleName, !IO),
        io.write_string(".", !IO),
        term.context_init(Context0),
        SubTerm = term.functor(term.atom(PredName), Term, Context0),
        mercury_output_term_nq(VarSet, print_name_only, next_to_graphic_token,
            SubTerm, !IO)
    ;
        SymName = unqualified(PredName),
        term.context_init(Context0),
        SubTerm = term.functor(term.atom(PredName), Term, Context0),
        mercury_output_term_nq(VarSet, print_name_only, next_to_graphic_token,
            SubTerm, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_output_disj(prog_varset::in, int::in, goal::in,
    io::di, io::uo) is det.

mercury_output_disj(VarSet, Indent, Goal, !IO) :-
    mercury_output_newline(Indent, !IO),
    io.write_string(";", !IO),
    Indent1 = Indent + 1,
    mercury_output_newline(Indent1, !IO),
    ( if Goal = disj_expr(_, SubGoalA, SubGoalB) then
        mercury_output_goal(VarSet, Indent1, SubGoalA, !IO),
        mercury_output_disj(VarSet, Indent, SubGoalB, !IO)
    else
        mercury_output_goal(VarSet, Indent1, Goal, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_output_par_conj(prog_varset::in, int::in, goal::in,
    io::di, io::uo) is det.

mercury_output_par_conj(VarSet, Indent, Goal, !IO) :-
    mercury_output_newline(Indent, !IO),
    io.write_string("&", !IO),
    Indent1 = Indent + 1,
    mercury_output_newline(Indent1, !IO),
    ( if Goal = par_conj_expr(_, SubGoalA, SubGoalB) then
        mercury_output_goal(VarSet, Indent1, SubGoalA, !IO),
        mercury_output_par_conj(VarSet, Indent, SubGoalB, !IO)
    else
        mercury_output_goal(VarSet, Indent1, Goal, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_output_orelse_goals(prog_varset::in, int::in, list(goal)::in,
    io::di, io::uo) is det.

mercury_output_orelse_goals(VarSet, Indent, Goals, !IO) :-
    (
        Goals = []
    ;
        Goals = [HeadGoal | TailGoals],
        (
            TailGoals = [],
            mercury_output_goal(VarSet, Indent + 1, HeadGoal, !IO)
        ;
            TailGoals = [_|_],
            mercury_output_goal(VarSet, Indent + 1, HeadGoal, !IO),
            mercury_output_newline(Indent, !IO),
            io.write_string("orelse", !IO),
            mercury_output_newline(Indent, !IO),
            mercury_output_orelse_goals(VarSet, Indent, TailGoals, !IO)
        )
    ).

%---------------------------------------------------------------------------%

:- pred mercury_output_some(varset(T)::in, list(var(T))::in, list(var(T))::in,
    io::di, io::uo) is det.

mercury_output_some(VarSet, Vars, StateVars, !IO) :-
    ( if
        ( Vars = [_ | _]
        ; StateVars = [_ | _]
        )
    then
        io.write_string(" some [", !IO),
        mercury_output_vars(VarSet, print_name_only, Vars, !IO),
        ( if
            Vars = [_ | _],
            StateVars = [_ | _]
        then
            io.write_string(", ", !IO),
            % XXX BUG: we should print StateVars even if Vars = [].
            mercury_output_state_vars(VarSet, print_name_only, StateVars, !IO)
        else
            true
        ),
        io.write_string("]", !IO)
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred mercury_output_promise_eqv_solutions_goal(prog_varset::in, int::in,
    list(prog_var)::in, list(prog_var)::in, list(prog_var)::in,
    list(prog_var)::in, goal::in, string::in, io::di, io::uo) is det.

mercury_output_promise_eqv_solutions_goal(VarSet, Indent,
        Vars, StateVars, DotSVars, ColonSVars, Goal, Keyword, !IO) :-
    ( if
        Vars = [],
        StateVars = [],
        DotSVars = [],
        ColonSVars = []
    then
        % This should have been caught be parse_goal when reading in
        % the term, but there is no point in aborting here.
        mercury_output_goal(VarSet, Indent, Goal, !IO)
    else
        io.write_string(Keyword, !IO),
        io.write_string(" [", !IO),
        mercury_output_vars(VarSet, print_name_only, Vars, !IO),
        ( if
            Vars = [_ | _],
            StateVars = [_ | _]
        then
            io.write_string(", ", !IO)
        else
            true
        ),
        mercury_output_state_vars_using_prefix(VarSet, print_name_only,
            "!", StateVars, !IO),
        ( if
            ( Vars = [_ | _]
            ; StateVars = [_ | _]
            ),
            DotSVars = [_ | _]
        then
            io.write_string(", ", !IO)
        else
            true
        ),
        mercury_output_state_vars_using_prefix(VarSet, print_name_only,
            "!.", DotSVars, !IO),
        ( if
            ( Vars = [_ | _]
            ; StateVars = [_ | _]
            ; DotSVars = [_ | _]
            ),
            ColonSVars = [_ | _]
        then
            io.write_string(", ", !IO)
        else
            true
        ),
        mercury_output_state_vars_using_prefix(VarSet, print_name_only,
            "!:", ColonSVars, !IO),
        io.write_string("] (", !IO),
        Indent1 = Indent + 1,
        mercury_output_newline(Indent1, !IO),
        mercury_output_goal(VarSet, Indent1, Goal, !IO),
        mercury_output_newline(Indent, !IO),
        io.write_string(")", !IO)
    ).

:- pred mercury_output_state_vars_using_prefix(prog_varset::in,
    var_name_print::in, string::in, list(prog_var)::in, io::di, io::uo) is det.

mercury_output_state_vars_using_prefix(_VarSet, _VarNamePrint, _BangPrefix,
        [], !IO).
mercury_output_state_vars_using_prefix(VarSet, VarNamePrint, BangPrefix,
        [SVar | SVars], !IO) :-
    io.write_string(BangPrefix, !IO),
    mercury_format_var(VarSet, VarNamePrint, SVar, !IO),
    (
        SVars = [_ | _],
        io.write_string(", ", !IO),
        mercury_output_state_vars_using_prefix(VarSet, VarNamePrint,
            BangPrefix, SVars, !IO)
    ;
        SVars = []
    ).

%---------------------------------------------------------------------------%

write_goal_warnings(HeadWarning, TailWarnings, !IO) :-
    io.write_string(goal_warning_to_string(HeadWarning), !IO),
    (
        TailWarnings = []
    ;
        TailWarnings = [HeadTailWarning | TailTailWarnings],
        io.write_string(", ", !IO),
        write_goal_warnings(HeadTailWarning, TailTailWarnings, !IO)
    ).

%---------------------------------------------------------------------------%

mercury_output_trace_expr(PrintBase, TraceExpr, !IO) :-
    (
        TraceExpr = trace_base(Base),
        PrintBase(Base, !IO)
    ;
        TraceExpr = trace_not(TraceExprA),
        io.write_string("not(", !IO),
        mercury_output_trace_expr(PrintBase, TraceExprA, !IO),
        io.write_string(")", !IO)
    ;
        TraceExpr = trace_op(trace_or, TraceExprA, TraceExprB),
        io.write_string("(", !IO),
        mercury_output_trace_expr(PrintBase, TraceExprA, !IO),
        io.write_string(") or (", !IO),
        mercury_output_trace_expr(PrintBase, TraceExprB, !IO),
        io.write_string(")", !IO)
    ;
        TraceExpr = trace_op(trace_and, TraceExprA, TraceExprB),
        mercury_output_trace_expr(PrintBase, TraceExprA, !IO),
        io.write_string(" and ", !IO),
        mercury_output_trace_expr(PrintBase, TraceExprB, !IO)
    ).

mercury_output_trace_compiletime(CompileTime, !IO) :-
    (
        CompileTime = trace_flag(FlagName),
        io.write_string("flag(", !IO),
        term_io.quote_string(FlagName, !IO),
        io.write_string(")", !IO)
    ;
        CompileTime = trace_grade(Grade),
        parse_trace_grade_name(GradeName, Grade),
        io.write_string("grade(", !IO),
        io.write_string(GradeName, !IO),
        io.write_string(")", !IO)
    ;
        CompileTime = trace_trace_level(Level),
        io.write_string("tracelevel(", !IO),
        (
            Level = trace_level_shallow,
            io.write_string("shallow", !IO)
        ;
            Level = trace_level_deep,
            io.write_string("deep", !IO)
        ),
        io.write_string(")", !IO)
    ).

mercury_output_trace_runtime(trace_envvar(EnvVarName), !IO) :-
    io.write_string("env(", !IO),
    term_io.quote_string(EnvVarName, !IO),
    io.write_string(")", !IO).

%---------------------------------------------------------------------------%

:- pred mercury_output_trace_mutable_var(prog_varset::in, var_name_print::in,
    trace_mutable_var::in, io::di, io::uo) is det.

mercury_output_trace_mutable_var(VarSet, VarNamePrint, MutableVar, !IO) :-
    MutableVar = trace_mutable_var(MutableName, StateVar),
    io.write_string("state(", !IO),
    io.write_string(MutableName, !IO),
    io.write_string(", !", !IO),
    mercury_output_var(VarSet, VarNamePrint, StateVar, !IO),
    io.write_string(")", !IO).

:- pred mercury_output_trace_mutable_var_and_comma(prog_varset::in,
    var_name_print::in, trace_mutable_var::in, bool::in, bool::out,
    io::di, io::uo) is det.

mercury_output_trace_mutable_var_and_comma(VarSet, VarNamePrint,
        MutableVar, !NeedComma, !IO) :-
    mercury_output_comma_if_needed(!.NeedComma, !IO),
    !:NeedComma = yes,
    mercury_output_trace_mutable_var(VarSet, VarNamePrint, MutableVar, !IO).

%---------------------------------------------------------------------------%

:- pred mercury_output_catch(prog_varset::in, int::in, catch_expr::in,
    io::di, io::uo) is det.

mercury_output_catch(VarSet, Indent, catch_expr(Pattern, Goal), !IO) :-
    io.write_string("catch ", !IO),
    mercury_output_term(VarSet, print_name_only, Pattern, !IO),
    io.write_string(" ->", !IO),
    mercury_output_newline(Indent + 1, !IO),
    mercury_output_goal(VarSet, Indent + 1, Goal, !IO),
    mercury_output_newline(Indent, !IO).

%---------------------------------------------------------------------------%

:- pred mercury_output_comma_if_needed(bool::in, io::di, io::uo) is det.

mercury_output_comma_if_needed(no, !IO).
mercury_output_comma_if_needed(yes, !IO) :-
    io.write_string(", ", !IO).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_tree_out_clause.
%---------------------------------------------------------------------------%
