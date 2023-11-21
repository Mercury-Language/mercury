%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2008-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: hlds_desc.m.
% Main author: zs.
%
% This module's functions are intended to generate short descriptions
% of parts of the HLDS for use in debugging messages, and as comments
% in generated code. Whereas the code in hlds_out.m is intended to
% completely describe a given construct, the code here is intended
% to orient programmers about what part of the HLDS is currently
% being processed.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module hlds.hlds_desc.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.var_table.

:- import_module list.

%-----------------------------------------------------------------------------%

    % Return a short, less than one line description of the given goal,
    % given the ModuleInfo and the var_table of the procedure the goal is from.
    %
    % The returned string won't end with a newline.
    %
:- func describe_goal(module_info, var_table, hlds_goal) = string.

    % Return a short but nevertheless structured description of the given goal,
    % given the ModuleInfo and the var_table of the procedure the goal is from.
    % The description will have at most one line per atomic goal, but
    % the descriptions of structured goals will be longer.
    %
    % The returned string will consist of one or more line lines,
    % each ending with a newline.
    %
:- func describe_structured_goal(module_info, var_table, int, hlds_goal)
    = string.

    % If the list is empty, return the empty string; if the list contains
    % some variables, return the descriptions of those variables between
    % parentheses. The var_table should be the var_table of the procedure the
    % arguments are from.
    %
:- func describe_args(var_table, list(prog_var)) = string.

    % Return a description of the given variable, given the var_table of the
    % procedure it is from.
    %
:- func describe_var(var_table, prog_var) = string.

    % This type is isomorphic to should_module_qualify in hlds_error_util.m,
    % but we want to allow that module and this one to be used independently
    % of each other.
:- type include_module_name
    --->    do_not_include_module_name
    ;       include_module_name.

    % Return a description of the given predicate.
    %
:- func describe_pred_from_id(include_module_name, module_info, pred_id)
    = string.
:- func describe_pred(include_module_name, pred_info) = string.

    % Return a description of the given procedure of the given predicate.
    %
:- func describe_proc_from_id(include_module_name, module_info, pred_proc_id)
    = string.
:- func describe_proc(include_module_name, pred_info, proc_id) = string.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.pred_name.
:- import_module libs.
:- import_module libs.indent.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_tree_out_cons_id.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.parse_tree_out_sym_name.
:- import_module parse_tree.parse_tree_out_term.

:- import_module string.
:- import_module term_context.

describe_goal(ModuleInfo, VarTable, Goal) = FullDesc :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    (
        GoalExpr = unify(_, _, _, Unification, _),
        Desc = describe_unify(VarTable, Unification)
    ;
        GoalExpr = plain_call(_, _, ArgVars, _, _, SymName),
        Desc = describe_plain_call(VarTable, SymName, ArgVars)
    ;
        GoalExpr = generic_call(GCall, ArgVars, _, _, _),
        Desc = describe_generic_call(VarTable, GCall, ArgVars)
    ;
        GoalExpr = call_foreign_proc(_, PredId, _, Args, ExtraArgs, _, _),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        Name = pred_info_name(PredInfo),
        ArgVars = list.map(foreign_arg_var, Args),
        ExtraVars = list.map(foreign_arg_var, ExtraArgs),
        string.format("foreign %s%s%s",
            [s(Name), s(describe_args(VarTable, ArgVars)),
            s(describe_args(VarTable, ExtraVars))], Desc)
    ;
        GoalExpr = conj(ConjType, SubGoals),
        (
            SubGoals = [],
            Desc = "true"
        ;
            SubGoals = [_ | _],
            ( ConjType = plain_conj,    ConjStr = "conj"
            ; ConjType = parallel_conj, ConjStr = "par_conj"
            ),
            list.length(SubGoals, NumSubGoals),
            string.format("%s(%d subgoals)", [s(ConjStr), i(NumSubGoals)],
                Desc)
        )
    ;
        GoalExpr = disj(SubGoals),
        (
            SubGoals = [],
            Desc = "fail"
        ;
            SubGoals = [_ | _],
            list.length(SubGoals, NumSubGoals),
            string.format("disj(%d subgoals)", [i(NumSubGoals)], Desc)
        )
    ;
        GoalExpr = switch(Var, _, _),
        Desc = "switch on " ++ describe_var(VarTable, Var)
    ;
        GoalExpr = negation(_),
        Desc = "negation"
    ;
        GoalExpr = scope(Reason, _),
        Desc = describe_scope_reason(Reason)
    ;
        GoalExpr = if_then_else(_, _, _, _),
        Desc = "if_then_else"
    ;
        GoalExpr = shorthand(_),
        Desc = "shorthand"
    ),
    Context = goal_info_get_context(GoalInfo),
    Line = term_context.context_line(Context),
    FullDesc = Desc ++ "@" ++ int_to_string(Line).

describe_structured_goal(ModuleInfo, VarTable, Indent, Goal) = Desc :-
    Lines = do_describe_structured_goal(ModuleInfo, VarTable, Goal),
    ( if Indent = 0 then
        IndentedLines = Lines
    else
        IndentedLines = list.map(add_indent2_prefix(Indent), Lines)
    ),
    string.append_list(IndentedLines, Desc).

:- func do_describe_structured_goal(module_info, var_table, hlds_goal) =
    list(string).

do_describe_structured_goal(ModuleInfo, VarTable, Goal) = Lines :-
    Goal = hlds_goal(GoalExpr, _GoalInfo),
    (
        GoalExpr = unify(_, _, _, Unification, _),
        Desc = describe_unify(VarTable, Unification),
        Lines = desc_to_lines(Desc)
    ;
        GoalExpr = plain_call(_, _, ArgVars, _, _, SymName),
        Desc = describe_plain_call(VarTable, SymName, ArgVars),
        Lines = desc_to_lines(Desc)
    ;
        GoalExpr = generic_call(GCall, ArgVars, _, _, _),
        Desc = describe_generic_call(VarTable, GCall, ArgVars),
        Lines = desc_to_lines(Desc)
    ;
        GoalExpr = call_foreign_proc(_, PredId, _, Args, ExtraArgs, _, _),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        Name = pred_info_name(PredInfo),
        ArgVars = list.map(foreign_arg_var, Args),
        ExtraVars = list.map(foreign_arg_var, ExtraArgs),
        string.format("foreign %s%s%s",
            [s(Name), s(describe_args(VarTable, ArgVars)),
            s(describe_args(VarTable, ExtraVars))], Desc),
        Lines = desc_to_lines(Desc)
    ;
        GoalExpr = conj(ConjType, SubGoals),
        (
            SubGoals = [],
            Lines = ["true\n"]
        ;
            SubGoals = [_ | _],
            ( ConjType = plain_conj,    ConjStr = "conj"
            ; ConjType = parallel_conj, ConjStr = "par_conj"
            ),
            string.format("%s(\n", [s(ConjStr)], FirstLine),
            SubGoalLineLists = list.map(
                do_describe_structured_goal(ModuleInfo, VarTable), SubGoals),
            list.condense(SubGoalLineLists, SubGoalLines),
            IndentedSubGoalLines = list.map(indent_desc_line, SubGoalLines),
            LastLine = ")\n",
            Lines = [FirstLine | IndentedSubGoalLines] ++ [LastLine]
        )
    ;
        GoalExpr = disj(SubGoals),
        (
            SubGoals = [],
            Lines = ["fail\n"]
        ;
            SubGoals = [HeadSubGoal | TailSubGoals],
            FirstLine = "disj(\n",
            HeadSubGoalLineList = do_describe_structured_goal(
                ModuleInfo, VarTable, HeadSubGoal),
            TailSubGoalLineLists = list.map(do_describe_structured_goal(
                ModuleInfo, VarTable), TailSubGoals),
            DisjunctLines =
                disjunction_lines(HeadSubGoalLineList, TailSubGoalLineLists),
            LastLine = ")\n",
            Lines = [FirstLine | DisjunctLines] ++ [LastLine]
        )
    ;
        GoalExpr = switch(Var, _, Cases),
        string.format("switch(%s,\n", [s(describe_var(VarTable, Var))],
            FirstLine),
        CaseLineLists = list.map(
            do_describe_structured_case(ModuleInfo, VarTable), Cases),
        list.condense(CaseLineLists, CaseLines),
        IndentedCaseLines = list.map(indent_desc_line, CaseLines),
        LastLine = ")\n",
        Lines = [FirstLine | IndentedCaseLines] ++ [LastLine]
    ;
        GoalExpr = negation(SubGoal),
        FirstLine = "not(\n",
        SubGoalLines =
            do_describe_structured_goal(ModuleInfo, VarTable, SubGoal),
        IndentedSubGoalLines = list.map(indent_desc_line, SubGoalLines),
        LastLine = ")\n",
        Lines = [FirstLine | IndentedSubGoalLines] ++ [LastLine]
    ;
        GoalExpr = scope(Reason, SubGoal),
        ReasonDesc = describe_scope_reason(Reason),
        string.format("scope(%s,\n", [s(ReasonDesc)], FirstLine),
        SubGoalLines =
            do_describe_structured_goal(ModuleInfo, VarTable, SubGoal),
        IndentedSubGoalLines = list.map(indent_desc_line, SubGoalLines),
        LastLine = ")\n",
        Lines = [FirstLine | IndentedSubGoalLines] ++ [LastLine]
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        CondLines =
            do_describe_structured_goal(ModuleInfo, VarTable, Cond),
        ITE1 = "( if\n",
        ITE2 = "then\n",
        ITE3 = "else\n",
        ITE4 = ")\n",
        IndentedCondLines = list.map(indent_desc_line, CondLines),
        ThenLines =
            do_describe_structured_goal(ModuleInfo, VarTable, Then),
        IndentedThenLines = list.map(indent_desc_line, ThenLines),
        ElseLines =
            do_describe_structured_goal(ModuleInfo, VarTable, Else),
        IndentedElseLines = list.map(indent_desc_line, ElseLines),
        Lines =
            [ITE1 | IndentedCondLines] ++
            [ITE2 | IndentedThenLines] ++
            [ITE3 | IndentedElseLines] ++
            [ITE4]
    ;
        GoalExpr = shorthand(_),
        Desc = "shorthand",
        Lines = desc_to_lines(Desc)
    ).

:- func do_describe_structured_case(module_info, var_table, case) =
    list(string).

do_describe_structured_case(ModuleInfo, VarTable, Case) = Lines :-
    Case = case(MainConsId, OtherConsIds, Goal),
    MainConsIdStr = cons_id_and_arity_to_string(MainConsId),
    OtherConsIdStrs = list.map(cons_id_and_arity_to_string, OtherConsIds),
    ConsIdStr = string.join_list(" or ", [MainConsIdStr | OtherConsIdStrs]),
    string.format("case %s:\n", [s(ConsIdStr)], FirstLine),
    GoalLines = do_describe_structured_goal(ModuleInfo, VarTable, Goal),
    IndentedGoalLines = list.map(indent_desc_line, GoalLines),
    Lines = [FirstLine | IndentedGoalLines].

:- func desc_to_lines(string) = list(string).

desc_to_lines(Desc) = [Desc ++ "\n"].

:- func disjunction_lines(list(string), list(list(string))) = list(string).

disjunction_lines(HeadGroup, TailGroups) = Lines :-
    IndentedHeadLines = list.map(indent_desc_line, HeadGroup),
    (
        TailGroups = [],
        Lines = IndentedHeadLines
    ;
        TailGroups = [HeadTailGroup | TailTailGroups],
        TailLines = disjunction_lines(HeadTailGroup, TailTailGroups),
        Lines = IndentedHeadLines ++ [";\n" | TailLines]
    ).

:- func indent_desc_line(string) = string.

indent_desc_line(Line) = "  " ++ Line.

%-----------------------------------------------------------------------------%

:- func describe_unify(var_table, unification) = string.

describe_unify(VarTable, Unification) = Desc :-
    (
        (
            Unification = construct(Var, ConsId, Args, _, _, _, _),
            OpStr = "<="
        ;
            Unification = deconstruct(Var, ConsId, Args, _, _, _),
            OpStr = "=>"
        ),
        VarStr = describe_var(VarTable, Var),
        ConsIdStr = cons_id_and_arity_to_string(ConsId),
        ArgsStr = describe_args(VarTable, Args),
        string.format("%s %s %s%s",
            [s(VarStr), s(OpStr), s(ConsIdStr), s(ArgsStr)], Desc)
    ;
        (
            Unification = assign(VarA, VarB),
            OpStr = ":="
        ;
            Unification = simple_test(VarA, VarB),
            OpStr = "-="
        ),
        string.format("%s %s %s",
            [s(describe_var(VarTable, VarA)), s(OpStr),
            s(describe_var(VarTable, VarB))], Desc)
    ;
        Unification = complicated_unify(_, _, _),
        Desc = "complicated unify"
    ).

:- func describe_plain_call(var_table, sym_name, list(prog_var)) = string.

describe_plain_call(VarTable, SymName, ArgVars) = Desc :-
    Desc = sym_name_to_string(SymName) ++ describe_args(VarTable, ArgVars).

:- func describe_generic_call(var_table, generic_call, list(prog_var))
    = string.

describe_generic_call(VarTable, GCall, ArgVars) = Desc :-
    ArgsStr = describe_args(VarTable, ArgVars),
    (
        GCall = higher_order(Var, _, _, _),
        string.format("%s%s",
            [s(describe_var(VarTable, Var)), s(ArgsStr)], Desc)
    ;
        GCall = class_method(Var, _, _, MethodSNA),
        string.format("%s[%s]%s",
            [s(pf_sym_name_pred_form_arity_to_string(MethodSNA)),
            s(describe_var(VarTable, Var)), s(ArgsStr)], Desc)
    ;
        GCall = event_call(EventName),
        string.format("%s %s", [s(EventName), s(ArgsStr)], Desc)
    ;
        GCall = cast(CastType),
        string.format("%s %s",
            [s(describe_cast(CastType)), s(ArgsStr)], Desc)
    ).

:- func describe_cast(cast_kind) = string.

describe_cast(CastType) = Desc :-
    (
        CastType = unsafe_type_cast,
        Desc = "unsafe_type_cast"
    ;
        CastType = unsafe_type_inst_cast,
        Desc = "unsafe_type_inst_cast"
    ;
        CastType = equiv_type_cast,
        Desc = "equiv_type_cast"
    ;
        CastType = exists_cast,
        Desc = "exists_cast"
    ;
        CastType = subtype_coerce,
        Desc = "coerce"
    ).

:- func describe_scope_reason(scope_reason) = string.

describe_scope_reason(Reason) = Desc :-
    (
        Reason = disable_warnings(_, _),
        Desc = "disable warnings"
    ;
        Reason = exist_quant(_, _),
        Desc = "scope exist quant"
    ;
        Reason = promise_solutions(_, _),
        Desc = "scope promise solutions"
    ;
        Reason = promise_purity(_),
        Desc = "scope promise purity"
    ;
        Reason = require_detism(_),
        Desc = "scope require detism"
    ;
        Reason = require_complete_switch(_),
        Desc = "scope require complete switch"
    ;
        Reason = require_switch_arms_detism(_, _),
        Desc = "scope require switch arm detism"
    ;
        Reason = commit(_),
        Desc = "scope commit"
    ;
        Reason = barrier(_),
        Desc = "scope barrier"
    ;
        Reason = from_ground_term(_, _),
        Desc = "scope from_ground_term"
    ;
        Reason = trace_goal(_, _, _, _, _),
        Desc = "scope trace goal"
    ;
        Reason = loop_control(_, _, _),
        Desc = "scope loop control goal"
    ).

%---------------------------------------------------------------------------%

describe_args(_, []) = "".
describe_args(VarTable, Vars @ [_ | _]) = Str :-
    VarStrs = list.map(describe_var(VarTable), Vars),
    VarsStr = string.join_list(", ", VarStrs),
    string.format("(%s)", [s(VarsStr)], Str).

%---------------------------------------------------------------------------%

describe_var(VarTable, Var) =
    mercury_var_to_string(VarTable, print_name_and_num, Var).

%---------------------------------------------------------------------------%

describe_pred_from_id(IncludeModuleName, ModuleInfo, PredId) = PredDesc :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    PredDesc = describe_pred(IncludeModuleName, PredInfo).

describe_pred(IncludeModuleName, PredInfo) = ProcDesc :-
    % XXX This function should subcontract its work to pred_name.m.
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    PredName = pred_info_name(PredInfo),
    user_arity(Arity) = pred_info_user_arity(PredInfo),
    pred_info_get_origin(PredInfo, Origin),
    ( if Origin = origin_compiler(made_for_uci(SpecialId, TypeCtor)) then
        FullPredName = string.format("%s_for_%s",
            [s(get_special_pred_id_generic_name(SpecialId)),
            s(arg_type_ctor_name_to_string(TypeCtor))])
    else
        FullPredName = PredName
    ),
    (
        IncludeModuleName = do_not_include_module_name,
        ProcDesc = string.format("%s %s/%d",
            [s(pred_or_func_to_str(PredOrFunc)), s(FullPredName), i(Arity)])
    ;
        IncludeModuleName = include_module_name,
        ModuleName = pred_info_module(PredInfo),
        ProcDesc = string.format("%s %s.%s/%d",
            [s(pred_or_func_to_str(PredOrFunc)),
            s(sym_name_to_string(ModuleName)), s(FullPredName), i(Arity)])
    ).

:- func arg_type_ctor_name_to_string(type_ctor) = string.

arg_type_ctor_name_to_string(TypeCtor) = TypeCtorStr :-
    TypeCtor = type_ctor(TypeCtorSymName, TypeCtorArity),
    TypeCtorStr = string.format("%s_%d",
        [s(sym_name_to_string(TypeCtorSymName)), i(TypeCtorArity)]).

%---------------------------------------------------------------------------%

describe_proc_from_id(IncludeModuleName, ModuleInfo, PredProcId) = ProcDesc :-
    PredProcId = proc(PredId, ProcId),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ProcDesc = describe_proc(IncludeModuleName, PredInfo, ProcId).

describe_proc(IncludeModuleName, PredInfo, ProcId) = ProcDesc :-
    PredDesc = describe_pred(IncludeModuleName, PredInfo),
    ProcDesc = string.format("%s-%d",
        [s(PredDesc), i(proc_id_to_int(ProcId))]).

%---------------------------------------------------------------------------%
:- end_module hlds.hlds_desc.
%---------------------------------------------------------------------------%
