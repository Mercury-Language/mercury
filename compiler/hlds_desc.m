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
:- func describe_goal(module_info, var_table, hlds_goal) = string.

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
        )
    ;
        GoalExpr = plain_call(_, _, Args, _, _, SymName),
        Desc = sym_name_to_string(SymName) ++ describe_args(VarTable, Args)
    ;
        GoalExpr = generic_call(GCall, Args, _, _, _),
        ArgsStr = describe_args(VarTable, Args),
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
        )
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
        GoalExpr = conj(_, _),
        Desc = "conj"
    ;
        GoalExpr = disj(_),
        Desc = "disj"
    ;
        GoalExpr = switch(Var, _, _),
        Desc = "switch on " ++ describe_var(VarTable, Var)
    ;
        GoalExpr = negation(_),
        Desc = "negation"
    ;
        GoalExpr = scope(Reason, _),
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
        )
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

:- func describe_cast(cast_kind) = string.

describe_cast(CastType) = Desc :-
    (
        ( CastType = unsafe_type_cast
        ; CastType = unsafe_type_inst_cast
        ; CastType = equiv_type_cast
        ; CastType = exists_cast
        ),
        Desc = "cast"
    ;
        CastType = subtype_coerce,
        Desc = "coerce"
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
