%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2008 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: program_representation_utils.m.
% Author: pbone.
%
% Utilities for working with the program representation structures in the
% mdbcomp library.  This file is not part of the mdbcomp library, since it
% contains routines only used by the deep profiling tools.  Code here should be
% moved into the mdbcomp.program_representation.m module if it's to be used by
% other tools.
%
%-----------------------------------------------------------------------------%

:- module program_representation_utils.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.program_representation.

:- import_module cord.
:- import_module string.

%----------------------------------------------------------------------------%

    % Ugly-print a module to a string representation.  A cord of strings is
    % returned rather than a string, since this reduces the cost of string
    % concatenations.
    %
:- pred print_module_to_strings(module_rep::in, cord(string)::out) is det.

    % Ugly-print a procedure to a string representation.
    %
:- pred print_proc_to_strings(proc_rep::in, cord(string)::out) is det.

%----------------------------------------------------------------------------%

    % Search a program representation for the given procedure and return it's
    % procedure representation if found, otherwise fail.
    %
:- pred progrep_search_proc(prog_rep::in, string_proc_label::in, proc_rep::out)
    is semidet.

%----------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.prim_data.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module maybe.

%----------------------------------------------------------------------------%

print_module_to_strings(ModuleRep, Strings) :-
    ModuleRep = module_rep(ModuleName, _StringTable, ProcReps),
    list.map(print_proc_to_strings, ProcReps, ProcStrings),
    Strings = cord.cons(string.format("Module %s\n", [s(ModuleName)]), 
        cord_list_to_cord(ProcStrings)).

print_proc_to_strings(ProcRep, Strings) :-
    ProcRep = proc_rep(ProcLabel, ProcDefnRep),
    ProcDefnRep = proc_defn_rep(ArgVarReps, GoalRep, VarTable, Detism),
    print_proc_label_to_strings(Detism, ProcLabel, ProcLabelString),
    print_args_to_strings(VarTable, ArgVarReps, ArgsString),
    print_goal_to_strings(VarTable, 1, GoalRep, GoalString),
    Strings = ProcLabelString ++ ArgsString ++ cord.singleton(" :-\n") ++
        GoalString ++ nl.

:- pred print_proc_label_to_strings(detism_rep::in, string_proc_label::in, 
    cord(string)::out) is det.

print_proc_label_to_strings(Detism, ProcLabel, Strings) :-
    (
        ProcLabel = str_ordinary_proc_label(PredFunc, DeclModule, _DefModule,
            Name, Arity, Mode),
        (
            PredFunc = pf_predicate,
            PF = "pred"
        ;
            PredFunc = pf_function,
            PF = "func"
        ),
        string.format(" %s %s.%s/%d-%d",
            [s(PF), s(DeclModule), s(Name), i(Arity), i(Mode)], String)
    ;
        ProcLabel = str_special_proc_label(TypeName, TypeModule, _DefModule,
            Name, Arity, Mode),
        string.format(" %s for %s.%s/%d-%d",
            [s(Name), s(TypeModule), s(TypeName), i(Arity), i(Mode)], String)
    ),
    detism_to_string(Detism, DetismString),
    Strings = DetismString ++ cord.singleton(String).

%-----------------------------------------------------------------------------%

:- pred print_goal_to_strings(var_table::in, int::in, goal_rep::in,
    cord(string)::out) is det.

print_goal_to_strings(VarTable, Indent, GoalRep, Strings) :-
    GoalRep = goal_rep(GoalExprRep, DetismRep, _),
    (
        GoalExprRep = conj_rep(ConjGoalReps),
        print_conj_to_strings(VarTable, Indent, ConjGoalReps, Strings)
    ;
        GoalExprRep = disj_rep(DisjGoalReps),
        detism_to_string(DetismRep, DetismString),
        print_disj_to_strings(VarTable, Indent, DisjGoalReps, no, DisjString),
        Strings = indent(Indent) ++ DetismString ++ cord.singleton(" (\n") ++
            DisjString ++ indent(Indent) ++ cord.singleton(")\n") 
    ;
        GoalExprRep = switch_rep(SwitchVarRep, CasesRep),
        detism_to_string(DetismRep, DetismString),
        lookup_var_name(VarTable, SwitchVarRep, SwitchVarName),
        string.format(" ( switch on %s\n", [s(SwitchVarName)],
            SwitchOpenString),
        print_switch_to_strings(VarTable, Indent, CasesRep, no, SwitchString),
        Strings = indent(Indent) ++ DetismString ++
            cord.singleton(SwitchOpenString) ++ SwitchString ++ 
            indent(Indent) ++ cord.singleton(")\n")
    ;
        GoalExprRep = ite_rep(CondRep, ThenRep, ElseRep),
        detism_to_string(DetismRep, DetismString),
        print_goal_to_strings(VarTable, Indent + 1, CondRep, CondString),
        print_goal_to_strings(VarTable, Indent + 1, ThenRep, ThenString),
        print_goal_to_strings(VarTable, Indent + 1, ElseRep, ElseString),
        IndentString = indent(Indent),
        Strings = IndentString ++ DetismString ++
            cord.singleton(" (\n") ++ CondString ++ IndentString ++
            cord.singleton("->\n") ++ ThenString ++ IndentString ++
            cord.singleton(";\n") ++ ElseString ++ IndentString ++
            cord.singleton(")\n")
    ;
        GoalExprRep = negation_rep(SubGoalRep),
        print_goal_to_strings(VarTable, Indent + 1, SubGoalRep, SubGoalString),
        Strings = indent(Indent) ++ cord.singleton("not (\n") ++ SubGoalString
            ++ indent(Indent) ++ cord.singleton(")\n")
    ;
        GoalExprRep = scope_rep(SubGoalRep, MaybeCut),
        detism_to_string(DetismRep, DetismString),
        (
            MaybeCut = scope_is_cut,
            CutString = cord.empty 
        ;
            MaybeCut = scope_is_no_cut,
            CutString = cord.singleton(" cut")
        ),
        print_goal_to_strings(VarTable, Indent + 1, SubGoalRep, SubGoalString),
        Strings = indent(Indent) ++ DetismString ++ cord.singleton(" scope") ++
            CutString ++ cord.singleton(" (\n") ++ SubGoalString ++
            indent(Indent) ++ cord.singleton(")\n") 
    ;
        GoalExprRep = atomic_goal_rep(_FileName, _LineNumber,
            _BoundVars, AtomicGoalRep),
        print_atomic_goal_to_strings(VarTable, Indent, DetismRep,
            AtomicGoalRep, Strings)
    ).

:- pred print_conj_to_strings(var_table::in, int::in, list(goal_rep)::in,
    cord(string)::out) is det.

print_conj_to_strings(VarTable, Indent, GoalReps, Strings) :-
    (
        GoalReps = [],
        Strings = cord.snoc(indent(Indent), "true\n")
    ;
        GoalReps = [_ | _],
        print_conj_2_to_strings(VarTable, Indent, GoalReps, Strings)
    ).

:- pred print_conj_2_to_strings(var_table::in, int::in, list(goal_rep)::in, 
    cord(string)::out) is det.

print_conj_2_to_strings(_, _Indent, [], cord.empty).
print_conj_2_to_strings(VarTable, Indent, [GoalRep | GoalReps], Strings) :-
    % We use the absence of a separator to denote conjunction.
    %
    % We could try to append the comma at the end of each goal that is
    % not last in a conjunction, but that would be significant work,
    % and (at least for now) there is no real need for it.
    print_goal_to_strings(VarTable, Indent, GoalRep, GoalString),
    print_conj_2_to_strings(VarTable, Indent, GoalReps, ConjString),
    Strings = GoalString ++ ConjString.

:- pred print_disj_to_strings(var_table::in, int::in, list(goal_rep)::in,
    bool::in, cord(string)::out) is det.

print_disj_to_strings(_, _Indent, [], _PrintSemi, cord.empty).
print_disj_to_strings(VarTable, Indent, [GoalRep | GoalReps], PrintSemi, Strings) :-
    (
        PrintSemi = no,
        DelimString = cord.empty
    ;
        PrintSemi = yes,
        DelimString = indent(Indent) ++ cord.singleton(";\n")
    ),
    print_goal_to_strings(VarTable, Indent + 1, GoalRep, GoalString),
    print_disj_to_strings(VarTable, Indent, GoalReps, yes, DisjString),
    Strings = DelimString ++ GoalString ++ DisjString.

:- pred print_switch_to_strings(var_table::in, int::in, list(case_rep)::in,
    bool::in, cord(string)::out) is det.

print_switch_to_strings(_, _Indent, [], _PrintSemi, cord.empty).
print_switch_to_strings(VarTable, Indent, [CaseRep | CaseReps], PrintSemi, Strings) :-
    (
        PrintSemi = no,
        DelimString = cord.empty
    ;
        PrintSemi = yes,
        DelimString = indent(Indent) ++ cord.singleton(";\n")
    ),
    CaseRep = case_rep(MainConsIdArityRep, OtherConsIdArityRep, GoalRep),
    print_cons_id_and_arity_to_strings(Indent + 1, MainConsIdArityRep,
        ConsIdArityString),
    list.map(print_cons_id_and_arity_to_strings(Indent + 1),
        OtherConsIdArityRep, OtherConsIdArityStrings),
    print_goal_to_strings(VarTable, Indent + 1, GoalRep, GoalString),
    print_switch_to_strings(VarTable, Indent, CaseReps, yes, CaseStrings),
    Strings = DelimString ++ ConsIdArityString ++
        cord_list_to_cord(OtherConsIdArityStrings) ++ GoalString ++
        CaseStrings.

:- pred print_cons_id_and_arity_to_strings(int::in, cons_id_arity_rep::in,
    cord(string)::out) is det.

print_cons_id_and_arity_to_strings(Indent, ConsIdArityRep, Strings) :-
    ConsIdArityRep = cons_id_arity_rep(ConsIdRep, Arity),
    string.format("%% case %s/%d\n", [s(ConsIdRep), i(Arity)], String),
    Strings = cord.snoc(indent(Indent + 1), String).

%-----------------------------------------------------------------------------%

:- pred print_atomic_goal_to_strings(var_table::in, int::in, detism_rep::in, 
    atomic_goal_rep::in, cord(string)::out) is det.

print_atomic_goal_to_strings(VarTable, Indent, DetismRep, AtomicGoalRep,
        Strings) :-
    (
        (
            AtomicGoalRep = unify_construct_rep(VarRep, ConsIdRep, ArgReps),
            UnifyOp = "<="
        ;
            AtomicGoalRep = unify_deconstruct_rep(VarRep, ConsIdRep, ArgReps),
            UnifyOp = "=>"
        ),
        lookup_var_name(VarTable, VarRep, VarName),
        string.format(" %s %s %s", [s(VarName), s(UnifyOp), s(ConsIdRep)],
            UnifyString),
        print_args_to_strings(VarTable, ArgReps, ArgsString),
        Strings0 = cord.cons(UnifyString, ArgsString) 
    ;
        (
            AtomicGoalRep = partial_construct_rep(VarRep, ConsIdRep,
                MaybeArgReps),
            UnifyOp = "<="
        ;
            AtomicGoalRep = partial_deconstruct_rep(VarRep, ConsIdRep,
                MaybeArgReps),
            UnifyOp = "=>"
        ),
        lookup_var_name(VarTable, VarRep, VarName),
        string.format(" %s %s %s", [s(VarName), s(UnifyOp), s(ConsIdRep)],
            UnifyString),
        print_maybe_args_to_strings(VarTable, MaybeArgReps, ArgsString),
        Strings0 = cord.cons(UnifyString, ArgsString)
    ;
        AtomicGoalRep = unify_assign_rep(TargetRep, SourceRep),
        lookup_var_name(VarTable, TargetRep, TargetName),
        lookup_var_name(VarTable, SourceRep, SourceName),
        string.format(" %s := %s", [s(TargetName), s(SourceName)], String),
        Strings0 = cord.singleton(String)
    ;
        AtomicGoalRep = cast_rep(TargetRep, SourceRep),
        lookup_var_name(VarTable, TargetRep, TargetName),
        lookup_var_name(VarTable, SourceRep, SourceName),
        string.format(" cast %s to %s", [s(SourceName), s(TargetName)], String),
        Strings0 = cord.singleton(String)
    ;
        AtomicGoalRep = unify_simple_test_rep(TargetRep, SourceRep),
        lookup_var_name(VarTable, TargetRep, TargetName),
        lookup_var_name(VarTable, SourceRep, SourceName),
        string.format(" %s == %s", [s(SourceName), s(TargetName)], String),
        Strings0 = cord.singleton(String)
    ;
        AtomicGoalRep = pragma_foreign_code_rep(Args),
        print_args_to_strings(VarTable, Args, ArgsString),
        Strings0 = cord.singleton(" foreign_proc(") ++ ArgsString ++
            cord.singleton(")") 
    ;
        AtomicGoalRep = higher_order_call_rep(HOVarRep, Args),
        lookup_var_name(VarTable, HOVarRep, HOVarName),
        string.format(" %s(", [s(HOVarName)], HeadString),
        print_args_to_strings(VarTable, Args, ArgsString),
        Strings0 = cord.singleton(HeadString) ++ ArgsString ++
            cord.singleton(")")
    ;
        AtomicGoalRep = method_call_rep(TCIVarRep, MethodNumber, Args),
        lookup_var_name(VarTable, TCIVarRep, TCIVarName),
        string.format(" method %d of %s(", [i(MethodNumber), s(TCIVarName)],
            HeadString),
        print_args_to_strings(VarTable, Args, ArgsString),
        Strings0 = cord.singleton(HeadString) ++ ArgsString ++
            cord.singleton(")")
    ;
        AtomicGoalRep = plain_call_rep(Module, Pred, Args),
        string.format(" %s.%s", [s(Module), s(Pred)], HeadString),
        print_args_to_strings(VarTable, Args, ArgsString),
        Strings0 = cord.cons(HeadString, ArgsString)
    ;
        AtomicGoalRep = builtin_call_rep(Module, Pred, Args),
        string.format(" builtin %s.%s", [s(Module), s(Pred)], HeadString),
        print_args_to_strings(VarTable, Args, ArgsString),
        Strings0 = cord.cons(HeadString, ArgsString)
    ;
        AtomicGoalRep = event_call_rep(Event, Args),
        string.format(" event %s", [s(Event)], HeadString),
        print_args_to_strings(VarTable, Args, ArgsString),
        Strings0 = cord.cons(HeadString, ArgsString)
    ),
    detism_to_string(DetismRep, DetismString),
    Strings = indent(Indent) ++ DetismString ++ Strings0 ++ nl.

%-----------------------------------------------------------------------------%

:- pred print_args_to_strings(var_table::in, list(var_rep)::in,
    cord(string)::out) is det.

print_args_to_strings(VarTable, Args, Strings) :-
    (
        Args = [],
        Strings = cord.empty
    ;
        Args = [_ | _],
        print_args_2_to_strings(VarTable, Args, cord.empty, ArgsStr),
        Strings = cord.cons("(", cord.snoc(ArgsStr, ")"))
    ).

:- pred print_args_2_to_strings(var_table::in, list(var_rep)::in, 
    cord(string)::in, cord(string)::out) is det.

print_args_2_to_strings(_,        [],                 _,      cord.empty).
print_args_2_to_strings(VarTable, [VarRep | VarReps], Prefix, Strings) :-
    lookup_var_name(VarTable, VarRep, VarName),
    print_args_2_to_strings(VarTable, VarReps, cord.singleton(", "), ArgsString),
    Strings = Prefix ++ cord.cons(VarName, ArgsString).

:- pred print_maybe_args_to_strings(var_table::in, list(maybe(var_rep))::in, 
    cord(string)::out) is det.

print_maybe_args_to_strings(VarTable, MaybeArgs, Strings) :-
    (
        MaybeArgs = [],
        Strings = cord.empty
    ;
        MaybeArgs = [_ | _],
        print_maybe_args_2_to_strings(VarTable, MaybeArgs, cord.empty, ArgsStr),
        Strings = cord.cons("(", cord.snoc(ArgsStr, ")"))
    ).

:- pred print_maybe_args_2_to_strings(var_table::in, list(maybe(var_rep))::in,
    cord(string)::in, cord(string)::out) is det.

print_maybe_args_2_to_strings(_, [], _, cord.empty).
print_maybe_args_2_to_strings(VarTable, [MaybeVarRep | MaybeVarReps], Prefix, Strings) :-
    (
        MaybeVarRep = no,
        VarName = "_"
    ;
        MaybeVarRep = yes(VarRep),
        lookup_var_name(VarTable, VarRep, VarName)
    ),
    print_maybe_args_2_to_strings(VarTable, MaybeVarReps, cord.singleton(", "),
        ArgsString),
    Strings = Prefix ++ cord.cons(VarName, ArgsString).

:- func indent(int) = cord(string).

indent(N) =
    ( N =< 0 ->
        cord.empty
    ;
        cord.singleton("  ") ++ indent(N - 1)
    ).

:- pred detism_to_string(detism_rep::in, cord(string)::out) is det.

detism_to_string(Detism, DetismStrCord) :-
    (
        Detism = det_rep,
        DetismStr = "det"
    ;
        Detism = semidet_rep,
        DetismStr = "semidet"
    ;
        Detism = nondet_rep,
        DetismStr = "nondet"
    ;
        Detism = multidet_rep,
        DetismStr = "multi"
    ;
        Detism = cc_nondet_rep,
        DetismStr = "cc_nondet"
    ;
        Detism = cc_multidet_rep,
        DetismStr = "cc_multi"
    ;
        Detism = erroneous_rep,
        DetismStr = "erroneous"
    ;
        Detism = failure_rep,
        DetismStr = "failure"
    ),
    DetismStrCord = cord.singleton(DetismStr).

%----------------------------------------------------------------------------%

:- func nl = cord(string).

nl = cord.singleton("\n").

%----------------------------------------------------------------------------%

% TODO: It'd be nice if this predicate had some sort of indexes or binary tree
% to use to speed up it's search.
progrep_search_proc(ProgRep, ProcLabel, ProcRep) :-
    % XXX: what's the difference between these two module fields? which should
    % I be using.
    ( ProcLabel = str_ordinary_proc_label(_, Module, _Module2, _, _, _)
    ; ProcLabel = str_special_proc_label(_, Module, _Module2, _, _, _)
    ),
    progrep_search_module(ProgRep, Module, ModuleRep),
    modulerep_search_proc(ModuleRep, ProcLabel, ProcRep).

    % Search for a module within a program representation.
    %
:- pred progrep_search_module(prog_rep::in, string::in, module_rep::out) 
    is semidet.

progrep_search_module(ProgRep, ModuleName, ModuleRep) :-
   ProgRep = prog_rep(ModuleReps),
   modulerep_list_search_module(ModuleReps, ModuleName, ModuleRep).

    % Search for a module within a list of module representations.
    %
:- pred modulerep_list_search_module(list(module_rep)::in, string::in,
    module_rep::out) is semidet.

modulerep_list_search_module([], _, _) :- fail.
modulerep_list_search_module([ModuleRep0 | ModuleReps], ModuleName, ModuleRep)
        :-
    ( ModuleRep0 ^ mr_name = ModuleName ->
        ModuleRep = ModuleRep0
    ;
        modulerep_list_search_module(ModuleReps, ModuleName, ModuleRep)
    ).

    % Search for a procedure within a module representation.
    %
:- pred modulerep_search_proc(module_rep::in, string_proc_label::in,
    proc_rep::out) is semidet.

modulerep_search_proc(ModuleRep, ProcLabel, ProcRep) :-
    procrep_list_search_proc(ModuleRep ^ mr_procs, ProcLabel, ProcRep).

    % Search for a procedure within a list of procedure representations.
    %
:- pred procrep_list_search_proc(list(proc_rep)::in, string_proc_label::in,
    proc_rep::out) is semidet.

procrep_list_search_proc([], _, _) :- fail.
procrep_list_search_proc([ProcRep0 | ProcReps], ProcLabel, ProcRep) :-
    ( ProcRep0 ^ pr_id = ProcLabel ->
        ProcRep = ProcRep0
    ;
        procrep_list_search_proc(ProcReps, ProcLabel, ProcRep)
    ).

%----------------------------------------------------------------------------%

