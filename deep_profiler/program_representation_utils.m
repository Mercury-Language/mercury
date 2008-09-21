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
:- import_module measurements.
:- import_module profile.
:- import_module report.

:- import_module cord.
:- import_module map.
:- import_module string.
:- import_module unit.

%----------------------------------------------------------------------------%

    % Ugly-print a module to a string representation.  A cord of strings is
    % returned rather than a string, since this reduces the cost of string
    % concatenations.
    %
:- pred print_module_to_strings(module_rep::in, cord(string)::out) is det.

    % Print a procedure to a string representation.
    %
:- pred print_proc_to_strings(proc_rep(GoalAnn), cord(string)) <=
    (goal_annotation(GoalAnn)).
:- mode print_proc_to_strings(in, out) is det.

%----------------------------------------------------------------------------%

:- typeclass goal_annotation(T) where [
            % Print the goal annotation for inclusion by print_proc_to_strings
            % above.
            %
        pred print_goal_annotation_to_strings(T::in, cord(string)::out) is det
    ].

    % A goal with no particular annotation has empty strings printed for goal
    % annotations.
    %
:- instance goal_annotation(unit).

%----------------------------------------------------------------------------%

    % Search a program representation for the given procedure and return it's
    % procedure representation if found, otherwise fail.
    %
:- pred progrep_search_proc(prog_rep::in, string_proc_label::in, proc_rep::out)
    is semidet.

%----------------------------------------------------------------------------%

    % Annotate the program representation structure with coverage information.
    %
:- pred procrep_annotate_with_coverage(own_prof_info::in,
    map(goal_path, call_site_perf)::in, map(goal_path, coverage_point)::in,
    map(goal_path, coverage_point)::in, proc_rep::in, 
    proc_rep(coverage_info)::out) is det.

%----------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.prim_data.

:- import_module array.
:- import_module bool.
:- import_module int.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module require.

%----------------------------------------------------------------------------%

print_module_to_strings(ModuleRep, Strings) :-
    ModuleRep = module_rep(ModuleName, _StringTable, ProcReps),
    map.foldl((pred(_::in, Proc::in, Str0::in, Str::out) is det :-
            print_proc_to_strings(Proc, Str1),
            Str = Str0 ++ Str1
        ), ProcReps, cord.empty, ProcStrings),
    Strings = cord.cons(string.format("Module %s\n", [s(ModuleName)]), 
        ProcStrings).

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

:- pred print_goal_to_strings(var_table, int, goal_rep(GoalAnn), cord(string))
    <= goal_annotation(GoalAnn).
:- mode print_goal_to_strings(in, in, in, out) is det.

print_goal_to_strings(VarTable, Indent, GoalRep, Strings) :-
    GoalRep = goal_rep(GoalExprRep, DetismRep, GoalAnnotation),
    detism_to_string(DetismRep, DetismString),
    print_goal_annotation_to_strings(GoalAnnotation, GoalAnnotationString),
    (
        GoalExprRep = conj_rep(ConjGoalReps),
        print_conj_to_strings(VarTable, Indent, ConjGoalReps,
            Strings)
    ;
        GoalExprRep = disj_rep(DisjGoalReps),
        print_disj_to_strings(VarTable, Indent, DisjGoalReps, no, DisjString),
        Strings = indent(Indent) ++ DetismString ++ GoalAnnotationString ++
            cord.singleton(" (\n") ++ DisjString ++ indent(Indent) ++
            cord.singleton(")\n") 
    ;
        GoalExprRep = switch_rep(SwitchVarRep, CanFail, CasesRep),
        lookup_var_name(VarTable, SwitchVarRep, SwitchVarName),
        string.format(" ( %s switch on %s\n", 
            [s(string(CanFail)), s(SwitchVarName)], SwitchOpenString),
        print_switch_to_strings(VarTable, Indent, CasesRep, no, SwitchString),
        Strings = indent(Indent) ++ DetismString ++ GoalAnnotationString ++
            cord.singleton(SwitchOpenString) ++ SwitchString ++ 
            indent(Indent) ++ cord.singleton(")\n")
    ;
        GoalExprRep = ite_rep(CondRep, ThenRep, ElseRep),
        print_goal_to_strings(VarTable, Indent + 1, CondRep, CondString),
        print_goal_to_strings(VarTable, Indent + 1, ThenRep, ThenString),
        print_goal_to_strings(VarTable, Indent + 1, ElseRep, ElseString),
        IndentString = indent(Indent),
        Strings = IndentString ++ DetismString ++ GoalAnnotationString ++
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
        (
            MaybeCut = scope_is_cut,
            CutString = cord.empty 
        ;
            MaybeCut = scope_is_no_cut,
            CutString = cord.singleton(" cut")
        ),
        print_goal_to_strings(VarTable, Indent + 1, SubGoalRep, SubGoalString),
        Strings = indent(Indent) ++ DetismString ++ GoalAnnotationString ++ 
            cord.singleton(" scope") ++ CutString ++ cord.singleton(" (\n") ++
            SubGoalString ++ indent(Indent) ++ cord.singleton(")\n") 
    ;
        GoalExprRep = atomic_goal_rep(_FileName, _LineNumber,
            _BoundVars, AtomicGoalRep),
        print_atomic_goal_to_strings(GoalAnnotationString, VarTable, Indent,
            DetismRep, AtomicGoalRep, Strings)
    ).

:- pred print_conj_to_strings(var_table, int, list(goal_rep(GoalAnn)),
    cord(string)) <= goal_annotation(GoalAnn).
:- mode print_conj_to_strings(in, in, in, out) is det.

print_conj_to_strings(VarTable, Indent, GoalReps, Strings) :-
    (
        GoalReps = [],
        Strings = cord.snoc(indent(Indent), "true\n")
    ;
        GoalReps = [_ | _],
        print_conj_2_to_strings(VarTable, Indent, GoalReps, Strings)
    ).

:- pred print_conj_2_to_strings(var_table, int, list(goal_rep(GoalAnn)), 
    cord(string)) <= goal_annotation(GoalAnn).
:- mode print_conj_2_to_strings(in, in, in, out) is det.

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

:- pred print_disj_to_strings(var_table, int, list(goal_rep(GoalAnn)), bool,
    cord(string)) <= goal_annotation(GoalAnn).
:- mode print_disj_to_strings(in, in, in, in, out) is det.

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

:- pred print_switch_to_strings(var_table, int, list(case_rep(GoalAnn)), bool,
    cord(string)) <= goal_annotation(GoalAnn).
:- mode print_switch_to_strings(in, in, in, in, out) is det.

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

:- pred print_atomic_goal_to_strings(cord(string)::in, var_table::in, int::in,
    detism_rep::in, atomic_goal_rep::in, cord(string)::out) is det.

print_atomic_goal_to_strings(GoalAnnotationString, VarTable, Indent, DetismRep,
        AtomicGoalRep, Strings) :-
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
    Strings = indent(Indent) ++ DetismString ++ GoalAnnotationString ++
        Strings0 ++ nl.

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

:- instance goal_annotation(unit) where [
        pred(print_goal_annotation_to_strings/2) is print_unit_to_strings
    ].

:- pred print_unit_to_strings(unit::in, cord(string)::out) is det.

print_unit_to_strings(_, cord.empty).

%----------------------------------------------------------------------------%

progrep_search_proc(ProgRep, ProcLabel, ProcRep) :-
    ( ProcLabel = str_ordinary_proc_label(_, Module, _DefModule, _, _, _)
    ; ProcLabel = str_special_proc_label(_, Module, _DefModule, _, _, _)
    ),
    progrep_search_module(ProgRep, Module, ModuleRep),
    modulerep_search_proc(ModuleRep, ProcLabel, ProcRep).

    % Search for a module within a program representation.
    %
:- pred progrep_search_module(prog_rep::in, string::in, module_rep::out) 
    is semidet.

progrep_search_module(ProgRep, ModuleName, ModuleRep) :-
   ProgRep = prog_rep(ModuleReps),
   map.search(ModuleReps, ModuleName, ModuleRep).

    % Search for a procedure within a module representation.
    %
:- pred modulerep_search_proc(module_rep::in, string_proc_label::in,
    proc_rep::out) is semidet.

modulerep_search_proc(ModuleRep, ProcLabel, ProcRep) :-
    map.search(ModuleRep ^ mr_procs, ProcLabel, ProcRep).

%----------------------------------------------------------------------------%

procrep_annotate_with_coverage(OwnProf, CallSites, SolnsCoveragePoints,
        BranchCoveragePoints, !ProcRep) :-
    some [!ProcDefn, !GoalRep] (
        !:ProcDefn = !.ProcRep ^ pr_defn,
        !:GoalRep = !.ProcDefn ^ pdr_goal,
        Calls = calls(OwnProf),
        Exits = exits(OwnProf),
        ( Calls = Exits ->
            Coverage = coverage_known_det(Calls)
        ;
            Coverage = coverage_known(Calls, Exits)
        ),
        CoverageReference =
            coverage_reference_info(CallSites, SolnsCoveragePoints, 
                BranchCoveragePoints),
        goal_annotate_coverage(CoverageReference, empty_goal_path, Coverage, _,
            !GoalRep),
        !:ProcDefn = !.ProcDefn ^ pdr_goal := !.GoalRep,
        !:ProcRep = !.ProcRep ^ pr_defn := !.ProcDefn
    ).

    % These maps are keyed by goal_path, which is a structure with arbitrary,
    % comparing these structures is less efficient than comparing simple
    % structures like the alternative goal_path_string, however, that involves
    % frequently constructing strings from goal paths.  Using goal_path_string
    % may be faster but I'd rather not make this optimisation without first
    % testing it.
    %
:- type coverage_reference_info
    --->    coverage_reference_info(
                cri_call_sites              :: map(goal_path, call_site_perf),
                cri_solns_coverage_points   :: map(goal_path, coverage_point),
                cri_branch_coverage_points  :: map(goal_path, coverage_point)
            ).

    % Annotate a goal and it's children with coverage information.
    %
:- pred goal_annotate_coverage(coverage_reference_info::in, goal_path::in,
    coverage_info::in, coverage_info::out,
    goal_rep(unit)::in, goal_rep(coverage_info)::out) is det.

goal_annotate_coverage(Info, GoalPath, !Coverage, Goal0, Goal) :-
    Goal0 = goal_rep(GoalExpr0, Detism, _),

    % Gather any coverage information about this goal and apply it.
    (
        get_coverage_after(!.Coverage) = coverage_unknown,
        map.search(Info ^ cri_solns_coverage_points, GoalPath, CoveragePoint)
    ->
        CoveragePoint = coverage_point(Coverage, _, _),
        !:Coverage = merge_coverage(get_coverage_before(!.Coverage),
            coverage_known_after(Coverage))
    ;
        true
    ),
    % TODO: Infer that if a goal has a coverage of exactly 0 before it, then it
    % must have a coverage of exactly 0 after it.  And that a goal that cannot
    % fail that has a coverage of 0 after it, must have a coverage of 0 before
    % it.
    maybe_propagate_det_coverage(Detism, GoalPath, !Coverage),

    % Calculate coverage of any inner goals.
    (
        GoalExpr0 = conj_rep(Conjuncts0),
        conj_annotate_coverage(Info, GoalPath, 1, !Coverage,
            Conjuncts0, Conjuncts),
        GoalExpr = conj_rep(Conjuncts)
    ;
        GoalExpr0 = disj_rep(Disjuncts0),
        disj_annotate_coverage(Info, Detism, GoalPath, !Coverage,
            Disjuncts0, Disjuncts),
        GoalExpr = disj_rep(Disjuncts)
    ;
        GoalExpr0 = switch_rep(Var, CanFail, Cases0),
        switch_annotate_coverage(Info, Detism, GoalPath, !Coverage,
            Cases0, Cases),
        GoalExpr = switch_rep(Var, CanFail, Cases)
    ;
        GoalExpr0 = ite_rep(Cond0, Then0, Else0),
        ite_annotate_coverage(Info, GoalPath, !Coverage, Cond0, Cond,
            Then0, Then, Else0, Else),
        GoalExpr = ite_rep(Cond, Then, Else)
    ;
        GoalExpr0 = negation_rep(NegGoal0),
        negation_annotate_coverage(Info, GoalPath, !Coverage, 
            NegGoal0, NegGoal), 
        GoalExpr = negation_rep(NegGoal)
    ;
        GoalExpr0 = scope_rep(ScopedGoal0, MaybeCut),
        scope_annotate_coverage(Info, GoalPath, MaybeCut, !Coverage, 
            ScopedGoal0, ScopedGoal),
        GoalExpr = scope_rep(ScopedGoal, MaybeCut)
    ;
        GoalExpr0 = atomic_goal_rep(Filename, Line, Vars, AtomicGoal),
        ( 
            ( AtomicGoal = unify_construct_rep(_, _, _)
            ; AtomicGoal = unify_deconstruct_rep(_, _, _)
            ; AtomicGoal = partial_deconstruct_rep(_, _, _)
            ; AtomicGoal = partial_construct_rep(_, _, _)
            ; AtomicGoal = unify_assign_rep(_, _)
            ; AtomicGoal = cast_rep(_, _)
            ; AtomicGoal = unify_simple_test_rep(_, _)
            ; AtomicGoal = event_call_rep(_, _)
            )
        ;
            ( AtomicGoal = higher_order_call_rep(_, _)
            ; AtomicGoal = method_call_rep(_, _, _)
            ; AtomicGoal = plain_call_rep(_, _, _)
            ; AtomicGoal = builtin_call_rep(_, _, _)
            ; AtomicGoal = pragma_foreign_code_rep(_)
            ),
            ( map.search(Info ^ cri_call_sites, GoalPath, CallSite) ->
                Summary = CallSite ^ csf_summary_perf,
                % Entry due to redo is not counted at the point before the
                % goal, it's represented when the number of exists is greater
                % than the number of calls,  This won't work with nondet code
                % which should be fixed in the future.
                Calls = Summary ^ perf_row_calls,
                Exits = Summary ^ perf_row_exits, 
                !:Coverage = coverage_known(Calls, Exits)
            ;
                (
                    % These goal call types must have call sites, whereas some
                    % builtins and foreign code pragmas may not.
                    ( AtomicGoal = higher_order_call_rep(_, _)
                    ; AtomicGoal = method_call_rep(_, _, _)
                    ; AtomicGoal = plain_call_rep(_, _, _)
                    )
                ->
                    error("Couldn't look up call site for port counts GP: " ++
                        goal_path_to_string(GoalPath))
                ;
                    true
                )
            )
        ),
        GoalExpr = atomic_goal_rep(Filename, Line, Vars, AtomicGoal)
    ),
    maybe_propagate_det_coverage(Detism, GoalPath, !Coverage),
    Goal = goal_rep(GoalExpr, Detism, !.Coverage),
    trace [ compile_time(flag("debug_coverage_propagation")), io(!IO) ] (
        io.write_string("goal_annotate_coverage: done\n", !IO),
        io.format("\tGoalPath: %s\n\tDetism %s\n\tCoverage; %s\n", 
            [s(goal_path_to_string(GoalPath)), 
             s(string(Detism)), 
             s(string(!.Coverage))], !IO)
    ),
    require(check_coverage_regarding_detism(!.Coverage, Detism), 
        string.format("check_coverage_regarding_detism failed: %s %s", 
            [s(string(!.Coverage)), s(string(Detism))])).

    % Annotate a conjunction with coverage information.  This folds from the
    % right over the list of conjuncts (backwards).
    %
    % The list of goals is the tail of a conjunction, the coverage argument
    % describes the coverage of this list of goals if it where the entire
    % conjunction.  However each goal has it's own coverage.
    %
:- pred conj_annotate_coverage(coverage_reference_info::in, goal_path::in,
    int::in, coverage_info::in, coverage_info::out,
    list(goal_rep(unit))::in, list(goal_rep(coverage_info))::out) is det.

conj_annotate_coverage(_, GoalPath, ConjunctNum, !Coverage, [], []) :-
    % The empty conjunction is equivalent to 'true' which is deterministic,
    ConjGoalPath = goal_path_add_at_end(GoalPath, step_conj(ConjunctNum)),
    propagate_det_coverage(ConjGoalPath, !Coverage).

conj_annotate_coverage(Info, GoalPath, ConjunctNum, !Coverage, 
        [Conj0 | Conjs0], [Conj | Conjs]) :-
    split_coverage(!.Coverage, CoverageBefore0, CoverageAfter0),
    conj_annotate_coverage(Info, GoalPath, ConjunctNum+1,
        CoverageAfter0, TailCoverage1, Conjs0, Conjs1),
    split_coverage(TailCoverage1, CoverageBeforeTail1, CoverageAfter1),

    goal_transition_coverage(CoverageAfterHead0, CoverageBeforeTail1),
    HeadCoverage0 = merge_coverage(CoverageBefore0, CoverageAfterHead0),
    goal_annotate_coverage(Info,
        goal_path_add_at_end(GoalPath, step_conj(ConjunctNum)),
        HeadCoverage0, HeadCoverage, Conj0, Conj),
    
    % If computing the coverage for the head gave us information that can be
    % used to re-compute the coverage for the tail, and we don't already know
    % the coverage at the beginning of the tail.  Then re-compute the coverage
    % for the tail.
    split_coverage(HeadCoverage, CoverageBefore, CoverageAfterHead),
    (
        CoverageBeforeTail1 = coverage_unknown,
        CoverageAfterHead = coverage_known_after(Count)
    -> 
        CoverageBeforeTail = coverage_known_before(Count),
        TailCoverage2 = merge_coverage(CoverageBeforeTail, CoverageAfter1),
        conj_annotate_coverage(Info, GoalPath, ConjunctNum+1,
            TailCoverage2, TailCoverage, Conjs0, Conjs),
        CoverageAfter = get_coverage_after(TailCoverage)
    ;
        Conjs = Conjs1,
        CoverageAfter = CoverageAfter1
    ),
    !:Coverage = merge_coverage(CoverageBefore, CoverageAfter).

    % Compute the coverage information for a disjunction.
    %
    % Rules:
    %   - The coverage before a disjunction is equal to the coverage before the
    %     first disjunct.
    %   - The coverage after a disjunction is equal to the sum of coverages
    %     after each disjunct.
    %   - If the disjunction has at most one solution, then the coverage
    %     entering a disjunct is the failure count of the previous disjunct.
    %
    % Examples:
    %   A semidet disjunction.
    %     5 ( 5 D1 2; 3 D2 2; 1 D3 0 ) 4
    %
    %   A nondet disjunction.
    %     5 ( 5 D1 2; 5 D2 3; 5 D3 1 ) 6 (2 exit, 4 redo)
    %
    % For simplicity start with a backwards-only traversal, Not all the rules
    % described in this comment are applied.
    %
:- pred disj_annotate_coverage(coverage_reference_info::in, detism_rep::in,
    goal_path::in, coverage_info::in, coverage_info::out,
    list(goal_rep(unit))::in, list(goal_rep(coverage_info))::out) is det.

disj_annotate_coverage(Info, _Detism, GoalPath, !Coverage, 
        Disjs0, Disjs) :-
    CoverageBefore0 = get_coverage_before(!.Coverage),
    disj_annotate_coverage_2(Info, GoalPath, 1,
        Disjs0, Disjs, CoverageBefore),

    % If coverage before the disjunction was unknown before and is now
    % discovered, update it.
    (
        CoverageBefore0 = coverage_unknown,
        CoverageBefore = coverage_known_before(_)
    ->
        CoverageAfter = get_coverage_after(!.Coverage),
        !:Coverage = merge_coverage(CoverageBefore, CoverageAfter)
    ;
        true
    ).

:- pred disj_annotate_coverage_2(coverage_reference_info::in,
    goal_path::in, int::in,
    list(goal_rep)::in, list(goal_rep(coverage_info))::out,
    coverage_info::out(coverage_before)) is det.

disj_annotate_coverage_2(_, _, _, [], [], coverage_known_before(0)).

disj_annotate_coverage_2(Info, GoalPath, DisjNum, 
        [Disj0 | Disjs0], [Disj | Disjs], CoverageBefore) :-
    disj_annotate_coverage_2(Info, GoalPath, DisjNum + 1,
        Disjs0, Disjs, _),

    ThisGoalPath = goal_path_add_at_end(GoalPath, step_disj(DisjNum)),
    get_branch_coverage(Info, ThisGoalPath, CoverageBeforeDisj),
    % This can be set from the coverage entering the next disjunct, however the
    % transformation in the compiler doesn't do this, so for simplicity, this
    % is pessimistic.  Otherwise set is using CoverageBeforeTail.
    CoverageAfterDisj = coverage_unknown,
    CoverageDisj0 = merge_coverage(CoverageBeforeDisj, CoverageAfterDisj),

    goal_annotate_coverage(Info, ThisGoalPath, CoverageDisj0, CoverageDisj,
        Disj0, Disj),
    CoverageBefore = get_coverage_before(CoverageDisj).

:- pred switch_annotate_coverage(coverage_reference_info::in, detism_rep::in,
    goal_path::in, coverage_info::in, coverage_info::out, 
    list(case_rep(unit))::in, list(case_rep(coverage_info))::out) is det.

switch_annotate_coverage(Info, Detism, GoalPath, !Coverage, Cases0, Cases) :-
    switch_annotate_coverage_2(Info, Detism, GoalPath, 1,
        coverage_known_det(0), SwitchCoverage, !.Coverage, Cases0, Cases),
    % Use the newly computed coverage if it's more informed than the current
    % coverage.
    (
        !.Coverage = coverage_known_det(_)
    ;
        !.Coverage = coverage_known(_, _)
    ;
        !.Coverage = coverage_known_before(Before),
        (
            coverage_count_after(SwitchCoverage, After)
        ->
            !:Coverage = coverage_known(Before, After)
        ;
            true
        )
    ;
        !.Coverage = coverage_known_after(After),
        (
            coverage_count_before(SwitchCoverage, Before)
        ->
            !:Coverage = coverage_known(Before, After)
        ;
            true
        )
    ;
        !.Coverage = coverage_unknown,
        !:Coverage = SwitchCoverage
    ),

    require(check_switch_coverage(Detism, Cases, !.Coverage),
        string.format("check_switch_coverage failed\n\t" ++ 
            "Detism: %s\n\tCases: %s\n\tCoverage: %s\n",
        [s(string(Detism)), s(string(Cases)), s(string(!.Coverage))])).

    % switch_annotate_coverage_2(Info, Detism, GoalPath, CaseNum, 
    %   !CoverageSum, SwitchCoverage, !Cases),
    %
    % Perform coverage annotation on cases from the left to the right, The head
    % of the !.Cases list is case number CaseNum, SwitchCoverage is the
    % coverage for the entire switch as known by the caller, !CoverageSum is
    % the sum of the coverage so far.
    %
    % For this goal we use a forwards traversal, since the last goal may not
    % have a coverage point after it, in the expectation that the coverage at
    % the end of the last goal may need to be computed from the coverage of
    % each of the other goals.
    %
:- pred switch_annotate_coverage_2(coverage_reference_info::in, detism_rep::in, 
    goal_path::in, int::in, coverage_info::in, coverage_info::out,
    coverage_info::in, 
    list(case_rep(unit))::in, list(case_rep(coverage_info))::out) is det.

switch_annotate_coverage_2(_, _, _, _, !CoverageSum, _, [], []).

switch_annotate_coverage_2(Info, Detism, GoalPath, CaseNum, 
        !CoverageSum, SwitchCoverage, [ Case0 | Cases0 ], [ Case | Cases ]) :-
    CaseGoalPath = goal_path_add_at_end(GoalPath, 
        step_switch(CaseNum, no)),
    
    % If this is the last case in the switch, then it's coverage information
    % may be computed from the coverage of other cases and the coverage of the
    % whole switch.  This is only done for the last goal, since only this
    % optimisation is made by the coverage profiling code in the compiler.
    %
    % If we can't calculate it's coverage information then try to retrieve the
    % information from a coverage point associated with the switch branch.
    %
    (
        Cases0 = [],
        detism_get_can_fail(Detism) = cannot_fail
    ->
        (
            coverage_count_before(SwitchCoverage, SwitchCountBefore),
            coverage_count_before(!.CoverageSum, SumCountBefore)
        ->
            CoverageBefore0 = 
                coverage_known_before(SwitchCountBefore - SumCountBefore)
        ;
            % Search for a coverage point for this case.
            get_branch_coverage(Info, CaseGoalPath, CoverageBefore0)
        ),
        (
            coverage_count_after(SwitchCoverage, SwitchCountAfter),
            coverage_count_after(!.CoverageSum, SumCountAfter)
        ->
            CoverageAfter0 = 
                coverage_known_after(SwitchCountAfter - SumCountAfter)
        ;
            CoverageAfter0 = coverage_unknown
        ),
        Coverage0 = merge_coverage(CoverageBefore0, CoverageAfter0)
    ;
        % Search for a coverage point for this case.
        get_branch_coverage(Info, CaseGoalPath, Coverage0)
    ),

    % Look for a coverage point for this switch case.
    Case0 = case_rep(ConsID, OtherConsIDs, Goal0),
    goal_annotate_coverage(Info, CaseGoalPath, Coverage0, Coverage,
        Goal0, Goal),
    Case = case_rep(ConsID, OtherConsIDs, Goal),
  
    % Keep a sum of the coverage seen in cases so far.
    (
        coverage_count_before(!.CoverageSum, SumCountBefore1),
        coverage_count_before(Coverage, CountBefore)
    ->
        CoverageSumBefore = coverage_known_before(SumCountBefore1 + CountBefore)
    ;
        CoverageSumBefore = coverage_unknown
    ),
    (
        coverage_count_after(!.CoverageSum, SumCountAfter1),
        coverage_count_after(Coverage, CountAfter)
    ->
        CoverageSumAfter = coverage_known_after(SumCountAfter1 + CountAfter)
    ;
        CoverageSumAfter = coverage_unknown
    ),
    !:CoverageSum = merge_coverage(CoverageSumBefore, CoverageSumAfter),

    switch_annotate_coverage_2(Info, Detism, GoalPath, CaseNum + 1,
        !CoverageSum, SwitchCoverage, Cases0, Cases).

    % Propagate coverage information for if-then-else goals.
    %
    % Step 1:
    %   Compute the coverage of the Then and Else goals,
    %
    % Step 2:
    %   Infer and compute coverage information for the cond goal.
    %   
    % Step 3:
    %   Infer coverage information for goals using the determinisms of the then
    %   and else branches and the switch as a whole, and any coverage
    %   information as computed above.
    %
    % Step 4:
    %   Re-compute coverages for any sub goals within the Then and Else goals
    %   whose coverage is more known than after step 1.
    %
:- pred ite_annotate_coverage(coverage_reference_info::in, goal_path::in,
    coverage_info::in, coverage_info::out, 
    goal_rep::in, goal_rep(coverage_info)::out, 
    goal_rep::in, goal_rep(coverage_info)::out,
    goal_rep::in, goal_rep(coverage_info)::out) is det.

ite_annotate_coverage(Info, GoalPath, !Coverage,
        Cond0, Cond, Then0, Then, Else0, Else) :-
    CondGoalPath = goal_path_add_at_end(GoalPath, step_ite_cond),
    ThenGoalPath = goal_path_add_at_end(GoalPath, step_ite_then),
    ElseGoalPath = goal_path_add_at_end(GoalPath, step_ite_else),
    CondDetism = Cond0 ^ goal_detism_rep,

    % Step 1, compute coverage of each goal without inference.
    get_branch_coverage(Info, ThenGoalPath, ThenCoverage0),
    goal_annotate_coverage(Info, ThenGoalPath, ThenCoverage0, ThenCoverage1,
        Then0, Then1),
    get_branch_coverage(Info, ElseGoalPath, ElseCoverage0),
    goal_annotate_coverage(Info, ElseGoalPath, ElseCoverage0, ElseCoverage1,
        Else0, Else1),
    
    % Step 2: Infer coverage for the Cond goal..
    ( 
        get_coverage_before(ThenCoverage1) =
            coverage_known_before(ThenEntryCount)
    ->
        CondCoverageAfter0 = coverage_known_after(ThenEntryCount)
    ;
        CondCoverageAfter0 = coverage_unknown
    ),
    CondCoverage0 = merge_coverage(get_coverage_before(!.Coverage),
        CondCoverageAfter0),
    goal_annotate_coverage(Info, CondGoalPath, CondCoverage0, CondCoverage, 
        Cond0, Cond),
    split_coverage(CondCoverage, CondCoverageBefore, CondCoverageAfter),

    % Step 3: Infer coverages for the Then and Else goals if unknown.
    CoverageAfter0 = get_coverage_after(!.Coverage),
    split_coverage(ThenCoverage1, ThenCoverageBefore1, ThenCoverageAfter1),
    (
        ThenCoverageBefore1 = coverage_unknown,
        CondCoverageAfter = coverage_known_after(CondCountAfterPrime)
    ->
        ThenCoverageBefore2 = coverage_known_before(CondCountAfterPrime),
        trace [ compile_time(flag("debug_coverage_propagation")), io(!IO) ] (
            io.format("ITE Set coverage before Then: %d\n", 
                [i(CondCountAfterPrime)], !IO)
        )
    ;
        ThenCoverageBefore2 = ThenCoverageBefore1
    ),
    (
        ThenCoverageAfter1 = coverage_unknown,
        CoverageAfter0 = coverage_known_after(CountAfterPrime),
        ElseCoverageAfter1 = coverage_known_after(ElseCountAfterPrime)
    ->
        ThenCoverageAfter2 = 
            coverage_known_after(CountAfterPrime - ElseCountAfterPrime),
        trace [ compile_time(flag("debug_coverage_propagation")), io(!IO) ] (
            io.format("ITE Set coverage after Then: %d - %d\n", 
                [i(CountAfterPrime), i(ElseCountAfterPrime)], !IO)
        )
    ;
        ThenCoverageAfter2 = ThenCoverageAfter1
    ),
    ThenCoverage2 = merge_coverage(ThenCoverageBefore2, ThenCoverageAfter2),
    split_coverage(ElseCoverage1, ElseCoverageBefore1, ElseCoverageAfter1),
    (
        ElseCoverageBefore1 = coverage_unknown,
        CondCoverageAfter = coverage_known_after(CondCountAfter),
        CondCoverageBefore = coverage_known_before(CondCountBefore),
        detism_get_solutions(CondDetism) = NumSolutions,
        ( NumSolutions = at_most_zero
        ; NumSolutions = at_most_one
        )
    ->
        CondFailures = CondCountBefore - CondCountAfter,
        ElseCoverageBefore2 = coverage_known_before(CondFailures),
        trace [ compile_time(flag("debug_coverage_propagation")), io(!IO) ] (
            io.format("ITE Set coverage before Else: %d\n", 
                [i(CondFailures)], !IO)
        )
    ;
        ElseCoverageBefore2 = ElseCoverageBefore1
    ),
    (
        ElseCoverageAfter1 = coverage_unknown,
        CoverageAfter0 = coverage_known_after(CountAfter),
        ThenCoverageAfter1 = coverage_known_after(ThenCountAfterPrime)
    ->
        ElseCoverageAfter2 = 
            coverage_known_after(CountAfter - ThenCountAfterPrime),
        trace [ compile_time(flag("debug_coverage_propagation")), io(!IO) ] (
            io.format("ITE Set coverage after Else: %d - %d\n", 
                [i(CountAfter), i(ThenCountAfterPrime)], !IO)
        )
    ;
        ElseCoverageAfter2 = ElseCoverageAfter1
    ),
    ElseCoverage2 = merge_coverage(ElseCoverageBefore2, ElseCoverageAfter2),
   
    % Step 4: If more coverage information was inferred then complete the
    % coverage calculations for any inner goals in Then and Else.
    ( ThenCoverage1 \= ThenCoverage2 ->
        goal_annotate_coverage(Info, ThenGoalPath, ThenCoverage2, ThenCoverage,
            Then0, Then)
    ;
        ThenCoverage = ThenCoverage2,
        % Then1 is the result of the previous call to gaol_annotate_coverage
        % for the then goal.
        Then = Then1
    ),
    ( ElseCoverage1 \= ElseCoverage2 ->
        goal_annotate_coverage(Info, ElseGoalPath, ElseCoverage2, ElseCoverage,
            Else0, Else)
    ;
        ElseCoverage = ElseCoverage2,
        % Else1 is the result of the previous call to gaol_annotate_coverage
        % for the else goal.
        Else = Else1
    ),
   
    % Finally update the coverage state for the whole switch.
    (
        get_coverage_after(ThenCoverage) =
            coverage_known_after(ThenCountAfter),
        get_coverage_after(ElseCoverage) = 
            coverage_known_after(ElseCountAfter)
    ->
        CoverageAfter = coverage_known_after(ThenCountAfter + ElseCountAfter),
        trace [ compile_time(flag("debug_coverage_propagation")), io(!IO) ] (
            io.format("ITE Set coverage after ITE: %d + %d\n", 
                [i(ThenCountAfter), i(ElseCountAfter)], !IO)
        )
    ;
        CoverageAfter = get_coverage_after(!.Coverage)
    ),
    ( get_coverage_before(CondCoverage) = coverage_known_before(CountBefore) ->
        CoverageBefore = coverage_known_before(CountBefore),
        trace [ compile_time(flag("debug_coverage_propagation")), io(!IO) ] ( 
            io.format("ITE Set coverage before ITE: %d\n", [i(CountBefore)],
                !IO)
        )
    ;
        CoverageBefore = get_coverage_before(!.Coverage)
    ),
    !:Coverage = merge_coverage(CoverageBefore, CoverageAfter),
    require(check_ite_coverage(!.Coverage, CondCoverage, ThenCoverage,
            ElseCoverage, CondDetism), 
        string.format("check_ite_coverage/4 failed\n" ++ 
            "\tWhole: %s\n\tCond: %s\n\tThen: %s\n\tElse: %s\n",
            [s(string(!.Coverage)), s(string(CondCoverage)),
            s(string(ThenCoverage)), s(string(ElseCoverage))])).

    % Get the coverage information from a coverage point about the branch
    % referenced by the given goal path.
    %
:- pred get_branch_coverage(coverage_reference_info::in, goal_path::in,
    coverage_info::out(coverage_before)) is det.

get_branch_coverage(Info, GoalPath, Coverage) :-
    ( 
        map.search(Info ^ cri_branch_coverage_points, GoalPath, CP)
    ->
        CP = coverage_point(Count, _, _),
        Coverage = coverage_known_before(Count)
    ;
        Coverage = coverage_unknown
    ).

:- pred negation_annotate_coverage(coverage_reference_info::in, goal_path::in,
    coverage_info::in, coverage_info::out, 
    goal_rep::in, goal_rep(coverage_info)::out) is det.

negation_annotate_coverage(Info, GoalPath, Coverage0, Coverage,
        NegGoal0, NegGoal) :-
    split_coverage(Coverage0, CoverageBefore, CoverageAfter0),
    (
        CoverageAfter0 = coverage_known_after(CountAfter0),
        CoverageBefore = coverage_known_before(CountBefore)
    ->
        CoverageAfter1 = coverage_known_after(CountBefore - CountAfter0)
    ;
        CoverageAfter1 = coverage_unknown
    ),
    Coverage1 = merge_coverage(CoverageBefore, CoverageAfter1),
    NegGoalPath = goal_path_add_at_end(GoalPath, step_neg),
    goal_annotate_coverage(Info, NegGoalPath, Coverage1, Coverage2,
        NegGoal0, NegGoal),
    CoverageAfter2 = get_coverage_after(Coverage2),
    (
        CoverageAfter2 = coverage_known_after(CountAfter2),
        CoverageBefore = coverage_known_before(CountBeforePrime)
    ->
        CoverageAfter = coverage_known_after(CountBeforePrime - CountAfter2)
    ;
        CoverageAfter = coverage_unknown 
    ),
    Coverage = merge_coverage(CoverageBefore, CoverageAfter).
        
:- pred scope_annotate_coverage(coverage_reference_info::in, goal_path::in,
    maybe_cut::in, coverage_info::in, coverage_info::out,
    goal_rep::in, goal_rep(coverage_info)::out) is det.

scope_annotate_coverage(Info, GoalPath, MaybeCut, !Coverage, 
        ScopedGoal0, ScopedGoal) :-
    maybe_cut_discard_solutions(MaybeCut, !Coverage),
    ScopeGoalPath = goal_path_add_at_end(GoalPath, step_scope(MaybeCut)),
    goal_annotate_coverage(Info, ScopeGoalPath, !Coverage, ScopedGoal0,
        ScopedGoal),
    maybe_cut_discard_solutions(MaybeCut, !Coverage).

:- pred maybe_cut_discard_solutions(maybe_cut::in,
    coverage_info::in, coverage_info::out) is det.

maybe_cut_discard_solutions(MaybeCut, !Coverage) :-
    (
        MaybeCut = scope_is_cut,
        (
            ( !.Coverage = coverage_unknown
            ; !.Coverage = coverage_known_after(_)
            ),
            !:Coverage = coverage_unknown
        ;
            ( !.Coverage = coverage_known(Before, _)
            ; !.Coverage = coverage_known_det(Before)
            ; !.Coverage = coverage_known_before(Before)
            ),
            !:Coverage = coverage_known_before(Before)
        )
    ;
        MaybeCut = scope_is_no_cut
    ).
    
%----------------------------------------------------------------------------%
%
% These predicates are used to check that computed coverage counts make sense.
%
    
    % Check that the coverage of a goal makes sense given the determinism of
    % that goal.
    %
:- pred check_coverage_regarding_detism(coverage_info::in, detism_rep::in) 
    is semidet.

check_coverage_regarding_detism(Coverage, Detism) :-
    ( Detism = det_rep
    ; Detism = cc_multidet_rep
    ),
    ( Coverage = coverage_known(Count, Count)
    ; Coverage = coverage_known_det(_)
    ; Coverage = coverage_unknown
    ).
check_coverage_regarding_detism(Coverage, Detism) :-
    ( Detism = semidet_rep
    ; Detism = cc_nondet_rep
    ),
    (
        Coverage = coverage_known(Entry, Exit),
        Entry >= Exit
    ; Coverage = coverage_known_before(_)
    ; Coverage = coverage_known_after(_)
    ; Coverage = coverage_known_det(_)
    ; Coverage = coverage_unknown
    ).
check_coverage_regarding_detism(Coverage, multidet_rep) :-
    (
        Coverage = coverage_known(Entry, Exit),
        Entry =< Exit
    ; Coverage = coverage_known_before(_)
    ; Coverage = coverage_known_after(_)
    ; Coverage = coverage_known_det(_)
    ; Coverage = coverage_unknown
    ).
check_coverage_regarding_detism(Coverage, Detism) :-
    ( Detism = erroneous_rep
    ; Detism = failure_rep
    ),
    ( Coverage = coverage_known(_, 0)
    % This probably wont occur, but might
    ; Coverage = coverage_known_det(0)
    ; Coverage = coverage_known_before(_)
    ; Coverage = coverage_known_after(0)
    % This shouldn't occur, we should infer at least coverage_known_after(0)
    ; Coverage = coverage_unknown
    ).
check_coverage_regarding_detism(_Coverage, nondet_rep).

    % Check that the coverages over the switch make sense.  This works only for
    % deterministic switches.
    % 
    % XXX: Re-write this to work on entry counts for switches that cannot fail
    % only.
    %
:- pred check_switch_coverage(detism_rep::in,
    list(case_rep(coverage_info))::in, coverage_info::in) is semidet.

check_switch_coverage(Detism, Cases, Coverage) :-
    (
        ( Detism = det_rep
        ; Detism = cc_multidet_rep
        ),
        list.foldl(sum_switch_case_coverage, Cases, yes(0), MaybeSum),
        (
            MaybeSum = yes(Sum),
            ( 
                ( Coverage = coverage_known_det(Sum)
                ; Coverage = coverage_known(Sum, _)
                ; Coverage = coverage_known_before(Sum)
                ; Coverage = coverage_known_after(_)
                ; Coverage = coverage_unknown
                )
            )
        ;
            MaybeSum = no
        )
    ;
        ( Detism = semidet_rep
        ; Detism = multidet_rep
        ; Detism = nondet_rep
        ; Detism = cc_nondet_rep
        ; Detism = failure_rep
        ; Detism = erroneous_rep
        )
    ).

:- pred sum_switch_case_coverage(case_rep(coverage_info)::in,
    maybe(int)::in, maybe(int)::out) is det.

sum_switch_case_coverage(case_rep(_, _, Goal), !Acc) :-
    (
        !.Acc = yes(Count),
        Coverage = Goal ^ goal_annotation,
        (
            ( Coverage = coverage_known_det(Addend)
            ; Coverage = coverage_known(Addend, _)
            ; Coverage = coverage_known_before(Addend)
            ),
            !:Acc = yes(Count + Addend)
        ;
            ( Coverage = coverage_unknown
            ; Coverage = coverage_known_after(_)
            ),
            !:Acc = no
        )
    ;
        !.Acc = no
    ).

:- pred check_ite_coverage(coverage_info::in, coverage_info::in,
    coverage_info::in, coverage_info::in, detism_rep::in) is semidet.

check_ite_coverage(WholeCoverage, CondCoverage, ThenCoverage, ElseCoverage,
        CondDetism) :-
    (
        coverage_count_before(WholeCoverage, WholeBefore),
        coverage_count_before(CondCoverage, CondBefore)
    ->
        WholeBefore = CondBefore
    ;
        true
    ),
    (
        coverage_count_after(WholeCoverage, WholeAfter),
        coverage_count_after(ThenCoverage, ThenAfter),
        coverage_count_after(ElseCoverage, ElseAfter)
    ->
        WholeAfter = ThenAfter + ElseAfter
    ;
        true
    ),
    (
        coverage_count_after(CondCoverage, CondAfter),
        coverage_count_before(ThenCoverage, ThenBefore)
    ->
        CondAfter = ThenBefore
    ;
        true
    ),
    (
        % This can only be checked when the condition cannot succeed more than
        % once.
        NumSolutions = detism_get_solutions(CondDetism),
        ( NumSolutions = at_most_one
        ; NumSolutions = at_most_zero
        ),
        coverage_count_before(CondCoverage, CondBeforePrime),
        coverage_count_after(CondCoverage, CondAfterPrime),
        coverage_count_before(ElseCoverage, ElseBefore)  
    ->
        ElseBefore = CondBeforePrime - CondAfterPrime
    ;
        true
    ).

%----------------------------------------------------------------------------%
%
% Coverage information helper predicates.
%

    % Retrive the 'before' coverage count, if there is one, otherwise fail.
    %
:- pred coverage_count_before(coverage_info::in, int::out) is semidet.

coverage_count_before(coverage_known(Count, _), Count).
coverage_count_before(coverage_known_before(Count), Count).
coverage_count_before(coverage_known_det(Count), Count).
    
    % Retreive the 'after' coverage count, or fail.
    %
:- pred coverage_count_after(coverage_info::in, int::out) is semidet.

coverage_count_after(coverage_known(_, Count), Count).
coverage_count_after(coverage_known_after(Count), Count).
coverage_count_after(coverage_known_det(Count), Count).

    % The coverage before a det goal should always equal the coverage after.
:- pred propagate_det_coverage(goal_path::in, 
    coverage_info::in, coverage_info::out) is det.

propagate_det_coverage(GoalPath, !Coverage) :-
    (
        !.Coverage = coverage_unknown
    ;
        !.Coverage = coverage_known_det(_)
    ;
        !.Coverage = coverage_known(Before, After),
        ( Before = After ->
            !:Coverage = coverage_known_det(Before)
        ;
            error(format("Coverage before /= after for a det goal: %s, GP: %s",
                [s(string(!.Coverage)), s(goal_path_to_string(GoalPath))]))
        )
    ;
        ( !.Coverage = coverage_known_before(Coverage)
        ; !.Coverage = coverage_known_after(Coverage)
        ),
        !:Coverage = coverage_known_det(Coverage)
    ).

    % If the determinism is deterministic or cc_multi use
    % propagate_det_coverage.
    %
:- pred maybe_propagate_det_coverage(detism_rep::in, goal_path::in, 
    coverage_info::in, coverage_info::out) is det.

maybe_propagate_det_coverage(Detism, GoalPath, !Coverage) :-
    (
        ( Detism = det_rep
        ; Detism = cc_multidet_rep ),
        propagate_det_coverage(GoalPath, !Coverage)
    ;
        Detism = semidet_rep
    ;
        Detism = nondet_rep
    ;
        Detism = multidet_rep
    ;
        Detism = cc_nondet_rep
    ;
        Detism = erroneous_rep
    ;
        Detism = failure_rep
    ).

    % Information about the coverage before a goal only.
    %
:- inst coverage_before
    --->    coverage_unknown
    ;       coverage_known_before(ground).

    % Information about the coverage after a goal only.
    %
:- inst coverage_after
    --->    coverage_unknown
    ;       coverage_known_after(ground).

    % Split a coverage information structure into the coverage before and after
    % a goal.
    %
:- pred split_coverage(coverage_info, coverage_info, coverage_info).
:- mode split_coverage(in, out(coverage_before), out(coverage_after)) is det.
:- mode split_coverage(out, in(coverage_before), in(coverage_after))
    is cc_multi.

split_coverage(Coverage, Before, After) :-
    (
        Coverage = coverage_unknown,
        Before = coverage_unknown,
        After = coverage_unknown
    ;
        Coverage = coverage_known_before(CB),
        Before = coverage_known_before(CB),
        After = coverage_unknown
    ;
        Coverage = coverage_known_after(CA),
        Before = coverage_unknown,
        After = coverage_known_after(CA)
    ;
        Coverage = coverage_known_det(C),
        Before = coverage_known_before(C),
        After = coverage_known_after(C)
    ;
        Coverage = coverage_known(CB, CA),
        Before = coverage_known_before(CB),
        After = coverage_known_after(CA)
    ).

:- func get_coverage_before(coverage_info) = coverage_info.
:- mode get_coverage_before(in) = out(coverage_before) is det.

get_coverage_before(Coverage0) = Coverage :-
    split_coverage(Coverage0, Coverage, _).

:- func get_coverage_after(coverage_info) = coverage_info.
:- mode get_coverage_after(in) = out(coverage_after) is det.

get_coverage_after(Coverage0) = Coverage :-
    split_coverage(Coverage0, _, Coverage).

    % Merge coverage information before and after a goal into coverage
    % information for the whole goal.
    %
:- func merge_coverage(coverage_info, coverage_info) = coverage_info.
:- mode merge_coverage(in(coverage_before), in(coverage_after)) = out is det.

merge_coverage(CoverageBefore, CoverageAfter) = Coverage :-
    promise_equivalent_solutions [Coverage]
        (split_coverage(Coverage, CoverageBefore, CoverageAfter)).

    % If a goal exists (G1 , G2), then the coverage information after G1 is
    % equal to the coverage information before G2.
    %
:- pred goal_transition_coverage(coverage_info, coverage_info).
:- mode goal_transition_coverage(in(coverage_after), out(coverage_before))
    is det.
:- mode goal_transition_coverage(out(coverage_after), in(coverage_before))
    is det.

goal_transition_coverage(CoverageAfterG1, CoverageBeforeG2) :-
    (
        CoverageAfterG1 = coverage_unknown,
        CoverageBeforeG2 = coverage_unknown
    ;
        CoverageAfterG1 = coverage_known_after(C),
        CoverageBeforeG2 = coverage_known_before(C)
    ).

%----------------------------------------------------------------------------%

:- type solution_count
    --->    at_most_zero
    ;       at_most_one   % Including committed choice.
    ;       at_most_many.

:- func detism_get_solutions(detism_rep) = solution_count.

detism_get_solutions(det_rep) =         at_most_one.
detism_get_solutions(semidet_rep) =     at_most_one.
detism_get_solutions(multidet_rep) =    at_most_many.
detism_get_solutions(nondet_rep) =      at_most_many.
detism_get_solutions(cc_multidet_rep) = at_most_one.
detism_get_solutions(cc_nondet_rep) =   at_most_one.
detism_get_solutions(erroneous_rep) =   at_most_zero.
detism_get_solutions(failure_rep) =     at_most_zero.

:- type can_fail
    --->    can_fail
    ;       cannot_fail.

:- func detism_get_can_fail(detism_rep) = can_fail.

detism_get_can_fail(det_rep) =          cannot_fail.
detism_get_can_fail(semidet_rep) =      can_fail.
detism_get_can_fail(multidet_rep) =     cannot_fail.
detism_get_can_fail(nondet_rep) =       can_fail.
detism_get_can_fail(cc_multidet_rep) =  cannot_fail.
detism_get_can_fail(cc_nondet_rep) =    can_fail.
detism_get_can_fail(erroneous_rep) =    cannot_fail.
detism_get_can_fail(failure_rep) =      can_fail.

%----------------------------------------------------------------------------%

