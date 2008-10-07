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
% moved into the mdbcomp.program_representation.m module if it is to be used by
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

    % Ugly-print a module to a string representation. We return a cord of
    % strings is returned rather than a string, since this reduces the cost
    % of string concatenations.
    %
:- pred print_module_to_strings(module_rep::in, cord(string)::out) is det.

    % Print a procedure to a string representation.
    %
:- pred print_proc_to_strings(proc_rep(GoalAnn)::in, cord(string)::out) is det
    <= goal_annotation(GoalAnn).

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

    % Search a program representation for the given procedure and return its
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
    map.foldl(accumulate_print_proc_to_strings, ProcReps,
        cord.empty, ProcStrings),
    Strings = cord.cons(string.format("Module %s\n", [s(ModuleName)]),
        ProcStrings).

    % Print a procedure to a string representation.
    %
:- pred accumulate_print_proc_to_strings(string_proc_label::in,
    proc_rep(GoalAnn)::in, cord(string)::in, cord(string)::out) is det
    <= goal_annotation(GoalAnn).

accumulate_print_proc_to_strings(_, Proc, !Strings) :-
    print_proc_to_strings(Proc, ProcStrings),
    !:Strings = !.Strings ++ ProcStrings.

print_proc_to_strings(ProcRep, Strings) :-
    ProcRep = proc_rep(ProcLabel, ProcDefnRep),
    ProcDefnRep = proc_defn_rep(ArgVarReps, GoalRep, VarTable, Detism),
    print_proc_label_to_strings(Detism, ProcLabel, ProcLabelString),
    print_args_to_strings(print_head_var, VarTable, ArgVarReps, ArgsString),
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

:- pred print_goal_to_strings(var_table::in, int::in, goal_rep(GoalAnn)::in,
    cord(string)::out) is det <= goal_annotation(GoalAnn).

print_goal_to_strings(VarTable, Indent, GoalRep, Strings) :-
    GoalRep = goal_rep(GoalExprRep, DetismRep, GoalAnnotation),
    detism_to_string(DetismRep, DetismString),
    print_goal_annotation_to_strings(GoalAnnotation, GoalAnnotationString),
    (
        GoalExprRep = conj_rep(ConjGoalReps),
        print_conj_to_strings(VarTable, Indent, ConjGoalReps, Strings)
    ;
        GoalExprRep = disj_rep(DisjGoalReps),
        print_disj_to_strings(VarTable, Indent, DisjGoalReps, no, DisjString),
        Strings = indent(Indent) ++ DetismString ++ GoalAnnotationString ++
            cord.singleton(" (\n") ++ DisjString ++ indent(Indent) ++
            cord.singleton(")\n")
    ;
        GoalExprRep = switch_rep(SwitchVarRep, CanFail, CasesRep),
        lookup_var_name(VarTable, SwitchVarRep, SwitchVarName),
        string.format(" %s switch on %s\n",
            [s(string(CanFail)), s(SwitchVarName)], SwitchOnString),
        print_switch_to_strings(VarTable, Indent, CasesRep, no, SwitchString),
        Strings = indent(Indent) ++ DetismString ++ GoalAnnotationString ++
            cord.singleton(SwitchOnString) ++
            indent(Indent) ++ cord.singleton("(\n") ++ SwitchString ++
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
            CutString = cord.singleton(" cut")
        ;
            MaybeCut = scope_is_no_cut,
            CutString = cord.empty
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

:- pred print_conj_to_strings(var_table::in, int::in,
    list(goal_rep(GoalAnn))::in, cord(string)::out) is det
    <= goal_annotation(GoalAnn).

print_conj_to_strings(VarTable, Indent, GoalReps, Strings) :-
    (
        GoalReps = [],
        Strings = cord.snoc(indent(Indent), "true\n")
    ;
        GoalReps = [_ | _],
        print_conj_2_to_strings(VarTable, Indent, GoalReps, Strings)
    ).

:- pred print_conj_2_to_strings(var_table::in, int::in,
    list(goal_rep(GoalAnn))::in, cord(string)::out) is det
    <= goal_annotation(GoalAnn).

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

:- pred print_disj_to_strings(var_table::in, int::in,
    list(goal_rep(GoalAnn))::in, bool::in, cord(string)::out) is det
    <= goal_annotation(GoalAnn).

print_disj_to_strings(_, _Indent, [], _PrintSemi, cord.empty).
print_disj_to_strings(VarTable, Indent, [GoalRep | GoalReps], PrintSemi,
        Strings) :-
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

:- pred print_switch_to_strings(var_table::in, int::in,
    list(case_rep(GoalAnn))::in, bool::in, cord(string)::out) is det
    <= goal_annotation(GoalAnn).

print_switch_to_strings(_, _Indent, [], _PrintSemi, cord.empty).
print_switch_to_strings(VarTable, Indent, [CaseRep | CaseReps], PrintSemi,
        Strings) :-
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
        print_args_to_strings(lookup_var_name, VarTable, ArgReps, ArgsString),
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
        print_args_to_strings(print_maybe_var, VarTable, MaybeArgReps,
            ArgsString),
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
        print_args_to_strings(lookup_var_name, VarTable, Args, ArgsString),
        Strings0 = cord.singleton(" foreign_proc(") ++ ArgsString ++
            cord.singleton(")")
    ;
        AtomicGoalRep = higher_order_call_rep(HOVarRep, Args),
        lookup_var_name(VarTable, HOVarRep, HOVarName),
        string.format(" %s(", [s(HOVarName)], HeadString),
        print_args_to_strings(lookup_var_name, VarTable, Args, ArgsString),
        Strings0 = cord.singleton(HeadString) ++ ArgsString ++
            cord.singleton(")")
    ;
        AtomicGoalRep = method_call_rep(TCIVarRep, MethodNumber, Args),
        lookup_var_name(VarTable, TCIVarRep, TCIVarName),
        string.format(" method %d of %s(", [i(MethodNumber), s(TCIVarName)],
            HeadString),
        print_args_to_strings(lookup_var_name, VarTable, Args, ArgsString),
        Strings0 = cord.singleton(HeadString) ++ ArgsString ++
            cord.singleton(")")
    ;
        AtomicGoalRep = plain_call_rep(Module, Pred, Args),
        string.format(" %s.%s", [s(Module), s(Pred)], HeadString),
        print_args_to_strings(lookup_var_name, VarTable, Args, ArgsString),
        Strings0 = cord.cons(HeadString, ArgsString)
    ;
        AtomicGoalRep = builtin_call_rep(Module, Pred, Args),
        string.format(" builtin %s.%s", [s(Module), s(Pred)], HeadString),
        print_args_to_strings(lookup_var_name, VarTable, Args, ArgsString),
        Strings0 = cord.cons(HeadString, ArgsString)
    ;
        AtomicGoalRep = event_call_rep(Event, Args),
        string.format(" event %s", [s(Event)], HeadString),
        print_args_to_strings(lookup_var_name, VarTable, Args, ArgsString),
        Strings0 = cord.cons(HeadString, ArgsString)
    ),
    detism_to_string(DetismRep, DetismString),
    Strings = indent(Indent) ++ DetismString ++ GoalAnnotationString ++
        Strings0 ++ nl.

%-----------------------------------------------------------------------------%

:- pred print_args_to_strings(pred(var_table, T, string), var_table,
    list(T), cord(string)).
:- mode print_args_to_strings(pred(in, in, out) is det, in, in, out) is det.

print_args_to_strings(PrintArg, VarTable, Args, Strings) :-
    (
        Args = [],
        Strings = cord.empty
    ;
        Args = [_ | _],
        print_args_2_to_strings(PrintArg, VarTable, Args, cord.empty, ArgsStr),
        Strings = cord.cons("(", cord.snoc(ArgsStr, ")"))
    ).

:- pred print_args_2_to_strings(pred(var_table, T, string), var_table,
    list(T), cord(string), cord(string)).
:- mode print_args_2_to_strings(pred(in, in, out) is det, in, in, in, out)
    is det.

print_args_2_to_strings(_, _, [], _, cord.empty).
print_args_2_to_strings(PrintArg, VarTable, [Arg | Args], Prefix, Strings) :-
    PrintArg(VarTable, Arg, ArgName),
    print_args_2_to_strings(PrintArg, VarTable, Args, cord.singleton(", "),
        ArgsString),
    Strings = Prefix ++ cord.cons(ArgName, ArgsString).

:- pred print_maybe_var(var_table::in, maybe(var_rep)::in, string::out) is det.

print_maybe_var(_, no, "_").
print_maybe_var(VarTable, yes(VarRep), VarName) :-
    lookup_var_name(VarTable, VarRep, VarName).

:- pred print_head_var(var_table::in, head_var_rep::in, string::out) is det.

print_head_var(VarTable, head_var_rep(VarRep, VarMode), String) :-
    lookup_var_name(VarTable, VarRep, VarName),
    VarMode = var_mode_rep(InitialInst, FinalInst),
    inst_rep_to_string(InitialInst, InitialInstStr),
    inst_rep_to_string(FinalInst, FinalInstStr),
    String = string.format("%s::(%s >> %s)",
        [s(VarName), s(InitialInstStr), s(FinalInstStr)]).

:- pred inst_rep_to_string(inst_rep::in, string::out) is det.

inst_rep_to_string(ir_free_rep, "free").
inst_rep_to_string(ir_ground_rep, "ground").
inst_rep_to_string(ir_other_rep, "other").

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

:- type coverage_before
    --->    before_unknown
    ;       before_known(int).

:- type coverage_after
    --->    after_unknown
    ;       after_known(int).

:- type sum_coverage_before
    --->    sum_before_unknown
    ;       sum_before_known(int).

:- type sum_coverage_after
    --->    sum_after_unknown
    ;       sum_after_known(int).

    % Annotate a procedure representation structure with coverage information.
    %
    % The following trace flags control debugging for this predicate.
    %
    %   debug_coverage_propagation:
    %       Print out diagnostic messages to aid in the debugging of the
    %       propagation coverage algorithm.
    %
    %   no_coverage_propagation_assertions:
    %       Disable assertions used to test this algorithm, This allows the
    %       algorithm to proceed past the problem and allow the programmer to
    %       view erroneous output.
    %
procrep_annotate_with_coverage(OwnProf, CallSites, SolnsCoveragePoints,
        BranchCoveragePoints, !ProcRep) :-
    some [!ProcDefn, !GoalRep] (
        !:ProcDefn = !.ProcRep ^ pr_defn,
        !:GoalRep = !.ProcDefn ^ pdr_goal,
        Calls = calls(OwnProf),
        Exits = exits(OwnProf),
        Before0 = before_known(Calls),
        After0 = after_known(Exits),
        CoverageReference = coverage_reference_info(CallSites,
            SolnsCoveragePoints, BranchCoveragePoints),
        goal_annotate_coverage(CoverageReference, empty_goal_path,
            Before0, _, After0, _, !GoalRep),
        !:ProcDefn = !.ProcDefn ^ pdr_goal := !.GoalRep,
        !:ProcRep = !.ProcRep ^ pr_defn := !.ProcDefn
    ).

    % These maps are keyed by goal_path, comparing these structures is less
    % efficient than comparing simple structures like the alternative
    % goal_path_string, however, that involves frequently constructing strings
    % from goal paths.  Using goal_path_string may be faster but I'd rather not
    % make this optimisation without first testing it.
    %
:- type coverage_reference_info
    --->    coverage_reference_info(
                cri_call_sites              :: map(goal_path, call_site_perf),
                cri_solns_coverage_points   :: map(goal_path, coverage_point),
                cri_branch_coverage_points  :: map(goal_path, coverage_point)
            ).

    % Annotate a goal and its children with coverage information.
    %
:- pred goal_annotate_coverage(coverage_reference_info::in, goal_path::in,
    coverage_before::in, coverage_before::out,
    coverage_after::in, coverage_after::out,
    goal_rep(unit)::in, goal_rep(coverage_info)::out) is det.

goal_annotate_coverage(Info, GoalPath, !Before, !After, Goal0, Goal) :-
    Goal0 = goal_rep(GoalExpr0, Detism, _),

    % Calculate coverage of any inner goals.
    (
        GoalExpr0 = conj_rep(Conjuncts0),
        conj_annotate_coverage(Info, GoalPath, !Before, !After,
            Conjuncts0, Conjuncts),
        GoalExpr = conj_rep(Conjuncts)
    ;
        GoalExpr0 = disj_rep(Disjuncts0),
        disj_annotate_coverage(Info, Detism, GoalPath, !Before, !After,
            Disjuncts0, Disjuncts),
        GoalExpr = disj_rep(Disjuncts)
    ;
        GoalExpr0 = switch_rep(Var, CanFail, Cases0),
        switch_annotate_coverage(Info, CanFail, GoalPath, !Before, !After,
            Cases0, Cases),
        GoalExpr = switch_rep(Var, CanFail, Cases)
    ;
        GoalExpr0 = ite_rep(Cond0, Then0, Else0),
        ite_annotate_coverage(Info, GoalPath, !Before, !After, Cond0, Cond,
            Then0, Then, Else0, Else),
        GoalExpr = ite_rep(Cond, Then, Else)
    ;
        GoalExpr0 = negation_rep(NegGoal0),
        negation_annotate_coverage(Info, GoalPath, Detism, !Before, !After,
            NegGoal0, NegGoal),
        GoalExpr = negation_rep(NegGoal)
    ;
        GoalExpr0 = scope_rep(ScopedGoal0, MaybeCut),
        scope_annotate_coverage(Info, GoalPath, MaybeCut, Detism, 
            !Before, !After, ScopedGoal0, ScopedGoal),
        GoalExpr = scope_rep(ScopedGoal, MaybeCut)
    ;
        GoalExpr0 = atomic_goal_rep(Filename, Line, Vars, AtomicGoal),
        % Note that GoalExpr != GoalExpr0, since they are of different types.
        GoalExpr = atomic_goal_rep(Filename, Line, Vars, AtomicGoal),
        (
            ( AtomicGoal = plain_call_rep(_, _, _)
            ; AtomicGoal = higher_order_call_rep(_, _)
            ; AtomicGoal = method_call_rep(_, _, _)
            ),
            ( map.search(Info ^ cri_call_sites, GoalPath, CallSite) ->
                Summary = CallSite ^ csf_summary_perf,
                % Entry due to redo is not counted at the point before the
                % goal, it is represented when the number of exists is greater
                % than the number of calls. XXX This won't work with nondet
                % code, which should be fixed in the future.
                Calls = Summary ^ perf_row_calls,
                Exits = Summary ^ perf_row_exits,
                !:Before = before_known(Calls),
                !:After = after_known(Exits)
            ;
                error("Couldn't look up call site for port counts GP: " ++
                    goal_path_to_string(GoalPath))
            )
        ;
            ( AtomicGoal = builtin_call_rep(_, _, _)
            ; AtomicGoal = unify_construct_rep(_, _, _)
            ; AtomicGoal = unify_deconstruct_rep(_, _, _)
            ; AtomicGoal = partial_construct_rep(_, _, _)
            ; AtomicGoal = partial_deconstruct_rep(_, _, _)
            ; AtomicGoal = unify_assign_rep(_, _)
            ; AtomicGoal = cast_rep(_, _)
            ; AtomicGoal = unify_simple_test_rep(_, _)
            ; AtomicGoal = pragma_foreign_code_rep(_)
            ; AtomicGoal = event_call_rep(_, _)
            ),
            propagate_detism_coverage(Detism, GoalPath, !Before, !After)
        )
    ),

    % Search for a coverage point after this goal.
    ( map.search(Info ^ cri_solns_coverage_points, GoalPath, CoveragePoint) ->
        CoveragePoint = coverage_point(CoverageAfterCount, _, _),
        after_count_from_either_source(after_known(CoverageAfterCount), !After)
    ;
        true
    ),
    GoalCoverage = construct_before_after_coverage(!.Before, !.After),
    Goal = goal_rep(GoalExpr, Detism, GoalCoverage),

    trace [compile_time(flag("debug_coverage_propagation")), io(!IO)] (
        io.write_string("goal_annotate_coverage: done\n", !IO),
        io.format("\tGoalPath: %s\n\tDetism %s\n\tCoverage; %s\n",
            [s(goal_path_to_string(GoalPath)),
             s(string(Detism)),
             s(string(GoalCoverage))], !IO)
    ),
    trace [compile_time(not flag("no_coverage_propagation_assertions"))] (
        require(check_coverage_complete(GoalCoverage, GoalExpr),
            string.format("check_coverage_complete failed\n" ++
                "\tCoverage: %s\n\tGoalPath: %s\n",
                [s(string(GoalCoverage)), s(goal_path_to_string(GoalPath))])),
        require(check_coverage_regarding_detism(GoalCoverage, Detism),
            string.format("check_coverage_regarding_detism failed: %s %s",
                    [s(string(GoalCoverage)), s(string(Detism))]))
    ).

:- func construct_before_after_coverage(coverage_before, coverage_after)
    = coverage_info.

construct_before_after_coverage(Before, After) = Coverage :-
    (
        Before = before_unknown,
        After = after_unknown,
        Coverage = coverage_unknown
    ;
        Before = before_unknown,
        After = after_known(AfterExecCount),
        Coverage = coverage_known_after(AfterExecCount)
    ;
        Before = before_known(BeforeExecCount),
        After = after_unknown,
        Coverage = coverage_known_before(BeforeExecCount)
    ;
        Before = before_known(BeforeExecCount),
        After = after_known(AfterExecCount),
        ( BeforeExecCount = AfterExecCount ->
            Coverage = coverage_known_same(BeforeExecCount)
        ;
            Coverage = coverage_known(BeforeExecCount, AfterExecCount)
        )
    ).

    % Annotate a conjunction with coverage information.
    %
:- pred conj_annotate_coverage(coverage_reference_info::in, goal_path::in,
    coverage_before::in, coverage_before::out,
    coverage_after::in, coverage_after::out,
    list(goal_rep(unit))::in, list(goal_rep(coverage_info))::out) is det.

conj_annotate_coverage(Info, GoalPath, !Before, !After, Conjs0, Conjs) :-
    conj_annotate_coverage_2(Info, GoalPath, 1, !Before, !After,
        Conjs0, Conjs).

    % Annotate a conjunction with coverage information.
    %
    % The list of goals is the tail of a conjunction, the coverage argument
    % describes the coverage of this list of goals if it were the entire
    % conjunction.  Each goal also has it's own coverage.
    %
:- pred conj_annotate_coverage_2(coverage_reference_info::in,
    goal_path::in, int::in,
    coverage_before::in, coverage_before::out,
    coverage_after::in, coverage_after::out,
    list(goal_rep(unit))::in, list(goal_rep(coverage_info))::out) is det.

conj_annotate_coverage_2(_, GoalPath, ConjunctNum, !Before, !After, [], []) :-
    % The empty conjunction is equivalent to 'true' which is deterministic,
    ConjGoalPath = goal_path_add_at_end(GoalPath, step_conj(ConjunctNum)),
    propagate_det_coverage(ConjGoalPath, !Before, !After).
conj_annotate_coverage_2(Info, GoalPath, ConjunctNum, !Before, !After,
        [Conj0 | Conjs0], [Conj | Conjs]) :-
    HeadGoalPath = goal_path_add_at_end(GoalPath, step_conj(ConjunctNum)),
    goal_annotate_coverage(Info, HeadGoalPath,
        !Before, after_unknown, CoverageAfterHead, Conj0, Conj),
    after_to_before_coverage(CoverageAfterHead, CoverageBeforeTail),
    conj_annotate_coverage_2(Info, GoalPath, ConjunctNum + 1,
        CoverageBeforeTail, _, !After, Conjs0, Conjs).

    % Compute the coverage information for a disjunction.
    %
    % Rules:
    %   - The coverage before a disjunction is equal to the coverage before the
    %     first disjunct.
    %   - The coverage after a disjunction is equal to the sum of coverages
    %     after each disjunct.
    %
:- pred disj_annotate_coverage(coverage_reference_info::in,
    detism_rep::in, goal_path::in,
    coverage_before::in, coverage_before::out,
    coverage_after::in, coverage_after::out,
    list(goal_rep(unit))::in, list(goal_rep(coverage_info))::out) is det.

disj_annotate_coverage(Info, Detism, GoalPath, !Before, !After,
        Disjs0, Disjs) :-
    % XXX In theory, we could update Before using information from any counter
    % at the start of the first disjunct, but we don't do that (yet).  This may
    % not be useful for some disjunctions, for example those called from a
    % single solution context or committed-choice.
    CoverageBefore = !.Before,
    Solutions = detism_get_solutions(Detism),
    disj_annotate_coverage_2(Info, GoalPath, 1, Solutions,
        CoverageBefore, sum_after_known(0), SumAfterDisjuncts, Disjs0, Disjs),
    after_count_from_either_source_sum(SumAfterDisjuncts, !After).

:- pred disj_annotate_coverage_2(coverage_reference_info::in,
    goal_path::in, int::in, solution_count_rep::in, coverage_before::in,
    sum_coverage_after::in, sum_coverage_after::out,
    list(goal_rep)::in, list(goal_rep(coverage_info))::out) is det.

disj_annotate_coverage_2(_, _, _, _, _, !SumAfter, [], []).
disj_annotate_coverage_2(Info, GoalPath, DisjNum, Solutions,
        Before0, !SumAfter, [Disj0 | Disjs0], [Disj | Disjs]) :-
    DisjGoalPath = goal_path_add_at_end(GoalPath, step_disj(DisjNum)),
    (
        Before0 = before_known(_),
        Before1 = Before0
    ;
        Before0 = before_unknown,
        get_branch_start_coverage(Info, DisjGoalPath, Before1)
    ),
    goal_annotate_coverage(Info, DisjGoalPath,
        Before1, _, after_unknown, After, Disj0, Disj),
    sum_after_coverage(After, !SumAfter),
    % We don't know how many times the start of the next disjunct is executed
    % unless we have a counter there.
    disj_annotate_coverage_2(Info, GoalPath, DisjNum + 1, Solutions,
        before_unknown, !SumAfter, Disjs0, Disjs).

:- pred switch_annotate_coverage(coverage_reference_info::in,
    switch_can_fail_rep::in, goal_path::in,
    coverage_before::in, coverage_before::out,
    coverage_after::in, coverage_after::out,
    list(case_rep(unit))::in, list(case_rep(coverage_info))::out) is det.

switch_annotate_coverage(Info, CanFail, GoalPath, !Before, !After,
        Cases0, Cases) :-
    trace [compile_time(flag("debug_coverage_propagation")), io(!IO)] (
        io.format("Switch: Before0: %s, After0: %s\n",
            [s(string(!.Before)), s(string(!.After))], !IO)
    ),

    switch_annotate_coverage_2(Info, CanFail, GoalPath, 1,
        sum_before_known(0), SumBefore, sum_after_known(0), SumAfter,
        !.Before, !.After, Cases0, Cases),
    % For can_fail switches, the sum of the exec counts at the starts of the
    % arms may be less than the exec count at the start of the switch. However,
    % even for can_fail switches, the sum of the exec counts at the *ends* of
    % the arms will always equal the exec count at the end of the switch.
    (
        CanFail = switch_can_not_fail_rep,
        before_count_from_either_source_sum(SumBefore, !Before)
    ;
        CanFail = switch_can_fail_rep
    ),
    after_count_from_either_source_sum(SumAfter, !After),

    trace [compile_time(not flag("no_coverage_propagation_assertions"))] (
        require(check_switch_coverage(CanFail, Cases, !.Before),
            string.format("check_switch_coverage failed\n\t" ++
                "CanFail: %s\n\tCases: %s\n\tBefore: %s, After: %s\n",
                [s(string(CanFail)), s(string(Cases)),
                s(string(!.Before)), s(string(!.After))]))
    ).

    % switch_annotate_coverage_2(Info, Detism, GoalPath, CaseNum,
    %   !CoverageSum, SwitchCoverage, !Cases),
    %
    % Perform coverage annotation on cases from the left to the right.
    % The head of the !.Cases list is case number CaseNum, SwitchCoverage
    % is the coverage for the entire switch as known by the caller,
    % !CoverageSum is the sum of the coverage so far.
    %
    % For this goal we use a forwards traversal, since the last goal may not
    % have a coverage point after it, in the expectation that the coverage at
    % the end of the last goal may need to be computed from the coverage of
    % each of the other goals.
    %
:- pred switch_annotate_coverage_2(coverage_reference_info::in,
    switch_can_fail_rep::in, goal_path::in, int::in,
    sum_coverage_before::in, sum_coverage_before::out,
    sum_coverage_after::in, sum_coverage_after::out,
    coverage_before::in, coverage_after::in,
    list(case_rep(unit))::in, list(case_rep(coverage_info))::out) is det.

switch_annotate_coverage_2(_, _, _, _, !SumBefore, !SumAfter, _, _, [], []).
switch_annotate_coverage_2(Info, CanFail, GoalPath, CaseNum,
        !SumBefore, !SumAfter, SwitchBefore, SwitchAfter,
        [Case0 | Cases0], [Case | Cases]) :-
    CaseGoalPath = goal_path_add_at_end(GoalPath, step_switch(CaseNum, no)),

    % If this is the last case in the switch, then its coverage information
    % may be computed from the coverage of other cases and the coverage of the
    % whole switch.  This is only done for the last goal, since only this
    % optimisation is made by the coverage transformation in the compiler.
    %
    % If we cannot calculate this case's coverage information, then try to
    % retrieve the information from a coverage point associated with the case.
    (
        Cases0 = [],
        CanFail = switch_can_not_fail_rep
    ->
        (
            SwitchBefore = before_known(SwitchBeforeExecCount),
            !.SumBefore = sum_before_known(SumBeforeExecCount)
        ->
            BeforeCase0 =
                before_known(SwitchBeforeExecCount - SumBeforeExecCount)
        ;
            % Search for a coverage point for this case.
            get_branch_start_coverage(Info, CaseGoalPath, BeforeCase0)
        ),
        (
            SwitchAfter = after_known(SwitchAfterExecCount),
            !.SumAfter = sum_after_known(SumAfterExecCount)
        ->
            AfterCase0 = after_known(SwitchAfterExecCount - SumAfterExecCount)
        ;
            AfterCase0 = after_unknown
        )
    ;
        % Search for a coverage point for this case.
        get_branch_start_coverage(Info, CaseGoalPath, BeforeCase0),
        AfterCase0 = after_unknown
    ),

    % Calculate and annotate the coverage for the case itself.
    Case0 = case_rep(ConsID, OtherConsIDs, Goal0),
    goal_annotate_coverage(Info, CaseGoalPath,
        BeforeCase0, BeforeCase, AfterCase0, AfterCase, Goal0, Goal),
    Case = case_rep(ConsID, OtherConsIDs, Goal),

    % Keep a sum of the execution counts seen in cases so far.
    sum_before_coverage(BeforeCase, !SumBefore),
    sum_after_coverage(AfterCase, !SumAfter),

    switch_annotate_coverage_2(Info, CanFail, GoalPath, CaseNum + 1,
        !SumBefore, !SumAfter, SwitchBefore, SwitchAfter, Cases0, Cases).

    % Propagate coverage information for if-then-else goals.
    %
:- pred ite_annotate_coverage(coverage_reference_info::in, goal_path::in,
    coverage_before::in, coverage_before::out,
    coverage_after::in, coverage_after::out,
    goal_rep::in, goal_rep(coverage_info)::out,
    goal_rep::in, goal_rep(coverage_info)::out,
    goal_rep::in, goal_rep(coverage_info)::out) is det.

ite_annotate_coverage(Info, GoalPath, !Before, !After,
        Cond0, Cond, Then0, Then, Else0, Else) :-
    CondGoalPath = goal_path_add_at_end(GoalPath, step_ite_cond),
    ThenGoalPath = goal_path_add_at_end(GoalPath, step_ite_then),
    ElseGoalPath = goal_path_add_at_end(GoalPath, step_ite_else),
    CondDetism = Cond0 ^ goal_detism_rep,

    % Step 1:
    %   Call goal_annotate_coverage for the condition goal.
    goal_annotate_coverage(Info, CondGoalPath,
        !.Before, BeforeCond, after_unknown, AfterCond, Cond0, Cond),
    after_to_before_coverage(AfterCond, BeforeThen0),

    % Step 2:
    %   Lookup coverage information for the starts of the then and else goals.
    (
        BeforeThen0 = before_known(_),
        BeforeThen1 = BeforeThen0
    ;
        BeforeThen0 = before_unknown,
        get_branch_start_coverage(Info, ThenGoalPath, BeforeThen1)
    ),
    % XXX It should be possible, if the condition is not at_most_many and does
    % not throw exceptions, to compute BeforeElse as the difference between the
    % counts in the initial value of !.Before and AfterCond, if both are known.
    % check_ite_coverage already knows the relationship.  Using exception
    % counts on call goals and propagating them through the coverage annotation
    % algorithms can solve this.
    get_branch_start_coverage(Info, ElseGoalPath, BeforeElse1),

    trace [compile_time(flag("debug_coverage_propagation")), io(!IO)] (
        io.format("ITE Coverage inferred before then and else branches:\n" ++
            "\tWhole: %s %s\n\tThen: %s\n\tElse: %s\n" ++
            "\tGoalPath %s\n",
            [s(string(!.Before)), s(string(!.After)),
            s(string(BeforeThen1)), s(string(BeforeElse1)),
            s(goal_path_to_string(GoalPath))], !IO)
    ),

    % Step 3:
    %   Call goal_annotate_coverage for the then and else goals.
    goal_annotate_coverage(Info, ThenGoalPath,
        BeforeThen1, BeforeThen, after_unknown, AfterThen, Then0, Then),
    goal_annotate_coverage(Info, ElseGoalPath,
        BeforeElse1, BeforeElse, after_unknown, AfterElse, Else0, Else),

    % Step 4:
    %   Update what we know about the if-then-else as a whole.
    before_count_from_either_source(BeforeCond, !Before),
    (
        AfterThen = after_known(AfterThenExecCount),
        AfterElse = after_known(AfterElseExecCount)
    ->
        AfterThenElse = after_known(AfterThenExecCount + AfterElseExecCount),
        after_count_from_either_source(AfterThenElse, !After)
    ;
        true
    ),

    trace [compile_time(not flag("no_coverage_propagation_assertions"))] (
        require(
            check_ite_coverage(!.Before, !.After, BeforeCond, AfterCond,
                BeforeThen, AfterThen, BeforeElse, AfterElse, CondDetism),
            string.format("check_ite_coverage/4 failed\n" ++
                "\tWhole: %s %s\n" ++
                "\tCond: %s %s\n\tThen: %s %s\n\tElse: %s %s\n" ++
                "\tGoalPath: %s\n",
                [s(string(!.Before)), s(string(!.After)),
                s(string(BeforeCond)), s(string(AfterCond)),
                s(string(BeforeThen)), s(string(AfterThen)),
                s(string(BeforeElse)), s(string(AfterElse)),
                s(goal_path_to_string(GoalPath))]))
    ).

:- pred not_unify(T::in, T::in) is semidet.

not_unify(A, B) :- not unify(A, B).

    % Get the coverage information from a coverage point about the branch
    % referenced by the given goal path.
    %
:- pred get_branch_start_coverage(coverage_reference_info::in, goal_path::in,
    coverage_before::out) is det.

get_branch_start_coverage(Info, GoalPath, Before) :-
    ( map.search(Info ^ cri_branch_coverage_points, GoalPath, CP) ->
        CP = coverage_point(ExecCount, _, _),
        Before = before_known(ExecCount)
    ;
        Before = before_unknown
    ).

:- pred negation_annotate_coverage(coverage_reference_info::in, goal_path::in,
    detism_rep::in, coverage_before::in, coverage_before::out,
    coverage_after::in, coverage_after::out,
    goal_rep::in, goal_rep(coverage_info)::out) is det.

negation_annotate_coverage(Info, GoalPath, Detism, !Before, !After,
        NegGoal0, NegGoal) :-
    NegGoalPath = goal_path_add_at_end(GoalPath, step_neg),
    goal_annotate_coverage(Info, NegGoalPath,
        !Before, after_unknown, _CoverageAfter, NegGoal0, NegGoal),
    % Set the coverage after based on the coverage before and the determinism
    % of the entire goal.  There's no reason to update the coverage before this
    % goal since it has already been applied to the inner goal.
    propagate_detism_coverage(Detism, GoalPath, !.Before, _Before, !After), 
    trace [compile_time(flag("debug_coverage_propagation")), io(!IO)] (
        io.format("Negation: setting negation: before %s, after %s\n",
            [s(string(!.Before)), s(string(!.After))], !IO)
    ).

:- pred scope_annotate_coverage(coverage_reference_info::in,
    goal_path::in, maybe_cut::in, detism_rep::in,
    coverage_before::in, coverage_before::out,
    coverage_after::in, coverage_after::out,
    goal_rep::in, goal_rep(coverage_info)::out) is det.

scope_annotate_coverage(Info, GoalPath, MaybeCut, Detism, !Before, !After,
        ScopedGoal0, ScopedGoal) :-
    ScopeGoalPath = goal_path_add_at_end(GoalPath, step_scope(MaybeCut)),
    AfterScopeGoal = !.After,
    (
        MaybeCut = scope_is_cut,
        AfterScopedGoal0 = after_unknown
    ;
        MaybeCut = scope_is_no_cut,
        AfterScopedGoal0 = !.After
    ),
    goal_annotate_coverage(Info, ScopeGoalPath,
        !Before, AfterScopedGoal0, AfterScopedGoal, ScopedGoal0, ScopedGoal),
    (
        MaybeCut = scope_is_cut,
        (
            AfterScopeGoal = after_known(_),
            !:After = AfterScopeGoal
        ;
            AfterScopeGoal = after_unknown,
            % If coverage is unknown after the scoped goal try to infer it from
            % the determinism of the goal.
            propagate_detism_coverage(Detism, GoalPath, !.Before, _Before,
                after_unknown, !:After)
        )
    ;
        MaybeCut = scope_is_no_cut,
        !:After = AfterScopedGoal
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
    detism_coverage_ok(Coverage, Detism) = yes.

:- func detism_coverage_ok(coverage_info, detism_rep) = bool.

detism_coverage_ok(Coverage, Detism) = OK :-
    (
        ( Detism = det_rep
        ; Detism = cc_multidet_rep
        ),
        (
            ( Coverage = coverage_known_same(_)
            ; Coverage = coverage_unknown
            ),
            OK = yes
        ;
            Coverage = coverage_known(Entry, Exit),
            % Execution may leave via the Excp port rather than the exit port.
            % so the exit port count may be smaller than or equal to the entry
            % port count.
            ( Entry >= Exit ->
                OK = yes
            ;
                OK = no
            )
        ;
            ( Coverage = coverage_known_before(_)
            ; Coverage = coverage_known_after(_)
            ),
            % If you known coverage at one of these points, you can compute
            % the coverage at the other point.
            OK = no
        )
    ;
        ( Detism = semidet_rep
        ; Detism = cc_nondet_rep
        ),
        (
            ( Coverage = coverage_known_before(_)
            ; Coverage = coverage_known_after(_)
            ; Coverage = coverage_known_same(_)
            ; Coverage = coverage_unknown
            ),
            OK = yes
        ;
            Coverage = coverage_known(Entry, Exit),
            ( Entry >= Exit ->
                OK = yes
            ;
                OK = no
            )
        )
    ;
        Detism = multidet_rep,
        (
            ( Coverage = coverage_known_before(_)
            ; Coverage = coverage_known_after(_)
            ; Coverage = coverage_known_same(_)
            ; Coverage = coverage_unknown
            ),
            OK = yes
        ;
            Coverage = coverage_known(_Entry, _Exit),
            % If the goal throws exceptions no inequalities can be used to
            % check the correctness of the coverage information.
            OK = yes
        )
    ;
        Detism = nondet_rep,
        OK = yes
    ;
        ( Detism = erroneous_rep
        ; Detism = failure_rep
        ),
        (
            % The coverage_known_dert case probably won't occur, but it might.
            ( Coverage = coverage_known(_, Exit)
            ; Coverage = coverage_known_same(Exit)
            ; Coverage = coverage_known_after(Exit)
            ),
            ( Exit = 0 ->
                OK = yes
            ;
                OK = no
            )
        ;
            Coverage = coverage_known_before(_),
            OK = yes
        ;
            Coverage = coverage_unknown,
            % This shouldn't occur, we should infer at least
            % coverage_known_after(0).
            OK = yes
        )
    ).

    % Check that the coverage on the switch goal and on its cases do not
    % contradict with each other.  This works only for cannot_fail switches.
    %
:- pred check_switch_coverage(switch_can_fail_rep::in,
    list(case_rep(coverage_info))::in, coverage_before::in) is semidet.

check_switch_coverage(CanFail, Cases, Before) :-
    (
        CanFail = switch_can_not_fail_rep,
        list.foldl(sum_switch_case_coverage, Cases, yes(0), MaybeSum),
        (
            MaybeSum = yes(Sum),
            (
                ( Before = before_known(Sum)
                ; Before = before_unknown
                )
            )
        ;
            MaybeSum = no
        )
    ;
        CanFail = switch_can_fail_rep
    ).

:- pred sum_switch_case_coverage(case_rep(coverage_info)::in,
    maybe(int)::in, maybe(int)::out) is det.

sum_switch_case_coverage(case_rep(_, _, Goal), !Acc) :-
    (
        !.Acc = yes(Count),
        Coverage = Goal ^ goal_annotation,
        (
            ( Coverage = coverage_known_same(Addend)
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

:- pred check_ite_coverage(coverage_before::in, coverage_after::in,
    coverage_before::in, coverage_after::in,
    coverage_before::in, coverage_after::in,
    coverage_before::in, coverage_after::in,
    detism_rep::in) is semidet.

check_ite_coverage(Before, After, BeforeCond, AfterCond,
        BeforeThen, AfterThen, _BeforeElse, AfterElse, CondDetism) :-
    (
        Before = before_known(BeforeExecCount),
        BeforeCond = before_known(BeforeCondExecCount)
    ->
        BeforeExecCount = BeforeCondExecCount
    ;
        true
    ),
    (
        After = after_known(AfterExecCount),
        AfterThen = after_known(AfterThenExecCount),
        AfterElse = after_known(AfterElseExecCount)
    ->
        AfterExecCount = AfterThenExecCount + AfterElseExecCount
    ;
        true
    ),
    (
        AfterCond = after_known(AfterCondExecCount),
        BeforeThen = before_known(BeforeKnownExecCount)
    ->
        AfterCondExecCount = BeforeKnownExecCount
    ;
        true
    ),
    % Since the condition may throw exceptions and exception count information
    % is not propagated checking the coverage before the else goal based on the
    % coverage before and after the condition goal cannot be done.
    
    ( AfterCond = after_known(AfterCondExecCount2) ->
        NumSolutions = detism_get_solutions(CondDetism),
        (
            NumSolutions = at_most_zero_rep,
            AfterCondExecCount2 = 0
        ;
            ( NumSolutions = at_most_one_rep
            ; NumSolutions = at_most_many_rep
            )
        )
    ;
        true
    ).

:- pred check_coverage_complete(coverage_info::in, goal_expr_rep(T)::in)
    is semidet.

check_coverage_complete(coverage_known(_, _), _GoalExpr).
check_coverage_complete(coverage_known_same(_), _GoalExpr).
% Uncomment this clause if, in the future, we allow coverage to be incomplete
% for trivial goals.
%check_coverage_complete(Coverage, GoalExpr) :-
%    ( Coverage = coverage_known_before(_)
%    ; Coverage = coverage_known_after(_)
%    ; Coverage = coverage_unknown
%    ),
%    goal_expr_is_trivial(GoalExpr).

:- func goal_is_trivial(goal_rep(T)) = bool.

goal_is_trivial(Goal) = IsTrivial:-
    GoalExpr = Goal ^ goal_expr_rep,
    IsTrivial = goal_expr_is_trivial(GoalExpr).

:- func goal_expr_is_trivial(goal_expr_rep(T)) = bool.

goal_expr_is_trivial(GoalRep) = IsTrivial :-
    (
        (
            GoalRep = conj_rep(SubGoalReps)
        ;
            GoalRep = disj_rep(SubGoalReps)
        ;
            GoalRep = switch_rep(_, _, CaseReps),
            SubGoalReps = list.map(project_case_rep_goal, CaseReps)
        ;
            GoalRep = ite_rep(CondRep, ThenRep, ElseRep),
            SubGoalReps = [CondRep, ThenRep, ElseRep]
        ),
        SubGoalIsTrivials = list.map(goal_is_trivial, SubGoalReps),
        bool.and_list(SubGoalIsTrivials, IsTrivial)
    ;
        ( GoalRep = negation_rep(SubGoalRep)
        ; GoalRep = scope_rep(SubGoalRep, _)
        ),
        IsTrivial = goal_is_trivial(SubGoalRep)
    ;
        GoalRep = atomic_goal_rep(_, _, _, AtomicGoalRep),
        (
            ( AtomicGoalRep = plain_call_rep(_, _, _)
            ; AtomicGoalRep = higher_order_call_rep(_, _)
            ; AtomicGoalRep = method_call_rep(_, _, _)
            ),
            IsTrivial = no
        ;
            ( AtomicGoalRep = unify_construct_rep(_, _, _)
            ; AtomicGoalRep = unify_deconstruct_rep(_, _, _)
            ; AtomicGoalRep = partial_deconstruct_rep(_, _, _)
            ; AtomicGoalRep = partial_construct_rep(_, _, _)
            ; AtomicGoalRep = unify_assign_rep(_, _)
            ; AtomicGoalRep = cast_rep(_, _)
            ; AtomicGoalRep = unify_simple_test_rep(_, _)
            % Built-in calls are cheap enough to consider to be trivial.
            ; AtomicGoalRep = builtin_call_rep(_, _, _)
            ; AtomicGoalRep = pragma_foreign_code_rep(_)
            ; AtomicGoalRep = event_call_rep(_, _)
            ),
            IsTrivial = yes
        )
    ).

%----------------------------------------------------------------------------%
%
% Coverage information helper predicates.
%

    % The coverage before a det goal should always equal the coverage after.
    %
:- pred propagate_det_coverage(goal_path::in,
    coverage_before::in, coverage_before::out,
    coverage_after::in, coverage_after::out) is det.

propagate_det_coverage(GoalPath, !Before, !After) :-
    (
        !.Before = before_unknown,
        !.After = after_unknown
    ;
        !.Before = before_known(BeforeExecCount),
        !.After = after_known(AfterExecCount),
        trace [compile_time(not flag("no_coverage_propagation_assertions"))] (
            require(unify(BeforeExecCount, AfterExecCount),
                format("Coverage before %d /= after %d for a det goal: GP: %s",
                    [i(BeforeExecCount), i(AfterExecCount),
                    s(goal_path_to_string(GoalPath))]))
        )
    ;
        !.Before = before_known(BeforeExecCount),
        !.After = after_unknown,
        !:After = after_known(BeforeExecCount)
    ;
        !.Before = before_unknown,
        !.After = after_known(AfterExecCount),
        !:Before = before_known(AfterExecCount)
    ).

    % If the determinism is deterministic or cc_multi use
    % propagate_det_coverage.
    %
    % Note: This predicate must not be called on deterministic call goals,
    % since the coverage after the call may be different to the coverage before
    % if the called code throws an exception.
    %
:- pred propagate_detism_coverage(detism_rep::in, goal_path::in,
    coverage_before::in, coverage_before::out,
    coverage_after::in, coverage_after::out) is det.

propagate_detism_coverage(Detism, GoalPath, !Before, !After) :-
    % TODO: Infer that if a goal has a coverage of exactly 0 before it, then it
    % must have a coverage of exactly 0 after it.  And that a goal that cannot
    % fail or throw an exception that has a coverage of 0 after it, must have a
    % coverage of 0 before it - Since the coverage profiling and propagation
    % algorithms are already complete this isn't required.  It should be
    % considered if we choose not to calculate coverage for trivial goals.
    (
        ( Detism = det_rep
        ; Detism = cc_multidet_rep
        ),
        propagate_det_coverage(GoalPath, !Before, !After)
    ;
        ( Detism = erroneous_rep
        ; Detism = failure_rep
        ),
        % Execution never reaches the end of these goals.
        !:After = after_known(0)
    ;
        ( Detism = semidet_rep
        ; Detism = nondet_rep
        ; Detism = multidet_rep
        ; Detism = cc_nondet_rep
        )
        % We can infer nothing for goals with these determinisms.
    ).

:- pred after_to_before_coverage(coverage_after::in, coverage_before::out)
    is det.

after_to_before_coverage(After, Before) :-
    (
        After = after_unknown,
        Before = before_unknown
    ;
        After = after_known(ExecCount),
        Before = before_known(ExecCount)
    ).

:- pred after_count_from_either_source(coverage_after::in,
    coverage_after::in, coverage_after::out) is det.

after_count_from_either_source(AfterA, AfterB, After) :-
    (
        AfterA = after_unknown,
        AfterB = after_unknown,
        After = after_unknown
    ;
        AfterA = after_unknown,
        AfterB = after_known(AfterCount),
        After = after_known(AfterCount)
    ;
        AfterA = after_known(AfterCount),
        AfterB = after_unknown,
        After = after_known(AfterCount)
    ;
        AfterA = after_known(AfterCountA),
        AfterB = after_known(AfterCountB),
        require(unify(AfterCountA, AfterCountB),
            "after_count_from_either_source: mismatch"),
        After = after_known(AfterCountA)
    ).

:- pred after_count_from_either_source_sum(sum_coverage_after::in,
    coverage_after::in, coverage_after::out) is det.

after_count_from_either_source_sum(AfterA, AfterB, After) :-
    (
        AfterA = sum_after_unknown,
        AfterB = after_unknown,
        After = after_unknown
    ;
        AfterA = sum_after_unknown,
        AfterB = after_known(AfterCount),
        After = after_known(AfterCount)
    ;
        AfterA = sum_after_known(AfterCount),
        AfterB = after_unknown,
        After = after_known(AfterCount)
    ;
        AfterA = sum_after_known(AfterCountA),
        AfterB = after_known(AfterCountB),
        require(unify(AfterCountA, AfterCountB),
            "after_count_from_either_source: mismatch"),
        After = after_known(AfterCountA)
    ).

:- pred before_count_from_either_source(coverage_before::in,
    coverage_before::in, coverage_before::out) is det.

before_count_from_either_source(BeforeA, BeforeB, Before) :-
    (
        BeforeA = before_unknown,
        BeforeB = before_unknown,
        Before = before_unknown
    ;
        BeforeA = before_unknown,
        BeforeB = before_known(BeforeCount),
        Before = before_known(BeforeCount)
    ;
        BeforeA = before_known(BeforeCount),
        BeforeB = before_unknown,
        Before = before_known(BeforeCount)
    ;
        BeforeA = before_known(BeforeCountA),
        BeforeB = before_known(BeforeCountB),
        require(unify(BeforeCountA, BeforeCountB),
            "before_count_from_either_source: mismatch"),
        Before = before_known(BeforeCountA)
    ).

:- pred before_count_from_either_source_sum(sum_coverage_before::in,
    coverage_before::in, coverage_before::out) is det.

before_count_from_either_source_sum(BeforeA, BeforeB, Before) :-
    (
        BeforeA = sum_before_unknown,
        BeforeB = before_unknown,
        Before = before_unknown
    ;
        BeforeA = sum_before_unknown,
        BeforeB = before_known(BeforeCount),
        Before = before_known(BeforeCount)
    ;
        BeforeA = sum_before_known(BeforeCount),
        BeforeB = before_unknown,
        Before = before_known(BeforeCount)
    ;
        BeforeA = sum_before_known(BeforeCountA),
        BeforeB = before_known(BeforeCountB),
        require(unify(BeforeCountA, BeforeCountB),
            "before_count_from_either_source: mismatch"),
        Before = before_known(BeforeCountA)
    ).

:- pred sum_before_coverage(coverage_before::in,
    sum_coverage_before::in, sum_coverage_before::out) is det.

sum_before_coverage(Before, !SumBefore) :-
    (
        !.SumBefore = sum_before_known(SumExecCount),
        Before = before_known(ExecCount)
    ->
        !:SumBefore = sum_before_known(SumExecCount + ExecCount)
    ;
        !:SumBefore = sum_before_unknown
    ).

:- pred sum_after_coverage(coverage_after::in,
    sum_coverage_after::in, sum_coverage_after::out) is det.

sum_after_coverage(After, !SumAfter) :-
    (
        !.SumAfter = sum_after_known(SumExecCount),
        After = after_known(ExecCount)
    ->
        !:SumAfter = sum_after_known(SumExecCount + ExecCount)
    ;
        !:SumAfter = sum_after_unknown
    ).

%----------------------------------------------------------------------------%

:- type solution_count_rep
    --->    at_most_zero_rep
    ;       at_most_one_rep   % Including committed choice.
    ;       at_most_many_rep.

:- func detism_get_solutions(detism_rep) = solution_count_rep.

detism_get_solutions(det_rep) =         at_most_one_rep.
detism_get_solutions(semidet_rep) =     at_most_one_rep.
detism_get_solutions(multidet_rep) =    at_most_many_rep.
detism_get_solutions(nondet_rep) =      at_most_many_rep.
detism_get_solutions(cc_multidet_rep) = at_most_one_rep.
detism_get_solutions(cc_nondet_rep) =   at_most_one_rep.
detism_get_solutions(erroneous_rep) =   at_most_zero_rep.
detism_get_solutions(failure_rep) =     at_most_zero_rep.

:- type can_fail_rep
    --->    can_fail_rep
    ;       cannot_fail_rep.

:- func detism_get_can_fail(detism_rep) = can_fail_rep.

detism_get_can_fail(det_rep) =          cannot_fail_rep.
detism_get_can_fail(semidet_rep) =      can_fail_rep.
detism_get_can_fail(multidet_rep) =     cannot_fail_rep.
detism_get_can_fail(nondet_rep) =       can_fail_rep.
detism_get_can_fail(cc_multidet_rep) =  cannot_fail_rep.
detism_get_can_fail(cc_nondet_rep) =    can_fail_rep.
detism_get_can_fail(erroneous_rep) =    cannot_fail_rep.
detism_get_can_fail(failure_rep) =      can_fail_rep.

%----------------------------------------------------------------------------%

