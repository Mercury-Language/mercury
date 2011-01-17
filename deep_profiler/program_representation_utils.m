%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2008, 2010-2011 The University of Melbourne.
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
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.program_representation.

:- import_module cord.
:- import_module list.
:- import_module set.
:- import_module string.
:- import_module unit.

%----------------------------------------------------------------------------%

    % Ugly-print a module to a string representation. We return a cord of
    % strings is returned rather than a string, since this reduces the cost
    % of string concatenations.
    %
:- pred print_module_to_strings(module_rep::in, cord(string)::out) is det.

    % Print a procedure to a string representation using a higher order value
    % to lookup goal attributes.
    %
:- pred print_proc_to_strings(func(goal_id) = GoalAnn, proc_rep(goal_id),
    cord(string)) <= goal_annotation(GoalAnn).
:- mode print_proc_to_strings(func(in) = out is det, in, out) is det.

    % Print a procedure to a string representation.
    %
:- pred print_proc_to_strings(proc_rep(GoalAnn)::in, cord(string)::out) is det
    <= goal_annotation(GoalAnn).

    % Print a proc label to a string.
    %
:- pred print_proc_label_to_string(string_proc_label::in, string::out) is det.

:- type print_goal_info(Key, GoalAnn)
    --->    print_goal_info(
                pgi_lookup_annotation       :: (func(Key) = GoalAnn),
                pgi_var_table               :: var_table
            ).

    % print_goal_to_strings(Lookup, VarTable, Indent, RevGoalPathSteps, Goal,
    %   Strings):
    %
    % Print a goal (recursively) to a string representation.
    %
:- pred print_goal_to_strings(print_goal_info(T, GoalAnn)::in, int::in,
    list(goal_path_step)::in, goal_rep(T)::in, cord(string)::out) is det
    <= goal_annotation(GoalAnn).

%----------------------------------------------------------------------------%

:- typeclass goal_annotation(T) where [
    % Print the goal annotation for inclusion by print_proc_to_strings
    % above.
    %
    pred print_goal_annotation_to_strings(var_table::in, T::in,
        cord(cord(string))::out) is det
].

    % A goal with no particular annotation has empty strings printed for goal
    % annotations.
    %
:- instance goal_annotation(unit).

%----------------------------------------------------------------------------%

    % Goal IDs are a more efficient way to identify goals than goal paths.
    %
    % The allow annotations to be kept in an array indexed by the goal id.
    %
:- pred label_goals(goal_id::out, containing_goal_map::out,
    goal_rep(T)::in, goal_rep(goal_id)::out) is det.

%----------------------------------------------------------------------------%

    % Search a program representation for the given procedure and return its
    % procedure representation if found, otherwise fail.
    %
:- pred progrep_search_proc(prog_rep::in, string_proc_label::in, proc_rep::out)
    is semidet.

%----------------------------------------------------------------------------%

    % A map of variables to instantiation states,  Like inst_map within the
    % compiler.
    %
:- type inst_map.

    % Build the initial inst for a procedure.
    %
:- func initial_inst_map(proc_defn_rep(T)) = inst_map.

    % inst_map_ground_vars(Vars, DepVars, !InstMap, SeenDuplicateInstantiaton).
    %
    % Make the variables in the given list ground in the new copy of the inst
    % map.
    %
    % DepVars is the set of variables that each of the Vars directly depends
    % upon.
    %
    % SeenDuplicateInstantiation will be true iff at least one of these
    % variables is already ground.
    %
:- pred inst_map_ground_vars(list(var_rep)::in, set(var_rep)::in, inst_map::in,
    inst_map::out, seen_duplicate_instantiation::out) is det.

    % This type represents whether a traversal has seen more than one
    % instantiation of a variable within a single branch.  If at the end of a
    % traversal a duplicate instantiation has been seen we can either accept a
    % pessimistic default or abort parallelisation of this particular
    % conjunction.
    %
:- type seen_duplicate_instantiation
    --->    seen_duplicate_instantiation
    ;       have_not_seen_duplicate_instantiation.

:- func merge_seen_duplicate_instantiation(seen_duplicate_instantiation,
    seen_duplicate_instantiation) = seen_duplicate_instantiation.

    % Retrieve the instantiatedness of a variable, and variables that it's
    % binding depends upon from the instmap, if the variable is new ir_free is
    % returned and the variables it depends upon is the empty set.
    %
:- pred inst_map_get(inst_map::in, var_rep::in, inst_rep::out,
    set(var_rep)::out) is det.

    % Merge two inst maps from different branches of execution.
    %
:- func merge_inst_map(inst_map, detism_rep, inst_map, detism_rep) = inst_map.

    % Retrieve all the variables this variable depends upon,  indirect
    % dependencies are also returned.
    %
:- pred inst_map_get_var_deps(inst_map::in, var_rep::in, set(var_rep)::out)
    is det.

%----------------------------------------------------------------------------%

    % A difference between too inst maps.  This lists the variables that are
    % instantiated by a particular goal.
    %
:- type inst_map_delta.

    % Get the set of variables that are instantiated by this inst map.
    %
:- pred inst_map_delta_get_var_set(inst_map_delta::in, set(var_rep)::out)
    is det.

    % The empty inst_map_delta.  Nothing is instantiated.
    %
:- pred empty_inst_map_delta(inst_map_delta::out) is det.
:- func empty_inst_map_delta = inst_map_delta.

    % calc_inst_map_delta(InstMapBefore, InstMapAfter, InstMapDelta)
    %
    % Calculate the difference between two inst maps.
    %
    % InstMapAfter is InstMapBefore after the variables in InstMapDelta have
    % been instantiated.
    %
:- pred calc_inst_map_delta(inst_map::in, inst_map::in, inst_map_delta::out)
    is det.

%----------------------------------------------------------------------------%

    % Retrieve a set of all the vars involved with this atomic goal.
    %
:- pred atomic_goal_get_vars(atomic_goal_rep::in, set(var_rep)::out) is det.

%----------------------------------------------------------------------------%

:- type atomic_goal_is_call
    --->    atomic_goal_is_call(list(var_rep))
    ;       atomic_goal_is_trivial.

:- pred atomic_goal_is_call(atomic_goal_rep::in, atomic_goal_is_call::out)
    is det.

%----------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.prim_data.

:- import_module array.
:- import_module bool.
:- import_module counter.
:- import_module int.
:- import_module io.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module std_util.
:- import_module svmap.

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

print_proc_to_strings(Lookup, ProcRep, Strings) :-
    print_proc_to_strings_2(Lookup, ProcRep, Strings).

print_proc_to_strings(ProcRep, Strings) :-
    print_proc_to_strings_2(id, ProcRep, Strings).

:- pred print_proc_to_strings_2(func(X) = GoalAnn, proc_rep(X),
    cord(string)) <= goal_annotation(GoalAnn).
:- mode print_proc_to_strings_2(func(in) = out is det, in, out) is det.

print_proc_to_strings_2(Lookup, ProcRep, Strings) :-
    ProcRep = proc_rep(ProcLabel, ProcDefnRep),
    ProcDefnRep = proc_defn_rep(ArgVarReps, GoalRep, VarTable, Detism),
    print_proc_label_to_string(ProcLabel, ProcLabelString0),
    detism_to_string(Detism, DetismString),
    ProcLabelString = DetismString ++ cord.singleton(" ") ++
        cord.singleton(ProcLabelString0),
    print_args_to_strings(print_head_var, VarTable, ArgVarReps, ArgsString),
    print_goal_to_strings(print_goal_info(Lookup, VarTable), 1, [], GoalRep,
        GoalString),
    Strings = ProcLabelString ++ ArgsString ++ cord.singleton(" :-\n") ++
        GoalString ++ nl.

print_proc_label_to_string(ProcLabel, String) :-
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
        string.format("%s %s.%s/%d-%d",
            [s(PF), s(DeclModule), s(Name), i(Arity), i(Mode)], String)
    ;
        ProcLabel = str_special_proc_label(TypeName, TypeModule, _DefModule,
            Name, Arity, Mode),
        string.format("%s for %s.%s/%d-%d",
            [s(Name), s(TypeModule), s(TypeName), i(Arity), i(Mode)], String)
    ).

%-----------------------------------------------------------------------------%

print_goal_to_strings(Info, Indent, RevGoalPathSteps, GoalRep, Strings) :-
    GoalRep = goal_rep(GoalExprRep, DetismRep, AnnotationKey),
    VarTable = Info ^ pgi_var_table,
    (
        GoalExprRep = conj_rep(ConjGoalReps),
        print_conj_to_strings(Info, Indent, RevGoalPathSteps,
            ConjGoalReps, ExprString)
    ;
        GoalExprRep = disj_rep(DisjGoalReps),
        print_disj_to_strings(Info, Indent, RevGoalPathSteps, 1, DisjGoalReps,
            no, DisjString),
        ExprString = indent(Indent) ++
            cord.singleton("(\n") ++ DisjString ++ indent(Indent) ++
            cord.singleton(")\n")
    ;
        GoalExprRep = switch_rep(SwitchVarRep, CanFail, CasesRep),
        lookup_var_name(VarTable, SwitchVarRep, SwitchVarName),
        string.format("%s switch on %s\n",
            [s(string(CanFail)), s(SwitchVarName)], SwitchOnString),
        print_switch_to_strings(Info, Indent + 1, RevGoalPathSteps, 1,
            CasesRep, no, SwitchString),
        ExprString = indent(Indent) ++ cord.singleton(SwitchOnString) ++
            indent(Indent) ++ cord.singleton("(\n") ++ SwitchString ++
            indent(Indent) ++ cord.singleton(")\n")
    ;
        GoalExprRep = ite_rep(CondRep, ThenRep, ElseRep),
        RevGoalPathStepsCond = [step_ite_cond | RevGoalPathSteps],
        RevGoalPathStepsThen = [step_ite_then | RevGoalPathSteps],
        RevGoalPathStepsElse = [step_ite_else | RevGoalPathSteps],
        print_goal_to_strings(Info, Indent + 1, RevGoalPathStepsCond,
            CondRep, CondString),
        print_goal_to_strings(Info, Indent + 1, RevGoalPathStepsThen,
            ThenRep, ThenString),
        print_goal_to_strings(Info, Indent + 1, RevGoalPathStepsElse,
            ElseRep, ElseString),
        IndentString = indent(Indent),
        ExprString = IndentString ++ cord.singleton("(\n") ++ CondString ++
            IndentString ++ cord.singleton("->\n") ++ ThenString ++
            IndentString ++ cord.singleton(";\n") ++ ElseString ++
            IndentString ++ cord.singleton(")\n")
    ;
        GoalExprRep = negation_rep(SubGoalRep),
        RevSubGoalPathSteps = [step_neg | RevGoalPathSteps],
        print_goal_to_strings(Info, Indent + 1, RevSubGoalPathSteps,
            SubGoalRep, SubGoalString),
        ExprString = indent(Indent) ++ cord.singleton("not (\n") ++
            SubGoalString ++ indent(Indent) ++ cord.singleton(")\n")
    ;
        GoalExprRep = scope_rep(SubGoalRep, MaybeCut),
        (
            MaybeCut = scope_is_cut,
            CutString = cord.singleton(" cut")
        ;
            MaybeCut = scope_is_no_cut,
            CutString = cord.empty
        ),
        RevSubGoalPathSteps = [step_scope(MaybeCut) | RevGoalPathSteps],
        print_goal_to_strings(Info, Indent + 1, RevSubGoalPathSteps,
            SubGoalRep, SubGoalString),
        ExprString = indent(Indent) ++ cord.singleton("scope") ++ CutString ++
            cord.singleton(" (\n") ++
            SubGoalString ++ indent(Indent) ++ cord.singleton(")\n")
    ;
        GoalExprRep = atomic_goal_rep(_FileName, _LineNumber,
            _BoundVars, AtomicGoalRep),
        print_atomic_goal_to_strings(VarTable, AtomicGoalRep, ExprString0),
        ExprString = indent(Indent) ++ ExprString0
    ),

    ( GoalExprRep = conj_rep(_) ->
        LinePrefix = indent(Indent) ++ singleton("% conjunction: "),
        ExtraLineForConjunctions = nl
    ;
        LinePrefix = indent(Indent) ++ singleton("% "),
        ExtraLineForConjunctions = empty
    ),
    detism_to_string(DetismRep, DetismString),
    DetismLine = LinePrefix ++ DetismString ++ nl,
    LookupAnnotation = Info ^ pgi_lookup_annotation,
    GoalAnnotation = LookupAnnotation(AnnotationKey),
    print_goal_annotation_to_strings(VarTable, GoalAnnotation,
        GoalAnnotationLines0),
    ( is_empty(GoalAnnotationLines0) ->
        GoalAnnotationLines = empty
    ;
        GoalAnnotationLines1 = map((func(Line) = LinePrefix ++ Line ++ nl),
            GoalAnnotationLines0),
        GoalAnnotationLines = foldr(++, GoalAnnotationLines1, empty)
    ),

    GoalPathString0 = rev_goal_path_to_string(rgp(RevGoalPathSteps)),
    ( GoalPathString0 = "" ->
        GoalPathString = "root goal"
    ;
        GoalPathString = GoalPathString0
    ),
    GoalPathLine = LinePrefix ++ cord.singleton(GoalPathString) ++ nl,

    Strings = GoalPathLine
        ++ DetismLine
        ++ GoalAnnotationLines
        ++ ExtraLineForConjunctions
        ++ ExprString.

:- pred print_conj_to_strings(print_goal_info(T, GoalAnn)::in, int::in,
    list(goal_path_step)::in, list(goal_rep(T))::in,
    cord(string)::out) is det
    <= goal_annotation(GoalAnn).

print_conj_to_strings(Info, Indent, RevGoalPathSteps, GoalReps, Strings) :-
    (
        GoalReps = [],
        Strings = cord.snoc(indent(Indent), "true\n")
    ;
        GoalReps = [_ | _],
        print_conj_to_strings_2(Info, Indent, RevGoalPathSteps, 1, GoalReps,
            Strings)
    ).

:- pred print_conj_to_strings_2(print_goal_info(T, GoalAnn)::in, int::in,
    list(goal_path_step)::in, int::in, list(goal_rep(T))::in,
    cord(string)::out)
    is det <= goal_annotation(GoalAnn).

print_conj_to_strings_2(_, _Indent, _, _, [], cord.empty).
print_conj_to_strings_2(Info, Indent, RevGoalPathSteps, ConjNum,
        [GoalRep | GoalReps], Strings) :-
    % We use the absence of a separator to denote conjunction.
    %
    % We could try to append the comma at the end of each goal that is
    % not last in a conjunction, but that would be significant work,
    % and (at least for now) there is no real need for it.
    RevSubGoalPathSteps = [step_conj(ConjNum) | RevGoalPathSteps],
    print_goal_to_strings(Info, Indent, RevSubGoalPathSteps, GoalRep,
        HeadGoalString),
    print_conj_to_strings_2(Info, Indent, RevGoalPathSteps, ConjNum + 1,
        GoalReps, TailGoalsString),
    (
        GoalReps = [],
        Separator = empty
    ;
        GoalReps = [_ | _],
        Separator = indent(Indent) ++ singleton(",\n")
    ),
    Strings = HeadGoalString ++ Separator ++ TailGoalsString.

:- pred print_disj_to_strings(print_goal_info(T, GoalAnn)::in, int::in,
    list(goal_path_step)::in, int::in, list(goal_rep(T))::in, bool::in,
    cord(string)::out)
    is det <= goal_annotation(GoalAnn).

print_disj_to_strings(_, _Indent, _, _, [], _PrintSemi, cord.empty).
print_disj_to_strings(Info, Indent, RevGoalPathSteps, DisjNum,
        [GoalRep | GoalReps], PrintSemi, Strings) :-
    (
        PrintSemi = no,
        DelimString = cord.empty
    ;
        PrintSemi = yes,
        DelimString = indent(Indent) ++ cord.singleton(";\n")
    ),
    RevSubGoalPathSteps = [step_disj(DisjNum) | RevGoalPathSteps],
    print_goal_to_strings(Info, Indent + 1, RevSubGoalPathSteps, GoalRep,
        HeadGoalString),
    print_disj_to_strings(Info, Indent, RevGoalPathSteps, DisjNum + 1,
        GoalReps, yes, TailGoalsString),
    Strings = DelimString ++ HeadGoalString ++ TailGoalsString.

:- pred print_switch_to_strings(print_goal_info(T, GoalAnn)::in, int::in,
    list(goal_path_step)::in, int::in, list(case_rep(T))::in, bool::in,
    cord(string)::out) is det
    <= goal_annotation(GoalAnn).

print_switch_to_strings(_, _Indent, _, _, [], _PrintSemi, cord.empty).
print_switch_to_strings(Info, Indent, RevGoalPathSteps, CaseNum,
        [CaseRep | CaseReps], PrintSemi, Strings) :-
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
    RevSubGoalPathSteps = [step_switch(CaseNum, no) | RevGoalPathSteps],
    print_goal_to_strings(Info, Indent + 1, RevSubGoalPathSteps,
        GoalRep, HeadGoalString),
    print_switch_to_strings(Info, Indent, RevGoalPathSteps, CaseNum + 1,
        CaseReps, yes, TailCasesStrings),
    Strings = DelimString ++ ConsIdArityString ++
        cord_list_to_cord(OtherConsIdArityStrings) ++ HeadGoalString ++
        TailCasesStrings.

:- pred print_cons_id_and_arity_to_strings(int::in, cons_id_arity_rep::in,
    cord(string)::out) is det.

print_cons_id_and_arity_to_strings(Indent, ConsIdArityRep, Strings) :-
    ConsIdArityRep = cons_id_arity_rep(ConsIdRep, Arity),
    string.format("%% case %s/%d\n", [s(ConsIdRep), i(Arity)], String),
    Strings = cord.snoc(indent(Indent), String).

%-----------------------------------------------------------------------------%

:- pred print_atomic_goal_to_strings(var_table::in, atomic_goal_rep::in,
    cord(string)::out) is det.

print_atomic_goal_to_strings(VarTable, AtomicGoalRep, Strings) :-
    (
        (
            AtomicGoalRep = unify_construct_rep(VarRep, ConsIdRep, ArgReps),
            UnifyOp = "<="
        ;
            AtomicGoalRep = unify_deconstruct_rep(VarRep, ConsIdRep, ArgReps),
            UnifyOp = "=>"
        ),
        lookup_var_name(VarTable, VarRep, VarName),
        string.format("%s %s %s", [s(VarName), s(UnifyOp), s(ConsIdRep)],
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
        string.format("%s %s %s", [s(VarName), s(UnifyOp), s(ConsIdRep)],
            UnifyString),
        print_args_to_strings(print_maybe_var, VarTable, MaybeArgReps,
            ArgsString),
        Strings0 = cord.cons(UnifyString, ArgsString)
    ;
        AtomicGoalRep = unify_assign_rep(TargetRep, SourceRep),
        lookup_var_name(VarTable, TargetRep, TargetName),
        lookup_var_name(VarTable, SourceRep, SourceName),
        string.format("%s := %s", [s(TargetName), s(SourceName)], String),
        Strings0 = cord.singleton(String)
    ;
        AtomicGoalRep = cast_rep(TargetRep, SourceRep),
        lookup_var_name(VarTable, TargetRep, TargetName),
        lookup_var_name(VarTable, SourceRep, SourceName),
        string.format("cast %s to %s", [s(SourceName), s(TargetName)], String),
        Strings0 = cord.singleton(String)
    ;
        AtomicGoalRep = unify_simple_test_rep(TargetRep, SourceRep),
        lookup_var_name(VarTable, TargetRep, TargetName),
        lookup_var_name(VarTable, SourceRep, SourceName),
        string.format("%s == %s", [s(SourceName), s(TargetName)], String),
        Strings0 = cord.singleton(String)
    ;
        AtomicGoalRep = pragma_foreign_code_rep(Args),
        print_args_to_strings(lookup_var_name, VarTable, Args, ArgsString),
        Strings0 = cord.singleton("foreign_proc(") ++ ArgsString ++
            cord.singleton(")")
    ;
        AtomicGoalRep = higher_order_call_rep(HOVarRep, Args),
        lookup_var_name(VarTable, HOVarRep, HOVarName),
        string.format("%s(", [s(HOVarName)], HeadString),
        print_args_to_strings(lookup_var_name, VarTable, Args, ArgsString),
        Strings0 = cord.singleton(HeadString) ++ ArgsString ++
            cord.singleton(")")
    ;
        AtomicGoalRep = method_call_rep(TCIVarRep, MethodNumber, Args),
        lookup_var_name(VarTable, TCIVarRep, TCIVarName),
        string.format("method %d of %s(", [i(MethodNumber), s(TCIVarName)],
            HeadString),
        print_args_to_strings(lookup_var_name, VarTable, Args, ArgsString),
        Strings0 = cord.singleton(HeadString) ++ ArgsString ++
            cord.singleton(")")
    ;
        AtomicGoalRep = plain_call_rep(Module, Pred, Args),
        string.format("%s.%s", [s(Module), s(Pred)], HeadString),
        print_args_to_strings(lookup_var_name, VarTable, Args, ArgsString),
        Strings0 = cord.cons(HeadString, ArgsString)
    ;
        AtomicGoalRep = builtin_call_rep(Module, Pred, Args),
        string.format("builtin %s.%s", [s(Module), s(Pred)], HeadString),
        print_args_to_strings(lookup_var_name, VarTable, Args, ArgsString),
        Strings0 = cord.cons(HeadString, ArgsString)
    ;
        AtomicGoalRep = event_call_rep(Event, Args),
        string.format("event %s", [s(Event)], HeadString),
        print_args_to_strings(lookup_var_name, VarTable, Args, ArgsString),
        Strings0 = cord.cons(HeadString, ArgsString)
    ),
    Strings = Strings0 ++ nl.

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

:- func nl_indent(int) = cord(string).

nl_indent(N) = nl ++ indent(N).

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
    pred(print_goal_annotation_to_strings/3) is print_unit_to_strings
].

:- pred print_unit_to_strings(var_table::in, unit::in, cord(cord(string))::out)
    is det.

print_unit_to_strings(_, _, cord.empty).

%----------------------------------------------------------------------------%

progrep_search_proc(ProgRep, ProcLabel, ProcRep) :-
    ( ProcLabel = str_ordinary_proc_label(_, _DeclModule, DefModule, _, _, _)
    ; ProcLabel = str_special_proc_label(_, _DeclModule, DefModule, _, _, _)
    ),
    progrep_search_module(ProgRep, DefModule, ModuleRep),
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
%
% Label goals with IDs.
%

label_goals(goal_id(LastIdPlus1 - 1), Map, !Goal) :-
    label_goal(whole_body_goal, !Goal, counter.init(0), Counter,
        map.init, Map),
    allocate(LastIdPlus1, Counter, _).

:- pred label_goal(containing_goal::in,
    goal_rep(T)::in, goal_rep(goal_id)::out, counter::in, counter::out,
    map(goal_id, containing_goal)::in, map(goal_id, containing_goal)::out)
    is det.

label_goal(ContainingGoal, !Goal, !Counter, !Map) :-
    !.Goal = goal_rep(GoalExpr0, Detism, _),
    allocate(GoalIdNum, !Counter),
    GoalId = goal_id(GoalIdNum),
    svmap.det_insert(GoalId, ContainingGoal, !Map),
    (
        GoalExpr0 = conj_rep(Conjs0),
        map_foldl3(label_goal_wrapper((func(N) = step_conj(N)), GoalId),
            Conjs0, Conjs, 1, _, !Counter, !Map),
        GoalExpr = conj_rep(Conjs)
    ;
        GoalExpr0 = disj_rep(Disjs0),
        map_foldl3(label_goal_wrapper((func(N) = step_conj(N)), GoalId),
            Disjs0, Disjs, 1, _, !Counter, !Map),
        GoalExpr = disj_rep(Disjs)
    ;
        GoalExpr0 = switch_rep(Var, CanFail, Cases0),
        map_foldl3(label_case(GoalId), Cases0, Cases, 1, _, !Counter, !Map),
        GoalExpr = switch_rep(Var, CanFail, Cases)
    ;
        GoalExpr0 = ite_rep(Cond0, Then0, Else0),
        label_goal(containing_goal(GoalId, step_ite_cond), Cond0, Cond,
            !Counter, !Map),
        label_goal(containing_goal(GoalId, step_ite_then), Then0, Then,
            !Counter, !Map),
        label_goal(containing_goal(GoalId, step_ite_else), Else0, Else,
            !Counter, !Map),
        GoalExpr = ite_rep(Cond, Then, Else)
    ;
        GoalExpr0 = negation_rep(SubGoal0),
        label_goal(containing_goal(GoalId, step_neg), SubGoal0, SubGoal,
            !Counter, !Map),
        GoalExpr = negation_rep(SubGoal)
    ;
        GoalExpr0 = scope_rep(SubGoal0, ScopeIsCut),
        label_goal(containing_goal(GoalId, step_scope(ScopeIsCut)),
            SubGoal0, SubGoal, !Counter, !Map),
        GoalExpr = scope_rep(SubGoal, ScopeIsCut)
    ;
        GoalExpr0 = atomic_goal_rep(File, Line, BoundVars, AtomicGoal),
        GoalExpr = atomic_goal_rep(File, Line, BoundVars, AtomicGoal)
    ),
    !:Goal = goal_rep(GoalExpr, Detism, GoalId).

:- pred label_goal_wrapper(func(int) = goal_path_step, goal_id,
    goal_rep(T), goal_rep(goal_id), int, int, counter, counter,
    map(goal_id, containing_goal), map(goal_id, containing_goal)) is det.
:- mode label_goal_wrapper(func(in) = out is det, in,
    in, out, in, out, in, out, in, out) is det.

label_goal_wrapper(MakePathStep, ParentGoalId, !Goal, !GoalNum, !Counter,
        !Map) :-
    label_goal(containing_goal(ParentGoalId, MakePathStep(!.GoalNum)),
        !Goal, !Counter, !Map),
    !:GoalNum = !.GoalNum + 1.

:- pred label_case(goal_id::in, case_rep(T)::in, case_rep(goal_id)::out,
    int::in, int::out, counter::in, counter::out,
    containing_goal_map::in, containing_goal_map::out) is det.

label_case(ParentGoalId, !Case, !CaseNum, !Counter, !Map) :-
    !.Case = case_rep(MainConsId, OtherConsIds, Goal0),
    label_goal_wrapper((func(N) = step_switch(N, no)), ParentGoalId,
            Goal0, Goal, !CaseNum, !Counter, !Map),
    !:Case = case_rep(MainConsId, OtherConsIds, Goal).

%----------------------------------------------------------------------------%

:- type inst_map
    --->    inst_map(
                im_inst_map         :: map(var_rep, inst_rep),
                    % The actual inst map.

                im_var_dep_map      :: map(var_rep, set(var_rep))
                    % A tree describing dependencies between bound variables.
            ).

initial_inst_map(ProcDefn) = InstMap :-
    HeadVars = ProcDefn ^ pdr_head_vars,
    list.foldl(add_head_var_inst_to_map, HeadVars,
        inst_map(map.init, map.init), InstMap).

:- pred add_head_var_inst_to_map(head_var_rep::in,
    inst_map::in, inst_map::out) is det.

add_head_var_inst_to_map(head_var_rep(VarRep, ModeRep), !InstMap) :-
    ModeRep = var_mode_rep(InitialInstRep, _),
    add_inst_mapping(VarRep, InitialInstRep, set.init, !InstMap).

    % Add an inst mapping.
    %
:- pred add_inst_mapping(var_rep::in, inst_rep::in, set(var_rep)::in,
    inst_map::in, inst_map::out) is det.

add_inst_mapping(VarRep, InstRep, DepVars, InstMap0, InstMap) :-
    InstMap0 = inst_map(VarToInst0, VarToDepVars0),
    map.det_insert(VarToInst0, VarRep, InstRep, VarToInst),
    map.det_insert(VarToDepVars0, VarRep, DepVars, VarToDepVars),
    InstMap = inst_map(VarToInst, VarToDepVars).

merge_inst_map(InstMapA, DetismA, InstMapB, DetismB) = InstMap :-
    (
        ( DetismA = erroneous_rep
        ; DetismA = failure_rep
        ),
        InstMap = InstMapB
    ;
        ( DetismA = det_rep
        ; DetismA = semidet_rep
        ; DetismA = nondet_rep
        ; DetismA = multidet_rep
        ; DetismA = cc_nondet_rep
        ; DetismA = cc_multidet_rep
        ),
        (
            ( DetismB = erroneous_rep
            ; DetismB = failure_rep
            ),
            InstMap = InstMapA
        ;
            ( DetismB = det_rep
            ; DetismB = semidet_rep
            ; DetismB = nondet_rep
            ; DetismB = multidet_rep
            ; DetismB = cc_nondet_rep
            ; DetismB = cc_multidet_rep
            ),
            InstMapA = inst_map(VarToInstA, VarToDepVarsA),
            InstMapB = inst_map(VarToInstB, VarToDepVarsB),
            map.union(inst_intersect, VarToInstA, VarToInstB, VarToInst),
            map.union(set.union, VarToDepVarsA, VarToDepVarsB, VarToDepVars),
            InstMap = inst_map(VarToInst, VarToDepVars)
        )
    ).

:- pred inst_intersect(inst_rep::in, inst_rep::in, inst_rep::out) is det.

inst_intersect(ir_free_rep,     ir_free_rep,    ir_free_rep).
inst_intersect(ir_free_rep,     ir_ground_rep,  ir_other_rep).
inst_intersect(ir_free_rep,     ir_other_rep,   ir_other_rep).
inst_intersect(ir_ground_rep,   ir_free_rep,    ir_other_rep).
inst_intersect(ir_ground_rep,   ir_ground_rep,  ir_ground_rep).
inst_intersect(ir_ground_rep,   ir_other_rep,   ir_other_rep).
inst_intersect(ir_other_rep,    ir_free_rep,    ir_other_rep).
inst_intersect(ir_other_rep,    ir_ground_rep,  ir_other_rep).
inst_intersect(ir_other_rep,    ir_other_rep,   ir_other_rep).

inst_map_ground_vars(Vars, DepVars, !InstMap, SeenDuplicateInstantiation) :-
    list.foldl2(inst_map_ground_var(DepVars), Vars, !InstMap,
        have_not_seen_duplicate_instantiation, SeenDuplicateInstantiation).

:- pred inst_map_ground_var(set(var_rep)::in, var_rep::in,
    inst_map::in, inst_map::out, seen_duplicate_instantiation::in,
    seen_duplicate_instantiation::out) is det.

inst_map_ground_var(DepVars0, Var, InstMap0, InstMap, !SeenDuplicateInstantiation) :-
    InstMap0 = inst_map(VarToInst0, VarToDepVars0),
    ( map.search(VarToInst0, Var, InstPrime) ->
        Inst = InstPrime
    ;
        Inst = ir_free_rep
    ),
    (
        Inst = ir_free_rep,
        NewInst = ir_ground_rep,
        DepVars = DepVars0
    ;
        ( Inst = ir_ground_rep
        ; Inst = ir_other_rep
        ),
        NewInst = ir_other_rep,
        map.lookup(VarToDepVars0, Var, DepVarsFromIM),
        DepVars = set.union(DepVars0,  DepVarsFromIM),
        !:SeenDuplicateInstantiation = seen_duplicate_instantiation
    ),
    map.set(VarToInst0, Var, NewInst, VarToInst),
    map.set(VarToDepVars0, Var, DepVars, VarToDepVars),
    InstMap = inst_map(VarToInst, VarToDepVars).

inst_map_get(inst_map(VarToInst, VarToDepVars), Var, Inst, DepVars) :-
    ( map.search(VarToInst, Var, InstPrime) ->
        Inst = InstPrime,
        map.lookup(VarToDepVars, Var, DepVars)
    ;
        Inst = ir_free_rep,
        DepVars = set.init
    ).

inst_map_get_var_deps(inst_map(_, VarToDepVars), VarRep, DepVars) :-
    inst_map_get_var_deps_2(VarToDepVars, VarRep, set.init, DepVars).

:- pred inst_map_get_var_deps_2(map(var_rep, set(var_rep))::in, var_rep::in,
    set(var_rep)::in, set(var_rep)::out) is det.

inst_map_get_var_deps_2(VarToDepVars, VarRep, !Set) :-
    ( set.contains(!.Set, VarRep) ->
        true
        % This variable has already been visited, this prevents following any
        % (impossible) cycles in the graph, or following the same path twice
        % when there are diamonds in the graph.
    ;
        ( map.search(VarToDepVars, VarRep, DepVars) ->
            !:Set = set.union(!.Set, DepVars),
            set.fold(inst_map_get_var_deps_2(VarToDepVars), DepVars, !Set)
        ;
            true
        )
    ).

%----------------------------------------------------------------------------%

:- type inst_map_delta
    --->    inst_map_delta(set(var_rep)).

inst_map_delta_get_var_set(inst_map_delta(Vars), Vars).

empty_inst_map_delta(inst_map_delta(Vars)) :-
    set.init(Vars).
empty_inst_map_delta = InstMap :-
    empty_inst_map_delta(InstMap).

calc_inst_map_delta(Before, After, inst_map_delta(DeltaVars)) :-
    map.foldl((pred(Var::in, Inst::in, Set0::in, Set::out) is det :-
            (
                map.search(Before ^ im_inst_map, Var, BeforeInst)
            ->
                (
                    BeforeInst = ir_free_rep,
                    (
                        Inst = ir_free_rep,
                        Set = Set0
                    ;
                        ( Inst = ir_ground_rep
                        ; Inst = ir_other_rep
                        ),
                        % This variable has become more instantiated.
                        set.insert(Set0, Var, Set)
                    )
                ;
                    BeforeInst = ir_ground_rep,
                    (
                        Inst = ir_free_rep,
                        error("calc_inst_map_delta: " ++
                            "Variables cannot become less instantiated.")
                    ;
                        ( Inst = ir_ground_rep
                        ; Inst = ir_other_rep
                        )
                    ),
                    Set = Set0
                ;
                    BeforeInst = ir_other_rep,
                    (
                        Inst = ir_free_rep,
                        error("calc_inst_map_delta: " ++
                            "Variables cannot become less instantiated.")
                    ;
                        ( Inst = ir_ground_rep
                        ; Inst = ir_other_rep
                        )
                    ),
                    Set = Set0
                )
            ;
                % If we couldn't find the variable then it was free, It may
                % have been in the head of the procedure.
                (
                    Inst = ir_free_rep,
                    Set = Set0
                ;
                    ( Inst = ir_ground_rep
                    ; Inst = ir_other_rep
                    ),
                    % This variable has become more instantiated.
                    set.insert(Set0, Var, Set)
                )
            )
        ), After ^ im_inst_map, set.init, DeltaVars).

%----------------------------------------------------------------------------%

atomic_goal_get_vars(AtomicGoal, Vars) :-
    (
        ( AtomicGoal = unify_construct_rep(Var, _, VarsL)
        ; AtomicGoal = unify_deconstruct_rep(Var, _, VarsL)
        ; AtomicGoal = higher_order_call_rep(Var, VarsL)
        ; AtomicGoal = method_call_rep(Var, _, VarsL)
        ),
        Vars0 = list_to_set(VarsL),
        set.insert(Vars0, Var, Vars)
    ;
        ( AtomicGoal = partial_construct_rep(Var, _, MaybeVars)
        ; AtomicGoal = partial_deconstruct_rep(Var, _, MaybeVars)
        ),
        list.foldl((pred(MaybeVar::in, Set0::in, Set::out) is det :-
                (
                    MaybeVar = yes(VarI),
                    set.insert(Set0, VarI, Set)
                ;
                    MaybeVar = no,
                    Set = Set0
                )), MaybeVars, set.init, Vars0),
        set.insert(Vars0, Var, Vars)
    ;
        ( AtomicGoal = unify_assign_rep(VarA, VarB)
        ; AtomicGoal = cast_rep(VarA, VarB)
        ; AtomicGoal = unify_simple_test_rep(VarA, VarB)
        ),
        Vars = list_to_set([ VarA, VarB ])
    ;
        ( AtomicGoal = pragma_foreign_code_rep(VarsL)
        ; AtomicGoal = event_call_rep(_, VarsL)
        ; AtomicGoal = builtin_call_rep(_, _, VarsL)
        ; AtomicGoal = plain_call_rep(_, _, VarsL)
        ),
        Vars = list_to_set(VarsL)
    ).

merge_seen_duplicate_instantiation(A, B) = R :-
    (
        A = have_not_seen_duplicate_instantiation,
        B = have_not_seen_duplicate_instantiation
    ->
        R = have_not_seen_duplicate_instantiation
    ;
        R = seen_duplicate_instantiation
    ).

%----------------------------------------------------------------------------%

atomic_goal_is_call(AtomicGoal, IsCall) :-
    (
        ( AtomicGoal = unify_construct_rep(_, _, _)
        ; AtomicGoal = unify_deconstruct_rep(_, _, _)
        ; AtomicGoal = partial_construct_rep(_, _, _)
        ; AtomicGoal = partial_deconstruct_rep(_, _, _)
        ; AtomicGoal = unify_assign_rep(_, _)
        ; AtomicGoal = cast_rep(_, _)
        ; AtomicGoal = unify_simple_test_rep(_, _)
        ; AtomicGoal = pragma_foreign_code_rep(_)
        ; AtomicGoal = builtin_call_rep(_, _, _)
        ; AtomicGoal = event_call_rep(_, _)
        ),
        IsCall = atomic_goal_is_trivial
    ;
        ( AtomicGoal = higher_order_call_rep(_, Args)
        ; AtomicGoal = method_call_rep(_, _, Args)
        ; AtomicGoal = plain_call_rep(_, _, Args)
        ),
        IsCall = atomic_goal_is_call(Args)
    ).

%----------------------------------------------------------------------------%
