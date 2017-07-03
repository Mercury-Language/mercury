%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2008, 2010-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: program_representation_utils.m.
% Author: pbone.
%
% Utilities for working with the program representation structures in the
% mdbcomp library. This file is not part of the mdbcomp library, since it
% contains routines only used by the deep profiling tools. Code here
% should be moved into the mdbcomp.program_representation.m module
% if it is to be used by other tools.
%
%---------------------------------------------------------------------------%

:- module program_representation_utils.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.program_representation.

:- import_module cord.
:- import_module list.
:- import_module set.
:- import_module unit.

%---------------------------------------------------------------------------%

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
                pgi_var_name_table          :: var_name_table
            ).

    % print_goal_to_strings(Lookup, VarTable, Indent, RevGoalPath, Goal,
    %   Strings):
    %
    % Print a goal (recursively) to a string representation.
    %
:- pred print_goal_to_strings(print_goal_info(T, GoalAnn)::in, int::in,
    reverse_goal_path::in, goal_rep(T)::in, cord(string)::out) is det
    <= goal_annotation(GoalAnn).

%---------------------------------------------------------------------------%

:- typeclass goal_annotation(T) where [
    % Print the goal annotation for inclusion by print_proc_to_strings
    % above.
    %
    pred print_goal_annotation_to_strings(var_name_table::in, T::in,
        cord(cord(string))::out) is det
].

    % A goal with no particular annotation has empty strings printed for goal
    % annotations.
    %
:- instance goal_annotation(unit).

%---------------------------------------------------------------------------%

    % Goal IDs are a more efficient way to identify goals than goal paths.
    %
    % The allow annotations to be kept in an array indexed by the goal id.
    %
:- pred label_goals(goal_id::out, containing_goal_map::out,
    goal_rep(T)::in, goal_rep(goal_id)::out) is det.

%---------------------------------------------------------------------------%

    % Search a program representation for the given procedure and return its
    % procedure representation if found, otherwise fail.
    %
:- pred progrep_search_proc(prog_rep::in, string_proc_label::in, proc_rep::out)
    is semidet.

%---------------------------------------------------------------------------%

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

    % Retrieve the instantiatedness of a variable, and variables that it's
    % binding depends upon from the instmap, if the variable is new ir_free is
    % returned and the variables it depends upon is the empty set.
    %
:- pred inst_map_get(inst_map::in, var_rep::in, inst_rep::out,
    set(var_rep)::out) is det.

    % Retrieve all the variables this variable depends upon,  indirect
    % dependencies are also returned.
    %
:- pred inst_map_get_var_deps(inst_map::in, var_rep::in, set(var_rep)::out)
    is det.

    % Merge two inst maps from different branches of execution.
    %
:- func merge_inst_map(inst_map, detism_rep, inst_map, detism_rep) = inst_map.

    % This type represents whether a traversal has seen more than one
    % instantiation of a variable within a single branch. If at the end of a
    % traversal a duplicate instantiation has been seen, we can either
    % accept a pessimistic default, or abort parallelisation of this particular
    % conjunction.
    %
:- type seen_duplicate_instantiation
    --->    seen_duplicate_instantiation
    ;       have_not_seen_duplicate_instantiation.

:- func merge_seen_duplicate_instantiation(seen_duplicate_instantiation,
    seen_duplicate_instantiation) = seen_duplicate_instantiation.

%---------------------------------------------------------------------------%

    % A difference between too inst maps. This lists the variables that are
    % instantiated by a particular goal.
    %
:- type inst_map_delta.

    % Get the set of variables that are instantiated by this inst map.
    %
:- pred inst_map_delta_get_var_set(inst_map_delta::in, set(var_rep)::out)
    is det.

    % The empty inst_map_delta. Nothing is instantiated.
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

%---------------------------------------------------------------------------%

    % Retrieve a set of all the vars involved with this atomic goal.
    %
:- pred atomic_goal_get_vars(atomic_goal_rep::in, set(var_rep)::out) is det.

%---------------------------------------------------------------------------%

:- type atomic_goal_is_call
    --->    atomic_goal_is_call(list(var_rep))
    ;       atomic_goal_is_trivial.

:- pred atomic_goal_is_call(atomic_goal_rep::in, atomic_goal_is_call::out)
    is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.

:- import_module bool.
:- import_module counter.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module std_util.
:- import_module string.

%---------------------------------------------------------------------------%

print_module_to_strings(ModuleRep, Strings) :-
    ModuleRep = module_rep(ModuleName, _StringTable, OISUTypesProcs,
        TypeTableMap, ProcReps),
    HeaderString = string.format("Module %s\n", [s(ModuleName)]),
    list.foldl(accumulate_print_oisu_type_procs_to_strings, OISUTypesProcs,
        cord.empty, OISUStrs),
    map.foldl(accumulate_print_type_table_entries_to_strings, TypeTableMap,
        cord.empty, TypeTableStrs0),
    ( if cord.is_empty(TypeTableStrs0) then
        TypeTableStrs = TypeTableStrs0
    else
        TypeTableStrs = cord.singleton("\nType table:\n") ++
            TypeTableStrs0 ++ nl
    ),
    map.foldl(accumulate_print_proc_to_strings, ProcReps,
        cord.empty, ProcRepStrs),
    Strings = cord.singleton(HeaderString) ++ OISUStrs ++ TypeTableStrs
        ++ ProcRepStrs.

%---------------------------------------------------------------------------%

:- pred accumulate_print_oisu_type_procs_to_strings(oisu_type_procs::in,
    cord(string)::in, cord(string)::out) is det.

accumulate_print_oisu_type_procs_to_strings(OISUTypeProcs, !Strings) :-
    print_oisu_type_procs_to_strings(OISUTypeProcs, OISUStr),
    !:Strings = !.Strings ++ OISUStr.

:- pred print_oisu_type_procs_to_strings(oisu_type_procs::in,
    cord(string)::out) is det.

print_oisu_type_procs_to_strings(OISUTypeProcs, Str) :-
    OISUTypeProcs = oisu_type_procs(TypeCtor,
        CreatorProcLabels, MutatorProcLabels, DestructorProcLabels),
    list.map(print_proc_label_to_string, CreatorProcLabels, CreatorStrs),
    list.map(print_proc_label_to_string, MutatorProcLabels, MutatorStrs),
    list.map(print_proc_label_to_string, DestructorProcLabels, DestructorStrs),
    CreatorNlCords = list.map(add_nl, CreatorStrs),
    MutatorNlCords = list.map(add_nl, MutatorStrs),
    DestructorNlCords = list.map(add_nl, DestructorStrs),
    Str = cord.from_list(["\nOISU type constructor ", TypeCtor])
        ++ cord.cons("\nCreator procs:\n",
            cord.cord_list_to_cord(CreatorNlCords))
        ++ cord.cons("\nMutator procs:\n",
            cord.cord_list_to_cord(MutatorNlCords))
        ++ cord.cons("\nDestructor procs:\n",
            cord.cord_list_to_cord(DestructorNlCords)).

%---------------------------------------------------------------------------%

:- pred accumulate_print_type_table_entries_to_strings(int::in,
    type_rep::in, cord(string)::in, cord(string)::out) is det.

accumulate_print_type_table_entries_to_strings(TypeNum, TypeRep, !Strings) :-
    string.int_to_string(TypeNum, TypeNumStr),
    type_rep_to_strings(TypeRep, TypeRepStrCord),
    Str = cord.singleton(TypeNumStr)
        ++ cord.singleton(" -> ")
        ++ TypeRepStrCord
        ++ cord.singleton("\n"),
    !:Strings = !.Strings ++ Str.

:- pred type_rep_to_strings(type_rep::in, cord(string)::out) is det.

type_rep_to_strings(TypeRep, Cord) :-
    (
        TypeRep = defined_type_rep(TypeCtorSymName, ArgTypes),
        TypeCtorSymNameStr = sym_name_to_string(TypeCtorSymName),
        TypeCtorSymNameCord = cord.singleton(TypeCtorSymNameStr),
        (
            ArgTypes = [],
            Cord = TypeCtorSymNameCord
        ;
            ArgTypes = [HeadTypeRep | TailTypeReps],
            arg_type_reps_to_strings(HeadTypeRep, TailTypeReps, ArgTypesCord),
            Cord = TypeCtorSymNameCord
                ++ cord.singleton("(")
                ++ ArgTypesCord
                ++ cord.singleton(")")
        )
    ;
        TypeRep = builtin_type_rep(BuiltinTypeRep),
        (
            BuiltinTypeRep = builtin_type_int_rep,
            TypeNameStr = "int"
        ;
            BuiltinTypeRep = builtin_type_uint_rep,
            TypeNameStr = "uint"
        ;
            BuiltinTypeRep = builtin_type_int8_rep,
            TypeNameStr = "int8"
        ;
            BuiltinTypeRep = builtin_type_uint8_rep,
            TypeNameStr = "uint8"
        ;
            BuiltinTypeRep = builtin_type_int16_rep,
            TypeNameStr = "int16"
        ;
            BuiltinTypeRep = builtin_type_uint16_rep,
            TypeNameStr = "uint16"
        ;
            BuiltinTypeRep = builtin_type_int32_rep,
            TypeNameStr = "int32"
        ;
            BuiltinTypeRep = builtin_type_uint32_rep,
            TypeNameStr = "uint32"
        ;
            BuiltinTypeRep = builtin_type_float_rep,
            TypeNameStr = "float"
        ;
            BuiltinTypeRep = builtin_type_string_rep,
            TypeNameStr = "string"
        ;
            BuiltinTypeRep = builtin_type_char_rep,
            TypeNameStr = "char"
        ),
        Cord = cord.singleton(TypeNameStr)
    ;
        TypeRep = tuple_type_rep(ArgTypes),
        (
            ArgTypes = [],
            Cord = cord.singleton("{}")
        ;
            ArgTypes = [HeadTypeRep | TailTypeReps],
            arg_type_reps_to_strings(HeadTypeRep, TailTypeReps, ArgTypesCord),
            Cord =
                cord.singleton("{")
                ++ ArgTypesCord
                ++ cord.singleton("}")
        )
    ;
        TypeRep = higher_order_type_rep(ArgTypes, MaybeResultType),
        (
            MaybeResultType = no,
            (
                ArgTypes = [],
                Cord = cord.singleton("pred ()")
            ;
                ArgTypes = [HeadTypeRep | TailTypeReps],
                arg_type_reps_to_strings(HeadTypeRep, TailTypeReps,
                    ArgTypesCord),
                Cord = cord.singleton("pred(")
                    ++ ArgTypesCord
                    ++ cord.singleton(")")
            )
        ;
            MaybeResultType = yes(ResultType),
            type_rep_to_strings(ResultType, ResultTypeCord),
            (
                ArgTypes = [],
                Cord = cord.singleton("func = ") ++ ResultTypeCord
            ;
                ArgTypes = [HeadTypeRep | TailTypeReps],
                arg_type_reps_to_strings(HeadTypeRep, TailTypeReps,
                    ArgTypesCord),
                Cord = cord.singleton("func(")
                    ++ ArgTypesCord
                    ++ cord.singleton(") = ")
                    ++ ResultTypeCord
            )
        )
    ;
        TypeRep = type_var_rep(N),
        string.int_to_string(N, NStr),
        Cord = cord.singleton("T" ++ NStr)
    ).

:- pred arg_type_reps_to_strings(type_rep::in, list(type_rep)::in,
    cord(string)::out) is det.

arg_type_reps_to_strings(HeadTypeRep, [], Cord) :-
    type_rep_to_strings(HeadTypeRep, Cord).
arg_type_reps_to_strings(HeadTypeRep, [HeadTailTypeRep | TailTailTypeReps],
        Cord) :-
    type_rep_to_strings(HeadTypeRep, HeadCord),
    arg_type_reps_to_strings(HeadTailTypeRep, TailTailTypeReps, TailCord),
    Cord = HeadCord ++ cord.singleton(", ") ++ TailCord.

%---------------------------------------------------------------------------%

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
    ProcDefnRep = proc_defn_rep(ArgVarReps, GoalRep, VarNameTable,
        MaybeVarTypeTable, Detism),
    print_proc_label_to_string(ProcLabel, ProcLabelString0),
    detism_to_string(Detism, DetismString),
    ProcLabelString = DetismString ++ cord.singleton(" ") ++
        cord.singleton(ProcLabelString0),
    print_args_to_strings(print_head_var, VarNameTable, ArgVarReps,
        ArgsString),
    print_goal_to_strings(print_goal_info(Lookup, VarNameTable), 1, rgp_nil,
        GoalRep, GoalString),
    MainStrings = ProcLabelString ++ ArgsString ++ cord.singleton(" :-\n") ++
        GoalString ++ nl,
    (
        MaybeVarTypeTable = no,
        Strings = MainStrings
    ;
        MaybeVarTypeTable = yes(VarTypeTable),
        map.foldl(accumulate_var_type_table_entry_strings(VarNameTable),
            VarTypeTable, cord.init, TypeTableStrings),
        Strings = TypeTableStrings ++ MainStrings
    ).

:- pred accumulate_var_type_table_entry_strings(var_name_table::in,
    var_rep::in, type_rep::in, cord(string)::in, cord(string)::out) is det.

accumulate_var_type_table_entry_strings(VarNameTable, VarNum, TypeRep,
        !Strings) :-
    string.int_to_string(VarNum, VarNumStr),
    ( if
        search_var_name(VarNameTable, VarNum, VarName),
        not VarName = ""
    then
        VarIdStrs = cord.from_list([VarName, " ", VarNumStr, " -> "])
    else
        VarIdStrs = cord.from_list(["unnamed_var ", VarNumStr, " -> "])
    ),
    type_rep_to_strings(TypeRep, TypeRepStrs),
    EntryStrs = VarIdStrs ++ TypeRepStrs ++ nl,
    !:Strings = !.Strings ++ EntryStrs.

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

%---------------------------------------------------------------------------%

print_goal_to_strings(Info, Indent, RevGoalPath, GoalRep, Strings) :-
    GoalRep = goal_rep(GoalExprRep, DetismRep, AnnotationKey),
    VarTable = Info ^ pgi_var_name_table,
    (
        GoalExprRep = conj_rep(ConjGoalReps),
        print_conj_to_strings(Info, Indent, RevGoalPath,
            ConjGoalReps, ExprString)
    ;
        GoalExprRep = disj_rep(DisjGoalReps),
        print_disj_to_strings(Info, Indent, RevGoalPath, 1, DisjGoalReps,
            no, DisjString),
        ExprString = indent(Indent) ++
            cord.singleton("(\n") ++ DisjString ++ indent(Indent) ++
            cord.singleton(")\n")
    ;
        GoalExprRep = switch_rep(SwitchVarRep, CanFail, CasesRep),
        lookup_var_name(VarTable, SwitchVarRep, SwitchVarName),
        string.format("%s switch on %s\n",
            [s(string(CanFail)), s(SwitchVarName)], SwitchOnString),
        print_switch_to_strings(Info, Indent + 1, RevGoalPath, 1,
            CasesRep, no, SwitchString),
        ExprString = indent(Indent) ++ cord.singleton(SwitchOnString) ++
            indent(Indent) ++ cord.singleton("(\n") ++ SwitchString ++
            indent(Indent) ++ cord.singleton(")\n")
    ;
        GoalExprRep = ite_rep(CondRep, ThenRep, ElseRep),
        RevGoalPathCond = rgp_cons(RevGoalPath, step_ite_cond),
        RevGoalPathThen = rgp_cons(RevGoalPath, step_ite_then),
        RevGoalPathElse = rgp_cons(RevGoalPath, step_ite_else),
        print_goal_to_strings(Info, Indent + 1, RevGoalPathCond,
            CondRep, CondString),
        print_goal_to_strings(Info, Indent + 1, RevGoalPathThen,
            ThenRep, ThenString),
        print_goal_to_strings(Info, Indent + 1, RevGoalPathElse,
            ElseRep, ElseString),
        IndentString = indent(Indent),
        ExprString = IndentString ++ cord.singleton("(\n") ++ CondString ++
            IndentString ++ cord.singleton("->\n") ++ ThenString ++
            IndentString ++ cord.singleton(";\n") ++ ElseString ++
            IndentString ++ cord.singleton(")\n")
    ;
        GoalExprRep = negation_rep(SubGoalRep),
        RevSubGoalPath = rgp_cons(RevGoalPath, step_neg),
        print_goal_to_strings(Info, Indent + 1, RevSubGoalPath,
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
        RevSubGoalPath = rgp_cons(RevGoalPath, step_scope(MaybeCut)),
        print_goal_to_strings(Info, Indent + 1, RevSubGoalPath,
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

    ( if GoalExprRep = conj_rep(_) then
        LinePrefix = indent(Indent) ++ singleton("% conjunction: "),
        ExtraLineForConjunctions = nl
    else
        LinePrefix = indent(Indent) ++ singleton("% "),
        ExtraLineForConjunctions = empty
    ),
    detism_to_string(DetismRep, DetismString),
    DetismLine = LinePrefix ++ DetismString ++ nl,
    LookupAnnotation = Info ^ pgi_lookup_annotation,
    GoalAnnotation = LookupAnnotation(AnnotationKey),
    print_goal_annotation_to_strings(VarTable, GoalAnnotation,
        GoalAnnotationLines0),
    ( if is_empty(GoalAnnotationLines0) then
        GoalAnnotationLines = empty
    else
        GoalAnnotationLines1 = map((func(Line) = LinePrefix ++ Line ++ nl),
            GoalAnnotationLines0),
        GoalAnnotationLines = foldr(++, GoalAnnotationLines1, empty)
    ),

    GoalPathString0 = rev_goal_path_to_string(RevGoalPath),
    ( if GoalPathString0 = "" then
        GoalPathString = "root goal"
    else
        GoalPathString = GoalPathString0
    ),
    GoalPathLine = LinePrefix ++ cord.singleton(GoalPathString) ++ nl,

    Strings = GoalPathLine
        ++ DetismLine
        ++ GoalAnnotationLines
        ++ ExtraLineForConjunctions
        ++ ExprString.

:- pred print_conj_to_strings(print_goal_info(T, GoalAnn)::in, int::in,
    reverse_goal_path::in, list(goal_rep(T))::in, cord(string)::out) is det
    <= goal_annotation(GoalAnn).

print_conj_to_strings(Info, Indent, RevGoalPath, GoalReps, Strings) :-
    (
        GoalReps = [],
        Strings = cord.snoc(indent(Indent), "true\n")
    ;
        GoalReps = [_ | _],
        print_conj_to_strings_2(Info, Indent, RevGoalPath, 1, GoalReps,
            Strings)
    ).

:- pred print_conj_to_strings_2(print_goal_info(T, GoalAnn)::in, int::in,
    reverse_goal_path::in, int::in, list(goal_rep(T))::in, cord(string)::out)
    is det <= goal_annotation(GoalAnn).

print_conj_to_strings_2(_, _Indent, _, _, [], cord.empty).
print_conj_to_strings_2(Info, Indent, RevGoalPath, ConjNum,
        [GoalRep | GoalReps], Strings) :-
    % We use the absence of a separator to denote conjunction.
    %
    % We could try to append the comma at the end of each goal that is
    % not last in a conjunction, but that would be significant work,
    % and (at least for now) there is no real need for it.
    RevSubGoalPath = rgp_cons(RevGoalPath, step_conj(ConjNum)),
    print_goal_to_strings(Info, Indent, RevSubGoalPath, GoalRep,
        HeadGoalString),
    print_conj_to_strings_2(Info, Indent, RevGoalPath, ConjNum + 1,
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
    reverse_goal_path::in, int::in, list(goal_rep(T))::in, bool::in,
    cord(string)::out)
    is det <= goal_annotation(GoalAnn).

print_disj_to_strings(_, _Indent, _, _, [], _PrintSemi, cord.empty).
print_disj_to_strings(Info, Indent, RevGoalPath, DisjNum,
        [GoalRep | GoalReps], PrintSemi, Strings) :-
    (
        PrintSemi = no,
        DelimString = cord.empty
    ;
        PrintSemi = yes,
        DelimString = indent(Indent) ++ cord.singleton(";\n")
    ),
    RevSubGoalPath = rgp_cons(RevGoalPath, step_disj(DisjNum)),
    print_goal_to_strings(Info, Indent + 1, RevSubGoalPath, GoalRep,
        HeadGoalString),
    print_disj_to_strings(Info, Indent, RevGoalPath, DisjNum + 1,
        GoalReps, yes, TailGoalsString),
    Strings = DelimString ++ HeadGoalString ++ TailGoalsString.

:- pred print_switch_to_strings(print_goal_info(T, GoalAnn)::in, int::in,
    reverse_goal_path::in, int::in, list(case_rep(T))::in, bool::in,
    cord(string)::out) is det
    <= goal_annotation(GoalAnn).

print_switch_to_strings(_, _Indent, _, _, [], _PrintSemi, cord.empty).
print_switch_to_strings(Info, Indent, RevGoalPath, CaseNum,
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
    RevSubGoalPath = rgp_cons(RevGoalPath,
        step_switch(CaseNum, unknown_num_functors_in_type)),
    print_goal_to_strings(Info, Indent + 1, RevSubGoalPath,
        GoalRep, HeadGoalString),
    print_switch_to_strings(Info, Indent, RevGoalPath, CaseNum + 1,
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

%---------------------------------------------------------------------------%

:- pred print_atomic_goal_to_strings(var_name_table::in, atomic_goal_rep::in,
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

%---------------------------------------------------------------------------%

:- pred print_args_to_strings(pred(var_name_table, T, string), var_name_table,
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

:- pred print_args_2_to_strings(pred(var_name_table, T, string),
    var_name_table, list(T), cord(string), cord(string)).
:- mode print_args_2_to_strings(pred(in, in, out) is det, in, in, in, out)
    is det.

print_args_2_to_strings(_, _, [], _, cord.empty).
print_args_2_to_strings(PrintArg, VarTable, [Arg | Args], Prefix, Strings) :-
    PrintArg(VarTable, Arg, ArgName),
    print_args_2_to_strings(PrintArg, VarTable, Args, cord.singleton(", "),
        ArgsString),
    Strings = Prefix ++ cord.cons(ArgName, ArgsString).

:- pred print_maybe_var(var_name_table::in, maybe(var_rep)::in, string::out)
    is det.

print_maybe_var(_, no, "_").
print_maybe_var(VarTable, yes(VarRep), VarName) :-
    lookup_var_name(VarTable, VarRep, VarName).

:- pred print_head_var(var_name_table::in, head_var_rep::in, string::out)
    is det.

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

%---------------------------------------------------------------------------%

:- func indent(int) = cord(string).

indent(N) =
    ( if N =< 0 then
        cord.empty
    else
        cord.singleton("  ") ++ indent(N - 1)
    ).

:- func nl_indent(int) = cord(string).

nl_indent(N) = nl ++ indent(N).

:- func nl = cord(string).

nl = cord.singleton("\n").

:- func add_nl(string) = cord(string).

add_nl(Str) = cord.from_list([Str, "\n"]).

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

:- instance goal_annotation(unit) where [
    pred(print_goal_annotation_to_strings/3) is print_unit_to_strings
].

:- pred print_unit_to_strings(var_name_table::in, unit::in,
    cord(cord(string))::out) is det.

print_unit_to_strings(_, _, cord.empty).

%---------------------------------------------------------------------------%
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
    map.det_insert(GoalId, ContainingGoal, !Map),
    (
        GoalExpr0 = conj_rep(Conjs0),
        map_foldl3(label_goal_wrapper((func(N) = step_conj(N)), GoalId),
            Conjs0, Conjs, 1, _, !Counter, !Map),
        GoalExpr = conj_rep(Conjs)
    ;
        GoalExpr0 = disj_rep(Disjs0),
        map_foldl3(label_goal_wrapper((func(N) = step_disj(N)), GoalId),
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

:- pred label_goal_wrapper(
    (func(int) = goal_path_step)::in(func(in) = out is det), goal_id::in,
    goal_rep(T)::in, goal_rep(goal_id)::out, int::in, int::out,
    counter::in, counter::out,
    map(goal_id, containing_goal)::in, map(goal_id, containing_goal)::out)
    is det.

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
    label_goal_wrapper(
        (func(N) = step_switch(N, unknown_num_functors_in_type)),
        ParentGoalId, Goal0, Goal, !CaseNum, !Counter, !Map),
    !:Case = case_rep(MainConsId, OtherConsIds, Goal).

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

:- type inst_map
    --->    inst_map(
                % The actual inst map.
                im_inst_map         :: map(var_rep, inst_rep),

                % A tree describing dependencies between bound variables.
                im_var_dep_map      :: map(var_rep, set(var_rep))
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
    map.det_insert(VarRep, InstRep, VarToInst0, VarToInst),
    map.det_insert(VarRep, DepVars, VarToDepVars0, VarToDepVars),
    InstMap = inst_map(VarToInst, VarToDepVars).

%---------------------------------------------------------------------------%

inst_map_ground_vars(Vars, DepVars, !InstMap, SeenDuplicateInstantiation) :-
    list.foldl2(inst_map_ground_var(DepVars), Vars, !InstMap,
        have_not_seen_duplicate_instantiation, SeenDuplicateInstantiation).

:- pred inst_map_ground_var(set(var_rep)::in, var_rep::in,
    inst_map::in, inst_map::out, seen_duplicate_instantiation::in,
    seen_duplicate_instantiation::out) is det.

inst_map_ground_var(DepVars0, Var, InstMap0, InstMap,
        !SeenDuplicateInstantiation) :-
    InstMap0 = inst_map(VarToInst0, VarToDepVars0),
    ( if map.search(VarToInst0, Var, InstPrime) then
        Inst = InstPrime
    else
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
    map.set(Var, NewInst, VarToInst0, VarToInst),
    map.set(Var, DepVars, VarToDepVars0, VarToDepVars),
    InstMap = inst_map(VarToInst, VarToDepVars).

%---------------------------------------------------------------------------%

inst_map_get(inst_map(VarToInst, VarToDepVars), Var, Inst, DepVars) :-
    ( if map.search(VarToInst, Var, InstPrime) then
        Inst = InstPrime,
        map.lookup(VarToDepVars, Var, DepVars)
    else
        Inst = ir_free_rep,
        DepVars = set.init
    ).

inst_map_get_var_deps(inst_map(_, VarToDepVars), VarRep, DepVars) :-
    inst_map_get_var_deps_2(VarToDepVars, VarRep, set.init, DepVars).

:- pred inst_map_get_var_deps_2(map(var_rep, set(var_rep))::in, var_rep::in,
    set(var_rep)::in, set(var_rep)::out) is det.

inst_map_get_var_deps_2(VarToDepVars, VarRep, !Set) :-
    ( if set.contains(!.Set, VarRep) then
        true
        % This variable has already been visited. Stopping here prevents
        % following any cycles in the graph (which should be impossible anyway)
        % or following the same path twice if there are diamonds in the graph.
    else
        ( if map.search(VarToDepVars, VarRep, DepVars) then
            !:Set = set.union(!.Set, DepVars),
            set.fold(inst_map_get_var_deps_2(VarToDepVars), DepVars, !Set)
        else
            true
        )
    ).

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

merge_seen_duplicate_instantiation(A, B) = R :-
    ( if
        A = have_not_seen_duplicate_instantiation,
        B = have_not_seen_duplicate_instantiation
    then
        R = have_not_seen_duplicate_instantiation
    else
        R = seen_duplicate_instantiation
    ).

%---------------------------------------------------------------------------%

:- type inst_map_delta
    --->    inst_map_delta(set(var_rep)).

inst_map_delta_get_var_set(inst_map_delta(Vars), Vars).

empty_inst_map_delta(inst_map_delta(Vars)) :-
    set.init(Vars).
empty_inst_map_delta = InstMap :-
    empty_inst_map_delta(InstMap).

calc_inst_map_delta(Before, After, inst_map_delta(DeltaVars)) :-
    map.foldl(
        ( pred(Var::in, Inst::in, Set0::in, Set::out) is det :-
            ( if
                map.search(Before ^ im_inst_map, Var, BeforeInst)
            then
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
                        set.insert(Var, Set0, Set)
                    )
                ;
                    BeforeInst = ir_ground_rep,
                    (
                        Inst = ir_free_rep,
                        unexpected($pred,
                            "variable should become less instantiated")
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
                        unexpected($pred,
                            "variable should become less instantiated")
                    ;
                        ( Inst = ir_ground_rep
                        ; Inst = ir_other_rep
                        )
                    ),
                    Set = Set0
                )
            else
                % If we couldn't find the variable then it was free;
                % it may have been in the head of the procedure.
                (
                    Inst = ir_free_rep,
                    Set = Set0
                ;
                    ( Inst = ir_ground_rep
                    ; Inst = ir_other_rep
                    ),
                    % This variable has become more instantiated.
                    set.insert(Var, Set0, Set)
                )
            )
        ), After ^ im_inst_map, set.init, DeltaVars).

%---------------------------------------------------------------------------%

atomic_goal_get_vars(AtomicGoal, Vars) :-
    (
        ( AtomicGoal = unify_construct_rep(Var, _, VarsL)
        ; AtomicGoal = unify_deconstruct_rep(Var, _, VarsL)
        ; AtomicGoal = higher_order_call_rep(Var, VarsL)
        ; AtomicGoal = method_call_rep(Var, _, VarsL)
        ),
        Vars0 = list_to_set(VarsL),
        set.insert(Var, Vars0, Vars)
    ;
        ( AtomicGoal = partial_construct_rep(Var, _, MaybeVars)
        ; AtomicGoal = partial_deconstruct_rep(Var, _, MaybeVars)
        ),
        list.foldl(
            ( pred(MaybeVar::in, Set0::in, Set::out) is det :-
                (
                    MaybeVar = yes(VarI),
                    set.insert(VarI, Set0, Set)
                ;
                    MaybeVar = no,
                    Set = Set0
                )
            ), MaybeVars, set.init, Vars0),
        set.insert(Var, Vars0, Vars)
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

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%
