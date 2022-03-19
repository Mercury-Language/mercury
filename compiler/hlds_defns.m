%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% This module provides the capability to print out the list of the entities
% defined in the given module, and the length (in terms of lines) of predicate
% definitions.
%
%-----------------------------------------------------------------------------%

:- module hlds.hlds_defns.
:- interface.

:- import_module hlds.hlds_module.

:- import_module io.

    % When splitting up a module, it is sometimes hard to ensure that
    % every part of the original module ends up in exactly one of the
    % successor modules.
    %
    % This predicate prints a list of the main kinds of entities
    % (types, insts, modes, functions and predicates) defined the given module.
    % Programmers can use this information using commands such as
    % "comm -12 m1.defns m2.defns", which will list the entities
    % that are defined in both m1 and m2.
    %
:- pred write_hlds_defns(io.text_output_stream::in, module_info::in,
    io::di, io::uo) is det.

    % For each predicate (or function) in the module, print its file name
    % and the number of lines in its definition. Order the output by
    % predicate or function name and arity.
    %
    % (Since information such as the presence of a lone close parenthesis
    % on the last line of a clause is not preserved in the HLDS, this
    % line count will be approximate, but it is still useful for e.g.
    % finding excessively-long predicates that should be split up.)
    %
:- pred write_hlds_defn_line_counts(io.text_output_stream::in, module_info::in,
    io::di, io::uo) is det.

    % For each predicate (or function) in the module, print its approximate
    % first and last line numbers. (See the comment above about why
    % the "approximate" part is unavoidable.) Order the output by first
    % line number.
    %
:- pred write_hlds_defn_extents(io.text_output_stream::in, module_info::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_class.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_inst_mode.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.

:- import_module assoc_list.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.

%-----------------------------------------------------------------------------%

write_hlds_defns(Stream, ModuleInfo, !IO) :-
    module_info_get_name(ModuleInfo, ModuleName),

    module_info_get_type_table(ModuleInfo, TypeTable),
    get_all_type_ctor_defns(TypeTable, TypeCtorDefns),
    gather_local_type_names(ModuleName, TypeCtorDefns,
        set.init, TypeNameArities),

    module_info_get_inst_table(ModuleInfo, InstTable),
    inst_table_get_user_insts(InstTable, UserInstTable),
    map.keys(UserInstTable, UserInstIds),
    gather_local_inst_names(ModuleName, UserInstIds,
        set.init, InstNameArities),

    module_info_get_mode_table(ModuleInfo, ModeTable),
    mode_table_get_mode_defns(ModeTable, ModeDefnMap),
    map.keys(ModeDefnMap, ModeCtors),
    gather_local_mode_names(ModuleName, ModeCtors,
        set.init, ModeNameArities),

    module_info_get_pred_id_table(ModuleInfo, PredIdTable),
    map.to_sorted_assoc_list(PredIdTable, PredDefns),
    gather_local_pred_names(ModuleName, PredDefns,
        set.init, FuncNameArities, set.init, PredNameArities),

    module_info_get_class_table(ModuleInfo, ClassTable),
    map.keys(ClassTable, ClassIds),
    gather_local_typeclass_names(ModuleName, ClassIds,
        set.init, ClassNameArities),

    module_info_get_instance_table(ModuleInfo, InstanceTable),
    map.to_sorted_assoc_list(InstanceTable, InstanceDefns),
    gather_local_instance_names(ModuleName, InstanceDefns,
        set.init, InstanceDescs),

    % We print the output in this order to ensure that the resulting file
    % is sorted.
    output_prefixed_name_arities(Stream, "func ",
        set.to_sorted_list(FuncNameArities), !IO),
    output_prefixed_name_arities(Stream, "inst ",
        set.to_sorted_list(InstNameArities), !IO),
    output_prefixed_strings(Stream, "instance ",
        set.to_sorted_list(InstanceDescs), !IO),
    output_prefixed_name_arities(Stream, "mode ",
        set.to_sorted_list(ModeNameArities), !IO),
    output_prefixed_name_arities(Stream, "pred ",
        set.to_sorted_list(PredNameArities), !IO),
    output_prefixed_name_arities(Stream, "type ",
        set.to_sorted_list(TypeNameArities), !IO),
    output_prefixed_name_arities(Stream, "typeclass ",
        set.to_sorted_list(ClassNameArities), !IO).

%-----------------------------------------------------------------------------%

:- pred gather_local_type_names(module_name::in,
    assoc_list(type_ctor, hlds_type_defn)::in,
    set(name_arity)::in, set(name_arity)::out) is det.

gather_local_type_names(_, [], !TypeNameArities).
gather_local_type_names(ModuleName, [TypeCtor - _Defn | TypeCtorDefns],
        !TypeNameArities) :-
    TypeCtor = type_ctor(TypeCtorSymName, TypeCtorArity),
    (
        TypeCtorSymName = unqualified(_),
        unexpected($pred, "unqualified type_ctor name")
    ;
        TypeCtorSymName = qualified(TypeCtorModuleName, TypeCtorName),
        ( if TypeCtorModuleName = ModuleName then
            set.insert(name_arity(TypeCtorName, TypeCtorArity),
                !TypeNameArities)
        else
            true
        )
    ),
    gather_local_type_names(ModuleName, TypeCtorDefns, !TypeNameArities).

%-----------------------------------------------------------------------------%

:- pred gather_local_inst_names(module_name::in, list(inst_ctor)::in,
    set(name_arity)::in, set(name_arity)::out) is det.

gather_local_inst_names(_, [], !InstNameArities).
gather_local_inst_names(ModuleName, [InstCtor | InstCtors],
        !InstNameArities) :-
    InstCtor = inst_ctor(InstSymName, InstArity),
    (
        InstSymName = unqualified(_),
        unexpected($pred, "unqualified inst_ctor name")
    ;
        InstSymName = qualified(InstModuleName, InstName),
        ( if InstModuleName = ModuleName then
            set.insert(name_arity(InstName, InstArity), !InstNameArities)
        else
            true
        )
    ),
    gather_local_inst_names(ModuleName, InstCtors, !InstNameArities).

%-----------------------------------------------------------------------------%

:- pred gather_local_mode_names(module_name::in, list(mode_ctor)::in,
    set(name_arity)::in, set(name_arity)::out) is det.

gather_local_mode_names(_, [], !ModeNameArities).
gather_local_mode_names(ModuleName, [ModeCtor | ModeCtors],
        !ModeNameArities) :-
    ModeCtor = mode_ctor(ModeSymName, ModeArity),
    (
        ModeSymName = unqualified(_),
        unexpected($pred, "unqualified mode_ctor name")
    ;
        ModeSymName = qualified(ModeModuleName, ModeName),
        ( if ModeModuleName = ModuleName then
            set.insert(name_arity(ModeName, ModeArity), !ModeNameArities)
        else
            true
        )
    ),
    gather_local_mode_names(ModuleName, ModeCtors, !ModeNameArities).

%-----------------------------------------------------------------------------%

:- pred gather_local_pred_names(module_name::in,
    assoc_list(pred_id, pred_info)::in,
    set(name_arity)::in, set(name_arity)::out,
    set(name_arity)::in, set(name_arity)::out) is det.

gather_local_pred_names(_ModuleName, [], !FuncNameArities, !PredNameArities).
gather_local_pred_names(ModuleName, [PredDefn | PredDefns],
        !FuncNameArities, !PredNameArities) :-
    PredDefn = _PredId - PredInfo,
    pred_info_get_module_name(PredInfo, PredModuleName),
    pred_info_get_origin(PredInfo, Origin),
    ( if
        PredModuleName = ModuleName,
        Origin = origin_user(_)
    then
        pred_info_get_name(PredInfo, PredName),
        PredArity = pred_info_orig_arity(PredInfo),
        NameArity = name_arity(PredName, PredArity),
        PorF = pred_info_is_pred_or_func(PredInfo),
        (
            PorF = pf_function,
            set.insert(NameArity, !FuncNameArities)
        ;
            PorF = pf_predicate,
            set.insert(NameArity, !PredNameArities)
        )
    else
        true
    ),
    gather_local_pred_names(ModuleName, PredDefns,
        !FuncNameArities, !PredNameArities).

%-----------------------------------------------------------------------------%

:- pred gather_local_typeclass_names(module_name::in, list(class_id)::in,
    set(name_arity)::in, set(name_arity)::out) is det.

gather_local_typeclass_names(_, [], !ClassNameArities).
gather_local_typeclass_names(ModuleName, [ClassId | ClassIds],
        !ClassNameArities) :-
    ClassId = class_id(ClassSymName, ClassArity),
    (
        ClassSymName = unqualified(_),
        unexpected($pred, "unqualified class_id name")
    ;
        ClassSymName = qualified(ClassModuleName, ClassName),
        ( if ClassModuleName = ModuleName then
            set.insert(name_arity(ClassName, ClassArity), !ClassNameArities)
        else
            true
        )
    ),
    gather_local_typeclass_names(ModuleName, ClassIds, !ClassNameArities).

%-----------------------------------------------------------------------------%

:- pred gather_local_instance_names(module_name::in,
    assoc_list(class_id, list(hlds_instance_defn))::in,
    set(string)::in, set(string)::out) is det.

gather_local_instance_names(_, [], !InstanceDescs).
gather_local_instance_names(ModuleName, [InstancePair | InstancePairs],
        !InstanceDescs) :-
    InstancePair = ClassId - InstanceDefns,
    gather_local_instance_names_for_class(ModuleName, ClassId, InstanceDefns,
        !InstanceDescs),
    gather_local_instance_names(ModuleName, InstancePairs, !InstanceDescs).

:- pred gather_local_instance_names_for_class(module_name::in, class_id::in,
    list(hlds_instance_defn)::in, set(string)::in, set(string)::out) is det.

gather_local_instance_names_for_class(_, _, [], !InstanceDescs).
gather_local_instance_names_for_class(ModuleName, ClassId,
        [InstanceDefn | InstanceDefns], !InstanceDescs) :-
    InstanceModuleName = InstanceDefn ^ instdefn_module,
    OrigTypes = InstanceDefn ^ instdefn_orig_types,
    ( if InstanceModuleName = ModuleName then
        ClassId = class_id(ClassSymName, ClassArity),
        ClassName = unqualify_name(ClassSymName),

        list.map(instance_type_to_desc, OrigTypes, OrigTypeDescs),
        TypeVectorDesc = string.join_list(", ", OrigTypeDescs),

        string.format("%s/%d <%s>",
            [s(ClassName), i(ClassArity), s(TypeVectorDesc)], InstanceDesc),
        set.insert(InstanceDesc, !InstanceDescs)
    else
        true
    ),
    gather_local_instance_names_for_class(ModuleName, ClassId,
        InstanceDefns, !InstanceDescs).

:- pred instance_type_to_desc(mer_type::in, string::out) is det.

instance_type_to_desc(Type, TypeDesc) :-
    type_to_ctor_det(Type, TypeCtor),
    TypeCtor = type_ctor(TypeSymName, TypeArity),
    TypeName = unqualify_name(TypeSymName),
    TypeDesc = TypeName ++ "/" ++ string.int_to_string(TypeArity).

%-----------------------------------------------------------------------------%

:- pred output_prefixed_name_arities(io.text_output_stream::in,
    string::in, list(name_arity)::in, io::di, io::uo) is det.

output_prefixed_name_arities(_Stream, _Prefix, [], !IO).
output_prefixed_name_arities(Stream, Prefix, [NameArity | NameArities], !IO) :-
    NameArity = name_arity(Name, Arity),
    io.write_string(Stream, Prefix, !IO),
    io.write_string(Stream, Name, !IO),
    io.write_string(Stream, "/", !IO),
    io.write_int(Stream, Arity, !IO),
    io.nl(Stream, !IO),
    output_prefixed_name_arities(Stream, Prefix, NameArities, !IO).

:- pred output_prefixed_strings(io.text_output_stream::in,
    string::in, list(string)::in, io::di, io::uo) is det.

output_prefixed_strings(_Stream, _Prefix, [], !IO).
output_prefixed_strings(Stream, Prefix, [Str | Strs], !IO) :-
    io.write_string(Stream, Prefix, !IO),
    io.write_string(Stream, Str, !IO),
    io.nl(Stream, !IO),
    output_prefixed_strings(Stream, Prefix, Strs, !IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

write_hlds_defn_line_counts(Stream, ModuleInfo, !IO) :-
    module_info_get_name(ModuleInfo, ModuleName),

    module_info_get_pred_id_table(ModuleInfo, PredIdTable),
    map.values(PredIdTable, PredInfos),
    list.foldl(gather_pred_line_counts(ModuleName), PredInfos,
        [], PredLineCounts),

    list.sort(compare_pred_line_counts_by_pred_name,
        PredLineCounts, SortedPredLineCounts),
    list.foldl(acc_max_predname_arity_str_len, SortedPredLineCounts,
        0, MaxLen),
    list.foldl(write_pred_line_count(Stream, MaxLen + 1),
        SortedPredLineCounts, !IO).

write_hlds_defn_extents(Stream, ModuleInfo, !IO) :-
    module_info_get_name(ModuleInfo, ModuleName),

    module_info_get_pred_id_table(ModuleInfo, PredIdTable),
    map.values(PredIdTable, PredInfos),
    list.foldl(gather_pred_line_counts(ModuleName), PredInfos,
        [], PredLineCounts),

    list.sort(compare_pred_line_counts_by_start_line_number,
        PredLineCounts, SortedPredLineCounts),
    list.foldl(acc_max_predname_arity_str_len, SortedPredLineCounts,
        0, MaxLen),
    list.foldl(write_pred_extent(Stream, MaxLen + 1),
        SortedPredLineCounts, !IO).

%-----------------------------------------------------------------------------%

:- type pred_line_count
    --->    pred_line_count(
                plc_name_arity_str  :: string,
                plc_file_name       :: string,
                plc_first_line      :: int,
                plc_last_line       :: int,
                plc_line_count      :: int
            ).

:- pred gather_pred_line_counts(module_name::in, pred_info::in,
    list(pred_line_count)::in, list(pred_line_count)::out) is det.

gather_pred_line_counts(ModuleName, PredInfo, !PredLineCounts) :-
    pred_info_get_module_name(PredInfo, PredModuleName),
    pred_info_get_origin(PredInfo, Origin),
    ( if
        PredModuleName = ModuleName,
        Origin = origin_user(_)
    then
        pred_info_get_clauses_info(PredInfo, ClausesInfo),
        clauses_info_get_clauses_rep(ClausesInfo, ClausesRep, _ItemNumbers),
        get_clause_list_maybe_repeated(ClausesRep, Clauses),
        (
            Clauses = []
        ;
            Clauses = [HeadClause | TailClauses],
            FirstClause = HeadClause,
            ( if last(TailClauses, LastClausePrime) then
                LastClause = LastClausePrime
            else
                LastClause = HeadClause
            ),
            find_first_context(FirstClause ^ clause_body, FirstContext),
            find_last_context(LastClause ^ clause_body, LastContext),
            FirstContext = context(FirstFileName, FirstLineNumber0),
            LastContext = context(LastFileName, LastLineNumber0),
            % In some rare cases, the "last" part of the goal
            % appears *before* the "first" part.
            ( if FirstLineNumber0 > LastLineNumber0 then
                FirstLineNumber = LastLineNumber0,
                LastLineNumber = FirstLineNumber0
            else
                FirstLineNumber = FirstLineNumber0,
                LastLineNumber = LastLineNumber0
            ),
            ( if FirstFileName = LastFileName then
                pred_info_get_name(PredInfo, PredName),
                PredArity = pred_info_orig_arity(PredInfo),
                string.format("%s/%d", [s(PredName), i(PredArity)],
                    PredNameArityStr),
                LineCount = LastLineNumber - FirstLineNumber + 1,
                PLC = pred_line_count(PredNameArityStr, FirstFileName,
                    FirstLineNumber, LastLineNumber, LineCount),
                !:PredLineCounts = [PLC | !.PredLineCounts]
            else
                % We don't have the consistent information we need
                % to compute anything useful.
                true
            )
        )
    else
        true
    ).

%-----------------------------------------------------------------------------%

:- pred find_first_context(hlds_goal::in, prog_context::out) is det.

find_first_context(Goal, FirstContext) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    (
        ( GoalExpr = unify(_, _, _, _, _)
        ; GoalExpr = plain_call(_, _, _, _, _, _)
        ; GoalExpr = generic_call(_, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        FirstContext = goal_info_get_context(GoalInfo)
    ;
        ( GoalExpr = conj(_Kind, SubGoals)
        ; GoalExpr = disj(SubGoals)
        ),
        (
            SubGoals = [],
            FirstContext = goal_info_get_context(GoalInfo)
        ;
            SubGoals = [HeadSubGoal | _TailSubGoal],
            find_first_context(HeadSubGoal, FirstContext)
        )
    ;
        GoalExpr = switch(_, _, Cases),
        (
            Cases = [],
            FirstContext = goal_info_get_context(GoalInfo)
        ;
            Cases = [HeadCase | _TailCases],
            HeadCase = case(_MainConsId, _OtherConsIds, SubGoal),
            find_first_context(SubGoal, FirstContext)
        )
    ;
        ( GoalExpr = negation(SubGoal)
        ; GoalExpr = scope(_Reason, SubGoal)
        ),
        find_first_context(SubGoal, FirstContext)
    ;
        GoalExpr = if_then_else(_Vars, Cond, _Then, _Else),
        find_first_context(Cond, FirstContext)
    ;
        GoalExpr = shorthand(Shorthand),
        (
            Shorthand = bi_implication(SubGoalA, _SubGoalB),
            find_first_context(SubGoalA, FirstContext)
        ;
            Shorthand = atomic_goal(_Type, _Outer, _Inner, _OutputVars,
                MainGoal, _OrElseGoals, _Inners),
            find_first_context(MainGoal, FirstContext)
        ;
            Shorthand = try_goal(_MaybeIO, _ResultVar, SubGoal),
            find_first_context(SubGoal, FirstContext)
        )
    ).

:- pred find_last_context(hlds_goal::in, prog_context::out) is det.

find_last_context(Goal, FirstContext) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    (
        ( GoalExpr = unify(_, _, _, _, _)
        ; GoalExpr = plain_call(_, _, _, _, _, _)
        ; GoalExpr = generic_call(_, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        FirstContext = goal_info_get_context(GoalInfo)
    ;
        ( GoalExpr = conj(_Kind, SubGoals)
        ; GoalExpr = disj(SubGoals)
        ),
        ( if last(SubGoals, LastSubGoal) then
            find_first_context(LastSubGoal, FirstContext)
        else
            FirstContext = goal_info_get_context(GoalInfo)
        )
    ;
        GoalExpr = switch(_, _, Cases),
        ( if last(Cases, LastCase) then
            LastCase = case(_MainConsId, _OtherConsIds, SubGoal),
            find_first_context(SubGoal, FirstContext)
        else
            FirstContext = goal_info_get_context(GoalInfo)
        )
    ;
        ( GoalExpr = negation(SubGoal)
        ; GoalExpr = scope(_Reason, SubGoal)
        ),
        find_first_context(SubGoal, FirstContext)
    ;
        GoalExpr = if_then_else(_Vars, _Cond, _Then, Else),
        find_first_context(Else, FirstContext)
    ;
        GoalExpr = shorthand(Shorthand),
        (
            Shorthand = bi_implication(_SubGoalA, SubGoalB),
            find_first_context(SubGoalB, FirstContext)
        ;
            Shorthand = atomic_goal(_Type, _Outer, _Inner, _OutputVars,
                MainGoal, OrElseGoals, _Inners),
            ( if last(OrElseGoals, LastOrElseGoal) then
                find_first_context(LastOrElseGoal, FirstContext)
            else
                find_first_context(MainGoal, FirstContext)
            )
        ;
            Shorthand = try_goal(_MaybeIO, _ResultVar, SubGoal),
            find_first_context(SubGoal, FirstContext)
        )
    ).

%-----------------------------------------------------------------------------%

:- pred compare_pred_line_counts_by_pred_name(
    pred_line_count::in, pred_line_count::in, comparison_result::out) is det.

compare_pred_line_counts_by_pred_name(PredLineCountA, PredLineCountB,
        Result) :-
    PredLineCountA = pred_line_count(PredNameArityStrA, _FileNameA,
        _FirstLineA, _LastLineA, _LineCountA),
    PredLineCountB = pred_line_count(PredNameArityStrB, _FileNameB,
        _FirstLineB, _LastLineB, _LineCountB),
    compare(Result, PredNameArityStrA, PredNameArityStrB).

:- pred compare_pred_line_counts_by_start_line_number(
    pred_line_count::in, pred_line_count::in, comparison_result::out) is det.

compare_pred_line_counts_by_start_line_number(PredLineCountA, PredLineCountB,
        Result) :-
    PredLineCountA = pred_line_count(_PredNameArityStrA, _FileNameA,
        FirstLineA, _LastLineA, _LineCountA),
    PredLineCountB = pred_line_count(_PredNameArityStrB, _FileNameB,
        FirstLineB, _LastLineB, _LineCountB),
    compare(Result, FirstLineA, FirstLineB).

%-----------------------------------------------------------------------------%

:- pred acc_max_predname_arity_str_len(pred_line_count::in,
    int::in, int::out) is det.

acc_max_predname_arity_str_len(PredLineCount, !MaxLen) :-
    PredLineCount = pred_line_count(PredNameArityStr, _, _, _, _),
    string.length(PredNameArityStr, Len),
    int.max(Len, !MaxLen).

%-----------------------------------------------------------------------------%

:- pred write_pred_line_count(io.text_output_stream::in, int::in,
    pred_line_count::in, io::di, io::uo) is det.

write_pred_line_count(Stream, NameWidth, PredLineCount, !IO) :-
    PredLineCount = pred_line_count(PredNameArityStr, FileName,
        _FirstLine, _LastLine, LineCount),
    io.format(Stream, "%-*s %-30s %6d\n",
        [i(NameWidth), s(PredNameArityStr), s(FileName), i(LineCount)], !IO).

:- pred write_pred_extent(io.text_output_stream::in, int::in,
    pred_line_count::in, io::di, io::uo) is det.

write_pred_extent(Stream, NameWidth, PredLineCount, !IO) :-
    PredLineCount = pred_line_count(PredNameArityStr, _FileName,
        FirstLine, LastLine, _LineCount),
    io.format(Stream, "%-*s %6d to %6d\n",
        [i(NameWidth), s(PredNameArityStr), i(FirstLine), i(LastLine)], !IO).

%-----------------------------------------------------------------------------%
:- end_module hlds.hlds_defns.
%-----------------------------------------------------------------------------%
