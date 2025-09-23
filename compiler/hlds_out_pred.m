%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2009-2012 The University of Melbourne.
% Copyright (C) 2014-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: hlds_out_pred.m.
% Main authors: conway, fjh.
%
%---------------------------------------------------------------------------%

:- module hlds.hlds_out.hlds_out_pred.
:- interface.

:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.hlds_pred.
:- import_module libs.
:- import_module libs.indent.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.var_db.
:- import_module parse_tree.var_table.

:- import_module io.
:- import_module list.
:- import_module string.
:- import_module string.builder.

%---------------------------------------------------------------------------%

    % write_pred(Info, Stream, Lang, ModuleInfo, PredId, PredInfo, !IO):
    %
:- pred write_pred(hlds_out_info::in, io.text_output_stream::in,
    output_lang::in, module_info::in, pred_id::in, pred_info::in,
    io::di, io::uo) is det.
:- pred format_pred(hlds_out_info::in, output_lang::in, module_info::in,
    pred_id::in, pred_info::in,
    string.builder.state::di, string.builder.state::uo) is det.

:- type write_which_modes
    --->    write_actual_modes
    ;       write_declared_modes.

    % write_clause(Info, Stream, Lang, ModuleInfo, PredId, PredOrFunc,
    %   VarTable, TypeQual, VarNamePrint, WriteWhichModes, Indent,
    %   HeadTerms, Clause, !IO).
    %
:- pred write_clause(hlds_out_info::in, io.text_output_stream::in,
    output_lang::in, module_info::in, pred_id::in, pred_or_func::in,
    var_name_source::in, type_qual::in,
    var_name_print::in, write_which_modes::in, indent::in,
    list(prog_term)::in, clause::in, io::di, io::uo) is det.
:- pred format_clause(hlds_out_info::in, output_lang::in, module_info::in,
    pred_id::in, pred_or_func::in, var_name_source::in, type_qual::in,
    var_name_print::in, write_which_modes::in, indent::in,
    list(prog_term)::in, clause::in,
    string.builder.state::di, string.builder.state::uo) is det.

%---------------------------------------------------------------------------%

:- pred write_table_arg_infos(io.text_output_stream::in, tvarset::in,
    table_arg_infos::in, io::di, io::uo) is det.
:- pred format_table_arg_infos(tvarset::in, table_arg_infos::in,
    string.builder.state::di, string.builder.state::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_args.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_markers.
:- import_module hlds.hlds_out.hlds_out_goal.
:- import_module hlds.hlds_proc_util.
:- import_module hlds.hlds_rtti.
:- import_module hlds.pred_name.
:- import_module hlds.status.
:- import_module hlds.var_table_hlds.
:- import_module libs.globals.
:- import_module libs.trace_params.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.program_representation.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.parse_tree_out_pragma.
:- import_module parse_tree.parse_tree_out_pred_decl.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_tree_out_type.
:- import_module parse_tree.parse_tree_to_term.
:- import_module parse_tree.prog_ctgc.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.
:- import_module transform_hlds.
:- import_module transform_hlds.term_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module term.
:- import_module term_subst.
:- import_module uint.
:- import_module varset.

%---------------------------------------------------------------------------%
%
% Write out predicates.
%

write_pred(Info, Stream, Lang, ModuleInfo, PredId, PredInfo, !IO) :-
    State0 = string.builder.init,
    format_pred(Info, Lang, ModuleInfo, PredId, PredInfo, State0, State),
    Str = string.builder.to_string(State),
    io.write_string(Stream, Str, !IO).

format_pred(Info, Lang, ModuleInfo, PredId, PredInfo, !State) :-
    PredModuleName = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    pred_info_get_arg_types(PredInfo, ArgTypes),
    pred_info_get_exist_quant_tvars(PredInfo, ExistQVars),
    pred_info_get_typevarset(PredInfo, TVarSet),
    pred_info_get_clauses_info(PredInfo, ClausesInfo),
    pred_info_get_status(PredInfo, PredStatus),
    pred_info_get_markers(PredInfo, Markers),
    pred_info_get_class_context(PredInfo, ClassContext),
    pred_info_get_constraint_proof_map(PredInfo, ProofMap),
    pred_info_get_constraint_map(PredInfo, ConstraintMap),
    pred_info_get_purity(PredInfo, Purity),
    pred_info_get_external_type_params(PredInfo, ExternalTypeParams),
    pred_info_get_var_name_remap(PredInfo, VarNameRemap),
    DumpOptions = Info ^ hoi_dump_hlds_options,
    DumpVarNums = DumpOptions ^ dump_var_numbers_in_names,
    (
        DumpVarNums = yes,
        VarNamePrint = print_name_and_num
    ;
        DumpVarNums = no,
        VarNamePrint = print_name_only
    ),
    pred_info_get_proc_table(PredInfo, ProcTable),
    map.to_assoc_list(ProcTable, ProcIdsInfos),
    find_filled_in_procs(ProcIdsInfos, FilledInProcIdsInfos),
    DumpClauses = DumpOptions ^ dump_clauses,
    (
        DumpClauses = yes,
        % Information about predicates is dumped if 'C' suboption is on.
        PredSymName = qualified(PredModuleName, PredName),
        (
            PredOrFunc = pf_predicate,
            MaybeDet = maybe.no,
            mercury_format_pred_type(TVarSet, VarNamePrint, ExistQVars,
                PredSymName, ArgTypes, MaybeDet, Purity, ClassContext,
                string.builder.handle, !State)
        ;
            PredOrFunc = pf_function,
            pred_args_to_func_args(ArgTypes, FuncArgTypes, FuncRetType),
            MaybeDet = maybe.no,
            mercury_format_func_type(TVarSet, VarNamePrint, ExistQVars,
                PredSymName, FuncArgTypes, FuncRetType, MaybeDet, Purity,
                ClassContext, string.builder.handle, !State)
        ),
        ClausesInfo = clauses_info(VarSet, ExplicitVarTypes,
            VarTable0, RttiVarMaps, _TVarSet, HeadVars, ClausesRep,
            _ItemNumbers, _HaveForeignClauses, _HadSyntaxErrors),
        ( if varset.is_empty(VarSet) then
            VarNameSrc = vns_var_table(VarTable0)
        else
            VarNameSrc = vns_varset(VarSet)
        ),
        ( if var_table_is_empty(VarTable0) then
            make_var_table(ModuleInfo, VarSet, ExplicitVarTypes, VarTable)
        else
            VarTable = VarTable0
        ),
        pred_id_to_int(PredId, PredIdInt),
        PredOrFuncStr = pred_or_func_to_full_str(PredOrFunc),
        PredStatusStr = pred_import_status_to_string(PredStatus),
        pred_info_get_goal_type(PredInfo, GoalType),

        string.builder.format("%% pred id: %d, category: %s, status %s\n",
            [i(PredIdInt), s(PredOrFuncStr), s(PredStatusStr)], !State),
        string.builder.format("%% goal_type: %s\n",
            [s(string.string(GoalType))], !State),

        format_pred_markers(Markers, !State),
        pred_info_get_obsolete_in_favour_of(PredInfo, MaybeObsoleteInFavourOf),
        (
            MaybeObsoleteInFavourOf = no
        ;
            MaybeObsoleteInFavourOf = yes(ObsoleteInFavourOf),
            string.builder.append_string("% obsolete in favour of one of\n",
                !State),
            list.foldl(format_obsolete_in_favour_of,
                ObsoleteInFavourOf, !State)
        ),
        pred_info_get_format_call_info(PredInfo, MaybeFormatCall),
        (
            MaybeFormatCall = no
        ;
            MaybeFormatCall = yes(FormatCall),
            FormatCall = format_call_info(_Context, OoMFormatStringValues),
            FormatStringValues = one_or_more_to_list(OoMFormatStringValues),
            output_format_string_values(FormatStringValues, !State)
        ),
        format_pred_types(VarNamePrint, TVarSet, VarTable, RttiVarMaps,
            ProofMap, ConstraintMap, ExternalTypeParams, !State),
        format_pred_proc_var_name_remap(VarNameSrc, VarNameRemap, !State),

        get_clause_list_maybe_repeated(ClausesRep, Clauses),
        (
            FilledInProcIdsInfos = [],
            GetProcNum =
                ( func(ProcId - _) = N :-
                    proc_id_to_int(ProcId, N)
                ),
            ProcNums = list.map(GetProcNum, ProcIdsInfos),
            FilledInProcNums = list.map(GetProcNum, FilledInProcIdsInfos),
            string.builder.format("%% procedures: %s, filled in %s\n",
                [s(string.string(ProcNums)),
                s(string.string(FilledInProcNums))], !State),
            (
                Clauses = [],
                ClauseCountReport = report_clause_count(Clauses),
                string.builder.format("%% clause count: %s\n",
                    [s(ClauseCountReport)], !State)
            ;
                Clauses = [_ | _],
                set_dump_opts_for_clauses(Info, InfoForClauses),
                format_clauses(InfoForClauses, Lang, ModuleInfo,
                    PredId, PredOrFunc, VarNameSrc, no_tvarset_var_table,
                    VarNamePrint, HeadVars, Clauses, !State)
            )
        ;
            FilledInProcIdsInfos = [_ | _],
            ClauseCountReport = report_clause_count(Clauses),
            string.builder.format("%% clause count: %s\n",
                [s(ClauseCountReport)], !State)
        ),

        pred_info_get_origin(PredInfo, Origin),
        OriginStr = dump_origin(TVarSet, VarNamePrint, "% origin: ", Origin),
        string.builder.append_string(OriginStr, !State),
        PrintedPred = yes
    ;
        DumpClauses = no,
        PrintedPred = no
    ),

    (
        FilledInProcIdsInfos = [_ | _],
        format_procs_loop(Info, VarNamePrint, ModuleInfo, PredId, PredInfo,
            FilledInProcIdsInfos, !State),
        PrintedProc = yes
    ;
        FilledInProcIdsInfos = [],
        PrintedProc = no
    ),

    ( if
        ( PrintedPred = yes
        ; PrintedProc = yes
        )
    then
        string.builder.append_string("\n", !State)
    else
        true
    ).

:- pred output_format_string_values(list(format_string_values)::in,
    string.builder.state::di, string.builder.state::uo) is det.

output_format_string_values([], !State).
output_format_string_values([FmtStringValue | FmtStringValues], !State) :-
    FmtStringValue = format_string_values(OrigFmtStr, OrigValues,
        CurFmtStr, CurValues),
    string.builder.format(
        "%% format call: format string in arg %d/%d, values in arg %d/%d\n",
        [i(OrigFmtStr), i(CurFmtStr), i(OrigValues), i(CurValues)], !State),
    output_format_string_values(FmtStringValues, !State).

:- func report_clause_count(list(clause)) = string.

report_clause_count(Clauses) = Report :-
    count_clause_langs(Clauses, 0, Mer, 0, C, 0, Cs, 0, Java),
    Total = Mer + C + Cs + Java,
    ( if Total = 0 then
        Report = "0"
    else
        MerStr = clause_count_to_str("Mercury", Mer),
        CStr = clause_count_to_str("C", C),
        CsStr = clause_count_to_str("C#", Cs),
        JavaStr = clause_count_to_str("Java", Java),
        list.filter(unify(""), [MerStr, CStr, CsStr, JavaStr], _, LangStrs),
        (
            LangStrs = [],
            unexpected($pred, "LangStrs = []")
        ;
            LangStrs = [Report]
        ;
            LangStrs = [_, _  | _],
            LangsStr = string.join_list(", ", LangStrs),
            string.format("%d total (%s)", [i(Total), s(LangsStr)], Report)
        )
    ).

:- pred count_clause_langs(list(clause)::in,
    int::in, int::out, int::in, int::out, int::in, int::out, int::in, int::out)
    is det.

count_clause_langs([], !Mer, !C, !Cs, !Java).
count_clause_langs([Clause | Clauses], !Mer, !C, !Cs, !Java) :-
    Lang = Clause ^ clause_lang,
    (
        Lang = impl_lang_mercury,
        !:Mer = !.Mer + 1
    ;
        Lang = impl_lang_foreign(ForeignLang),
        (
            ForeignLang = lang_c,
            !:C = !.C + 1
        ;
            ForeignLang = lang_csharp,
            !:Cs = !.Cs + 1
        ;
            ForeignLang = lang_java,
            !:Java = !.Java + 1
        )
    ),
    count_clause_langs(Clauses, !Mer, !C, !Cs, !Java).

:- func clause_count_to_str(string, int) = string.

clause_count_to_str(LangStr, Count) = LangReport :-
    ( if Count = 0 then
        LangReport = ""
    else
        string.format("%d %s", [i(Count), s(LangStr)], LangReport)
    ).

%---------------------%

:- pred find_filled_in_procs(assoc_list(proc_id, proc_info)::in,
    assoc_list(proc_id, proc_info)::out) is det.

find_filled_in_procs([], []).
find_filled_in_procs([ProcIdInfo | ProcIdsInfos], FilledInProcIdsInfos) :-
    find_filled_in_procs(ProcIdsInfos, TailFilledInProcIdsInfos),
    ProcIdInfo = _ProcId - ProcInfo,
    proc_info_get_goal(ProcInfo, Goal),
    Goal = hlds_goal(GoalExpr, _),
    ( if GoalExpr = conj(plain_conj, []) then
        FilledInProcIdsInfos = TailFilledInProcIdsInfos
    else
        FilledInProcIdsInfos = [ProcIdInfo | TailFilledInProcIdsInfos]
    ).

%---------------------%

:- pred format_pred_markers(pred_markers::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_pred_markers(Markers, !State) :-
    markers_to_marker_list(Markers, MarkerList),
    (
        MarkerList = []
    ;
        MarkerList = [_ | _],
        list.map(marker_name, MarkerList, MarkerNames),
        MarkerNamesStr = string.join_list(", ", MarkerNames),
        string.builder.format("%% markers: %s\n", [s(MarkerNamesStr)], !State)
    ).

%---------------------%

:- pred format_obsolete_in_favour_of(sym_name_arity::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_obsolete_in_favour_of(ObsoleteInFavourOf, !State) :-
    ObsoleteInFavourOf = sym_name_arity(SymName, Arity),
    string.builder.format("%%    %s/%d\n",
        [s(sym_name_to_string(SymName)), i(Arity)], !State).

%---------------------%

:- pred format_pred_types(var_name_print::in, tvarset::in, var_table::in,
    rtti_varmaps::in, constraint_proof_map::in, constraint_map::in,
    list(tvar)::in, string.builder.state::di, string.builder.state::uo) is det.

format_pred_types(VarNamePrint, TVarSet, VarTable, RttiVarMaps,
        ProofMap, ConstraintMap, ExternalTypeParams, !State) :-
    format_rtti_varmaps(VarNamePrint, TVarSet, VarTable, RttiVarMaps, !State),
    ( if map.is_empty(ProofMap) then
        true
    else
        format_constraint_proof_map(0u, VarNamePrint, TVarSet,
            ProofMap, !State),
        string.builder.append_string("\n", !State)
    ),
    ( if map.is_empty(ConstraintMap) then
        true
    else
        format_constraint_map(VarNamePrint, TVarSet, ConstraintMap, !State)
    ),
    (
        ExternalTypeParams = []
    ;
        ExternalTypeParams = [_ | _],
        string.builder.append_string("% external_type_params:\n", !State),
        string.builder.append_string("% ", !State),
        mercury_format_vars_vs(TVarSet, VarNamePrint, ExternalTypeParams,
            string.builder.handle, !State),
        string.builder.append_string("\n", !State)
    ),
    format_var_types(VarNamePrint, TVarSet, VarTable, !State).

:- pred format_constraint_map(var_name_print::in, tvarset::in,
    constraint_map::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_constraint_map(VarNamePrint, VarSet, ConstraintMap, !State) :-
    string.builder.append_string("% constraint map:\n", !State),
    map.foldl(format_constraint_map_entry(VarNamePrint, VarSet),
        ConstraintMap, !State).

:- pred format_constraint_map_entry(var_name_print::in, tvarset::in,
    constraint_id::in, prog_constraint::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_constraint_map_entry(VarNamePrint, VarSet,
        ConstraintId, ProgConstraint, !State) :-
    string.builder.append_string("% ", !State),
    format_constraint_id(ConstraintId, !State),
    string.builder.append_string(": ", !State),
    mercury_format_constraint(VarSet, VarNamePrint, ProgConstraint,
        string.builder.handle, !State),
    string.builder.append_string("\n", !State).

:- pred format_constraint_id(constraint_id::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_constraint_id(ConstraintId, !State) :-
    ConstraintId = constraint_id(ConstraintType, GoalId, N),
    (
        ConstraintType = assumed,
        ConstraintTypeChar = 'E'
    ;
        ConstraintType = unproven,
        ConstraintTypeChar = 'A'
    ),
    GoalId = goal_id(GoalIdNum),
    string.builder.format("(%c, %u, %d)",
        [c(ConstraintTypeChar), u(GoalIdNum), i(N)], !State).

%---------------------------------------------------------------------------%

:- pred set_dump_opts_for_clauses(hlds_out_info::in, hlds_out_info::out)
    is det.

set_dump_opts_for_clauses(Info, ClausesInfo) :-
    GenOptions = Info ^ hoi_dump_hlds_options,
    some [!ClauseOptions] (
        % Nuke all dump options except the ones explicitly copied below.
        !:ClauseOptions = empty_dump_options,
        !ClauseOptions ^ dump_goal_type_contexts :=
            GenOptions ^ dump_goal_type_contexts,
        !ClauseOptions ^ dump_goal_nonlocals :=
            GenOptions ^ dump_goal_nonlocals,
        !ClauseOptions ^ dump_var_numbers_in_names :=
            GenOptions ^ dump_var_numbers_in_names,
        !ClauseOptions ^ dump_goal_features :=
            GenOptions ^ dump_goal_features,
        !ClauseOptions ^ dump_goal_ids_paths :=
            GenOptions ^ dump_goal_ids_paths,
        ClausesInfo = Info ^ hoi_dump_hlds_options := !.ClauseOptions
    ).

:- pred format_clauses(hlds_out_info::in, output_lang::in, module_info::in,
    pred_id::in, pred_or_func::in, var_name_source::in, type_qual::in,
    var_name_print::in, proc_arg_vector(prog_var)::in, list(clause)::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_clauses(Info, Lang, ModuleInfo, PredId, PredOrFunc, VarNameSrc,
        TypeQual, VarNamePrint, HeadVarsVector, Clauses, !State) :-
    HeadVars = proc_arg_vector_to_list(HeadVarsVector),
    term_subst.var_list_to_term_list(HeadVars, HeadTerms),
    format_clauses_loop(Info, Lang, ModuleInfo, PredId, PredOrFunc,
        VarNameSrc, TypeQual, VarNamePrint, HeadTerms, 1, Clauses, !State).

:- pred format_clauses_loop(hlds_out_info::in,
    output_lang::in, module_info::in, pred_id::in, pred_or_func::in,
    var_name_source::in, type_qual::in, var_name_print::in,
    list(prog_term)::in, int::in, list(clause)::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_clauses_loop(Info, Lang, ModuleInfo, PredId, PredOrFunc,
        VarNameSrc, TypeQual, VarNamePrint, HeadTerms,
        CurClauseNum, Clauses, !State) :-
    (
        Clauses = []
    ;
        Clauses = [FirstClause | LaterClauses],
        string.builder.format("%% clause %d\n", [i(CurClauseNum)], !State),
        format_clause(Info, Lang, ModuleInfo, PredId, PredOrFunc,
            VarNameSrc, TypeQual, VarNamePrint, write_actual_modes, 0u,
            HeadTerms, FirstClause, !State),
        format_clauses_loop(Info, Lang, ModuleInfo, PredId, PredOrFunc,
            VarNameSrc, TypeQual, VarNamePrint, HeadTerms,
            CurClauseNum + 1, LaterClauses, !State)
    ).

write_clause(Info, Stream, Lang, ModuleInfo, PredId, PredOrFunc, VarNameSrc,
        TypeQual, VarNamePrint, WriteWhichModes, Indent, HeadTerms,
        Clause, !IO) :-
    State0 = string.builder.init,
    format_clause(Info, Lang, ModuleInfo, PredId, PredOrFunc, VarNameSrc,
        TypeQual, VarNamePrint, WriteWhichModes, Indent, HeadTerms,
        Clause, State0, State),
    Str = string.builder.to_string(State),
    io.write_string(Stream, Str, !IO).

format_clause(Info, Lang, ModuleInfo, PredId, PredOrFunc, VarNameSrc,
        TypeQual, VarNamePrint, WriteWhichModes, Indent, HeadTerms,
        Clause, !State) :-
    Clause = clause(ApplicableModes, Goal, ImplLang, Context,
        _StateVarWarnings, _UnusedSVarDescs, _MaybeFact),
    IndentStr = indent2_string(Indent),
    Indent1 = Indent + 1u,
    DumpOptions = Info ^ hoi_dump_hlds_options,
    DumpClauseModes = DumpOptions ^ dump_clause_modes,
    (
        ApplicableModes = all_modes
    ;
        ApplicableModes = selected_modes(Modes),
        (
            DumpClauseModes = yes,
            string.builder.format(
                "%s%% modes for which this clause applies: ",
                [s(IndentStr)], !State),
            ModeInts = list.map(proc_id_to_int, Modes),
            format_intlist(ModeInts, !State),
            string.builder.append_string("\n", !State)
        ;
            DumpClauseModes = no
        )
    ;
        ApplicableModes = unify_in_in_modes,
        (
            DumpClauseModes = yes,
            string.builder.format(
                "%s%% this clause applies only to <in,in> unify modes.\n",
                [s(IndentStr)], !State)
        ;
            DumpClauseModes = no
        )
    ;
        ApplicableModes = unify_non_in_in_modes,
        (
            DumpClauseModes = yes,
            string.builder.format(
                "%s%% this clause applies only to non <in,in> unify modes.\n",
                [s(IndentStr)], !State)
        ;
            DumpClauseModes = no
        )
    ),
    (
        ImplLang = impl_lang_mercury
    ;
        ImplLang = impl_lang_foreign(ForeignLang),
        string.builder.format("%s%% language of implementation: %s\n",
            [s(IndentStr), s(foreign_language_string(ForeignLang))], !State)
    ),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    AllProcIds = pred_info_all_procids(PredInfo),
    ( if
        ApplicableModes = selected_modes(SelectedProcIds),
        SelectedProcIds \= AllProcIds
    then
        % If SelectedProcIds contains more than one mode, the output will have
        % multiple clause heads. This won't be pretty and it won't be
        % syntactically valid, but it is more useful for debugging
        % than a compiler abort during the dumping process.
        format_annotated_clause_heads(ModuleInfo, Lang, VarNameSrc,
            VarNamePrint, WriteWhichModes, PredId, PredOrFunc, SelectedProcIds,
            Context, HeadTerms, !State)
    else
        format_clause_head(ModuleInfo, VarNameSrc, VarNamePrint,
            PredId, PredOrFunc, HeadTerms, !State)
    ),
    ( if Goal = hlds_goal(conj(plain_conj, []), _GoalInfo) then
        string.builder.append_string(".\n", !State)
    else
        string.builder.append_string(" :-\n", !State),
        pred_info_get_typevarset(PredInfo, TVarSet),
        varset.init(InstVarSet),
        InfoGoal = hlds_out_info_goal(Info, ModuleInfo,
            VarNameSrc, VarNamePrint, TVarSet, InstVarSet, TypeQual),
        do_format_goal(InfoGoal, Indent1, ".\n", Goal, !State)
    ).

:- pred format_annotated_clause_heads(module_info::in, output_lang::in,
    var_name_source::in, var_name_print::in, write_which_modes::in,
    pred_id::in, pred_or_func::in, list(proc_id)::in, term.context::in,
    list(prog_term)::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_annotated_clause_heads(_, _, _, _, _, _, _, [], _, _, !State).
format_annotated_clause_heads(ModuleInfo, Lang, VarNameSrc,
        VarNamePrint, WriteWhichModes, PredId, PredOrFunc, [ProcId | ProcIds],
        Context, HeadTerms, !State) :-
    format_annotated_clause_head(ModuleInfo, Lang, VarNameSrc,
        VarNamePrint, WriteWhichModes, PredId, PredOrFunc, ProcId,
        Context, HeadTerms, !State),
    format_annotated_clause_heads(ModuleInfo, Lang, VarNameSrc,
        VarNamePrint, WriteWhichModes, PredId, PredOrFunc, ProcIds,
        Context, HeadTerms, !State).

:- pred format_annotated_clause_head(module_info::in, output_lang::in,
    var_name_source::in, var_name_print::in, write_which_modes::in,
    pred_id::in, pred_or_func::in, proc_id::in, term.context::in,
    list(prog_term)::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_annotated_clause_head(ModuleInfo, Lang, VarNameSrc, VarNamePrint,
        WriteWhichModes, PredId, PredOrFunc, ProcId,
        Context, HeadTerms, !State) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_proc_table(PredInfo, Procedures),
    ( if map.search(Procedures, ProcId, ProcInfo) then
        % When writing `.opt' files, use the declared argument modes so that
        % the modes are guaranteed to be syntactically identical to those
        % in the original program. The test in add_clause.m to check whether
        % a clause matches a procedure tests for syntactic identity (roughly).
        % The modes returned by proc_info_get_argmodes may have been slightly
        % expanded by propagate_types_into_modes.
        %
        % We can't use the declared argument modes when writing HLDS dumps
        % because the modes of the type-infos will not have been added,
        % so the call to assoc_list.from_corresponding_lists below
        % will abort. `.opt' files are written before the polymorphism pass.
        (
            WriteWhichModes = write_actual_modes,
            proc_info_get_argmodes(ProcInfo, ArgModes)
        ;
            WriteWhichModes = write_declared_modes,
            proc_info_declared_argmodes(ProcInfo, ArgModes)
        ),
        assoc_list.from_corresponding_lists(HeadTerms, ArgModes,
            AnnotatedPairs),
        AnnotatedHeadTerms = list.map(add_mode_qualifier(Lang, Context),
            AnnotatedPairs),
        format_clause_head(ModuleInfo, VarNameSrc,
            VarNamePrint, PredId, PredOrFunc, AnnotatedHeadTerms, !State)
    else
        % This procedure, even though it existed in the past, has been
        % eliminated.
        true
    ).

:- func add_mode_qualifier(output_lang, prog_context,
    pair(prog_term, mer_mode)) = prog_term.

add_mode_qualifier(Lang, Context, HeadTerm - Mode) = AnnotatedTerm :-
    construct_qualified_term_with_context(unqualified("::"),
        [HeadTerm, mode_to_term_with_context(Lang, Context, Mode)],
        Context, AnnotatedTerm).

:- pred format_clause_head(module_info::in, var_name_source::in,
    var_name_print::in, pred_id::in, pred_or_func::in, list(prog_term)::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_clause_head(ModuleInfo, VarNameSrc, VarNamePrint, PredId,
        PredOrFunc, HeadTerms, !State) :-
    PredName = predicate_name(ModuleInfo, PredId),
    ModuleName = predicate_module(ModuleInfo, PredId),
    (
        PredOrFunc = pf_function,
        pred_args_to_func_args(HeadTerms, FuncArgs, RetVal),
        ArgsStr = qualified_functor_with_term_args_to_string(VarNameSrc,
            VarNamePrint, ModuleName, term.atom(PredName), FuncArgs),
        string.builder.append_string(ArgsStr, !State),
        string.builder.append_string(" = ", !State),
        mercury_format_term_nq_src(VarNameSrc, VarNamePrint,
            next_to_graphic_token, RetVal, string.builder.handle, !State)
    ;
        PredOrFunc = pf_predicate,
        ArgsStr = qualified_functor_with_term_args_to_string(VarNameSrc,
            VarNamePrint, ModuleName, term.atom(PredName), HeadTerms),
        string.builder.append_string(ArgsStr, !State)
    ).

%---------------------------------------------------------------------------%
%
% Write out procedures.
%

:- pred format_procs_loop(hlds_out_info::in, var_name_print::in,
    module_info::in, pred_id::in, pred_info::in,
    assoc_list(proc_id, proc_info)::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_procs_loop(_, _, _, _, _, [], !State).
format_procs_loop(Info, VarNamePrint, ModuleInfo,
        PredId, PredInfo, [ProcId - ProcInfo | ProcIdsInfos], !State) :-
    format_proc(Info, VarNamePrint, ModuleInfo,
        PredId, PredInfo, ProcId, ProcInfo, !State),
    format_procs_loop(Info, VarNamePrint, ModuleInfo,
        PredId, PredInfo, ProcIdsInfos, !State).

:- pred format_proc(hlds_out_info::in, var_name_print::in, module_info::in,
    pred_id::in, pred_info::in, proc_id::in, proc_info::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_proc(Info, VarNamePrint, ModuleInfo, PredId, PredInfo,
        ProcId, ProcInfo, !State) :-
    pred_info_get_typevarset(PredInfo, TVarSet),
    proc_info_get_can_process(ProcInfo, CanProcess),
    proc_info_get_var_table(ProcInfo, VarTable),
    proc_info_get_declared_determinism(ProcInfo, DeclaredDeterminism),
    proc_info_get_inferred_determinism(ProcInfo, InferredDeterminism),
    proc_info_get_headvars(ProcInfo, HeadVars),
    proc_info_get_argmodes(ProcInfo, HeadModes),
    proc_info_get_maybe_arglives(ProcInfo, MaybeArgLives),
    proc_info_get_reg_r_headvars(ProcInfo, RegR_HeadVars),
    proc_info_get_maybe_arg_info(ProcInfo, MaybeArgInfos),
    proc_info_get_goal(ProcInfo, Goal),
    proc_info_get_maybe_arg_size_info(ProcInfo, MaybeArgSize),
    proc_info_get_maybe_termination_info(ProcInfo, MaybeTermination),
    proc_info_get_structure_sharing(ProcInfo, MaybeStructureSharing),
    proc_info_get_structure_reuse(ProcInfo, MaybeStructureReuse),
    proc_info_get_rtti_varmaps(ProcInfo, RttiVarMaps),
    proc_info_get_cse_nopull_contexts(ProcInfo, CseNoPullContexts),
    proc_info_get_eval_method(ProcInfo, EvalMethod),
    proc_info_get_deleted_call_callees(ProcInfo, DeletedCallCalleeSet),
    proc_info_get_is_address_taken(ProcInfo, IsAddressTaken),
    proc_info_get_has_parallel_conj(ProcInfo, HasParallelConj),
    proc_info_get_has_user_event(ProcInfo, HasUserEvent),
    proc_info_get_maybe_proc_table_io_info(ProcInfo, MaybeProcTableIOInfo),
    proc_info_get_call_table_tip(ProcInfo, MaybeCallTableTip),
    proc_info_get_maybe_deep_profile_info(ProcInfo, MaybeDeepProfileInfo),
    proc_info_get_maybe_untuple_info(ProcInfo, MaybeUntupleInfo),
    proc_info_get_var_name_remap(ProcInfo, VarNameRemap),
    Indent1 = 1u,
    Indent1Str = indent2_string(Indent1),

    DumpOptions = Info ^ hoi_dump_hlds_options,
    pred_id_to_int(PredId, PredIdInt),
    proc_id_to_int(ProcId, ProcIdInt),
    PredIdStr = pred_id_to_dev_string(ModuleInfo, PredId),
    DetismStr = determinism_to_string(InferredDeterminism),
    string.builder.format("%s%% pred id %d: %s\n",
        [s(Indent1Str), i(PredIdInt), s(PredIdStr)], !State),
    string.builder.format("%s%% mode number %d (%s)\n",
        [s(Indent1Str), i(ProcIdInt), s(DetismStr)], !State),

    format_var_types(VarNamePrint, TVarSet, VarTable, !State),
    format_rtti_varmaps(VarNamePrint, TVarSet, VarTable, RttiVarMaps, !State),

    format_proc_flags(CanProcess, IsAddressTaken,
        HasParallelConj, HasUserEvent, !State),
    string.builder.format("%% cse_nopull_contexts: %s\n",
        [s(string.string(CseNoPullContexts))], !State),
    format_proc_tabling_info(VarTable, TVarSet, VarNamePrint,
        EvalMethod, MaybeProcTableIOInfo, MaybeCallTableTip, !State),
    format_proc_deep_profiling_info(VarTable, VarNamePrint,
        MaybeDeepProfileInfo, !State),

    DumpTermination = DumpOptions ^ dump_termination_analysis,
    (
        DumpTermination = yes,
        format_proc_termination_info(MaybeArgSize, MaybeTermination, !State)
    ;
        DumpTermination = no
    ),

    format_proc_opt_info(DumpOptions, VarTable, TVarSet, VarNamePrint,
        MaybeStructureSharing, MaybeStructureReuse,
        MaybeUntupleInfo, !State),
    format_proc_deleted_callee_set(DeletedCallCalleeSet, !State),
    format_pred_proc_var_name_remap(vns_var_table(VarTable),
        VarNameRemap, !State),
    format_eff_trace_level(ModuleInfo, PredInfo, ProcInfo, !State),

    PredSymName = unqualified(predicate_name(ModuleInfo, PredId)),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    varset.init(ModeVarSet),
    (
        PredOrFunc = pf_predicate,
        MaybeWithInst = maybe.no,
        mercury_format_pred_mode_decl(output_debug, ModeVarSet,
            PredSymName, HeadModes, MaybeWithInst,
            DeclaredDeterminism, string.builder.handle, !State)
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(HeadModes, FuncHeadModes, RetHeadMode),
        mercury_format_func_mode_decl(output_debug, ModeVarSet,
            PredSymName, FuncHeadModes, RetHeadMode,
            DeclaredDeterminism, string.builder.handle, !State)
    ),
    format_proc_arg_info(DumpOptions, VarTable, VarNamePrint,
        MaybeArgLives, RegR_HeadVars, MaybeArgInfos, !State),
    pred_info_get_status(PredInfo, PredStatus),
    ( if
        PredStatus = pred_status(status_pseudo_imported),
        hlds_pred.in_in_unification_proc_id(ProcId)
    then
        true
    else
        proc_info_get_stack_slots(ProcInfo, StackSlots),
        format_stack_slots(VarTable, VarNamePrint, StackSlots, !State),
        term_subst.var_list_to_term_list(HeadVars, HeadTerms),
        format_clause_head(ModuleInfo, vns_var_table(VarTable),
            VarNamePrint, PredId, PredOrFunc, HeadTerms, !State),
        string.builder.append_string(" :-\n", !State),
        proc_info_get_inst_varset(ProcInfo, InstVarSet),
        format_goal(Info, ModuleInfo, vns_var_table(VarTable),
            VarNamePrint, TVarSet, InstVarSet, Indent1, ".\n", Goal, !State)
    ).

%---------------------------------------------------------------------------%

:- pred format_var_types(var_name_print::in, tvarset::in, var_table::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_var_types(VarNamePrint, TVarSet, VarTable, !State) :-
    var_table_count(VarTable, NumVars),
    string.builder.format("%% variable table (%d entries):\n",
        [i(NumVars)], !State),
    var_table_to_sorted_assoc_list(VarTable, VarsEntries),
    format_var_types_loop(VarNamePrint, TVarSet, VarsEntries, !State).

:- pred format_var_types_loop(var_name_print::in,
    tvarset::in, assoc_list(prog_var, var_table_entry)::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_var_types_loop(_, _, [], !State).
format_var_types_loop(VarNamePrint, TypeVarSet,
        [Var - Entry | VarsEntries], !State) :-
    Entry = vte(Name, Type, IsDummy),
    term.var_to_int(Var, VarNum),
    VarStr = mercury_var_raw_to_string(VarNamePrint, Var, Name),
    TypeStr = mercury_type_to_string(TypeVarSet, VarNamePrint, Type),
    (
        IsDummy = is_dummy_type,
        DummySuffix = " (dummy type)"
    ;
        IsDummy = is_not_dummy_type,
        DummySuffix = ""
    ),
    string.builder.format("%% var #%d, %s: %s%s\n",
        [i(VarNum), s(VarStr), s(TypeStr), s(DummySuffix)], !State),
    format_var_types_loop(VarNamePrint, TypeVarSet, VarsEntries, !State).

%---------------------%

:- pred format_rtti_varmaps(var_name_print::in, tvarset::in, var_table::in,
    rtti_varmaps::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_rtti_varmaps(VarNamePrint, TVarSet, VarTable, RttiVarMaps, !State) :-
    string.builder.append_string("% type_info varmap:\n", !State),
    rtti_varmaps_tvars(RttiVarMaps, TypeVars),
    list.foldl(
        format_type_info_locn(VarNamePrint, TVarSet, VarTable, RttiVarMaps),
        TypeVars, !State),
    string.builder.append_string("% typeclass_info varmap:\n", !State),
    rtti_varmaps_reusable_constraints(RttiVarMaps, Constraints),
    list.foldl(
        format_typeclass_info_var(VarNamePrint, TVarSet, VarTable,
            RttiVarMaps),
        Constraints, !State),
    string.builder.append_string("% rtti_var_info:\n", !State),
    rtti_varmaps_rtti_prog_vars(RttiVarMaps, ProgVars),
    list.foldl(
        format_rtti_var_info(VarNamePrint, TVarSet, VarTable, RttiVarMaps),
        ProgVars, !State).

:- pred format_type_info_locn(var_name_print::in,
    tvarset::in, var_table::in, rtti_varmaps::in, tvar::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_type_info_locn(VarNamePrint, TVarSet, VarTable, RttiVarMaps, TVar,
        !State) :-
    TVarStr = mercury_var_to_string_vs(TVarSet, VarNamePrint, TVar),
    term.var_to_int(TVar, TVarNum),
    string.builder.format("%% %s(number %d) -> ",
        [s(TVarStr), i(TVarNum)], !State),
    rtti_lookup_type_info_locn(RttiVarMaps, TVar, Locn),
    (
        Locn = type_info(Var),
        VarStr = mercury_var_to_string(VarTable, VarNamePrint, Var),
        string.builder.format("type_info(%s)", [s(VarStr)], !State)
    ;
        Locn = typeclass_info(Var, Index),
        VarStr = mercury_var_to_string(VarTable, VarNamePrint, Var),
        string.builder.format("typeclass_info(%s, %d)",
            [s(VarStr), i(Index)], !State)
    ),
    term.var_to_int(Var, VarNum),
    string.builder.format(" (number %d)\n", [i(VarNum)], !State).

:- pred format_typeclass_info_var(var_name_print::in, tvarset::in,
    var_table::in, rtti_varmaps::in, prog_constraint::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_typeclass_info_var(VarNamePrint, TVarSet, VarTable, RttiVarMaps,
        Constraint, !State) :-
    string.builder.append_string("% ", !State),
    mercury_format_constraint(TVarSet, VarNamePrint, Constraint,
        string.builder.handle, !State),
    string.builder.append_string(" -> ", !State),
    rtti_lookup_typeclass_info_var(RttiVarMaps, Constraint, Var),
    mercury_format_var(VarTable, VarNamePrint, Var,
        string.builder.handle, !State),
    string.builder.append_string("\n", !State).

:- pred format_rtti_var_info(var_name_print::in,
    tvarset::in, var_table::in, rtti_varmaps::in, prog_var::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_rtti_var_info(VarNamePrint, TVarSet, VarTable, RttiVarMaps, Var,
        !State) :-
    term.var_to_int(Var, VarNum),
    VarStr = mercury_var_to_string(VarTable, VarNamePrint, Var),
    string.builder.format("%% %s (number %d) -> ",
        [s(VarStr), i(VarNum)], !State),
    rtti_varmaps_var_info(RttiVarMaps, Var, VarInfo),
    (
        VarInfo = type_info_var(Type),
        string.builder.append_string("type_info for ", !State),
        mercury_format_type(TVarSet, VarNamePrint, Type,
            string.builder.handle, !State)
    ;
        VarInfo = typeclass_info_var(Constraint),
        string.builder.append_string("typeclass_info for ", !State),
        mercury_format_constraint(TVarSet, VarNamePrint, Constraint,
            string.builder.handle, !State)
    ;
        VarInfo = non_rtti_var,
        unexpected($pred, "non rtti var")
    ),
    string.builder.append_string("\n", !State).

%---------------------%

:- pred format_proc_flags(can_process::in, is_address_taken::in,
    has_parallel_conj::in, has_user_event::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_proc_flags(CanProcess, IsAddressTaken, HasParallelConj,
        HasUserEvent, !State) :-
    (
        CanProcess = can_process_now
    ;
        CanProcess = cannot_process_yet,
        string.builder.append_string("% cannot_process_yet\n", !State)
    ),
    (
        IsAddressTaken = address_is_taken,
        string.builder.append_string("% address is taken\n", !State)
    ;
        IsAddressTaken = address_is_not_taken,
        string.builder.append_string("% address is not taken\n", !State)
    ),
    (
        HasParallelConj = has_parallel_conj,
        string.builder.append_string(
            "% contains parallel conjunction\n", !State)
    ;
        HasParallelConj = has_no_parallel_conj,
        string.builder.append_string(
            "% does not contain parallel conjunction\n", !State)
    ),
    (
        HasUserEvent = has_user_event,
        string.builder.append_string("% contains user event\n", !State)
    ;
        HasUserEvent = has_no_user_event,
        string.builder.append_string("% does not contain user event\n", !State)
    ).

%---------------------%

:- pred format_proc_tabling_info(
    var_table::in, tvarset::in, var_name_print::in, eval_method::in,
    maybe(proc_table_io_info)::in, maybe(prog_var)::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_proc_tabling_info(VarTable, TVarSet, VarNamePrint,
        EvalMethod, MaybeProcTableIOInfo, MaybeCallTableTip, !State) :-
    (
        EvalMethod = eval_normal
    ;
        EvalMethod = eval_tabled(TabledMethod),
        string.builder.format("%% eval method: %s\n",
            [s(tabled_eval_method_to_string(TabledMethod))], !State)
    ),
    (
        MaybeProcTableIOInfo = yes(ProcTableIOInfo),
        format_proc_table_io_info(TVarSet, ProcTableIOInfo, !State)
    ;
        MaybeProcTableIOInfo = no
    ),
    (
        MaybeCallTableTip = yes(CallTableTip),
        string.builder.append_string("% call table tip: ", !State),
        mercury_format_var(VarTable, VarNamePrint, CallTableTip,
            string.builder.handle, !State),
        string.builder.append_string("\n", !State)
    ;
        MaybeCallTableTip = no
    ).

:- pred format_proc_table_io_info(tvarset::in, proc_table_io_info::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_proc_table_io_info(TVarSet, ProcTableIOInfo, !State) :-
    ProcTableIOInfo = proc_table_io_info(MaybeArgInfos),
    (
        MaybeArgInfos = no,
        string.builder.append_string(
            "% proc table io info: io tabled, no arg_infos\n", !State)
    ;
        MaybeArgInfos = yes(ArgInfos),
        string.builder.append_string(
            "% proc table io info: io tabled, arg_infos:\n", !State),
        format_table_arg_infos(TVarSet, ArgInfos, !State)
    ).

write_table_arg_infos(Stream, TVarSet, TableArgInfos, !IO) :-
    State0 = string.builder.init,
    format_table_arg_infos(TVarSet, TableArgInfos, State0, State),
    Str = string.builder.to_string(State),
    io.write_string(Stream, Str, !IO).

format_table_arg_infos(TVarSet, TableArgInfos, !State) :-
    TableArgInfos = table_arg_infos(ArgInfos, TVarMap),
    string.builder.append_string("% arg infos:\n", !State),
    list.foldl(format_table_arg_info(TVarSet), ArgInfos, !State),
    map.to_assoc_list(TVarMap, TVarPairs),
    (
        TVarPairs = []
    ;
        TVarPairs = [_ | _],
        string.builder.append_string("% type var map:\n", !State),
        list.foldl(format_table_tvar_map_entry(TVarSet), TVarPairs, !State)
    ).

:- pred format_table_arg_info(tvarset::in, table_arg_info::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_table_arg_info(TVarSet, ArgInfo, !State) :-
    ArgInfo = table_arg_info(HeadVarNum, HeadVarName, SlotNum, Type),
    TVarStr = mercury_type_to_string(TVarSet, print_name_and_num, Type),
    string.builder.format("%% %s / %d in slot %d, type %s\n",
        [s(HeadVarName), i(HeadVarNum), i(SlotNum), s(TVarStr)], !State).

:- pred format_table_tvar_map_entry(tvarset::in, pair(tvar, table_locn)::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_table_tvar_map_entry(TVarSet, TVar - Locn, !State) :-
    TVarStr = mercury_var_to_string_vs(TVarSet, print_name_and_num, TVar),
    string.builder.format("%% typeinfo for %s -> ", [s(TVarStr)], !State),
    (
        Locn = table_locn_direct(N),
        string.builder.format("direct in register %d\n", [i(N)], !State)
    ;
        Locn = table_locn_indirect(N, O),
        string.builder.format("indirect from register %d, offset %d\n",
            [i(N), i(O)], !State)
    ).

%---------------------%

:- pred format_proc_deep_profiling_info(var_table::in, var_name_print::in,
    maybe(deep_profile_proc_info)::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_proc_deep_profiling_info(VarTable, VarNamePrint,
        MaybeDeepProfileInfo, !State) :-
    (
        MaybeDeepProfileInfo = yes(DeepProfileInfo),
        DeepProfileInfo = deep_profile_proc_info(MaybeRecInfo,
            MaybeDeepLayout, _),
        (
            MaybeRecInfo = yes(DeepRecInfo),
            DeepRecInfo = deep_recursion_info(Role, _),
            string.builder.append_string("% deep recursion info: ", !State),
            (
                Role = deep_prof_inner_proc(DeepPredProcId),
                string.builder.append_string("inner, outer is ", !State)
            ;
                Role = deep_prof_outer_proc(DeepPredProcId),
                string.builder.append_string("outer, inner is ", !State)
            ),
            DeepPredProcId = proc(DeepPredId, DeepProcId),
            pred_id_to_int(DeepPredId, DeepPredInt),
            proc_id_to_int(DeepProcId, DeepProcInt),
            string.builder.format("%d/%d\n",
                [i(DeepPredInt), i(DeepProcInt)], !State)
        ;
            MaybeRecInfo = no
        ),
        (
            MaybeDeepLayout = yes(DeepLayout),
            DeepLayout = hlds_deep_layout(ProcStatic, ExcpVars),
            format_hlds_proc_static(ProcStatic, !State),
            ExcpVars = hlds_deep_excp_vars(TopCSD, MiddleCSD,
                MaybeOldOutermost),
            string.builder.append_string("% deep layout info: ", !State),
            string.builder.append_string("TopCSD is ", !State),
            mercury_format_var(VarTable, VarNamePrint, TopCSD,
                string.builder.handle, !State),
            string.builder.append_string(", MiddleCSD is ", !State),
            mercury_format_var(VarTable, VarNamePrint, MiddleCSD,
                string.builder.handle, !State),
            (
                MaybeOldOutermost = yes(OldOutermost),
                string.builder.append_string(", OldOutermost is ", !State),
                mercury_format_var(VarTable, VarNamePrint, OldOutermost,
                    string.builder.handle, !State)
            ;
                MaybeOldOutermost = no
            ),
            string.builder.append_string("\n", !State)
        ;
            MaybeDeepLayout = no
        )
    ;
        MaybeDeepProfileInfo = no
    ).

:- pred format_hlds_proc_static(hlds_proc_static::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_hlds_proc_static(ProcStatic, !State) :-
    ProcStatic = hlds_proc_static(FileName, LineNumber,
        InInterface, CallSiteStatics, CoveragePoints),
    string.builder.format("%% proc static filename: %s\n",
        [s(FileName)], !State),
    string.builder.format("%% proc static line number: %d\n",
        [i(LineNumber)], !State),
    string.builder.format("%% proc static is interface: %s\n",
        [s(string.string(InInterface))], !State),
    list.foldl2(format_hlds_ps_call_site,
        CallSiteStatics, 0, _, !State),
    list.foldl2(format_hlds_ps_coverage_point,
        CoveragePoints, 0, _, !State).

:- pred format_hlds_ps_call_site(call_site_static_data::in, int::in, int::out,
    string.builder.state::di, string.builder.state::uo) is det.

format_hlds_ps_call_site(CallSiteStaticData, !SlotNum, !State) :-
    string.builder.format("%% call site static slot %d\n",
        [i(!.SlotNum)], !State),
    (
        CallSiteStaticData = normal_call(CalleeRttiProcLabel, TypeSubst,
            FileName, LineNumber, GoalPath),
        string.builder.format("%% normal call to %s\n",
            [s(string.string(CalleeRttiProcLabel))], !State),
        string.builder.format("%% type subst <%s>, goal path <%s>\n",
            [s(TypeSubst), s(goal_path_to_string(GoalPath))], !State),
        string.builder.format("%% filename <%s>, line number <%d>\n",
            [s(FileName), i(LineNumber)], !State)
    ;
        (
            CallSiteStaticData = special_call(FileName, LineNumber, GoalPath),
            string.builder.append_string("% special call\n", !State)
        ;
            CallSiteStaticData = higher_order_call(FileName, LineNumber,
                GoalPath),
            string.builder.append_string("% higher order call\n", !State)
        ;
            CallSiteStaticData = method_call(FileName, LineNumber, GoalPath),
            string.builder.append_string("% method call\n", !State)
        ;
            CallSiteStaticData = callback(FileName, LineNumber, GoalPath),
            string.builder.append_string("% callback\n", !State)
        ),
        string.builder.format(
            "%% filename <%s>, line number <%d>, goal path <%s>\n",
            [s(FileName), i(LineNumber), s(goal_path_to_string(GoalPath))],
            !State)
    ),
    !:SlotNum = !.SlotNum + 1.

:- pred format_hlds_ps_coverage_point(coverage_point_info::in,
    int::in, int::out,
    string.builder.state::di, string.builder.state::uo) is det.

format_hlds_ps_coverage_point(CoveragePointInfo, !SlotNum, !State) :-
    CoveragePointInfo = coverage_point_info(RevGoalPath, PointType),
    string.builder.format(
        "%% coverage point slot %d: goal path <%s>, type %s\n",
        [i(!.SlotNum), s(rev_goal_path_to_string(RevGoalPath)),
            s(coverage_point_to_string(PointType))], !State),
    !:SlotNum = !.SlotNum + 1.

:- func coverage_point_to_string(cp_type) = string.

coverage_point_to_string(cp_type_coverage_after) = "after".
coverage_point_to_string(cp_type_branch_arm) = "branch arm".

%---------------------%

:- pred format_proc_termination_info(maybe(arg_size_info)::in,
    maybe(termination_info)::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_proc_termination_info(MaybeArgSize, MaybeTermination, !State) :-
    SizeStr = maybe_arg_size_info_to_string(yes, MaybeArgSize),
    TermStr = maybe_termination_info_to_string(yes, MaybeTermination),
    string.builder.format("%% arg size properties: %s\n",
        [s(SizeStr)], !State),
    string.builder.format("%% termination properties: %s\n",
        [s(TermStr)], !State).

%---------------------%

:- pred format_proc_opt_info(hlds_dump_options::in, var_table::in, tvarset::in,
    var_name_print::in, maybe(structure_sharing_domain_and_status)::in,
    maybe(structure_reuse_domain_and_status)::in, maybe(untuple_proc_info)::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_proc_opt_info(DumpOptions, VarTable, TVarSet, VarNamePrint,
        MaybeStructureSharing, MaybeStructureReuse, MaybeUntupleInfo,
        !State) :-
    DumpStructSharing = DumpOptions ^ dump_struct_sharing_info,
    ( if
        DumpStructSharing = yes,
        MaybeStructureSharing = yes(StructureSharing)
    then
        string.builder.append_string("% structure sharing: \n", !State),
        StructureSharing =
            structure_sharing_domain_and_status(SharingAs, _Status),
        dump_structure_sharing_domain(string.builder.handle,
            vns_var_table(VarTable), TVarSet, SharingAs, !State)
    else
        true
    ),
    DumpUseReuse = DumpOptions ^ dump_use_reuse_info,
    ( if
        DumpUseReuse = yes,
        MaybeStructureReuse = yes(StructureReuse)
    then
        string.builder.append_string("% structure reuse: \n", !State),
        StructureReuse =
            structure_reuse_domain_and_status(ReuseAs, _ReuseStatus),
        dump_structure_reuse_domain(string.builder.handle,
            vns_var_table(VarTable), TVarSet, ReuseAs, !State)
    else
        true
    ),
    (
        MaybeUntupleInfo = yes(UntupleInfo),
        format_untuple_info(VarTable, VarNamePrint, UntupleInfo, !State)
    ;
        MaybeUntupleInfo = no
    ).

:- pred format_untuple_info(var_table::in, var_name_print::in,
    untuple_proc_info::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_untuple_info(VarTable, VarNamePrint, UntupleInfo, !State) :-
    UntupleInfo = untuple_proc_info(UntupleMap),
    string.builder.append_string("% untuple:\n", !State),
    map.foldl(format_untuple_info_loop(VarTable, VarNamePrint),
        UntupleMap, !State).

:- pred format_untuple_info_loop(var_table::in, var_name_print::in,
    prog_var::in, list(prog_var)::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_untuple_info_loop(VarTable, VarNamePrint,
        OldVar, NewVars, !State) :-
    string.builder.format("%%\t%s -> %s\n",
        [s(mercury_var_to_string(VarTable, VarNamePrint, OldVar)),
        s(mercury_vars_to_string(VarTable, VarNamePrint, NewVars))],
        !State).

%---------------------%

:- pred format_proc_deleted_callee_set(set(pred_proc_id)::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_proc_deleted_callee_set(DeletedCallCalleeSet, !State) :-
    set.to_sorted_list(DeletedCallCalleeSet, DeletedCallCallees),
    (
        DeletedCallCallees = []
    ;
        DeletedCallCallees = [_ | _],
        string.builder.format("%% procedures called from deleted goals: %s\n",
            [s(string.string(DeletedCallCallees))], !State)
    ).

%---------------------%

:- pred format_pred_proc_var_name_remap(var_name_source::in,
    map(prog_var, string)::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_pred_proc_var_name_remap(VarNameSrc, VarNameRemap, !State) :-
    map.to_assoc_list(VarNameRemap, VarNameRemapList),
    (
        VarNameRemapList = []
    ;
        VarNameRemapList = [VarNameRemapHead | VarNameRemapTail],
        string.builder.append_string("% var name remap: ", !State),
        format_var_name_remap(VarNameSrc,
            VarNameRemapHead, VarNameRemapTail, !State),
        string.builder.append_string("\n", !State)
    ).

:- pred format_var_name_remap(var_name_source::in,
    pair(prog_var, string)::in, list(pair(prog_var, string))::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_var_name_remap(VarNameSrc, Head, Tail, !State) :-
    Head = Var - NewName,
    VarName = mercury_var_to_string_src(VarNameSrc, print_name_and_num, Var),
    string.builder.format("%s -> %s", [s(VarName), s(NewName)], !State),
    (
        Tail = []
    ;
        Tail = [TailHead | TailTail],
        string.builder.append_string(", ", !State),
        format_var_name_remap(VarNameSrc, TailHead, TailTail, !State)
    ).

%---------------------%

:- pred format_eff_trace_level(module_info::in, pred_info::in, proc_info::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_eff_trace_level(ModuleInfo, PredInfo, ProcInfo, !State) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.get_trace_level(Globals, TraceLevel),
    EffTraceLevel =
        eff_trace_level_for_proc(ModuleInfo, PredInfo, ProcInfo, TraceLevel),
    string.builder.format("%% effective trace level: %s\n",
        [s(eff_trace_level_dump(EffTraceLevel))], !State).

%---------------------%

:- pred format_proc_arg_info(hlds_dump_options::in, var_table::in,
    var_name_print::in, maybe(list(is_live))::in,
    set_of_progvar::in, maybe(list(arg_info))::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_proc_arg_info(DumpOptions, VarTable, VarNamePrint,
        MaybeArgLives, RegR_HeadVars, MaybeArgInfos, !State) :-
    (
        MaybeArgLives = yes(ArgLives),
        string.builder.format("%% arg lives: %s\n",
            [s(string.string(ArgLives))], !State)
    ;
        MaybeArgLives = no
    ),
    ( if set_of_var.is_non_empty(RegR_HeadVars) then
        string.builder.append_string("% reg_r headvars: ", !State),
        mercury_format_vars(VarTable, VarNamePrint,
            set_of_var.to_sorted_list(RegR_HeadVars),
            string.builder.handle, !State),
        string.builder.append_string("\n", !State)
    else
        true
    ),
    DumpArgPassing = DumpOptions ^ dump_arg_passing_info,
    ( if
        DumpArgPassing = yes,
        MaybeArgInfos = yes(ArgInfos)
    then
        string.builder.format("%% arg_infos: %s\n",
            [s(string.string(ArgInfos))], !State)
    else
        true
    ).

%---------------------%

:- pred format_stack_slots(var_table::in, var_name_print::in, stack_slots::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_stack_slots(VarTable, VarNamePrint, StackSlots, !State) :-
    map.to_assoc_list(StackSlots, VarSlotList0),
    VarSlotList = assoc_list.map_values_only(stack_slot_to_abs_locn,
        VarSlotList0),
    format_var_to_abs_locns(vns_var_table(VarTable), VarNamePrint,
        0u, VarSlotList, !State).

%---------------------------------------------------------------------------%
:- end_module hlds.hlds_out.hlds_out_pred.
%---------------------------------------------------------------------------%
