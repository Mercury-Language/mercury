
%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2009-2012 The University of Melbourne.
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
:- import_module hlds.vartypes.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.prog_data.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

    % write_pred(Info, Stream, Lang, ModuleInfo, PredId, PredInfo, !IO):
    %
:- pred write_pred(hlds_out_info::in, io.text_output_stream::in,
    output_lang::in, module_info::in, pred_id::in, pred_info::in,
    io::di, io::uo) is det.

:- type write_which_modes
    --->    write_actual_modes
    ;       write_declared_modes.

    % write_clause(Info, Stream, Lang, ModuleInfo, PredId, PredOrFunc,
    %   VarSet, TypeQual, VarNamePrint, WriteWhichModes, Indent,
    %   HeadTerms, Clause, !IO).
    %
:- pred write_clause(hlds_out_info::in, io.text_output_stream::in,
    output_lang::in, module_info::in, pred_id::in, pred_or_func::in,
    prog_varset::in, maybe_vartypes::in,
    var_name_print::in, write_which_modes::in, int::in,
    list(prog_term)::in, clause::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred write_table_arg_infos(io.text_output_stream::in, tvarset::in,
    table_arg_infos::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_args.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_out.hlds_out_goal.
:- import_module hlds.hlds_rtti.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.program_representation.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.parse_tree_out_pragma.
:- import_module parse_tree.parse_tree_out_pred_decl.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_tree_to_term.
:- import_module parse_tree.prog_ctgc.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.
:- import_module transform_hlds.
:- import_module transform_hlds.term_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%
%
% Write out predicates.
%

write_pred(Info, Stream, Lang, ModuleInfo, PredId, PredInfo, !IO) :-
    Module = pred_info_module(PredInfo),
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
    ( if string.contains_char(DumpOptions, 'v') then
        VarNamePrint = print_name_and_num
    else
        VarNamePrint = print_name_only
    ),
    pred_info_get_proc_table(PredInfo, ProcTable),
    map.to_assoc_list(ProcTable, ProcIdsInfos),
    find_filled_in_procs(ProcIdsInfos, FilledInProcIdsInfos),
    ( if string.contains_char(DumpOptions, 'C') then
        % Information about predicates is dumped if 'C' suboption is on.
        (
            PredOrFunc = pf_predicate,
            mercury_output_pred_type(Stream, TVarSet, VarNamePrint, ExistQVars,
                qualified(Module, PredName), ArgTypes, no,
                Purity, ClassContext, !IO)
        ;
            PredOrFunc = pf_function,
            pred_args_to_func_args(ArgTypes, FuncArgTypes, FuncRetType),
            mercury_output_func_type(Stream, TVarSet, VarNamePrint, ExistQVars,
                qualified(Module, PredName), FuncArgTypes, FuncRetType, no,
                Purity, ClassContext, !IO)
        ),
        ClausesInfo = clauses_info(VarSet, _, _, VarTypes, HeadVars,
            ClausesRep, _ItemNumbers, RttiVarMaps,
            _HaveForeignClauses, _HadSyntaxErrors),
        pred_id_to_int(PredId, PredIdInt),
        PredOrFuncStr = pred_or_func_to_full_str(PredOrFunc),
        PredStatusStr = pred_import_status_to_string(PredStatus),
        pred_info_get_goal_type(PredInfo, GoalType),

        io.format(Stream, "%% pred id: %d, category: %s, status %s\n",
            [i(PredIdInt), s(PredOrFuncStr), s(PredStatusStr)], !IO),
        io.write_string(Stream, "% goal_type: ", !IO),
        io.write(Stream, GoalType, !IO),
        io.write_string(Stream, "\n", !IO),

        write_pred_markers(Stream, Markers, !IO),
        pred_info_get_obsolete_in_favour_of(PredInfo, MaybeObsoleteInFavourOf),
        (
            MaybeObsoleteInFavourOf = no
        ;
            MaybeObsoleteInFavourOf = yes(ObsoleteInFavourOf),
            io.write_string(Stream, "% obsolete in favour of one of\n", !IO),
            list.foldl(write_obsolete_in_favour_of(Stream),
                ObsoleteInFavourOf, !IO)
        ),
        write_pred_types(Stream, VarSet, TVarSet, VarNamePrint,
            RttiVarMaps, ProofMap, ConstraintMap, ExternalTypeParams,
            VarTypes, !IO),
        write_pred_proc_var_name_remap(Stream, VarSet, VarNameRemap, !IO),

        get_clause_list_maybe_repeated(ClausesRep, Clauses),
        ( if
            % Print the clauses only if (a) we have some, and (b) we haven't
            % already copied them to the proc_infos.
            Clauses = [_ | _],
            FilledInProcIdsInfos = []
        then
            set_dump_opts_for_clauses(Info, InfoForClauses),
            write_clauses(InfoForClauses, Stream, Lang, ModuleInfo,
                PredId, PredOrFunc, VarSet, no_varset_vartypes, VarNamePrint,
                HeadVars, Clauses, !IO)
        else
            true
        ),

        pred_info_get_origin(PredInfo, Origin),
        write_origin(Stream, ModuleInfo, TVarSet, VarNamePrint, Origin, !IO),
        PrintedPred = yes
    else
        PrintedPred = no
    ),
    ( if
        string.contains_char(DumpOptions, 'x'),
        FilledInProcIdsInfos = [_ | _]
    then
        write_procs_loop(Info, Stream, VarNamePrint, ModuleInfo,
            PredId, PredInfo, FilledInProcIdsInfos, !IO),
        PrintedProc = yes
    else
        PrintedProc = no
    ),
    ( if
        ( PrintedPred = yes
        ; PrintedProc = yes
        )
    then
        io.nl(Stream, !IO)
    else
        true
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

:- pred write_pred_markers(io.text_output_stream::in, pred_markers::in,
    io::di, io::uo) is det.

write_pred_markers(Stream, Markers, !IO) :-
    markers_to_marker_list(Markers, MarkerList),
    (
        MarkerList = []
    ;
        MarkerList = [_ | _],
        list.map(marker_name, MarkerList, MarkerNames),
        MarkerNamesStr = string.join_list(", ", MarkerNames),
        io.format(Stream, "%% markers: %s\n", [s(MarkerNamesStr)], !IO)
    ).

%---------------------%

:- pred write_obsolete_in_favour_of(io.text_output_stream::in,
    sym_name_arity::in, io::di, io::uo) is det.

write_obsolete_in_favour_of(Stream, ObsoleteInFavourOf, !IO) :-
    ObsoleteInFavourOf = sym_name_arity(SymName, Arity),
    io.format(Stream, "%%    %s/%d\n",
        [s(sym_name_to_string(SymName)), i(Arity)], !IO).

%---------------------%

:- pred write_pred_types(io.text_output_stream::in,
    prog_varset::in, tvarset::in, var_name_print::in, rtti_varmaps::in,
    constraint_proof_map::in, constraint_map::in,
    list(tvar)::in, vartypes::in, io::di, io::uo) is det.

write_pred_types(Stream, VarSet, TVarSet, VarNamePrint, RttiVarMaps,
        ProofMap, ConstraintMap, ExternalTypeParams, VarTypes, !IO) :-
    write_rtti_varmaps(Stream, VarSet, TVarSet, VarNamePrint,
        RttiVarMaps, !IO),
    ( if map.is_empty(ProofMap) then
        true
    else
        write_constraint_proof_map(Stream, 0, VarNamePrint, TVarSet,
            ProofMap, !IO),
        io.write_string(Stream, "\n", !IO)
    ),
    ( if map.is_empty(ConstraintMap) then
        true
    else
        write_constraint_map(Stream, VarNamePrint, TVarSet,
            ConstraintMap, !IO)
    ),
    (
        ExternalTypeParams = []
    ;
        ExternalTypeParams = [_ | _],
        io.write_string(Stream, "% external_type_params:\n", !IO),
        io.write_string(Stream, "% ", !IO),
        mercury_output_vars(TVarSet, VarNamePrint, ExternalTypeParams,
            Stream, !IO),
        io.write_string(Stream, "\n", !IO)
    ),
    write_var_types(Stream, VarSet, TVarSet, VarNamePrint, VarTypes, !IO).

:- pred write_constraint_map(io.text_output_stream::in, var_name_print::in,
    tvarset::in, constraint_map::in, io::di, io::uo) is det.

write_constraint_map(Stream, VarNamePrint, VarSet, ConstraintMap, !IO) :-
    io.write_string(Stream, "% Constraint map:\n", !IO),
    map.foldl(write_constraint_map_entry(Stream, VarNamePrint, VarSet),
        ConstraintMap, !IO).

:- pred write_constraint_map_entry(io.text_output_stream::in,
    var_name_print::in, tvarset::in, constraint_id::in, prog_constraint::in,
    io::di, io::uo) is det.

write_constraint_map_entry(Stream, VarNamePrint, VarSet,
        ConstraintId, ProgConstraint, !IO) :-
    io.write_string(Stream, "% ", !IO),
    write_constraint_id(Stream, ConstraintId, !IO),
    io.write_string(Stream, ": ", !IO),
    mercury_output_constraint(VarSet, VarNamePrint, ProgConstraint,
        Stream, !IO),
    io.nl(Stream, !IO).

:- pred write_constraint_id(io.text_output_stream::in, constraint_id::in,
    io::di, io::uo) is det.

write_constraint_id(Stream, ConstraintId, !IO) :-
    ConstraintId = constraint_id(ConstraintType, GoalId, N),
    (
        ConstraintType = assumed,
        ConstraintTypeChar = 'E'
    ;
        ConstraintType = unproven,
        ConstraintTypeChar = 'A'
    ),
    GoalId = goal_id(GoalIdNum),
    io.format(Stream, "(%c, %d, %d)",
        [c(ConstraintTypeChar), i(GoalIdNum), i(N)], !IO).

%---------------------%

:- pred write_origin(io.text_output_stream::in, module_info::in,
    tvarset::in, var_name_print::in, pred_origin::in, io::di, io::uo) is det.

write_origin(Stream, ModuleInfo, TVarSet, VarNamePrint, Origin, !IO) :-
    % XXX CLEANUP Either a function version of this predicate should replace
    % pred_info_id_to_string in hlds_out_util.m, or vice versa.
    (
        Origin = origin_special_pred(_, _),
        io.write_string(Stream, "% special pred\n", !IO)
    ;
        Origin = origin_instance_method(_, MethodConstraints),
        MethodConstraints = instance_method_constraints(ClassId,
            InstanceTypes, InstanceConstraints, ClassMethodConstraints),
        io.write_string(Stream, "% instance method\n", !IO),
        ClassId = class_id(ClassName, _),
        io.write_string(Stream,
            "% class name and instance type vector:\n", !IO),
        io.write_string(Stream, "%   ", !IO),
        mercury_output_constraint(TVarSet, VarNamePrint,
            constraint(ClassName, InstanceTypes), Stream, !IO),
        io.nl(Stream, !IO),
        write_origin_constraints(Stream, "instance constraints",
            TVarSet, VarNamePrint, InstanceConstraints, !IO),
        ClassMethodConstraints = constraints(MethodUnivConstraints,
            MethodExistConstraints),
        write_origin_constraints(Stream, "method universal constraints",
            TVarSet, VarNamePrint, MethodUnivConstraints, !IO),
        write_origin_constraints(Stream, "method existential constraints",
            TVarSet, VarNamePrint, MethodExistConstraints, !IO)
    ;
        Origin = origin_class_method(ClassId, MethodId),
        ClassId = class_id(ClassSymName, ClassArity),
        MethodId = pf_sym_name_arity(MethodPredOrFunc,
            MethodSymName, MethodPredFormArity),
        user_arity_pred_form_arity(MethodPredOrFunc,
            MethodUserArity, MethodPredFormArity),
        MethodUserArity = user_arity(MethodUserArityInt),
        io.format(Stream, "%% class method %s %s/%d for %s/%d\n",
            [s(pred_or_func_to_string(MethodPredOrFunc)),
            s(sym_name_to_string(MethodSymName)), i(MethodUserArityInt),
            s(sym_name_to_string(ClassSymName)), i(ClassArity)], !IO)
    ;
        Origin = origin_transformed(Transformation, _, OrigPredId),
        OrigPredIdNum = pred_id_to_int(OrigPredId),
        io.format(Stream, "%% transformed from pred id %d\n",
            [i(OrigPredIdNum)], !IO),
        io.write_string(Stream, "% ", !IO),
        io.write_string(Stream,
            pred_id_to_string(ModuleInfo, OrigPredId), !IO),
        io.nl(Stream, !IO),
        io.write_string(Stream, "% transformation: ", !IO),
        io.write_line(Stream, Transformation, !IO)
    ;
        Origin = origin_created(Creation),
        io.write_string(Stream, "% created: ", !IO),
        io.write_line(Stream, Creation, !IO)
    ;
        Origin = origin_assertion(_, _),
        io.write_string(Stream, "% assertion\n", !IO)
    ;
        Origin = origin_solver_type(TypeCtorSymName, TypeCtorArity,
            SolverAuxPredKind),
        TypeCtorStr = sym_name_arity_to_string(
            sym_name_arity(TypeCtorSymName, TypeCtorArity)),
        (
            SolverAuxPredKind = solver_type_to_ground_pred,
            SolverAuxPredKindStr = "to ground conversion predicate"
        ;
            SolverAuxPredKind = solver_type_to_any_pred,
            SolverAuxPredKindStr = "to any conversion predicate"
        ;
            SolverAuxPredKind = solver_type_from_ground_pred,
            SolverAuxPredKindStr = "from ground conversion predicate"
        ;
            SolverAuxPredKind = solver_type_from_any_pred,
            SolverAuxPredKindStr = "from any conversion predicate"
        ),
        io.format(Stream, "%% %s for %s\n",
            [s(SolverAuxPredKindStr), s(TypeCtorStr)], !IO)
    ;
        Origin = origin_tabling(BasePredCallId, TablingAuxPredKind),
        BasePredStr = pf_sym_name_orig_arity_to_string(BasePredCallId),
        (
            TablingAuxPredKind = tabling_aux_pred_stats,
            TablingAuxPredKindStr = "table statistics predicate"
        ;
            TablingAuxPredKind = tabling_aux_pred_reset,
            TablingAuxPredKindStr = "table reset predicate"
        ),
        io.format(Stream, "%% %s for %s\n",
            [s(TablingAuxPredKindStr), s(BasePredStr)], !IO)
    ;
        Origin = origin_mutable(MutableModuleName, MutableName,
            MutablePredKind),
        MutableModuleNameStr = sym_name_to_string(MutableModuleName),
        (
            MutablePredKind = mutable_pred_std_get,
            MutablePredKindStr = "std get predicate"
        ;
            MutablePredKind = mutable_pred_std_set,
            MutablePredKindStr = "std set predicate"
        ;
            MutablePredKind = mutable_pred_io_get,
            MutablePredKindStr = "io get predicate"
        ;
            MutablePredKind = mutable_pred_io_set,
            MutablePredKindStr = "io set predicate"
        ;
            MutablePredKind = mutable_pred_unsafe_get,
            MutablePredKindStr = "unsafe get predicate"
        ;
            MutablePredKind = mutable_pred_unsafe_set,
            MutablePredKindStr = "unsafe set predicate"
        ;
            MutablePredKind = mutable_pred_constant_get,
            MutablePredKindStr = "constant get predicate"
        ;
            MutablePredKind = mutable_pred_constant_secret_set,
            MutablePredKindStr = "constant secret set predicate"
        ;
            MutablePredKind = mutable_pred_lock,
            MutablePredKindStr = "lock predicate"
        ;
            MutablePredKind = mutable_pred_unlock,
            MutablePredKindStr = "unlock predicate"
        ;
            MutablePredKind = mutable_pred_pre_init,
            MutablePredKindStr = "preinit predicate"
        ;
            MutablePredKind = mutable_pred_init,
            MutablePredKindStr = "init predicate"
        ),
        io.format(Stream, "%% %s for mutable %s in module %s\n",
            [s(MutablePredKindStr), s(MutableName),
            s(MutableModuleNameStr)], !IO)
    ;
        Origin = origin_initialise,
        io.write_string(Stream, "% initialise\n", !IO)
    ;
        Origin = origin_finalise,
        io.write_string(Stream, "% finalise\n", !IO)
    ;
        ( Origin = origin_lambda(_, _, _)
        ; Origin = origin_user(_)
        )
    ).

:- pred write_origin_constraints(io.text_output_stream::in, string::in,
    tvarset::in, var_name_print::in, list(prog_constraint)::in, io::di, io::uo)
    is det.

write_origin_constraints(Stream, Msg, TVarSet, VarNamePrint,
        Constraints, !IO) :-
    (
        Constraints = [],
        io.format(Stream, "%% %s: none\n", [s(Msg)], !IO)
    ;
        Constraints = [_ | _],
        io.format(Stream, "%% %s:\n", [s(Msg)], !IO),
        list.foldl(write_origin_constraint(Stream, TVarSet, VarNamePrint),
            Constraints, !IO)
    ).

:- pred write_origin_constraint(io.text_output_stream::in,
    tvarset::in, var_name_print::in, prog_constraint::in, io::di, io::uo)
    is det.

write_origin_constraint(Stream, TVarSet, VarNamePrint, Constraint, !IO) :-
    io.write_string(Stream, "%       ", !IO),
    mercury_output_constraint(TVarSet, VarNamePrint, Constraint, Stream, !IO),
    io.nl(Stream, !IO).

%---------------------------------------------------------------------------%

:- pred set_dump_opts_for_clauses(hlds_out_info::in, hlds_out_info::out)
    is det.

set_dump_opts_for_clauses(Info, ClausesInfo) :-
    OptionsStr = Info ^ hoi_dump_hlds_options,
    some [!DumpStr] (
        !:DumpStr = "",
        ( if string.contains_char(OptionsStr, 'c') then
            !:DumpStr = !.DumpStr ++ "c"
        else
            true
        ),
        ( if string.contains_char(OptionsStr, 'n') then
            !:DumpStr = !.DumpStr ++ "n"
        else
            true
        ),
        ( if string.contains_char(OptionsStr, 'v') then
            !:DumpStr = !.DumpStr ++ "v"
        else
            true
        ),
        ( if string.contains_char(OptionsStr, 'g') then
            !:DumpStr = !.DumpStr ++ "g"
        else
            true
        ),
        ( if string.contains_char(OptionsStr, 'P') then
            !:DumpStr = !.DumpStr ++ "P"
        else
            true
        ),
        DumpStr = !.DumpStr
    ),
    ClausesInfo = Info ^ hoi_dump_hlds_options := DumpStr.

:- pred write_clauses(hlds_out_info::in, io.text_output_stream::in,
    output_lang::in, module_info::in, pred_id::in, pred_or_func::in,
    prog_varset::in, maybe_vartypes::in, var_name_print::in,
    proc_arg_vector(prog_var)::in, list(clause)::in, io::di, io::uo) is det.

write_clauses(Info, Stream, Lang, ModuleInfo, PredId, PredOrFunc, VarSet,
        TypeQual, VarNamePrint, HeadVarsVector, Clauses, !IO) :-
    HeadVars = proc_arg_vector_to_list(HeadVarsVector),
    term.var_list_to_term_list(HeadVars, HeadTerms),
    write_clauses_loop(Info, Stream, Lang, ModuleInfo, PredId, PredOrFunc,
        VarSet, TypeQual, VarNamePrint, HeadTerms, Clauses, 1, !IO).

:- pred write_clauses_loop(hlds_out_info::in, io.text_output_stream::in,
    output_lang::in, module_info::in, pred_id::in, pred_or_func::in,
    prog_varset::in, maybe_vartypes::in, var_name_print::in,
    list(prog_term)::in, list(clause)::in, int::in, io::di, io::uo) is det.

write_clauses_loop(Info, Stream, Lang, ModuleInfo, PredId, PredOrFunc, VarSet,
        TypeQual, VarNamePrint, HeadTerms, Clauses, ClauseNum, !IO) :-
    (
        Clauses = []
    ;
        Clauses = [FirstClause | LaterClauses],
        io.format(Stream, "%% clause %d\n", [i(ClauseNum)], !IO),
        write_clause(Info, Stream, Lang, ModuleInfo, PredId, PredOrFunc,
            VarSet, TypeQual, VarNamePrint, write_actual_modes, 0,
            HeadTerms, FirstClause, !IO),
        write_clauses_loop(Info, Stream, Lang, ModuleInfo,PredId, PredOrFunc,
            VarSet, TypeQual, VarNamePrint, HeadTerms, LaterClauses,
            ClauseNum + 1, !IO)
    ).

write_clause(Info, Stream, Lang, ModuleInfo,PredId, PredOrFunc, VarSet,
        TypeQual, VarNamePrint, WriteWhichModes, Indent, HeadTerms,
        Clause, !IO) :-
    Clause = clause(ApplicableModes, Goal, ImplLang, Context,
        _StateVarWarnings),
    Indent1 = Indent + 1,
    DumpOptions = Info ^ hoi_dump_hlds_options,
    (
        ApplicableModes = all_modes
    ;
        ApplicableModes = selected_modes(Modes),
        ( if string.contains_char(DumpOptions, 'm') then
            write_indent(Stream, Indent, !IO),
            io.write_string(Stream,
                "% Modes for which this clause applies: ", !IO),
            ModeInts = list.map(proc_id_to_int, Modes),
            write_intlist(Stream, ModeInts, !IO),
            io.write_string(Stream, "\n", !IO)
        else
            true
        )
    ;
        ApplicableModes = unify_in_in_modes,
        ( if string.contains_char(DumpOptions, 'm') then
            write_indent(Stream, Indent, !IO),
            io.write_string(Stream, 
                "% This clause applies only to <in,in> unify modes.\n", !IO)
        else
            true
        )
    ;
        ApplicableModes = unify_non_in_in_modes,
        ( if string.contains_char(DumpOptions, 'm') then
            write_indent(Stream, Indent, !IO),
            io.write_string(Stream, 
                "% This clause applies only to non <in,in> unify modes.\n",
                !IO)
        else
            true
        )
    ),
    (
        ImplLang = impl_lang_mercury
    ;
        ImplLang = impl_lang_foreign(ForeignLang),
        io.write_string(Stream, "% Language of implementation: ", !IO),
        io.write_line(Stream, ForeignLang, !IO)
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
        write_annotated_clause_heads(Stream, ModuleInfo, Lang, VarSet,
            VarNamePrint, WriteWhichModes, PredId, PredOrFunc, SelectedProcIds,
            Context, HeadTerms, !IO)
    else
        write_clause_head(Stream, ModuleInfo, VarSet, VarNamePrint,
            PredId, PredOrFunc, HeadTerms, !IO)
    ),
    ( if Goal = hlds_goal(conj(plain_conj, []), _GoalInfo) then
        io.write_string(Stream, ".\n", !IO)
    else
        io.write_string(Stream, " :-\n", !IO),
        do_write_goal(Info, Stream, ModuleInfo, VarSet, TypeQual, VarNamePrint,
            Indent1, ".\n", Goal, !IO)
    ).

:- pred write_annotated_clause_heads(io.text_output_stream::in,
    module_info::in, output_lang::in, prog_varset::in, var_name_print::in,
    write_which_modes::in, pred_id::in, pred_or_func::in, list(proc_id)::in,
    term.context::in, list(prog_term)::in, io::di, io::uo) is det.

write_annotated_clause_heads(_, _, _, _, _, _, _, _, [], _, _, !IO).
write_annotated_clause_heads(Stream, ModuleInfo, Lang, VarSet,
    VarNamePrint, WriteWhichModes, PredId, PredOrFunc, [ProcId | ProcIds],
        Context, HeadTerms, !IO) :-
    write_annotated_clause_head(Stream, ModuleInfo, Lang, VarSet,
        VarNamePrint, WriteWhichModes, PredId, PredOrFunc, ProcId,
        Context, HeadTerms, !IO),
    write_annotated_clause_heads(Stream, ModuleInfo, Lang, VarSet,
        VarNamePrint, WriteWhichModes, PredId, PredOrFunc, ProcIds,
        Context, HeadTerms, !IO).

:- pred write_annotated_clause_head(io.text_output_stream::in,
    module_info::in, output_lang::in, prog_varset::in, var_name_print::in,
    write_which_modes::in, pred_id::in, pred_or_func::in, proc_id::in,
    term.context::in, list(prog_term)::in, io::di, io::uo) is det.

write_annotated_clause_head(Stream, ModuleInfo, Lang,VarSet, VarNamePrint,
        WriteWhichModes, PredId, PredOrFunc, ProcId,
        Context, HeadTerms, !IO) :-
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
        write_clause_head(Stream, ModuleInfo, VarSet, VarNamePrint, PredId,
            PredOrFunc, AnnotatedHeadTerms, !IO)
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

:- pred write_clause_head(io.text_output_stream::in, module_info::in,
    prog_varset::in, var_name_print::in, pred_id::in, pred_or_func::in,
    list(prog_term)::in, io::di, io::uo) is det.

write_clause_head(Stream, ModuleInfo, VarSet, VarNamePrint, PredId, PredOrFunc,
        HeadTerms, !IO) :-
    PredName = predicate_name(ModuleInfo, PredId),
    ModuleName = predicate_module(ModuleInfo, PredId),
    (
        PredOrFunc = pf_function,
        pred_args_to_func_args(HeadTerms, FuncArgs, RetVal),
        io.write_string(Stream,
            qualified_functor_with_term_args_to_string(VarSet, VarNamePrint,
                ModuleName, term.atom(PredName), FuncArgs),
            !IO),
        io.write_string(Stream, " = ", !IO),
        mercury_output_term_nq(VarSet, VarNamePrint, next_to_graphic_token,
            RetVal, Stream, !IO)
    ;
        PredOrFunc = pf_predicate,
        io.write_string(Stream,
            qualified_functor_with_term_args_to_string(VarSet, VarNamePrint,
                ModuleName, term.atom(PredName), HeadTerms),
            !IO)
    ).

%---------------------------------------------------------------------------%
%
% Write out procedures.
%

:- pred write_procs_loop(hlds_out_info::in, io.text_output_stream::in,
    var_name_print::in, module_info::in, pred_id::in, pred_info::in,
    assoc_list(proc_id, proc_info)::in, io::di, io::uo) is det.

write_procs_loop(_, _, _, _, _, _, [], !IO).
write_procs_loop(Info, Stream, VarNamePrint, ModuleInfo,
        PredId, PredInfo, [ProcId - ProcInfo | ProcIdsInfos], !IO) :-
    write_proc(Info, Stream, VarNamePrint, ModuleInfo,
        PredId, PredInfo, ProcId, ProcInfo, !IO),
    write_procs_loop(Info, Stream, VarNamePrint, ModuleInfo,
        PredId, PredInfo, ProcIdsInfos, !IO).

:- pred write_proc(hlds_out_info::in, io.text_output_stream::in,
    var_name_print::in, module_info::in, pred_id::in, pred_info::in,
    proc_id::in, proc_info::in, io::di, io::uo) is det.

write_proc(Info, Stream, VarNamePrint, ModuleInfo, PredId, PredInfo,
        ProcId, ProcInfo, !IO) :-
    pred_info_get_typevarset(PredInfo, TVarSet),
    proc_info_get_can_process(ProcInfo, CanProcess),
    proc_info_get_varset(ProcInfo, VarSet),
    proc_info_get_vartypes(ProcInfo, VarTypes),
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
    Indent1 = 1,

    DumpOptions = Info ^ hoi_dump_hlds_options,
    pred_id_to_int(PredId, PredIdInt),
    proc_id_to_int(ProcId, ProcIdInt),
    PredIdStr = pred_id_to_string(ModuleInfo, PredId),
    DetismStr = determinism_to_string(InferredDeterminism),
    write_indent(Stream, Indent1, !IO),
    io.format(Stream, "%% pred id %d: %s\n",
        [i(PredIdInt), s(PredIdStr)], !IO),
    write_indent(Stream, Indent1, !IO),
    ( if proc_info_is_valid_mode(ProcInfo) then
        io.format(Stream, "%% mode number %d (%s)\n",
            [i(ProcIdInt), s(DetismStr)], !IO)
    else
        io.format(Stream, "%% mode number %d (%s) INVALID MODE\n",
            [i(ProcIdInt), s(DetismStr)], !IO)
    ),

    write_var_types(Stream, VarSet, TVarSet, VarNamePrint, VarTypes, !IO),
    write_rtti_varmaps(Stream, VarSet, TVarSet, VarNamePrint,
        RttiVarMaps, !IO),

    write_proc_flags(Stream, CanProcess, IsAddressTaken,
        HasParallelConj, HasUserEvent, !IO),
    io.write_string(Stream, "% cse_nopull_contexts: ", !IO),
    io.write_line(Stream, CseNoPullContexts, !IO),
    write_proc_tabling_info(Stream, VarSet, TVarSet, VarNamePrint,
        EvalMethod, MaybeProcTableIOInfo, MaybeCallTableTip, !IO),
    write_proc_deep_profiling_info(Stream, VarSet, VarNamePrint,
        MaybeDeepProfileInfo, !IO),
    write_proc_termination_info(Stream, DumpOptions,
        MaybeArgSize, MaybeTermination, !IO),
    write_proc_opt_info(Stream, DumpOptions, VarSet, TVarSet,
        VarNamePrint, MaybeStructureSharing, MaybeStructureReuse,
        MaybeUntupleInfo, !IO),
    write_proc_deleted_callee_set(Stream, DeletedCallCalleeSet, !IO),
    write_pred_proc_var_name_remap(Stream, VarSet, VarNameRemap, !IO),

    PredSymName = unqualified(predicate_name(ModuleInfo, PredId)),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    varset.init(ModeVarSet),
    (
        PredOrFunc = pf_predicate,
        MaybeWithInst = maybe.no,
        mercury_output_pred_mode_decl(Stream, output_debug, ModeVarSet,
            PredSymName, HeadModes, MaybeWithInst,
            DeclaredDeterminism, !IO)
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(HeadModes, FuncHeadModes, RetHeadMode),
        mercury_output_func_mode_decl(Stream, output_debug, ModeVarSet,
            PredSymName, FuncHeadModes, RetHeadMode,
            DeclaredDeterminism, !IO)
    ),
    write_proc_arg_info(Stream, DumpOptions, VarSet, VarNamePrint,
        MaybeArgLives, RegR_HeadVars, MaybeArgInfos, !IO),
    pred_info_get_status(PredInfo, PredStatus),
    ( if
        PredStatus = pred_status(status_pseudo_imported),
        hlds_pred.in_in_unification_proc_id(ProcId)
    then
        true
    else
        proc_info_get_stack_slots(ProcInfo, StackSlots),
        write_stack_slots(Stream, VarSet, VarNamePrint, StackSlots, !IO),
        term.var_list_to_term_list(HeadVars, HeadTerms),
        write_clause_head(Stream, ModuleInfo, VarSet, VarNamePrint,
            PredId, PredOrFunc, HeadTerms, !IO),
        io.write_string(Stream, " :-\n", !IO),
        write_goal(Info, Stream, ModuleInfo, VarSet, VarNamePrint,
            Indent1, ".\n", Goal, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred write_var_types(io.text_output_stream::in,
    prog_varset::in, tvarset::in, var_name_print::in, vartypes::in,
    io::di, io::uo) is det.

write_var_types(Stream, VarSet, TVarSet, VarNamePrint, VarTypes, !IO) :-
    vartypes_count(VarTypes, NumVarTypes),
    io.write_string(Stream, "% variable types map ", !IO),
    io.format(Stream, "(%d entries):\n", [i(NumVarTypes)], !IO),
    vartypes_vars(VarTypes, Vars),
    write_var_types_loop(Stream, VarSet, TVarSet, VarNamePrint, VarTypes,
        Vars, !IO).

:- pred write_var_types_loop(io.text_output_stream::in,
    prog_varset::in, tvarset::in, var_name_print::in, vartypes::in,
    list(prog_var)::in, io::di, io::uo) is det.

write_var_types_loop(_, _, _, _, _, [], !IO).
write_var_types_loop(Stream, VarSet, TypeVarSet, VarNamePrint, VarTypes,
        [Var | Vars], !IO) :-
    lookup_var_type(VarTypes, Var, Type),
    VarStr = mercury_var_to_string(VarSet, VarNamePrint, Var),
    term.var_to_int(Var, VarNum),
    TypeStr = mercury_type_to_string(TypeVarSet, VarNamePrint, Type),
    io.format(Stream, "%% %s (number %d): %s\n",
        [s(VarStr), i(VarNum), s(TypeStr)], !IO),
    write_var_types_loop(Stream, VarSet, TypeVarSet, VarNamePrint, VarTypes,
        Vars, !IO).

%---------------------%

:- pred write_rtti_varmaps(io.text_output_stream::in,
    prog_varset::in, tvarset::in, var_name_print::in,
    rtti_varmaps::in, io::di, io::uo) is det.

write_rtti_varmaps(Stream, VarSet, TVarSet, VarNamePrint, RttiVarMaps, !IO) :-
    io.write_string(Stream, "% type_info varmap:\n", !IO),
    rtti_varmaps_tvars(RttiVarMaps, TypeVars),
    list.foldl(
        write_type_info_locn(Stream, VarSet, TVarSet, VarNamePrint,
            RttiVarMaps),
        TypeVars, !IO),
    io.write_string(Stream, "% typeclass_info varmap:\n", !IO),
    rtti_varmaps_reusable_constraints(RttiVarMaps, Constraints),
    list.foldl(
        write_typeclass_info_var(Stream, VarSet, TVarSet, VarNamePrint,
            RttiVarMaps),
        Constraints, !IO),
    io.write_string(Stream, "% rtti_var_info:\n", !IO),
    rtti_varmaps_rtti_prog_vars(RttiVarMaps, ProgVars),
    list.foldl(
        write_rtti_var_info(Stream, VarSet, TVarSet, VarNamePrint,
            RttiVarMaps),
        ProgVars, !IO).

:- pred write_type_info_locn(io.text_output_stream::in,
    prog_varset::in, tvarset::in, var_name_print::in, rtti_varmaps::in,
    tvar::in, io::di, io::uo) is det.

write_type_info_locn(Stream, VarSet, TVarSet, VarNamePrint, RttiVarMaps,
        TVar, !IO) :-
    TVarStr = mercury_var_to_string(TVarSet, VarNamePrint, TVar),
    term.var_to_int(TVar, TVarNum),
    io.format(Stream, "%% %s(number %d) -> ", [s(TVarStr), i(TVarNum)], !IO),
    rtti_lookup_type_info_locn(RttiVarMaps, TVar, Locn),
    (
        Locn = type_info(Var),
        VarStr = mercury_var_to_string(VarSet, VarNamePrint, Var),
        io.format(Stream, "type_info(%s)", [s(VarStr)], !IO)
    ;
        Locn = typeclass_info(Var, Index),
        VarStr = mercury_var_to_string(VarSet, VarNamePrint, Var),
        io.format(Stream, "typeclass_info(%s, %d)",
            [s(VarStr), i(Index)], !IO)
    ),
    term.var_to_int(Var, VarNum),
    io.format(Stream, " (number %d)\n", [i(VarNum)], !IO).

:- pred write_typeclass_info_var(io.text_output_stream::in,
    prog_varset::in, tvarset::in, var_name_print::in, rtti_varmaps::in,
    prog_constraint::in, io::di, io::uo) is det.

write_typeclass_info_var(Stream, VarSet, TVarSet, VarNamePrint, RttiVarMaps,
        Constraint, !IO) :-
    io.write_string(Stream, "% ", !IO),
    mercury_output_constraint(TVarSet, VarNamePrint, Constraint, Stream, !IO),
    io.write_string(Stream, " -> ", !IO),
    rtti_lookup_typeclass_info_var(RttiVarMaps, Constraint, Var),
    mercury_output_var(VarSet, VarNamePrint, Var, Stream, !IO),
    io.nl(Stream, !IO).

:- pred write_rtti_var_info(io.text_output_stream::in,
    prog_varset::in, tvarset::in, var_name_print::in, rtti_varmaps::in,
    prog_var::in, io::di, io::uo) is det.

write_rtti_var_info(Stream, VarSet, TVarSet, VarNamePrint, RttiVarMaps,
        Var, !IO) :-
    term.var_to_int(Var, VarNum),
    VarStr = mercury_var_to_string(VarSet, VarNamePrint, Var),
    io.format(Stream, "%% %s (number %d) -> ", [s(VarStr), i(VarNum)], !IO),
    rtti_varmaps_var_info(RttiVarMaps, Var, VarInfo),
    (
        VarInfo = type_info_var(Type),
        io.write_string(Stream, "type_info for ", !IO),
        mercury_output_type(TVarSet, VarNamePrint, Type, Stream, !IO)
    ;
        VarInfo = typeclass_info_var(Constraint),
        io.write_string(Stream, "typeclass_info for ", !IO),
        mercury_output_constraint(TVarSet, VarNamePrint, Constraint,
            Stream, !IO)
    ;
        VarInfo = non_rtti_var,
        unexpected($pred, "non rtti var")
    ),
    io.nl(Stream, !IO).

%---------------------%

:- pred write_proc_flags(io.text_output_stream::in, can_process::in,
    is_address_taken::in, has_parallel_conj::in, has_user_event::in,
    io::di, io::uo) is det.

write_proc_flags(Stream, CanProcess, IsAddressTaken, HasParallelConj,
        HasUserEvent, !IO) :-
    (
        CanProcess = can_process_now
    ;
        CanProcess = cannot_process_yet,
        io.write_string(Stream, "% cannot_process_yet\n", !IO)
    ),
    (
        IsAddressTaken = address_is_taken,
        io.write_string(Stream, "% address is taken\n", !IO)
    ;
        IsAddressTaken = address_is_not_taken,
        io.write_string(Stream, "% address is not taken\n", !IO)
    ),
    (
        HasParallelConj = has_parallel_conj,
        io.write_string(Stream,
            "% contains parallel conjunction\n", !IO)
    ;
        HasParallelConj = has_no_parallel_conj,
        io.write_string(Stream,
            "% does not contain parallel conjunction\n", !IO)
    ),
    (
        HasUserEvent = has_user_event,
        io.write_string(Stream, "% contains user event\n", !IO)
    ;
        HasUserEvent = has_no_user_event,
        io.write_string(Stream, "% does not contain user event\n", !IO)
    ).

%---------------------%

:- pred write_proc_tabling_info(io.text_output_stream::in,
    prog_varset::in, tvarset::in, var_name_print::in, eval_method::in,
    maybe(proc_table_io_info)::in, maybe(prog_var)::in,
    io::di, io::uo) is det.

write_proc_tabling_info(Stream, VarSet, TVarSet, VarNamePrint,
        EvalMethod, MaybeProcTableIOInfo, MaybeCallTableTip, !IO) :-
    (
        EvalMethod = eval_normal
    ;
        EvalMethod = eval_tabled(TabledMethod),
        io.format(Stream, "%% eval method: %s\n",
            [s(tabled_eval_method_to_string(TabledMethod))], !IO)
    ),
    (
        MaybeProcTableIOInfo = yes(ProcTableIOInfo),
        write_proc_table_io_info(Stream, TVarSet, ProcTableIOInfo, !IO)
    ;
        MaybeProcTableIOInfo = no
    ),
    (
        MaybeCallTableTip = yes(CallTableTip),
        io.write_string(Stream, "% call table tip: ", !IO),
        mercury_output_var(VarSet, VarNamePrint, CallTableTip, Stream, !IO),
        io.write_string(Stream, "\n", !IO)
    ;
        MaybeCallTableTip = no
    ).

:- pred write_proc_table_io_info(io.text_output_stream::in, tvarset::in,
    proc_table_io_info::in, io::di, io::uo) is det.

write_proc_table_io_info(Stream, TVarSet, ProcTableIOInfo, !IO) :-
    ProcTableIOInfo = proc_table_io_info(MaybeArgInfos),
    (
        MaybeArgInfos = no,
        io.write_string(Stream,
            "% proc table io info: io tabled, no arg_infos\n", !IO)
    ;
        MaybeArgInfos = yes(ArgInfos),
        io.write_string(Stream,
            "% proc table io info: io tabled, arg_infos:\n", !IO),
        write_table_arg_infos(Stream, TVarSet, ArgInfos, !IO)
    ).

write_table_arg_infos(Stream, TVarSet, TableArgInfos, !IO) :-
    TableArgInfos = table_arg_infos(ArgInfos, TVarMap),
    io.write_string(Stream, "% arg infos:\n", !IO),
    list.foldl(write_table_arg_info(Stream, TVarSet), ArgInfos, !IO),
    map.to_assoc_list(TVarMap, TVarPairs),
    (
        TVarPairs = []
    ;
        TVarPairs = [_ | _],
        io.write_string(Stream, "% type var map:\n", !IO),
        list.foldl(write_table_tvar_map_entry(Stream, TVarSet), TVarPairs, !IO)
    ).

:- pred write_table_arg_info(io.text_output_stream::in, tvarset::in,
    table_arg_info::in, io::di, io::uo) is det.

write_table_arg_info(Stream, TVarSet, ArgInfo, !IO) :-
    ArgInfo = table_arg_info(HeadVarNum, HeadVarName, SlotNum, Type),
    TVarStr = mercury_type_to_string(TVarSet, print_name_and_num, Type),
    io.format(Stream, "%% %s / %d in slot %d, type %s\n",
        [s(HeadVarName), i(HeadVarNum), i(SlotNum), s(TVarStr)], !IO).

:- pred write_table_tvar_map_entry(io.text_output_stream::in, tvarset::in,
    pair(tvar, table_locn)::in, io::di, io::uo) is det.

write_table_tvar_map_entry(Stream, TVarSet, TVar - Locn, !IO) :-
    TVarStr = mercury_var_to_string(TVarSet, print_name_and_num, TVar),
    io.format(Stream, "%% typeinfo for %s -> ", [s(TVarStr)], !IO),
    (
        Locn = table_locn_direct(N),
        io.format(Stream, "direct in register %d\n", [i(N)], !IO)
    ;
        Locn = table_locn_indirect(N, O),
        io.format(Stream,
            "indirect from register %d, offset %d\n", [i(N), i(O)], !IO)
    ).

%---------------------%

:- pred write_proc_deep_profiling_info(io.text_output_stream::in,
    prog_varset::in, var_name_print::in, maybe(deep_profile_proc_info)::in,
    io::di, io::uo) is det.

write_proc_deep_profiling_info(Stream, VarSet, VarNamePrint,
        MaybeDeepProfileInfo, !IO) :-
    (
        MaybeDeepProfileInfo = yes(DeepProfileInfo),
        DeepProfileInfo = deep_profile_proc_info(MaybeRecInfo,
            MaybeDeepLayout, _),
        (
            MaybeRecInfo = yes(DeepRecInfo),
            DeepRecInfo = deep_recursion_info(Role, _),
            io.write_string(Stream, "% deep recursion info: ", !IO),
            (
                Role = deep_prof_inner_proc(DeepPredProcId),
                io.write_string(Stream, "inner, outer is ", !IO)
            ;
                Role = deep_prof_outer_proc(DeepPredProcId),
                io.write_string(Stream, "outer, inner is ", !IO)
            ),
            DeepPredProcId = proc(DeepPredId, DeepProcId),
            pred_id_to_int(DeepPredId, DeepPredInt),
            proc_id_to_int(DeepProcId, DeepProcInt),
            io.format(Stream, "%d/%d\n", [i(DeepPredInt), i(DeepProcInt)], !IO)
        ;
            MaybeRecInfo = no
        ),
        (
            MaybeDeepLayout = yes(DeepLayout),
            DeepLayout = hlds_deep_layout(ProcStatic, ExcpVars),
            write_hlds_proc_static(Stream, ProcStatic, !IO),
            ExcpVars = hlds_deep_excp_vars(TopCSD, MiddleCSD,
                MaybeOldOutermost),
            io.write_string(Stream, "% deep layout info: ", !IO),
            io.write_string(Stream, "TopCSD is ", !IO),
            mercury_output_var(VarSet, VarNamePrint, TopCSD, Stream, !IO),
            io.write_string(Stream, ", MiddleCSD is ", !IO),
            mercury_output_var(VarSet, VarNamePrint, MiddleCSD, Stream, !IO),
            (
                MaybeOldOutermost = yes(OldOutermost),
                io.write_string(Stream, ", OldOutermost is ", !IO),
                mercury_output_var(VarSet, VarNamePrint, OldOutermost,
                    Stream, !IO)
            ;
                MaybeOldOutermost = no
            ),
            io.write_string(Stream, "\n", !IO)
        ;
            MaybeDeepLayout = no
        )
    ;
        MaybeDeepProfileInfo = no
    ).

:- pred write_hlds_proc_static(io.text_output_stream::in,
    hlds_proc_static::in, io::di, io::uo) is det.

write_hlds_proc_static(Stream, ProcStatic, !IO) :-
    ProcStatic = hlds_proc_static(FileName, LineNumber,
        InInterface, CallSiteStatics, CoveragePoints),
    io.format(Stream, "%% proc static filename: %s\n", [s(FileName)], !IO),
    io.format(Stream, "%% proc static line number: %d\n", [i(LineNumber)], !IO),
    io.write_string(Stream, "% proc static is interface: ", !IO),
    io.write_line(Stream, InInterface, !IO),
    list.foldl2(write_hlds_ps_call_site(Stream),
        CallSiteStatics, 0, _, !IO),
    list.foldl2(write_hlds_ps_coverage_point(Stream),
        CoveragePoints, 0, _, !IO).

:- pred write_hlds_ps_call_site(io.text_output_stream::in,
    call_site_static_data::in, int::in, int::out, io::di, io::uo) is det.

write_hlds_ps_call_site(Stream, CallSiteStaticData, !SlotNum, !IO) :-
    io.format(Stream, "%% call site static slot %d\n", [i(!.SlotNum)], !IO),
    (
        CallSiteStaticData = normal_call(CalleeRttiProcLabel, TypeSubst,
            FileName, LineNumber, GoalPath),
        io.write_string(Stream, "% normal call to ", !IO),
        io.write_line(Stream, CalleeRttiProcLabel, !IO),
        io.format(Stream, "%% type subst <%s>, goal path <%s>\n",
            [s(TypeSubst), s(goal_path_to_string(GoalPath))], !IO),
        io.format(Stream, "%% filename <%s>, line number <%d>\n",
            [s(FileName), i(LineNumber)], !IO)
    ;
        (
            CallSiteStaticData = special_call(FileName, LineNumber, GoalPath),
            io.write_string(Stream, "% special call\n", !IO)
        ;
            CallSiteStaticData = higher_order_call(FileName, LineNumber,
                GoalPath),
            io.write_string(Stream, "% higher order call\n", !IO)
        ;
            CallSiteStaticData = method_call(FileName, LineNumber, GoalPath),
            io.write_string(Stream, "% method call\n", !IO)
        ;
            CallSiteStaticData = callback(FileName, LineNumber, GoalPath),
            io.write_string(Stream, "% callback\n", !IO)
        ),
        io.format(Stream,
            "%% filename <%s>, line number <%d>, goal path <%s>\n",
            [s(FileName), i(LineNumber), s(goal_path_to_string(GoalPath))],
            !IO)
    ),
    !:SlotNum = !.SlotNum + 1.

:- pred write_hlds_ps_coverage_point(io.text_output_stream::in,
    coverage_point_info::in, int::in, int::out, io::di, io::uo) is det.

write_hlds_ps_coverage_point(Stream, CoveragePointInfo, !SlotNum, !IO) :-
    CoveragePointInfo = coverage_point_info(RevGoalPath, PointType),
    io.format(Stream, "%% coverage point slot %d: goal path <%s>, type %s\n",
        [i(!.SlotNum), s(rev_goal_path_to_string(RevGoalPath)),
            s(coverage_point_to_string(PointType))], !IO),
    !:SlotNum = !.SlotNum + 1.

:- func coverage_point_to_string(cp_type) = string.

coverage_point_to_string(cp_type_coverage_after) = "after".
coverage_point_to_string(cp_type_branch_arm) = "branch arm".

%---------------------%

:- pred write_proc_termination_info(io.text_output_stream::in, string::in,
    maybe(arg_size_info)::in, maybe(termination_info)::in,
    io::di, io::uo) is det.

write_proc_termination_info(Stream, DumpOptions,  MaybeArgSize,
        MaybeTermination, !IO) :-
    ( if string.contains_char(DumpOptions, 't') then
        SizeStr = maybe_arg_size_info_to_string(yes, MaybeArgSize),
        TermStr = maybe_termination_info_to_string(yes, MaybeTermination),
        io.format(Stream, "%% Arg size properties: %s\n", [s(SizeStr)], !IO),
        io.format(Stream, "%% Termination properties: %s\n", [s(TermStr)], !IO)
    else
        true
    ).

%---------------------%

:- pred write_proc_opt_info(io.text_output_stream::in, string::in,
    prog_varset::in, tvarset::in, var_name_print::in,
    maybe(structure_sharing_domain_and_status)::in,
    maybe(structure_reuse_domain_and_status)::in,
    maybe(untuple_proc_info)::in,
    io::di, io::uo) is det.

write_proc_opt_info(Stream, DumpOptions, VarSet, TVarSet, VarNamePrint,
        MaybeStructureSharing, MaybeStructureReuse, MaybeUntupleInfo, !IO) :-
    ( if
        string.contains_char(DumpOptions, 'S'),
        MaybeStructureSharing = yes(StructureSharing)
    then
        io.write_string(Stream, "% Structure sharing: \n", !IO),
        StructureSharing =
            structure_sharing_domain_and_status(SharingAs, _Status),
        dump_structure_sharing_domain(Stream, VarSet, TVarSet, SharingAs, !IO)
    else
        true
    ),
    ( if
        string.contains_char(DumpOptions, 'R'),
        MaybeStructureReuse = yes(StructureReuse)
    then
        io.write_string(Stream, "% Structure reuse: \n", !IO),
        StructureReuse =
            structure_reuse_domain_and_status(ReuseAs, _ReuseStatus),
        dump_structure_reuse_domain(Stream, VarSet, TVarSet, ReuseAs, !IO)
    else
        true
    ),
    (
        MaybeUntupleInfo = yes(UntupleInfo),
        write_untuple_info(Stream, VarSet, VarNamePrint, UntupleInfo, !IO)
    ;
        MaybeUntupleInfo = no
    ).

:- pred write_untuple_info(io.text_output_stream::in, prog_varset::in,
    var_name_print::in, untuple_proc_info::in, io::di, io::uo) is det.

write_untuple_info(Stream, VarSet, VarNamePrint, UntupleInfo, !IO) :-
    UntupleInfo = untuple_proc_info(UntupleMap),
    io.write_string(Stream, "% untuple:\n", !IO),
    map.foldl(write_untuple_info_loop(Stream, VarSet, VarNamePrint),
        UntupleMap, !IO).

:- pred write_untuple_info_loop(io.text_output_stream::in, prog_varset::in,
    var_name_print::in, prog_var::in, prog_vars::in, io::di, io::uo) is det.

write_untuple_info_loop(Stream, VarSet, VarNamePrint, OldVar, NewVars, !IO) :-
    io.format(Stream, "%%\t%s -> %s\n",
        [s(mercury_var_to_string(VarSet, VarNamePrint, OldVar)),
        s(mercury_vars_to_string(VarSet, VarNamePrint, NewVars))], !IO).

%---------------------%

:- pred write_proc_deleted_callee_set(io.text_output_stream::in,
    set(pred_proc_id)::in, io::di, io::uo) is det.

write_proc_deleted_callee_set(Stream, DeletedCallCalleeSet, !IO) :-
    set.to_sorted_list(DeletedCallCalleeSet, DeletedCallCallees),
    (
        DeletedCallCallees = []
    ;
        DeletedCallCallees = [_ | _],
        io.write_string(Stream,
            "% procedures called from deleted goals: ", !IO),
        io.write_line(Stream, DeletedCallCallees, !IO)
    ).

%---------------------%

:- pred write_pred_proc_var_name_remap(io.text_output_stream::in,
    prog_varset::in, map(prog_var, string)::in, io::di, io::uo) is det.

write_pred_proc_var_name_remap(Stream, VarSet, VarNameRemap, !IO) :-
    map.to_assoc_list(VarNameRemap, VarNameRemapList),
    (
        VarNameRemapList = []
    ;
        VarNameRemapList = [VarNameRemapHead | VarNameRemapTail],
        io.write_string(Stream, "% var name remap: ", !IO),
        write_var_name_remap(Stream, VarSet,
            VarNameRemapHead, VarNameRemapTail, !IO),
        io.nl(Stream, !IO)
    ).

:- pred write_var_name_remap(io.text_output_stream::in, prog_varset::in,
    pair(prog_var, string)::in, list(pair(prog_var, string))::in,
    io::di, io::uo) is det.

write_var_name_remap(Stream, VarSet, Head, Tail, !IO) :-
    Head = Var - NewName,
    mercury_output_var(VarSet, print_name_and_num, Var, Stream, !IO),
    io.write_string(Stream, " -> ", !IO),
    io.write_string(Stream, NewName, !IO),
    (
        Tail = []
    ;
        Tail = [TailHead | TailTail],
        io.write_string(Stream, ", ", !IO),
        write_var_name_remap(Stream, VarSet, TailHead, TailTail, !IO)
    ).

%---------------------%

:- pred write_proc_arg_info(io.text_output_stream::in, string::in,
    prog_varset::in, var_name_print::in,
    maybe(list(is_live))::in, set_of_progvar::in, maybe(list(arg_info))::in,
    io::di, io::uo) is det.

write_proc_arg_info(Stream, DumpOptions, VarSet, VarNamePrint,
        MaybeArgLives, RegR_HeadVars, MaybeArgInfos, !IO) :-
    (
        MaybeArgLives = yes(ArgLives),
        io.write_string(Stream, "% arg lives: ", !IO),
        io.print_line(Stream, ArgLives, !IO)
    ;
        MaybeArgLives = no
    ),
    ( if set_of_var.is_non_empty(RegR_HeadVars) then
        io.write_string(Stream, "% reg_r headvars: ", !IO),
        write_out_list(mercury_output_var(VarSet, VarNamePrint),
            ", ", set_of_var.to_sorted_list(RegR_HeadVars), Stream, !IO),
        io.nl(Stream, !IO)
    else
        true
    ),
    ( if
        string.contains_char(DumpOptions, 'A'),
        MaybeArgInfos = yes(ArgInfos)
    then
        io.write_string(Stream, "% arg_infos: ", !IO),
        io.print_line(Stream, ArgInfos, !IO)
    else
        true
    ).

%---------------------%

:- pred write_stack_slots(io.text_output_stream::in, prog_varset::in,
    var_name_print::in, stack_slots::in, io::di, io::uo) is det.

write_stack_slots(Stream, VarSet, VarNamePrint, StackSlots, !IO) :-
    map.to_assoc_list(StackSlots, VarSlotList0),
    VarSlotList = assoc_list.map_values_only(stack_slot_to_abs_locn,
        VarSlotList0),
    write_var_to_abs_locns(Stream, VarSet, VarNamePrint, 0, VarSlotList, !IO).

%---------------------------------------------------------------------------%
:- end_module hlds.hlds_out.hlds_out_pred.
%---------------------------------------------------------------------------%
