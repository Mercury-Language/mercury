%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2009-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: hlds_out_pred.m.
% Main authors: conway, fjh.
%
%-----------------------------------------------------------------------------%

:- module hlds.hlds_out.hlds_out_pred.
:- interface.

:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

:- pred write_pred(hlds_out_info::in, output_lang::in, int::in,
    module_info::in, pred_id::in, pred_info::in, io::di, io::uo) is det.

    % write_clause(Info, Lang, Indent, ModuleInfo, PredId, VarSet,
    %   AppendVarNums, HeadTerms, PredOrFunc, Clause, UseDeclaredModes,
    %   MaybeVarTypes, !IO).
    %
:- pred write_clause(hlds_out_info::in, output_lang::in, int::in,
    module_info::in, pred_id::in, prog_varset::in, bool::in,
    list(prog_term)::in, pred_or_func::in, clause::in, bool::in,
    maybe_vartypes::in, io::di, io::uo) is det.

    % write_proc(Info, Indent, AppendVarNums, ModuleInfo, PredId, ProcId,
    %    ImportStatus, Proc, !IO).
    %
:- pred write_proc(hlds_out_info::in, int::in, bool::in, module_info::in,
    pred_id::in, proc_id::in, import_status::in, proc_info::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- pred write_table_arg_infos(tvarset::in, table_arg_infos::in,
    io::di, io::uo) is det.

:- pred write_space_and_table_trie_step(tvarset::in,
    table_step_desc::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- func import_status_to_string(import_status) = string.

    % Print out the name of a marker.
    %
:- pred write_marker(marker::in, io::di, io::uo) is det.

    % Find the name of a marker.
    %
:- pred marker_name(marker::in, string::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_args.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_out.hlds_out_goal.
:- import_module hlds.hlds_out.hlds_out_mode.
:- import_module hlds.hlds_rtti.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.program_representation.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_ctgc.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.

:- import_module assoc_list.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%
%
% Write out predicates.
%

write_pred(Info, Lang, Indent, ModuleInfo, PredId, PredInfo, !IO) :-
    Module = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    pred_info_get_arg_types(PredInfo, ArgTypes),
    pred_info_get_exist_quant_tvars(PredInfo, ExistQVars),
    pred_info_get_typevarset(PredInfo, TVarSet),
    pred_info_get_clauses_info(PredInfo, ClausesInfo),
    pred_info_get_context(PredInfo, Context),
    pred_info_get_import_status(PredInfo, ImportStatus),
    pred_info_get_markers(PredInfo, Markers),
    pred_info_get_class_context(PredInfo, ClassContext),
    pred_info_get_constraint_proofs(PredInfo, Proofs),
    pred_info_get_constraint_map(PredInfo, ConstraintMap),
    pred_info_get_purity(PredInfo, Purity),
    pred_info_get_head_type_params(PredInfo, HeadTypeParams),
    pred_info_get_var_name_remap(PredInfo, VarNameRemap),
    DumpOptions = Info ^ hoi_dump_hlds_options,
    ( string.contains_char(DumpOptions, 'v') ->
        AppendVarNums = yes
    ;
        AppendVarNums = no
    ),
    ( string.contains_char(DumpOptions, 'C') ->
        % Information about predicates is dumped if 'C' suboption is on.
        (
            PredOrFunc = pf_predicate,
            mercury_output_pred_type(TVarSet, ExistQVars,
                qualified(Module, PredName), ArgTypes, no, Purity,
                ClassContext, Context, AppendVarNums, !IO)
        ;
            PredOrFunc = pf_function,
            pred_args_to_func_args(ArgTypes, FuncArgTypes, FuncRetType),
            mercury_output_func_type(TVarSet, ExistQVars,
                qualified(Module, PredName), FuncArgTypes, FuncRetType, no,
                Purity, ClassContext, Context, AppendVarNums, !IO)
        )
    ;
        true
    ),
    ClausesInfo = clauses_info(VarSet, _, _, VarTypes, HeadVars, ClausesRep,
        _ItemNumbers, RttiVarMaps, _HaveForeignClauses),
    ( string.contains_char(DumpOptions, 'C') ->
        write_indent(Indent, !IO),
        io.write_string("% pred id: ", !IO),
        pred_id_to_int(PredId, PredInt),
        io.write_int(PredInt, !IO),
        io.write_string(", category: ", !IO),
        write_pred_or_func(PredOrFunc, !IO),
        io.write_string(", status: ", !IO),
        io.write_string(import_status_to_string(ImportStatus), !IO),
        io.write_string("\n", !IO),
        io.write_string("% goal_type: ", !IO),
        pred_info_get_goal_type(PredInfo, GoalType),
        io.write(GoalType, !IO),
        io.write_string("\n", !IO),
        markers_to_marker_list(Markers, MarkerList),
        (
            MarkerList = []
        ;
            MarkerList = [_ | _],
            io.write_string("% markers: ", !IO),
            write_marker_list(MarkerList, !IO),
            io.write_string("\n", !IO)
        ),
        write_rtti_varmaps(Indent, AppendVarNums, RttiVarMaps, VarSet, TVarSet,
            !IO),
        ( map.is_empty(Proofs) ->
            true
        ;
            write_constraint_proofs(Indent, TVarSet, Proofs, AppendVarNums,
                !IO),
            io.write_string("\n", !IO)
        ),
        ( map.is_empty(ConstraintMap) ->
            true
        ;
            write_constraint_map(Indent, TVarSet, ConstraintMap, AppendVarNums,
                !IO)
        ),

        (
            HeadTypeParams = [_ | _],
            io.write_string("% head_type_params:\n", !IO),
            io.write_string("% ", !IO),
            mercury_output_vars(TVarSet, AppendVarNums, HeadTypeParams, !IO),
            io.write_string("\n", !IO)
        ;
            HeadTypeParams = []
        ),
        write_var_types(Indent, VarSet, AppendVarNums, VarTypes, TVarSet, !IO),

        map.to_assoc_list(VarNameRemap, VarNameRemapList),
        (
            VarNameRemapList = []
        ;
            VarNameRemapList = [VarNameRemapHead | VarNameRemapTail],
            write_indent(Indent, !IO),
            io.write_string("% var name remap: ", !IO),
            write_var_name_remap(VarNameRemapHead, VarNameRemapTail, VarSet,
                !IO),
            io.nl(!IO)
        ),

        get_clause_list(ClausesRep, Clauses),
        (
            Clauses = [_ | _],
            set_dump_opts_for_clauses(Info, InfoForClauses),
            write_clauses(InfoForClauses, Lang, Indent, ModuleInfo,
                PredId, VarSet, AppendVarNums, HeadVars, PredOrFunc, Clauses,
                no_varset_vartypes, !IO)
        ;
            Clauses = []
        ),

        pred_info_get_origin(PredInfo, Origin),
        (
            Origin = origin_instance_method(_, MethodConstraints),
            MethodConstraints = instance_method_constraints(ClassId,
                InstanceTypes, InstanceConstraints, ClassMethodConstraints),
            io.write_string("% instance method constraints:\n", !IO),
            ClassId = class_id(ClassName, _),
            mercury_output_constraint(TVarSet, AppendVarNums,
                constraint(ClassName, InstanceTypes), !IO),
            io.nl(!IO),
            io.write_string("instance constraints: ", !IO),
            io.write_list(InstanceConstraints, ", ",
                mercury_output_constraint(TVarSet, AppendVarNums), !IO),
            io.nl(!IO),

            ClassMethodConstraints = constraints(MethodUnivConstraints,
                MethodExistConstraints),
            io.write_string("method univ constraints: ", !IO),
            io.write_list(MethodUnivConstraints, ", ",
                mercury_output_constraint(TVarSet, AppendVarNums), !IO),
            io.nl(!IO),
            io.write_string("method exist constraints: ", !IO),
            io.write_list(MethodExistConstraints, ", ",
                mercury_output_constraint(TVarSet, AppendVarNums), !IO),
            io.nl(!IO)
        ;
            Origin = origin_special_pred(_),
            io.write_string("% special pred\n", !IO)
        ;
            Origin = origin_transformed(Transformation, _, OrigPredId),
            OrigPredIdNum = pred_id_to_int(OrigPredId),
            io.format("%% transformed from pred id %d\n",
                [i(OrigPredIdNum)], !IO),
            io.write_string("% ", !IO),
            write_pred_id(ModuleInfo, OrigPredId, !IO),
            io.nl(!IO),
            io.write_string("% transformation: ", !IO),
            io.write(Transformation, !IO),
            io.nl(!IO)
        ;
            Origin = origin_created(Creation),
            io.write_string("% created: ", !IO),
            io.write(Creation, !IO),
            io.nl(!IO)
        ;
            Origin = origin_assertion(_, _),
            io.write_string("% assertion\n", !IO)
        ;
            Origin = origin_lambda(_, _, _)
        ;
            Origin = origin_user(_)
        )
    ;
        true
    ),
    write_procs(Info, Indent, AppendVarNums, ModuleInfo, PredId, ImportStatus,
        PredInfo, !IO),
    io.write_string("\n", !IO).

:- pred set_dump_opts_for_clauses(hlds_out_info::in, hlds_out_info::out)
    is det.

set_dump_opts_for_clauses(Info, ClausesInfo) :-
    OptionsStr = Info ^ hoi_dump_hlds_options,
    some [!DumpStr] (
        !:DumpStr = "",
        ( string.contains_char(OptionsStr, 'c') ->
            !:DumpStr = !.DumpStr ++ "c"
        ;
            true
        ),
        ( string.contains_char(OptionsStr, 'n') ->
            !:DumpStr = !.DumpStr ++ "n"
        ;
            true
        ),
        ( string.contains_char(OptionsStr, 'v') ->
            !:DumpStr = !.DumpStr ++ "v"
        ;
            true
        ),
        ( string.contains_char(OptionsStr, 'g') ->
            !:DumpStr = !.DumpStr ++ "g"
        ;
            true
        ),
        ( string.contains_char(OptionsStr, 'P') ->
            !:DumpStr = !.DumpStr ++ "P"
        ;
            true
        ),
        DumpStr = !.DumpStr
    ),
    ClausesInfo = Info ^ hoi_dump_hlds_options := DumpStr.

    % write_clauses(Info, Indent, ModuleInfo, PredId, VarSet,
    %   AppendVarNums, HeadVars, PredOrFunc, Clauses, MaybeVarTypes, !IO).
    %
:- pred write_clauses(hlds_out_info::in, output_lang::in, int::in,
    module_info::in, pred_id::in, prog_varset::in, bool::in,
    proc_arg_vector(prog_var)::in, pred_or_func::in, list(clause)::in,
    maybe_vartypes::in, io::di, io::uo) is det.

write_clauses(Info, Lang, Indent, ModuleInfo, PredId, VarSet, AppendVarNums,
        HeadVars, PredOrFunc, Clauses0, TypeQual, !IO) :-
    HeadVarList = proc_arg_vector_to_list(HeadVars),
    write_clauses_loop(Info, Lang, Indent, ModuleInfo, PredId, VarSet,
        AppendVarNums, HeadVarList, PredOrFunc, Clauses0, TypeQual, 1, !IO).

:- pred write_clauses_loop(hlds_out_info::in, output_lang::in, int::in,
    module_info::in, pred_id::in, prog_varset::in, bool::in,
    list(prog_var)::in, pred_or_func::in, list(clause)::in, maybe_vartypes::in,
    int::in, io::di, io::uo) is det.

write_clauses_loop(Info, Lang, Indent, ModuleInfo, PredId,
        VarSet, AppendVarNums, HeadVars, PredOrFunc, Clauses0,
        TypeQual, ClauseNum, !IO) :-
    (
        Clauses0 = [Clause | Clauses],
        term.var_list_to_term_list(HeadVars, HeadTerms),
        UseDeclaredModes = no,
        io.write_string("% clause ", !IO),
        io.write_int(ClauseNum, !IO),
        io.write_string("\n", !IO),
        write_clause(Info, Lang, Indent, ModuleInfo, PredId, VarSet,
            AppendVarNums, HeadTerms, PredOrFunc, Clause,
            UseDeclaredModes, TypeQual, !IO),
        write_clauses_loop(Info, Lang, Indent, ModuleInfo, PredId, VarSet,
            AppendVarNums, HeadVars, PredOrFunc, Clauses, TypeQual,
            ClauseNum + 1, !IO)
    ;
        Clauses0 = []
    ).

write_clause(Info, Lang, Indent, ModuleInfo, PredId, VarSet, AppendVarNums,
        HeadTerms, PredOrFunc, Clause, UseDeclaredModes, TypeQual, !IO) :-
    Clause = clause(ApplicableModes, Goal, ImplLang, Context,
        _StateVarWarnings),
    Indent1 = Indent + 1,
    DumpOptions = Info ^ hoi_dump_hlds_options,
    (
        ApplicableModes = all_modes
    ;
        ApplicableModes = selected_modes(Modes),
        ( string.contains_char(DumpOptions, 'm') ->
            write_indent(Indent, !IO),
            io.write_string("% Modes for which this clause applies: ", !IO),
            ModeInts = list.map(proc_id_to_int, Modes),
            write_intlist(ModeInts, !IO),
            io.write_string("\n", !IO)
        ;
            true
        )
    ),
    (
        ImplLang = impl_lang_mercury
    ;
        ImplLang = impl_lang_foreign(ForeignLang),
        io.write_string("% Language of implementation: ", !IO),
        io.write(ForeignLang, !IO),
        io.nl(!IO)
    ),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    AllProcIds = pred_info_procids(PredInfo),
    (
        ApplicableModes = selected_modes(SelectedProcIds),
        SelectedProcIds \= AllProcIds
    ->
        % If SelectedProcIds contains more than one mode, the output will have
        % multiple clause heads. This won't be pretty and it won't be
        % syntactically valid, but it is more useful for debugging
        % than a compiler abort during the dumping process.
        write_annotated_clause_heads(ModuleInfo, Lang, Context, PredId,
            SelectedProcIds, VarSet, AppendVarNums, HeadTerms, PredOrFunc,
            UseDeclaredModes, !IO)
    ;
        write_clause_head(ModuleInfo, PredId, VarSet, AppendVarNums,
            HeadTerms, PredOrFunc, !IO)
    ),
    ( Goal = hlds_goal(conj(plain_conj, []), _GoalInfo) ->
        io.write_string(".\n", !IO)
    ;
        io.write_string(" :-\n", !IO),
        do_write_goal(Info, Goal, ModuleInfo, VarSet, AppendVarNums,
            Indent1, ".\n", TypeQual, !IO)
    ).

:- pred write_annotated_clause_heads(module_info::in, output_lang::in,
    term.context::in, pred_id::in, list(proc_id)::in, prog_varset::in,
    bool::in, list(prog_term)::in, pred_or_func::in, bool::in,
    io::di, io::uo) is det.

write_annotated_clause_heads(_, _, _, _, [], _, _, _, _, _, !IO).
write_annotated_clause_heads(ModuleInfo, Lang, Context, PredId,
        [ProcId | ProcIds], VarSet, AppendVarNums, HeadTerms, PredOrFunc,
        UseDeclaredModes, !IO) :-
    write_annotated_clause_head(ModuleInfo, Lang, Context, PredId,
        ProcId, VarSet, AppendVarNums, HeadTerms,
        PredOrFunc, UseDeclaredModes, !IO),
    write_annotated_clause_heads(ModuleInfo, Lang, Context, PredId,
        ProcIds, VarSet, AppendVarNums, HeadTerms,
        PredOrFunc, UseDeclaredModes, !IO).

:- pred write_annotated_clause_head(module_info::in, output_lang::in,
    term.context::in, pred_id::in, proc_id::in, prog_varset::in, bool::in,
    list(prog_term)::in, pred_or_func::in, bool::in, io::di, io::uo) is det.

write_annotated_clause_head(ModuleInfo, Lang, Context, PredId, ProcId, VarSet,
        AppendVarNums, HeadTerms, PredOrFunc, UseDeclaredModes, !IO) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_procedures(PredInfo, Procedures),
    ( map.search(Procedures, ProcId, ProcInfo) ->
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
            UseDeclaredModes = yes,
            proc_info_declared_argmodes(ProcInfo, ArgModes)
        ;
            UseDeclaredModes = no,
            proc_info_get_argmodes(ProcInfo, ArgModes)
        ),
        assoc_list.from_corresponding_lists(HeadTerms, ArgModes,
            AnnotatedPairs),
        AnnotatedHeadTerms = list.map(add_mode_qualifier(Lang, Context),
            AnnotatedPairs),
        write_clause_head(ModuleInfo, PredId, VarSet, AppendVarNums,
            AnnotatedHeadTerms, PredOrFunc, !IO)
    ;
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

:- pred write_clause_head(module_info::in, pred_id::in,
    prog_varset::in, bool::in, list(prog_term)::in, pred_or_func::in,
    io::di, io::uo) is det.

write_clause_head(ModuleInfo, PredId, VarSet, AppendVarNums, HeadTerms,
        PredOrFunc, !IO) :-
    PredName = predicate_name(ModuleInfo, PredId),
    ModuleName = predicate_module(ModuleInfo, PredId),
    (
        PredOrFunc = pf_function,
        pred_args_to_func_args(HeadTerms, FuncArgs, RetVal),
        write_qualified_functor_with_term_args(ModuleName,
            term.atom(PredName), FuncArgs, VarSet, AppendVarNums, !IO),
        io.write_string(" = ", !IO),
        mercury_output_term_nq(VarSet, AppendVarNums, next_to_graphic_token,
            RetVal, !IO)
    ;
        PredOrFunc = pf_predicate,
        write_qualified_functor_with_term_args(ModuleName,
            term.atom(PredName), HeadTerms, VarSet, AppendVarNums, !IO)
    ).

import_status_to_string(status_local) =
    "local".
import_status_to_string(status_exported) =
    "exported".
import_status_to_string(status_opt_exported) =
    "opt_exported".
import_status_to_string(status_abstract_exported) =
    "abstract_exported".
import_status_to_string(status_pseudo_exported) =
    "pseudo_exported".
import_status_to_string(status_imported(import_locn_interface)) =
    "imported in the interface".
import_status_to_string(status_imported(import_locn_implementation)) =
    "imported in the implementation".
import_status_to_string(status_imported(
        import_locn_ancestor_private_interface_proper)) =
    "imported from an ancestor's private interface".
import_status_to_string(status_imported(import_locn_ancestor)) =
    "imported by an ancestor".
import_status_to_string(status_external(Status)) =
    "external (and " ++ import_status_to_string(Status) ++ ")".
import_status_to_string(status_abstract_imported) =
    "abstract_imported".
import_status_to_string(status_opt_imported) =
    "opt_imported".
import_status_to_string(status_pseudo_imported) =
    "pseudo_imported".
import_status_to_string(status_exported_to_submodules) =
    "exported_to_submodules".

:- pred write_var_types(int::in, prog_varset::in, bool::in,
    vartypes::in, tvarset::in, io::di, io::uo) is det.

write_var_types(Indent, VarSet, AppendVarNums, VarTypes, TVarSet, !IO) :-
    vartypes_count(VarTypes, NumVarTypes),
    write_indent(Indent, !IO),
    io.write_string("% variable types map ", !IO),
    io.format("(%d entries):\n", [i(NumVarTypes)], !IO),
    vartypes_vars(VarTypes, Vars),
    write_var_types_loop(Vars, Indent, VarSet, AppendVarNums, VarTypes,
        TVarSet, !IO).

:- pred write_var_types_loop(list(prog_var)::in, int::in, prog_varset::in,
    bool::in, vartypes::in, tvarset::in, io::di, io::uo) is det.

write_var_types_loop([], _, _, _, _, _, !IO).
write_var_types_loop([Var | Vars], Indent, VarSet, AppendVarNums, VarTypes,
        TypeVarSet, !IO) :-
    lookup_var_type(VarTypes, Var, Type),
    write_indent(Indent, !IO),
    io.write_string("% ", !IO),
    mercury_output_var(VarSet, AppendVarNums, Var, !IO),
    io.write_string(" (number ", !IO),
    term.var_to_int(Var, VarNum),
    io.write_int(VarNum, !IO),
    io.write_string(")", !IO),
    io.write_string(": ", !IO),
    mercury_output_type(TypeVarSet, AppendVarNums, Type, !IO),
    io.write_string("\n", !IO),
    write_var_types_loop(Vars, Indent, VarSet, AppendVarNums, VarTypes,
        TypeVarSet, !IO).

:- pred write_rtti_varmaps(int::in, bool::in, rtti_varmaps::in,
    prog_varset::in, tvarset::in, io::di, io::uo) is det.

write_rtti_varmaps(Indent, AppendVarNums, RttiVarMaps, VarSet, TVarSet, !IO) :-
    write_indent(Indent, !IO),
    io.write_string("% type_info varmap:\n", !IO),
    rtti_varmaps_tvars(RttiVarMaps, TypeVars),
    list.foldl(write_type_info_locn(Indent, AppendVarNums, RttiVarMaps,
        VarSet, TVarSet), TypeVars, !IO),
    write_indent(Indent, !IO),
    io.write_string("% typeclass_info varmap:\n", !IO),
    rtti_varmaps_reusable_constraints(RttiVarMaps, Constraints),
    list.foldl(write_typeclass_info_var(Indent, AppendVarNums, RttiVarMaps,
        VarSet, TVarSet), Constraints, !IO),
    write_indent(Indent, !IO),
    io.write_string("% rtti_var_info:\n", !IO),
    rtti_varmaps_rtti_prog_vars(RttiVarMaps, ProgVars),
    list.foldl(write_rtti_var_info(Indent, AppendVarNums, RttiVarMaps,
        VarSet, TVarSet), ProgVars, !IO).

:- pred write_type_info_locn(int::in, bool::in, rtti_varmaps::in,
    prog_varset::in, tvarset::in, tvar::in, io::di, io::uo) is det.

write_type_info_locn(Indent, AppendVarNums, RttiVarMaps, VarSet, TVarSet, TVar,
        !IO) :-
    write_indent(Indent, !IO),
    io.write_string("% ", !IO),

    mercury_output_var(TVarSet, AppendVarNums, TVar, !IO),
    io.write_string(" (number ", !IO),
    term.var_to_int(TVar, TVarNum),
    io.write_int(TVarNum, !IO),
    io.write_string(")", !IO),

    io.write_string(" -> ", !IO),
    rtti_lookup_type_info_locn(RttiVarMaps, TVar, Locn),
    (
        Locn = type_info(Var),
        io.write_string("type_info(", !IO),
        mercury_output_var(VarSet, AppendVarNums, Var, !IO),
        io.write_string(") ", !IO)
    ;
        Locn = typeclass_info(Var, Index),
        io.write_string("typeclass_info(", !IO),
        mercury_output_var(VarSet, AppendVarNums, Var, !IO),
        io.write_string(", ", !IO),
        io.write_int(Index, !IO),
        io.write_string(") ", !IO)
    ),
    io.write_string(" (number ", !IO),
    term.var_to_int(Var, VarNum),
    io.write_int(VarNum, !IO),
    io.write_string(")", !IO),
    io.write_string("\n", !IO).

:- pred write_typeclass_info_var(int::in, bool::in, rtti_varmaps::in,
    prog_varset::in, tvarset::in, prog_constraint::in, io::di, io::uo) is det.

write_typeclass_info_var(Indent, AppendVarNums, RttiVarMaps, VarSet, TVarSet,
        Constraint, !IO) :-
    write_indent(Indent, !IO),
    io.write_string("% ", !IO),
    mercury_output_constraint(TVarSet, AppendVarNums, Constraint, !IO),
    io.write_string(" -> ", !IO),
    rtti_lookup_typeclass_info_var(RttiVarMaps, Constraint, Var),
    mercury_output_var(VarSet, AppendVarNums, Var, !IO),
    io.nl(!IO).

:- pred write_rtti_var_info(int::in, bool::in, rtti_varmaps::in,
    prog_varset::in, tvarset::in, prog_var::in, io::di, io::uo) is det.

write_rtti_var_info(Indent, AppendVarNums, RttiVarMaps, VarSet, TVarSet, Var,
        !IO) :-
    write_indent(Indent, !IO),
    io.write_string("% ", !IO),
    mercury_output_var(VarSet, AppendVarNums, Var, !IO),
    io.write_string(" (number ", !IO),
    term.var_to_int(Var, VarNum),
    io.write_int(VarNum, !IO),
    io.write_string(") ", !IO),
    io.write_string(" -> ", !IO),
    rtti_varmaps_var_info(RttiVarMaps, Var, VarInfo),
    (
        VarInfo = type_info_var(Type),
        io.write_string("type_info for ", !IO),
        mercury_output_type(TVarSet, AppendVarNums, Type, !IO)
    ;
        VarInfo = typeclass_info_var(Constraint),
        io.write_string("typeclass_info for ", !IO),
        mercury_output_constraint(TVarSet, AppendVarNums, Constraint, !IO)
    ;
        VarInfo = non_rtti_var,
        unexpected($module, $pred, "non rtti var")
    ),
    io.nl(!IO).

:- pred write_stack_slots(int::in, stack_slots::in, prog_varset::in,
    bool::in, io::di, io::uo) is det.

write_stack_slots(Indent, StackSlots, VarSet, AppendVarNums, !IO) :-
    map.to_assoc_list(StackSlots, VarSlotList0),
    VarSlotList = assoc_list.map_values_only(stack_slot_to_abs_locn,
        VarSlotList0),
    write_var_to_abs_locns(VarSlotList, VarSet, AppendVarNums, Indent, !IO).

:- pred write_untuple_info(untuple_proc_info::in, prog_varset::in,
    bool::in, int::in, io::di, io::uo) is det.

write_untuple_info(untuple_proc_info(UntupleMap), VarSet, AppendVarNums,
        Indent, !IO) :-
    write_indent(Indent, !IO),
    io.write_string("% untuple:\n", !IO),
    map.foldl(write_untuple_info_loop(VarSet, AppendVarNums, Indent),
        UntupleMap, !IO).

:- pred write_untuple_info_loop(prog_varset::in, bool::in, int::in,
    prog_var::in, prog_vars::in, io::di, io::uo) is det.

write_untuple_info_loop(VarSet, AppendVarNums, Indent, OldVar, NewVars, !IO) :-
    write_indent(Indent, !IO),
    io.write_string("%\t", !IO),
    mercury_output_var(VarSet, AppendVarNums, OldVar, !IO),
    io.write_string("\t-> ", !IO),
    mercury_output_vars(VarSet, AppendVarNums, NewVars, !IO),
    io.nl(!IO).

:- pred write_var_name_remap(pair(prog_var, string)::in,
    list(pair(prog_var, string))::in, prog_varset::in, io::di, io::uo) is det.

write_var_name_remap(Head, Tail, VarSet, !IO) :-
    Head = Var - NewName,
    AppendVarNums = yes,
    mercury_output_var(VarSet, AppendVarNums, Var, !IO),
    io.write_string(" -> ", !IO),
    io.write_string(NewName, !IO),
    (
        Tail = []
    ;
        Tail = [TailHead | TailTail],
        io.write_string(", ", !IO),
        write_var_name_remap(TailHead, TailTail, VarSet, !IO)
    ).

%-----------------------------------------------------------------------------%
%
% Write out procedures.
%

:- pred write_procs(hlds_out_info::in, int::in, bool::in, module_info::in,
    pred_id::in, import_status::in, pred_info::in, io::di, io::uo) is det.

write_procs(Info, Indent, AppendVarNums, ModuleInfo, PredId, ImportStatus,
        PredInfo, !IO) :-
    pred_info_get_procedures(PredInfo, ProcTable),
    ProcIds = pred_info_procids(PredInfo),
    write_procs_loop(Info, ProcIds, AppendVarNums, ModuleInfo, Indent, PredId,
        ImportStatus, ProcTable, !IO).

:- pred write_procs_loop(hlds_out_info::in, list(proc_id)::in, bool::in,
    module_info::in, int::in, pred_id::in, import_status::in, proc_table::in,
    io::di, io::uo) is det.

write_procs_loop(_, [], _, _, _, _, _, _, !IO).
write_procs_loop(Info, [ProcId | ProcIds], AppendVarNums, ModuleInfo, Indent,
        PredId, ImportStatus, ProcTable, !IO) :-
    map.lookup(ProcTable, ProcId, ProcInfo),
    write_proc(Info, Indent, AppendVarNums, ModuleInfo, PredId, ProcId,
        ImportStatus, ProcInfo, !IO),
    write_procs_loop(Info, ProcIds, AppendVarNums, ModuleInfo, Indent,
        PredId, ImportStatus, ProcTable, !IO).

write_proc(Info, Indent, AppendVarNums, ModuleInfo, PredId, ProcId,
        ImportStatus, Proc, !IO) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_typevarset(PredInfo, TVarSet),
    proc_info_get_vartypes(Proc, VarTypes),
    proc_info_get_declared_determinism(Proc, DeclaredDeterminism),
    proc_info_get_inferred_determinism(Proc, InferredDeterminism),
    proc_info_get_varset(Proc, VarSet),
    proc_info_get_headvars(Proc, HeadVars),
    proc_info_get_argmodes(Proc, HeadModes),
    proc_info_get_maybe_arglives(Proc, MaybeArgLives),
    proc_info_get_reg_r_headvars(Proc, RegR_HeadVars),
    proc_info_maybe_arg_info(Proc, MaybeArgInfos),
    proc_info_get_goal(Proc, Goal),
    proc_info_get_context(Proc, ModeContext),
    proc_info_get_maybe_arg_size_info(Proc, MaybeArgSize),
    proc_info_get_maybe_termination_info(Proc, MaybeTermination),
    proc_info_get_structure_sharing(Proc, MaybeStructureSharing),
    proc_info_get_structure_reuse(Proc, MaybeStructureReuse),
    proc_info_get_rtti_varmaps(Proc, RttiVarMaps),
    proc_info_get_eval_method(Proc, EvalMethod),
    proc_info_get_is_address_taken(Proc, IsAddressTaken),
    proc_info_get_has_parallel_conj(Proc, HasParallelConj),
    proc_info_get_has_user_event(Proc, HasUserEvent),
    proc_info_get_maybe_proc_table_io_info(Proc, MaybeProcTableIOInfo),
    proc_info_get_call_table_tip(Proc, MaybeCallTableTip),
    proc_info_get_maybe_deep_profile_info(Proc, MaybeDeepProfileInfo),
    proc_info_get_maybe_untuple_info(Proc, MaybeUntupleInfo),
    proc_info_get_var_name_remap(Proc, VarNameRemap),
    Indent1 = Indent + 1,

    DumpOptions = Info ^ hoi_dump_hlds_options,
    ( string.contains_char(DumpOptions, 'x') ->
        write_indent(Indent1, !IO),
        io.write_string("% pred id ", !IO),
        pred_id_to_int(PredId, PredInt),
        io.write_int(PredInt, !IO),
        io.nl(!IO),
        write_indent(Indent1, !IO),
        io.write_string("% mode number ", !IO),
        proc_id_to_int(ProcId, ProcInt),
        io.write_int(ProcInt, !IO),
        io.write_string(" of ", !IO),
        write_pred_id(ModuleInfo, PredId, !IO),
        io.write_string(" (", !IO),
        io.write_string(determinism_to_string(InferredDeterminism), !IO),
        io.write_string("):\n", !IO),

        ( string.contains_char(DumpOptions, 't') ->
            write_indent(Indent, !IO),
            io.write_string("% Arg size properties: ", !IO),
            write_maybe_arg_size_info(MaybeArgSize, yes, !IO),
            io.nl(!IO),
            write_indent(Indent, !IO),
            io.write_string("% Termination properties: ", !IO),
            write_maybe_termination_info(MaybeTermination, yes, !IO),
            io.nl(!IO)
        ;
            true
        ),

        % Dump structure sharing information.
        ( string.contains_char(DumpOptions, 'S') ->
            write_indent(Indent, !IO),
            io.write_string("% Structure sharing: \n", !IO),
            (
                MaybeStructureSharing = yes(StructureSharing),
                StructureSharing =
                    structure_sharing_domain_and_status(SharingAs, _Status),
                dump_maybe_structure_sharing_domain(VarSet, TVarSet,
                    yes(SharingAs), !IO)
            ;
                MaybeStructureSharing = no,
                dump_maybe_structure_sharing_domain(VarSet, TVarSet, no, !IO)
            )
        ;
            true
        ),

        % Dump structure reuse information.
        ( string.contains_char(DumpOptions, 'R') ->
            write_indent(Indent, !IO),
            io.write_string("% Structure reuse: \n", !IO),
            (
                MaybeStructureReuse = yes(StructureReuse),
                StructureReuse =
                    structure_reuse_domain_and_status(ReuseAs, _ReuseStatus),
                dump_maybe_structure_reuse_domain(VarSet, TVarSet,
                    yes(ReuseAs), !IO)
            ;
                MaybeStructureReuse = no,
                dump_maybe_structure_reuse_domain(VarSet, TVarSet, no, !IO)
            )
        ;
            true
        ),

        write_indent(Indent, !IO),
        write_var_types(Indent, VarSet, AppendVarNums, VarTypes, TVarSet, !IO),
        write_rtti_varmaps(Indent, AppendVarNums, RttiVarMaps, VarSet, TVarSet,
            !IO),

        (
            IsAddressTaken = address_is_taken,
            io.write_string("% address is taken\n", !IO)
        ;
            IsAddressTaken = address_is_not_taken,
            io.write_string("% address is not taken\n", !IO)
        ),
        (
            HasParallelConj = has_parallel_conj,
            io.write_string("% contains parallel conjunction\n", !IO)
        ;
            HasParallelConj = has_no_parallel_conj,
            io.write_string("% does not contain parallel conjunction\n", !IO)
        ),
        (
            HasUserEvent = has_user_event,
            io.write_string("% contains user event\n", !IO)
        ;
            HasUserEvent = has_no_user_event,
            io.write_string("% does not contain user event\n", !IO)
        ),
        (
            EvalMethod = eval_normal
        ;
            ( EvalMethod = eval_loop_check
            ; EvalMethod = eval_memo
            ; EvalMethod = eval_minimal(_)
            ; EvalMethod = eval_table_io(_, _)
            ),
            io.write_string("% eval method: ", !IO),
            write_eval_method(EvalMethod, !IO),
            io.write_string("\n", !IO)
        ),
        (
            MaybeProcTableIOInfo = yes(ProcTableIOInfo),
            write_proc_table_io_info(TVarSet, ProcTableIOInfo, !IO)
        ;
            MaybeProcTableIOInfo = no
        ),
        (
            MaybeCallTableTip = yes(CallTableTip),
            io.write_string("% call table tip: ", !IO),
            mercury_output_var(VarSet, AppendVarNums, CallTableTip, !IO),
            io.write_string("\n", !IO)
        ;
            MaybeCallTableTip = no
        ),
        (
            MaybeDeepProfileInfo = yes(DeepProfileInfo),
            DeepProfileInfo = deep_profile_proc_info(MaybeRecInfo,
                MaybeDeepLayout, _),
            (
                MaybeRecInfo = yes(DeepRecInfo),
                DeepRecInfo = deep_recursion_info(Role, _),
                io.write_string("% deep recursion info: ", !IO),
                (
                    Role = deep_prof_inner_proc(DeepPredProcId),
                    io.write_string("inner, outer is ", !IO)
                ;
                    Role = deep_prof_outer_proc(DeepPredProcId),
                    io.write_string("outer, inner is ", !IO)
                ),
                DeepPredProcId = proc(DeepPredId, DeepProcId),
                pred_id_to_int(DeepPredId, DeepPredInt),
                proc_id_to_int(DeepProcId, DeepProcInt),
                io.write_int(DeepPredInt, !IO),
                io.write_string("/", !IO),
                io.write_int(DeepProcInt, !IO),
                io.write_string("\n", !IO)
            ;
                MaybeRecInfo = no
            ),
            (
                MaybeDeepLayout = yes(DeepLayout),
                DeepLayout = hlds_deep_layout(ProcStatic, ExcpVars),
                write_hlds_proc_static(ProcStatic, !IO),
                ExcpVars = hlds_deep_excp_vars(TopCSD, MiddleCSD,
                    MaybeOldOutermost),
                io.write_string("% deep layout info: ", !IO),
                io.write_string("TopCSD is ", !IO),
                mercury_output_var(VarSet, AppendVarNums, TopCSD, !IO),
                io.write_string(", MiddleCSD is ", !IO),
                mercury_output_var(VarSet, AppendVarNums, MiddleCSD, !IO),
                (
                    MaybeOldOutermost = yes(OldOutermost),
                    io.write_string(", OldOutermost is ", !IO),
                    mercury_output_var(VarSet, AppendVarNums, OldOutermost,
                        !IO)
                ;
                    MaybeOldOutermost = no
                ),
                io.write_string("\n", !IO)
            ;
                MaybeDeepLayout = no
            )
        ;
            MaybeDeepProfileInfo = no
        ),
        (
            MaybeUntupleInfo = yes(UntupleInfo),
            write_untuple_info(UntupleInfo, VarSet, AppendVarNums,
                Indent, !IO)
        ;
            MaybeUntupleInfo = no
        ),
        map.to_assoc_list(VarNameRemap, VarNameRemapList),
        (
            VarNameRemapList = []
        ;
            VarNameRemapList = [VarNameRemapHead | VarNameRemapTail],
            write_indent(Indent, !IO),
            io.write_string("% var name remap: ", !IO),
            write_var_name_remap(VarNameRemapHead, VarNameRemapTail, VarSet,
                !IO),
            io.nl(!IO)
        ),

        write_indent(Indent, !IO),
        PredName = predicate_name(ModuleInfo, PredId),
        PredOrFunc = pred_info_is_pred_or_func(PredInfo),
        varset.init(ModeVarSet),
        (
            PredOrFunc = pf_predicate,
            mercury_output_pred_mode_decl(ModeVarSet, unqualified(PredName),
                HeadModes, DeclaredDeterminism, ModeContext, !IO)
        ;
            PredOrFunc = pf_function,
            pred_args_to_func_args(HeadModes, FuncHeadModes, RetHeadMode),
            mercury_output_func_mode_decl(ModeVarSet, unqualified(PredName),
                FuncHeadModes, RetHeadMode, DeclaredDeterminism, ModeContext,
                !IO)
        ),

        (
            MaybeArgLives = yes(ArgLives),
            write_indent(Indent, !IO),
            io.write_string("% arg lives: ", !IO),
            io.print(ArgLives, !IO),
            io.nl(!IO)
        ;
            MaybeArgLives = no
        ),
        ( set_of_var.is_non_empty(RegR_HeadVars) ->
            write_indent(Indent, !IO),
            io.write_string("% reg_r headvars: ", !IO),
            io.write_list(set_of_var.to_sorted_list(RegR_HeadVars),
                ", ", mercury_output_var(VarSet, AppendVarNums), !IO),
            io.nl(!IO)
        ;
            true
        ),
        (
            string.contains_char(DumpOptions, 'A'),
            MaybeArgInfos = yes(ArgInfos)
        ->
            write_indent(Indent, !IO),
            io.write_string("% arg_infos: ", !IO),
            io.print(ArgInfos, !IO),
            io.nl(!IO)
        ;
            true
        ),
        (
            ImportStatus = status_pseudo_imported,
            hlds_pred.in_in_unification_proc_id(ProcId)
        ->
            true
        ;
            proc_info_get_stack_slots(Proc, StackSlots),
            write_indent(Indent, !IO),
            write_stack_slots(Indent, StackSlots, VarSet, AppendVarNums, !IO),
            write_indent(Indent, !IO),
            term.var_list_to_term_list(HeadVars, HeadTerms),
            write_clause_head(ModuleInfo, PredId, VarSet, AppendVarNums,
                HeadTerms, PredOrFunc, !IO),
            io.write_string(" :-\n", !IO),
            write_goal(Info, Goal, ModuleInfo, VarSet, AppendVarNums,
                Indent1, ".\n", !IO)
        )
    ;
        true
    ).

%-----------------------------------------------------------------------------%
%
% Write out proc static structures for deep profiling.
%

:- pred write_hlds_proc_static(hlds_proc_static::in, io::di, io::uo) is det.

write_hlds_proc_static(ProcStatic, !IO) :-
    ProcStatic = hlds_proc_static(FileName, LineNumber,
        InInterface, CallSiteStatics, CoveragePoints),
    io.write_string("% proc static filename: ", !IO),
    io.write_string(FileName, !IO),
    io.nl(!IO),
    io.write_string("% proc static line number: ", !IO),
    io.write_int(LineNumber, !IO),
    io.nl(!IO),
    io.write_string("% proc static is interface: ", !IO),
    io.write(InInterface, !IO),
    io.nl(!IO),
    list.foldl2(write_hlds_ps_call_site, CallSiteStatics, 0, _, !IO),
    list.foldl2(write_hlds_ps_coverage_point, CoveragePoints, 0, _, !IO).

:- pred write_hlds_ps_call_site(call_site_static_data::in, int::in, int::out,
    io::di, io::uo) is det.

write_hlds_ps_call_site(CallSiteStaticData, !SlotNum, !IO) :-
    io.format("%% call site static slot %d\n", [i(!.SlotNum)], !IO),
    (
        CallSiteStaticData = normal_call(CalleeRttiProcLabel, TypeSubst,
            FileName, LineNumber, GoalPath),
        io.write_string("% normal call to ", !IO),
        io.write(CalleeRttiProcLabel, !IO),
        io.nl(!IO),
        io.format("%% type subst <%s>, goal path <%s>\n",
            [s(TypeSubst), s(goal_path_to_string(GoalPath))], !IO),
        io.format("%% filename <%s>, line number <%d>\n",
            [s(FileName), i(LineNumber)], !IO)
    ;
        (
            CallSiteStaticData = special_call(FileName, LineNumber, GoalPath),
            io.write_string("% special call\n", !IO)
        ;
            CallSiteStaticData = higher_order_call(FileName, LineNumber,
                GoalPath),
            io.write_string("% higher order call\n", !IO)
        ;
            CallSiteStaticData = method_call(FileName, LineNumber, GoalPath),
            io.write_string("% method call\n", !IO)
        ;
            CallSiteStaticData = callback(FileName, LineNumber, GoalPath),
            io.write_string("% callback\n", !IO)
        ),
        io.format("%% filename <%s>, line number <%d>, goal path <%s>\n",
            [s(FileName), i(LineNumber), s(goal_path_to_string(GoalPath))],
            !IO)
    ),
    !:SlotNum = !.SlotNum + 1.

:- pred write_hlds_ps_coverage_point(coverage_point_info::in,
    int::in, int::out, io::di, io::uo) is det.

write_hlds_ps_coverage_point(CoveragePointInfo, !SlotNum, !IO) :-
    CoveragePointInfo = coverage_point_info(RevGoalPath, PointType),
    io.format("%% coverage point slot %d: goal path <%s>, type %s\n",
        [i(!.SlotNum), s(rev_goal_path_to_string(RevGoalPath)),
            s(coverage_point_to_string(PointType))], !IO),
    !:SlotNum = !.SlotNum + 1.

:- func coverage_point_to_string(cp_type) = string.

coverage_point_to_string(cp_type_coverage_after) = "after".
coverage_point_to_string(cp_type_branch_arm) = "branch arm".

%-----------------------------------------------------------------------------%
%
% Write out tabling information for "tabled for io" procedures.
%

:- pred write_proc_table_io_info(tvarset::in, proc_table_io_info::in,
    io::di, io::uo) is det.

write_proc_table_io_info(TVarSet, ProcTableIOInfo, !IO) :-
    ProcTableIOInfo = proc_table_io_info(MaybeArgInfos),
    (
        MaybeArgInfos = no,
        io.write_string("% proc table io info: io tabled, no arg_infos\n", !IO)
    ;
        MaybeArgInfos = yes(ArgInfos),
        io.write_string("% proc table io info: io tabled, arg_infos:\n", !IO),
        write_table_arg_infos(TVarSet, ArgInfos, !IO)
    ).

write_table_arg_infos(TVarSet, TableArgInfos, !IO) :-
    TableArgInfos = table_arg_infos(ArgInfos, TVarMap),
    io.write_string("% arg infos:\n", !IO),
    list.foldl(write_table_arg_info(TVarSet), ArgInfos, !IO),
    map.to_assoc_list(TVarMap, TVarPairs),
    (
        TVarPairs = []
    ;
        TVarPairs = [_ | _],
        io.write_string("% type var map:\n", !IO),
        list.foldl(write_table_tvar_map_entry(TVarSet), TVarPairs, !IO)
    ).

:- pred write_table_arg_info(tvarset::in, table_arg_info::in, io::di, io::uo)
    is det.

write_table_arg_info(TVarSet, ArgInfo, !IO) :-
    ArgInfo = table_arg_info(HeadVarNum, HeadVarName, SlotNum, Type),
    io.write_string("% ", !IO),
    io.write_string(HeadVarName, !IO),
    io.write_string("/", !IO),
    io.write_int(HeadVarNum, !IO),
    io.write_string(" in slot ", !IO),
    io.write_int(SlotNum, !IO),
    io.write_string(", type ", !IO),
    io.write_string(mercury_type_to_string(TVarSet, yes, Type), !IO),
    io.nl(!IO).

:- pred write_table_tvar_map_entry(tvarset::in,
    pair(tvar, table_locn)::in, io::di, io::uo) is det.

write_table_tvar_map_entry(TVarSet, TVar - Locn, !IO) :-
    io.write_string("% typeinfo for ", !IO),
    io.write_string(mercury_var_to_string(TVarSet, yes, TVar), !IO),
    io.write_string(" -> ", !IO),
    (
        Locn = table_locn_direct(N),
        io.format("direct in register %d\n", [i(N)], !IO)
    ;
        Locn = table_locn_indirect(N, O),
        io.format("indirect from register %d, offset %d\n", [i(N), i(O)], !IO)
    ).

write_space_and_table_trie_step(TVarSet, StepDesc, !IO) :-
    StepDesc = table_step_desc(VarName, TrieStep),
    io.write_string(" ", !IO),
    io.write_string(VarName, !IO),
    io.write_string(":", !IO),
    io.write_string(table_trie_step_desc(TVarSet, TrieStep), !IO).

:- func table_trie_step_desc(tvarset, table_trie_step) = string.

table_trie_step_desc(TVarSet, Step) = Str :-
    (
        Step = table_trie_step_int,
        Str = "int"
    ;
        Step = table_trie_step_char,
        Str = "char"
    ;
        Step = table_trie_step_string,
        Str = "string"
    ;
        Step = table_trie_step_float,
        Str = "float"
    ;
        Step = table_trie_step_dummy,
        Str = "dummy"
    ;
        Step = table_trie_step_enum(N),
        Str = "enum(" ++ int_to_string(N) ++ ")"
    ;
        Step = table_trie_step_foreign_enum,
        Str = "foreign_enum"
    ;
        Step = table_trie_step_general(Type, IsPoly, IsAddr),
        (
            IsPoly = table_is_poly,
            IsPolyStr = "poly"
        ;
            IsPoly = table_is_mono,
            IsPolyStr = "mono"
        ),
        (
            IsAddr = table_value,
            IsAddrStr = "value"
        ;
            IsAddr = table_addr,
            IsAddrStr = "addr"
        ),
        Str = "general(" ++ mercury_type_to_string(TVarSet, yes, Type) ++
            ", " ++ IsPolyStr ++ ", " ++ IsAddrStr ++ ")"
    ;
        Step = table_trie_step_typeinfo,
        Str = "typeinfo"
    ;
        Step = table_trie_step_typeclassinfo,
        Str = "typeclassinfo"
    ;
        Step = table_trie_step_promise_implied,
        Str = "promise_implied"
    ).

%-----------------------------------------------------------------------------%
%
% Write out constraint maps.
%

:- pred write_constraint_map(int::in, tvarset::in,
    constraint_map::in, bool::in, io::di, io::uo) is det.

write_constraint_map(Indent, VarSet, ConstraintMap, AppendVarNums, !IO) :-
    write_indent(Indent, !IO),
    io.write_string("% Constraint Map:\n", !IO),
    map.foldl(write_constraint_map_entry(Indent, VarSet, AppendVarNums),
        ConstraintMap, !IO).

:- pred write_constraint_map_entry(int::in, tvarset::in, bool::in,
    constraint_id::in, prog_constraint::in, io::di, io::uo) is det.

write_constraint_map_entry(Indent, VarSet, AppendVarNums, ConstraintId,
        ProgConstraint, !IO) :-
    write_indent(Indent, !IO),
    io.write_string("% ", !IO),
    write_constraint_id(ConstraintId, !IO),
    io.write_string(": ", !IO),
    mercury_output_constraint(VarSet, AppendVarNums, ProgConstraint, !IO),
    io.nl(!IO).

:- pred write_constraint_id(constraint_id::in, io::di, io::uo) is det.

write_constraint_id(ConstraintId, !IO) :-
    ConstraintId = constraint_id(ConstraintType, GoalId, N),
    (
        ConstraintType = assumed,
        io.write_string("(E, ", !IO)
    ;
        ConstraintType = unproven,
        io.write_string("(A, ", !IO)
    ),
    GoalId = goal_id(GoalIdNum),
    io.write_int(GoalIdNum, !IO),
    io.write_string(", ", !IO),
    io.write_int(N, !IO),
    io.write_char(')', !IO).

%-----------------------------------------------------------------------------%
%
% Write out predicate markers.
%

:- pred write_marker_list(list(marker)::in, io::di, io::uo) is det.

write_marker_list(Markers, !IO) :-
    io.write_list(Markers, ", ", write_marker, !IO).

write_marker(Marker, !IO) :-
    marker_name(Marker, Name),
    io.write_string(Name, !IO).

marker_name(marker_stub, "stub").
marker_name(marker_builtin_stub, "builtin_stub").
marker_name(marker_infer_type, "infer_type").
marker_name(marker_infer_modes, "infer_modes").
marker_name(marker_user_marked_inline, "inline").
marker_name(marker_user_marked_no_inline, "no_inline").
marker_name(marker_heuristic_inline, "heuristic_inline").
marker_name(marker_obsolete, "obsolete").
marker_name(marker_no_detism_warning, "no_determinism_warning").
marker_name(marker_class_method, "class_method").
marker_name(marker_class_instance_method, "class_instance_method").
marker_name(marker_named_class_instance_method, "named_class_instance_method").
marker_name(marker_is_impure, "impure").
marker_name(marker_is_semipure, "semipure").
marker_name(marker_promised_pure, "promise_pure").
marker_name(marker_promised_semipure, "promise_semipure").
marker_name(marker_promised_equivalent_clauses, "promise_equivalent_clauses").
marker_name(marker_terminates, "terminates").
marker_name(marker_check_termination, "check_termination").
marker_name(marker_does_not_terminate, "does_not_terminate").
marker_name(marker_calls_are_fully_qualified, "calls_are_fully_qualified").
marker_name(marker_mode_check_clauses, "mode_check_clauses").
marker_name(marker_mutable_access_pred, "mutable_access_pred").
marker_name(marker_has_require_scope, "has_require_scope").
marker_name(marker_has_format_call, "has_format_call").

%-----------------------------------------------------------------------------%
:- end_module hlds.hlds_out.hlds_out_pred.
%-----------------------------------------------------------------------------%
