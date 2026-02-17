%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% Copyright (C) 2023-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Add implementation pragmas to the HLDS.
%
%---------------------------------------------------------------------------%

:- module hlds.make_hlds.add_pragma_impl.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.make_hlds.make_hlds_types.
:- import_module hlds.make_hlds.qual_info.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_item.

:- import_module io.
:- import_module list.

%---------------------%

:- pred add_impl_pragmas(io.text_output_stream::in,
    ims_list(item_impl_pragma_info)::in,
    ims_cord(impl_pragma_tabled_info)::in,
        ims_cord(impl_pragma_tabled_info)::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_impl_pragmas_tabled(io.text_output_stream::in,
    ims_list(impl_pragma_tabled_info)::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_pragma_foreign_proc_export(impl_pragma_fproc_export_info::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------%

:- pred add_impl_markers(ims_list(item_impl_marker_info)::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_code_util.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_markers.
:- import_module hlds.hlds_pred.
:- import_module hlds.make_hlds.add_foreign_proc.
:- import_module hlds.make_hlds.add_pragma_tabling.
:- import_module hlds.make_hlds.add_pragma_util.
:- import_module hlds.make_hlds.make_hlds_warn.
:- import_module hlds.make_hlds_error.
:- import_module hlds.pred_table.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module ll_backend.
:- import_module ll_backend.fact_table_gen.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.parse_tree_out_pred_decl.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_util.

:- import_module bool.
:- import_module cord.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module varset.

%---------------------------------------------------------------------------%
%
% Adding impl pragmas to the HLDS.
%

add_impl_pragmas(_, [], !PragmaTabledListCord, !ModuleInfo, !Specs).
add_impl_pragmas(ProgressStream, [ImsList | ImsLists],
        !PragmaTabledListCord, !ModuleInfo, !Specs) :-
    ImsList = ims_sub_list(ItemMercuryStatus, Items),
    list.foldl3(add_impl_pragma(ProgressStream, ItemMercuryStatus), Items,
        cord.init, PragmaTabledCord, !ModuleInfo, !Specs),
    PragmaTabledList = cord.list(PragmaTabledCord),
    (
        PragmaTabledList = []
    ;
        PragmaTabledList = [_ | _],
        SubList = ims_sub_list(ItemMercuryStatus, PragmaTabledList),
        cord.snoc(SubList, !PragmaTabledListCord)
    ),
    add_impl_pragmas(ProgressStream, ImsLists,
        !PragmaTabledListCord, !ModuleInfo, !Specs).

add_impl_pragmas_tabled(_, [], !ModuleInfo, !QualInfo, !Specs).
add_impl_pragmas_tabled(ProgressStream, [ImsList | ImsLists],
        !ModuleInfo, !QualInfo, !Specs) :-
    ImsList = ims_sub_list(ItemMercuryStatus, Items),
    list.foldl3(
        add_impl_pragma_tabled(ProgressStream, ItemMercuryStatus), Items,
        !ModuleInfo, !QualInfo, !Specs),
    add_impl_pragmas_tabled(ProgressStream, ImsLists,
        !ModuleInfo, !QualInfo, !Specs).

%---------------------%

:- pred add_impl_pragma(io.text_output_stream::in, item_mercury_status::in,
    item_impl_pragma_info::in,
    cord(impl_pragma_tabled_info)::in, cord(impl_pragma_tabled_info)::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_impl_pragma(ProgressStream, ItemMercuryStatus, Pragma, !PragmaTabledCord,
        !ModuleInfo, !Specs) :-
    (
        Pragma = impl_pragma_foreign_decl(FDInfo),
        % XXX STATUS Check ItemMercuryStatus
        FDInfo = impl_pragma_foreign_decl_info(Lang, IsLocal, CHeader,
            Context, _),
        ForeignDeclCode = foreign_decl_code(Lang, IsLocal, CHeader, Context),
        module_add_foreign_decl_code_user(ForeignDeclCode, !ModuleInfo)
    ;
        Pragma = impl_pragma_foreign_code(FCInfo),
        % XXX STATUS Check ItemMercuryStatus
        FCInfo = impl_pragma_foreign_code_info(Lang, BodyCode, Context, _),
        warn_suspicious_foreign_code(Lang, BodyCode, Context, !Specs),
        ForeignBodyCode = foreign_body_code(Lang, BodyCode, Context),
        module_add_foreign_body_code(ForeignBodyCode, !ModuleInfo)
    ;
        Pragma = impl_pragma_fproc_export(FEInfo),
        add_pragma_foreign_proc_export(FEInfo, !ModuleInfo, !Specs)
    ;
        Pragma = impl_pragma_external_proc(ExternalInfo),
        add_pragma_external_proc(ExternalInfo, !ModuleInfo, !Specs)
    ;
        Pragma = impl_pragma_fact_table(FTInfo),
        item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
        add_pragma_fact_table(ProgressStream, ItemMercuryStatus, PredStatus,
            FTInfo, !ModuleInfo, !Specs)
    ;
        Pragma = impl_pragma_tabled(TabledInfo),
        cord.snoc(TabledInfo, !PragmaTabledCord)
    ;
        Pragma = impl_pragma_req_tail_rec(TailrecWarningPragma),
        add_pragma_require_tail_rec(TailrecWarningPragma,
            !ModuleInfo, !Specs)
    ;
        Pragma = impl_pragma_req_feature_set(RFSInfo),
        RFSInfo = impl_pragma_req_feature_set_info(FeatureSet, Context, _),
        check_required_feature_set(!.ModuleInfo, FeatureSet,
            ItemMercuryStatus, Context, !Specs)
    ).

%---------------------%

add_pragma_foreign_proc_export(FPEInfo, !ModuleInfo, !Specs) :-
    FPEInfo = impl_pragma_fproc_export_info(Origin, Lang,
        PredNameModesPF, ExportedName, VarSet, Context, _),
    PredNameModesPF = proc_pf_name_modes(PredOrFunc, SymName, ArgModes),
    PredFormArity = arg_list_arity(ArgModes),
    user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
    look_up_pragma_pf_sym_arity(!.ModuleInfo, may_be_partially_qualified,
        lfh_user_error, Context, "foreign_export",
        PredOrFunc, SymName, UserArity, MaybePredId),
    (
        MaybePredId = ok1(PredId),
        module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
        % predicate_table_get_preds(PredTable, Preds),
        % map.lookup(Preds, PredId, PredInfo),
        pred_info_get_proc_table(PredInfo, Procs),
        map.to_assoc_list(Procs, ExistingProcs),
        ( if
            get_procedure_matching_declmodes_with_renaming(!.ModuleInfo,
                ExistingProcs, ArgModes, ProcId)
        then
            map.lookup(Procs, ProcId, ProcInfo0),
            proc_info_get_declared_determinism(ProcInfo0, MaybeDetism),
            % We cannot catch those multi or nondet procedures that don't have
            % a determinism declaration until after determinism analysis.
            ( if
                MaybeDetism = yes(Detism),
                ( Detism = detism_non
                ; Detism = detism_multi
                )
            then
                varset.coerce(VarSet, InstVarSet),
                ModeSubDeclStr = mercury_mode_subdecl_to_string(output_debug,
                    PredOrFunc, InstVarSet, SymName, ArgModes, MaybeDetism),
                Pieces = [words("Error:")] ++
                    color_as_subject([pragma_decl("foreign_export"),
                        words("declarations")]) ++
                    [words("are")] ++
                    color_as_incorrect([words("not allowed")]) ++
                    [words("for procedures that can succeed"),
                    words("more than once, such as")] ++
                    color_as_subject([words_quote(ModeSubDeclStr),
                        suffix(".")])  ++
                    [nl],
                Spec = spec($pred, severity_error, phase_pt2h,
                    Context, Pieces),
                !:Specs = [Spec | !.Specs]
            else
                % Only add the foreign export if the specified language matches
                % one of the foreign languages available for this backend.
                module_info_get_globals(!.ModuleInfo, Globals),
                globals.get_backend_foreign_languages(Globals, ForeignLangs),
                ( if list.member(Lang, ForeignLangs) then
                    module_info_get_pragma_exported_procs(!.ModuleInfo,
                        PragmaExportedProcs0),
                    NewExportedProc = pragma_exported_proc(Lang,
                        PredId, ProcId, ExportedName, Context),
                    PragmaExportedProcs =
                        cord.snoc(PragmaExportedProcs0, NewExportedProc),
                    module_info_set_pragma_exported_procs(PragmaExportedProcs,
                        !ModuleInfo)
                else
                    true
                ),
                % Record that there was a foreign_export pragma for
                % this procedure, regardless of the specified language.
                % We do this so that dead procedure elimination does not
                % generate incorrect warnings about dead procedures
                % (e.g. those that are foreign_exported to languages other than
                % those languages that are supported by the current backend.)
                proc_info_set_has_any_foreign_exports(has_foreign_exports,
                    ProcInfo0, ProcInfo),
                module_info_set_pred_proc_info(PredId, ProcId,
                    PredInfo, ProcInfo, !ModuleInfo)
            )
        else
            (
                Origin = item_origin_user,
                DescPieces =
                    [pragma_decl("foreign_export"), words("declaration")],
                report_undeclared_mode_error(!.ModuleInfo, PredId, PredInfo,
                    VarSet, ArgModes, DescPieces, Context, !Specs)
            ;
                Origin = item_origin_compiler(_CompilerAttrs)
                % We do not warn about errors in export pragmas created by
                % the compiler as part of a source-to-source transformation.
                % XXX Why not?
            )
        )
    ;
        MaybePredId = error1(Specs),
        (
            Origin = item_origin_user,
            !:Specs = Specs ++ !.Specs
        ;
            Origin = item_origin_compiler(_CompilerAttrs)
            % We do not warn about errors in export pragmas created by
            % the compiler as part of a source-to-source transformation.
            % XXX Why not?
        )
    ).

%---------------------%

:- pred add_pragma_external_proc(impl_pragma_external_proc_info::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_external_proc(ExternalInfo, !ModuleInfo, !Specs) :-
    % XXX STATUS Check ItemMercuryStatus
    ExternalInfo = impl_pragma_external_proc_info(PFNameArity, MaybeBackend,
        Context, _),
    module_info_get_globals(!.ModuleInfo, Globals),
    CurrentBackend = lookup_current_backend(Globals),
    ( if
        (
            MaybeBackend = no
        ;
            MaybeBackend = yes(Backend),
            Backend = CurrentBackend
        )
    then
        % `external' declarations can only apply to things defined
        % in this module, since everything else is already external.
        module_info_get_predicate_table(!.ModuleInfo, PredicateTable0),
        PFNameArity = pred_pf_name_arity(PredOrFunc, SymName, UserArity),
        (
            PredOrFunc = pf_predicate,
            predicate_table_lookup_pred_sym_arity(PredicateTable0,
                is_fully_qualified, SymName, UserArity, PredIds),
            predicate_table_lookup_pred_sym(PredicateTable0,
                is_fully_qualified, SymName, AllArityPredIds),
            DeclPieces = [decl("external_pred"), words("pragma")]
        ;
            PredOrFunc = pf_function,
            predicate_table_lookup_func_sym_arity(PredicateTable0,
                is_fully_qualified, SymName, UserArity, PredIds),
            predicate_table_lookup_func_sym(PredicateTable0,
                is_fully_qualified, SymName, AllArityPredIds),
            DeclPieces = [decl("external_func"), words("pragma")]
        ),
        (
            PredIds = [_ | _],
            list.foldl2(mark_pred_as_external(Context), PredIds,
                !ModuleInfo, !Specs)
        ;
            PredIds = [],
            module_info_get_pred_id_table(!.ModuleInfo, PredIdTable0),
            find_user_arities_other_than(PredIdTable0, AllArityPredIds,
                UserArity, OtherUserArities),
            report_undefined_pred_or_func_error(yes(PredOrFunc),
                SymName, UserArity, OtherUserArities, Context,
                DeclPieces, !Specs)
        )
    else
        true
    ).

:- pred mark_pred_as_external(prog_context::in, pred_id::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

mark_pred_as_external(Context, PredId, !ModuleInfo, !Specs) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    pred_info_get_clauses_info(PredInfo0, ClausesInfo0),
    clauses_info_get_clauses_rep(ClausesInfo0, ClausesRep0, _ItemNumbers),
    IsEmpty = clause_list_is_empty(ClausesRep0),
    (
        IsEmpty = yes,
        pred_info_mark_as_external(PredInfo0, PredInfo),
        module_info_set_pred_info(PredId, PredInfo, !ModuleInfo)
    ;
        IsEmpty = no,
        PredOrFunc = pred_info_is_pred_or_func(PredInfo0),
        pred_info_get_name(PredInfo0, PredName),
        user_arity(UserArityInt) = pred_info_user_arity(PredInfo0),
        NameArity = name_arity(PredName, UserArityInt),
        Pieces = [words("The"), p_or_f(PredOrFunc)] ++
            color_as_subject([name_arity(NameArity)]) ++
            [words("has clauses, so")] ++
            color_as_incorrect([words("it cannot be marked as external.")]) ++
            [nl],
        Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
        !:Specs = [Spec | !.Specs]
    ).

%---------------------%

    % add_pragma_fact_table(ProgressStream, IMS, Status, FTInfo, Context,
    %   !ModuleInfo, !Info):
    %
    % Add a `pragma fact_table' declaration to the HLDS. This predicate calls
    % the fact table compiler (fact_table_compile_facts) to create a separate
    % `.o' file for the fact_table and then creates separate pieces of
    % `pragma c_code' to access the table in each mode of the fact table
    % predicate.
    %
:- pred add_pragma_fact_table(io.text_output_stream::in,
    item_mercury_status::in, pred_status::in, impl_pragma_fact_table_info::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_fact_table(ProgressStream, ItemMercuryStatus, PredStatus, FTInfo,
        !ModuleInfo, !Specs) :-
    FTInfo = impl_pragma_fact_table_info(PredSpec, FileName, Context, _),
    PredSpec = pred_pfu_name_arity(PFU, PredSymName, UserArity),
    get_matching_pred_ids(!.ModuleInfo, "fact_table", require_one_match,
        pragma_does_not_allow_modes, Context, PFU, PredSymName, UserArity,
        MatchingPredIdResult),
    (
        MatchingPredIdResult = mpids_ok(PredId, _TailPredIds, _WarnSpecs),
        module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
        fact_table_check_args(!.ModuleInfo, Context, PredId, PredInfo0,
            CheckResult),
        (
            CheckResult = fact_table_args_not_ok(CheckSpecs),
            !:Specs = CheckSpecs ++ !.Specs,
            pred_info_get_markers(PredInfo0, PredMarkers0),
            add_marker(marker_fact_table_semantic_errors,
                PredMarkers0, PredMarkers),
            pred_info_set_markers(PredMarkers, PredInfo0, PredInfo),
            module_info_set_pred_info(PredId, PredInfo, !ModuleInfo)
        ;
            CheckResult = fact_table_args_ok(GenInfo),

            % Compile the fact table into a separate .o file.
            % We should be able to dispense with the impure shenanigans
            % when we replace fact tables with fast code for large
            % disjunctions.
            some [!IO] (
                promise_pure (
                    semipure io.unsafe_get_io_state(!:IO),
                    fact_table_compile_facts(ProgressStream, !.ModuleInfo,
                        FileName, Context, GenInfo, C_HeaderCode,
                        PrimaryProcId, PredInfo0, PredInfo1, !Specs, !IO),
                    impure io.unsafe_set_io_state(!.IO)
                )
            ),

            % The C code for fact tables includes C labels. We cannot inline
            % this code, because if we did, the result would be duplicate
            % labels in the generated code. So we must disable inlining
            % for fact_table procedures.
            add_marker_pred_info(marker_user_marked_no_inline,
                PredInfo1, PredInfo),
            module_info_set_pred_info(PredId, PredInfo, !ModuleInfo),
            pred_info_get_proc_table(PredInfo, ProcTable),
            ProcIds = pred_info_all_procids(PredInfo),
            PredOrFunc = pred_info_is_pred_or_func(PredInfo),

            % Create foreign_decls to declare extern variables.
            ForeignDeclCode = foreign_decl_code(lang_c,
                foreign_decl_is_local, floi_literal(C_HeaderCode),
                Context),
            module_add_foreign_decl_code_aux(ForeignDeclCode, !ModuleInfo),

            module_add_fact_table_file(FileName, !ModuleInfo),

            % Create foreign_procs to access the table in each mode.
            add_fact_table_procs(ProgressStream, PredOrFunc, PredSymName,
                ItemMercuryStatus, PredStatus, ProcTable,  PrimaryProcId,
                Context, GenInfo, ProcIds, !ModuleInfo, !Specs)
        )
    ;
        MatchingPredIdResult = mpids_error(ErrorSpecs),
        !:Specs = ErrorSpecs ++ !.Specs
    ).

    % Add a `pragma foreign_proc' for each mode of the fact table lookup
    % to the HLDS.
    %
    % `pragma fact_table's are represented in the HLDS by a
    % `pragma foreign_proc' for each mode of the predicate.
    %
:- pred add_fact_table_procs(io.text_output_stream::in, pred_or_func::in,
    sym_name::in, item_mercury_status::in, pred_status::in,
    proc_table::in, proc_id::in, prog_context::in, fact_table_gen_info::in,
    list(proc_id)::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_fact_table_procs(_, _, _, _, _, _, _, _, _, [], !ModuleInfo, !Specs).
add_fact_table_procs(ProgressStream, PredOrFunc, SymName,
        ItemMercuryStatus, PredStatus, ProcTable, PrimaryProcId, Context,
        GenInfo, [ProcId | ProcIds], !ModuleInfo, !Specs) :-
    add_fact_table_proc(ProgressStream, PredOrFunc, SymName,
        ItemMercuryStatus, PredStatus, ProcTable, PrimaryProcId, Context,
        GenInfo, ProcId, !ModuleInfo, !Specs),
    add_fact_table_procs(ProgressStream, PredOrFunc, SymName,
        ItemMercuryStatus, PredStatus, ProcTable, PrimaryProcId, Context,
        GenInfo, ProcIds, !ModuleInfo, !Specs).

:- pred add_fact_table_proc(io.text_output_stream::in,
    pred_or_func::in, sym_name::in,
    item_mercury_status::in, pred_status::in, proc_table::in, proc_id::in,
    prog_context::in, fact_table_gen_info::in, proc_id::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_fact_table_proc(ProgressStream, PredOrFunc, SymName,
        ItemMercuryStatus, PredStatus, ProcTable, PrimaryProcId, Context,
        GenInfo, ProcId, !ModuleInfo, !Specs) :-
    map.lookup(ProcTable, ProcId, ProcInfo),
    proc_info_get_inst_varset(ProcInfo, InstVarSet),

    fact_table_generate_c_code_for_proc(!.ModuleInfo, SymName,
        ProcId, PrimaryProcId, ProcInfo, GenInfo,
        ProgVarSet, PragmaVars, C_ProcCode, C_ExtraCode),

    Attrs0 = default_attributes(lang_c),
    set_may_call_mercury(proc_will_not_call_mercury, Attrs0, Attrs1),
    set_thread_safe(proc_thread_safe, Attrs1, Attrs2),
    % Fact tables procedures should be considered pure.
    set_purity(purity_pure, Attrs2, Attrs3),
    set_refers_to_llds_stack(refers_to_llds_stack, Attrs3, Attrs),
    FCInfo = item_foreign_proc_info(Attrs, SymName, PredOrFunc, PragmaVars,
        ProgVarSet, InstVarSet, fp_impl_ordinary(C_ProcCode, no),
        Context, item_no_seq_num),
    % XXX Should return this instead.
    add_foreign_proc(ProgressStream, ItemMercuryStatus, PredStatus, FCInfo,
        !ModuleInfo, !Specs),
    ( if C_ExtraCode = "" then
        true
    else
        ForeignBodyCode = foreign_body_code(lang_c, floi_literal(C_ExtraCode),
            Context),
        % XXX Should return this instead.
        module_add_foreign_body_code(ForeignBodyCode, !ModuleInfo)
    ).

%---------------------%

:- pred add_pragma_require_tail_rec(impl_pragma_req_tail_rec_info::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_require_tail_rec(Pragma, !ModuleInfo, !Specs) :-
    Pragma = impl_pragma_req_tail_rec_info(PredSpec, RequireTailrec,
        Context, _),
    PredSpec = pred_or_proc_pfumm_name(PFUMM, PredSymName),
    pfumm_to_maybe_pf_arity_maybe_modes(PFUMM, MaybePredOrFunc, UserArity,
        MaybeModes),
    UserArity = user_arity(UserArityInt),
    PFU = maybe_pred_or_func_to_pfu(MaybePredOrFunc),
    get_matching_pred_ids(!.ModuleInfo, "require_tail_recursion",
        require_one_match, pragma_allows_modes, Context,
        PFU, PredSymName, UserArity, MatchingPredIdResult),
    (
        MatchingPredIdResult = mpids_ok(PredId, _TailPredIds, _WarnSpecs),
        SNA = sym_name_arity(PredSymName, UserArityInt),
        module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
        pred_info_get_proc_table(PredInfo0, Procs0),
        map.to_assoc_list(Procs0, Procs),
        (
            MaybeModes = yes(Modes),
            % Choose the matching proc.
            ( if
                % We have to take inst variables into account (two free
                % variables need to be unified, not just compared) when
                % searching for the matching procedure.
                %
                % pbone: I looked up how to do this and found an example in
                % add_pragma_foreign_proc/8 in add_foreign_proc.m.
                % It also contained this comment which may be relevant:
                %
                %   XXX We should probably also check that each pair in the
                %   renaming has the same name. See the comment in
                %   add_foreign_proc.
                %
                get_procedure_matching_declmodes_with_renaming(!.ModuleInfo,
                    Procs, Modes, ProcId)
            then
                map.lookup(Procs0, ProcId, Proc),
                add_pragma_require_tail_rec_proc(RequireTailrec, Context,
                    MaybePredOrFunc, MaybeModes, SNA, ProcId - Proc,
                    PredInfo0, PredInfo, !Specs)
            else
                PredInfo = PredInfo0,
                PredOrFunc = pred_info_is_pred_or_func(PredInfo),
                PFNameArity =
                    pred_pf_name_arity(PredOrFunc, PredSymName, UserArity),
                Pieces = [words("Error:")] ++
                    color_as_subject([pragma_decl("require_tail_recursion"),
                        words("declaration")]) ++
                    [words("for")] ++
                    color_as_incorrect([words("undeclared mode of"),
                        unqual_pf_sym_name_user_arity(PFNameArity),
                        suffix(".")]) ++
                    [nl],
                Spec = spec($pred, severity_error, phase_pt2h,
                    Context, Pieces),
                !:Specs = [Spec | !.Specs]
            )
        ;
            MaybeModes = no,
            list.foldl2(
                add_pragma_require_tail_rec_proc(RequireTailrec, Context,
                    MaybePredOrFunc, MaybeModes, SNA),
                Procs, PredInfo0, PredInfo, !Specs)
        ),
        module_info_set_pred_info(PredId, PredInfo, !ModuleInfo)
    ;
        MatchingPredIdResult = mpids_error(ErrorSpecs),
        !:Specs = ErrorSpecs ++ !.Specs
    ).

:- pred add_pragma_require_tail_rec_proc(require_tail_recursion::in,
    prog_context::in, maybe(pred_or_func)::in, maybe(list(mer_mode))::in,
    sym_name_arity::in, pair(proc_id, proc_info)::in,
    pred_info::in, pred_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_require_tail_rec_proc(RequireTailrec, Context, MaybePredOrFunc,
        MaybeModes, SNA, ProcId - ProcInfo0, !PredInfo, !Specs) :-
    proc_info_get_maybe_require_tailrec_info(ProcInfo0,
        MaybeRequireTailrecOrig),
    (
        MaybeRequireTailrecOrig = yes(RequireTailrecOrig),
        (
            MaybePredOrFunc = no,
            PorFPieces = []
        ;
            MaybePredOrFunc = yes(PredOrFunc),
            PorFPieces = [p_or_f(PredOrFunc)]
        ),
        (
            MaybeModes = no,
            OneModeOfPieces = []
        ;
            MaybeModes = yes(_),
            OneModeOfPieces = [words("one of mode of")]
        ),
        MainPieces = [words("Error:")] ++
            color_as_incorrect([words("conflicting"),
                pragma_decl("require_tail_recursion"), words("pragmas")]) ++
            [words("for")] ++ OneModeOfPieces ++ PorFPieces ++
            color_as_subject([qual_sym_name_arity(SNA), suffix(".")]) ++
            [nl],
        OrigPieces = [words("The earlier pragma is here."), nl],
        ( RequireTailrecOrig = suppress_tailrec_warnings(ContextOrig)
        ; RequireTailrecOrig = enable_tailrec_warnings(_, _, ContextOrig)
        ),
        Spec = error_spec($pred, severity_error, phase_pt2h,
            [msg(Context, MainPieces),
            msg(ContextOrig, OrigPieces)]),
        !:Specs = [Spec | !.Specs]
    ;
        MaybeRequireTailrecOrig = no,
        proc_info_set_require_tailrec_info(RequireTailrec,
            ProcInfo0, ProcInfo),
        pred_info_set_proc_info(ProcId, ProcInfo, !PredInfo)
    ).

%---------------------%

:- pred check_required_feature_set(module_info::in, set(required_feature)::in,
    item_mercury_status::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_required_feature_set(ModuleInfo, FeatureSet, ItemMercuryStatus, Context,
        !Specs) :-
    (
        ItemMercuryStatus = item_defined_in_other_module(_),
        % `require_feature_set' pragmas are not included in interface files
        % (including private interfaces) and so this case should not occur.
        unexpected($pred, "imported require_feature_set pragma")
    ;
        ItemMercuryStatus = item_defined_in_this_module(_),
        module_info_get_globals(ModuleInfo, Globals),
        set.fold(check_required_feature(Globals, Context), FeatureSet, !Specs)
    ).

:- pred check_required_feature(globals::in,
    prog_context::in, required_feature::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_required_feature(Globals, Context, Feature, !Specs) :-
    (
        Feature = reqf_concurrency,
        current_grade_supports_concurrency(Globals, IsConcurrencySupported),
        (
            IsConcurrencySupported = no,
            Pieces = [words("Error: this module must be compiled")] ++
                color_as_incorrect([words("in a grade that"),
                    words("supports concurrent execution.")]) ++
                [nl],
            Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
            !:Specs = [Spec | !.Specs]
        ;
            IsConcurrencySupported = yes
        )
    ;
        Feature = reqf_single_prec_float,
        globals.lookup_bool_option(Globals, single_prec_float,
            SinglePrecFloat),
        (
            SinglePrecFloat = no,
            Pieces = [words("Error: this module must be compiled")] ++
                color_as_incorrect([words("in a grade that"),
                    words("uses single precision floats.")]) ++
                [nl],
            VerbosePieces = [words("Grades that use single precision floats"),
                words("contain the grade modifier"),
                quote("spf"), suffix("."), nl],
            Msg = simple_msg(Context,
                [always(Pieces), verbose_only(verbose_once, VerbosePieces)]),
            Spec = error_spec($pred, severity_error, phase_pt2h, [Msg]),
            !:Specs = [Spec | !.Specs]
        ;
            SinglePrecFloat = yes
        )
    ;
        Feature = reqf_double_prec_float,
        globals.lookup_bool_option(Globals, single_prec_float,
            SinglePrecFloat),
        (
            SinglePrecFloat = yes,
            Pieces = [words("Error: this module must be compiled")] ++
                color_as_incorrect([words("in a grade that"),
                    words("uses double precision floats.")]) ++
                [nl],
            VerbosePieces = [words("Grades that use double precision floats"),
                words("do not contain the grade modifier"),
                quote("spf"), suffix("."), nl],
            Msg = simple_msg(Context,
                [always(Pieces), verbose_only(verbose_once, VerbosePieces)]),
            Spec = error_spec($pred, severity_error, phase_pt2h, [Msg]),
            !:Specs = [Spec | !.Specs]
        ;
            SinglePrecFloat = no
        )
    ;
        Feature = reqf_memo,
        current_grade_supports_tabling(Globals,
            tabled_memo(table_attr_ignore_with_warning), IsTablingSupported),
        (
            IsTablingSupported = no,
            Pieces = [words("Error: this module must be compiled")] ++
                color_as_incorrect([words("in a grade that"),
                    words("supports memoisation.")]) ++
                [nl],
            Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
            !:Specs = [Spec | !.Specs]
        ;
            IsTablingSupported = yes
        )
    ;
        Feature = reqf_parallel_conj,
        current_grade_supports_par_conj(Globals, IsParConjSupported),
        (
            IsParConjSupported = no,
            Pieces = [words("Error: this module must be compiled")] ++
                color_as_incorrect([words("in a grade that"),
                    words("supports executing conjuntions in parallel.")]) ++
                [nl],
            Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
            !:Specs = [Spec | !.Specs]
        ;
            IsParConjSupported = yes
        )
    ;
        Feature = reqf_trailing,
        globals.lookup_bool_option(Globals, use_trail, UseTrail),
        (
            UseTrail = no,
            Pieces = [words("Error: this module must be compiled")] ++
                color_as_incorrect([words("in a grade that"),
                    words("supports trailing.")]) ++
                [nl],
            VerbosePieces = [words("Grades that support trailing contain"),
                words("the grade modifier"), quote("tr"), suffix("."), nl],
            Msg = simple_msg(Context,
                [always(Pieces), verbose_only(verbose_once, VerbosePieces)]),
            Spec = error_spec($pred, severity_error, phase_pt2h, [Msg]),
            !:Specs = [Spec | !.Specs]
        ;
            UseTrail = yes
        )
    ;
        Feature = reqf_strict_sequential,
        globals.lookup_bool_option(Globals, reorder_conj, ReorderConj),
        globals.lookup_bool_option(Globals, reorder_disj, ReorderDisj),
        globals.lookup_bool_option(Globals, fully_strict, FullyStrict),
        ( if
            ReorderConj = no,
            ReorderDisj = no,
            FullyStrict = yes
        then
            true
        else
            Pieces = [words("Error: this module must be compiled")] ++
                color_as_incorrect([words("using the strict"),
                    words("sequential semantics.")]) ++
                [nl],
            Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
            !:Specs = [Spec | !.Specs]
        )
    ;
        Feature = reqf_conservative_gc,
        globals.get_gc_method(Globals, GC_Method),
        (
            % We consider gc_automatic to be conservative even it may not be.
            % This is okay because this feature is only of interest with the C
            % backends. We ignore it if the target language is something else.
            ( GC_Method = gc_automatic
            ; GC_Method = gc_boehm
            ; GC_Method = gc_boehm_debug
            ; GC_Method = gc_hgc
            )
        ;
            ( GC_Method = gc_accurate
            ; GC_Method = gc_none
            ),
            Pieces = [words("Error: this module must be compiled")] ++
                color_as_incorrect([words("in a grade that"),
                    words("uses conservative garbage collection.")]) ++
                [nl],
            Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
            !:Specs = [Spec | !.Specs]
        )
    ).

%---------------------%

:- pred add_impl_pragma_tabled(io.text_output_stream::in,
    item_mercury_status::in, impl_pragma_tabled_info::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_impl_pragma_tabled(ProgressStream, ItemMercuryStatus, Tabled,
        !ModuleInfo, !QualInfo, !Specs) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, type_layout, TypeLayout),
    (
        TypeLayout = yes,
        item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
        module_add_pragma_tabled(ProgressStream, Tabled,
            ItemMercuryStatus, PredStatus, !ModuleInfo, !QualInfo, !Specs)
    ;
        TypeLayout = no,
        Tabled = impl_pragma_tabled_info(TabledMethod, _, _, Context, _),
        PragmaName = tabled_eval_method_to_pragma_name(TabledMethod),
        Pieces = [words("Error:")] ++
            color_as_subject([pragma_decl(PragmaName),
                words("declaration")]) ++
            color_as_incorrect(
                [words("requires type_ctor_layout structures.")]) ++
            [words("Don't use --no-type-layout to disable them."), nl],
        Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
        !:Specs = [Spec | !.Specs]
    ).

%---------------------------------------------------------------------------%
%
% Adding impl markers to the HLDS.
%

add_impl_markers([], !ModuleInfo, !Specs).
add_impl_markers([ImsList | ImsLists], !ModuleInfo, !Specs) :-
    ImsList = ims_sub_list(ItemMercuryStatus, Items),
    list.foldl2(add_impl_marker(ItemMercuryStatus), Items,
        !ModuleInfo, !Specs),
    add_impl_markers(ImsLists, !ModuleInfo, !Specs).

%---------------------%

:- pred add_impl_marker(item_mercury_status::in, item_impl_marker_info::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_impl_marker(ItemMercuryStatus, ImplMarker, !ModuleInfo, !Specs) :-
    ImplMarker = item_impl_marker_info(MarkerKind, PFUNameArity, Context, _),
    (
        MarkerKind = ipmk_inline,
        % Note that mode_check_inline conflicts with inline because
        % it implies no_inline.
        add_pred_marker(PFUNameArity, "inline", psc_impl,
            ItemMercuryStatus, Context, marker_user_marked_inline,
            [marker_user_marked_no_inline, marker_mode_check_clauses],
            !ModuleInfo, !Specs)
    ;
        MarkerKind = ipmk_no_inline,
        add_pred_marker(PFUNameArity, "no_inline", psc_impl,
            ItemMercuryStatus, Context, marker_user_marked_no_inline,
            [marker_user_marked_inline], !ModuleInfo, !Specs)
    ;
        MarkerKind = ipmk_consider_used,
        add_pred_marker(PFUNameArity, "consider_used", psc_impl,
            ItemMercuryStatus, Context, marker_consider_used,
            [], !ModuleInfo, !Specs)
    ;
        MarkerKind = ipmk_mode_check_clauses,
        add_pred_marker(PFUNameArity, "mode_check_clauses", psc_impl,
            ItemMercuryStatus, Context, marker_mode_check_clauses,
            [], !ModuleInfo, !Specs),
        % Allowing the predicate to be inlined could lead to code generator
        % aborts. This is because the caller that inlines this predicate may
        % then push other code into the disjunction or switch's branches,
        % which would invalidate the instmap_deltas that the mode_check_clauses
        % marker prevents the recomputation of.
        add_pred_marker(PFUNameArity, "mode_check_clauses", psc_impl,
            ItemMercuryStatus, Context, marker_mmc_marked_no_inline,
            [marker_user_marked_inline], !ModuleInfo, !Specs)
    ;
        MarkerKind = ipmk_no_detism_warning,
        add_pred_marker(PFUNameArity, "no_determinism_warning", psc_impl,
            ItemMercuryStatus, Context, marker_no_detism_warning,
            [], !ModuleInfo, !Specs)
    ;
        MarkerKind = ipmk_promise_pure,
        add_pred_marker(PFUNameArity, "promise_pure", psc_impl,
            ItemMercuryStatus, Context, marker_promised_pure,
            [], !ModuleInfo, !Specs)
    ;
        MarkerKind = ipmk_promise_semipure,
        add_pred_marker(PFUNameArity, "promise_semipure", psc_impl,
            ItemMercuryStatus, Context, marker_promised_semipure,
            [], !ModuleInfo, !Specs)
    ;
        MarkerKind = ipmk_promise_eqv_clauses,
        add_pred_marker(PFUNameArity, "promise_equivalent_clauses", psc_impl,
            ItemMercuryStatus, Context, marker_promised_equivalent_clauses,
            [], !ModuleInfo, !Specs)
    ;
        MarkerKind = ipmk_req_sw_arms_type_order,
        add_pred_marker(PFUNameArity,
            "require_switch_arms_in_type_order", psc_impl,
            ItemMercuryStatus, Context, marker_req_sw_arms_type_order,
            [], !ModuleInfo, !Specs)
    ).

%---------------------------------------------------------------------------%
:- end_module hlds.make_hlds.add_pragma_impl.
%---------------------------------------------------------------------------%
