%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2012,2014 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module hlds.make_hlds.add_pragma.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.make_hlds.make_hlds_passes.
:- import_module hlds.make_hlds.qual_info.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_item.

:- import_module list.

%-----------------------------------------------------------------------------%

:- pred add_pass_2_pragma(item_pragma_info::in,
    item_status::in, item_status::out, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_pass_3_pragma(item_pragma_info::in,
    import_status::in, import_status::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.c_util.
:- import_module backend_libs.foreign.
:- import_module backend_libs.rtti.
:- import_module check_hlds.mode_util.
:- import_module hlds.code_model.
:- import_module hlds.hlds_args.
:- import_module hlds.hlds_code_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_rtti.
:- import_module hlds.make_hlds.add_pred.
:- import_module hlds.make_hlds.make_hlds_error.
:- import_module hlds.make_hlds.make_hlds_passes.
:- import_module hlds.make_hlds.make_hlds_warn.
:- import_module hlds.make_hlds.qual_info.
:- import_module hlds.make_tags.
:- import_module hlds.pred_table.
:- import_module hlds.quantification.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module ll_backend.
:- import_module ll_backend.fact_table.
:- import_module ll_backend.rtti_out.
:- import_module mdbcomp.prim_data.
:- import_module ml_backend.
:- import_module ml_backend.mlds.
:- import_module ml_backend.mlds_to_c.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.modules.
:- import_module parse_tree.prog_ctgc.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.
:- import_module recompilation.
:- import_module transform_hlds.term_constr_main.
:- import_module transform_hlds.term_constr_util.
:- import_module transform_hlds.term_util.

:- import_module assoc_list.
:- import_module bag.
:- import_module bimap.
:- import_module bool.
:- import_module int.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module multi_map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

add_pass_2_pragma(ItemPragma, !Status, !ModuleInfo, !Specs) :-
    ItemPragma = item_pragma_info(Origin, Pragma, Context, _SeqNum),
    % Check for invalid pragmas in the `interface' section.
    !.Status = item_status(ImportStatus, _),
    Allowed = pragma_allowed_in_interface(Pragma),
    (
        Allowed = no,
        (
            Origin = user,
            error_if_exported(ImportStatus, Context, "`pragma' declaration",
                !Specs)
        ;
            % We don't report this as an error as it just clutters up
            % the compiler output - the *real* error is whatever caused
            % the compiler to create this pragma.
            Origin = compiler(_)
        )
    ;
        Allowed = yes
    ),
    (
        Pragma = pragma_foreign_decl(FDInfo),
        FDInfo = pragma_info_foreign_decl(Lang, IsLocal, C_Header),
        module_add_foreign_decl(Lang, IsLocal, C_Header, Context, !ModuleInfo)
    ;
        Pragma = pragma_foreign_code(FCInfo),
        FCInfo = pragma_info_foreign_code(Lang, Body_Code),
        module_add_foreign_body_code(Lang, Body_Code, Context, !ModuleInfo)
    ;
        Pragma = pragma_foreign_import_module(FIMInfo),
        FIMInfo = pragma_info_foreign_import_module(Lang, Import),
        module_add_foreign_import_module(Lang, Import, Context, !ModuleInfo)
    ;
        Pragma = pragma_inline(PredNameArity),
        PredNameArity = pred_name_arity(Name, Arity),
        add_pred_marker("inline", Name, Arity, ImportStatus, Context,
            marker_user_marked_inline, [marker_user_marked_no_inline],
            !ModuleInfo, !Specs)
    ;
        Pragma = pragma_no_inline(PredNameArity),
        PredNameArity = pred_name_arity(Name, Arity),
        add_pred_marker("no_inline", Name, Arity, ImportStatus, Context,
            marker_user_marked_no_inline, [marker_user_marked_inline],
            !ModuleInfo, !Specs)
    ;
        % Used for inter-module unused argument elimination.
        % This can only appear in .opt files.
        Pragma = pragma_unused_args(UnusedArgsInfo),
        ( ImportStatus \= status_opt_imported ->
            Pieces = [words("Error: illegal use of pragma `unused_args'.")],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        ;
            add_pragma_unused_args(UnusedArgsInfo, Context,
                !ModuleInfo, !Specs)
        )
    ;
        Pragma = pragma_exceptions(ExceptionsInfo),
        ( ImportStatus \= status_opt_imported ->
            Pieces = [words("Error: illegal use of pragma `exceptions'.")],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        ;
            add_pragma_exceptions(ExceptionsInfo, Context, !ModuleInfo, !Specs)
        )
    ;
        Pragma = pragma_trailing_info(TrailingInfo),
        ( ImportStatus \= status_opt_imported ->
            Pieces = [words("Error: illegal use of pragma `trailing_info'.")],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        ;
            add_pragma_trailing_info(TrailingInfo, Context,
                !ModuleInfo, !Specs)
        )
    ;
        Pragma = pragma_mm_tabling_info(MMTablingInfo),
        ( ImportStatus \= status_opt_imported ->
            Pieces =
                [words("Error: illegal use of pragma `mm_tabling_info',")],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        ;
            add_pragma_mm_tabling_info(MMTablingInfo, Context,
                !ModuleInfo, !Specs)
        )
    ;
        Pragma = pragma_obsolete(PredNameArity),
        PredNameArity = pred_name_arity(Name, Arity),
        add_pred_marker("obsolete", Name, Arity, ImportStatus,
            Context, marker_obsolete, [], !ModuleInfo, !Specs)
    ;
        Pragma = pragma_no_detism_warning(PredNameArity),
        PredNameArity = pred_name_arity(Name, Arity),
        add_pred_marker("no_determinism_warning", Name, Arity, ImportStatus,
            Context, marker_no_detism_warning, [], !ModuleInfo, !Specs)
    ;
        Pragma = pragma_promise_eqv_clauses(PredNameArity),
        PredNameArity = pred_name_arity(Name, Arity),
        add_pred_marker("promise_equivalent_clauses", Name, Arity,
            ImportStatus, Context, marker_promised_equivalent_clauses, [],
            !ModuleInfo, !Specs)
    ;
        Pragma = pragma_promise_pure(PredNameArity),
        PredNameArity = pred_name_arity(Name, Arity),
        add_pred_marker("promise_pure", Name, Arity, ImportStatus,
            Context, marker_promised_pure, [], !ModuleInfo, !Specs)
    ;
        Pragma = pragma_promise_semipure(PredNameArity),
        PredNameArity = pred_name_arity(Name, Arity),
        add_pred_marker("promise_semipure", Name, Arity, ImportStatus,
            Context, marker_promised_semipure, [], !ModuleInfo, !Specs)
    ;
        Pragma = pragma_terminates(PredNameArity),
        PredNameArity = pred_name_arity(Name, Arity),
        add_pred_marker("terminates", Name, Arity, ImportStatus, Context,
            marker_terminates,
            [marker_check_termination, marker_does_not_terminate],
            !ModuleInfo, !Specs)
    ;
        Pragma = pragma_does_not_terminate(PredNameArity),
        PredNameArity = pred_name_arity(Name, Arity),
        add_pred_marker("does_not_terminate", Name, Arity, ImportStatus,
            Context, marker_does_not_terminate,
            [marker_check_termination, marker_terminates], !ModuleInfo, !Specs)
    ;
        Pragma = pragma_check_termination(PredNameArity),
        PredNameArity = pred_name_arity(Name, Arity),
        add_pred_marker("check_termination", Name, Arity, ImportStatus,
            Context, marker_check_termination,
            [marker_terminates, marker_does_not_terminate],
            !ModuleInfo, !Specs)
    ;
        Pragma = pragma_mode_check_clauses(PredNameArity),
        PredNameArity = pred_name_arity(Name, Arity),
        add_pred_marker("mode_check_clauses", Name, Arity, ImportStatus,
            Context, marker_mode_check_clauses, [], !ModuleInfo, !Specs),

        % Allowing the predicate to be inlined could lead to code generator
        % aborts. This is because the caller that inlines this predicate may
        % then push other code into the disjunction or switch's branches,
        % which would invalidate the instmap_deltas that the mode_check_clauses
        % feature prevents the recomputation of.
        add_pred_marker("mode_check_clauses", Name, Arity, ImportStatus,
            Context, marker_user_marked_no_inline, [marker_user_marked_inline],
            !ModuleInfo, !Specs)
    ;
        Pragma = pragma_require_feature_set(RFSInfo),
        RFSInfo = pragma_info_require_feature_set(FeatureSet),
        check_required_feature_set(FeatureSet, ImportStatus, Context,
            !ModuleInfo, !Specs)
    ;
        % Ignore `pragma source_file' declarations in pass 2;
        % they have already been handled while reading in the items.
        Pragma = pragma_source_file(_)
    ;
        % Ignore these pragmas in pass 2; we will handle them in pass 3,
        % when we process clauses.
        ( Pragma = pragma_foreign_proc(_)
        ; Pragma = pragma_type_spec(_)
        ; Pragma = pragma_tabled(_)
        ; Pragma = pragma_fact_table(_)
        )
    ;
        % Ignore these pragmas in pass 2; we will handle them in pass 3,
        % after we have added all the types.
        ( Pragma = pragma_foreign_export_enum(_)
        ; Pragma = pragma_foreign_enum(_)
        ; Pragma = pragma_reserve_tag(_)
        ; Pragma = pragma_oisu(_)
        )
    ;
        % Ignore these pragmas in pass 2; we will handle them in pass 3,
        % after default function modes have been added.
        ( Pragma = pragma_foreign_proc_export(_)
        ; Pragma = pragma_termination_info(_)
        ; Pragma = pragma_termination2_info(_)
        )
    ;
        % Ignore these pragmas in pass 2; we will handle them in pass 3.
        % XXX Document the reason why we handle them then.
        ( Pragma = pragma_structure_sharing(_)
        ; Pragma = pragma_structure_reuse(_)
        )
    ).

%-----------------------------------------------------------------------------%

add_pass_3_pragma(ItemPragma, !Status, !ModuleInfo, !QualInfo, !Specs) :-
    ItemPragma = item_pragma_info(Origin, Pragma, Context, SeqNum),
    (
        Pragma = pragma_foreign_proc(FPInfo),
        add_pragma_foreign_proc(FPInfo, !.Status, Context, yes(SeqNum),
            !ModuleInfo, !Specs)
    ;
        Pragma = pragma_foreign_proc_export(FEInfo),
        add_pragma_foreign_proc_export(Origin, FEInfo, Context,
            !ModuleInfo, !Specs)
    ;
        Pragma = pragma_foreign_export_enum(FEEInfo),
        add_pragma_foreign_export_enum(FEEInfo, !.Status, Context,
            !ModuleInfo, !Specs)
    ;
        Pragma = pragma_foreign_enum(FEInfo),
        add_pragma_foreign_enum(FEInfo, !.Status, Context, !ModuleInfo, !Specs)
    ;
        Pragma = pragma_type_spec(TypeSpecInfo),
        add_pragma_type_spec(TypeSpecInfo, Context,
            !ModuleInfo, !QualInfo, !Specs)
    ;
        Pragma = pragma_tabled(TabledInfo),
        module_info_get_globals(!.ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, type_layout, TypeLayout),
        (
            TypeLayout = yes,
            module_add_pragma_tabled(TabledInfo, Context, !Status,
                !ModuleInfo, !QualInfo, !Specs)
        ;
            TypeLayout = no,
            TabledInfo = pragma_info_tabled(EvalMethod, _, _, _),
            Pieces = [words("Error:"),
                quote(":- pragma " ++ eval_method_to_string(EvalMethod)),
                words("declaration requires type_ctor_layout structures."),
                words("Don't use --no-type-layout to disable them."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        )
    ;
        Pragma = pragma_fact_table(FTInfo),
        add_pragma_fact_table(FTInfo, !.Status, Context, !ModuleInfo, !Specs)
    ;
        Pragma = pragma_reserve_tag(TypeCtor),
        add_pragma_reserve_tag(TypeCtor, !.Status, Context,
            !ModuleInfo, !Specs)
    ;
        Pragma = pragma_oisu(OISUInfo),
        add_pragma_oisu(OISUInfo, !.Status, Context, !ModuleInfo, !Specs)
    ;
        Pragma = pragma_termination_info(TermInfo),
        add_pragma_termination_info(TermInfo, Context, !ModuleInfo, !Specs)
    ;
        Pragma = pragma_termination2_info(Term2Info),
        add_pragma_termination2_info(Term2Info, Context, !ModuleInfo, !Specs)
    ;
        Pragma = pragma_structure_sharing(SharingInfo),
        add_pragma_structure_sharing(SharingInfo, Context, !ModuleInfo, !Specs)
    ;
        Pragma = pragma_structure_reuse(ReuseInfo),
        add_pragma_structure_reuse(ReuseInfo, Context, !ModuleInfo, !Specs)
    ;
        % Ignore these kinds of pragmas in pass 3, since they have
        % already been handled earlier, in pass 2, or even earlier.
        ( Pragma = pragma_foreign_decl(_)
        ; Pragma = pragma_foreign_code(_)
        ; Pragma = pragma_foreign_import_module(_)
        ; Pragma = pragma_inline(_)
        ; Pragma = pragma_no_inline(_)
        ; Pragma = pragma_unused_args(_)
        ; Pragma = pragma_exceptions(_)
        ; Pragma = pragma_trailing_info(_)
        ; Pragma = pragma_mm_tabling_info(_)
        ; Pragma = pragma_obsolete(_)
        ; Pragma = pragma_no_detism_warning(_)
        ; Pragma = pragma_source_file(_)
        ; Pragma = pragma_promise_eqv_clauses(_)
        ; Pragma = pragma_promise_pure(_)
        ; Pragma = pragma_promise_semipure(_)
        ; Pragma = pragma_terminates(_)
        ; Pragma = pragma_does_not_terminate(_)
        ; Pragma = pragma_check_termination(_)
        ; Pragma = pragma_mode_check_clauses(_)
        ; Pragma = pragma_require_feature_set(_)
        )
    ).

%-----------------------------------------------------------------------------%

:- pred add_pragma_foreign_proc_export(item_origin::in,
    pragma_info_foreign_proc_export::in, prog_context::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_foreign_proc_export(Origin, FPEInfo, Context, !ModuleInfo,
        !Specs) :-
    FPEInfo = pragma_info_foreign_proc_export(Lang, PrednameModesPF,
        ExportedName),
    PrednameModesPF = pred_name_modes_pf(Name, Modes, PredOrFunc),
    module_info_get_predicate_table(!.ModuleInfo, PredTable),
    list.length(Modes, Arity),
    predicate_table_lookup_pf_sym_arity(PredTable, may_be_partially_qualified,
        PredOrFunc, Name, Arity, PredIds),
    (
        PredIds = [PredId],
        add_pragma_foreign_export_2(Arity, PredTable, Origin, Lang, Name,
            PredId, Modes, ExportedName, Context, !ModuleInfo, !Specs)
    ;
        PredIds = [_, _ | _],
        StartPieces = [words("error: ambiguous"), p_or_f(PredOrFunc),
            words("name in"), quote("pragma foreign_export"),
            words("declaration."), nl,
            words("The possible matches are:"), nl_indent_delta(1)],
        PredIdPiecesList = list.map(
            describe_one_pred_name(!.ModuleInfo, should_module_qualify),
            PredIds),
        PredIdPieces = component_list_to_line_pieces(PredIdPiecesList,
            [suffix(".")]),
        MainPieces = StartPieces ++ PredIdPieces,
        VerbosePieces = [words("An explicit module qualifier"),
            words("may be necessary.")],
        Msg = simple_msg(Context,
            [always(MainPieces), verbose_only(VerbosePieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ;
        PredIds = [],
        (
            Origin = user,
            undefined_pred_or_func_error(Name, Arity, Context,
                [quote(":- pragma foreign_export"), words("declaration")],
                !Specs)
        ;
            Origin = compiler(Details),
            (
                Details = initialise_decl
            ;
                Details = mutable_decl
            ;
                Details = finalise_decl
            ;
                ( Details = solver_type
                ; Details = foreign_imports
                ; Details = pragma_memo_attribute
                ),
                unexpected($module, $pred,
                    "Bad introduced foreign_export pragma.")
            )
        )
    ).

:- pred add_pragma_foreign_export_2(arity::in, predicate_table::in,
    item_origin::in, foreign_language::in,
    sym_name::in, pred_id::in, list(mer_mode)::in, string::in,
    prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_foreign_export_2(Arity, PredTable, Origin, Lang, Name, PredId,
        Modes, ExportedName, Context, !ModuleInfo, !Specs) :-
    predicate_table_get_preds(PredTable, Preds),
    map.lookup(Preds, PredId, PredInfo),
    pred_info_get_procedures(PredInfo, Procs),
    map.to_assoc_list(Procs, ExistingProcs),
    (
        get_procedure_matching_declmodes_with_renaming(ExistingProcs,
            Modes, !.ModuleInfo, ProcId)
    ->
        map.lookup(Procs, ProcId, ProcInfo0),
        proc_info_get_declared_determinism(ProcInfo0, MaybeDetism),
        % We cannot catch those multi or nondet procedures that don't have
        % a determinism declaration until after determinism analysis.
        (
            MaybeDetism = yes(Detism),
            ( Detism = detism_non
            ; Detism = detism_multi
            )
        ->
            Pieces = [words("Error: "),
                fixed("`:- pragma foreign_export' declaration"),
                words("for a procedure that has"),
                words("a declared determinism of"),
                fixed(determinism_to_string(Detism) ++ "."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
                [Msg]),
            !:Specs = [Spec | !.Specs]
        ;
            % Only add the foreign export if the specified language matches
            % one of the foreign languages available for this backend.
            %
            module_info_get_globals(!.ModuleInfo, Globals),
            globals.get_backend_foreign_languages(Globals, ForeignLanguages),
            ( list.member(Lang, ForeignLanguages) ->
                module_info_get_pragma_exported_procs(!.ModuleInfo,
                    PragmaExportedProcs0),
                NewExportedProc = pragma_exported_proc(Lang,
                    PredId, ProcId, ExportedName, Context),
                PragmaExportedProcs =
                    [NewExportedProc | PragmaExportedProcs0],
                module_info_set_pragma_exported_procs(PragmaExportedProcs,
                    !ModuleInfo)
            ;
                true
            ),

            % Record that there was a foreign_export pragma for this procedure,
            % regardless of the specified language.  We do this so that dead
            % procedure elimination does not generate incorrect warnings about
            % dead procedures (e.g.  those that are foreign_exported to
            % languages other than those languages that are supported by the
            % current backend.)
            %
            proc_info_set_has_any_foreign_exports(has_foreign_exports,
                ProcInfo0, ProcInfo), 
            module_info_set_pred_proc_info(PredId, ProcId, PredInfo, ProcInfo,
                !ModuleInfo)
        )
    ;
        % We do not warn about errors in export pragmas created by the
        % compiler as part of a source-to-source transformation.
        (
            Origin = user,
            undefined_mode_error(Name, Arity, Context,
                [quote(":- pragma foreign_export"), words("declaration")],
                !Specs)
        ;
            Origin = compiler(Details),
            (
                Details = initialise_decl
            ;
                Details = mutable_decl
            ;
                Details = finalise_decl
            ;
                ( Details = solver_type
                ; Details = foreign_imports
                ; Details = pragma_memo_attribute
                ),
                unexpected($module, $pred,
                    "Bad introduced foreign_export pragma.")
            )
        )
    ).

%-----------------------------------------------------------------------------%

:- pred add_pragma_reserve_tag(type_ctor::in, import_status::in,
    prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_reserve_tag(TypeCtor, PragmaStatus, Context, !ModuleInfo, !Specs) :-
    TypeCtor = type_ctor(TypeName, TypeArity),
    module_info_get_type_table(!.ModuleInfo, TypeTable0),
    ( search_type_ctor_defn(TypeTable0, TypeCtor, TypeDefn0) ->
        hlds_data.get_type_defn_body(TypeDefn0, TypeBody0),
        hlds_data.get_type_defn_status(TypeDefn0, TypeStatus),
        (
            not (
                TypeStatus = PragmaStatus
            ;
                TypeStatus = status_abstract_exported,
                ( PragmaStatus = status_local
                ; PragmaStatus = status_exported_to_submodules
                )
            )
        ->
            MaybeSeverity = yes(severity_error),
            ErrorPieces = [words("error:"), quote("pragma reserve_tag"),
                words("declaration must have"),
                words("the same visibility as the type definition.")]
        ;
            (
                TypeBody0 = hlds_du_type(Body, _CtorTags0, _CheaperTagTest,
                    _DuTypeKind, MaybeUserEqComp, MaybeDirectArgCtors,
                    ReservedTag0, _ReservedAddr, IsForeign),
                (
                    ReservedTag0 = uses_reserved_tag,
                    % Make doubly sure that we don't get any spurious warnings
                    % with intermodule optimization ...
                    TypeStatus \= status_opt_imported
                ->
                    MaybeSeverity = yes(severity_warning),
                    ErrorPieces = [words("warning: multiple"),
                        quote("pragma reserved_tag"),
                        words("declarations for the same type."), nl]
                ;
                    MaybeSeverity = no,
                    ErrorPieces = []
                ),

                % We passed all the semantic checks. Mark the type as having
                % a reserved tag, and recompute the constructor tags.
                ReservedTag = uses_reserved_tag,
                module_info_get_globals(!.ModuleInfo, Globals),
                assign_constructor_tags(Body, MaybeUserEqComp, TypeCtor,
                    ReservedTag, Globals, CtorTags, ReservedAddr, DuTypeKind),
                TypeBody = hlds_du_type(Body, CtorTags, no_cheaper_tag_test,
                    DuTypeKind, MaybeUserEqComp, MaybeDirectArgCtors,
                    ReservedTag, ReservedAddr, IsForeign),
                hlds_data.set_type_defn_body(TypeBody, TypeDefn0, TypeDefn),
                replace_type_ctor_defn(TypeCtor, TypeDefn,
                    TypeTable0, TypeTable),
                module_info_set_type_table(TypeTable, !ModuleInfo)
            ;
                ( TypeBody0 = hlds_eqv_type(_)
                ; TypeBody0 = hlds_foreign_type(_)
                ; TypeBody0 = hlds_solver_type(_, _)
                ; TypeBody0 = hlds_abstract_type(_)
                ),
                MaybeSeverity = yes(severity_error),
                ErrorPieces = [words("error:"),
                    sym_name_and_arity(TypeName / TypeArity),
                    words("is not a discriminated union type."), nl]
            )
        )
    ;
        MaybeSeverity = yes(severity_error),
        ErrorPieces = [words("error: undefined type"),
            sym_name_and_arity(TypeName / TypeArity), suffix("."), nl]
    ),
    (
        ErrorPieces = []
    ;
        ErrorPieces = [_ | _],
        (
            MaybeSeverity = yes(Severity)
        ;
            MaybeSeverity = no,
            unexpected($module, $pred, "no severity")
        ),
        ContextPieces = [words("In"), quote("pragma reserve_tag"),
            words("declaration for"), sym_name_and_arity(TypeName / TypeArity),
            suffix(":"), nl],
        Msg = simple_msg(Context, [always(ContextPieces ++ ErrorPieces)]),
        Spec = error_spec(Severity, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ).

%-----------------------------------------------------------------------------%

:- pred add_pragma_foreign_export_enum(pragma_info_foreign_export_enum::in,
    import_status::in, prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_foreign_export_enum(FEEInfo, _ImportStatus, Context,
        !ModuleInfo, !Specs) :-
    FEEInfo = pragma_info_foreign_export_enum(Lang, TypeCtor,
        Attributes, Overrides),
    TypeCtor = type_ctor(TypeName, TypeArity),
    module_info_get_type_table(!.ModuleInfo, TypeTable),
    ContextPieces = [words("In"), quote("pragma foreign_export_enum"),
        words("declaration for"),
        sym_name_and_arity(TypeName / TypeArity), suffix(":"), nl],
    (
        % Emit an error message for foreign_export_enum pragmas for the
        % builtin atomic types.
        TypeArity = 0,
        ( TypeName = unqualified("character")
        ; TypeName = unqualified("float")
        ; TypeName = unqualified("int")
        ; TypeName = unqualified("string")
        )
    ->
        MaybeSeverity = yes(severity_error),
        ErrorPieces = [words("error: "),
            sym_name_and_arity(TypeName / TypeArity),
            words("is an atomic type"), suffix("."), nl]
    ;
        ( search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn) ->
            get_type_defn_body(TypeDefn, TypeBody),
            (
                ( TypeBody = hlds_eqv_type(_)
                ; TypeBody = hlds_abstract_type(_)
                ; TypeBody = hlds_solver_type(_, _)
                ; TypeBody = hlds_foreign_type(_)
                ),
                MaybeSeverity = yes(severity_error),
                ErrorPieces = [words("error: "),
                    sym_name_and_arity(TypeName / TypeArity),
                    words("is not an enumeration type"), suffix("."), nl]
            ;
                % XXX How should we handle IsForeignType here?
                TypeBody = hlds_du_type(Ctors, _TagValues, _CheaperTagTest,
                    DuTypeKind, _MaybeUserEq, _MaybeDirectArgCtors,
                    _ReservedTag, _ReservedAddr, _IsForeignType),
                (
                    ( DuTypeKind = du_type_kind_mercury_enum
                    ; DuTypeKind = du_type_kind_foreign_enum(_)
                    ; DuTypeKind = du_type_kind_direct_dummy
                    ),
                    Attributes = export_enum_attributes(MaybePrefix,
                        MakeUpperCase),
                    (
                        MaybePrefix = yes(Prefix)
                    ;
                        MaybePrefix = no,
                        Prefix = ""
                    ),
                    build_export_enum_overrides_map(TypeName, Context,
                        ContextPieces, Overrides, MaybeOverridesMap, !Specs),
                    (
                        MaybeOverridesMap = yes(OverridesMap),
                        build_export_enum_name_map(ContextPieces, Lang,
                            TypeName, TypeArity, Context, Prefix,
                            MakeUpperCase, OverridesMap, Ctors, MaybeMapping,
                            !Specs),
                        (
                            MaybeMapping = yes(Mapping),
                            ExportedEnum = exported_enum_info(Lang, Context,
                                TypeCtor, Mapping),
                            module_info_get_exported_enums(!.ModuleInfo,
                                ExportedEnums0),
                            ExportedEnums = [ExportedEnum | ExportedEnums0],
                            module_info_set_exported_enums(ExportedEnums,
                                !ModuleInfo)
                        ;
                            MaybeMapping = no
                        )
                    ;
                        MaybeOverridesMap = no
                    ),
                    ErrorPieces = [],
                    MaybeSeverity = no
                ;
                    ( DuTypeKind = du_type_kind_general
                    ; DuTypeKind = du_type_kind_notag(_, _, _)
                    ),
                    MaybeSeverity = yes(severity_error),
                    % XXX Maybe we should add a verbose error message that
                    % identifies the non-zero arity constructors.
                    ErrorPieces = [words("error: "),
                        sym_name_and_arity(TypeName / TypeArity),
                        words("is not an enumeration type."),
                        words("It has one or more non-zero arity"),
                        words("constructors."), nl]
                )
            )
        ;
            % This case corresponds to an undefined type. We do not issue
            % an error message for it here since module qualification
            % will have already done so.
            MaybeSeverity = no,
            ErrorPieces = []
        )
    ),
    (
        ErrorPieces = []
    ;
        ErrorPieces = [_ | _],
        (
            MaybeSeverity = yes(Severity)
        ;
            MaybeSeverity = no,
            unexpected($module, $pred, "no severity")
        ),
        Msg = simple_msg(Context, [always(ContextPieces ++ ErrorPieces)]),
        Spec = error_spec(Severity, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ).

:- pred build_export_enum_overrides_map(sym_name::in, prog_context::in,
    format_components::in, assoc_list(sym_name, string)::in,
    maybe(map(sym_name, string))::out,
    list(error_spec)::in, list(error_spec)::out) is det.

build_export_enum_overrides_map(TypeName, Context, ContextPieces,
        OverridesList0, MaybeOverridesMap, !Specs) :-
    ( sym_name_get_module_name(TypeName, ModuleName0) ->
        ModuleName = ModuleName0
    ;
        unexpected($module, $pred,
            "unqualified type name while building override map")
    ),
    % Strip off module qualifiers that match those of the type being exported.
    % We leave those that do not match so that they can be reported as errors
    % later.
    StripQualifiers = (func(Name0) = Name :-
        (
            Name0 = qualified(ModuleQualifier, UnqualName),
            ( ModuleQualifier = ModuleName ->
                Name = unqualified(UnqualName)
            ;
                Name = Name0
            )
        ;
            Name0 = unqualified(_),
            Name = Name0
        )
    ),
    OverridesList = assoc_list.map_keys_only(StripQualifiers,
        OverridesList0),
    ( bimap.from_assoc_list(OverridesList, OverridesMap0) ->
        OverridesMap = bimap.forward_map(OverridesMap0),
        MaybeOverridesMap = yes(OverridesMap)
    ;
        MaybeOverridesMap = no,
        % XXX we should report exactly why it is not a bijective.
        ErrorPieces = [words("error: "),
            words("the user-specified mapping between Mercury and"),
            words("foreign names does not form a bijection.")],
        Msg = simple_msg(Context, [always(ContextPieces ++ ErrorPieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ).

:- pred build_export_enum_name_map(format_components::in, foreign_language::in,
    sym_name::in, arity::in, prog_context::in, string::in,
    uppercase_export_enum::in, map(sym_name, string)::in,
    list(constructor)::in, maybe(map(sym_name, string))::out,
    list(error_spec)::in, list(error_spec)::out) is det.

build_export_enum_name_map(ContextPieces, Lang, TypeName, TypeArity, Context,
        Prefix, MakeUpperCase, Overrides0, Ctors, MaybeMapping, !Specs) :-
    (
        TypeName = qualified(TypeModuleQual, _)
    ;
        % The type name should have been module qualified by now.
        TypeName = unqualified(_),
        unexpected($module, $pred,
            "unqualified type name for foreign_export_enum")
    ),

    list.foldl3(
        add_ctor_to_name_map(Lang, Prefix, MakeUpperCase, TypeModuleQual),
        Ctors, Overrides0, Overrides, map.init, NameMap, [], BadCtors),

    % Check for any remaining user-specified renamings that didn't match
    % the constructors of the type and report and error for them.

    ( not map.is_empty(Overrides) ->
       InvalidRenamingPieces = [words("user-specified foreign names"),
            words("for constructors that do not match match"),
            words("any of the constructors of"),
            sym_name_and_arity(TypeName / TypeArity), suffix("."), nl],
        InvalidRenamings = map.keys(Overrides),
        InvalidRenamingComponents =
            list.map((func(S) = [sym_name(S)]), InvalidRenamings),
        InvalidRenamingList = component_list_to_line_pieces(
            InvalidRenamingComponents, [nl]),
        InvalidRenamingVerbosePieces = [words("The following"),
            words(choose_number(InvalidRenamings,
                "constructor does", "constructors do")),
            words("not match"), suffix(":"), nl_indent_delta(2)]
            ++ InvalidRenamingList,
        InvalidRenamingMsg = simple_msg(Context,
            [always(ContextPieces ++ InvalidRenamingPieces),
                verbose_only(InvalidRenamingVerbosePieces)]),
        InvalidRenamingSpec = error_spec(severity_error,
            phase_parse_tree_to_hlds, [InvalidRenamingMsg]),
        list.cons(InvalidRenamingSpec, !Specs),
        MaybeMapping = no
        % NOTE: in the presence of this error we do not report if
        % constructors could not be converted to names in the foreign
        % language.
    ;
        (
            BadCtors = [],
            check_name_map_for_conflicts(Context, ContextPieces, NameMap,
                MaybeMapping, !Specs)
        ;
            BadCtors = [_ | _],
            (
                Lang = lang_c,
                What = "C identifiers."
            ;
                Lang = lang_java,
                What = "Java identifiers."
            ;
                ( Lang = lang_csharp
                ; Lang = lang_il
                ; Lang = lang_erlang
                ),
                sorry($module, $pred,
                    "foreign_export_enum pragma for unsupported language")
            ),
            BadCtorsErrorPieces = [
                words("error: not all the constructors of the type"),
                sym_name_and_arity(TypeName / TypeArity),
                words("can be converted into valid " ++ What)
            ],
            list.sort(BadCtors, SortedBadCtors),
            BadCtorComponents = list.map((func(S) = [sym_name(S)]),
                SortedBadCtors),
            BadCtorsList = component_list_to_line_pieces(
                BadCtorComponents, [nl]),
            BadCtorsVerboseErrorPieces = [words("The following"),
                words(choose_number(BadCtors, "constructor", "constructors")),
                words("cannot be converted:"), nl_indent_delta(2)]
                ++ BadCtorsList,
            BadCtorsMsg = simple_msg(Context,
                [always(ContextPieces ++ BadCtorsErrorPieces),
                    verbose_only(BadCtorsVerboseErrorPieces)]),
            BadCtorsSpec = error_spec(severity_error,
                phase_parse_tree_to_hlds, [BadCtorsMsg]),
            list.cons(BadCtorsSpec, !Specs),
            MaybeMapping = no
        )
    ).

    % Check that the mapping from foreign names to Mercury names is not
    % one-to-many.
    %
:- pred check_name_map_for_conflicts(prog_context::in, format_components::in,
    map(sym_name, string)::in, maybe(map(sym_name, string))::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_name_map_for_conflicts(Context, ContextPieces, NameMap,
        MaybeNameMap, !Specs) :-
    NamesAndForeignNames = map.to_assoc_list(NameMap),
    ( bimap.from_assoc_list(NamesAndForeignNames, _) ->
        MaybeNameMap = yes(NameMap)
    ;
        MaybeNameMap = no,
        % XXX we should report exactly why it is not bijective.
        ErrorPieces = [words("error:"),
            words("the mapping between Mercury and foreign names"),
            words("does not form a bijection."), nl],
        Msg = simple_msg(Context, [always(ContextPieces ++ ErrorPieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ).

    % add_ctor_to_name_map(ForeignLanguage, Overrides, Prefix, Ctor, !Map,
    %   !BadCtors):
    %
:- pred add_ctor_to_name_map(foreign_language::in,
    string::in, uppercase_export_enum::in, sym_name::in, constructor::in,
    map(sym_name, string)::in, map(sym_name, string)::out,
    map(sym_name, string)::in, map(sym_name, string)::out,
    list(sym_name)::in, list(sym_name)::out) is det.

add_ctor_to_name_map(Lang, Prefix, MakeUpperCase, _TypeModQual, Ctor,
        !Overrides, !NameMap, !BadCtors) :-
    CtorSymName = Ctor ^ cons_name,
    (
        % All of the constructor sym_names should be module qualified by now.
        % We unqualify them before inserting them into the mapping since
        % the code in export.m expects that to be done.

        CtorSymName = qualified(_, _),
        UnqualCtorName = unqualify_name(CtorSymName),
        UnqualSymName = unqualified(UnqualCtorName)
    ;
        CtorSymName = unqualified(_),
        unexpected($module, $pred, "unqualified constructor name")
    ),

    % If the user specified a name for this constructor then use that.
    ( map.remove(UnqualSymName, UserForeignName, !Overrides) ->
        ForeignNameTail = UserForeignName
    ;
        % Otherwise try to derive a name automatically from the
        % constructor name.
        (
            MakeUpperCase = uppercase_export_enum,
            ForeignNameTail = string.to_upper(UnqualCtorName)
        ;
            MakeUpperCase = do_not_uppercase_export_enum,
            ForeignNameTail = UnqualCtorName
        )
    ),
    ForeignName = Prefix ++ ForeignNameTail,
    (
        ( Lang = lang_c
        ; Lang = lang_java
        ; Lang = lang_csharp
        ),
        IsValidForeignName = pred_to_bool(is_valid_c_identifier(ForeignName))
    ;
        ( Lang = lang_il
        ; Lang = lang_erlang
        ),
        sorry($module, $pred, "foreign_export_enum for target language")
    ),
    (
        IsValidForeignName = yes,
        map.det_insert(UnqualSymName, ForeignName, !NameMap)
    ;
        IsValidForeignName = no,
        list.cons(UnqualSymName, !BadCtors)
    ).

%-----------------------------------------------------------------------------%

:- pred add_pragma_foreign_enum(pragma_info_foreign_enum::in,
    import_status::in, prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_foreign_enum(FEInfo, ImportStatus, Context, !ModuleInfo, !Specs) :-
    FEInfo = pragma_info_foreign_enum(Lang, TypeCtor, ForeignTagValues),
    TypeCtor = type_ctor(TypeName, TypeArity),
    module_info_get_type_table(!.ModuleInfo, TypeTable0),
    ContextPieces = [words("In"), quote("pragma foreign_enum"),
        words("declaration for"),
        sym_name_and_arity(TypeName / TypeArity), suffix(":"), nl],
    (
        % Emit an error message for foreign_enum pragmas for the
        % builtin atomic types.
        TypeArity = 0,
        ( TypeName = unqualified("character")
        ; TypeName = unqualified("float")
        ; TypeName = unqualified("int")
        ; TypeName = unqualified("string")
        )
    ->
        MaybeSeverity = yes(severity_error),
        ErrorPieces = [words("error: "),
            sym_name_and_arity(TypeName / TypeArity),
            words("is an atomic type"), suffix(".")]
    ;
        search_type_ctor_defn(TypeTable0, TypeCtor, TypeDefn0)
    ->
        get_type_defn_body(TypeDefn0, TypeBody0),
        (
            ( TypeBody0 = hlds_eqv_type(_)
            ; TypeBody0 = hlds_abstract_type(_)
            ; TypeBody0 = hlds_solver_type(_, _)
            ; TypeBody0 = hlds_foreign_type(_)
            ),
            MaybeSeverity = yes(severity_error),
            ErrorPieces = [words("error: "),
                sym_name_and_arity(TypeName / TypeArity),
                words("is not an enumeration type"), suffix(".")]
        ;
            TypeBody0 = hlds_du_type(Ctors, OldTagValues, CheaperTagTest,
                DuTypeKind0, MaybeUserEq, MaybeDirectArgCtors,
                ReservedTag, ReservedAddr, IsForeignType),
            % Work out what language's foreign_enum pragma we should be
            % looking at for the current compilation target language.
            module_info_get_globals(!.ModuleInfo, Globals),
            globals.get_target(Globals, TargetLanguage),
            LangForForeignEnums =
                target_lang_to_foreign_enum_lang(TargetLanguage),
            (
                ( DuTypeKind0 = du_type_kind_direct_dummy
                ; DuTypeKind0 = du_type_kind_mercury_enum
                ),
                get_type_defn_status(TypeDefn0, TypeStatus),
                % Either both the type and the pragma are defined in this
                % module or they are both imported. Any other combination
                % is illegal.
                IsTypeLocal = status_defined_in_this_module(TypeStatus),
                (
                    (
                        IsTypeLocal = yes,
                        ( ImportStatus = status_local
                        ; ImportStatus = status_exported_to_submodules
                        )
                    ;
                        IsTypeLocal = no,
                        status_is_imported(ImportStatus) = yes
                    )
                ->
                    % XXX We should also check that this type is not
                    % the subject of a reserved tag pragma.
                    DuTypeKind = du_type_kind_foreign_enum(Lang),
                    build_foreign_enum_tag_map(Context, ContextPieces,
                        TypeName, ForeignTagValues, MaybeForeignTagMap,
                        !Specs),
                    (
                        LangForForeignEnums = Lang,
                        MaybeForeignTagMap = yes(ForeignTagMap)
                    ->
                        map.foldl2(make_foreign_tag(Lang, ForeignTagMap),
                            OldTagValues, map.init, TagValues, [],
                            UnmappedCtors),
                        (
                            UnmappedCtors = [],
                            TypeBody = hlds_du_type(Ctors, TagValues,
                                CheaperTagTest, DuTypeKind, MaybeUserEq,
                                MaybeDirectArgCtors, ReservedTag, ReservedAddr,
                                IsForeignType),
                            set_type_defn_body(TypeBody, TypeDefn0, TypeDefn),
                            replace_type_ctor_defn(TypeCtor, TypeDefn,
                                TypeTable0, TypeTable),
                            module_info_set_type_table(TypeTable, !ModuleInfo)
                        ;
                            UnmappedCtors = [_ | _],
                            add_foreign_enum_unmapped_ctors_error(Context,
                                ContextPieces, UnmappedCtors, !Specs)
                        )
                    ;
                        % If there are no matching foreign_enum pragmas for
                        % this target language, then don't do anything.
                        true
                    ),
                    MaybeSeverity = no,
                    ErrorPieces = []
                ;
                    ImportStatus = status_exported
                ->
                    add_foreign_enum_pragma_in_interface_error(Context,
                        TypeName, TypeArity, !Specs),
                    MaybeSeverity = no,
                    ErrorPieces = []
                ;
                    MaybeSeverity = yes(severity_error),
                    ErrorPieces = [words("error: "),
                        sym_name_and_arity(TypeName / TypeArity),
                        words("is not defined in this module.")]
                )
            ;
                DuTypeKind0 = du_type_kind_foreign_enum(_),
                (
                    ( LangForForeignEnums \= Lang
                    ; ImportStatus = status_opt_imported
                    )
                ->
                     MaybeSeverity = no,
                     ErrorPieces = []
                ;
                     MaybeSeverity = yes(severity_error),
                     ErrorPieces = [words("error: "),
                        sym_name_and_arity(TypeName / TypeArity),
                        words("has multiple foreign_enum pragmas.")]
                )
            ;
                ( DuTypeKind0 = du_type_kind_general
                ; DuTypeKind0 = du_type_kind_notag(_, _, _)
                ),
                MaybeSeverity = yes(severity_error),
                ErrorPieces = [words("error: "),
                    sym_name_and_arity(TypeName / TypeArity),
                    words("is not an enumeration type"), suffix(".")]
            )
        )
    ;
        % This else-branch corresponds to an undefined type. We do not
        % issue an error message for it here since module qualification
        % will have already done so.
        MaybeSeverity = no,
        ErrorPieces = []
    ),
    (
        ErrorPieces = []
    ;
        ErrorPieces = [_ | _],
        (
            MaybeSeverity = yes(Severity)
        ;
            MaybeSeverity = no,
            unexpected($module, $pred, "no severity")
        ),
        Msg = simple_msg(Context, [always(ContextPieces ++ ErrorPieces)]),
        Spec = error_spec(Severity, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ).

:- pred build_foreign_enum_tag_map(prog_context::in, format_components::in,
    sym_name::in, assoc_list(sym_name, string)::in,
    maybe(map(sym_name, string))::out,
    list(error_spec)::in, list(error_spec)::out) is det.

build_foreign_enum_tag_map(Context, ContextPieces, TypeName, ForeignTagValues0,
        MaybeForeignTagMap, !Specs) :-
    ( sym_name_get_module_name(TypeName, TypeModuleName0) ->
        TypeModuleName = TypeModuleName0
    ;
        unexpected($module, $pred,
            "unqualified type name while processing foreign tags.")
    ),
    list.map_foldl(fixup_foreign_tag_val_qualification(TypeModuleName),
        ForeignTagValues0, ForeignTagValues1, [], BadCtors),
    (
        BadCtors = [],
        ( bimap.from_assoc_list(ForeignTagValues1, ForeignTagValues) ->
            ForeignTagMap = ForeignTagValues ^ forward_map,
            MaybeForeignTagMap = yes(ForeignTagMap)
        ;
            add_foreign_enum_bijection_error(Context, ContextPieces, !Specs),
            MaybeForeignTagMap = no
        )
    ;
        BadCtors = [_ | _],
        MaybeForeignTagMap = no
    ).

    % The constructor names we get from the parse tree may be unqualified
    % but the ones we match against in the HLDS are not. Module qualify them.
    %
    % XXX module_qual.m should really be doing this rather than add_pragma.m.
    %
:- pred fixup_foreign_tag_val_qualification(module_name::in,
    pair(sym_name, string)::in, pair(sym_name, string)::out,
    list(sym_name)::in, list(sym_name)::out) is det.

fixup_foreign_tag_val_qualification(TypeModuleName, !NamesAndTags,
        !BadCtors) :-
    !.NamesAndTags = CtorSymName0 - ForeignTag,
    (
        CtorSymName0 = unqualified(Name),
        CtorSymName = qualified(TypeModuleName, Name)
    ;
        CtorSymName0 = qualified(CtorModuleName, Name),
        ( match_sym_name(CtorModuleName, TypeModuleName) ->
            CtorSymName = qualified(TypeModuleName, Name)
        ;
            !:BadCtors = [CtorSymName0 | !.BadCtors],
            CtorSymName = CtorSymName0
        )
    ),
    !:NamesAndTags = CtorSymName - ForeignTag.

    % For a given target language work out which language's foreign_enum
    % pragma we should be looking at.
    %
:- func target_lang_to_foreign_enum_lang(compilation_target)
    = foreign_language.

target_lang_to_foreign_enum_lang(target_c) = lang_c.
target_lang_to_foreign_enum_lang(target_il) = lang_il.
target_lang_to_foreign_enum_lang(target_csharp) = lang_csharp.
target_lang_to_foreign_enum_lang(target_java) = lang_java.
target_lang_to_foreign_enum_lang(target_x86_64) =
    sorry($module, $pred, "pragma foreign_enum and --target `x86_64'.").
target_lang_to_foreign_enum_lang(target_erlang) = lang_erlang.

:- pred make_foreign_tag(foreign_language::in, map(sym_name, string)::in,
    cons_id::in, cons_tag::in,
    cons_tag_values::in, cons_tag_values::out,
    list(sym_name)::in, list(sym_name)::out) is det.

make_foreign_tag(ForeignLanguage, ForeignTagMap, ConsId, _, !ConsTagValues,
        !UnmappedCtors) :-
    ( ConsId = cons(ConsSymName0, 0, _) ->
        ConsSymName = ConsSymName0
    ;
        unexpected($module, $pred, "non arity zero enumeration constant.")
    ),
    ( map.search(ForeignTagMap, ConsSymName, ForeignTagValue) ->
        ForeignTag = foreign_tag(ForeignLanguage, ForeignTagValue),
        map.set(ConsId, ForeignTag, !ConsTagValues)
    ;
        !:UnmappedCtors = [ConsSymName | !.UnmappedCtors]
    ).

:- pred add_foreign_enum_unmapped_ctors_error(prog_context::in,
    list(format_component)::in,
    list(sym_name)::in(non_empty_list),
    list(error_spec)::in, list(error_spec)::out) is det.

add_foreign_enum_unmapped_ctors_error(Context, ContextPieces, UnmappedCtors0,
        !Specs) :-
    ErrorPieces = [
        words("error: not all constructors have a foreign value.")
    ],
    list.sort(UnmappedCtors0, UnmappedCtors),
    CtorComponents = list.map((func(S) = [sym_name(S)]), UnmappedCtors),
    CtorList = component_list_to_line_pieces(CtorComponents, [nl]),
    DoOrDoes = choose_number(UnmappedCtors,
        "constructor does not have a foreign value",
        "constructors do not have foreign values"),
    VerboseErrorPieces = [words("The following"), words(DoOrDoes),
        nl_indent_delta(2)] ++ CtorList,
    Msg = simple_msg(Context,
        [always(ContextPieces ++ ErrorPieces),
            verbose_only(VerboseErrorPieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    list.cons(Spec, !Specs).

:- pred add_foreign_enum_bijection_error(prog_context::in,
    format_components::in, list(error_spec)::in, list(error_spec)::out)
    is det.

add_foreign_enum_bijection_error(Context, ContextPieces, !Specs) :-
    ErrorPieces = [words("error: "),
        words("the mapping between Mercury enumeration values and"),
        words("foreign values does not form a bijection."), nl],
    Msg = simple_msg(Context, [always(ContextPieces ++ ErrorPieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    list.cons(Spec, !Specs).

:- pred add_foreign_enum_pragma_in_interface_error(prog_context::in,
    sym_name::in, arity::in, list(error_spec)::in, list(error_spec)::out)
    is det.

add_foreign_enum_pragma_in_interface_error(Context, TypeName, TypeArity,
        !Specs) :-
    ErrorPieces = [words("Error: "),
        words("`pragma foreign_enum' declaration for"),
        sym_name_and_arity(TypeName / TypeArity),
        words("in module interface."), nl ],
    Msg = simple_msg(Context, [always(ErrorPieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    list.cons(Spec, !Specs).

%-----------------------------------------------------------------------------%

:- pred add_pragma_unused_args(pragma_info_unused_args::in,
    prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_unused_args(UnusedArgsInfo, Context, !ModuleInfo, !Specs) :-
    UnusedArgsInfo = pragma_info_unused_args(PredNameArityPFMn, UnusedArgs),
    PredNameArityPFMn = pred_name_arity_pf_mn(SymName, Arity, PredOrFunc,
        ModeNum),
    module_info_get_predicate_table(!.ModuleInfo, Preds),
    predicate_table_lookup_pf_sym_arity(Preds, is_fully_qualified,
        PredOrFunc, SymName, Arity, PredIds),
    (
        PredIds = [PredId],
        module_info_get_unused_arg_info(!.ModuleInfo, UnusedArgInfo0),
        % Convert the mode number to a proc_id.
        proc_id_to_int(ProcId, ModeNum),
        map.set(proc(PredId, ProcId), UnusedArgs,
            UnusedArgInfo0, UnusedArgInfo),
        module_info_set_unused_arg_info(UnusedArgInfo, !ModuleInfo)
    ;
        PredIds = [],
        Pieces = [words("Internal compiler error: "),
            words("unknown predicate in"), quote("pragma unused_args"),
            suffix("."), nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ;
        PredIds = [_, _ | _],
        Pieces = [words("Internal compiler error: "),
            words("ambiguous predicate in "), quote("pragma unused_args"),
            suffix("."), nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ).

%-----------------------------------------------------------------------------%

:- pred add_pragma_exceptions(pragma_info_exceptions::in, prog_context::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_exceptions(ExceptionsInfo, _Context, !ModuleInfo, !Specs) :-
    ExceptionsInfo = pragma_info_exceptions(PredNameArityPFMn, ThrowStatus),
    PredNameArityPFMn = pred_name_arity_pf_mn(SymName, Arity, PredOrFunc,
        ModeNum),
    module_info_get_predicate_table(!.ModuleInfo, Preds),
    predicate_table_lookup_pf_sym_arity(Preds, is_fully_qualified,
        PredOrFunc, SymName, Arity, PredIds),
    (
        PredIds = [PredId],
        module_info_get_exception_info(!.ModuleInfo, ExceptionInfo0),
        % convert the mode number to a proc_id
        proc_id_to_int(ProcId, ModeNum),
        ProcExceptionInfo = proc_exception_info(ThrowStatus, no),
        map.set(proc(PredId, ProcId), ProcExceptionInfo,
            ExceptionInfo0, ExceptionInfo),
        module_info_set_exception_info(ExceptionInfo, !ModuleInfo)
    ;
        ( PredIds = []
        ; PredIds = [_, _ | _]
        )
        % XXX We'll just ignore this for the time being -
        % it causes errors with transitive-intermodule optimization.
        % XXX What kinds of errors?
    ).

%-----------------------------------------------------------------------------%

:- pred add_pragma_trailing_info(pragma_info_trailing_info::in,
    prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_trailing_info(TrailingInfo, _Context, !ModuleInfo, !Specs) :-
    TrailingInfo = pragma_info_trailing_info(PredNameArityPFMn,
        TrailingStatus),
    PredNameArityPFMn = pred_name_arity_pf_mn(SymName, Arity, PredOrFunc,
        ModeNum),
    module_info_get_predicate_table(!.ModuleInfo, Preds),
    predicate_table_lookup_pf_sym_arity(Preds, is_fully_qualified,
        PredOrFunc, SymName, Arity, PredIds),
    (
        PredIds = [PredId],
        module_info_get_trailing_info(!.ModuleInfo, TrailingMap0),
        proc_id_to_int(ProcId, ModeNum),
        map.set(proc(PredId, ProcId), proc_trailing_info(TrailingStatus, no),
            TrailingMap0, TrailingMap),
        module_info_set_trailing_info(TrailingMap, !ModuleInfo)
    ;
        ( PredIds = []
        ; PredIds = [_, _ | _]
        )
        % XXX We'll just ignore this for the time being -
        % it causes errors with transitive-intermodule optimization.
        % XXX What kinds of errors?
    ).

%-----------------------------------------------------------------------------%

:- pred add_pragma_mm_tabling_info(pragma_info_mm_tabling_info::in,
    prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_mm_tabling_info(MMTablingInfo, _Context, !ModuleInfo, !Specs) :-
    MMTablingInfo = pragma_info_mm_tabling_info(PredNameArityPFMn,
        TablingStatus),
    PredNameArityPFMn = pred_name_arity_pf_mn(SymName, Arity, PredOrFunc,
        ModeNum),
    module_info_get_predicate_table(!.ModuleInfo, Preds),
    predicate_table_lookup_pf_sym_arity(Preds, is_fully_qualified,
        PredOrFunc, SymName, Arity, PredIds),
    (
        PredIds = [PredId],
        module_info_get_mm_tabling_info(!.ModuleInfo, TablingInfo0),
        proc_id_to_int(ProcId, ModeNum),
        map.set(proc(PredId, ProcId), proc_mm_tabling_info(TablingStatus, no),
            TablingInfo0, TablingInfo),
        module_info_set_mm_tabling_info(TablingInfo, !ModuleInfo)
    ;
        ( PredIds = []
        ; PredIds = [_, _ | _]
        )
        % XXX We'll just ignore this for the time being -
        % it causes errors with transitive-intermodule optimization.
        % XXX What kinds of errors?
    ).

%-----------------------------------------------------------------------------%

:- pred add_pragma_type_spec(pragma_info_type_spec::in, term.context::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_type_spec(TSInfo, Context, !ModuleInfo, !QualInfo, !Specs) :-
    TSInfo = pragma_info_type_spec(SymName, _, Arity, MaybePredOrFunc,
        _, _, _, _),
    module_info_get_predicate_table(!.ModuleInfo, Preds),
    (
        MaybePredOrFunc = yes(PredOrFunc),
        adjust_func_arity(PredOrFunc, Arity, PredArity),
        predicate_table_lookup_pf_sym_arity(Preds, is_fully_qualified,
            PredOrFunc, SymName, PredArity, PredIds)
    ;
        MaybePredOrFunc = no,
        predicate_table_lookup_sym_arity(Preds, is_fully_qualified,
            SymName, Arity, PredIds)
    ),
    (
        PredIds = [_ | _],
        list.foldl3(add_pragma_type_spec_2(TSInfo, Context), PredIds,
            !ModuleInfo, !QualInfo, !Specs)
    ;
        PredIds = [],
        undefined_pred_or_func_error(SymName, Arity, Context,
            [quote(":- pragma type_spec"), words("declaration")], !Specs)
    ).

:- pred add_pragma_type_spec_2(pragma_info_type_spec::in,
    prog_context::in, pred_id::in, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_type_spec_2(TSInfo0, Context, PredId, !ModuleInfo, !QualInfo,
        !Specs) :-
    TSInfo0 = pragma_info_type_spec(SymName, SpecName, Arity, _, MaybeModes,
        Subst, TVarSet0, ExpandedItems),
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    handle_pragma_type_spec_subst(Context, Subst, PredInfo0,
        TVarSet0, TVarSet, Types, ExistQVars, ClassContext, SubstOk,
        !ModuleInfo, !Specs),
    (
        SubstOk = yes(RenamedSubst),
        pred_info_get_procedures(PredInfo0, Procs0),
        handle_pragma_type_spec_modes(SymName, Arity, Context, MaybeModes,
            MaybeProcIds, Procs0, Procs1, !ModuleInfo, !Specs),
        % Remove any imported structure sharing and reuse information for the
        % original procedure as they won't be (directly) applicable.
        map.map_values_only(reset_imported_structure_sharing_reuse,
            Procs1, Procs),
        module_info_get_globals(!.ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, user_guided_type_specialization,
            DoTypeSpec),
        globals.lookup_bool_option(Globals, smart_recompilation, Smart),
        % XXX Should check whether smart recompilation has been disabled?
        (
            MaybeProcIds = yes(ProcIds),
            % Even if we aren't doing type specialization, we need to create
            % the interface procedures for local predicates to check the
            % type-class correctness of the requested specializations.
            %
            % If we are doing smart recompilation, we need to record the
            % pragmas even if we aren't doing type specialization, to avoid
            % problems with differing output for the recompilation tests
            % in debugging grades.

            ( DoTypeSpec = yes
            ; \+ pred_info_is_imported(PredInfo0)
            ; Smart = yes
            )
        ->
            % Build a clause to call the old predicate with the specified types
            % to force the specialization. For imported predicates this forces
            % the creation of the proper interface.
            %
            PredOrFunc = pred_info_is_pred_or_func(PredInfo0),
            adjust_func_arity(PredOrFunc, Arity, PredArity),
            varset.init(ArgVarSet0),
            make_n_fresh_vars("HeadVar__", PredArity, Args,
                ArgVarSet0, ArgVarSet),
            % XXX We could use explicit type qualifications here for the
            % argument types, but explicit type qualification doesn't work
            % correctly with type inference due to a bug somewhere in
            % typecheck.m -- the explicitly declared types are not kept in
            % sync with the predicate's tvarset after the first pass of
            % type checking.
            % map.from_corresponding_lists(Args, Types, VarTypes0)
            init_vartypes(VarTypes0),
            goal_info_init(GoalInfo0),
            set_of_var.list_to_set(Args, NonLocals),
            goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo1),
            goal_info_set_context(Context, GoalInfo1, GoalInfo),

            % We don't record the called predicate as used -- it is only used
            % if there is some other call. This call is only used to make
            % higher_order.m generate the interface for the type specialized
            % procedure, and will be removed by higher_order.m after that
            % is done.
            do_construct_pred_or_func_call(PredId, PredOrFunc,
                SymName, Args, GoalInfo, Goal),
            Clause = clause(selected_modes(ProcIds), Goal, impl_lang_mercury,
                Context, []),
            map.init(TVarNameMap),
            ArgsVec = proc_arg_vector_init(PredOrFunc, Args),
            set_clause_list([Clause], ClausesRep),
            rtti_varmaps_init(RttiVarMaps),
            HasForeignClauses = no,
            Clauses = clauses_info(ArgVarSet, VarTypes0, TVarNameMap,
                VarTypes0, ArgsVec, ClausesRep,
                init_clause_item_numbers_comp_gen,
                RttiVarMaps, HasForeignClauses),
            pred_info_get_markers(PredInfo0, Markers0),
            add_marker(marker_calls_are_fully_qualified, Markers0, Markers),
            map.init(Proofs),
            map.init(ConstraintMap),

            ( pred_info_is_imported(PredInfo0) ->
                Status = status_opt_imported
            ;
                pred_info_get_import_status(PredInfo0, Status)
            ),

            ModuleName = pred_info_module(PredInfo0),
            pred_info_get_origin(PredInfo0, OrigOrigin),
            SubstDesc = list.map(subst_desc, Subst),
            Origin = origin_transformed(
                transform_type_specialization(SubstDesc), OrigOrigin, PredId),
            pred_info_get_var_name_remap(PredInfo0, VarNameRemap),
            pred_info_init(ModuleName, SpecName, PredArity, PredOrFunc,
                Context, Origin, Status, goal_type_none, Markers,
                Types, TVarSet, ExistQVars, ClassContext, Proofs,
                ConstraintMap, Clauses, VarNameRemap, NewPredInfo0),
            pred_info_set_procedures(Procs, NewPredInfo0, NewPredInfo),
            module_info_get_predicate_table(!.ModuleInfo, PredTable0),
            predicate_table_insert(NewPredInfo, NewPredId,
                PredTable0, PredTable),
            module_info_set_predicate_table(PredTable, !ModuleInfo),

            % Record the type specialisation in the module_info.
            module_info_get_type_spec_info(!.ModuleInfo, TypeSpecInfo0),
            TypeSpecInfo0 = type_spec_info(ProcsToSpec0,
                ForceVersions0, SpecMap0, PragmaMap0),
            list.map((pred(ProcId::in, PredProcId::out) is det :-
                    PredProcId = proc(PredId, ProcId)
                ), ProcIds, PredProcIds),
            set.insert_list(PredProcIds, ProcsToSpec0, ProcsToSpec),
            set.insert(NewPredId, ForceVersions0, ForceVersions),

            ( Status = status_opt_imported ->
                % For imported predicates dead_proc_elim.m needs to know that
                % if the original predicate is used, the predicate to force
                % the production of the specialised interface is also used.
                multi_map.set(PredId, NewPredId, SpecMap0, SpecMap)
            ;
                SpecMap = SpecMap0
            ),
            TSInfo = pragma_info_type_spec(SymName, SpecName, Arity,
                yes(PredOrFunc), MaybeModes, map.to_assoc_list(RenamedSubst),
                TVarSet, ExpandedItems),
            multi_map.set(PredId, TSInfo, PragmaMap0, PragmaMap),
            TypeSpecInfo = type_spec_info(ProcsToSpec, ForceVersions, SpecMap,
                PragmaMap),
            module_info_set_type_spec_info(TypeSpecInfo, !ModuleInfo),

            IsImported = status_is_imported(Status),
            (
                IsImported = yes,
                ItemType = pred_or_func_to_item_type(PredOrFunc),
                apply_to_recompilation_info(
                    recompilation.record_expanded_items(
                        item_id(ItemType, item_name(SymName, Arity)),
                        ExpandedItems),
                    !QualInfo)
            ;
                IsImported = no
            )
        ;
            true
        )
    ;
        SubstOk = no
    ).

:- func subst_desc(pair(tvar, mer_type)) = pair(int, mer_type).

subst_desc(TVar - Type) = var_to_int(TVar) - Type.

    % Check that the type substitution for a `:- pragma type_spec'
    % declaration is valid.
    % A type substitution is invalid if:
    %   - it substitutes unknown type variables
    %   - it substitutes existentially quantified type variables
    % Type substitutions are also invalid if the replacement types are
    % not ground, however this is a (hopefully temporary) limitation
    % of the current implementation, so it only results in a warning.
    %
:- pred handle_pragma_type_spec_subst(prog_context::in,
    assoc_list(tvar, mer_type)::in, pred_info::in, tvarset::in, tvarset::out,
    list(mer_type)::out, existq_tvars::out, prog_constraints::out,
    maybe(tsubst)::out, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

handle_pragma_type_spec_subst(Context, Subst, PredInfo0, TVarSet0, TVarSet,
        Types, ExistQVars, ClassContext, SubstOk, !ModuleInfo, !Specs) :-
    assoc_list.keys(Subst, VarsToSub),
    (
        Subst = [],
        unexpected($module, $pred, "empty substitution")
    ;
        Subst = [_ | _],
        find_duplicate_list_elements(VarsToSub, MultiSubstVars0),
        (
            MultiSubstVars0 = [_ | _],
            list.sort_and_remove_dups(MultiSubstVars0, MultiSubstVars),
            report_multiple_subst_vars(PredInfo0, Context, TVarSet0,
                MultiSubstVars, !Specs),
            ExistQVars = [],
            Types = [],
            ClassContext = constraints([], []),
            varset.init(TVarSet),
            SubstOk = no
        ;
            MultiSubstVars0 = [],
            pred_info_get_typevarset(PredInfo0, CalledTVarSet),
            varset.create_name_var_map(CalledTVarSet, NameVarIndex0),
            list.filter((pred(Var::in) is semidet :-
                varset.lookup_name(TVarSet0, Var, VarName),
                \+ map.contains(NameVarIndex0, VarName)
            ), VarsToSub, UnknownVarsToSub),
            (
                UnknownVarsToSub = [],
                % Check that the substitution is not recursive.
                set.list_to_set(VarsToSub, VarsToSubSet),

                assoc_list.values(Subst, SubstTypes0),
                type_vars_list(SubstTypes0, TVarsInSubstTypes0),
                set.list_to_set(TVarsInSubstTypes0, TVarsInSubstTypes),

                set.intersect(TVarsInSubstTypes, VarsToSubSet, RecSubstTVars0),
                set.to_sorted_list(RecSubstTVars0, RecSubstTVars),

                (
                    RecSubstTVars = [],
                    map.init(TVarRenaming0),
                    list.append(VarsToSub, TVarsInSubstTypes0, VarsToReplace),

                    get_new_tvars(VarsToReplace, TVarSet0, CalledTVarSet,
                        TVarSet, NameVarIndex0, _,
                        TVarRenaming0, TVarRenaming),

                    % Check that none of the existentially quantified variables
                    % were substituted.
                    map.apply_to_list(VarsToSub, TVarRenaming,
                        RenamedVarsToSub),
                    pred_info_get_exist_quant_tvars(PredInfo0, ExistQVars),
                    list.filter((pred(RenamedVar::in) is semidet :-
                        list.member(RenamedVar, ExistQVars)
                    ), RenamedVarsToSub, SubExistQVars),
                    (
                        SubExistQVars = [],
                        map.init(TypeSubst0),
                        apply_variable_renaming_to_type_list(TVarRenaming,
                            SubstTypes0, SubstTypes),
                        assoc_list.from_corresponding_lists(RenamedVarsToSub,
                            SubstTypes, SubAL),
                        list.foldl(map_set_from_pair, SubAL,
                            TypeSubst0, TypeSubst),

                        % Apply the substitution.
                        pred_info_get_arg_types(PredInfo0, Types0),
                        pred_info_get_class_context(PredInfo0, ClassContext0),
                        apply_rec_subst_to_type_list(TypeSubst, Types0, Types),
                        apply_rec_subst_to_prog_constraints(TypeSubst,
                            ClassContext0, ClassContext),
                        SubstOk = yes(TypeSubst)
                    ;
                        SubExistQVars = [_ | _],
                        report_subst_existq_tvars(PredInfo0, Context,
                            SubExistQVars, !Specs),
                        Types = [],
                        ClassContext = constraints([], []),
                        SubstOk = no
                    )
                ;
                    RecSubstTVars = [_ | _],
                    report_recursive_subst(PredInfo0, Context, TVarSet0,
                        RecSubstTVars, !Specs),
                    ExistQVars = [],
                    Types = [],
                    ClassContext = constraints([], []),
                    varset.init(TVarSet),
                    SubstOk = no
                )
            ;
                UnknownVarsToSub = [_ | _],
                report_unknown_vars_to_subst(PredInfo0, Context, TVarSet0,
                    UnknownVarsToSub, !Specs),
                ExistQVars = [],
                Types = [],
                ClassContext = constraints([], []),
                varset.init(TVarSet),
                SubstOk = no
            )
        )
    ).

:- pred map_set_from_pair(pair(K, V)::in, map(K, V)::in, map(K, V)::out)
    is det.

map_set_from_pair(K - V, !Map) :-
    map.set(K, V, !Map).

:- pred find_duplicate_list_elements(list(T)::in, list(T)::out) is det.

find_duplicate_list_elements([], []).
find_duplicate_list_elements([H | T], DupVars) :-
    find_duplicate_list_elements(T, DupVars0),
    ( list.member(H, T) ->
        DupVars = [H | DupVars0]
    ;
        DupVars = DupVars0
    ).

:- pred report_subst_existq_tvars(pred_info::in, prog_context::in,
    list(tvar)::in, list(error_spec)::in, list(error_spec)::out) is det.

report_subst_existq_tvars(PredInfo, Context, SubExistQVars, !Specs) :-
    pred_info_get_typevarset(PredInfo, TVarSet),
    Pieces = pragma_type_spec_to_pieces(PredInfo) ++
        [words("error: the substitution includes"),
        words("the existentially quantified type")] ++
        report_variables(SubExistQVars, TVarSet) ++ [suffix(".")],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

:- pred report_recursive_subst(pred_info::in, prog_context::in, tvarset::in,
    list(tvar)::in, list(error_spec)::in, list(error_spec)::out) is det.

report_recursive_subst(PredInfo, Context, TVarSet, RecursiveVars, !Specs) :-
    Pieces = pragma_type_spec_to_pieces(PredInfo) ++
        [words("error:")] ++ report_variables(RecursiveVars, TVarSet) ++
        [words(choose_number(RecursiveVars, "occurs", "occur")),
        words("on both sides of the substitution.")],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

:- pred report_multiple_subst_vars(pred_info::in, prog_context::in,
    tvarset::in, list(tvar)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_multiple_subst_vars(PredInfo, Context, TVarSet, MultiSubstVars,
        !Specs) :-
    Pieces = pragma_type_spec_to_pieces(PredInfo) ++
        [words("error:")] ++ report_variables(MultiSubstVars, TVarSet) ++
        [words(choose_number(MultiSubstVars, "has", "have")),
        words("multiple replacement types.")],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

:- pred report_unknown_vars_to_subst(pred_info::in, prog_context::in,
    tvarset::in, list(tvar)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_unknown_vars_to_subst(PredInfo, Context, TVarSet, UnknownVars,
        !Specs) :-
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    (
        PredOrFunc = pf_predicate,
        Decl = "`:- pred'"
    ;
        PredOrFunc = pf_function,
        Decl = "`:- func'"
    ),
    Pieces = pragma_type_spec_to_pieces(PredInfo) ++
        [words("error:")] ++ report_variables(UnknownVars, TVarSet) ++
        [words(choose_number(UnknownVars, "does not", "do not")),
        words("occur in the"), fixed(Decl), words("declaration.")],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

:- func pragma_type_spec_to_pieces(pred_info) = list(format_component).

pragma_type_spec_to_pieces(PredInfo) = Pieces :-
    Module = pred_info_module(PredInfo),
    Name = pred_info_name(PredInfo),
    Arity = pred_info_orig_arity(PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    SimpleCallId = simple_call_id(PredOrFunc, qualified(Module, Name), Arity),
    Pieces = [words("In"), quote(":- pragma type_spec"),
        words("declaration for"), simple_call(SimpleCallId), suffix(":"), nl].

:- func report_variables(list(tvar), tvarset) = list(format_component).

report_variables(SubExistQVars, VarSet) =
    [words(choose_number(SubExistQVars, "variable", "variables")),
    quote(mercury_vars_to_string(VarSet, no, SubExistQVars))].

    % Check that the mode list for a `:- pragma type_spec' declaration
    % specifies a known procedure.
    %
:- pred handle_pragma_type_spec_modes(sym_name::in, arity::in,
    prog_context::in, maybe(list(mer_mode))::in,
    maybe(list(proc_id))::out, proc_table::in, proc_table::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

handle_pragma_type_spec_modes(SymName, Arity, Context, MaybeModes,
        MaybeProcIds, !Procs, !ModuleInfo, !Specs) :-
    (
        MaybeModes = yes(Modes),
        map.to_assoc_list(!.Procs, ExistingProcs),
        (
            get_procedure_matching_argmodes(ExistingProcs, Modes,
                !.ModuleInfo, ProcId)
        ->
            map.lookup(!.Procs, ProcId, ProcInfo),
            !:Procs = map.singleton(ProcId, ProcInfo),
            ProcIds = [ProcId],
            MaybeProcIds = yes(ProcIds)
        ;
            module_info_incr_errors(!ModuleInfo),
            undefined_mode_error(SymName, Arity, Context,
                [quote(":- pragma type_spec"), words("declaration")], !Specs),
            MaybeProcIds = no
        )
    ;
        MaybeModes = no,
        map.keys(!.Procs, ProcIds),
        MaybeProcIds = yes(ProcIds)
    ).

:- pred reset_imported_structure_sharing_reuse(
    proc_info::in, proc_info::out) is det.

reset_imported_structure_sharing_reuse(!ProcInfo) :-
    proc_info_reset_imported_structure_sharing(!ProcInfo),
    proc_info_reset_imported_structure_reuse(!ProcInfo).

%-----------------------------------------------------------------------------%

:- pred add_pragma_termination2_info(pragma_info_termination2_info::in,
    prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_termination2_info(Term2Info, Context, !ModuleInfo, !Specs) :-
    Term2Info = pragma_info_termination2_info(PredModesPF,
        MaybePragmaSuccessArgSizeInfo, MaybePragmaFailureArgSizeInfo,
        MaybePragmaTerminationInfo),
    PredModesPF = pred_name_modes_pf(SymName, ModeList, PredOrFunc),
    module_info_get_predicate_table(!.ModuleInfo, Preds),
    list.length(ModeList, Arity),
    predicate_table_lookup_pf_sym_arity(Preds, is_fully_qualified,
        PredOrFunc, SymName, Arity, PredIds),
    (
        PredIds = []
        % XXX This happens in `.trans_opt' files sometimes --
        % so just ignore it.
        %   undefined_pred_or_func_error(
        %        SymName, Arity, Context,
        %        "`:- pragma termination2_info' declaration", !Specs)
    ;
        PredIds = [PredId],
        module_info_get_preds(!.ModuleInfo, PredTable0),
        map.lookup(PredTable0, PredId, PredInfo0),
        pred_info_get_procedures(PredInfo0, ProcTable0),
        map.to_assoc_list(ProcTable0, ProcList),
        (
            get_procedure_matching_declmodes_with_renaming(ProcList,
                ModeList, !.ModuleInfo, ProcId)
        ->
            map.lookup(ProcTable0, ProcId, ProcInfo0),
            add_context_to_constr_termination_info(
                MaybePragmaTerminationInfo, Context, MaybeTerminationInfo),

            some [!TermInfo] (
                proc_info_get_termination2_info(ProcInfo0, !:TermInfo),

                !TermInfo ^ import_success := MaybePragmaSuccessArgSizeInfo,
                !TermInfo ^ import_failure := MaybePragmaFailureArgSizeInfo,
                !TermInfo ^ term_status := MaybeTerminationInfo,

                proc_info_set_termination2_info(!.TermInfo,
                    ProcInfo0, ProcInfo)
            ),
            map.det_update(ProcId, ProcInfo, ProcTable0, ProcTable),
            pred_info_set_procedures(ProcTable, PredInfo0, PredInfo),
            map.det_update(PredId, PredInfo, PredTable0, PredTable),
            module_info_set_preds(PredTable, !ModuleInfo)
        ;
            Pieces = [words("Error: `:- pragma termination2_info'"),
                words("declaration for undeclared mode of"),
                simple_call(simple_call_id(PredOrFunc, SymName, Arity)),
                suffix("."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        )
    ;
        PredIds = [_, _ | _],
        Pieces = [words("Error: ambiguous predicate name"),
            simple_call(simple_call_id(PredOrFunc, SymName, Arity)),
            words("in"), fixed("`pragma termination2_info'."), nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ).

%-----------------------------------------------------------------------------%

:- pred add_pragma_structure_sharing(pragma_info_structure_sharing::in,
    prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_structure_sharing(SharingInfo, Context, !ModuleInfo, !Specs):-
    SharingInfo = pragma_info_structure_sharing(PredNameModesPF,
        HeadVars, Types, MaybeSharingDomain),
    (
        MaybeSharingDomain = no
    ;
        MaybeSharingDomain = yes(SharingDomain),
        PredNameModesPF = pred_name_modes_pf(SymName, ModeList, PredOrFunc),
        module_info_get_predicate_table(!.ModuleInfo, Preds),
        list.length(ModeList, Arity),
        predicate_table_lookup_pf_sym_arity(Preds, is_fully_qualified,
            PredOrFunc, SymName, Arity, PredIds),
        (
            PredIds = []
            % XXX This happens in `.trans_opt' files sometimes --
            % so just ignore it.
            %   undefined_pred_or_func_error(SymName, Arity, Context,
            %       "`:- pragma structure_sharing' declaration",
            %       !Specs)
        ;
            PredIds = [PredId],
            module_info_get_preds(!.ModuleInfo, PredTable0),
            map.lookup(PredTable0, PredId, PredInfo0),
            pred_info_get_procedures(PredInfo0, ProcTable0),
            map.to_assoc_list(ProcTable0, ProcList),
            (
                get_procedure_matching_declmodes_with_renaming(ProcList,
                    ModeList, !.ModuleInfo, ProcId)
            ->
                map.lookup(ProcTable0, ProcId, ProcInfo0),
                proc_info_set_imported_structure_sharing(HeadVars, Types,
                    SharingDomain, ProcInfo0, ProcInfo),
                map.det_update(ProcId, ProcInfo, ProcTable0, ProcTable),
                pred_info_set_procedures(ProcTable, PredInfo0, PredInfo),
                map.det_update(PredId, PredInfo, PredTable0, PredTable),
                module_info_set_preds(PredTable, !ModuleInfo)
            ;
                Pieces = [words("Error: `:- pragma structure_sharing'"),
                    words("declaration for undeclared mode of"),
                    simple_call(simple_call_id(PredOrFunc, SymName, Arity)),
                    suffix("."), nl],
                Msg = simple_msg(Context, [always(Pieces)]),
                Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
                    [Msg]),
                !:Specs = [Spec | !.Specs]
            )
        ;
            PredIds = [_ , _ | _],
            Pieces = [words("Error: ambiguous predicate name"),
                simple_call(simple_call_id(PredOrFunc, SymName, Arity)),
                words("in"), quote("pragma structure_sharing."),
                suffix("."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
                [Msg]),
            !:Specs = [Spec | !.Specs]
        )
    ).

%-----------------------------------------------------------------------------%

:- pred add_pragma_structure_reuse(pragma_info_structure_reuse::in,
    prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_structure_reuse(ReuseInfo, Context, !ModuleInfo, !Specs):-
    ReuseInfo = pragma_info_structure_reuse(PredNameModesPF, HeadVars,
        Types, MaybeReuseDomain),
    (
        MaybeReuseDomain = no
    ;
        MaybeReuseDomain = yes(ReuseDomain),
        PredNameModesPF = pred_name_modes_pf(SymName, ModeList, PredOrFunc),
        module_info_get_predicate_table(!.ModuleInfo, Preds),
        list.length(ModeList, Arity),
        predicate_table_lookup_pf_sym_arity(Preds, is_fully_qualified,
            PredOrFunc, SymName, Arity, PredIds),
        (
            PredIds = []
            % XXX This happens in `.trans_opt' files sometimes --
            % so just ignore it
            %   undefined_pred_or_func_error(SymName, Arity, Context,
            %       "`:- pragma structure_sharing' declaration",
            %       !Specs)
        ;
            PredIds = [PredId],
            module_info_get_preds(!.ModuleInfo, PredTable0),
            map.lookup(PredTable0, PredId, PredInfo0),
            pred_info_get_procedures(PredInfo0, ProcTable0),
            map.to_assoc_list(ProcTable0, ProcList),
            (
                get_procedure_matching_declmodes_with_renaming(ProcList,
                    ModeList, !.ModuleInfo, ProcId)
            ->
                map.lookup(ProcTable0, ProcId, ProcInfo0),
                proc_info_set_imported_structure_reuse(HeadVars, Types,
                    ReuseDomain, ProcInfo0, ProcInfo),
                map.det_update(ProcId, ProcInfo, ProcTable0, ProcTable),
                pred_info_set_procedures(ProcTable, PredInfo0, PredInfo),
                map.det_update(PredId, PredInfo, PredTable0, PredTable),
                module_info_set_preds(PredTable, !ModuleInfo)
            ;
                Pieces = [words("Error: `:- pragma structure_reuse'"),
                    words("declaration for undeclared mode of"),
                    simple_call(simple_call_id(PredOrFunc, SymName, Arity)),
                    suffix("."), nl],
                Msg = simple_msg(Context, [always(Pieces)]),
                Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
                    [Msg]),
                !:Specs = [Spec | !.Specs]
            )
        ;
            PredIds = [_, _ | _],
            Pieces = [words("Error: ambiguous predicate name"),
                simple_call(simple_call_id(PredOrFunc, SymName, Arity)),
                words("in"), quote("pragma structure_reuse"), suffix("."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        )
    ).

%-----------------------------------------------------------------------------%

:- pred add_pragma_termination_info(pragma_info_termination_info::in,
    prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_termination_info(TermInfo, Context, !ModuleInfo, !Specs) :-
    TermInfo = pragma_info_termination_info(PredModesPF,
        MaybePragmaArgSizeInfo, MaybePragmaTerminationInfo),
    PredModesPF = pred_name_modes_pf(SymName, ModeList, PredOrFunc),
    module_info_get_predicate_table(!.ModuleInfo, Preds),
    list.length(ModeList, Arity),
    predicate_table_lookup_pf_sym_arity(Preds, is_fully_qualified,
        PredOrFunc, SymName, Arity, PredIds),
    (
        PredIds = []
        % XXX This happens in `.trans_opt' files sometimes --
        % so just ignore it.
        %   undefined_pred_or_func_error(SymName, Arity, Context,
        %       "`:- pragma termination_info' declaration",
        %       !Specs
    ;
        PredIds = [PredId],
        module_info_get_preds(!.ModuleInfo, PredTable0),
        map.lookup(PredTable0, PredId, PredInfo0),
        pred_info_get_procedures(PredInfo0, ProcTable0),
        map.to_assoc_list(ProcTable0, ProcList),
        (
            get_procedure_matching_declmodes_with_renaming(ProcList,
                ModeList, !.ModuleInfo, ProcId)
        ->
            add_context_to_arg_size_info(MaybePragmaArgSizeInfo,
                Context, MaybeArgSizeInfo),
            add_context_to_termination_info(MaybePragmaTerminationInfo,
                Context, MaybeTerminationInfo),
            map.lookup(ProcTable0, ProcId, ProcInfo0),
            proc_info_set_maybe_arg_size_info(MaybeArgSizeInfo,
                ProcInfo0, ProcInfo1),
            proc_info_set_maybe_termination_info(MaybeTerminationInfo,
                ProcInfo1, ProcInfo),
            map.det_update(ProcId, ProcInfo, ProcTable0, ProcTable),
            pred_info_set_procedures(ProcTable, PredInfo0, PredInfo),
            map.det_update(PredId, PredInfo, PredTable0, PredTable),
            module_info_set_preds(PredTable, !ModuleInfo)
        ;
            module_info_incr_errors(!ModuleInfo),
            Pieces = [words("Error: `:- pragma termination_info'"),
                words("declaration for undeclared mode of"),
                simple_call(simple_call_id(PredOrFunc, SymName, Arity)),
                suffix("."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
                [Msg]),
            !:Specs = [Spec | !.Specs]
        )
    ;
        PredIds = [_, _ | _],
        Pieces = [words("Error: ambiguous predicate name"),
            simple_call(simple_call_id(PredOrFunc, SymName, Arity)),
            words("in"), quote("pragma termination_info"), suffix("."), nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ).

%-----------------------------------------------------------------------------%

:- pred add_pragma_foreign_proc(pragma_info_foreign_proc::in,
    import_status::in, prog_context::in, maybe(int)::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_foreign_proc(FPInfo, Status, Context, MaybeItemNumber,
        !ModuleInfo, !Specs) :-
    FPInfo = pragma_info_foreign_proc(Attributes0, PredName, PredOrFunc,
        PVars, ProgVarSet, _InstVarset, PragmaImpl),

    % Begin by replacing any maybe_thread_safe foreign_proc attributes
    % with the actual thread safety attributes which we get from the
    % `--maybe-thread-safe' option.

    module_info_get_globals(!.ModuleInfo, Globals),
    globals.get_maybe_thread_safe(Globals, MaybeThreadSafe),
    ThreadSafe = get_thread_safe(Attributes0),
    (
        ThreadSafe = proc_maybe_thread_safe,
        (
            MaybeThreadSafe = yes,
            set_thread_safe(proc_thread_safe, Attributes0, Attributes)
        ;
            MaybeThreadSafe = no,
            set_thread_safe(proc_not_thread_safe, Attributes0, Attributes)
        )
    ;
        ( ThreadSafe = proc_thread_safe
        ; ThreadSafe = proc_not_thread_safe
        ),
        Attributes = Attributes0
    ),
    module_info_get_name(!.ModuleInfo, ModuleName),
    PragmaForeignLanguage = get_foreign_language(Attributes),
    list.length(PVars, Arity),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    (
        VeryVerbose = yes,
        trace [io(!IO)] (
            io.write_string("% Processing `:- pragma foreign_proc' for ", !IO),
            write_simple_call_id(PredOrFunc, PredName/Arity, !IO),
            io.write_string("...\n", !IO)
        )
    ;
        VeryVerbose = no
    ),

    globals.get_backend_foreign_languages(Globals, BackendForeignLangs),

    % Lookup the pred declaration in the predicate table.
    % If it is not there, print an error message and insert
    % a dummy declaration for the predicate.
    module_info_get_predicate_table(!.ModuleInfo, PredTable0),
    predicate_table_lookup_pf_sym_arity(PredTable0, is_fully_qualified,
        PredOrFunc, PredName, Arity, PredIds),
    (
        PredIds = [],
        preds_add_implicit_report_error(!ModuleInfo, ModuleName,
            PredName, Arity, PredOrFunc, Status, no, Context,
            origin_user(PredName),
            [quote(":- pragma foreign_proc"), words("declaration")],
            PredId, !Specs)
    ;
        PredIds = [PredId]
    ;
        PredIds = [PredId, _ | _],
        % Any attempt to define more than one pred with the same PredOrFunc,
        % PredName and Arity should have been caught earlier, and an error
        % message generated. We continue so that we can try to find more
        % errors.
        AmbiPieces = [words("Error: ambiguous predicate name"),
            simple_call(simple_call_id(PredOrFunc, PredName, Arity)),
            words("in"), quote("pragma foreign_proc"), suffix("."), nl],
        AmbiMsg = simple_msg(Context, [always(AmbiPieces)]),
        AmbiSpec = error_spec(severity_error, phase_parse_tree_to_hlds,
            [AmbiMsg]),
        !:Specs = [AmbiSpec | !.Specs]
    ),

    % Lookup the pred_info for this pred, add the pragma to the proc_info
    % in the proc_table in the pred_info, and save the pred_info.
    module_info_get_predicate_table(!.ModuleInfo, PredTable1),
    predicate_table_get_preds(PredTable1, Preds0),
    some [!PredInfo] (
        map.lookup(Preds0, PredId, !:PredInfo),

        % status_opt_imported preds are initially tagged as status_imported
        % and are tagged as status_opt_imported only if/when we see a clause
        % (including a `foreign_proc' clause) for them.
        ( Status = status_opt_imported ->
            pred_info_set_import_status(status_opt_imported, !PredInfo)
        ;
            true
        ),

        % Record the existence of this "clause".
        pred_info_get_clauses_info(!.PredInfo, ClausesInfo0),
        ItemNumbers0 = ClausesInfo0 ^ cli_item_numbers,
        add_clause_item_number(MaybeItemNumber, Context, item_is_foreign_proc,
            ItemNumbers0, ItemNumbers),
        ClausesInfo1 = ClausesInfo0 ^ cli_item_numbers := ItemNumbers,
        pred_info_set_clauses_info(ClausesInfo1, !PredInfo),
        module_info_set_pred_info(PredId, !.PredInfo, !ModuleInfo),

        PredInfo0 = !.PredInfo,

        CurrentBackend = lookup_current_backend(Globals),
        ExtraAttrs = get_extra_attributes(Attributes),
        (
            is_applicable_for_current_backend(CurrentBackend, ExtraAttrs) = no
        ->
            % Ignore this foreign_proc.
            true
        ;
            pred_info_is_imported(!.PredInfo)
        ->
            Pieces = [words("Error:"), quote(":- pragma foreign_proc"),
                words("declaration for imported"),
                simple_call(simple_call_id(PredOrFunc, PredName, Arity)),
                suffix("."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        ;
            % Don't add clauses for foreign languages other than the ones
            % we can generate code for.
            not list.member(PragmaForeignLanguage, BackendForeignLangs)
        ->
            pred_info_update_goal_type(goal_type_foreign,
                PredInfo0, !:PredInfo),
            module_info_set_pred_info(PredId, !.PredInfo, !ModuleInfo)
        ;
            % Add the pragma declaration to the proc_info for this procedure.
            pred_info_get_procedures(!.PredInfo, Procs),
            map.to_assoc_list(Procs, ExistingProcs),
            pragma_get_modes(PVars, Modes),
            SimpleCallId = simple_call_id(PredOrFunc, PredName, Arity),
            (
                % The inst variables for the foreign_proc declaration
                % and predmode declarations are from different varsets.
                % We cannot just unify the argument modes directly because
                % the representation of the inst variables may be different.
                % Instead we need to allow for a renaming between the
                % inst variables in the argument modes of the foreign_proc
                % and those of the predmode declaration.
                %
                % XXX We should probably also check that each pair in
                % the renaming has the same name.
                get_procedure_matching_declmodes_with_renaming(ExistingProcs,
                    Modes, !.ModuleInfo, ProcId)
            ->
                pred_info_get_arg_types(!.PredInfo, ArgTypes),
                pred_info_get_purity(!.PredInfo, Purity),
                pred_info_get_markers(!.PredInfo, Markers),
                clauses_info_add_pragma_foreign_proc(Purity, Attributes,
                    PredId, ProcId, ProgVarSet, PVars, ArgTypes, PragmaImpl,
                    Context, PredOrFunc, PredName, Arity, Markers,
                    ClausesInfo1, ClausesInfo, !ModuleInfo, !Specs),
                pred_info_set_clauses_info(ClausesInfo, !PredInfo),
                pred_info_update_goal_type(goal_type_foreign, !PredInfo),
                map.det_update(PredId, !.PredInfo, Preds0, Preds),
                predicate_table_set_preds(Preds, PredTable1, PredTable),
                module_info_set_predicate_table(PredTable, !ModuleInfo),
                pragma_get_var_infos(PVars, ArgInfoBox),
                assoc_list.keys(ArgInfoBox, ArgInfo),
                warn_singletons_in_pragma_foreign_proc(!.ModuleInfo,
                    PragmaImpl, PragmaForeignLanguage, ArgInfo, Context,
                    SimpleCallId, PredId, ProcId, !Specs)
            ;
                Pieces = [words("Error:"),
                    quote(":- pragma foreign_proc"), words("declaration"),
                    words("for undeclared mode of"),
                    simple_call(SimpleCallId), suffix("."), nl],
                Msg = simple_msg(Context, [always(Pieces)]),
                Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
                    [Msg]),
                !:Specs = [Spec | !.Specs]
            )
        )
    ).

%-----------------------------------------------------------------------------%

:- pred module_add_pragma_tabled(pragma_info_tabled::in, prog_context::in,
    import_status::in, import_status::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

module_add_pragma_tabled(TabledInfo, Context, !Status, !ModuleInfo,
        !QualInfo, !Specs) :-
    TabledInfo = pragma_info_tabled(EvalMethod, PredNameArityMPF,
        MaybeModes, MaybeAttributes),
    PredNameArityMPF = pred_name_arity_mpf(PredName, Arity, MaybePredOrFunc),
    module_info_get_predicate_table(!.ModuleInfo, PredicateTable0),
    EvalMethodStr = eval_method_to_string(EvalMethod),
    (
        MaybePredOrFunc = yes(PredOrFunc0),
        PredOrFunc = PredOrFunc0,

        % Lookup the pred declaration in the predicate table.
        % If it is not there, print an error message and insert
        % a dummy declaration for the predicate.
        predicate_table_lookup_pf_sym_arity(PredicateTable0,
            is_fully_qualified, PredOrFunc, PredName, Arity, PredIds0),
        (
            PredIds0 = [_ | _],
            PredIds = PredIds0
        ;
            PredIds0 = [],
            module_info_get_name(!.ModuleInfo, ModuleName),
            DescPieces = [quote(":- pragma " ++ EvalMethodStr),
                words("declaration")],
            preds_add_implicit_report_error(!ModuleInfo, ModuleName,
                PredName, Arity, PredOrFunc, !.Status, no, Context,
                origin_user(PredName), DescPieces, PredId, !Specs),
            PredIds = [PredId]
        )
    ;
        MaybePredOrFunc = no,
        predicate_table_lookup_sym_arity(PredicateTable0,
            is_fully_qualified, PredName, Arity, PredIds0),
        (
            PredIds0 = [_ | _],
            PredIds = PredIds0
        ;
            PredIds0 = [],
            module_info_get_name(!.ModuleInfo, ModuleName),
            DescPieces = [quote(":- pragma " ++ EvalMethodStr),
                words("declaration")],
            preds_add_implicit_report_error(!ModuleInfo, ModuleName,
                PredName, Arity, pf_predicate, !.Status, no, Context,
                origin_user(PredName), DescPieces, PredId, !Specs),
            PredIds = [PredId]
        )
    ),
    (
        MaybeAttributes = yes(Attributes),
        Statistics = Attributes ^ table_attr_statistics,
        AllowReset = Attributes ^ table_attr_allow_reset,
        (
            PredIds = [_]
        ;
            PredIds = [_, _ | _],
            (
                Statistics = table_gather_statistics,
                StatsPieces = [words("Error: cannot request statistics"),
                    words("for the ambiguous name"),
                    sym_name_and_arity(PredName / Arity), suffix(","),
                    words("since the compiler-generated statistics predicate"),
                    words("would have an ambiguous name too."), nl],
                StatsMsg = simple_msg(Context, [always(StatsPieces)]),
                StatsSpec = error_spec(severity_error,
                    phase_parse_tree_to_hlds, [StatsMsg]),
                !:Specs = [StatsSpec | !.Specs]
            ;
                Statistics = table_dont_gather_statistics
            ),
            (
                AllowReset = table_allow_reset,
                ResetPieces = [words("Error: cannot request allow_reset"),
                    words("for the ambiguous name"),
                    sym_name_and_arity(PredName / Arity), suffix(","),
                    words("since the compiler-generated reset predicate"),
                    words("would have an ambiguous name too."), nl],
                ResetMsg = simple_msg(Context, [always(ResetPieces)]),
                ResetSpec = error_spec(severity_error,
                    phase_parse_tree_to_hlds, [ResetMsg]),
                !:Specs = [ResetSpec | !.Specs]
            ;
                AllowReset = table_dont_allow_reset
            )
        )
    ;
        MaybeAttributes = no
    ),
    list.foldl4(
        module_add_pragma_tabled_2(EvalMethod, PredName, Arity,
            MaybePredOrFunc, MaybeModes, MaybeAttributes, Context),
        PredIds, !Status, !ModuleInfo, !QualInfo, !Specs).

:- pred module_add_pragma_tabled_2(eval_method::in, sym_name::in, int::in,
    maybe(pred_or_func)::in, maybe(list(mer_mode))::in,
    maybe(table_attributes)::in, prog_context::in, pred_id::in,
    import_status::in, import_status::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

module_add_pragma_tabled_2(EvalMethod0, PredName, Arity0, MaybePredOrFunc,
        MaybeModes, MaybeAttributes, Context, PredId,
        !Status, !ModuleInfo, !QualInfo, !Specs) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    ( EvalMethod0 = eval_minimal(_) ->
        globals.lookup_bool_option(Globals, use_minimal_model_own_stacks,
            OwnStacks),
        (
            OwnStacks = yes,
            EvalMethod = eval_minimal(own_stacks_consumer)
        ;
            OwnStacks = no,
            EvalMethod = eval_minimal(stack_copy)
        )
    ;
        EvalMethod = EvalMethod0
    ),

    module_info_get_predicate_table(!.ModuleInfo, PredicateTable),
    predicate_table_get_preds(PredicateTable, Preds),
    map.lookup(Preds, PredId, PredInfo0),
    (
        MaybePredOrFunc = yes(PredOrFunc0),
        PredOrFunc = PredOrFunc0
    ;
        MaybePredOrFunc = no,
        PredOrFunc = pred_info_is_pred_or_func(PredInfo0)
    ),
    adjust_func_arity(PredOrFunc, Arity0, Arity),

    EvalMethodStr = eval_method_to_string(EvalMethod),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    (
        VeryVerbose = yes,
        trace [io(!IO)] (
            io.write_string("% Processing `:- pragma ", !IO),
            io.write_string(EvalMethodStr, !IO),
            io.write_string("' for ", !IO),
            write_simple_call_id(PredOrFunc, PredName/Arity, !IO),
            io.write_string("...\n", !IO)
        )
    ;
        VeryVerbose = no
    ),

    % Issue a warning if this predicate/function has a pragma inline
    % declaration. Tabled procedures cannot be inlined.
    pred_info_get_markers(PredInfo0, Markers),
    globals.lookup_bool_option(Globals, warn_table_with_inline, WarnInline),
    SimpleCallId = simple_call_id(PredOrFunc, PredName, Arity),
    (
        check_marker(Markers, marker_user_marked_inline),
        WarnInline = yes
    ->
        InlineWarningPieces = [words("Warning: "), simple_call(SimpleCallId),
            words("has a"), quote(":- pragma " ++ EvalMethodStr),
            words("declaration but also has a"),
            quote(":- pragma inline"), words("declaration."), nl,
            words("This inline pragma will be ignored"),
            words("since tabled predicates cannot be inlined."), nl,
            words("You can use the"), quote("--no-warn-table-with-inline"),
            words("option to suppress this warning."), nl],
        InlineWarningMsg = simple_msg(Context, [always(InlineWarningPieces)]),
        InlineWarningSpec = error_spec(severity_warning,
            phase_parse_tree_to_hlds, [InlineWarningMsg]),
        !:Specs = [InlineWarningSpec | !.Specs]
    ;
        true
    ),
    ( pred_info_is_imported(PredInfo0) ->
        Pieces = [words("Error: "), quote(":- pragma " ++ EvalMethodStr),
            words("declaration for imported"), simple_call(SimpleCallId),
            suffix("."), nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ;
        % Do we have to make sure the tabled preds are stratified?
        ( eval_method_needs_stratification(EvalMethod) = yes ->
            module_info_get_stratified_preds(!.ModuleInfo, StratPredIds0),
            set.insert(PredId, StratPredIds0, StratPredIds),
            module_info_set_stratified_preds(StratPredIds, !ModuleInfo)
        ;
            true
        ),

        % Add the eval model to the proc_info for this procedure.
        pred_info_get_procedures(PredInfo0, ProcTable0),
        map.to_assoc_list(ProcTable0, ExistingProcs),
        (
            MaybeModes = yes(Modes),
            (
                get_procedure_matching_argmodes(ExistingProcs, Modes,
                    !.ModuleInfo, ProcId)
            ->
                map.lookup(ProcTable0, ProcId, ProcInfo0),
                set_eval_method_create_aux_preds(ProcId, ProcInfo0, Context,
                    SimpleCallId, yes, EvalMethod, MaybeAttributes,
                    ProcTable0, ProcTable, !Status, !ModuleInfo, !QualInfo,
                    !Specs),
                pred_info_set_procedures(ProcTable, PredInfo0, PredInfo),
                module_info_set_pred_info(PredId, PredInfo, !ModuleInfo)
            ;
                Pieces = [words("Error:"),
                    quote(":- pragma " ++ EvalMethodStr),
                    words("declaration for undeclared mode of"),
                    simple_call(SimpleCallId), suffix("."), nl],
                Msg = simple_msg(Context, [always(Pieces)]),
                Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
                    [Msg]),
                !:Specs = [Spec | !.Specs]
            )
        ;
            MaybeModes = no,
            (
                ExistingProcs = [],
                Pieces = [words("Error: "),
                    quote(":- pragma " ++ EvalMethodStr),
                    words("declaration for"), simple_call(SimpleCallId),
                    words("with no declared modes."), nl],
                Msg = simple_msg(Context, [always(Pieces)]),
                Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
                    [Msg]),
                !:Specs = [Spec | !.Specs]
            ;
                ExistingProcs = [_ | ExistingProcsTail],
                (
                    ExistingProcsTail = [],
                    SingleProc = yes
                ;
                    ExistingProcsTail = [_ | _],
                    SingleProc = no
                ),
                set_eval_method_create_aux_preds_list(ExistingProcs, Context,
                    SimpleCallId, SingleProc, EvalMethod, MaybeAttributes,
                    ProcTable0, ProcTable, !Status, !ModuleInfo, !QualInfo,
                    !Specs),
                pred_info_set_procedures(ProcTable, PredInfo0, PredInfo),
                module_info_set_pred_info(PredId, PredInfo, !ModuleInfo)
            )
        )
    ).

:- pred set_eval_method_create_aux_preds_list(
    assoc_list(proc_id, proc_info)::in, prog_context::in, simple_call_id::in,
    bool::in, eval_method::in, maybe(table_attributes)::in,
    proc_table::in, proc_table::out, import_status::in, import_status::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

set_eval_method_create_aux_preds_list([], _, _, _, _, _, !ProcTable,
        !Status, !ModuleInfo, !QualInfo, !Specs).
set_eval_method_create_aux_preds_list([ProcId - ProcInfo0 | Rest], Context,
        SimpleCallId, SingleProc, EvalMethod, MaybeAttributes, !ProcTable,
        !Status, !ModuleInfo, !QualInfo, !Specs) :-
    set_eval_method_create_aux_preds(ProcId, ProcInfo0, Context, SimpleCallId,
        SingleProc, EvalMethod, MaybeAttributes, !ProcTable,
        !Status, !ModuleInfo, !QualInfo, !Specs),
    set_eval_method_create_aux_preds_list(Rest, Context, SimpleCallId,
        SingleProc, EvalMethod, MaybeAttributes, !ProcTable,
        !Status, !ModuleInfo, !QualInfo, !Specs).

:- pred set_eval_method_create_aux_preds(proc_id::in, proc_info::in,
    prog_context::in, simple_call_id::in, bool::in, eval_method::in,
    maybe(table_attributes)::in, proc_table::in, proc_table::out,
    import_status::in, import_status::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

set_eval_method_create_aux_preds(ProcId, ProcInfo0, Context, SimpleCallId,
        SingleProc, EvalMethod, MaybeAttributes, !ProcTable,
        !Status, !ModuleInfo, !QualInfo, !Specs) :-
    proc_info_get_eval_method(ProcInfo0, OldEvalMethod),
    % NOTE: We don't bother detecting multiple tabling pragmas
    % of the same type here.
    ( OldEvalMethod \= eval_normal ->
        % We get here only if we have already processed a tabling pragma for
        % this procedure.
        EvalMethodStr = eval_method_to_string(EvalMethod),
        ( OldEvalMethod = EvalMethod ->
            Pieces = [words("Error:"), simple_call(SimpleCallId),
                words("has duplicate"), fixed(EvalMethodStr),
                words("pragmas specified."), nl]
        ;
            OldEvalMethodStr = eval_method_to_string(OldEvalMethod),
            Pieces = [words("Error:"), simple_call(SimpleCallId),
                words("has both"), fixed(OldEvalMethodStr), words("and"),
                fixed(EvalMethodStr), words("pragmas specified."),
                words("Only one kind of tabling pragma may be applied to it."),
                nl]
        ),
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ;
        proc_info_get_maybe_declared_argmodes(ProcInfo0,
            MaybeDeclaredArgModes),
        (
            MaybeDeclaredArgModes = no,
            EvalMethodStr = eval_method_to_string(EvalMethod),
            Pieces = [words("Error:"), quote("pragma " ++ EvalMethodStr),
                words("declaration for"), simple_call(SimpleCallId),
                suffix(","), words("which has no declared modes."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        ;
            MaybeDeclaredArgModes = yes(DeclaredArgModes),
            (
                MaybeAttributes = yes(Attributes),
                Strictness = Attributes ^ table_attr_strictness,
                Statistics = Attributes ^ table_attr_statistics,
                AllowReset = Attributes ^ table_attr_allow_reset
            ;
                MaybeAttributes = no,
                Strictness = all_strict,
                Statistics = table_dont_gather_statistics,
                AllowReset = table_dont_allow_reset
            ),
            (
                Strictness = specified(MaybeArgMethods, _HiddenArgMethod),
                check_pred_args_against_tabling_methods(DeclaredArgModes,
                    MaybeArgMethods, !.ModuleInfo, 1, MaybeError)
            ;
                ( Strictness = all_strict
                ; Strictness = all_fast_loose
                ),
                check_pred_args_against_tabling(DeclaredArgModes, !.ModuleInfo,
                    1, MaybeError)
            ),
            (
                MaybeError = yes(ArgMsg - ErrorMsg),
                EvalMethodStr = eval_method_to_string(EvalMethod),
                Pieces = [words("Error in"),
                    quote("pragma " ++ EvalMethodStr),
                    words("declaration for"), simple_call(SimpleCallId),
                    suffix(":"), nl, fixed(ArgMsg), words(ErrorMsg), nl],
                Msg = simple_msg(Context, [always(Pieces)]),
                Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
                    [Msg]),
                !:Specs = [Spec | !.Specs]
            ;
                MaybeError = no
            ),
            proc_info_set_eval_method(EvalMethod, ProcInfo0, ProcInfo1),
            proc_info_set_table_attributes(MaybeAttributes,
                ProcInfo1, ProcInfo),
            map.det_update(ProcId, ProcInfo, !ProcTable),
            % We create the statistics and reset predicates if requested
            % even in the presence of errors above, because if didn't do so,
            % later compiler passes would report errors at the sites where
            % these predicates are called.
            % XXX Currently, we only create the statistics predicates in C
            % grades; references to them in non-C grades will cause errors.
            module_info_get_globals(!.ModuleInfo, Globals),
            get_backend_foreign_languages(Globals, ForeignLanguages),
            ( list.member(lang_c, ForeignLanguages) ->
                (
                    Statistics = table_gather_statistics,
                    create_tabling_statistics_pred(ProcId, Context,
                        SimpleCallId, SingleProc, !ProcTable,
                        !Status, !ModuleInfo, !QualInfo, !Specs)
                ;
                    Statistics = table_dont_gather_statistics
                )
            ;
                true
            ),
            (
                AllowReset = table_allow_reset,
                create_tabling_reset_pred(ProcId, Context,
                    SimpleCallId, SingleProc, !ProcTable,
                    !Status, !ModuleInfo, !QualInfo, !Specs)
            ;
                AllowReset = table_dont_allow_reset
            )
        )
    ).

:- pred create_tabling_statistics_pred(proc_id::in, prog_context::in,
    simple_call_id::in, bool::in, proc_table::in, proc_table::out,
    import_status::in, import_status::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

create_tabling_statistics_pred(ProcId, Context, SimpleCallId, SingleProc,
        !ProcTable, !Status, !ModuleInfo, !QualInfo, !Specs) :-
    TableBuiltinModule = mercury_table_statistics_module,
    StatsTypeName = qualified(TableBuiltinModule, "proc_table_statistics"),
    StatsType = defined_type(StatsTypeName, [], kind_star),
    ArgDecl1 = type_and_mode(StatsType, out_mode),
    ArgDecl2 = type_and_mode(io_state_type, di_mode),
    ArgDecl3 = type_and_mode(io_state_type, uo_mode),
    ArgDecls = [ArgDecl1, ArgDecl2, ArgDecl3],

    StatsPredSymName = tabling_stats_pred_name(SimpleCallId, ProcId,
        SingleProc),
    varset.init(VarSet0),
    varset.init(InstVarSet),
    ExistQVars = [],
    Constraints = constraints([], []),
    WithType = no,
    WithInst = no,
    Condition = cond_true,
    Origin = compiler(pragma_memo_attribute),
    StatsPredDecl = item_pred_decl_info(Origin, VarSet0, InstVarSet,
        ExistQVars, pf_predicate, StatsPredSymName, ArgDecls,
        WithType, WithInst, yes(detism_det), Condition, purity_pure,
        Constraints, Context, -1),
    StatsPredDeclItem = item_pred_decl(StatsPredDecl),
    ItemStatus0 = item_status(!.Status, may_be_unqualified),
    add_item_decl_pass_1(StatsPredDeclItem, _, ItemStatus0, _,
        !ModuleInfo, !Specs),

    some [!Attrs, !VarSet] (
        !:Attrs = default_attributes(lang_c),
        % It is easier to construct a complex Mercury structure if we are
        % allowed to use Mercury code to build it out of simple components
        % of primitive types.
        set_may_call_mercury(proc_may_call_mercury, !Attrs),
        set_thread_safe(proc_thread_safe, !Attrs),
        set_purity(purity_pure, !Attrs),
        set_may_duplicate(yes(proc_may_not_duplicate), !Attrs),
        varset.init(!:VarSet),
        varset.new_named_var("Stats", Stats, !VarSet),
        varset.new_named_var("IO0", IO0, !VarSet),
        varset.new_named_var("IO", IO, !VarSet),
        Arg1 = pragma_var(Stats, "Stats", out_mode, always_boxed),
        Arg2 = pragma_var(IO0, "_IO0", di_mode, always_boxed),
        Arg3 = pragma_var(IO, "_IO", uo_mode, always_boxed),

        Global = table_info_c_global_var_name(!.ModuleInfo, SimpleCallId,
            ProcId),
        StatsCode = "MR_get_tabling_stats(&" ++ Global ++ ", &Stats);",
        StatsImpl = fp_impl_ordinary(StatsCode, yes(Context)),
        StatsPragmaFCInfo = pragma_info_foreign_proc(!.Attrs, StatsPredSymName,
            pf_predicate, [Arg1, Arg2, Arg3], !.VarSet, InstVarSet, StatsImpl),
        StatsPragma = pragma_foreign_proc(StatsPragmaFCInfo),
        StatsPragmaInfo = item_pragma_info(compiler(pragma_memo_attribute),
            StatsPragma, Context, -1),
        StatsImplItem = item_pragma(StatsPragmaInfo)
    ),
    % XXX Instead of calling add_item_pass_3, we should call what *it* calls.
    add_item_pass_3(StatsImplItem, !Status, !ModuleInfo, !QualInfo, !Specs).

:- pred create_tabling_reset_pred(proc_id::in, prog_context::in,
    simple_call_id::in, bool::in, proc_table::in, proc_table::out,
    import_status::in, import_status::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

create_tabling_reset_pred(ProcId, Context, SimpleCallId, SingleProc,
         !ProcTable, !Status, !ModuleInfo, !QualInfo, !Specs) :-
    ArgDecl1 = type_and_mode(io_state_type, di_mode),
    ArgDecl2 = type_and_mode(io_state_type, uo_mode),
    ArgDecls = [ArgDecl1, ArgDecl2],

    ResetPredSymName = tabling_reset_pred_name(SimpleCallId, ProcId,
        SingleProc),
    varset.init(VarSet0),
    varset.init(InstVarSet),
    ExistQVars = [],
    Constraints = constraints([], []),
    WithType = no,
    WithInst = no,
    Condition = cond_true,
    Origin = compiler(pragma_memo_attribute),
    ResetPredDecl = item_pred_decl_info(Origin, VarSet0, InstVarSet,
        ExistQVars, pf_predicate, ResetPredSymName, ArgDecls,
        WithType, WithInst, yes(detism_det), Condition, purity_pure,
        Constraints, Context, -1),
    ResetPredDeclItem = item_pred_decl(ResetPredDecl),
    ItemStatus0 = item_status(!.Status, may_be_unqualified),
    add_item_decl_pass_1(ResetPredDeclItem, _, ItemStatus0, _,
        !ModuleInfo, !Specs),

    some [!Attrs, !VarSet] (
        module_info_get_globals(!.ModuleInfo, Globals),
        get_target(Globals, TargetLang),
        (
            ( TargetLang = target_c
            ; TargetLang = target_x86_64
            ),
            ForeignLang = lang_c
        ;
            TargetLang = target_il,
            ForeignLang = lang_csharp
        ;
            TargetLang = target_csharp,
            ForeignLang = lang_csharp
        ;
            TargetLang = target_java,
            ForeignLang = lang_java
        ;
            TargetLang = target_erlang,
            ForeignLang = lang_erlang
        ),
        !:Attrs = default_attributes(ForeignLang),
        set_may_call_mercury(proc_will_not_call_mercury, !Attrs),
        set_thread_safe(proc_thread_safe, !Attrs),
        set_purity(purity_pure, !Attrs),
        set_may_duplicate(yes(proc_may_not_duplicate), !Attrs),
        varset.init(!:VarSet),
        varset.new_named_var("IO0", IO0, !VarSet),
        varset.new_named_var("IO", IO, !VarSet),
        Arg1 = pragma_var(IO0, "_IO0", di_mode, always_boxed),
        Arg2 = pragma_var(IO, "_IO", uo_mode, always_boxed),

        current_grade_supports_tabling(Globals, IsTablingSupported),
        (
            IsTablingSupported = yes,
            Global = table_info_c_global_var_name(!.ModuleInfo, SimpleCallId,
                ProcId),
            ResetCode = Global ++ ".MR_pt_tablenode.MR_integer = 0;"
        ;
            IsTablingSupported = no,
            ResetCode = ""
        ),
        ResetImpl = fp_impl_ordinary(ResetCode, yes(Context)),
        ResetPragmaFCInfo = pragma_info_foreign_proc(!.Attrs, ResetPredSymName,
            pf_predicate, [Arg1, Arg2], !.VarSet, InstVarSet, ResetImpl),
        ResetPragma = pragma_foreign_proc(ResetPragmaFCInfo),
        ResetPragmaInfo = item_pragma_info(compiler(pragma_memo_attribute),
            ResetPragma, Context, -1),
        ResetImplItem = item_pragma(ResetPragmaInfo)
    ),
    % XXX Instead of calling add_item_pass_3, we should call what *it* calls.
    add_item_pass_3(ResetImplItem, !Status, !ModuleInfo, !QualInfo, !Specs).

:- func tabling_stats_pred_name(simple_call_id, proc_id, bool) = sym_name.

tabling_stats_pred_name(SimpleCallId, ProcId, SingleProc) =
    tabling_pred_name("table_statistics_for", SimpleCallId, ProcId,
        SingleProc).

:- func tabling_reset_pred_name(simple_call_id, proc_id, bool) = sym_name.

tabling_reset_pred_name(SimpleCallId, ProcId, SingleProc) =
    tabling_pred_name("table_reset_for", SimpleCallId, ProcId, SingleProc).

:- func tabling_pred_name(string, simple_call_id, proc_id, bool) = sym_name.

tabling_pred_name(Prefix, SimpleCallId, ProcId, SingleProc) = NewSymName :-
    SimpleCallId = simple_call_id(PorF, SymName, Arity0),
    (
        PorF = pf_predicate,
        Arity = Arity0
    ;
        PorF = pf_function,
        Arity = Arity0 - 1
    ),
    (
        SymName = qualified(ModuleName, Name),
        MaybeModuleName = yes(ModuleName)
    ;
        SymName = unqualified(Name),
        MaybeModuleName = no
    ),
    NewName0 = Prefix ++ "_" ++ Name ++ "_" ++ int_to_string(Arity),
    (
        SingleProc = yes,
        NewName = NewName0
    ;
        SingleProc = no,
        NewName = NewName0 ++ "_" ++ int_to_string(proc_id_to_int(ProcId))
    ),
    (
        MaybeModuleName = yes(ModuleNameAgain),
        NewSymName = qualified(ModuleNameAgain, NewName)
    ;
        MaybeModuleName = no,
        NewSymName = unqualified(NewName)
    ).

:- func table_info_c_global_var_name(module_info, simple_call_id, proc_id)
    = string.

table_info_c_global_var_name(ModuleInfo, SimpleCallId, ProcId) = VarName :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.get_target(Globals, Target),
    expect(unify(Target, target_c), $module, $pred,
        "memo table statistics and reset are supported only for C"),
    globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),
    module_info_get_name(ModuleInfo, ModuleName),
    SimpleCallId = simple_call_id(PredOrFunc, PredSymName, Arity),
    PredName = unqualify_name(PredSymName),
    (
        HighLevelCode = yes,
        MaybeModuleName = no,
        % We set CodeModel and NoReturnValue to dummy values because we cannot
        % do any better right now. The code that outputs the mlds_proc_label
        % of an mlds_tabling_ref should use mlds_std_tabling_proc_label to
        % set these fields to the same values.
        CodeModel = model_det,
        NoReturnValue = no,
        MLDS_PredLabel = mlds_user_pred_label(PredOrFunc, MaybeModuleName,
            PredName, Arity, CodeModel, NoReturnValue),
        MLDS_ProcLabel = mlds_proc_label(MLDS_PredLabel, ProcId),
        VarName = sym_name_mangle(ModuleName) ++ "__" ++
            mlds_tabling_data_name(MLDS_ProcLabel, tabling_info)
    ;
        HighLevelCode = no,
        proc_id_to_int(ProcId, ProcIdInt),
        ProcLabel = ordinary_proc_label(ModuleName, PredOrFunc, ModuleName,
            PredName, Arity, ProcIdInt),
        VarName = proc_tabling_info_var_name(ProcLabel)
    ).

:- func proc_tabling_info_var_name(proc_label) = string.

proc_tabling_info_var_name(ProcLabel) =
    tabling_struct_data_addr_string(ProcLabel, tabling_info).

:- pred check_pred_args_against_tabling_methods(list(mer_mode)::in,
    list(maybe(arg_tabling_method))::in, module_info::in, int::in,
    maybe(pair(string))::out) is det.

check_pred_args_against_tabling_methods([], [], _, _, no).
check_pred_args_against_tabling_methods([], [_ | _], _, _, MaybeError) :-
    MaybeError = yes("too many argument tabling methods specified." - "").
check_pred_args_against_tabling_methods([_ | _], [], _, _, MaybeError) :-
    MaybeError = yes("not enough argument tabling methods specified." - "").
check_pred_args_against_tabling_methods([Mode | Modes],
        [MaybeArgMethod | MaybeArgMethods], ModuleInfo, ArgNum, MaybeError) :-
    % XXX We should check not just the boundedness of the argument, but also
    % whether it has any uniqueness annotation: tabling destroys uniqueness.
    ( mode_is_fully_input(ModuleInfo, Mode) ->
        (
            MaybeArgMethod = yes(_),
            check_pred_args_against_tabling_methods(Modes, MaybeArgMethods,
                ModuleInfo, ArgNum + 1, MaybeError)
        ;
            MaybeArgMethod = no,
            MaybeError = yes(("argument " ++ int_to_string(ArgNum) ++ ":") -
                ("argument tabling method `" ++
                maybe_arg_tabling_method_to_string(MaybeArgMethod) ++
                "' is not compatible with input modes."))
        )
    ; mode_is_fully_output(ModuleInfo, Mode) ->
        (
            MaybeArgMethod = yes(_),
            MaybeError = yes(("argument " ++ int_to_string(ArgNum) ++ ":") -
                ("argument tabling method `" ++
                maybe_arg_tabling_method_to_string(MaybeArgMethod) ++
                "' is not compatible with output modes."))
        ;
            MaybeArgMethod = no,
            check_pred_args_against_tabling_methods(Modes, MaybeArgMethods,
                ModuleInfo, ArgNum + 1, MaybeError)
        )
    ;
        MaybeError = yes(("argument " ++ int_to_string(ArgNum) ++ ":") -
            "is neither input or output.")
    ).

:- pred check_pred_args_against_tabling(list(mer_mode)::in, module_info::in,
    int::in, maybe(pair(string))::out) is det.

check_pred_args_against_tabling([], _, _, no).
check_pred_args_against_tabling([Mode | Modes], ModuleInfo, ArgNum,
        MaybeError) :-
    ( mode_is_fully_input(ModuleInfo, Mode) ->
        check_pred_args_against_tabling(Modes, ModuleInfo, ArgNum + 1,
            MaybeError)
    ; mode_is_fully_output(ModuleInfo, Mode) ->
        check_pred_args_against_tabling(Modes, ModuleInfo, ArgNum + 1,
            MaybeError)
    ;
        MaybeError = yes(("argument " ++ int_to_string(ArgNum)) -
            "is neither input or output.")
    ).

    % Extract the modes from the list of pragma_vars.
    %
:- pred pragma_get_modes(list(pragma_var)::in, list(mer_mode)::out) is det.

pragma_get_modes([], []).
pragma_get_modes([PragmaVar | Vars], [Mode | Modes]) :-
    PragmaVar = pragma_var(_Var, _Name, Mode, _BoxPolicy),
    pragma_get_modes(Vars, Modes).

%-----------------------------------------------------------------------------%

    % Extract the vars from the list of pragma_vars.
    %
:- pred pragma_get_vars(list(pragma_var)::in, list(prog_var)::out) is det.

pragma_get_vars([], []).
pragma_get_vars([PragmaVar | PragmaVars], [Var | Vars]) :-
    PragmaVar = pragma_var(Var, _Name, _Mode, _BoxPolicy),
    pragma_get_vars(PragmaVars, Vars).

    % Extract the names from the list of pragma_vars.
    %
:- pred pragma_get_var_infos(list(pragma_var)::in,
    list(pair(maybe(pair(string, mer_mode)), box_policy))::out) is det.

pragma_get_var_infos([], []).
pragma_get_var_infos([PragmaVar | PragmaVars], [Info | Infos]) :-
    PragmaVar = pragma_var(_Var, Name, Mode, BoxPolicy),
    Info = yes(Name - Mode) - BoxPolicy,
    pragma_get_var_infos(PragmaVars, Infos).

%---------------------------------------------------------------------------%

:- pred add_pragma_oisu(pragma_info_oisu::in, import_status::in,
    prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_oisu(OISUInfo, Status, Context, !ModuleInfo, !Specs) :-
    OISUInfo = pragma_info_oisu(TypeCtor, Creators, Mutators, Destructors),
    some [!OISUSpecs] (
        !:OISUSpecs = [],
        ThisModule = status_defined_in_this_module(Status),
        (
            ThisModule = no
        ;
            ThisModule = yes,
            Exported = status_is_exported_to_non_submodules(Status),
            (
                Exported = yes
            ;
                Exported = no,
                StatusPieces = [quote("pragma oisu"),
                    words("declarations must always be exported."), nl],
                StatusMsg = simple_msg(Context, [always(StatusPieces)]),
                StatusSpec = error_spec(severity_error,
                    phase_parse_tree_to_hlds, [StatusMsg]),
                !:OISUSpecs = [StatusSpec | !.OISUSpecs]
            ),

            module_info_get_type_table(!.ModuleInfo, TypeTable),
            ( search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn) ->
                hlds_data.get_type_defn_status(TypeDefn, TypeStatus),
                ( TypeStatus = status_abstract_exported ->
                    true
                ;
                    TypePieces = [words("The type in a"), quote("pragma oisu"),
                        words("declaration must always be abstract exported."),
                        nl],
                    TypeMsg = simple_msg(Context, [always(TypePieces)]),
                    TypeSpec = error_spec(severity_error,
                        phase_parse_tree_to_hlds, [TypeMsg]),
                    !:OISUSpecs = [TypeSpec | !.OISUSpecs]
                )
            ;
%               TypePieces = [words("The type in this"), quote("pragma oisu"),
%                   words("declaration is undefined."), nl],
%               TypeMsg = simple_msg(Context, [always(TypePieces)]),
%               TypeSpec = error_spec(severity_error, phase_parse_tree_to_hlds,
%                   [TypeMsg]),
%               !:OISUSpecs = [TypeSpec | !.OISUSpecs]

                % Module qualification will already have reported the error.
                % Any message we could generate here would be a duplicate.
                true
            )
        ),

        list.map_foldl2(
            find_unique_pred_for_oisu(!.ModuleInfo, Context, TypeCtor,
                "creator"),
            Creators, CreatorPredIds, 1, _, !OISUSpecs),
        list.map_foldl2(
            find_unique_pred_for_oisu(!.ModuleInfo, Context, TypeCtor,
                "mutator"),
            Mutators, MutatorPredIds, 1, _, !OISUSpecs),
        list.map_foldl2(
            find_unique_pred_for_oisu(!.ModuleInfo, Context, TypeCtor,
                "destructor"),
            Destructors, DestructorPredIds, 1, _, !OISUSpecs),

        (
            !.OISUSpecs = [],
            OISUPreds = oisu_preds(CreatorPredIds, MutatorPredIds,
                DestructorPredIds),
            module_info_get_oisu_map(!.ModuleInfo, OISUMap0),
            ( map.insert(TypeCtor, OISUPreds, OISUMap0, OISUMap) ->
                module_info_set_oisu_map(OISUMap, !ModuleInfo)
            ;
                TypeCtor = type_ctor(TypeName, TypeArity),
                DupPieces = [words("Duplicate"), quote("pragma oisu"),
                    words("declaration for"),
                    sym_name_and_arity(TypeName/TypeArity), suffix("."), nl],
                DupMsg = simple_msg(Context, [always(DupPieces)]),
                DupSpec = error_spec(severity_error, phase_parse_tree_to_hlds,
                    [DupMsg]),
                !:Specs = [DupSpec | !.Specs]
            )
        ;
            !.OISUSpecs = [_ | _],
            !:Specs = !.OISUSpecs ++ !.Specs
        )
    ).

:- pred find_unique_pred_for_oisu(module_info::in, prog_context::in,
    type_ctor::in, string::in, pred_name_arity::in, pred_id::out,
    int::in, int::out, list(error_spec)::in, list(error_spec)::out) is det.

find_unique_pred_for_oisu(ModuleInfo, Context, TypeCtor, Kind,
        PredNameArity, PredId, !SeqNum, !Specs) :-
    module_info_get_predicate_table(ModuleInfo, PredicateTable),
    PredNameArity = pred_name_arity(PredName, PredArity),
    predicate_table_lookup_sym_arity(PredicateTable, is_fully_qualified,
        PredName, PredArity, PredIds),
    (
        PredIds = [],
        predicate_table_lookup_sym(PredicateTable, is_fully_qualified,
            PredName, LooseArityPredIds),
        (
            LooseArityPredIds = [],
            TypeCtor = type_ctor(TypeName, TypeArity),
            Pieces = [words("In the"), nth_fixed(!.SeqNum),
                fixed(Kind), words("predicate specification"),
                words("within the"), quote("pragma oisu"),
                words("declaration for"),
                sym_name_and_arity(TypeName/TypeArity), suffix(":"), nl,
                words("error: predicate"),
                sym_name_and_arity(PredName/PredArity),
                words("is undefined."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg])
        ;
            LooseArityPredIds = [_ | _],
            list.map(lookup_pred_orig_arity(ModuleInfo),
                LooseArityPredIds, ArityPieces),
            list.sort_and_remove_dups(ArityPieces, SortedArityPieces),
            (
                SortedArityPieces = [],
                unexpected($module, $pred, "no arity pieces")
            ;
                SortedArityPieces = [_],
                ExpArities = SortedArityPieces
            ;
                SortedArityPieces = [_, _ | _],
                ExpArities = [words("one of") |
                    component_list_to_pieces(SortedArityPieces)]
            ),
            TypeCtor = type_ctor(TypeName, TypeArity),
            Pieces = [words("In the"), nth_fixed(!.SeqNum),
                fixed(Kind), words("predicate specification"),
                words("within the"), quote("pragma oisu"),
                words("declaration for"),
                sym_name_and_arity(TypeName/TypeArity), suffix(":"), nl,
                words("error: predicate"),
                sym_name_and_arity(PredName/PredArity),
                words("has the wrong arity."),
                words("Actual arity is"), int_fixed(PredArity), suffix(","),
                words("expected arity is")] ++ ExpArities ++ [suffix("."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg])
        ),
        !:Specs = [Spec | !.Specs],
        PredId = invalid_pred_id
    ;
        PredIds = [PredId]
    ;
        PredIds = [_, _ | _],
        TypeCtor = type_ctor(TypeName, TypeArity),
        Pieces = [words("In the"), nth_fixed(!.SeqNum),
            fixed(Kind), words("predicate specification"),
            words("within the"), quote("pragma oisu"),
            words("declaration for"),
            sym_name_and_arity(TypeName/TypeArity), suffix(":"), nl,
            words("error: ambiguous predicate name"),
            sym_name_and_arity(PredName/PredArity), suffix("."), nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs],
        PredId = invalid_pred_id
    ),
    !:SeqNum = !.SeqNum + 1.

:- pred lookup_pred_orig_arity(module_info::in, pred_id::in,
    format_component::out) is det.

lookup_pred_orig_arity(ModuleInfo, PredId, Piece) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    OrigArity = pred_info_orig_arity(PredInfo),
    Piece = int_fixed(OrigArity).

%---------------------------------------------------------------------------%

    % add_pragma_fact_table(FTInfo, Status, Context,
    %   !ModuleInfo, !Info):
    %
    % Add a `pragma fact_table' declaration to the HLDS. This predicate calls
    % the fact table compiler (fact_table_compile_facts) to create a separate
    % `.o' file for the fact_table and then creates separate pieces of
    % `pragma c_code' to access the table in each mode of the fact table
    % predicate.
    %
:- pred add_pragma_fact_table(pragma_info_fact_table::in,
    import_status::in, prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_fact_table(FTInfo, Status, Context, !ModuleInfo, !Specs) :-
    FTInfo = pragma_info_fact_table(PredArity, FileName),
    PredArity = pred_name_arity(Pred, Arity),
    module_info_get_predicate_table(!.ModuleInfo, PredicateTable),
    predicate_table_lookup_sym_arity(PredicateTable, is_fully_qualified,
        Pred, Arity, PredIds),
    (
        PredIds = [],
        undefined_pred_or_func_error(Pred, Arity, Context,
            [quote(":- pragma fact_table"), words("declaration")], !Specs)
    ;
        PredIds = [HeadPredId | TailPredIds],
        (
            TailPredIds = [],      % only one predicate found
            PredId = HeadPredId,
            module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),

            % Compile the fact table into a separate .o file.
            % We should be able to dispense with the impure shenanigans
            % when we replace fact tables with fast code for large
            % disjunctions.
            some [!IO] (
                promise_pure (
                    semipure private_builtin.trace_get_io_state(!:IO),
                    fact_table_compile_facts(Pred, Arity, FileName,
                        PredInfo0, PredInfo, Context, !.ModuleInfo,
                        C_HeaderCode, PrimaryProcId, !IO),
                    impure private_builtin.trace_set_io_state(!.IO)
                )
            ),

            module_info_set_pred_info(PredId, PredInfo, !ModuleInfo),
            pred_info_get_procedures(PredInfo, ProcTable),
            pred_info_get_arg_types(PredInfo, ArgTypes),
            ProcIds = pred_info_procids(PredInfo),
            PredOrFunc = pred_info_is_pred_or_func(PredInfo),
            adjust_func_arity(PredOrFunc, Arity, NumArgs),

            % Create foreign_decls to declare extern variables.
            module_add_foreign_decl(lang_c, foreign_decl_is_local,
                literal(C_HeaderCode), Context, !ModuleInfo),

            module_add_fact_table_file(FileName, !ModuleInfo),

            % Create foreign_procs to access the table in each mode.
            add_fact_table_procedures(ProcIds, PrimaryProcId,
                ProcTable, Pred, PredOrFunc, NumArgs, ArgTypes, Status,
                Context, !ModuleInfo, !Specs)
        ;
            TailPredIds = [_ | _],     % >1 predicate found
            Pieces = [words("In"), quote("pragma fact_table"), words("for"),
                sym_name_and_arity(Pred/Arity), suffix(":"), nl,
                words("error: ambiguous predicate/function name."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        )
    ).

    % Add a `pragma c_code' for each mode of the fact table lookup to the
    % HLDS.
    % `pragma fact_table's are represented in the HLDS by a
    % `pragma c_code' for each mode of the predicate.
    %
:- pred add_fact_table_procedures(list(proc_id)::in, proc_id::in,
    proc_table::in, sym_name::in, pred_or_func::in, arity::in,
    list(mer_type)::in, import_status::in, prog_context::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_fact_table_procedures([],_,_,_,_,_,_,_,_, !ModuleInfo, !Specs).
add_fact_table_procedures([ProcId | ProcIds], PrimaryProcId, ProcTable,
        SymName, PredOrFunc, Arity, ArgTypes, Status, Context,
        !ModuleInfo, !Specs) :-
    add_fact_table_proc(ProcId, PrimaryProcId, ProcTable, SymName,
        PredOrFunc, Arity, ArgTypes, Status, Context,
        !ModuleInfo, !Specs),
    add_fact_table_procedures(ProcIds, PrimaryProcId, ProcTable,
        SymName, PredOrFunc, Arity, ArgTypes, Status, Context,
        !ModuleInfo, !Specs).

:- pred add_fact_table_proc(proc_id::in, proc_id::in, proc_table::in,
    sym_name::in, pred_or_func::in, arity::in, list(mer_type)::in,
    import_status::in, prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_fact_table_proc(ProcId, PrimaryProcId, ProcTable, SymName,
        PredOrFunc, Arity, ArgTypes, Status, Context, !ModuleInfo, !Specs) :-
    map.lookup(ProcTable, ProcId, ProcInfo),
    varset.init(ProgVarSet0),
    varset.new_vars(Arity, Vars, ProgVarSet0, ProgVarSet),
    proc_info_get_argmodes(ProcInfo, Modes),
    proc_info_get_inst_varset(ProcInfo, InstVarSet),
    fact_table_pragma_vars(Vars, Modes, ProgVarSet, PragmaVars),

    % We should be able to dispense with the impure shenanigans
    % when we replace fact tables with fast code for large disjunctions.
    some [!IO] (
        promise_pure (
            semipure private_builtin.trace_get_io_state(!:IO),
            fact_table_generate_c_code(SymName, PragmaVars, ProcId,
                PrimaryProcId, ProcInfo, ArgTypes, !.ModuleInfo,
                C_ProcCode, C_ExtraCode, !IO),
            impure private_builtin.trace_set_io_state(!.IO)
        )
    ),

    Attrs0 = default_attributes(lang_c),
    set_may_call_mercury(proc_will_not_call_mercury, Attrs0, Attrs1),
    set_thread_safe(proc_thread_safe, Attrs1, Attrs2),
    % Fact tables procedures should be considered pure.
    set_purity(purity_pure, Attrs2, Attrs3),
    add_extra_attribute(refers_to_llds_stack, Attrs3, Attrs),
    MaybeItemNumber = no,
    FCInfo = pragma_info_foreign_proc(Attrs, SymName, PredOrFunc, PragmaVars,
        ProgVarSet, InstVarSet, fp_impl_ordinary(C_ProcCode, no)),
    add_pragma_foreign_proc(FCInfo, Status, Context, MaybeItemNumber,
        !ModuleInfo, !Specs),
    ( C_ExtraCode = "" ->
        true
    ;
        module_add_foreign_body_code(lang_c, literal(C_ExtraCode), Context,
            !ModuleInfo)
    ),

    % The C code for fact tables includes C labels; we cannot inline this code,
    % because if we try, the result may be duplicate labels in the generated
    % code. So we must disable inlining for fact_table procedures.
    add_pred_marker("fact_table", SymName, Arity, Status, Context,
        marker_user_marked_no_inline, [], !ModuleInfo, !Specs).

    % Create a list(pragma_var) that looks like the ones that are created
    % for foreign_proc in prog_io.m.
    % This is required by module_add_pragma_c_code to add the C code for
    % the procedure to the HLDS.
    %
:- pred fact_table_pragma_vars(list(prog_var)::in, list(mer_mode)::in,
    prog_varset::in, list(pragma_var)::out) is det.

fact_table_pragma_vars(Vars0, Modes0, VarSet, PragmaVars0) :-
    (
        Vars0 = [Var | VarsTail],
        Modes0 = [Mode | ModesTail]
    ->
        varset.lookup_name(VarSet, Var, Name),
        PragmaVar = pragma_var(Var, Name, Mode, native_if_possible),
        fact_table_pragma_vars(VarsTail, ModesTail, VarSet, PragmaVarsTail),
        PragmaVars0 = [PragmaVar | PragmaVarsTail]
    ;
        PragmaVars0 = []
    ).

    % Add the pragma_foreign_proc goal to the clauses_info for this procedure.
    % To do so, we must also insert unifications between the variables in the
    % pragma foreign_proc declaration and the head vars of the pred. Also
    % return the hlds_goal.
    %
:- pred clauses_info_add_pragma_foreign_proc(purity::in,
    pragma_foreign_proc_attributes::in, pred_id::in, proc_id::in,
    prog_varset::in, list(pragma_var)::in, list(mer_type)::in,
    pragma_foreign_proc_impl::in, prog_context::in,
    pred_or_func::in, sym_name::in, arity::in, pred_markers::in,
    clauses_info::in, clauses_info::out, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

clauses_info_add_pragma_foreign_proc(Purity, Attributes0,
        PredId, ProcId, PVarSet, PVars, OrigArgTypes, PragmaImpl0,
        Context, PredOrFunc, PredName, Arity, Markers,
        !ClausesInfo, !ModuleInfo, !Specs) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    ( pred_info_is_builtin(PredInfo) ->
        % When bootstrapping a change that defines a builtin using
        % normal Mercury code, we need to disable the generation
        % of the error message, and just ignore the definition.
        module_info_get_globals(!.ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, allow_defn_of_builtins,
            AllowDefnOfBuiltin),
        (
            AllowDefnOfBuiltin = no,
            Msg = simple_msg(Context,
                [always([words("Error: foreign_proc for builtin."), nl])]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        ;
            AllowDefnOfBuiltin = yes
        )
    ;
        AllProcIds = pred_info_all_procids(PredInfo),
        clauses_info_do_add_pragma_foreign_proc(Purity, Attributes0,
            PredId, ProcId, AllProcIds, PVarSet, PVars, OrigArgTypes,
            PragmaImpl0, Context, PredOrFunc, PredName, Arity,
            Markers, !ClausesInfo, !ModuleInfo, !Specs)
    ).

:- pred clauses_info_do_add_pragma_foreign_proc(purity::in,
    pragma_foreign_proc_attributes::in, pred_id::in, proc_id::in,
    list(proc_id)::in, prog_varset::in, list(pragma_var)::in,
    list(mer_type)::in, pragma_foreign_proc_impl::in, prog_context::in,
    pred_or_func::in, sym_name::in, arity::in, pred_markers::in,
    clauses_info::in, clauses_info::out, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

clauses_info_do_add_pragma_foreign_proc(Purity, Attributes0,
        PredId, ProcId, AllProcIds, PVarSet, PVars, OrigArgTypes, PragmaImpl0,
        Context, PredOrFunc, PredName, Arity, Markers,
        !ClausesInfo, !ModuleInfo, !Specs) :-
    % Our caller should have already added this foreign_proc to ItemNumbers.
    !.ClausesInfo = clauses_info(VarSet0, ExplicitVarTypes, TVarNameMap,
        InferredVarTypes, HeadVars, ClauseRep, ItemNumbers,
        RttiVarMaps, _HasForeignClauses),

    get_clause_list(ClauseRep, Clauses),

    % Currently we can override Mercury clauses with a foreign_proc right here,
    % which means that semantic analysis never sees those Mercury clauses.
    % Any errors in them thus do get picked not when they first arise, but
    % only when the code gets compiled for a target that requires their use.
    % XXX We should retain and check the Mercury clauses, and override them
    % with a more specific foreign language implementation only after semantic
    % analysis.
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.get_target(Globals, Target),
    NewLang = get_foreign_language(Attributes0),
    add_foreign_proc_update_existing_clauses(PredName, Arity, PredOrFunc,
        Context, Globals, Target, NewLang, AllProcIds, ProcId,
        Clauses, NewClauses0, Overridden, !Specs),

    globals.get_backend_foreign_languages(Globals, BackendForeignLanguages),
    pragma_get_vars(PVars, Args0),
    pragma_get_var_infos(PVars, ArgInfo),

    % If the foreign language not one of the backend languages, we will
    % have to generate an interface to it in a backend language.
    foreign.extrude_pragma_implementation(BackendForeignLanguages,
        PVars, PredName, PredOrFunc, Context, !ModuleInfo,
        Attributes0, Attributes1, PragmaImpl0, PragmaImpl),

    % Check for arguments occurring more than once.
    bag.init(ArgBag0),
    bag.insert_list(Args0, ArgBag0, ArgBag),
    bag.to_assoc_list(ArgBag, ArgBagAL0),
    list.filter(
        (pred(Arg::in) is semidet :-
            Arg = _ - Occurrences,
            Occurrences > 1
        ), ArgBagAL0, ArgBagAL),
    assoc_list.keys(ArgBagAL, MultipleArgs),

    (
        MultipleArgs = [_ | _],
        adjust_func_arity(PredOrFunc, OrigArity, Arity),
        SimpleCallId = simple_call_id(PredOrFunc, PredName, OrigArity),
        Pieces1 = [words("In"), quote(":- pragma foreign_proc"),
            words("declaration for"), simple_call(SimpleCallId),
            suffix(":"), nl],
        (
            MultipleArgs = [MultipleArg],
            Pieces2 = [words("error: variable"),
                quote(mercury_var_to_string(PVarSet, no, MultipleArg)),
                words("occurs multiple times in the argument list."), nl]
        ;
            MultipleArgs = [_, _ | _],
            Pieces2 = [words("error: variables"),
                quote(mercury_vars_to_string(PVarSet, no, MultipleArgs)),
                words("occur multiple times in the argument list."), nl]
        ),
        Msg = simple_msg(Context, [always(Pieces1 ++ Pieces2)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ;
        MultipleArgs = [],
        % Build the foreign_proc.
        goal_info_init(GoalInfo0),
        goal_info_set_context(Context, GoalInfo0, GoalInfo1),

        % Check that the purity of a predicate/function declaration agrees
        % with the (promised) purity of the foreign proc. We do not perform
        % this check if there is a promise_{pure,semipure} pragma for the
        % predicate/function, since in that case they will differ anyway.
        (
            ( check_marker(Markers, marker_promised_pure)
            ; check_marker(Markers, marker_promised_semipure)
            )
        ->
            true
        ;
            ForeignAttributePurity = get_purity(Attributes1),
            ( ForeignAttributePurity = Purity ->
                true
            ;
                purity_name(ForeignAttributePurity, ForeignAttributePurityStr),
                purity_name(Purity, PurityStr),
                Pieces = [words("Error: foreign clause for"),
                    p_or_f(PredOrFunc), sym_name_and_arity(PredName / Arity),
                    words("has purity"), words(ForeignAttributePurityStr),
                    words("but that"), p_or_f(PredOrFunc),
                    words("has been declared"), words(PurityStr),
                    suffix("."), nl],
                Msg = simple_msg(Context, [always(Pieces)]),
                Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
                    [Msg]),
                !:Specs = [Spec | !.Specs]
            )
        ),
        (
            Overridden = overridden_by_old_foreign_proc
        ;
            Overridden = not_overridden_by_old_foreign_proc,

            % Put the purity in the goal_info in case this foreign code is
            % inlined.
            goal_info_set_purity(Purity, GoalInfo1, GoalInfo),
            % XXX ARGVEC - the foreign_args field in the hlds_goal_expr type
            % should also be a an proc_arg_vector rather than a list.
            HeadVarList = proc_arg_vector_to_list(HeadVars),
            make_foreign_args(HeadVarList, ArgInfo, OrigArgTypes, ForeignArgs),
            % Perform some renaming in any user annotated sharing information.
            maybe_rename_user_annotated_sharing_information(Globals,
                Args0, HeadVarList, OrigArgTypes, Attributes1, Attributes),
            ExtraArgs = [],
            MaybeTraceRuntimeCond = no,
            GoalExpr = call_foreign_proc(Attributes, PredId, ProcId,
                ForeignArgs, ExtraArgs, MaybeTraceRuntimeCond, PragmaImpl),
            HldsGoal0 = hlds_goal(GoalExpr, GoalInfo),
            init_vartypes(EmptyVarTypes),
            rtti_varmaps_init(EmptyRttiVarmaps),
            implicitly_quantify_clause_body_general(
                ordinary_nonlocals_maybe_lambda, HeadVarList, _Warnings,
                HldsGoal0, HldsGoal, VarSet0, VarSet, EmptyVarTypes, _,
                EmptyRttiVarmaps, _),
            NewClause = clause(selected_modes([ProcId]), HldsGoal,
                impl_lang_foreign(NewLang), Context, []),
            NewClauses = [NewClause | NewClauses0],
            HasForeignClauses = yes,
            set_clause_list(NewClauses, NewClauseRep),
            !:ClausesInfo = clauses_info(VarSet, ExplicitVarTypes, TVarNameMap,
                InferredVarTypes, HeadVars, NewClauseRep, ItemNumbers,
                RttiVarMaps, HasForeignClauses)
        )
    ).

    % Rename any user annotated structure sharing information from the
    % variables (incl. type variables) in terms of which that information
    % is expressed, to the formal variables in terms of which the clause
    % is expressed.
    %
:- pred maybe_rename_user_annotated_sharing_information(globals::in,
    list(prog_var)::in, list(prog_var)::in, list(mer_type)::in,
    pragma_foreign_proc_attributes::in, pragma_foreign_proc_attributes::out)
    is det.

maybe_rename_user_annotated_sharing_information(Globals,
        ActualHeadVars, FormalHeadVars, FormalTypes, !Attributes):-
    globals.lookup_bool_option(Globals, structure_sharing_analysis,
        SharingAnalysis),
    (
        SharingAnalysis = no
    ;
        SharingAnalysis = yes,
        rename_user_annotated_sharing(ActualHeadVars, FormalHeadVars,
            FormalTypes, get_user_annotated_sharing(!.Attributes),
            FormalUserSharing),
        set_user_annotated_sharing(FormalUserSharing, !Attributes)
    ).

:- func is_applicable_for_current_backend(backend,
    list(pragma_foreign_proc_extra_attribute)) = bool.

is_applicable_for_current_backend(_CurrentBackend, []) = yes.
is_applicable_for_current_backend(CurrentBackend, [Attr | Attrs]) = Result :-
    (
        ( Attr = max_stack_size(_)
        ; Attr = refers_to_llds_stack
        ; Attr = needs_call_standard_output_registers
        ),
        Result = is_applicable_for_current_backend(CurrentBackend, Attrs)
    ;
        Attr = backend(Backend),
        ( Backend = CurrentBackend ->
            Result = is_applicable_for_current_backend(CurrentBackend, Attrs)
        ;
            Result = no
        )
    ).

:- type overridden_by_old_foreign_proc
    --->    overridden_by_old_foreign_proc
    ;       not_overridden_by_old_foreign_proc.

:- pred add_foreign_proc_update_existing_clauses(sym_name::in, arity::in,
    pred_or_func::in, prog_context::in, globals::in, compilation_target::in,
    foreign_language::in, list(proc_id)::in, proc_id::in,
    list(clause)::in, list(clause)::out,
    overridden_by_old_foreign_proc::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_foreign_proc_update_existing_clauses(PredName, Arity, PredOrFunc,
        NewContext, Globals, Target, NewLang, AllProcIds, NewClauseProcId,
        Clauses0, Clauses, Overridden, !Specs) :-
    (
        Clauses0 = [],
        Clauses = [],
        Overridden = not_overridden_by_old_foreign_proc
    ;
        Clauses0 = [FirstClause0 | LaterClauses0],
        add_foreign_proc_update_existing_clauses(PredName, Arity, PredOrFunc,
            NewContext, Globals, Target, NewLang, AllProcIds, NewClauseProcId,
            LaterClauses0, LaterClauses, LaterOverridden, !Specs),
        FirstClause0 = clause(ApplProcIds0, Body, ClauseLang, ClauseContext,
            StateVarWarnings),
        (
            ClauseLang = impl_lang_mercury,
            (
                ApplProcIds0 = all_modes,
                ProcIds0 = AllProcIds
            ;
                ApplProcIds0 = selected_modes(ProcIds0)
            ),
            ( list.delete_first(ProcIds0, NewClauseProcId, ProcIds) ->
                (
                    ProcIds = [],
                    % This clause is totally overridden by the new
                    % foreign_proc, so delete it.
                    Clauses = LaterClauses
                ;
                    ProcIds = [_ | _],
                    % This clause is overridden by the new foreign_proc only
                    % in some modes, so mark it as being applicable only in the
                    % remaining modes.
                    FirstClause = clause(selected_modes(ProcIds), Body,
                        ClauseLang, ClauseContext, StateVarWarnings),
                    Clauses = [FirstClause | LaterClauses]
                )
            ;
                % This clause is not applicable to the mode of the new
                % foreign_proc, so leave it alone.
                Clauses = [FirstClause0 | LaterClauses]
            ),
            % A Mercury clause can never take precedence over a foreign_proc.
            Overridden = LaterOverridden
        ;
            ClauseLang = impl_lang_foreign(OldLang),
            (
                ApplProcIds0 = all_modes,
                unexpected($module, $pred, "all_modes")
            ;
                ApplProcIds0 = selected_modes(ProcIds0)
            ),
            ( list.delete_first(ProcIds0, NewClauseProcId, ProcIds) ->
                PreferNewForeignLang = prefer_foreign_language(Globals, Target,
                    OldLang, NewLang),
                (
                    PreferNewForeignLang = yes,
                    (
                        ProcIds = [],
                        % The language of the new foreign_proc is preferred
                        % to the language of the old foreign_proc,
                        % so we should replace the old foreign_proc.
                        Clauses = LaterClauses,
                        Overridden = LaterOverridden
                    ;
                        ProcIds = [_ | _],
                        % The language of the new foreign_proc is preferred
                        % to the language of the old foreign_proc,
                        % but the old foreign_proc is still applicable
                        % in some modes, so we keep it in those modes.
                        %
                        % XXX This should not happen.
                        FirstClause = clause(selected_modes(ProcIds), Body,
                            ClauseLang, ClauseContext, StateVarWarnings),
                        Clauses = [FirstClause | LaterClauses],
                        Overridden = LaterOverridden
                    ),
                    % Any later clause that overrides the new foreign_proc
                    % should have overridden this old foreign_proc as well.
                    expect(
                        unify(LaterOverridden,
                            not_overridden_by_old_foreign_proc),
                        $module, $pred, "inconsistent old foreign_procs")
                ;
                    PreferNewForeignLang = no,
                    % We prefer the old foreign_proc to the new one,
                    % so keep the old one and tell our caller to ignore
                    % the new one.
                    Clauses = [FirstClause0 | LaterClauses],
                    Overridden = overridden_by_old_foreign_proc,

                    % However, if the old and the new foreign_procs are
                    % in the same language, then we emit an error message
                    % as well.
                    % XXX This won't detect multiple clauses in languages
                    % that are not supported by this backend, since we filter
                    % out foreign_procs in such languages way before we get
                    % here.
                    ( OldLang = NewLang ->
                        PiecesA = [words("Error: multiple clauses for"),
                            p_or_f(PredOrFunc),
                            sym_name_and_arity(PredName / Arity),
                            words("in language"),
                            words(foreign_language_string(OldLang)),
                            suffix("."), nl],
                        PiecesB = [words("The first occurrence was here."),
                            nl],
                        MsgA = simple_msg(NewContext, [always(PiecesA)]),
                        MsgB = error_msg(yes(ClauseContext), treat_as_first, 0,
                            [always(PiecesB)]),
                        Spec = error_spec(severity_error,
                            phase_parse_tree_to_hlds, [MsgA, MsgB]),
                        !:Specs = [Spec | !.Specs]
                    ;
                        true
                    )
                )
            ;
                % This old foreign_proc is not overridden by the new one,
                % so leave it alone.
                Clauses = [FirstClause0 | LaterClauses],
                Overridden = LaterOverridden
            )
        )
    ).

%----------------------------------------------------------------------------%
%
% Code for checking required feature set pragmas.
%

:- pred check_required_feature_set(set(required_feature)::in,
    import_status::in, prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_required_feature_set(FeatureSet, ImportStatus, Context, !ModuleInfo,
        !Specs) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    IsImported = status_is_imported(ImportStatus),
    (
        % `require_feature_set' pragmas are not included in interface files
        % (including private interfaces) and so this case should not occur.
        IsImported = yes,
        unexpected($module, $pred, "imported require_feature_set pragma")
    ;
        IsImported = no,
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
            Pieces = [words("Error: this module must be compiled in a grade"),
                words("that supports concurrent execution."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
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
            Pieces = [words("Error: this module must be compiled in a grade"),
                words("that uses single precision floats."), nl],
            VerbosePieces = [words("Grades that use single precision floats"),
                words("contain the grade modifier"),
                quote("spf"), suffix("."), nl],
            Msg = simple_msg(Context,
                [always(Pieces), verbose_only(VerbosePieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
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
            Pieces = [words("Error: this module must be compiled in a grade"),
                words("that uses double precision floats."), nl],
            VerbosePieces = [words("Grades that use double precision floats"),
                words("do not contain the grade modifier"),
                quote("spf"), suffix("."), nl],
            Msg = simple_msg(Context,
                [always(Pieces), verbose_only(VerbosePieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        ;
            SinglePrecFloat = no
        )
    ;
        Feature = reqf_memo,
        current_grade_supports_tabling(Globals, IsTablingSupported),
        (
            IsTablingSupported = no,
            Pieces = [words("Error: this module must be compiled in a grade"),
                words("that supports memoisation."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        ;
            IsTablingSupported = yes
        )
    ;
        Feature = reqf_parallel_conj,
        current_grade_supports_par_conj(Globals, IsParConjSupported),
        (
            IsParConjSupported = no,
            Pieces = [words("Error: this module must be compiled in a grade"),
                words("that supports executing conjuntions in parallel."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        ;
            IsParConjSupported = yes
        )
    ;
        Feature = reqf_trailing,
        globals.lookup_bool_option(Globals, use_trail, UseTrail),
        (
            UseTrail = no,
            Pieces = [words("Error: this module must be compiled in a grade"),
                words("that supports trailing.")],
            VerbosePieces = [words("Grades that support trailing contain"),
                words("the grade modifiers"), quote("tr"),
                words("or"), quote("trseg"), suffix("."), nl],
            Msg = simple_msg(Context,
                [always(Pieces), verbose_only(VerbosePieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        ;
            UseTrail = yes
        )
    ;
        Feature = reqf_strict_sequential,
        globals.lookup_bool_option(Globals, reorder_conj, ReorderConj),
        globals.lookup_bool_option(Globals, reorder_disj, ReorderDisj),
        globals.lookup_bool_option(Globals, fully_strict, FullyStrict),
        (
            ReorderConj = no,
            ReorderDisj = no,
            FullyStrict = yes
        ->
            true
        ;
            Pieces = [words("Error: this module must be compiled using"),
                words("the strict sequential semantics."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        )
    ;
        Feature = reqf_conservative_gc,
        globals.get_gc_method(Globals, GC_Method),
        (
            % We consider gc_automatic to be conservative even it may not be.
            % This is okay because this feature is only of interest with the
            % C or asm backends. We ignore it if the target language is
            % something else.

            ( GC_Method = gc_automatic
            ; GC_Method = gc_boehm
            ; GC_Method = gc_boehm_debug
            ; GC_Method = gc_hgc
            ; GC_Method = gc_mps
            )
        ;
            ( GC_Method = gc_accurate
            ; GC_Method = gc_none
            ),
            Pieces = [words("Error: this module must be compiled in a grade"),
                words("that uses conservative garbage collection."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        )
    ).

%----------------------------------------------------------------------------%
:- end_module hlds.make_hlds.add_pragma.
%----------------------------------------------------------------------------%
