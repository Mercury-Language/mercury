%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2010 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module hlds.make_hlds.add_pragma.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.make_hlds.make_hlds_passes.
:- import_module hlds.make_hlds.qual_info.
:- import_module libs.globals.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.

:- import_module assoc_list.
:- import_module list.
:- import_module term.

%-----------------------------------------------------------------------------%

:- pred add_pragma(item_pragma_info::in,
    item_status::in, item_status::out, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_pragma_foreign_export(item_origin::in, foreign_language::in,
    sym_name::in, pred_or_func::in, list(mer_mode)::in, string::in,
    prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_pragma_reserve_tag(sym_name::in, arity::in, import_status::in,
    prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_pragma_foreign_export_enum(foreign_language::in, sym_name::in,
    arity::in, export_enum_attributes::in, assoc_list(sym_name, string)::in,
    import_status::in, prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_pragma_foreign_enum(foreign_language::in, sym_name::in,
    arity::in, assoc_list(sym_name, string)::in, import_status::in,
    prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_pragma_type_spec(pragma_type::in(pragma_type_spec),
    term.context::in, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_pragma_termination2_info(pred_or_func::in, sym_name::in,
    list(mer_mode)::in, maybe(pragma_constr_arg_size_info)::in,
    maybe(pragma_constr_arg_size_info)::in,
    maybe(pragma_termination_info)::in, prog_context::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_pragma_termination_info(pred_or_func::in, sym_name::in,
    list(mer_mode)::in, maybe(pragma_arg_size_info)::in,
    maybe(pragma_termination_info)::in, prog_context::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_pragma_structure_sharing(pred_or_func::in, sym_name::in,
    list(mer_mode)::in, list(prog_var)::in, list(mer_type)::in,
    maybe(structure_sharing_domain)::in, prog_context::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_pragma_structure_reuse(pred_or_func::in, sym_name::in,
    list(mer_mode)::in, list(prog_var)::in, list(mer_type)::in,
    maybe(structure_reuse_domain)::in, prog_context::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % module_add_pragma_import:
    %
    % Handles `pragma import' declarations, by figuring out which predicate
    % the `pragma import' declaration applies to, and adding a clause
    % for that predicate containing an appropriate HLDS `foreign_proc' goal.
    %
    % NB. Any changes here might also require similar changes to the
    % handling of `pragma foreign_export' declarations, in export.m.
    %
:- pred module_add_pragma_import(sym_name::in, pred_or_func::in,
    list(mer_mode)::in, pragma_foreign_proc_attributes::in, string::in,
    import_status::in, prog_context::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred module_add_pragma_foreign_proc(pragma_foreign_proc_attributes::in,
    sym_name::in, pred_or_func::in, list(pragma_var)::in, prog_varset::in,
    inst_varset::in, pragma_foreign_code_impl::in, import_status::in,
    prog_context::in, maybe(int)::in, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred module_add_pragma_tabled(eval_method::in, sym_name::in, int::in,
    maybe(pred_or_func)::in, maybe(list(mer_mode))::in,
    maybe(table_attributes)::in, prog_context::in,
    import_status::in, import_status::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % module_add_pragma_fact_table(PredName, Arity, FileName,
    %   Status, Context, Module0, Module, !Info):
    %
    % Add a `pragma fact_table' declaration to the HLDS. This predicate calls
    % the fact table compiler (fact_table_compile_facts) to create a separate
    % `.o' file for the fact_table and then creates separate pieces of
    % `pragma c_code' to access the table in each mode of the fact table
    % predicate.
    %
:- pred module_add_pragma_fact_table(sym_name::in, arity::in, string::in,
    import_status::in, prog_context::in, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- func lookup_current_backend(globals) = backend.

    % Find the procedure with declared argmodes which match the ones we want.
    % If there was no mode declaration, then use the inferred argmodes.
    % Allow for a renaming between the inst vars.
    %
:- pred get_procedure_matching_declmodes_with_renaming(
    assoc_list(proc_id, proc_info)::in, list(mer_mode)::in,
    module_info::in, proc_id::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.c_util.
:- import_module backend_libs.foreign.
:- import_module backend_libs.rtti.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.code_model.
:- import_module hlds.hlds_args.
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
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module ll_backend.
:- import_module ll_backend.fact_table.
:- import_module ll_backend.rtti_out.
:- import_module ml_backend.
:- import_module ml_backend.mlds.
:- import_module ml_backend.mlds_to_c.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.modules.
:- import_module parse_tree.prog_ctgc.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_io.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_util.
:- import_module recompilation.
:- import_module transform_hlds.term_constr_main.
:- import_module transform_hlds.term_constr_util.
:- import_module transform_hlds.term_util.

:- import_module bag.
:- import_module bimap.
:- import_module bool.
:- import_module int.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module multi_map.
:- import_module pair.
:- import_module set.
:- import_module string.
:- import_module svmap.
:- import_module svvarset.
:- import_module varset.

%-----------------------------------------------------------------------------%

add_pragma(ItemPragma, !Status, !ModuleInfo, !Specs) :-
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
        % Ignore `pragma source_file' declarations - they're dealt
        % with elsewhere.
        Pragma = pragma_source_file(_)
    ;
        Pragma = pragma_foreign_code(Lang, Body_Code),
        module_add_foreign_body_code(Lang, Body_Code, Context, !ModuleInfo)
    ;
        Pragma  = pragma_foreign_decl(Lang, IsLocal, C_Header),
        module_add_foreign_decl(Lang, IsLocal, C_Header, Context, !ModuleInfo)
    ;
        Pragma  = pragma_foreign_import_module(Lang, Import),
        module_add_foreign_import_module(Lang, Import, Context, !ModuleInfo)
    ;
        % Handle pragma foreign procs later on (when we process clauses).
        Pragma = pragma_foreign_proc(_, _, _, _, _, _, _)
    ;
        % Handle pragma foreign_export_enum (after we have added all the
        % types).
        Pragma = pragma_foreign_export_enum(_, _, _, _, _)
    ;
        % Likewise for pragma foreign_enum.
        Pragma = pragma_foreign_enum(_, _, _, _)
    ;
        % Handle pragma tabled decls later on (when we process clauses).
        Pragma = pragma_tabled(_, _, _, _, _, _)
    ;
        Pragma = pragma_inline(Name, Arity),
        add_pred_marker("inline", Name, Arity, ImportStatus, Context,
            marker_user_marked_inline, [marker_user_marked_no_inline],
            !ModuleInfo, !Specs)
    ;
        Pragma = pragma_no_inline(Name, Arity),
        add_pred_marker("no_inline", Name, Arity, ImportStatus, Context,
            marker_user_marked_no_inline, [marker_user_marked_inline],
            !ModuleInfo, !Specs)
    ;
        Pragma = pragma_obsolete(Name, Arity),
        add_pred_marker("obsolete", Name, Arity, ImportStatus,
            Context, marker_obsolete, [], !ModuleInfo, !Specs)
    ;
        % Handle pragma import decls later on (when we process
        % clauses and pragma c_code).
        Pragma = pragma_import(_, _, _, _, _)
    ;
        % Handle pragma foreign_export decls later on, after default
        % function modes have been added.
        Pragma = pragma_foreign_export(_, _, _, _, _)
    ;
        % Used for inter-module unused argument elimination.
        % This can only appear in .opt files.
        Pragma = pragma_unused_args(PredOrFunc, SymName, Arity, ModeNum,
            UnusedArgs),
        ( ImportStatus \= status_opt_imported ->
            Pieces = [words("Error: illegal use of pragma `unused_args'.")],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        ;
            add_pragma_unused_args(PredOrFunc, SymName, Arity, ModeNum,
                UnusedArgs, Context, !ModuleInfo, !Specs)
        )
    ;
        Pragma = pragma_exceptions(PredOrFunc, SymName, Arity, ModeNum,
            ThrowStatus),
        ( ImportStatus \= status_opt_imported ->
            Pieces = [words("Error: illegal use of pragma `exceptions'.")],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        ;
            add_pragma_exceptions(PredOrFunc, SymName, Arity, ModeNum,
                ThrowStatus, Context, !ModuleInfo, !Specs)
        )
    ;
        Pragma = pragma_trailing_info(PredOrFunc, SymName, Arity, ModeNum,
            TrailingStatus),
        ( ImportStatus \= status_opt_imported ->
            Pieces = [words("Error: illegal use of pragma `trailing_info'.")],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        ;
            add_pragma_trailing_info(PredOrFunc, SymName, Arity, ModeNum,
                TrailingStatus, Context, !ModuleInfo, !Specs)
        )
    ;
        Pragma = pragma_mm_tabling_info(PredOrFunc, SymName, Arity, ModeNum,
            MM_TablingStatus),
        ( ImportStatus \= status_opt_imported ->
            Pieces =
                [words("Error: illegal use of pragma `mm_tabling_info',")],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        ;
            add_pragma_mm_tabling_info(PredOrFunc, SymName, Arity, ModeNum,
                MM_TablingStatus, Context, !ModuleInfo, !Specs)
        )
    ;
        % Handle pragma type_spec decls later on (when we process clauses).
        Pragma = pragma_type_spec(_, _, _, _, _, _, _, _)
    ;
        % Handle pragma fact_table decls later on (when we process clauses
        % -- since these decls take the place of clauses).
        Pragma = pragma_fact_table(_, _, _)
    ;
        % Handle pragma reserve_tag decls later on (when we process clauses
        % -- they need to be handled after the type definitions
        % have been added).
        Pragma = pragma_reserve_tag(_, _)
    ;
        Pragma = pragma_promise_pure(Name, Arity),
        add_pred_marker("promise_pure", Name, Arity, ImportStatus,
            Context, marker_promised_pure, [], !ModuleInfo, !Specs)
    ;
        Pragma = pragma_promise_semipure(Name, Arity),
        add_pred_marker("promise_semipure", Name, Arity, ImportStatus,
            Context, marker_promised_semipure, [], !ModuleInfo, !Specs)
    ;
        Pragma = pragma_promise_equivalent_clauses(Name, Arity),
        add_pred_marker("promise_equivalent_clauses", Name, Arity,
            ImportStatus, Context, marker_promised_equivalent_clauses, [],
            !ModuleInfo, !Specs)
    ;
        % Handle pragma termination_info decls later on, in pass 3 --
        % we need to add function default modes before handling
        % these pragmas
        Pragma = pragma_termination_info(_, _, _, _, _)
    ;
        % As for termination_info pragmas
        Pragma = pragma_termination2_info(_, _, _, _, _, _)
    ;
        Pragma = pragma_terminates(Name, Arity),
        add_pred_marker("terminates", Name, Arity, ImportStatus, Context,
            marker_terminates,
            [marker_check_termination, marker_does_not_terminate],
            !ModuleInfo, !Specs)
    ;
        Pragma = pragma_does_not_terminate(Name, Arity),
        add_pred_marker("does_not_terminate", Name, Arity, ImportStatus,
            Context, marker_does_not_terminate,
            [marker_check_termination, marker_terminates], !ModuleInfo, !Specs)
    ;
        Pragma = pragma_check_termination(Name, Arity),
        add_pred_marker("check_termination", Name, Arity, ImportStatus,
            Context, marker_check_termination,
            [marker_terminates, marker_does_not_terminate],
            !ModuleInfo, !Specs)
    ;
        Pragma = pragma_structure_sharing(_, _, _, _, _, _)
    ;
        Pragma = pragma_structure_reuse(_, _, _, _, _, _)
    ;
        Pragma = pragma_mode_check_clauses(Name, Arity),
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
        Pragma = pragma_require_feature_set(FeatureSet),
        check_required_feature_set(FeatureSet, ImportStatus, Context,
            !ModuleInfo, !Specs)
    ).

add_pragma_foreign_export(Origin, Lang, Name, PredOrFunc, Modes,
        ExportedName, Context, !ModuleInfo, !Specs) :-
    module_info_get_predicate_table(!.ModuleInfo, PredTable),
    list.length(Modes, Arity),
    (
        predicate_table_search_pf_sym_arity(PredTable,
            may_be_partially_qualified, PredOrFunc, Name, Arity, PredIds),
        ( PredIds = [_]
        ; PredIds = [_, _ | _]
        )
    ->
        (
            PredIds = [PredId],
            add_pragma_foreign_export_2(Arity, PredTable, Origin, Lang, Name,
                PredId, Modes, ExportedName, Context, !ModuleInfo, !Specs)
        ;
            PredIds = [_, _ | _],
            StartPieces = [
                words("error: ambiguous"), p_or_f(PredOrFunc),
                words("name in"), quote("pragma foreign_export"),
                words("declaration."), nl,
                words("The possible matches are:"), nl_indent_delta(1)
            ],
            PredIdPiecesList = list.map(
                describe_one_pred_name(!.ModuleInfo, should_module_qualify),
                PredIds),
            PredIdPieces = component_list_to_line_pieces(PredIdPiecesList,
                [suffix(".")]),
            MainPieces = StartPieces ++ PredIdPieces,
            VerbosePieces = [
                words("An explicit module qualifier may"),
                words("be necessary.")
            ],
            Msg = simple_msg(Context,
                [always(MainPieces), verbose_only(VerbosePieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        )
    ;
        (
            Origin = user,
            undefined_pred_or_func_error(Name, Arity, Context,
                "`:- pragma foreign_export' declaration", !Specs)
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
                unexpected(this_file, "Bad introduced foreign_export pragma.")
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
        map.lookup(Procs, ProcId, ProcInfo),
        proc_info_get_declared_determinism(ProcInfo, MaybeDetism),
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
                fixed(determinism_to_string(Detism) ++ ".")
            ],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
                [Msg]),
            !:Specs = [Spec | !.Specs]
        ;
            % Emit a warning about using pragma foreign_export with
            % a foreign language that is not supported.
            % XXX That's currently C#.
            (
                Lang = lang_csharp,
                Pieces = [words("Warning:"),
                    fixed("`:- pragma foreign_export' declarations"),
                    words("are not yet implemented for language"),
                    words(foreign_language_string(Lang)), suffix("."), nl],
                Msg = simple_msg(Context, [always(Pieces)]),
                Spec = error_spec(severity_warning,
                    phase_parse_tree_to_hlds, [Msg]),
                !:Specs = [Spec | !.Specs]
            ;
                ( Lang = lang_c
                ; Lang = lang_il
                ; Lang = lang_java
                ; Lang = lang_erlang
                )
            ),

            % Only add the foreign export if the specified language matches
            % one of the foreign languages available for this backend.
            module_info_get_globals(!.ModuleInfo, Globals),
            globals.get_backend_foreign_languages(Globals, ForeignLanguages),
            (
                % XXX C# exports currently cause an
                % assertion failure in the MLDS->IL code generator.

                Lang \= lang_csharp,
                list.member(Lang, ForeignLanguages)
            ->
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
            )
        )
    ;
        % We do not warn about errors in export pragmas created by the
        % compiler as part of a source-to-source transformation.
        (
            Origin = user,
            undefined_mode_error(Name, Arity, Context,
                "`:- pragma foreign_export' declaration", !Specs)
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
                unexpected(this_file,
                    "Bad introduced foreign_export pragma.")
            )
        )
    ).

%-----------------------------------------------------------------------------%

add_pragma_reserve_tag(TypeName, TypeArity, PragmaStatus, Context, !ModuleInfo,
        !Specs) :-
    TypeCtor = type_ctor(TypeName, TypeArity),
    module_info_get_type_table(!.ModuleInfo, TypeTable0),
    ContextPieces = [
        words("In"), quote("pragma reserve_tag"), words("declaration for"),
        sym_name_and_arity(TypeName / TypeArity), suffix(":"), nl
    ],
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
            ErrorPieces = [
                words("error:"), quote("pragma reserve_tag"),
                words("declaration must have"),
                words("the same visibility as the type definition.")
            ]
        ;
            (
                TypeBody0 = hlds_du_type(Body, _CtorTags0, _CheaperTagTest,
                    _IsEnum0, MaybeUserEqComp, ReservedTag0, _ReservedAddr,
                    IsForeign),
                (
                    ReservedTag0 = uses_reserved_tag,
                    % Make doubly sure that we don't get any spurious warnings
                    % with intermodule optimization ...
                    TypeStatus \= status_opt_imported
                ->
                    MaybeSeverity = yes(severity_warning),
                    ErrorPieces = [
                        words("warning: multiple"),
                        quote("pragma reserved_tag"),
                        words("declarations for the same type.")
                    ]
                ;
                    MaybeSeverity = no,
                    ErrorPieces = []
                ),

                % We passed all the semantic checks. Mark the type as having
                % a reserved tag, and recompute the constructor tags.
                ReservedTag = uses_reserved_tag,
                module_info_get_globals(!.ModuleInfo, Globals),
                assign_constructor_tags(Body, MaybeUserEqComp, TypeCtor,
                    ReservedTag, Globals, CtorTags, ReservedAddr, EnumDummy),
                TypeBody = hlds_du_type(Body, CtorTags, no_cheaper_tag_test,
                    EnumDummy, MaybeUserEqComp, ReservedTag, ReservedAddr,
                    IsForeign),
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
                ErrorPieces = [
                    words("error:"), sym_name_and_arity(TypeName / TypeArity),
                    words("is not a discriminated union type."), nl
                ]
            )
        )
    ;
        MaybeSeverity = yes(severity_error),
        ErrorPieces = [
            words("error: undefined type"),
            sym_name_and_arity(TypeName / TypeArity), suffix("."), nl
        ]
    ),
    (
        ErrorPieces = []
    ;
        ErrorPieces = [_ | _],
        (
            MaybeSeverity = yes(Severity)
        ;
            MaybeSeverity = no,
            unexpected(this_file, "add_pragma_reserve_tag: no severity")
        ),
        Msg = simple_msg(Context, [always(ContextPieces ++ ErrorPieces)]),
        Spec = error_spec(Severity, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ).

%-----------------------------------------------------------------------------%

add_pragma_foreign_export_enum(Lang, TypeName, TypeArity, Attributes,
        Overrides, _ImportStatus, Context, !ModuleInfo, !Specs) :-
    TypeCtor = type_ctor(TypeName, TypeArity),
    module_info_get_type_table(!.ModuleInfo, TypeTable),
    ContextPieces = [
        words("In"), fixed("`pragma foreign_export_enum'"),
        words("declaration for"),
        sym_name_and_arity(TypeName / TypeArity), suffix(":"), nl
    ],
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
        ErrorPieces = [
            words("error: "),
            sym_name_and_arity(TypeName / TypeArity),
            words("is an atomic type"),
            suffix(".")
        ]
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
                ErrorPieces = [
                    words("error: "),
                    sym_name_and_arity(TypeName / TypeArity),
                    words("is not an enumeration type"),
                    suffix(".")
                ]
            ;
                % XXX How should we handle IsForeignType here?
                TypeBody = hlds_du_type(Ctors, _TagValues, _CheaperTagTest,
                    DuTypeKind, _MaybeUserEq, _ReservedTag, _ReservedAddr,
                    _IsForeignType),
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
                            TypeName, TypeArity, Context, Prefix, MakeUpperCase,
                            OverridesMap, Ctors, MaybeMapping, !Specs),
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
                    ErrorPieces = [
                        words("error: "),
                        sym_name_and_arity(TypeName / TypeArity),
                        words("is not an enumeration type."),
                        words("It has one more non-zero arity"),
                        words("constructors.")
                    ]
                )
            )
        ;
            % This case corresponds to an undefined type.  We do not
            % issue an error message for it here since module qualification
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
            unexpected(this_file, "add_foreign_export_enum: no severity")
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
        unexpected(this_file,
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
        ErrorPieces = [
            words("error: "),
            words("the user-specified mapping between Mercury and"),
            words("foreign names does not form a bijection.")
        ],
        Msg = simple_msg(Context, [always(ContextPieces ++ ErrorPieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ).

:- pred build_export_enum_name_map(format_components::in,
    foreign_language::in, sym_name::in, arity::in, prog_context::in,
    string::in, uppercase_export_enum::in, map(sym_name, string)::in,
    list(constructor)::in, maybe(map(sym_name, string))::out,
    list(error_spec)::in, list(error_spec)::out) is det.

build_export_enum_name_map(ContextPieces, Lang, TypeName, TypeArity,
        Context, Prefix, MakeUpperCase, Overrides0, Ctors, MaybeMapping, !Specs) :-
    (
        TypeName = qualified(TypeModuleQual, _)
    ;
        % The type name should have been module qualified by now.
        TypeName = unqualified(_),
        unexpected(this_file, "unqualified type name for foreign_export_enum")
    ),

    list.foldl3(
        add_ctor_to_name_map(Lang, Prefix, MakeUpperCase, TypeModuleQual),
        Ctors, Overrides0, Overrides, map.init, NameMap, [], BadCtors),
    %
    % Check for any remaining user-specified renamings that didn't
    % match the constructors of the type and report and error
    % for them.
    %
    ( not map.is_empty(Overrides) ->
       InvalidRenamingPieces = [
            words("user-specified foreign names for constructors"),
            words("that do not match match any of the constructors of"),
            sym_name_and_arity(TypeName / TypeArity), suffix(".")
        ],
        InvalidRenamings = map.keys(Overrides),
        InvalidRenamingComponents =
            list.map((func(S) = [sym_name(S)]), InvalidRenamings),
        InvalidRenamingList = component_list_to_line_pieces(
            InvalidRenamingComponents, [nl]),
        InvalidRenamingVerbosePieces = [
            words("The following"),
            words(choose_number(InvalidRenamings,
                "constructor does", "constructors do")),
            words("not match"), suffix(":"), nl_indent_delta(2)
        ] ++ InvalidRenamingList,
        InvalidRenamingMsg = simple_msg(Context,
            [
                always(ContextPieces ++ InvalidRenamingPieces),
                verbose_only(InvalidRenamingVerbosePieces)
            ]),
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
                sorry(this_file,
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
            BadCtorsVerboseErrorPieces = [
                words("The following"),
                words(choose_number(BadCtors, "constructor",
                    "constructors")),
                words("cannot be converted:"), nl_indent_delta(2)
            ] ++ BadCtorsList,
            BadCtorsMsg = simple_msg(Context,
                [
                    always(ContextPieces ++ BadCtorsErrorPieces),
                    verbose_only(BadCtorsVerboseErrorPieces)
                ]),
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
    NamesAndForeignNames  = map.to_assoc_list(NameMap),
    ( bimap.from_assoc_list(NamesAndForeignNames, _) ->
        MaybeNameMap = yes(NameMap)
    ;
        MaybeNameMap = no,
        % XXX we should report exactly why it is not bijective.
        ErrorPieces = [
            words("error: the mapping between Mercury and foreign names"),
            words("does not form a bijection.")
        ],
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
        %
        CtorSymName    = qualified(_, _),
        UnqualCtorName = unqualify_name(CtorSymName),
        UnqualSymName  = unqualified(UnqualCtorName)
    ;
        CtorSymName = unqualified(_),
        unexpected(this_file, "unqualified constructor name")
    ),
    %
    % If the user specified a name for this constructor then use that.
    %
    ( svmap.remove(UnqualSymName, UserForeignName, !Overrides) ->
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
        Lang = lang_c,
        IsValidForeignName = pred_to_bool(is_valid_c_identifier(ForeignName))
    ;
        Lang = lang_java,
        IsValidForeignName = pred_to_bool(is_valid_c_identifier(ForeignName))
    ;
        ( Lang = lang_csharp
        ; Lang = lang_il
        ; Lang = lang_erlang
        ),
        sorry(this_file, "foreign_export_enum for language other than C")
    ),
    (
        IsValidForeignName = yes,
        svmap.det_insert(UnqualSymName, ForeignName, !NameMap)
    ;
        IsValidForeignName = no,
        list.cons(UnqualSymName, !BadCtors)
    ).

%-----------------------------------------------------------------------------%

add_pragma_foreign_enum(Lang, TypeName, TypeArity, ForeignTagValues,
        ImportStatus, Context, !ModuleInfo, !Specs) :-
    TypeCtor = type_ctor(TypeName, TypeArity),
    module_info_get_type_table(!.ModuleInfo, TypeTable0),
    ContextPieces = [
        words("In"), fixed("`pragma foreign_enum'"),
        words("declaration for"),
        sym_name_and_arity(TypeName / TypeArity), suffix(":"), nl
    ],
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
                DuTypeKind0, MaybeUserEq, ReservedTag, ReservedAddr,
                IsForeignType),
            % Work out what language's foreign_enum pragma we should be
            % looking at for the the current compilation target language.
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
                % module or they are both imported.  Any other combination
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
                                ReservedTag, ReservedAddr, IsForeignType),
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
                        % this target language then don't do anything.
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
                ( LangForForeignEnums \= Lang ->
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
        % This else-branch corresponds to an undefined type.  We do not
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
            unexpected(this_file, "add_foreign_enum: no severity")
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
        unexpected(this_file,
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
    % but the ones we match against in the HLDS are not.  Module qualify
    % them.
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
        CtorSymName  = qualified(TypeModuleName, Name)
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

target_lang_to_foreign_enum_lang(target_c)    = lang_c.
target_lang_to_foreign_enum_lang(target_il)   = lang_il.
target_lang_to_foreign_enum_lang(target_java) = lang_java.
target_lang_to_foreign_enum_lang(target_asm) =
    sorry(this_file, "pragma foreign_enum and --target `asm'.").
target_lang_to_foreign_enum_lang(target_x86_64) =
    sorry(this_file, "pragma foreign_enum and --target `x86_64'.").
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
        unexpected(this_file, "non arity zero enumeration constant.")
    ),
    ( map.search(ForeignTagMap, ConsSymName, ForeignTagValue) ->
        ForeignTag = foreign_tag(ForeignLanguage, ForeignTagValue),
        svmap.set(ConsId, ForeignTag, !ConsTagValues)
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
    VerboseErrorPieces = [
        words("The following"), words(DoOrDoes),
        nl_indent_delta(2)
    ] ++ CtorList,
    Msg = simple_msg(Context,
        [
            always(ContextPieces ++ ErrorPieces),
            verbose_only(VerboseErrorPieces)
        ]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
        [Msg]),
    list.cons(Spec, !Specs).

:- pred add_foreign_enum_bijection_error(prog_context::in,
    format_components::in, list(error_spec)::in, list(error_spec)::out)
    is det.

add_foreign_enum_bijection_error(Context, ContextPieces, !Specs) :-
    ErrorPieces = [
        words("error: "),
        words("the mapping between Mercury enumeration values and"),
        words("foreign values does not form a bijection.")
    ],
    Msg = simple_msg(Context, [always(ContextPieces ++ ErrorPieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    list.cons(Spec, !Specs).

:- pred add_foreign_enum_pragma_in_interface_error(prog_context::in,
    sym_name::in, arity::in, list(error_spec)::in, list(error_spec)::out)
    is det.

add_foreign_enum_pragma_in_interface_error(Context, TypeName, TypeArity,
        !Specs) :-
    ErrorPieces = [
        words("Error: "),
        words("`pragma foreign_enum' declaration for"),
        sym_name_and_arity(TypeName / TypeArity),
        words("in module interface.")
    ],
    Msg = simple_msg(Context, [always(ErrorPieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    list.cons(Spec, !Specs).

%-----------------------------------------------------------------------------%

:- pred add_pragma_unused_args(pred_or_func::in, sym_name::in, arity::in,
    mode_num::in, list(int)::in, prog_context::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_unused_args(PredOrFunc, SymName, Arity, ModeNum, UnusedArgs,
        Context, !ModuleInfo, !Specs) :-
    module_info_get_predicate_table(!.ModuleInfo, Preds),
    (
        predicate_table_search_pf_sym_arity(Preds, is_fully_qualified,
            PredOrFunc, SymName, Arity, [PredId])
    ->
        module_info_get_unused_arg_info(!.ModuleInfo, UnusedArgInfo0),
        % Convert the mode number to a proc_id.
        proc_id_to_int(ProcId, ModeNum),
        map.set(UnusedArgInfo0, proc(PredId, ProcId), UnusedArgs,
            UnusedArgInfo),
        module_info_set_unused_arg_info(UnusedArgInfo, !ModuleInfo)
    ;
        Pieces = [words("Internal compiler error: "),
            words("unknown predicate in `pragma unused_args'.")],
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ).

%-----------------------------------------------------------------------------%

:- pred add_pragma_exceptions(pred_or_func::in, sym_name::in, arity::in,
    mode_num::in, exception_status::in, prog_context::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_exceptions(PredOrFunc, SymName, Arity, ModeNum, ThrowStatus,
        _Context, !ModuleInfo, !Specs) :-
    module_info_get_predicate_table(!.ModuleInfo, Preds),
    (
        predicate_table_search_pf_sym_arity(Preds, is_fully_qualified,
            PredOrFunc, SymName, Arity, [PredId])
    ->
        some [!ExceptionInfo] (
            module_info_get_exception_info(!.ModuleInfo, !:ExceptionInfo),
            % convert the mode number to a proc_id
            proc_id_to_int(ProcId, ModeNum),
            ProcExceptionInfo = proc_exception_info(ThrowStatus, no),
            svmap.set(proc(PredId, ProcId), ProcExceptionInfo,
                !ExceptionInfo),
            module_info_set_exception_info(!.ExceptionInfo, !ModuleInfo)
        )
    ;
        % XXX We'll just ignore this for the time being -
        % it causes errors with transitive-intermodule optimization.
        % io.write_string("Internal compiler error: " ++
        %   "unknown predicate in `pragma exceptions'.\n")
        true
    ).

%-----------------------------------------------------------------------------%

:- pred add_pragma_trailing_info(pred_or_func::in, sym_name::in, arity::in,
    mode_num::in, trailing_status::in, prog_context::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_trailing_info(PredOrFunc, SymName, Arity, ModeNum, TrailingStatus,
        _Context, !ModuleInfo, !Specs) :-
    module_info_get_predicate_table(!.ModuleInfo, Preds),
    (
        predicate_table_search_pf_sym_arity(Preds, is_fully_qualified,
            PredOrFunc, SymName, Arity, [PredId])
    ->
        module_info_get_trailing_info(!.ModuleInfo, TrailingInfo0),
        proc_id_to_int(ProcId, ModeNum),
        map.set(TrailingInfo0, proc(PredId, ProcId),
            proc_trailing_info(TrailingStatus, no),
            TrailingInfo),
        module_info_set_trailing_info(TrailingInfo, !ModuleInfo)
    ;
        % XXX We'll just ignore this for the time being -
        % it causes errors with transitive-intermodule optimization.
        % io.write_string("Internal compiler error: " ++
        %   "unknown predicate in `pragma trailing_info'.\n"),
        true
    ).

%-----------------------------------------------------------------------------%

:- pred add_pragma_mm_tabling_info(pred_or_func::in, sym_name::in, arity::in,
    mode_num::in, mm_tabling_status::in, prog_context::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_mm_tabling_info(PredOrFunc, SymName, Arity, ModeNum,
        TablingStatus, _Context, !ModuleInfo, !Specs) :-
    module_info_get_predicate_table(!.ModuleInfo, Preds),
    (
        predicate_table_search_pf_sym_arity(Preds, is_fully_qualified,
            PredOrFunc, SymName, Arity, [PredId])
    ->
        some [!TablingInfo] (
            module_info_get_mm_tabling_info(!.ModuleInfo, !:TablingInfo),
            proc_id_to_int(ProcId, ModeNum),
            svmap.set(proc(PredId, ProcId),
                proc_mm_tabling_info(TablingStatus, no), !TablingInfo),
            module_info_set_mm_tabling_info(!.TablingInfo, !ModuleInfo)
        )
    ;
        % XXX We'll just ignore this for the time being -
        % it causes errors with transitive-intermodule optimization.
        % io.write_string("Internal compiler error: " ++
        %   "unknown predicate in `pragma trailing_info'.\n"),
        true
    ).

%-----------------------------------------------------------------------------%

add_pragma_type_spec(Pragma, Context, !ModuleInfo, !QualInfo, !Specs) :-
    Pragma = pragma_type_spec(SymName, _, Arity, MaybePredOrFunc, _, _, _, _),
    module_info_get_predicate_table(!.ModuleInfo, Preds),
    (
        (
            MaybePredOrFunc = yes(PredOrFunc),
            adjust_func_arity(PredOrFunc, Arity, PredArity),
            predicate_table_search_pf_sym_arity(Preds, is_fully_qualified,
                PredOrFunc, SymName, PredArity, PredIds)
        ;
            MaybePredOrFunc = no,
            predicate_table_search_sym_arity(Preds, is_fully_qualified,
                SymName, Arity, PredIds)
        ),
        PredIds = [_ | _]
    ->
        list.foldl3(add_pragma_type_spec_2(Pragma, Context), PredIds,
            !ModuleInfo, !QualInfo, !Specs)
    ;
        undefined_pred_or_func_error(SymName, Arity, Context,
            "`:- pragma type_spec' declaration", !Specs)
    ).

:- pred add_pragma_type_spec_2(pragma_type::in(pragma_type_spec),
    prog_context::in, pred_id::in, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_type_spec_2(Pragma0, Context, PredId, !ModuleInfo, !QualInfo,
        !Specs) :-
    Pragma0 = pragma_type_spec(SymName, SpecName, Arity, _, MaybeModes, Subst,
        TVarSet0, ExpandedItems),
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
            % If we're doing smart recompilation we need to record the pragmas
            % even if we aren't doing type specialization to avoid problems
            % with differing output for the recompilation tests in debugging
            % grades.

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
            map.init(VarTypes0),
            goal_info_init(GoalInfo0),
            set.list_to_set(Args, NonLocals),
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
                Context),
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
            set.insert_list(ProcsToSpec0, PredProcIds, ProcsToSpec),
            set.insert(ForceVersions0, NewPredId, ForceVersions),

            ( Status = status_opt_imported ->
                % For imported predicates dead_proc_elim.m needs to know that
                % if the original predicate is used, the predicate to force
                % the production of the specialised interface is also used.
                multi_map.set(SpecMap0, PredId, NewPredId, SpecMap)
            ;
                SpecMap = SpecMap0
            ),
            Pragma = pragma_type_spec(SymName, SpecName, Arity,
                yes(PredOrFunc), MaybeModes, map.to_assoc_list(RenamedSubst),
                TVarSet, ExpandedItems),
            multi_map.set(PragmaMap0, PredId, Pragma, PragmaMap),
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
        unexpected(this_file,
            "handle_pragma_type_spec_subst: empty substitution")
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
    svmap.set(K, V, !Map).

:- pred find_duplicate_list_elements(list(T)::in, list(T)::out) is det.

find_duplicate_list_elements([], []).
find_duplicate_list_elements([H | T], Vars) :-
    find_duplicate_list_elements(T, Vars0),
    ( list.member(H, T) ->
        Vars = [H | Vars0]
    ;
        Vars = Vars0
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
    Pieces = [words("In `:- pragma type_spec' declaration for"),
        simple_call(SimpleCallId), suffix(":"), nl].

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
            map.det_insert(map.init, ProcId, ProcInfo, !:Procs),
            ProcIds = [ProcId],
            MaybeProcIds = yes(ProcIds)
        ;
            module_info_incr_errors(!ModuleInfo),
            undefined_mode_error(SymName, Arity, Context,
                "`:- pragma type_spec' declaration", !Specs),
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

add_pragma_termination2_info(PredOrFunc, SymName, ModeList,
        MaybePragmaSuccessArgSizeInfo, MaybePragmaFailureArgSizeInfo,
        MaybePragmaTerminationInfo, Context, !ModuleInfo, !Specs) :-
    module_info_get_predicate_table(!.ModuleInfo, Preds),
    list.length(ModeList, Arity),
    (
        predicate_table_search_pf_sym_arity(Preds,
        is_fully_qualified, PredOrFunc, SymName, Arity, PredIds),
        PredIds = [_ | _]
    ->
        ( PredIds = [PredId] ->
            module_info_preds(!.ModuleInfo, PredTable0),
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

                    !:TermInfo = !.TermInfo ^ import_success :=
                        MaybePragmaSuccessArgSizeInfo,
                    !:TermInfo = !.TermInfo ^ import_failure :=
                        MaybePragmaFailureArgSizeInfo,
                    !:TermInfo = !.TermInfo ^ term_status :=
                        MaybeTerminationInfo,

                    proc_info_set_termination2_info(!.TermInfo,
                        ProcInfo0, ProcInfo)
                ),
                map.det_update(ProcTable0, ProcId, ProcInfo, ProcTable),
                pred_info_set_procedures(ProcTable, PredInfo0, PredInfo),
                map.det_update(PredTable0, PredId, PredInfo, PredTable),
                module_info_set_preds(PredTable, !ModuleInfo)
            ;
                Pieces = [words("Error: `:- pragma termination2_info'"),
                    words("declaration for undeclared mode of"),
                    simple_call(simple_call_id(PredOrFunc, SymName, Arity)),
                    suffix("."), nl],
                Msg = simple_msg(Context, [always(Pieces)]),
                Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
                    [Msg]),
                !:Specs = [Spec | !.Specs]
            )
        ;
            Pieces = [words("Error: ambiguous predicate name"),
                simple_call(simple_call_id(PredOrFunc, SymName, Arity)),
                words("in"), fixed("`pragma termination2_info'."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
                [Msg]),
            !:Specs = [Spec | !.Specs]
        )
    ;
        % XXX This happens in `.trans_opt' files sometimes --
        % so just ignore it.
        true
        %   undefined_pred_or_func_error(
        %        SymName, Arity, Context,
        %        "`:- pragma termination2_info' declaration", !Specs)
    ).

%-----------------------------------------------------------------------------%

add_pragma_structure_sharing(_PredOrFunc, _SymName, _ModeList, _HeadVars,
        _Types, no, _Context, !ModuleInfo, !Specs).
add_pragma_structure_sharing(PredOrFunc, SymName, ModeList, HeadVars,
        Types, yes(SharingDomain), Context, !ModuleInfo, !Specs):-
    module_info_get_predicate_table(!.ModuleInfo, Preds),
    list.length(ModeList, Arity),
    (
        predicate_table_search_pf_sym_arity(Preds, is_fully_qualified,
            PredOrFunc, SymName, Arity, PredIds),
        PredIds = [_ | _]
    ->
        ( PredIds = [PredId] ->
            module_info_preds(!.ModuleInfo, PredTable0),
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
                map.det_update(ProcTable0, ProcId, ProcInfo, ProcTable),
                pred_info_set_procedures(ProcTable, PredInfo0, PredInfo),
                map.det_update(PredTable0, PredId, PredInfo, PredTable),
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
            Pieces = [words("Error: ambiguous predicate name"),
                simple_call(simple_call_id(PredOrFunc, SymName, Arity)),
                words("in"), quote("pragma structure_sharing."),
                suffix("."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        )
    ;
        % XXX This happens in `.trans_opt' files sometimes --
        % so just ignore it.
        true
        %   undefined_pred_or_func_error(SymName, Arity, Context,
        %       "`:- pragma structure_sharing' declaration",
        %       !Specs)
    ).

add_pragma_structure_reuse(_PredOrFunc, _SymName, _ModeList, _HeadVars,
        _Types, no, _Context, !ModuleInfo, !Specs).
add_pragma_structure_reuse(PredOrFunc, SymName, ModeList, HeadVars,
        Types, yes(ReuseDomain), Context, !ModuleInfo, !Specs):-
    module_info_get_predicate_table(!.ModuleInfo, Preds),
    list.length(ModeList, Arity),
    (
        predicate_table_search_pf_sym_arity(Preds, is_fully_qualified,
            PredOrFunc, SymName, Arity, PredIds),
        PredIds = [_ | _]
    ->
        ( PredIds = [PredId] ->
            module_info_preds(!.ModuleInfo, PredTable0),
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
                map.det_update(ProcTable0, ProcId, ProcInfo, ProcTable),
                pred_info_set_procedures(ProcTable, PredInfo0, PredInfo),
                map.det_update(PredTable0, PredId, PredInfo, PredTable),
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
            Pieces = [words("Error: ambiguous predicate name"),
                simple_call(simple_call_id(PredOrFunc, SymName, Arity)),
                words("in"), quote("pragma structure_reuse"), suffix("."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        )
    ;
        % XXX This happens in `.trans_opt' files sometimes --
        % so just ignore it
        true
        %   undefined_pred_or_func_error(SymName, Arity, Context,
        %       "`:- pragma structure_sharing' declaration",
        %       !Specs)
    ).
%-----------------------------------------------------------------------------%

add_pragma_termination_info(PredOrFunc, SymName, ModeList,
        MaybePragmaArgSizeInfo, MaybePragmaTerminationInfo,
        Context, !ModuleInfo, !Specs) :-
    module_info_get_predicate_table(!.ModuleInfo, Preds),
    list.length(ModeList, Arity),
    (
        predicate_table_search_pf_sym_arity(Preds, is_fully_qualified,
            PredOrFunc, SymName, Arity, PredIds),
        PredIds = [_ | _]
    ->
        ( PredIds = [PredId] ->
            module_info_preds(!.ModuleInfo, PredTable0),
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
                map.det_update(ProcTable0, ProcId, ProcInfo, ProcTable),
                pred_info_set_procedures(ProcTable, PredInfo0, PredInfo),
                map.det_update(PredTable0, PredId, PredInfo, PredTable),
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
            Pieces = [words("Error: ambiguous predicate name"),
                simple_call(simple_call_id(PredOrFunc, SymName, Arity)),
                words("in"), fixed("`pragma termination_info'."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        )
    ;
        % XXX This happens in `.trans_opt' files sometimes --
        % so just ignore it.
        true
        %   undefined_pred_or_func_error(SymName, Arity, Context,
        %       "`:- pragma termination_info' declaration",
        %       !Specs
    ).

module_add_pragma_import(PredName, PredOrFunc, Modes, Attributes, C_Function,
        Status, Context, !ModuleInfo, !QualInfo, !Specs) :-
    module_info_get_name(!.ModuleInfo, ModuleName),
    list.length(Modes, Arity),

    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    (
        VeryVerbose = yes,
        trace [io(!IO)] (
            io.write_string("% Processing `:- pragma import' for ", !IO),
            write_simple_call_id(PredOrFunc, PredName/Arity, !IO),
            io.write_string("...\n", !IO)
        )
    ;
        VeryVerbose = no
    ),

    % Lookup the pred declaration in the predicate table. (If it's not there,
    % print an error message and insert a dummy declaration for the predicate.)
    module_info_get_predicate_table(!.ModuleInfo, PredicateTable0),
    (
        predicate_table_search_pf_sym_arity(PredicateTable0,
            is_fully_qualified, PredOrFunc, PredName, Arity, [PredId0])
    ->
        PredId = PredId0
    ;
        preds_add_implicit_report_error(ModuleName, PredOrFunc,
            PredName, Arity, Status, no, Context, origin_user(PredName),
            "`:- pragma import' declaration", PredId, !ModuleInfo, !Specs)
    ),
    % Lookup the pred_info for this pred, and check that it is valid.
    module_info_get_predicate_table(!.ModuleInfo, PredicateTable2),
    predicate_table_get_preds(PredicateTable2, Preds0),
    map.lookup(Preds0, PredId, PredInfo0),
    % Opt_imported preds are initially tagged as imported and are tagged as
    % opt_imported only if/when we see a clause (including a `pragma import'
    % clause) for them.
    ( Status = status_opt_imported ->
        pred_info_set_import_status(status_opt_imported, PredInfo0, PredInfo1)
    ;
        PredInfo1 = PredInfo0
    ),
    ( pred_info_is_imported(PredInfo1) ->
        Pieces = [words("Error: `:- pragma import' declaration for imported"),
            simple_call(simple_call_id(PredOrFunc, PredName, Arity)),
            suffix("."), nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ; pred_info_clause_goal_type(PredInfo1) ->
        module_info_incr_errors(!ModuleInfo),
        Pieces = [words("Error: `:- pragma import' declaration for"),
            simple_call(simple_call_id(PredOrFunc, PredName, Arity)),
            words("with preceding clauses."), nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ;
        pred_info_update_goal_type(goal_type_foreign, PredInfo1, PredInfo2),
            % Add the pragma declaration to the proc_info for this procedure.
        pred_info_get_procedures(PredInfo2, Procs),
        map.to_assoc_list(Procs, ExistingProcs),
        (
            get_procedure_matching_argmodes(ExistingProcs, Modes,
                !.ModuleInfo, ProcId)
        ->
            pred_add_pragma_import(PredId, ProcId, Attributes, C_Function,
                Context, PredInfo2, PredInfo, !ModuleInfo, !QualInfo, !Specs),
            map.det_update(Preds0, PredId, PredInfo, Preds),
            predicate_table_set_preds(Preds,
                PredicateTable2, PredicateTable),
            module_info_set_predicate_table(PredicateTable, !ModuleInfo)
        ;
            Pieces = [words("Error: `:- pragma import' declaration"),
                words("for undeclared mode of"),
                simple_call(simple_call_id(PredOrFunc, PredName, Arity)),
                suffix("."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        )
    ).

    % Pred_add_pragma_import is a subroutine of module_add_pragma_import
    % which adds the c_code for a `pragma import' declaration to a pred_info.
    %
:- pred pred_add_pragma_import(pred_id::in, proc_id::in,
    pragma_foreign_proc_attributes::in, string::in, prog_context::in,
    pred_info::in, pred_info::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

pred_add_pragma_import(PredId, ProcId, Attributes, C_Function, Context,
        !PredInfo, !ModuleInfo, !QualInfo, !Specs) :-
    pred_info_get_procedures(!.PredInfo, Procs),
    map.lookup(Procs, ProcId, ProcInfo),
    foreign.make_pragma_import(!.PredInfo, ProcInfo, C_Function, Context,
        PragmaImpl, VarSet, PragmaVars, ArgTypes, Arity, PredOrFunc,
        !ModuleInfo, !Specs),

    % Lookup some information we need from the pred_info and proc_info.
    PredName = pred_info_name(!.PredInfo),
    PredModule = pred_info_module(!.PredInfo),
    pred_info_get_purity(!.PredInfo, Purity),
    pred_info_get_markers(!.PredInfo, Markers),

    pred_info_get_clauses_info(!.PredInfo, ClausesInfo0),
    ItemNumbers0 = ClausesInfo0 ^ cli_item_numbers,
    add_clause_item_number(no, Context, item_is_foreign_proc,
        ItemNumbers0, ItemNumbers),
    ClausesInfo1 = ClausesInfo0 ^ cli_item_numbers := ItemNumbers,

    % Add the code for this `pragma import' to the clauses_info.
    clauses_info_add_pragma_foreign_proc(pragma_import_foreign_proc,
        Purity, Attributes, PredId, ProcId,
        VarSet, PragmaVars, ArgTypes, PragmaImpl, Context,
        PredOrFunc, qualified(PredModule, PredName), Arity, Markers,
        ClausesInfo1, ClausesInfo, !ModuleInfo, !Specs),

    % Store the clauses_info etc. back into the pred_info.
    pred_info_set_clauses_info(ClausesInfo, !PredInfo).

%-----------------------------------------------------------------------------%

module_add_pragma_foreign_proc(Attributes0, PredName, PredOrFunc, PVars,
        ProgVarSet, _InstVarset, PragmaImpl, Status, Context, MaybeItemNumber,
        !ModuleInfo, !QualInfo, !Specs) :-
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
    % (If it's not there, print an error message and insert
    % a dummy declaration for the predicate.)
    module_info_get_predicate_table(!.ModuleInfo, PredTable0),
    (
        predicate_table_search_pf_sym_arity(PredTable0, is_fully_qualified,
            PredOrFunc, PredName, Arity, [PredId0])
    ->
        PredId = PredId0
    ;
        preds_add_implicit_report_error(ModuleName, PredOrFunc,
            PredName, Arity, Status, no, Context, origin_user(PredName),
            "`:- pragma foreign_proc' declaration",
            PredId, !ModuleInfo, !Specs)
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
            Pieces = [words("Error: `:- pragma foreign_proc'"),
                words("(or `pragma c_code')"),
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
                clauses_info_add_pragma_foreign_proc(standard_foreign_proc,
                    Purity, Attributes, PredId, ProcId, ProgVarSet, PVars,
                    ArgTypes, PragmaImpl, Context, PredOrFunc,
                    PredName, Arity, Markers, ClausesInfo1, ClausesInfo,
                    !ModuleInfo, !Specs),
                pred_info_set_clauses_info(ClausesInfo, !PredInfo),
                pred_info_update_goal_type(goal_type_foreign, !PredInfo),
                map.det_update(Preds0, PredId, !.PredInfo, Preds),
                predicate_table_set_preds(Preds, PredTable1, PredTable),
                module_info_set_predicate_table(PredTable, !ModuleInfo),
                pragma_get_var_infos(PVars, ArgInfoBox),
                assoc_list.keys(ArgInfoBox, ArgInfo),
                warn_singletons_in_pragma_foreign_proc(PragmaImpl,
                    PragmaForeignLanguage, ArgInfo, Context, SimpleCallId,
                    PredId, ProcId, !.ModuleInfo, !Specs)
            ;
                Pieces = [words("Error: `:- pragma foreign_proc' declaration"),
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

module_add_pragma_tabled(EvalMethod, PredName, Arity, MaybePredOrFunc,
        MaybeModes, MaybeAttributes, Context, !Status, !ModuleInfo,
        !QualInfo, !Specs) :-
    module_info_get_predicate_table(!.ModuleInfo, PredicateTable0),
    EvalMethodStr = eval_method_to_string(EvalMethod),
    (
        MaybePredOrFunc = yes(PredOrFunc0),
        PredOrFunc = PredOrFunc0,

        % Lookup the pred declaration in the predicate table.
        % (If it is not there, print an error message and insert
        % a dummy declaration for the predicate.)
        (
            predicate_table_search_pf_sym_arity(PredicateTable0,
                is_fully_qualified, PredOrFunc, PredName, Arity, PredIds0)
        ->
            PredIds = PredIds0
        ;
            module_info_get_name(!.ModuleInfo, ModuleName),
            string.format("`:- pragma %s' declaration", [s(EvalMethodStr)],
                Message1),
            preds_add_implicit_report_error(ModuleName, PredOrFunc, PredName,
                Arity, !.Status, no, Context, origin_user(PredName), Message1,
                PredId, !ModuleInfo, !Specs),
            PredIds = [PredId]
        )
    ;
        MaybePredOrFunc = no,
        (
            predicate_table_search_sym_arity(PredicateTable0,
                is_fully_qualified, PredName, Arity, PredIds0)
        ->
            PredIds = PredIds0
        ;
            module_info_get_name(!.ModuleInfo, ModuleName),
            string.format("`:- pragma %s' declaration", [s(EvalMethodStr)],
                Message1),
            preds_add_implicit_report_error(ModuleName, pf_predicate, PredName,
                Arity, !.Status, no, Context, origin_user(PredName), Message1,
                PredId, !ModuleInfo, !Specs),
            PredIds = [PredId]
        )
    ),
    (
        MaybeAttributes = yes(Attributes),
        Statistics = Attributes ^ table_attr_statistics,
        AllowReset = Attributes ^ table_attr_allow_reset,
        ( PredIds = [_, _ | _] ->
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
        ;
            true
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
        TablePragmaStr = string.format("`:- pragma %s'", [s(EvalMethodStr)]),
        InlineWarningPieces = [words("Warning: "), simple_call(SimpleCallId),
            words("has a"), fixed(TablePragmaStr),
            words("declaration but also has a"),
            quote(":- pragma inline"), words("declaration."), nl,
            words("This inline pragma will be ignored"),
            words("since tabled predicates cannot be inlined."), nl,
            words("You can use the"), fixed("`--no-warn-table-with-inline'"),
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
            set.insert(StratPredIds0, PredId, StratPredIds),
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
                    fixed("`:- pragma " ++ EvalMethodStr ++ "'"),
                    words("declaration for undeclared mode of"),
                    simple_call(SimpleCallId), suffix(".")],
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
                    fixed("`pragma " ++ EvalMethodStr ++ "'"),
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
            svmap.det_update(ProcId, ProcInfo, !ProcTable),
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
        svvarset.new_named_var("Stats", Stats, !VarSet),
        svvarset.new_named_var("IO0", IO0, !VarSet),
        svvarset.new_named_var("IO", IO, !VarSet),
        Arg1 = pragma_var(Stats, "Stats", out_mode, always_boxed),
        Arg2 = pragma_var(IO0, "_IO0", di_mode, always_boxed),
        Arg3 = pragma_var(IO, "_IO", uo_mode, always_boxed),

        Global = table_info_c_global_var_name(!.ModuleInfo, SimpleCallId,
            ProcId),
        StatsCode = "MR_get_tabling_stats(&" ++ Global ++ ", &Stats);",
        StatsImpl = fc_impl_ordinary(StatsCode, yes(Context)),
        StatsPragma = pragma_foreign_proc(!.Attrs, StatsPredSymName,
            pf_predicate, [Arg1, Arg2, Arg3], !.VarSet, InstVarSet, StatsImpl),
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
            ; TargetLang = target_asm
            ; TargetLang = target_x86_64
            ),
            ForeignLang = lang_c
        ;
            TargetLang = target_il,
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
        svvarset.new_named_var("IO0", IO0, !VarSet),
        svvarset.new_named_var("IO", IO, !VarSet),
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
        ResetImpl = fc_impl_ordinary(ResetCode, yes(Context)),
        ResetPragma = pragma_foreign_proc(!.Attrs, ResetPredSymName,
            pf_predicate, [Arg1, Arg2], !.VarSet, InstVarSet, ResetImpl),
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
    expect(unify(Target, target_c), this_file,
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

%---------------------------------------------------------------------------%

    % Extract the names from the list of pragma_vars.
    %
:- pred pragma_get_var_infos(list(pragma_var)::in,
    list(pair(maybe(pair(string, mer_mode)), box_policy))::out) is det.

pragma_get_var_infos([], []).
pragma_get_var_infos([PragmaVar | PragmaVars], [Info | Infos]) :-
    PragmaVar = pragma_var(_Var, Name, Mode, BoxPolicy),
    Info = yes(Name - Mode) - BoxPolicy,
    pragma_get_var_infos(PragmaVars, Infos).

module_add_pragma_fact_table(Pred, Arity, FileName, Status, Context,
        !ModuleInfo, !QualInfo, !Specs) :-
    module_info_get_predicate_table(!.ModuleInfo, PredicateTable),
    (
        predicate_table_search_sym_arity(PredicateTable, is_fully_qualified,
            Pred, Arity, PredIDs0),
        PredIDs0 = [PredID | PredIDs1]
    ->
        (
            PredIDs1 = [],      % only one predicate found
            module_info_pred_info(!.ModuleInfo, PredID, PredInfo0),

            % Compile the fact table into a separate .o file.
            % We should be able to dispense with the impure shenanigans
            % when we replace fact tables with fast code for large
            % disjunctions.
            some [!IO] (
                promise_pure (
                    semipure private_builtin.trace_get_io_state(!:IO),
                    fact_table_compile_facts(Pred, Arity, FileName,
                        PredInfo0, PredInfo, Context, !.ModuleInfo,
                        C_HeaderCode, PrimaryProcID, !IO),
                    impure private_builtin.trace_set_io_state(!.IO)
                )
            ),

            module_info_set_pred_info(PredID, PredInfo, !ModuleInfo),
            pred_info_get_procedures(PredInfo, ProcTable),
            pred_info_get_arg_types(PredInfo, ArgTypes),
            ProcIDs = pred_info_procids(PredInfo),
            PredOrFunc = pred_info_is_pred_or_func(PredInfo),
            adjust_func_arity(PredOrFunc, Arity, NumArgs),

            % Create foreign_decls to declare extern variables.
            module_add_foreign_decl(lang_c, foreign_decl_is_local,
                C_HeaderCode, Context, !ModuleInfo),

            module_add_fact_table_file(FileName, !ModuleInfo),

            % Create foreign_procs to access the table in each mode.
            module_add_fact_table_procedures(ProcIDs, PrimaryProcID,
                ProcTable, Pred, PredOrFunc, NumArgs, ArgTypes, Status,
                Context, !ModuleInfo, !QualInfo, !Specs)
        ;
            PredIDs1 = [_ | _],     % >1 predicate found
            Pieces = [words("In pragma fact_table for"),
                sym_name_and_arity(Pred/Arity), suffix(":"), nl,
                words("error: ambiguous predicate/function name."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        )
    ;
        undefined_pred_or_func_error(Pred, Arity, Context,
            "`:- pragma fact_table' declaration", !Specs)
    ).

    % Add a `pragma c_code' for each mode of the fact table lookup to the
    % HLDS.
    % `pragma fact_table's are represented in the HLDS by a
    % `pragma c_code' for each mode of the predicate.
    %
:- pred module_add_fact_table_procedures(list(proc_id)::in, proc_id::in,
    proc_table::in, sym_name::in, pred_or_func::in, arity::in,
    list(mer_type)::in, import_status::in, prog_context::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

module_add_fact_table_procedures([],_,_,_,_,_,_,_,_, !ModuleInfo, !QualInfo,
        !Specs).
module_add_fact_table_procedures([ProcID | ProcIDs], PrimaryProcID, ProcTable,
        SymName, PredOrFunc, Arity, ArgTypes, Status, Context,
        !ModuleInfo, !QualInfo, !Specs) :-
    module_add_fact_table_proc(ProcID, PrimaryProcID, ProcTable, SymName,
        PredOrFunc, Arity, ArgTypes, Status, Context,
        !ModuleInfo, !QualInfo, !Specs),
    module_add_fact_table_procedures(ProcIDs, PrimaryProcID, ProcTable,
        SymName, PredOrFunc, Arity, ArgTypes, Status, Context,
        !ModuleInfo, !QualInfo, !Specs).

:- pred module_add_fact_table_proc(proc_id::in, proc_id::in, proc_table::in,
    sym_name::in, pred_or_func::in, arity::in, list(mer_type)::in,
    import_status::in, prog_context::in, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

module_add_fact_table_proc(ProcID, PrimaryProcID, ProcTable, SymName,
        PredOrFunc, Arity, ArgTypes, Status, Context, !ModuleInfo, !QualInfo,
        !Specs) :-
    map.lookup(ProcTable, ProcID, ProcInfo),
    varset.init(ProgVarSet0),
    varset.new_vars(ProgVarSet0, Arity, Vars, ProgVarSet),
    proc_info_get_argmodes(ProcInfo, Modes),
    proc_info_get_inst_varset(ProcInfo, InstVarSet),
    fact_table_pragma_vars(Vars, Modes, ProgVarSet, PragmaVars),

    % We should be able to dispense with the impure shenanigans
    % when we replace fact tables with fast code for large disjunctions.
    some [!IO] (
        promise_pure (
            semipure private_builtin.trace_get_io_state(!:IO),
            fact_table_generate_c_code(SymName, PragmaVars, ProcID,
                PrimaryProcID, ProcInfo, ArgTypes, !.ModuleInfo,
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
    module_add_pragma_foreign_proc(Attrs, SymName, PredOrFunc, PragmaVars,
        ProgVarSet, InstVarSet, fc_impl_ordinary(C_ProcCode, no), Status,
        Context, MaybeItemNumber, !ModuleInfo, !QualInfo, !Specs),
    ( C_ExtraCode = "" ->
        true
    ;
        module_add_foreign_body_code(lang_c, C_ExtraCode, Context, !ModuleInfo)
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

    % This type is used to distinguish between those foreign_procs that
    % were created by the transformation for `:- pragma import' and those
    % that were not.
    %
:- type foreign_proc_origin
    --->    standard_foreign_proc
    ;       pragma_import_foreign_proc.

    % Add the pragma_foreign_proc goal to the clauses_info for this procedure.
    % To do so, we must also insert unifications between the variables in the
    % pragma foreign_proc declaration and the head vars of the pred. Also
    % return the hlds_goal.
    %
:- pred clauses_info_add_pragma_foreign_proc(foreign_proc_origin::in,
    purity::in, pragma_foreign_proc_attributes::in, pred_id::in, proc_id::in,
    prog_varset::in, list(pragma_var)::in, list(mer_type)::in,
    pragma_foreign_code_impl::in, prog_context::in,
    pred_or_func::in, sym_name::in, arity::in, pred_markers::in,
    clauses_info::in, clauses_info::out, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

clauses_info_add_pragma_foreign_proc(Origin, Purity, Attributes0,
        PredId, ProcId, PVarSet, PVars, OrigArgTypes, PragmaImpl0,
        Context, PredOrFunc, PredName, Arity, Markers,
        !ClausesInfo, !ModuleInfo, !Specs) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    ( pred_info_is_builtin(PredInfo) ->
        % When bootstrapping a change that redefines a builtin as
        % normal Mercury code, you may need to disable this action.
        Msg = simple_msg(Context,
            [always([words("Error: foreign_proc for builtin.")])]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ;
        AllProcIds = pred_info_all_procids(PredInfo),
        clauses_info_do_add_pragma_foreign_proc(Origin, Purity, Attributes0,
            PredId, ProcId, AllProcIds, PVarSet, PVars, OrigArgTypes,
            PragmaImpl0, Context, PredOrFunc, PredName, Arity,
            Markers, !ClausesInfo, !ModuleInfo, !Specs)
    ).

:- pred clauses_info_do_add_pragma_foreign_proc(foreign_proc_origin::in,
    purity::in, pragma_foreign_proc_attributes::in, pred_id::in, proc_id::in,
    list(proc_id)::in, prog_varset::in, list(pragma_var)::in,
    list(mer_type)::in, pragma_foreign_code_impl::in, prog_context::in,
    pred_or_func::in, sym_name::in, arity::in, pred_markers::in,
    clauses_info::in, clauses_info::out, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

clauses_info_do_add_pragma_foreign_proc(Origin, Purity, Attributes0,
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
    bag.insert_list(ArgBag0, Args0, ArgBag),
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
        Pieces1 = [words("In `:- pragma foreign_proc' declaration for"),
            simple_call(SimpleCallId), suffix(":"), nl],
        (
            MultipleArgs = [MultipleArg],
            Pieces2 = [words("error: variable"),
                quote(mercury_var_to_string(PVarSet, no, MultipleArg)),
                words("occurs multiple times in the argument list.")]
        ;
            MultipleArgs = [_, _ | _],
            Pieces2 = [words("error: variables"),
                quote(mercury_vars_to_string(PVarSet, no, MultipleArgs)),
                words("occur multiple times in the argument list.")]
        ),
        Msg = simple_msg(Context, [always(Pieces1 ++ Pieces2)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ;
        MultipleArgs = [],
        % Build the foreign_proc.
        goal_info_init(GoalInfo0),
        goal_info_set_context(Context, GoalInfo0, GoalInfo1),

        % Check that the purity of a predicate/function declaration agrees with
        % the (promised) purity of the foreign proc.  We do not perform this
        % check there is a promise_{pure,semipure} pragma for the
        % predicate/function since in that case they will differ anyway.  We
        % also do not perform this check if the foreign_proc was introduced as
        % a result of a `:- pragma import' declaration since doing so results
        % in spurious error messages about non-existent foreign_procs.  For
        % that case we assume that the code that constructs the foreign_procs
        % from the import pragmas sets the purity attributes correctly.
        (
            ( Origin = pragma_import_foreign_proc
            ; check_marker(Markers, marker_promised_pure)
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
            map.init(EmptyVarTypes),
            rtti_varmaps_init(EmptyRttiVarmaps),
            implicitly_quantify_clause_body_general(
                ordinary_nonlocals_maybe_lambda, HeadVarList, _Warnings,
                HldsGoal0, HldsGoal, VarSet0, VarSet, EmptyVarTypes, _,
                EmptyRttiVarmaps, _),
            NewClause = clause(selected_modes([ProcId]), HldsGoal,
                impl_lang_foreign(NewLang), Context),
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

lookup_current_backend(Globals) = CurrentBackend :-
    globals.lookup_bool_option(Globals, highlevel_code, HighLevel),
    (
        HighLevel = yes,
        CurrentBackend = high_level_backend
    ;
        HighLevel= no,
        CurrentBackend = low_level_backend
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
        FirstClause0 = clause(ApplProcIds0, Body, ClauseLang, ClauseContext),
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
                        ClauseLang, ClauseContext),
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
                unexpected(this_file,
                    "add_foreign_proc_update_existing_clauses: all_modes")
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
                            ClauseLang, ClauseContext),
                        Clauses = [FirstClause | LaterClauses],
                        Overridden = LaterOverridden
                    ),
                    % Any later clause that overrides the new foreign_proc
                    % should have overridden this old foreign_proc as well.
                    expect(
                        unify(LaterOverridden,
                            not_overridden_by_old_foreign_proc),
                        this_file,
                        "inconsistent old foreign_procs")
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
                        PiecesA = [
                            words("Error: multiple clauses for"),
                            p_or_f(PredOrFunc),
                            sym_name_and_arity(PredName / Arity),
                            words("in language"),
                            words(foreign_language_string(OldLang)),
                            suffix("."), nl
                        ],
                        PiecesB = [
                            words("The first occurrence was here.")
                        ],
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

    % Find the procedure with argmodes which match the ones we want.
    %
:- pred get_procedure_matching_argmodes(assoc_list(proc_id, proc_info)::in,
    list(mer_mode)::in, module_info::in, proc_id::out) is semidet.

get_procedure_matching_argmodes(Procs, Modes0, ModuleInfo, ProcId) :-
    list.map(constrain_inst_vars_in_mode, Modes0, Modes),
    get_procedure_matching_argmodes_2(Procs, Modes, ModuleInfo, ProcId).

:- pred get_procedure_matching_argmodes_2(assoc_list(proc_id, proc_info)::in,
    list(mer_mode)::in, module_info::in, proc_id::out) is semidet.

get_procedure_matching_argmodes_2([P | Procs], Modes, ModuleInfo, OurProcId) :-
    P = ProcId - ProcInfo,
    proc_info_get_argmodes(ProcInfo, ArgModes),
    ( mode_list_matches(Modes, ArgModes, ModuleInfo) ->
        OurProcId = ProcId
    ;
        get_procedure_matching_argmodes_2(Procs, Modes, ModuleInfo, OurProcId)
    ).

:- pred mode_list_matches(list(mer_mode)::in, list(mer_mode)::in,
    module_info::in) is semidet.

mode_list_matches([], [], _).
mode_list_matches([Mode1 | Modes1], [Mode2 | Modes2], ModuleInfo) :-
    % Use mode_get_insts_semidet instead of mode_get_insts to avoid
    % aborting if there are undefined modes.
    mode_get_insts_semidet(ModuleInfo, Mode1, Inst1, Inst2),
    mode_get_insts_semidet(ModuleInfo, Mode2, Inst1, Inst2),
    mode_list_matches(Modes1, Modes2, ModuleInfo).


get_procedure_matching_declmodes_with_renaming(Procs, Modes0,
        ModuleInfo, ProcId) :-
    list.map(constrain_inst_vars_in_mode, Modes0, Modes),
    get_procedure_matching_declmodes_with_renaming_2(Procs, Modes,
        ModuleInfo, ProcId).

:- pred get_procedure_matching_declmodes_with_renaming_2(
    assoc_list(proc_id, proc_info)::in, list(mer_mode)::in,
    module_info::in, proc_id::out) is semidet.

get_procedure_matching_declmodes_with_renaming_2([P | Procs], Modes,
        ModuleInfo, OurProcId) :-
    P = ProcId - ProcInfo,
    proc_info_declared_argmodes(ProcInfo, ArgModes),
    ( mode_list_matches_with_renaming(Modes, ArgModes, ModuleInfo) ->
        OurProcId = ProcId
    ;
        get_procedure_matching_declmodes_with_renaming_2(Procs, Modes,
            ModuleInfo, OurProcId)
    ).

%----------------------------------------------------------------------------%

:- type inst_var_renaming == map(inst_var, inst_var).
:- type inst_var_renamings == list(inst_var_renaming).

    % Succeeds if two lists of modes match allowing for a renaming
    % of inst variables between the two lists.
    %
:- pred mode_list_matches_with_renaming(list(mer_mode)::in,
    list(mer_mode)::in, module_info::in) is semidet.

mode_list_matches_with_renaming(ModesA, ModesB, ModuleInfo) :-
    mode_list_matches_with_renaming(ModesA, ModesB, _, ModuleInfo).

:- pred mode_list_matches_with_renaming(list(mer_mode)::in,
    list(mer_mode)::in, inst_var_renaming::out, module_info::in)
    is semidet.

mode_list_matches_with_renaming(ModesA, ModesB, Renaming, ModuleInfo) :-
    mode_list_matches_with_renaming_2(ModesA, ModesB, [], Renamings,
        ModuleInfo),
    list.foldl(merge_inst_var_renamings, Renamings, map.init, Renaming).

:- pred mode_list_matches_with_renaming_2(
    list(mer_mode)::in, list(mer_mode)::in,
    inst_var_renamings::in, inst_var_renamings::out,
    module_info::in) is semidet.

mode_list_matches_with_renaming_2([], [], !Renaming, _).
mode_list_matches_with_renaming_2([ModeA | ModesA], [ModeB | ModesB],
        !Substs, ModuleInfo) :-
    % We use mode_get_insts_semidet instead of mode_get_insts to avoid
    % aborting if there are undefined modes.  (Undefined modes get
    % reported later).

    mode_get_insts_semidet(ModuleInfo, ModeA, InstAInitial, InstAFinal),
    mode_get_insts_semidet(ModuleInfo, ModeB, InstBInitial, InstBFinal),
    match_insts_with_renaming(ModuleInfo, InstAInitial, InstBInitial,
        InitialSubst),
    match_insts_with_renaming(ModuleInfo, InstAFinal, InstBFinal,
        FinalSubst),
    list.append([InitialSubst, FinalSubst], !Substs),
    mode_list_matches_with_renaming_2(ModesA, ModesB, !Substs, ModuleInfo).

:- pred match_corresponding_inst_lists_with_renaming(module_info::in,
    list(mer_inst)::in, list(mer_inst)::in,
    inst_var_renaming::in, inst_var_renaming::out) is semidet.

match_corresponding_inst_lists_with_renaming(_, [], [], !Renaming).
match_corresponding_inst_lists_with_renaming(ModuleInfo, [A | As], [B | Bs],
        !Renaming) :-
    match_insts_with_renaming(ModuleInfo, A, B, Renaming0),
    merge_inst_var_renamings(Renaming0, !Renaming),
    match_corresponding_inst_lists_with_renaming(ModuleInfo, As, Bs,
        !Renaming).

:- pred match_corresponding_bound_inst_lists_with_renaming(module_info::in,
    list(bound_inst)::in, list(bound_inst)::in,
    inst_var_renaming::in, inst_var_renaming::out) is semidet.

match_corresponding_bound_inst_lists_with_renaming(_, [], [], !Renaming).
match_corresponding_bound_inst_lists_with_renaming(ModuleInfo,
        [A | As], [B | Bs], !Renaming) :-
    A = bound_functor(ConsId, ArgsA),
    B = bound_functor(ConsId, ArgsB),
    match_corresponding_inst_lists_with_renaming(ModuleInfo, ArgsA, ArgsB,
        map.init, Renaming0),
    merge_inst_var_renamings(Renaming0, !Renaming),
    match_corresponding_bound_inst_lists_with_renaming(ModuleInfo, As, Bs,
        !Renaming).

:- pred match_insts_with_renaming(module_info::in, mer_inst::in, mer_inst::in,
    map(inst_var, inst_var)::out) is semidet.

match_insts_with_renaming(ModuleInfo, InstA, InstB, Renaming) :-
    InstA = any(Uniq, HOInstInfoA),
    InstB = any(Uniq, HOInstInfoB),
    match_ho_inst_infos_with_renaming(ModuleInfo, HOInstInfoA, HOInstInfoB,
        Renaming).
match_insts_with_renaming(_, free, free, map.init).
match_insts_with_renaming(_, free(Type), free(Type), map.init).
match_insts_with_renaming(ModuleInfo, InstA, InstB, Renaming) :-
    InstA = bound(Uniq, BoundInstsA),
    InstB = bound(Uniq, BoundInstsB),
    match_corresponding_bound_inst_lists_with_renaming(ModuleInfo,
        BoundInstsA, BoundInstsB, map.init, Renaming).
match_insts_with_renaming(ModuleInfo, InstA, InstB, Renaming) :-
    InstA = ground(Uniq, HOInstInfoA),
    InstB = ground(Uniq, HOInstInfoB),
    match_ho_inst_infos_with_renaming(ModuleInfo, HOInstInfoA, HOInstInfoB,
        Renaming).
match_insts_with_renaming(_, not_reached, not_reached, map.init).
match_insts_with_renaming(_, inst_var(VarA), inst_var(VarB), Subst) :-
    svmap.insert(VarA, VarB, map.init, Subst).
match_insts_with_renaming(ModuleInfo, InstA, InstB, Subst) :-
    InstA = constrained_inst_vars(InstVarSetA, SpecInstA),
    InstB = constrained_inst_vars(InstVarSetB, SpecInstB),
    %
    % We'll deal with the specified inst first.
    %
    match_insts_with_renaming(ModuleInfo, SpecInstA, SpecInstB,
        Subst0),
    ListVarA = set.to_sorted_list(InstVarSetA),
    ListVarB = set.to_sorted_list(InstVarSetB),
    (
        ListVarA = [VarA0], ListVarB = [VarB0]
    ->
        VarA = VarA0,
        VarB = VarB0
    ;
        unexpected(this_file,
            "match_inst_with_renaming: non-singleton sets")
    ),
    ( map.search(Subst0, VarA, SpecVarB) ->
        % If VarA was already in the renaming then check that it's consistent
        % with the renaming from the set of inst vars.
        VarB = SpecVarB,
        Subst = Subst0
    ;
        map.insert(Subst0, VarA, VarB, Subst)
    ).
match_insts_with_renaming(ModuleInfo, InstA, InstB, Renaming) :-
    InstA = defined_inst(InstNameA),
    InstB = defined_inst(InstNameB),
    match_inst_names_with_renaming(ModuleInfo, InstNameA, InstNameB, Renaming).
match_insts_with_renaming(ModuleInfo, InstA, InstB, Renaming) :-
    InstA = abstract_inst(Name, ArgsA),
    InstB = abstract_inst(Name, ArgsB),
    match_corresponding_inst_lists_with_renaming(ModuleInfo, ArgsA, ArgsB,
        map.init, Renaming).

:- pred match_ho_inst_infos_with_renaming(module_info::in, ho_inst_info::in,
    ho_inst_info::in, map(inst_var, inst_var)::out) is semidet.

match_ho_inst_infos_with_renaming(ModuleInfo, HOInstInfoA, HOInstInfoB,
        Renaming) :-
    (
        HOInstInfoA = none,
        HOInstInfoB = none,
        Renaming = map.init
    ;
        HOInstInfoA = higher_order(PredInstInfoA),
        HOInstInfoB = higher_order(PredInstInfoB),
        PredInstInfoA = pred_inst_info(PredOrFunc, ModesA, Detism),
        PredInstInfoB = pred_inst_info(PredOrFunc, ModesB, Detism),
        mode_list_matches_with_renaming(ModesA, ModesB, Renaming, ModuleInfo)
    ).

:- pred match_inst_names_with_renaming(module_info::in,
    inst_name::in, inst_name::in, inst_var_renaming::out) is semidet.

match_inst_names_with_renaming(ModuleInfo, InstNameA, InstNameB, Renaming) :-
    InstNameA = user_inst(Name, ArgsA),
    InstNameB = user_inst(Name, ArgsB),
    match_corresponding_inst_lists_with_renaming(ModuleInfo,
        ArgsA, ArgsB, map.init, Renaming).
%
% XXX The rest of these are introduced by the compiler, it doesn't
% look like they need any special treatment.
%
match_inst_names_with_renaming(_, Inst @ merge_inst(_, _), Inst, map.init).
match_inst_names_with_renaming(_, Inst @ unify_inst(_, _, _, _), Inst,
        map.init).
match_inst_names_with_renaming(_, Inst @ ground_inst(_, _, _, _), Inst,
        map.init).
match_inst_names_with_renaming(_, Inst @ any_inst(_, _, _, _), Inst,
        map.init).
match_inst_names_with_renaming(_, Inst @ shared_inst(_), Inst, map.init).
match_inst_names_with_renaming(_, Inst @ mostly_uniq_inst(_), Inst, map.init).
match_inst_names_with_renaming(_, Inst @ typed_ground(_, _), Inst, map.init).
match_inst_names_with_renaming(_, Inst @ typed_inst(_, _), Inst, map.init).

:- pred merge_inst_var_renamings(inst_var_renaming::in,
    inst_var_renaming::in, inst_var_renaming::out) is semidet.

merge_inst_var_renamings(RenamingA, RenamingB, Result) :-
    map.union(merge_common_inst_vars, RenamingA, RenamingB, Result).

:- pred merge_common_inst_vars(inst_var::in, inst_var::in, inst_var::out)
    is semidet.

merge_common_inst_vars(A, A, A).

%----------------------------------------------------------------------------%
%
% Code for checking required feature set pragmas
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
        unexpected(this_file, "imported require_feature_set pragma")
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
                words("that supports concurrent execution.")],
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
                words("that uses single precision floats.")],
            VerbosePieces = [words("Grades that use single precision floats"),
                words("contain the grade modifier"),
                quote("spf"), suffix(".")],
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
                words("that uses double precision floats.")],
            VerbosePieces = [words("Grades that use double precision floats"),
                words("do not contain the grade modifier"),
                quote("spf"), suffix(".")],
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
                words("that supports memoisation.")],
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
                words("that supports executing conjuntions in parallel.")],
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
                words("or"), quote("trseg"), suffix(".")],
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
                words("the strict sequential semantics.")],
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
                words("that uses conservative garbage collection.")],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        )
    ).

%----------------------------------------------------------------------------%

:- func this_file = string.

this_file =  "add_pragma.m".

%----------------------------------------------------------------------------%
:- end_module add_pragma.
%----------------------------------------------------------------------------%
