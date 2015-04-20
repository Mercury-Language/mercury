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
    item_status::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_pass_3_pragma(item_pragma_info::in,
    import_status::in, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- include_module hlds.make_hlds.add_pragma.add_foreign_enum.
:- include_module hlds.make_hlds.add_pragma.add_pragma_tabling.
:- include_module hlds.make_hlds.add_pragma.add_pragma_type_spec.

:- import_module hlds.hlds_code_util.
:- import_module hlds.make_hlds.add_foreign_proc.
:- import_module hlds.make_hlds.add_pragma.add_foreign_enum.
:- import_module hlds.make_hlds.add_pragma.add_pragma_tabling.
:- import_module hlds.make_hlds.add_pragma.add_pragma_type_spec.
:- import_module hlds.make_hlds.add_pred.
:- import_module hlds.make_hlds.make_hlds_error.
:- import_module hlds.make_hlds.make_hlds_warn.
:- import_module hlds.make_hlds.qual_info.
:- import_module hlds.make_tags.
:- import_module hlds.pred_table.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module ll_backend.
:- import_module ll_backend.fact_table.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.
:- import_module transform_hlds.term_constr_main.
:- import_module transform_hlds.term_constr_util.
:- import_module transform_hlds.term_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module varset.

%-----------------------------------------------------------------------------%

add_pass_2_pragma(ItemPragma, Status, !ModuleInfo, !Specs) :-
    ItemPragma = item_pragma_info(Pragma, MaybeAttrs, Context, _SeqNum),
    % Check for invalid pragmas in the `interface' section.
    Status = item_status(ImportStatus, _),
    Allowed = pragma_allowed_in_interface(Pragma),
    (
        Allowed = no,
        (
            MaybeAttrs = item_origin_user,
            error_if_exported(ImportStatus, Context,
                [decl("pragma"), words("declaration")], !Specs)
        ;
            % We don't report this as an error as it just clutters up
            % the compiler output - the *real* error is whatever caused
            % the compiler to create this pragma.
            MaybeAttrs = item_origin_compiler(_)
        )
    ;
        Allowed = yes
    ),
    (
        Pragma = pragma_foreign_decl(FDInfo),
        FDInfo = pragma_info_foreign_decl(Lang, IsLocal, CHeader),
        ForeignDeclCode = foreign_decl_code(Lang, IsLocal, CHeader, Context),
        module_add_foreign_decl_code(ForeignDeclCode, !ModuleInfo)
    ;
        Pragma = pragma_foreign_code(FCInfo),
        FCInfo = pragma_info_foreign_code(Lang, BodyCode),
        ForeignBodyCode = foreign_body_code(Lang, BodyCode, Context),
        module_add_foreign_body_code(ForeignBodyCode, !ModuleInfo)
    ;
        Pragma = pragma_foreign_import_module(FIMInfo),
        FIMInfo = pragma_info_foreign_import_module(Lang, Import),
        ForeignImportModule =
            foreign_import_module_info(Lang, Import, Context),
        module_add_foreign_import_module(ForeignImportModule, !ModuleInfo)
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
            Pieces = [words("Error: illegal use of pragma"),
                quote("unused_args"), suffix(".")],
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
            Pieces = [words("Error: illegal use of pragma"),
                quote("exceptions"), suffix(".")],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        ;
            add_pragma_exceptions(ExceptionsInfo, Context, !ModuleInfo, !Specs)
        )
    ;
        Pragma = pragma_trailing_info(TrailingInfo),
        ( ImportStatus \= status_opt_imported ->
            Pieces = [words("Error: illegal use of pragma"),
                quote("trailing_info"), suffix(".")],
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
            Pieces = [words("Error: illegal use of pragma"),
                    quote("mm_tabling_info"), suffix(".")],
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
        % marker prevents the recomputation of.
        add_pred_marker("mode_check_clauses", Name, Arity, ImportStatus,
            Context, marker_user_marked_no_inline, [marker_user_marked_inline],
            !ModuleInfo, !Specs)
    ;
        Pragma = pragma_require_feature_set(RFSInfo),
        RFSInfo = pragma_info_require_feature_set(FeatureSet),
        check_required_feature_set(FeatureSet, ImportStatus, Context,
            !ModuleInfo, !Specs)
    ;
        Pragma = pragma_foreign_export_enum(FEEInfo),
        add_pragma_foreign_export_enum(FEEInfo, ImportStatus, Context,
            !ModuleInfo, !Specs)
    ;
        Pragma = pragma_foreign_enum(FEInfo),
        add_pragma_foreign_enum(FEInfo, ImportStatus, Context,
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
        ( Pragma = pragma_reserve_tag(_)
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
        PredIds = [],
        Pieces = [words("Internal compiler error:"),
            words("unknown predicate in"), pragma_decl("unused_args"),
            words("declaration."), nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ;
        PredIds = [PredId],
        module_info_get_unused_arg_info(!.ModuleInfo, UnusedArgInfo0),
        % Convert the mode number to a proc_id.
        proc_id_to_int(ProcId, ModeNum),
        map.set(proc(PredId, ProcId), UnusedArgs,
            UnusedArgInfo0, UnusedArgInfo),
        module_info_set_unused_arg_info(UnusedArgInfo, !ModuleInfo)
    ;
        PredIds = [_, _ | _],
        Pieces = [words("Internal compiler error:"),
            words("ambiguous predicate in"), pragma_decl("unused_args"),
            words("declaration."), nl],
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

    % add_pred_marker(PragmaName, Name, Arity, Status,
    %   Context, Marker, ConflictMarkers, !ModuleInfo, !Specs):
    %
    % Adds Marker to the marker list of the pred(s) with give Name and Arity,
    % updating the ModuleInfo. If the named pred does not exist, or the pred
    % already has a marker in ConflictMarkers, report an error.
    %
:- pred add_pred_marker(string::in, sym_name::in, arity::in, import_status::in,
    prog_context::in, marker::in, list(marker)::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pred_marker(PragmaName, Name, Arity, Status, Context, Marker,
        ConflictMarkers, !ModuleInfo, !Specs) :-
    ( marker_must_be_exported(Marker) ->
        MustBeExported = yes
    ;
        MustBeExported = no
    ),
    do_add_pred_marker(PragmaName, Name, Arity, Status, MustBeExported,
        Context, add_marker_pred_info(Marker), !ModuleInfo, PredIds, !Specs),
    module_info_get_preds(!.ModuleInfo, Preds),
    pragma_check_markers(Preds, PredIds, ConflictMarkers, Conflict),
    (
        Conflict = yes,
        pragma_conflict_error(Name, Arity, Context, PragmaName, !Specs)
    ;
        Conflict = no
    ).

    % Succeed if a marker for an exported procedure must also be exported.
    %
:- pred marker_must_be_exported(marker::in) is semidet.

marker_must_be_exported(_) :-
    semidet_fail.

:- type add_marker_pred_info == pred(pred_info, pred_info).
:- inst add_marker_pred_info == (pred(in, out) is det).

:- pred do_add_pred_marker(string::in, sym_name::in, arity::in,
    import_status::in, bool::in, term.context::in,
    add_marker_pred_info::in(add_marker_pred_info),
    module_info::in, module_info::out, list(pred_id)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

do_add_pred_marker(PragmaName, Name, Arity, Status, MustBeExported, Context,
        UpdatePredInfo, !ModuleInfo, PredIds, !Specs) :-
    get_matching_pred_ids(!.ModuleInfo, Name, Arity, PredIds),
    (
        PredIds = [_ | _],
        module_info_get_predicate_table(!.ModuleInfo, PredTable0),
        predicate_table_get_preds(PredTable0, Preds0),

        pragma_add_marker(PredIds, UpdatePredInfo, Status,
            MustBeExported, Preds0, Preds, WrongStatus),
        (
            WrongStatus = yes,
            pragma_status_error(Name, Arity, Context, PragmaName, !Specs)
        ;
            WrongStatus = no
        ),

        predicate_table_set_preds(Preds, PredTable0, PredTable),
        module_info_set_predicate_table(PredTable, !ModuleInfo)
    ;
        PredIds = [],
        DescPieces = [pragma_decl(PragmaName), words("declaration")],
        undefined_pred_or_func_error(Name, Arity, Context, DescPieces, !Specs)
    ).

    % For each pred_id in the list, check whether markers present in the list
    % of conflicting markers are also present in the corresponding pred_info.
    % The bool indicates whether there was a conflicting marker present.
    %
:- pred pragma_check_markers(pred_table::in, list(pred_id)::in,
    list(marker)::in, bool::out) is det.

pragma_check_markers(_, [], _, no).
pragma_check_markers(PredTable, [PredId | PredIds], ConflictList, Conflict) :-
    map.lookup(PredTable, PredId, PredInfo),
    pred_info_get_markers(PredInfo, Markers),
    (
        some [Marker] (
            list.member(Marker, ConflictList),
            check_marker(Markers, Marker)
        )
    ->
        Conflict = yes
    ;
        pragma_check_markers(PredTable, PredIds, ConflictList, Conflict)
    ).

    % For each pred_id in the list, add the given markers to the
    % list of markers in the corresponding pred_info.
    %
:- pred pragma_add_marker(list(pred_id)::in,
    add_marker_pred_info::in(add_marker_pred_info), import_status::in,
    bool::in, pred_table::in, pred_table::out, bool::out) is det.

pragma_add_marker([], _, _, _, !PredTable, no).
pragma_add_marker([PredId | PredIds], UpdatePredInfo, Status, MustBeExported,
        !PredTable, WrongStatus) :-
    map.lookup(!.PredTable, PredId, PredInfo0),
    UpdatePredInfo(PredInfo0, PredInfo),
    (
        pred_info_is_exported(PredInfo),
        MustBeExported = yes,
        Status \= status_exported
    ->
        WrongStatus0 = yes
    ;
        WrongStatus0 = no
    ),
    map.det_update(PredId, PredInfo, !PredTable),
    pragma_add_marker(PredIds, UpdatePredInfo, Status, MustBeExported,
        !PredTable, WrongStatus1),
    bool.or(WrongStatus0, WrongStatus1, WrongStatus).

:- pred add_marker_pred_info(marker::in, pred_info::in, pred_info::out) is det.

add_marker_pred_info(Marker, !PredInfo) :-
    pred_info_get_markers(!.PredInfo, Markers0),
    add_marker(Marker, Markers0, Markers),
    pred_info_set_markers(Markers, !PredInfo).

:- pred get_matching_pred_ids(module_info::in, sym_name::in, arity::in,
    list(pred_id)::out) is det.

get_matching_pred_ids(Module0, Name, Arity, PredIds) :-
    module_info_get_predicate_table(Module0, PredTable0),
    % Check that the pragma is module qualified.
    (
        Name = unqualified(_),
        unexpected($module, $pred, "unqualified name")
    ;
        Name = qualified(_, _),
        predicate_table_lookup_sym_arity(PredTable0, is_fully_qualified,
            Name, Arity, PredIds)
    ).

:- pred pragma_status_error(sym_name::in, int::in, prog_context::in,
    string::in, list(error_spec)::in, list(error_spec)::out) is det.

pragma_status_error(Name, Arity, Context, PragmaName, !Specs) :-
    Pieces = [words("Error:"), pragma_decl(PragmaName),
        words("declaration for exported predicate or function"),
        sym_name_and_arity(Name / Arity),
        words("must also be exported."), nl],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

:- pred pragma_conflict_error(sym_name::in, int::in, prog_context::in,
    string::in, list(error_spec)::in, list(error_spec)::out) is det.

pragma_conflict_error(Name, Arity, Context, PragmaName, !Specs) :-
    Pieces = [words("Error:"), pragma_decl(PragmaName),
        words("declaration conflicts with previous pragma for"),
        sym_name_and_arity(Name / Arity), suffix("."), nl],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

%----------------------------------------------------------------------------%

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
                [always(Pieces), verbose_only(verbose_once, VerbosePieces)]),
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
                [always(Pieces), verbose_only(verbose_once, VerbosePieces)]),
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
                [always(Pieces), verbose_only(verbose_once, VerbosePieces)]),
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
            Pieces = [words("Error: this module must be compiled in a grade"),
                words("that uses conservative garbage collection."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        )
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

add_pass_3_pragma(ItemPragma, Status, !ModuleInfo, !QualInfo, !Specs) :-
    ItemPragma = item_pragma_info(Pragma, MaybeAttrs, Context, SeqNum),
    (
        Pragma = pragma_foreign_proc(FPInfo),
        add_pragma_foreign_proc(FPInfo, Status, Context, yes(SeqNum),
            !ModuleInfo, !Specs)
    ;
        Pragma = pragma_foreign_proc_export(FEInfo),
        add_pragma_foreign_proc_export(MaybeAttrs, FEInfo, Context,
            !ModuleInfo, !Specs)
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
            module_add_pragma_tabled(TabledInfo, Context, Status,
                !ModuleInfo, !QualInfo, !Specs)
        ;
            TypeLayout = no,
            TabledInfo = pragma_info_tabled(EvalMethod, _, _, _),
            Pieces = [words("Error:"),
                pragma_decl(eval_method_to_string(EvalMethod)),
                words("declaration requires type_ctor_layout structures."),
                words("Don't use --no-type-layout to disable them."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        )
    ;
        Pragma = pragma_fact_table(FTInfo),
        add_pragma_fact_table(FTInfo, Status, Context, !ModuleInfo, !Specs)
    ;
        Pragma = pragma_reserve_tag(TypeCtor),
        add_pragma_reserve_tag(TypeCtor, Status, Context, !ModuleInfo, !Specs)
    ;
        Pragma = pragma_oisu(OISUInfo),
        add_pragma_oisu(OISUInfo, Status, Context, !ModuleInfo, !Specs)
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
        ; Pragma = pragma_foreign_export_enum(_)
        ; Pragma = pragma_foreign_enum(_)
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
            [pragma_decl("fact_table"), words("declaration")], !Specs)
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
                    semipure io.unsafe_get_io_state(!:IO),
                    fact_table_compile_facts(Pred, Arity, FileName,
                        PredInfo0, PredInfo, Context, !.ModuleInfo,
                        C_HeaderCode, PrimaryProcId, !IO),
                    impure io.unsafe_set_io_state(!.IO)
                )
            ),

            module_info_set_pred_info(PredId, PredInfo, !ModuleInfo),
            pred_info_get_proc_table(PredInfo, ProcTable),
            pred_info_get_arg_types(PredInfo, ArgTypes),
            ProcIds = pred_info_procids(PredInfo),
            PredOrFunc = pred_info_is_pred_or_func(PredInfo),
            adjust_func_arity(PredOrFunc, Arity, NumArgs),

            % Create foreign_decls to declare extern variables.
            ForeignDeclCode = foreign_decl_code(lang_c, foreign_decl_is_local,
                literal(C_HeaderCode), Context),
            module_add_foreign_decl_code(ForeignDeclCode, !ModuleInfo),

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

    % Add a `pragma foreign_proc' for each mode of the fact table lookup
    % to the HLDS.
    %
    % `pragma fact_table's are represented in the HLDS by a
    % `pragma foreign_proc' for each mode of the predicate.
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
            semipure io.unsafe_get_io_state(!:IO),
            fact_table_generate_c_code(SymName, PragmaVars, ProcId,
                PrimaryProcId, ProcInfo, ArgTypes, !.ModuleInfo,
                C_ProcCode, C_ExtraCode, !IO),
            impure io.unsafe_set_io_state(!.IO)
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
        ForeignBodyCode = foreign_body_code(lang_c, literal(C_ExtraCode),
            Context),
        module_add_foreign_body_code(ForeignBodyCode, !ModuleInfo)
    ),

    % The C code for fact tables includes C labels. We cannot inline this code,
    % because if we did, the result would be duplicate labels in the generated
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
            ErrorPieces = [words("error:"), pragma_decl("reserve_tag"),
                words("declaration must have"),
                words("the same visibility as the type definition.")],
            MaybeError = yes({severity_error, ErrorPieces})
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
                    ErrorPieces = [words("warning: multiple"),
                        pragma_decl("reserved_tag"),
                        words("declarations for the same type."), nl],
                    MaybeError = yes({severity_warning, ErrorPieces})
                ;
                    MaybeError = no
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
                ErrorPieces = [words("error:"),
                    sym_name_and_arity(TypeName / TypeArity),
                    words("is not a discriminated union type."), nl],
                MaybeError = yes({severity_error, ErrorPieces})
            )
        )
    ;
        ErrorPieces = [words("error: undefined type"),
            sym_name_and_arity(TypeName / TypeArity), suffix("."), nl],
        MaybeError = yes({severity_error, ErrorPieces})
    ),
    (
        MaybeError = no
    ;
        MaybeError = yes({Severity, MaybeErrorPieces}),
        ContextPieces = [words("In"), pragma_decl("reserve_tag"),
            words("declaration for"), sym_name_and_arity(TypeName / TypeArity),
            suffix(":"), nl],
        Msg = simple_msg(Context, [always(ContextPieces ++ MaybeErrorPieces)]),
        Spec = error_spec(Severity, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ).

%-----------------------------------------------------------------------------%

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
                DupPieces = [words("Duplicate"), pragma_decl("oisu"),
                    words("declarations for"),
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
            words("within the"), pragma_decl("oisu"),
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
        pred_info_get_proc_table(PredInfo0, ProcTable0),
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
            pred_info_set_proc_table(ProcTable, PredInfo0, PredInfo),
            map.det_update(PredId, PredInfo, PredTable0, PredTable),
            module_info_set_preds(PredTable, !ModuleInfo)
        ;
            module_info_incr_errors(!ModuleInfo),
            Pieces = [words("Error:"), pragma_decl("termination_info"),
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
            words("in"), pragma_decl("termination_info"),
            words("declaration."), nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ).

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
        pred_info_get_proc_table(PredInfo0, ProcTable0),
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
            pred_info_set_proc_table(ProcTable, PredInfo0, PredInfo),
            map.det_update(PredId, PredInfo, PredTable0, PredTable),
            module_info_set_preds(PredTable, !ModuleInfo)
        ;
            Pieces = [words("Error:"), pragma_decl("termination2_info"),
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
            words("in"), pragma_decl("termination2_info"),
            words("declaration."), nl],
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
            pred_info_get_proc_table(PredInfo0, ProcTable0),
            map.to_assoc_list(ProcTable0, ProcList),
            (
                get_procedure_matching_declmodes_with_renaming(ProcList,
                    ModeList, !.ModuleInfo, ProcId)
            ->
                map.lookup(ProcTable0, ProcId, ProcInfo0),
                proc_info_set_imported_structure_sharing(HeadVars, Types,
                    SharingDomain, ProcInfo0, ProcInfo),
                map.det_update(ProcId, ProcInfo, ProcTable0, ProcTable),
                pred_info_set_proc_table(ProcTable, PredInfo0, PredInfo),
                map.det_update(PredId, PredInfo, PredTable0, PredTable),
                module_info_set_preds(PredTable, !ModuleInfo)
            ;
                Pieces = [words("Error:"), pragma_decl("structure_sharing"),
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
                words("in"), pragma_decl("structure_sharing."),
                words("declaration."), nl],
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
            pred_info_get_proc_table(PredInfo0, ProcTable0),
            map.to_assoc_list(ProcTable0, ProcList),
            (
                get_procedure_matching_declmodes_with_renaming(ProcList,
                    ModeList, !.ModuleInfo, ProcId)
            ->
                map.lookup(ProcTable0, ProcId, ProcInfo0),
                proc_info_set_imported_structure_reuse(HeadVars, Types,
                    ReuseDomain, ProcInfo0, ProcInfo),
                map.det_update(ProcId, ProcInfo, ProcTable0, ProcTable),
                pred_info_set_proc_table(ProcTable, PredInfo0, PredInfo),
                map.det_update(PredId, PredInfo, PredTable0, PredTable),
                module_info_set_preds(PredTable, !ModuleInfo)
            ;
                Pieces = [words("Error:"), pragma_decl("structure_reuse"),
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
                words("in"), pragma_decl("structure_reuse"),
                words("declaration."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        )
    ).

%----------------------------------------------------------------------------%
:- end_module hlds.make_hlds.add_pragma.
%----------------------------------------------------------------------------%
