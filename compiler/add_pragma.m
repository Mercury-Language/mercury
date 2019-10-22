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
:- import_module hlds.make_hlds.qual_info.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_item.

:- import_module list.

%-----------------------------------------------------------------------------%

:- inst item_pragma_info_inst(I) for item_pragma_info/0
    --->    item_pragma_info(I, ground, ground, ground).

:- inst pragma_pass_2_inst for pragma_type/0
    --->    pragma_foreign_decl(ground)
    ;       pragma_foreign_code(ground)
    ;       pragma_external_proc(ground)
    ;       pragma_inline(ground)
    ;       pragma_no_inline(ground)
    ;       pragma_consider_used(ground)
    ;       pragma_unused_args(ground)
    ;       pragma_exceptions(ground)
    ;       pragma_trailing_info(ground)
    ;       pragma_mm_tabling_info(ground)
    ;       pragma_obsolete_pred(ground)
    ;       pragma_obsolete_proc(ground)
    ;       pragma_no_detism_warning(ground)
    ;       pragma_require_tail_recursion(ground)
    ;       pragma_promise_eqv_clauses(ground)
    ;       pragma_promise_pure(ground)
    ;       pragma_promise_semipure(ground)
    ;       pragma_terminates(ground)
    ;       pragma_does_not_terminate(ground)
    ;       pragma_check_termination(ground)
    ;       pragma_mode_check_clauses(ground)
    ;       pragma_require_feature_set(ground).

:- inst pragma_pass_3_inst for pragma_type/0
    --->    pragma_foreign_proc(ground)
    ;       pragma_type_spec(ground)
    ;       pragma_tabled(ground)
    ;       pragma_fact_table(ground)
    ;       pragma_oisu(ground)
    ;       pragma_foreign_proc_export(ground)
    ;       pragma_termination_info(ground)
    ;       pragma_termination2_info(ground)
    ;       pragma_structure_sharing(ground)
    ;       pragma_structure_reuse(ground).

:- inst ims_pragma_pass_2 ==
    ims_item(item_pragma_info_inst(pragma_pass_2_inst)).
:- inst ims_pragma_pass_3 ==
    ims_item(item_pragma_info_inst(pragma_pass_3_inst)).

%-----------------------------------------------------------------------------%

:- pred add_pass_2_pragmas(
    list(ims_item(item_pragma_info))::in(list_skel(ims_pragma_pass_2)),
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_pass_3_pragmas(
    list(ims_item(item_pragma_info))::in(list_skel(ims_pragma_pass_3)),
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%

:- pred add_pragma_foreign_proc_export(item_maybe_attrs::in,
    pragma_info_foreign_proc_export::in, prog_context::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- include_module hlds.make_hlds.add_pragma.add_pragma_tabling.
:- include_module hlds.make_hlds.add_pragma.add_pragma_type_spec.

:- import_module hlds.hlds_code_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_pred.
:- import_module hlds.hlds_pred.
:- import_module hlds.make_hlds.add_foreign_proc.
:- import_module hlds.make_hlds.add_pragma.add_pragma_tabling.
:- import_module hlds.make_hlds.add_pragma.add_pragma_type_spec.
:- import_module hlds.make_hlds.make_hlds_warn.
:- import_module hlds.make_hlds_error.
:- import_module hlds.pred_table.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module ll_backend.
:- import_module ll_backend.fact_table.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.
:- import_module transform_hlds.
:- import_module transform_hlds.term_constr_main_types.
:- import_module transform_hlds.term_constr_util.
:- import_module transform_hlds.term_util.

:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module io.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module varset.

%-----------------------------------------------------------------------------%

add_pass_2_pragmas([], !ModuleInfo, !Specs).
add_pass_2_pragmas([SectionItem | SectionItems], !ModuleInfo, !Specs) :-
    add_pass_2_pragma(SectionItem, !ModuleInfo, !Specs),
    add_pass_2_pragmas(SectionItems, !ModuleInfo, !Specs).

add_pass_3_pragmas([], !ModuleInfo, !QualInfo, !Specs).
add_pass_3_pragmas([SectionItem | SectionItems],
        !ModuleInfo, !QualInfo, !Specs) :-
    add_pass_3_pragma(SectionItem, !ModuleInfo, !QualInfo, !Specs),
    add_pass_3_pragmas(SectionItems, !ModuleInfo, !QualInfo, !Specs).

%-----------------------------------------------------------------------------%

:- pred add_pass_2_pragma(ims_item(item_pragma_info)::in(ims_pragma_pass_2),
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pass_2_pragma(SectionItem, !ModuleInfo, !Specs) :-
    SectionItem = ims_item(ItemMercuryStatus, ItemPragmaInfo),
    report_if_pragma_is_wrongly_in_interface(ItemMercuryStatus, ItemPragmaInfo,
        !Specs),
    ItemPragmaInfo = item_pragma_info(Pragma, _MaybeAttrs, Context, _SeqNum),
    (
        Pragma = pragma_foreign_decl(FDInfo),
        % XXX STATUS Check ItemMercuryStatus
        FDInfo = pragma_info_foreign_decl(Lang, IsLocal, CHeader),
        ForeignDeclCode = foreign_decl_code(Lang, IsLocal, CHeader, Context),
        module_add_foreign_decl_code(ForeignDeclCode, !ModuleInfo)
    ;
        Pragma = pragma_foreign_code(FCInfo),
        % XXX STATUS Check ItemMercuryStatus
        FCInfo = pragma_info_foreign_code(Lang, BodyCode),
        warn_suspicious_foreign_code(Lang, BodyCode, Context, !Specs),
        ForeignBodyCode = foreign_body_code(Lang, BodyCode, Context),
        module_add_foreign_body_code(ForeignBodyCode, !ModuleInfo)
    ;
        Pragma = pragma_external_proc(ExternalInfo),
        % XXX STATUS Check ItemMercuryStatus
        ExternalInfo = pragma_info_external_proc(PredSymName, Arity, PorF,
            MaybeBackend),
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
            (
                PorF = pf_predicate,
                predicate_table_lookup_pred_sym_arity(PredicateTable0,
                    is_fully_qualified, PredSymName, Arity, PredIds),
                predicate_table_lookup_pred_sym(PredicateTable0,
                    is_fully_qualified, PredSymName, AllArityPredIds),
                MissingPieces = [decl("external_pred"), words("pragma")]
            ;
                PorF = pf_function,
                predicate_table_lookup_func_sym_arity(PredicateTable0,
                    is_fully_qualified, PredSymName, Arity, PredIds),
                predicate_table_lookup_func_sym(PredicateTable0,
                    is_fully_qualified, PredSymName, AllArityPredIds),
                MissingPieces = [decl("external_func"), words("pragma")]
            ),
            (
                PredIds = [_ | _],
                list.foldl(mark_pred_as_external, PredIds, !ModuleInfo)
            ;
                PredIds = [],
                module_info_get_preds(!.ModuleInfo, PredTable0),
                find_pred_arities_other_than(PredTable0, AllArityPredIds,
                    Arity, OtherArities),
                report_undefined_pred_or_func_error(yes(PorF), PredSymName,
                    Arity, OtherArities, Context, MissingPieces, !Specs)
            )
        else
            true
        )
    ;
        Pragma = pragma_inline(PredSymNameArity),
        item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
        % Note that mode_check_inline conflicts with inline because
        % it implies no_inline.
        add_pred_marker("inline", PredSymNameArity, PredStatus, Context,
            marker_user_marked_inline,
            [marker_user_marked_no_inline, marker_mode_check_clauses],
            !ModuleInfo, !Specs)
    ;
        Pragma = pragma_no_inline(PredSymNameArity),
        item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
        add_pred_marker("no_inline", PredSymNameArity, PredStatus, Context,
            marker_user_marked_no_inline, [marker_user_marked_inline],
            !ModuleInfo, !Specs)
    ;
        Pragma = pragma_consider_used(PredSymNameArity),
        item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
        add_pred_marker("consider_used", PredSymNameArity, PredStatus, Context,
            marker_consider_used, [], !ModuleInfo, !Specs)
    ;
        % Used for inter-module unused argument elimination.
        % This can only appear in .opt files.
        Pragma = pragma_unused_args(UnusedArgsInfo),
        ( if
            ItemMercuryStatus = item_defined_in_other_module(ItemImport),
            ItemImport = item_import_opt_int
        then
            add_pragma_unused_args(UnusedArgsInfo, Context,
                !ModuleInfo, !Specs)
        else
            Pieces = [words("Error:"), pragma_decl("unused_args"),
                words("declarations may appear only in"),
                words("automatically generated optimization files."), nl],
            Spec = simplest_spec(severity_error, phase_parse_tree_to_hlds,
                Context, Pieces),
            !:Specs = [Spec | !.Specs]
        )
    ;
        Pragma = pragma_exceptions(ExceptionsInfo),
        ( if
            ItemMercuryStatus = item_defined_in_other_module(ItemImport),
            ItemImport = item_import_opt_int
        then
            add_pragma_exceptions(ExceptionsInfo, Context, !ModuleInfo, !Specs)
        else
            Pieces = [words("Error:"), pragma_decl("exceptions"),
                words("declarations may appear only in"),
                words("automatically generated optimization files."), nl],
            Spec = simplest_spec(severity_error, phase_parse_tree_to_hlds,
                Context, Pieces),
            !:Specs = [Spec | !.Specs]
        )
    ;
        Pragma = pragma_trailing_info(TrailingInfo),
        ( if
            ItemMercuryStatus = item_defined_in_other_module(ItemImport),
            ItemImport = item_import_opt_int
        then
            add_pragma_trailing_info(TrailingInfo, Context,
                !ModuleInfo, !Specs)
        else
            Pieces = [words("Error:"), pragma_decl("trailing_info"),
                words("declarations may appear only in"),
                words("automatically generated optimization files."), nl],
            Spec = simplest_spec(severity_error, phase_parse_tree_to_hlds,
                Context, Pieces),
            !:Specs = [Spec | !.Specs]
        )
    ;
        Pragma = pragma_mm_tabling_info(MMTablingInfo),
        ( if
            ItemMercuryStatus = item_defined_in_other_module(ItemImport),
            ItemImport = item_import_opt_int
        then
            add_pragma_mm_tabling_info(MMTablingInfo, Context,
                !ModuleInfo, !Specs)
        else
            Pieces = [words("Error:"), pragma_decl("mm_tabling_info"),
                words("declarations may appear only in"),
                words("automatically generated optimization files."), nl],
            Spec = simplest_spec(severity_error, phase_parse_tree_to_hlds,
                Context, Pieces),
            !:Specs = [Spec | !.Specs]
        )
    ;
        Pragma = pragma_obsolete_pred(ObsoletePredInfo),
        item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
        mark_pred_as_obsolete(ObsoletePredInfo, PredStatus, Context,
            !ModuleInfo, !Specs)
    ;
        Pragma = pragma_obsolete_proc(ObsoleteProcInfo),
        item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
        mark_proc_as_obsolete(ObsoleteProcInfo, PredStatus, Context,
            !ModuleInfo, !Specs)
    ;
        Pragma = pragma_no_detism_warning(PredNameArity),
        item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
        add_pred_marker("no_determinism_warning", PredNameArity, PredStatus,
            Context, marker_no_detism_warning, [], !ModuleInfo, !Specs)
    ;
        Pragma = pragma_require_tail_recursion(TailrecWarningPragma),
        add_pragma_require_tail_recursion(TailrecWarningPragma, Context,
            !ModuleInfo, !Specs)
    ;
        Pragma = pragma_promise_eqv_clauses(PredNameArity),
        item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
        add_pred_marker("promise_equivalent_clauses", PredNameArity,
            PredStatus, Context, marker_promised_equivalent_clauses, [],
            !ModuleInfo, !Specs)
    ;
        Pragma = pragma_promise_pure(PredNameArity),
        item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
        add_pred_marker("promise_pure", PredNameArity, PredStatus,
            Context, marker_promised_pure, [], !ModuleInfo, !Specs)
    ;
        Pragma = pragma_promise_semipure(PredNameArity),
        item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
        add_pred_marker("promise_semipure", PredNameArity, PredStatus,
            Context, marker_promised_semipure, [], !ModuleInfo, !Specs)
    ;
        Pragma = pragma_terminates(PredNameArity),
        item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
        add_pred_marker("terminates", PredNameArity, PredStatus, Context,
            marker_terminates,
            [marker_check_termination, marker_does_not_terminate],
            !ModuleInfo, !Specs)
    ;
        Pragma = pragma_does_not_terminate(PredNameArity),
        item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
        add_pred_marker("does_not_terminate", PredNameArity, PredStatus,
            Context, marker_does_not_terminate,
            [marker_check_termination, marker_terminates], !ModuleInfo, !Specs)
    ;
        Pragma = pragma_check_termination(PredNameArity),
        item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
        add_pred_marker("check_termination", PredNameArity, PredStatus,
            Context, marker_check_termination,
            [marker_terminates, marker_does_not_terminate],
            !ModuleInfo, !Specs)
    ;
        Pragma = pragma_mode_check_clauses(PredNameArity),
        item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
        add_pred_marker("mode_check_clauses", PredNameArity, PredStatus,
            Context, marker_mode_check_clauses, [], !ModuleInfo, !Specs),

        % Allowing the predicate to be inlined could lead to code generator
        % aborts. This is because the caller that inlines this predicate may
        % then push other code into the disjunction or switch's branches,
        % which would invalidate the instmap_deltas that the mode_check_clauses
        % marker prevents the recomputation of.
        add_pred_marker("mode_check_clauses", PredNameArity, PredStatus,
            Context, marker_user_marked_no_inline, [marker_user_marked_inline],
            !ModuleInfo, !Specs)
    ;
        Pragma = pragma_require_feature_set(RFSInfo),
        RFSInfo = pragma_info_require_feature_set(FeatureSet),
        check_required_feature_set(FeatureSet, ItemMercuryStatus, Context,
            !ModuleInfo, !Specs)
    ).

%-----------------------------------------------------------------------------%

:- pred mark_pred_as_external(pred_id::in,
    module_info::in, module_info::out) is det.

mark_pred_as_external(PredId, !ModuleInfo) :-
    module_info_get_preds(!.ModuleInfo, PredTable0),
    map.lookup(PredTable0, PredId, PredInfo0),
    pred_info_mark_as_external(PredInfo0, PredInfo),
    map.det_update(PredId, PredInfo, PredTable0, PredTable),
    module_info_set_preds(PredTable, !ModuleInfo).

%-----------------------------------------------------------------------------%

:- pred add_pragma_unused_args(pragma_info_unused_args::in,
    prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_unused_args(UnusedArgsInfo, Context, !ModuleInfo, !Specs) :-
    UnusedArgsInfo = pragma_info_unused_args(PredNameArityPFMn, UnusedArgs),
    PredNameArityPFMn = pred_name_arity_pf_mn(SymName, Arity, PredOrFunc,
        ModeNum),
    look_up_pragma_pf_sym_arity(!.ModuleInfo, is_fully_qualified,
        lfh_internal_error, Context, "unused_args",
        PredOrFunc, SymName, Arity, MaybePredId),
    (
        MaybePredId = ok1(PredId),
        module_info_get_unused_arg_info(!.ModuleInfo, UnusedArgInfo0),
        % Convert the mode number to a proc_id.
        proc_id_to_int(ProcId, ModeNum),
        map.set(proc(PredId, ProcId), UnusedArgs,
            UnusedArgInfo0, UnusedArgInfo),
        module_info_set_unused_arg_info(UnusedArgInfo, !ModuleInfo)
    ;
        MaybePredId = error1(Specs),
        !:Specs = Specs ++ !.Specs
    ).

%-----------------------------------------------------------------------------%

:- pred add_pragma_exceptions(pragma_info_exceptions::in, prog_context::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_exceptions(ExceptionsInfo, Context, !ModuleInfo, !Specs) :-
    ExceptionsInfo = pragma_info_exceptions(PredNameArityPFMn, ThrowStatus),
    PredNameArityPFMn = pred_name_arity_pf_mn(SymName, Arity, PredOrFunc,
        ModeNum),
    look_up_pragma_pf_sym_arity(!.ModuleInfo, is_fully_qualified,
        lfh_ignore, Context, "exceptions",
        PredOrFunc, SymName, Arity, MaybePredId),
    (
        MaybePredId = ok1(PredId),
        proc_id_to_int(ProcId, ModeNum),
        module_info_pred_proc_info(!.ModuleInfo, PredId, ProcId,
            PredInfo0, ProcInfo0),
        ProcExceptionInfo = proc_exception_info(ThrowStatus, no),
        proc_info_set_exception_info(yes(ProcExceptionInfo),
            ProcInfo0, ProcInfo),
        module_info_set_pred_proc_info(PredId, ProcId, PredInfo0, ProcInfo,
            !ModuleInfo)
    ;
        MaybePredId = error1(_Specs)
        % XXX We'll just ignore this for the time being -
        % it causes errors with transitive-intermodule optimization.
        % XXX What kinds of errors?
    ).

%-----------------------------------------------------------------------------%

:- pred add_pragma_trailing_info(pragma_info_trailing_info::in,
    prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_trailing_info(TrailingInfo, Context, !ModuleInfo, !Specs) :-
    TrailingInfo = pragma_info_trailing_info(PredNameArityPFMn,
        TrailingStatus),
    PredNameArityPFMn = pred_name_arity_pf_mn(SymName, Arity, PredOrFunc,
        ModeNum),
    look_up_pragma_pf_sym_arity(!.ModuleInfo, is_fully_qualified,
        lfh_ignore, Context, "trailing_info",
        PredOrFunc, SymName, Arity, MaybePredId),
    (
        MaybePredId = ok1(PredId),
        proc_id_to_int(ProcId, ModeNum),
        module_info_pred_proc_info(!.ModuleInfo, PredId, ProcId,
            PredInfo0, ProcInfo0),
        ProcTrailingInfo = proc_trailing_info(TrailingStatus, no),
        proc_info_set_trailing_info(yes(ProcTrailingInfo),
            ProcInfo0, ProcInfo),
        module_info_set_pred_proc_info(PredId, ProcId, PredInfo0, ProcInfo,
            !ModuleInfo)
    ;
        MaybePredId = error1(_Specs)
        % XXX We'll just ignore this for the time being -
        % it causes errors with transitive-intermodule optimization.
        % XXX What kinds of errors?
    ).

%-----------------------------------------------------------------------------%

:- pred add_pragma_mm_tabling_info(pragma_info_mm_tabling_info::in,
    prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_mm_tabling_info(MMTablingInfo, Context, !ModuleInfo, !Specs) :-
    MMTablingInfo = pragma_info_mm_tabling_info(PredNameArityPFMn,
        TablingStatus),
    PredNameArityPFMn = pred_name_arity_pf_mn(SymName, Arity, PredOrFunc,
        ModeNum),
    look_up_pragma_pf_sym_arity(!.ModuleInfo, is_fully_qualified,
        lfh_ignore, Context, "mm_tabling_info",
        PredOrFunc, SymName, Arity, MaybePredId),
    (
        MaybePredId = ok1(PredId),
        proc_id_to_int(ProcId, ModeNum),
        module_info_pred_proc_info(!.ModuleInfo, PredId, ProcId,
            PredInfo0, ProcInfo0),
        ProcMMTablingInfo = proc_mm_tabling_info(TablingStatus, no),
        proc_info_set_mm_tabling_info(yes(ProcMMTablingInfo),
            ProcInfo0, ProcInfo),
        module_info_set_pred_proc_info(PredId, ProcId, PredInfo0, ProcInfo,
            !ModuleInfo)
    ;
        MaybePredId = error1(_Specs)
        % XXX We'll just ignore this for the time being -
        % it causes errors with transitive-intermodule optimization.
        % XXX What kinds of errors?
    ).

%-----------------------------------------------------------------------------%

:- pred add_pragma_require_tail_recursion(
    pragma_info_require_tail_recursion::in, prog_context::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_require_tail_recursion(Pragma, Context, !ModuleInfo, !Specs) :-
    Pragma ^ rtr_proc_id =
        pred_name_arity_mpf_mmode(PredSymName, Arity, MaybePF, MaybeMode),
    get_matching_pred_ids(!.ModuleInfo, PredSymName, Arity,
        PredIds, OtherArities),
    (
        PredIds = [],
        Pieces = [pragma_decl("require_tail_recursion"), words("pragma")],
        report_undefined_pred_or_func_error(MaybePF, PredSymName, Arity,
            OtherArities, Context, Pieces, !Specs)
    ;
        PredIds = [PredId],
        PredSymNameArity = sym_name_arity(PredSymName, Arity),

        module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
        pred_info_get_proc_table(PredInfo0, Procs0),
        map.to_assoc_list(Procs0, Procs),
        (
            MaybeMode = yes(Mode),
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
                    Procs, Mode, ProcId)
            then
                map.lookup(Procs0, ProcId, Proc),
                RequireTailrec = Pragma ^ rtr_require_tailrec,
                add_pragma_require_tail_recursion_proc(RequireTailrec, Context,
                    PredSymNameArity, ProcId - Proc,
                    PredInfo0, PredInfo, !Specs)
            else
                PredInfo = PredInfo0,
                PredOrFunc = pred_info_is_pred_or_func(PredInfo0),
                SimpleCallId = simple_call_id(PredOrFunc, PredSymName, Arity),
                Pieces = [words("Error:"),
                    pragma_decl("require_tail_recursion"),
                    words("declaration for undeclared mode of"),
                    simple_call(SimpleCallId), suffix("."), nl],
                Spec = simplest_spec(severity_error, phase_parse_tree_to_hlds,
                    Context, Pieces),
                !:Specs = [Spec | !.Specs]
            )
        ;
            MaybeMode = no,
            list.foldl2(
                add_pragma_require_tail_recursion_proc(
                    Pragma ^ rtr_require_tailrec, Context, PredSymNameArity),
                Procs, PredInfo0, PredInfo, !Specs)
        ),
        module_info_set_pred_info(PredId, PredInfo, !ModuleInfo)
    ;
        PredIds = [_, _ | _],
        Pieces = [words("Error: ambiguous predicate or function in"),
            pragma_decl("require_tail_recursion"), words("pragma."), nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ).

:- pred add_pragma_require_tail_recursion_proc(
    require_tail_recursion::in, prog_context::in,
    sym_name_and_arity::in, pair(proc_id, proc_info)::in,
    pred_info::in, pred_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_require_tail_recursion_proc(RequireTailrec, Context,
        SymNameAndArity, ProcId - ProcInfo0, !PredInfo, !Specs) :-
    proc_info_get_maybe_require_tailrec_info(ProcInfo0,
        MaybeRequireTailrecOrig),
    (
        MaybeRequireTailrecOrig = yes(RequireTailrecOrig),
        MainPieces = [words("Error: conflicting"),
            pragma_decl("require_tail_recursion"), words("pragmas for"),
            qual_sym_name_and_arity(SymNameAndArity),
            words("or one of its modes.")],
        OrigPieces = [words("Earlier pragma is here.")],
        ( RequireTailrecOrig = suppress_tailrec_warnings(ContextOrig)
        ; RequireTailrecOrig = enable_tailrec_warnings(_, _, ContextOrig)
        ),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
            [simplest_msg(Context, MainPieces),
             simplest_msg(ContextOrig, OrigPieces)]),
        !:Specs = [Spec | !.Specs]
    ;
        MaybeRequireTailrecOrig = no,
        proc_info_set_require_tailrec_info(RequireTailrec,
            ProcInfo0, ProcInfo),
        pred_info_set_proc_info(ProcId, ProcInfo, !PredInfo)
    ).

%-----------------------------------------------------------------------------%

    % add_pred_marker(PragmaName, PredNameArity, Status, Context,
    %   Marker, ConflictMarkers, !ModuleInfo, !Specs):
    %
    % Adds Marker to the marker list of the pred(s) with the given
    % PredNameArity, updating the ModuleInfo. If the named pred does not exist,
    % or the pred(s) already has/have a marker in ConflictMarkers,
    % report an error.
    %
:- pred add_pred_marker(string::in, pred_name_arity::in, pred_status::in,
    prog_context::in, pred_marker::in, list(pred_marker)::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pred_marker(PragmaName, PredSymNameArity, Status, Context,
        Marker, ConflictMarkers, !ModuleInfo, !Specs) :-
    ( if marker_must_be_exported(Marker) then
        MustBeExported = yes
    else
        MustBeExported = no
    ),
    do_add_pred_marker(PragmaName, PredSymNameArity, Status, MustBeExported,
        Context, add_marker_pred_info(Marker), !ModuleInfo, PredIds, !Specs),
    module_info_get_preds(!.ModuleInfo, PredTable),
    list.map(get_pred_markers(PredTable), PredIds, PredMarkerSets),
    PredMarkers = set.union_list(PredMarkerSets),
    set.intersect(PredMarkers, set.list_to_set(ConflictMarkers),
        ConflictingPredMarkerSet),
    set.to_sorted_list(ConflictingPredMarkerSet, ConflictingPredMarkers0),
    (
        ConflictingPredMarkers0 = [_ | _],
        ( if
            list.member(marker_mode_check_clauses, ConflictingPredMarkers0),
            list.member(marker_user_marked_no_inline, ConflictingPredMarkers0)
        then
            % The no_inline marker would have been added implicitly
            % for the mode_check_clauses pragma. In the usual case where
            % the programmer didn't also add an explicit no_inline pragma,
            % mentioning the conflict with no_inline would be more confusing
            % than helpful.
            list.delete_all(ConflictingPredMarkers0,
                marker_user_marked_no_inline, ConflictingPredMarkers)
        else
            ConflictingPredMarkers = ConflictingPredMarkers0
        ),
        pragma_conflict_error(PredSymNameArity, Context, PragmaName,
            ConflictingPredMarkers, !Specs)
    ;
        ConflictingPredMarkers0 = []
    ).

    % Succeed if a marker for an exported procedure must also be exported.
    %
:- pred marker_must_be_exported(pred_marker::in) is semidet.

marker_must_be_exported(_) :-
    semidet_fail.

:- type add_marker_pred_info == pred(pred_info, pred_info).
:- inst add_marker_pred_info == (pred(in, out) is det).

:- pred do_add_pred_marker(string::in, pred_name_arity::in,
    pred_status::in, bool::in, term.context::in,
    add_marker_pred_info::in(add_marker_pred_info),
    module_info::in, module_info::out, list(pred_id)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

do_add_pred_marker(PragmaName, PredSymNameArity, Status, MustBeExported,
        Context, UpdatePredInfo, !ModuleInfo, PredIds, !Specs) :-
    PredSymNameArity = pred_name_arity(PredSymName, Arity),
    get_matching_pred_ids(!.ModuleInfo, PredSymName, Arity, PredIds,
        OtherArities),
    (
        PredIds = [_ | _],
        module_info_get_predicate_table(!.ModuleInfo, PredTable0),
        predicate_table_get_preds(PredTable0, Preds0),
        pragma_add_marker(PredIds, UpdatePredInfo, Status,
            MustBeExported, Preds0, Preds, WrongStatus),
        (
            WrongStatus = yes,
            pragma_status_error(PredSymNameArity, Context, PragmaName, !Specs)
        ;
            WrongStatus = no
        ),
        predicate_table_set_preds(Preds, PredTable0, PredTable),
        module_info_set_predicate_table(PredTable, !ModuleInfo)
    ;
        PredIds = [],
        DescPieces = [pragma_decl(PragmaName), words("declaration")],
        report_undefined_pred_or_func_error(no, PredSymName, Arity,
            OtherArities, Context, DescPieces, !Specs)
    ).

    % For each pred_id in the list, check whether markers present in the list
    % of conflicting markers are also present in the corresponding pred_info.
    % The output is a set of the names of the conflicting markers present.
    %
:- pred get_pred_markers(pred_table::in, pred_id::in,
    set(pred_marker)::out) is det.

get_pred_markers(PredTable, PredId, Markers) :-
    map.lookup(PredTable, PredId, PredInfo),
    pred_info_get_markers(PredInfo, Markers).

    % For each pred_id in the list, add the given markers to the
    % list of markers in the corresponding pred_info.
    %
:- pred pragma_add_marker(list(pred_id)::in,
    add_marker_pred_info::in(add_marker_pred_info), pred_status::in,
    bool::in, pred_table::in, pred_table::out, bool::out) is det.

pragma_add_marker([], _, _, _, !PredTable, no).
pragma_add_marker([PredId | PredIds], UpdatePredInfo, Status, MustBeExported,
        !PredTable, WrongStatus) :-
    map.lookup(!.PredTable, PredId, PredInfo0),
    UpdatePredInfo(PredInfo0, PredInfo),
    ( if
        pred_info_is_exported(PredInfo),
        MustBeExported = yes,
        Status \= pred_status(status_exported)
    then
        WrongStatus0 = yes
    else
        WrongStatus0 = no
    ),
    map.det_update(PredId, PredInfo, !PredTable),
    pragma_add_marker(PredIds, UpdatePredInfo, Status, MustBeExported,
        !PredTable, WrongStatus1),
    bool.or(WrongStatus0, WrongStatus1, WrongStatus).

:- pred add_marker_pred_info(pred_marker::in, pred_info::in, pred_info::out)
    is det.

add_marker_pred_info(Marker, !PredInfo) :-
    pred_info_get_markers(!.PredInfo, Markers0),
    add_marker(Marker, Markers0, Markers),
    pred_info_set_markers(Markers, !PredInfo).

    % Given a symname and arity, return (in PredIds) the predicates and
    % functions with that name and arity. A reference to SymName/Arity
    % could have meant any of the entries in PredIds. If PredIds is empty,
    % then we will want to generate an error message. This message should
    % mention that while given name does not exist with the given arity,
    % it does exist with some other arity, so we also return (in OtherArities)
    % the arities of all the predicates and functions with that name
    % but with some *other* arity.
    %
:- pred get_matching_pred_ids(module_info::in, sym_name::in, arity::in,
    list(pred_id)::out, list(int)::out) is det.

get_matching_pred_ids(ModuleInfo, SymName, Arity, PredIds, OtherArities) :-
    module_info_get_predicate_table(ModuleInfo, PredTable0),
    % Check that the pragma is module qualified.
    (
        SymName = unqualified(_),
        unexpected($pred, "unqualified name")
    ;
        SymName = qualified(_, _),
        predicate_table_lookup_sym_arity(PredTable0, is_fully_qualified,
            SymName, Arity, PredIds),
        (
            PredIds = [],
            predicate_table_lookup_sym(PredTable0, is_fully_qualified,
                SymName, SymOnlyPredIds),
            module_info_get_preds(ModuleInfo, Preds0),
            find_pred_arities_other_than(Preds0, SymOnlyPredIds,
                Arity, OtherArities)
        ;
            PredIds = [_ | _],
            % There is no point in filling this in; our caller won't need it.
            OtherArities = []
        )
    ).

:- pred pragma_status_error(pred_name_arity::in, prog_context::in,
    string::in, list(error_spec)::in, list(error_spec)::out) is det.

pragma_status_error(PredSymNameArity, Context, PragmaName, !Specs) :-
    PredSymNameArity = pred_name_arity(PredSymName, Arity),
    Pieces = [words("Error:"), pragma_decl(PragmaName),
        words("declaration for exported predicate or function"),
        unqual_sym_name_and_arity(sym_name_arity(PredSymName, Arity)),
        words("must also be exported."), nl],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

:- pred pragma_conflict_error(pred_name_arity::in, prog_context::in,
    string::in, list(pred_marker)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

pragma_conflict_error(PredSymNameArity, Context, PragmaName, ConflictMarkers,
        !Specs) :-
    PredSymNameArity = pred_name_arity(PredSymName, Arity),
    list.map(marker_name, ConflictMarkers, ConflictNames),
    Pieces = [words("Error:"), pragma_decl(PragmaName),
        words("declaration conflicts with previous")] ++
        list_to_pieces(ConflictNames) ++
        [words(choose_number(ConflictNames, "pragma for", "pragmas for")),
        unqual_sym_name_and_arity(sym_name_arity(PredSymName, Arity)),
        suffix("."), nl],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

%----------------------------------------------------------------------------%

:- pred mark_pred_as_obsolete(pragma_info_obsolete_pred::in,
    pred_status::in, prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

mark_pred_as_obsolete(ObsoletePredInfo, PragmaStatus, Context,
        !ModuleInfo, !Specs) :-
    ObsoletePredInfo =
        pragma_info_obsolete_pred(PredNameArity, ObsoleteInFavourOf),
    PredNameArity = pred_name_arity(SymName, Arity),
    get_matching_pred_ids(!.ModuleInfo, SymName, Arity, PredIds, OtherArities),
    (
        PredIds = [_ | _],
        module_info_get_predicate_table(!.ModuleInfo, PredTable0),
        predicate_table_get_preds(PredTable0, Preds0),
        mark_pred_ids_as_obsolete(ObsoleteInFavourOf, PragmaStatus, PredIds,
            no, WrongStatus, Preds0, Preds),
        (
            WrongStatus = yes,
            pragma_status_error(PredNameArity, Context, "obsolete", !Specs)
        ;
            WrongStatus = no
        ),
        predicate_table_set_preds(Preds, PredTable0, PredTable),
        module_info_set_predicate_table(PredTable, !ModuleInfo)
    ;
        PredIds = [],
        DescPieces = [pragma_decl("obsolete"), words("declaration")],
        report_undefined_pred_or_func_error(no, SymName, Arity, OtherArities,
            Context, DescPieces, !Specs)
    ).

:- pred mark_pred_ids_as_obsolete(list(sym_name_and_arity)::in,
    pred_status::in, list(pred_id)::in, bool::in, bool::out,
    pred_table::in, pred_table::out) is det.

mark_pred_ids_as_obsolete(_, _, [], !WrongStatus, !PredTable).
mark_pred_ids_as_obsolete(ObsoleteInFavourOf, PragmaStatus, [PredId | PredIds],
        !WrongStatus, !PredTable) :-
    map.lookup(!.PredTable, PredId, PredInfo0),
    ( if
        pred_info_is_exported(PredInfo0),
        pred_status_is_exported(PragmaStatus) = no
    then
        !:WrongStatus = yes
    else
        true
    ),
    pred_info_get_obsolete_in_favour_of(PredInfo0, MaybeObsoleteInFavourOf0),
    (
        MaybeObsoleteInFavourOf0 = no,
        MaybeObsoleteInFavourOf = yes(ObsoleteInFavourOf)
    ;
        MaybeObsoleteInFavourOf0 = yes(ObsoleteInFavourOf0),
        MaybeObsoleteInFavourOf = yes(ObsoleteInFavourOf0 ++ ObsoleteInFavourOf)
    ),
    pred_info_set_obsolete_in_favour_of(MaybeObsoleteInFavourOf,
        PredInfo0, PredInfo),
    map.det_update(PredId, PredInfo, !PredTable),
    mark_pred_ids_as_obsolete(ObsoleteInFavourOf, PragmaStatus, PredIds,
        !WrongStatus, !PredTable).

%----------------------------------------------------------------------------%

:- pred mark_proc_as_obsolete(pragma_info_obsolete_proc::in,
    pred_status::in, prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

mark_proc_as_obsolete(ObsoleteProcInfo, PragmaStatus, Context,
        !ModuleInfo, !Specs) :-
    ObsoleteProcInfo =
        pragma_info_obsolete_proc(PredNameModesPF, ObsoleteInFavourOf),
    PredNameModesPF = pred_name_modes_pf(SymName, Modes, PredOrFunc),
    list.length(Modes, Arity),
    look_up_pragma_pf_sym_arity(!.ModuleInfo, is_fully_qualified,
        lfh_user_error, Context, "obsolete_proc",
        PredOrFunc, SymName, Arity, MaybePredId),
    (
        MaybePredId = ok1(PredId),
        module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
        ( if
            pred_info_is_exported(PredInfo0),
            pred_status_is_exported(PragmaStatus) = no
        then
            PredNameArity = pred_name_arity(SymName, Arity),
            pragma_status_error(PredNameArity, Context, "obsolete_proc",
                !Specs)
        else
            true
        ),
        SimpleCallId = simple_call_id(PredOrFunc, SymName, Arity),
        ProcTransform =
            ( pred(ProcInfo0::in, ProcInfo::out) is det :-
                proc_info_get_obsolete_in_favour_of(ProcInfo0,
                    MaybeObsoleteInFavourOf0),
                (
                    MaybeObsoleteInFavourOf0 = no,
                    MaybeObsoleteInFavourOf = yes(ObsoleteInFavourOf)
                ;
                    MaybeObsoleteInFavourOf0 = yes(ObsoleteInFavourOf0),
                    MaybeObsoleteInFavourOf =
                        yes(ObsoleteInFavourOf0 ++ ObsoleteInFavourOf)
                ),
                proc_info_set_obsolete_in_favour_of(MaybeObsoleteInFavourOf,
                    ProcInfo0, ProcInfo)
            ),
        transform_selected_mode_of_pred(PredId, SimpleCallId, Modes,
            "obsolete_proc", Context, ProcTransform, !ModuleInfo, !Specs)
    ;
        MaybePredId = error1(Specs),
        !:Specs = Specs ++ !.Specs
    ).

%----------------------------------------------------------------------------%

:- pred check_required_feature_set(set(required_feature)::in,
    item_mercury_status::in, prog_context::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_required_feature_set(FeatureSet, ItemMercuryStatus, Context,
        !ModuleInfo, !Specs) :-
    (
        ItemMercuryStatus = item_defined_in_other_module(_),
        % `require_feature_set' pragmas are not included in interface files
        % (including private interfaces) and so this case should not occur.
        unexpected($pred, "imported require_feature_set pragma")
    ;
        ItemMercuryStatus = item_defined_in_this_module(_),
        module_info_get_globals(!.ModuleInfo, Globals),
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
        ( if
            ReorderConj = no,
            ReorderDisj = no,
            FullyStrict = yes
        then
            true
        else
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

:- pred add_pass_3_pragma(ims_item(item_pragma_info)::in(ims_pragma_pass_3),
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pass_3_pragma(SectionItem, !ModuleInfo, !QualInfo, !Specs) :-
    SectionItem = ims_item(ItemMercuryStatus, ItemPragmaInfo),
    report_if_pragma_is_wrongly_in_interface(ItemMercuryStatus, ItemPragmaInfo,
        !Specs),
    ItemPragmaInfo = item_pragma_info(Pragma, MaybeAttrs, Context, SeqNum),
    (
        Pragma = pragma_foreign_proc(FPInfo),
        item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
        add_pragma_foreign_proc(FPInfo, PredStatus, Context, yes(SeqNum),
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
            item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
            module_add_pragma_tabled(TabledInfo, Context, PredStatus,
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
        item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
        add_pragma_fact_table(FTInfo, PredStatus, Context, !ModuleInfo, !Specs)
    ;
        Pragma = pragma_oisu(OISUInfo),
        add_pragma_oisu(OISUInfo, ItemMercuryStatus, Context,
            !ModuleInfo, !Specs)
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
    ).

%---------------------------------------------------------------------------%

add_pragma_foreign_proc_export(MaybeAttrs, FPEInfo, Context,
        !ModuleInfo, !Specs) :-
    FPEInfo = pragma_info_foreign_proc_export(Lang, PrednameModesPF,
        ExportedName),
    PrednameModesPF = pred_name_modes_pf(SymName, Modes, PredOrFunc),
    list.length(Modes, Arity),
    look_up_pragma_pf_sym_arity(!.ModuleInfo, may_be_partially_qualified,
        lfh_user_error, Context, "foreign_export",
        PredOrFunc, SymName, Arity, MaybePredId),
    (
        MaybePredId = ok1(PredId),
        module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
        % predicate_table_get_preds(PredTable, Preds),
        % map.lookup(Preds, PredId, PredInfo),
        pred_info_get_proc_table(PredInfo, Procs),
        map.to_assoc_list(Procs, ExistingProcs),
        ( if
            get_procedure_matching_declmodes_with_renaming(!.ModuleInfo,
                ExistingProcs, Modes, ProcId)
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
                Pieces = [words("Error:"),
                    pragma_decl("foreign_export"), words("declaration"),
                    words("for a procedure that has"),
                    words("a declared determinism of"),
                    fixed(determinism_to_string(Detism)), suffix("."), nl],
                Spec = simplest_spec(severity_error, phase_parse_tree_to_hlds,
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
                MaybeAttrs = item_origin_user,
                report_undefined_mode_error(SymName, Arity, Context,
                    [pragma_decl("foreign_export"), words("declaration")],
                    !Specs)
            ;
                MaybeAttrs = item_origin_compiler(_CompilerAttrs)
                % We do not warn about errors in export pragmas created by
                % the compiler as part of a source-to-source transformation.
            )
        )
    ;
        MaybePredId = error1(Specs),
        (
            MaybeAttrs = item_origin_user,
            !:Specs = Specs ++ !.Specs
        ;
            MaybeAttrs = item_origin_compiler(_CompilerAttrs)
            % We do not warn about errors in export pragmas created by
            % the compiler as part of a source-to-source transformation.
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
    pred_status::in, prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_fact_table(FTInfo, PredStatus, Context, !ModuleInfo, !Specs) :-
    FTInfo = pragma_info_fact_table(PredSymNameArity, FileName),
    PredSymNameArity = pred_name_arity(PredSymName, Arity),
    get_matching_pred_ids(!.ModuleInfo, PredSymName, Arity, PredIds,
        OtherArities),
    (
        PredIds = [],
        report_undefined_pred_or_func_error(no, PredSymName, Arity,
            OtherArities, Context,
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
                    fact_table_compile_facts(PredSymName, Arity, FileName,
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
                floi_literal(C_HeaderCode), Context),
            module_add_foreign_decl_code(ForeignDeclCode, !ModuleInfo),

            module_add_fact_table_file(FileName, !ModuleInfo),

            % Create foreign_procs to access the table in each mode.
            add_fact_table_procedures(ProcIds, PrimaryProcId,
                ProcTable, PredSymName, PredOrFunc, NumArgs, ArgTypes,
                PredStatus, Context, !ModuleInfo, !Specs)
        ;
            TailPredIds = [_ | _],     % >1 predicate found
            Pieces = [words("In"), quote("pragma fact_table"), words("for"),
                qual_sym_name_and_arity(sym_name_arity(PredSymName, Arity)),
                suffix(":"), nl,
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
    list(mer_type)::in, pred_status::in, prog_context::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_fact_table_procedures([],_,_,_,_,_,_,_,_, !ModuleInfo, !Specs).
add_fact_table_procedures([ProcId | ProcIds], PrimaryProcId, ProcTable,
        SymName, PredOrFunc, Arity, ArgTypes, PredStatus, Context,
        !ModuleInfo, !Specs) :-
    add_fact_table_proc(ProcId, PrimaryProcId, ProcTable, SymName,
        PredOrFunc, Arity, ArgTypes, PredStatus, Context,
        !ModuleInfo, !Specs),
    add_fact_table_procedures(ProcIds, PrimaryProcId, ProcTable,
        SymName, PredOrFunc, Arity, ArgTypes, PredStatus, Context,
        !ModuleInfo, !Specs).

:- pred add_fact_table_proc(proc_id::in, proc_id::in, proc_table::in,
    sym_name::in, pred_or_func::in, arity::in, list(mer_type)::in,
    pred_status::in, prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_fact_table_proc(ProcId, PrimaryProcId, ProcTable, SymName,
        PredOrFunc, Arity, ArgTypes, PredStatus, Context,
        !ModuleInfo, !Specs) :-
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
    add_pragma_foreign_proc(FCInfo, PredStatus, Context, MaybeItemNumber,
        !ModuleInfo, !Specs),
    ( if C_ExtraCode = "" then
        true
    else
        ForeignBodyCode = foreign_body_code(lang_c, floi_literal(C_ExtraCode),
            Context),
        module_add_foreign_body_code(ForeignBodyCode, !ModuleInfo)
    ),

    % The C code for fact tables includes C labels. We cannot inline this code,
    % because if we did, the result would be duplicate labels in the generated
    % code. So we must disable inlining for fact_table procedures.
    add_pred_marker("fact_table", pred_name_arity(SymName, Arity), PredStatus,
        Context, marker_user_marked_no_inline, [], !ModuleInfo, !Specs).

    % Create a list(pragma_var) that looks like the ones that are created
    % for foreign_procs in the parser.
    % This is required by module_add_pragma_c_code to add the C code for
    % the procedure to the HLDS.
    %
:- pred fact_table_pragma_vars(list(prog_var)::in, list(mer_mode)::in,
    prog_varset::in, list(pragma_var)::out) is det.

fact_table_pragma_vars(Vars0, Modes0, VarSet, PragmaVars0) :-
    ( if
        Vars0 = [Var | VarsTail],
        Modes0 = [Mode | ModesTail]
    then
        varset.lookup_name(VarSet, Var, Name),
        PragmaVar = pragma_var(Var, Name, Mode, bp_native_if_possible),
        fact_table_pragma_vars(VarsTail, ModesTail, VarSet, PragmaVarsTail),
        PragmaVars0 = [PragmaVar | PragmaVarsTail]
    else
        PragmaVars0 = []
    ).

%-----------------------------------------------------------------------------%

:- pred add_pragma_oisu(pragma_info_oisu::in, item_mercury_status::in,
    prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_oisu(OISUInfo, ItemMercuryStatus, Context, !ModuleInfo, !Specs) :-
    OISUInfo = pragma_info_oisu(TypeCtor, Creators, Mutators, Destructors),
    some [!OISUSpecs] (
        !:OISUSpecs = [],
        (
            ItemMercuryStatus = item_defined_in_other_module(_)
        ;
            ItemMercuryStatus = item_defined_in_this_module(ItemExport),
            (
                ItemExport = item_export_anywhere
            ;
                ( ItemExport = item_export_nowhere
                ; ItemExport = item_export_only_submodules
                ),
                StatusPieces = [quote("pragma oisu"),
                    words("declarations must always be exported."), nl],
                StatusMsg = simple_msg(Context, [always(StatusPieces)]),
                StatusSpec = error_spec(severity_error,
                    phase_parse_tree_to_hlds, [StatusMsg]),
                !:OISUSpecs = [StatusSpec | !.OISUSpecs]
            ),

            module_info_get_type_table(!.ModuleInfo, TypeTable),
            ( if search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn) then
                hlds_data.get_type_defn_status(TypeDefn, TypeStatus),
                ( if TypeStatus = type_status(status_abstract_exported) then
                    true
                else
                    TypePieces = [words("The type in a"), quote("pragma oisu"),
                        words("declaration must always be abstract exported."),
                        nl],
                    TypeMsg = simple_msg(Context, [always(TypePieces)]),
                    TypeSpec = error_spec(severity_error,
                        phase_parse_tree_to_hlds, [TypeMsg]),
                    !:OISUSpecs = [TypeSpec | !.OISUSpecs]
                )
            else
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
            ( if map.insert(TypeCtor, OISUPreds, OISUMap0, OISUMap) then
                module_info_set_oisu_map(OISUMap, !ModuleInfo)
            else
                TypeCtor = type_ctor(TypeName, TypeArity),
                DupPieces = [words("Duplicate"), pragma_decl("oisu"),
                    words("declarations for"),
                    qual_sym_name_and_arity(
                        sym_name_arity(TypeName, TypeArity)),
                    suffix("."), nl],
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
                qual_sym_name_and_arity(sym_name_arity(TypeName, TypeArity)),
                suffix(":"), nl,
                words("error: predicate"),
                qual_sym_name_and_arity(sym_name_arity(PredName, PredArity)),
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
                unexpected($pred, "no arity pieces")
            ;
                SortedArityPieces = [_],
                ExpArities = SortedArityPieces
            ;
                SortedArityPieces = [_, _ | _],
                ExpArities = [words("one of") |
                    component_list_to_pieces("and", SortedArityPieces)]
            ),
            TypeCtor = type_ctor(TypeName, TypeArity),
            Pieces = [words("In the"), nth_fixed(!.SeqNum),
                fixed(Kind), words("predicate specification"),
                words("within the"), quote("pragma oisu"),
                words("declaration for"),
                qual_sym_name_and_arity(sym_name_arity(TypeName, TypeArity)),
                suffix(":"), nl,
                words("error: predicate"),
                qual_sym_name_and_arity(sym_name_arity(PredName, PredArity)),
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
            qual_sym_name_and_arity(sym_name_arity(TypeName, TypeArity)),
            suffix(":"), nl,
            words("error: ambiguous predicate name"),
            qual_sym_name_and_arity(sym_name_arity(PredName, PredArity)),
            suffix("."), nl],
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
    TermInfo = pragma_info_termination_info(PredNameModesPF,
        MaybePragmaArgSizeInfo, MaybePragmaTerminationInfo),
    PredNameModesPF = pred_name_modes_pf(SymName, Modes, PredOrFunc),
    list.length(Modes, Arity),
    % XXX lfh_ignore:
    % This happens in `.trans_opt' files sometimes, so just ignore it.
    look_up_pragma_pf_sym_arity(!.ModuleInfo, is_fully_qualified,
        lfh_ignore, Context, "termination_info",
        PredOrFunc, SymName, Arity, MaybePredId),
    (
        MaybePredId = ok1(PredId),
        SimpleCallId = simple_call_id(PredOrFunc, SymName, Arity),
        add_context_to_arg_size_info(MaybePragmaArgSizeInfo, Context,
            MaybeArgSizeInfo),
        add_context_to_termination_info(MaybePragmaTerminationInfo, Context,
            MaybeTerminationInfo),
        ProcTransform =
            ( pred(ProcInfo0::in, ProcInfo::out) is det :-
                proc_info_set_maybe_arg_size_info(MaybeArgSizeInfo,
                    ProcInfo0, ProcInfo1),
                proc_info_set_maybe_termination_info(MaybeTerminationInfo,
                    ProcInfo1, ProcInfo)
            ),
        transform_selected_mode_of_pred(PredId, SimpleCallId, Modes,
            "termination_info", Context, ProcTransform, !ModuleInfo, !Specs)
    ;
        MaybePredId = error1(_Specs)
    ).

%-----------------------------------------------------------------------------%

:- pred add_pragma_termination2_info(pragma_info_termination2_info::in,
    prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_termination2_info(Term2Info, Context, !ModuleInfo, !Specs) :-
    Term2Info = pragma_info_termination2_info(PredNameModesPF,
        MaybePragmaSuccessArgSizeInfo, MaybePragmaFailureArgSizeInfo,
        MaybePragmaTerminationInfo),
    PredNameModesPF = pred_name_modes_pf(SymName, Modes, PredOrFunc),
    list.length(Modes, Arity),
    % XXX lfh_ignore:
    % This happens in `.trans_opt' files sometimes, so just ignore it.
    look_up_pragma_pf_sym_arity(!.ModuleInfo, is_fully_qualified,
        lfh_ignore, Context, "termination2_info",
        PredOrFunc, SymName, Arity, MaybePredId),
    (
        MaybePredId = ok1(PredId),
        SimpleCallId = simple_call_id(PredOrFunc, SymName, Arity),
        ProcTransform =
            ( pred(ProcInfo0::in, ProcInfo::out) is det :-
                add_context_to_constr_termination_info(
                    MaybePragmaTerminationInfo, Context, MaybeTerminationInfo),
                some [!TermInfo] (
                    proc_info_get_termination2_info(ProcInfo0, !:TermInfo),
                    term2_info_set_import_success(MaybePragmaSuccessArgSizeInfo,
                        !TermInfo),
                    term2_info_set_import_failure(MaybePragmaFailureArgSizeInfo,
                        !TermInfo),
                    term2_info_set_term_status(MaybeTerminationInfo,
                        !TermInfo),
                    proc_info_set_termination2_info(!.TermInfo,
                        ProcInfo0, ProcInfo)
                )
            ),
        transform_selected_mode_of_pred(PredId, SimpleCallId, Modes,
            "termination2_info", Context, ProcTransform, !ModuleInfo, !Specs)
    ;
        MaybePredId = error1(_Specs)
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
        PredNameModesPF = pred_name_modes_pf(SymName, Modes, PredOrFunc),
        list.length(Modes, Arity),
        % XXX lfh_ignore:
        % This happens in `.trans_opt' files sometimes, so just ignore it.
        look_up_pragma_pf_sym_arity(!.ModuleInfo, is_fully_qualified,
            lfh_ignore, Context, "structure_sharing",
            PredOrFunc, SymName, Arity, MaybePredId),
        (
            MaybePredId = ok1(PredId),
            SimpleCallId = simple_call_id(PredOrFunc, SymName, Arity),
            ProcTransform = proc_info_set_imported_structure_sharing(HeadVars,
                Types, SharingDomain),
            transform_selected_mode_of_pred(PredId, SimpleCallId, Modes,
                "structure_sharing", Context, ProcTransform,
                !ModuleInfo, !Specs)
        ;
            MaybePredId = error1(_Specs)
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
        PredNameModesPF = pred_name_modes_pf(SymName, Modes, PredOrFunc),
        list.length(Modes, Arity),
        % XXX lfh_ignore:
        % This happens in `.trans_opt' files sometimes, so just ignore it.
        look_up_pragma_pf_sym_arity(!.ModuleInfo, is_fully_qualified,
            lfh_ignore, Context, "structure_reuse",
            PredOrFunc, SymName, Arity, MaybePredId),
        (
            MaybePredId = ok1(PredId),
            SimpleCallId = simple_call_id(PredOrFunc, SymName, Arity),
            ProcTransform = proc_info_set_imported_structure_reuse(HeadVars,
                Types, ReuseDomain),
            transform_selected_mode_of_pred(PredId, SimpleCallId, Modes,
                "structure_reuse", Context, ProcTransform, !ModuleInfo, !Specs)
        ;
            MaybePredId = error1(_Specs)
        )
    ).

%----------------------------------------------------------------------------%

:- pred transform_selected_mode_of_pred(pred_id::in, simple_call_id::in,
    list(mer_mode)::in, string::in, prog_context::in,
    pred(proc_info, proc_info)::in(pred(in, out) is det),
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

transform_selected_mode_of_pred(PredId, SimpleCallId, Modes,
        PragmaName, Context, ProcTransform, !ModuleInfo, !Specs) :-
    module_info_get_preds(!.ModuleInfo, PredTable0),
    map.lookup(PredTable0, PredId, PredInfo0),
    pred_info_get_proc_table(PredInfo0, ProcTable0),
    map.to_assoc_list(ProcTable0, ProcList),
    ( if
        get_procedure_matching_declmodes_with_renaming(!.ModuleInfo,
            ProcList, Modes, ProcId)
    then
        map.lookup(ProcTable0, ProcId, ProcInfo0),
        ProcTransform(ProcInfo0, ProcInfo),
        map.det_update(ProcId, ProcInfo, ProcTable0, ProcTable),
        pred_info_set_proc_table(ProcTable, PredInfo0, PredInfo),
        map.det_update(PredId, PredInfo, PredTable0, PredTable),
        module_info_set_preds(PredTable, !ModuleInfo)
    else
        Pieces = [words("Error:"), pragma_decl(PragmaName),
            words("declaration for undeclared mode of"),
            simple_call(SimpleCallId), suffix("."), nl],
        Spec = simplest_spec(severity_error, phase_parse_tree_to_hlds,
            Context, Pieces),
        !:Specs = [Spec | !.Specs]
    ).

%----------------------------------------------------------------------------%

:- type lookup_failure_handling
    --->    lfh_ignore
    ;       lfh_user_error
    ;       lfh_internal_error.

:- pred look_up_pragma_pf_sym_arity(module_info::in, is_fully_qualified::in,
    lookup_failure_handling::in, prog_context::in, string::in,
    pred_or_func::in, sym_name::in, int::in, maybe1(pred_id)::out) is det.

look_up_pragma_pf_sym_arity(ModuleInfo, IsFullyQualified, FailHandling,
        Context, PragmaName, PredOrFunc, SymName, Arity, MaybePredId) :-
    module_info_get_predicate_table(ModuleInfo, PredTable),
    predicate_table_lookup_pf_sym_arity(PredTable, IsFullyQualified,
        PredOrFunc, SymName, Arity, PredIds),
    (
        PredIds = [],
        (
            FailHandling = lfh_ignore,
            Specs = []
        ;
            FailHandling = lfh_user_error,
            predicate_table_lookup_pf_sym(PredTable,
                may_be_partially_qualified,
                PredOrFunc, SymName, AllArityPredIds),
            module_info_get_preds(ModuleInfo, Preds),
            find_pred_arities_other_than(Preds, AllArityPredIds, Arity,
                OtherArities),
            DescPieces = [pragma_decl(PragmaName), words("declaration")],
            report_undefined_pred_or_func_error(yes(PredOrFunc), SymName,
                Arity, OtherArities, Context, DescPieces, [], Specs)
        ;
            FailHandling = lfh_internal_error,
            Pieces = [words("Internal compiler error:"),
                words("unknown predicate name in"), pragma_decl(PragmaName),
                words("declaration."), nl],
            Spec = simplest_spec(severity_error, phase_parse_tree_to_hlds,
                Context, Pieces),
            Specs = [Spec]
        ),
        MaybePredId = error1(Specs)
    ;
        PredIds = [PredId],
        MaybePredId = ok1(PredId)
    ;
        PredIds = [_, _ | _],
        % A fully qualified lookup should *never* yield more than one pred_id.
        expect(unify(IsFullyQualified, may_be_partially_qualified), $pred,
            "two or more PredIds but is_fully_qualified"),
        (
            FailHandling = lfh_ignore,
            Specs = []
        ;
            FailHandling = lfh_user_error,
            StartPieces = [words("Error: ambiguous"), p_or_f(PredOrFunc),
                words("name in"), pragma_decl(PragmaName),
                words("declaration."), nl,
                words("The possible matches are:"), nl_indent_delta(1)],
            PredIdPiecesList = list.map(
                describe_one_pred_name(ModuleInfo, should_module_qualify),
                PredIds),
            PredIdPieces = component_list_to_line_pieces(PredIdPiecesList,
                [suffix(".")]),
            MainPieces = StartPieces ++ PredIdPieces,
            VerbosePieces = [words("An explicit module qualifier"),
                words("may be necessary to select the right match."), nl],
            Msg = simple_msg(Context,
                [always(MainPieces),
                verbose_only(verbose_always, VerbosePieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            Specs = [Spec]
        ;
            FailHandling = lfh_internal_error,
            Pieces = [words("Internal compiler error:"),
                words("ambiguous predicate name in"), pragma_decl(PragmaName),
                words("declaration."), nl],
            Spec = simplest_spec(severity_error, phase_parse_tree_to_hlds,
                Context, Pieces),
            Specs = [Spec]
        ),
        MaybePredId = error1(Specs)
    ).

%----------------------------------------------------------------------------%
:- end_module hlds.make_hlds.add_pragma.
%----------------------------------------------------------------------------%
