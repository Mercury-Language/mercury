%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% Copyright (C) 2023-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module hlds.make_hlds.add_pragma.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.make_hlds.make_hlds_types.
:- import_module hlds.make_hlds.qual_info.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_item.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

:- pred add_decl_pragmas(io.text_output_stream::in,
    ims_list(item_decl_pragma_info)::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_decl_pragmas_type_spec_constr(io.text_output_stream::in,
    list(decl_pragma_type_spec_constr_info)::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_decl_pragmas_type_spec(list(decl_pragma_type_spec_info)::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_decl_pragmas_termination(list(decl_pragma_termination_info)::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_decl_pragmas_termination2(list(decl_pragma_termination2_info)::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_decl_pragmas_sharing(list(decl_pragma_struct_sharing_info)::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_decl_pragmas_reuse(list(decl_pragma_struct_reuse_info)::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------%

:- pred add_decl_markers(ims_list(item_decl_marker_info)::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

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

%---------------------%

:- pred add_gen_pragma_unused_args(gen_pragma_unused_args_info::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_gen_pragma_exceptions(gen_pragma_exceptions_info::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_gen_pragma_trailing(gen_pragma_trailing_info::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_gen_pragma_mm_tabling(gen_pragma_mm_tabling_info::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_code_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_markers.
:- import_module hlds.hlds_pred.
:- import_module hlds.make_hlds.add_foreign_proc.
:- import_module hlds.make_hlds.add_pragma_tabling.
:- import_module hlds.make_hlds.add_pragma_type_spec.
:- import_module hlds.make_hlds.make_hlds_warn.
:- import_module hlds.make_hlds_error.
:- import_module hlds.pred_table.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module ll_backend.
:- import_module ll_backend.fact_table.
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
:- import_module transform_hlds.
:- import_module transform_hlds.term_constr_main_types.
:- import_module transform_hlds.term_constr_util.
:- import_module transform_hlds.term_util.

:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%
%
% Adding decl pragmas to the HLDS.
%

add_decl_pragmas(_, [], !ModuleInfo, !QualInfo, !Specs).
add_decl_pragmas(ProgressStream, [ImsList | ImsLists],
        !ModuleInfo, !QualInfo, !Specs) :-
    ImsList = ims_sub_list(ItemMercuryStatus, Items),
    list.foldl3(add_decl_pragma(ProgressStream, ItemMercuryStatus), Items,
        !ModuleInfo, !QualInfo, !Specs),
    add_decl_pragmas(ProgressStream, ImsLists, !ModuleInfo, !QualInfo, !Specs).

add_decl_pragmas_type_spec_constr(_, [], !ModuleInfo, !QualInfo, !Specs).
add_decl_pragmas_type_spec_constr(ProgressStream, [Pragma | Pragmas],
        !ModuleInfo, !QualInfo, !Specs) :-
    add_pragma_type_spec_constr(ProgressStream, Pragma,
        !ModuleInfo, !QualInfo, !Specs),
    add_decl_pragmas_type_spec_constr(ProgressStream, Pragmas,
        !ModuleInfo, !QualInfo, !Specs).

add_decl_pragmas_type_spec([], !ModuleInfo, !QualInfo, !Specs).
add_decl_pragmas_type_spec([Pragma | Pragmas],
        !ModuleInfo, !QualInfo, !Specs) :-
    add_pragma_type_spec(Pragma, !ModuleInfo, !QualInfo, !Specs),
    add_decl_pragmas_type_spec(Pragmas, !ModuleInfo, !QualInfo, !Specs).

add_decl_pragmas_termination([], !ModuleInfo, !Specs).
add_decl_pragmas_termination([Pragma | Pragmas], !ModuleInfo, !Specs) :-
    add_pragma_termination(Pragma, !ModuleInfo, !Specs),
    add_decl_pragmas_termination(Pragmas, !ModuleInfo, !Specs).

add_decl_pragmas_termination2([], !ModuleInfo, !Specs).
add_decl_pragmas_termination2([Pragma | Pragmas], !ModuleInfo, !Specs) :-
    add_pragma_termination2(Pragma, !ModuleInfo, !Specs),
    add_decl_pragmas_termination2(Pragmas, !ModuleInfo, !Specs).

add_decl_pragmas_sharing([], !ModuleInfo, !Specs).
add_decl_pragmas_sharing([Pragma | Pragmas], !ModuleInfo, !Specs) :-
    add_pragma_struct_sharing(Pragma, !ModuleInfo, !Specs),
    add_decl_pragmas_sharing(Pragmas, !ModuleInfo, !Specs).

add_decl_pragmas_reuse([], !ModuleInfo, !Specs).
add_decl_pragmas_reuse([Pragma | Pragmas], !ModuleInfo, !Specs) :-
    add_pragma_struct_reuse(Pragma, !ModuleInfo, !Specs),
    add_decl_pragmas_reuse(Pragmas, !ModuleInfo, !Specs).

%---------------------%

:- pred add_decl_pragma(io.text_output_stream::in, item_mercury_status::in,
    item_decl_pragma_info::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_decl_pragma(ProgressStream, ItemMercuryStatus, Pragma,
        !ModuleInfo, !QualInfo, !Specs) :-
    (
        Pragma = decl_pragma_obsolete_pred(ObsoletePredInfo),
        mark_pred_as_obsolete(ObsoletePredInfo, ItemMercuryStatus,
            !ModuleInfo, !Specs)
    ;
        Pragma = decl_pragma_obsolete_proc(ObsoleteProcInfo),
        mark_proc_as_obsolete(ObsoleteProcInfo, ItemMercuryStatus,
            !ModuleInfo, !Specs)
    ;
        Pragma = decl_pragma_format_call(FormatCallInfo),
        mark_pred_as_format_call(FormatCallInfo, ItemMercuryStatus,
            !ModuleInfo, !Specs)
    ;
        Pragma = decl_pragma_type_spec_constr(TypeSpecConstrInfo),
        add_pragma_type_spec_constr(ProgressStream, TypeSpecConstrInfo,
            !ModuleInfo, !QualInfo, !Specs)
    ;
        Pragma = decl_pragma_type_spec(TypeSpecInfo),
        add_pragma_type_spec(TypeSpecInfo, !ModuleInfo, !QualInfo, !Specs)
    ;
        Pragma = decl_pragma_oisu(OISUInfo),
        add_pragma_oisu(OISUInfo, ItemMercuryStatus, !ModuleInfo, !Specs)
    ;
        Pragma = decl_pragma_termination(TermInfo),
        add_pragma_termination(TermInfo, !ModuleInfo, !Specs)
    ;
        Pragma = decl_pragma_termination2(Term2Info),
        add_pragma_termination2(Term2Info, !ModuleInfo, !Specs)
    ;
        Pragma = decl_pragma_struct_sharing(SharingInfo),
        add_pragma_struct_sharing(SharingInfo, !ModuleInfo, !Specs)
    ;
        Pragma = decl_pragma_struct_reuse(ReuseInfo),
        add_pragma_struct_reuse(ReuseInfo, !ModuleInfo, !Specs)
    ).

%---------------------%

:- pred mark_pred_as_obsolete(decl_pragma_obsolete_pred_info::in,
    item_mercury_status::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

mark_pred_as_obsolete(ObsoletePredInfo, PragmaStatus,
        !ModuleInfo, !Specs) :-
    ObsoletePredInfo = decl_pragma_obsolete_pred_info(PredSpec,
        ObsoleteInFavourOf, Context, _),
    PredSpec = pred_pfu_name_arity(PFU, SymName, UserArity),
    get_matching_pred_ids(!.ModuleInfo, "obsolete", do_not_require_one_match,
        pragma_does_not_allow_modes, Context, PFU, SymName, UserArity,
        MatchingPredIdResult),
    (
        MatchingPredIdResult = mpids_ok(HeadPredId, TailPredIds, WarnSpecs),
        PredIds = [HeadPredId | TailPredIds],
        !:Specs = WarnSpecs ++ !.Specs,
        module_info_get_pred_id_table(!.ModuleInfo, PredIdTable0),
        mark_pred_ids_as_obsolete(ObsoleteInFavourOf, PragmaStatus, Context,
            PredIds, PredIdTable0, PredIdTable, !Specs),
        module_info_set_pred_id_table(PredIdTable, !ModuleInfo)
    ;
        MatchingPredIdResult = mpids_error(ErrorSpecs),
        !:Specs = ErrorSpecs ++ !.Specs
    ).

:- pred mark_pred_ids_as_obsolete(list(sym_name_arity)::in,
    item_mercury_status::in, prog_context::in, list(pred_id)::in,
    pred_id_table::in, pred_id_table::out,
    list(error_spec)::in, list(error_spec)::out) is det.

mark_pred_ids_as_obsolete(_, _, _, [], !PredTable, !Specs).
mark_pred_ids_as_obsolete(ObsoleteInFavourOf, PragmaStatus, Context,
        [PredId | PredIds], !PredTable, !Specs) :-
    map.lookup(!.PredTable, PredId, PredInfo0),
    check_pragma_status("obsolete", psc_decl, PragmaStatus, Context,
        PredInfo0, !Specs),
    pred_info_get_obsolete_in_favour_of(PredInfo0, MaybeObsoleteInFavourOf0),
    (
        MaybeObsoleteInFavourOf0 = no,
        MaybeObsoleteInFavourOf = yes(ObsoleteInFavourOf)
    ;
        MaybeObsoleteInFavourOf0 = yes(ObsoleteInFavourOf0),
        MaybeObsoleteInFavourOf =
            yes(ObsoleteInFavourOf0 ++ ObsoleteInFavourOf)
    ),
    pred_info_set_obsolete_in_favour_of(MaybeObsoleteInFavourOf,
        PredInfo0, PredInfo),
    map.det_update(PredId, PredInfo, !PredTable),
    mark_pred_ids_as_obsolete(ObsoleteInFavourOf, PragmaStatus, Context,
        PredIds, !PredTable, !Specs).

%---------------------%

:- pred mark_proc_as_obsolete(decl_pragma_obsolete_proc_info::in,
    item_mercury_status::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

mark_proc_as_obsolete(ObsoleteProcInfo, PragmaStatus, !ModuleInfo, !Specs) :-
    ObsoleteProcInfo = decl_pragma_obsolete_proc_info(PredNameModesPF,
        ObsoleteInFavourOf, Context, _),
    PredNameModesPF = proc_pf_name_modes(PredOrFunc, SymName, Modes),
    list.length(Modes, PredFormArityInt),
    user_arity_pred_form_arity(PredOrFunc, UserArity,
        pred_form_arity(PredFormArityInt)),
    look_up_pragma_pf_sym_arity(!.ModuleInfo, is_fully_qualified,
        lfh_user_error, Context, "obsolete_proc",
        PredOrFunc, SymName, UserArity, MaybePredId),
    (
        MaybePredId = ok1(PredId),
        module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
        check_pragma_status("obsolete_proc", psc_decl, PragmaStatus, Context,
            PredInfo0, !Specs),
        PFNameArity = pred_pf_name_arity(PredOrFunc, SymName, UserArity),
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
        transform_selected_mode_of_pred(PredId, PFNameArity, Modes,
            "obsolete_proc", Context, ProcTransform, !ModuleInfo, !Specs)
    ;
        MaybePredId = error1(Specs),
        !:Specs = Specs ++ !.Specs
    ).

%---------------------%

:- pred mark_pred_as_format_call(decl_pragma_format_call_info::in,
    item_mercury_status::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

mark_pred_as_format_call(FormatCallInfo, PragmaStatus, !ModuleInfo, !Specs) :-
    FormatCallInfo =
        decl_pragma_format_call_info(PredSpec, OoMArgSpecs, Context, _),
    PredSpec = pred_pf_name_arity(PredOrFunc, SymName, UserArity),
    look_up_pragma_pf_sym_arity(!.ModuleInfo, is_fully_qualified,
        lfh_user_error, Context, "format_call",
        PredOrFunc, SymName, UserArity, MaybePredId),
    (
        MaybePredId = error1(PredIdSpecs),
        !:Specs = PredIdSpecs ++ !.Specs
    ;
        MaybePredId = ok1(PredId),
        module_info_get_pred_id_table(!.ModuleInfo, PredIdTable0),
        map.lookup(PredIdTable0, PredId, PredInfo0),
        check_pragma_status("format_call", psc_decl, PragmaStatus, Context,
            PredInfo0, !Specs),
        pred_info_get_format_call_info(PredInfo0, MaybeFormatCall0),
        (
            MaybeFormatCall0 = no,
            % Record the presence of the format_call pragma for this pred.
            FormatCall = format_call_info(Context, OoMArgSpecs),
            pred_info_set_format_call_info(yes(FormatCall),
                PredInfo0, PredInfo),
            map.det_update(PredId, PredInfo, PredIdTable0, PredIdTable),
            module_info_set_pred_id_table(PredIdTable, !ModuleInfo),
            % Record this pragma as needing to be checked by the
            % check_pragma_format_call_preds pass.
            module_info_get_format_call_pragma_preds(!.ModuleInfo, FCPreds0),
            set.insert(PredId, FCPreds0, FCPreds),
            module_info_set_format_call_pragma_preds(FCPreds, !ModuleInfo)
        ;
            MaybeFormatCall0 = yes(format_call_info(OldContext, _)),
            FirstPieces = [words("Error:")] ++
                color_as_incorrect([words("duplicate"),
                    pragma_decl("format_call"), words("declaration")]) ++
                [words("for")] ++
                color_as_subject([unqual_pf_sym_name_user_arity(PredSpec),
                    suffix(".")]) ++
                [nl],
            FirstMsg = msg(Context, FirstPieces),
            SecondPieces = [words("The original"),
                pragma_decl("format_call"), words("declaration"),
                words("was here."), nl],
            SecondMsg = msg(OldContext, SecondPieces),
            Spec = error_spec($pred, severity_error, phase_pt2h,
                [FirstMsg, SecondMsg]),
            !:Specs = [Spec | !.Specs]
        )
    ).

%---------------------%

:- pred add_pragma_oisu(decl_pragma_oisu_info::in, item_mercury_status::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_oisu(OISUInfo, ItemMercuryStatus, !ModuleInfo, !Specs) :-
    OISUInfo = decl_pragma_oisu_info(TypeCtor, Creators, Mutators, Destructors,
        Context, _),
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
                StatusSpec = spec($pred, severity_error, phase_pt2h,
                    Context, StatusPieces),
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
                    TypeSpec = spec($pred, severity_error, phase_pt2h,
                        Context, TypePieces),
                    !:OISUSpecs = [TypeSpec | !.OISUSpecs]
                )
            else
%               TypePieces = [words("The type in this"), quote("pragma oisu"),
%                   words("declaration is undefined."), nl],
%               TypeMsg = simple_msg(Context, [always(TypePieces)]),
%               TypeSpec = error_spec(severity_error, phase_pt2h, [TypeMsg]),
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
                DupPieces = [words("Duplicate"), pragma_decl("oisu"),
                    words("declaration for"), qual_type_ctor(TypeCtor),
                    suffix("."), nl],
                DupSpec = spec($pred, severity_error, phase_pt2h,
                    Context, DupPieces),
                !:Specs = [DupSpec | !.Specs]
            )
        ;
            !.OISUSpecs = [_ | _],
            !:Specs = !.OISUSpecs ++ !.Specs
        )
    ).

:- pred find_unique_pred_for_oisu(module_info::in, prog_context::in,
    type_ctor::in, string::in, pred_pf_name_arity::in, pred_id::out,
    int::in, int::out, list(error_spec)::in, list(error_spec)::out) is det.

find_unique_pred_for_oisu(ModuleInfo, Context, TypeCtor, Kind,
        PredSpec, PredId, !SeqNum, !Specs) :-
    module_info_get_predicate_table(ModuleInfo, PredicateTable),
    PredSpec = pred_pf_name_arity(PredOrFunc, PredName, UserArity),
    user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
    predicate_table_lookup_pf_sym_arity(PredicateTable, is_fully_qualified,
        PredOrFunc, PredName, PredFormArity, PredIds),
    (
        PredIds = [],
        predicate_table_lookup_sym(PredicateTable, is_fully_qualified,
            PredName, LooseArityPredIds),
        % TODO: reword the error messages to avoid embedding the assumption
        % that PredSpec cannot refer to a function.
        UserArity = user_arity(UserArityInt),
        (
            LooseArityPredIds = [],
            Pieces = [words("In the"), nth_fixed(!.SeqNum),
                fixed(Kind), words("predicate specification"),
                words("within the"), quote("pragma oisu"),
                words("declaration for"), qual_type_ctor(TypeCtor),
                suffix(":"), nl,
                words("error: predicate"),
                qual_sym_name_arity(sym_name_arity(PredName, UserArityInt)),
                words("is undefined."), nl],
            Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces)
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
                    piece_list_to_pieces("and", SortedArityPieces)]
            ),
            Pieces = [words("In the"), nth_fixed(!.SeqNum),
                fixed(Kind), words("predicate specification"),
                words("within the"), quote("pragma oisu"),
                words("declaration for"), qual_type_ctor(TypeCtor),
                suffix(":"), nl,
                words("error: predicate"),
                qual_sym_name_arity(sym_name_arity(PredName, UserArityInt)),
                words("has the wrong arity."),
                words("Actual arity is"), int_fixed(UserArityInt), suffix(","),
                words("expected arity is")] ++ ExpArities ++ [suffix("."), nl],
            Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces)
        ),
        !:Specs = [Spec | !.Specs],
        PredId = invalid_pred_id
    ;
        PredIds = [PredId]
    ;
        PredIds = [_, _ | _],
        UserArity = user_arity(UserArityInt),
        PredOrFuncStr = pred_or_func_to_full_str(PredOrFunc),
        Pieces = [words("In the"), nth_fixed(!.SeqNum),
            fixed(Kind), words("predicate specification"),
            words("within the"), pragma_decl("oisu"), words("declaration"),
            words("for"), qual_type_ctor(TypeCtor), suffix(":"), nl,
            words("error: ambiguous"), words(PredOrFuncStr), words("name"),
            qual_sym_name_arity(sym_name_arity(PredName, UserArityInt)),
            suffix("."), nl],
        Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
        !:Specs = [Spec | !.Specs],
        PredId = invalid_pred_id
    ),
    !:SeqNum = !.SeqNum + 1.

:- pred lookup_pred_orig_arity(module_info::in, pred_id::in,
    format_piece::out) is det.

lookup_pred_orig_arity(ModuleInfo, PredId, Piece) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_orig_arity(PredInfo, pred_form_arity(OrigArity)),
    Piece = int_fixed(OrigArity).

%---------------------%

:- pred add_pragma_termination(decl_pragma_termination_info::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_termination(TermInfo, !ModuleInfo, !Specs) :-
    TermInfo = decl_pragma_termination_info(PredNameModesPF,
        MaybePragmaArgSizeInfo, MaybePragmaTerminationInfo, Context, _),
    PredNameModesPF = proc_pf_name_modes(PredOrFunc, SymName, Modes),
    PredFormArity = arg_list_arity(Modes),
    user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
    % XXX lfh_ignore:
    % This happens in `.trans_opt' files sometimes, so just ignore it.
    look_up_pragma_pf_sym_arity(!.ModuleInfo, is_fully_qualified,
        lfh_ignore, Context, "termination_info",
        PredOrFunc, SymName, UserArity, MaybePredId),
    (
        MaybePredId = ok1(PredId),
        PFNameArity = pred_pf_name_arity(PredOrFunc, SymName, UserArity),
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
        transform_selected_mode_of_pred(PredId, PFNameArity, Modes,
            "termination_info", Context, ProcTransform, !ModuleInfo, !Specs)
    ;
        MaybePredId = error1(_Specs)
    ).

%---------------------%

:- pred add_pragma_termination2(decl_pragma_termination2_info::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_termination2(Term2Info, !ModuleInfo, !Specs) :-
    Term2Info = decl_pragma_termination2_info(PredNameModesPF,
        MaybePragmaSuccessArgSizeInfo, MaybePragmaFailureArgSizeInfo,
        MaybePragmaTerminationInfo, Context, _),
    PredNameModesPF = proc_pf_name_modes(PredOrFunc, SymName, Modes),
    PredFormArity = arg_list_arity(Modes),
    user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
    % XXX lfh_ignore:
    % This happens in `.trans_opt' files sometimes, so just ignore it.
    look_up_pragma_pf_sym_arity(!.ModuleInfo, is_fully_qualified,
        lfh_ignore, Context, "termination2_info",
        PredOrFunc, SymName, UserArity, MaybePredId),
    (
        MaybePredId = ok1(PredId),
        PFNameArity = pred_pf_name_arity(PredOrFunc, SymName, UserArity),
        ProcTransform =
            ( pred(ProcInfo0::in, ProcInfo::out) is det :-
                add_context_to_constr_termination_info(
                    MaybePragmaTerminationInfo, Context, MaybeTerminationInfo),
                some [!TermInfo] (
                    proc_info_get_termination2_info(ProcInfo0, !:TermInfo),
                    term2_info_set_import_success(
                        MaybePragmaSuccessArgSizeInfo, !TermInfo),
                    term2_info_set_import_failure(
                        MaybePragmaFailureArgSizeInfo, !TermInfo),
                    term2_info_set_term_status(MaybeTerminationInfo,
                        !TermInfo),
                    proc_info_set_termination2_info(!.TermInfo,
                        ProcInfo0, ProcInfo)
                )
            ),
        transform_selected_mode_of_pred(PredId, PFNameArity, Modes,
            "termination2_info", Context, ProcTransform, !ModuleInfo, !Specs)
    ;
        MaybePredId = error1(_Specs)
    ).

%---------------------%

:- pred add_pragma_struct_sharing(decl_pragma_struct_sharing_info::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_struct_sharing(SharingInfo, !ModuleInfo, !Specs):-
    SharingInfo = decl_pragma_struct_sharing_info(PredNameModesPF,
        HeadVars, Types, _VarSet, _TVarSet, MaybeSharingDomain, Context, _),
    (
        MaybeSharingDomain = no
    ;
        MaybeSharingDomain = yes(SharingDomain),
        PredNameModesPF = proc_pf_name_modes(PredOrFunc, SymName, Modes),
        list.length(Modes, PredFormArityInt),
        user_arity_pred_form_arity(PredOrFunc, UserArity,
            pred_form_arity(PredFormArityInt)),
        look_up_pragma_pf_sym_arity(!.ModuleInfo, is_fully_qualified,
            lfh_ignore, Context, "structure_sharing",
            PredOrFunc, SymName, UserArity, MaybePredId),
        (
            MaybePredId = ok1(PredId),
            PFNameArity = pred_pf_name_arity(PredOrFunc, SymName, UserArity),
            ProcTransform = proc_info_set_imported_structure_sharing(HeadVars,
                Types, SharingDomain),
            transform_selected_mode_of_pred(PredId, PFNameArity, Modes,
                "structure_sharing", Context, ProcTransform,
                !ModuleInfo, !Specs)
        ;
            MaybePredId = error1(Specs),
            % XXX There used to be a comment here which said:
            %   XXX lfh_ignore:
            %   This happens in .trans_opt files sometimes, so just ignore it.
            % This was probably because the original code writing out these
            % pragmas was writing Modes with output_debug, not output_mercury.
            !:Specs = Specs ++ !.Specs
        )
    ).

%---------------------%

:- pred add_pragma_struct_reuse(decl_pragma_struct_reuse_info::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_struct_reuse(ReuseInfo, !ModuleInfo, !Specs):-
    ReuseInfo = decl_pragma_struct_reuse_info(PredNameModesPF,
        HeadVars, Types, _VarSet, _TVarSet, MaybeReuseDomain, Context, _),
    (
        MaybeReuseDomain = no
    ;
        MaybeReuseDomain = yes(ReuseDomain),
        PredNameModesPF = proc_pf_name_modes(PredOrFunc, SymName, Modes),
        list.length(Modes, PredFormArityInt),
        user_arity_pred_form_arity(PredOrFunc, UserArity,
            pred_form_arity(PredFormArityInt)),
        look_up_pragma_pf_sym_arity(!.ModuleInfo, is_fully_qualified,
            lfh_ignore, Context, "structure_reuse",
            PredOrFunc, SymName, UserArity, MaybePredId),
        (
            MaybePredId = ok1(PredId),
            PFNameArity = pred_pf_name_arity(PredOrFunc, SymName, UserArity),
            ProcTransform = proc_info_set_imported_structure_reuse(HeadVars,
                Types, ReuseDomain),
            transform_selected_mode_of_pred(PredId, PFNameArity, Modes,
                "structure_reuse", Context, ProcTransform, !ModuleInfo, !Specs)
        ;
            MaybePredId = error1(Specs),
            % XXX There used to be a comment here which said:
            %   XXX lfh_ignore:
            %   This happens in .trans_opt files sometimes, so just ignore it.
            % This was probably because the original code writing out these
            % pragmas was writing Modes with output_debug, not output_mercury.
            !:Specs = Specs ++ !.Specs
        )
    ).

%---------------------------------------------------------------------------%
%
% Adding decl markers to the HLDS.
%

add_decl_markers([], !ModuleInfo, !Specs).
add_decl_markers([ImsList | ImsLists], !ModuleInfo, !Specs) :-
    ImsList = ims_sub_list(ItemMercuryStatus, Items),
    list.foldl2(add_decl_marker(ItemMercuryStatus), Items,
        !ModuleInfo, !Specs),
    add_decl_markers(ImsLists, !ModuleInfo, !Specs).

%---------------------%

:- pred add_decl_marker(item_mercury_status::in, item_decl_marker_info::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_decl_marker(ItemMercuryStatus, DeclMarker, !ModuleInfo, !Specs) :-
    DeclMarker = item_decl_marker_info(MarkerKind, PFUNameArity, Context, _),
    (
        MarkerKind = dpmk_terminates,
        add_pred_marker(PFUNameArity, "terminates", psc_decl,
            ItemMercuryStatus, Context, marker_terminates,
            [marker_check_termination, marker_does_not_terminate],
            !ModuleInfo, !Specs)
    ;
        MarkerKind = dpmk_does_not_terminate,
        add_pred_marker(PFUNameArity, "does_not_terminate", psc_decl,
            ItemMercuryStatus, Context, marker_does_not_terminate,
            [marker_check_termination, marker_terminates], !ModuleInfo, !Specs)
    ;
        MarkerKind = dpmk_check_termination,
        add_pred_marker(PFUNameArity, "check_termination", psc_decl,
            ItemMercuryStatus, Context, marker_check_termination,
            [marker_terminates, marker_does_not_terminate],
            !ModuleInfo, !Specs)
    ).

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
%
% Adding generated pragmas to the HLDS.
%

add_gen_pragma_unused_args(UnusedArgsInfo, !ModuleInfo, !Specs) :-
    UnusedArgsInfo = gen_pragma_unused_args_info(PredNameArityPFMn, UnusedArgs,
        Context, _),
    PredNameArityPFMn = proc_pf_name_arity_mn(PredOrFunc, SymName, UserArity,
        ModeNum),
    look_up_pragma_pf_sym_arity(!.ModuleInfo, is_fully_qualified,
        lfh_internal_error, Context, "unused_args",
        PredOrFunc, SymName, UserArity, MaybePredId),
    (
        MaybePredId = ok1(PredId),
        module_info_get_unused_arg_info(!.ModuleInfo, UnusedArgInfo0),
        % Convert the mode number to a proc_id.
        proc_id_to_int(ProcId, ModeNum),
        PredProcId = proc(PredId, ProcId),
        map.set(PredProcId, UnusedArgs, UnusedArgInfo0, UnusedArgInfo),
        module_info_set_unused_arg_info(UnusedArgInfo, !ModuleInfo)
    ;
        MaybePredId = error1(Specs),
        !:Specs = Specs ++ !.Specs
    ).

add_gen_pragma_exceptions(Exceptions, !ModuleInfo, !Specs) :-
    Exceptions = gen_pragma_exceptions_info(PredNameArityPFMn, ThrowStatus,
        Context, _),
    PredNameArityPFMn = proc_pf_name_arity_mn(PredOrFunc, SymName, UserArity,
        ModeNum),
    % XXX We will just ignore errors for the time being -
    % it causes errors with transitive-intermodule optimization.
    % XXX What kinds of errors?
    look_up_pragma_pf_sym_arity_mode_num(!.ModuleInfo, is_fully_qualified,
        lfh_ignore, Context, "exceptions",
        PredOrFunc, SymName, UserArity, ModeNum, MaybePredProc),
    (
        MaybePredProc = ok4(PredId, ProcId, PredInfo0, ProcInfo0),
        ProcExceptionInfo = proc_exception_info(ThrowStatus, no),
        proc_info_set_exception_info(yes(ProcExceptionInfo),
            ProcInfo0, ProcInfo),
        module_info_set_pred_proc_info(PredId, ProcId, PredInfo0, ProcInfo,
            !ModuleInfo)
    ;
        MaybePredProc = error4(Specs),
        !:Specs = Specs ++ !.Specs
    ).

add_gen_pragma_trailing(Trailing, !ModuleInfo, !Specs) :-
    Trailing = gen_pragma_trailing_info(PredNameArityPFMn, TrailingStatus,
        Context, _),
    PredNameArityPFMn = proc_pf_name_arity_mn(PredOrFunc, SymName, UserArity,
        ModeNum),
    % XXX We will just ignore errors for the time being -
    % it causes errors with transitive-intermodule optimization.
    % XXX What kinds of errors?
    look_up_pragma_pf_sym_arity_mode_num(!.ModuleInfo, is_fully_qualified,
        lfh_ignore, Context, "trailing_info",
        PredOrFunc, SymName, UserArity, ModeNum, MaybePredProc),
    (
        MaybePredProc = ok4(PredId, ProcId, PredInfo0, ProcInfo0),
        ProcTrailingInfo = proc_trailing_info(TrailingStatus, no),
        proc_info_set_trailing_info(yes(ProcTrailingInfo),
            ProcInfo0, ProcInfo),
        module_info_set_pred_proc_info(PredId, ProcId, PredInfo0, ProcInfo,
            !ModuleInfo)
    ;
        MaybePredProc = error4(Specs),
        !:Specs = Specs ++ !.Specs
    ).

add_gen_pragma_mm_tabling(MMTabling, !ModuleInfo, !Specs) :-
    MMTabling = gen_pragma_mm_tabling_info(PredNameArityPFMn, TablingStatus,
        Context, _),
    PredNameArityPFMn = proc_pf_name_arity_mn(PredOrFunc, SymName, UserArity,
        ModeNum),
    % XXX We will just ignore errors for the time being -
    % it causes errors with transitive-intermodule optimization.
    % XXX What kinds of errors?
    look_up_pragma_pf_sym_arity_mode_num(!.ModuleInfo, is_fully_qualified,
        lfh_ignore, Context, "mm_tabling_info",
        PredOrFunc, SymName, UserArity, ModeNum, MaybePredProc),
    (
        MaybePredProc = ok4(PredId, ProcId, PredInfo0, ProcInfo0),
        ProcMMTablingInfo = proc_mm_tabling_info(TablingStatus, no),
        proc_info_set_mm_tabling_info(yes(ProcMMTablingInfo),
            ProcInfo0, ProcInfo),
        module_info_set_pred_proc_info(PredId, ProcId, PredInfo0, ProcInfo,
            !ModuleInfo)
    ;
        MaybePredProc = error4(Specs),
        !:Specs = Specs ++ !.Specs
    ).

%---------------------------------------------------------------------------%
%
% Predicates that are useful in more than one place above.
%
%---------------------------------------------------------------------------%

    % add_pred_marker(PredSymNameArity, Status, Context, PragmaName, Marker,
    %   ConflictMarkers, !ModuleInfo, !Specs):
    %
    % Adds Marker to the marker list of the pred(s) with the given
    % PredNameArity, updating the ModuleInfo. If the named pred does not exist,
    % or the pred(s) already has/have a marker in ConflictMarkers,
    % report an error.
    %
:- pred add_pred_marker(pred_pfu_name_arity::in, string::in,
    pragma_status_class::in, item_mercury_status::in, prog_context::in,
    pred_marker::in, list(pred_marker)::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pred_marker(PredSpec, PragmaName, PragmaStatusClass, PragmaStatus, Context,
        Marker, ConflictMarkers, !ModuleInfo, !Specs) :-
    PredSpec = pred_pfu_name_arity(PFU, PredSymName, UserArity),
    get_matching_pred_ids(!.ModuleInfo, PragmaName, do_not_require_one_match,
        pragma_does_not_allow_modes, Context, PFU, PredSymName, UserArity,
        MatchingPredIdResult),
    (
        MatchingPredIdResult = mpids_ok(HeadPredId, TailPredIds, WarnSpecs),
        PredIds = [HeadPredId | TailPredIds],
        !:Specs = WarnSpecs ++ !.Specs,
        module_info_get_pred_id_table(!.ModuleInfo, PredIdTable0),
        pragma_add_marker(PragmaName, PragmaStatusClass, PragmaStatus, Context,
            add_marker_pred_info(Marker), PredIds,
            PredIdTable0, PredIdTable, !Specs),
        module_info_set_pred_id_table(PredIdTable, !ModuleInfo),

        list.map(get_pred_markers(PredIdTable), PredIds, PredMarkerSets),
        PredMarkers = set.union_list(PredMarkerSets),
        set.intersect(PredMarkers, set.list_to_set(ConflictMarkers),
            BadPredMarkers),
        ( if set.is_empty(BadPredMarkers) then
            true
        else
            pragma_conflict_error(PredSpec, Context, PragmaName,
                BadPredMarkers, !Specs)
        )
    ;
        MatchingPredIdResult = mpids_error(ErrorSpecs),
        !:Specs = ErrorSpecs ++ !.Specs
    ).

    % For each pred_id in the list, check whether markers present in the list
    % of conflicting markers are also present in the corresponding pred_info.
    % The output is a set of the names of the conflicting markers present.
    %
:- pred get_pred_markers(pred_id_table::in, pred_id::in,
    set(pred_marker)::out) is det.

get_pred_markers(PredIdTable, PredId, Markers) :-
    map.lookup(PredIdTable, PredId, PredInfo),
    pred_info_get_markers(PredInfo, Markers).

:- type add_marker_pred_info == pred(pred_info, pred_info).
:- inst add_marker_pred_info == (pred(in, out) is det).

    % For each pred_id in the list, add the given markers to the
    % list of markers in the corresponding pred_info.
    %
:- pred pragma_add_marker(string::in, pragma_status_class::in,
    item_mercury_status::in, prog_context::in,
    add_marker_pred_info::in(add_marker_pred_info),
    list(pred_id)::in, pred_id_table::in, pred_id_table::out,
    list(error_spec)::in, list(error_spec)::out) is det.

pragma_add_marker(_, _, _, _, _, [], !PredTable, !Specs).
pragma_add_marker(PragmaName, PragmaStatusClass, PragmaStatus, Context,
        UpdatePredInfo, [PredId | PredIds], !PredTable, !Specs) :-
    map.lookup(!.PredTable, PredId, PredInfo0),
    UpdatePredInfo(PredInfo0, PredInfo),
    map.det_update(PredId, PredInfo, !PredTable),
    check_pragma_status(PragmaName, PragmaStatusClass, PragmaStatus, Context,
        PredInfo, !Specs),
    pragma_add_marker(PragmaName, PragmaStatusClass, PragmaStatus,
        Context, UpdatePredInfo, PredIds, !PredTable, !Specs).

:- pred add_marker_pred_info(pred_marker::in, pred_info::in, pred_info::out)
    is det.

add_marker_pred_info(Marker, !PredInfo) :-
    pred_info_get_markers(!.PredInfo, Markers0),
    add_marker(Marker, Markers0, Markers),
    pred_info_set_markers(Markers, !PredInfo).

:- pred pragma_conflict_error(pred_pfu_name_arity::in, prog_context::in,
    string::in, set(pred_marker)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

pragma_conflict_error(PredSpec, Context, PragmaName, ConflictMarkers,
        !Specs) :-
    PredSpec = pred_pfu_name_arity(PFU, PredSymName, UserArity),
    UserArity = user_arity(UserArityInt),
    SNA = sym_name_arity(PredSymName, UserArityInt),
    (
        PFU = pfu_unknown,
        PorFPieces = []
    ;
        PFU = pfu_predicate,
        PorFPieces = [words("predicate")]
    ;
        PFU = pfu_function,
        PorFPieces = [words("function")]
    ),
    list.map(marker_name, set.to_sorted_list(ConflictMarkers), ConflictNames),
    Pieces = [words("Error:")] ++
        color_as_subject([pragma_decl(PragmaName),
            words("declaration")]) ++
        color_as_incorrect([words("conflicts")]) ++
        [words("with previous")] ++
        fixed_list_to_pieces("and", ConflictNames) ++
        [words(choose_number(ConflictNames, "pragma for", "pragmas for"))] ++
        color_as_subject(PorFPieces ++
            [unqual_sym_name_arity(SNA), suffix(".")]) ++
        [nl],
    Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
    !:Specs = [Spec | !.Specs].

%---------------------%

:- type maybe_require_one_match
    --->    do_not_require_one_match
    ;       require_one_match.

:- type matching_pred_ids_result
    --->    mpids_ok(pred_id, list(pred_id), list(error_spec))
    ;       mpids_error(list(error_spec)).

    % Given maybe a pred_or_func, a symname and arity, return all
    % the pred_ids that match. Reasons for the possible return of more than one
    % pred_id include the symname not being fully module qualified (which
    % allows from more than one fully qualified module name), and no
    % pred_or_func indication being specified (which allows a match from
    % both a predicate and a function in the same module).
    %
    % If PredIds is empty, then we will want to generate an error message.
    % This message should mention that while given name does not exist
    % with the given arity, it does exist with some other arity,
    % so we also return (in OtherArities) the arities of all the predicates
    % and functions with that name but with some *other* arity.
    %
:- pred get_matching_pred_ids(module_info::in, string::in,
    maybe_require_one_match::in, does_pragma_allow_modes::in, prog_context::in,
    pred_func_or_unknown::in, sym_name::in, user_arity::in,
    matching_pred_ids_result::out) is det.

get_matching_pred_ids(ModuleInfo, Pragma, RequireOneMatch, PragmaAllowsModes,
        Context, PFU, SymName, UserArity, Result) :-
    module_info_get_predicate_table(ModuleInfo, PredTable0),
    % Check that the pragma is module qualified.
    (
        SymName = unqualified(_),
        unexpected($pred, "unqualified name")
    ;
        SymName = qualified(_, _),
        (
            PFU = pfu_unknown,
            predicate_table_lookup_sym_arity(PredTable0, is_fully_qualified,
                SymName, UserArity, PredIds),
            warn_about_pfu_unknown(ModuleInfo, Pragma, PragmaAllowsModes,
                SymName, UserArity, Context, WarnSpecs)
        ;
            PFU = pfu_predicate,
            user_arity_pred_form_arity(pf_predicate, UserArity, PredFormArity),
            predicate_table_lookup_pf_sym_arity(PredTable0, is_fully_qualified,
                pf_predicate, SymName, PredFormArity, PredIds),
            WarnSpecs = []
        ;
            PFU = pfu_function,
            user_arity_pred_form_arity(pf_function, UserArity, PredFormArity),
            predicate_table_lookup_pf_sym_arity(PredTable0, is_fully_qualified,
                pf_function, SymName, PredFormArity, PredIds),
            WarnSpecs = []
        ),
        (
            PredIds = [],
            predicate_table_lookup_sym(PredTable0, is_fully_qualified,
                SymName, SymOnlyPredIds),
            % If PFU is not pfu_unknown, we *could* count a pred_id
            % in SymOnlyPredIds only if it has the right PredOrFunc field, but
            %
            % - there may be no such pred_id, and
            % - even if there are such pred_ids, there is no guarantee
            %   that the user meant one of them.
            module_info_get_pred_id_table(ModuleInfo, PredIdTable0),
            find_user_arities_other_than(PredIdTable0, SymOnlyPredIds,
                UserArity, OtherUserArities),
            DescPieces = [pragma_decl(Pragma), words("declaration")],
            report_undefined_pred_or_func_error(pfu_to_maybe_pred_or_func(PFU),
                SymName, UserArity, OtherUserArities, Context, DescPieces,
                [], NoMatchSpecs),
            Result = mpids_error(NoMatchSpecs ++ WarnSpecs)
        ;
            PredIds = [HeadPredId],
            Result = mpids_ok(HeadPredId, [], WarnSpecs)
        ;
            PredIds = [HeadPredId | TailPredIds],
            TailPredIds = [_ | _],
            UserArity = user_arity(UserArityInt),
            (
                RequireOneMatch = do_not_require_one_match,
                module_info_get_globals(ModuleInfo, Globals),
                globals.lookup_bool_option(Globals, warn_ambiguous_pragma,
                    WarnActual),
                (
                    WarnActual = no,
                    Result = mpids_ok(HeadPredId, TailPredIds, WarnSpecs)
                ;
                    WarnActual = yes,
                    SNA = sym_name_arity(SymName, UserArityInt),
                    ActualPieces = [words("In"), pragma_decl(Pragma),
                        words("declaration for")] ++
                        color_as_subject([unqual_sym_name_arity(SNA),
                            suffix(":")]) ++
                        [nl,
                        words("warning:")] ++
                        color_as_incorrect([words("ambiguous name"),
                            words("could refer to either"),
                            words("a predicate or a function.")]) ++
                        [nl],
                    ActualSpec = spec($pred, severity_warning, phase_pt2h,
                        Context, ActualPieces),
                    % There is no point in printing WarnSpecs warning about
                    % *possible* ambiguity when ActualSpec reports a warning
                    % about an *actual* ambiguity, since the latter implies
                    % the former.
                    Result = mpids_ok(HeadPredId, TailPredIds, [ActualSpec])
                )
            ;
                RequireOneMatch = require_one_match,
                SNA = sym_name_arity(SymName, UserArityInt),
                ErrorPieces = [words("In"), pragma_decl(Pragma),
                    words("declaration for")] ++
                    color_as_subject([unqual_sym_name_arity(SNA),
                        suffix(":")]) ++
                    [nl,
                    words("error:")] ++
                    color_as_incorrect([words("ambiguous name"),
                        words("could refer to either"),
                        words("a predicate or a function.")]) ++
                        [nl],
                ErrorSpec = spec($pred, severity_error, phase_pt2h,
                    Context, ErrorPieces),
                Result = mpids_error([ErrorSpec])
            )
        )
    ).

%---------------------%

:- pred transform_selected_mode_of_pred(pred_id::in, pred_pf_name_arity::in,
    list(mer_mode)::in, string::in, prog_context::in,
    pred(proc_info, proc_info)::in(pred(in, out) is det),
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

transform_selected_mode_of_pred(PredId, PFNameArity, Modes,
        PragmaName, Context, ProcTransform, !ModuleInfo, !Specs) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    pred_info_get_proc_table(PredInfo0, ProcTable0),
    map.to_assoc_list(ProcTable0, ProcList),
    ( if
        get_procedure_matching_declmodes_with_renaming(!.ModuleInfo,
            ProcList, Modes, ProcId)
    then
        map.lookup(ProcTable0, ProcId, ProcInfo0),
        ProcTransform(ProcInfo0, ProcInfo),
        pred_info_set_proc_info(ProcId, ProcInfo, PredInfo0, PredInfo),
        module_info_set_pred_info(PredId, PredInfo, !ModuleInfo)
    else
        Pieces = [words("Error:"), pragma_decl(PragmaName),
            words("declaration for")] ++
            color_as_incorrect([words("undeclared mode")]) ++
            [words("of"), qual_pf_sym_name_user_arity(PFNameArity),
            suffix("."), nl],
        Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
        !:Specs = [Spec | !.Specs]
    ).

%---------------------%

:- type lookup_failure_handling
    --->    lfh_ignore
    ;       lfh_user_error
    ;       lfh_internal_error.

:- pred look_up_pragma_pf_sym_arity(module_info::in, is_fully_qualified::in,
    lookup_failure_handling::in, prog_context::in, string::in,
    pred_or_func::in, sym_name::in, user_arity::in, maybe1(pred_id)::out)
    is det.

look_up_pragma_pf_sym_arity(ModuleInfo, IsFullyQualified, FailHandling,
        Context, PragmaName, PredOrFunc, SymName, UserArity, MaybePredId) :-
    module_info_get_predicate_table(ModuleInfo, PredTable),
    user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
    predicate_table_lookup_pf_sym_arity(PredTable, IsFullyQualified,
        PredOrFunc, SymName, PredFormArity, PredIds),
    (
        PredIds = [],
        (
            FailHandling = lfh_user_error,
            predicate_table_lookup_pf_sym(PredTable,
                may_be_partially_qualified,
                PredOrFunc, SymName, AllArityPredIds),
            module_info_get_pred_id_table(ModuleInfo, PredIdTable),
            find_user_arities_other_than(PredIdTable, AllArityPredIds,
                UserArity, OtherUserArities),
            DeclPieces = [pragma_decl(PragmaName), words("declaration")],
            report_undefined_pred_or_func_error(yes(PredOrFunc), SymName,
                UserArity, OtherUserArities, Context, DeclPieces, [], Specs)
        ;
            FailHandling = lfh_internal_error,
            Spec = report_unknown_pred_or_func(severity_error,
                PragmaName, Context, PredOrFunc, SymName, UserArity),
            Specs = [Spec]
        ;
            FailHandling = lfh_ignore,
            module_info_get_globals(ModuleInfo, Globals),
            globals.lookup_bool_option(Globals, inform_ignored_pragma_errors,
                InformIgnored),
            (
                InformIgnored = no,
                Specs = []
            ;
                InformIgnored = yes,
                Spec = report_unknown_pred_or_func(severity_error,
                    PragmaName, Context, PredOrFunc, SymName, UserArity),
                Specs = [Spec]
            )
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
            FailHandling = lfh_user_error,
            UserArity = user_arity(UserArityInt),
            SNA = sym_name_arity(SymName, UserArityInt),
            PredIdPiecesList =
                list.map(describe_qual_pred_name(ModuleInfo), PredIds),
            PredIdPieces = pieces_list_to_color_line_pieces(color_hint,
                [suffix(".")], PredIdPiecesList),
            MainPieces = [words("Error:")] ++
                color_as_incorrect([words("ambiguous"), p_or_f(PredOrFunc),
                    words("name")]) ++
                color_as_subject([qual_sym_name_arity(SNA)]) ++
                [words("in"), pragma_decl(PragmaName), words("declaration."),
                nl,
                words("The possible matches are:"), nl_indent_delta(1)] ++
                PredIdPieces ++ [nl_indent_delta(-1)],
            VerbosePieces = [words("An explicit module qualifier"),
                words("may be necessary to select the right match."), nl],
            Msg = simple_msg(Context,
                [always(MainPieces),
                verbose_only(verbose_always, VerbosePieces)]),
            Spec = error_spec($pred, severity_error, phase_pt2h, [Msg]),
            Specs = [Spec]
        ;
            FailHandling = lfh_internal_error,
            Spec = report_ambiguous_pred_or_func(severity_error,
                PragmaName, Context, PredOrFunc, SymName, UserArity),
            Specs = [Spec]
        ;
            FailHandling = lfh_ignore,
            module_info_get_globals(ModuleInfo, Globals),
            globals.lookup_bool_option(Globals, inform_ignored_pragma_errors,
                InformIgnored),
            (
                InformIgnored = no,
                Specs = []
            ;
                InformIgnored = yes,
                Spec = report_ambiguous_pred_or_func(severity_informational,
                    PragmaName, Context, PredOrFunc, SymName, UserArity),
                Specs = [Spec]
            )
        ),
        MaybePredId = error1(Specs)
    ).

:- func report_unknown_pred_or_func(error_severity, string, prog_context,
    pred_or_func, sym_name, user_arity) = error_spec.

report_unknown_pred_or_func(Severity, PragmaName, Context,
        PredOrFunc, SymName, UserArity) = Spec :-
    UserArity = user_arity(UserArityInt),
    SNA = sym_name_arity(SymName, UserArityInt),
    Pieces = [words("Internal compiler error:")] ++
        color_as_incorrect([words("unknown"), p_or_f(PredOrFunc),
            qual_sym_name_arity(SNA)]) ++
        [words("in"), pragma_decl(PragmaName), words("declaration."), nl],
    Spec = spec($pred, Severity, phase_pt2h, Context, Pieces).

:- func report_ambiguous_pred_or_func(error_severity, string, prog_context,
    pred_or_func, sym_name, user_arity) = error_spec.

report_ambiguous_pred_or_func(Severity, PragmaName, Context,
        PredOrFunc, SymName, UserArity) = Spec :-
    UserArity = user_arity(UserArityInt),
    SNA = sym_name_arity(SymName, UserArityInt),
    Pieces = [words("Internal compiler error:")] ++
        color_as_incorrect([words("ambiguous"), p_or_f(PredOrFunc),
            words("name"), qual_sym_name_arity(SNA)]) ++
        [words("in"), pragma_decl(PragmaName), words("declaration."), nl],
    Spec = spec($pred, Severity, phase_pt2h, Context, Pieces).

:- pred look_up_pragma_pf_sym_arity_mode_num(module_info::in,
    is_fully_qualified::in, lookup_failure_handling::in, prog_context::in,
    string::in, pred_or_func::in, sym_name::in, user_arity::in, int::in,
    maybe4(pred_id, proc_id, pred_info, proc_info)::out) is det.

look_up_pragma_pf_sym_arity_mode_num(ModuleInfo, IsFullyQualified,
        FailHandling, Context, PragmaName, PredOrFunc, SymName, UserArity,
        ModeNum, MaybePredProcId) :-
    look_up_pragma_pf_sym_arity(ModuleInfo, IsFullyQualified, FailHandling,
        Context, PragmaName, PredOrFunc, SymName, UserArity, MaybePredId),
    (
        MaybePredId = ok1(PredId),
        proc_id_to_int(ProcId, ModeNum),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        pred_info_get_proc_table(PredInfo, ProcTable),
        ( if map.search(ProcTable, ProcId, ProcInfo) then
            MaybePredProcId = ok4(PredId, ProcId, PredInfo, ProcInfo)
        else
            PorF = pred_info_is_pred_or_func(PredInfo),
            PredName = pred_info_name(PredInfo),
            user_arity(UserArityInt) = pred_info_user_arity(PredInfo),
            NameArity = name_arity(PredName, UserArityInt),
            Pieces = [words("Internal compiler error:"),
                pragma_decl(PragmaName), words("declaration."),
                words("for")] ++
                color_as_incorrect([words("unknown mode number"),
                    int_fixed(ModeNum), words("of"),
                    p_or_f(PorF), name_arity(NameArity), suffix(".")]) ++
                [nl],
            Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
            MaybePredProcId = error4([Spec])
        )
    ;
        MaybePredId = error1(Specs),
        MaybePredProcId = error4(Specs)
    ).

%---------------------%

    % Symnames in pragmas always include a module name. Usually,
    % the pragma will give the name of a predicate (or function) without
    % module qualifying it, in which case the parser implicitly qualifies it
    % with the name of the module the pragma was read from. If the pragma
    % does include a (possibly partial) module qualification, the parser
    % will check whether this matches what would have been the implicit
    % qualification. Therefore module name mismatches between pragmas
    % and the predicates they apply to are impossible.
    %
    % If the pragma (and thus the predicate) occurs outside the current module,
    % then we do not generate any error or warning message for it, even if
    % it would otherwise merit one. This is because the error is in the other
    % module, and should be reported when *that* module is compiled.
    %
    % If the pragma and the predicate both occur inside the current module,
    % there are four possibilities:
    %
    % 1: pred is not exported, pragma is not exported
    % 2: pred is not exported, pragma is exported
    % 3: pred is exported, pragma is not exported
    % 4: pred is exported, pragma is exported
    %
    % Case 1 is always OK.
    %
    % Case 2 is always an error.
    %
    % Case 3 is normal for impl pragmas, since such pragmas are effectively
    % part of the implementation of the predicate, which is always private.
    % For decl pragmas, it merits a warning, since it causes some caller
    % of the predicate (the ones outside this module) to know less about
    % this predicate than they could.
    %
    % Case 4 is normal for decl pragmas, but is an error for impl pragmas.
    % (Note that an impl pragma in the implementation section of module A
    % may still reach another module B if its predicate is opt_exported.)
    %
:- type pragma_status_class
    --->    psc_decl
    ;       psc_impl.

:- pred check_pragma_status(string::in, pragma_status_class::in,
    item_mercury_status::in, prog_context::in, pred_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_pragma_status(PragmaName, StatusClass, PragmaStatus, Context,
        PredInfo, !Specs) :-
    (
        PragmaStatus = item_defined_in_other_module(_)
    ;
        PragmaStatus = item_defined_in_this_module(PragmaExport),
        (
            ( PragmaExport = item_export_nowhere
            ; PragmaExport = item_export_only_submodules
            ),
            PragmaExported = no
        ;
            PragmaExport = item_export_anywhere,
            PragmaExported = yes
        ),
        pred_info_get_status(PredInfo, PredStatus),
        PredExported = pred_status_is_exported_to_non_submodules(PredStatus),
        (
            PredExported = no,
            (
                PragmaExported = no
                % Case 1: ok.
            ;
                PragmaExported = yes,
                % Case 2: error regardless of pragma status class.
                PredNamePieces = describe_one_pred_info_name(
                    yes(color_subject), should_not_module_qualify, [],
                    PredInfo),
                Pieces = [words("Error: since the")] ++ PredNamePieces ++
                    [words("is not exported, the"),
                    pragma_decl(PragmaName), words("declaration for it")] ++
                    color_as_incorrect(
                        [words("may not be exported either.")]) ++
                    [nl],
                Spec = spec($pred, severity_error, phase_pt2h,
                    Context, Pieces),
                !:Specs = [Spec | !.Specs]
            )
        ;
            PredExported = yes,
            (
                PragmaExported = no,
                % Case 3: ok for impl pragmas, warning for decl pragmas.
                (
                    StatusClass = psc_decl,
                    PredNamePieces = describe_one_pred_info_name(
                        yes(color_subject), should_not_module_qualify, [],
                        PredInfo),
                    Pieces = [words("Warning: since the")] ++ PredNamePieces ++
                        [words("is exported, the"),
                        pragma_decl(PragmaName),
                        words("declaration for it")] ++
                        color_as_incorrect(
                            [words("should also be exported.")]) ++
                        [nl],
                    Spec = spec($pred, severity_warning, phase_pt2h,
                        Context, Pieces),
                    !:Specs = [Spec | !.Specs]
                ;
                    StatusClass = psc_impl
                )
            ;
                PragmaExported = yes,
                % Case 4: ok for decl pragmas, error for impl pragmas.
                (
                    StatusClass = psc_decl
                ;
                    StatusClass = psc_impl,
                    % NOTE We never generate this error message, because
                    % the parser generates an error message for any impl
                    % pragmas in interfaces, and declines to add them
                    % to the parse tree. Indeed, the parse_tree_module_src
                    % type, which represents the parse trees of source modules,
                    % cannot represent impl pragmas in interface sections.
                    Pieces = [words("Error: a"),
                        pragma_decl(PragmaName), words("declaration")] ++
                        color_as_incorrect([words("may not be exported,")]) ++
                        [words("even if"),
                        words("the predicate or function it refers to"),
                        words("is exported."), nl],
                    Spec = spec($pred, severity_error, phase_pt2h,
                        Context, Pieces),
                    !:Specs = [Spec | !.Specs]
                )
            )
        )
    ).

%---------------------------------------------------------------------------%
:- end_module hlds.make_hlds.add_pragma.
%---------------------------------------------------------------------------%
