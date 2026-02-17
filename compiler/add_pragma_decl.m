%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% Copyright (C) 2023-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Add declarative pragmas to the HLDS.
%
%---------------------------------------------------------------------------%

:- module hlds.make_hlds.add_pragma_decl.
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

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_data.
:- import_module hlds.hlds_markers.
:- import_module hlds.hlds_pred.
:- import_module hlds.make_hlds.add_pragma_type_spec.
:- import_module hlds.make_hlds.add_pragma_type_spec_constr.
:- import_module hlds.make_hlds.add_pragma_util.
:- import_module hlds.make_hlds_error.
:- import_module hlds.pred_table.
:- import_module hlds.status.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_util.
:- import_module transform_hlds.
:- import_module transform_hlds.term_constr_main_types.
:- import_module transform_hlds.term_constr_util.
:- import_module transform_hlds.term_util.

:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
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

:- pred add_decl_pragma(io.text_output_stream::in,
    item_mercury_status::in, item_decl_pragma_info::in,
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
:- end_module hlds.make_hlds.add_pragma_decl.
%---------------------------------------------------------------------------%
