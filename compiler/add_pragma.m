%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012,2014 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module hlds.make_hlds.add_pragma.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.make_hlds.qual_info.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_item.

:- import_module list.

%---------------------------------------------------------------------------%

:- pred add_decl_pragmas(ims_list(item_decl_pragma_info)::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_decl_pragmas_type_spec(list(item_type_spec)::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_decl_pragmas_term_info(list(item_termination)::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_decl_pragmas_term2_info(list(item_termination2)::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_decl_pragmas_sharing(list(item_struct_sharing)::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_decl_pragmas_reuse(list(item_struct_reuse)::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------%

:- pred add_impl_pragmas(ims_list(item_impl_pragma_info)::in,
    ims_list(item_tabled)::in, ims_list(item_tabled)::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_impl_pragmas_tabled(ims_list(item_tabled)::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_pragma_info_foreign_proc_export(
    item_pragma_info(pragma_info_foreign_proc_export)::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------%

:- pred add_gen_pragma_unused_args(item_unused_args::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_gen_pragma_exceptions(item_exceptions::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_gen_pragma_trailing(item_trailing::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_gen_pragma_mm_tabling(item_mm_tabling::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- include_module hlds.make_hlds.add_pragma.add_pragma_tabling.
:- include_module hlds.make_hlds.add_pragma.add_pragma_type_spec.

:- import_module hlds.hlds_code_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_pred.
:- import_module hlds.make_hlds.add_foreign_proc.
:- import_module hlds.make_hlds.add_pragma.add_pragma_tabling.
:- import_module hlds.make_hlds.add_pragma.add_pragma_type_spec.
:- import_module hlds.make_hlds.make_hlds_warn.
:- import_module hlds.make_hlds_error.
:- import_module hlds.pred_table.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.op_mode.
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

%---------------------------------------------------------------------------%
%
% Loop over lists of pragmas to add to the HLDS.
%

add_decl_pragmas([], !ModuleInfo, !QualInfo, !Specs).
add_decl_pragmas([ImsList | ImsLists], !ModuleInfo, !QualInfo, !Specs) :-
    ImsList = ims_sub_list(ItemMercuryStatus, Items),
    list.foldl3(add_decl_pragma(ItemMercuryStatus), Items,
        !ModuleInfo, !QualInfo, !Specs),
    add_decl_pragmas(ImsLists, !ModuleInfo, !QualInfo, !Specs).

add_decl_pragmas_type_spec([], !ModuleInfo, !QualInfo, !Specs).
add_decl_pragmas_type_spec([PragmaInfo | PragmaInfos],
        !ModuleInfo, !QualInfo, !Specs) :-
    PragmaInfo = item_pragma_info(Pragma, Context, _SeqNum),
    add_pragma_type_spec(Pragma, Context, !ModuleInfo, !QualInfo, !Specs),
    add_decl_pragmas_type_spec(PragmaInfos, !ModuleInfo, !QualInfo, !Specs).

add_decl_pragmas_term_info([], !ModuleInfo, !Specs).
add_decl_pragmas_term_info([PragmaInfo | PragmaInfos],
        !ModuleInfo, !Specs) :-
    PragmaInfo = item_pragma_info(Pragma, Context, _SeqNum),
    add_pragma_termination_info(Pragma, Context, !ModuleInfo, !Specs),
    add_decl_pragmas_term_info(PragmaInfos, !ModuleInfo, !Specs).

add_decl_pragmas_term2_info([], !ModuleInfo, !Specs).
add_decl_pragmas_term2_info([PragmaInfo | PragmaInfos],
        !ModuleInfo, !Specs) :-
    PragmaInfo = item_pragma_info(Pragma, Context, _SeqNum),
    add_pragma_termination2_info(Pragma, Context, !ModuleInfo, !Specs),
    add_decl_pragmas_term2_info(PragmaInfos, !ModuleInfo, !Specs).

add_decl_pragmas_sharing([], !ModuleInfo, !Specs).
add_decl_pragmas_sharing([PragmaInfo | PragmaInfos],
        !ModuleInfo, !Specs) :-
    PragmaInfo = item_pragma_info(Pragma, Context, _SeqNum),
    add_pragma_structure_sharing(Pragma, Context, !ModuleInfo, !Specs),
    add_decl_pragmas_sharing(PragmaInfos, !ModuleInfo, !Specs).

add_decl_pragmas_reuse([], !ModuleInfo, !Specs).
add_decl_pragmas_reuse([PragmaInfo | PragmaInfos],
        !ModuleInfo, !Specs) :-
    PragmaInfo = item_pragma_info(Pragma, Context, _SeqNum),
    add_pragma_structure_reuse(Pragma, Context, !ModuleInfo, !Specs),
    add_decl_pragmas_reuse(PragmaInfos, !ModuleInfo, !Specs).

%---------------------%

add_impl_pragmas([], !RevPragmaTabled, !ModuleInfo, !QualInfo, !Specs).
add_impl_pragmas([ImsList | ImsLists],
        !RevPragmaTabledLists, !ModuleInfo, !QualInfo, !Specs) :-
    ImsList = ims_sub_list(ItemMercuryStatus, Items),
    list.foldl4(add_impl_pragma(ItemMercuryStatus), Items,
        [], RevPragmaTableds, !ModuleInfo, !QualInfo, !Specs),
    (
        RevPragmaTableds = []
    ;
        RevPragmaTableds = [_ | _],
        list.reverse(RevPragmaTableds, PragmaTableds),
        PragmaTabledList = ims_sub_list(ItemMercuryStatus, PragmaTableds),
        !:RevPragmaTabledLists = [PragmaTabledList | !.RevPragmaTabledLists]
    ),
    add_impl_pragmas(ImsLists,
        !RevPragmaTabledLists, !ModuleInfo, !QualInfo, !Specs).

add_impl_pragmas_tabled([], !ModuleInfo, !QualInfo, !Specs).
add_impl_pragmas_tabled([ImsList | ImsLists],
        !ModuleInfo, !QualInfo, !Specs) :-
    ImsList = ims_sub_list(ItemMercuryStatus, Items),
    list.foldl3(add_impl_pragma_tabled(ItemMercuryStatus), Items,
        !ModuleInfo, !QualInfo, !Specs),
    add_impl_pragmas_tabled(ImsLists, !ModuleInfo, !QualInfo, !Specs).

%---------------------------------------------------------------------------%
%
% Adding decl pragmas to the HLDS.
%

:- pred add_decl_pragma(item_mercury_status::in, item_decl_pragma_info::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_decl_pragma(ItemMercuryStatus, ItemPragmaInfo,
        !ModuleInfo, !QualInfo, !Specs) :-
    ItemPragmaInfo = item_pragma_info(Pragma, Context, _SeqNum),
    (
        Pragma = decl_pragma_obsolete_pred(ObsoletePredInfo),
        item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
        mark_pred_as_obsolete(ObsoletePredInfo, PredStatus, Context,
            !ModuleInfo, !Specs)
    ;
        Pragma = decl_pragma_obsolete_proc(ObsoleteProcInfo),
        item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
        mark_proc_as_obsolete(ObsoleteProcInfo, PredStatus, Context,
            !ModuleInfo, !Specs)
    ;
        Pragma = decl_pragma_type_spec(TypeSpecInfo),
        add_pragma_type_spec(TypeSpecInfo, Context,
            !ModuleInfo, !QualInfo, !Specs)
    ;
        Pragma = decl_pragma_oisu(OISUInfo),
        add_pragma_oisu(OISUInfo, ItemMercuryStatus, Context,
            !ModuleInfo, !Specs)
    ;
        Pragma = decl_pragma_terminates(PredNameArity),
        item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
        add_pred_marker("terminates", PredNameArity, PredStatus, Context,
            marker_terminates,
            [marker_check_termination, marker_does_not_terminate],
            !ModuleInfo, !Specs)
    ;
        Pragma = decl_pragma_does_not_terminate(PredNameArity),
        item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
        add_pred_marker("does_not_terminate", PredNameArity, PredStatus,
            Context, marker_does_not_terminate,
            [marker_check_termination, marker_terminates], !ModuleInfo, !Specs)
    ;
        Pragma = decl_pragma_check_termination(PredNameArity),
        item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
        add_pred_marker("check_termination", PredNameArity, PredStatus,
            Context, marker_check_termination,
            [marker_terminates, marker_does_not_terminate],
            !ModuleInfo, !Specs)
    ;
        Pragma = decl_pragma_termination_info(TermInfo),
        add_pragma_termination_info(TermInfo, Context, !ModuleInfo, !Specs)
    ;
        Pragma = decl_pragma_termination2_info(Term2Info),
        add_pragma_termination2_info(Term2Info, Context, !ModuleInfo, !Specs)
    ;
        Pragma = decl_pragma_structure_sharing(SharingInfo),
        add_pragma_structure_sharing(SharingInfo, Context, !ModuleInfo, !Specs)
    ;
        Pragma = decl_pragma_structure_reuse(ReuseInfo),
        add_pragma_structure_reuse(ReuseInfo, Context, !ModuleInfo, !Specs)
    ).

%---------------------%

:- pred mark_pred_as_obsolete(pragma_info_obsolete_pred::in,
    pred_status::in, prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

mark_pred_as_obsolete(ObsoletePredInfo, PragmaStatus, Context,
        !ModuleInfo, !Specs) :-
    ObsoletePredInfo =
        pragma_info_obsolete_pred(PredSpec, ObsoleteInFavourOf),
    maybe_warn_about_pfu_unknown(!.ModuleInfo, "obsolete", PredSpec, Context,
        !Specs),
    PredSpec = pred_pfu_name_arity(PFU, SymName, UserArity),
    get_matching_pred_ids(!.ModuleInfo, PFU, SymName, UserArity,
        PredIds, OtherUserArities),
    (
        PredIds = [_ | _],
        module_info_get_pred_id_table(!.ModuleInfo, PredIdTable0),
        mark_pred_ids_as_obsolete(ObsoleteInFavourOf, PragmaStatus, PredIds,
            no, WrongStatus, PredIdTable0, PredIdTable),
        (
            WrongStatus = yes,
            pragma_status_error(pfu_to_string(PFU), SymName, UserArity,
                Context, "obsolete", !Specs)
        ;
            WrongStatus = no
        ),
        module_info_set_pred_id_table(PredIdTable, !ModuleInfo)
    ;
        PredIds = [],
        DescPieces = [pragma_decl("obsolete"), words("declaration")],
        UserArity = user_arity(UserArityInt),
        OtherArities = list.map(project_user_arity_int, OtherUserArities),
        report_undefined_pred_or_func_error(pfu_to_maybe_pred_or_func(PFU),
            SymName, UserArityInt, OtherArities, Context, DescPieces, !Specs)
    ).

:- pred mark_pred_ids_as_obsolete(list(sym_name_arity)::in,
    pred_status::in, list(pred_id)::in, bool::in, bool::out,
    pred_id_table::in, pred_id_table::out) is det.

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
        MaybeObsoleteInFavourOf =
            yes(ObsoleteInFavourOf0 ++ ObsoleteInFavourOf)
    ),
    pred_info_set_obsolete_in_favour_of(MaybeObsoleteInFavourOf,
        PredInfo0, PredInfo),
    map.det_update(PredId, PredInfo, !PredTable),
    mark_pred_ids_as_obsolete(ObsoleteInFavourOf, PragmaStatus, PredIds,
        !WrongStatus, !PredTable).

%---------------------%

:- pred mark_proc_as_obsolete(pragma_info_obsolete_proc::in,
    pred_status::in, prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

mark_proc_as_obsolete(ObsoleteProcInfo, PragmaStatus, Context,
        !ModuleInfo, !Specs) :-
    ObsoleteProcInfo =
        pragma_info_obsolete_proc(PredNameModesPF, ObsoleteInFavourOf),
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
        ( if
            pred_info_is_exported(PredInfo0),
            pred_status_is_exported(PragmaStatus) = no
        then
            pragma_status_error(pf_to_string(PredOrFunc), SymName, UserArity,
                Context, "obsolete_proc", !Specs)
        else
            true
        ),
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
                StatusSpec = simplest_spec($pred, severity_error,
                    phase_parse_tree_to_hlds, Context, StatusPieces),
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
                    TypeSpec = simplest_spec($pred, severity_error,
                        phase_parse_tree_to_hlds, Context, TypePieces),
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
                DupPieces = [words("Duplicate"), pragma_decl("oisu"),
                    words("declaration for"), qual_type_ctor(TypeCtor),
                    suffix("."), nl],
                DupSpec = simplest_spec($pred, severity_error,
                    phase_parse_tree_to_hlds, Context, DupPieces),
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
            Spec = simplest_spec($pred, severity_error,
                phase_parse_tree_to_hlds, Context, Pieces)
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
            Spec = simplest_spec($pred, severity_error,
                phase_parse_tree_to_hlds, Context, Pieces)
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
        Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
            Context, Pieces),
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

%---------------------%

:- pred add_pragma_termination_info(pragma_info_termination_info::in,
    prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_termination_info(TermInfo, Context, !ModuleInfo, !Specs) :-
    TermInfo = pragma_info_termination_info(PredNameModesPF,
        MaybePragmaArgSizeInfo, MaybePragmaTerminationInfo),
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

:- pred add_pragma_termination2_info(pragma_info_termination2_info::in,
    prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_termination2_info(Term2Info, Context, !ModuleInfo, !Specs) :-
    Term2Info = pragma_info_termination2_info(PredNameModesPF,
        MaybePragmaSuccessArgSizeInfo, MaybePragmaFailureArgSizeInfo,
        MaybePragmaTerminationInfo),
    PredNameModesPF = proc_pf_name_modes(PredOrFunc, SymName, Modes),
    list.length(Modes, PredFormArityInt),
    user_arity_pred_form_arity(PredOrFunc, UserArity,
        pred_form_arity(PredFormArityInt)),
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

:- pred add_pragma_structure_sharing(pragma_info_structure_sharing::in,
    prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_structure_sharing(SharingInfo, Context, !ModuleInfo, !Specs):-
    SharingInfo = pragma_info_structure_sharing(PredNameModesPF,
        HeadVars, Types, _VarSet, _TVarSet, MaybeSharingDomain),
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

:- pred add_pragma_structure_reuse(pragma_info_structure_reuse::in,
    prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_structure_reuse(ReuseInfo, Context, !ModuleInfo, !Specs):-
    ReuseInfo = pragma_info_structure_reuse(PredNameModesPF,
        HeadVars, Types, _VarSet, _TVarSet, MaybeReuseDomain),
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
% Adding impl pragmas to the HLDS.
%

:- pred add_impl_pragma(item_mercury_status::in, item_impl_pragma_info::in,
    list(item_tabled)::in, list(item_tabled)::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_impl_pragma(ItemMercuryStatus, ItemPragmaInfo, !RevPragmaTabled,
        !ModuleInfo, !QualInfo, !Specs) :-
    ItemPragmaInfo = item_pragma_info(Pragma, Context, SeqNum),
    (
        Pragma = impl_pragma_foreign_decl(FDInfo),
        % XXX STATUS Check ItemMercuryStatus
        FDInfo = pragma_info_foreign_decl(Lang, IsLocal, CHeader),
        ForeignDeclCode = foreign_decl_code(Lang, IsLocal, CHeader, Context),
        module_add_foreign_decl_code_user(ForeignDeclCode, !ModuleInfo)
    ;
        Pragma = impl_pragma_foreign_code(FCInfo),
        % XXX STATUS Check ItemMercuryStatus
        FCInfo = pragma_info_foreign_code(Lang, BodyCode),
        warn_suspicious_foreign_code(Lang, BodyCode, Context, !Specs),
        ForeignBodyCode = foreign_body_code(Lang, BodyCode, Context),
        module_add_foreign_body_code(ForeignBodyCode, !ModuleInfo)
    ;
        Pragma = impl_pragma_foreign_proc(FPInfo),
        item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
        PragmaFPInfo = item_pragma_info(FPInfo, Context, SeqNum),
        add_pragma_foreign_proc(PredStatus, PragmaFPInfo, !ModuleInfo, !Specs)
    ;
        Pragma = impl_pragma_foreign_proc_export(FEInfo),
        add_pragma_foreign_proc_export(FEInfo, Context, !ModuleInfo, !Specs)
    ;
        Pragma = impl_pragma_external_proc(ExternalInfo),
        add_pragma_external_proc(ExternalInfo, Context, !ModuleInfo, !Specs)
    ;
        Pragma = impl_pragma_fact_table(FTInfo),
        item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
        add_pragma_fact_table(FTInfo, PredStatus, Context, !ModuleInfo, !Specs)
    ;
        Pragma = impl_pragma_tabled(TabledInfo),
        ItemPragmaTabledInfo = item_pragma_info(TabledInfo, Context, SeqNum),
        !:RevPragmaTabled = [ItemPragmaTabledInfo | !.RevPragmaTabled]
    ;
        Pragma = impl_pragma_inline(PredSymNameArity),
        item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
        % Note that mode_check_inline conflicts with inline because
        % it implies no_inline.
        add_pred_marker("inline", PredSymNameArity, PredStatus, Context,
            marker_user_marked_inline,
            [marker_user_marked_no_inline, marker_mode_check_clauses],
            !ModuleInfo, !Specs)
    ;
        Pragma = impl_pragma_no_inline(PredSymNameArity),
        item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
        add_pred_marker("no_inline", PredSymNameArity, PredStatus, Context,
            marker_user_marked_no_inline, [marker_user_marked_inline],
            !ModuleInfo, !Specs)
    ;
        Pragma = impl_pragma_consider_used(PredSymNameArity),
        item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
        add_pred_marker("consider_used", PredSymNameArity, PredStatus, Context,
            marker_consider_used, [], !ModuleInfo, !Specs)
    ;
        Pragma = impl_pragma_mode_check_clauses(PredNameArity),
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
        Pragma = impl_pragma_no_detism_warning(PredNameArity),
        item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
        add_pred_marker("no_determinism_warning", PredNameArity, PredStatus,
            Context, marker_no_detism_warning, [], !ModuleInfo, !Specs)
    ;
        Pragma = impl_pragma_require_tail_rec(TailrecWarningPragma),
        add_pragma_require_tail_rec(TailrecWarningPragma, Context,
            !ModuleInfo, !Specs)
    ;
        Pragma = impl_pragma_promise_pure(PredNameArity),
        item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
        add_pred_marker("promise_pure", PredNameArity, PredStatus,
            Context, marker_promised_pure, [], !ModuleInfo, !Specs)
    ;
        Pragma = impl_pragma_promise_semipure(PredNameArity),
        item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
        add_pred_marker("promise_semipure", PredNameArity, PredStatus,
            Context, marker_promised_semipure, [], !ModuleInfo, !Specs)
    ;
        Pragma = impl_pragma_promise_eqv_clauses(PredNameArity),
        item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
        add_pred_marker("promise_equivalent_clauses", PredNameArity,
            PredStatus, Context, marker_promised_equivalent_clauses, [],
            !ModuleInfo, !Specs)
    ;
        Pragma = impl_pragma_require_feature_set(RFSInfo),
        RFSInfo = pragma_info_require_feature_set(FeatureSet),
        check_required_feature_set(FeatureSet, ItemMercuryStatus, Context,
            !ModuleInfo, !Specs)
    ).

%---------------------%

add_pragma_info_foreign_proc_export(PragmaFPEInfo, !ModuleInfo, !Specs) :-
    PragmaFPEInfo = item_pragma_info(FPEInfo, Context, _SeqNum),
    add_pragma_foreign_proc_export(FPEInfo, Context, !ModuleInfo, !Specs).

:- pred add_pragma_foreign_proc_export(pragma_info_foreign_proc_export::in,
    prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_foreign_proc_export(FPEInfo, Context, !ModuleInfo, !Specs) :-
    FPEInfo = pragma_info_foreign_proc_export(Origin, Lang,
        PredNameModesPF, ExportedName, VarSet),
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
                Pieces = [words("Error:"),
                    pragma_decl("foreign_export"), words("declaration"),
                    words("for a procedure that has"),
                    words("a declared determinism of"),
                    fixed(determinism_to_string(Detism)), suffix("."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_parse_tree_to_hlds, Context, Pieces),
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

:- pred add_pragma_external_proc(pragma_info_external_proc::in,
    prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_external_proc(ExternalInfo, Context, !ModuleInfo, !Specs) :-
    % XXX STATUS Check ItemMercuryStatus
    ExternalInfo = pragma_info_external_proc(PFNameArity, MaybeBackend),
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
        UserArity = user_arity(UserArityInt),
        (
            PredOrFunc = pf_predicate,
            predicate_table_lookup_pred_sym_arity(PredicateTable0,
                is_fully_qualified, SymName, UserArity, PredIds),
            predicate_table_lookup_pred_sym(PredicateTable0,
                is_fully_qualified, SymName, AllArityPredIds),
            MissingPieces = [decl("external_pred"), words("pragma")]
        ;
            PredOrFunc = pf_function,
            predicate_table_lookup_func_sym_arity(PredicateTable0,
                is_fully_qualified, SymName, UserArity, PredIds),
            predicate_table_lookup_func_sym(PredicateTable0,
                is_fully_qualified, SymName, AllArityPredIds),
            MissingPieces = [decl("external_func"), words("pragma")]
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
            OtherArities =
                list.map(project_user_arity_int, OtherUserArities),
            report_undefined_pred_or_func_error(yes(PredOrFunc),
                SymName, UserArityInt, OtherArities, Context,
                MissingPieces, !Specs)
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
        Arity = pred_info_orig_arity(PredInfo0),
        SNA = sym_name_arity(unqualified(PredName), Arity),
        Pieces = [words("The"), p_or_f(PredOrFunc),
            unqual_sym_name_arity(SNA), words("has clauses,"),
            words("so it cannot be marked as external."), nl],
        Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
            Context, Pieces),
        !:Specs = [Spec | !.Specs]
    ).

%---------------------%

    % add_pragma_fact_table(FTInfo, Status, Context, !ModuleInfo, !Info):
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
    FTInfo = pragma_info_fact_table(PredSpec, FileName),
    maybe_warn_about_pfu_unknown(!.ModuleInfo, "fact_table", PredSpec,
        Context, !Specs),
    PredSpec = pred_pfu_name_arity(PFU, PredSymName, UserArity),
    get_matching_pred_ids(!.ModuleInfo, PFU, PredSymName, UserArity,
        PredIds, OtherUserArities),
    (
        PredIds = [],
        UserArity = user_arity(UseArityInt),
        OtherArities = list.map(project_user_arity_int, OtherUserArities),
        report_undefined_pred_or_func_error(pfu_to_maybe_pred_or_func(PFU),
            PredSymName, UseArityInt, OtherArities, Context,
            [pragma_decl("fact_table"), words("declaration")], !Specs)
    ;
        PredIds = [HeadPredId | TailPredIds],
        (
            TailPredIds = [],      % only one predicate found
            PredId = HeadPredId,
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
                        fact_table_compile_facts(!.ModuleInfo, FileName,
                            Context, GenInfo, C_HeaderCode, PrimaryProcId,
                            PredInfo0, PredInfo, !Specs, !IO),
                        impure io.unsafe_set_io_state(!.IO)
                    )
                ),

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
                add_fact_table_procs(PredOrFunc, PredSymName, UserArity,
                    PredStatus, ProcTable,  PrimaryProcId, Context, GenInfo,
                    ProcIds, !ModuleInfo, !Specs)
            )
        ;
            TailPredIds = [_ | _],     % >1 predicate found
            UserArity = user_arity(UserArityInt),
            Pieces = [words("In"), quote("pragma fact_table"), words("for"),
                qual_sym_name_arity(sym_name_arity(PredSymName, UserArityInt)),
                suffix(":"), nl,
                words("error: ambiguous predicate/function name."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_parse_tree_to_hlds, Context, Pieces),
            !:Specs = [Spec | !.Specs]
        )
    ).

    % Add a `pragma foreign_proc' for each mode of the fact table lookup
    % to the HLDS.
    %
    % `pragma fact_table's are represented in the HLDS by a
    % `pragma foreign_proc' for each mode of the predicate.
    %
:- pred add_fact_table_procs(pred_or_func::in, sym_name::in, user_arity::in,
    pred_status::in, proc_table::in, proc_id::in, prog_context::in,
    fact_table_gen_info::in, list(proc_id)::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_fact_table_procs(_, _, _, _, _, _, _, _, [], !ModuleInfo, !Specs).
add_fact_table_procs(PredOrFunc, SymName, UserArity, PredStatus, ProcTable,
        PrimaryProcId, Context, GenInfo, [ProcId | ProcIds],
        !ModuleInfo, !Specs) :-
    add_fact_table_proc(PredOrFunc, SymName, UserArity, PredStatus, ProcTable,
        PrimaryProcId, Context, GenInfo, ProcId, !ModuleInfo, !Specs),
    add_fact_table_procs(PredOrFunc, SymName, UserArity, PredStatus, ProcTable,
        PrimaryProcId, Context, GenInfo, ProcIds, !ModuleInfo, !Specs).

:- pred add_fact_table_proc(pred_or_func::in, sym_name::in, user_arity::in,
    pred_status::in, proc_table::in, proc_id::in, prog_context::in,
    fact_table_gen_info::in, proc_id::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_fact_table_proc(PredOrFunc, SymName, UserArity, PredStatus, ProcTable,
        PrimaryProcId, Context, GenInfo, ProcId, !ModuleInfo, !Specs) :-
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
    add_extra_attribute(refers_to_llds_stack, Attrs3, Attrs),
    SeqNum = item_no_seq_num,
    FCInfo = pragma_info_foreign_proc(Attrs, SymName, PredOrFunc, PragmaVars,
        ProgVarSet, InstVarSet, fp_impl_ordinary(C_ProcCode, no)),
    PragmaFCInfo = item_pragma_info(FCInfo, Context, SeqNum),
    add_pragma_foreign_proc(PredStatus, PragmaFCInfo, !ModuleInfo, !Specs),
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
    ( PredOrFunc = pf_predicate, PFU = pfu_predicate
    ; PredOrFunc = pf_function, PFU = pfu_function
    ),
    PredSpec = pred_pfu_name_arity(PFU, SymName, UserArity),
    add_pred_marker("fact_table", PredSpec, PredStatus, Context,
        marker_user_marked_no_inline, [], !ModuleInfo, !Specs).

%---------------------%

:- pred add_pragma_require_tail_rec(pragma_info_require_tail_rec::in,
    prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_require_tail_rec(Pragma, Context, !ModuleInfo, !Specs) :-
    Pragma = pragma_info_require_tail_rec(PredSpec, RequireTailrec),
    PredSpec = pred_or_proc_pfumm_name(PFUMM, PredSymName),
    pfumm_to_maybe_pf_arity_maybe_modes(PFUMM, MaybePredOrFunc, UserArity,
        MaybeModes),
    UserArity = user_arity(UserArityInt),
    PFU = maybe_pred_or_func_to_pfu(MaybePredOrFunc),
    get_matching_pred_ids(!.ModuleInfo, PFU, PredSymName, UserArity,
        PredIds, OtherUserArities),
    (
        PredIds = [],
        Pieces = [pragma_decl("require_tail_recursion"), words("pragma")],
        OtherArities = list.map(project_user_arity_int, OtherUserArities),
        report_undefined_pred_or_func_error(MaybePredOrFunc, PredSymName,
            UserArityInt, OtherArities, Context, Pieces, !Specs)
    ;
        PredIds = [PredId],
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
                    MaybePredOrFunc, SNA, ProcId - Proc,
                    PredInfo0, PredInfo, !Specs)
            else
                PredInfo = PredInfo0,
                PredOrFunc = pred_info_is_pred_or_func(PredInfo),
                PFNameArity =
                    pred_pf_name_arity(PredOrFunc, PredSymName, UserArity),
                Pieces = [words("Error:"),
                    pragma_decl("require_tail_recursion"),
                    words("declaration for undeclared mode of"),
                    qual_pf_sym_name_user_arity(PFNameArity), suffix("."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_parse_tree_to_hlds, Context, Pieces),
                !:Specs = [Spec | !.Specs]
            )
        ;
            MaybeModes = no,
            list.foldl2(
                add_pragma_require_tail_rec_proc(RequireTailrec, Context,
                    MaybePredOrFunc, SNA),
                Procs, PredInfo0, PredInfo, !Specs)
        ),
        module_info_set_pred_info(PredId, PredInfo, !ModuleInfo)
    ;
        PredIds = [_, _ | _],
        Pieces = [words("Error: ambiguous predicate or function in"),
            pragma_decl("require_tail_recursion"), words("pragma."), nl],
        Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
            Context, Pieces),
        !:Specs = [Spec | !.Specs]
    ).

:- pred add_pragma_require_tail_rec_proc(require_tail_recursion::in,
    prog_context::in, maybe(pred_or_func)::in, sym_name_arity::in,
    pair(proc_id, proc_info)::in, pred_info::in, pred_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_require_tail_rec_proc(RequireTailrec, Context, MaybePredOrFunc, SNA,
        ProcId - ProcInfo0, !PredInfo, !Specs) :-
    proc_info_get_maybe_require_tailrec_info(ProcInfo0,
        MaybeRequireTailrecOrig),
    (
        MaybeRequireTailrecOrig = yes(RequireTailrecOrig),
        (
            MaybePredOrFunc = no,
            PorFPieces = []
        ;
            MaybePredOrFunc = yes(PredOrFunc),
            PorFPieces = [words(pred_or_func_to_full_str(PredOrFunc))]
        ),
        MainPieces = [words("Error: conflicting"),
            pragma_decl("require_tail_recursion"), words("pragmas for")] ++
            PorFPieces ++ [qual_sym_name_arity(SNA),
            words("or one of its modes."), nl],
        OrigPieces = [words("Earlier pragma is here."), nl],
        ( RequireTailrecOrig = suppress_tailrec_warnings(ContextOrig)
        ; RequireTailrecOrig = enable_tailrec_warnings(_, _, ContextOrig)
        ),
        Spec = error_spec($pred, severity_error, phase_parse_tree_to_hlds,
            [simplest_msg(Context, MainPieces),
            simplest_msg(ContextOrig, OrigPieces)]),
        !:Specs = [Spec | !.Specs]
    ;
        MaybeRequireTailrecOrig = no,
        proc_info_set_require_tailrec_info(RequireTailrec,
            ProcInfo0, ProcInfo),
        pred_info_set_proc_info(ProcId, ProcInfo, !PredInfo)
    ).

%---------------------%

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
            Spec = simplest_spec($pred, severity_error,
                phase_parse_tree_to_hlds, Context, Pieces),
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
            Spec = error_spec($pred, severity_error, phase_parse_tree_to_hlds,
                [Msg]),
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
            Spec = error_spec($pred, severity_error, phase_parse_tree_to_hlds,
                [Msg]),
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
            Pieces = [words("Error: this module must be compiled in a grade"),
                words("that supports memoisation."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_parse_tree_to_hlds, Context, Pieces),
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
            Spec = simplest_spec($pred, severity_error,
                phase_parse_tree_to_hlds, Context, Pieces),
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
            Spec = error_spec($pred, severity_error, phase_parse_tree_to_hlds,
                [Msg]),
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
            Spec = simplest_spec($pred, severity_error,
                phase_parse_tree_to_hlds, Context, Pieces),
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
            Spec = simplest_spec($pred, severity_error,
                phase_parse_tree_to_hlds, Context, Pieces),
            !:Specs = [Spec | !.Specs]
        )
    ).

%---------------------%

:- pred add_impl_pragma_tabled(item_mercury_status::in, item_tabled::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_impl_pragma_tabled(ItemMercuryStatus, ItemPragmaInfo,
        !ModuleInfo, !QualInfo, !Specs) :-
    ItemPragmaInfo = item_pragma_info(TabledInfo, Context, _SeqNum),
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, type_layout, TypeLayout),
    (
        TypeLayout = yes,
        item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
        module_add_pragma_tabled(TabledInfo, Context,
            ItemMercuryStatus, PredStatus, !ModuleInfo, !QualInfo, !Specs)
    ;
        TypeLayout = no,
        TabledInfo = pragma_info_tabled(TabledMethod, _, _),
        Pieces = [words("Error:"),
            pragma_decl(tabled_eval_method_to_pragma_name(TabledMethod)),
            words("declaration requires type_ctor_layout structures."),
            words("Don't use --no-type-layout to disable them."), nl],
        Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
            Context, Pieces),
        !:Specs = [Spec | !.Specs]
    ).

%---------------------------------------------------------------------------%
%
% Adding generated pragmas to the HLDS.
%

add_gen_pragma_unused_args(ItemPragmaInfo, !ModuleInfo, !Specs) :-
    ItemPragmaInfo = item_pragma_info(UnusedArgsInfo, Context, _SeqNum),
    UnusedArgsInfo = pragma_info_unused_args(PredNameArityPFMn, UnusedArgs),
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

add_gen_pragma_exceptions(ItemPragmaInfo, !ModuleInfo, !Specs) :-
    ItemPragmaInfo = item_pragma_info(ExceptionsInfo, Context, _SeqNum),
    ExceptionsInfo = pragma_info_exceptions(PredNameArityPFMn, ThrowStatus),
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

add_gen_pragma_trailing(ItemPragmaInfo, !ModuleInfo, !Specs) :-
    ItemPragmaInfo = item_pragma_info(TrailingInfo, Context, _SeqNum),
    TrailingInfo = pragma_info_trailing_info(PredNameArityPFMn,
        TrailingStatus),
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

add_gen_pragma_mm_tabling(ItemPragmaInfo, !ModuleInfo, !Specs) :-
    ItemPragmaInfo = item_pragma_info(MMTablingInfo, Context, _SeqNum),
    MMTablingInfo = pragma_info_mm_tabling_info(PredNameArityPFMn,
        TablingStatus),
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

    % add_pred_marker(PragmaName, PredNameArity, Status, Context,
    %   Marker, ConflictMarkers, !ModuleInfo, !Specs):
    %
    % Adds Marker to the marker list of the pred(s) with the given
    % PredNameArity, updating the ModuleInfo. If the named pred does not exist,
    % or the pred(s) already has/have a marker in ConflictMarkers,
    % report an error.
    %
:- pred add_pred_marker(string::in, pred_pfu_name_arity::in,
    pred_status::in, prog_context::in, pred_marker::in, list(pred_marker)::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pred_marker(PragmaName, PredSymNameArity, Status, Context,
        Marker, ConflictMarkers, !ModuleInfo, !Specs) :-
    ( if marker_must_be_exported(Marker) then
        MustBeExported = yes
    else
        MustBeExported = no
    ),
    maybe_warn_about_pfu_unknown(!.ModuleInfo, PragmaName, PredSymNameArity,
        Context, !Specs),
    do_add_pred_marker(PragmaName, PredSymNameArity, Status, MustBeExported,
        Context, add_marker_pred_info(Marker), !ModuleInfo, PredIds, !Specs),
    module_info_get_pred_id_table(!.ModuleInfo, PredIdTable),
    list.map(get_pred_markers(PredIdTable), PredIds, PredMarkerSets),
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

:- pred do_add_pred_marker(string::in, pred_pfu_name_arity::in,
    pred_status::in, bool::in, term.context::in,
    add_marker_pred_info::in(add_marker_pred_info),
    module_info::in, module_info::out, list(pred_id)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

do_add_pred_marker(PragmaName, PredSpec, Status, MustBeExported,
        Context, UpdatePredInfo, !ModuleInfo, PredIds, !Specs) :-
    PredSpec = pred_pfu_name_arity(PFU, PredSymName, UserArity),
    get_matching_pred_ids(!.ModuleInfo, PFU, PredSymName, UserArity,
        PredIds, OtherUserArities),
    (
        PredIds = [_ | _],
        module_info_get_pred_id_table(!.ModuleInfo, PredIdTable0),
        pragma_add_marker(PredIds, UpdatePredInfo, Status,
            MustBeExported, PredIdTable0, PredIdTable, WrongStatus),
        (
            WrongStatus = yes,
            pragma_status_error(pfu_to_string(PFU), PredSymName, UserArity,
                Context, PragmaName, !Specs)
        ;
            WrongStatus = no
        ),
        module_info_set_pred_id_table(PredIdTable, !ModuleInfo)
    ;
        PredIds = [],
        UserArity = user_arity(UserArityInt),
        OtherArities = list.map(project_user_arity_int, OtherUserArities),
        DescPieces = [pragma_decl(PragmaName), words("declaration")],
        report_undefined_pred_or_func_error(pfu_to_maybe_pred_or_func(PFU),
            PredSymName, UserArityInt, OtherArities, Context, DescPieces,
            !Specs)
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

    % For each pred_id in the list, add the given markers to the
    % list of markers in the corresponding pred_info.
    %
:- pred pragma_add_marker(list(pred_id)::in,
    add_marker_pred_info::in(add_marker_pred_info), pred_status::in,
    bool::in, pred_id_table::in, pred_id_table::out, bool::out) is det.

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

:- pred pragma_conflict_error(pred_pfu_name_arity::in, prog_context::in,
    string::in, list(pred_marker)::in,
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
    list.map(marker_name, ConflictMarkers, ConflictNames),
    Pieces = [words("Error:"), pragma_decl(PragmaName),
        words("declaration conflicts with previous")] ++
        list_to_pieces(ConflictNames) ++
        [words(choose_number(ConflictNames, "pragma for", "pragmas for"))]
        ++ PorFPieces ++ [unqual_sym_name_arity(SNA), suffix("."), nl],
    Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
        Context, Pieces),
    !:Specs = [Spec | !.Specs].

%---------------------%

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
:- pred get_matching_pred_ids(module_info::in,
    pred_func_or_unknown::in, sym_name::in, user_arity::in,
    list(pred_id)::out, list(user_arity)::out) is det.

get_matching_pred_ids(ModuleInfo, PFU, SymName, UserArity, PredIds,
        OtherUserArities) :-
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
                SymName, UserArity, PredIds)
        ;
            PFU = pfu_predicate,
            user_arity_pred_form_arity(pf_predicate, UserArity, PredFormArity),
            predicate_table_lookup_pf_sym_arity(PredTable0, is_fully_qualified,
                pf_predicate, SymName, PredFormArity, PredIds)
        ;
            PFU = pfu_function,
            user_arity_pred_form_arity(pf_function, UserArity, PredFormArity),
            predicate_table_lookup_pf_sym_arity(PredTable0, is_fully_qualified,
                pf_function, SymName, PredFormArity, PredIds)
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
                UserArity, OtherUserArities)
        ;
            PredIds = [_ | _],
            % There is no point in filling this in; our caller won't need it.
            OtherUserArities = []
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
            words("declaration for undeclared mode of"),
            qual_pf_sym_name_user_arity(PFNameArity), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
            Context, Pieces),
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
            UserArity = user_arity(UserArityInt),
            OtherArities = list.map(project_user_arity_int, OtherUserArities),
            DescPieces = [pragma_decl(PragmaName), words("declaration")],
            report_undefined_pred_or_func_error(yes(PredOrFunc), SymName,
                UserArityInt, OtherArities, Context, DescPieces, [], Specs)
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
            StartPieces = [words("Error: ambiguous"), p_or_f(PredOrFunc),
                words("name in"), pragma_decl(PragmaName),
                words("declaration."), nl,
                words("The possible matches are:"), nl_indent_delta(1)],
            PredIdPiecesList = list.map(
                describe_one_pred_name(ModuleInfo, should_module_qualify),
                PredIds),
            PredIdPieces = component_list_to_line_pieces(PredIdPiecesList,
                [suffix("."), nl]),
            MainPieces = StartPieces ++ PredIdPieces,
            VerbosePieces = [words("An explicit module qualifier"),
                words("may be necessary to select the right match."), nl],
            Msg = simple_msg(Context,
                [always(MainPieces),
                verbose_only(verbose_always, VerbosePieces)]),
            Spec = error_spec($pred, severity_error, phase_parse_tree_to_hlds,
                [Msg]),
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
    Pieces = [words("Internal compiler error:"),
        words("unknown"), words(pred_or_func_to_full_str(PredOrFunc)),
        qual_sym_name_arity(SNA), words("in"),
        pragma_decl(PragmaName), words("declaration."), nl],
    Spec = simplest_spec($pred, Severity, phase_parse_tree_to_hlds,
        Context, Pieces).

:- func report_ambiguous_pred_or_func(error_severity, string, prog_context,
    pred_or_func, sym_name, user_arity) = error_spec.

report_ambiguous_pred_or_func(Severity, PragmaName, Context,
        PredOrFunc, SymName, UserArity) = Spec :-
    UserArity = user_arity(UserArityInt),
    SNA = sym_name_arity(SymName, UserArityInt),
    Pieces = [words("Internal compiler error:"),
        words("ambiguous"), words(pred_or_func_to_full_str(PredOrFunc)),
        words("name"), qual_sym_name_arity(SNA), words("in"),
        pragma_decl(PragmaName), words("declaration."), nl],
    Spec = simplest_spec($pred, Severity, phase_parse_tree_to_hlds,
        Context, Pieces).

:- pred look_up_pragma_pf_sym_arity_mode_num(module_info::in,
    is_fully_qualified::in, lookup_failure_handling::in, prog_context::in,
    string::in, pred_or_func::in, sym_name::in, user_arity::in, int::in,
    maybe4(pred_id, proc_id, pred_info, proc_info)::out) is det.

look_up_pragma_pf_sym_arity_mode_num(ModuleInfo, IsFullyQualified, FailHandling,
        Context, PragmaName, PredOrFunc, SymName, UserArity, ModeNum,
        MaybePredProcId) :-
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
            Pieces = [words("Internal compiler error:"),
                words("ambiguous predicate name in"), pragma_decl(PragmaName),
                words("declaration."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_parse_tree_to_hlds, Context, Pieces),
            MaybePredProcId = error4([Spec])
        )
    ;
        MaybePredId = error1(Specs),
        MaybePredProcId = error4(Specs)
    ).

%---------------------%

:- pred maybe_warn_about_pfu_unknown(module_info::in, string::in,
    pred_pfu_name_arity::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

maybe_warn_about_pfu_unknown(ModuleInfo, PragmaName, PFUSNA, Context,
        !Specs) :-
    PFUSNA = pred_pfu_name_arity(PFU, SymName, user_arity(UserArityInt)),
    (
        ( PFU = pfu_predicate
        ; PFU = pfu_function
        )
    ;
        PFU = pfu_unknown,
        module_info_get_globals(ModuleInfo, Globals),
        module_info_get_name(ModuleInfo, ModuleName),
        globals.lookup_bool_option(Globals,
            warn_potentially_ambiguous_pragma, Warn),
        globals.get_op_mode(Globals, OpMode),
        ( if
            Warn = yes,
            OpMode = opm_top_args(opma_augment(opmau_generate_code(_))),
            SymName = qualified(ModuleName, _)
        then
            SNA = sym_name_arity(SymName, UserArityInt),
            Pieces = [words("Warning: the"), pragma_decl(PragmaName),
                words("declaration for"), unqual_sym_name_arity(SNA),
                words("does not say whether it refers"),
                words("to a predicate or to a function."), nl,
                words("(You can specify this information"),
                words("by wrapping up"), unqual_sym_name_arity(SNA),
                words("inside"), quote("pred(...)"), words("or"),
                quote("func(...)"), suffix(".)"), nl],
            Spec = simplest_spec($pred, severity_warning,
                phase_parse_tree_to_hlds, Context, Pieces),
            !:Specs = [Spec | !.Specs]
        else
            true
        )
    ).

:- pred maybe_warn_about_pfumm_unknown(module_info::in, string::in,
    pred_func_or_unknown_maybe_modes::in, sym_name::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

maybe_warn_about_pfumm_unknown(ModuleInfo, PragmaName, PFUMM, SymName, Context,
        !Specs) :-
    (
        ( PFUMM = pfumm_predicate(_)
        ; PFUMM = pfumm_function(_)
        )
    ;
        PFUMM = pfumm_unknown(user_arity(UserArityInt)),
        module_info_get_globals(ModuleInfo, Globals),
        module_info_get_name(ModuleInfo, ModuleName),
        globals.lookup_bool_option(Globals,
            warn_potentially_ambiguous_pragma, Warn),
        globals.get_op_mode(Globals, OpMode),
        ( if
            Warn = yes,
            OpMode = opm_top_args(opma_augment(opmau_generate_code(_))),
            SymName = qualified(ModuleName, _)
        then
            SNA = sym_name_arity(SymName, UserArityInt),
            Pieces = [words("Warning: the"), pragma_decl(PragmaName),
                words("declaration for"), unqual_sym_name_arity(SNA),
                words("does not say whether it refers"),
                words("to a predicate or to a function."), nl,
                words("(You can specify this information"),
                words("either by wrapping up"), unqual_sym_name_arity(SNA),
                words("inside"), quote("pred(...)"), words("or"),
                quote("func(...)"), suffix(","),
                words("or by specifying its argument modes.)"), nl],
            Spec = simplest_spec($pred, severity_warning,
                phase_parse_tree_to_hlds, Context, Pieces),
            !:Specs = [Spec | !.Specs]
        else
            true
        )
    ).

%---------------------%

:- pred pragma_status_error(string::in, sym_name::in, user_arity::in,
    prog_context::in, string::in,
    list(error_spec)::in, list(error_spec)::out) is det.

pragma_status_error(PorFStr, SymName, UserArity, Context, PragmaName,
        !Specs) :-
    UserArity = user_arity(UserArityInt),
    SNA = sym_name_arity(SymName, UserArityInt),
    Pieces = [words("Error:"), pragma_decl(PragmaName), words("declaration"),
        words("for exported"), words(PorFStr), unqual_sym_name_arity(SNA),
        words("must also be exported."), nl],
    Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
        Context, Pieces),
    !:Specs = [Spec | !.Specs].

:- func pfu_to_string(pred_func_or_unknown) = string.

pfu_to_string(PFU) = Str :-
    ( PFU = pfu_unknown,   Str = "predicate or function"
    ; PFU = pfu_predicate, Str = "predicate"
    ; PFU = pfu_function,  Str = "function"
    ).

:- func pf_to_string(pred_or_func) = string.

pf_to_string(PredOrFunc) = Str :-
    ( PredOrFunc = pf_predicate, Str = "predicate"
    ; PredOrFunc = pf_function,  Str = "function"
    ).

%---------------------------------------------------------------------------%
:- end_module hlds.make_hlds.add_pragma.
%---------------------------------------------------------------------------%
