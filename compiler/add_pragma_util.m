%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% Copyright (C) 2023-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module contains predicates that are useful when adding
% more than one kind of pragma to the HLDS.
%
%---------------------------------------------------------------------------%

:- module hlds.make_hlds.add_pragma_util.
:- interface.

:- import_module hlds.hlds_markers.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.make_hlds_error.
:- import_module hlds.pred_table.
:- import_module hlds.status.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.

:- import_module list.
:- import_module set.

%---------------------------------------------------------------------------%

:- type lookup_failure_handling
    --->    lfh_ignore
    ;       lfh_user_error
    ;       lfh_internal_error.

:- pred look_up_pragma_pf_sym_arity(module_info::in, is_fully_qualified::in,
    lookup_failure_handling::in, prog_context::in, string::in,
    pred_or_func::in, sym_name::in, user_arity::in, maybe1(pred_id)::out)
    is det.

:- pred look_up_pragma_pf_sym_arity_mode_num(module_info::in,
    is_fully_qualified::in, lookup_failure_handling::in, prog_context::in,
    string::in, pred_or_func::in, sym_name::in, user_arity::in, int::in,
    maybe4(pred_id, proc_id, pred_info, proc_info)::out) is det.

%---------------------%

:- func report_unknown_pred_or_func(spec_severity, string, prog_context,
    pred_or_func, sym_name, user_arity) = error_spec.

:- func report_ambiguous_pred_or_func(spec_severity, string, prog_context,
    pred_or_func, sym_name, user_arity) = error_spec.

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

%---------------------%

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

%---------------------%

    % For each pred_id in the list, check whether markers present in the list
    % of conflicting markers are also present in the corresponding pred_info.
    % The output is a set of the names of the conflicting markers present.
    %
:- pred get_pred_markers(pred_id_table::in, pred_id::in,
    set(pred_marker)::out) is det.

%---------------------%

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

%---------------------%

:- pred add_marker_pred_info(pred_marker::in, pred_info::in, pred_info::out)
    is det.

%---------------------%

:- pred pragma_conflict_error(pred_pfu_name_arity::in, prog_context::in,
    string::in, set(pred_marker)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

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

%---------------------%

:- pred transform_selected_mode_of_pred(pred_id::in, pred_pf_name_arity::in,
    list(mer_mode)::in, string::in, prog_context::in,
    pred(proc_info, proc_info)::in(pred(in, out) is det),
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_code_util.
:- import_module hlds.hlds_error_util.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.prog_util.

:- import_module bool.
:- import_module map.
:- import_module maybe.
:- import_module require.

%---------------------------------------------------------------------------%

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
                Severity =
                    severity_informational(inform_ignored_pragma_errors),
                Spec = report_ambiguous_pred_or_func(Severity, PragmaName,
                    Context, PredOrFunc, SymName, UserArity),
                Specs = [Spec]
            )
        ),
        MaybePredId = error1(Specs)
    ).

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

%---------------------------------------------------------------------------%

report_unknown_pred_or_func(Severity, PragmaName, Context,
        PredOrFunc, SymName, UserArity) = Spec :-
    UserArity = user_arity(UserArityInt),
    SNA = sym_name_arity(SymName, UserArityInt),
    Pieces = [words("Internal compiler error:")] ++
        color_as_incorrect([words("unknown"), p_or_f(PredOrFunc),
            qual_sym_name_arity(SNA)]) ++
        [words("in"), pragma_decl(PragmaName), words("declaration."), nl],
    Spec = spec($pred, Severity, phase_pt2h, Context, Pieces).

report_ambiguous_pred_or_func(Severity, PragmaName, Context,
        PredOrFunc, SymName, UserArity) = Spec :-
    UserArity = user_arity(UserArityInt),
    SNA = sym_name_arity(SymName, UserArityInt),
    Pieces = [words("Internal compiler error:")] ++
        color_as_incorrect([words("ambiguous"), p_or_f(PredOrFunc),
            words("name"), qual_sym_name_arity(SNA)]) ++
        [words("in"), pragma_decl(PragmaName), words("declaration."), nl],
    Spec = spec($pred, Severity, phase_pt2h, Context, Pieces).

%---------------------------------------------------------------------------%

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
                    Severity = severity_warning(warn_nonexported_pragma),
                    Spec = spec($pred, Severity, phase_pt2h, Context, Pieces),
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

get_pred_markers(PredIdTable, PredId, Markers) :-
    map.lookup(PredIdTable, PredId, PredInfo),
    pred_info_get_markers(PredInfo, Markers).

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

add_marker_pred_info(Marker, !PredInfo) :-
    pred_info_get_markers(!.PredInfo, Markers0),
    add_marker(Marker, Markers0, Markers),
    pred_info_set_markers(Markers, !PredInfo).

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
                    Severity = severity_warning(warn_ambiguous_pragma),
                    ActualSpec = spec($pred, Severity, phase_pt2h,
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

%---------------------------------------------------------------------------%
:- end_module hlds.make_hlds.add_pragma_util.
%---------------------------------------------------------------------------%
