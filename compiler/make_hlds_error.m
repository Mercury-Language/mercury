%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2006, 2008, 2012 The University of Melbourne.
% Copyright (C) 2014-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: make_hlds_error.m.
%
% Utility predicates for writing out warning and error messages when
% building the HLDS. Error messages specific to a given submodule of
% make_hlds.m are in that specific submodule; this module is for error messages
% that are either needed by more than one submodule of make_hlds.m, or are
% needed outside make_hlds.m.
%
%---------------------------------------------------------------------------%

:- module hlds.make_hlds_error.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.status.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.

:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

    % Report that an instance of the kind of entity described by the first
    % argument has more than one definition. The instance is specified
    % by the second and third arguments, and the current and previous
    % definitions are located at the two contexts given.
    %
    % If the format_piece list is not empty, it is added to the end
    % of the message we generate for the first context.
    %
:- pred report_multiply_defined(string::in, sym_name::in, user_arity::in,
    prog_context::in, prog_context::in, list(format_piece)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

    % report_undefined_pred_or_func_error(MaybePorF, SymName,
    %    UserArity, OtherUserArities, Context, DeclPieces, !Specs):
    %
    % Report a reference to SymName, which may be a predicate or function
    % (though it may not be known which), in a declaration described by
    % DeclPieces (which will usually have the form "some kind of pragma
    % declaration") with arity UserArity, when that sym_name is not defined
    % with that arity. OtherUserArities will list the arities, if any,
    % for which SymName *is* defined.
    %
:- pred report_undefined_pred_or_func_error(maybe(pred_or_func)::in,
    sym_name::in, user_arity::in, list(user_arity)::in, prog_context::in,
    list(format_piece)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred report_undeclared_mode_error(module_info::in,
    pred_id::in, pred_info::in, prog_varset::in, list(mer_mode)::in,
    list(format_piece)::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred maybe_report_undefined_pred_error(module_info::in,
    pred_or_func::in, sym_name::in, pred_form_arity::in, pred_status::in,
    maybe_class_method::in, prog_context::in, list(format_piece)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%

:- pred maybe_warn_about_pfumm_unknown(module_info::in, string::in,
    pred_func_or_unknown_maybe_modes::in, sym_name::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

:- type does_pragma_allow_modes
    --->    pragma_does_not_allow_modes
    ;       pragma_allows_modes.

:- pred warn_about_pfu_unknown(module_info::in, string::in,
    does_pragma_allow_modes::in, sym_name::in, user_arity::in,
    prog_context::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.mode_errors.
:- import_module hlds.hlds_error_util.
:- import_module hlds.pred_table.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.op_mode.
:- import_module libs.options.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_pred_decl.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_util.

:- import_module bool.
:- import_module set.
:- import_module string.
:- import_module varset.

%---------------------------------------------------------------------------%

report_multiply_defined(EntityKind, SymName, UserArity, Context, OrigContext,
        ExtraPieces, !Specs) :-
    % The flattening of source item blocks by modules.m puts
    % all items in a given section together. Since the original
    % source code may have had the contents of the different sections
    % intermingled, this may change the relative order of items.
    % Put them back in the original order for this error message.
    compare(CmpRes, OrigContext, Context),
    (
        ( CmpRes = (<)
        ; CmpRes = (=)
        ),
        FirstContext = OrigContext,
        SecondContext = Context
    ;
        CmpRes = (>),
        FirstContext = Context,
        SecondContext = OrigContext
    ),

    UserArity = user_arity(UserArityInt),
    SNA = sym_name_arity(SymName, UserArityInt),
    SecondDeclPieces = [words("Error: the"), fixed(EntityKind)] ++
        color_as_subject([qual_sym_name_arity(SNA)]) ++
        [words("is")] ++
        color_as_incorrect([words("multiply defined.")]) ++
        [nl],
    FirstDeclPieces = [words("Here is its previous definition."), nl],
    SecondDeclMsg = msg(SecondContext, SecondDeclPieces),
    FirstDeclMsg = msg(FirstContext, FirstDeclPieces),
    (
        ExtraPieces = [],
        ExtraMsgs = []
    ;
        ExtraPieces = [_ | _],
        ExtraMsgs = [msg(SecondContext, ExtraPieces)]
    ),
    Spec = error_spec($pred, severity_error, phase_pt2h,
        [SecondDeclMsg, FirstDeclMsg | ExtraMsgs]),
    !:Specs = [Spec | !.Specs].

report_undefined_pred_or_func_error(MaybePorF, SymName,
        UserArity, OtherUserArities, Context, DeclPieces, !Specs) :-
    (
        MaybePorF = no,
        SNAPrefixPieces = [],
        PredOrFuncPieces = [decl("pred"), words("or"), decl("func")]
    ;
        MaybePorF = yes(pf_predicate),
        SNAPrefixPieces = [words("predicate")],
        PredOrFuncPieces = [decl("pred")]
    ;
        MaybePorF = yes(pf_function),
        SNAPrefixPieces = [words("function")],
        PredOrFuncPieces = [decl("func")]
    ),
    UserArity = user_arity(UserArityInt),
    SNA = sym_name_arity(SymName, UserArityInt),
    MainPieces = [words("Error:") | DeclPieces] ++
        [words("for") | SNAPrefixPieces] ++
        color_as_subject([unqual_sym_name_arity(SNA)]) ++
        color_as_incorrect([words("without")]) ++
        [words("a corresponding")] ++ PredOrFuncPieces ++
        [words("declaration."), nl],
    (
        OtherUserArities = [],
        OtherArityPieces = []
    ;
        OtherUserArities = [_ | _],
        OtherUserArityInts =
            list.map(project_user_arity_int, OtherUserArities),
        list.map(string.int_to_string, OtherUserArityInts, OtherArityStrs),
        OtherArityPieces =
            [unqual_sym_name(SymName)] ++
            color_as_hint([words("does exist")]) ++
            [words("with"),
            words(choose_number(OtherArityStrs, "arity", "arities"))] ++
            fixed_list_to_color_pieces(color_correct, "and", [suffix(".")],
                OtherArityStrs) ++
            [nl]
    ),
    Pieces = MainPieces ++ OtherArityPieces,
    Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%

report_undeclared_mode_error(ModuleInfo, PredId, PredInfo, VarSet, ArgModes,
        DescPieces, Context, !Specs) :-
    PredColonPieces = describe_one_pred_name(ModuleInfo, yes(color_subject),
        should_not_module_qualify, [suffix(":")], PredId),
    strip_module_names_from_mode_list(strip_builtin_module_name,
        set_default_func, ArgModes, StrippedArgModes),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    Name = pred_info_name(PredInfo),
    MaybeDet = no,
    SubDeclStr = mercury_mode_subdecl_to_string(output_debug, PredOrFunc,
        varset.coerce(VarSet), unqualified(Name), StrippedArgModes, MaybeDet),

    MainPieces = [words("In") | DescPieces] ++ [words("for")] ++
        PredColonPieces ++ [nl,
        words("error: mode annotation specifies")] ++
        color_as_incorrect([words("undeclared mode"), quote(SubDeclStr),
            suffix(".")]) ++
        [nl],
    ProcIds = pred_info_all_procids(PredInfo),
    (
        ProcIds = [],
        VerbosePieces = [words("(There are no declared modes for this"),
            p_or_f(PredOrFunc), suffix(".)"), nl]
    ;
        ProcIds = [ProcIdsHead | ProcIdsTail],
        (
            ProcIdsTail = [],
            VerbosePieces = [words("The declared mode for this"),
                p_or_f(PredOrFunc), words("is:"),
                nl_indent_delta(1)] ++
                mode_decl_for_pred_info_to_pieces(PredInfo, ProcIdsHead) ++
                [nl_indent_delta(-1)]
        ;
            ProcIdsTail = [_ | _],
            VerbosePieces = [words("The declared modes for this"),
                p_or_f(PredOrFunc), words("are the following:"),
                nl_indent_delta(1)] ++
                pieces_list_to_line_pieces(
                    list.map(mode_decl_for_pred_info_to_pieces(PredInfo),
                        ProcIds)) ++
                [nl_indent_delta(-1)]
        )
    ),
    Msg = simple_msg(Context,
        [always(MainPieces), verbose_only(verbose_always, VerbosePieces)]),
    Spec = error_spec($pred, severity_error, phase_pt2h, [Msg]),
    !:Specs = [Spec | !.Specs].

:- func mode_decl_for_pred_info_to_pieces(pred_info, proc_id)
    = list(format_piece).

mode_decl_for_pred_info_to_pieces(PredInfo, ProcId) =
    [words(":- mode"),
    words(mode_decl_to_string(output_debug, ProcId, PredInfo)),
    suffix(".")].

%---------------------------------------------------------------------------%

maybe_report_undefined_pred_error(ModuleInfo, PredOrFunc, SymName,
        PredFormArity, Status, IsClassMethod, Context, DescPieces, !Specs) :-
    % Our caller (or one of its ancestors) will add an implicit declaration
    % for every undeclared predicate or function that has a reference to it
    % either in a clause or in some other declaration (e.g. a tabling pragma).
    % It will also mark the predicate as one whose type should be inferred.

    % We allow programmers to define predicates without declaring them first
    % if the user has specified the `--infer-types' option, unless
    % circumstances force us to require a predicate declaration anyway.
    %
    % The two relevant circumstances are:
    %
    % - predicates exported from their defining module, which must be declared
    %   to allow the compiler to put that declaration into the module's
    %   interface file without running the typechecker, and
    %
    % - predicates which implement type class methods.
    %   XXX Document the reason for the requirement here.

    DefinedInThisModule = pred_status_defined_in_this_module(Status),
    IsExported = pred_status_is_exported(Status),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, infer_types, InferTypes),
    ( if
        DefinedInThisModule = yes,
        IsExported = no,
        IsClassMethod = is_not_a_class_method,
        InferTypes = yes
    then
        true
    else
        user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
        UserArity = user_arity(UserArityInt),
        SNA = sym_name_arity(SymName, UserArityInt),
        PredOrFuncStr = pred_or_func_to_str(PredOrFunc),
        MainPieces = [invis_order_default_start(1, ""),
            words("Error:") | DescPieces] ++
            [words("for"), p_or_f(PredOrFunc)] ++
            color_as_subject([unqual_sym_name_arity(SNA)]) ++
            color_as_incorrect([words("without")]) ++
            [words("a corresponding"), decl(PredOrFuncStr),
            words("declaration."), nl],
        MainMsg = msg(Context, MainPieces),

        module_info_get_predicate_table(ModuleInfo, PredicateTable),
        predicate_table_lookup_pf_sym(PredicateTable,
            is_fully_qualified, PredOrFunc, SymName, AllArityPredIds),
        gather_porf_arities(ModuleInfo, AllArityPredIds, PredOrFunc,
            PorFArities),
        set.delete(PredFormArity, PorFArities, OtherPredFormArities),
        % The sorting is to make the error message easier to read.
        % There should not be any duplicates among OtherArities, but better
        % safe than sorry ...
        set.to_sorted_list(OtherPredFormArities, OtherPredFormAritiesList),
        FullPredOrFuncStr = pred_or_func_to_full_str(PredOrFunc),
        (
            OtherPredFormAritiesList = [],
            Spec = error_spec($pred, severity_error, phase_pt2h, [MainMsg])
        ;
            (
                OtherPredFormAritiesList = [OtherPredFormArity],
                ArityPiece = pred_form_arity_to_int_fixed(PredOrFunc,
                    OtherPredFormArity),
                OtherAritiesPieces = [words("However, a"),
                    words(FullPredOrFuncStr), words("of that name"),
                    words("does exist with arity")] ++
                    color_as_correct([ArityPiece, suffix(".")]) ++
                    [nl]
            ;
                OtherPredFormAritiesList = [_, _ | _],
                ArityPieces =
                    list.map(pred_form_arity_to_int_fixed(PredOrFunc),
                        OtherPredFormAritiesList),
                OtherAritiesPieces = [words("However,"),
                    words(FullPredOrFuncStr), suffix("s"),
                    words("of that name do exist with arities")] ++
                    piece_list_to_color_pieces(color_correct, "and",
                        [suffix(".")], ArityPieces) ++
                    [nl]
            ),
            OtherAritiesMsg = msg(Context, OtherAritiesPieces),
            Spec = error_spec($pred, severity_error, phase_pt2h,
                [MainMsg, OtherAritiesMsg])
        ),
        !:Specs = [Spec | !.Specs]
    ).

    % Given a list of pred ids, find out which of them represent
    % procedures which have the right pred_or_func field (WantedPorF),
    % and return their original arities.
    %
:- pred gather_porf_arities(module_info::in, list(pred_id)::in,
    pred_or_func::in, set(pred_form_arity)::out) is det.

gather_porf_arities(_ModuleInfo, [], _WantedPorF, set.init).
gather_porf_arities(ModuleInfo, [PredId | PredIds], WantedPorF,
        !:PorFArities) :-
    gather_porf_arities(ModuleInfo, PredIds, WantedPorF, !:PorFArities),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    PorF = pred_info_is_pred_or_func(PredInfo),
    ( if PorF = WantedPorF then
        pred_info_get_markers(PredInfo, Markers),
        ( if marker_is_present(Markers, marker_no_pred_decl) then
            % This pred has no declaration, so including its arity in the list
            % would be misleading.
            true
        else
            PredFormArity = pred_info_pred_form_arity(PredInfo),
            set.insert(PredFormArity, !PorFArities)
        )
    else
        true
    ).

:- func pred_form_arity_to_int_fixed(pred_or_func, pred_form_arity)
    = format_piece.

pred_form_arity_to_int_fixed(PredOrFunc, PredFormArity) = Component :-
    user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
    UserArity = user_arity(UserArityInt),
    Component = int_fixed(UserArityInt).

%---------------------------------------------------------------------------%

maybe_warn_about_pfumm_unknown(ModuleInfo, PragmaName, PFUMM, SymName, Context,
        !Specs) :-
    (
        ( PFUMM = pfumm_predicate(_)
        ; PFUMM = pfumm_function(_)
        )
    ;
        PFUMM = pfumm_unknown(UserArity),
        warn_about_pfu_unknown(ModuleInfo, PragmaName, pragma_allows_modes,
            SymName, UserArity, Context, WarnSpecs),
        !:Specs = WarnSpecs ++ !.Specs
    ).

warn_about_pfu_unknown(ModuleInfo, PragmaName, PragmaAllowsModes,
        SymName, UserArity, Context, Specs) :-
    module_info_get_globals(ModuleInfo, Globals),
    module_info_get_name(ModuleInfo, ModuleName),
    globals.lookup_bool_option(Globals,
        warn_potentially_ambiguous_pragma, Warn),
    globals.get_op_mode(Globals, OpMode),
    ( if
        Warn = yes,
        OpMode = opm_top_args(opma_augment(opmau_front_and_middle(_)), _),
        SymName = qualified(ModuleName, _)
    then
        UserArity = user_arity(UserArityInt),
        SNA = sym_name_arity(SymName, UserArityInt),
        (
            PragmaAllowsModes = pragma_does_not_allow_modes,
            Pieces = [words("Warning: the")] ++
                color_as_subject([pragma_decl(PragmaName),
                    words("declaration for"), unqual_sym_name_arity(SNA)]) ++
                color_as_incorrect([words("does not say whether it refers"),
                    words("to a predicate or to a function.")]) ++
                [nl,
                words("(You can specify this information"),
                words("by wrapping up"), unqual_sym_name_arity(SNA),
                words("inside"), quote("pred(...)"), words("or"),
                quote("func(...)"), suffix(".)"), nl]
        ;
            PragmaAllowsModes = pragma_allows_modes,
            Pieces = [words("Warning: the")] ++
                color_as_subject([pragma_decl(PragmaName),
                    words("declaration for"), unqual_sym_name_arity(SNA)]) ++
                color_as_incorrect([words("does not say whether it refers"),
                    words("to a predicate or to a function.")]) ++
                [nl,
                words("(You can specify this information"),
                words("either by wrapping up"), unqual_sym_name_arity(SNA),
                words("inside"), quote("pred(...)"), words("or"),
                quote("func(...)"), suffix(","),
                words("or by specifying its argument modes.)"), nl]
        ),
        Spec = spec($pred, severity_warning, phase_pt2h, Context, Pieces),
        Specs = [Spec]
    else
        Specs = []
    ).

%---------------------------------------------------------------------------%
:- end_module hlds.make_hlds_error.
%---------------------------------------------------------------------------%
