%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2006, 2008 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: make_hlds_error.m.
%
% Utility predicates for writing out warning and error messages when
% building the HLDS. Error messages specific to a given submodule of
% make_hlds.m are in that specific submodule; this submodule is for error
% messages that are needed by more than one submodule.
%
%-----------------------------------------------------------------------------%

:- module hlds.make_hlds.make_hlds_error.
:- interface.

:- import_module hlds.hlds_pred.
:- import_module libs.globals.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module list.

%-----------------------------------------------------------------------------%

:- pred multiple_def_error(import_status::in, sym_name::in, int::in,
    string::in, prog_context::in, prog_context::in, list(format_component)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred undefined_pred_or_func_error(sym_name::in, int::in, prog_context::in,
    list(format_component)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

    % Similar to undeclared_mode_error, but gives less information.
    % XXX perhaps we should get rid of this, and change the callers to
    % instead call undeclared_mode_error.
    %
:- pred undefined_mode_error(sym_name::in, int::in, prog_context::in,
    list(format_component)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred maybe_undefined_pred_error(globals::in, sym_name::in, int::in,
    pred_or_func::in, import_status::in, bool::in, prog_context::in,
    list(format_component)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

    % Emit an error if something is exported. (Used to check for
    % when things shouldn't be exported.)
    %
:- pred error_if_exported(import_status::in, prog_context::in,
    format_components::in, list(error_spec)::in, list(error_spec)::out) is det.

    % Emit an error reporting that something should not have occurred in
    % a module interface.
    %
:- pred error_is_exported(prog_context::in, format_components::in,
    list(error_spec)::in, list(error_spec)::out) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.mode_errors.
:- import_module hlds.hlds_error_util.
:- import_module libs.options.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_out.

:- import_module varset.

%-----------------------------------------------------------------------------%

multiple_def_error(Status, Name, Arity, DefType, Context, OrigContext,
        ExtraPieces, !Specs) :-
    ( Status = status_opt_imported ->
        % We don't take care not to read the same declaration from multiple
        % sources with inter-module optimization, so ignore multiple definition
        % errors in the items read for inter-module optimization.
        true
    ;
        Pieces1 = [words("Error:"), fixed(DefType),
            sym_name_and_arity(Name / Arity), words("multiply defined."), nl],
        Pieces2 = [words("Here is the previous definition of"),
            fixed(DefType), sym_name_and_arity(Name / Arity), suffix("."), nl],
        Msg1 = simple_msg(Context, [always(Pieces1)]),
        Msg2 = error_msg(yes(OrigContext), treat_as_first, 0,
            [always(Pieces2)]),
        (
            ExtraPieces = [],
            ExtraMsgs = []
        ;
            ExtraPieces = [_ | _],
            ExtraMsgs = [simple_msg(Context, [always(ExtraPieces)])]
        ),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
            [Msg1, Msg2] ++ ExtraMsgs),
        !:Specs = [Spec | !.Specs]
    ).

undefined_pred_or_func_error(Name, Arity, Context, DescPieces, !Specs) :-
    Pieces = [words("Error:") | DescPieces] ++ [words("for"),
        sym_name_and_arity(Name / Arity),
        words("without corresponding"), decl("pred"), words("or"),
        decl("func"), words("declaration.")],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

undefined_mode_error(Name, Arity, Context, DescPieces, !Specs) :-
    Pieces = [words("Error:") | DescPieces] ++ [words("for"),
        sym_name_and_arity(Name / Arity),
        words("specifies non-existent mode.")],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

    % Similar to undefined_mode_error, but gives more information.
    % XXX the documentation here should be somewhat less circular.
    %
:- func undeclared_mode_error(list(mer_mode), prog_varset,
    pred_id, pred_info, module_info, prog_context) = error_spec.

undeclared_mode_error(ModeList, VarSet, PredId, PredInfo, ModuleInfo, Context)
        = Spec :-
    PredIdPieces = describe_one_pred_name(ModuleInfo,
        should_not_module_qualify, PredId),
    strip_builtin_qualifiers_from_mode_list(ModeList, StrippedModeList),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    Name = pred_info_name(PredInfo),
    MaybeDet = no,
    SubDeclStr = mercury_mode_subdecl_to_string(PredOrFunc,
        varset.coerce(VarSet), unqualified(Name), StrippedModeList,
        MaybeDet, Context),

    MainPieces = [words("In clause for")] ++ PredIdPieces ++ [suffix(":"), nl,
        words("error: mode annotation specifies undeclared mode"),
        quote(SubDeclStr), suffix("."), nl],
    ProcIds = pred_info_all_procids(PredInfo),
    (
        ProcIds = [],
        VerbosePieces = [words("(There are no declared modes for this"),
            p_or_f(PredOrFunc), suffix(".)"), nl]
    ;
        ProcIds = [_ | _],
        VerbosePieces = [words("The declared modes for this"),
            p_or_f(PredOrFunc), words("are the following:"),
            nl_indent_delta(1)] ++
            component_list_to_line_pieces(
                list.map(mode_decl_for_pred_info_to_pieces(PredInfo), ProcIds),
                []) ++
            [nl_indent_delta(-1)]
    ),
    Msg = simple_msg(Context,
        [always(MainPieces), verbose_only(VerbosePieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]).

:- func mode_decl_for_pred_info_to_pieces(pred_info, proc_id)
    = list(format_component).

mode_decl_for_pred_info_to_pieces(PredInfo, ProcId) =
    [words(":- mode"), words(mode_decl_to_string(ProcId, PredInfo)),
    suffix(".")].

maybe_undefined_pred_error(Globals, Name, Arity, PredOrFunc, Status,
        IsClassMethod, Context, DescPieces, !Specs) :-
    % This is not considered an unconditional error anymore:
    % if there is no `:- pred' or `:- func' declaration,
    % and the declaration is local, and not a type class method,
    % and the `--infer-types' option was specified,
    % then we just add an implicit declaration for that predicate or
    % function, marking it as one whose type will be inferred.

    DefinedInThisModule = status_defined_in_this_module(Status),
    IsExported = status_is_exported(Status),
    globals.lookup_bool_option(Globals, infer_types, InferTypes),
    (
        DefinedInThisModule = yes,
        IsExported = no,
        IsClassMethod = no,
        InferTypes = yes
    ->
        true
    ;
        Pieces = [words("Error:") | DescPieces] ++ [words("for"),
            simple_call(simple_call_id(PredOrFunc, Name, Arity)), nl,
            words("without corresponding"),
            decl(pred_or_func_to_str(PredOrFunc)), words("declaration."), nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ).

error_is_exported(Context, ItemPieces, !Specs) :-
    Pieces = [words("Error:")] ++ ItemPieces ++
        [words("in module interface."), nl],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

error_if_exported(Status, Context, Item, !Specs) :-
    ( Status = status_exported ->
        error_is_exported(Context, Item, !Specs)
    ;
        true
    ).

%----------------------------------------------------------------------------%
:- end_module hlds.make_hlds.make_hlds_error.
%----------------------------------------------------------------------------%
