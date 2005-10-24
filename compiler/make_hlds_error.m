%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Utility predicates for writing out warning and error messages when
% building the HLDS. Error messages specific to a given submodule of
% make_hlds.m are in that specific submodule; this submodule is for error
% messages that are needed by more than one submodule.
%

%-----------------------------------------------------------------------------%

:- module hlds__make_hlds__make_hlds_error.
:- interface.

:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.
:- import_module mdbcomp__prim_data.
:- import_module parse_tree__prog_data.

:- import_module bool.
:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

:- pred multiple_def_error(import_status::in, sym_name::in, int::in,
    string::in, prog_context::in, prog_context::in, bool::out,
    io::di, io::uo) is det.

:- pred undefined_pred_or_func_error(sym_name::in, int::in, prog_context::in,
    string::in, io::di, io::uo) is det.

    % Similar to undeclared_mode_error, but gives less information.
    % XXX perhaps we should get rid of this, and change the callers to
    % instead call undeclared_mode_error.
    %
:- pred undefined_mode_error(sym_name::in, int::in, prog_context::in,
    string::in, io::di, io::uo) is det.

    % Similar to undefined_mode_error, but gives more information.
    % XXX the documentation here should be somewhat less circular.
    %
:- pred undeclared_mode_error(list(mer_mode)::in, prog_varset::in,
    pred_id::in, pred_info::in, module_info::in, prog_context::in,
    io::di, io::uo) is det.

:- pred maybe_undefined_pred_error(sym_name::in, int::in, pred_or_func::in,
    import_status::in, bool::in, prog_context::in, string::in,
    io::di, io::uo) is det.
    
    % Emit an error if something is exported.  (Used to check for
    % when things shouldn't be exported.)
    %
:- pred error_if_exported(import_status::in, prog_context::in, string::in,
    io::di, io::uo) is det.

    % Emit an error reporting that something should not have occurred in
    % a module interface.
    %
:- pred error_is_exported(prog_context::in, string::in, io::di, io::uo)
    is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds__mode_errors.
:- import_module hlds__hlds_out.
:- import_module libs__globals.
:- import_module libs__options.
:- import_module parse_tree__error_util.
:- import_module parse_tree__mercury_to_mercury.
:- import_module parse_tree__prog_mode.
:- import_module parse_tree__prog_out.

:- import_module std_util.
:- import_module string.
:- import_module varset.

%-----------------------------------------------------------------------------%

multiple_def_error(Status, Name, Arity, DefType, Context, OrigContext,
        FoundError, !IO) :-
    ( Status \= opt_imported ->
        Pieces = [words("Error:"),
            fixed(DefType), sym_name_and_arity(Name / Arity),
            words("multiply defined.")],
        write_error_pieces(Context, 0, Pieces, !IO),
        OrigPieces = [words("Here is the previous definition of"),
            fixed(DefType), sym_name_and_arity(Name / Arity),
            suffix(".")],
        write_error_pieces(OrigContext, 0, OrigPieces, !IO),
        io__set_exit_status(1, !IO),
        FoundError = yes
    ;
        % We don't take care not to read the same declaration
        % from multiple sources with inter-module optimization
        % so ignore multiple definition errors in the items read
        % for inter-module optimization.
        FoundError = no
    ).

undefined_pred_or_func_error(Name, Arity, Context, Description, !IO) :-
    % This used to say `preceding' instead of `corresponding.'
    % Which is more correct?
    Pieces = [words("Error:"), words(Description), words("for"),
        sym_name_and_arity(Name / Arity),
        words("without corresponding `pred' or `func' declaration.")],
    write_error_pieces(Context, 0, Pieces, !IO),
    io__set_exit_status(1, !IO).

undefined_mode_error(Name, Arity, Context, Description, !IO) :-
    Pieces = [words("Error:"), words(Description), words("for"),
        sym_name_and_arity(Name / Arity),
        words("specifies non-existent mode.")],
    write_error_pieces(Context, 0, Pieces, !IO),
    io__set_exit_status(1, !IO).

undeclared_mode_error(ModeList, VarSet, PredId, PredInfo, ModuleInfo,
        Context, !IO) :-
    prog_out__write_context(Context, !IO),
    io__write_string("In clause for ", !IO),
    hlds_out__write_pred_id(ModuleInfo, PredId, !IO),
    io__write_string(":\n", !IO),
    prog_out__write_context(Context, !IO),
    io__write_string(
        "  error: mode annotation specifies undeclared mode\n", !IO),
    prog_out__write_context(Context, !IO),
    io__write_string("  `", !IO),
    strip_builtin_qualifiers_from_mode_list(ModeList, StrippedModeList),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    Name = pred_info_name(PredInfo),
    MaybeDet = no,
    mercury_output_mode_subdecl(PredOrFunc, varset__coerce(VarSet),
        unqualified(Name), StrippedModeList, MaybeDet, Context, !IO),
    io__write_string("'\n", !IO),
    prog_out__write_context(Context, !IO),
    io__write_string("  of ", !IO),
    hlds_out__write_pred_id(ModuleInfo, PredId, !IO),
    io__write_string(".\n", !IO),
    globals__io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
    ProcIds = pred_info_all_procids(PredInfo),
    (
        ProcIds = [],
        prog_out__write_context(Context, !IO),
        io__write_string("  (There are no declared modes for this ", !IO),
        write_pred_or_func(PredOrFunc, !IO),
        io__write_string(".)\n", !IO)
    ;
        ProcIds = [_ | _],
        (
            VerboseErrors = yes,
            io__write_string("\tThe declared modes for this ", !IO),
            write_pred_or_func(PredOrFunc, !IO),
            io__write_string(" are the following:\n", !IO),
            list__foldl(output_mode_decl_for_pred_info(PredInfo), ProcIds, !IO)
        ;
            VerboseErrors = no,
            globals.io_set_extra_error_info(yes, !IO)
        )
    ).

:- pred output_mode_decl_for_pred_info(pred_info::in, proc_id::in,
    io::di, io::uo) is det.

output_mode_decl_for_pred_info(PredInfo, ProcId, !IO) :-
    io__write_string("\t\t:- mode ", !IO),
    output_mode_decl(ProcId, PredInfo, !IO),
    io__write_string(".\n", !IO).

    % This is not considered an unconditional error anymore:
    % if there is no `:- pred' or `:- func' declaration,
    % and the declaration is local, and not a type class method,
    % and the `--infer-types' option was specified,
    % then we just add an implicit declaration for that predicate or
    % function, marking it as one whose type will be inferred.
    %
    % If this module is for a query generated by the Aditi dbsh
    % (--aditi-only is set), allow mode declarations for exported
    % predicates with no `:- pred' or `:- func' declaration.
    % The predicate will never be called from a compiled Mercury
    % procedure. The RL bytecode for the predicate will be called
    % directly using information from the generated
    % `<module>.derived_schema' file to work out the argument
    % types of the output relation.
    %
maybe_undefined_pred_error(Name, Arity, PredOrFunc, Status, IsClassMethod,
        Context, Description, !IO) :-
    status_defined_in_this_module(Status, DefinedInThisModule),
    status_is_exported(Status, IsExported),
    globals__io_lookup_bool_option(infer_types, InferTypes, !IO),
    globals__io_lookup_bool_option(aditi_only, AditiOnly, !IO),
    (
        (
            DefinedInThisModule = yes,
            IsExported = no,
            IsClassMethod = no,
            InferTypes = yes
        ;
            AditiOnly = yes
        )
    ->
        true
    ;
        Pieces = [words("Error:"), words(Description), words("for"),
            words(simple_call_id_to_string(PredOrFunc, Name, Arity)), nl,
            words("without preceding"),
            fixed("`" ++ pred_or_func_to_str(PredOrFunc) ++ "'"),
            words("declaration.")],
        write_error_pieces(Context, 0, Pieces, !IO),
        io__set_exit_status(1, !IO)
    ).

%     % This predicate is currently unused.
% 
% :- pred clause_for_imported_pred_error(sym_name::in, arity::in,
%     pred_or_func::in, prog_context::in, io::di, io::uo) is det.
% 
% clause_for_imported_pred_error(Name, Arity, PredOrFunc, Context, !IO) :-
%     Pieces = [words("Error: clause for imported"),
%         pred_or_func(PredOrFunc),
%         sym_name_and_arity(Name / Arity),
%         suffix(".")],
%     write_error_pieces(Context, 0, Pieces, !IO),
%     io__set_exit_status(1, !IO).

error_is_exported(Context, Message, !IO) :-
    Error = [   words("Error:"),
                fixed(Message), 
                words("in module interface.")],
    write_error_pieces(Context, 0, Error, !IO),
    io.set_exit_status(1, !IO).

error_if_exported(Status, Context, Message, !IO) :-
    ( Status = exported ->
        error_is_exported(Context, Message, !IO)
    ;
        true
    ).

%----------------------------------------------------------------------------%
:- end_module make_hlds_error.
%----------------------------------------------------------------------------%
