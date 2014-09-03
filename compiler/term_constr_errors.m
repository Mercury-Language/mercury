%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002, 2005-2007, 2010-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: term_constr_errors.m.
% Main author: juliensf.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.term_constr_errors.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.prog_data.

:- import_module io.
:- import_module list.
:- import_module pair.

%-----------------------------------------------------------------------------%
%
% Termination 2 Errors
%

% The termination errors are all in reference to possible non-termination.
% While it is possible for pass 1 to go amiss the worst that will happen
% (barring an abnormal abort) is that the size of the arguments will be
% unconstrained.

:- type termination2_error
    --->    imported_pred
            % Termination could not be proved because it depends upon
            % information from another module and that information is not
            % available.

    ;       can_loop_proc_called(pred_proc_id, pred_proc_id)
            % Termination could not be proved because the procedure called
            % another procedure that may not terminate.

    ;       cond_not_satisfied
            % Termination could not be proved because no set of decreasing
            % argument could be found.

    ;       horder_call
            % Termination could not be proved because the procedure makes
            % higher-order calls.

    ;       does_not_term_pragma(pred_id)
            % Termination could not be proved because the procedure was marked
            % with a `does_not_terminate' pragma.

    ;       foreign_proc_called(pred_proc_id).
            % Termination depends upon the properties of a piece of foreign
            % code that cannot be established as terminating.

:- type term_constr_errors.error == pair(prog_context, termination2_error).

:- type term2_errors == list(term_constr_errors.error).

%-----------------------------------------------------------------------------%

:- pred report_termination2_errors(list(pred_proc_id)::in, term2_errors::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_error_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.error_util.
:- import_module transform_hlds.term_util.  % for get_context_from_scc/5

:- import_module bool.
:- import_module int.
:- import_module maybe.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%

report_termination2_errors(SCC, Errors, !ModuleInfo, !IO) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, check_termination2, NormalErrors),
    globals.lookup_bool_option(Globals, verbose_check_termination2,
        VerboseErrors),
    (
        IsCheckTerm = (pred(PPId::in) is semidet :-
            module_info_pred_proc_info(!.ModuleInfo, PPId, PredInfo, _),
            not pred_info_is_imported(PredInfo),
            pred_info_get_markers(PredInfo, Markers),
            check_marker(Markers, marker_check_termination)
        ),
        CheckTermPPIds = list.filter(IsCheckTerm, SCC),
        list.is_not_empty(CheckTermPPIds)
    ->
        report_term_errors(SCC, Errors, !.ModuleInfo, !IO),
        io.set_exit_status(1, !IO),
        module_info_incr_errors(!ModuleInfo)
    ;
        IsNonImported = (pred(PPId::in) is semidet :-
            module_info_pred_proc_info(!.ModuleInfo, PPId, PredInfo, _),
            not pred_info_is_imported(PredInfo)
        ),
        NonImportedPPIds = list.filter(IsNonImported, SCC),
        list.is_not_empty(NonImportedPPIds),
        (
            VerboseErrors = yes,
            PrintErrors = Errors
        ;
            VerboseErrors = no,
            (
                NormalErrors = yes,
                IsNonSimple = (pred(ContextError::in) is semidet :-
                    ContextError = _ - Error,
                    not indirect_error(Error)
                ),
                PrintErrors0 = list.filter(IsNonSimple, Errors),
                % If there are no direct errors, report the indirect ones
                % instead.
                (
                    PrintErrors0 = [],
                    PrintErrors = Errors
                ;
                    PrintErrors0 = [_ | _],
                    PrintErrors = PrintErrors0
                )
            ;
                NormalErrors = no,
                fail
            )
        )
    ->
        term_constr_errors.report_term_errors(SCC, PrintErrors, !.ModuleInfo,
            !IO)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

:- pred report_term_errors(list(pred_proc_id)::in, term2_errors::in,
    module_info::in, io::di, io::uo) is det.

report_term_errors(SCC, Errors, Module, !IO) :-
    get_context_from_scc(SCC, Module, Context),
    ( SCC = [PPId] ->
        Pieces0  = [words("Termination of")],
        ProcName = describe_one_proc_name(Module, should_module_qualify,
            PPId),
        Pieces1  = Pieces0 ++ ProcName,
        Single   = yes(PPId)
    ;
        Pieces0 = [
            words("Termination of the mutually"),
            words("recursive procedures")
        ],
        ProcNames = describe_several_proc_names(Module,
            should_module_qualify, SCC),
        Pieces1 = Pieces0 ++ ProcNames,
        Single = no
    ),
    module_info_get_globals(Module, Globals),
    (
        Errors = [],
        Pieces2 = [words("not proven, for unknown reason(s).")],
        write_error_pieces(Globals, Context, 0, Pieces1 ++ Pieces2, !IO)
    ;
        Errors = [Error],
        Pieces2 = [words("not proven for the following reason:")],
        write_error_pieces(Globals, Context, 0, Pieces1 ++ Pieces2, !IO),
        output_error(Error, Single, no, 0, Module, !IO)
    ;
        Errors = [_, _ | _],
        Pieces2 = [words("not proven for the following reasons:")],
        write_error_pieces(Globals, Context, 0, Pieces1 ++ Pieces2, !IO),
        output_errors(Errors, Single, 1, 0, Module, !IO)
    ).

:- pred output_errors(term2_errors::in,
    maybe(pred_proc_id)::in, int::in, int::in, module_info::in,
    io::di, io::uo) is det.

output_errors([], _, _, _, _, !IO).
output_errors([Error | Errors], Single, ErrNum0, Indent, Module, !IO) :-
    output_error(Error, Single, yes(ErrNum0), Indent, Module, !IO),
    output_errors(Errors, Single, ErrNum0 + 1, Indent, Module, !IO).

:- pred output_error(term_constr_errors.error::in, maybe(pred_proc_id)::in,
    maybe(int)::in, int::in, module_info::in, io::di, io::uo) is det.

output_error(Context - Error, Single, ErrorNum, Indent, Module, !IO) :-
    description(Error, Single, Module, Pieces0, _),
    (
        ErrorNum = yes(N),
        string.int_to_string(N, Nstr),
        string.append_list(["Reason ", Nstr, ":"], Preamble),
        Pieces = [fixed(Preamble) | Pieces0]
    ;
        ErrorNum = no,
        Pieces = Pieces0
    ),
    module_info_get_globals(Module, Globals),
    write_error_pieces(Globals, Context, Indent, Pieces, !IO).

:- pred description(termination2_error::in,
    maybe(pred_proc_id)::in, module_info::in, list(format_component)::out,
    maybe(pred_proc_id)::out) is det.

description(cond_not_satisfied, _, _, Pieces, no) :-
    Pieces = [
        words("The termination condition"),
        words("is not satisfiable.")
    ].

description(imported_pred, _, _, Pieces, no) :-
    Pieces = [
        words("It contains one or more"),
        words("predicates and/or functions"),
        words("imported from another module.")
    ].

description(can_loop_proc_called(CallerPPId, CalleePPId),
        Single, Module, Pieces, no) :-
    (
        Single = yes(PPId),
        expect(unify(PPId, CallerPPId), $module, $pred,
            "caller outside this SCC"),
        Piece1 = [words("It")]
    ;
        Single = no,
        ProcName = describe_one_proc_name(Module, should_module_qualify,
            CallerPPId),
        Piece1 = ProcName
    ),
    Piece2 = words("calls"),
    CalleePiece = describe_one_proc_name(Module, should_module_qualify,
        CalleePPId),
    Pieces3 = [words("which could not be proven to terminate.")],
    Pieces  = Piece1 ++ [Piece2] ++ CalleePiece ++ Pieces3.

description(horder_call, _, _, Pieces, no) :-
    Pieces = [words("It contains a higher-order call.")].

description(does_not_term_pragma(PredId), Single, Module, Pieces, no) :-
    Pieces1 = [words("There is a"), pragma_decl("does_not_terminate"),
        words("declaration for")],
    (
        Single = yes(PPId),
        PPId = proc(SCCPredId, _),
        expect(unify(PredId, SCCPredId), $module, $pred,
            "does not terminate pragma outside this SCC"),
        Piece2 = [words("it.")]
    ;
        Single = no,
        Piece2Nodot = describe_one_pred_name(Module,
            should_module_qualify, PredId),
        Piece2 = Piece2Nodot ++ [words(".")]
    ),
    Pieces = Pieces1 ++ Piece2.

description(foreign_proc_called(PPId), _Single, Module, Pieces, no) :-
    Name = describe_one_proc_name(Module, should_module_qualify, PPId),
    Pieces = [words("There is a call the foreign procedure")] ++
        Name ++ [words("which is not known to terminate.")].

%-----------------------------------------------------------------------------%

:- pred indirect_error(termination2_error::in) is semidet.

indirect_error(imported_pred).
indirect_error(horder_call).
indirect_error(does_not_term_pragma(_)).
indirect_error(can_loop_proc_called(_, _)).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.term_constr_errors.
%-----------------------------------------------------------------------------%
