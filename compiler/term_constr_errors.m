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

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module list.

%-----------------------------------------------------------------------------%
%
% Termination 2 Errors
%

% The termination errors are all in reference to possible non-termination.
% While it is possible for pass 1 to go amiss the worst that will happen
% (barring an abnormal abort) is that the size of the arguments will be
% unconstrained.

:- type term2_error_kind
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

:- type term2_error
    --->    term2_error(prog_context, term2_error_kind).

%-----------------------------------------------------------------------------%

:- pred maybe_report_term2_errors(module_info::in, scc::in,
    list(term2_error)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_error_util.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module transform_hlds.term_util.

:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module string.

%-----------------------------------------------------------------------------%

maybe_report_term2_errors(ModuleInfo, SCC, Errors, !Specs) :-
    decide_what_term2_errors_to_report(ModuleInfo, SCC, Errors,
        MaybeErrorsToReport),
    (
        MaybeErrorsToReport = no
    ;
        MaybeErrorsToReport = yes(ErrorsToReport),
        report_term2_errors(ModuleInfo, SCC, ErrorsToReport, !Specs)
    ).

:- pred decide_what_term2_errors_to_report(module_info::in,
    scc::in, list(term2_error)::in, maybe(list(term2_error))::out) is det.

decide_what_term2_errors_to_report(ModuleInfo, SCC, Errors,
        MaybeErrorsToReport) :-
    % NOTE The code of this predicate follows the same logic as the
    % code of decide_what_term_errors_to_report in termination.m.
    % Although there are differences in the data types they operate on
    % and the options they consult, any changes here probably require
    % corresponding changes there as well.

    % The logic of this code is documented in comments in
    % decide_what_term_errors_to_report.

    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, check_termination2, NormalErrors),
    globals.lookup_bool_option(Globals, verbose_check_termination2,
        VerboseErrors),
    ( if
        IsCheckTerm =
            ( pred(PPId::in) is semidet :-
                module_info_pred_proc_info(ModuleInfo, PPId, PredInfo, _),
                not pred_info_is_imported(PredInfo),
                pred_info_get_markers(PredInfo, Markers),
                check_marker(Markers, marker_check_termination)
            ),
        set.filter(IsCheckTerm, SCC, CheckTermPPIds),
        set.is_non_empty(CheckTermPPIds)
    then
        MaybeErrorsToReport = yes(Errors)
    else if
        IsNonImported =
            ( pred(PPId::in) is semidet :-
                module_info_pred_proc_info(ModuleInfo, PPId, PredInfo, _),
                not pred_info_is_imported(PredInfo)
            ),
        set.filter(IsNonImported, SCC, NonImportedPPIds),
        set.is_non_empty(NonImportedPPIds)
    then
        (
            VerboseErrors = yes,
            MaybeErrorsToReport = yes(Errors)
        ;
            VerboseErrors = no,
            (
                NormalErrors = yes,
                IsDirect =
                    ( pred(Error::in) is semidet :-
                        Error = term2_error(_, ErrorKind),
                        term2_error_kind_is_direct(ErrorKind) = yes
                    ),
                list.filter(IsDirect, Errors, DirectErrors),
                (
                    DirectErrors = [],
                    MaybeErrorsToReport = yes(Errors)
                ;
                    DirectErrors = [_ | _],
                    MaybeErrorsToReport = yes(DirectErrors)
                )
            ;
                NormalErrors = no,
                MaybeErrorsToReport = no
            )
        )
    else
        MaybeErrorsToReport = no
    ).

%-----------------------------------------------------------------------------%

:- pred report_term2_errors(module_info::in, scc::in,
    list(term2_error)::in, list(error_spec)::in, list(error_spec)::out) is det.

report_term2_errors(ModuleInfo, SCC, Errors, !Specs) :-
    get_context_from_scc(ModuleInfo, SCC, Context),
    ( if set.is_singleton(SCC, PPId) then
        Pieces0 = [words("Termination of")],
        ProcName = describe_one_proc_name(ModuleInfo,
            should_module_qualify, PPId),
        Pieces1 = Pieces0 ++ ProcName,
        Single = yes(PPId)
    else
        Pieces0 = [words("Termination of the"),
            words("mutually recursive procedures")],
        ProcNames = describe_several_proc_names(ModuleInfo,
            should_module_qualify, set.to_sorted_list(SCC)),
        Pieces1 = Pieces0 ++ ProcNames,
        Single = no
    ),
    (
        Errors = [],
        Pieces2 = [words("not proven, for unknown reason(s).")],
        ReasonMsgsCord = cord.init
    ;
        Errors = [Error],
        Pieces2 = [words("not proven for the following reason:")],
        describe_term2_error(ModuleInfo, Single, no, Error,
            cord.init, ReasonMsgsCord)
    ;
        Errors = [_, _ | _],
        Pieces2 = [words("not proven for the following reasons:")],
        describe_term2_errors(ModuleInfo, Single, 1, Errors,
            cord.init, ReasonMsgsCord)
    ),
    ReasonMsgs = cord.list(ReasonMsgsCord),
    Msgs = [simple_msg(Context, [always(Pieces1 ++ Pieces2)]) | ReasonMsgs],
    Spec = error_spec(severity_warning, phase_termination_analysis, Msgs),
    !:Specs = [Spec | !.Specs].

:- pred describe_term2_errors(module_info::in, maybe(pred_proc_id)::in,
    int::in, list(term2_error)::in,
    cord(error_msg)::in, cord(error_msg)::out) is det.

describe_term2_errors(_, _, _, [], !ReasonMsgsCord).
describe_term2_errors(ModuleInfo, Single, ErrNum0, [Error | Errors],
        !ReasonMsgsCord) :-
    describe_term2_error(ModuleInfo, Single, yes(ErrNum0), Error,
        !ReasonMsgsCord),
    describe_term2_errors(ModuleInfo, Single, ErrNum0 + 1, Errors,
        !ReasonMsgsCord).

:- pred describe_term2_error(module_info::in, maybe(pred_proc_id)::in,
    maybe(int)::in, term2_error::in,
    cord(error_msg)::in, cord(error_msg)::out) is det.

describe_term2_error(ModuleInfo, Single, MaybeErrorNum, Error,
        !ReasonMsgsCord) :-
    Error = term2_error(Context, ErrorKind),
    term2_error_kind_description(ModuleInfo, Single, ErrorKind, Pieces0),
    (
        MaybeErrorNum = yes(N),
        string.int_to_string(N, Nstr),
        Preamble = "Reason " ++ Nstr ++ ":",
        Pieces = [fixed(Preamble) | Pieces0]
    ;
        MaybeErrorNum = no,
        Pieces = Pieces0
    ),
    ReasonMsg = error_msg(yes(Context), treat_as_first, 0, [always(Pieces)]),
    !:ReasonMsgsCord = cord.snoc(!.ReasonMsgsCord, ReasonMsg).

:- pred term2_error_kind_description(module_info::in, maybe(pred_proc_id)::in,
    term2_error_kind::in, list(format_component)::out) is det.

term2_error_kind_description(ModuleInfo, Single, Error, Pieces) :-
    (
        Error = cond_not_satisfied,
        Pieces = [words("The termination condition"),
            words("is not satisfiable."), nl]
    ;
        Error = imported_pred,
        Pieces = [words("It contains one or more"),
            words("predicates and/or functions"),
            words("imported from another module."), nl]
    ;
        Error = can_loop_proc_called(CallerPPId, CalleePPId),
        (
            Single = yes(PPId),
            expect(unify(PPId, CallerPPId), $pred, "caller outside this SCC"),
            Piece1 = [words("It")]
        ;
            Single = no,
            ProcName = describe_one_proc_name(ModuleInfo,
                should_module_qualify, CallerPPId),
            Piece1 = ProcName
        ),
        Piece2 = words("calls"),
        CalleePiece = describe_one_proc_name(ModuleInfo,
            should_module_qualify, CalleePPId),
        Pieces3 = [words("which could not be proven to terminate."), nl],
        Pieces  = Piece1 ++ [Piece2] ++ CalleePiece ++ Pieces3
    ;
        Error = horder_call,
        Pieces = [words("It contains a higher-order call."), nl]
    ;
        Error = does_not_term_pragma(PredId),
        Pieces1 = [words("There is a"), pragma_decl("does_not_terminate"),
            words("declaration for")],
        (
            Single = yes(PPId),
            PPId = proc(SCCPredId, _),
            expect(unify(PredId, SCCPredId), $pred,
                "does not terminate pragma outside this SCC"),
            Pieces2 = [words("it."), nl]
        ;
            Single = no,
            PredDesc = describe_one_pred_name(ModuleInfo,
                should_module_qualify, PredId),
            Pieces2 = PredDesc ++ [suffix("."), nl]
        ),
        Pieces = Pieces1 ++ Pieces2
    ;
        Error = foreign_proc_called(PPId),
        Name = describe_one_proc_name(ModuleInfo, should_module_qualify, PPId),
        Pieces = [words("There is a call the foreign procedure")] ++
            Name ++ [words("which is not known to terminate."), nl]
    ).

%-----------------------------------------------------------------------------%

:- func term2_error_kind_is_direct(term2_error_kind) = bool.

term2_error_kind_is_direct(ErrorKind) = IsDirect :-
    (
        ( ErrorKind = cond_not_satisfied
        ; ErrorKind = foreign_proc_called(_)
        ),
        IsDirect = yes
    ;
        ( ErrorKind = imported_pred
        ; ErrorKind = horder_call
        ; ErrorKind = does_not_term_pragma(_)
        ; ErrorKind = can_loop_proc_called(_, _)
        ),
        IsDirect = no
    ).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.term_constr_errors.
%-----------------------------------------------------------------------------%
