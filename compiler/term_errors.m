%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2000, 2003-2006, 2010-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: term_errors.m.
% Main author: crs.
%
% This module prints out the various error messages that are produced by the
% various modules of termination analysis.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.term_errors.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module bag.
:- import_module bool.
:- import_module list.

%-----------------------------------------------------------------------------%

:- type term_error_kind
    --->    pragma_foreign_code
            % The analysis result depends on the change constant
            % of a piece of pragma foreign code, (which cannot be
            % obtained without analyzing the foreign code, which is
            % something we cannot do).
            % Valid in both passes.

    ;       imported_pred
            % The SCC contains some imported procedures,
            % whose code is not accessible.

    ;       can_loop_proc_called(pred_proc_id, pred_proc_id)
            % can_loop_proc_called(Caller, Callee, Context)
            % The call from Caller to Callee at the associated
            % context is to a procedure (Callee) whose termination
            % info is set to can_loop.
            % Although this error does not prevent us from
            % producing argument size information, it would
            % prevent us from proving termination.
            % We look for this error in pass 1; if we find it,
            % we do not perform pass 2.

    ;       horder_args(pred_proc_id, pred_proc_id)
            % horder_args(Caller, Callee, Context)
            % The call from Caller to Callee at the associated
            % context has some arguments of a higher order type.
            % Valid in both passes.

    ;       horder_call
            % horder_call
            % There is a higher order call at the associated
            % context.  Valid in both passes.

    ;       method_call
            % method_call
            % There is a call to a typeclass method at the associated
            % context.  Valid in both passes.

    ;       inf_termination_const(pred_proc_id, pred_proc_id)
            % inf_termination_const(Caller, Callee)
            % The call from Caller to Callee at the associated
            % context is to a procedure (Callee) whose arg size
            % info is set to infinite.
            % Valid in both passes.

    ;       ho_inf_termination_const(pred_proc_id, list(pred_proc_id))
            % ho_inf_termination_const(Caller, Callees).
            % Caller makes a call to either call/N or apply/N
            % at the associated context.  'Callees' gives the
            % possible values of the higher-order argument.

    ;       not_subset(pred_proc_id, bag(prog_var), bag(prog_var))
            % not_subset(Proc, SupplierVariables, InHeadVariables)
            % This error occurs when the bag of active variables
            % is not a subset of the input head variables.
            % Valid error only in pass 1.

    ;       inf_call(pred_proc_id, pred_proc_id)
            % inf_call(Caller, Callee)
            % The call from Caller to Callee at the associated
            % context has infinite weight.
            % Valid error only in pass 2.

    ;       cycle(pred_proc_id, assoc_list(pred_proc_id, prog_context))
            % cycle(StartPPId, CallSites)
            % In the cycle of calls starting at StartPPId and
            % going through the named call sites may be an
            % infinite loop.
            % Valid error only in pass 2.

    ;       no_eqns
            % There are no equations in this SCC.
            % This has 2 possible causes. (1) If the predicate has
            % no output arguments, no equations will be created
            % for them. The change constant of the predicate is
            % undefined, but it will also never be used.
            % (2) If the procedure is a builtin predicate, with
            % an empty body, traversal cannot create any equations.
            % Valid error only in pass 1.

    ;       too_many_paths
            % There are too many distinct paths to be analyzed.
            % Valid in both passes (which analyze different sets
            % of paths).

    ;       solver_failed
            % The solver could not find finite termination
            % constants for the procedures in the SCC.
            % Valid only in pass 1.

    ;       is_builtin(pred_id)
            % The termination constant of the given builtin is
            % set to infinity; this happens when the type of at
            % least one output argument permits a norm greater
            % than zero.

    ;       does_not_term_pragma(pred_id)
            % The given procedure has a does_not_terminate pragma.

    ;       inconsistent_annotations
            % The pragma terminates/does_not_terminate declarations
            % for the procedures in this SCC are inconsistent.

    ;       does_not_term_foreign(pred_proc_id).
            % The procedure contains foreign code that may
            % make calls back to Mercury.  By default such
            % code is assumed to be non-terminating.

:- type term_error
    --->    term_error(prog_context, term_error_kind).

:- pred report_term_errors(module_info::in, scc::in, list(term_error)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

    % An error is considered an indirect error if it is due either to a
    % language feature we cannot analyze or due to an error in another part
    % of the code. By default, we do not issue warnings about indirect errors,
    % since in the first case, the programmer cannot do anything about it,
    % and in the second case, the piece of code that the programmer *can* do
    % something about is not this piece.
    %
:- func term_error_kind_is_direct_error(term_error_kind) = bool.

    % A fatal error is one that prevents pass 2 from proving termination.
    %
:- func term_error_kind_is_fatal_error(term_error_kind) = bool.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_error_util.
:- import_module parse_tree.prog_data_pragma.
:- import_module transform_hlds.term_util.

:- import_module cord.
:- import_module int.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

report_term_errors(ModuleInfo, SCC, Errors, !Specs) :-
    get_context_from_scc(ModuleInfo, SCC, Context),
    ( if set.is_singleton(SCC, PPId) then
        Pieces1 = [words("Termination of")] ++
            describe_one_proc_name(ModuleInfo, should_module_qualify, PPId),
        Single = yes(PPId)
    else
        Pieces1 = [words("Termination of the "),
            words("mutually recursive procedures")] ++
            describe_several_proc_names(ModuleInfo,
                should_module_qualify, set.to_sorted_list(SCC)),
        Single = no
    ),
    (
        Errors = [],
        % XXX This should never happen but for some reason, it often does.
        % error("empty list of errors")
        Pieces2 = [words("not proven, for unknown reason(s).")],
        Pieces = Pieces1 ++ Pieces2,
        ReasonMsgsCord = cord.init
    ;
        Errors = [Error],
        Pieces2 = [words("not proven for the following reason:")],
        Pieces = Pieces1 ++ Pieces2,
        describe_term_error(ModuleInfo, Single, Error, no,
            cord.init, ReasonMsgsCord, !Specs)
    ;
        Errors = [_, _ | _],
        Pieces2 = [words("not proven for the following reasons:")],
        Pieces = Pieces1 ++ Pieces2,
        describe_term_errors(ModuleInfo, Single, Errors, 1,
            cord.init, ReasonMsgsCord, !Specs)
    ),
    ReasonMsgs = cord.list(ReasonMsgsCord),
    Msgs = [simplest_msg(Context, Pieces) | ReasonMsgs],
    Spec = error_spec($pred, severity_warning, phase_termination_analysis,
        Msgs),
    !:Specs = [Spec | !.Specs].

:- pred report_arg_size_errors(module_info::in, scc::in, list(term_error)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_arg_size_errors(ModuleInfo, SCC, Errors, !Specs) :-
    get_context_from_scc(ModuleInfo, SCC, Context),
    ( if set.is_singleton(SCC, PPId) then
        Pieces1 = [words("Termination constant of")] ++
            describe_one_proc_name(ModuleInfo, should_module_qualify, PPId),
        Single = yes(PPId)
    else
        Pieces1 = [words("Termination constants"),
            words("of the mutually recursive procedures")] ++
            describe_several_proc_names(ModuleInfo,
                should_module_qualify, set.to_sorted_list(SCC)),
        Single = no
    ),
    Piece2 = words("set to infinity for the following"),
    (
        Errors = [],
        unexpected($pred, "empty list of errors")
    ;
        Errors = [Error],
        Piece3 = words("reason:"),
        Pieces = Pieces1 ++ [Piece2, Piece3],
        describe_term_error(ModuleInfo, Single, Error, no,
            cord.init, ReasonMsgsCord, !Specs)
    ;
        Errors = [_, _ | _],
        Piece3 = words("reasons:"),
        Pieces = Pieces1 ++ [Piece2, Piece3],
        describe_term_errors(ModuleInfo, Single, Errors, 1,
            cord.init, ReasonMsgsCord, !Specs)
    ),
    ReasonMsgs = cord.list(ReasonMsgsCord),
    Msgs = [simplest_msg(Context, Pieces) | ReasonMsgs],
    Spec = error_spec($pred, severity_warning, phase_termination_analysis,
        Msgs),
    !:Specs = [Spec | !.Specs].

:- pred describe_term_errors(module_info::in, maybe(pred_proc_id)::in,
    list(term_error)::in, int::in, cord(error_msg)::in, cord(error_msg)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

describe_term_errors(_, _, [], _, !Msgs, !Specs).
describe_term_errors(ModuleInfo, Single, [Error | Errors], ErrNum0,
        !Msgs, !Specs) :-
    describe_term_error(ModuleInfo, Single, Error, yes(ErrNum0),
        !Msgs, !Specs),
    describe_term_errors(ModuleInfo, Single, Errors, ErrNum0 + 1,
        !Msgs, !Specs).

:- pred describe_term_error(module_info::in, maybe(pred_proc_id)::in,
    term_error::in, maybe(int)::in, cord(error_msg)::in, cord(error_msg)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

describe_term_error(ModuleInfo, Single, TermErrorContext, ErrorNum,
        !ReasonMsgs, !Specs) :-
    TermErrorContext = term_error(Context, ErrorKind),
    term_error_kind_description(ModuleInfo, Single, ErrorKind, Pieces0,
        Reason),
    (
        ErrorNum = yes(N),
        string.int_to_string(N, Nstr),
        Preamble = "Reason " ++ Nstr ++ ":",
        Pieces = [fixed(Preamble) | Pieces0]
    ;
        ErrorNum = no,
        Pieces = Pieces0
    ),
    ReasonMsg = error_msg(yes(Context), treat_as_first, 0, [always(Pieces)]),
    !:ReasonMsgs = cord.snoc(!.ReasonMsgs, ReasonMsg),
    (
        Reason = yes(InfArgSizePPId),
        lookup_proc_arg_size_info(ModuleInfo, InfArgSizePPId, ArgSize),
        ( if ArgSize = yes(infinite(ArgSizeErrors)) then
            % XXX Should we add a Msg about the relevance of the spec
            % added by the folliwng call?
            % XXX the next line is cheating
            ArgSizePPIdSCC = set.make_singleton_set(InfArgSizePPId),
            report_arg_size_errors(ModuleInfo, ArgSizePPIdSCC, ArgSizeErrors,
                !Specs)
        else
            unexpected($pred,
                "inf arg size procedure does not have inf arg size")
        )
    ;
        Reason = no
    ).

:- pred term_error_kind_description(module_info::in, maybe(pred_proc_id)::in,
    term_error_kind::in, list(format_component)::out,
    maybe(pred_proc_id)::out) is det.

term_error_kind_description(ModuleInfo, Single, ErrorKind, Pieces, Reason) :-
    (
        ErrorKind = horder_call,
        Pieces = [words("It contains a higher order call."), nl],
        Reason = no
    ;
        ErrorKind = method_call,
        Pieces = [words("It contains a typeclass method call."), nl],
        Reason = no
    ;
        ErrorKind = pragma_foreign_code,
        Pieces = [words("It depends on the properties of"),
            words("foreign language code included via a"),
            pragma_decl("foreign_proc"), words("declaration."), nl],
        Reason = no
    ;
        ErrorKind = inf_call(CallerPPId, CalleePPId),
        (
            Single = yes(PPId),
            expect(unify(PPId, CallerPPId), $pred,
                "inf_call: caller outside this SCC"),
            Pieces1 = [words("It")]
        ;
            Single = no,
            Pieces1 = describe_one_proc_name(ModuleInfo,
                should_module_qualify, CallerPPId)
        ),
        Piece2 = words("calls"),
        CalleePieces = describe_one_proc_name(ModuleInfo,
            should_module_qualify, CalleePPId),
        Pieces3 = [words("with an unbounded increase"),
            words("in the size of the input arguments."), nl],
        Pieces = Pieces1 ++ [Piece2] ++ CalleePieces ++ Pieces3,
        Reason = no
    ;
        ErrorKind = can_loop_proc_called(CallerPPId, CalleePPId),
        (
            Single = yes(PPId),
            expect(unify(PPId, CallerPPId), $pred,
                "can_loop_proc_called: caller outside this SCC"),
            Pieces1 = [words("It")]
        ;
            Single = no,
            Pieces1 = describe_one_proc_name(ModuleInfo,
                should_module_qualify, CallerPPId)
        ),
        Piece2 = words("calls"),
        CalleePieces = describe_one_proc_name(ModuleInfo,
            should_module_qualify, CalleePPId),
        Piece3 = words("which could not be proven to terminate."),
        Pieces = Pieces1 ++ [Piece2] ++ CalleePieces ++ [Piece3, nl],
        Reason = no
    ;
        ErrorKind = imported_pred,
        Pieces = [words("It contains one or more"),
            words("predicates and/or functions"),
            words("imported from another module."), nl],
        Reason = no
    ;
        ErrorKind = horder_args(CallerPPId, CalleePPId),
        (
            Single = yes(PPId),
            expect(unify(PPId, CallerPPId), $pred,
                "horder_args: caller outside this SCC"),
            Pieces1 = [words("It")]
        ;
            Single = no,
            Pieces1 = describe_one_proc_name(ModuleInfo,
                should_module_qualify, CallerPPId)
        ),
        Piece2 = words("calls"),
        CalleePieces = describe_one_proc_name(ModuleInfo,
            should_module_qualify, CalleePPId),
        Piece3 = words("with one or more higher order arguments."),
        Pieces = Pieces1 ++ [Piece2] ++ CalleePieces ++ [Piece3, nl],
        Reason = no
    ;
        ErrorKind = inf_termination_const(CallerPPId, CalleePPId),
        (
            Single = yes(PPId),
            expect(unify(PPId, CallerPPId), $pred,
                "inf_termination_const: caller outside this SCC"),
            Pieces1 = [words("It")]
        ;
            Single = no,
            Pieces1 = describe_one_proc_name(ModuleInfo,
                should_module_qualify, CallerPPId)
        ),
        Piece2 = words("calls"),
        CalleePieces = describe_one_proc_name(ModuleInfo,
            should_module_qualify, CalleePPId),
        Piece3 = words("which has a termination constant of infinity."),
        Pieces = Pieces1 ++ [Piece2] ++ CalleePieces ++ [Piece3, nl],
        Reason = yes(CalleePPId)
    ;
        ErrorKind = ho_inf_termination_const(CallerPPId, _ClosurePPIds),
        % XXX We should print out the names of the non-terminating closures.
        (
            Single = yes(PPId),
            expect(unify(PPId, CallerPPId), $pred,
                "ho_info_termination_const: caller outside this SCC"),
            Pieces1 = [words("It")]
        ;
            Single = no,
            Pieces1 = describe_one_proc_name(ModuleInfo,
                should_module_qualify, CallerPPId)
        ),
        Pieces2 = [words("makes one or more higher-order calls."),
            words("Each of these higher-order calls has a"),
            words("termination constant of infinity."), nl],
        Pieces = Pieces1 ++ Pieces2,
        Reason = no
    ;
        ErrorKind = not_subset(ProcPPId, OutputSuppliers, HeadVars),
        (
            Single = yes(PPId),
            ( if PPId = ProcPPId then
                Pieces1 = [words("The set of its output supplier variables")]
            else
                % XXX this should never happen (but it does)
                % error("not_subset outside this SCC"),
                PPIdPieces = describe_one_proc_name(ModuleInfo,
                    should_module_qualify, ProcPPId),
                Pieces1 = [words("The set of output supplier variables of")
                    | PPIdPieces]
            )
        ;
            Single = no,
            PPIdPieces = describe_one_proc_name(ModuleInfo,
                should_module_qualify, ProcPPId),
            Pieces1 = [words("The set of output supplier variables of") |
                PPIdPieces]
        ),
        ProcPPId = proc(PredId, ProcId),
        module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
        proc_info_get_varset(ProcInfo, Varset),
        term_errors_var_bag_description(OutputSuppliers, Varset,
            OutputSuppliersNames),
        list.map((pred(OS::in, FOS::out) is det :- FOS = fixed(OS)),
            OutputSuppliersNames, OutputSuppliersPieces),
        Pieces3 = [words("is not a subset of the head variables")],
        term_errors_var_bag_description(HeadVars, Varset, HeadVarsNames),
        list.map((pred(HV::in, FHV::out) is det :- FHV = fixed(HV)),
            HeadVarsNames, HeadVarsPieces),
        Pieces = Pieces1 ++ OutputSuppliersPieces ++ Pieces3 ++
            HeadVarsPieces ++ [suffix("."), nl],
        Reason = no
    ;
        ErrorKind = cycle(_StartPPId, CallSites),
        ( if CallSites = [DirectCall] then
            SitePieces = describe_one_call_site(ModuleInfo,
                should_module_qualify, DirectCall),
            Pieces = [words("At the recursive call to") | SitePieces] ++
                [words("the arguments are not guaranteed"),
                words("to decrease in size."), nl]
        else
            Pieces = [words("In the recursive cycle through the calls to")] ++
                describe_several_call_sites(ModuleInfo,
                    should_module_qualify, CallSites) ++
                [words("the arguments are"),
                    words("not guaranteed to decrease in size."), nl]
        ),
        Reason = no
    ;
        ErrorKind = too_many_paths,
        Pieces = [words("There are too many execution paths"),
            words("for the analysis to process."), nl],
        Reason = no
    ;
        ErrorKind = no_eqns,
        Pieces = [words("The analysis was unable to form any constraints"),
            words("between the arguments of this group of procedures."), nl],
        Reason = no
    ;
        ErrorKind = solver_failed,
        Pieces = [words("The solver found the constraints produced"),
            words("by the analysis to be infeasible."), nl],
        Reason = no
    ;
        ErrorKind = is_builtin(_PredId),
        % XXX expect(unify(Single, yes(_)), $pred,
        %       "builtin not alone in SCC"),
        Pieces = [words("It is a builtin predicate."), nl],
        Reason = no
    ;
        ErrorKind = does_not_term_pragma(PredId),
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
            Pieces2 = describe_one_pred_name(ModuleInfo,
                should_module_qualify, PredId) ++ [suffix("."), nl]
        ),
        Pieces = Pieces1 ++ Pieces2,
        Reason = no
    ;
        ErrorKind = inconsistent_annotations,
        Pieces = [words("The termination pragmas are inconsistent."), nl],
        Reason = no
    ;
        ErrorKind = does_not_term_foreign(_),
        Pieces = [words("It contains foreign code that"),
            words("may make one or more calls back to Mercury."), nl],
        Reason = no
    ).

%----------------------------------------------------------------------------%

:- pred term_errors_var_bag_description(bag(prog_var)::in, prog_varset::in,
    list(string)::out) is det.

term_errors_var_bag_description(HeadVars, Varset, Pieces) :-
    bag.to_assoc_list(HeadVars, HeadVarCountList),
    term_errors_var_bag_description_2(HeadVarCountList, Varset, yes, Pieces).

:- pred term_errors_var_bag_description_2(assoc_list(prog_var, int)::in,
    prog_varset::in, bool::in, list(string)::out) is det.

term_errors_var_bag_description_2([], _, _, ["{}"]).
term_errors_var_bag_description_2([Var - Count | VarCounts], Varset, First,
        [Piece | Pieces]) :-
    varset.lookup_name(Varset, Var, VarName),
    ( if Count > 1 then
        string.append(VarName, "*", VarCountPiece0),
        string.int_to_string(Count, CountStr),
        string.append(VarCountPiece0, CountStr, VarCountPiece)
    else
        VarCountPiece = VarName
    ),
    (
        First = yes,
        string.append("{", VarCountPiece, Piece0)
    ;
        First = no,
        Piece0 = VarCountPiece
    ),
    (
        VarCounts = [],
        string.append(Piece0, "}.", Piece),
        Pieces = []
    ;
        VarCounts = [_ | _],
        Piece = Piece0,
        term_errors_var_bag_description_2(VarCounts, Varset, First, Pieces)
    ).

%-----------------------------------------------------------------------------%

% XXX Some of the following (and in is_fatal_error/1 as well) look wrong.
% Some of them should probably be calling unexpected/2 - juliensf.

term_error_kind_is_direct_error(ErrorKind) = IsDirect :-
    (
        ( ErrorKind = horder_call
        ; ErrorKind = method_call
        ; ErrorKind = pragma_foreign_code
        ; ErrorKind = imported_pred
        ; ErrorKind = can_loop_proc_called(_, _)
        ; ErrorKind = horder_args(_, _)
        ; ErrorKind = does_not_term_pragma(_)
        ),
        IsDirect = no
    ;
        ( ErrorKind = cycle(_, _)
        ; ErrorKind = does_not_term_foreign(_)
        ; ErrorKind = ho_inf_termination_const(_, _)
        ; ErrorKind = inf_call(_, _)
        ; ErrorKind = inf_termination_const(_, _)
        ; ErrorKind = is_builtin(_)
        ; ErrorKind = no_eqns
        ; ErrorKind = not_subset(_, _, _)
        ; ErrorKind = solver_failed
        ; ErrorKind = too_many_paths
        ; ErrorKind = inconsistent_annotations
        ),
        IsDirect = yes
    ).

term_error_kind_is_fatal_error(ErrorKind) = IsFatal :-
    (
        ( ErrorKind = horder_call
        ; ErrorKind = horder_args(_, _)
        ; ErrorKind = imported_pred
        ; ErrorKind = method_call
        ),
        IsFatal = yes
    ;
        ( ErrorKind = pragma_foreign_code
        ; ErrorKind = can_loop_proc_called(_, _)
        ; ErrorKind = does_not_term_pragma(_)
        ; ErrorKind = cycle(_, _)
        ; ErrorKind = does_not_term_foreign(_)
        ; ErrorKind = ho_inf_termination_const(_, _)
        ; ErrorKind = inf_call(_, _)
        ; ErrorKind = inf_termination_const(_, _)
        ; ErrorKind = is_builtin(_)
        ; ErrorKind = no_eqns
        ; ErrorKind = not_subset(_, _, _)
        ; ErrorKind = solver_failed
        ; ErrorKind = too_many_paths
        ; ErrorKind = inconsistent_annotations
        ),
        IsFatal = no
    ).

%----------------------------------------------------------------------------%
:- end_module transform_hlds.term_errors.
%----------------------------------------------------------------------------%
