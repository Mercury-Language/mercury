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

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module bag.
:- import_module bool.
:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

:- type termination_error
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

:- type termination_error_contexts == list(termination_error_context).
:- type termination_error_context
    --->    termination_error_context(termination_error, prog_context).

:- pred report_term_errors(list(pred_proc_id)::in,
    list(termination_error_context)::in, module_info::in, io::di, io::uo)
    is det.

    % An error is considered an indirect error if it is due either to a
    % language feature we cannot analyze or due to an error in another part
    % of the code. By default, we do not issue warnings about indirect errors,
    % since in the first case, the programmer cannot do anything about it,
    % and in the second case, the piece of code that the programmer *can* do
    % something about is not this piece.
    %
:- func is_indirect_error(termination_error) = bool.

    % A fatal error is one that prevents pass 2 from proving termination.
    %
:- func is_fatal_error(termination_error) = bool.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_error_util.
:- import_module parse_tree.error_util.
:- import_module transform_hlds.term_util.

:- import_module bag.
:- import_module bool.
:- import_module int.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

% XXX Some of the following (and in is_fatal_error/1 as well) look wrong.
% Some of them should probably be calling unexpected/2 - juliensf.

is_indirect_error(horder_call) = yes.
is_indirect_error(method_call) = yes.
is_indirect_error(pragma_foreign_code) = yes.
is_indirect_error(imported_pred) = yes.
is_indirect_error(can_loop_proc_called(_, _)) = yes.
is_indirect_error(horder_args(_, _)) = yes.
is_indirect_error(does_not_term_pragma(_)) = yes.
is_indirect_error(cycle(_, _)) = no.
is_indirect_error(does_not_term_foreign(_)) = no.
is_indirect_error(ho_inf_termination_const(_, _)) = no.
is_indirect_error(inf_call(_, _)) = no.
is_indirect_error(inf_termination_const(_, _)) = no.
is_indirect_error(is_builtin(_)) = no.
is_indirect_error(no_eqns) = no.
is_indirect_error(not_subset(_, _, _)) = no.
is_indirect_error(solver_failed) = no.
is_indirect_error(too_many_paths) = no.
is_indirect_error(inconsistent_annotations) = no.

is_fatal_error(horder_call) = yes.
is_fatal_error(horder_args(_, _)) = yes.
is_fatal_error(imported_pred) = yes.
is_fatal_error(method_call) = yes.
is_fatal_error(pragma_foreign_code) = no.
is_fatal_error(can_loop_proc_called(_, _)) = no.
is_fatal_error(does_not_term_pragma(_)) = no.
is_fatal_error(cycle(_, _)) = no.
is_fatal_error(does_not_term_foreign(_)) = no.
is_fatal_error(ho_inf_termination_const(_, _)) = no.
is_fatal_error(inf_call(_, _)) = no.
is_fatal_error(inf_termination_const(_, _)) = no.
is_fatal_error(is_builtin(_)) = no.
is_fatal_error(no_eqns) = no.
is_fatal_error(not_subset(_, _, _)) = no.
is_fatal_error(solver_failed) = no.
is_fatal_error(too_many_paths) = no.
is_fatal_error(inconsistent_annotations) = no.

%-----------------------------------------------------------------------------%

report_term_errors(SCC, Errors, Module, !IO) :-
    module_info_get_globals(Module, Globals),
    get_context_from_scc(SCC, Module, Context),
    ( SCC = [PPId] ->
        Pieces1 = [words("Termination of")] ++
            describe_one_proc_name(Module, should_module_qualify, PPId),
        Single = yes(PPId)
    ;
        Pieces1 = [words("Termination of the "),
            words("mutually recursive procedures")] ++
            describe_several_proc_names(Module, should_module_qualify, SCC),
        Single = no
    ),
    (
        Errors = [],
        % XXX This should never happen but for some reason, it often does.
        % error("empty list of errors")
        Pieces2 = [words("not proven, for unknown reason(s).")],
        list.append(Pieces1, Pieces2, Pieces),
        write_error_pieces(Globals, Context, 0, Pieces, !IO)
    ;
        Errors = [Error],
        Pieces2 = [words("not proven for the following reason:")],
        list.append(Pieces1, Pieces2, Pieces),
        write_error_pieces(Globals, Context, 0, Pieces, !IO),
        output_term_error(Error, Single, no, 0, Module, !IO)
    ;
        Errors = [_, _ | _],
        Pieces2 = [words("not proven for the following reasons:")],
        list.append(Pieces1, Pieces2, Pieces),
        write_error_pieces(Globals, Context, 0, Pieces, !IO),
        output_term_errors(Errors, Single, 1, 0, Module, !IO)
    ).

:- pred report_arg_size_errors(list(pred_proc_id)::in,
    list(termination_error_context)::in, module_info::in,
    io::di, io::uo) is det.

report_arg_size_errors(SCC, Errors, Module, !IO) :-
    module_info_get_globals(Module, Globals),
    get_context_from_scc(SCC, Module, Context),
    ( SCC = [PPId] ->
        Pieces1 = [words("Termination constant of")] ++
            describe_one_proc_name(Module, should_module_qualify, PPId),
        Single = yes(PPId)
    ;
        Pieces1 = [words("Termination constants"),
            words("of the mutually recursive procedures")] ++
            describe_several_proc_names(Module, should_module_qualify, SCC),
        Single = no
    ),
    Piece2 = words("set to infinity for the following"),
    (
        Errors = [],
        unexpected($module, $pred, "empty list of errors")
    ;
        Errors = [Error],
        Piece3 = words("reason:"),
        list.append(Pieces1, [Piece2, Piece3], Pieces),
        write_error_pieces(Globals, Context, 0, Pieces, !IO),
        output_term_error(Error, Single, no, 0, Module, !IO)
    ;
        Errors = [_, _ | _],
        Piece3 = words("reasons:"),
        list.append(Pieces1, [Piece2, Piece3], Pieces),
        write_error_pieces(Globals, Context, 0, Pieces, !IO),
        output_term_errors(Errors, Single, 1, 0, Module, !IO)
    ).

:- pred output_term_errors(list(termination_error_context)::in,
    maybe(pred_proc_id)::in, int::in, int::in, module_info::in,
    io::di, io::uo) is det.

output_term_errors([], _, _, _, _, !IO).
output_term_errors([Error | Errors], Single, ErrNum0, Indent, Module, !IO) :-
    output_term_error(Error, Single, yes(ErrNum0), Indent, Module, !IO),
    output_term_errors(Errors, Single, ErrNum0 + 1, Indent, Module, !IO).

:- pred output_term_error(termination_error_context::in,
    maybe(pred_proc_id)::in, maybe(int)::in, int::in, module_info::in,
    io::di, io::uo) is det.

output_term_error(TermErrorContext, Single, ErrorNum, Indent, Module, !IO) :-
    TermErrorContext = termination_error_context(Error, Context),
    description(Error, Single, Module, Pieces0, Reason),
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
    write_error_pieces(Globals, Context, Indent, Pieces, !IO),
    (
        Reason = yes(InfArgSizePPId),
        lookup_proc_arg_size_info(Module, InfArgSizePPId, ArgSize),
        ( ArgSize = yes(infinite(ArgSizeErrors)) ->
            % XXX the next line is cheating
            ArgSizePPIdSCC = [InfArgSizePPId],
            report_arg_size_errors(ArgSizePPIdSCC, ArgSizeErrors, Module, !IO)
        ;
            unexpected($module, $pred,
                "inf arg size procedure does not have inf arg size")
        )
    ;
        Reason = no
    ).

:- pred description(termination_error::in,
    maybe(pred_proc_id)::in, module_info::in, list(format_component)::out,
    maybe(pred_proc_id)::out) is det.

description(horder_call, _, _, Pieces, no) :-
    Pieces = [words("It contains a higher order call.")].

description(method_call, _, _, Pieces, no) :-
    Pieces = [words("It contains a typeclass method call.")].

description(pragma_foreign_code, _, _, Pieces, no) :-
    Pieces = [
        words("It depends on the properties of"),
        words("foreign language code included via a"),
        pragma_decl("foreign_proc"),
        words("declaration.")
    ].

description(TermError, Single, Module, Pieces, no) :-
    TermError = inf_call(CallerPPId, CalleePPId),
    (
        Single = yes(PPId),
        expect(unify(PPId, CallerPPId), $module, $pred,
            "inf_call: caller outside this SCC"),
        Pieces1 = [words("It")]
    ;
        Single = no,
        Pieces1 = describe_one_proc_name(Module, should_module_qualify,
            CallerPPId)
    ),
    Piece2 = words("calls"),
    CalleePieces = describe_one_proc_name(Module, should_module_qualify,
        CalleePPId),
    Pieces3 = [words("with an unbounded increase"),
        words("in the size of the input arguments.")],
    Pieces = Pieces1 ++ [Piece2] ++ CalleePieces ++ Pieces3.

description(TermError, Single, Module, Pieces, no) :-
    TermError = can_loop_proc_called(CallerPPId, CalleePPId),
    (
        Single = yes(PPId),
        expect(unify(PPId, CallerPPId), $module, $pred,
            "can_loop_proc_called: caller outside this SCC"),
        Pieces1 = [words("It")]
    ;
        Single = no,
        Pieces1 = describe_one_proc_name(Module, should_module_qualify,
            CallerPPId)
    ),
    Piece2 = words("calls"),
    CalleePieces = describe_one_proc_name(Module, should_module_qualify,
        CalleePPId),
    Piece3 = words("which could not be proven to terminate."),
    Pieces = Pieces1 ++ [Piece2] ++ CalleePieces ++ [Piece3].

description(imported_pred, _, _, Pieces, no) :-
    Pieces = [
        words("It contains one or more"),
        words("predicates and/or functions"),
        words("imported from another module.")
    ].

description(TermError, Single, Module, Pieces, no) :-
    TermError = horder_args(CallerPPId, CalleePPId),
    (
        Single = yes(PPId),
        expect(unify(PPId, CallerPPId), $module, $pred,
            "horder_args: caller outside this SCC"),
        Pieces1 = [words("It")]
    ;
        Single = no,
        Pieces1 = describe_one_proc_name(Module, should_module_qualify,
            CallerPPId)
    ),
    Piece2 = words("calls"),
    CalleePieces = describe_one_proc_name(Module, should_module_qualify,
        CalleePPId),
    Piece3 = words("with one or more higher order arguments."),
    Pieces = Pieces1 ++ [Piece2] ++ CalleePieces ++ [Piece3].

description(TermError, Single, Module, Pieces, yes(CalleePPId)) :-
    TermError = inf_termination_const(CallerPPId, CalleePPId),
    (
        Single = yes(PPId),
        expect(unify(PPId, CallerPPId), $module, $pred,
            "inf_termination_const: caller outside this SCC"),
        Pieces1 = [words("It")]
    ;
        Single = no,
        Pieces1 = describe_one_proc_name(Module, should_module_qualify,
            CallerPPId)
    ),
    Piece2 = words("calls"),
    CalleePieces = describe_one_proc_name(Module, should_module_qualify,
        CalleePPId),
    Piece3 = words("which has a termination constant of infinity."),
    Pieces = Pieces1 ++ [Piece2] ++ CalleePieces ++ [Piece3].

description(TermError, Single, Module, Pieces, no) :-
    %
    % XXX We should print out the names of the non-terminating closures.
    %
    TermError = ho_inf_termination_const(CallerPPId, _ClosurePPIds),
    (
        Single = yes(PPId),
        expect(unify(PPId, CallerPPId), $module, $pred,
            "ho_info_termination_const: caller outside this SCC"),
        Pieces1 = [words("It")]
    ;
        Single = no,
        Pieces1 = describe_one_proc_name(Module, should_module_qualify,
            CallerPPId)
    ),
    Piece2 = words("makes one or more higher-order calls."),
    Piece3 = words("Each of these higher-order calls has a"),
    Piece4 = words("termination constant of infinity."),
    Pieces = Pieces1 ++ [Piece2, Piece3, Piece4].

description(not_subset(ProcPPId, OutputSuppliers, HeadVars),
        Single, Module, Pieces, no) :-
    (
        Single = yes(PPId),
        ( PPId = ProcPPId ->
            Pieces1 = [words("The set of"),
                words("its output supplier variables")]
        ;
            % XXX this should never happen (but it does)
            % error("not_subset outside this SCC"),
            PPIdPieces = describe_one_proc_name(Module,
                should_module_qualify, ProcPPId),
            Pieces1 = [words("The set of"),
                words("output supplier variables of") | PPIdPieces]
        )
    ;
        Single = no,
        PPIdPieces = describe_one_proc_name(Module,
            should_module_qualify, ProcPPId),
        Pieces1 = [words("The set of output supplier variables of") |
            PPIdPieces]
    ),
    ProcPPId = proc(PredId, ProcId),
    module_info_pred_proc_info(Module, PredId, ProcId, _, ProcInfo),
    proc_info_get_varset(ProcInfo, Varset),
    term_errors_var_bag_description(OutputSuppliers, Varset,
        OutputSuppliersNames),
    list.map((pred(OS::in, FOS::out) is det :- FOS = fixed(OS)),
        OutputSuppliersNames, OutputSuppliersPieces),
    Pieces3 = [words("is not a subset of the head variables")],
    term_errors_var_bag_description(HeadVars, Varset, HeadVarsNames),
    list.map((pred(HV::in, FHV::out) is det :- FHV = fixed(HV)),
        HeadVarsNames, HeadVarsPieces),
    list.condense([Pieces1, OutputSuppliersPieces, Pieces3,
        HeadVarsPieces], Pieces).

description(cycle(_StartPPId, CallSites), _, Module, Pieces, no) :-
    ( CallSites = [DirectCall] ->
        SitePieces = describe_one_call_site(Module,
            should_module_qualify, DirectCall),
        Pieces = [words("At the recursive call to") | SitePieces] ++
            [
                words("the arguments are"),
                words("not guaranteed to decrease in size.")
            ]
    ;
        Pieces1 = [words("In the recursive cycle"),
            words("through the calls to")],
        SitePieces = describe_several_call_sites(Module,
            should_module_qualify, CallSites),
        Pieces2 = [words("the arguments are"),
            words("not guaranteed to decrease in size.")],
        list.condense([Pieces1, SitePieces, Pieces2], Pieces)
    ).

description(too_many_paths, _, _, Pieces, no) :-
    Pieces = [
        words("There are too many execution paths"),
        words("for the analysis to process.")
    ].

description(no_eqns, _, _, Pieces, no) :-
    Pieces = [
        words("The analysis was unable to form any constraints"),
        words("between the arguments of this group of procedures.")
    ].

description(solver_failed, _, _, Pieces, no)  :-
    Pieces = [
        words("The solver found the constraints produced"),
        words("by the analysis to be infeasible.")
    ].

description(is_builtin(_PredId), _Single, _, Pieces, no) :-
    % XXX expect(unify(Single, yes(_)), $module, $pred,
    %       "builtin not alone in SCC"),
    Pieces = [words("It is a builtin predicate.")].

description(does_not_term_pragma(PredId), Single, Module,
        Pieces, no) :-
    Pieces1 = [
        words("There is a"), pragma_decl("does_not_terminate"),
        words("declaration for")],
    (
        Single = yes(PPId),
        PPId = proc(SCCPredId, _),
        expect(unify(PredId, SCCPredId), $module, $pred,
            "does not terminate pragma outside this SCC"),
        Pieces2 = [words("it.")]
    ;
        Single = no,
        Pieces2 = describe_one_pred_name(Module, should_module_qualify,
            PredId) ++ [suffix(".")]
    ),
    list.append(Pieces1, Pieces2, Pieces).

description(inconsistent_annotations, _, _, Pieces, no) :-
    Pieces = [words("The termination pragmas are inconsistent.")].

description(does_not_term_foreign(_), _, _, Pieces, no) :-
    Pieces = [
        words("It contains foreign code that"),
        words("may make one or more calls back to Mercury.")
    ].

%----------------------------------------------------------------------------%

:- pred term_errors_var_bag_description(bag(prog_var)::in, prog_varset::in,
    list(string)::out) is det.

term_errors_var_bag_description(HeadVars, Varset, Pieces) :-
    bag.to_assoc_list(HeadVars, HeadVarCountList),
    term_errors_var_bag_description_2(HeadVarCountList, Varset, yes,
        Pieces).

:- pred term_errors_var_bag_description_2(assoc_list(prog_var, int)::in,
    prog_varset::in, bool::in, list(string)::out) is det.

term_errors_var_bag_description_2([], _, _, ["{}"]).
term_errors_var_bag_description_2([Var - Count | VarCounts], Varset, First,
        [Piece | Pieces]) :-
    varset.lookup_name(Varset, Var, VarName),
    ( Count > 1 ->
        string.append(VarName, "*", VarCountPiece0),
        string.int_to_string(Count, CountStr),
        string.append(VarCountPiece0, CountStr, VarCountPiece)
    ;
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
        VarCounts = [_|_],
        Piece = Piece0,
        term_errors_var_bag_description_2(VarCounts, Varset, First, Pieces)
    ).

%----------------------------------------------------------------------------%
:- end_module transform_hlds.term_errors.
%----------------------------------------------------------------------------%
