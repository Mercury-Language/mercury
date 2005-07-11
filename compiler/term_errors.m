%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2000, 2003-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% term_errors.m
% Main author: crs.
%
% This module prints out the various error messages that are produced by
% the various modules of termination analysis.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds__term_errors.

:- interface.

:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.
:- import_module parse_tree__prog_data.

:- import_module assoc_list.
:- import_module bag.
:- import_module io.
:- import_module list.
:- import_module std_util.

:- type termination_error
	--->	pragma_foreign_code
			% The analysis result depends on the change constant
			% of a piece of pragma foreign code, (which cannot be
			% obtained without analyzing the foreign code, which is
			% something we cannot do).
			% Valid in both passes.

	;	imported_pred
			% The SCC contains some imported procedures,
			% whose code is not accessible.

	;	can_loop_proc_called(pred_proc_id, pred_proc_id)
			% can_loop_proc_called(Caller, Callee, Context)
			% The call from Caller to Callee at the associated
			% context is to a procedure (Callee) whose termination
			% info is set to can_loop.
			% Although this error does not prevent us from
			% producing argument size information, it would
			% prevent us from proving termination.
			% We look for this error in pass 1; if we find it,
			% we do not perform pass 2.

	;	horder_args(pred_proc_id, pred_proc_id)
			% horder_args(Caller, Callee, Context)
			% The call from Caller to Callee at the associated
			% context has some arguments of a higher order type.
			% Valid in both passes.

	;	horder_call
			% horder_call
			% There is a higher order call at the associated
			% context.  Valid in both passes.
	;
		method_call
			% method_call
			% There is a call to a typeclass method at the associated
			% context.  Valid in both passes.
	;
		aditi_call
			% There is a call to an Aditi builtin at the associated
			% context. Valid in both passes.

	;	inf_termination_const(pred_proc_id, pred_proc_id)
			% inf_termination_const(Caller, Callee)
			% The call from Caller to Callee at the associated
			% context is to a procedure (Callee) whose arg size
			% info is set to infinite.
			% Valid in both passes.

	;	ho_inf_termination_const(pred_proc_id, list(pred_proc_id))
			% ho_inf_termination_const(Caller, Callees).
			% Caller makes a call to either call/N or apply/N
			% at the associated context.  'Callees' gives the
			% possible values of the higher-order argument.

	;	not_subset(pred_proc_id, bag(prog_var), bag(prog_var))
			% not_subset(Proc, SupplierVariables, InHeadVariables)
			% This error occurs when the bag of active variables
			% is not a subset of the input head variables.
			% Valid error only in pass 1.

	;	inf_call(pred_proc_id, pred_proc_id)
			% inf_call(Caller, Callee)
			% The call from Caller to Callee at the associated
			% context has infinite weight.
			% Valid error only in pass 2.

	;	cycle(pred_proc_id, assoc_list(pred_proc_id, prog_context))
			% cycle(StartPPId, CallSites)
			% In the cycle of calls starting at StartPPId and
			% going through the named call sites may be an
			% infinite loop.
			% Valid error only in pass 2.

	;	no_eqns
			% There are no equations in this SCC.
			% This has 2 possible causes. (1) If the predicate has
			% no output arguments, no equations will be created
			% for them. The change constant of the predicate is
			% undefined, but it will also never be used.
			% (2) If the procedure is a builtin predicate, with
			% an empty body, traversal cannot create any equations.
			% Valid error only in pass 1.

	;	too_many_paths
			% There are too many distinct paths to be analyzed.
			% Valid in both passes (which analyze different sets
			% of paths).

	;	solver_failed
			% The solver could not find finite termination
			% constants for the procedures in the SCC.
			% Valid only in pass 1.

	;	is_builtin(pred_id)
			% The termination constant of the given builtin is
			% set to infinity; this happens when the type of at
			% least one output argument permits a norm greater
			% than zero.

	;	does_not_term_pragma(pred_id)
			% The given procedure has a does_not_terminate pragma.

	;	inconsistent_annotations
			% The pragma terminates/does_not_terminate declarations
			% for the procedures in this SCC are inconsistent.

	;	does_not_term_foreign(pred_proc_id).
			% The procedure contains foreign code that may
			% make calls back to Mercury.  By default such
			% code is assumed to be non-terminating.

:- type term_errors__error == pair(prog_context, termination_error).

:- pred term_errors__report_term_errors(list(pred_proc_id)::in,
	list(term_errors__error)::in, module_info::in, io::di, io::uo) is det.

% An error is considered an indirect error if it is due either to a
% language feature we cannot analyze or due to an error in another part
% of the code. By default, we do not issue warnings about indirect errors,
% since in the first case, the programmer cannot do anything about it,
% and in the second case, the piece of code that the programmer *can* do
% something about is not this piece.

:- pred indirect_error(term_errors__termination_error::in) is semidet.

:- implementation.

:- import_module hlds__hlds_error_util.
:- import_module hlds__hlds_out.
:- import_module hlds__passes_aux.
:- import_module libs__globals.
:- import_module libs__options.
:- import_module parse_tree__error_util.
:- import_module parse_tree__mercury_to_mercury.
:- import_module parse_tree__prog_out.
:- import_module transform_hlds__term_util.

:- import_module bag.
:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module varset.

indirect_error(horder_call).
indirect_error(method_call).
indirect_error(aditi_call).
indirect_error(pragma_foreign_code).
indirect_error(imported_pred).
indirect_error(can_loop_proc_called(_, _)).
indirect_error(horder_args(_, _)).
indirect_error(does_not_term_pragma(_)).

term_errors__report_term_errors(SCC, Errors, Module, !IO) :-
	get_context_from_scc(SCC, Module, Context),
	( SCC = [PPId] ->
		Pieces1 = [words("Termination of")] ++
			describe_one_proc_name(Module, should_module_qualify,
				PPId),
		Single = yes(PPId)
	;
		Pieces1 = [words("Termination of the "),
			words("mutually recursive procedures")] ++
			describe_several_proc_names(Module,
				should_module_qualify, SCC),
		Single = no
	),
	(
		Errors = [],
		% XXX this should never happen
		% XXX but for some reason, it often does
		% error("empty list of errors")
		Pieces2 = [words("not proven, for unknown reason(s).")],
		list__append(Pieces1, Pieces2, Pieces),
		write_error_pieces(Context, 0, Pieces, !IO)
	;
		Errors = [Error],
		Pieces2 = [words("not proven for the following reason:")],
		list__append(Pieces1, Pieces2, Pieces),
		write_error_pieces(Context, 0, Pieces, !IO),
		term_errors__output_error(Error, Single, no, 0, Module, !IO)
	;
		Errors = [_, _ | _],
		Pieces2 = [words("not proven for the following reasons:")],
		list__append(Pieces1, Pieces2, Pieces),
		write_error_pieces(Context, 0, Pieces, !IO),
		term_errors__output_errors(Errors, Single, 1, 0, Module, !IO)
	).

:- pred term_errors__report_arg_size_errors(list(pred_proc_id)::in,
	list(term_errors__error)::in, module_info::in,
	io::di, io::uo) is det.

term_errors__report_arg_size_errors(SCC, Errors, Module, !IO) :-
	get_context_from_scc(SCC, Module, Context),
	( SCC = [PPId] ->
		Pieces1 = [words("Termination constant of")] ++
			describe_one_proc_name(Module, should_module_qualify,
				PPId),
		Single = yes(PPId)
	;
		Pieces1 = [words("Termination constants"),
			words("of the mutually recursive procedures")] ++
			describe_several_proc_names(Module,
				should_module_qualify, SCC),
		Single = no
	),
	Piece2 = words("set to infinity for the following"),
	(
		Errors = [],
		error("empty list of errors")
	;
		Errors = [Error],
		Piece3 = words("reason:"),
		list__append(Pieces1, [Piece2, Piece3], Pieces),
		write_error_pieces(Context, 0, Pieces, !IO),
		term_errors__output_error(Error, Single, no, 0, Module, !IO)
	;
		Errors = [_, _ | _],
		Piece3 = words("reasons:"),
		list__append(Pieces1, [Piece2, Piece3], Pieces),
		write_error_pieces(Context, 0, Pieces, !IO),
		term_errors__output_errors(Errors, Single, 1, 0, Module, !IO)
	).

:- pred term_errors__output_errors(list(term_errors__error)::in,
	maybe(pred_proc_id)::in, int::in, int::in, module_info::in,
	io::di, io::uo) is det.

term_errors__output_errors([], _, _, _, _, !IO).
term_errors__output_errors([Error | Errors], Single, ErrNum0, Indent, Module,
		!IO) :-
	term_errors__output_error(Error, Single, yes(ErrNum0), Indent, Module,
		!IO),
	term_errors__output_errors(Errors, Single, ErrNum0 + 1, Indent, Module,
		!IO).

:- pred term_errors__output_error(term_errors__error::in,
	maybe(pred_proc_id)::in, maybe(int)::in, int::in, module_info::in,
	io::di, io::uo) is det.

term_errors__output_error(Context - Error, Single, ErrorNum, Indent, Module,
		!IO) :-
	term_errors__description(Error, Single, Module, Pieces0, Reason),
	( ErrorNum = yes(N) ->
		string__int_to_string(N, Nstr),
		string__append_list(["Reason ", Nstr, ":"], Preamble),
		Pieces = [fixed(Preamble) | Pieces0]
	;
		Pieces = Pieces0
	),
	write_error_pieces(Context, Indent, Pieces, !IO),
	( Reason = yes(InfArgSizePPId) ->
		lookup_proc_arg_size_info(Module, InfArgSizePPId, ArgSize),
		( ArgSize = yes(infinite(ArgSizeErrors)) ->
			% XXX the next line is cheating
			ArgSizePPIdSCC = [InfArgSizePPId],
			term_errors__report_arg_size_errors(ArgSizePPIdSCC,
				ArgSizeErrors, Module, !IO)
		;
			error("inf arg size procedure " ++
				"does not have inf arg size")
		)
	;
		true
	).

:- pred term_errors__description(termination_error::in,
	maybe(pred_proc_id)::in, module_info::in, list(format_component)::out,
	maybe(pred_proc_id)::out) is det.

term_errors__description(horder_call, _, _, Pieces, no) :-
	Pieces = [words("It contains a higher order call.")].

term_errors__description(method_call, _, _, Pieces, no) :-
	Pieces = [words("It contains a typeclass method call.")].

term_errors__description(aditi_call, _, _, Pieces, no) :-
	Pieces = [words("It contains an Aditi builtin call.")]. 

term_errors__description(pragma_foreign_code, _, _, Pieces, no) :-
	Pieces = [words("It depends on the properties of"),
		words("foreign language code included via a"),
		fixed("`:- pragma c_code'"),
		words("or"),
		fixed("`:- pragma foreign'"),
		words("declaration.")].

term_errors__description(inf_call(CallerPPId, CalleePPId),
		Single, Module, Pieces, no) :-
	(
		Single = yes(PPId),
		require(unify(PPId, CallerPPId), "caller outside this SCC"),
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

term_errors__description(can_loop_proc_called(CallerPPId, CalleePPId),
		Single, Module, Pieces, no) :-
	(
		Single = yes(PPId),
		require(unify(PPId, CallerPPId), "caller outside this SCC"),
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

term_errors__description(imported_pred, _, _, Pieces, no) :-
	Pieces = [words("It contains one or more"),
		words("predicates and/or functions"),
		words("imported from another module.")].

term_errors__description(horder_args(CallerPPId, CalleePPId), Single, Module,
		Pieces, no) :-
	(
		Single = yes(PPId),
		require(unify(PPId, CallerPPId), "caller outside this SCC"),
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

term_errors__description(inf_termination_const(CallerPPId, CalleePPId),
		Single, Module, Pieces, yes(CalleePPId)) :-
	(
		Single = yes(PPId),
		require(unify(PPId, CallerPPId), "caller outside this SCC"),
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

term_errors__description(ho_inf_termination_const(CallerPPId, _ClosurePPIds),
		Single, Module, Pieces, no) :-
	(
		Single = yes(PPId),
		require(unify(PPId, CallerPPId), "caller outside this SCC"),
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

term_errors__description(not_subset(ProcPPId, OutputSuppliers, HeadVars),
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
				words("output supplier variables of") |
				PPIdPieces]
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
	proc_info_varset(ProcInfo, Varset),
	term_errors_var_bag_description(OutputSuppliers, Varset,
		OutputSuppliersNames),
	list__map((pred(OS::in, FOS::out) is det :- FOS = fixed(OS)),
		OutputSuppliersNames, OutputSuppliersPieces),
	Pieces3 = [words("is not a subset of the head variables")],
	term_errors_var_bag_description(HeadVars, Varset, HeadVarsNames),
	list__map((pred(HV::in, FHV::out) is det :- FHV = fixed(HV)),
		HeadVarsNames, HeadVarsPieces),
	list__condense([Pieces1, OutputSuppliersPieces, Pieces3,
		HeadVarsPieces], Pieces).

term_errors__description(cycle(_StartPPId, CallSites), _, Module, Pieces, no) :-
	( CallSites = [DirectCall] ->
		SitePieces = describe_one_call_site(Module,
			should_module_qualify, DirectCall),
		Pieces = [words("At the recursive call to") | SitePieces] ++
			[words("the arguments are"),
			words("not guaranteed to decrease in size.")]
	;
		Pieces1 = [words("In the recursive cycle"),
			words("through the calls to")],
		SitePieces = describe_several_call_sites(Module,
			should_module_qualify, CallSites),
		Pieces2 = [words("the arguments are"),
			words("not guaranteed to decrease in size.")],
		list__condense([Pieces1, SitePieces, Pieces2], Pieces)
	).

term_errors__description(too_many_paths, _, _, Pieces, no) :-
	Pieces = [words("There are too many execution paths"),
		words("for the analysis to process.")].

term_errors__description(no_eqns, _, _, Pieces, no) :-
	Pieces = [words("The analysis was unable to form any constraints"),
		words("between the arguments of this group of procedures.")].

term_errors__description(solver_failed, _, _, Pieces, no)  :-
	Pieces = [words("The solver found the constraints produced"),
		words("by the analysis to be infeasible.")].

term_errors__description(is_builtin(_PredId), _Single, _, Pieces, no) :-
	% XXX require(unify(Single, yes(_)), "builtin not alone in SCC"),
	Pieces = [words("It is a builtin predicate.")].

term_errors__description(does_not_term_pragma(PredId), Single, Module,
		Pieces, no) :-
	Pieces1 = [words(
		"There is a `:- pragma does_not_terminate' declaration for")],
	(
		Single = yes(PPId),
		PPId = proc(SCCPredId, _),
		require(unify(PredId, SCCPredId),
			"does not terminate pragma outside this SCC"),
		Pieces2 = [words("it.")]
	;
		Single = no,
		Pieces2 = describe_one_pred_name(Module, should_module_qualify,
			PredId) ++ [suffix(".")]
	),
	list__append(Pieces1, Pieces2, Pieces).

term_errors__description(inconsistent_annotations, _, _, Pieces, no) :-
	Pieces = [words("The termination pragmas are inconsistent.")].

term_errors__description(does_not_term_foreign(_), _, _, Pieces, no) :-
	Piece1 = words("It contains foreign code that"),
	Piece2 = words("may make one or more calls back to Mercury."),
	Pieces = [Piece1, Piece2].

%----------------------------------------------------------------------------%

:- pred term_errors_var_bag_description(bag(prog_var)::in, prog_varset::in,
	list(string)::out) is det.

term_errors_var_bag_description(HeadVars, Varset, Pieces) :-
	bag__to_assoc_list(HeadVars, HeadVarCountList),
	term_errors_var_bag_description_2(HeadVarCountList, Varset, yes,
		Pieces).

:- pred term_errors_var_bag_description_2(assoc_list(prog_var, int)::in,
	prog_varset::in, bool::in, list(string)::out) is det.

term_errors_var_bag_description_2([], _, _, ["{}"]).
term_errors_var_bag_description_2([Var - Count | VarCounts], Varset, First,
		[Piece | Pieces]) :-
	varset__lookup_name(Varset, Var, VarName),
	( Count > 1 ->
		string__append(VarName, "*", VarCountPiece0),
		string__int_to_string(Count, CountStr),
		string__append(VarCountPiece0, CountStr, VarCountPiece)
	;
		VarCountPiece = VarName
	),
	( First = yes ->
		string__append("{", VarCountPiece, Piece0)
	;
		Piece0 = VarCountPiece
	),
	( VarCounts = [] ->
		string__append(Piece0, "}.", Piece),
		Pieces = []
	;
		Piece = Piece0,
		term_errors_var_bag_description_2(VarCounts, Varset, First,
			Pieces)
	).

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%
