%-----------------------------------------------------------------------------%
% Copyright (C) 1997 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% This module handles the generation of traces for the trace analysis system.
%
% For the general basis of trace analysis systems, see the paper
% "Opium: An extendable trace analyser for Prolog" by Mireille Ducasse,
% available from http://www.irisa.fr/lande/ducasse.
%
% We reserve two slots in the stack frame of the traced procedure.
% One contains the call sequence number, which is set in the procedure prolog
% by incrementing a global counter. The other contains the call depth, which
% is also set by incrementing a global variable containing the depth of the
% caller. The caller sets this global variable from its own saved depth
% just before the call.

% Author: zs.

%-----------------------------------------------------------------------------%

:- module trace.

:- interface.

:- import_module llds, code_info.

:- type trace_port	--->	call
			;	exit
			;	fail.

:- type trace_info.

:- pred trace__setup(code_info::in, code_info::out) is det.

:- pred trace__generate_slot_fill_code(trace_info::in, code_tree::out) is det.

:- pred trace__generate_depth_reset_code(trace_info::in, code_tree::out) is det.

:- pred trace__generate_event_code(trace_port::in, trace_info::in,
	code_tree::out, code_info::in, code_info::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_module, hlds_pred, tree.
:- import_module int, list, std_util, string, require.

:- type trace_info
	--->	trace_info(
			lval,	% stack slot of call sequence number
			lval	% stack slot of call depth
		).

trace__setup -->
	code_info__get_trace_slot(CallNumSlot),
	code_info__get_trace_slot(CallDepthSlot),
	{ TraceInfo = trace_info(CallNumSlot, CallDepthSlot) },
	code_info__set_maybe_trace_info(yes(TraceInfo)).

trace__generate_slot_fill_code(TraceInfo, TraceCode) :-
	TraceInfo = trace_info(CallNumLval, CallDepthLval),
	trace__stackref_to_string(CallNumLval, CallNumStr),
	trace__stackref_to_string(CallDepthLval, CallDepthStr),
	string__append(CallNumStr, " = MR_trace_incr_seq();\n",
		CallNumStmt),
	string__append(CallDepthStr, " = MR_trace_incr_depth();\n",
		CallDepthStmt),
	TraceCode = node([
		c_code(CallNumStmt) - "",
		c_code(CallDepthStmt) - ""
	]).

trace__generate_depth_reset_code(TraceInfo, TraceCode) :-
	TraceInfo = trace_info(_CallNumLval, CallDepthLval),
	trace__stackref_to_string(CallDepthLval, CallDepthStr),
	string__append_list(["MR_trace_reset_depth(", CallDepthStr, ");\n"],
		Stmt),
	TraceCode = node([
		c_code(Stmt) - ""
	]).

trace__generate_event_code(Port, TraceInfo, TraceCode) -->
	code_info__get_pred_id(PredId),
	code_info__get_proc_id(ProcId),
	code_info__get_module_info(ModuleInfo),
	code_info__get_proc_model(CodeModel),
	{
	TraceInfo = trace_info(CallNumLval, CallDepthLval),
	trace__stackref_to_string(CallNumLval, CallNumStr),
	trace__stackref_to_string(CallDepthLval, CallDepthStr),
	predicate_module(ModuleInfo, PredId, ModuleName),
	predicate_name(ModuleInfo, PredId, PredName),
	predicate_arity(ModuleInfo, PredId, Arity),
	string__int_to_string(Arity, ArityStr),
	Quote = """",
	trace__port_to_string(Port, PortStr),
	trace__code_model_to_string(CodeModel, CodeModelStr),
	proc_id_to_int(ProcId, ProcInt),
	ModeNum is ProcInt mod 10000,
	string__int_to_string(ModeNum, ModeNumStr),
	string__append_list([
		"MR_trace(",
		PortStr, ", ",
		CodeModelStr, ", ",
		CallNumStr, ", ",
		CallDepthStr, ", ",
		Quote, ModuleName, Quote, ", ",
		Quote, PredName, Quote, ", ",
		ArityStr, ", ",
		ModeNumStr, ");\n"],
		TraceStmt),
	TraceCode = node([c_code(TraceStmt) - ""])
	}.

:- pred trace__port_to_string(trace_port::in, string::out) is det.

trace__port_to_string(call, "MR_PORT_CALL").
trace__port_to_string(exit, "MR_PORT_EXIT").
trace__port_to_string(fail, "MR_PORT_FAIL").

:- pred trace__code_model_to_string(code_model, string).
:- mode trace__code_model_to_string(in, out) is det.

trace__code_model_to_string(model_det,  "MR_MODEL_DET").
trace__code_model_to_string(model_semi, "MR_MODEL_SEMI").
trace__code_model_to_string(model_non,  "MR_MODEL_NON").

:- pred trace__stackref_to_string(lval, string).
:- mode trace__stackref_to_string(in, out) is det.

trace__stackref_to_string(Lval, LvalStr) :-
	( Lval = stackvar(Slot) ->
		string__int_to_string(Slot, SlotString),
		string__append_list(["detstackvar(", SlotString, ")"], LvalStr)
	; Lval = framevar(Slot) ->
		string__int_to_string(Slot, SlotString),
		string__append_list(["framevar(", SlotString, ")"], LvalStr)
	;
		error("non-stack lval in stackref_to_string")
	).
