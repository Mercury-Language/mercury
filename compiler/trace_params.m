%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: trace_params.m.
%
% Author: zs.
%
% This module defines the parameters of execution tracing at various trace
% levels and with various settings of the --suppress-trace option.

%-----------------------------------------------------------------------------%

:- module libs__trace_params.

:- interface.

:- import_module ll_backend__llds. % XXX for trace_port
:- import_module bool.

:- type trace_level.
:- type trace_suppress_items.

	% The bool should be the setting of the `require_tracing' option.
:- pred convert_trace_level(string::in, bool::in, trace_level::out) is semidet.

:- pred convert_trace_suppress(string::in, trace_suppress_items::out)
	is semidet.

	% These functions check for various properties of the trace level.
:- func trace_level_is_none(trace_level) = bool.
:- func trace_level_needs_input_vars(trace_level) = bool.
:- func trace_level_needs_fixed_slots(trace_level) = bool.
:- func trace_level_needs_from_full_slot(trace_level) = bool.
:- func trace_level_allows_delay_death(trace_level) = bool.
:- func trace_needs_return_info(trace_level, trace_suppress_items) = bool.
:- func trace_needs_all_var_names(trace_level, trace_suppress_items) = bool.
:- func trace_needs_proc_body_reps(trace_level, trace_suppress_items) = bool.
:- func trace_needs_port(trace_level, trace_suppress_items, trace_port) = bool.

	% Should optimization passes maintain meaningful
	% variable names where possible.
:- func trace_level_needs_meaningful_var_names(trace_level) = bool.

:- func trace_level_none = trace_level.

	% This is used to represent the trace level in the module layout.
:- func trace_level_rep(trace_level) = string.

:- implementation.

:- import_module char, string, list, set.

:- type trace_level
	--->	none
	;	shallow
	;	deep
	;	decl
	;	decl_rep.

:- type trace_suppress_item
	--->	port(trace_port)
	;	return_info
	;	all_var_names
	;	proc_body_reps.

:- type trace_suppress_items == set(trace_suppress_item).

trace_level_none = none.

convert_trace_level("minimum", no, none).
convert_trace_level("minimum", yes, shallow).
convert_trace_level("shallow", _, shallow).
convert_trace_level("deep", _, deep).
convert_trace_level("decl", _, decl).
convert_trace_level("rep", _, decl_rep).
convert_trace_level("default", no, none).
convert_trace_level("default", yes, deep).

trace_level_is_none(none) = yes.
trace_level_is_none(shallow) = no.
trace_level_is_none(deep) = no.
trace_level_is_none(decl) = no.
trace_level_is_none(decl_rep) = no.

trace_level_needs_input_vars(none) = no.
trace_level_needs_input_vars(shallow) = yes.
trace_level_needs_input_vars(deep) = yes.
trace_level_needs_input_vars(decl) = yes.
trace_level_needs_input_vars(decl_rep) = yes.

trace_level_needs_fixed_slots(none) = no.
trace_level_needs_fixed_slots(shallow) = yes.
trace_level_needs_fixed_slots(deep) = yes.
trace_level_needs_fixed_slots(decl) = yes.
trace_level_needs_fixed_slots(decl_rep) = yes.

trace_level_needs_from_full_slot(none) = no.
trace_level_needs_from_full_slot(shallow) = yes.
trace_level_needs_from_full_slot(deep) = no.
trace_level_needs_from_full_slot(decl) = no.
trace_level_needs_from_full_slot(decl_rep) = no.

trace_level_allows_delay_death(none) = no.
trace_level_allows_delay_death(shallow) = no.
trace_level_allows_delay_death(deep) = yes.
trace_level_allows_delay_death(decl) = yes.
trace_level_allows_delay_death(decl_rep) = yes.

trace_level_needs_meaningful_var_names(none) = no.
trace_level_needs_meaningful_var_names(shallow) = no.
trace_level_needs_meaningful_var_names(deep) = yes.
trace_level_needs_meaningful_var_names(decl) = yes.
trace_level_needs_meaningful_var_names(decl_rep) = yes.

trace_needs_return_info(TraceLevel, TraceSuppressItems) = Need :-
	(
		trace_level_has_return_info(TraceLevel) = yes,
		\+ set__member(return_info, TraceSuppressItems)
	->
		Need = yes
	;
		Need = no
	).

trace_needs_all_var_names(TraceLevel, TraceSuppressItems) = Need :-
	(
		trace_level_has_all_var_names(TraceLevel) = yes,
		\+ set__member(all_var_names, TraceSuppressItems)
	->
		Need = yes
	;
		Need = no
	).

trace_needs_proc_body_reps(TraceLevel, TraceSuppressItems) = Need :-
	(
		trace_level_has_proc_body_reps(TraceLevel) = yes,
		\+ set__member(proc_body_reps, TraceSuppressItems)
	->
		Need = yes
	;
		Need = no
	).

:- func trace_level_has_return_info(trace_level) = bool.
:- func trace_level_has_all_var_names(trace_level) = bool.
:- func trace_level_has_proc_body_reps(trace_level) = bool.

trace_level_has_return_info(none) = no.
trace_level_has_return_info(shallow) = yes.
trace_level_has_return_info(deep) = yes.
trace_level_has_return_info(decl) = yes.
trace_level_has_return_info(decl_rep) = yes.

trace_level_has_all_var_names(none) = no.
trace_level_has_all_var_names(shallow) = no.
trace_level_has_all_var_names(deep) = no.
trace_level_has_all_var_names(decl) = yes.
trace_level_has_all_var_names(decl_rep) = yes.

trace_level_has_proc_body_reps(none) = no.
trace_level_has_proc_body_reps(shallow) = no.
trace_level_has_proc_body_reps(deep) = no.
trace_level_has_proc_body_reps(decl) = no.
trace_level_has_proc_body_reps(decl_rep) = yes.

convert_trace_suppress(SuppressString, SuppressItemSet) :-
	SuppressWords = string__words(char_is_comma, SuppressString),
	list__map(convert_item_name, SuppressWords, SuppressItemLists),
	list__condense(SuppressItemLists, SuppressItems),
	set__list_to_set(SuppressItems, SuppressItemSet).

:- pred char_is_comma(char::in) is semidet.

char_is_comma(',').

:- func convert_port_name(string) = trace_port is semidet.

	% The call port cannot be disabled, because its layout structure is
	% referred to implicitly by the redo command in mdb.
	%
	% The exception port cannot be disabled, because it is never put into
	% compiler-generated code in the first place; such events are created
	% on the fly by library/exception.m.
% convert_port_name("call") = call.
convert_port_name("exit") = exit.
convert_port_name("fail") = fail.
convert_port_name("redo") = redo.
% convert_port_name("excp") = exception.
convert_port_name("exception") = exception.
convert_port_name("cond") = ite_cond.
convert_port_name("ite_cond") = ite_cond.
convert_port_name("then") = ite_then.
convert_port_name("ite_then") = ite_then.
convert_port_name("else") = ite_else.
convert_port_name("ite_else") = ite_else.
convert_port_name("nege") = neg_enter.
convert_port_name("neg_enter") = neg_enter.
convert_port_name("negs") = neg_success.
convert_port_name("neg_success") = neg_success.
convert_port_name("negf") = neg_failure.
convert_port_name("neg_failure") = neg_failure.
convert_port_name("swtc") = switch.
convert_port_name("switch") = switch.
convert_port_name("disj") = disj.
convert_port_name("frst") = nondet_pragma_first.
convert_port_name("nondet_pragma_first") = nondet_pragma_first.
convert_port_name("latr") = nondet_pragma_first.
convert_port_name("nondet_pragma_later") = nondet_pragma_later.

:- func convert_port_class_name(string) = list(trace_port) is semidet.

convert_port_class_name("interface") =
	[call, exit, redo, fail, exception].
convert_port_class_name("internal") =
	[ite_then, ite_else, switch, disj].
convert_port_class_name("context") =
	[ite_cond, neg_enter, neg_success, neg_failure].

:- func convert_other_name(string) = trace_suppress_item is semidet.

convert_other_name("return") = return_info.
convert_other_name("return_info") = return_info.
convert_other_name("names") = all_var_names.
convert_other_name("all_var_names") = all_var_names.
convert_other_name("bodies") = proc_body_reps.
convert_other_name("proc_body_reps") = proc_body_reps.

:- pred convert_item_name(string::in, list(trace_suppress_item)::out)
	is semidet.

convert_item_name(String, Names) :-
	( convert_port_name(String) = PortName ->
		Names = [port(PortName)]
	; convert_port_class_name(String) = PortNames ->
		list__map(wrap_port, PortNames, Names)
	; convert_other_name(String) = OtherName ->
		Names = [OtherName]
	;
		fail
	).

:- pred wrap_port(trace_port::in, trace_suppress_item::out) is det.

wrap_port(Port, port(Port)).

	% If this is modified, then the corresponding code in
	% runtime/mercury_stack_layout.h needs to be updated.
trace_level_rep(none)	  = "MR_TRACE_LEVEL_NONE".
trace_level_rep(shallow)  = "MR_TRACE_LEVEL_SHALLOW".
trace_level_rep(deep)	  = "MR_TRACE_LEVEL_DEEP".
trace_level_rep(decl)	  = "MR_TRACE_LEVEL_DECL".
trace_level_rep(decl_rep) = "MR_TRACE_LEVEL_DECL_REP".

%-----------------------------------------------------------------------------%

:- type port_category
	--->	interface	% The events that describe the interface of a
				% procedure with its callers.
	;	internal	% The events inside each procedure that were
				% present in the initial procedural debugger.
	;	context.	% The events inside each procedure that we
				% added because the declarative debugger needs
				% to know when (potentially) negated contexts
				% start and end.

:- func trace_port_category(trace_port) = port_category.

trace_port_category(call)			= interface.
trace_port_category(exit)			= interface.
trace_port_category(fail)			= interface.
trace_port_category(redo)			= interface.
trace_port_category(exception)			= interface.
trace_port_category(ite_cond)			= context.
trace_port_category(ite_then)			= internal.
trace_port_category(ite_else)			= internal.
trace_port_category(neg_enter)			= context.
trace_port_category(neg_success)		= context.
trace_port_category(neg_failure)		= context.
trace_port_category(switch)			= internal.
trace_port_category(disj)			= internal.
trace_port_category(nondet_pragma_first)	= internal.
trace_port_category(nondet_pragma_later)	= internal.

:- func trace_level_port_categories(trace_level) = list(port_category).

trace_level_port_categories(none) = [].
trace_level_port_categories(shallow) = [interface].
trace_level_port_categories(deep) = [interface, internal].
trace_level_port_categories(decl) = [interface, internal, context].
trace_level_port_categories(decl_rep) = [interface, internal, context].

:- func trace_level_allows_port_suppression(trace_level) = bool.

trace_level_allows_port_suppression(none) = no.		% no ports exist
trace_level_allows_port_suppression(shallow) = yes.
trace_level_allows_port_suppression(deep) = yes.
trace_level_allows_port_suppression(decl) = no.
trace_level_allows_port_suppression(decl_rep) = no.

trace_needs_port(TraceLevel, TraceSuppressItems, Port) = NeedsPort :-
	(
		trace_port_category(Port) = Category,
		list__member(Category,
			trace_level_port_categories(TraceLevel)),
		\+ (
			trace_level_allows_port_suppression(TraceLevel) = yes,
			set__member(port(Port), TraceSuppressItems)
		)
	->
		NeedsPort = yes
	;
		NeedsPort = no
	).
