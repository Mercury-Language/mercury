%-----------------------------------------------------------------------------%
% Copyright (C) 1998 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: debugger_interface.m
% Authors: fjh, jahier
% Purpose:
%	This module provide support routines needed by
%	runtime/mercury_trace_external.c for interfacing to an external
%	(in a different process) debugger, in particular an Opium-style
%	debugger.
%
% This module corresponds to what is called the "Query Handler" in Opium.

:- module debugger_interface.
:- interface. 

% This module exports the following C functions:
%	ML_DI_output_current
%	ML_DI_found_match
%	ML_DI_read_request_from_socket
% These are used by runtime/mercury_trace.c.

:- pred dummy_pred_to_avoid_warning_about_nothing_exported is det.

:- implementation.
:- import_module io, require.
:- import_module list, bool, std_util.

dummy_pred_to_avoid_warning_about_nothing_exported.

% The stuff defined below is also defined in modules prog_data, hlds_data.

:- type arity == int.

:- type determinism == int. 
	% encoded as specified in ../runtime/mercury_stack_layout.h
	% and ../compiler/stack_layout.m.

% The stuff defined below is similar to types goal_path and trace_port
% defined in modules compiler/hlds_goal.m and compiler/trace.m.
% This enumeration must be EXACTLY the same as the MR_trace_port enum in
% runtime/mercury_trace.h, and in the same order, since the code assumes
% the representation is the same.

:- type trace_port_type
	--->	call
	;	exit
	;	fail
	;	ite_then
	;	ite_else
	;	disj
	;	switch
	;	nondet_pragma_first
	;	nondet_pragma_later
	.

:- type goal_path_string == string.



% This is known as "debugger_query" in the Opium documentation.
% The debugger_request type is used for request sent
% from the debugger process to the Mercury program being debugged.
% This type would need to be extended to handle spypoints, etc.
:- type debugger_request
	--->	hello_reply		% yes, I'm here

		% A `forward_move' request instructs the debuggee
		% to step forward until we reach a trace event
		% which matches the specified attributes.
	;	forward_move(
			match(event_number),
			match(call_number),
			match(depth_number),
			match(trace_port_type),
			match(string),		% module name
			match(string),		% pred name
			match(arity),
			match(int),		% mode number
			match(determinism),
			match(list(univ)), 	% the arguments
				% XXX we could provide better ways of
				% matching on arguments
			match(goal_path_string)
		)

		% A `current' request instructs the debuggee to
		% retrieve the specified attributes of the current
		% trace event and report them to the debugger
		% via the socket.
	;	current(
			wanted,	% event_number
			wanted,	% call_number
			wanted,	% depth_number
			wanted,	% port
			wanted,	% module name
			wanted,	% pred name
			wanted,	% arity
			wanted,	% proc_id (mode number)
			wanted,	% determinism
			wanted,	% arguments
				% XXX should provide a way of getting
				% just part of the arguments
				% (to reduce unnecessary socket traffic)
			wanted	% goal_path_string
		)
	;	abort_prog
			% just abort the program
	;	no_trace
			% stop tracing, and run the program to completion
	;	error(string)
			% something went wrong when trying to get the
			% next request
	.

:- type event_number == int.
:- type call_number == int.
:- type depth_number == int.


% `match' is called "get status" in the Opium documentation.
% This type defines a unary predicate which determines whether
% or not a particular value will be selected.
:- type match(T)
	--->	nop		% nop: value = -
	;	exact(T)	% exact(X): value = X
	;	neg(T)		% neg(X): value \= X
	;	list(list(T))	% list(L): list__member(value, L)
	;	interval(T,T)	% interval(Low, High): Low =< X, X =< High
	.


% The debugger_response type is used for response sent
% to the debugger process from the Mercury program being debugged.
% This type would need to be extended.
:- type debugger_response
	% sending hello
	--->	hello	% are you there?
	% start the synchronous communication with the debugger
	;	start
	% responses to forward_move
	;	forward_move_match_found
	;	forward_move_match_not_found
	% responses to current 
	;	current(
			maybe(event_number),
			maybe(call_number),
			maybe(depth_number),
			maybe(trace_port_type),
			maybe(string),	% module name
			maybe(string),	% pred name
			maybe(arity),
			maybe(int),	% mode number
			maybe(determinism),
			maybe(list(univ)),
			maybe(goal_path_string)
		)
	;	last_event
	% responses to abort_prog or no_trace
	;	ok
	% responses to anything
	;	error(string)
	.

% `wanted' is called "current status" in the Opium documentation
:- type wanted == bool.


%-----------------------------------------------------------------------------%

% output_currentML_DI_output_current":
%	send to the debugger (e.g. Opium) the wanted features.

:- pragma export(output_current(in, in, in, in, in, in, in, in, in, in,
		in, in, in, di, uo), "ML_DI_output_current").
			
:- pred output_current(event_number, call_number, depth_number, trace_port_type,
	/* module name */ string, /* pred name */ string, arity,
	/* mode num */ int, determinism, /* the arguments */ list(univ),
	goal_path_string, debugger_request,
	io__output_stream, io__state, io__state).
:- mode output_current(in, in, in, in, in, in, in, in, in, in, in, in, in,
	di, uo) is det.

output_current(EventNumber, CallNumber, DepthNumber, Port,
		ModuleName, PredName, Arity, ModeNum, Determinism, Args,
		Path, DebuggerRequest, OutputStream) -->
	(
		{ DebuggerRequest = current(WantEventNumber,
			WantCallNumber, WantDepthNumber, WantPort,
			WantModuleName, WantPredName, WantArity,
			WantModeNum, WantDeterminism, WantArgs, WantPath) }
	->
		{
		get_if_wanted(WantEventNumber, EventNumber,
				MaybeEventNumber),
		get_if_wanted(WantCallNumber, CallNumber, MaybeCallNumber),
		get_if_wanted(WantDepthNumber, DepthNumber,
				MaybeDepthNumber),
		get_if_wanted(WantPort, Port, MaybePort),
		get_if_wanted(WantModuleName, ModuleName, MaybeModuleName),
		get_if_wanted(WantPredName, PredName, MaybePredName),
		get_if_wanted(WantArity, Arity, MaybeArity),
		get_if_wanted(WantModeNum, ModeNum, MaybeModeNum),
		get_if_wanted(WantDeterminism, Determinism,
				MaybeDeterminism),
		get_if_wanted(WantArgs, Args, MaybeArgs),
		get_if_wanted(WantPath, Path, MaybePath),
		CurrentTraceInfo = current(MaybeEventNumber,
			MaybeCallNumber, MaybeDepthNumber, MaybePort,
			MaybeModuleName, MaybePredName, MaybeArity,
			MaybeModeNum, MaybeDeterminism, MaybeArgs,
			MaybePath)
		},
		/****
		io__print("debugger_interface: Send to Opium: "),
		io__write(CurrentTraceInfo),
		io__print(".\n"),	
		****/
		io__write(OutputStream, CurrentTraceInfo),
		io__print(OutputStream, ".\n"),
		io__flush_output(OutputStream)
	;
		{ error("output_current: current expected") }
	).

:- pred get_if_wanted(wanted, T, maybe(T)).
:- mode get_if_wanted(in, in, out) is det.
% get_if_wanted(Wanted, Value, MaybeValue).
get_if_wanted(yes, X, yes(X)).
get_if_wanted(no, _, no).

%-----------------------------------------------------------------------------%

:- pragma export(found_match(in, in, in, in, in, in, in, in, in,  in,
			in, in), "ML_DI_found_match").
			
:- pred found_match(event_number, call_number, depth_number, trace_port_type,
	/* module name */ string, /* pred name */ string, arity,
	/* mode num */ int, determinism, /* the arguments */ list(univ),
				% XXX we could provide better ways of
				% matching on arguments
	goal_path_string, debugger_request).
:- mode found_match(in, in, in, in, in, in, in, in, in,  in, in, in)
	is semidet.

found_match(EventNumber, CallNumber, DepthNumber, Port,
		ModuleName, PredName, Arity, ModeNum, Determinism, Args,
		Path, DebuggerRequest) :-
	(
		DebuggerRequest = forward_move(MatchEventNumber,
			MatchCallNumber, MatchDepthNumber, MatchPort,
			MatchModuleName, MatchPredName, MatchArity,
			MatchModeNum, MatchDeterminism, MatchArgs, MatchPath)
	->
		match(MatchEventNumber, EventNumber),
		match(MatchCallNumber, CallNumber),
		match(MatchDepthNumber, DepthNumber),
		match(MatchPort, Port),
		match(MatchModuleName, ModuleName),
		match(MatchPredName, PredName),
		match(MatchArity, Arity),
		match(MatchModeNum, ModeNum),
		match(MatchDeterminism, Determinism),
		match(MatchArgs, Args),
		match(MatchPath, Path)
	;
		error("found_match: forward_move expected")
	).


% match(MatchPattern, Value) is true iff Value matches the specified pattern.
:- pred match(match(T), T).
:- mode match(in, in) is semidet.

match(nop, _).
match(exact(X), X).
match(neg(X), Y) :- X \= Y.
match(list(L), X) :- list__member(X, L).
match(interval(Low, High), X) :-
	% X >= Low, X =< High
	compare(LE, X, High), 
	(LE = (<) ; LE = (=)),
	compare(GE, X, Low), 
	(GE = (>) ; GE = (=)).


%-----------------------------------------------------------------------------%

:- pred read_request_from_socket(io__input_stream, debugger_request, int,
		io__state, io__state).
			
:- mode read_request_from_socket(in, out, out, di, uo) is det.

:- pragma export(read_request_from_socket(in, out, out, di, uo),
		"ML_DI_read_request_from_socket").

read_request_from_socket(SocketStream, Request, RequestType) -->
	io__read(SocketStream, MaybeRequest),
	( { MaybeRequest = ok(Request0) },
		{ Request = Request0 }
	; { MaybeRequest = error(ErrorMsg, _LineNum) },
		{ Request = error(ErrorMsg) }
	; { MaybeRequest = eof },
		{ Request = error("end of file") }
	),
	{ classify_request(Request, RequestType) }.

	/***********
	% debugging stuff.
	io__stderr_stream(StdErr),
	io__print(StdErr, "debugger_interface: Receive the Request:+"),
	io__print(StdErr, Request),
	io__print(StdErr, "+ from opium\ndebugger_interface: RequestType = "),
	io__print(StdErr, RequestType),
	io__print(StdErr, ".\n").
	***********/



:- pred classify_request(debugger_request, int).
:- mode classify_request(in, out) is det.

% the numbers here should match the definition of
% MR_debugger_request_type in runtime/mercury_trace.c.

classify_request(hello_reply, 0).
classify_request(forward_move(_, _, _, _, _, _, _, _, _, _, _), 1).
classify_request(current(_, _, _, _, _, _, _, _, _, _, _), 2).
classify_request(abort_prog, 3).
classify_request(no_trace, 4).
classify_request(error(_), 5).

%-----------------------------------------------------------------------------%
