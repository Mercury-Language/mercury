%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1998-2001, 2003, 2005-2006, 2011 The University of Melbourne.
% Copyright (C) 2015-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: debugger_interface.m.
% Authors: fjh, jahier.
%
% Purpose:
%   This module provide support routines needed by
%   runtime/mercury_trace_external.c for interfacing to an external
%   (in a different process) debugger, in particular an Opium-style debugger.
%
% This module corresponds to what is called the "Query Handler" in Opium.
%
%---------------------------------------------------------------------------%

:- module mdb.debugger_interface.
:- interface.

% This module exports the following C functions:
%   ML_DI_output_current_slots_user
%   ML_DI_output_current_slots_comp
%   ML_DI_output_current_vars
%   ML_DI_output_current_nth_var
%   ML_DI_output_current_live_var_names
%   ML_DI_found_match_user
%   ML_DI_found_match_comp
%   ML_DI_read_request_from_socket
% These are used by trace/mercury_trace_external.c.

:- pred dummy_pred_to_avoid_warning_about_nothing_exported is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdb.interactive_query.
:- import_module mdb.util.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.prim_data.

:- import_module io.
:- import_module list.
:- import_module require.
:- import_module univ.

dummy_pred_to_avoid_warning_about_nothing_exported.

% The stuff defined below is also defined in modules prog_data, hlds_data.

:- type arity == int.

:- type determinism == int.
    % encoded as specified in ../runtime/mercury_stack_layout.h
    % and ../compiler/stack_layout.m.

% Depending whether the Opium side is requesting for a user defined procedure
% or a compiler generated one, the event has not exactly the same structure.
% The differences between the two types of event are gathered in a forward_move
% slot of that type.

:- type pred_match
    --->    match_user_pred(
                % match user-defined preds only
                match(pred_or_func),
                match(string)       % declaration module name
            )

    ;       match_compiler_generated_pred(
                % match compiler-generated preds only
                match(string),      % type name
                match(string)       % type module name
            )

    ;       match_any_pred.
            % match either user-defined or compiler-generated preds

% This is known as "debugger_query" in the Opium documentation.
% The debugger_request type is used for request sent from the debugger process
% to the Mercury program being debugged. This type would need to be extended
% to handle spypoints, etc.

:- type debugger_request
    --->    hello_reply     % yes, I'm here

    ;       forward_move(
            % A `forward_move' request instructs the debuggee
            % to step forward until we reach a trace event
            % which matches the specified attributes.
                match(event_number),
                match(call_number),
                match(depth_number),
                match(trace_port),
                pred_match,
                match(string),      % definition module name
                match(string),      % pred name
                match(arity),
                match(int),         % mode number
                match(determinism),
                match(list(univ)),  % the arguments
                                    % XXX we could provide better ways of
                                    % matching on arguments
                match(goal_path_string)
            )

    ;       current_slots
            % A `current_slots' request instructs the debuggee to
            % retrieve the attributes of the current trace
            % event (except for argument slot) and report them
            % to the debugger via the socket.

    ;       current_vars
            % A `current_vars' request instructs the debuggee to
            % retrieve the list of values and the list of internal
            % names of live variables of the current event and
            % report it to the debugger via the socket.
            % XXX should provide a way of getting
            % just part of the arguments
            % (to reduce unnecessary socket traffic)

    ;       current_live_var_names
            % A `current_live_var_names' request instructs the debuggee to
            % retrieve the list of internal names of the currently
            % live variables and a list of their corresponding types.

    ;       current_nth_var(int)
            % A 'current_nth_var' request instructs the debuggee to
            % retrieve the specified live variable.

    ;       abort_prog
            % just abort the program

    ;       no_trace
            % stop tracing, and run the program to completion

    ;       retry
            % Restarts execution at the call port of the call
            % corresponding to the current event

    ;       stack
            % Print the ancestors stack.
    ;       nondet_stack
            % Prints the contents of the fixed slots of the
            % frames on the nondet stack.

    ;       stack_regs
            % Print the contents of the virtual machine registers
            % that point to the det and nondet stacks.

    ;       error(string)
            % Something went wrong when trying to get the next request.

    ;       query(imports)
            % To type interactive queries.

    ;       cc_query(imports)
            % To type cc interactive queries.

    ;       io_query(imports)
            % To type interactive queries that perform io.

    ;       mmc_options(options_string)
            % Options to compile queries with.

    ;       browse(string)
            % To call the term browser.

    ;       link_collect(string)
            % Dynamically link the collect module with the current execution.

    ;       collect
            % Execute the collect command.

    ;       current_grade
            % Retrieve the grade the current execution has been compiled with.

    ;       collect_arg_on
            % Switch the argument collecting on (for collect request).

    ;       collect_arg_off.
            % Switch the argument collecting off (for collect request).

:- type event_number == int.
:- type call_number == int.
:- type depth_number == int.

    % `match' is called "get status" in the Opium documentation.
    % This type defines a unary predicate which determines whether
    % or not a particular value will be selected.
:- type match(T)
    --->    nop             % nop: value = -
    ;       exact(T)        % exact(X): value = X
    ;       neg(T)          % neg(X): value \= X
    ;       list(list(T))   % list(L): list.member(value, L)
    ;       interval(T,T).  % interval(Low, High): Low =< X, X =< High

    % The debugger_response type is used for response sent
    % to the debugger process from the Mercury program being debugged.
:- type debugger_response
    --->    response_hello
            % sending hello
            % are you there?

    ;       response_start
            % start the synchronous communication with the debugger

    % responses to forward_move
    ;       response_forward_move_match_found
    ;       response_forward_move_match_not_found

    % responses to current
    ;       response_current_slots_user(
                % responses to current_slots for user event
                event_number,
                call_number,
                depth_number,
                trace_port,
                pred_or_func,
                string,     % declaration module name
                string,     % definition module name
                string,     % pred name
                arity,
                int,        % mode number
                determinism,
                goal_path_string,
                line_number
            )

    ;       response_current_slots_comp(
                % responses to current_slots for compiler generated event
                event_number,
                call_number,
                depth_number,
                trace_port,
                string,     % name type
                string,     % module type
                string,     % definition module
                string,     % pred name
                arity,
                int,        % mode number
                determinism,
                goal_path_string,
                line_number
            )

    % responses to current_vars
    ;       response_current_vars(list(univ), list(string))

    % responses to current_nth_var
    ;       response_current_nth_var(univ)

    % responses to current_live_var_names
    ;       response_current_live_var_names(list(string), list(string))

    % response sent when the last event is reached
    ;       response_last_event

    % responses to a successful browse request session
    ;       response_browser_end

    % responses to a successful mmc_option request
    ;       response_mmc_option_ok

    % responses to requests that proceeded successfully
    ;       response_ok

    % responses to requests that went wrong
    ;           response_error(string)

    % responses to stack
    % The protocol between the debugger and the debuggee is described is
    % trace/mercury_trace_external.c.
    ;       response_level(int)
            % stack level
    ;       response_proc(string, string, string, int, int)
            % compiler generated proc
    ;       response_proc(string, string, int, int)
            % user generated proc
    ;       response_def_module(string)
    ;       response_detail(int, int, int)
    ;       response_pred
    ;       response_func
    ;       response_det(string)
    ;       response_end_stack

    % responses to stack_regs
    ;       response_stack_regs(int, int, int)

    % responses to link_collect
    ;       response_link_collect_succeeded
    ;       response_link_collect_failed

    % responses to collect
    ;       response_collect_linked
    ;       response_collect_not_linked

    % responses to current_grade
    ;       response_grade(string)

    % responses to collect
    %;      response_collected(collected_type)
    % This is commented out because collected_type is unknown at compile time,
    % since it is defined by users in the dynamically linked collect module.

    % sent if the execution is not terminated after a collect request
    ;       response_execution_continuing

    % sent if the execution is terminated after a collect request
    ;       response_execution_terminated

    % responses to collect_arg_on
    ;       response_collect_arg_on_ok

    % responses to collect_arg_off
    ;       response_collect_arg_off_ok.

%---------------------------------------------------------------------------%
%   send to the debugger (e.g. Opium) the wanted features.

% output_current_slots_user "ML_DI_output_current_slots_user":
%   send to the debugger (e.g. Opium) the attributes of the current event
%   except the list of arguments.

:- pragma foreign_export("C",
    output_current_slots_user(in, in, in, in, in, in, in, in, in, in, in, in,
        in, in, di, uo),
    "ML_DI_output_current_slots_user").

:- pred output_current_slots_user(event_number::in, call_number::in,
    depth_number::in, trace_port::in, pred_or_func::in,
    /* declarated module name */ string::in,
    /* definition module name */ string::in, /* pred name */ string::in,
    arity::in, /* mode num */ int::in, determinism::in, goal_path_string::in,
    line_number::in, io.text_output_stream::in, io::di, io::uo) is det.

output_current_slots_user(EventNumber, CallNumber, DepthNumber, Port,
        PredOrFunc, DeclModuleName, DefModuleName, PredName, Arity, ModeNum,
        Determinism, Path, LineNo, OutputStream, !IO) :-
    Response = response_current_slots_user(EventNumber, CallNumber,
        DepthNumber, Port, PredOrFunc, DeclModuleName, DefModuleName,
        PredName, Arity, ModeNum, Determinism, Path, LineNo),
    io.write(OutputStream, Response, !IO),
    io.print(OutputStream, ".\n", !IO),
    io.flush_output(OutputStream, !IO).

% output_current_slots_comp "ML_DI_output_current_slots_comp":
%   send to the debugger (e.g. Opium) the attributes of the current event
%   except the list of arguments.

:- pragma foreign_export("C",
    output_current_slots_comp(in, in, in, in, in, in, in, in, in, in, in, in,
        in, in, di, uo),
    "ML_DI_output_current_slots_comp").

:- pred output_current_slots_comp(event_number::in, call_number::in,
    depth_number::in, trace_port::in, /* name type */ string::in,
    /* module type */ string::in, /* definition module */ string::in,
    /* pred name */ string::in, arity::in, /* mode num */ int::in,
    determinism::in, goal_path_string::in, line_number::in,
    io.text_output_stream::in, io::di, io::uo) is det.

output_current_slots_comp(EventNumber, CallNumber, DepthNumber, Port,
        NameType, ModuleType, DefModuleName, PredName, Arity,
        ModeNum, Determinism, Path, LineNo, OutputStream, !IO) :-
    Response = response_current_slots_comp(EventNumber, CallNumber,
        DepthNumber, Port, NameType, ModuleType, DefModuleName,
        PredName, Arity, ModeNum, Determinism, Path, LineNo),
    io.write(OutputStream, Response, !IO),
    io.print(OutputStream, ".\n", !IO),
    io.flush_output(OutputStream, !IO).

% output_current_vars "ML_DI_output_current_vars":
%   send to the debugger the list of the live variables of the current
%   event.

:- pragma foreign_export("C", output_current_vars(in, in, in, di, uo),
    "ML_DI_output_current_vars").

:- pred output_current_vars(list(univ)::in, list(string)::in,
    io.text_output_stream::in, io::di, io::uo) is det.

output_current_vars(VarList, StringList, OutputStream, !IO) :-
    Response = response_current_vars(VarList, StringList),
    io.write(OutputStream, Response, !IO),
    io.print(OutputStream, ".\n", !IO),
    io.flush_output(OutputStream, !IO).

% output_current_nth_var "ML_DI_output_current_nth_var":
%   send to the debugger the requested live variable of the current event.

:- pragma foreign_export("C", output_current_nth_var(in, in, di, uo),
    "ML_DI_output_current_nth_var").

:- pred output_current_nth_var(univ::in, io.text_output_stream::in,
    io::di, io::uo)
    is det.

output_current_nth_var(Var, OutputStream, !IO) :-
    Response = response_current_nth_var(Var),
    io.write(OutputStream, Response, !IO),
    io.print(OutputStream, ".\n", !IO),
    io.flush_output(OutputStream, !IO).

:- pragma foreign_export("C",
    output_current_live_var_names(in, in, in, di, uo),
    "ML_DI_output_current_live_var_names").

:- pred output_current_live_var_names(list(string)::in, list(string)::in,
    io.text_output_stream::in, io::di, io::uo) is det.

output_current_live_var_names(LiveVarNameList, LiveVarTypeList, OutputStream,
        !IO) :-
    Response = response_current_live_var_names(LiveVarNameList,
        LiveVarTypeList),
    io.write(OutputStream, Response, !IO),
    io.print(OutputStream, ".\n", !IO),
    io.flush_output(OutputStream, !IO).

%---------------------------------------------------------------------------%

:- pragma foreign_export("C", get_var_number(in) = out,
    "ML_DI_get_var_number").

    % This function is intended to retrieve the integer in
    % "current_nth_var(int)" requests.
    %
:- func get_var_number(debugger_request) = int.

get_var_number(DebuggerRequest) = VarNumber :-
    ( if DebuggerRequest = current_nth_var(Var) then
        Var = VarNumber
    else
        unexpected($pred, "not a current_nth_var request")
    ).

%---------------------------------------------------------------------------%

:- pragma foreign_export("C",
    found_match_user(in, in, in, in, in, in, in, in, in, in, in, in, in, in),
    "ML_DI_found_match_user").

:- pred found_match_user(event_number::in, call_number::in, depth_number::in,
    trace_port::in, pred_or_func::in, /* declarated module name */ string::in,
    /* defined module name */ string::in, /* pred name */ string::in,
    arity::in, /* mode num */ int::in, determinism::in,
    /* the arguments */ list(univ)::in, goal_path_string::in,
    debugger_request::in) is semidet.

found_match_user(EventNumber, CallNumber, DepthNumber, Port, PredOrFunc,
        DeclModuleName, DefModuleName, PredName, Arity, ModeNum,
        Determinism, Args, Path, DebuggerRequest) :-
    % XXX We could provide better ways of matching on arguments.
    ( if
        DebuggerRequest = forward_move(MatchEventNumber,
            MatchCallNumber, MatchDepthNumber, MatchPort,
            UserPredMatch, MatchDefModuleName, MatchPredName,
            MatchArity, MatchModeNum, MatchDeterminism,
            MatchArgs, MatchPath)
    then
        match(MatchEventNumber, EventNumber),
        match(MatchCallNumber, CallNumber),
        match(MatchDepthNumber, DepthNumber),
        match(MatchPort, Port),
        ( if
            UserPredMatch = match_user_pred(MatchPredOrFunc,
                MatchDeclModuleName)
        then
            match(MatchPredOrFunc, PredOrFunc),
            match(MatchDeclModuleName, DeclModuleName)
        else
            UserPredMatch = match_any_pred
        ),
        match(MatchDefModuleName, DefModuleName),
        match(MatchPredName, PredName),
        match(MatchArity, Arity),
        match(MatchModeNum, ModeNum),
        match(MatchDeterminism, Determinism),
        match(MatchArgs, Args),
        match(MatchPath, Path)
    else
        unexpected($pred, "forward_move expected")
    ).

    % match(MatchPattern, Value) is true iff Value matches the specified
    % pattern.
    %
:- pred match(match(T)::in, T::in) is semidet.

match(nop, _).
match(exact(X), X).
match(neg(X), Y) :- X \= Y.
match(list(L), X) :- list.member(X, L).
match(interval(Low, High), X) :-
    % X >= Low, X =< High
    compare(LE, X, High),
    ( LE = (<) ; LE = (=) ),
    compare(GE, X, Low),
    ( GE = (>) ; GE = (=) ).

:- pragma foreign_export("C",
    found_match_comp(in, in, in, in, in, in, in, in, in, in, in, in, in, in),
    "ML_DI_found_match_comp").

:- pred found_match_comp(event_number::in, call_number::in, depth_number::in,
    trace_port::in, /* name type */ string::in, /* module type */ string::in,
    /* definition module name */ string::in, /* pred name */ string::in,
    arity::in, /* mode num */ int::in, determinism::in,
    /* the arguments */ list(univ)::in, goal_path_string::in,
    debugger_request::in) is semidet.

found_match_comp(EventNumber, CallNumber, DepthNumber, Port, NameType,
        ModuleType, DefModuleName, PredName, Arity, ModeNum,
        Determinism, Args, Path, DebuggerRequest) :-
    % XXX We could provide better ways of matching on arguments.
    ( if
        DebuggerRequest = forward_move(MatchEventNumber,
            MatchCallNumber, MatchDepthNumber, MatchPort,
            CompilerGeneratedPredMatch,
            MatchDefModuleName, MatchPredName, MatchArity,
            MatchModeNum, MatchDeterminism, MatchArgs, MatchPath)
    then
        match(MatchEventNumber, EventNumber),
        match(MatchCallNumber, CallNumber),
        match(MatchDepthNumber, DepthNumber),
        match(MatchPort, Port),
        ( if
            CompilerGeneratedPredMatch =
                match_compiler_generated_pred(MatchNameType,
            MatchModuleType)
        then
            match(MatchNameType, NameType),
            match(MatchModuleType, ModuleType)
        else
            CompilerGeneratedPredMatch = match_any_pred
        ),
        match(MatchDefModuleName, DefModuleName),
        match(MatchPredName, PredName),
        match(MatchArity, Arity),
        match(MatchModeNum, ModeNum),
        match(MatchDeterminism, Determinism),
        match(MatchArgs, Args),
        match(MatchPath, Path)
    else
        unexpected($pred, "forward_move expected")
    ).

%---------------------------------------------------------------------------%

:- pred read_request_from_socket(io.text_input_stream::in,
    debugger_request::out, int::out, io::di, io::uo) is det.

:- pragma foreign_export("C",
    read_request_from_socket(in, out, out, di, uo),
    "ML_DI_read_request_from_socket").

read_request_from_socket(SocketStream, Request, RequestType, !IO) :-
    io.read(SocketStream, MaybeRequest, !IO),
    (
        MaybeRequest = ok(Request0),
        Request = Request0
    ;
        MaybeRequest = error(ErrorMsg, _LineNum),
        Request = error(ErrorMsg)
    ;
        MaybeRequest = eof,
        Request = error("end of file")
    ),
    classify_request(Request, RequestType).

    % debugging stuff.
    % io.stderr_stream(StdErr, !IO),
    % io.print(StdErr, "debugger_interface: Receive the Request:+", !IO),
    % io.print(StdErr, Request, !IO),
    % io.print(StdErr, "+ from opium\ndebugger_interface: RequestType = ",
    %   !IO),
    % io.print(StdErr, RequestType, !IO),
    % io.print(StdErr, ".\n", !IO).

%---------------------------------------------------------------------------%

:- pred get_list_modules_to_import(debugger_request::in, int::out,
    imports::out) is det.

:- pragma foreign_export("C", get_list_modules_to_import(in, out, out),
    "ML_DI_get_list_modules_to_import").

get_list_modules_to_import(DebuggerRequest, ListLength, ModulesList) :-
    ( if DebuggerRequest = query(List) then
        ModulesList = List
    else if DebuggerRequest = cc_query(List) then
        ModulesList = List
    else if DebuggerRequest = io_query(List) then
        ModulesList = List
    else
        unexpected($pred, "not a query request")
    ),
    length(ModulesList, ListLength).

%---------------------------------------------------------------------------%

:- pred get_mmc_options(debugger_request::in, options_string::out) is det.
:- pragma foreign_export("C", get_mmc_options(in, out),
    "ML_DI_get_mmc_options").

get_mmc_options(DebuggerRequest, Options) :-
    ( if DebuggerRequest = mmc_options(OptionsPrim) then
        Options = OptionsPrim
    else
        unexpected($pred, "not a mmc_options request")
    ).

%---------------------------------------------------------------------------%

    % This predicate allows mercury_trace_external.c to retrieve the name
    % of the object file to link the current execution with from a
    % `link_collect(ObjectFileName)' request.
    %
:- pred get_object_file_name(debugger_request::in, string::out) is det.
:- pragma foreign_export("C", get_object_file_name(in, out),
    "ML_DI_get_object_file_name").

get_object_file_name(DebuggerRequest, ObjectFileName) :-
    ( if DebuggerRequest = link_collect(ObjectFileNamePrime) then
        ObjectFileName = ObjectFileNamePrime
    else
        unexpected($pred, "not a link_collect request")
    ).

%---------------------------------------------------------------------------%

:- pred init_mercury_string(string::out) is det.
:- pragma foreign_export("C", init_mercury_string(out),
    "ML_DI_init_mercury_string").

init_mercury_string("").

%---------------------------------------------------------------------------%

    % This predicate allows mercury_trace_external.c to retrieve the name
    % of the variable to browse from a `browse(var_name)' request.
    %
:- pred get_variable_name(debugger_request::in, string::out) is det.
:- pragma foreign_export("C", get_variable_name(in, out),
    "ML_DI_get_variable_name").

get_variable_name(DebuggerRequest, Options) :-
    ( if DebuggerRequest = browse(OptionsPrime) then
        Options = OptionsPrime
    else
        unexpected($pred, "not a browse request")
    ).

%---------------------------------------------------------------------------%

:- pred classify_request(debugger_request::in, int::out) is det.

% The numbers here should match the definition of
% MR_debugger_request_type in runtime/mercury_trace_external.c.
classify_request(hello_reply, 0).
classify_request(forward_move(_, _, _, _, _, _, _, _, _, _, _, _), 1).
classify_request(current_vars, 2).
classify_request(current_slots, 3).
classify_request(no_trace, 4).
classify_request(abort_prog, 5).
classify_request(error(_), 6).
classify_request(current_live_var_names, 7).
classify_request(current_nth_var(_), 8).
classify_request(retry, 9).
classify_request(stack, 10).
classify_request(nondet_stack, 11).
classify_request(stack_regs, 12).
classify_request(query(_),13).
classify_request(cc_query(_),14).
classify_request(io_query(_),15).
classify_request(mmc_options(_),16).
classify_request(browse(_),17).
classify_request(link_collect(_),18).
classify_request(collect,19).
classify_request(current_grade,20).
classify_request(collect_arg_on,21).
classify_request(collect_arg_off,22).

%---------------------------------------------------------------------------%
:- end_module mdb.debugger_interface.
%---------------------------------------------------------------------------%
