%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2007, 2011 The University of Melbourne.
% Copyright (C) 2014, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: erlang_builtin.m.
% Main author: wangp.
% Stability: low.
%
% This file is intended to hold things related to Erlang for the Erlang grade.
% In non-Erlang grades this file should do nothing.
%
% Currently it contains a server that is started at program initialisation
% to emulate global variables.  Lookups and updates of global mutables work by
% sending and receiving messages to this server.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module erlang_builtin.
:- interface.

% This module exports nothing yet for public consumption; all exports
% are via foreign_export.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module io.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred start_global_server(io::di, io::uo) is det.
:- pragma foreign_export("Erlang", start_global_server(di, uo),
    "ML_start_global_server").

:- pragma foreign_proc("Erlang",
    start_global_server(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Pid = spawn(fun global_server_loop/0),
    Pid ! {set_exit_status, 0},
    register('ML_erlang_global_server', Pid)
").

start_global_server(!IO).

:- pred stop_global_server(io::di, io::uo) is det.
:- pragma foreign_export("Erlang", stop_global_server(di, uo),
    "ML_stop_global_server").

:- pragma foreign_proc("Erlang",
    stop_global_server(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    'ML_erlang_global_server' ! {stop, self()},
    receive
        {stop_ack} ->
            void
    end
").

stop_global_server(!IO).

:- pragma foreign_code("Erlang", "

mutable_key(MutableName) ->
    {'ML_mutables', MutableName}.

env_var_key(EnvVarName) ->
    {'ML_env_vars', EnvVarName}.

global_server_loop() ->
    receive
        {get_mutable, MutableName, From} ->
            Value = get(mutable_key(MutableName)),
            From ! {get_mutable_ack, Value},
            global_server_loop();

        {set_mutable, MutableName, Value} ->
            put(mutable_key(MutableName), Value),
            global_server_loop();

        {init_env_var, EnvVarNameStr} ->
            % EnvVarNameStr is a string (list of integers), not a binary.
            case os:getenv(EnvVarNameStr) of
                false ->
                    Value = false;
                _ ->
                    Value = true
            end,
            put(env_var_key(list_to_binary(EnvVarNameStr)), Value),
            global_server_loop();

        {trace_evaluate_runtime_condition, Cond, From} ->
            Ret = trace_eval_runtime_cond(Cond),
            From ! {trace_evaluate_runtime_condition_ack, Ret},
            global_server_loop();

        {init_std_streams, Streams} ->
            put('ML_std_streams', Streams),
            global_server_loop();

        {get_std_streams, From} ->
            Streams = get('ML_std_streams'),
            From ! {get_std_streams_ack, Streams},
            global_server_loop();

        {get_exit_status, From} ->
            ExitStatus = get('ML_exit_status'),
            From ! {get_exit_status_ack, ExitStatus},
            global_server_loop();

        {set_exit_status, ExitStatus} ->
            put('ML_exit_status', ExitStatus),
            global_server_loop();

        {stop, From} ->
            From ! {stop_ack};

        Any ->
            io:format(
            ""** erlang_global_server ignoring unrecognised message: ~p~n"",
                [Any]),
            global_server_loop()
    end.

trace_eval_runtime_cond({env_var, EnvVarName}) ->
    get(env_var_key(EnvVarName));
trace_eval_runtime_cond({'not', Cond}) ->
    not trace_eval_runtime_cond(Cond);
trace_eval_runtime_cond({'or', CondA, CondB}) ->
    trace_eval_runtime_cond(CondA) orelse trace_eval_runtime_cond(CondB);
trace_eval_runtime_cond({'and', CondA, CondB}) ->
    trace_eval_runtime_cond(CondA) andalso trace_eval_runtime_cond(CondB).
").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
