%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
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
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module erlang_builtin.
:- interface.

% This module exports nothing yet for public consumption; all exports
% are via foreign_export.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module io.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred start_global_server(io::di, io::uo) is det.
:- pragma foreign_export("Erlang", start_global_server(di, uo),
    "ML_start_global_server").

:- pragma foreign_proc("Erlang",
    start_global_server(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Pid = spawn(fun global_server_loop/0),
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

global_server_loop() ->
    receive
        {get_mutable, MutableName, From} ->
            Value = get(mutable_key(MutableName)),
            From ! {get_mutable_ack, Value},
            global_server_loop();

        {set_mutable, MutableName, Value} ->
            put(mutable_key(MutableName), Value),
            global_server_loop();

        {stop, From} ->
            From ! {stop_ack};

        Any ->
            io:format(
            ""** erlang_global_server ignoring unrecognised message: ~p~n"",
                [Any]),
            global_server_loop()
    end.
").

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "erlang_builtin.m".

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
