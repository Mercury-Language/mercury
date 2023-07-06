%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2014 Mission Critical IT.
% Copyright (C) 2014 The Mercury team.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Author: Paul Bone
%
% These utilities make it easier to test concurrent code. In particular,
% a concurrent program's IO actions may occur in a nondeterminsic order.
% This module provides alternative IO operations that provide some order
% so that program output matches test output (when the test passes).
%
%---------------------------------------------------------------------------%

:- module thread_barrier_test_helper_1.

:- interface.

:- import_module io.
:- import_module string.

%---------------------------------------------------------------------------%

    % This type represents all the output of all the threads in the program.
    %
:- type all_threads_output.

    % This type represents the output of an individual thread.
    %
:- type thread_output.

:- pred init_all_thread_output(all_threads_output::out, io::di, io::uo)
    is det.

    % new_thread_output(N) = Output
    %
    % Create a new thread output object for thread number N.
    %
:- pred init_thread_output(all_threads_output::in, int::in,
    thread_output::out, io::di, io::uo) is det.

    % Save some output into the buffer.
    %
:- pred t_write_string(thread_output::in, string::in, io::di, io::uo) is det.

    % Close this threads output stream.  All streams must be closed as
    % write_all_thread_output/3 will use this to ensure it has received all
    % the messages it should have.
    %
:- pred close_thread_output(thread_output::in, io::di, io::uo) is det.

    % Write out this set of buffers.
    %
:- pred write_all_thread_output(all_threads_output::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module thread.
:- import_module thread.channel.
:- import_module set.

%---------------------------------------------------------------------------%

:- type all_threads_output
    --->    all_threads_output(
                channel(message)
            ).

:- type thread_output
    --->    thread_output(
                to_thread           :: int,
                to_chan             :: channel(message)
            ).

:- type message
    --->    message_output(
                om_thread       :: int,
                om_string       :: string
            )
    ;       message_open(
                mopen_thread    :: int
            )
    ;       message_close(
                mc_thread       :: int
            ).

%---------------------------------------------------------------------------%

init_all_thread_output(all_threads_output(Chan), !IO) :-
    channel.init(Chan, !IO).

init_thread_output(AllOutput, Thread, Output, !IO) :-
    AllOutput = all_threads_output(Chan),
    Output = thread_output(Thread, Chan),

    % Put a message that will be used later to show that this Output has
    % been opened.
    put(Chan, message_open(Thread), !IO).

t_write_string(Output, String, !IO) :-
    put(Output ^ to_chan, message_output(Output ^ to_thread, String), !IO).

write_all_thread_output(AllOutput, !IO) :-
    get_all_messages(AllOutput, set.init, map.init, Messages, !IO),
    foldl(write_out_thread_messages, Messages, !IO).

    % Messages indexed by thread.  Each list is stored in reverse order.
    %
:- type messages == map(int, list(string)).

:- pred get_all_messages(all_threads_output::in, set(int)::in,
    messages::in, messages::out, io::di, io::uo) is det.

get_all_messages(AllOutput, OpenThreads, !Messages, !IO) :-
    AllOutput = all_threads_output(Chan),
    ( if is_empty(OpenThreads) then
        % If this might be the end of the messages then we only try and
        % take, so we know if we should exit.
        try_take(Chan, MaybeMessage, !IO)
    else
        % OTOH, if there may be threads that have not finished sending our
        % messages, then we use a blocking take to ensure that we don't miss
        % their messages.
        take(Chan, Message0, !IO),
        MaybeMessage = yes(Message0)
    ),
    (
        MaybeMessage = yes(Message),
        (
            Message = message_output(Thread, String),
            ( if map.search(!.Messages, Thread, TMessages0) then
                TMessages = [String | TMessages0]
            else
                TMessages = [String]
            ),
            map.set(Thread, TMessages, !Messages),
            get_all_messages(AllOutput, OpenThreads, !Messages, !IO)
        ;
            Message = message_open(Thread),
            get_all_messages(AllOutput, insert(OpenThreads, Thread),
                !Messages, !IO)
        ;
            Message = message_close(Thread),
            get_all_messages(AllOutput, delete(OpenThreads, Thread),
                !Messages, !IO)
        )
    ;
        MaybeMessage = no
    ).

:- pred write_out_thread_messages(int::in, list(string)::in, io::di, io::uo)
    is det.

write_out_thread_messages(Thread, Messages, !IO) :-
    io.format("Messages from thread %d:\n", [i(Thread)], !IO),
    list.foldr(write_out_message, Messages, !IO).

close_thread_output(Output, !IO) :-
    put(Output ^ to_chan, message_close(Output ^ to_thread), !IO).

:- pred write_out_message(string::in, io::di, io::uo) is det.

write_out_message(String, !IO) :-
    io.format("\t%s\n", [s(String)], !IO).
