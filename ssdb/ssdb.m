%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: ssdb.m.
% Author: oannet.
%
% This module is automatically imported into every module that is compiled
% using --source-to-source-debug.
%
% It provides the primitives which are needed by this source-to-source
% transformation to allow debugging.
%
%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- module ssdb.
:- interface.


:- type ssdb_proc_id
    --->    ssdb_proc_id(
                proc_name   :: string
            ).

:- type ssdb_event_type
    --->    ssdb_call
    ;       ssdb_exit
    ;       ssdb_redo
    ;       ssdb_fail
    .

    %
    % This routine is called at each event that occurs
    %
:- impure pred handle_event(ssdb_proc_id::in, ssdb_event_type::in) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module io.
:- import_module int.
:- import_module list.
:- import_module require.
:- import_module stack.
:- import_module string.

%----------------------------------------------------------------------------%

:- type debugger_state
    --->    state(
                % Current event number
                ssdb_event_number   :: int,                 

                % Call Sequence Number
                ssdb_csn            :: int,                 

                % Depth of the function
                ssdb_call_depth     :: int,                 

                % Where the program should stop next time
                ssdb_next_stop      :: next_stop,           

                % The shadow stack
                ssdb_stack          :: stack(stack_elem)    
            ).


:- type stack_elem
    --->    elem(
                proc_id         :: ssdb_proc_id,
                initial_state   :: debugger_state
                    % The debugger state at the call port.
            ).

    %
    % Type filled by the prompt function to configure the next step in the
    % handle_event function 
    %
:- type what_next
    --->    what_next_step
    ;       what_next_next
    ;       what_next_finish(int).


    %
    % Type filled by the handle_event function to determine the next stop of
    % the prompt function
    %
:- type next_stop
    --->    step
    ;       next(int)
    ;       final_port(int).


%----------------------------------------------------------------------------%

    %
    % Initialize the debugger state
    % XXX Will be extended
    %
:- func init_debugger_state = debugger_state.

init_debugger_state = DbgState :-
    EventNum = 0,
    CSN = 0,
    Depth = 0,
    NextStop = step,
    Stack = stack.init,
    DbgState = state(EventNum, CSN, Depth, NextStop, Stack).

:- mutable(debugger_state, debugger_state, init_debugger_state, ground, 
    [untrailed, attach_to_io_state]).

%----------------------------------------------------------------------------%


    %
    % Write the event out and call the prompt
    % XXX Not yet implemented : redo, fail
    %
handle_event(ProcId, Event) :-
    impure get_event_num_inc(EventNum),
    impure update_depth(Event, PrintDepth),

    (
        Event = ssdb_call,
        impure get_csn_inc(_),

        semipure get_debugger_state(InitialState),
        E = elem(ProcId, InitialState),
        stack.push(InitialState ^ ssdb_stack, E, FinalStack),
        StateEv = InitialState ^ ssdb_stack := FinalStack,
        impure set_debugger_state(StateEv)
    ;
        Event = ssdb_exit,
        semipure get_debugger_state(InitialState),
        stack.pop_det(InitialState ^ ssdb_stack, E, FinalStack),
        StateEv = InitialState ^ ssdb_stack := FinalStack,
        impure set_debugger_state(StateEv)
    ;
        Event = ssdb_redo,
        error("ssdb_redo: not yet implemented")
    ;
        Event = ssdb_fail,
        error("ssdb_fail: not yet implemented")
    ),
 
    semipure get_debugger_state(State0),

    PrintCSN = E ^ initial_state ^ ssdb_csn,

    NextStop0 = State0 ^ ssdb_next_stop,
    (
        NextStop0 = step,
        Stop = yes
    ;
        NextStop0 = next(StopCSN),
        is_same_event(StopCSN, PrintCSN, Stop)
    ;
        NextStop0 = final_port(StopCSN),
        (
            Event = ssdb_exit,
            is_same_event(StopCSN, PrintCSN, Stop)
        ;
            Event = ssdb_call,
            Stop = no
        )
    ),
    
    (
        Stop = yes,
        some [!IO] 
        (
            impure invent_io(!:IO),
            io.write_string("       ", !IO),
            io.write_int(EventNum, !IO),
            io.write_string("\t", !IO),
            io.write_string(ProcId ^ proc_name, !IO),
            io.write_string(".", !IO),
            io.write(Event, !IO),
            io.write_string("\t\t| DEPTH = ", !IO),
            io.write_int(PrintDepth, !IO),
            io.write_string("\t| CSN = ", !IO),
            io.write_int(PrintCSN, !IO),
            io.nl(!IO),
        
            semipure get_shadow_stack(ShadowStack),
            impure prompt(ShadowStack, 0, WhatNext, !IO),

            impure consume_io(!.IO),
        
            (
                WhatNext = what_next_step,
                NextStop = step
            ;
                WhatNext = what_next_next,
                NextStop = next(PrintCSN)
            ;
                WhatNext = what_next_finish(EndCSN),
                NextStop = final_port(EndCSN)
            ),

% XXX do not forget get the latest modification (like new breakpoint)
            semipure get_debugger_state(State1),
            State = State1 ^ ssdb_next_stop := NextStop,
            impure set_debugger_state(State)
        )
    ;
        Stop = no
    ).


    %
    % Determine if two CSN are equals and if yes, do a stop
    %
:- pred is_same_event(int::in, int::in, bool::out) is det.

is_same_event(CSNA, CSNB, IsSame) :-
    IsSame = (CSNA = CSNB -> yes ; no).

    %
    % Increment and return the event number
    % Update the state with the new event number
    %
:- impure pred get_event_num_inc(int::out) is det.

get_event_num_inc(EventNum) :-
    semipure get_debugger_state(State0),
    EventNum0 = State0 ^ ssdb_event_number,
    EventNum = EventNum0 + 1,
    State = State0 ^ ssdb_event_number := EventNum,
    impure set_debugger_state(State).

    %
    % For the given event type, update the depth in the debugger state
    % and return the depth for the given event.
    %
:- impure pred update_depth(ssdb_event_type::in, int::out) is det.

update_depth(Event, PrintDepth) :-
    semipure get_debugger_state(State0),
    Depth0 = State0 ^ ssdb_call_depth,
    (
        ( Event = ssdb_call
        ; Event = ssdb_redo
        ),
        Depth = Depth0 + 1,
        PrintDepth = Depth0
    ;
        ( Event = ssdb_exit
        ; Event = ssdb_fail
        ),
        Depth = Depth0 - 1,
        PrintDepth = Depth
    ),
    State = State0 ^ ssdb_call_depth := Depth,
    impure set_debugger_state(State).

    %
    % Increment and return the call sequence number
    % Update the state with this new call sequence number
    %
:- impure pred get_csn_inc(int::out) is det.

get_csn_inc(CSN) :-
    semipure get_debugger_state(State0),
    CSN0 = State0 ^ ssdb_csn,
    CSN = CSN0 + 1,
    State = State0 ^ ssdb_csn := CSN,
    impure set_debugger_state(State).

    %
    % Return the current event number
    %
:- semipure pred get_event_num(int::out) is det.

get_event_num(EventNum) :-
    semipure get_debugger_state(State0),
    EventNum = State0 ^ ssdb_event_number.

    %
    % Return the current call sequence number
    %
:- semipure pred get_csn(int::out) is det.

get_csn(CSN) :-
    semipure get_debugger_state(State0),
    CSN = State0 ^ ssdb_csn.
    
    %
    % Return the current shadow stack
    %
:- semipure pred get_shadow_stack(stack(stack_elem)::out) is det.

get_shadow_stack(ShadowStack) :-
    semipure get_debugger_state(State0),
    ShadowStack = State0 ^ ssdb_stack.

%----------------------------------------------------------------------------%

    %
    % Display the prompt to debug
    %
    % h     :: help
    % f     :: finish (go to the next exit or fail of the current call)
    % n     :: next
    % s | _ :: next step
    %

:- impure pred prompt(stack(stack_elem)::in, int::in, what_next::out, 
                io::di, io::uo) is det.

prompt(ShadowStack, Depth, WhatNext, !IO) :-
    io.write_string("ssdb> ", !IO),
        %read a string in input and return a string
    io.read_line_as_string(Result, !IO), 
    (
        Result = ok(String0),
            %string minus any single trailing newline character
        String = string.chomp(String0), 

        ( 
            String = "h" ->
            io.nl(!IO),
            io.write_string("s   :: step", !IO),
            io.nl(!IO),
            io.write_string("n   :: next", !IO),
            io.nl(!IO),
            io.write_string("f   :: finish", !IO),
            io.nl(!IO),
            io.nl(!IO),
            impure prompt(ShadowStack, Depth, WhatNext, !IO)
        
        ; 
            String = "n" ->
            WhatNext = what_next_next
        ;
            (
                String = "s"
            ;
                String = ""
            )
            ->
                WhatNext = what_next_step
        ;
            String = "f" ->
            stack.top_det(ShadowStack, FrameStack),
            CSN = FrameStack ^  initial_state ^ ssdb_csn,
            WhatNext = what_next_finish(CSN)
        ;
            io.write_string("huh?\n", !IO),
            impure prompt(ShadowStack, Depth, WhatNext, !IO)
        )
    ;
        Result = eof,
        error("eof from read_line_as_string")
    ;
        Result = error(_),
        error("error from read_line_as_string")
    ).


%----------------------------------------------------------------------------%

:- impure pred invent_io(io::uo) is det.

:- pragma foreign_proc("C",
    invent_io(_IO::uo),
    [will_not_call_mercury, thread_safe], "").

:- pragma foreign_proc("Erlang",
    invent_io(_IO::uo),
    [will_not_call_mercury, thread_safe], "void").

:- pragma foreign_proc("C#",
    invent_io(_IO::uo),
    [will_not_call_mercury, thread_safe], "").

:- pragma foreign_proc("Java",
    invent_io(_IO::uo),
    [will_not_call_mercury, thread_safe], "").

:- impure pred consume_io(io::di) is det.

:- pragma foreign_proc("C",
    consume_io(_IO::di),
    [will_not_call_mercury, thread_safe], "").

:- pragma foreign_proc("Erlang",
    consume_io(_IO::di),
    [will_not_call_mercury, thread_safe], "void").

:- pragma foreign_proc("C#",
    consume_io(_IO::di),
    [will_not_call_mercury, thread_safe], "").

:- pragma foreign_proc("Java",
    consume_io(_IO::di),
    [will_not_call_mercury, thread_safe], "").

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%
