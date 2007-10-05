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
:- import_module string.

%----------------------------------------------------------------------------%

:- type debugger_state
    --->    state(
                event_number    :: int,   % Current event number
                csn             :: int,   % Call Sequence Number
                call_depth      :: int,   % Depth of the function
                stack           :: stack  % The shadow stack
            ).


:- type stack == list(stack_elem).

:- type stack_elem
    --->    elem(
                proc_id         :: ssdb_proc_id,
                initial_state   :: debugger_state
		    % The debugger state at the call port.
            ).



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
    Stack = [],
    DbgState = state(EventNum, CSN, Depth, Stack).

:- mutable(debugger_state, debugger_state, init_debugger_state, ground, 
                [untrailed, attach_to_io_state]).

%----------------------------------------------------------------------------%


    %
    % For the moment we just write the event out.
    % Later this will be extended.
    %
handle_event(ProcId, Event) :-
    impure get_event_num_inc(EventNum),
    impure update_depth(Event, PrintDepth),

    (
	Event = ssdb_call,
	impure get_csn_inc(_),

	semipure get_debugger_state(InitialState),
	S = elem(ProcId, InitialState),
	impure push(S)
    ;
	Event = ssdb_exit,
	impure pop(S)
    ;
	Event = ssdb_redo,
	error("ssdb_redo: not yet implemented")
    ;
	Event = ssdb_fail,
	error("ssdb_fail: not yet implemented")
    ),

    PrintCSN = S ^ initial_state ^ csn,
    
    some [!IO] 
    (
        impure invent_io(!:IO),
        io.write_string("       ", !IO),
        io.write_int(EventNum, !IO),
        io.write_string("   ", !IO),
        io.write_string(ProcId ^ proc_name, !IO),
        io.write_string(".", !IO),
        io.write(Event, !IO),
        io.write_string("   | DEPTH = ", !IO),
        io.write_int(PrintDepth, !IO),
        io.write_string(" | CSN = ", !IO),
        io.write_int(PrintCSN, !IO),
        io.nl(!IO),
        
        impure consume_io(!.IO),

            % XXX don not forget get the latest modification (like new breakpoint)
	true
    ).


    %
    % Return the event number after increment
    %
:- impure pred get_event_num_inc(int::out) is det.

get_event_num_inc(EventNum) :-
    semipure get_debugger_state(State0),
    EventNum0 = State0 ^ event_number,
    EventNum = EventNum0 + 1,
    State = State0 ^ event_number := EventNum,
    impure set_debugger_state(State).

    %
    % For the given event type, update the depth in the debugger state
    % and return the depth for the given event.
    %
:- impure pred update_depth(ssdb_event_type::in, int::out) is det.

update_depth(Event, PrintDepth) :-
    semipure get_debugger_state(State0),
    Depth0 = State0 ^ call_depth,
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
    State = State0 ^ call_depth := Depth,
    impure set_debugger_state(State).

    %
    % Return the csn after increment
    %
:- impure pred get_csn_inc(int::out) is det.

get_csn_inc(CSN) :-
    semipure get_debugger_state(State0),
    CSN0 = State0 ^ csn,
    CSN = CSN0 + 1,
    State = State0 ^ csn := CSN,
    impure set_debugger_state(State).

    %
    % push the element on to the debugger shadow stack
    %
:- impure pred push(stack_elem::in) is det.

push(E) :-
    semipure get_debugger_state(State0),
    Stack0 = State0 ^ stack,
    Stack = [E | Stack0],
    State = State0 ^ stack := Stack,
    impure set_debugger_state(State).

    %
    % pop the debugger shadow stack returning
    % the element on the top of the stack.
    %
:- impure pred pop(stack_elem::out) is det.

pop(E) :-
    semipure get_debugger_state(State0),
    Stack0 = State0 ^ stack,
    (
	Stack0 = [],
	error("ssdb.pop: empty stack")
    ;
	Stack0 = [E | Stack]
    ),
    State = State0 ^ stack := Stack,
    impure set_debugger_state(State).

    %
    % Return the current event number
    %
:- semipure pred get_event_num(int::out) is det.

get_event_num(EventNum) :-
    semipure get_debugger_state(State0),
    EventNum = State0 ^ event_number.

    %
    % Return the current csn
    %
:- semipure pred get_csn(int::out) is det.

get_csn(CSN) :-
    semipure get_debugger_state(State0),
    CSN = State0 ^ csn.

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
