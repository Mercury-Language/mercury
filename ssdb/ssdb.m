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
                module_name :: string,
                proc_name   :: string
            ).

:- type ssdb_event_type
    --->    ssdb_call
    ;       ssdb_exit
    ;       ssdb_redo
    ;       ssdb_fail
    .

    %
    % This routine is called at each event that occurs.
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
:- import_module set.
:- import_module stack.
:- import_module string.

%----------------------------------------------------------------------------%

:- type debugger_state
    --->    state(
                % Current event number.
                ssdb_event_number   :: int,                 

                % Call Sequence Number.
                ssdb_csn            :: int,                 

                % Depth of the function.
                ssdb_call_depth     :: int,                 

                % Where the program should stop next time.
                ssdb_next_stop      :: next_stop,           

                % The shadow stack.
                ssdb_stack          :: stack(stack_elem),

                % The set of breakpoint added.
                ssdb_breakpoints    :: set(breakpoint)
            ).


:- type stack_elem
    --->    elem(
                se_proc_id         :: ssdb_proc_id,
                
                % The debugger state at the call port.
                se_initial_state   :: debugger_state
            ).

    %
    % Type used by the prompt predicate to configure the next step in the
    % handle_event predicate.
    %
:- type what_next
    --->    wn_step
    ;       wn_next
    ;       wn_continue
    ;       wn_finish(int).


    %
    % Type used by the handle_event predicate to determine the next stop of
    % the prompt predicate.
    %
:- type next_stop
    --->    ns_step
    ;       ns_next(int)
    ;       ns_continue
    ;       ns_final_port(int).


:- type breakpoint
    --->    breakpoint(
                bp_module_name  :: string,
                bp_pred_name    :: string
	    ).


%----------------------------------------------------------------------------%

    %
    % Initialize the debugger state.
    % XXX Will be extended.
    %
:- func init_debugger_state = debugger_state.

init_debugger_state = DbgState :-
    EventNum = 0,
    CSN = 0,
    Depth = 0,
    NextStop = ns_step,
    Stack = stack.init,
    Breakpoints = set.init,
    DbgState = state(EventNum, CSN, Depth, NextStop, Stack, Breakpoints).

:- mutable(debugger_state, debugger_state, init_debugger_state, ground, 
    [untrailed, attach_to_io_state]).

%----------------------------------------------------------------------------%


    %
    % Write the event out and call the prompt.
    % XXX Not yet implemented : redo, fail.
    %
handle_event(ProcId, Event) :-
    impure get_event_num_inc(EventNum),
    impure update_depth(Event, PrintDepth),

    ( 
	Event = ssdb_call,
        impure get_csn_inc(_),

        semipure get_debugger_state(InitialState),
        StackFrame = elem(ProcId, InitialState),
        stack.push(InitialState ^ ssdb_stack, StackFrame, FinalStack),
        StateEv = InitialState ^ ssdb_stack := FinalStack,
        impure set_debugger_state(StateEv)
    ;
        Event = ssdb_exit,
        semipure get_debugger_state(InitialState),
        stack.pop_det(InitialState ^ ssdb_stack, StackFrame, FinalStack),
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

    CSN = StackFrame ^ se_initial_state ^ ssdb_csn,

    NextStop0 = State0 ^ ssdb_next_stop,
    (
        NextStop0 = ns_step,
        Stop = yes
    ;
        NextStop0 = ns_next(StopCSN),
        is_same_event(StopCSN, CSN, Stop)
    ;
        NextStop0 = ns_continue,
        ( set.contains(State0 ^ ssdb_breakpoints, 
            breakpoint(ProcId ^ module_name, ProcId ^ proc_name)) 
	->
            Stop = yes
        ;
            Stop = no
        )
    ;
        NextStop0 = ns_final_port(StopCSN),
        (
            Event = ssdb_exit,
            is_same_event(StopCSN, CSN, Stop)
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
            io.write_string(ProcId ^ module_name, !IO),
            io.write_string(".", !IO),
            io.write_string(ProcId ^ proc_name, !IO),
            io.write_string(".", !IO),
            io.write(Event, !IO),
            io.write_string("\t\t| DEPTH = ", !IO),
            io.write_int(PrintDepth, !IO),
            io.write_string("\t| CSN = ", !IO),
            io.write_int(CSN, !IO),
            io.nl(!IO),
        
            semipure get_shadow_stack(ShadowStack),
            impure prompt(ShadowStack, 0, WhatNext, !IO),

            impure consume_io(!.IO),
        
            (
                WhatNext = wn_step,
                NextStop = ns_step
            ;
                WhatNext = wn_next,
                NextStop = ns_next(CSN)
            ;
                WhatNext = wn_continue,
                NextStop = ns_continue
            ;
                WhatNext = wn_finish(EndCSN),
                NextStop = ns_final_port(EndCSN)
            ),

            % Set the last update :  breakpoints.
            semipure get_debugger_state(State1),
            State = State1 ^ ssdb_next_stop := NextStop,
            impure set_debugger_state(State)
        )
    ;
        Stop = no
    ).

    %
    % IsSame is 'yes' iff the two call sequence numbers are equal, 
    % 'no' otherwise.
    %
:- pred is_same_event(int::in, int::in, bool::out) is det.

is_same_event(CSNA, CSNB, IsSame) :-
    IsSame = (CSNA = CSNB -> yes ; no).
    
    %
    % Return the current event number.
    %
:- semipure pred get_event_num(int::out) is det.

get_event_num(EventNum) :-
    semipure get_debugger_state(State0),
    EventNum = State0 ^ ssdb_event_number.

    %
    % Increment the current event number in the debugger state, 
    % returning the new event number.
    %
:- impure pred get_event_num_inc(int::out) is det.

get_event_num_inc(EventNum) :-
    semipure get_debugger_state(State0),
    EventNum0 = State0 ^ ssdb_event_number,
    EventNum = EventNum0 + 1,
    State = State0 ^ ssdb_event_number := EventNum,
    impure set_debugger_state(State).

    %
    % For a given event type, update the depth in the debugger state,
    % returning the updated depth.
    %
:- impure pred update_depth(ssdb_event_type::in, int::out) is det.

update_depth(Event, ReturnDepth) :-
    semipure get_debugger_state(State0),
    Depth0 = State0 ^ ssdb_call_depth,
    (
        ( Event = ssdb_call
        ; Event = ssdb_redo
        ),
        Depth = Depth0 + 1,
        ReturnDepth = Depth0
    ;
        ( Event = ssdb_exit
        ; Event = ssdb_fail
        ),
        Depth = Depth0 - 1,
        ReturnDepth = Depth
    ),
    State = State0 ^ ssdb_call_depth := Depth,
    impure set_debugger_state(State).

    %
    % Increment the current call sequence number in the debugger state,
    % returning the new call seuqence number.
    %
:- impure pred get_csn_inc(int::out) is det.

get_csn_inc(CSN) :-
    semipure get_debugger_state(State0),
    CSN0 = State0 ^ ssdb_csn,
    CSN = CSN0 + 1,
    State = State0 ^ ssdb_csn := CSN,
    impure set_debugger_state(State).

    %
    % Return the current call sequence number.
    %
:- semipure pred get_csn(int::out) is det.

get_csn(CSN) :-
    semipure get_debugger_state(State0),
    CSN = State0 ^ ssdb_csn.
    
    %
    % Return the current shadow stack.
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
    % c     :: continue
    % b X Y :: breakpoint X = module_name Y = predicate_name
    %

:- impure pred prompt(stack(stack_elem)::in, int::in, what_next::out, 
                io::di, io::uo) is det.

prompt(ShadowStack, Depth, WhatNext, !IO) :-
    io.write_string("ssdb> ", !IO),
    % Read a string in input and return a string.
    io.read_line_as_string(Result, !IO), 
    (
        Result = ok(String0),
        % String minus any single trailing newline character.
        String = string.chomp(String0),
	Words = string.words(String), 

        ( Words = ["h"] ->
            io.nl(!IO),
            io.write_string("s      :: step", !IO),
            io.nl(!IO),
            io.write_string("n      :: next", !IO),
            io.nl(!IO),
            io.write_string("b X Y  :: insert breakpoint where :", !IO),
            io.write_string(" X = module name", !IO),
            io.write_string(" and Y = predicate name", !IO),
            io.nl(!IO),
            io.write_string("c      :: next", !IO),
            io.nl(!IO),
            io.write_string("f      :: finish", !IO),
            io.nl(!IO),
            io.nl(!IO),
            impure prompt(ShadowStack, Depth, WhatNext, !IO)
        
        ; Words = ["n"] ->
            WhatNext = wn_next

        ;
            ( Words = ["s"]
            ; list.is_empty(Words)
            )
        ->
                WhatNext = wn_step

        ; Words = ["c"] ->
            WhatNext = wn_continue

        ; 
            Words = ["b", ModuleName, ProcedureName] 
        ->
            semipure get_debugger_state(State0),
            Breakpoints0 = State0 ^ ssdb_breakpoints,
            Breakpoints = set.insert(Breakpoints0, breakpoint(ModuleName, 
                ProcedureName)),
            State = State0 ^ ssdb_breakpoints := Breakpoints,
            io.print(Breakpoints, !IO),nl(!IO),
            impure set_debugger_state(State),
            impure prompt(ShadowStack, Depth, WhatNext, !IO)

        ; Words = ["f"] ->
            stack.top_det(ShadowStack, FrameStack),
            CSN = FrameStack ^  se_initial_state ^ ssdb_csn,
            WhatNext = wn_finish(CSN)

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
