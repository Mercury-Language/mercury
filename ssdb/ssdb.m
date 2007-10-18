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
:- import_module list.

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
    % The list of all variables in use in a procedure.
    %
:- type list_var_value == list(var_value).

    %
    % Record the instantiatedness and value of each variable used in a
    % procedure.
    %
:- type var_value
    --->    unbound_head_var(var_name, pos)
    ;       some [T] bound_head_var(var_name, pos, T)
    ;       some [T] bound_other_var(var_name, T).

    %
    % Variable name.
    %
:- type var_name == string.
    
    %
    % The argument position of the head variable.
    % Positions are numbered from 0.
    %
:- type pos == int.

    %
    % This routine is called at each event that occurs.
    %
:- impure pred handle_event(ssdb_proc_id::in, ssdb_event_type::in, 
    list_var_value::in) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module io.
:- import_module int.
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
                ssdb_breakpoints    :: set(breakpoint),

                % The list of the goal's argument.
                ssdb_list_var_value :: list(var_value)
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
    ListVarValue = [],
    DbgState = state(EventNum, CSN, Depth, NextStop, Stack, Breakpoints, 
        ListVarValue).

:- mutable(debugger_state, debugger_state, init_debugger_state, ground, 
    [untrailed, attach_to_io_state]).

%----------------------------------------------------------------------------%


    %
    % Write the event out and call the prompt.
    % XXX Not yet implemented : redo, fail.
    %
handle_event(ProcId, Event, ListVarValue) :-
    impure get_event_num_inc(EventNum),
    impure update_depth(Event, PrintDepth),

    ( 
        Event = ssdb_call,
        % set the new CSN.
        impure get_csn_inc(_),
        % set the list_var_value of the debugger state  with the list received
        impure set_list_var_value(ListVarValue),

        % Push the new stack frame on top of the shadow stack.
        semipure get_debugger_state(InitialState),
        StackFrame = elem(ProcId, InitialState),
        stack.push(InitialState ^ ssdb_stack, StackFrame, FinalStack),
        StateEv = InitialState ^ ssdb_stack := FinalStack,
        impure set_debugger_state(StateEv)
    ;
        % Just get the top stack frame. It will be popped at the end of
        % handle_event. We need to leave the frame in place, e.g. for
        % printing variables.
        Event = ssdb_exit,
        impure set_list_var_value_in_stack(ListVarValue),
        semipure get_debugger_state(InitialState),
        stack.top_det(InitialState ^ ssdb_stack, StackFrame)
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
    ),
    
    ( Event = ssdb_call

    ; Event = ssdb_exit,
        semipure get_debugger_state(PopState),
        stack.pop_det(PopState ^ ssdb_stack, _StackFrame1, FinalStack1),
        StateEv1 = PopState ^ ssdb_stack := FinalStack1,
        impure set_debugger_state(StateEv1)

    /* XXX currently commented out because above these two cases
    ** throw an exception above and hence the compiler warns about
    ** these two cases being redundant
    ; Event = ssdb_redo

    ; Event = ssdb_fail
    */

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

:- impure pred set_list_var_value(list(var_value)::in) is det.

set_list_var_value(ListVarValue) :-
    semipure get_debugger_state(State0),
    State = State0 ^ ssdb_list_var_value := ListVarValue,
    impure set_debugger_state(State).

:- impure pred set_list_var_value_in_stack(list(var_value)::in) is det.

set_list_var_value_in_stack(ListVarValue) :-
    semipure get_debugger_state(State0),
    stack.pop_det(State0 ^ ssdb_stack, StackFrame, PopedStack),
    ProcId = StackFrame ^ se_proc_id,
    InitialState = StackFrame ^ se_initial_state,
    NewState = InitialState ^ ssdb_list_var_value := ListVarValue,
    Elem = elem(ProcId, NewState),
    stack.push(PopedStack, Elem, FinalStack),
    State = State0 ^ ssdb_stack := FinalStack,
    impure set_debugger_state(State).


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
    % p     :: print
    % dump  :: print stack trace
    % u     :: up
    % d     :: down
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
            io.write_string("c      :: continue until next breakpoint", !IO),
            io.nl(!IO),
            io.write_string("f      :: finish", !IO),
            io.nl(!IO),
            io.write_string("p      :: print goal's argument", !IO),
            io.nl(!IO),
            io.write_string("dump   :: print stack trace", !IO),
            io.nl(!IO),
            io.write_string("u      :: up", !IO),
            io.nl(!IO),
            io.write_string("d      :: down", !IO),
            io.nl(!IO),
            io.nl(!IO),
            impure prompt(ShadowStack, Depth, WhatNext, !IO)

        ; Words = ["p"] ->
            CurrentFrame = stack.top_det(ShadowStack),
            ListVarValue = CurrentFrame ^ se_initial_state  ^ 
                ssdb_list_var_value,
            print_vars(ListVarValue, !IO),
            impure prompt(ShadowStack, Depth, WhatNext, !IO)

        ; Words = ["dump"] ->
            print_frames_list(ShadowStack, Depth, !IO),
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

        ; Words = ["d"] ->
            (
                DownDepth = Depth - 1,
                DownDepth >= 0
            ->
                impure prompt(ShadowStack, DownDepth, WhatNext, !IO)
            ;
                io.print("Impossible to go down\n", !IO),
                impure prompt(ShadowStack, Depth, WhatNext, !IO)
            )
            
        ; Words = ["u"] ->
            (
                UpDepth = Depth + 1,
                UpDepth < stack.depth(ShadowStack) 
            ->
                impure prompt(ShadowStack, UpDepth, WhatNext, !IO)
            ;
                io.print("Impossible to go up\n", !IO),
                impure prompt(ShadowStack, Depth, WhatNext, !IO)
            )

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

    %
    % Print the Stack Trace
    %
:- pred print_frames_list(stack(stack_elem)::in, int::in, 
    io::di, io::uo) is det.

print_frames_list(ShadowStack0, Depth, !IO) :-
    ( if not stack.is_empty(ShadowStack0) then
        stack.pop_det(ShadowStack0, PopFrame, ShadowStack),
        (if Depth = 0 then
            print_stack_frame(yes, PopFrame, !IO)
        else
            print_stack_frame(no, PopFrame, !IO)
        ),
        print_frames_list(ShadowStack, Depth - 1, !IO)
    else
        true
    ).


:- pred print_stack_frame(bool::in, stack_elem::in, io::di, io::uo) is det.

print_stack_frame(Starred, Frame, !IO) :-
    Module = Frame ^ se_proc_id ^ module_name ,
    Procedure = Frame ^ se_proc_id ^ proc_name ,

    (
        Starred = yes,
        io.write_char('*', !IO)
    ;
        Starred = no,
        io.write_char(' ', !IO)
    ),
    io.format("  %s.%s(\n", [s(Module), s(Procedure)], !IO),
    ListVarValue = Frame ^ se_initial_state  ^ ssdb_list_var_value,
    print_vars(ListVarValue, !IO),
    io.write_string(")\n", !IO).

    %
    % Print the given list of variables and their values, if bound.
    % XXX We should treat the io.state better.
:- pred print_vars(list(var_value)::in, io::di, io::uo) is det.

print_vars(Vars, !IO) :-
    list.foldl(print_var, Vars, !IO).

:- pred print_var(var_value::in, io::di, io::uo) is det.

print_var(unbound_head_var(Name, Pos), !IO) :-
    io.write_char('\t', !IO),
    io.write_string("unbound_head\t", !IO),
    io.write_string(Name, !IO),
    io.write_string(":\t", !IO),
    io.write_int(Pos, !IO),
    io.write_string("\t=\t", !IO),
    io.write_string("_", !IO),
    io.nl(!IO).

print_var(bound_head_var(Name, Pos, T), !IO) :-
    ( if not string.prefix(Name, "STATE_VARIABLE_IO") then
        io.write_char('\t', !IO),
        io.write_string("bound_head\t", !IO),
        io.write_string(Name, !IO),
        io.write_string(":\t", !IO),
        io.write_int(Pos, !IO),
        io.write_string("\t=\t", !IO),
        io.print(T, !IO),
        io.nl(!IO)
    else
        io.write_char('\t', !IO),
        io.write_string("bound_head\t", !IO),
        io.write_string(Name, !IO),
        io.nl(!IO)
    ).
    
print_var(bound_other_var(Name, T), !IO) :-
    io.write_char('\t', !IO),
    io.write_string("bound_other\t", !IO),
    io.write_string(Name, !IO),
    io.write_string(":\t_\t", !IO),
    io.write_string("=\t", !IO),
    io.print(T, !IO),
    io.nl(!IO).


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
