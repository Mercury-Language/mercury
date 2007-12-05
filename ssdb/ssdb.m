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
    ;       ssdb_fail
    ;       ssdb_call_nondet
    ;       ssdb_exit_nondet
    ;       ssdb_redo_nondet
    ;       ssdb_fail_nondet
    .

    %
    % Type to determine if it is necessary to do a retry.
    %
:- type ssdb_retry
    --->    do_retry
    ;       do_not_retry
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
    % This routine is called at each call event that occurs.
    %
:- impure pred handle_event_call(ssdb_proc_id::in, list_var_value::in) is det.

    %
    % This routine is called at each call event in a nondet procedure.
    %
:- impure pred handle_event_call_nondet(ssdb_proc_id::in, 
    list_var_value::in) is det.

    %
    % This routine is called at each exit event that occurs.
    %
:- impure pred handle_event_exit(ssdb_proc_id::in, list_var_value::in, 
    ssdb_retry::out) is det.

    %
    % This routine is called at each exit event in a nondet procedure.
    %
:- impure pred handle_event_exit_nondet(ssdb_proc_id::in, 
    list_var_value::in) is det.

    %
    % This routine is called at each fail event that occurs.
    %
:- impure pred handle_event_fail(ssdb_proc_id::in, list_var_value::in, 
    ssdb_retry::out) is det.

    %
    % This routine is called at each fail event in a nondet procedure.
    %
:- impure pred handle_event_fail_nondet(ssdb_proc_id::in, list_var_value::in, 
    ssdb_retry::out) is det.

    %
    % This routine is called at each redo event in a nondet procedure.
    %
:- impure pred handle_event_redo_nondet(ssdb_proc_id::in, 
    list_var_value::in) is det.


%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module bool.
:- import_module io.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module pretty_printer.
:- import_module require.
:- import_module set.
:- import_module stack.
:- import_module string.
:- import_module univ.

:- import_module mdb.
:- import_module mdb.browse.
:- import_module mdb.browser_info.
:- import_module mdb.browser_term.

%----------------------------------------------------------------------------%

    %
    % These variables are all mutable, they are used to record the diffrents 
    % state of the debugger.
    %

:- type cur_ssdb_next_stop == next_stop.

:- type cur_ssdb_breakpoints == map(pair(string,string), breakpoint).

:- type cur_ssdb_shadow_stack == stack(stack_elem).

:- type cur_ssdb_shadow_stack_nondet == stack(stack_elem).

    %
    % Note: debugger_disabled must be first because io.init_state/2 is called
    % before the `do_nothing' mutable is initialised.  At that time `do_nothing'
    % will have a value of zero.  By putting debugger_disabled first, it will
    % be represented by zero so the SSDB port code will correctly do nothing
    % until after the library is initialised.
    % XXX In near future, the debugger_disabled state should be removed.
    %
:- type debugger_state
    --->    debugger_disabled
    ;       debugger_on
    ;       debugger_off.

    %
    % Frame of the current call procedure.
    %
:- type stack_elem
    --->    elem(
                % Event Number
                se_event_number     :: int,
                
                % Call Sequence Number.
                se_csn              :: int,
                
                % Depth of the procedure.
                se_depth            :: int,
                
                % The goal's module name and procedure name.
                se_proc_id          :: ssdb_proc_id,
                
                % The list of the procedure's arguments.
                se_list_var_value   :: list(var_value)
            ).


%----------------------------------------------------------------------------%

    %
    % Type used by the prompt predicate to configure the next step in the
    % handle_event predicate.
    %
:- type what_next
    --->    wn_step
    ;       wn_next
    ;       wn_continue
    ;       wn_finish(int)
    ;       wn_retry(int)
    ;       wn_retry_nondet(int)
    ;       wn_goto(int).

:- inst what_next_no_retry
    --->    wn_step
    ;       wn_next
    ;       wn_continue
    ;       wn_finish(ground)
    ;       wn_goto(ground).


    %
    % Type used by the handle_event predicate to determine the next stop of
    % the prompt predicate.
    %
:- type next_stop
    --->    ns_step
            % Stop at next step.

    ;       ns_next(int)
            % Stop at next event of the number between brackets.

    ;       ns_continue
            % Continue until next breakpoint.

    ;       ns_final_port(int, ssdb_retry)
            % Stop at final port (exit or fail) of the number between brakets,
            % the ssdb_retry is used to retry the right csn number.

    ;       ns_final_port_nondet(int, ssdb_retry)
            % Same as ns_final_port but for nondet procedure.

    ;       ns_goto(int).
            % Stop at the Event Number given in argument.


    %
    % A breakpoint is represented by his module and procedure name.
    %
:- type breakpoint
    --->    breakpoint(
                bp_number       :: int,
                bp_module_name  :: string,
                bp_pred_name    :: string,
                bp_state        :: bp_state
            ).

:- type bp_state
    --->    bp_state_enabled
    ;       bp_state_disabled.


%----------------------------------------------------------------------------%


    %
    % Initialization of the mutable variables.
    %

:- mutable(cur_ssdb_event_number, int, 0, ground, 
    [untrailed, attach_to_io_state]).

:- mutable(cur_ssdb_csn, int, 0, ground, [untrailed, attach_to_io_state]).

:- mutable(cur_ssdb_depth, int, 0, ground, [untrailed, attach_to_io_state]).

:- mutable(cur_ssdb_next_stop, cur_ssdb_next_stop, ns_step, ground, 
    [untrailed, attach_to_io_state]).

:- mutable(cur_ssdb_breakpoints, cur_ssdb_breakpoints, map.init, ground, 
    [untrailed, attach_to_io_state]).

:- mutable(cur_ssdb_number_of_breakpoint, int, 0, 
    ground, [untrailed, attach_to_io_state]).

:- mutable(cur_ssdb_shadow_stack, cur_ssdb_shadow_stack, stack.init, ground, 
    [untrailed, attach_to_io_state]).

:- mutable(cur_ssdb_shadow_stack_nondet, cur_ssdb_shadow_stack_nondet, 
    stack.init, ground, [untrailed, attach_to_io_state]).

:- mutable(debugger_state, debugger_state, init_debugger_state, ground, 
    [untrailed, attach_to_io_state]).

:- func init_debugger_state = debugger_state is det.

init_debugger_state = DebuggerState :-
    promise_pure (
        some [!IO] (
            impure invent_io(!:IO),
            io.get_environment_var("SSDB", MaybeEnv, !IO),
            impure consume_io(!.IO)
        )
    ),
    (
        MaybeEnv = yes(_),
        DebuggerState = debugger_on
    ;
        MaybeEnv = no,
        DebuggerState = debugger_disabled
    ).


%----------------------------------------------------------------------------%

    %
    % Call at call port. It writes the event out and call the prompt.
    %
handle_event_call(ProcId, ListVarValue) :-
    semipure get_debugger_state(DebuggerState),
    ( 
        DebuggerState = debugger_on,
        
        Event = ssdb_call,
        impure get_ssdb_event_number_inc(EventNum),
        impure get_ssdb_csn_inc(CSN),
        impure get_ssdb_depth_inc(PrintDepth),

        % Push the new stack frame on top of the shadow stack.
        semipure get_cur_ssdb_shadow_stack(ShadowStack0),
        StackFrame = elem(EventNum, CSN, PrintDepth, ProcId, ListVarValue),
        stack.push(ShadowStack0, StackFrame, ShadowStack),
        impure set_cur_ssdb_shadow_stack(ShadowStack),

        semipure should_stop_at_this_event(Event, EventNum, CSN, ProcId, Stop, 
            _AutoRetry),
        (
            Stop = yes,
            some [!IO] 
            (
                impure invent_io(!:IO),
                
                print_event_info(Event, EventNum, ProcId, PrintDepth, CSN, 
                    !IO),
             
                impure prompt(Event, ShadowStack, 0, WhatNext, !IO),

                impure consume_io(!.IO),

                impure what_next_stop(EventNum, CSN, WhatNext, _Retry)
            )
        ;
            Stop = no
        )
    ;
        DebuggerState = debugger_off
    ;
        DebuggerState = debugger_disabled
    ).


    %
    % Call at call port of nondet procedure. It writes the event out and call 
    % the prompt.
    %
handle_event_call_nondet(ProcId, ListVarValue) :-
    semipure get_debugger_state(DebuggerState),
    ( 
        DebuggerState = debugger_on,
        
        Event = ssdb_call_nondet,
        impure get_ssdb_event_number_inc(EventNum),
        impure get_ssdb_csn_inc(CSN),
        impure get_ssdb_depth_inc(PrintDepth),

        % Push the new stack frame on top of the shadow stack.
        StackFrame = elem(EventNum, CSN, PrintDepth, ProcId, ListVarValue),
        
        semipure get_cur_ssdb_shadow_stack(ShadowStack0),
        stack.push(ShadowStack0, StackFrame, ShadowStack),
        impure set_cur_ssdb_shadow_stack(ShadowStack),

        semipure get_cur_ssdb_shadow_stack_nondet(ShadowStackNonDet0),
        stack.push(ShadowStackNonDet0, StackFrame, ShadowStackNonDet),
        impure set_cur_ssdb_shadow_stack_nondet(ShadowStackNonDet),

        semipure should_stop_at_this_event(Event, EventNum, CSN, ProcId, Stop, 
            _AutoRetry),
        (
            Stop = yes,
            some [!IO] 
            (
                impure invent_io(!:IO),
                
                print_event_info(Event, EventNum, ProcId, PrintDepth, CSN, 
                    !IO),
             
                impure prompt(Event, ShadowStack, 0, WhatNext, !IO),

                impure consume_io(!.IO),

                impure what_next_stop(EventNum, CSN, WhatNext, _Retry)
            )
        ;
            Stop = no
        )
    ;
        DebuggerState = debugger_off
    ;
        DebuggerState = debugger_disabled
    ).


    %
    % Call at exit port. Write the event out and call the prompt.
    %
handle_event_exit(ProcId, ListVarValue, Retry) :-
    semipure get_debugger_state(DebuggerState),
    ( 
        DebuggerState = debugger_on,
        
        Event = ssdb_exit,
        impure get_ssdb_event_number_inc(EventNum),
        impure get_ssdb_depth_dec(PrintDepth),
        impure set_list_var_value_in_shadow_stack(ListVarValue),

        % Just get the top stack frame. It will be popped at the end of
        % handle_event. We need to leave the frame in place, e.g. for
        % printing variables at the exit port of the procedure.
        semipure get_cur_ssdb_shadow_stack(ShadowStack0),
        stack.top_det(ShadowStack0, StackFrame),
        CSN = StackFrame ^ se_csn,
        
        semipure should_stop_at_this_event(Event, EventNum, CSN, ProcId, Stop, 
            AutoRetry),
        (
            Stop = yes,
            some [!IO] 
            (
                impure invent_io(!:IO),
                
                print_event_info(Event, EventNum, ProcId, PrintDepth + 1, CSN, 
                    !IO),
             
                (
                    AutoRetry = do_retry,
                    WhatNext = wn_retry(CSN)
                ;
                    AutoRetry = do_not_retry,
                    impure prompt(Event, ShadowStack0, 0, WhatNext, !IO)
                ),

                impure consume_io(!.IO),

                impure what_next_stop(EventNum, CSN, WhatNext, Retry)
            )
        ;
            Stop = no,
            Retry = do_not_retry
        ),
        
        stack.pop_det(ShadowStack0, _StackFrame1, ShadowStack),
        impure set_cur_ssdb_shadow_stack(ShadowStack)
    ;
        ( DebuggerState = debugger_off
        ; DebuggerState = debugger_disabled
        ),
        Retry = do_not_retry
    ).


    %
    % Call at exit port of nondet procedure only.
    %
handle_event_exit_nondet(ProcId, ListVarValue) :-
    semipure get_debugger_state(DebuggerState),
    ( 
        DebuggerState = debugger_on,
        
        Event = ssdb_exit_nondet,
        impure get_ssdb_event_number_inc(EventNum),
        impure get_ssdb_depth_dec(PrintDepth),
        impure set_list_var_value_in_shadow_stack(ListVarValue),

        % Just get the top stack frame. It will be popped at the end of
        % handle_event. We need to leave the frame in place, e.g. for
        % printing variables at the exit port of the procedure.
        semipure get_cur_ssdb_shadow_stack(ShadowStack0),
        stack.top_det(ShadowStack0, StackFrame),
        CSN = StackFrame ^ se_csn,
        
        semipure should_stop_at_this_event(Event, EventNum, CSN, ProcId, Stop, 
            AutoRetry),
        (
            Stop = yes,
            some [!IO] 
            (
                impure invent_io(!:IO),
                
                print_event_info(Event, EventNum, ProcId, PrintDepth + 1, CSN, 
                    !IO),
             
                (
                    AutoRetry = do_retry,
                    WhatNext = wn_retry(CSN)
                ;
                    AutoRetry = do_not_retry,
                    impure prompt(Event, ShadowStack0, 0, WhatNext, !IO)
                ),

                impure consume_io(!.IO),
        
                impure what_next_stop(EventNum, CSN, WhatNext, _Retry)
            )
        ;
            Stop = no
        ),
        
        stack.pop_det(ShadowStack0, _StackFrame1, ShadowStack),
        impure set_cur_ssdb_shadow_stack(ShadowStack)
    ;
        ( DebuggerState = debugger_off
        ; DebuggerState = debugger_disabled
        )
    ).


    %
    % Call at fail port. Write the event out and call the prompt.
    %
handle_event_fail(ProcId, _ListVarValue, Retry) :-
    semipure get_debugger_state(DebuggerState),
    ( 
        DebuggerState = debugger_on,

        Event = ssdb_fail,
        impure get_ssdb_event_number_inc(EventNum),
        impure get_ssdb_depth_dec(PrintDepth),
        semipure get_cur_ssdb_shadow_stack(ShadowStack0),
        stack.top_det(ShadowStack0, StackFrame),
        CSN = StackFrame ^ se_csn,
        
        semipure should_stop_at_this_event(Event, EventNum, CSN, ProcId, Stop, 
            AutoRetry),
        (
            Stop = yes,
            some [!IO] 
            (
                impure invent_io(!:IO),
                
                print_event_info(Event, EventNum, ProcId, PrintDepth + 1, CSN, 
                    !IO),
             
                (
                    AutoRetry = do_retry,
                    WhatNext = wn_retry(CSN)
                ;
                    AutoRetry = do_not_retry,
                    impure prompt(Event, ShadowStack0, 0, WhatNext, !IO)
                ),

                impure consume_io(!.IO),

                impure what_next_stop(EventNum, CSN, WhatNext, Retry)
            )
        ;
            Stop = no,
            Retry = do_not_retry
        ),
        
        stack.pop_det(ShadowStack0, _StackFrame1, ShadowStack),
        impure set_cur_ssdb_shadow_stack(ShadowStack)
    ;
        ( DebuggerState = debugger_off
        ; DebuggerState = debugger_disabled
        ),
        Retry = do_not_retry
    ).


    %
    % Call at fail port of nondet procedure only.
    %
handle_event_fail_nondet(ProcId, _ListVarValue, Retry) :-
    Event = ssdb_fail_nondet,
    
    semipure get_debugger_state(DebuggerState),
    ( 
        DebuggerState = debugger_on,

        impure get_ssdb_event_number_inc(EventNum),
        semipure get_cur_ssdb_shadow_stack(ShadowStack0),
        stack.top_det(ShadowStack0, StackFrame),
        CSN = StackFrame ^ se_csn,
        impure get_ssdb_depth_dec(PrintDepth),
        semipure get_cur_ssdb_shadow_stack_nondet(ShadowStackNonDet0),
        
        semipure should_stop_at_this_event(Event, EventNum, CSN, ProcId, Stop, 
            AutoRetry),
        (
            Stop = yes,

            some [!IO]
            (
                impure invent_io(!:IO),
                
                print_event_info(Event, EventNum, ProcId, PrintDepth + 1, CSN, 
                    !IO),
             
                (
                    AutoRetry = do_retry,
                    (
                        semipure get_correct_frame_nondet(ProcId, PrintDepth+1, 
                            StackFrame)
                    ->
                        EventNumF = StackFrame ^ se_event_number,
                        CSNF = StackFrame ^ se_csn,
                        impure set_cur_ssdb_event_number(EventNumF-1),
                        impure set_cur_ssdb_csn(CSNF-1)
                    ;
                        error("Unexpected error : ssdb/ssdb.m " ++ 
                            "get_correct_frame_nondet failed")
                    ),
                    WhatNext = wn_retry(CSN)
                ;
                    AutoRetry = do_not_retry,
                    impure prompt(Event, ShadowStack0, 0, WhatNext, !IO)
                ),

                impure consume_io(!.IO),

                impure what_next_stop(EventNum, CSN, WhatNext, Retry)
            )
        ;
            Stop = no,
            Retry = do_not_retry
        ),

        stack.pop_det(ShadowStack0, _StackFrame, ShadowStack),
        stack.pop_det(ShadowStackNonDet0, _StackFrameNonDet, ShadowStackNonDet),
        impure set_cur_ssdb_shadow_stack(ShadowStack),
        impure set_cur_ssdb_shadow_stack_nondet(ShadowStackNonDet)
    ;
        DebuggerState = debugger_off,

        semipure get_cur_ssdb_depth(Depth),
        (   % if this frame is the required frame
            semipure get_correct_frame_nondet(ProcId, Depth+1, _StackFrame)
        ->  % then debugger will stop at next event
            impure set_debugger_state(debugger_on),
            Retry = do_retry
        ;   % otherwise continue
            Retry = do_not_retry
        )
    ;
        DebuggerState = debugger_disabled,
        Retry = do_not_retry
    ).


    %
    % Call at redo port in nondet procedure. Write the event out and call 
    % the prompt.
    %
handle_event_redo_nondet(ProcId, _ListVarValue) :-
    semipure get_debugger_state(DebuggerState),
    ( 
        DebuggerState = debugger_on,

        Event = ssdb_redo_nondet,
        impure get_ssdb_event_number_inc(EventNum),
        impure get_ssdb_depth_inc(PrintDepth),

        (
            semipure get_correct_frame_nondet(ProcId, PrintDepth, StackFrame)
        ->
            semipure get_cur_ssdb_shadow_stack(ShadowStack0),
            stack.push(ShadowStack0, StackFrame, ShadowStack),
            impure set_cur_ssdb_shadow_stack(ShadowStack),
            CSN = StackFrame ^ se_csn,
        
            semipure should_stop_at_this_event(Event, EventNum, CSN, ProcId, 
                Stop, _AutoRetry),
            (
                Stop = yes,
                some [!IO] 
                (
                    impure invent_io(!:IO),
                
                    print_event_info(Event, EventNum, ProcId, PrintDepth, CSN,
                        !IO),
             
                    impure prompt(Event, ShadowStack, 0, WhatNext, !IO),

                    impure consume_io(!.IO),

                    impure what_next_stop(EventNum, CSN, WhatNext, _Retry)
                )
            ;
                Stop = no
            )
        ;
            error("Unexpected error : ssdb/ssdb.m : " ++
                "get_correct_frame_nondet failed")
        )

    ;
        ( DebuggerState = debugger_off
        ; DebuggerState = debugger_disabled
        )
    ).


%----------------------------------------------------------------------------%


    %
    % IsSame is 'yes' iff the two call sequence numbers are equal, 
    % 'no' otherwise.
    %
:- pred is_same_int(int::in, int::in, bool::out) is det.

is_same_int(IntA, IntB, IsSame) :-
    IsSame = (IntA = IntB -> yes ; no).


    %
    % Increment the CSN and return the new value.
    %
:- impure pred get_ssdb_csn_inc(int::out) is det.

get_ssdb_csn_inc(CSN) :-
    semipure get_cur_ssdb_csn(CSN0),
    CSN = CSN0 + 1,
    impure set_cur_ssdb_csn(CSN).


    %
    % Increment the Event Number and return the new value.
    %
:- impure pred get_ssdb_event_number_inc(int::out) is det.

get_ssdb_event_number_inc(EventNum) :-
    semipure get_cur_ssdb_event_number(EventNum0),
    EventNum = EventNum0 + 1,
    impure set_cur_ssdb_event_number(EventNum).

    %
    % Increment the depth and return the new value.
    %
:- impure pred get_ssdb_depth_inc(int::out) is det.

get_ssdb_depth_inc(Depth) :-
    semipure get_cur_ssdb_shadow_stack(ShadowStack),
    Depth0 = stack.depth(ShadowStack),
    Depth = Depth0 + 1,
    impure set_cur_ssdb_depth(Depth).

    %
    % Decrement the depth and return the new value.
    %
:- impure pred get_ssdb_depth_dec(int::out) is det.

get_ssdb_depth_dec(Depth) :-
    semipure get_cur_ssdb_shadow_stack(ShadowStack),
    Depth0 = stack.depth(ShadowStack),
    Depth = Depth0 - 1,
    impure set_cur_ssdb_depth(Depth).

    
    %
    % Setter of the se_list_var_value in the first stack_elem.
    %
:- impure pred set_list_var_value_in_shadow_stack(list(var_value)::in) is det.

set_list_var_value_in_shadow_stack(ListVarValue) :-
    semipure get_cur_ssdb_shadow_stack(ShadowStack0),
    stack.pop_det(ShadowStack0, StackFrame0, PopedStack),
    StackFrame = StackFrame0 ^ se_list_var_value := ListVarValue,
    stack.push(PopedStack, StackFrame, ShadowStack),
    impure set_cur_ssdb_shadow_stack(ShadowStack).


    %
    % should_stop_at_the_event(Event, CSN, EventNum, ProcId, Stop, AutoRetry).
    %
    % Set Stop, if Stop equals yes, the prompt will be call.
    %
:- semipure pred should_stop_at_this_event(ssdb_event_type::in, int::in, 
    int::in, ssdb_proc_id::in, bool::out, ssdb_retry::out) is det.

should_stop_at_this_event(Event, EventNum, CSN, ProcId, ShouldStopAtEvent, 
        AutoRetry) :-
    semipure get_cur_ssdb_next_stop(NextStop),
    (
        NextStop = ns_step,
        ShouldStopAtEvent = yes,
        AutoRetry = do_not_retry
    ;
        NextStop = ns_next(StopCSN),
        is_same_int(StopCSN, CSN, ShouldStopAtEvent),
        AutoRetry = do_not_retry
    ;
        NextStop = ns_continue,
        semipure get_cur_ssdb_breakpoints(BreakPoints),
        (
            map.search(BreakPoints, 
                pair(ProcId ^ module_name, ProcId ^ proc_name), BreakPoint)
        ->
            (
                BreakPoint ^ bp_state = bp_state_enabled
            ->
                ShouldStopAtEvent = yes
            ;
                ShouldStopAtEvent = no
            )
        ;
            ShouldStopAtEvent = no
        ),
        AutoRetry = do_not_retry
    ;
        NextStop = ns_final_port(StopCSN, AutoRetry),
        (
            ( Event = ssdb_exit
            ; Event = ssdb_exit_nondet
            ; Event = ssdb_fail
            ; Event = ssdb_fail_nondet
            ),
            is_same_int(StopCSN, CSN, ShouldStopAtEvent)
        ;
            ( Event = ssdb_call
            ; Event = ssdb_call_nondet
            ; Event = ssdb_redo_nondet
            ),
            ShouldStopAtEvent = no
        )
    ;
        NextStop = ns_final_port_nondet(StopCSN, AutoRetry),
        (
            Event = ssdb_fail_nondet,
            is_same_int(StopCSN, CSN, ShouldStopAtEvent)
        ;
            ( Event = ssdb_call
            ; Event = ssdb_exit
            ; Event = ssdb_fail
            ; Event = ssdb_call_nondet
            ; Event = ssdb_exit_nondet
            ; Event = ssdb_redo_nondet
            ),
            ShouldStopAtEvent = no
        )
    ;
        NextStop = ns_goto(EventNumToGo),
        is_same_int(EventNumToGo, EventNum, ShouldStopAtEvent),
        AutoRetry = do_not_retry
    ).


    %
    % what_next_stop(CSN, EventNum, WhatNext, Retry).
    %
    % Set the NextStop and the Retry variable according to the WhatNext value.
    % In the case where the WathNext is set for a retry, it modify the 
    % debugger_state at his old value which it had at the call point.
    %
:- impure pred what_next_stop(int::in, int::in, what_next::in, 
    ssdb_retry::out) is det.

what_next_stop(EventNum, CSN, WhatNext, Retry) :-
    (
        WhatNext = wn_step,
        NextStop = ns_step,
        Retry = do_not_retry
    ;
        WhatNext = wn_next,
        NextStop = ns_next(CSN),
        Retry = do_not_retry
    ;
        WhatNext = wn_continue,
        NextStop = ns_continue,
        Retry = do_not_retry
    ;
        WhatNext = wn_finish(EndCSN),
        NextStop = ns_final_port(EndCSN, do_not_retry),
        Retry = do_not_retry
    ;
        WhatNext = wn_retry(RetryCSN),
        (
            RetryCSN = CSN
        ->
            NextStop = ns_step,
            Retry = do_retry
        ;
            NextStop = ns_final_port(RetryCSN, do_retry),
            Retry = do_not_retry
        )
    ;
        WhatNext = wn_retry_nondet(RetryCSN),
        (
            NextStop = ns_final_port_nondet(RetryCSN, do_retry),
            Retry = do_not_retry
        )
    ;
        WhatNext = wn_goto(EventNumToGo),
        (
            EventNum = EventNumToGo
        ->
            NextStop = ns_step,
            Retry = do_not_retry
        ;
            NextStop = ns_goto(EventNumToGo),
            Retry = do_not_retry
        )
    ),
    impure set_cur_ssdb_next_stop(NextStop).


    %
    % This two following predicates get the right informations in the 
    % shadow_stack_nondet about the current procedure.
    %
:- semipure pred get_correct_frame_nondet(ssdb_proc_id::in, int::in, 
    stack_elem::out) is semidet.

get_correct_frame_nondet(ProcId, Depth, StackFrame) :-
    semipure get_cur_ssdb_shadow_stack_nondet(ShadowStackNonDet),
    get_correct_frame_nondet_2(ProcId, Depth, ShadowStackNonDet, StackFrame).


:- pred get_correct_frame_nondet_2(ssdb_proc_id::in, int::in, 
    stack(stack_elem)::in, stack_elem::out) is semidet.

get_correct_frame_nondet_2(ProcId, Depth, ShadowStackNonDet0, StackFrame) :-
    (
        stack.is_empty(ShadowStackNonDet0)
    ->
        fail
    ;
        stack.pop_det(ShadowStackNonDet0, Frame, ShadowStackNonDet),
        (
            Frame ^ se_proc_id ^ module_name = ProcId ^ module_name,
            Frame ^ se_proc_id ^ proc_name = ProcId ^ proc_name,
            Frame ^ se_depth = Depth
        ->
            StackFrame = Frame
        ;
            get_correct_frame_nondet_2(ProcId, Depth, ShadowStackNonDet, 
                StackFrame)
        )
    ).


    %
    % Print the current informations at this event point.
    %
:- pred print_event_info(ssdb_event_type::in, int::in, ssdb_proc_id::in, 
    int::in, int::in, io::di, io::uo) is det.
    
print_event_info(Event, EventNum, ProcId, PrintDepth, CSN, !IO) :-
    io.write_string("       ", !IO),
    io.write_int(EventNum, !IO),
    io.write_string("\t", !IO),
    io.write_int(CSN, !IO),
    io.write_string("\t", !IO),
    io.write_int(PrintDepth, !IO),
    io.write_string("\t", !IO),
    ( 
        ( Event = ssdb_call
        ; Event = ssdb_call_nondet
        ),
        io.write_string("CALL", !IO)
    ;
        ( Event = ssdb_exit
        ; Event = ssdb_exit_nondet
        ),
        io.write_string("EXIT", !IO)
    ;
        ( Event = ssdb_fail
        ; Event = ssdb_fail_nondet
        ),
        io.write_string("FAIL", !IO)
    ;
        Event = ssdb_redo_nondet,
        io.write_string("REDO", !IO)
    ),
    io.write_string("\t\t", !IO),
    io.write_string(ProcId ^ module_name, !IO),
    io.write_string(".", !IO),
    io.write_string(ProcId ^ proc_name, !IO),
    io.nl(!IO).


%----------------------------------------------------------------------------%

    %
    % Display the prompt to debug.
    %
    % h     :: help
    % f     :: finish (go to the next exit or fail of the current call)
    % n     :: next
    % s | _ :: next step
    % c     :: continue
    % b X Y :: breakpoint X = module_name Y = predicate_name
    % b info:: print info of breakpoints
    % delete/enable/disable */N 
    %       :: delete/enable/disable all/Nth breakpoint
    % p     :: print
    % dump  :: print stack trace
    % u     :: up
    % d     :: down
    % g N   :: goto Nth event number
    %

:- impure pred prompt(ssdb_event_type::in, stack(stack_elem)::in, int::in, 
                what_next::out, io::di, io::uo) is det.

prompt(Event, ShadowStack, Depth, WhatNext, !IO) :-
    % XXX use stdout_stream
    io.write_string("ssdb> ", !IO),
    % Read a string in input and return a string.
    io.read_line_as_string(io.stdin_stream, Result, !IO), 
    (
        Result = ok(String0),
        % String minus any single trailing newline character.
        String = string.chomp(String0),
        Words = string.words(String), 

        ( 
            ( Words = ["h"]
            ; Words = ["help"]
            )
        ->
                io.nl(!IO),
                io.write_string("\nPrincipal Commands", !IO),
                io.write_string("\n------------------", !IO),
                io.nl(!IO),
                io.write_string("\n<step> or <s> or < >", !IO),
                io.write_string("\n<next> or <n>n", !IO),
                io.write_string("\n<continue> or <c>", !IO),
                io.write_string("\n<finish> or <f>", !IO),
                io.write_string("\n<retry> or <r>", !IO),
                io.write_string("\n<break X Y> or <b X Y>", !IO),
                io.write_string("\n<break info> or <b info>", !IO),
                io.write_string("\n<enable / disable / delete *>", !IO),
                io.write_string("\n<enable / disable / delete N>", !IO),
                io.write_string("\n<print> or <p>", !IO),
                io.write_string("\n<stack> or <st>", !IO),
                io.write_string("\n<up> or <u>", !IO),
                io.write_string("\n<down> or <d>", !IO),
                io.write_string("\n<goto N> or <g N>", !IO),
                io.write_string("\n<help> or <h>", !IO),
                io.nl(!IO),
                io.nl(!IO),
                io.write_string("\nConsult the file : " ++
                    "/ssdb/SSDB_COMMAND_HELP.txt for details", !IO),
                io.nl(!IO),
                impure prompt(Event, ShadowStack, Depth, WhatNext, !IO)

        ; 
            ( Words = ["p"] 
            ; Words = ["print"]
            )
        ->
                CurrentFrame = stack.top_det(ShadowStack),
                ListVarValue = CurrentFrame ^ se_list_var_value,
                print_vars(ListVarValue, !IO),
                impure prompt(Event, ShadowStack, Depth, WhatNext, !IO)

        ; 
            ( Words = ["st"]
            ; Words = ["stack"] 
            )
        ->
                print_frames_list(ShadowStack, Depth, !IO),
                impure prompt(Event, ShadowStack, Depth, WhatNext, !IO)

        ; 
            ( Words = ["n"] 
            ; Words = ["next"]
            ) 
        ->
                ( 
                    ( Event = ssdb_call 
                    ; Event = ssdb_call_nondet
                    ; Event = ssdb_redo_nondet
                    ) 
                ->
                    WhatNext = wn_next
                ;
                    io.write_string("Impossible at exit or fail port\n", !IO),
                    impure prompt(Event, ShadowStack, Depth, WhatNext, !IO)
                )

        ;
%             ( list.is_empty(Words)
            ( Words = []
            ; Words = ["s"]
            ; Words = ["step"]
            )
        ->
                WhatNext = wn_step

        ; 
            ( Words = ["c"] 
            ; Words = ["continue"]
            )
        ->
                WhatNext = wn_continue

        ; 
            ( Words = ["b", ModuleName, ProcedureName]
            ; Words = ["break", ModuleName, ProcedureName]
            )
        ->
                semipure get_cur_ssdb_breakpoints(BreakPoints0),
                Key = pair(ModuleName, ProcedureName),
                ( map.contains(BreakPoints0, Key)
                ->
                    io.write_string("The new breakpoint already exist\n", !IO),
                    impure prompt(Event, ShadowStack, Depth, WhatNext, !IO)
                ;   
                    semipure get_cur_ssdb_number_of_breakpoint(Number),
                    NewBreakPoint = breakpoint(Number+1, ModuleName, 
                        ProcedureName, bp_state_enabled),
                    map.det_insert(BreakPoints0, Key, NewBreakPoint, 
                        BreakPoints),
                    BreakPointsListValue = map.values(BreakPoints),
                    print_breakpoints(BreakPointsListValue, !IO),
                    impure set_cur_ssdb_breakpoints(BreakPoints),
                    impure set_cur_ssdb_number_of_breakpoint(Number+1),
                    impure prompt(Event, ShadowStack, Depth, WhatNext, !IO)
                )
        
        ; 
            ( Words = ["f"] 
            ; Words = ["finish"]
            )
        ->
                ( 
                    ( Event = ssdb_call
                    ; Event = ssdb_call_nondet
                    ; Event = ssdb_redo_nondet
                    ) 
                ->
                    stack.top_det(ShadowStack, FrameStack),
                    CSN = FrameStack ^  se_csn,
                    WhatNext = wn_finish(CSN)
                ;
                    io.write_string("impossible at exit or fail port\n", !IO),
                    impure prompt(Event, ShadowStack, Depth, WhatNext, !IO)
                )

        ; 
            ( Words = ["f", NStr] 
            ; Words = ["finish", NStr]
            ) 
        ->
                ( 
                    string.to_int(NStr, Num),
                    semipure get_cur_ssdb_depth(CurDepth)
                ->
                    (
                        Num >= 1,
                        Num =< CurDepth 
                    ->
                        get_correct_frame_with_num(Num, ShadowStack, 
                            StackFrame),
                        CSN = StackFrame ^ se_csn,
                        WhatNext = wn_finish(CSN)
                    ;
                        io.format("The number must be between 1 and %i\n", 
                            [i(CurDepth)], !IO),
                        impure prompt(Event, ShadowStack, Depth, WhatNext, !IO)
                    )
                ;
                    io.write_string("The number must be an integer\n", !IO),
                    impure prompt(Event, ShadowStack, Depth, WhatNext, !IO)
                )

        ; 
            ( Words = ["d"] 
            ; Words = ["down"] 
            ) 
        ->
                (
                    DownDepth = Depth - 1,
                    DownDepth >= 0
                ->
                    impure prompt(Event, ShadowStack, DownDepth, WhatNext, !IO)
                ;
                    io.write_string("Impossible to go down\n", !IO),
                    impure prompt(Event, ShadowStack, Depth, WhatNext, !IO)
                )
            
        ; 
            ( Words = ["u"] 
            ; Words = ["up"]
            )
        ->
                (
                    UpDepth = Depth + 1,
                    UpDepth < stack.depth(ShadowStack) 
                ->
                    impure prompt(Event, ShadowStack, UpDepth, WhatNext, !IO)
                ;
                    io.write_string("Impossible to go up\n", !IO),
                    impure prompt(Event, ShadowStack, Depth, WhatNext, !IO)
                )

        ; 
            ( Words = ["r"] 
            ; Words = ["retry"] 
            )
        ->
                (
                    ( Event = ssdb_exit
                    ; Event = ssdb_fail
                    ; Event = ssdb_fail_nondet
                    ),
                    stack.top_det(ShadowStack, FrameStack),
                    EventNum = FrameStack ^ se_event_number,
                    CSN = FrameStack ^ se_csn,
                    impure set_cur_ssdb_event_number(EventNum-1),
                    impure set_cur_ssdb_csn(CSN-1),
                    WhatNext = wn_retry(CSN)
                ;
                    Event = ssdb_exit_nondet,
                    stack.top_det(ShadowStack, FrameStack),
                    EventNum = FrameStack ^ se_event_number,
                    CSN = FrameStack ^ se_csn,
                    impure set_debugger_state(debugger_off),
                    % Set the event number and the CSN minus 1 because it will 
                    % be increment at the next event. So, we need to set the 
                    % value at the event just before the retried procedure.
                    impure set_cur_ssdb_event_number(EventNum-1),
                    impure set_cur_ssdb_csn(CSN-1),
                    WhatNext = wn_retry_nondet(CSN)
                ;
                    ( Event = ssdb_call
                    ; Event = ssdb_call_nondet
                    ; Event = ssdb_redo_nondet
                    ),
                    io.write_string("Impossible at call or redo port\n", !IO),
                    impure prompt(Event, ShadowStack, Depth, WhatNext, !IO)
                )

        ; 
            ( Words = ["r", NStr] 
            ; Words = ["retry", NStr]
            )
        ->
                ( 
                    string.to_int(NStr, Num),
                    semipure get_cur_ssdb_depth(CurDepth)
                ->
                    (
                        Num >= 1,
                        Num =< CurDepth 
                    ->
                        (
                            ( Event = ssdb_exit
                            ; Event = ssdb_fail
                            ; Event = ssdb_fail_nondet
                            ),
                            get_correct_frame_with_num(Num, ShadowStack, 
                                FrameStack),
                            EventNum = FrameStack ^ se_event_number,
                            CSN = FrameStack ^ se_csn,
                            impure set_cur_ssdb_event_number(EventNum-1),
                            impure set_cur_ssdb_csn(CSN-1),
                            WhatNext = wn_retry(CSN)
                        ;
                            Event = ssdb_exit_nondet,
                            get_correct_frame_with_num(Num, ShadowStack, 
                                FrameStack),
                            EventNum = FrameStack ^ se_event_number,
                            CSN = FrameStack ^ se_csn,
                            impure set_debugger_state(debugger_off),
                            % Set the event number and the CSN minus 1 because 
                            % it will be increment at the next event. So, we 
                            % need to be at the event just before the call.
                            impure set_cur_ssdb_event_number(EventNum-1),
                            impure set_cur_ssdb_csn(CSN-1),     
                            WhatNext = wn_retry_nondet(CSN)
                        ;
                            ( Event = ssdb_call
                            ; Event = ssdb_call_nondet
                            ; Event = ssdb_redo_nondet
                            ),
                            io.write_string("Impossible at call or redo 
                                port\n", !IO),
                            impure prompt(Event, ShadowStack, Depth, WhatNext, 
                                !IO)
                        )
                    ;
                        io.format("The number must be between 1 and %i\n", 
                            [i(CurDepth)], !IO),
                        impure prompt(Event, ShadowStack, Depth, WhatNext, !IO)
                    )
                ;
                    io.write_string("The number must be an integer\n", !IO),
                    impure prompt(Event, ShadowStack, Depth, WhatNext, !IO)
                )

        ; 
            ( Words = ["g", EventNumToGoStr] 
            ; Words = ["goto", EventNumToGoStr]
            )
        ->
                ( 
                    string.to_int(EventNumToGoStr, EventNumToGo) 
                ->
                    WhatNext = wn_goto(EventNumToGo)
                ;
                    io.write_string("The number must be an integer\n", !IO),
                    impure prompt(Event, ShadowStack, Depth, WhatNext, !IO)
                )

        ; 
            ( Words = ["b", "info"] 
            ; Words = ["break", "info"]
            )
        ->
                semipure get_cur_ssdb_breakpoints(BreakPoints),
                BreakPointsListValue = map.values(BreakPoints),
                print_breakpoints(BreakPointsListValue, !IO),
                impure prompt(Event, ShadowStack, Depth, WhatNext, !IO)

        ; Words = ["disable", "*"] ->
            impure modify_state_breakpoints(bp_state_disabled, !IO),
            impure prompt(Event, ShadowStack, Depth, WhatNext, !IO)

        ; Words = ["disable", NumStr] ->
            ( 
                string.to_int(NumStr, Num) 
            ->
                impure modify_state_breakpoint_with_num(bp_state_disabled, Num, 
                    !IO),
                impure prompt(Event, ShadowStack, Depth, WhatNext, !IO)
            ;
                io.write_string("The number must be an integer\n", !IO),
                impure prompt(Event, ShadowStack, Depth, WhatNext, !IO)
            )

        ; Words = ["enable", "*"] ->
            impure modify_state_breakpoints(bp_state_enabled, !IO),
            impure prompt(Event, ShadowStack, Depth, WhatNext, !IO)

        ; Words = ["enable", NumStr] ->
            ( 
                string.to_int(NumStr, Num) 
            ->
                impure modify_state_breakpoint_with_num(bp_state_enabled, Num, 
                    !IO),
                impure prompt(Event, ShadowStack, Depth, WhatNext, !IO)
            ;
                io.write_string("The number must be an integer\n", !IO),
                impure prompt(Event, ShadowStack, Depth, WhatNext, !IO)
            )

        ; Words = ["delete", "*"] ->
            BreakPoints = map.init,
            impure set_cur_ssdb_breakpoints(BreakPoints),
            io.write_string("All breakpoints have been deleted.\n", !IO),
            impure prompt(Event, ShadowStack, Depth, WhatNext, !IO)

        ; Words = ["delete", NumStr] ->
            ( 
                string.to_int(NumStr, Num) 
            ->
                impure delete_breakpoint_with_num(Num, !IO),
                impure prompt(Event, ShadowStack, Depth, WhatNext, !IO)
            ;
                io.write_string("The number must be an integer\n", !IO),
                impure prompt(Event, ShadowStack, Depth, WhatNext, !IO)
            )

        ; Words = ["browse", VarName] ->
            (
                get_correct_frame_with_num(1, ShadowStack, CurFrame),
                ListVarValue = CurFrame ^ se_list_var_value,
                list_var_value_to_assoc_list(ListVarValue, AssListVarValue),
                assoc_list.search(AssListVarValue, VarName, Univ)
            ->
                io.stdin_stream(StdIn, !IO),
                io.stdout_stream(StdOut, !IO),
                browser_info.init_persistent_state(State0),
                BT = browser_term.univ_to_browser_term(Univ),
                promise_equivalent_solutions [!:IO] (
                    browse.browse_browser_term_no_modes(BT, StdIn, StdOut, _, 
                    State0, _State1, !IO)),
                impure prompt(Event, ShadowStack, Depth, WhatNext, !IO)
            ;
                io.write_string("\nError in browse command\n", !IO),
                impure prompt(Event, ShadowStack, Depth, WhatNext, !IO)
            )
        ;
            io.write_string("huh?\n", !IO),
            impure prompt(Event, ShadowStack, Depth, WhatNext, !IO)
        )
    ;
        Result = eof,
        error("eof from read_line_as_string")
    ;
        Result = error(_),
        error("error from read_line_as_string")
    ).


    %
    % Transform the list(var_value) into a assoc_list. As it is for the browser 
    % use, only the bound variable are put into the assoc_list structure.
    %
:- pred list_var_value_to_assoc_list(list(var_value)::in, 
    assoc_list(string, univ)::out) is det.

list_var_value_to_assoc_list([], []).
list_var_value_to_assoc_list([VarValue | VarValues], AssocListVarValue) :-
    (
        VarValue = unbound_head_var(_Name, _Pos),
        list_var_value_to_assoc_list(VarValues, AssocListVarValue)
    ;
        VarValue = bound_head_var(Name, _Pos, Value),
        type_to_univ(Value, ValueUniv),
        list_var_value_to_assoc_list(VarValues, AssocListVarValue0),
        AssocListVarValue = [pair(Name, ValueUniv) | AssocListVarValue0]
    ;
        VarValue = bound_other_var(Name, Value),
        type_to_univ(Value, ValueUniv),
        list_var_value_to_assoc_list(VarValues, AssocListVarValue0),
        AssocListVarValue = [pair(Name, ValueUniv) | AssocListVarValue0]
    ).

    %
    % get_correct_frame_with_num(Num, ShadowStack, Frame).
    %
    % Get the Nth frame from the shadow stack, beginning from the top.
    % Num should be in the interval of 1 =< Num =< Depth.
    % If Num = 1, get the top frame. 
    % If Num = Depth, get the deepest frame.
    %
:- pred get_correct_frame_with_num(int::in, stack(stack_elem)::in, 
    stack_elem::out) is det.

get_correct_frame_with_num(Num, ShadowStack0, StackFrame) :-
    ( Num = 1 ->
        stack.top_det(ShadowStack0, StackFrame)

    ; Num > 1 ->
        stack.pop_det(ShadowStack0, _Frame, ShadowStack),
        get_correct_frame_with_num(Num-1, ShadowStack, StackFrame)

    ;
        % it shouldn't arrive here.
        error("Unexpected error : get_correct_frame_with_num")
    ).

    %
    % Disable or enable all breakpoints.
    %
:- impure pred modify_state_breakpoints(bp_state::in, io::di, io::uo) is det.

modify_state_breakpoints(State, !IO) :-
    semipure get_cur_ssdb_breakpoints(BreakPoints),
    BreakPointListValue = map.values(BreakPoints),
    modify_state_breakpoint(State, BreakPointListValue, BreakPoints, 
        BreakPointsModified, !IO),
    impure set_cur_ssdb_breakpoints(BreakPointsModified).


    %
    % Modify state (enable or disable) of one breakpoint.
    %
:- pred modify_state_breakpoint(bp_state::in, list(breakpoint)::in, 
        map(pair(string, string), breakpoint)::in, 
        map(pair(string, string), breakpoint)::out, 
        io::di, io::uo) is det.

modify_state_breakpoint(_State, [], !BreakPoints, !IO).
modify_state_breakpoint(State, [BreakPoint0|BreakPoints], !BreakPoints, !IO) :-
    BreakPoint = BreakPoint0 ^ bp_state := State,   
    print_breakpoint(BreakPoint, !IO),
    map.det_update(!.BreakPoints, 
        pair(BreakPoint0 ^ bp_module_name, BreakPoint0 ^ bp_pred_name), 
        BreakPoint, !:BreakPoints),
    modify_state_breakpoint(State, BreakPoints, !BreakPoints, !IO).


    %
    % modify_state_breakpoint_with_num(State, Num, !IO).
    %
    % Modify the state of the breakpoint with the number which match Num.
    %
:- impure pred modify_state_breakpoint_with_num(bp_state::in, int::in, 
    io::di, io::uo) is det.

modify_state_breakpoint_with_num(State, Num, !IO) :-
    (
        semipure get_cur_ssdb_breakpoints(BreakPoints),
        BreakPointListValue = map.values(BreakPoints),
        find_breakpoint_with_num(Num, BreakPointListValue, BreakPointToModify)
    ->
        modify_state_breakpoint(State, [BreakPointToModify], BreakPoints, 
            BreakPointsModified, !IO),
        impure set_cur_ssdb_breakpoints(BreakPointsModified)
    ;
        io.write_string("No breakpoint found.\n", !IO)
    ).
    


    %
    % delete_breakpoint_with_num(Num, !IO).
    %
    % Delete the breakpoint that match with Num.
    %
:- impure pred delete_breakpoint_with_num(int::in, io::di, io::uo) is det.

delete_breakpoint_with_num(Num, !IO) :-
    (
        semipure get_cur_ssdb_breakpoints(BreakPoints0),
        BreakPointsListValue = map.values(BreakPoints0),
        find_breakpoint_with_num(Num, BreakPointsListValue, BPToDelete)
    ->
        Module = BPToDelete ^ bp_module_name,
        Procedure = BPToDelete ^ bp_pred_name,
        map.delete(BreakPoints0, pair(Module, Procedure), BreakPoints),
        impure set_cur_ssdb_breakpoints(BreakPoints),
        io.format("Breakpoint on %s.%s deleted\n", [s(Module), s(Procedure)], 
            !IO)
    ;
        io.write_string("No breakpoint found.\n", !IO)
    ).


    %
    % find_breakpoint_with_num(Num, ListBreakPoint, BreakPointFound)
    %
    % As the structure of a breakpoint have a Number, this predicate will 
    % return BreakPointFound with bp_number that match with the given Num.
    %
:- pred find_breakpoint_with_num(int::in, list(breakpoint)::in, 
    breakpoint::out) is semidet.

find_breakpoint_with_num(Num, [BP|ListBreakPoint], BreakPointFound) :-
    (
        BP ^ bp_number = Num
    ->
        BreakPointFound = BP 
    ;
        find_breakpoint_with_num(Num, ListBreakPoint, BreakPointFound)
    ).


%----------------------------------------------------------------------------%

    %
    % Print the Stack Trace. Predicate call at the 'stack' command.
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


    %
    % Print one frame.
    %
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
    ListVarValue = Frame ^ se_list_var_value,
    print_vars(ListVarValue, !IO),
    io.write_string("   )\n", !IO).


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
    io.write_char('\t', !IO),
    io.write_string("bound_head\t", !IO),
    io.write_string(Name, !IO),
    io.write_string(":\t", !IO),
    io.write_int(Pos, !IO),
    io.write_string("\t=\t", !IO),
    Doc = pretty_printer.format(T),
    write_doc(Doc, !IO),
    io.nl(!IO).
    
print_var(bound_other_var(Name, T), !IO) :-
    io.write_char('\t', !IO),
    io.write_string("bound_other\t", !IO),
    io.write_string(Name, !IO),
    io.write_string(":\t_\t", !IO),
    io.write_string("=\t", !IO),
    Doc = pretty_printer.format(T),
    write_doc(Doc, !IO),
    io.nl(!IO).


    %
    % Print the current list of breakpoints with their details.
    % 
:- pred print_breakpoints(list(breakpoint)::in, io::di, io::uo) is det.

print_breakpoints(BreakPoints, !IO) :-
    list.foldl(print_breakpoint, BreakPoints, !IO).

:- pred print_breakpoint(breakpoint::in, io::di, io::uo) is det.

print_breakpoint(BreakPoint, !IO) :-
    io.write_char('\t', !IO),
    io.write_int(BreakPoint ^ bp_number, !IO),
    io.write_char('\t', !IO),
    io.write_string(BreakPoint ^ bp_module_name, !IO),
    io.write_string(".", !IO),
    io.write_string(BreakPoint ^ bp_pred_name, !IO),
    io.write_string("\t", !IO),
    ( 
        BreakPoint ^ bp_state = bp_state_enabled,
        io.write_string("enable", !IO)
    ;
        BreakPoint ^ bp_state = bp_state_disabled,
        io.write_string("disable", !IO)
    ),
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
