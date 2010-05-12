%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2007, 2010 The University of Melbourne.
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
    ;       ssdb_fail_nondet.

    % Type to determine if it is necessary to do a retry.
    %
:- type ssdb_retry
    --->    do_retry
    ;       do_not_retry.

    % The list of all variables in use in a procedure.
    %
:- type list_var_value == list(var_value).

    % Record the instantiatedness and value of each variable used in a
    % procedure.
    %
:- type var_value
    --->    unbound_head_var(var_name, pos)
    ;       some [T] bound_head_var(var_name, pos, T)
    ;       some [T] bound_other_var(var_name, T).

    % Variable name.
    %
:- type var_name == string.

    % The argument position of the head variable.
    % Positions are numbered from 0.
    %
:- type pos == int.

    % This routine is called at each call event that occurs.
    %
:- impure pred handle_event_call(ssdb_proc_id::in, list_var_value::in) is det.

    % This routine is called at each call event in a nondet procedure.
    %
:- impure pred handle_event_call_nondet(ssdb_proc_id::in,
    list_var_value::in) is det.

    % This routine is called at each exit event that occurs.
    %
:- impure pred handle_event_exit(ssdb_proc_id::in, list_var_value::in,
    ssdb_retry::out) is det.

    % This routine is called at each exit event in a nondet procedure.
    %
:- impure pred handle_event_exit_nondet(ssdb_proc_id::in,
    list_var_value::in) is det.

    % This routine is called at each fail event that occurs.
    %
:- impure pred handle_event_fail(ssdb_proc_id::in, list_var_value::in,
    ssdb_retry::out) is det.

    % This routine is called at each fail event in a nondet procedure.
    %
:- impure pred handle_event_fail_nondet(ssdb_proc_id::in, list_var_value::in,
    ssdb_retry::out) is det.

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
:- import_module pprint.
:- import_module require.
:- import_module set.
:- import_module stack.
:- import_module string.
:- import_module univ.

:- import_module mdb.
:- import_module mdb.browse.
:- import_module mdb.browser_info.
:- import_module mdb.browser_term.

:- pragma foreign_decl("C",
"
    #include ""mercury_signal.h""
    static void MR_ssdb_sigint_handler(void);
").

%----------------------------------------------------------------------------%

:- type cur_ssdb_next_stop == next_stop.

:- type cur_ssdb_breakpoints == map(pair(string,string), breakpoint).

:- type cur_ssdb_shadow_stack == stack(stack_elem).

:- type cur_ssdb_shadow_stack_nondet == stack(stack_elem).

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

    % Type used by the read_and_execute_cmd predicate to configure
    % the next step in the handle_event predicate.
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

    % Type used by the handle_event predicate to determine the next stop of
    % the read_and_execute_cmd predicate.
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

    % Initialization of the mutable variables.
    %

:- mutable(cur_ssdb_event_number, int, 0, ground,
    [untrailed, attach_to_io_state]).

:- mutable(cur_ssdb_csn, int, 0, ground,
    [untrailed, attach_to_io_state]).

:- mutable(cur_ssdb_depth, int, 0, ground,
    [untrailed, attach_to_io_state]).

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

%-----------------------------------------------------------------------------%

:- mutable(tty_in, io.input_stream, io.stdin_stream, ground,
    [untrailed, attach_to_io_state]).
:- mutable(tty_out, io.output_stream, io.stdout_stream, ground,
    [untrailed, attach_to_io_state]).

:- mutable(saved_input_stream, io.input_stream, io.stdin_stream, ground,
    [untrailed, attach_to_io_state]).
:- mutable(saved_output_stream, io.output_stream, io.stdout_stream, ground,
    [untrailed, attach_to_io_state]).

    % This must be after the tty streams.
:- mutable(debugger_state, debugger_state, init_debugger_state, ground,
    [untrailed, attach_to_io_state]).

:- func init_debugger_state = debugger_state is det.

init_debugger_state = DebuggerState :-
    some [!IO] promise_pure (
        impure invent_io(!:IO),
        io.get_environment_var("SSDB", MaybeEnv, !IO),
        io.get_environment_var("SSDB_TTY", MaybeTTY, !IO),
        (
            ( MaybeEnv = yes(_)
            ; MaybeTTY = yes(_)
            )
        ->
            DebuggerState = debugger_on,
            (
                MaybeTTY = yes(FileName),
                io.open_input(FileName, InputRes, !IO),
                (
                    InputRes = ok(InputStream),
                    set_tty_in(InputStream, !IO)
                ;
                    InputRes = error(_)
                ),
                io.open_output(FileName, OutputRes, !IO),
                (
                    OutputRes = ok(OutputStream),
                    set_tty_out(OutputStream, !IO)
                ;
                    OutputRes = error(_)
                )
            ;
                MaybeTTY = no
            ),
            install_sigint_handler(!IO)
        ;
            DebuggerState = debugger_disabled
        ),
        impure consume_io(!.IO)
    ).

%-----------------------------------------------------------------------------%

:- pred install_sigint_handler(io::di, io::uo) is det.

install_sigint_handler(!IO).

:- pragma foreign_proc("C",
    install_sigint_handler(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    MR_setup_signal(SIGINT, (MR_Code *) MR_ssdb_sigint_handler,
        MR_FALSE, ""ssdb: cannot install SIGINT signal handler"");
    IO = IO0;
").

:- pragma foreign_code("C",
"
static void MR_ssdb_sigint_handler(void)
{
    SSDB_step_next_stop();
}
").

:- pragma foreign_proc("Java",
    install_sigint_handler(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    // This is an undocumented, unsupported and non-portable interface in the
    // Sun JVM but it seems there is no alternative.
    sun.misc.Signal.handle(new sun.misc.Signal(""INT""), new SigIntHandler());
    IO = IO0;
").

:- pragma foreign_code("Java",
"
public static class SigIntHandler implements sun.misc.SignalHandler {
    @Override
    public void handle(sun.misc.Signal sig) {
        SSDB_step_next_stop();
    }
}
").

:- pred step_next_stop(io::di, io::uo) is det.

:- pragma foreign_export("C", step_next_stop(di, uo),
    "SSDB_step_next_stop").
:- pragma foreign_export("Java", step_next_stop(di, uo),
    "SSDB_step_next_stop").

step_next_stop(!IO) :-
    set_cur_ssdb_next_stop(ns_step, !IO).

%----------------------------------------------------------------------------%

    % Call at call port. It writes out the event and calls
    % read_and_execute_cmd.
    %
handle_event_call(ProcId, ListVarValue) :-
    some [!IO]
    (
        impure invent_io(!:IO),
        get_debugger_state(DebuggerState, !IO),
        (
            DebuggerState = debugger_on,
            save_streams(!IO),

            Event = ssdb_call,
            get_ssdb_event_number_inc(EventNum, !IO),
            get_ssdb_csn_inc(CSN, !IO),
            get_ssdb_depth_inc(PrintDepth, !IO),

            % Push the new stack frame on top of the shadow stack.
            get_cur_ssdb_shadow_stack(ShadowStack0, !IO),
            StackFrame = elem(EventNum, CSN, PrintDepth, ProcId, ListVarValue),
            stack.push(ShadowStack0, StackFrame, ShadowStack),
            set_cur_ssdb_shadow_stack(ShadowStack, !IO),

            should_stop_at_this_event(Event, EventNum, CSN, ProcId, Stop,
                _AutoRetry, !IO),
            (
                Stop = yes,
                print_event_info(Event, EventNum, ProcId, PrintDepth, CSN,
                    !IO),
                read_and_execute_cmd(Event, ShadowStack, 0, WhatNext, !IO),
                what_next_stop(EventNum, CSN, WhatNext, _Retry, !IO)
            ;
                Stop = no
            ),

            restore_streams(!IO)
        ;
            DebuggerState = debugger_off
        ;
            DebuggerState = debugger_disabled
        ),
        impure consume_io(!.IO)
    ).

    % Call at call port of nondet procedure. It writes out the event and calls
    % read_and_execute_cmd.
    %
handle_event_call_nondet(ProcId, ListVarValue) :-
    some [!IO]
    (
        impure invent_io(!:IO),
        get_debugger_state(DebuggerState, !IO),
        (
            DebuggerState = debugger_on,
            save_streams(!IO),

            Event = ssdb_call_nondet,
            get_ssdb_event_number_inc(EventNum, !IO),
            get_ssdb_csn_inc(CSN, !IO),
            get_ssdb_depth_inc(PrintDepth, !IO),

            % Push the new stack frame on top of the shadow stack.
            StackFrame = elem(EventNum, CSN, PrintDepth, ProcId, ListVarValue),

            get_cur_ssdb_shadow_stack(ShadowStack0, !IO),
            stack.push(ShadowStack0, StackFrame, ShadowStack),
            set_cur_ssdb_shadow_stack(ShadowStack, !IO),

            get_cur_ssdb_shadow_stack_nondet(ShadowStackNonDet0, !IO),
            stack.push(ShadowStackNonDet0, StackFrame, ShadowStackNonDet),
            set_cur_ssdb_shadow_stack_nondet(ShadowStackNonDet, !IO),

            should_stop_at_this_event(Event, EventNum, CSN, ProcId, Stop,
                _AutoRetry, !IO),
            (
                Stop = yes,
                print_event_info(Event, EventNum, ProcId, PrintDepth, CSN,
                    !IO),
                read_and_execute_cmd(Event, ShadowStack, 0, WhatNext, !IO),
                what_next_stop(EventNum, CSN, WhatNext, _Retry, !IO)
            ;
                Stop = no
            ),

            restore_streams(!IO)
        ;
            DebuggerState = debugger_off
        ;
            DebuggerState = debugger_disabled
        ),
        impure consume_io(!.IO)
    ).

    % Call at exit port. Writes out the event and calls read_and_execute_cmd.
    %
handle_event_exit(ProcId, ListVarValue, Retry) :-
    some [!IO]
    (
        impure invent_io(!:IO),
        get_debugger_state(DebuggerState, !IO),
        (
            DebuggerState = debugger_on,
            save_streams(!IO),

            Event = ssdb_exit,
            get_ssdb_event_number_inc(EventNum, !IO),
            get_cur_ssdb_depth(PrintDepth, !IO),
            set_list_var_value_in_shadow_stack(ListVarValue, !IO),

            % Just get the top stack frame. It will be popped at the end of
            % handle_event. We need to leave the frame in place, e.g. for
            % printing variables at the exit port of the procedure.
            get_cur_ssdb_shadow_stack(ShadowStack0, !IO),
            stack.top_det(ShadowStack0, StackFrame),
            CSN = StackFrame ^ se_csn,

            should_stop_at_this_event(Event, EventNum, CSN, ProcId, Stop,
                AutoRetry, !IO),
            (
                Stop = yes,
                (
                    AutoRetry = do_retry,
                    EventNumF = StackFrame ^ se_event_number,
                    CSNF = StackFrame ^ se_csn,
                    set_cur_ssdb_event_number(EventNumF - 1, !IO),
                    set_cur_ssdb_csn(CSNF - 1, !IO),
                    WhatNext = wn_retry(CSN)
                ;
                    AutoRetry = do_not_retry,
                    print_event_info(Event, EventNum, ProcId, PrintDepth, CSN,
                        !IO),
                    read_and_execute_cmd(Event, ShadowStack0, 0, WhatNext, !IO)
                ),
                what_next_stop(EventNum, CSN, WhatNext, Retry, !IO)
            ;
                Stop = no,
                Retry = do_not_retry
            ),

            get_ssdb_depth_dec(_Depth, !IO),
            stack.pop_det(ShadowStack0, _StackFrame1, ShadowStack),
            set_cur_ssdb_shadow_stack(ShadowStack, !IO),

            restore_streams(!IO)
        ;
            ( DebuggerState = debugger_off
            ; DebuggerState = debugger_disabled
            ),
            Retry = do_not_retry
        ),
        impure consume_io(!.IO)
    ).

    % Call at exit port of nondet procedure only.
    %
handle_event_exit_nondet(ProcId, ListVarValue) :-
    some [!IO]
    (
        impure invent_io(!:IO),
        get_debugger_state(DebuggerState, !IO),
        (
            DebuggerState = debugger_on,
            save_streams(!IO),

            Event = ssdb_exit_nondet,
            get_ssdb_event_number_inc(EventNum, !IO),
            get_cur_ssdb_depth(PrintDepth, !IO),
            set_list_var_value_in_shadow_stack(ListVarValue, !IO),

            % Just get the top stack frame. It will be popped at the end of
            % handle_event. We need to leave the frame in place, e.g. for
            % printing variables at the exit port of the procedure.
            get_cur_ssdb_shadow_stack(ShadowStack0, !IO),
            stack.top_det(ShadowStack0, StackFrame),
            CSN = StackFrame ^ se_csn,

            should_stop_at_this_event(Event, EventNum, CSN, ProcId, Stop,
                AutoRetry, !IO),
            (
                Stop = yes,
                (
                    AutoRetry = do_retry,
                    WhatNext = wn_retry(CSN)
                ;
                    AutoRetry = do_not_retry,
                    print_event_info(Event, EventNum, ProcId, PrintDepth, CSN,
                        !IO),
                    read_and_execute_cmd(Event, ShadowStack0, 0, WhatNext, !IO)
                ),
                what_next_stop(EventNum, CSN, WhatNext, _Retry, !IO)
            ;
                Stop = no
            ),

            get_ssdb_depth_dec(_Depth, !IO),
            stack.pop_det(ShadowStack0, _StackFrame1, ShadowStack),
            set_cur_ssdb_shadow_stack(ShadowStack, !IO),

            restore_streams(!IO)
        ;
            ( DebuggerState = debugger_off
            ; DebuggerState = debugger_disabled
            )
        ),
        impure consume_io(!.IO)
    ).

    % Call at fail port. Writes out the event and calls read_and_execute_cmd.
    %
handle_event_fail(ProcId, _ListVarValue, Retry) :-
    some [!IO]
    (
        impure invent_io(!:IO),
        get_debugger_state(DebuggerState, !IO),
        (
            DebuggerState = debugger_on,
            save_streams(!IO),

            Event = ssdb_fail,
            get_ssdb_event_number_inc(EventNum, !IO),
            get_cur_ssdb_depth(PrintDepth, !IO),
            get_cur_ssdb_shadow_stack(ShadowStack0, !IO),
            stack.top_det(ShadowStack0, StackFrame),
            CSN = StackFrame ^ se_csn,

            should_stop_at_this_event(Event, EventNum, CSN, ProcId, Stop,
                AutoRetry, !IO),
            (
                Stop = yes,
                (
                    AutoRetry = do_retry,
                    EventNumF = StackFrame ^ se_event_number,
                    CSNF = StackFrame ^ se_csn,
                    set_cur_ssdb_event_number(EventNumF - 1, !IO),
                    set_cur_ssdb_csn(CSNF - 1, !IO),
                    WhatNext = wn_retry(CSN)
                ;
                    AutoRetry = do_not_retry,
                    print_event_info(Event, EventNum, ProcId, PrintDepth, CSN,
                        !IO),
                    read_and_execute_cmd(Event, ShadowStack0, 0, WhatNext, !IO)
                ),
                what_next_stop(EventNum, CSN, WhatNext, Retry, !IO)
            ;
                Stop = no,
                Retry = do_not_retry
            ),

            get_ssdb_depth_dec(_Depth, !IO),
            stack.pop_det(ShadowStack0, _StackFrame1, ShadowStack),
            set_cur_ssdb_shadow_stack(ShadowStack, !IO),

            restore_streams(!IO)
        ;
            ( DebuggerState = debugger_off
            ; DebuggerState = debugger_disabled
            ),
            Retry = do_not_retry
        ),
        impure consume_io(!.IO)
    ).

    % Call at fail port of nondet procedure only.
    %
handle_event_fail_nondet(ProcId, _ListVarValue, Retry) :-
    some [!IO]
    (
        impure invent_io(!:IO),
        get_debugger_state(DebuggerState, !IO),
        Event = ssdb_fail_nondet,
        (
            DebuggerState = debugger_on,
            save_streams(!IO),

            get_ssdb_event_number_inc(EventNum, !IO),
            get_cur_ssdb_shadow_stack(ShadowStack0, !IO),
            stack.top_det(ShadowStack0, StackFrame),
            CSN = StackFrame ^ se_csn,
            get_cur_ssdb_depth(PrintDepth, !IO),
            get_cur_ssdb_shadow_stack_nondet(ShadowStackNonDet0, !IO),

            should_stop_at_this_event(Event, EventNum, CSN, ProcId, Stop,
                AutoRetry, !IO),
            (
                Stop = yes,
                (
                    AutoRetry = do_retry,
                    get_frame_at_depth_nondet(ProcId, PrintDepth,
                        MaybeStackFrameFound, !IO),
                    (
                        MaybeStackFrameFound = yes(StackFrameFound),
                        EventNumF = StackFrameFound ^ se_event_number,
                        CSNF = StackFrameFound ^ se_csn,
                        set_cur_ssdb_event_number(EventNumF - 1, !IO),
                        set_cur_ssdb_csn(CSNF - 1, !IO)
                    ;
                        MaybeStackFrameFound = no,
                        error("Unexpected error: ssdb/ssdb.m " ++
                            "get_frame_at_depth_nondet failed")
                    ),
                    WhatNext = wn_retry(CSN)
                ;
                    AutoRetry = do_not_retry,
                    print_event_info(Event, EventNum, ProcId, PrintDepth, CSN,
                        !IO),
                    read_and_execute_cmd(Event, ShadowStack0, 0, WhatNext, !IO)
                ),
                what_next_stop(EventNum, CSN, WhatNext, Retry, !IO)
            ;
                Stop = no,
                Retry = do_not_retry
            ),

            get_ssdb_depth_dec(_Depth, !IO),
            stack.pop_det(ShadowStack0, _StackFrame, ShadowStack),
            stack.pop_det(ShadowStackNonDet0, _StackFrameNonDet,
                ShadowStackNonDet),
            set_cur_ssdb_shadow_stack(ShadowStack, !IO),
            set_cur_ssdb_shadow_stack_nondet(ShadowStackNonDet, !IO),

            restore_streams(!IO)
        ;
            DebuggerState = debugger_off,
            get_cur_ssdb_depth(Depth, !IO),
            % If this is the required frame, then make the debugger stop
            % at the next event; otherwise continue.
            get_frame_at_depth_nondet(ProcId, Depth + 1, MaybeStackFrame, !IO),
            (
                MaybeStackFrame = yes(_StackFrame),
                set_debugger_state(debugger_on, !IO),
                Retry = do_retry
            ;
                MaybeStackFrame = no,
                Retry = do_not_retry
            )
        ;
            DebuggerState = debugger_disabled,
            Retry = do_not_retry
        ),
        impure consume_io(!.IO)
    ).

    % Call at redo port in nondet procedure. Writes out the event and calls
    % read_and_execute_cmd.
    %
handle_event_redo_nondet(ProcId, _ListVarValue) :-
    some [!IO]
    (
        impure invent_io(!:IO),
        get_debugger_state(DebuggerState, !IO),
        (
            DebuggerState = debugger_on,
            save_streams(!IO),

            Event = ssdb_redo_nondet,
            get_ssdb_event_number_inc(EventNum, !IO),
            get_ssdb_depth_inc(PrintDepth, !IO),

            get_frame_at_depth_nondet(ProcId, PrintDepth, MaybeStackFrame,
                !IO),
            (
                MaybeStackFrame = yes(StackFrame),
                get_cur_ssdb_shadow_stack(ShadowStack0, !IO),
                stack.push(ShadowStack0, StackFrame, ShadowStack),
                set_cur_ssdb_shadow_stack(ShadowStack, !IO),
                CSN = StackFrame ^ se_csn,

                should_stop_at_this_event(Event, EventNum, CSN, ProcId,
                    Stop, _AutoRetry, !IO),
                (
                    Stop = yes,
                    print_event_info(Event, EventNum, ProcId, PrintDepth, CSN,
                        !IO),
                    read_and_execute_cmd(Event, ShadowStack, 0, WhatNext, !IO),
                    what_next_stop(EventNum, CSN, WhatNext, _Retry, !IO)
                ;
                    Stop = no
                )
            ;
                MaybeStackFrame = no,
                error("Unexpected error: ssdb/ssdb.m : " ++
                    "get_frame_at_depth_nondet failed")
            ),

            restore_streams(!IO)
        ;
            ( DebuggerState = debugger_off
            ; DebuggerState = debugger_disabled
            )
        ),
        impure consume_io(!.IO)
    ).

%----------------------------------------------------------------------------%

    % IsSame is 'yes' iff the two call sequence numbers are equal,
    % 'no' otherwise.
    %
:- pred is_same_int(int::in, int::in, bool::out) is det.

is_same_int(IntA, IntB, IsSame) :-
    IsSame = (IntA = IntB -> yes ; no).

    % Increment the CSN and return the new value.
    %
:- pred get_ssdb_csn_inc(int::out, io::di, io::uo) is det.

get_ssdb_csn_inc(CSN, !IO) :-
    get_cur_ssdb_csn(CSN0, !IO),
    CSN = CSN0 + 1,
    set_cur_ssdb_csn(CSN, !IO).

    % Increment the Event Number and return the new value.
    %
:- pred get_ssdb_event_number_inc(int::out, io::di, io::uo) is det.

get_ssdb_event_number_inc(EventNum, !IO) :-
    get_cur_ssdb_event_number(EventNum0, !IO),
    EventNum = EventNum0 + 1,
    set_cur_ssdb_event_number(EventNum, !IO).

    % Increment the depth and return the new value.
    %
:- pred get_ssdb_depth_inc(int::out, io::di, io::uo) is det.

get_ssdb_depth_inc(Depth, !IO) :-
    get_cur_ssdb_shadow_stack(ShadowStack, !IO),
    Depth0 = stack.depth(ShadowStack),
    Depth = Depth0 + 1,
    set_cur_ssdb_depth(Depth, !IO).

    % Decrement the depth and return the new value.
    %
:- pred get_ssdb_depth_dec(int::out, io::di, io::uo) is det.

get_ssdb_depth_dec(Depth, !IO) :-
    get_cur_ssdb_shadow_stack(ShadowStack, !IO),
    Depth0 = stack.depth(ShadowStack),
    Depth = Depth0 - 1,
    set_cur_ssdb_depth(Depth, !IO).

    % Setter of the se_list_var_value in the first stack_elem.
    %
:- pred set_list_var_value_in_shadow_stack(list(var_value)::in,
    io::di, io::uo) is det.

set_list_var_value_in_shadow_stack(ListVarValue, !IO) :-
    get_cur_ssdb_shadow_stack(ShadowStack0, !IO),
    stack.pop_det(ShadowStack0, StackFrame0, PopedStack),
    StackFrame = StackFrame0 ^ se_list_var_value := ListVarValue,
    stack.push(PopedStack, StackFrame, ShadowStack),
    set_cur_ssdb_shadow_stack(ShadowStack, !IO).

    % should_stop_at_the_event(Event, CSN, EventNum, ProcId, Stop, AutoRetry).
    %
    % Figure out whether we should stop execution and start user interaction.
    %
:- pred should_stop_at_this_event(ssdb_event_type::in, int::in, int::in,
    ssdb_proc_id::in, bool::out, ssdb_retry::out, io::di, io::uo) is det.

should_stop_at_this_event(Event, EventNum, CSN, ProcId, ShouldStopAtEvent,
        AutoRetry, !IO) :-
    get_cur_ssdb_next_stop(NextStop, !IO),
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
        get_cur_ssdb_breakpoints(BreakPoints, !IO),
        (
            map.search(BreakPoints,
                pair(ProcId ^ module_name, ProcId ^ proc_name), BreakPoint)
        ->
            BreakPointState = BreakPoint ^ bp_state,
            (
                BreakPointState = bp_state_enabled,
                ShouldStopAtEvent = yes
            ;
                BreakPointState = bp_state_disabled,
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

    % what_next_stop(EventNum, CSN, WhatNext, Retry).
    %
    % Set the NextStop and the Retry variable according to the WhatNext value.
    % In the case where the WathNext is set for a retry, it modify the
    % debugger_state at his old value which it had at the call point.
    %
:- pred what_next_stop(int::in, int::in, what_next::in, ssdb_retry::out,
    io::di, io::uo) is det.

what_next_stop(EventNum, CSN, WhatNext, Retry, !IO) :-
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
        ( RetryCSN = CSN ->
            NextStop = ns_step,
            Retry = do_retry
        ;
            NextStop = ns_final_port(RetryCSN, do_retry),
            Retry = do_not_retry
        )
     ;
         WhatNext = wn_retry_nondet(RetryCSN),
         NextStop = ns_final_port_nondet(RetryCSN, do_retry),
         Retry = do_not_retry
    ;
        WhatNext = wn_goto(EventNumToGo),
        ( EventNum = EventNumToGo ->
            NextStop = ns_step,
            Retry = do_not_retry
        ;
            NextStop = ns_goto(EventNumToGo),
            Retry = do_not_retry
        )
    ),
    set_cur_ssdb_next_stop(NextStop, !IO).

    % Look up the procedure at the specified depth in the nondet shadow stack.
    %
:- pred get_frame_at_depth_nondet(ssdb_proc_id::in, int::in,
    maybe(stack_elem)::out, io::di, io::uo) is det.

get_frame_at_depth_nondet(ProcId, Depth, StackFrame, !IO) :-
    get_cur_ssdb_shadow_stack_nondet(ShadowStackNonDet, !IO),
    get_frame_at_depth_nondet_2(ProcId, Depth, ShadowStackNonDet, StackFrame).

:- pred get_frame_at_depth_nondet_2(ssdb_proc_id::in, int::in,
    stack(stack_elem)::in, maybe(stack_elem)::out) is det.

get_frame_at_depth_nondet_2(ProcId, Depth, ShadowStackNonDet0,
        MaybeStackFrame) :-
    ( stack.is_empty(ShadowStackNonDet0) ->
        MaybeStackFrame = no
    ;
        stack.pop_det(ShadowStackNonDet0, Frame, ShadowStackNonDet),
        (
            Frame ^ se_proc_id ^ module_name = ProcId ^ module_name,
            Frame ^ se_proc_id ^ proc_name = ProcId ^ proc_name,
            Frame ^ se_depth = Depth
        ->
            MaybeStackFrame = yes(Frame)
        ;
            get_frame_at_depth_nondet_2(ProcId, Depth, ShadowStackNonDet,
                MaybeStackFrame)
        )
    ).

%----------------------------------------------------------------------------%

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

:- type ssdb_cmd
    --->    ssdb_help

    ;       ssdb_step
    ;       ssdb_next
    ;       ssdb_goto
    ;       ssdb_continue
    ;       ssdb_finish

    ;       ssdb_retry

    ;       ssdb_stack
    ;       ssdb_print
    ;       ssdb_browse
    ;       ssdb_vars
    ;       ssdb_down
    ;       ssdb_up

    ;       ssdb_break
    ;       ssdb_enable
    ;       ssdb_disable
    ;       ssdb_delete

    ;       ssdb_quit.

:- pred ssdb_cmd_name(string::in, ssdb_cmd::out) is semidet.

ssdb_cmd_name("h",          ssdb_help).
ssdb_cmd_name("help",       ssdb_help).

ssdb_cmd_name("s",          ssdb_step).
ssdb_cmd_name("step",       ssdb_step).
ssdb_cmd_name("n",          ssdb_next).
ssdb_cmd_name("next",       ssdb_next).
ssdb_cmd_name("g",          ssdb_goto).
ssdb_cmd_name("goto",       ssdb_goto).
ssdb_cmd_name("c",          ssdb_continue).
ssdb_cmd_name("continue",   ssdb_continue).
ssdb_cmd_name("f",          ssdb_finish).
ssdb_cmd_name("finish",     ssdb_finish).

ssdb_cmd_name("r",          ssdb_retry).
ssdb_cmd_name("retry",      ssdb_retry).

ssdb_cmd_name("st",         ssdb_stack).
ssdb_cmd_name("stack",      ssdb_stack).
ssdb_cmd_name("p",          ssdb_print).
ssdb_cmd_name("print",      ssdb_print).
ssdb_cmd_name("browse",     ssdb_browse).
ssdb_cmd_name("vars",       ssdb_vars).
ssdb_cmd_name("v",          ssdb_vars).
ssdb_cmd_name("d",          ssdb_down).
ssdb_cmd_name("down",       ssdb_down).
ssdb_cmd_name("u",          ssdb_up).
ssdb_cmd_name("up",         ssdb_up).

ssdb_cmd_name("b",          ssdb_break).
ssdb_cmd_name("break",      ssdb_break).
ssdb_cmd_name("enable",     ssdb_enable).
ssdb_cmd_name("disable",    ssdb_disable).
ssdb_cmd_name("delete",     ssdb_delete).

ssdb_cmd_name("q",          ssdb_quit).
ssdb_cmd_name("quit",       ssdb_quit).

% Useful commands:
%   level N         set level
%   print           print the current atom

%---------------------------------------------------------------------------%

    % Display the prompt, read a user command, and execute it.
    %
:- pred read_and_execute_cmd(ssdb_event_type::in, stack(stack_elem)::in,
    int::in, what_next::out, io::di, io::uo) is det.

read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO) :-
    io.write_string("ssdb> ", !IO),
    io.flush_output(!IO),
    % Read a string in input and return a string.
    io.read_line_as_string(Result, !IO),
    (
        Result = ok(String0),
        % Delete the trailing newline character.
        String = string.chomp(String0),
        Words = string.words(String),
        (
            Words = [],
            % We execute the default command. Alternatively, we could just do
            % nothing, and call read_and_execute_cmd recursively.
            execute_cmd(ssdb_step, [], Event, ShadowStack, Depth, WhatNext,
                !IO)
        ;
            Words = [CmdWord | ArgWords],
            % Implementing aliases would require only looking up an alias map
            % here.
            ( ssdb_cmd_name(CmdWord, Cmd) ->
                execute_cmd(Cmd, ArgWords, Event, ShadowStack, Depth, WhatNext,
                    !IO)
            ;
                io.format("%s: unknown command (try \"help\")\n", [s(CmdWord)],
                    !IO),
                read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
            )
        )
    ;
        Result = eof,
        execute_cmd(ssdb_quit, [], Event, ShadowStack, Depth, WhatNext, !IO)
    ;
        Result = error(Error),
        io.error_message(Error, Msg),
        io.format("could not read command: %s\n", [s(Msg)], !IO),
        execute_cmd(ssdb_quit, [], Event, ShadowStack, Depth, WhatNext, !IO)
    ).

    % Execute a command.
    %
:- pred execute_cmd(ssdb_cmd::in, list(string)::in, ssdb_event_type::in,
    stack(stack_elem)::in, int::in, what_next::out, io::di, io::uo) is det.

execute_cmd(Cmd, Args, Event, ShadowStack, Depth, WhatNext, !IO) :-
    (
        Cmd = ssdb_help,
        execute_ssdb_help(Args, Event, ShadowStack, Depth, WhatNext, !IO)
    ;
        Cmd = ssdb_step,
        execute_ssdb_step(Args, Event, ShadowStack, Depth, WhatNext, !IO)
    ;
        Cmd = ssdb_next,
        execute_ssdb_next(Args, Event, ShadowStack, Depth, WhatNext, !IO)
    ;
        Cmd = ssdb_goto,
        execute_ssdb_goto(Args, Event, ShadowStack, Depth, WhatNext, !IO)
    ;
        Cmd = ssdb_continue,
        execute_ssdb_continue(Args, Event, ShadowStack, Depth, WhatNext, !IO)
    ;
        Cmd = ssdb_finish,
        execute_ssdb_finish(Args, Event, ShadowStack, Depth, WhatNext, !IO)
    ;
        Cmd = ssdb_retry,
        execute_ssdb_retry(Args, Event, ShadowStack, Depth, WhatNext, !IO)
    ;
        Cmd = ssdb_stack,
        execute_ssdb_stack(Args, Event, ShadowStack, Depth, WhatNext, !IO)
    ;
        Cmd = ssdb_print,
        execute_ssdb_print(Args, Event, ShadowStack, Depth, WhatNext, !IO)
    ;
        Cmd = ssdb_browse,
        execute_ssdb_browse(Args, Event, ShadowStack, Depth, WhatNext, !IO)
    ;
        Cmd = ssdb_vars,
        execute_ssdb_vars(Args, Event, ShadowStack, Depth, WhatNext, !IO)
    ;
        Cmd = ssdb_down,
        execute_ssdb_down(Args, Event, ShadowStack, Depth, WhatNext, !IO)
    ;
        Cmd = ssdb_up,
        execute_ssdb_up(Args, Event, ShadowStack, Depth, WhatNext, !IO)
    ;
        Cmd = ssdb_break,
        execute_ssdb_break(Args, Event, ShadowStack, Depth, WhatNext, !IO)
    ;
        Cmd = ssdb_enable,
        execute_ssdb_enable(Args, Event, ShadowStack, Depth, WhatNext, !IO)
    ;
        Cmd = ssdb_disable,
        execute_ssdb_disable(Args, Event, ShadowStack, Depth, WhatNext, !IO)
    ;
        Cmd = ssdb_delete,
        execute_ssdb_delete(Args, Event, ShadowStack, Depth, WhatNext, !IO)
    ;
        Cmd = ssdb_quit,
        execute_ssdb_quit(Args, Event, ShadowStack, Depth, WhatNext, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred execute_ssdb_help(list(string)::in, ssdb_event_type::in,
    stack(stack_elem)::in, int::in, what_next::out, io::di, io::uo) is det.

execute_ssdb_help(Args, Event, ShadowStack, Depth, WhatNext, !IO) :-
    (
        Args = [],
        print_help(!IO)
    ;
        Args = [_ | _],
        % We should provide more detailed help if the user specifies a command
        % name.
        print_help(!IO)
    ),
    read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO).

:- pred execute_ssdb_step(list(string)::in, ssdb_event_type::in,
    stack(stack_elem)::in, int::in, what_next::out, io::di, io::uo) is det.

execute_ssdb_step(Args, Event, ShadowStack, Depth, WhatNext, !IO) :-
    (
        Args = [],
        WhatNext = wn_step
    ;
        Args = [_ | _],
        % We should provide more detailed help.
        print_help(!IO),
        read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
    ).

:- pred execute_ssdb_next(list(string)::in, ssdb_event_type::in,
    stack(stack_elem)::in, int::in, what_next::out, io::di, io::uo) is det.

execute_ssdb_next(Args, Event, ShadowStack, Depth, WhatNext, !IO) :-
    (
        Args = [],
        (
            ( Event = ssdb_call
            ; Event = ssdb_call_nondet
            ; Event = ssdb_redo_nondet
            )
        ->
            WhatNext = wn_next
        ;
            io.write_string("The `next' command can be executed "
                ++ "only at a call or redo port.\n", !IO),
            read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
        )
    ;
        Args = [_ | _],
        % We should provide more detailed help.
        print_help(!IO),
        read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
    ).

:- pred execute_ssdb_goto(list(string)::in, ssdb_event_type::in,
    stack(stack_elem)::in, int::in, what_next::out, io::di, io::uo) is det.

execute_ssdb_goto(Args, Event, ShadowStack, Depth, WhatNext, !IO) :-
    (
        Args = [],
        % We should provide more detailed help.
        print_help(!IO),
        read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
    ;
        Args = [EventNumToGoStr],
        ( string.to_int(EventNumToGoStr, EventNumToGo) ->
            get_cur_ssdb_event_number(CurEventNum, !IO),
            ( EventNumToGo > CurEventNum ->
                WhatNext = wn_goto(EventNumToGo)
            ;
                io.write_string("The debugger cannot go to a past event.\n",
                    !IO),
                read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
            )
        ;
            io.write_string("The event number to go to must be an integer.\n",
                !IO),
            read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
        )
    ;
        Args = [_, _ | _],
        % We should provide more detailed help.
        print_help(!IO),
        read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
    ).

:- pred execute_ssdb_continue(list(string)::in, ssdb_event_type::in,
    stack(stack_elem)::in, int::in, what_next::out, io::di, io::uo) is det.

execute_ssdb_continue(Args, Event, ShadowStack, Depth, WhatNext, !IO) :-
    (
        Args = [],
        WhatNext = wn_continue
    ;
        Args = [_ | _],
        % We should provide more detailed help.
        print_help(!IO),
        read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
    ).

:- pred execute_ssdb_finish(list(string)::in, ssdb_event_type::in,
    stack(stack_elem)::in, int::in, what_next::out, io::di, io::uo) is det.

execute_ssdb_finish(Args, Event, ShadowStack, Depth, WhatNext, !IO) :-
    (
        Args = [],
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
            io.write_string("The `finish' command can be executed "
                ++ "only at a call or redo port.\n", !IO),
            read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
        )
    ;
        Args = [Arg],
        ( string.to_int(Arg, Num) ->
            get_cur_ssdb_depth(CurDepth, !IO),
            (
                Num >= 0,
                Num =< CurDepth - 1
            ->
                get_correct_frame_with_num(Num, ShadowStack, StackFrame),
                CSN = StackFrame ^ se_csn,
                WhatNext = wn_finish(CSN)
            ;
                io.format("The depth must be between 1 and %i.\n",
                    [i(CurDepth)], !IO),
                read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
            )
        ;
            io.write_string("The depth must be an integer.\n", !IO),
            read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
        )
    ;
        Args = [_, _ | _],
        % We should provide more detailed help.
        print_help(!IO),
        read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
    ).

:- pred execute_ssdb_retry(list(string)::in, ssdb_event_type::in,
    stack(stack_elem)::in, int::in, what_next::out, io::di, io::uo) is det.

execute_ssdb_retry(Args, Event, ShadowStack, Depth, WhatNext, !IO) :-
    % XXX: The cases for empty argument list and an argument list consisting of
    % one number look to be the result of cut-and-paste programming: we should
    % instead use common code parameterised in the appropriate places.
    %
    % XXX: For some reason, the original code here handled the case of the
    % number argument being zero as if the command had no argument at all.
    (
        Args = [],
        (
            ( Event = ssdb_exit
            ; Event = ssdb_fail
            ; Event = ssdb_fail_nondet
            ),
            stack.top_det(ShadowStack, FrameStack),
            EventNum = FrameStack ^ se_event_number,
            CSN = FrameStack ^ se_csn,
            set_cur_ssdb_event_number(EventNum - 1, !IO),
            set_cur_ssdb_csn(CSN - 1, !IO),
            WhatNext = wn_retry(CSN)
        ;
            Event = ssdb_exit_nondet,
            stack.top_det(ShadowStack, FrameStack),
            EventNum = FrameStack ^ se_event_number,
            CSN = FrameStack ^ se_csn,
            set_debugger_state(debugger_off, !IO),
            % Set the event number to CSN - 1 because it will be incremented
            % at the next event. So, we need to set it to the number of the
            % event just *before* the call to the retried procedure.
            set_cur_ssdb_event_number(EventNum - 1, !IO),
            set_cur_ssdb_csn(CSN - 1, !IO),
            WhatNext = wn_retry(CSN)
        ;
            ( Event = ssdb_call
            ; Event = ssdb_call_nondet
            ; Event = ssdb_redo_nondet
            ),
            io.write_string("Cannot execute retry " ++
                "at a call or redo port.\n", !IO),
            read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
        )
    ;
        Args = [Arg],
        get_cur_ssdb_depth(CurDepth, !IO),
        ( string.to_int(Arg, Num) ->
            (
                Num = 0
            ->
                execute_ssdb_retry([], Event, ShadowStack, Depth, WhatNext,
                    !IO)
            ;
                Num >= 0,
                Num =< CurDepth - 1
            ->
                (
                    ( Event = ssdb_exit
                    ; Event = ssdb_fail
                    ; Event = ssdb_fail_nondet
                    ),
                    get_correct_frame_with_num(Num, ShadowStack, FrameStack),
                    EventNum = FrameStack ^ se_event_number,
                    CSN = FrameStack ^ se_csn,
                    set_cur_ssdb_event_number(EventNum - 1, !IO),
                    set_cur_ssdb_csn(CSN - 1, !IO),
                    WhatNext = wn_retry(CSN)
                ;
                    Event = ssdb_exit_nondet,
                    get_correct_frame_with_num(Num, ShadowStack, FrameStack),
                    CSN = FrameStack ^ se_csn,
                    % Set the event number and the CSN minus 1 because
                    % it will be increment at the next event. So, we
                    % need to be at the event just before the call.
                    get_cur_ssdb_shadow_stack_nondet(ShadowStackNonDet, !IO),
                    ( csn_is_in_stack(CSN, ShadowStackNonDet) ->
                        set_debugger_state(debugger_off, !IO),
                        WhatNext = wn_retry_nondet(CSN)
                    ;
                        WhatNext = wn_retry(CSN)
                    )
                ;
                    ( Event = ssdb_call
                    ; Event = ssdb_call_nondet
                    ; Event = ssdb_redo_nondet
                    ),
                    io.write_string("Cannot execute retry " ++
                        "at a call or redo port.\n", !IO),
                    read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext,
                        !IO)
                )
            ;
                io.format("The depth must be between 1 and %i.\n",
                    [i(CurDepth)], !IO),
                read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
            )
        ;
            io.write_string("The depth must be an integer.\n", !IO),
            read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
        )
    ;
        Args = [_, _ | _],
        % We should provide more detailed help.
        print_help(!IO),
        read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
    ).

:- pred execute_ssdb_stack(list(string)::in, ssdb_event_type::in,
    stack(stack_elem)::in, int::in, what_next::out, io::di, io::uo) is det.

execute_ssdb_stack(Args, Event, ShadowStack, Depth, WhatNext, !IO) :-
    (
        Args = [],
        print_frames_list(0, ShadowStack, Depth, !IO),
        read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
    ;
        Args = [_ | _],
        % We should provide more detailed help.
        print_help(!IO),
        read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
    ).

:- pred execute_ssdb_print(list(string)::in, ssdb_event_type::in,
    stack(stack_elem)::in, int::in, what_next::out, io::di, io::uo) is det.

execute_ssdb_print(Args, Event, ShadowStack, Depth, WhatNext, !IO) :-
    (
        Args = [],
        get_correct_frame_with_num(Depth, ShadowStack, CurrentFrame),
        ListVarValue = CurrentFrame ^ se_list_var_value,
        print_vars(ListVarValue, !IO)
    ;
        Args = [Arg],
        get_correct_frame_with_num(Depth, ShadowStack, CurrentFrame),
        ListVarValue = CurrentFrame ^ se_list_var_value,
        print_var_with_name(ListVarValue, Arg, !IO)
    ;
        Args = [_, _ | _],
        print_help(!IO)
    ),
    read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO).

:- pred execute_ssdb_browse(list(string)::in, ssdb_event_type::in,
    stack(stack_elem)::in, int::in, what_next::out, io::di, io::uo) is det.

execute_ssdb_browse(Args, Event, ShadowStack, Depth, WhatNext, !IO) :-
    (
        Args = [VarName],
        get_correct_frame_with_num(0, ShadowStack, CurFrame),
        ListVarValue = CurFrame ^ se_list_var_value,
        list_var_value_to_assoc_list(ListVarValue, VarDescs),
        browse_var(VarDescs, VarName, !IO)
    ;
        ( Args = []
        ; Args = [_, _ | _]
        ),
        % We should provide more detailed help.
        print_help(!IO)
    ),
    read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO).

:- pred execute_ssdb_vars(list(string)::in, ssdb_event_type::in,
    stack(stack_elem)::in, int::in, what_next::out, io::di, io::uo) is det.

execute_ssdb_vars(Args, Event, ShadowStack, Depth, WhatNext, !IO) :-
    (
        Args = [],
        get_correct_frame_with_num(Depth, ShadowStack, CurrentFrame),
        ListVarValue = CurrentFrame ^ se_list_var_value,
        print_vars_list(ListVarValue, 1, !IO)
    ;
        Args = [_ | _],
        % We should provide more detailed help.
        print_help(!IO)
    ),
    read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO).

:- pred execute_ssdb_down(list(string)::in, ssdb_event_type::in,
    stack(stack_elem)::in, int::in, what_next::out, io::di, io::uo) is det.

execute_ssdb_down(Args, Event, ShadowStack, Depth, WhatNext, !IO) :-
    (
        Args = [],
        (
            DownDepth = Depth - 1,
            DownDepth >= 0
        ->
            get_correct_frame_with_num(DownDepth, ShadowStack, FrameToPrint),
            stack.depth(ShadowStack, StackDepth),
            print_frame_info(FrameToPrint, StackDepth, !IO),
            read_and_execute_cmd(Event, ShadowStack, DownDepth, WhatNext, !IO)
        ;
            io.write_string("Already at bottom stack frame.\n", !IO),
            read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
        )
    ;
        Args = [_ | _],
        % We should provide more detailed help.
        print_help(!IO),
        read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
    ).

:- pred execute_ssdb_up(list(string)::in, ssdb_event_type::in,
    stack(stack_elem)::in, int::in, what_next::out, io::di, io::uo) is det.

execute_ssdb_up(Args, Event, ShadowStack, Depth, WhatNext, !IO) :-
    (
        Args = [],
        (
            UpDepth = Depth + 1,
            UpDepth < stack.depth(ShadowStack)
        ->
            get_correct_frame_with_num(UpDepth, ShadowStack, FrameToPrint),
            stack.depth(ShadowStack, StackDepth),
            print_frame_info(FrameToPrint, StackDepth, !IO),
            read_and_execute_cmd(Event, ShadowStack, UpDepth, WhatNext, !IO)
        ;
            io.write_string("Already at top stack frame.\n", !IO),
            read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
        )
    ;
        Args = [_ | _],
        % We should provide more detailed help.
        print_help(!IO),
        read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
    ).

:- pred execute_ssdb_break(list(string)::in, ssdb_event_type::in,
    stack(stack_elem)::in, int::in, what_next::out, io::di, io::uo) is det.

execute_ssdb_break(Args, Event, ShadowStack, Depth, WhatNext, !IO) :-
    (
        Args = [],
        % We should provide more detailed help.
        print_help(!IO),
        read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
    ;
        Args = [Arg],
        ( Arg = "info" ->
            get_cur_ssdb_breakpoints(BreakPoints, !IO),
            BreakPointsListValue = map.values(BreakPoints),
            print_breakpoints(BreakPointsListValue, !IO),
            read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
        ;
            % We should provide more detailed help.
            print_help(!IO),
            read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
        )
    ;
        Args = [ModuleName, ProcedureName],
        get_cur_ssdb_breakpoints(BreakPoints0, !IO),
        Key = pair(ModuleName, ProcedureName),
        ( map.contains(BreakPoints0, Key) ->
            io.write_string("The new breakpoint already exist\n", !IO),
            read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
        ;
            get_cur_ssdb_number_of_breakpoint(Number, !IO),
            NewBreakPoint = breakpoint(Number + 1, ModuleName,
                ProcedureName, bp_state_enabled),
            map.det_insert(BreakPoints0, Key, NewBreakPoint, BreakPoints),
            BreakPointsListValue = map.values(BreakPoints),
            print_breakpoints(BreakPointsListValue, !IO),
            set_cur_ssdb_breakpoints(BreakPoints, !IO),
            set_cur_ssdb_number_of_breakpoint(Number + 1, !IO),
            read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
        )
    ;
        Args = [_, _, _ | _],
        % We should provide more detailed help.
        print_help(!IO),
        read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
    ).

:- pred execute_ssdb_enable(list(string)::in, ssdb_event_type::in,
    stack(stack_elem)::in, int::in, what_next::out, io::di, io::uo) is det.

execute_ssdb_enable(Args, Event, ShadowStack, Depth, WhatNext, !IO) :-
    (
        Args = [],
        % We should provide more detailed help.
        print_help(!IO),
        read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
    ;
        Args = [Arg],
        ( Arg = "*" ->
            modify_state_breakpoints(bp_state_enabled, !IO),
            read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
        ; string.to_int(Arg, Num) ->
            modify_state_breakpoint_with_num(bp_state_enabled, Num, !IO),
            read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
        ;
            % We should provide more detailed help.
            print_help(!IO),
            read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
        )
    ;
        Args = [_, _ | _],
        % We should provide more detailed help.
        print_help(!IO),
        read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
    ).

:- pred execute_ssdb_disable(list(string)::in, ssdb_event_type::in,
    stack(stack_elem)::in, int::in, what_next::out, io::di, io::uo) is det.

execute_ssdb_disable(Args, Event, ShadowStack, Depth, WhatNext, !IO) :-
    (
        Args = [],
        % We should provide more detailed help.
        print_help(!IO),
        read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
    ;
        Args = [Arg],
        ( Arg = "*" ->
            modify_state_breakpoints(bp_state_disabled, !IO),
            read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
        ; string.to_int(Arg, Num) ->
            modify_state_breakpoint_with_num(bp_state_disabled, Num, !IO),
            read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
        ;
            io.write_string("The number must be an integer\n", !IO),
            % We should provide more detailed help.
            print_help(!IO),
            read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
        )
    ;
        Args = [_, _ | _],
        % We should provide more detailed help.
        print_help(!IO),
        read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
    ).

:- pred execute_ssdb_delete(list(string)::in, ssdb_event_type::in,
    stack(stack_elem)::in, int::in, what_next::out, io::di, io::uo) is det.

execute_ssdb_delete(Args, Event, ShadowStack, Depth, WhatNext, !IO) :-
    (
        Args = [],
        % We should provide more detailed help.
        print_help(!IO),
        read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
    ;
        Args = [Arg],
        ( Arg = "*" ->
            BreakPoints = map.init,
            set_cur_ssdb_breakpoints(BreakPoints, !IO),
            io.write_string("All breakpoints have been deleted.\n", !IO),
            read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
        ; string.to_int(Arg, Num) ->
            delete_breakpoint_with_num(Num, !IO),
            read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
        ;
            io.write_string("The number must be an integer\n", !IO),
            read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
        )
    ;
        Args = [_, _ | _],
        % We should provide more detailed help.
        print_help(!IO),
        read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
    ).

:- pred execute_ssdb_quit(list(string)::in, ssdb_event_type::in,
    stack(stack_elem)::in, int::in, what_next::out, io::di, io::uo) is det.

execute_ssdb_quit(Args, Event, ShadowStack, Depth, WhatNext, !IO) :-
    (
        Args = [],
        io.write_string("ssdb: are you sure you want to quit? ", !IO),
        io.flush_output(!IO),
        % Read a string in input and return a string.
        io.read_line_as_string(Result, !IO),
        (
            Result = ok(String),
            (
                ( string.prefix(String, "y")
                ; string.prefix(String, "Y")
                )
            ->
                exit_debugger(!IO),
                WhatNext = wn_step
            ;
                read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
            )
        ;
            Result = eof,
            exit_debugger(!IO),
            WhatNext = wn_step
        ;
            Result = error(_Error),
            exit_debugger(!IO),
            WhatNext = wn_step
        )
    ;
        Args = [_ | _],
        % Should we exit even in this case?
        print_help(!IO),
        read_and_execute_cmd(Event, ShadowStack, Depth, WhatNext, !IO)
    ).

%---------------------------------------------------------------------------%

    % csn_is_in_stack(CSN, Stack).
    %
    % Determine if a CSN from a given frame match a frame in the Stack
    %
:- pred csn_is_in_stack(int::in, stack(stack_elem)::in) is semidet.

csn_is_in_stack(CSN, ShadowStack0) :-
    stack.pop(ShadowStack0, Frame, ShadowStack),
    ( CSN = Frame ^ se_csn ->
        true
    ;
        csn_is_in_stack(CSN, ShadowStack)
    ).

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

    % get_correct_frame_with_num(Num, ShadowStack, Frame).
    %
    % Get the Nth frame from the shadow stack, beginning from the top.
    % Num should be in the interval of 0 =< Num =< Depth - 1.
    % If Num = 0, get the top frame.
    %
:- pred get_correct_frame_with_num(int::in, stack(stack_elem)::in,
    stack_elem::out) is det.

get_correct_frame_with_num(Num, ShadowStack0, StackFrame) :-
    ( Num = 0 ->
        stack.top_det(ShadowStack0, StackFrame)
    ; Num > 0 ->
        stack.pop_det(ShadowStack0, _Frame, ShadowStack),
        get_correct_frame_with_num(Num-1, ShadowStack, StackFrame)
    ;
        error("Unexpected error : get_correct_frame_with_num")
    ).

    % Disable or enable all breakpoints.
    %
:- pred modify_state_breakpoints(bp_state::in, io::di, io::uo) is det.

modify_state_breakpoints(State, !IO) :-
    get_cur_ssdb_breakpoints(BreakPoints, !IO),
    BreakPointListValue = map.values(BreakPoints),
    modify_state_breakpoint(State, BreakPointListValue, BreakPoints,
        BreakPointsModified, !IO),
    set_cur_ssdb_breakpoints(BreakPointsModified, !IO).

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

    % modify_state_breakpoint_with_num(State, Num, !IO).
    %
    % Modify the state of the breakpoint with the number which match Num.
    %
:- pred modify_state_breakpoint_with_num(bp_state::in, int::in,
    io::di, io::uo) is det.

modify_state_breakpoint_with_num(State, Num, !IO) :-
    get_cur_ssdb_breakpoints(BreakPoints, !IO),
    BreakPointListValue = map.values(BreakPoints),
    ( find_breakpoint_with_num(Num, BreakPointListValue, BreakPointToModify) ->
        modify_state_breakpoint(State, [BreakPointToModify], BreakPoints,
            BreakPointsModified, !IO),
        set_cur_ssdb_breakpoints(BreakPointsModified, !IO)
    ;
        io.format("ssdb: break point #%d does not exist.\n", [i(Num)], !IO)
    ).

    % delete_breakpoint_with_num(Num, !IO).
    %
    % Delete the breakpoint that match with Num.
    %
:- pred delete_breakpoint_with_num(int::in, io::di, io::uo) is det.

delete_breakpoint_with_num(Num, !IO) :-
    get_cur_ssdb_breakpoints(BreakPoints0, !IO),
    BreakPointsListValue = map.values(BreakPoints0),
    ( find_breakpoint_with_num(Num, BreakPointsListValue, BPToDelete) ->
        Module = BPToDelete ^ bp_module_name,
        Procedure = BPToDelete ^ bp_pred_name,
        map.delete(BreakPoints0, pair(Module, Procedure), BreakPoints),
        set_cur_ssdb_breakpoints(BreakPoints, !IO),
        io.format("Breakpoint on %s.%s deleted\n", [s(Module), s(Procedure)],
            !IO)
    ;
        io.format("ssdb: break point #%d does not exist.\n", [i(Num)], !IO)
    ).

    % find_breakpoint_with_num(Num, ListBreakPoint, BreakPointFound)
    %
    % As the structure of a breakpoint have a Number, this predicate will
    % return BreakPointFound with bp_number that match with the given Num.
    %
:- pred find_breakpoint_with_num(int::in, list(breakpoint)::in,
    breakpoint::out) is semidet.

find_breakpoint_with_num(Num, [BP|ListBreakPoint], BreakPointFound) :-
    ( BP ^ bp_number = Num ->
        BreakPointFound = BP
    ;
        find_breakpoint_with_num(Num, ListBreakPoint, BreakPointFound)
    ).

    % Exit the debugger.
    %
:- pred exit_debugger(io::di, io::uo) is det.

:- pragma foreign_proc("C",
    exit_debugger(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    exit(0);
    IO = IO0;
").

:- pragma foreign_proc("Java",
    exit_debugger(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    System.exit(0);
").

%----------------------------------------------------------------------------%

    % Print the current informations at this event point.
    %
:- pred print_event_info(ssdb_event_type::in, int::in, ssdb_proc_id::in,
    int::in, int::in, io::di, io::uo) is det.

print_event_info(Event, EventNum, ProcId, PrintDepth, CSN, !IO) :-
    % Should right align these numbers.
    io.write_string("\t", !IO),
    io.write_int(EventNum, !IO),
    io.write_string(":\t", !IO),
    io.write_int(CSN, !IO),
    io.write_string("  ", !IO),
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
    io.write_string(" ", !IO),
    % mdb writes pred/func here.
    io.write_string(ProcId ^ module_name, !IO),
    io.write_string(".", !IO),
    io.write_string(ProcId ^ proc_name, !IO),
    % mdb writes arity, mode, determinism and context here.
    io.nl(!IO).

%-----------------------------------------------------------------------------%

    % print_frame_info(Frame, StackDepth, !IO).
    %
    % Print the information of the frame gave in argument.
    %
:- pred print_frame_info(stack_elem::in, int::in, io::di, io::uo) is det.

print_frame_info(TopFrame, StackDepth, !IO) :-
    Depth = TopFrame ^ se_depth,
    ProcId = TopFrame ^ se_proc_id,
    ModuleName = ProcId ^ module_name,
    ProcName = ProcId ^ proc_name,
    RevDepth = StackDepth - Depth,
    io.format("%4d  %s.%s\n", [i(RevDepth), s(ModuleName), s(ProcName)], !IO).

%-----------------------------------------------------------------------------%

    % Print a summary of the commands.
    %
:- pred print_help(io::di, io::uo) is det.

print_help(!IO) :-
    io.write_string("<step> or <s> or blank", !IO),
    io.write_string("\n<next> or <n>", !IO),
    io.write_string("\n<continue> or <c>", !IO),
    io.write_string("\n<finish> or <f>", !IO),
    io.write_string("\n<retry> or <r>", !IO),
    io.write_string("\n<break X Y> or <b X Y>", !IO),
    io.write_string("\n<break info> or <b info>", !IO),
    io.write_string("\n<enable / disable / delete *>", !IO),
    io.write_string("\n<enable / disable / delete N>", !IO),
    io.write_string("\n<print> or <p>", !IO),
    io.write_string("\n<print VAR> or <p VAR>", !IO),
    io.write_string("\n<print N> or <p N>", !IO),
    io.write_string("\n<browse VAR>", !IO),
    io.write_string("\n<browse N>", !IO),
    io.write_string("\n<vars> or <v>", !IO),
    io.write_string("\n<stack> or <st>", !IO),
    io.write_string("\n<up> or <u>", !IO),
    io.write_string("\n<down> or <d>", !IO),
    io.write_string("\n<goto N> or <g N>", !IO),
    io.write_string("\n<help> or <h>", !IO),
    io.write_string("\n<quit> or <q>", !IO),
    io.write_string("\n\n", !IO).

%-----------------------------------------------------------------------------%

    % Print the Stack Trace. Predicate call at the 'stack' command.
    %
:- pred print_frames_list(int::in, stack(stack_elem)::in, int::in,
    io::di, io::uo) is det.

print_frames_list(Level, ShadowStack0, Depth, !IO) :-
    ( if not stack.is_empty(ShadowStack0) then
        stack.pop_det(ShadowStack0, PopFrame, ShadowStack),
        (if Depth = 0 then
            print_stack_frame(yes, Level, PopFrame, !IO)
        else
            print_stack_frame(no, Level, PopFrame, !IO)
        ),
        print_frames_list(Level + 1, ShadowStack, Depth - 1, !IO)
    else
        true
    ).

    % print_stack_frame(Starred, Level, Frame, !IO).
    %
    % Print the given Frame. The Level is the place of this frame in the
    % stack.
    %
:- pred print_stack_frame(bool::in, int::in, stack_elem::in,
    io::di, io::uo) is det.

print_stack_frame(Starred, Level, Frame, !IO) :-
    Module = Frame ^ se_proc_id ^ module_name,
    Procedure = Frame ^ se_proc_id ^ proc_name,
    (
        Starred = yes,
        io.write_char('*', !IO)
    ;
        Starred = no,
        io.write_char(' ', !IO)
    ),
    io.format("%5d\t%s.%s\n", [i(Level), s(Module), s(Procedure)], !IO).

%-----------------------------------------------------------------------------%

    % Print the given list of variables and their values, if bound.
    % XXX The pprint.write predicate is used for the moment instead of
    % pretty_printer because this last one had a strange behavior
    % The terms would always appear after io.write_string output in the ssdb
    % 'p' command. Somehting like:
    %
    % Var1 =
    % Var2 =
    % Var3 =
    % <term of Var1>
    % <term of Var2>
    % <term of Var3>
    %
    % whereas it should be:
    %
    % Var1 = <term of Var1>
    % etc.
    %
:- pred print_vars(list(var_value)::in, io::di, io::uo) is det.

print_vars(Vars, !IO) :-
    list.foldl(print_var, Vars, !IO).

:- pred print_var_with_name(list(var_value)::in, string::in,
    io::di, io::uo) is det.

print_var_with_name(VarDescs, VarName, !IO) :-
    (
        string.to_int(VarName, VarNum),
        VarNum > 0
    ->
        print_var_with_number(VarDescs, VarNum, !IO)
    ;
        % Since we don't have tab completion, make it easier for the user by
        % matching prefixes instead of the entire name.
        P = (pred(VarDesc::in) is semidet :-
            string.prefix(get_var_name(VarDesc), VarName)
        ),
        list.filter(P, VarDescs, MatchVars),
        (
            MatchVars = [],
            io.write_string("ssdb: there is no such variable.\n", !IO)
        ;
            MatchVars = [_ | _],
            print_vars(MatchVars, !IO)
        )
    ).

:- pred print_var_with_number(list(var_value)::in, int::in, io::di, io::uo)
    is det.

print_var_with_number(VarDescs, VarNum, !IO) :-
    ( list.index1(VarDescs, VarNum, VarDesc) ->
        print_var(VarDesc, !IO)
    ;
        io.write_string("ssdb: there aren't that many variables.\n", !IO)
    ).

:- pred print_var(var_value::in, io::di, io::uo) is det.

print_var(unbound_head_var(Name, Pos), !IO) :-
    print_var_prelude(Name, Pos, !IO),
    io.write_string("_\n", !IO).

print_var(bound_head_var(Name, Pos, T), !IO) :-
    print_var_prelude(Name, Pos, !IO),
    safe_write(T, !IO),
    io.nl(!IO).

print_var(bound_other_var(Name, T), !IO) :-
    print_var_prelude(Name, -1, !IO),
    safe_write(T, !IO),
    io.nl(!IO).

:- pred print_var_prelude(var_name::in, int::in, io::di, io::uo) is det.

print_var_prelude(Name, Pos, !IO) :-
    io.write_char('\t', !IO),
    io.write_string(Name, !IO),
    ( Pos >= 0 ->
        io.write_string(" (arg ", !IO),
        io.write_int(Pos + 1, !IO),
        io.write_string(")\t", !IO)
    ;
        io.write_string("\t", !IO)
    ).

:- pred safe_write(T::in, io::di, io::uo) is det.

safe_write(T, !IO) :-
    ( safe_to_write(T) ->
        pprint.write(80, to_doc(T), !IO)
    ;
        io.write_string("<>", !IO)
    ).

:- pred safe_to_write(T::in) is semidet.

safe_to_write(_) :-
    semidet_true.

:- pragma foreign_proc("C",
    safe_to_write(T::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (T != 0);
").

:- pragma foreign_proc("Java",
    safe_to_write(T::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (T != null);
").

:- func get_var_name(var_value) = string.

get_var_name(unbound_head_var(Name, _)) = Name.
get_var_name(bound_head_var(Name, _, _)) = Name.
get_var_name(bound_other_var(Name, _)) = Name.

%-----------------------------------------------------------------------------%

:- pred browse_var(assoc_list(string, univ)::in, string::in, io::di, io::uo)
    is det.

browse_var(VarDescs, VarName, !IO) :-
    (
        string.to_int(VarName, VarNum),
        VarNum > 0
    ->
        ( list.index1(VarDescs, VarNum, _ - Univ) ->
            browse_univ(Univ, !IO)
        ;
            io.write_string("ssdb: there aren't that many variables.\n", !IO)
        )
    ;
        assoc_list.search(VarDescs, VarName, Univ)
    ->
        browse_univ(Univ, !IO)
    ;
        io.write_string("ssdb: there is no such variable.\n", !IO)
    ).

:- pred browse_univ(univ::in, io::di, io::uo) is det.

browse_univ(Univ, !IO) :-
    io.stdin_stream(StdIn, !IO),
    io.stdout_stream(StdOut, !IO),
    browser_info.init_persistent_state(State0),
    BT = browser_term.univ_to_browser_term(Univ),
    promise_equivalent_solutions [!:IO] (
        browse.browse_browser_term_no_modes(BT, StdIn, StdOut, _,
        State0, _State1, !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred print_vars_list(list(var_value)::in, int::in, io::di, io::uo) is det.

print_vars_list([], _, !IO).
print_vars_list([Var | Vars], VarNum, !IO) :-
    io.format("\t%2d ", [i(VarNum)], !IO),
    ( Var = unbound_head_var(Name, Pos)
    ; Var = bound_head_var(Name, Pos, _)
    ; Var = bound_other_var(Name, _), Pos = -1
    ),
    io.write_string(Name, !IO),
    ( Pos >= 0 ->
        io.format(" (arg %d)\n", [i(Pos + 1)], !IO)
    ;
        io.nl(!IO)
    ),
    print_vars_list(Vars, VarNum + 1, !IO).

%-----------------------------------------------------------------------------%

    % Print the current list of breakpoints with their details.
    %
:- pred print_breakpoints(list(breakpoint)::in, io::di, io::uo) is det.

print_breakpoints(BreakPoints, !IO) :-
    (
        BreakPoints = [],
        io.write_string("There are no break points.\n", !IO)
    ;
        BreakPoints = [_ | _],
        list.foldl(print_breakpoint, BreakPoints, !IO)
    ).

:- pred print_breakpoint(breakpoint::in, io::di, io::uo) is det.

print_breakpoint(BreakPoint, !IO) :-
    BreakPointNum = BreakPoint ^ bp_number,
    (
        BreakPoint ^ bp_state = bp_state_enabled,
        Enabled = "+"
    ;
        BreakPoint ^ bp_state = bp_state_disabled,
        Enabled = "-"
    ),
    ModuleName = BreakPoint ^ bp_module_name,
    PredName = BreakPoint ^ bp_pred_name,
    io.format("%2d: %s %s.%s\n",
        [i(BreakPointNum), s(Enabled), s(ModuleName), s(PredName)], !IO).

%----------------------------------------------------------------------------%

:- pragma inline(invent_io/1).
:- impure pred invent_io(io::uo) is det.

:- pragma foreign_proc("C",
    invent_io(_IO::uo),
    [will_not_call_mercury, thread_safe],
"
").

:- pragma foreign_proc("Erlang",
    invent_io(_IO::uo),
    [will_not_call_mercury, thread_safe],
"
    void
").

:- pragma foreign_proc("C#",
    invent_io(_IO::uo),
    [will_not_call_mercury, thread_safe],
"
").

:- pragma foreign_proc("Java",
    invent_io(_IO::uo),
    [will_not_call_mercury, thread_safe],
"
").

:- pragma inline(consume_io/1).
:- impure pred consume_io(io::di) is det.

:- pragma foreign_proc("C",
    consume_io(_IO::di),
    [will_not_call_mercury, thread_safe],
"
").

:- pragma foreign_proc("Erlang",
    consume_io(_IO::di),
    [will_not_call_mercury, thread_safe],
"
    void
").

:- pragma foreign_proc("C#",
    consume_io(_IO::di),
    [will_not_call_mercury, thread_safe],
"
").

:- pragma foreign_proc("Java",
    consume_io(_IO::di),
    [will_not_call_mercury, thread_safe],
"
").

%-----------------------------------------------------------------------------%

:- pred save_streams(io::di, io::uo) is det.

save_streams(!IO) :-
    get_tty_in(TTY_in, !IO),
    get_tty_out(TTY_out, !IO),
    io.set_input_stream(TTY_in, OldInputStream, !IO),
    io.set_output_stream(TTY_out, OldOutputStream, !IO),
    set_saved_input_stream(OldInputStream, !IO),
    set_saved_output_stream(OldOutputStream, !IO).

:- pred restore_streams(io::di, io::uo) is det.

restore_streams(!IO) :-
    get_saved_input_stream(InputStream, !IO),
    get_saved_output_stream(OutputStream, !IO),
    io.set_input_stream(InputStream, _, !IO),
    io.set_output_stream(OutputStream, _, !IO).

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%
