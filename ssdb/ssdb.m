%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2007, 2010 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: ssdb.m.
% Authors: oannet, wangp.
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
    ;       ssdb_excp.

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

    % Update globals recording the context of the upcoming call.
    %
:- impure pred set_context(string::in, int::in) is det.

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
:- import_module char.
:- import_module io.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
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

%-----------------------------------------------------------------------------%

:- type cur_ssdb_breakpoints == map(pair(string,string), breakpoint).

    % Note: debugger_off must be first because io.init_state/2 is called
    % before the `do_nothing' mutable is initialised.  At that time `do_nothing'
    % will have a value of zero.  By putting debugger_off first, it will
    % be represented by zero so the SSDB port code will correctly do nothing
    % until after the library is initialised.
    %
:- type debugger_state
    --->    debugger_off
    ;       debugger_on.

:- type stack_frame
    --->    stack_frame(
                % Event Number
                sf_event_number     :: int,

                % Call Sequence Number.
                sf_csn              :: int,

                % Depth of the procedure.
                sf_depth            :: int,

                % The goal's module name and procedure name.
                sf_proc_id          :: ssdb_proc_id,

                % The call site.
                sf_call_site_file   :: string,
                sf_call_site_line   :: int,

                % The list of the procedure's arguments.
                sf_list_var_value   :: list(var_value)
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
    ;       wn_return
    ;       wn_exception
    ;       wn_retry(int)
    ;       wn_retry_nondet(int)
    ;       wn_goto(int).

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
            % Stop at the final port (exit or fail) of the given CSN.
            % The second argument says whether to automatically retry
            % upon reaching that port.

    ;       ns_final_port_nondet(int, ssdb_retry)
            % As above for nondet procedures.
            % Stop at the final port (fail) of the given CSN.

    ;       ns_nonexit
            % Stop at any non-exit port.

    ;       ns_goto(int)
            % Stop at the given event number.

    ;       ns_exception.
            % Stop at the next exception.

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

:- inst either_call
    --->    ssdb_call
    ;       ssdb_call_nondet.

:- inst either_fail
    --->    ssdb_fail
    ;       ssdb_fail_nondet.

%----------------------------------------------------------------------------%

:- mutable(cur_filename, string, "", ground,
    [untrailed, attach_to_io_state]).
:- mutable(cur_line_number, int, 0, ground,
    [untrailed, attach_to_io_state]).

:- mutable(cur_ssdb_event_number, int, 0, ground,
    [untrailed, attach_to_io_state]).

:- mutable(cur_ssdb_csn, int, 0, ground,
    [untrailed, attach_to_io_state]).

:- mutable(cur_ssdb_next_stop, next_stop, ns_step, ground,
    [untrailed, attach_to_io_state]).

:- mutable(cur_ssdb_breakpoints, cur_ssdb_breakpoints, map.init, ground,
    [untrailed, attach_to_io_state]).

:- mutable(cur_ssdb_number_of_breakpoint, int, 0,
    ground, [untrailed, attach_to_io_state]).

:- mutable(shadow_stack, list(stack_frame), [], ground,
    [untrailed, attach_to_io_state]).
:- mutable(shadow_stack_depth, int, 0, ground,
    [untrailed, attach_to_io_state]).

:- mutable(nondet_shadow_stack, list(stack_frame), [], ground,
    [untrailed, attach_to_io_state]).
:- mutable(nondet_shadow_stack_depth, int, 0, ground,
    [untrailed, attach_to_io_state]).

%-----------------------------------------------------------------------------%

:- mutable(browser_state, browser_persistent_state,
    init_browser_persistent_state, ground,
    [untrailed, attach_to_io_state]).

:- func init_browser_persistent_state = browser_persistent_state.

init_browser_persistent_state = State :-
    browser_info.init_persistent_state(State).

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
            install_sigint_handler(!IO),
            install_exception_hooks(!IO)
        ;
            DebuggerState = debugger_off
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
//  XXX Using the @Override annotation here causes compilation errors
//  with Java 1.5.
//  @Override
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

%-----------------------------------------------------------------------------%

set_context(FileName, Line) :-
    impure set_cur_filename(FileName),
    impure set_cur_line_number(Line).

%----------------------------------------------------------------------------%

handle_event_call(ProcId, ListVarValue) :-
    some [!IO] (
        impure invent_io(!:IO),
        get_debugger_state(DebuggerState, !IO),
        (
            DebuggerState = debugger_on,
            handle_event_call_2(ssdb_call, ProcId, ListVarValue, !IO)
        ;
            DebuggerState = debugger_off
        ),
        impure consume_io(!.IO)
    ).

handle_event_call_nondet(ProcId, ListVarValue) :-
    some [!IO] (
        impure invent_io(!:IO),
        get_debugger_state(DebuggerState, !IO),
        (
            DebuggerState = debugger_on,
            handle_event_call_2(ssdb_call_nondet, ProcId, ListVarValue, !IO)
        ;
            DebuggerState = debugger_off
        ),
        impure consume_io(!.IO)
    ).

:- pred handle_event_call_2(ssdb_event_type::in(either_call), ssdb_proc_id::in,
    list(var_value)::in, io::di, io::uo) is det.

:- pragma inline(handle_event_call_2/5).

handle_event_call_2(Event, ProcId, ListVarValue, !IO) :-
    get_ssdb_event_number_inc(EventNum, !IO),
    get_ssdb_csn_inc(CSN, !IO),
    stack_depth(OldDepth, !IO),
    Depth = OldDepth + 1,

    % Push the new stack frame on top of the shadow stack(s).
    get_cur_filename(SiteFile, !IO),
    get_cur_line_number(SiteLine, !IO),
    StackFrame = stack_frame(EventNum, CSN, Depth, ProcId, SiteFile, SiteLine,
        ListVarValue),
    stack_push(StackFrame, !IO),
    (
        Event = ssdb_call
    ;
        Event = ssdb_call_nondet,
        nondet_stack_push(StackFrame, !IO)
    ),

    should_stop_at_this_event(Event, EventNum, CSN, ProcId, Stop, _AutoRetry,
        !IO),
    (
        Stop = yes,
        save_streams(!IO),
        print_event_info(Event, EventNum, !IO),
        read_and_execute_cmd(Event, 0, WhatNext, !IO),
        update_next_stop(EventNum, CSN, WhatNext, _Retry, !IO),
        restore_streams(!IO)
    ;
        Stop = no
    ).

%-----------------------------------------------------------------------------%

handle_event_exit(ProcId, ListVarValue, Retry) :-
    some [!IO] (
        impure invent_io(!:IO),
        get_debugger_state(DebuggerState, !IO),
        (
            DebuggerState = debugger_on,
            handle_event_exit_2(ssdb_exit, ProcId, ListVarValue, Retry, !IO)
        ;
            DebuggerState = debugger_off,
            Retry = do_not_retry
        ),
        impure consume_io(!.IO)
    ).

handle_event_exit_nondet(ProcId, ListVarValue) :-
    some [!IO] (
        impure invent_io(!:IO),
        get_debugger_state(DebuggerState, !IO),
        (
            DebuggerState = debugger_on,
            handle_event_exit_2(ssdb_exit_nondet, ProcId, ListVarValue,
                _Retry, !IO)
        ;
            DebuggerState = debugger_off
        ),
        impure consume_io(!.IO)
    ).

:- pred handle_event_exit_2(ssdb_event_type::in, ssdb_proc_id::in,
    list(var_value)::in, ssdb_retry::out, io::di, io::uo) is det.

:- pragma inline(handle_event_exit_2/6).

handle_event_exit_2(Event, ProcId, ListVarValue, Retry, !IO) :-
    get_ssdb_event_number_inc(EventNum, !IO),
    stack_top_csn(CSN, !IO),
    should_stop_at_this_event(Event, EventNum, CSN, ProcId, Stop, AutoRetry,
        !IO),
    (
        Stop = yes,
        (
            AutoRetry = do_retry,
            WhatNext = wn_retry(CSN)
        ;
            AutoRetry = do_not_retry,
            % There is no need to update the variable list on the top stack
            % frame unless we are stopping to look at it.
            update_top_var_list(ListVarValue, !IO),
            save_streams(!IO),
            print_event_info(Event, EventNum, !IO),
            read_and_execute_cmd(Event, 0, WhatNext, !IO),
            restore_streams(!IO)
        ),
        update_next_stop(EventNum, CSN, WhatNext, Retry, !IO)
    ;
        Stop = no,
        Retry = do_not_retry
    ),
    stack_pop(!IO).

%-----------------------------------------------------------------------------%

handle_event_fail(ProcId, _ListVarValue, Retry) :-
    some [!IO] (
        impure invent_io(!:IO),
        get_debugger_state(DebuggerState, !IO),
        (
            DebuggerState = debugger_on,
            handle_event_fail_2(ssdb_fail, ProcId, Retry, !IO)
        ;
            DebuggerState = debugger_off,
            Retry = do_not_retry
        ),
        impure consume_io(!.IO)
    ).

handle_event_fail_nondet(ProcId, _ListVarValue, Retry) :-
    some [!IO] (
        impure invent_io(!:IO),
        get_debugger_state(DebuggerState, !IO),
        (
            DebuggerState = debugger_on,
            handle_event_fail_2(ssdb_fail_nondet, ProcId, Retry, !IO)
        ;
            DebuggerState = debugger_off,
            Retry = do_not_retry
        ),
        impure consume_io(!.IO)
    ).

:- pred handle_event_fail_2(ssdb_event_type::in(either_fail), ssdb_proc_id::in,
    ssdb_retry::out, io::di, io::uo) is det.

:- pragma inline(handle_event_fail_2/5).

handle_event_fail_2(Event, ProcId, Retry, !IO) :-
    get_ssdb_event_number_inc(EventNum, !IO),
    stack_top_csn(CSN, !IO),
    should_stop_at_this_event(Event, EventNum, CSN, ProcId, Stop, AutoRetry,
        !IO),
    (
        Stop = yes,
        (
            AutoRetry = do_retry,
            WhatNext = wn_retry(CSN)
        ;
            AutoRetry = do_not_retry,
            save_streams(!IO),
            print_event_info(Event, EventNum, !IO),
            read_and_execute_cmd(Event, 0, WhatNext, !IO),
            restore_streams(!IO)
        ),
        update_next_stop(EventNum, CSN, WhatNext, Retry, !IO)
    ;
        Stop = no,
        Retry = do_not_retry
    ),
    stack_pop(!IO),
    (
        Event = ssdb_fail
    ;
        Event = ssdb_fail_nondet,
        nondet_stack_pop(!IO)
    ).

handle_event_redo_nondet(ProcId, _ListVarValue) :-
    some [!IO] (
        impure invent_io(!:IO),
        get_debugger_state(DebuggerState, !IO),
        (
            DebuggerState = debugger_on,
            Event = ssdb_redo_nondet,
            get_ssdb_event_number_inc(EventNum, !IO),
            stack_depth(OldDepth, !IO),
            Depth = OldDepth + 1,
            lookup_nondet_stack_frame(ProcId, Depth, StackFrame, !IO),
            stack_push(StackFrame, !IO),
            CSN = StackFrame ^ sf_csn,
            should_stop_at_this_event(Event, EventNum, CSN, ProcId, Stop,
                _AutoRetry, !IO),
            (
                Stop = yes,
                save_streams(!IO),
                print_event_info(Event, EventNum, !IO),
                read_and_execute_cmd(Event, 0, WhatNext, !IO),
                update_next_stop(EventNum, CSN, WhatNext, _Retry, !IO),
                restore_streams(!IO)
            ;
                Stop = no
            )
        ;
            DebuggerState = debugger_off
        ),
        impure consume_io(!.IO)
    ).

:- pred lookup_nondet_stack_frame(ssdb_proc_id::in, int::in, stack_frame::out,
    io::di, io::uo) is det.

lookup_nondet_stack_frame(ProcId, Depth, StackFrame, !IO) :-
    search_nondet_stack_frame(ProcId, Depth, MaybeStackFrame, !IO),
    (
        MaybeStackFrame = yes(StackFrame)
    ;
        MaybeStackFrame = no,
        error("ssdb: lookup_nondet_stack_frame")
    ).

:- pred search_nondet_stack_frame(ssdb_proc_id::in, int::in,
    maybe(stack_frame)::out, io::di, io::uo) is det.

search_nondet_stack_frame(ProcId, Depth, StackFrame, !IO) :-
    nondet_stack_depth(StackDepth, !IO),
    search_nondet_stack_frame_2(ProcId, Depth, 0, StackDepth, StackFrame, !IO).

:- pred search_nondet_stack_frame_2(ssdb_proc_id::in, int::in, int::in,
    int::in, maybe(stack_frame)::out, io::di, io::uo) is det.

search_nondet_stack_frame_2(ProcId, Depth, N, StackDepth, MaybeStackFrame,
        !IO) :-
    ( N >= StackDepth ->
        MaybeStackFrame = no
    ;
        nondet_stack_index(N, Frame, !IO),
        (
            Frame ^ sf_proc_id ^ module_name = ProcId ^ module_name,
            Frame ^ sf_proc_id ^ proc_name = ProcId ^ proc_name,
            Frame ^ sf_depth = Depth
        ->
            MaybeStackFrame = yes(Frame)
        ;
            search_nondet_stack_frame_2(ProcId, Depth, N + 1, StackDepth,
                MaybeStackFrame, !IO)
        )
    ).

%----------------------------------------------------------------------------%
%
% Support for exception events (Java only currently)
%

:- pred install_exception_hooks(io::di, io::uo) is det.

install_exception_hooks(!IO).

:- pragma foreign_proc("Java",
    install_exception_hooks(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    exception.ssdb_hooks = new ssdb.SsdbHooks();
").

:- pragma foreign_code("Java", "
private static class SsdbHooks extends exception.SsdbHooks {
    @Override
    public void on_throw_impl(univ.Univ_0 univ) {
        ssdb.SSDB_handle_event_excp(""exception"", ""throw_impl"", univ);
    }

    @Override
    public int on_catch_impl() {
        return ssdb.SSDB_get_cur_ssdb_csn();
    }

    @Override
    public void on_catch_impl_exception(int CSN) {
        ssdb.SSDB_rollback_stack(CSN);
        ssdb.SSDB_rollback_nondet_stack(CSN);
    }
}
").

:- impure pred handle_event_excp(string::in, string::in, univ::in) is det.
:- pragma foreign_export("Java", handle_event_excp(in, in, in),
    "SSDB_handle_event_excp").

handle_event_excp(ModuleName, ProcName, Univ) :-
    some [!IO] (
        impure invent_io(!:IO),
        get_debugger_state(DebuggerState, !IO),
        (
            DebuggerState = debugger_on,
            ProcId = ssdb_proc_id(ModuleName, ProcName),
            VarDescs = ['new bound_head_var'("Univ", 1, Univ)],
            handle_event_excp_2(ProcId, VarDescs, !IO)
        ;
            DebuggerState = debugger_off
        ),
        impure consume_io(!.IO)
    ).

:- pred handle_event_excp_2(ssdb_proc_id::in, list(var_value)::in,
    io::di, io::uo) is det.

handle_event_excp_2(ProcId, ListVarValue, !IO) :-
    get_ssdb_event_number_inc(EventNum, !IO),
    get_ssdb_csn_inc(CSN, !IO),
    stack_depth(OldDepth, !IO),
    Depth = OldDepth + 1,

    % Push the new stack frame on top of the shadow stack(s).
    get_cur_filename(SiteFile, !IO),
    get_cur_line_number(SiteLine, !IO),
    StackFrame = stack_frame(EventNum, CSN, Depth, ProcId, SiteFile, SiteLine,
        ListVarValue),
    stack_push(StackFrame, !IO),

    Event = ssdb_excp,
    should_stop_at_this_event(Event, EventNum, CSN, ProcId, Stop, _AutoRetry,
        !IO),
    (
        Stop = yes,
        save_streams(!IO),
        print_event_info(Event, EventNum, !IO),
        read_and_execute_cmd(Event, 0, WhatNext, !IO),
        update_next_stop(EventNum, CSN, WhatNext, _Retry, !IO),
        restore_streams(!IO)
    ;
        Stop = no
    ).

%----------------------------------------------------------------------------%

:- pragma foreign_export("Java", get_cur_ssdb_csn(out),
    "SSDB_get_cur_ssdb_csn").

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

%-----------------------------------------------------------------------------%

:- pred stack_top(stack_frame::out, io::di, io::uo) is det.

stack_top(Frame, !IO) :-
    get_shadow_stack(Stack, !IO),
    (
        Stack = [],
        error("ssdb: stack_top on empty stack")
    ;
        Stack = [Frame | _]
    ).

:- pred stack_top_csn(int::out, io::di, io::uo) is det.

stack_top_csn(CSN, !IO) :-
    stack_top(Frame, !IO),
    CSN = Frame ^ sf_csn.

:- pred stack_index(int::in, stack_frame::out, io::di, io::uo) is det.

stack_index(Num, Frame, !IO) :-
    get_shadow_stack(Stack, !IO),
    list.index0_det(Stack, Num, Frame).

:- pred stack_depth(int::out, io::di, io::uo) is det.

stack_depth(Depth, !IO) :-
    get_shadow_stack_depth(Depth, !IO).

:- pred stack_push(stack_frame::in, io::di, io::uo) is det.

stack_push(Frame, !IO) :-
    get_shadow_stack(Stack, !IO),
    set_shadow_stack([Frame | Stack], !IO),
    get_shadow_stack_depth(Depth, !IO),
    set_shadow_stack_depth(Depth + 1, !IO).

:- pred stack_pop(io::di, io::uo) is det.

stack_pop(!IO) :-
    get_shadow_stack(Stack, !IO),
    get_shadow_stack_depth(Depth, !IO),
    (
        Stack = [],
        error("ssdb: stack_pop on empty stack")
    ;
        Stack = [_ | StackTail],
        set_shadow_stack(StackTail, !IO),
        set_shadow_stack_depth(Depth - 1, !IO)
    ).

    % Update the sf_list_var_value field of the top shadow stack element.
    %
:- pred update_top_var_list(list(var_value)::in, io::di, io::uo) is det.

update_top_var_list(ListVarValue, !IO) :-
    get_shadow_stack(Stack0, !IO),
    (
        Stack0 = [],
        error("ssdb: update_top_var_list on empty stack")
    ;
        Stack0 = [Frame0 | Frames],
        Frame = Frame0 ^ sf_list_var_value := ListVarValue,
        set_shadow_stack([Frame | Frames], !IO)
    ).

:- pred nondet_stack_index(int::in, stack_frame::out,
    io::di, io::uo) is det.

nondet_stack_index(Num, Frame, !IO) :-
    get_nondet_shadow_stack(Stack, !IO),
    list.index0_det(Stack, Num, Frame).

:- pred nondet_stack_depth(int::out, io::di, io::uo) is det.

nondet_stack_depth(Depth, !IO) :-
    get_nondet_shadow_stack_depth(Depth, !IO).

:- pred nondet_stack_push(stack_frame::in, io::di, io::uo)
    is det.

nondet_stack_push(Frame, !IO) :-
    get_nondet_shadow_stack(Stack, !IO),
    set_nondet_shadow_stack([Frame | Stack], !IO),
    get_nondet_shadow_stack_depth(Depth, !IO),
    set_nondet_shadow_stack_depth(Depth + 1, !IO).

:- pred nondet_stack_pop(io::di, io::uo) is det.

nondet_stack_pop(!IO) :-
    get_nondet_shadow_stack(Stack, !IO),
    get_nondet_shadow_stack_depth(Depth, !IO),
    (
        Stack = [],
        error("ssdb: nondet_stack_pop on empty stack")
    ;
        Stack = [_ | StackTail],
        set_nondet_shadow_stack(StackTail, !IO),
        set_nondet_shadow_stack_depth(Depth - 1, !IO)
    ).

:- pred rollback_stack(int::in, io::di, io::uo) is det.
:- pragma foreign_export("Java", rollback_stack(in, di, uo),
    "SSDB_rollback_stack").

rollback_stack(TargetCSN, !IO) :-
    stack_top(StackFrame, !IO),
    ( StackFrame ^ sf_csn =< TargetCSN ->
        set_cur_ssdb_csn(StackFrame ^ sf_csn, !IO)
    ;
        stack_pop(!IO),
        rollback_stack(TargetCSN, !IO)
    ).

:- pred rollback_nondet_stack(int::in, io::di, io::uo) is det.
:- pragma foreign_export("Java", rollback_nondet_stack(in, di, uo),
    "SSDB_rollback_nondet_stack").

rollback_nondet_stack(TargetCSN, !IO) :-
    nondet_stack_depth(StackDepth, !IO),
    ( StackDepth = 0 ->
        true
    ;
        nondet_stack_index(0, StackFrame, !IO),
        ( StackFrame ^ sf_csn =< TargetCSN ->
            true
        ;
            nondet_stack_pop(!IO),
            rollback_nondet_stack(TargetCSN, !IO)
        )
    ).

%-----------------------------------------------------------------------------%

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
            % Avoid generating garbage when the user hasn't set any
            % breakpoints. Eventually we should optimise the handling of
            % breakpoints with a more appropriate data structure.
            not map.is_empty(BreakPoints),
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
        NextStop = ns_final_port(StopCSN, AutoRetry0),
        (
            ( Event = ssdb_exit
            ; Event = ssdb_exit_nondet
            ; Event = ssdb_fail
            ; Event = ssdb_fail_nondet
            ),
            ( StopCSN = CSN ->
                ShouldStopAtEvent = yes,
                AutoRetry = AutoRetry0,
                (
                    AutoRetry = do_retry,
                    % NOTE: The event number and CSN used to be reset at the
                    % time the user entered the `retry' command.  That is
                    % incorrect as we may need to perform forward execution
                    % before reaching the final port of the target CSN to
                    % retry.  During forward execution the counters are
                    % incremented as usual, so the debugger state is corrupted.
                    stack_top(Frame, !IO),
                    reset_counters_for_retry(Frame, !IO)
                ;
                    AutoRetry = do_not_retry
                )
            ;
                ShouldStopAtEvent = no,
                AutoRetry = do_not_retry
            )
        ;
            Event = ssdb_excp,
            % Stop immediately, unless there is an exception handler which will
            % catch the exception before we reach the final port of StopCSN.
            get_shadow_stack(Stack, !IO),
            ( exception_handler_exists(StopCSN, Stack) ->
                ShouldStopAtEvent = no
            ;
                ShouldStopAtEvent = yes
            ),
            AutoRetry = do_not_retry
        ;
            ( Event = ssdb_call
            ; Event = ssdb_call_nondet
            ; Event = ssdb_redo_nondet
            ),
            ShouldStopAtEvent = no,
            AutoRetry = do_not_retry
        )
    ;
        NextStop = ns_final_port_nondet(StopCSN, AutoRetry0),
        (
            Event = ssdb_fail_nondet,
            ( StopCSN = CSN ->
                ShouldStopAtEvent = yes,
                AutoRetry = AutoRetry0,
                (
                    AutoRetry = do_retry,
                    nondet_stack_index(0, Frame, !IO),
                    ( Frame ^ sf_csn = CSN ->
                        % See note above.
                        reset_counters_for_retry(Frame, !IO)
                    ;
                        error("ssdb: nondet stack frame has unexpected CSN")
                    )
                ;
                    AutoRetry = do_not_retry
                )
            ;
                ShouldStopAtEvent = no,
                AutoRetry = do_not_retry
            )
        ;
            Event = ssdb_excp,
            get_shadow_stack(Stack, !IO),
            ( exception_handler_exists(StopCSN, Stack) ->
                ShouldStopAtEvent = no
            ;
                ShouldStopAtEvent = yes
            ),
            AutoRetry = do_not_retry
        ;
            ( Event = ssdb_call
            ; Event = ssdb_exit
            ; Event = ssdb_fail
            ; Event = ssdb_call_nondet
            ; Event = ssdb_exit_nondet
            ; Event = ssdb_redo_nondet
            ),
            ShouldStopAtEvent = no,
            AutoRetry = do_not_retry
        )
    ;
        NextStop = ns_nonexit,
        (
            ( Event = ssdb_call
            ; Event = ssdb_fail
            ; Event = ssdb_call_nondet
            ; Event = ssdb_redo_nondet
            ; Event = ssdb_fail_nondet
            ; Event = ssdb_excp
            ),
            ShouldStopAtEvent = yes
        ;
            ( Event = ssdb_exit
            ; Event = ssdb_exit_nondet
            ),
            ShouldStopAtEvent = no
        ),
        AutoRetry = do_not_retry
    ;
        NextStop = ns_goto(EventNumToGo),
        is_same_int(EventNumToGo, EventNum, ShouldStopAtEvent),
        AutoRetry = do_not_retry
    ;
        NextStop = ns_exception,
        (
            Event = ssdb_excp,
            ShouldStopAtEvent = yes
        ;
            ( Event = ssdb_call
            ; Event = ssdb_exit
            ; Event = ssdb_fail
            ; Event = ssdb_call_nondet
            ; Event = ssdb_exit_nondet
            ; Event = ssdb_redo_nondet
            ; Event = ssdb_fail_nondet
            ),
            ShouldStopAtEvent = no
        ),
        AutoRetry = do_not_retry
    ).

:- pred is_same_int(int::in, int::in, bool::out) is det.

is_same_int(IntA, IntB, IsSame) :-
    IsSame = (IntA = IntB -> yes ; no).

    % update_next_stop(EventNum, CSN, WhatNext, Retry).
    %
    % Set the NextStop and the Retry variable according to the WhatNext value.
    % In the case where the WathNext is set for a retry, it modify the
    % debugger_state at his old value which it had at the call point.
    %
:- pred update_next_stop(int::in, int::in, what_next::in, ssdb_retry::out,
    io::di, io::uo) is det.

update_next_stop(EventNum, CSN, WhatNext, Retry, !IO) :-
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
        WhatNext = wn_return,
        NextStop = ns_nonexit,
        Retry = do_not_retry
    ;
        WhatNext = wn_exception,
        NextStop = ns_exception,
        Retry = do_not_retry
    ;
        WhatNext = wn_retry(RetryCSN),
        ( RetryCSN = CSN ->
            NextStop = ns_step,
            Retry = do_retry,
            stack_top(Frame, !IO),
            reset_counters_for_retry(Frame, !IO)
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

    % Reset the event number and CSN counters in order to retry from event in
    % the given frame.
    %
:- pred reset_counters_for_retry(stack_frame::in, io::di, io::uo) is det.

reset_counters_for_retry(Frame, !IO) :-
    set_cur_ssdb_event_number(Frame ^ sf_event_number - 1, !IO),
    set_cur_ssdb_csn(Frame ^ sf_csn - 1, !IO).

:- pred exception_handler_exists(int::in, list(stack_frame)::in) is semidet.

exception_handler_exists(CSN, StackFrames) :-
    list.member(StackFrame, StackFrames),
    StackFrame ^ sf_csn >= CSN,
    pred_catches_exceptions(StackFrame ^ sf_proc_id).

    % Succeed if the given procedure is one which catches exceptions.
    %
:- pred pred_catches_exceptions(ssdb_proc_id::in) is semidet.

pred_catches_exceptions(ProcId) :-
    ProcId = ssdb_proc_id("exception", Name),
    ( Name = "try"
    ; Name = "try_io"
    ; Name = "try_store"
    ; Name = "try_all"
    ; Name = "incremental_try_all"
    ).

%----------------------------------------------------------------------------%

:- type ssdb_cmd
    --->    ssdb_help

    ;       ssdb_step
    ;       ssdb_next
    ;       ssdb_goto
    ;       ssdb_continue
    ;       ssdb_finish
    ;       ssdb_return
    ;       ssdb_exception

    ;       ssdb_retry

    ;       ssdb_stack
    ;       ssdb_print
    ;       ssdb_browse
    ;       ssdb_vars
    ;       ssdb_down
    ;       ssdb_up
    ;       ssdb_level
    ;       ssdb_current

    ;       ssdb_format
    ;       ssdb_format_param

    ;       ssdb_break
    ;       ssdb_enable
    ;       ssdb_disable
    ;       ssdb_delete

    ;       ssdb_quit.

:- pred ssdb_cmd_name(string, ssdb_cmd).
:- mode ssdb_cmd_name(in, out) is semidet.
:- mode ssdb_cmd_name(out, in) is multi.

ssdb_cmd_name("h",          ssdb_help).
ssdb_cmd_name("help",       ssdb_help).
ssdb_cmd_name("?",          ssdb_help).

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
ssdb_cmd_name("return",     ssdb_return).
ssdb_cmd_name("e",          ssdb_exception).
ssdb_cmd_name("ex",         ssdb_exception).
ssdb_cmd_name("exception",  ssdb_exception).

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
ssdb_cmd_name("level",      ssdb_level).
ssdb_cmd_name("lv",         ssdb_level).
ssdb_cmd_name("current",    ssdb_current).
ssdb_cmd_name("cur",        ssdb_current).

ssdb_cmd_name("format",     ssdb_format).
ssdb_cmd_name("format_param", ssdb_format_param).

ssdb_cmd_name("b",          ssdb_break).
ssdb_cmd_name("break",      ssdb_break).
ssdb_cmd_name("enable",     ssdb_enable).
ssdb_cmd_name("disable",    ssdb_disable).
ssdb_cmd_name("delete",     ssdb_delete).

ssdb_cmd_name("q",          ssdb_quit).
ssdb_cmd_name("quit",       ssdb_quit).

:- pred ssdb_cmd_name(string::in, ssdb_cmd::out, list(string)::out)
    is semidet.

ssdb_cmd_name("P", ssdb_print, ["*"]).

%---------------------------------------------------------------------------%

    % Display the prompt, read a user command, and execute it.
    % Depth is the level of the stack that the user is currently viewing.
    %
:- pred read_and_execute_cmd(ssdb_event_type::in, int::in, what_next::out,
    io::di, io::uo) is det.

read_and_execute_cmd(Event, Depth, WhatNext, !IO) :-
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
            execute_cmd(ssdb_step, [], Event, Depth, WhatNext, !IO)
        ;
            Words = [CmdWord | ArgWords],
            % Implementing aliases would require only looking up an alias map
            % here.
            ( ssdb_cmd_name(CmdWord, Cmd) ->
                execute_cmd(Cmd, ArgWords, Event, Depth, WhatNext, !IO)
            ; ssdb_cmd_name(CmdWord, Cmd, CmdArgs) ->
                execute_cmd(Cmd, CmdArgs ++ ArgWords, Event, Depth, WhatNext,
                    !IO)
            ;
                % A bare integer is treated like a step command.
                string.to_int(CmdWord, _)
            ->
                execute_cmd(ssdb_step, Words, Event, Depth, WhatNext, !IO)
            ;
                io.format("%s: unknown command (try \"help\")\n", [s(CmdWord)],
                    !IO),
                read_and_execute_cmd(Event, Depth, WhatNext, !IO)
            )
        )
    ;
        Result = eof,
        execute_cmd(ssdb_quit, [], Event, Depth, WhatNext, !IO)
    ;
        Result = error(Error),
        io.error_message(Error, Msg),
        io.format("could not read command: %s\n", [s(Msg)], !IO),
        execute_cmd(ssdb_quit, [], Event, Depth, WhatNext, !IO)
    ).

:- pred execute_cmd(ssdb_cmd::in, list(string)::in, ssdb_event_type::in,
    int::in, what_next::out, io::di, io::uo) is det.

execute_cmd(Cmd, Args, Event, Depth, WhatNext, !IO) :-
    (
        Cmd = ssdb_help,
        execute_ssdb_help(Args, !IO),
        read_and_execute_cmd(Event, Depth, WhatNext, !IO)
    ;
        Cmd = ssdb_step,
        execute_ssdb_step(Args, Event, Depth, WhatNext, !IO)
    ;
        Cmd = ssdb_next,
        execute_ssdb_next(Args, Event, Depth, WhatNext, !IO)
    ;
        Cmd = ssdb_goto,
        execute_ssdb_goto(Args, Event, Depth, WhatNext, !IO)
    ;
        Cmd = ssdb_continue,
        execute_ssdb_continue(Args, Event, Depth, WhatNext, !IO)
    ;
        Cmd = ssdb_finish,
        execute_ssdb_finish(Args, Event, Depth, WhatNext, !IO)
    ;
        Cmd = ssdb_return,
        execute_ssdb_return(Args, Event, Depth, WhatNext, !IO)
    ;
        Cmd = ssdb_exception,
        execute_ssdb_exception(Args, Event, Depth, WhatNext, !IO)
    ;
        Cmd = ssdb_retry,
        execute_ssdb_retry(Args, Event, Depth, WhatNext, !IO)
    ;
        Cmd = ssdb_stack,
        execute_ssdb_stack(Args, Depth, !IO),
        read_and_execute_cmd(Event, Depth, WhatNext, !IO)
    ;
        Cmd = ssdb_print,
        execute_ssdb_print(Args, Depth, !IO),
        read_and_execute_cmd(Event, Depth, WhatNext, !IO)
    ;
        Cmd = ssdb_browse,
        execute_ssdb_browse(Args, Depth, !IO),
        read_and_execute_cmd(Event, Depth, WhatNext, !IO)
    ;
        Cmd = ssdb_vars,
        execute_ssdb_vars(Args, Depth, !IO),
        read_and_execute_cmd(Event, Depth, WhatNext, !IO)
    ;
        Cmd = ssdb_down,
        execute_ssdb_down(Args, Depth, NewDepth, !IO),
        read_and_execute_cmd(Event, NewDepth, WhatNext, !IO)
    ;
        Cmd = ssdb_up,
        execute_ssdb_up(Args, Depth, NewDepth, !IO),
        read_and_execute_cmd(Event, NewDepth, WhatNext, !IO)
    ;
        Cmd = ssdb_level,
        execute_ssdb_level(Args, Depth, NewDepth, !IO),
        read_and_execute_cmd(Event, NewDepth, WhatNext, !IO)
    ;
        Cmd = ssdb_current,
        execute_ssdb_current(Args, Event, !IO),
        read_and_execute_cmd(Event, Depth, WhatNext, !IO)
    ;
        Cmd = ssdb_format,
        execute_ssdb_format(Args, !IO),
        read_and_execute_cmd(Event, Depth, WhatNext, !IO)
    ;
        Cmd = ssdb_format_param,
        execute_ssdb_format_param(Args, !IO),
        read_and_execute_cmd(Event, Depth, WhatNext, !IO)
    ;
        Cmd = ssdb_break,
        execute_ssdb_break(Args, !IO),
        read_and_execute_cmd(Event, Depth, WhatNext, !IO)
    ;
        Cmd = ssdb_enable,
        execute_ssdb_enable(Args, !IO),
        read_and_execute_cmd(Event, Depth, WhatNext, !IO)
    ;
        Cmd = ssdb_disable,
        execute_ssdb_disable(Args, !IO),
        read_and_execute_cmd(Event, Depth, WhatNext, !IO)
    ;
        Cmd = ssdb_delete,
        execute_ssdb_delete(Args, !IO),
        read_and_execute_cmd(Event, Depth, WhatNext, !IO)
    ;
        Cmd = ssdb_quit,
        execute_ssdb_quit(Args, !IO),
        read_and_execute_cmd(Event, Depth, WhatNext, !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred execute_ssdb_help(list(string)::in, io::di, io::uo) is det.

execute_ssdb_help(Args, !IO) :-
    (
        Args = [],
        print_help(!IO)
    ;
        Args = [_ | _],
        % We should provide more detailed help if the user specifies a command
        % name.
        print_help(!IO)
    ).

%-----------------------------------------------------------------------------%

:- pred execute_ssdb_step(list(string)::in, ssdb_event_type::in,
    int::in, what_next::out, io::di, io::uo) is det.

execute_ssdb_step(Args, Event, Depth, WhatNext, !IO) :-
    (
        Args = [],
        WhatNext = wn_step
    ;
        Args = [_ | _],
        (
            Args = [NStr],
            string.to_int(NStr, N),
            N > 0
        ->
            get_cur_ssdb_event_number(EventNumber, !IO),
            WhatNext = wn_goto(EventNumber + N)
        ;
            % We should provide more detailed help.
            print_help(!IO),
            read_and_execute_cmd(Event, Depth, WhatNext, !IO)
        )
    ).

:- pred execute_ssdb_next(list(string)::in, ssdb_event_type::in,
    int::in, what_next::out, io::di, io::uo) is det.

execute_ssdb_next(Args, Event, Depth, WhatNext, !IO) :-
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
            read_and_execute_cmd(Event, Depth, WhatNext, !IO)
        )
    ;
        Args = [_ | _],
        % We should provide more detailed help.
        print_help(!IO),
        read_and_execute_cmd(Event, Depth, WhatNext, !IO)
    ).

:- pred execute_ssdb_goto(list(string)::in, ssdb_event_type::in,
    int::in, what_next::out, io::di, io::uo) is det.

execute_ssdb_goto(Args, Event, Depth, WhatNext, !IO) :-
    (
        Args = [],
        % We should provide more detailed help.
        print_help(!IO),
        read_and_execute_cmd(Event, Depth, WhatNext, !IO)
    ;
        Args = [EventNumToGoStr],
        ( string.to_int(EventNumToGoStr, EventNumToGo) ->
            get_cur_ssdb_event_number(CurEventNum, !IO),
            ( EventNumToGo > CurEventNum ->
                WhatNext = wn_goto(EventNumToGo)
            ;
                io.write_string("The debugger cannot go to a past event.\n",
                    !IO),
                read_and_execute_cmd(Event, Depth, WhatNext, !IO)
            )
        ;
            io.write_string("The event number to go to must be an integer.\n",
                !IO),
            read_and_execute_cmd(Event, Depth, WhatNext, !IO)
        )
    ;
        Args = [_, _ | _],
        % We should provide more detailed help.
        print_help(!IO),
        read_and_execute_cmd(Event, Depth, WhatNext, !IO)
    ).

:- pred execute_ssdb_continue(list(string)::in, ssdb_event_type::in,
    int::in, what_next::out, io::di, io::uo) is det.

execute_ssdb_continue(Args, Event, Depth, WhatNext, !IO) :-
    (
        Args = [],
        WhatNext = wn_continue
    ;
        Args = [_ | _],
        % We should provide more detailed help.
        print_help(!IO),
        read_and_execute_cmd(Event, Depth, WhatNext, !IO)
    ).

:- pred execute_ssdb_finish(list(string)::in, ssdb_event_type::in,
    int::in, what_next::out, io::di, io::uo) is det.

execute_ssdb_finish(Args, Event, Depth, WhatNext, !IO) :-
    (
        Args = [],
        (
            ( Event = ssdb_call
            ; Event = ssdb_call_nondet
            ; Event = ssdb_redo_nondet
            )
        ->
            stack_top(StackFrame, !IO),
            CSN = StackFrame ^  sf_csn,
            WhatNext = wn_finish(CSN)
        ;
            io.write_string("The `finish' command can be executed "
                ++ "only at a call or redo port.\n", !IO),
            read_and_execute_cmd(Event, Depth, WhatNext, !IO)
        )
    ;
        Args = [Arg],
        ( string.to_int(Arg, Num) ->
            stack_depth(CurDepth, !IO),
            (
                Num >= 0,
                Num < CurDepth
            ->
                stack_index(Num, StackFrame, !IO),
                CSN = StackFrame ^ sf_csn,
                WhatNext = wn_finish(CSN)
            ;
                io.format("The depth must be between 0 and %i.\n",
                    [i(CurDepth - 1)], !IO),
                read_and_execute_cmd(Event, Depth, WhatNext, !IO)
            )
        ;
            io.write_string("The depth must be an integer.\n", !IO),
            read_and_execute_cmd(Event, Depth, WhatNext, !IO)
        )
    ;
        Args = [_, _ | _],
        % We should provide more detailed help.
        print_help(!IO),
        read_and_execute_cmd(Event, Depth, WhatNext, !IO)
    ).

:- pred execute_ssdb_return(list(string)::in, ssdb_event_type::in,
    int::in, what_next::out, io::di, io::uo) is det.

execute_ssdb_return(Args, Event, Depth, WhatNext, !IO) :-
    (
        Args = [],
        (
            ( Event = ssdb_exit
            ; Event = ssdb_exit_nondet
            ),
            WhatNext = wn_return
        ;
            ( Event = ssdb_call
            ; Event = ssdb_fail
            ; Event = ssdb_call_nondet
            ; Event = ssdb_redo_nondet
            ; Event = ssdb_fail_nondet
            ; Event = ssdb_excp
            ),
            io.write_string("This command is a no-op from this port.\n", !IO),
            read_and_execute_cmd(Event, Depth, WhatNext, !IO)
        )
    ;
        Args = [_ | _],
        print_help(!IO),
        read_and_execute_cmd(Event, Depth, WhatNext, !IO)
    ).

:- pred execute_ssdb_exception(list(string)::in, ssdb_event_type::in,
    int::in, what_next::out, io::di, io::uo) is det.

execute_ssdb_exception(Args, Event, Depth, WhatNext, !IO) :-
    (
        Args = [],
        WhatNext = wn_exception
    ;
        Args = [_ | _],
        io.write_string("The exception command accepts no arguments.\n", !IO),
        read_and_execute_cmd(Event, Depth, WhatNext, !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred execute_ssdb_retry(list(string)::in, ssdb_event_type::in,
    int::in, what_next::out, io::di, io::uo) is det.

execute_ssdb_retry(Args, Event, Depth, WhatNext, !IO) :-
    % XXX: For some reason, the original code here handled the case of the
    % number argument being zero as if the command had no argument at all.
    (
        Args = [],
        execute_ssdb_retry_2(0, Event, Depth, WhatNext, !IO)
    ;
        Args = [Arg],
        ( string.to_int(Arg, Num) ->
            stack_depth(CurDepth, !IO),
            (
                Num >= 0,
                Num < CurDepth
            ->
                execute_ssdb_retry_2(Num, Event, Depth, WhatNext, !IO)
            ;
                io.format("The depth must be between 0 and %i.\n",
                    [i(CurDepth - 1)], !IO),
                read_and_execute_cmd(Event, Depth, WhatNext, !IO)
            )
        ;
            io.write_string("The depth must be an integer.\n", !IO),
            read_and_execute_cmd(Event, Depth, WhatNext, !IO)
        )
    ;
        Args = [_, _ | _],
        % We should provide more detailed help.
        print_help(!IO),
        read_and_execute_cmd(Event, Depth, WhatNext, !IO)
    ).

:- pred execute_ssdb_retry_2(int::in, ssdb_event_type::in, int::in,
    what_next::out, io::di, io::uo) is det.

execute_ssdb_retry_2(Num, Event, Depth, WhatNext, !IO) :-
    stack_index(Num, Frame, !IO),
    CSN = Frame ^ sf_csn,
    (
        ( Event = ssdb_exit
        ; Event = ssdb_fail
        ; Event = ssdb_fail_nondet
        ),
        WhatNext = wn_retry(CSN)
    ;
        Event = ssdb_exit_nondet,
        nondet_stack_contains_csn(CSN, Found, !IO),
        (
            Found = yes,
            WhatNext = wn_retry_nondet(CSN)
        ;
            Found = no,
            WhatNext = wn_retry(CSN)
        )
    ;
        ( Event = ssdb_call
        ; Event = ssdb_call_nondet
        ; Event = ssdb_redo_nondet
        ; Event = ssdb_excp
        ),
        io.write_string("Cannot retry at call or redo port.\n", !IO),
        read_and_execute_cmd(Event, Depth, WhatNext, !IO)
    ).

:- pred nondet_stack_contains_csn(int::in, bool::out, io::di, io::uo) is det.

nondet_stack_contains_csn(CSN, Contains, !IO) :-
    nondet_stack_depth(StackDepth, !IO),
    nondet_stack_contains_csn_2(CSN, StackDepth - 1, Contains, !IO).

:- pred nondet_stack_contains_csn_2(int::in, int::in, bool::out,
    io::di, io::uo) is det.

nondet_stack_contains_csn_2(CSN, Depth, Contains, !IO) :-
    ( Depth < 0 ->
        Contains = no
    ;
        nondet_stack_index(Depth, StackFrame, !IO),
        ( CSN = StackFrame ^ sf_csn ->
            Contains = yes
        ;
            nondet_stack_contains_csn_2(CSN, Depth - 1, Contains, !IO)
        )
    ).

%-----------------------------------------------------------------------------%

:- pred execute_ssdb_stack(list(string)::in, int::in, io::di, io::uo) is det.

execute_ssdb_stack(Args, Depth, !IO) :-
    (
        Args = [],
        print_stack_trace(0, Depth, int.max_int, !IO)
    ;
        Args = [_ | _],
        (
            Args = [NStr],
            string.to_int(NStr, N),
            N > 0
        ->
            print_stack_trace(0, Depth, N, !IO)
        ;
            io.write_string("ssdb: `stack' command expects integer argument\n",
                !IO)
        )
    ).

:- pred execute_ssdb_print(list(string)::in, int::in, io::di, io::uo) is det.

execute_ssdb_print(!.Args, Depth, !IO) :-
    process_options(print_options, !Args, no, Res),
    (
        Res = ok(MaybeFormat),
        stack_index(Depth, StackFrame, !IO),
        ( !.Args = [] ->
            Term = goal_to_synthetic_term(StackFrame),
            print_browser_term(MaybeFormat, print, Term, !IO)
        ; !.Args = ["*"] ->
            ListVarValue = StackFrame ^ sf_list_var_value,
            (
                ListVarValue = [],
                io.write_string("ssdb: there are no live variables.\n", !IO)
            ;
                ListVarValue = [_ | _],
                print_vars(MaybeFormat, print_all, ListVarValue, !IO)
            )
        ; !.Args = [Arg] ->
            ListVarValue = StackFrame ^ sf_list_var_value,
            print_var_with_name(MaybeFormat, ListVarValue, Arg, !IO)
        ;
            print_help(!IO)
        )
    ;
        Res = error(Error),
        io.write_string("ssdb: ", !IO),
        io.write_string(io.error_message(Error), !IO),
        io.nl(!IO)
    ).

:- pred print_options(string::in, maybe(portray_format)::in,
    maybe(portray_format)::out) is semidet.

print_options("--flat", _, yes(flat)).
print_options("--pretty", _, yes(pretty)).
print_options("--raw", _, yes(raw_pretty)).
print_options("--verbose", _, yes(verbose)).
print_options("-f", _, yes(flat)).
print_options("-p", _, yes(pretty)).
print_options("-r", _, yes(raw_pretty)).
print_options("-v", _, yes(verbose)).

:- func goal_to_synthetic_term(stack_frame) = browser_term.

goal_to_synthetic_term(StackFrame) = Term :-
    ProcId = StackFrame ^ sf_proc_id,
    ProcId = ssdb_proc_id(_ModuleName, ProcName),
    % XXX I/O state arguments at the end of this list will be missing.
    % This can be fixed once we have the procedure arity.
    make_arg_univs(StackFrame ^ sf_list_var_value, 0, ArgUnivs),
    % XXX We need to know if the procedure is a predicate or function.
    FuncReturn = no,
    Term = synthetic_term(ProcName, ArgUnivs, FuncReturn).

:- pred make_arg_univs(list(var_value)::in, int::in, list(univ)::out) is det.

make_arg_univs([], _, []).
make_arg_univs([Var | Vars], Pos, ArgUnivs) :-
    (
        Var = unbound_head_var(_, VarPos),
        ( VarPos = Pos ->
            make_arg_univs(Vars, Pos + 1, ArgUnivs0)
        ;
            make_arg_univs(Vars, Pos, ArgUnivs0)
        ),
        type_to_univ('_' : mdb.browse.unbound, Univ),
        ArgUnivs = [Univ | ArgUnivs0]
    ;
        Var = bound_head_var(_, VarPos, Value),
        ( VarPos = Pos ->
            make_arg_univs(Vars, Pos + 1, ArgUnivs0),
            type_to_univ(Value, Univ)
        ;
            make_arg_univs(Vars, Pos, ArgUnivs0),
            type_to_univ('_' : mdb.browse.unbound, Univ)
        ),
        ArgUnivs = [Univ | ArgUnivs0]
    ;
        Var = bound_other_var(_, _),
        make_arg_univs(Vars, Pos, ArgUnivs)
    ).

:- pred execute_ssdb_browse(list(string)::in, int::in, io::di, io::uo) is det.

execute_ssdb_browse(Args, Depth, !IO) :-
    stack_index(Depth, StackFrame, !IO),
    (
        Args = [],
        browse_term(goal_to_synthetic_term(StackFrame), !IO)
    ;
        Args = [VarName],
        ListVarValue = StackFrame ^ sf_list_var_value,
        browse_var(ListVarValue, VarName, !IO)
    ;
        Args = [_, _ | _],
        % We should provide more detailed help.
        print_help(!IO)
    ).

:- pred execute_ssdb_vars(list(string)::in, int::in, io::di, io::uo) is det.

execute_ssdb_vars(Args, Depth, !IO) :-
    (
        Args = [],
        stack_index(Depth, StackFrame, !IO),
        ListVarValue = StackFrame ^ sf_list_var_value,
        print_vars_list(ListVarValue, 1, !IO)
    ;
        Args = [_ | _],
        % We should provide more detailed help.
        print_help(!IO)
    ).

:- pred execute_ssdb_down(list(string)::in, int::in, int::out, io::di, io::uo)
    is det.

execute_ssdb_down(Args, Depth, NewDepth, !IO) :-
    (
        Args = [],
        DownDepth = Depth - 1,
        ( DownDepth >= 0 ->
            NewDepth = DownDepth,
            print_depth_change(NewDepth, !IO)
        ;
            io.write_string("Already at bottom stack frame.\n", !IO),
            NewDepth = Depth
        )
    ;
        Args = [_ | _],
        % We should provide more detailed help.
        print_help(!IO),
        NewDepth = Depth
    ).

:- pred execute_ssdb_up(list(string)::in, int::in, int::out, io::di, io::uo)
    is det.

execute_ssdb_up(Args, Depth, NewDepth, !IO) :-
    (
        Args = [],
        stack_depth(StackDepth, !IO),
        UpDepth = Depth + 1,
        ( UpDepth < StackDepth ->
            NewDepth = UpDepth,
            print_depth_change(NewDepth, !IO)
        ;
            io.write_string("Already at top stack frame.\n", !IO),
            NewDepth = Depth
        )
    ;
        Args = [_ | _],
        % We should provide more detailed help.
        print_help(!IO),
        NewDepth = Depth
    ).

:- pred execute_ssdb_level(list(string)::in, int::in, int::out, io::di, io::uo)
    is det.

execute_ssdb_level(Args, Depth, NewDepth, !IO) :-
    (
        Args = [NStr],
        string.to_int(NStr, N)
    ->
        stack_depth(StackDepth, !IO),
        (
            N >= 0,
            N < StackDepth
        ->
            NewDepth = N,
            print_depth_change(NewDepth, !IO)
        ;
            io.write_string("ssdb: invalid level\n", !IO),
            NewDepth = Depth
        )
    ;
        io.write_string("ssdb: `level' requires integer argument\n", !IO),
        NewDepth = Depth
    ).

:- pred execute_ssdb_current(list(string)::in, ssdb_event_type::in,
    io::di, io::uo) is det.

execute_ssdb_current(Args, Event, !IO) :-
    (
        Args = [],
        get_cur_ssdb_event_number(EventNum, !IO),
        print_event_info(Event, EventNum, !IO)
    ;
        Args = [_ | _],
        print_help(!IO)
    ).

%-----------------------------------------------------------------------------%

:- pred execute_ssdb_format(list(string)::in, io::di, io::uo) is det.

execute_ssdb_format(!.Args, !IO) :-
    Config0 = format_config(no, no, no, no, no, no, no),
    process_options(format_options, !Args, Config0, Res),
    (
        Res = ok(format_config(P, B, A, F, Pr, V, NPr)),
        (
            !.Args = [Word],
            is_portray_format(Word, Format)
        ->
            get_browser_state(State0, !IO),
            FromBrowser = no,
            set_browser_param(FromBrowser, P, B, A, F, Pr, V, NPr,
                setting_format(Format), State0, State),
            set_browser_state(State, !IO)
        ;
            print_help(!IO)
        )
    ;
        Res = error(Error),
        io.write_string("ssdb: ", !IO),
        io.write_string(io.error_message(Error), !IO),
        io.nl(!IO)
    ).

:- pred execute_ssdb_format_param(list(string)::in, io::di, io::uo) is det.

execute_ssdb_format_param(!.Args, !IO) :-
    Config0 = format_config(no, no, no, no, no, no, no),
    process_options(format_param_options, !Args, Config0, Res),
    (
        Res = ok(format_config(P, B, A, F, Pr, V, NPr)),
        ( format_param_setting(!.Args, Setting) ->
            get_browser_state(State0, !IO),
            FromBrowser = no,
            set_browser_param(FromBrowser, P, B, A, F, Pr, V, NPr, Setting,
                State0, State),
            set_browser_state(State, !IO)
        ;
            print_help(!IO)
        )
    ;
        Res = error(Error),
        io.write_string("ssdb: ", !IO),
        io.write_string(io.error_message(Error), !IO),
        io.nl(!IO)
    ).

:- type format_config
    --->    format_config(
                print       :: bool,
                browse      :: bool,
                print_all   :: bool,
                f           :: bool,
                r           :: bool,
                v           :: bool,
                p           :: bool
            ).

:- pred format_options(string::in, format_config::in, format_config::out)
    is semidet.

format_options(Opt, !Config) :-
    (
        ( Opt = "-P"
        ; Opt = "--print"
        ),
        !Config ^ print := yes
    ;
        ( Opt = "-B"
        ; Opt = "--browse"
        ),
        !Config ^ browse := yes
    ;
        ( Opt = "-A"
        ; Opt = "--print-all"
        ),
        !Config ^ print_all := yes
    ).

:- pred format_param_options(string::in, format_config::in, format_config::out)
    is semidet.

format_param_options(Opt, !Config) :-
    (
        ( Opt = "-P"
        ; Opt = "--print"
        ),
        !Config ^ print := yes
    ;
        ( Opt = "-B"
        ; Opt = "--browse"
        ),
        !Config ^ browse := yes
    ;
        ( Opt = "-A"
        ; Opt = "--print-all"
        ),
        !Config ^ print_all := yes
    ;
        ( Opt = "-f"
        ; Opt = "--flat"
        ),
        !Config ^ f := yes
    ;
        ( Opt = "-r"
        ; Opt = "--raw"
        ),
        !Config ^ r := yes
    ;
        ( Opt = "-v"
        ; Opt = "--verbose"
        ),
        !Config ^ v := yes
    ;
        ( Opt = "-p"
        ; Opt = "--pretty"
        ),
        !Config ^ p := yes
    ).

:- pred is_portray_format(string, portray_format).
:- mode is_portray_format(in, out) is semidet.
:- mode is_portray_format(out, in) is det.

is_portray_format("flat", flat).
is_portray_format("raw_pretty", raw_pretty).
is_portray_format("verbose", verbose).
is_portray_format("pretty", pretty).

:- pred format_param_setting(list(string)::in, browser_info.setting::out)
    is semidet.

format_param_setting([Word, ValueStr], Setting) :-
    string.to_int(ValueStr, Value),
    (
        Word = "depth",
        Setting = setting_depth(Value)
    ;
        Word = "size",
        Setting = setting_size(Value)
    ;
        Word = "width",
        Setting = setting_width(Value)
    ;
        Word = "lines",
        Setting = setting_lines(Value)
    ).

%-----------------------------------------------------------------------------%

:- pred execute_ssdb_break(list(string)::in, io::di, io::uo) is det.

execute_ssdb_break(Args, !IO) :-
    (
        Args = [],
        % We should provide more detailed help.
        print_help(!IO)
    ;
        Args = [Arg],
        ( Arg = "info" ->
            get_cur_ssdb_breakpoints(BreakPoints, !IO),
            print_breakpoints(BreakPoints, !IO)
        ;
            % We should provide more detailed help.
            print_help(!IO)
        )
    ;
        Args = [ModuleName, ProcName],
        get_cur_ssdb_breakpoints(BreakPoints0, !IO),
        Key = pair(ModuleName, ProcName),
        ( map.contains(BreakPoints0, Key) ->
            io.write_string("The breakpoint already exists.\n", !IO)
        ;
            get_cur_ssdb_number_of_breakpoint(Number, !IO),
            NewBreakPoint = breakpoint(Number + 1, ModuleName, ProcName,
                bp_state_enabled),
            map.det_insert(BreakPoints0, Key, NewBreakPoint, BreakPoints),
            set_cur_ssdb_breakpoints(BreakPoints, !IO),
            set_cur_ssdb_number_of_breakpoint(Number + 1, !IO),
            print_breakpoint(NewBreakPoint, !IO)
        )
    ;
        Args = [_, _, _ | _],
        % We should provide more detailed help.
        print_help(!IO)
    ).

:- pred execute_ssdb_enable(list(string)::in, io::di, io::uo) is det.

execute_ssdb_enable(Args, !IO) :-
    (
        Args = [],
        % We should provide more detailed help.
        print_help(!IO)
    ;
        Args = [Arg],
        ( Arg = "*" ->
            modify_breakpoint_states(bp_state_enabled, !IO)
        ; string.to_int(Arg, Num) ->
            modify_breakpoint_state(Num, bp_state_enabled, !IO)
        ;
            % We should provide more detailed help.
            print_help(!IO)
        )
    ;
        Args = [_, _ | _],
        % We should provide more detailed help.
        print_help(!IO)
    ).

:- pred execute_ssdb_disable(list(string)::in, io::di, io::uo) is det.

execute_ssdb_disable(Args, !IO) :-
    (
        Args = [],
        % We should provide more detailed help.
        print_help(!IO)
    ;
        Args = [Arg],
        ( Arg = "*" ->
            modify_breakpoint_states(bp_state_disabled, !IO)
        ; string.to_int(Arg, Num) ->
            modify_breakpoint_state(Num, bp_state_disabled, !IO)
        ;
            io.write_string("The number must be an integer\n", !IO),
            % We should provide more detailed help.
            print_help(!IO)
        )
    ;
        Args = [_, _ | _],
        % We should provide more detailed help.
        print_help(!IO)
    ).

:- pred execute_ssdb_delete(list(string)::in, io::di, io::uo) is det.

execute_ssdb_delete(Args, !IO) :-
    (
        Args = [],
        % We should provide more detailed help.
        print_help(!IO)
    ;
        Args = [Arg],
        ( Arg = "*" ->
            set_cur_ssdb_breakpoints(map.init, !IO),
            io.write_string("All breakpoints have been deleted.\n", !IO)
        ; string.to_int(Arg, Num) ->
            delete_breakpoint(Num, !IO)
        ;
            io.write_string("The number must be an integer\n", !IO)
        )
    ;
        Args = [_, _ | _],
        % We should provide more detailed help.
        print_help(!IO)
    ).

%-----------------------------------------------------------------------------%

:- pred execute_ssdb_quit(list(string)::in, io::di, io::uo) is det.

execute_ssdb_quit(Args, !IO) :-
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
                exit_process(!IO)
            ;
                true
            )
        ;
            Result = eof,
            exit_process(!IO)
        ;
            Result = error(_Error),
            exit_process(!IO)
        )
    ;
        Args = [_ | _],
        ( Args = ["-y"] ->
            exit_process(!IO)
        ;
            print_help(!IO)
        )
    ).

%-----------------------------------------------------------------------------%

    % Disable or enable all breakpoints.
    %
:- pred modify_breakpoint_states(bp_state::in, io::di, io::uo) is det.

modify_breakpoint_states(State, !IO) :-
    get_cur_ssdb_breakpoints(BreakPoints0, !IO),
    SetState = (func(BP) = BP ^ bp_state := State),
    map.map_values_only(SetState, BreakPoints0) = BreakPoints,
    set_cur_ssdb_breakpoints(BreakPoints, !IO),
    print_breakpoints(BreakPoints, !IO).

    % modify_state_breakpoint_with_num(State, Num, !IO).
    %
    % Modify the state of the breakpoint with the number which match Num.
    %
:- pred modify_breakpoint_state(int::in, bp_state::in, io::di, io::uo) is det.

modify_breakpoint_state(Num, State, !IO) :-
    get_cur_ssdb_breakpoints(BreakPoints0, !IO),
    ( find_breakpoint(BreakPoints0, Num, Key, BreakPoint0) ->
        BreakPoint = BreakPoint0 ^ bp_state := State,
        map.det_update(BreakPoints0, Key, BreakPoint, BreakPoints),
        set_cur_ssdb_breakpoints(BreakPoints, !IO),
        print_breakpoint(BreakPoint, !IO)
    ;
        io.format("ssdb: break point #%d does not exist.\n", [i(Num)], !IO)
    ).

    % delete_breakpoint(Num, !IO).
    %
    % Delete the breakpoint that match with Num.
    %
:- pred delete_breakpoint(int::in, io::di, io::uo) is det.

delete_breakpoint(Num, !IO) :-
    get_cur_ssdb_breakpoints(BreakPoints0, !IO),
    ( find_breakpoint(BreakPoints0, Num, Key, _BreakPoint) ->
        map.delete(BreakPoints0, Key, BreakPoints),
        set_cur_ssdb_breakpoints(BreakPoints, !IO),
        Key = ModuleName - PredName,
        io.format("Breakpoint on %s.%s deleted.\n",
            [s(ModuleName), s(PredName)], !IO)
    ;
        io.format("ssdb: break point #%d does not exist.\n", [i(Num)], !IO)
    ).

    % find_breakpoint(BreakPoints, Num, Key, BreakPoint)
    %
    % Return the breakpoint with the given id number.
    %
:- pred find_breakpoint(cur_ssdb_breakpoints::in, int::in,
    pair(string, string)::out, breakpoint::out) is semidet.

find_breakpoint(BreakPoints, Num, Key, BreakPoint) :-
    % Breakpoints have unique integer ids so there is at most one solution.
    promise_equivalent_solutions [Key, BreakPoint] (
        map.member(BreakPoints, Key, BreakPoint),
        BreakPoint ^ bp_number = Num
    ).

%----------------------------------------------------------------------------%

    % Print the current information at this event point.
    %
:- pred print_event_info(ssdb_event_type::in, int::in, io::di, io::uo) is det.

print_event_info(Event, EventNum, !IO) :-
    stack_top(StackFrame, !IO),
    CSN = StackFrame ^ sf_csn,
    ProcId = StackFrame ^ sf_proc_id,
    PrintDepth = StackFrame ^ sf_depth,
    SiteFile = StackFrame ^ sf_call_site_file,
    SiteLine = StackFrame ^ sf_call_site_line,

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
    ;
        Event = ssdb_excp,
        io.write_string("EXCP", !IO)
    ),
    io.write_string(" ", !IO),
    % mdb writes pred/func here.
    io.write_string(ProcId ^ module_name, !IO),
    io.write_string(".", !IO),
    io.write_string(ProcId ^ proc_name, !IO),
    % mdb writes arity, mode, determinism and context here.
    io.format(" (%s:%d)\n", [s(SiteFile), i(SiteLine)], !IO).

%-----------------------------------------------------------------------------%

:- pred print_depth_change(int::in, io::di, io::uo) is det.

print_depth_change(Depth, !IO) :-
    io.format("Ancestor level set to %d:\n", [i(Depth)], !IO),
    stack_index(Depth, StackFrame, !IO),
    stack_depth(StackDepth, !IO),
    print_frame_info(StackFrame, StackDepth, !IO).

    % print_frame_info(Frame, StackDepth, !IO).
    %
    % Print the information of the frame gave in argument.
    %
:- pred print_frame_info(stack_frame::in, int::in, io::di, io::uo) is det.

print_frame_info(StackFrame, StackDepth, !IO) :-
    Depth = StackFrame ^ sf_depth,
    ProcId = StackFrame ^ sf_proc_id,
    ProcId = ssdb_proc_id(ModuleName, ProcName),
    SiteFile = StackFrame ^ sf_call_site_file,
    SiteLine = StackFrame ^ sf_call_site_line,
    RevDepth = StackDepth - Depth,
    io.format("%4d  %s.%s (%s:%d)\n",
        [i(RevDepth), s(ModuleName), s(ProcName), s(SiteFile), i(SiteLine)],
        !IO).

%-----------------------------------------------------------------------------%

:- pred print_stack_trace(int::in, int::in, int::in, io::di, io::uo) is det.

print_stack_trace(CurLevel, StarDepth, RemainingLines, !IO) :-
    stack_depth(StackDepth, !IO),
    (
        RemainingLines = 0,
        CurLevel < StackDepth - 1
    ->
        io.write_string("<more stack frames snipped>\n", !IO)
    ;
        CurLevel < StackDepth
    ->
        stack_index(CurLevel, CurFrame, !IO),
        compress_stack_frames(CurFrame, StackDepth, CurLevel, NextLevel, !IO),
        SkippedFrames = NextLevel - CurLevel,
        (
            StarDepth >= CurLevel,
            StarDepth < NextLevel
        ->
            Star = ('*')
        ;
            Star = (' ')
        ),
        print_stack_frame(Star, CurLevel, CurFrame, SkippedFrames, !IO),
        print_stack_trace(CurLevel + SkippedFrames, StarDepth,
            RemainingLines - 1, !IO)
    ;
        true
    ).

:- pred compress_stack_frames(stack_frame::in, int::in, int::in, int::out,
    io::di, io::uo) is det.

compress_stack_frames(RefFrame, StackDepth, Level, NextLevel, !IO) :-
    ( Level < StackDepth ->
        stack_index(Level, Frame, !IO),
        ( RefFrame ^ sf_proc_id = Frame ^ sf_proc_id ->
            compress_stack_frames(RefFrame, StackDepth, Level + 1, NextLevel,
                !IO)
        ;
            NextLevel = Level
        )
    ;
        NextLevel = Level
    ).

:- pred print_stack_frame(char::in, int::in, stack_frame::in, int::in,
    io::di, io::uo) is det.

print_stack_frame(Star, Level, Frame, SkippedFrames, !IO) :-
    Module = Frame ^ sf_proc_id ^ module_name,
    Procedure = Frame ^ sf_proc_id ^ proc_name,
    SiteFile = Frame ^ sf_call_site_file,
    SiteLine = Frame ^ sf_call_site_line,
    io.format("%c%4d", [c(Star), i(Level)], !IO),
    ( SkippedFrames > 1 ->
        io.format("%5d*", [i(SkippedFrames)], !IO),
        Etc = " and others"
    ;
        io.write_string("      ", !IO),
        Etc = ""
    ),
    io.format(" %s.%s (%s:%d%s)\n",
        [s(Module), s(Procedure), s(SiteFile), i(SiteLine), s(Etc)], !IO).

%-----------------------------------------------------------------------------%

    % Print the given list of variables and their values, if bound.
    %
:- pred print_vars(maybe(portray_format)::in, browse_caller_type::in,
    list(var_value)::in, io::di, io::uo) is det.

print_vars(MaybeFormat, CallerType, Vars, !IO) :-
    list.foldl(print_var(MaybeFormat, CallerType), Vars, !IO).

:- pred print_var_with_name(maybe(portray_format)::in, list(var_value)::in,
    string::in, io::di, io::uo) is det.

print_var_with_name(MaybeFormat, VarDescs, VarName, !IO) :-
    (
        string.to_int(VarName, VarNum),
        VarNum > 0
    ->
        print_var_with_number(MaybeFormat, VarDescs, VarNum, !IO)
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
            print_vars(MaybeFormat, print, MatchVars, !IO)
        )
    ).

:- pred print_var_with_number(maybe(portray_format)::in, list(var_value)::in,
    int::in, io::di, io::uo) is det.

print_var_with_number(MaybeFormat, VarDescs, VarNum, !IO) :-
    ( list.index1(VarDescs, VarNum, VarDesc) ->
        print_var(MaybeFormat, print, VarDesc, !IO)
    ;
        io.write_string("ssdb: there aren't that many variables.\n", !IO)
    ).

:- pred print_var(maybe(portray_format)::in, browse_caller_type::in,
    var_value::in, io::di, io::uo) is det.

print_var(MaybeFormat, CallerType, VarValue, !IO) :-
    (
        VarValue = unbound_head_var(Name, Pos),
        print_var_prelude(Name, Pos, !IO),
        io.write_string("_\n", !IO)
    ;
        VarValue = bound_head_var(Name, Pos, T),
        % print_var_prelude(Name, Pos, !IO),
        ( Pos >= 0 ->
            Prefix = string.format("\t%s (arg %d)\t", [s(Name), i(Pos + 1)])
        ;
            Prefix = string.format("\t%s\t", [s(Name)])
        ),
        safe_write(MaybeFormat, CallerType, Prefix, T, !IO)
    ;
        VarValue = bound_other_var(Name, T),
        Prefix = string.format("\t%s\t", [s(Name)]),
        safe_write(MaybeFormat, CallerType, Prefix, T, !IO)
    ).

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

:- pred safe_write(maybe(portray_format)::in, browse_caller_type::in,
    string::in, T::in, io::di, io::uo) is det.

safe_write(MaybeFormat, CallerType, Prefix, T, !IO) :-
    ( safe_to_write(T) ->
        io.write_string(Prefix, !IO),
        type_to_univ(T, Univ),
        print_browser_term(MaybeFormat, CallerType, plain_term(Univ), !IO)
    ;
        io.write_string(Prefix, !IO),
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

:- pred print_browser_term(maybe(portray_format)::in, browse_caller_type::in,
    browser_term::in, io::di, io::uo) is det.

print_browser_term(MaybeFormat, CallerType, Term, !IO) :-
    io.stdout_stream(StdOut, !IO),
    get_browser_state(State, !IO),
    promise_equivalent_solutions [!:IO] (
        (
            MaybeFormat = yes(Format),
            print_browser_term_format(Term, StdOut, CallerType, Format, State,
                !IO)
        ;
            MaybeFormat = no,
            print_browser_term(Term, StdOut, CallerType, State, !IO)
        )
    ).

:- func get_var_name(var_value) = string.

get_var_name(unbound_head_var(Name, _)) = Name.
get_var_name(bound_head_var(Name, _, _)) = Name.
get_var_name(bound_other_var(Name, _)) = Name.

%-----------------------------------------------------------------------------%

:- pred browse_var(list(var_value)::in, string::in, io::di, io::uo) is det.

browse_var(ListVarValue, VarName, !IO) :-
    (
        string.to_int(VarName, VarNum),
        VarNum > 0
    ->
        ( list.index1(ListVarValue, VarNum, VarValue) ->
            (
                VarValue = bound_head_var(_, _, Value),
                type_to_univ(Value, Univ),
                browse_term(plain_term(Univ), !IO)
            ;
                VarValue = bound_other_var(_, Value),
                type_to_univ(Value, Univ),
                browse_term(plain_term(Univ), !IO)
            ;
                VarValue = unbound_head_var(_, _),
                io.write_string("ssdb: the variable is unbound.\n", !IO)
            )
        ;
            io.write_string("ssdb: there aren't that many variables.\n", !IO)
        )
    ;
        list_var_value_to_assoc_list(ListVarValue, VarDescs),
        assoc_list.search(VarDescs, VarName, Univ)
    ->
        browse_term(plain_term(Univ), !IO)
    ;
        io.write_string("ssdb: there is no such variable.\n", !IO)
    ).

:- pred browse_term(browser_term::in, io::di, io::uo) is det.

browse_term(Term, !IO) :-
    io.stdin_stream(StdIn, !IO),
    io.stdout_stream(StdOut, !IO),
    get_browser_state(State0, !IO),
    promise_equivalent_solutions [State, !:IO] (
        browse.browse_browser_term_no_modes(Term, StdIn, StdOut, _,
            State0, State, !IO)
    ),
    set_browser_state(State, !IO).

%-----------------------------------------------------------------------------%

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
:- pred print_breakpoints(cur_ssdb_breakpoints::in, io::di, io::uo) is det.

print_breakpoints(BreakPoints, !IO) :-
    ( map.is_empty(BreakPoints) ->
        io.write_string("There are no break points.\n", !IO)
    ;
        % This relies on the integer id being the first field.
        list.sort(map.values(BreakPoints), SortedBreakPoints),
        list.foldl(print_breakpoint, SortedBreakPoints, !IO)
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

%-----------------------------------------------------------------------------%

    % Print a summary of the commands.
    %
:- pred print_help(io::di, io::uo) is det.

print_help(!IO) :-
    Lines = [
        "step [NUM] (s, default)",
        "next (n)",
        "goto NUM (g)",
        "continue (c)",
        "exception (e)",
        "retry [NUM] (r)",
        "print [-fprv] (p)",
        "print [-fprv] VAR|NUM",
        "print [-fprv] * (P)",
        "browse VAR|NUM",
        "vars (v)",
        "stack [NUM] (st)",
        "up (u)",
        "down (d)",
        "level NUM (lv)",
        "current (cur)",
        "format [-APB] flat|raw_pretty|pretty|verbose",
        "format_param [-APBfpv] depth|size|width|lines NUM",
        "break MODULE PRED (b)",
        "break info",
        "enable NUM|*",
        "disable NUM|*",
        "delete NUM|*",
        "help (h)",
        "quit [-y] (q)"
    ],
    io.write_list(Lines, "\n", io.write_string, !IO),
    io.write_string("\n\n", !IO).

%-----------------------------------------------------------------------------%

:- pred process_options(pred(string, T, T)::in(pred(in, in, out) is semidet),
    list(string)::in, list(string)::out, T::in, io.res(T)::out) is det.

process_options(Handler, Args0, Args, Data0, Res) :-
    (
        Args0 = [],
        Args = [],
        Res = ok(Data0)
    ;
        Args0 = [First | Rest],
        ( string.prefix(First, "--") ->
            ( Handler(First, Data0, Data1) ->
                process_options(Handler, Rest, Args, Data1, Res)
            ;
                Message = "unrecognised option `" ++ First ++ "'",
                Res = error(io.make_io_error(Message)),
                Args = Args0
            )
        ;
            string.prefix(First, "-"),
            string.to_char_list(First, [_ | FirstChars]),
            FirstChars = [_ | _]
        ->
            process_short_options(Handler, FirstChars, Data0, Res1),
            (
                Res1 = ok(Data1),
                process_options(Handler, Rest, Args, Data1, Res)
            ;
                Res1 = error(_),
                Res = Res1,
                Args = Args0
            )
        ;
            process_options(Handler, Rest, Rest1, Data0, Res),
            Args = [First | Rest1]
        )
    ).

:- pred process_short_options(
    pred(string, T, T)::in(pred(in, in, out) is semidet), list(char)::in,
    T::in, io.res(T)::out) is det.

process_short_options(Handler, Chars, Data0, Res) :-
    (
        Chars = [],
        Res = ok(Data0)
    ;
        Chars = [C | Cs],
        Option = string.from_char_list(['-', C]),
        ( Handler(Option, Data0, Data1) ->
            process_short_options(Handler, Cs, Data1, Res)
        ;
            Message = "unrecognised option `" ++ Option ++ "'",
            Res = error(io.make_io_error(Message))
        )
    ).

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

%-----------------------------------------------------------------------------%

:- pred exit_process(io::di, io::uo) is det.

:- pragma foreign_proc("C",
    exit_process(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    exit(0);
    IO = IO0;
").

:- pragma foreign_proc("Java",
    exit_process(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    System.exit(0);
").

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%
