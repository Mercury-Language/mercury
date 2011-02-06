%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2007, 2010 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This tests for a bug that was noticed with --deep-profiling and
% --profile-for-implicit-parallelism when compiling ssdb/ssdb.m
%
%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- module bug180.
:- interface.

:- import_module int.
:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------------%

:- type ssdb_proc_id
    --->    ssdb_proc_id(
                module_name :: string,
                proc_name   :: string
            ).

:- type ssdb_event_type
    --->    ssdb_call
    ;       ssdb_exit
    ;       ssdb_call_nondet
    ;       ssdb_exit_nondet.

:- type ssdb_retry
    --->    do_retry
    ;       do_not_retry.

:- type list_var_value == list(var_value).

:- type var_value
    --->    unbound_head_var(var_name, pos).

:- type var_name == string.
:- type pos == int.

:- impure pred handle_event_call(ssdb_proc_id::in, list_var_value::in) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module io.

%-----------------------------------------------------------------------------%

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

:- type what_next
    --->    wn_step
    ;       wn_continue.

:- type next_stop
    --->    ns_step
    ;       ns_next(int).

:- inst either_call
    --->    ssdb_call
    ;       ssdb_call_nondet.

:- type search_path == list(path_name).
:- type path_name  == string.

%----------------------------------------------------------------------------%

:- mutable(cur_ssdb_event_number, int, 0, ground,
    [untrailed, attach_to_io_state]).

:- mutable(nondet_shadow_stack, list(stack_frame), [], ground,
    [untrailed, attach_to_io_state]).

%-----------------------------------------------------------------------------%

    % This is thread-local to allow debugging of the initial thread in
    % multi-threaded programs.  As thread-local mutables inherit their values
    % from the parent thread, the user must temporarily disable debugging while
    % the child thread is created, using `pause_debugging'.
    %
:- mutable(debugger_state, debugger_state, debugger_off, ground,
    [untrailed, thread_local, attach_to_io_state]).

%-----------------------------------------------------------------------------%

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

:- pred handle_event_call_2(ssdb_event_type::in(either_call), ssdb_proc_id::in,
    list(var_value)::in, io::di, io::uo) is det.

:- pragma inline(handle_event_call_2/5).

handle_event_call_2(Event, ProcId, ListVarValue, !IO) :-
    get_cur_ssdb_event_number(EventNum0, !IO),
    EventNum = EventNum0 + 1,
    set_cur_ssdb_event_number(EventNum, !IO),

    % Push the new stack frame on top of the shadow stack(s).
    StackFrame = stack_frame(EventNum, 0, 0, ProcId, "", 0,
        ListVarValue),
    (
        Event = ssdb_call
    ;
        Event = ssdb_call_nondet,
        get_nondet_shadow_stack(NondetStack, !IO),
        set_nondet_shadow_stack([StackFrame | NondetStack], !IO)
    ),

    should_stop_at_this_event(Event, EventNum, 5, ProcId, Stop, _AutoRetry,
        !IO),
    (
        Stop = yes,
        print_event_info(Event, EventNum, !IO)
    ;
        Stop = no
    ).

:- pragma no_inline(should_stop_at_this_event/8).
:- pred should_stop_at_this_event(ssdb_event_type::in, int::in, int::in,
    ssdb_proc_id::in, bool::out, ssdb_retry::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % Print the current information at this event point.
    %
:- pragma no_inline(print_event_info/4).
:- pred print_event_info(ssdb_event_type::in, int::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- pragma inline(invent_io/1).
:- impure pred invent_io(io::uo) is det.

invent_io(IO) :-
    private_builtin.unsafe_type_cast(0, IO0),
    unsafe_promise_unique(IO0, IO),
    impure impure_true.

:- pragma inline(consume_io/1).
:- impure pred consume_io(io::di) is det.

consume_io(_) :-
    impure impure_true.

%-----------------------------------------------------------------------------%
