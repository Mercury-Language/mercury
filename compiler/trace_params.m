%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2008,2011-2012 The University of Melbourne.
% Copyright (C) 2014-2015,2019-2020,2022 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: trace_params.m.
% Author: zs.
%
% This module defines the parameters of execution tracing at various trace
% levels and with various settings of the --suppress-trace option.
%
% In most cases the trace level we want to apply to a procedure (which is its
% effective trace level) is the same as the global trace level. However, if the
% global trace level is shallow, then we optimize the handling of procedures
% that cannot be called from deep traced contexts. If a procedure is neither
% exported nor has its address taken, then it can only be called from other
% procedures in its module. If the module is shallow traced, this guarantees
% that we will never get any events from the procedure, so there is no point
% in including any tracing code in it in the first place. We therefore make
% its effective trace level "none" for must purposes (the purposes whose
% functions test effective trace levels). Apart from avoiding the overhead
% of calls to MR_trace, this also allows the code generator to preserve tail
% recursion optimization. However, we continue to generate the data structures
% that enable the debugger to walk the stack for such procedures. We accomplish
% this by making the relevant test work on the global trace level, not
% effective trace levels. Most of the other functions defined in this module
% convert the given (global) trace level into the effective trace level of
% the relevant procedure before calculating their result.
%
%-----------------------------------------------------------------------------%

:- module libs.trace_params.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.

:- import_module bool.
:- import_module maybe.

%-----------------------------------------------------------------------------%

    % The global trace level for a module.
    %
:- type trace_level.

    % The effective trace level for a procedure.
    %
:- type eff_trace_level.

    % There are some parts of the functionality of the debugger that are
    % enabled by default, but which are not always strictly necessary,
    % and can be suppressed. Values of this type specify exactly what parts
    % should be suppressed.
    %
:- type trace_suppress_items.

    % The string should be the value of the --trace-level option;
    % two bools should be the values of the `--require-tracing' and
    % `--decl-debug' grade options.
    %
    % If the string is an acceptable trace level in the specified kinds of
    % grades, return yes wrapper around the trace level.
    %
    % If the string is an known trace level that happens not to be
    % acceptable in the specified kinds of grades, return no.
    %
    % If the string is not known trace level, fail.
    %
:- pred convert_trace_level(string::in, bool::in, bool::in,
    maybe(trace_level)::out) is semidet.

:- pred convert_trace_suppress(string::in, trace_suppress_items::out)
    is semidet.
:- func default_trace_suppress = trace_suppress_items.

%-----------------------------------------------------------------------------%

:- func eff_trace_level_for_proc(module_info, pred_info, proc_info,
    trace_level) = eff_trace_level.

    % Given a trace level for a module, return the trace level we should use
    % for compiler-generated unify, index and compare predicates.
    %
:- func trace_level_for_unify_compare(trace_level) = trace_level.

%-----------------------------------------------------------------------------%

:- func trace_level_none = trace_level.
:- func eff_trace_level_none = eff_trace_level.

:- func at_least_at_shallow(eff_trace_level) = bool.
:- func at_least_at_deep(eff_trace_level) = bool.

%-----------------------------------------------------------------------------%

:- type maybe_exec_trace_enabled
    --->    exec_trace_is_not_enabled
    ;       exec_trace_is_enabled.

    % These functions check for various properties of either the global
    % or the effective trace level.
    %
:- func is_exec_trace_enabled_at_given_trace_level(trace_level) =
    maybe_exec_trace_enabled.
:- func is_exec_trace_enabled_at_eff_trace_level(eff_trace_level) =
    maybe_exec_trace_enabled.
:- func eff_trace_level_needs_fail_vars(eff_trace_level) = bool.
:- func eff_trace_level_needs_fixed_slots(eff_trace_level) = bool.
:- func eff_trace_level_needs_from_full_slot(eff_trace_level) = bool.
:- func eff_trace_level_allows_delay_death(eff_trace_level) = bool.
:- func eff_trace_needs_return_info(eff_trace_level, trace_suppress_items)
    = bool.
:- func trace_level_allows_tail_rec(trace_level) = bool.
:- func eff_trace_level_needs_meaningful_var_names(eff_trace_level) = bool.
:- func eff_trace_needs_all_var_names(eff_trace_level, trace_suppress_items)
    = bool.
:- func trace_needs_proc_body_reps(trace_level, trace_suppress_items) = bool.
:- func eff_trace_needs_proc_body_reps(eff_trace_level, trace_suppress_items)
    = bool.

%-----------------------------------------------------------------------------%

    % This is used to represent the trace level in the module layout
    % and in proc layouts in C code.
    %
:- func trace_level_rep(trace_level) = string.
:- func eff_trace_level_rep(eff_trace_level) = string.

    % This is used to represent the trace level in HLDS dumps.
    %
:- func eff_trace_level_dump(eff_trace_level) = string.

:- func eff_trace_level_needs_port(eff_trace_level, trace_suppress_items,
    trace_port) = bool.

:- func encode_suppressed_events(trace_suppress_items) = int.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.status.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module set.
:- import_module string.

%-----------------------------------------------------------------------------%

% The trace levels none, shallow, deep and decl_rep correspond to the similarly
% named options.
%
% The effective trace level for a procedure usually corresponds to the
% global trace level, but can be different in several circumstances.
%
% The most obvious one is that non-exported procedures in a module that is
% being compiled with shallow tracing normally need no tracing at all,
% and thus their effective trace level is none.
%
% However, there is an exception to this exception in the presence of
% user events.
%
% Basic_user is the effective trace level for procedures in shallow traced
% modules that contain a user defined event. This event requires, among other
% things, the preservation of variables in the procedure in which it occurs.
% It also requires the transmission of depth information through all procedures
% in the module that otherwise wouldn't be traced, which is what trace level
% basic does.
%
% In theory, in a shallow traced module, we could set the trace level of
% a procedure to none if that procedure is not the ancestor of any procedure
% containing a user event. However, that test cannot be implemented while
% retaining separate compilation, given that the call trees of procedures
% may cross module boundaries, and, in particular, may cross out of this module
% and then back again through a different entry point.

:- type trace_level
    --->    none
    ;       shallow
    ;       deep
    ;       decl_rep.

:- type eff_trace_level
    --->    eff_none
    ;       eff_basic
    ;       eff_basic_user
    ;       eff_shallow
    ;       eff_deep
    ;       eff_decl_rep.

:- type trace_suppress_item
    --->    suppress_port(trace_port)
    ;       suppress_return_info
    ;       suppress_all_var_names
    ;       suppress_proc_body_reps.

:- type trace_suppress_items == set(trace_suppress_item).

%-----------------------------------------------------------------------------%

convert_trace_level("minimum", no,  no,  yes(none)).
convert_trace_level("minimum", yes, no,  yes(shallow)).
convert_trace_level("minimum", _,   yes, yes(decl_rep)).
convert_trace_level("shallow", _,   no,  yes(shallow)).
convert_trace_level("shallow", _,   yes, no).
convert_trace_level("deep",    _,   no,  yes(deep)).
convert_trace_level("deep",    _,   yes, no).
convert_trace_level("decl",    _,   _,   yes(decl_rep)).
convert_trace_level("rep",     _,   _,   yes(decl_rep)).
convert_trace_level("default", no,  no,  yes(none)).
convert_trace_level("default", yes, no,  yes(deep)).
convert_trace_level("default", _,   yes, yes(decl_rep)).

convert_trace_suppress(SuppressString, SuppressItemSet) :-
    SuppressWords = string.words_separator(char_is_comma, SuppressString),
    list.map(convert_item_name, SuppressWords, SuppressItemLists),
    list.condense(SuppressItemLists, SuppressItems),
    set.list_to_set(SuppressItems, SuppressItemSet).

:- pred char_is_comma(char::in) is semidet.

char_is_comma(',').

:- pred convert_item_name(string::in, list(trace_suppress_item)::out)
    is semidet.

convert_item_name(String, Names) :-
    ( if convert_port_name(String) = PortName then
        Names = [suppress_port(PortName)]
    else if convert_port_class_name(String) = PortNames then
        list.map(wrap_port, PortNames, Names)
    else if convert_other_name(String) = OtherName then
        Names = [OtherName]
    else
        fail
    ).

:- pred wrap_port(trace_port::in, trace_suppress_item::out) is det.

wrap_port(Port, suppress_port(Port)).

:- func convert_port_name(string) = trace_port is semidet.

    % The call port cannot be disabled, because its layout structure is
    % referred to implicitly by the redo command in mdb.
    %
    % The exception port cannot be disabled, because it is never put into
    % compiler-generated code in the first place; such events are created
    % on the fly by library/exception.m.
% convert_port_name("call") = port_call.
convert_port_name("exit") = port_exit.
convert_port_name("fail") = port_fail.
convert_port_name("redo") = port_redo.
% convert_port_name("excp") = port_exception.
convert_port_name("exception") = port_exception.
convert_port_name("cond") = port_ite_cond.
convert_port_name("ite_cond") = port_ite_cond.
convert_port_name("then") = port_ite_then.
convert_port_name("ite_then") = port_ite_then.
convert_port_name("else") = port_ite_else.
convert_port_name("ite_else") = port_ite_else.
convert_port_name("nege") = port_neg_enter.
convert_port_name("neg_enter") = port_neg_enter.
convert_port_name("negs") = port_neg_success.
convert_port_name("neg_success") = port_neg_success.
convert_port_name("negf") = port_neg_failure.
convert_port_name("neg_failure") = port_neg_failure.
convert_port_name("swtc") = port_switch.
convert_port_name("switch") = port_switch.
convert_port_name("disj_first") = port_disj_first.
convert_port_name("disj_later") = port_disj_later.
convert_port_name("tail") = port_tailrec_call.
convert_port_name("user") = port_user.

:- func convert_port_class_name(string) = list(trace_port) is semidet.

convert_port_class_name("interface") =
    [port_call, port_exit, port_redo, port_fail, port_exception].
convert_port_class_name("internal") =
    [port_ite_then, port_ite_else, port_switch,
    port_disj_first, port_disj_later].
convert_port_class_name("context") =
    [port_ite_cond, port_neg_enter, port_neg_success, port_neg_failure].

:- func convert_other_name(string) = trace_suppress_item is semidet.

convert_other_name("return") = suppress_return_info.
convert_other_name("return_info") = suppress_return_info.
convert_other_name("names") = suppress_all_var_names.
convert_other_name("all_var_names") = suppress_all_var_names.
convert_other_name("bodies") = suppress_proc_body_reps.
convert_other_name("proc_body_reps") = suppress_proc_body_reps.

default_trace_suppress = set.init.

%-----------------------------------------------------------------------------%

eff_trace_level_for_proc(ModuleInfo, PredInfo, ProcInfo, TraceLevel)
        = EffTraceLevel :-
    ( if TraceLevel = none then
        EffTraceLevel = eff_none
    else
        pred_info_get_origin(PredInfo, Origin),
        (
            Origin = origin_special_pred(SpecialPred, _),
            % Unify and compare predicates can be called from the generic
            % unify and compare predicates in builtin.m, so they can be called
            % from outside this module even if they don't have their address
            % taken.
            %
            % Index predicates can never be called from anywhere except
            % the compare predicate.
            %
            % Initialise predicates invoke user-provided code. Whether that
            % code has debugging enabled or not, there is no point in
            % generating events in the initialise predicate itself.
            (
                SpecialPred = spec_pred_unify,
                EffTraceLevel = eff_shallow
            ;
                SpecialPred = spec_pred_compare,
                EffTraceLevel = eff_shallow
            ;
                SpecialPred = spec_pred_index,
                EffTraceLevel = eff_none
            )
        ;
            Origin = origin_transformed(Transform, _, _),
            ( if Transform = transform_io_tabling then
                % Predicates called by a predicate that is I/O tabled
                % should not be traced. If such a predicate were allowed
                % to generate events, then the event numbers of events
                % after the I/O primitive would be different between
                % the first and subsequent (idempotent) executions
                % of the same I/O action.
                EffTraceLevel = eff_none
            else
                EffTraceLevel = usual_eff_trace_level_for_proc(ModuleInfo,
                    PredInfo, ProcInfo, TraceLevel)
            )
        ;
            ( Origin = origin_user(_, _, _)
            ; Origin = origin_instance_method(_, _)
            ; Origin = origin_class_method(_, _)
            ; Origin = origin_deforestation(_, _)
            ; Origin = origin_assertion(_, _)
            ; Origin = origin_lambda(_, _, _)
            ; Origin = origin_solver_repn(_, _)
            ; Origin = origin_tabling(_, _)
            ; Origin = origin_mutable(_, _, _)
            ; Origin = origin_initialise
            ; Origin = origin_finalise
            ),
            EffTraceLevel = usual_eff_trace_level_for_proc(ModuleInfo,
                PredInfo, ProcInfo, TraceLevel)
        )
    ).

:- func usual_eff_trace_level_for_proc(module_info, pred_info, proc_info,
    trace_level) = eff_trace_level.

usual_eff_trace_level_for_proc(ModuleInfo, PredInfo, ProcInfo, TraceLevel)
        = EffTraceLevel :-
    (
        TraceLevel = none,
        EffTraceLevel = eff_none
    ;
        TraceLevel = shallow,
        pred_info_get_status(PredInfo, PredStatus),
        IsExported = pred_status_is_exported(PredStatus),
        proc_info_get_is_address_taken(ProcInfo, AddressTaken),
        ( if
            IsExported = no,
            AddressTaken = address_is_not_taken
        then
            proc_info_get_has_user_event(ProcInfo, ProcHasUserEvent),
            (
                ProcHasUserEvent = has_user_event,
                EffTraceLevel = eff_basic_user
            ;
                ProcHasUserEvent = has_no_user_event,
                module_info_get_has_user_event(ModuleInfo, ModuleHasUserEvent),
                (
                    ModuleHasUserEvent = has_user_event,
                    EffTraceLevel = eff_basic
                ;
                    ModuleHasUserEvent = has_no_user_event,
                    EffTraceLevel = eff_none
                )
            )
        else
            EffTraceLevel = eff_shallow
        )
    ;
        TraceLevel = deep,
        EffTraceLevel = eff_deep
    ;
        TraceLevel = decl_rep,
        EffTraceLevel = eff_decl_rep
    ).

trace_level_for_unify_compare(none) = none.
trace_level_for_unify_compare(shallow) = shallow.
trace_level_for_unify_compare(deep) = shallow.
trace_level_for_unify_compare(decl_rep) = shallow.

%-----------------------------------------------------------------------------%

trace_level_none = none.

eff_trace_level_none = eff_none.

at_least_at_shallow(eff_none) = no.
at_least_at_shallow(eff_basic) = no.
at_least_at_shallow(eff_basic_user) = no.
at_least_at_shallow(eff_shallow) = yes.
at_least_at_shallow(eff_deep) = yes.
at_least_at_shallow(eff_decl_rep) = yes.

at_least_at_deep(eff_none) = no.
at_least_at_deep(eff_basic) = no.
at_least_at_deep(eff_basic_user) = no.
at_least_at_deep(eff_shallow) = no.
at_least_at_deep(eff_deep) = yes.
at_least_at_deep(eff_decl_rep) = yes.

%-----------------------------------------------------------------------------%

is_exec_trace_enabled_at_given_trace_level(TraceLevel) = Enabled :-
    (
        TraceLevel = none,
        Enabled = exec_trace_is_not_enabled
    ;
        ( TraceLevel = shallow
        ; TraceLevel = deep
        ; TraceLevel = decl_rep
        ),
        Enabled = exec_trace_is_enabled
    ).

is_exec_trace_enabled_at_eff_trace_level(EffTraceLevel) = Enabled :-
    (
        EffTraceLevel = eff_none,
        Enabled = exec_trace_is_not_enabled
    ;
        ( EffTraceLevel = eff_basic
        ; EffTraceLevel = eff_basic_user
        ; EffTraceLevel = eff_shallow
        ; EffTraceLevel = eff_deep
        ; EffTraceLevel = eff_decl_rep
        ),
        Enabled = exec_trace_is_enabled
    ).

eff_trace_level_needs_fail_vars(EffTraceLevel) = NeedsFailVars :-
    (
        ( EffTraceLevel = eff_none
        ; EffTraceLevel = eff_basic
        ),
        NeedsFailVars = no
    ;
        ( EffTraceLevel = eff_basic_user
        ; EffTraceLevel = eff_shallow
        ; EffTraceLevel = eff_deep
        ; EffTraceLevel = eff_decl_rep
        ),
        NeedsFailVars = yes
    ).

eff_trace_level_needs_fixed_slots(EffTraceLevel) = NeedsFixedSlots :-
    (
        EffTraceLevel = eff_none,
        NeedsFixedSlots = no
    ;
        ( EffTraceLevel = eff_basic
        ; EffTraceLevel = eff_basic_user
        ; EffTraceLevel = eff_shallow
        ; EffTraceLevel = eff_deep
        ; EffTraceLevel = eff_decl_rep
        ),
        NeedsFixedSlots = yes
    ).

eff_trace_level_needs_from_full_slot(EffTraceLevel) = NeedsFromFullSlot :-
    (
        ( EffTraceLevel = eff_none
        ; EffTraceLevel = eff_basic
        ; EffTraceLevel = eff_basic_user
        ; EffTraceLevel = eff_deep
        ; EffTraceLevel = eff_decl_rep
        ),
        NeedsFromFullSlot = no
    ;
        EffTraceLevel = eff_shallow,
        NeedsFromFullSlot = yes
    ).

eff_trace_level_allows_delay_death(EffTraceLevel) = AllowsDelayDeath :-
    (
        ( EffTraceLevel = eff_none
        ; EffTraceLevel = eff_basic
        ; EffTraceLevel = eff_shallow
        ),
        AllowsDelayDeath = no
    ;
        ( EffTraceLevel = eff_basic_user
        ; EffTraceLevel = eff_deep
        ; EffTraceLevel = eff_decl_rep
        ),
        AllowsDelayDeath = yes
    ).

eff_trace_needs_return_info(EffTraceLevel, TraceSuppressItems) = NeedReturn :-
    ( if
        eff_trace_level_has_return_info(EffTraceLevel) = yes,
        not set.member(suppress_return_info, TraceSuppressItems)
    then
        NeedReturn = yes
    else
        NeedReturn = no
    ).

trace_level_allows_tail_rec(none) = yes.
trace_level_allows_tail_rec(shallow) = no.
trace_level_allows_tail_rec(deep) = yes.
trace_level_allows_tail_rec(decl_rep) = no.

eff_trace_level_needs_meaningful_var_names(eff_none) = no.
eff_trace_level_needs_meaningful_var_names(eff_basic) = no.
eff_trace_level_needs_meaningful_var_names(eff_basic_user) = yes.
eff_trace_level_needs_meaningful_var_names(eff_shallow) = no.
eff_trace_level_needs_meaningful_var_names(eff_deep) = yes.
eff_trace_level_needs_meaningful_var_names(eff_decl_rep) = yes.

eff_trace_needs_all_var_names(EffTraceLevel, TraceSuppressItems) = NeedNames :-
    ( if
        eff_trace_level_has_all_var_names(EffTraceLevel) = yes,
        not set.member(suppress_all_var_names, TraceSuppressItems)
    then
        NeedNames = yes
    else
        NeedNames = no
    ).

trace_needs_proc_body_reps(TraceLevel, TraceSuppressItems) = NeedBody :-
    ( if
        trace_level_has_proc_body_reps(TraceLevel) = yes,
        not set.member(suppress_proc_body_reps, TraceSuppressItems)
    then
        NeedBody = yes
    else
        NeedBody = no
    ).

eff_trace_needs_proc_body_reps(EffTraceLevel, TraceSuppressItems) = NeedBody :-
    ( if
        eff_trace_level_has_proc_body_reps(EffTraceLevel) = yes,
        not set.member(suppress_proc_body_reps, TraceSuppressItems)
    then
        NeedBody = yes
    else
        NeedBody = no
    ).

%-----------------------------------------------------------------------------%

:- func eff_trace_level_has_return_info(eff_trace_level) = bool.

eff_trace_level_has_return_info(eff_none) = no.
eff_trace_level_has_return_info(eff_basic) = yes.
eff_trace_level_has_return_info(eff_basic_user) = yes.
eff_trace_level_has_return_info(eff_shallow) = yes.
eff_trace_level_has_return_info(eff_deep) = yes.
eff_trace_level_has_return_info(eff_decl_rep) = yes.

:- func eff_trace_level_has_all_var_names(eff_trace_level) = bool.

eff_trace_level_has_all_var_names(eff_none) = no.
eff_trace_level_has_all_var_names(eff_basic) = no.
eff_trace_level_has_all_var_names(eff_basic_user) = no.
eff_trace_level_has_all_var_names(eff_shallow) = no.
eff_trace_level_has_all_var_names(eff_deep) = no.
eff_trace_level_has_all_var_names(eff_decl_rep) = yes.

:- func trace_level_has_proc_body_reps(trace_level) = bool.

trace_level_has_proc_body_reps(none) = no.
trace_level_has_proc_body_reps(shallow) = no.
trace_level_has_proc_body_reps(deep) = no.
trace_level_has_proc_body_reps(decl_rep) = yes.

:- func eff_trace_level_has_proc_body_reps(eff_trace_level) = bool.

eff_trace_level_has_proc_body_reps(eff_none) = no.
eff_trace_level_has_proc_body_reps(eff_basic) = no.
eff_trace_level_has_proc_body_reps(eff_basic_user) = no.
eff_trace_level_has_proc_body_reps(eff_shallow) = no.
eff_trace_level_has_proc_body_reps(eff_deep) = no.
eff_trace_level_has_proc_body_reps(eff_decl_rep) = yes.

    % If this is modified, then the corresponding code below
    % and in runtime/mercury_stack_layout.h needs to be updated.
trace_level_rep(none)       = "MR_TRACE_LEVEL_NONE".
trace_level_rep(shallow)    = "MR_TRACE_LEVEL_SHALLOW".
trace_level_rep(deep)       = "MR_TRACE_LEVEL_DEEP".
trace_level_rep(decl_rep)   = "MR_TRACE_LEVEL_DECL_REP".

    % If this is modified, then the corresponding code above
    % and in runtime/mercury_stack_layout.h needs to be updated.
eff_trace_level_rep(eff_none)       = "MR_TRACE_LEVEL_NONE".
eff_trace_level_rep(eff_basic)      = "MR_TRACE_LEVEL_BASIC".
eff_trace_level_rep(eff_basic_user) = "MR_TRACE_LEVEL_BASIC_USER".
eff_trace_level_rep(eff_shallow)    = "MR_TRACE_LEVEL_SHALLOW".
eff_trace_level_rep(eff_deep)       = "MR_TRACE_LEVEL_DEEP".
eff_trace_level_rep(eff_decl_rep)   = "MR_TRACE_LEVEL_DECL_REP".

eff_trace_level_dump(eff_none)       = "none".
eff_trace_level_dump(eff_basic)      = "basic".
eff_trace_level_dump(eff_basic_user) = "basic_user".
eff_trace_level_dump(eff_shallow)    = "shallow".
eff_trace_level_dump(eff_deep)       = "deep".
eff_trace_level_dump(eff_decl_rep)   = "decl_rep".

%-----------------------------------------------------------------------------%

:- type port_category
    --->    port_cat_interface
            % The events that describe the interface of a procedure
            % with its callers.

    ;       port_cat_internal
            % The events inside each procedure that were present
            % in the initial procedural debugger.

    ;       port_cat_context
            % The events inside each procedure that we added because
            % the declarative debugger needs to know when (potentially)
            % negated contexts start and end.

    ;       port_cat_user.
            % User defined events.

:- func trace_port_category(trace_port) = port_category.

trace_port_category(port_call)                = port_cat_interface.
trace_port_category(port_exit)                = port_cat_interface.
trace_port_category(port_fail)                = port_cat_interface.
trace_port_category(port_redo)                = port_cat_interface.
trace_port_category(port_exception)           = port_cat_interface.
trace_port_category(port_ite_cond)            = port_cat_context.
trace_port_category(port_ite_then)            = port_cat_internal.
trace_port_category(port_ite_else)            = port_cat_internal.
trace_port_category(port_neg_enter)           = port_cat_context.
trace_port_category(port_neg_success)         = port_cat_context.
trace_port_category(port_neg_failure)         = port_cat_context.
trace_port_category(port_switch)              = port_cat_internal.
trace_port_category(port_disj_first)          = port_cat_internal.
trace_port_category(port_disj_later)          = port_cat_internal.
trace_port_category(port_tailrec_call)        = port_cat_interface.
trace_port_category(port_user)                = port_cat_user.

:- func eff_trace_level_port_categories(eff_trace_level) = list(port_category).

eff_trace_level_port_categories(eff_none) = [].
eff_trace_level_port_categories(eff_basic) = [].
eff_trace_level_port_categories(eff_basic_user) = [port_cat_user].
eff_trace_level_port_categories(eff_shallow) = [port_cat_interface].
eff_trace_level_port_categories(eff_deep) =
    [port_cat_interface, port_cat_internal, port_cat_context, port_cat_user].
eff_trace_level_port_categories(eff_decl_rep) =
    [port_cat_interface, port_cat_internal, port_cat_context, port_cat_user].

:- func eff_trace_level_allows_port_suppression(eff_trace_level) = bool.

eff_trace_level_allows_port_suppression(eff_none) = no.     % no ports exist
eff_trace_level_allows_port_suppression(eff_basic) = yes.
eff_trace_level_allows_port_suppression(eff_basic_user) = yes.
eff_trace_level_allows_port_suppression(eff_shallow) = yes.
eff_trace_level_allows_port_suppression(eff_deep) = yes.
eff_trace_level_allows_port_suppression(eff_decl_rep) = no.

eff_trace_level_needs_port(EffTraceLevel, TraceSuppressItems, Port)
        = NeedsPort :-
    ( if
        trace_port_category(Port) = Category,
        list.member(Category, eff_trace_level_port_categories(EffTraceLevel)),
        not (
            eff_trace_level_allows_port_suppression(EffTraceLevel) = yes,
            set.member(suppress_port(Port), TraceSuppressItems)
        )
    then
        NeedsPort = yes
    else
        NeedsPort = no
    ).

encode_suppressed_events(SuppressedEvents) = SuppressedEventsInt :-
    set.fold(maybe_add_suppressed_event, SuppressedEvents,
        0, SuppressedEventsInt).

:- pred maybe_add_suppressed_event(trace_suppress_item::in, int::in, int::out)
    is det.

maybe_add_suppressed_event(SuppressItem, SuppressedEventsInt0,
        SuppressedEventsInt) :-
    (
        SuppressItem = suppress_port(Port),
        SuppressedEventsInt = SuppressedEventsInt0 \/ (1 << port_number(Port))
    ;
        ( SuppressItem = suppress_return_info
        ; SuppressItem = suppress_all_var_names
        ; SuppressItem = suppress_proc_body_reps
        ),
        SuppressedEventsInt = SuppressedEventsInt0
    ).

:- func port_number(trace_port) = int.

port_number(port_call) = 0.
port_number(port_exit) = 1.
port_number(port_redo) = 2.
port_number(port_fail) = 3.
port_number(port_tailrec_call) = 4.
port_number(port_exception) = 5.
port_number(port_ite_cond) = 6.
port_number(port_ite_then) = 7.
port_number(port_ite_else) = 8.
port_number(port_neg_enter) = 9.
port_number(port_neg_success) = 10.
port_number(port_neg_failure) = 11.
port_number(port_disj_first) = 12.
port_number(port_disj_later) = 13.
port_number(port_switch) = 14.
port_number(port_user) = 15.

%-----------------------------------------------------------------------------%
:- end_module libs.trace_params.
%-----------------------------------------------------------------------------%
