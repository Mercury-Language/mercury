%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2008,2011-2012 The University of Melbourne.
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

:- type trace_level.
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

:- func eff_trace_level(module_info, pred_info, proc_info, trace_level)
    = trace_level.

    % Given a trace level for a module, return the trace level we should use
    % for compiler-generated unify, index and compare predicates.
    %
:- func trace_level_for_unify_compare(trace_level) = trace_level.

%-----------------------------------------------------------------------------%

:- func trace_level_none = trace_level.

:- func at_least_at_shallow(trace_level) = bool.
:- func at_least_at_deep(trace_level) = bool.

%-----------------------------------------------------------------------------%

:- type maybe_exec_trace_enabled
    --->    exec_trace_is_not_enabled
    ;       exec_trace_is_enabled.

    % These functions check for various properties of the global trace level.
    %
:- func is_exec_trace_enabled_at_given_trace_level(trace_level) =
    maybe_exec_trace_enabled.
:- func trace_level_allows_delay_death(trace_level) = bool.
:- func trace_needs_return_info(trace_level, trace_suppress_items) = bool.
:- func trace_level_allows_tail_rec(trace_level) = bool.

    % Should optimization passes maintain meaningful variable names
    % where possible.
    %
:- func trace_level_needs_meaningful_var_names(trace_level) = bool.

    % This function checks for a property of the global trace level.
    %
:- func trace_needs_proc_body_reps(trace_level, trace_suppress_items) = bool.

    % These functions check for various properties of the given procedure's
    % effective trace level.
    %
:- func is_exec_trace_enabled_at_eff_trace_level(module_info,
    pred_info, proc_info, trace_level) = maybe_exec_trace_enabled.
:- func eff_trace_level_needs_input_vars(module_info, pred_info, proc_info,
    trace_level) = bool.
:- func eff_trace_level_needs_fail_vars(module_info, pred_info, proc_info,
    trace_level) = bool.
:- func eff_trace_level_needs_fixed_slots(module_info, pred_info, proc_info,
    trace_level) = bool.
:- func eff_trace_level_needs_from_full_slot(module_info, pred_info, proc_info,
    trace_level) = bool.
:- func eff_trace_needs_all_var_names(module_info, pred_info, proc_info,
    trace_level, trace_suppress_items) = bool.
:- func eff_trace_needs_proc_body_reps(module_info, pred_info, proc_info,
    trace_level, trace_suppress_items) = bool.
:- func eff_trace_needs_port(module_info, pred_info, proc_info, trace_level,
    trace_suppress_items, trace_port) = bool.

%-----------------------------------------------------------------------------%

    % This is used to represent the trace level in the module layout
    % and in proc layouts.
    %
:- func trace_level_rep(trace_level) = string.

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
% named options. The trace levels basic and basic_user cannot be specified on
% the command line; they can only be effective trace levels.
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
% containing a user event. However, that test is not one that can be
% implemented easily or at all, given that the call trees of procedures may
% cross module boundaries, and, in particular, may cross out of this module
% and then back again through a different entry point.

:- type trace_level
    --->    none
    ;       basic
    ;       basic_user
    ;       shallow
    ;       deep
    ;       decl_rep.

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

eff_trace_level(ModuleInfo, PredInfo, ProcInfo, TraceLevel) = EffTraceLevel :-
    ( if TraceLevel = none then
        EffTraceLevel = none
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
                EffTraceLevel = shallow
            ;
                SpecialPred = spec_pred_compare,
                EffTraceLevel = shallow
            ;
                SpecialPred = spec_pred_index,
                EffTraceLevel = none
            )
        ;
            Origin = origin_created(PredCreation),
            (
                PredCreation = created_by_io_tabling,
                % Predicates called by a predicate that is I/O tabled
                % should not be traced. If such a predicate were allowed
                % to generate events, then the event numbers of events
                % after the I/O primitive would be different between
                % the first and subsequent (idempotent) executions
                % of the same I/O action.
                EffTraceLevel = none
            ;
                PredCreation = created_by_deforestation,
                EffTraceLevel = usual_eff_trace_level(ModuleInfo,
                    PredInfo, ProcInfo, TraceLevel)
            )
        ;
            ( Origin = origin_instance_method(_, _)
            ; Origin = origin_class_method(_, _)
            ; Origin = origin_transformed(_, _, _)
            ; Origin = origin_assertion(_, _)
            ; Origin = origin_lambda(_, _, _)
            ; Origin = origin_solver_type(_, _, _)
            ; Origin = origin_tabling(_, _)
            ; Origin = origin_mutable(_, _, _)
            ; Origin = origin_initialise
            ; Origin = origin_finalise
            ; Origin = origin_user(_)
            ),
            EffTraceLevel = usual_eff_trace_level(ModuleInfo,
                PredInfo, ProcInfo, TraceLevel)
        )
    ).

:- func usual_eff_trace_level(module_info, pred_info, proc_info, trace_level)
    = trace_level.

usual_eff_trace_level(ModuleInfo, PredInfo, ProcInfo, TraceLevel)
        = EffTraceLevel :-
    pred_info_get_status(PredInfo, PredStatus),
    ( if
        TraceLevel = shallow,
        pred_status_is_exported(PredStatus) = no,
        proc_info_get_is_address_taken(ProcInfo, address_is_not_taken)
    then
        proc_info_get_has_user_event(ProcInfo, ProcHasUserEvent),
        (
            ProcHasUserEvent = has_user_event,
            EffTraceLevel = basic_user
        ;
            ProcHasUserEvent = has_no_user_event,
            module_info_get_has_user_event(ModuleInfo, ModuleHasUserEvent),
            (
                ModuleHasUserEvent = has_user_event,
                EffTraceLevel = basic
            ;
                ModuleHasUserEvent = has_no_user_event,
                EffTraceLevel = none
            )
        )
    else
        EffTraceLevel = TraceLevel
    ).

trace_level_for_unify_compare(none) = none.
trace_level_for_unify_compare(basic) = none.
trace_level_for_unify_compare(basic_user) = none.
trace_level_for_unify_compare(shallow) = shallow.
trace_level_for_unify_compare(deep) = shallow.
trace_level_for_unify_compare(decl_rep) = shallow.

%-----------------------------------------------------------------------------%

trace_level_none = none.

at_least_at_shallow(none) = no.
at_least_at_shallow(basic) = no.
at_least_at_shallow(basic_user) = no.
at_least_at_shallow(shallow) = yes.
at_least_at_shallow(deep) = yes.
at_least_at_shallow(decl_rep) = yes.

at_least_at_deep(none) = no.
at_least_at_deep(basic) = no.
at_least_at_deep(basic_user) = no.
at_least_at_deep(shallow) = no.
at_least_at_deep(deep) = yes.
at_least_at_deep(decl_rep) = yes.

%-----------------------------------------------------------------------------%

is_exec_trace_enabled_at_given_trace_level(none) = exec_trace_is_not_enabled.
is_exec_trace_enabled_at_given_trace_level(basic) = exec_trace_is_enabled.
is_exec_trace_enabled_at_given_trace_level(basic_user) = exec_trace_is_enabled.
is_exec_trace_enabled_at_given_trace_level(shallow) = exec_trace_is_enabled.
is_exec_trace_enabled_at_given_trace_level(deep) = exec_trace_is_enabled.
is_exec_trace_enabled_at_given_trace_level(decl_rep) = exec_trace_is_enabled.

:- func trace_level_needs_input_vars(trace_level) = bool.

trace_level_needs_input_vars(none) = no.
trace_level_needs_input_vars(basic) = no.
trace_level_needs_input_vars(basic_user) = no.
trace_level_needs_input_vars(shallow) = yes.
trace_level_needs_input_vars(deep) = yes.
trace_level_needs_input_vars(decl_rep) = yes.

:- func trace_level_needs_fail_vars(trace_level) = bool.

trace_level_needs_fail_vars(none) = no.
trace_level_needs_fail_vars(basic) = no.
trace_level_needs_fail_vars(basic_user) = yes.
trace_level_needs_fail_vars(shallow) = yes.
trace_level_needs_fail_vars(deep) = yes.
trace_level_needs_fail_vars(decl_rep) = yes.

:- func trace_level_needs_fixed_slots(trace_level) = bool.

trace_level_needs_fixed_slots(none) = no.
trace_level_needs_fixed_slots(basic) = yes.
trace_level_needs_fixed_slots(basic_user) = yes.
trace_level_needs_fixed_slots(shallow) = yes.
trace_level_needs_fixed_slots(deep) = yes.
trace_level_needs_fixed_slots(decl_rep) = yes.

:- func trace_level_needs_from_full_slot(trace_level) = bool.

trace_level_needs_from_full_slot(none) = no.
trace_level_needs_from_full_slot(basic) = no.
trace_level_needs_from_full_slot(basic_user) = no.
trace_level_needs_from_full_slot(shallow) = yes.
trace_level_needs_from_full_slot(deep) = no.
trace_level_needs_from_full_slot(decl_rep) = no.

trace_level_allows_delay_death(none) = no.
trace_level_allows_delay_death(basic) = no.
trace_level_allows_delay_death(basic_user) = yes.
trace_level_allows_delay_death(shallow) = no.
trace_level_allows_delay_death(deep) = yes.
trace_level_allows_delay_death(decl_rep) = yes.

trace_needs_return_info(TraceLevel, TraceSuppressItems) = Need :-
    ( if
        trace_level_has_return_info(TraceLevel) = yes,
        not set.member(suppress_return_info, TraceSuppressItems)
    then
        Need = yes
    else
        Need = no
    ).

trace_level_allows_tail_rec(none) = yes.
trace_level_allows_tail_rec(basic) = yes.
trace_level_allows_tail_rec(basic_user) = yes.
trace_level_allows_tail_rec(shallow) = no.
trace_level_allows_tail_rec(deep) = yes.
trace_level_allows_tail_rec(decl_rep) = no.

trace_level_needs_meaningful_var_names(none) = no.
trace_level_needs_meaningful_var_names(basic) = no.
trace_level_needs_meaningful_var_names(basic_user) = yes.
trace_level_needs_meaningful_var_names(shallow) = no.
trace_level_needs_meaningful_var_names(deep) = yes.
trace_level_needs_meaningful_var_names(decl_rep) = yes.

:- func trace_needs_all_var_names(trace_level, trace_suppress_items) = bool.

trace_needs_all_var_names(TraceLevel, TraceSuppressItems) = Need :-
    ( if
        trace_level_has_all_var_names(TraceLevel) = yes,
        not set.member(suppress_all_var_names, TraceSuppressItems)
    then
        Need = yes
    else
        Need = no
    ).

trace_needs_proc_body_reps(TraceLevel, TraceSuppressItems) = Need :-
    ( if
        trace_level_has_proc_body_reps(TraceLevel) = yes,
        not set.member(suppress_proc_body_reps, TraceSuppressItems)
    then
        Need = yes
    else
        Need = no
    ).

%-----------------------------------------------------------------------------%

is_exec_trace_enabled_at_eff_trace_level(ModuleInfo, PredInfo, ProcInfo,
        TraceLevel) =
    is_exec_trace_enabled_at_given_trace_level(
        eff_trace_level(ModuleInfo, PredInfo, ProcInfo, TraceLevel)).

eff_trace_level_needs_input_vars(ModuleInfo, PredInfo, ProcInfo, TraceLevel) =
    trace_level_needs_input_vars(
        eff_trace_level(ModuleInfo, PredInfo, ProcInfo, TraceLevel)).

eff_trace_level_needs_fail_vars(ModuleInfo, PredInfo, ProcInfo, TraceLevel) =
    trace_level_needs_fail_vars(
        eff_trace_level(ModuleInfo, PredInfo, ProcInfo, TraceLevel)).

eff_trace_level_needs_fixed_slots(ModuleInfo, PredInfo, ProcInfo, TraceLevel) =
    trace_level_needs_fixed_slots(
        eff_trace_level(ModuleInfo, PredInfo, ProcInfo, TraceLevel)).

eff_trace_level_needs_from_full_slot(ModuleInfo, PredInfo, ProcInfo,
        TraceLevel) =
    trace_level_needs_from_full_slot(
        eff_trace_level(ModuleInfo, PredInfo, ProcInfo, TraceLevel)).

eff_trace_needs_all_var_names(ModuleInfo, PredInfo, ProcInfo, TraceLevel,
        SuppressItems) =
    trace_needs_all_var_names(
        eff_trace_level(ModuleInfo, PredInfo, ProcInfo, TraceLevel),
        SuppressItems).

eff_trace_needs_proc_body_reps(ModuleInfo, PredInfo, ProcInfo, TraceLevel,
        SuppressItems) =
    trace_needs_proc_body_reps(
        eff_trace_level(ModuleInfo, PredInfo, ProcInfo, TraceLevel),
        SuppressItems).

eff_trace_needs_port(ModuleInfo, PredInfo, ProcInfo, TraceLevel, SuppressItems,
        Port) =
    trace_needs_port(
        eff_trace_level(ModuleInfo, PredInfo, ProcInfo, TraceLevel),
        SuppressItems, Port).

%-----------------------------------------------------------------------------%

:- func trace_level_has_return_info(trace_level) = bool.

trace_level_has_return_info(none) = no.
trace_level_has_return_info(basic) = yes.
trace_level_has_return_info(basic_user) = yes.
trace_level_has_return_info(shallow) = yes.
trace_level_has_return_info(deep) = yes.
trace_level_has_return_info(decl_rep) = yes.

:- func trace_level_has_all_var_names(trace_level) = bool.

trace_level_has_all_var_names(none) = no.
trace_level_has_all_var_names(basic) = no.
trace_level_has_all_var_names(basic_user) = no.
trace_level_has_all_var_names(shallow) = no.
trace_level_has_all_var_names(deep) = no.
trace_level_has_all_var_names(decl_rep) = yes.

:- func trace_level_has_proc_body_reps(trace_level) = bool.

trace_level_has_proc_body_reps(none) = no.
trace_level_has_proc_body_reps(basic) = no.
trace_level_has_proc_body_reps(basic_user) = no.
trace_level_has_proc_body_reps(shallow) = no.
trace_level_has_proc_body_reps(deep) = no.
trace_level_has_proc_body_reps(decl_rep) = yes.

    % If this is modified, then the corresponding code in
    % runtime/mercury_stack_layout.h needs to be updated.
trace_level_rep(none)       = "MR_TRACE_LEVEL_NONE".
trace_level_rep(basic)      = "MR_TRACE_LEVEL_BASIC".
trace_level_rep(basic_user) = "MR_TRACE_LEVEL_BASIC_USER".
trace_level_rep(shallow)    = "MR_TRACE_LEVEL_SHALLOW".
trace_level_rep(deep)       = "MR_TRACE_LEVEL_DEEP".
trace_level_rep(decl_rep)   = "MR_TRACE_LEVEL_DECL_REP".

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

:- func trace_level_port_categories(trace_level) = list(port_category).

trace_level_port_categories(none) = [].
trace_level_port_categories(basic) = [].
trace_level_port_categories(basic_user) = [port_cat_user].
trace_level_port_categories(shallow) = [port_cat_interface].
trace_level_port_categories(deep) =
    [port_cat_interface, port_cat_internal, port_cat_context, port_cat_user].
trace_level_port_categories(decl_rep) =
    [port_cat_interface, port_cat_internal, port_cat_context, port_cat_user].

:- func trace_level_allows_port_suppression(trace_level) = bool.

trace_level_allows_port_suppression(none) = no.     % no ports exist
trace_level_allows_port_suppression(basic) = yes.
trace_level_allows_port_suppression(basic_user) = yes.
trace_level_allows_port_suppression(shallow) = yes.
trace_level_allows_port_suppression(deep) = yes.
trace_level_allows_port_suppression(decl_rep) = no.

:- func trace_needs_port(trace_level, trace_suppress_items, trace_port) = bool.

trace_needs_port(TraceLevel, TraceSuppressItems, Port) = NeedsPort :-
    ( if
        trace_port_category(Port) = Category,
        list.member(Category, trace_level_port_categories(TraceLevel)),
        not (
            trace_level_allows_port_suppression(TraceLevel) = yes,
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
