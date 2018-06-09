%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1999-2002, 2005-2006 The University of Melbourne.
% Copyright (C) 2015-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: collect_lib.m.
% Author: jahier.
% Purpose:
%   This module defines functions that are needed to implement the
%   `collect' primitive.
%
%   `collect' collects runtime information from Mercury program executions.
%   It is intended to let users easily implement their own monitors with
%   acceptable performances.
%
%   To use it, users just need to define 4 things in a file, using the
%   Mercury syntax:
%       1) `accumulator_type' which is the type of the accumulator.
%       2) The predicate initialize/1 which initializes the
%          collecting variable. initialize/1 should respect the
%          following declarations:
%           :- pred initialize(accumulator_type).
%           :- mode initialize(out) is det.
%       3) The predicate filter/4 which updates the collecting
%          variable at each execution event. filter/4 also outputs
%          a variable that indicates whether to stop collecting.
%          If this variable is set to `stop', the collect process
%          stops; if it is set to `continue', it continues. If this
%          variable is always set to `continue', the collecting will
%          process until the last event is reached. filter/4 should
%          respect the following declarations:
%           :- pred filter(event, accumulator_type, accumulator_type,
%               stop_or_continue).
%           :- mode filter(in, di, uo, out) is det.
%               4) Optionally, a `post_process/2' function that lets
%                  one post-process the final value of the
%                  accumulator. `post_process/2' should respect the
%                  following declarations:
%                   :- pred post_process(accumulator_type, collected_type).
%                   :- mode post_process(in, out) is det.
%              If `collected_type' is different from `accumulator_type',
%              the `accumulator_type' should also be defined; otherwise
%              by default, `accumulator_type' is automatically defined as
%              being the same type as `collected_type'.
%       5) And optionally the mode definition of the second and the
%          third arguments of filter/4: `acc_in' and `acc_out'. Those
%          mode have `di' and `uo' respectively as default values.
%
%   Then, this file is used to generate the Mercury module `collect.m',
%   which is compiled and dynamically linked with the current execution.
%   When a `collect' request is made from the external debugger, a variable
%   of type accumulator_type is first initialized (with initialize/1) and
%   then updated (with filter/4) for all the events of the remaining
%   execution. When the end of the execution is reached, the last value of
%   the collecting variable is send to the debugger.

:- module mdb.collect_lib.
:- interface.

:- pred dummy_pred_to_avoid_warning_about_nothing_exported is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdb.dl.

:- import_module char.
:- import_module io.

%---------------------------------------------------------------------------%

dummy_pred_to_avoid_warning_about_nothing_exported.

:- pragma foreign_export("C",
    link_collect(in, out, out, out, out, out, out, out, di, uo),
    "ML_CL_link_collect").

:- pragma foreign_export("C", unlink_collect(in, di, uo),
    "ML_CL_unlink_collect").

% We need Handle to be able to close the shared object (dl.close) later on.
% When the link failed, we output NULL pointers instead of maybe pointers
% for performance reasons; indeed, filter will be called at every event
% so we don't want to pay the price of the maybe variable de-construction
% at each event.

% dynamically link the collect module;
:- pred link_collect(string::in, c_pointer::out, c_pointer::out,
    c_pointer::out, c_pointer::out, c_pointer::out, dl_result(handle)::out,
    char::out, io::di, io::uo) is det.

link_collect(ObjectFile, Filter, Initialize, PostProcess, SendResult,
        GetCollectType, MaybeHandle, Result, !IO) :-
    % Link in the object code for the module `collect' from ObjectFile.
    dl.open(ObjectFile, lazy, scope_local, MaybeHandle, !IO),
    (
        MaybeHandle = dl_error(Msg),
        print("dlopen failed: ", !IO),
        print(Msg, !IO),
        nl(!IO),
        set_to_null_pointer(Initialize),
        set_to_null_pointer(Filter),
        set_to_null_pointer(PostProcess),
        set_to_null_pointer(SendResult),
        set_to_null_pointer(GetCollectType),
        Result = 'n'
    ;
        MaybeHandle = dl_ok(Handle),

        % Look up the address of the C functions corresponding to the
        % initialize/1 and filter/15 predicates in the collect module.
        dl.sym(Handle, "ML_COLLECT_initialize", MaybeInitialize, !IO),
        dl.sym(Handle, "ML_COLLECT_filter", MaybeFilter, !IO),
        dl.sym(Handle, "ML_COLLECT_post_process", MaybePostProcess, !IO),
        dl.sym(Handle, "ML_COLLECT_send_collect_result", MaybeSendResult, !IO),
        dl.sym(Handle, "ML_COLLECT_collected_variable_type", MaybeType, !IO),
        ( if
            MaybeInitialize = dl_ok(Initialize0),
            MaybeFilter = dl_ok(Filter0),
            MaybePostProcess = dl_ok(PostProcess0),
            MaybeSendResult = dl_ok(SendResult0),
            MaybeType = dl_ok(Type0)
        then
            Result = 'y',
            Initialize = Initialize0,
            Filter = Filter0,
            PostProcess = PostProcess0,
            GetCollectType = Type0,
            SendResult = SendResult0
        else
            set_to_null_pointer(Initialize),
            set_to_null_pointer(Filter),
            set_to_null_pointer(PostProcess),
            set_to_null_pointer(SendResult),
            set_to_null_pointer(GetCollectType),
            Result = 'n'
        )
    ).

:- pred set_to_null_pointer(c_pointer::out) is det.
:- pragma foreign_proc("C",
    set_to_null_pointer(Pointer::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    (Pointer = (MR_Word) NULL)
").

:- pragma no_determinism_warning(set_to_null_pointer/1).
set_to_null_pointer(_) :-
    private_builtin.sorry("collect_lib.set_to_null_pointer").

%---------------------------------------------------------------------------%

    % Dynamically unlink a module that was dynamically linked in
    % using `link_collect'.
    %
:- pred unlink_collect(dl_result(handle)::in, io::di, io::uo) is det.

unlink_collect(MaybeHandle, !IO) :-
    (
        MaybeHandle = dl_error(_)
        % There is nothing to close since an error(_) here means that
        % the dlopen failed.
    ;
        MaybeHandle = dl_ok(Handle),
        dl.close(Handle, Result, !IO),
        display_close_result(Result, !IO)
    ).

:- pred display_close_result(dl_result::in, io::di, io::uo) is det.

display_close_result(dl_ok, !IO).
display_close_result(dl_error(String), !IO) :-
    print(String, !IO),
    nl(!IO).
