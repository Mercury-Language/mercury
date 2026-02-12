%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2021, 2023, 2025-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module libs.maybe_util.
:- interface.

:- import_module list.

:- type maybe_succeeded
    --->    did_not_succeed
    ;       succeeded.

:- func maybe_succeeded `and` maybe_succeeded = maybe_succeeded.

:- func and_list(list(maybe_succeeded)) = maybe_succeeded.

%---------------------%

:- type is_first
    --->    is_first
    ;       is_not_first.

:- type is_last
    --->    is_last
    ;       is_not_last.

%---------------------%

:- type maybe_changed
    --->    unchanged
    ;       changed.

:- func maybe_changed `or` maybe_changed = maybe_changed.

:- func or_list(list(maybe_changed)) = maybe_changed.

%---------------------%

:- type need_to_requantify
    --->    need_to_requantify
    ;       do_not_need_to_requantify.

%---------------------------------------------------------------------------%
:- implementation.
%---------------------------------------------------------------------------%

SucceededA `and` SucceededB =
    ( if SucceededA = succeeded, SucceededB = succeeded then
        succeeded
    else
        did_not_succeed
    ).

and_list(Succeededs) = AllSucceeded :-
    AllSucceeded = list.foldl(and, Succeededs, succeeded).

%---------------------------------------------------------------------------%

ChangedA `or` ChangedB =
    ( if (ChangedA = changed ; ChangedB = changed) then
        changed
    else
        unchanged
    ).

or_list(Changeds) = AnyChanged :-
    AnyChanged = list.foldl(or, Changeds, unchanged).

%---------------------------------------------------------------------------%
:- end_module libs.maybe_util.
%---------------------------------------------------------------------------%
