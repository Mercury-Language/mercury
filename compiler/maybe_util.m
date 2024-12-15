%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2021, 2023 The Mercury team.
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

:- type maybe_changed
    --->    unchanged
    ;       changed.

%---------------------------------------------------------------------------%

:- implementation.

SucceededA `and` SucceededB =
    ( if SucceededA = succeeded, SucceededB = succeeded then
        succeeded
    else
        did_not_succeed
    ).

and_list(Succeededs) = AllSucceeded :-
    AllSucceeded = list.foldl(and, Succeededs, succeeded).

%---------------------------------------------------------------------------%
:- end_module libs.maybe_util.
%---------------------------------------------------------------------------%
