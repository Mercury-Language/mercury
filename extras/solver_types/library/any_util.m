% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% any_util.m
% Ralph Becket <rafe@cs.mu.OZ.AU>
% Wed Sep  7 14:54:39 EST 2005
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%
% Utility predicates for use with inst any values.
%
%-----------------------------------------------------------------------------%

:- module any_util.

:- interface.



    % This predicate is useful for converting polymorphic non-solver type
    % values with inst any to inst ground (the compiler recognises that inst
    % any is equivalent to ground for non-polymorphic non-solver types).
    %
    % DON'T call this on solver type values unless you're absolutely sure they
    % are semantically ground.
    %
:- pred unsafe_cast_to_ground(T::(any >> ground)) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    unsafe_cast_to_ground(_X::(any >> ground)),
    [promise_pure, will_not_call_mercury, thread_safe],
"").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
