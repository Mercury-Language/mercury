%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1998, 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Main author: bromage
% Based heavily on lcsstype.m, written by bromage and simplified by
% Marnix Klooster <marnix@worldonline.nl>
%
% This module contains the type of a diff.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module difftype.
:- interface.

:- import_module list.
:- import_module pair.

%-----------------------------------------------------------------------------%

    % A pos is a non-negative number representing a position in a list.
    % The position before all elements is 0, the one between the first
    % and second elements is 1, etc.
    %
:- type pos == int.

%-----------------------------------------------------------------------------%

    % A segment is a pair of positions. Numbering items from 0,
    % segment P-Q stands for items P up to, but not including, Q.
    % (Rationale: see the interpretation of type pos above.)
    %
    % Invariant: In any segment X - Y, it should always be true that X =< Y.
    % If X=Y, the segment is empty.
    %
:- type segment == pair(pos,pos).

    % An edit operation is an addition, a deletion or a change.
    %
:- type edit
    --->    add(pos, segment)
    ;       delete(segment, pos)
    ;       change(segment, segment).

    % The complete diff of two file is a list of edit operations.
    %
    % Invariant: The edits must be in order, and must not overlap or touch.
    %
:- type diff == list(edit).

%-----------------------------------------------------------------------------%

:- pred first_mentioned_positions(edit::in, pos::out, pos::out) is det.

:- pred last_mentioned_positions(edit::in, pos::out, pos::out) is det.

%-----------------------------------------------------------------------------%

    % Add an edit to the start of a diff, producing a new diff.
    % This predicate determines what kind of edit this is, and merges with the
    % adjacent edits if appropriate.
    %
:- pred add_edit(segment::in, segment::in, diff::in, diff::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

first_mentioned_positions(add(X, Y - _), X, Y).
first_mentioned_positions(delete(X - _, Y), X, Y).
first_mentioned_positions(change(X - _, Y - _), X, Y).

last_mentioned_positions(add(X, _ - Y), X, Y).
last_mentioned_positions(delete(_ - X, Y), X, Y).
last_mentioned_positions(change(_ - X, _ - Y), X, Y).

%-----------------------------------------------------------------------------%

add_edit(X1 - X2, Y1 - Y2, [], Diff) :-
    ( if X1 = X2  then
        ( if Y1 = Y2 then
            Diff = []
        else
            Diff = [add(X1, Y1 - Y2)]
        )
    else
        ( if Y1 = Y2 then
            Diff = [delete(X1 - X2, Y1)]
        else
            Diff = [change(X1 - X2, Y1 - Y2)]
        )
    ).
add_edit(X1 - X2, Y1 - Y2, [Edit0 | Diff0], Diff) :-
    ( if
         Edit0 = add(X2, Y2 - Y3)
    then
        ( if X1 = X2 then
            Diff = [add(X1, Y1 - Y3) | Diff0]
        else
            Diff = [change(X1 - X2, Y1 - Y3) | Diff0]
        )
    else if
        Edit0 = delete(X2 - X3, Y2)
    then
        ( if Y1 = Y2 then
            Diff = [delete(X1 - X3, Y1) | Diff0]
        else
            Diff = [change(X1 - X3, Y1 - Y2) | Diff0]
        )
    else if
        Edit0 = change(X2 - X3, Y2 - Y3)
    then
        Diff = [change(X1 - X3, Y1 - Y3) | Diff0]
    else
        % This is just copied from the base case. Pretty much.
        ( if X1 = X2 then
            ( if Y1 = Y2 then
                Diff = [Edit0 | Diff0]
            else
                Diff = [add(X1, Y1 - Y2), Edit0 | Diff0]
            )
        else
            ( if Y1 = Y2 then
                Diff = [delete(X1 - X2, Y1), Edit0 | Diff0]
            else
                Diff = [change(X1 - X2, Y1 - Y2), Edit0 | Diff0]
            )
        )
    ).

%-----------------------------------------------------------------------------%
:- end_module difftype.
%-----------------------------------------------------------------------------%
