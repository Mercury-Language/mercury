%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1998 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Main author: bromage
% Based on lcsstype.m, written by bromage and simplified by
% Marnix Klooster <marnix@worldonline.nl>

% This module contains the type of a diff.

%-----------------------------------------------------------------------------%

:- module difftype.

:- interface.
:- import_module std_util, list.

	% A pos is a non-negative number representing a position in a
	% list.  The position before all elements is 0, the one
	% between the first and second elements is 1, etc.
:- type pos == int.

%-----------------------------------------------------------------------------%

	% A segment is a pair of positions.  Numbering items from 0,
	% segment P-Q stands for items P up to, but not including, Q.
	% (Rationale: see the interpretation of type pos above.)
	%
	% Invariant: In any segment X - Y, it should always be true
	% that X =< Y.  If X=Y, the segment is empty.
:- type segment == pair(pos,pos).

	% An edit operation is an addition, a deletion or a change.
:- type edit --->
		add(pos,segment)
	;	delete(segment,pos)
	;	change(segment,segment).

	% The complete diff of two file is a list of edit
	% operations.
	%
	% Invariant: The edits must be in order, and must
	% not overlap or touch.
:- type diff == list(edit).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
