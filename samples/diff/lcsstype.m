%-----------------------------------------------------------------------------%
% Copyright (C) 1995 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Main author: bromage
% Simplified by Marnix Klooster <marnix@worldonline.nl>
% Last changed 22 October 1996

% This module contains types common to the modules lcss and diffs.

%-----------------------------------------------------------------------------%

:- module lcsstype.

:- interface.
:- import_module std_util.


	% A pos is a non-negative number representing a position in a
	% list.  The position before all elements is 0, the one
	% between the first and second elements is 1, etc.
:- type pos == int.

	% The longest common subsequence of two lists can be
	% represented as an ordered list of "matches".  A match is a
	% pair of the form I-J where I is the number of an item in
	% item 1 and J is the number of an item in list 2.  (Note that
	% an item is numbered by the position immediately preceding
	% it, i.e., numbering starts at 0.)
:- type lcss == list(pair(pos,pos)).
