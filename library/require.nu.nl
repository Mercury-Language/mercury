%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: require.nu.nl.
% Main author: fjh.

% This module implements the predicates declared in require.m
% using non-logical NU-Prolog.

require(Goal, Message) :-
	( call(Goal) ->
		true
	;
		error(Message),
		fail
	).

error(Message) :-
	format(user_error, "\nSoftware error: ~s\n", [Message]),
	abort.

%-----------------------------------------------------------------------------%
