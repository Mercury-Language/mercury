%---------------------------------------------------------------------------%
% Copyright (C) 1994-1995, 1997 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: require.nu.nl.
% Main author: fjh.

% This module implements the error/1 predicate declared in require.m
% using non-logical NU-Prolog.

error(Message) :-
	format(user_error, "\nSoftware error: ~s\n", [Message]),
	abort.

%-----------------------------------------------------------------------------%
