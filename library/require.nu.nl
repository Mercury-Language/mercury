%-----------------------------------------------------------------------------%

% File: require.nu.nl.
% Main author: fjh.

% This module implements the predicates declared in require.nl
% using non-logical NU-Prolog.

require(Goal, Message) :-
	( call(Goal) ->
		true
	;
		error(Message),
		fail
	).

error(Message) :-
	format("~s\n", [Message]),
	abort.

%-----------------------------------------------------------------------------%
