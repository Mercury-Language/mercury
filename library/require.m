%-----------------------------------------------------------------------------%

:- module require.

% Main author: fjh.

% This module provides features similar to <assert.h> in C.

%-----------------------------------------------------------------------------%
:- interface.

:- pred	require(pred, string).
:- mode	require(input, input).

%	require(Goal, Message).
%		Call goal, and abort with error message if Goal fails.

:- pred error(string).
:- mode error(input).

%	error(Message).
%		Abort with error message.

:- implementation.

require(Goal, Message) :-
	( call(Goal) ->
		true
	;
		error(Message),
		fail
	).

error(Message) :-
	format("~s\n", [Message]),
	trace,		% this doesn't work with NU-Prolog, unfortunately.
	abort.

:- end_module require.
