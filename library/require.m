:- module require.
:- interface.

:- pred	require(pred, string).
%	require(Goal, Message).
%		Call goal, and abort with error message if Goal fails.

:- implementation.

require(Goal, Message) :-
	( call(Goal) ->
		true
	;
		error(Message),
		fail
	).

:- pred error(string).
error(Message) :-
	format("~s\n", [Message]),
	trace,
	fail.

:- end_module require.
