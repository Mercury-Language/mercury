%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module require.

% Main author: fjh.
% Stability: medium to high.

% This module provides features similar to <assert.h> in C.

%-----------------------------------------------------------------------------%
:- interface.

:- pred error(string).
:- mode error(in) is erroneous.

%	error(Message).
%		Abort with error message.


:- pred	require(pred, string).
:- mode	require((pred) is semidet, in) is det.

%	require(Goal, Message).
%		Call goal, and abort with error message if Goal fails.
%		This is not as useful as you might imagine, since it requires
%		that the goal not produce any output variables.  In
%		most circumstances you should use an explicit if-then-else
%		with a call to error/1 in the "else".

%-----------------------------------------------------------------------------%

:- implementation.

:- external(error/1).

require(Goal, Message) :-
	( call(Goal) ->
		true
	;
		error(Message),
		fail
	).

:- end_module require.

%-----------------------------------------------------------------------------%
