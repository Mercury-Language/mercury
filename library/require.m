%-----------------------------------------------------------------------------%

:- module require.

% Main author: fjh.

% This module provides features similar to <assert.h> in C.

%-----------------------------------------------------------------------------%
:- interface.

/***
:- pred	require(pred, string).
:- mode	require(in, in).

%	require(Goal, Message).
%		Call goal, and abort with error message if Goal fails.
****/

:- pred error(string).
:- mode error(in) is erroneous.

%	error(Message).
%		Abort with error message.

:- implementation.

/*
:- external("NU-Prolog", require/2).
:- external("NU-Prolog", error/1).
*/

:- end_module require.
