%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

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
:- external(require/2).
*/
:- external(error/1).

:- end_module require.
