%---------------------------------------------------------------------------%
% Copyright (C) 1997-1998 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: test_uncaught_exception.m.
% Main author: fjh.

% A test case for exception handling.

%-----------------------------------------------------------------------------%

:- module test_uncaught_exception.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is cc_multi.

:- implementation.
:- import_module std_util.
:- import_module exception.

main --> 
	{ throw("<exception thrown from main>") }.

