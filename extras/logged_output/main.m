%------------------------------------------------------------------------------%
% Copyright (C) 2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%------------------------------------------------------------------------------%
%
% module: 	main.m
% main author:	Peter Ross (petdr@miscrit.be)
%
% Use the logged_output stream.
%
%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
:- module main.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.
:- implementation.
:- import_module logged_output.
main -->
	logged_output__init("OUTPUT", Result),
	(
		{ Result = ok(OutputStream) }
	->
		io__write_string(OutputStream, "Hi there.\n")
	;
		io__write_string("Unable to open OUTPUT\n")
	).
