%----------------------------------------------------------------------------%
% Copyright (C) 2000-2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% mlds_to_java - Convert MLDS to Java code.
% Main author: juliensf 
% 
% XXX Not yet implemented.  
%-----------------------------------------------------------------------------%
:- module mlds_to_java.
:- interface.

:- import_module mlds.
:- import_module io.

:- pred mlds_to_java__output_mlds(mlds, io__state, io__state).
:- mode mlds_to_java__output_mlds(in, di, uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.
	
	%
	% the mlds to java code generator
	%

mlds_to_java__output_mlds(_MLDS) -->
	io__stderr_stream(Stream),
	io__write_string(Stream, "mlds_to_java.m: Java backend not yet implemented.\n").

:- end_module mlds_to_java.
%-----------------------------------------------------------------------------%
