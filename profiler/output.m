
%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% output.m
%
% Main author: petdr.
%
% Print's out the output.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module output.

:- interface.

:- import_module prof_info.
:- import_module io.

:- pred output__main(prof, io__state, io__state).
:- mode output__main(in, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

output__main(_Prof) -->
	io__write_string("In output__main\n").
