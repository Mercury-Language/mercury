%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% generate_output.m
%
% Main author: petdr.
%
% Take's the prof structure and generate's the output.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module generate_output.

:- interface.

:- import_module prof_info.
:- import_module relation, io.

:- pred generate_output__main(prof, prof, io__state, io__state).
:- mode generate_output__main(in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

generate_output__main(Prof, Prof) -->
	io__write_string("In generate_output__main\n").
