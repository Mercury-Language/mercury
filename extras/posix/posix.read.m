%------------------------------------------------------------------------------%
% Copyright (C) 1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%------------------------------------------------------------------------------%
%
% module: posix__read.m
% main author: conway@cs.mu.oz.au
%
%------------------------------------------------------------------------------%
:- module posix__read.

:- interface.

:- import_module text.

:- pred read(fd, int, posix__result(int), text, text, io__state, io__state).
:- mode read(in, in, out, di, uo, di, uo) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module int.

:- pragma c_header_code("
	#include <unistd.h>
	#include ""text_header.h""
").

%------------------------------------------------------------------------------%

read(Fd, ToRead, Result, Text0, Text) -->
	read0(Fd, ToRead, Read, Text0, Text),
	( { Read < 0 } ->
		errno(Err),
		{ Result = error(Err) }
	;
		{ Result = ok(Read) }
	).

:- pred read0(fd, int, int, text, text, io__state, io__state).
:- mode read0(in, in, out, di, uo, di, uo) is det.

:- pragma c_code(read0(Fd::in, ToRead::in, Read::out, Text0::di, Text::uo,
		IO0::di, IO::uo), [will_not_call_mercury, thread_safe], "{
	ME_Text *txtptr;

	txtptr = (ME_Text *) Text0;

	Read = read(Fd, txtptr->data, ToRead);

	Text = Text0;
	IO = IO0;
}").

%------------------------------------------------------------------------------%

