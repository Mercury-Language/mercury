%------------------------------------------------------------------------------%
% Copyright (C) 1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%------------------------------------------------------------------------------%
%
% module: posix:write.m
% main author: conway@cs.mu.oz.au
%
%------------------------------------------------------------------------------%
:- module posix:write.

:- interface.

:- import_module text.

:- pred write(fd, int, text, posix:result(int), io__state, io__state).
:- mode write(in, in, in, out, di, uo) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module int.

:- pragma c_header_code("
	#include <unistd.h>
	#include ""text_header.h""
").

%------------------------------------------------------------------------------%

write(Fd, ToWrite, Text, Result) -->
	write0(Fd, ToWrite, Text, Res),
	( { Res < 0 } ->
		errno(Err),
		{ Result = error(Err) }
	;
		{ Result = ok(Res) }
	).

:- pred write0(fd, int, text, int, io__state, io__state).
:- mode write0(in, in, in, out, di, uo) is det.

:- pragma c_code(write0(Fd::in, ToWrite::in, Text::in, Res::out,
		IO0::di, IO::uo), [will_not_call_mercury, thread_safe], "{
	ME_Text *txtptr;

	txtptr = (ME_Text *) Text;

	Res = write(Fd, txtptr->data, ToWrite);

	IO = IO0;
}").

%------------------------------------------------------------------------------%

