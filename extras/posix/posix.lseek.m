%------------------------------------------------------------------------------%
% Copyright (C) 1999, 2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%------------------------------------------------------------------------------%
%
% module: posix__lseek.m
% main author: conway@cs.mu.oz.au
%
%------------------------------------------------------------------------------%
:- module posix__lseek.

:- interface.

:- type whence
	--->	set
	;	cur
	;	end
	.

:- pred lseek(fd, int, lseek__whence, posix__result(int), io__state, io__state).
:- mode lseek(in, in, in, out, di, uo) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module int.

:- pragma c_header_code("
	#include <sys/types.h>
	#include <unistd.h>
").

%------------------------------------------------------------------------------%

lseek(Fd, Offset, Whence, Result) -->
	lseek0(Fd, Offset, whence(Whence), Res),
	( { Res < 0 } ->
		errno(Err),
		{ Result = error(Err) }
	;
		{ Result = ok(Res) }
	).

:- pred lseek0(fd, int, int, int, io__state, io__state).
:- mode lseek0(in, in, in, out, di, uo) is det.

:- pragma c_code(lseek0(Fd::in, Offset::in, Whence::in, Res::out,
		IO0::di, IO::uo), [will_not_call_mercury, thread_safe], "{

	Res = lseek(Fd, Offset, Whence);

	IO = IO0;
}").

:- func whence(lseek.whence) = int.

:- pragma c_code(whence(W::in) = (V::out),
		[will_not_call_mercury, thread_safe], "{
	static const int whence_flags[] = { SEEK_SET, SEEK_CUR, SEEK_END } ;
	V = whence_flags[W];
}").

%------------------------------------------------------------------------------%

