%------------------------------------------------------------------------------%
% Copyright (C) 1999, 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%------------------------------------------------------------------------------%
%
% module: posix__open.
% main author: conway@cs.mu.oz.au
%
% This module provides and interface to the open function and its
% relatives.
%
%------------------------------------------------------------------------------%
:- module posix__open.

:- interface.

:- import_module list.

:- type oflag
	--->	rdonly
	;	wronly
	;	rdwr
	;	creat
	;	excl
	;	noctty
	;	trunc
	;	append
	;	ndelay
	;	sync
	.

:- pred open(string, list(oflag), posix__result(fd), io__state, io__state).
:- mode open(in, in, out, di, uo) is det.

:- pred open(string, list(oflag), mode_t, posix__result(fd),
		io__state, io__state).
:- mode open(in, in, in, out, di, uo) is det.

:- pred creat(string, mode_t, posix__result(fd), io__state, io__state).
:- mode creat(in, in, out, di, uo) is det.

:- pred close(fd, posix__result, io__state, io__state).
:- mode close(in, out, di, uo) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module int.

:- pragma c_header_code("
	#include <unistd.h>
	#include <fcntl.h>
").

%------------------------------------------------------------------------------%

open(PathName, FlagList, Result) -->
	open0(PathName, oflags(FlagList), FdNo),
	( { FdNo < 0 } ->
		errno(Error),
		{ Result = error(Error) }
	;
		{ Result = ok(fd(FdNo)) }
	).

:- pred open0(string, int, int, io__state, io__state).
:- mode open0(in, in, out, di, uo) is det.

:- pragma c_code(open0(PathName::in, Flags::in, FileDes::out, IO0::di, IO::uo),
		[will_not_call_mercury, thread_safe], "{
	FileDes = open(PathName, Flags);
	IO = IO0;
}").

%------------------------------------------------------------------------------%

open(PathName, FlagList, Mode, Result) -->
	open0(PathName, oflags(FlagList), Mode, FdNo),
	( { FdNo < 0 } ->
		errno(Error),
		{ Result = error(Error) }
	;
		{ Result = ok(fd(FdNo)) }
	).

:- pred open0(string, int, mode_t, int, io__state, io__state).
:- mode open0(in, in, in, out, di, uo) is det.

:- pragma c_code(open0(PathName::in, Flags::in, Mode::in, FileDes::out,
		IO0::di, IO::uo), [will_not_call_mercury, thread_safe], "{
	FileDes = open(PathName, Flags, Mode);
	IO = IO0;
}").

%------------------------------------------------------------------------------%

creat(PathName, Mode, Result) -->
	creat0(PathName, Mode, FdNo),
	( { FdNo < 0 } ->
		errno(Error),
		{ Result = error(Error) }
	;
		{ Result = ok(fd(FdNo)) }
	).

:- pred creat0(string, mode_t, int, io__state, io__state).
:- mode creat0(in, in, out, di, uo) is det.

:- pragma c_code(creat0(PathName::in, Mode::in, FileDes::out,
		IO0::di, IO::uo), [will_not_call_mercury, thread_safe], "{
	FileDes = creat(PathName, Mode);
	IO = IO0;
}").

%------------------------------------------------------------------------------%

close(fd(FdNo), Result) -->
	close0(FdNo, Res0),
	( { Res0 < 0 } ->
		errno(Error),
		{ Result = error(Error) }
	;
		{ Result = ok }
	).

:- pred close0(int, int, io__state, io__state).
:- mode close0(in, out, di, uo) is det.

:- pragma c_code(close0(Fd::in, Res::out, IO0::di, IO::uo),
		[will_not_call_mercury, thread_safe], "{
	Res = close(Fd);
	IO = IO0;
}").

%------------------------------------------------------------------------------%

:- func oflags(list(oflag)) = int.

oflags(FlagList) = Or :-
	orflags(FlagList, 0, Or).

:- pred orflags(list(oflag), int, int).
:- mode orflags(in, in, out) is det.

orflags([], Or, Or).
orflags([F|Fs], Or0, Or) :-
	Or1 = Or0 \/ oflagval(F),
	orflags(Fs, Or1, Or).

:- func oflagval(oflag) = int.
:- mode (oflagval(in) = out) is det.

:- pragma c_code(oflagval(F::in) = (V::out),
		[will_not_call_mercury, thread_safe], "{
	static const int oflag_values[] = {
		O_RDONLY, O_WRONLY, O_RDWR, O_CREAT, O_EXCL, O_NOCTTY,
		O_TRUNC, O_APPEND, O_NDELAY, O_SYNC
	};

	V = oflag_values[F];
}").

