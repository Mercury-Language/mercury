%------------------------------------------------------------------------------%
% Copyright (C) 1999-2000, 2004 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%------------------------------------------------------------------------------%
%
% module: posix__select.m
% main author: conway@cs.mu.oz.au
%
%------------------------------------------------------------------------------%
:- module posix__select.

:- interface.

:- import_module bool.

:- type fdset_ptr.

:- pred select(int, fdset_ptr, fdset_ptr, fdset_ptr, timeval, posix__result(int),
		io__state, io__state).
:- mode select(in, in, in, in, in, out, di, uo) is det.

:- pred new_fdset_ptr(fdset_ptr, io__state, io__state).
:- mode new_fdset_ptr(out, di, uo) is det.

:- pred fd_clr(fd, fdset_ptr, io__state, io__state).
:- mode fd_clr(in, in, di, uo) is det.

:- pred fd_isset(fd, fdset_ptr, bool, io__state, io__state).
:- mode fd_isset(in, in, out, di, uo) is det.

:- pred fd_set(fd, fdset_ptr, io__state, io__state).
:- mode fd_set(in, in, di, uo) is det.

:- pred fd_zero(fdset_ptr, io__state, io__state).
:- mode fd_zero(in, di, uo) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module int, std_util.

:- pragma c_header_code("
	#include <sys/time.h>
	#include <sys/types.h>
	#include <unistd.h>

	#include ""posix_workarounds.h""
").

:- type fdset_ptr
	--->	fdset_ptr(c_pointer).

%------------------------------------------------------------------------------%

select(Fd, R, W, E, Timeout, Result) -->
	{ Timeout = timeval(TS, TM) },
	select0(Fd, R, W, E, TS, TM, Res),
	( { Res < 0 } ->
		errno(Err),
		{ Result = error(Err) }
	;
		{ Result = ok(Res) }
	).

:- pred select0(int, fdset_ptr, fdset_ptr, fdset_ptr, int, int, int, io__state, io__state).
:- mode select0(in, in, in, in, in, in, out, di, uo) is det.

:- pragma c_code(select0(N::in, R::in, W::in, E::in, TS::in, TM::in, Res::out,
		IO0::di, IO::uo), [will_not_call_mercury, thread_safe], "{
	struct timeval tv;

	tv.tv_sec = TS;
	tv.tv_usec = TM;
	Res = select(N, (fd_set *)R, (fd_set *)W, (fd_set *)E, &tv);

	IO = IO0;
}").

%------------------------------------------------------------------------------%

:- pragma c_code(new_fdset_ptr(Fds::out, IO0::di, IO::uo),
		[will_not_call_mercury, thread_safe], "{

	MR_incr_hp(Fds, 1+sizeof(fd_set)/sizeof(MR_Word));
	ME_fd_zero((fd_set *) Fds);

	IO = IO0;
}").

%------------------------------------------------------------------------------%

:- pragma c_code(fd_clr(Fd::in, Fds::in, IO0::di, IO::uo),
		[will_not_call_mercury, thread_safe], "{
	ME_fd_clr(Fd, (fd_set *) Fds);
	IO = IO0;
}").

%------------------------------------------------------------------------------%

:- pragma c_code(fd_zero(Fds::in, IO0::di, IO::uo),
		[will_not_call_mercury, thread_safe], "{
	ME_fd_zero((fd_set *) Fds);
	IO = IO0;
}").

%------------------------------------------------------------------------------%

:- pragma c_code(fd_isset(Fd::in, Fds::in, Res::out, IO0::di, IO::uo),
		[will_not_call_mercury, thread_safe], "{
	Res = (ME_fd_isset(Fd, (fd_set *) Fds) ? MR_YES : MR_NO );
	IO = IO0;
}").

%------------------------------------------------------------------------------%

:- pragma c_code(fd_set(Fd::in, Fds::in, IO0::di, IO::uo),
		[will_not_call_mercury, thread_safe], "{
	ME_fd_set(Fd, (fd_set *) Fds);
	IO = IO0;
}").

