%-----------------------------------------------------------------------------%
% Copyright (C) 1997, 2003 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
%
% file: mtogl.
% main author: conway.
%
% This module provides a binding to togl which is an OpenGL widget for Tcl/Tk.
%
%------------------------------------------------------------------------------%

:- module mtogl.

:- interface.

:- import_module io, mtcltk.

:- type togl.

:- pred mtogl__init(tcl_interp, tcl_status, io__state, io__state).
:- mode mtogl__init(in, out, di, uo) is det.

:- pred mtogl__create(pred(togl, io__state, io__state), io__state, io__state).
:- mode mtogl__create(pred(in, di, uo)is det, di, uo) is det.

:- pred mtogl__display(pred(togl, io__state, io__state), io__state, io__state).
:- mode mtogl__display(pred(in, di, uo)is det, di, uo) is det.

:- pred mtogl__reshape(pred(togl, io__state, io__state), io__state, io__state).
:- mode mtogl__reshape(pred(in, di, uo)is det, di, uo) is det.

:- pred mtogl__destroy(pred(togl, io__state, io__state), io__state, io__state).
:- mode mtogl__destroy(pred(in, di, uo)is det, di, uo) is det.

%:- pred mtogl__create_command(string, pred(togl, list(string),
%		io__state, io__state), io__state, io__state).
%:- mode mtogl__create_command(in, pred(in, in, di, uo) is det, di, uo) is det.

:- pred mtogl__post_redisplay(togl, io__state, io__state).
:- mode mtogl__post_redisplay(in, di, uo) is det.

:- pred mtogl__swap_buffers(togl, io__state, io__state).
:- mode mtogl__swap_buffers(in, di, uo) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- type togl == c_pointer.

:- pragma foreign_import_module("C", mtcltk).

:- pragma foreign_decl("C", "
	#include ""togl.h""
	extern MR_Word	mtogl_create_callback;
	void create_callback(struct Togl *togl);
	extern MR_Word	mtogl_display_callback;
	void display_callback(struct Togl *togl);
	extern MR_Word	mtogl_reshape_callback;
	void reshape_callback(struct Togl *togl);
	extern MR_Word	mtogl_destroy_callback;
	void destroy_callback(struct Togl *togl);
").

:- pragma foreign_code("C", "
	MR_Word	mtogl_create_callback;
	MR_Word	mtogl_display_callback;
	MR_Word	mtogl_reshape_callback;
	MR_Word	mtogl_destroy_callback;
").

:- pragma foreign_proc("C", mtogl__init(Interp::in, Stat::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure], "
{
	int err;

	err = Togl_Init((Tcl_Interp *) Interp);
	switch (err) {
		case TCL_OK:
			Stat = 0;
			break;
		case TCL_ERROR:
			Stat = 1;
			break;
		default:
			MR_fatal_error(
			""Togl_Init returned neither TCL_OK or TCL_ERROR"");
	}
	IO = IO0;
}").

%------------------------------------------------------------------------------%

:- pred do_create_callback(pred(togl, io__state, io__state), togl,
		io__state, io__state).
:- mode do_create_callback(pred(in, di, uo) is det, in, di, uo) is det.

do_create_callback(Closure, Togl) -->
	call(Closure, Togl).

:- pragma export(do_create_callback(pred(in, di, uo) is det, in, di, uo),
		"do_create_callback").

:- pragma foreign_proc("C", mtogl__create(Closure::pred(in, di, uo) is det,
		IO0::di, IO::uo),
	[may_call_mercury, promise_pure], "
	mtogl_create_callback = Closure;
	Togl_CreateFunc(create_callback);
	IO = IO0;
").

:- pragma foreign_code("C", "
void create_callback(struct Togl *togl)
{
	do_create_callback(mtogl_create_callback, (MR_Word) togl);
}
").

%------------------------------------------------------------------------------%

:- pred do_display_callback(pred(togl, io__state, io__state), togl,
		io__state, io__state).
:- mode do_display_callback(pred(in, di, uo) is det, in, di, uo) is det.

do_display_callback(Closure, Togl) -->
	call(Closure, Togl).

:- pragma export(do_display_callback(pred(in, di, uo) is det, in, di, uo),
		"do_display_callback").

:- pragma foreign_proc("C", mtogl__display(Closure::pred(in, di, uo) is det,
		IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure], "
	mtogl_display_callback = Closure;
	Togl_DisplayFunc(display_callback);
	IO = IO0;
").

:- pragma foreign_code("C", "
void display_callback(struct Togl *togl)
{
	do_display_callback(mtogl_display_callback, (Word) togl);
}
").

%------------------------------------------------------------------------------%

:- pred do_reshape_callback(pred(togl, io__state, io__state), togl,
		io__state, io__state).
:- mode do_reshape_callback(pred(in, di, uo) is det, in, di, uo) is det.

do_reshape_callback(Closure, Togl) -->
	call(Closure, Togl).

:- pragma export(do_reshape_callback(pred(in, di, uo) is det, in, di, uo),
		"do_reshape_callback").

:- pragma foreign_proc("C", mtogl__reshape(Closure::pred(in, di, uo) is det,
		IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure], "
	mtogl_reshape_callback = Closure;
	Togl_ReshapeFunc(reshape_callback);
	IO = IO0;
").

:- pragma foreign_code("C", "
void reshape_callback(struct Togl *togl)
{
	do_reshape_callback(mtogl_reshape_callback, (MR_Word) togl);
}
").

%------------------------------------------------------------------------------%

:- pred do_destroy_callback(pred(togl, io__state, io__state), togl,
		io__state, io__state).
:- mode do_destroy_callback(pred(in, di, uo) is det, in, di, uo) is det.

do_destroy_callback(Closure, Togl) -->
	call(Closure, Togl).

:- pragma export(do_destroy_callback(pred(in, di, uo) is det, in, di, uo),
		"do_destroy_callback").

:- pragma c_code(mtogl__destroy(Closure::pred(in, di, uo) is det,
		IO0::di, IO::uo),"
	mtogl_destroy_callback = Closure;
	Togl_DestroyFunc(destroy_callback);
	IO = IO0;
").

:- pragma foreign_code("C", "
void destroy_callback(struct Togl *togl)
{
	do_destroy_callback(mtogl_destroy_callback, (Word) togl);
}
").

%------------------------------------------------------------------------------%

:- pragma foreign_proc("C", mtogl__post_redisplay(Togl::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure], "
	Togl_PostRedisplay((struct Togl *) Togl);
	IO = IO0;
").

%------------------------------------------------------------------------------%

:- pragma foreign_proc("C", mtogl__swap_buffers(Togl::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure], "
	Togl_SwapBuffers((struct Togl *) Togl);
	IO = IO0;
").

%------------------------------------------------------------------------------%
:- end_module mtogl.
%------------------------------------------------------------------------------%
