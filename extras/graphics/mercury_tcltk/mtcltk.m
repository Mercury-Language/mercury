%-----------------------------------------------------------------------------%
% Copyright (C) 1997-1998,2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% mtcltk -- the Mercury interface to Tk/Tcl
% authors: conway, fjh
% Stability: medium.

%-----------------------------------------------------------------------------%
%
%	See the file "HOWTO" for instructions on how to link with
%	this library.
%
%-----------------------------------------------------------------------------%
:- module mtcltk.
:- interface.

:- import_module io.
:- import_module list, string.

	% The tcl_interp type is an abstract data type that
	% represents a Tcl interpreter.
:- type tcl_interp.

	% The tcl_status type is the type that is returned by tcl
	% commands to indicate whether or not they were successful.
:- type tcl_status ---> tcl_ok ; tcl_error.

	% mtcltk__main(Callback, Args):
	%	first initializes a Tcl interpreter `Interp' using `Args';
	%		the first `Arg' should be the program name (which you
	%		can obtain using io__progname), and the remainder
	%		are arguments that are passed to Interp
	%	then invokes `Callback' with `Interp', which you can use to
	%		add your own Tcl commands and/or to invoke Tk commands
	%	finally starts the Tk event loop
	%
:- pred mtcltk__main(pred(tcl_interp, io__state, io__state),
		list(string), io__state, io__state).
:- mode mtcltk__main(pred(in, di, uo) is det, in, di, uo) is det.

	% mtcltk__eval(Interp, Command, Status, Result):
	%	evaluates `Command' in `Interp'.
	%	if successful, returns `Status = tcl_ok'
	%	and binds `Result' to the return string,
	%	otherwise returns `Status = tcl_error'
	%	and binds `Result' to the error message.
:- pred mtcltk__eval(tcl_interp, string, tcl_status, string, io__state,
		io__state).
:- mode mtcltk__eval(in, in, out, out, di, uo) is det.

	% mtcltk__create_command(Interp, Name, Command):
	%	creates a new Tcl command called `Name' in `Interp'.
	%	Whenever `Name' is evaluated as a Tcl command in `Interp',
	%	the Tcl interpreter will use
	%		`call(Command, Interp, Args, Status, Result)'
	% 	to invoke the Mercury procedure `Command' passing `Interp'
	%	and `Args', which is the list of arguments (including the
	%	command name `Name') passed to the command.  If successful,
	%	`Command' should return `Status = tcl_ok' and a return value
	%	in `Result'.  If an error occurs, `Command' should return
	%	`Status = tcl_error' and should bind `Result' to an
	%	appropriate error message.
:- pred mtcltk__create_command(tcl_interp, string,
		pred(tcl_interp, list(string), tcl_status, string,
		io__state, io__state),
		io__state, io__state).
:- mode mtcltk__create_command(in, in, pred(in, in, out, out, di, uo) is det,
		di, uo) is det.

	% mtcltk__delete_command(Interp, Name, Result):
	%	deletes the Tcl/Tk command called `Name' in `Interp'
	%	and returns `Result'.
:- pred mtcltk__delete_command(tcl_interp, string, tcl_status,
		io__state, io__state).
:- mode mtcltk__delete_command(in, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- type tcl_interp == c_pointer.

:- pragma c_header_code("
/* 
 * tkAppInit.c --
 *
 *	Provides a default version of the Tcl_AppInit procedure for
 *	use in wish and similar Tk-based applications.
 *
 * Copyright (c) 1993 The Regents of the University of California.
 * Copyright (c) 1994 Sun Microsystems, Inc.
 *
 * See the file ""license.terms"" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * SCCS: @(#) tkAppInit.c 1.20 96/02/15 18:55:27
 */

#include ""tk.h""
#include ""mtcltk.h""
").


/*
 * The following variable is a special hack that is needed in order for
 * Sun shared libraries to be used for Tcl.
 */

:- pragma c_code("
extern int matherr(void);
int *tclDummyMathPtr = (int *) matherr;
").

:- pragma c_header_code("
	extern MR_Word mtcltk_mercury_initializer;
	char *mtcltk_strdup(const char *str);
").

:- pragma c_code("
	Word mtcltk_mercury_initializer;
").

:- pragma c_code(mtcltk__main(Closure::pred(in, di, uo) is det, Args::in,
		IO0::di, IO::uo), may_call_mercury, "
{
    MR_Word l;
    int     argc, i;
    char    **argv;

    /*
    ** convert arguments from a list of strings to an array of strings
    */
    argc = 0;
    for(l = Args; l != MR_list_empty(); l = MR_list_tail(l))
	argc++;
    MR_incr_hp(MR_LVALUE_CAST(Word, argv), argc + 1);

    for(i = 0, l = Args; l != list_empty(); l = list_tail(l), i++)
	argv[i] = (char *) MR_list_head(l);
    argv[i] = NULL;

    mtcltk_mercury_initializer = Closure;

    Tk_Main(argc, argv, Tcl_AppInit);
    IO = IO0;
}").

:- pred call_mercury_initializer(pred(tcl_interp, io__state, io__state),
		tcl_interp, io__state, io__state).
:- mode call_mercury_initializer(pred(in, di, uo) is det, in, di, uo) is det.

call_mercury_initializer(Closure, Interp) -->
	call(Closure, Interp).

:- pragma export(call_mercury_initializer(pred(in, di, uo) is det, in, di, uo),
		"mtcltk_call_mercury_initializer").

:- pragma c_code("
/*
 *----------------------------------------------------------------------
 *
 * Tcl_AppInit --
 *
 *	This procedure performs application-specific initialization.
 *	Most applications, especially those that incorporate additional
 *	packages, will have their own version of this procedure.
 *
 * Results:
 *	Returns a standard Tcl completion code, and leaves an error
 *	message in interp->result if an error occurs.
 *
 * Side effects:
 *	Depends on the startup script.
 *
 *----------------------------------------------------------------------
 */

int
Tcl_AppInit(Tcl_Interp *interp)
{
    static char tk_str[] = ""Tk"";

    if (Tcl_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
    if (Tk_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
    Tcl_StaticPackage(interp, tk_str, Tk_Init, (Tcl_PackageInitProc *) NULL);

    mtcltk_call_mercury_initializer(mtcltk_mercury_initializer, (Word)interp);

    return TCL_OK;
}
").

	% XXX Had to change Status to RStatus because using Status
	% resulted in *parse errors* in gcc :-(
:- pragma c_code(eval(Interp::in, Cmd::in, RStatus::out, Result::out,
		IO0::di, IO::uo), may_call_mercury, "
{
	int err;

	err = Tcl_Eval((Tcl_Interp *)Interp, (char *)Cmd);
	switch (err) {
		case TCL_OK:
			RStatus = 0;
			break;
		case TCL_ERROR:
			RStatus = 1;
			break;
		default:
			MR_fatal_error(""Tcl_Eval returned neither ""
					""TCL_OK or TCL_ERROR"");
	}
	Result = mtcltk_strdup(((Tcl_Interp *)Interp)->result);
	IO = IO0;
}
").

:- pragma c_code("
char *mtcltk_strdup(const char *str)
{
	MR_Word newstr;

	assert(str);
	MR_incr_hp_atomic(newstr, (strlen(str) + sizeof(MR_Word))
                / sizeof(MR_Word));
	assert(newstr);
	strcpy((char *) newstr, str);

	return (char *) newstr;
}
").

:- pragma c_header_code("
int mtcltk_do_callback(ClientData clientData, Tcl_Interp *interp,
		 int argc, char *argv[]);
").

:- pred call_mercury_closure(
		pred(tcl_interp, list(string), tcl_status, string,
			io__state, io__state),
		tcl_interp, list(string), tcl_status, string,
		io__state, io__state).
:- mode call_mercury_closure(pred(in, in, out, out, di, uo) is det,
		in, in, out, out, di, uo) is det.

call_mercury_closure(Closure, Interp, Args, Status, Result) -->
	call(Closure, Interp, Args, Status, Result).

:- pragma export(call_mercury_closure(pred(in, in, out, out, di, uo) is det,
		in, in, out, out, di, uo),
		"mtcltk_call_mercury_closure").

:- pragma c_code("
int mtcltk_do_callback(ClientData clientData, Tcl_Interp *interp,
		 int argc, char *argv[])
{
	MR_Word	status;
	MR_Word	args;
	int	i;

	/* convert the array of strings into a Mercury list of strings */
	args = MR_list_empty();
	for (i = argc - 1; i >= 0; i--) {
		args = MR_list_cons(mtcltk_strdup(argv[i]), args);
	}

	mtcltk_call_mercury_closure((MR_Word) clientData, (MR_Word) interp,
		args, &status, &interp->result);
/*
	fprintf(stderr, ""mercury result: `%s'\n"", interp->result);
*/
	return (mtcltk_tcl_status_ok(status) ? TCL_OK : TCL_ERROR);
}  
").

:- pred tcl_status_ok(tcl_status::in) is semidet.
:- pragma export(tcl_status_ok(in), "mtcltk_tcl_status_ok").
tcl_status_ok(tcl_ok).

:- pragma c_code(create_command(Interp::in, Name::in,
			Closure::pred(in, in, out, out, di, uo) is det,
			IO0::di, IO::uo), may_call_mercury,
"{
	Tcl_CreateCommand((Tcl_Interp *)Interp, Name, mtcltk_do_callback,
				(ClientData)Closure, NULL);
	IO = IO0;
}").

:- pragma c_code(delete_command(Interp::in, Name::in, Result::out,
			IO0::di, IO::uo),
"{
	int err;
	err = Tcl_DeleteCommand((Tcl_Interp *)Interp, Name);
	Result = (err == 0 ? 0 : 1);
	IO = IO0;
}").

:- pragma c_code("

#ifdef MR_CONSERVATIVE_GC

/*
** The addresses of the closures that we pass to Tk
** will be stored by Tk in malloc()'ed memory.
** However, it is essential that these pointers be
** visible to the garbage collector, otherwise it will
** think that the closures are unreferenced and reuse the storage.
** Hence we redefine malloc() and friends to call GC_malloc().
*/

void *malloc(size_t s)
{
	return GC_MALLOC(s);
}

void *calloc(size_t s, size_t n)
{
	void *t;
	t = GC_MALLOC(s*n);
	memset(t, 0, s*n);
	return t;
}

void *realloc(void *ptr, size_t s)
{
	return GC_REALLOC(ptr, s);
}

void free(void *ptr)
{
	GC_FREE(ptr);
}

#endif

").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
