%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-1998,2000, 2003, 2005-2006 The University of Melbourne.
% Copyright (C) 2014, 2018, 2020, 2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% File: mtcltk.m.
% Authors: conway, fjh.
% Stability: medium.
%
% A Mercury interface to Tcl/Tk.
%
% See the file "HOWTO" for instructions on how to link with this library.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module mtcltk.
:- interface.

:- import_module io.
:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------------%

    % The tcl_interp type is an abstract data type that
    % represents a Tcl interpreter.
    %
:- type tcl_interp.

    % The tcl_status type is the type that is returned by tcl
    % commands to indicate whether or not they were successful.
    %
:- type tcl_status
    --->    tcl_ok
    ;       tcl_error.

    % main(Callback, Args):
    %   first initializes a Tcl interpreter `Interp' using `Args';
    %       the first `Arg' should be the program name (which you
    %       can obtain using io.progname), and the remainder
    %       are arguments that are passed to Interp
    %   then invokes `Callback' with `Interp', which you can use to
    %       add your own Tcl commands and/or to invoke Tk commands
    %   finally starts the Tk event loop
    %
:- pred mtcltk.main(pred(tcl_interp, io, io)::(pred(in, di, uo) is det),
    list(string)::in, io::di, io::uo) is det.

    % eval(Interp, Command, Status, Result):
    %   evaluates `Command' in `Interp'.
    %   if successful, returns `Status = tcl_ok'
    %   and binds `Result' to the return string,
    %   otherwise returns `Status = tcl_error'
    %   and binds `Result' to the error message.
    %
:- pred eval(tcl_interp::in, string::in, tcl_status::out, string::out,
    io::di, io::uo) is det.

    % create_command(Interp, Name, Command):
    %   creates a new Tcl command called `Name' in `Interp'.
    %   Whenever `Name' is evaluated as a Tcl command in `Interp',
    %   the Tcl interpreter will use
    %       `call(Command, Interp, Args, Status, Result)'
    %   to invoke the Mercury procedure `Command' passing `Interp'
    %   and `Args', which is the list of arguments (including the
    %   command name `Name') passed to the command.  If successful,
    %   `Command' should return `Status = tcl_ok' and a return value
    %   in `Result'.  If an error occurs, `Command' should return
    %   `Status = tcl_error' and should bind `Result' to an
    %   appropriate error message.
    %
:- pred create_command(tcl_interp::in, string::in,
    pred(tcl_interp, list(string), tcl_status, string, io, io)
    ::(pred(in, in, out, out, di, uo) is det), io::di, io::uo) is det.

    % delete_command(Interp, Name, Result):
    %
    % Deletes the Tcl/Tk command called in `Name' in `Interp' and
    % returns `Result'.
    %
:- pred delete_command(tcl_interp::in, string::in, tcl_status::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_type("C", tcl_interp, "Tcl_Interp *",
   [can_pass_as_mercury_type]).

:- pragma foreign_decl("C", "
/*
** tkAppInit.c --
**
**  Provides a default version of the Tcl_AppInit procedure for
**  use in wish and similar Tk-based applications.
**
** Copyright (c) 1993 The Regents of the University of California.
** Copyright (c) 1994 Sun Microsystems, Inc.
**
** See the file ""license.terms"" for information on usage and redistribution
** of this file, and for a DISCLAIMER OF ALL WARRANTIES.
**
** SCCS: @(#) tkAppInit.c 1.20 96/02/15 18:55:27
**/

#if defined(__APPLE__) && defined(__MACH__)
    #include <Tk/tk.h>
#else
    #include ""tk.h""
#endif
").

:- pragma foreign_enum("C", tcl_status/0, [
    tcl_ok - "TCL_OK",
    tcl_error - "TCL_ERROR"
]).

:- pragma foreign_decl("C", "
    extern MR_Word mtcltk_mercury_initializer;
    extern char *mtcltk_strdup(const char *str);
").

:- pragma foreign_code("C", "
    MR_Word mtcltk_mercury_initializer;
").

:- pragma foreign_proc("C",
    main(Closure::pred(in, di, uo) is det, Args::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure], "
{
    MR_Word l;
    int     argc, i;
    char    **argv;
    MR_Word argv_word;

    /*
    ** Convert arguments from a list of strings to an array of strings.
    */
    argc = 0;
    for (l = Args; l != MR_list_empty(); l = MR_list_tail(l)) {
        argc++;
    }

    MR_incr_hp(argv_word, argc + 1);
    argv = (char **) argv_word;

    for (i = 0, l = Args; l != MR_list_empty(); l = MR_list_tail(l), i++) {
        argv[i] = (char *) MR_list_head(l);
    }

    argv[i] = NULL;

    mtcltk_mercury_initializer = Closure;

    Tk_Main(argc, argv, Tcl_AppInit);
}").

:- pragma foreign_export("C",
    call_mercury_initializer(pred(in, di, uo) is det, in, di, uo),
    "mtcltk_call_mercury_initializer").
:- pred call_mercury_initializer(
    pred(tcl_interp, io, io)::(pred(in, di, uo) is det),
    tcl_interp::in, io::di, io::uo) is det.
call_mercury_initializer(Closure, Interp, !IO) :-
    Closure(Interp, !IO).

:- pragma foreign_code("C", "
/*
** Tcl_AppInit --
**
**  This procedure performs application-specific initialization.
**  Most applications, especially those that incorporate additional
**  packages, will have their own version of this procedure.
**
** Results:
**  Returns a standard Tcl completion code, and leaves an error
**  message in interp->result if an error occurs.
**
** Side effects:
**  Depends on the startup script.
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

    Tcl_StaticPackage(interp, tk_str, Tk_Init,
        (Tcl_PackageInitProc *) NULL);

    mtcltk_call_mercury_initializer(mtcltk_mercury_initializer, interp);

    return TCL_OK;
}").

    % XXX Had to change Status to RStatus because using Status
    % resulted in *parse errors* in gcc :-(
:- pragma foreign_proc("C",
    eval(Interp::in, Cmd::in, RStatus::out, Result::out, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure],
"
    int err;

    err = Tcl_Eval(Interp, (char *)Cmd);

    switch (err) {
        case TCL_OK:
            RStatus = 0;
            break;

        case TCL_ERROR:
            RStatus = 1;
            break;

        default:
            MR_fatal_error(""Tcl_Eval returned neither TCL_OK or TCL_ERROR"");
    }

    Result = mtcltk_strdup(Tcl_GetStringResult(Interp));
").

:- pragma foreign_code("C", "
char *
mtcltk_strdup(const char *str)
{
    MR_Word newstr;

    MR_assert(str);
    MR_incr_hp_atomic(newstr, (strlen(str) + sizeof(MR_Word))
                / sizeof(MR_Word));
    MR_assert(newstr);
    strcpy((char *) newstr, str);

    return (char *) newstr;
}
").

:- pragma foreign_export("C",
    call_mercury_closure(pred(in, in, out, out, di, uo) is det,
        in, in, out, out, di, uo),
    "mtcltk_call_mercury_closure").
:- pred call_mercury_closure(
    pred(tcl_interp, list(string), tcl_status, string, io, io)
    ::(pred(in, in, out, out, di, uo) is det), tcl_interp::in,
    list(string)::in, tcl_status::out, string::out, io::di, io::uo)
    is det.
call_mercury_closure(Closure, Interp, Args, Status, Result, !IO) :-
    Closure(Interp, Args, Status, Result, !IO).

    % NOTE: CONST is defined in tcl.h.
    %
:- pragma foreign_decl("C", "

extern int
mtcltk_do_callback(ClientData clientData, Tcl_Interp *interp,
    int argc, CONST char *argv[]);

").

:- pragma foreign_code("C", "

int
mtcltk_do_callback(ClientData clientData, Tcl_Interp *interp,
    int argc, CONST char *argv[])
{
    MR_Word status;
    MR_Word args;
    int i;

    /*
    ** Convert the array of strings into a Mercury list of strings.
    */
    args = MR_list_empty();
    for (i = argc - 1; i >= 0; i--) {
        args = MR_list_cons((MR_Word) mtcltk_strdup(argv[i]),
            (MR_Word) args);
    }

    MR_String m_result;
    mtcltk_call_mercury_closure((MR_Word) clientData, interp,
        args, &status, &m_result);

    // Copy result string into Tcl allocated memory.
    char *t_result = Tcl_Alloc(strlen(m_result) + 1);
    if (t_result == NULL) {
        return TCL_ERROR;
    }
    strcpy(t_result, m_result);

    Tcl_SetResult(interp, t_result, TCL_DYNAMIC);
/*
    fprintf(stderr, ""mercury result: `%s'\n"", m_result);
*/
    return (mtcltk_tcl_status_ok(status) ? TCL_OK : TCL_ERROR);
}

").

:- pragma foreign_export("C", tcl_status_ok(in), "mtcltk_tcl_status_ok").
:- pred tcl_status_ok(tcl_status::in) is semidet.
tcl_status_ok(tcl_ok).

:- pragma foreign_proc("C",
    create_command(Interp::in, Name::in,
        Closure::pred(in, in, out, out, di, uo) is det,
        _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure],
"
    Tcl_CreateCommand(Interp, Name, mtcltk_do_callback,
        (ClientData)Closure, NULL);
").

:- pragma foreign_proc("C",
    delete_command(Interp::in, Name::in, Result::out, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure],
"
    int err;
    err = Tcl_DeleteCommand(Interp, Name);
    Result = (err == 0 ? 0 : 1);
").

:- pragma foreign_code("C", "

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
    return GC_MALLOC_UNCOLLECTABLE(s);
}

void *calloc(size_t s, size_t n)
{
    void *t;
    t = GC_MALLOC_UNCOLLECTABLE(s*n);
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

#endif /* MR_CONSERVATIVE_GC */

").

%-----------------------------------------------------------------------------%
:- end_module mtcltk.
%-----------------------------------------------------------------------------%
