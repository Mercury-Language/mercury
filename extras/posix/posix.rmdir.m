%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001, 2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Module: posix.rmdir.
% Main author: Michael Day <miked@lendtech.com.au>
%
%------------------------------------------------------------------------------%

:- module posix.rmdir.
:- interface.

:- import_module string.

:- pred rmdir(string::in, posix.result::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "
    #include <unistd.h>
").

%-----------------------------------------------------------------------------%

rmdir(Path, Result, !IO) :-
    rmdir0(Path, Res, !IO),
    ( if Res = 0 then
        Result = ok
    else
        errno(Err, !IO),
        Result = error(Err)
    ).                  

:- pred rmdir0(string::in, int::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    rmdir0(Path::in, Res::out, IO0::di, IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, tabled_for_io],
" 
    Res = rmdir(Path);
    IO = IO0;
").
        
%-----------------------------------------------------------------------------%
:- end_module posix.rmdir.
%-----------------------------------------------------------------------------%
