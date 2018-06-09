%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000, 2012 The University of Melbourne.
% Copyright (C) 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% module:   main.m
% main author:  Peter Ross (petdr@miscrit.be)
%
% Use the logged_output stream.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module main.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module logged_output.

main(!IO) :-
    logged_output.init("OUTPUT", Result, !IO),
    ( if Result = ok(OutputStream) then
        io.write_string(OutputStream, "Hi there.\n", !IO)
      else 
        io.write_string("Unable to open OUTPUT\n", !IO)
    ).

%-----------------------------------------------------------------------------%
:- end_module main.
%-----------------------------------------------------------------------------%
