%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2009 The University of Melbourne.
% Copyright (C) 2016 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module defines main/2.  Note that main/2 forwards all of its work to
% mercury_compile_main.real_main/2, but main/2 must be defined in this module
% so that the compiler executable is generated with the right name.
%
%---------------------------------------------------------------------------%

:- module mercury_compile.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module top_level.
:- import_module top_level.mercury_compile_main.

main(!IO) :-
    mercury_compile_main.real_main(!IO).

%---------------------------------------------------------------------------%
:- end_module mercury_compile.
%---------------------------------------------------------------------------%
