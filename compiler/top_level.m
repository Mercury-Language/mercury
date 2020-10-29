%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2009 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% This package contains the top-level stuff that uses all the other packages.
%
%-----------------------------------------------------------------------------%

:- module top_level.
:- interface.

:- include_module mercury_compile_main.
:- include_module mercury_compile_front_end.
:- include_module mercury_compile_middle_passes.
:- include_module mercury_compile_llds_back_end.
:- include_module mercury_compile_mlds_back_end.

%-----------------------------------------------------------------------------%
:- end_module top_level.
%-----------------------------------------------------------------------------%
