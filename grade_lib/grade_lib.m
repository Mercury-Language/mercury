%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2016-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% This is the top module of a library that handles grades.
% It contains
%
% - several representations of grades (you can see their descriptions
%   in compiler/notes/grade_library.html),
% - ways to turn representations of grades into strings, and vice versa,
% - ways to create partial specifications of grades (i.e. grade problems),
% - algorithms for solving those grade problems by checking their consistency
%   and extending them into full specifications.
%
% This library is intended to be linked into only (a) the compiler, and
% (b) the standalone programs in this directory.
%
% The modules in this library should import and use, besides each other,
% only modules of the Mercury standard library.

:- module grade_lib.

:- interface.

:- pred grade_lib_version(string::out) is det.

:- include_module grade_setup.
:- include_module grade_solver.
:- include_module grade_spec.
:- include_module grade_state.
:- include_module grade_string.
:- include_module grade_structure.
:- include_module grade_vars.

:- implementation.

grade_lib_version("v1").

%---------------------------------------------------------------------------%
:- end_module grade_lib.
%---------------------------------------------------------------------------%
