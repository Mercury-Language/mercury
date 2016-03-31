%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2016 The Mercury team.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This is the top module of a library that handles grades.
% It contains
%
% - several representations of grades (you can see their descriptions
%   in compiler/notes/grade_library.html),
% - ways to specify grades,
% - algorithms for turning those specifications into grades themselves,
% - ways to turn representations of grades into strings, and vice versa.
%
% This library is intended to be linked only into the compiler, and into
% the standalone programs in this directory.
%
% The modules in this library should import and use, besides each other,
% only modules of the Mercury standard library.

:- module grade_lib.

:- interface.

:- pred grade_lib.version(string::out) is det.

:- include_module grade_setup.
:- include_module grade_solver.
:- include_module grade_spec.
:- include_module grade_state.
:- include_module grade_string.
:- include_module grade_structure.
:- include_module grade_vars.

:- implementation.

grade_lib.version("v0").

%---------------------------------------------------------------------------%
:- end_module grade_lib.
%---------------------------------------------------------------------------%

