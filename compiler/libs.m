%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2003, 2005, 2008-2009 The University of Melbourne.
% Copyright (C) 2015, 2017, 2019-2025 The Mercury team
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% This package contains general utilities that are used by other packages.
%

:- module libs.
:- interface.

% Option handling.
:- include_module globals.
:- include_module options.
:- include_module handle_options.
:- include_module check_options.
:- include_module optimization_options.
:- include_module op_mode.
:- include_module compute_grade.
:- include_module trace_params.
:- include_module color_schemes.

% Error handling.
:- include_module compiler_util.
:- include_module maybe_util.

% Representation of mmakefile fragments.
:- include_module mmakefiles.

% Existence checks for required libraries.
:- include_module check_libgrades.

% Generic algorithms and data structures that for one reason or another
% are not in the standard library.
:- include_module dependency_graph.
:- include_module file_util.
:- include_module graph_colour.
:- include_module indent.
:- include_module lines_builder.
:- include_module md5.
:- include_module pickle.
:- include_module va_map.

% OS interfaces not provided by the standard library.
:- include_module copy_util.
:- include_module process_util.
:- include_module shell_util.
:- include_module system_cmds.
:- include_module timestamp.

% Constraint machinery for the termination analysers.
:- include_module lp.
:- include_module lp_rational.
:- include_module polyhedron.
:- include_module rat.

%-----------------------------------------------------------------------------%
:- end_module libs.
%-----------------------------------------------------------------------------%
