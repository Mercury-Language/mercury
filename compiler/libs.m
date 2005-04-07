%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2003, 2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% This package contains general utilities that are used by other packages.
%

:- module libs.
:- interface.

% option handling
:- include_module globals.
:- include_module handle_options.
:- include_module options.
:- include_module trace_params.

% generic algorithms and data structures that are not
% quite useful enough to go in the standard library
:- include_module atsort.
:- include_module graph_colour.
:- include_module tree.

% OS interfaces not provided by the standard library
:- include_module process_util.
:- include_module timestamp.

% Constraint machinery for the termination analyser.
:- include_module lp_rational.
:- include_module polyhedron.
:- include_module rat.

:- end_module libs.

%-----------------------------------------------------------------------------%
