%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2003, 2005, 2008-2009 The University of Melbourne.
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
:- include_module handle_options.
:- include_module options.
:- include_module trace_params.

% Error handling.
:- include_module compiler_util.

% Generic algorithms and data structures that are not quite useful enough
% or otherwise aren't in the standard library.
:- include_module atsort.
:- include_module file_util.
:- include_module graph_colour.
:- include_module int_emu.
:- include_module md4.
:- include_module pickle.

% OS interfaces not provided by the standard library.
:- include_module process_util.
:- include_module timestamp.

% Constraint machinery for the termination analysers.
:- include_module lp.
:- include_module lp_rational.
:- include_module polyhedron.
:- include_module rat.

%-----------------------------------------------------------------------------%
:- end_module libs.
%-----------------------------------------------------------------------------%
