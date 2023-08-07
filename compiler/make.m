%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2012 The University of Melbourne.
% Copyright (C) 2013-2021 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: make.m.
% Main author: stayl.
%
% A package for "mmc --make", a builtin Mercury-specific make replacement.
%
%---------------------------------------------------------------------------%

:- module make.
:- interface.

:- include_module build.
:- include_module module_dep_file.
:- include_module options_file.
:- include_module top_level.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- include_module dependencies.
:- include_module deps_set.
:- include_module file_names.
:- include_module hash.
:- include_module make_info.
:- include_module module_target.
:- include_module program_target.
:- include_module timestamp.
:- include_module track_flags.
:- include_module util.

%---------------------------------------------------------------------------%
:- end_module make.
%---------------------------------------------------------------------------%
