%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2012 The University of Melbourne.
% Copyright (C) 2013-2017, 2019-2025 The Mercury team.
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
:- include_module get_module_dep_info.
:- include_module module_dep_file.
:- include_module options_file.
:- include_module prereqs_cache.
:- include_module top_level.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- include_module check_up_to_date.
:- include_module clean.
:- include_module file_names.
:- include_module find_local_modules.
:- include_module hash.
:- include_module index_set.
:- include_module library_install.
:- include_module make_info.
:- include_module module_target.
:- include_module prereqs.
:- include_module program_target.
:- include_module timestamp.
:- include_module track_flags.
:- include_module util.

%---------------------------------------------------------------------------%
:- end_module make.
%---------------------------------------------------------------------------%
