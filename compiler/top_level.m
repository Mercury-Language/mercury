%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2009 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% This package contains the top-level stuff that uses all the other packages.
% In particular it contains the module mercury_compile.m, which defines main/2,
% and which invokes all the other parts of the Mercury compiler.

:- module top_level.
:- interface.

% The front-end phases.
:- import_module analysis.
:- import_module check_hlds.
:- import_module hlds.
:- import_module mode_robdd.
:- import_module parse_tree.
:- import_module transform_hlds.

% Back-ends that we currently use or plan to use.
:- import_module ll_backend.
:- import_module ml_backend.
:- import_module erl_backend.

% Incomplete back-ends.
:- import_module bytecode_backend.

% Misc utilities.
:- import_module backend_libs.
:- import_module libs.

:- include_module mercury_compile.
:- include_module mercury_compile_front_end.
:- include_module mercury_compile_middle_passes.
:- include_module mercury_compile_erl_back_end.
:- include_module mercury_compile_llds_back_end.
:- include_module mercury_compile_mlds_back_end.

% XXX It would be nicer to define `main' in top_level.mercury_compile,
% rather than defining it here. But that doesn't work with the Mercury
% compiler's .NET back-end, which assumes that main is defined in the program's
% top-level module.

:- use_module io.
:- pred main(io.state::di, io.state::uo) is det.

:- implementation.

:- use_module top_level.mercury_compile.

main(!IO) :-
    top_level.mercury_compile.real_main(!IO).

%-----------------------------------------------------------------------------%
:- end_module top_level.
%-----------------------------------------------------------------------------%
