%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2003 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% This package contains the top-level stuff that uses all the
% other packages.  In particular it contains the module mercury_compile.m,
% which defines main/2, and which invokes all the other parts of the
% Mercury compiler.
%

:- module top_level.
:- interface.

% the front-end phases
:- import_module check_hlds.
:- import_module hlds.
:- import_module parse_tree.
:- import_module transform_hlds.

% back-ends that we currently use or plan to use
:- import_module aditi_backend.
:- import_module ll_backend.
:- import_module ml_backend.

% obsolete or incomplete back-ends
:- import_module bytecode_backend.

% misc utilities
:- import_module backend_libs.
:- import_module libs.

:- include_module mercury_compile.

% XXX It would be nicer to define `main' in top_level.mercury_compile,
%     rather than defining it here.  But that doesn't work with the
%     Mercury compiler's .NET back-end, which assumes that main is defined
%     in the program's top-level module.
:- use_module io.
:- use_module top_level.mercury_compile.
:- pred main(io.state::di, io.state::uo) is det.

:- implementation.

main --> top_level.mercury_compile.real_main.

:- end_module top_level.

%-----------------------------------------------------------------------------%
