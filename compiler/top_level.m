%-----------------------------------------------------------------------------%
% Copyright (C) 2002 The University of Melbourne.
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
:- import_module parse_tree, hlds, check_hlds, transform_hlds.

% back-ends that we currently use or plan to use
:- import_module aditi_backend, ll_backend, ml_backend.

% obsolete or incomplete back-ends
:- import_module bytecode_backend.

% misc utilities
:- import_module libs, backend_libs.

:- include_module mercury_compile.

:- end_module top_level.

%-----------------------------------------------------------------------------%
