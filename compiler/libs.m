%-----------------------------------------------------------------------------%
% Copyright (C) 2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% This package contains general utilities that are used by other packages.
%

:- module libs.
:- interface.
:- import_module ll_backend. % XXX trace_params depends on llds__trace_port.

% option handling
:- include_module globals, options, handle_options, trace_params.

% generic algorithms and data structures that are not
% quite useful enough to go in the standard library
:- include_module tree, graph_colour, atsort.

% OS interfaces not provided by the standard library
:- include_module timestamp.
:- include_module process_util.

:- end_module libs.

%-----------------------------------------------------------------------------%
