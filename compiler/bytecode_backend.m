%-----------------------------------------------------------------------------%
% Copyright (C) 2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% This package contains the bytecode generator.
%
% Note that the bytecode interpreter, which was supposed to interpret
% the bytecodes that this back-end generates, is not yet implemented.
%
:- module bytecode_backend.
:- interface.
:- import_module transform_hlds, check_hlds. % are these needed?
:- import_module hlds, parse_tree, libs, backend_libs.

%-----------------------------------------------------------------------------%

:- include_module bytecode, bytecode_gen.

%-----------------------------------------------------------------------------%

:- implementation.
	% bytecode_gen uses ll_backend__call_gen.m
:- import_module ll_backend.

:- end_module bytecode_backend.

%-----------------------------------------------------------------------------%
