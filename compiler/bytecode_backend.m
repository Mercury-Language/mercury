%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2003 The University of Melbourne.
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

:- import_module backend_libs.
:- import_module check_hlds. 		% is this needed?
:- import_module hlds.
:- import_module libs.
:- import_module parse_tree.
:- import_module transform_hlds.	% is this needed?

%-----------------------------------------------------------------------------%

:- include_module bytecode.
:- include_module bytecode_gen.

%-----------------------------------------------------------------------------%

:- implementation.
	% bytecode_gen uses ll_backend__call_gen.m
:- import_module ll_backend.

:- end_module bytecode_backend.

%-----------------------------------------------------------------------------%
