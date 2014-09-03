%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2006 The University of Melbourne.
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

%-----------------------------------------------------------------------------%

:- include_module bytecode.
:- include_module bytecode_gen.

%-----------------------------------------------------------------------------%

:- implementation.
:- end_module bytecode_backend.

%-----------------------------------------------------------------------------%
