%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% The Erlang back-end.
%
% This package includes
% - the ELDS data structure, which is an abstract
%   representation of a subset of the Erlang language;
% - the ELDS code generator, which converts HLDS to ELDS;
% - the Erlang back-end which writes out the ELDS as Erlang code.
%
:- module erl_backend.
:- interface.

:- import_module hlds.
:- import_module parse_tree.

%-----------------------------------------------------------------------------%

:- include_module elds.

:- include_module erl_code_gen.
   :- include_module erl_call_gen.
   :- include_module erl_unify_gen.
:- include_module erl_code_util.
:- include_module erl_rtti.

:- include_module elds_to_erlang.

:- implementation.

:- import_module backend_libs.
:- import_module check_hlds.
:- import_module libs.
:- import_module mdbcomp.

%-----------------------------------------------------------------------------%
:- end_module erl_backend.
%-----------------------------------------------------------------------------%
