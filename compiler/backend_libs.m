%-----------------------------------------------------------------------------%
% Copyright (C) 2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Back-end libraries.
%
% This package contains utility modules that are each used by
% several different back-ends.
%
:- module backend_libs.
:- interface.
:- import_module transform_hlds, check_hlds. % are these needed?
:- import_module hlds, parse_tree, libs.

% modules that provide functionality used by several different back-ends
:- include_module builtin_ops.
:- include_module bytecode_data.
:- include_module c_util.
:- include_module code_model.
:- include_module switch_util.
:- include_module rtti, type_ctor_info, pseudo_type_info, base_typeclass_info.
:- include_module foreign, export.
:- include_module compile_target_code.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module ll_backend. % XXX for llds_out__name_mangle.

:- end_module backend_libs.

%-----------------------------------------------------------------------------%
