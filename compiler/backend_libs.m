%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2003 The University of Melbourne.
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
:- import_module check_hlds. 		% is this needed?
:- import_module hlds.
:- import_module libs.
:- import_module parse_tree.
:- import_module transform_hlds.	% is this needed?

% modules that provide functionality used by several different back-ends
:- include_module base_typeclass_info.
:- include_module builtin_ops.
:- include_module bytecode_data.
:- include_module c_util.
:- include_module code_model.
:- include_module compile_target_code.
:- include_module export.
:- include_module foreign.
:- include_module matching.
:- include_module name_mangle.
:- include_module proc_label.
:- include_module pseudo_type_info.
:- include_module rtti.
:- include_module switch_util.
:- include_module type_ctor_info.
:- include_module type_class_info.

:- end_module backend_libs.

%-----------------------------------------------------------------------------%
