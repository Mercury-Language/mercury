%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: prog_item_inst_mode.m.
% Author: zs.
%
%---------------------------------------------------------------------------%

:- module parse_tree.prog_item_inst_mode.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module map.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- type item_inst_defn_info
    == item_inst_defn_info_general(maybe_abstract_inst_defn).

:- type item_inst_defn_info_abstract
    == item_inst_defn_info_general(no_inst_defn).
:- type item_inst_defn_info_eqv
    == item_inst_defn_info_general(inst_defn).

:- type item_inst_defn_info_general(T)
    --->    item_inst_defn_info(
                % `:- inst ... = ...':
                % a definition of an inst.
                id_inst_name                    :: sym_name,
                id_inst_args                    :: list(inst_var),
                id_maybe_for_type               :: maybe(type_ctor),
                id_inst_defn                    :: T,
                id_varset                       :: inst_varset,
                id_context                      :: prog_context,
                id_seq_num                      :: item_seq_num
            ).

:- type no_inst_defn
    --->    no_inst_defn.

:- type maybe_abstract_inst_defn
    --->    abstract_inst_defn
    ;       nonabstract_inst_defn(inst_defn).

%---------------------------------------------------------------------------%

:- type item_mode_defn_info
    == item_mode_defn_info_general(maybe_abstract_mode_defn).

:- type item_mode_defn_info_abstract
    == item_mode_defn_info_general(no_mode_defn).
:- type item_mode_defn_info_eqv
    == item_mode_defn_info_general(mode_defn).

:- type item_mode_defn_info_general(T)
    --->    item_mode_defn_info(
                % `:- mode ... = ...':
                % a definition of a mode.
                md_mode_name                    :: sym_name,
                md_mode_args                    :: list(inst_var),
                md_mode_defn                    :: T,
                md_varset                       :: inst_varset,
                md_context                      :: prog_context,
                md_seq_num                      :: item_seq_num
            ).

:- type no_mode_defn
    --->    no_mode_defn.

:- type maybe_abstract_mode_defn
    --->    abstract_mode_defn
    ;       nonabstract_mode_defn(mode_defn).

%---------------------------------------------------------------------------%
%
% The representation of a checked-to-be-consistent set of inst definitions
% for every inst constructor defined in a module.
%

:- type inst_ctor_checked_map == map(inst_ctor, inst_ctor_checked_defn).

:- type inst_ctor_checked_defn
    --->    checked_defn_inst(std_inst_defn, src_defns_inst).

:- type std_inst_defn
    --->    std_inst_defn(std_inst_status, item_inst_defn_info).

:- type std_inst_status
    --->    std_inst_exported
            % The inst definition is exported.
    ;       std_inst_abstract_exported
            % Only the inst name is exported. Its definition is private.
    ;       std_inst_all_private.
            % Everything about the inst is private.

:- type src_defns_inst
    --->    src_defns_inst(
                % The inst definition (if any) in the interface.
                maybe(item_inst_defn_info),

                % The inst definition (if any) in the implementation.
                maybe(item_inst_defn_info)
            ).

%---------------------------------------------------------------------------%
%
% The representation of a checked-to-be-consistent set of mode definitions
% for every mode constructor defined in a module.
%

:- type mode_ctor_checked_map == map(mode_ctor, mode_ctor_checked_defn).

:- type mode_ctor_checked_defn
    --->    checked_defn_mode(std_mode_defn, src_defns_mode).

:- type std_mode_defn
    --->    std_mode_defn(std_mode_status, item_mode_defn_info).

:- type std_mode_status
    --->    std_mode_exported
            % The mode definition is exported.
    ;       std_mode_abstract_exported
            % Only the mode name is exported. Its definition is private.
    ;       std_mode_all_private.
            % Everything about the mode is private.

:- type src_defns_mode
    --->    src_defns_mode(
                % The mode definition (if any) in the interface.
                maybe(item_mode_defn_info),

                % The mode definition (if any) in the implementation.
                maybe(item_mode_defn_info)
            ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.prog_item_inst_mode.
%---------------------------------------------------------------------------%
