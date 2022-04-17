%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2022 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: make_hlds_type.m.
%
% This module defines the types that are used only during
% the construction of the HLDS.
%
%---------------------------------------------------------------------------%

:- module hlds.make_hlds.make_hlds_types.
:- interface.

:- import_module hlds.status.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module cord.
:- import_module list.

%---------------------------------------------------------------------------%

:- type found_invalid_type
    --->    did_not_find_invalid_type
    ;       found_invalid_type.

:- type found_invalid_inst_or_mode
    --->    did_not_find_invalid_inst_or_mode
    ;       found_invalid_inst_or_mode.

:- type ims_list(T) == list(ims_sub_list(T)).
:- type ims_cord(T) == cord(ims_sub_list(T)).
:- type ims_sub_list(T)
    --->    ims_sub_list(item_mercury_status, list(T)).

:- type sec_list(T) == list(sec_sub_list(T)).
:- type sec_cord(T) == cord(sec_sub_list(T)).
:- type sec_sub_list(T)
    --->    sec_sub_list(sec_info, list(T)).
:- type sec_info
    --->    sec_info(
                % The status of the items defined in the section.
                item_mercury_status,

                % Whether you need to fully module qualify the items
                % that are defined in this section, directly or indirectly.
                %
                % There is one item that defines something directly
                % to which this is relevant: predicate items.
                % For them, this controls whether a reference to the predicate
                % (or function) has to be fully qualified or not.
                %
                % For typeclass items, this controls whether references
                % to the method predicates and/or functions has to be
                % fully qualified or not.
                %
                % For mutable items, this controls whether references
                % to the access predicates have to be fully qualified or not.
                %
                % For type defn items for solver types, this controls whether
                % references to the solver type's auxiliary predicates
                % (for representation changes) have to be fully qualified
                % or not.
                %
                % For type defn items for du types, this controls whether
                % references to the type's function symbols have to be
                % fully qualified or not.
                %
                % As it happens, ALL these references can occur only in
                % the implementation section of a Mercury module.
                % This means that in cases where an imported module's
                % qualification needs are different in the interface and in the
                % implementation (due to it being imported via `use_module'
                % in the interface and `import_module' in the implementation),
                % this field should be set to the value appropriate for the
                % implementation section.
                need_qualifier
            ).

%---------------------------------------------------------------------------%
:- end_module hlds.make_hlds.make_hlds_types.
%---------------------------------------------------------------------------%
