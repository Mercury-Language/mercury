%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2016 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module defines the types that represent event specifications
% in the parse tree.
%
%---------------------------------------------------------------------------%

:- module parse_tree.prog_data_event.
:- interface.

:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.

:- type event_attribute
    --->    event_attribute(
                attr_num                    :: int,
                attr_name                   :: string,
                attr_type                   :: mer_type,
                attr_mode                   :: mer_mode,
                attr_maybe_synth_call       :: maybe(event_attr_synth_call)
            ).

:- type event_attr_synth_call
    --->    event_attr_synth_call(
                synth_func_attr_name_num    :: pair(string, int),
                synth_arg_attr_name_nums    :: assoc_list(string, int),
                synth_eval_order            :: list(int)
            ).

:- type event_spec
    --->    event_spec(
                event_spec_num              :: int,
                event_spec_name             :: string,
                event_spec_linenum          :: int,
                event_spec_attrs            :: list(event_attribute),
                event_spec_synth_order      :: list(int)
            ).

    % This type maps the name of an event to the event's specification.
:- type event_spec_map == map(string, event_spec).

:- type event_set
    --->    event_set(
                event_set_name              :: string,
                event_set_spec_map          :: event_spec_map
            ).

:- type event_set_data
    --->    event_set_data(
                event_set_data_name         :: string,
                event_set_data_description  :: string,
                event_set_data_specs        :: list(event_spec),
                event_set_data_max_num_attr :: int
            ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.prog_data_event.
%---------------------------------------------------------------------------%
