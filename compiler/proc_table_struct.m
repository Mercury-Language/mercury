%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% Copyright (C) 2013-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: proc_table_struct.m.
% Main authors: fjh, conway.
%
% This module defines types that are specific to fields of proc_infos.
%
%---------------------------------------------------------------------------%

:- module hlds.proc_table_struct.
:- interface.

:- import_module hlds.hlds_rtti.
:- import_module hlds.proc_info_types.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.

:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- type proc_table_struct_info
    --->    proc_table_struct_info(
                % The information we need to create the data structures
                % created by tabling for a procedure, and to interpret them
                % for the debugger (except the information -such as
                % determinism- that is already available from proc_layout
                % structures.
                %
                % The table_arg_infos list first all the input arguments,
                % then all the output arguments.
                %
                % The right tvarset for interpreting the types in the
                % table_arg_infos is the one stored below. It is taken from
                % the proc_info of the procedure whose table this structure
                % describes. Since we care only about the shapes of the types,
                % we don't care about neither the actual numerical values
                % nor the names of the type variables, so we don't care if
                % the tvarset in that proc_info changes after table_gen.m
                % takes the snapshot stored here.
                %
                % We record the rtti_proc_label of the procedure whose table
                % this is. We can't record its identity in the form of a
                % pred_proc_id, since that won't work if the procedure is
                % deleted before the code generation phase.

                ptsi_proc_label             :: rtti_proc_label,
                ptsi_tvarset                :: tvarset,
                ptsi_context                :: prog_context,
                ptsi_num_inputs             :: int,
                ptsi_num_outputs            :: int,
                ptsi_input_steps            :: list(table_step_desc),
                ptsi_maybe_output_steps     :: maybe(list(table_step_desc)),
                ptsi_gen_arg_infos          :: table_arg_infos,
                ptsi_eval_method            :: tabled_eval_method
            ).

:- type table_step_desc
    --->    table_step_desc(
                tsd_var_name                :: string,
                tsd_step                    :: table_trie_step
            ).

    % This type differs from the type table_step_kind in table_statistics.m
    % in the library in that
    % (a) in gives more information about the type of the corresponding
    % argument (if this info is needed and available),
    % (b) it doesn't have to be an enum, and
    % (c) it doesn't have to handle dummy steps.
    %
:- type table_trie_step
    --->    table_trie_step_dummy
    ;       table_trie_step_int(int_type)
    ;       table_trie_step_char
    ;       table_trie_step_string
    ;       table_trie_step_float
    ;       table_trie_step_enum(
                % The int gives the maximum enum value in the enum type + 1,
                % and thus the size of the corresponding trie node.
                % If the enum type is not a subtype, then the value is equal to
                % the number of alternatives in the type.
                int
            )
    ;       table_trie_step_foreign_enum
    ;       table_trie_step_general(
                mer_type,
                table_is_poly,
                table_value_or_addr
            )
    ;       table_trie_step_typeinfo
    ;       table_trie_step_typeclassinfo
    ;       table_trie_step_promise_implied.

    % Return a description of what kind of statistics we collect for a trie
    % step of a given kind. The description is the name of a value in the C
    % enum type MR_TableStepStatsKind. (We will need to generalize this
    % when we implement tabling for non-C backends.)
    %
:- func table_step_stats_kind(table_trie_step) = string.

:- type table_is_poly
    --->    table_is_mono       % The table type is monomorphic.
    ;       table_is_poly.      % The table type is polymorphic.

:- type table_value_or_addr
    --->    table_value         % We are tabling the value itself.
    ;       table_addr.         % We are tabling only the address.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module varset.

%---------------------------------------------------------------------------%

table_step_stats_kind(Step) = KindStr :-
    (
        ( Step = table_trie_step_int(_)
        ; Step = table_trie_step_char
        ; Step = table_trie_step_string
        ; Step = table_trie_step_float
        ; Step = table_trie_step_typeinfo
        ; Step = table_trie_step_typeclassinfo
        ; Step = table_trie_step_foreign_enum
        ),
        KindStr = "MR_TABLE_STATS_DETAIL_HASH"
    ;
        Step = table_trie_step_enum(_),
        KindStr = "MR_TABLE_STATS_DETAIL_ENUM"
    ;
        Step = table_trie_step_general(_Type, IsPoly, ValueOrAddr),
        (
            ValueOrAddr = table_addr,
            KindStr = "MR_TABLE_STATS_DETAIL_HASH"
        ;
            ValueOrAddr = table_value,
            (
                IsPoly = table_is_mono,
                KindStr = "MR_TABLE_STATS_DETAIL_DU"
            ;
                IsPoly = table_is_poly,
                KindStr = "MR_TABLE_STATS_DETAIL_POLY"
            )
        )
    ;
        ( Step = table_trie_step_promise_implied
        ; Step = table_trie_step_dummy
        ),
        KindStr = "MR_TABLE_STATS_DETAIL_NONE"
    ).

%---------------------------------------------------------------------------%
:- end_module hlds.proc_table_struct.
%---------------------------------------------------------------------------%
