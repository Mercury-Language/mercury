%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2008 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: display.m.
% Author: pbone.
%
% This module contains a data structure for displaying deep profiler reports.
% It may be versatile enough for other uses.
%
%-----------------------------------------------------------------------------%

:- module display.
:- interface.

:- import_module measurement_units.
:- import_module query.

:- import_module cord.
:- import_module list.
:- import_module maybe.
:- import_module string.

%-----------------------------------------------------------------------------%

:- type display
    --->    display(
                display_title       :: maybe(string),
                display_content     :: list(display_item)
            ).

:- type display_item
    --->    display_message(string)
    ;       display_table(table)
    ;       display_list(
                % Class of the list, may be used to display the list.
                list_class,

                % An optional title.
                maybe(string),

                % Items within the list.
                list(display_item)
            )
    ;       display_command_link(deep_link).

%-----------------------------------------------------------------------------%
%
% Table specific structures.
%

:- type table
    --->    table(
                % Enumeration of what the table stores, this can be used
                % for layout hints.
                table_class     :: table_class,

                % The number of columns in the table. If the number of cells
                % in any data row is not equal to this number, the table is not
                % well formed. The number of cells in the header may be smaller
                % than this value when there are header cells that span
                % multiple sub-header cells.
                table_num_cols  :: int,

                % The header row of the table.
                table_header    :: maybe(table_header),

                % The data in the table.
                table_rows      :: list(table_row)
            ).

:- type table_header
    --->    table_header(
                th_groups       :: list(table_header_group)
            ).

:- type table_header_group
    --->    table_header_group(
                % The table contents.
                thg_titles      :: table_header_group_columns,

                % The class may be used by a layout to make decisions
                % about how to paint this column.
                thg_class       :: table_column_class,

                thg_set_style   :: table_set_style
            ).

:- type table_header_group_columns
    --->    table_header_group_single(
                % The header of the single column in the group.
                thsc_title      :: table_data
            )
    ;       table_header_group_multi(
                % The spanning header, which applies to all columns
                % in the group.
                thmc_title      :: string,

                % The headers of the individual columns in the group.
                thmc_subtitles  :: list(table_data)
            ).

:- type table_set_style
    --->    table_set_style
    ;       table_do_not_set_style.

:- type table_row
    --->    table_row(
                tr_cells    :: list(table_cell)
            )
    ;       table_section_header(
                tsh_text    :: table_data
            ).

:- type table_cell
    --->    table_cell(
                tc_text     :: table_data
            )
    ;       table_empty_cell.

:- type table_class
    --->    table_class_plain
    ;       table_class_boxed.

:- type table_column_class
    --->    table_column_class_allocations
    ;       table_column_class_callseqs
    ;       table_column_class_memory
    ;       table_column_class_no_class
    ;       table_column_class_number
    ;       table_column_class_ordinal_rank
    ;       table_column_class_port_counts
    ;       table_column_class_proc
    ;       table_column_class_ticks_and_times.

    % Table data can be specified by type to allow formatting, for example
    % to align decimal points.
    %
:- type table_data
    --->    td_f(float)
    ;       td_i(int)
    ;       td_l(deep_link)
    ;       td_m(
                % The amount of memory.
                memory,

                % The units to display memory in.
                memory_units,

                % The number of decimal places to show.
                int
            )
    ;       td_p(percent)
    ;       td_s(string)
    ;       td_t(time).

:- func make_single_table_header_group(table_data,
    table_column_class, table_set_style) = table_header_group.

:- func make_multi_table_header_group(string, list(table_data),
    table_column_class, table_set_style) = table_header_group.

%-----------------------------------------------------------------------------%
%
% List specific structures
%

:- type list_class
    --->    list_class_vertical_no_bullets
    ;       list_class_vertical_bullets
    ;       list_class_horizontal.

%-----------------------------------------------------------------------------%
%
% Link specific structures
%

:- type deep_link
    --->    deep_link(
                % The link command.
                cmd,

                % The preferences for the link command.
                maybe(preferences),

                % A label for the link.
                string,

                % Class of the link may control how it is displayed.
                link_class
            ).

:- type link_class
    --->    link_class_link
    ;       link_class_control.

%-----------------------------------------------------------------------------%
%
% Predicates for working with display structures.
%

    % If given a header for a (group of) columns, this predicate adds it
    % to the end of the cord and adds the correct number of columns
    % to the column count.
    %
:- pred table_maybe_add_header_group(maybe(table_header_group)::in,
    cord(table_header_group)::in, cord(table_header_group)::out,
    int::in, int::out) is det.

    % Given a header for a (group of) columns, this predicate adds it
    % to the end of the cord and adds the correct number of columns
    % to the column count.
    %
:- pred table_add_header_group(table_header_group::in,
    cord(table_header_group)::in, cord(table_header_group)::out,
    int::in, int::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module int.
:- import_module pair.

%-----------------------------------------------------------------------------%

make_single_table_header_group(ColumnTitle, ColumnClass, SetStyle) =
    table_header_group(table_header_group_single(ColumnTitle),
        ColumnClass, SetStyle).

make_multi_table_header_group(MainTitle, SubTitles, ColumnClass, SetStyle) =
    table_header_group(table_header_group_multi(MainTitle, SubTitles),
        ColumnClass, SetStyle).

%-----------------------------------------------------------------------------%

table_maybe_add_header_group(MaybeHeaderGroup, !HeaderGroups, !NumColumns) :-
    (
        MaybeHeaderGroup = yes(HeaderGroup),
        table_add_header_group(HeaderGroup, !HeaderGroups, !NumColumns)
    ;
        MaybeHeaderGroup = no
    ).

table_add_header_group(HeaderGroup, !HeaderGroups, !NumColumns) :-
    HeaderGroup = table_header_group(ColumnTitles, _, _),
    (
        ColumnTitles = table_header_group_single(_),
        GroupColumns = 1
    ;
        ColumnTitles = table_header_group_multi(_, SubTitles),
        list.length(SubTitles, GroupColumns)
    ),
    !:HeaderGroups = cord.snoc(!.HeaderGroups, HeaderGroup),
    !:NumColumns = !.NumColumns + GroupColumns.

%-----------------------------------------------------------------------------%
:- end_module display.
%-----------------------------------------------------------------------------%
