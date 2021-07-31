%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2008-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: display.m.
% Author: pbone.
%
% This module contains a data structure for displaying deep profiler reports.
% It may be versatile enough for other uses.
%
%---------------------------------------------------------------------------%

:- module display.
:- interface.

:- import_module measurement_units.
:- import_module query.

:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- type display
    --->    display(
                display_title       :: maybe(string),
                display_content     :: list(display_item)
            ).

:- type display_item
    --->    display_heading(
                % A string to be displayed as a header.
                string
            )
    ;       display_text(
                % A string to be displayed as ordinary text.
                string
            )
    ;       display_paragraph_break
    ;       display_link(
                deep_link
            )
    ;       display_pseudo_link(
                % A string to be formatted exactly as if it were a link,
                % without it actually being a link. Used for situations
                % when a link would lead back to the same page.
                pseudo_link
            )
    ;       display_list(
                % Class of the list, may be used to display the list.
                list_class,

                % An optional title.
                maybe(string),

                % Items within the list.
                list(display_item)
            )
    ;       display_table(
                table
            )
    ;       display_verbatim(
                % A string to be displayed verbatim. It should be displayed
                % with a fixed width font and line breaks should be honoured.
                string
            )
    ;       display_developer(
                % A display item intended for developers only. These are
                % displayed only when in developer mode.
                display_item
            ).

%---------------------------------------------------------------------------%
%
% Table specific structures.
%

:- type table
    --->    table(
                % Information about how to format the table.
                table_class     :: table_class,

                % The number of columns in the table. You must get this number
                % (a) when you sum up the number of columns in each
                % table_header_group in the table_header (if it exists), and
                % (b) when sum up the column span of each cell in any table
                % row.
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

                % This field controls the mechanism that use to shade every
                % second column in the table, which should make it easier for
                % people to associate table entries with their columns.
                thg_colour      :: table_column_colour
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

:- type table_row
    --->    table_row(
                tr_cells        :: list(table_cell)
            )
    ;       table_separator_row
    ;       table_section_header(
                tsh_text        :: table_data
            )
    ;       table_developer_row(
                tdr_row         :: table_row
            ).

:- type table_cell
    --->    table_cell(
                tc_text         :: table_data
            )
    ;       table_multi_cell(
                tcs_text        :: table_data,
                tcs_span        :: int
            )
    ;       table_empty_cell.

:- type table_class
    --->    table_class_do_not_box
    ;       table_class_box
    ;       table_class_box_if_pref.

:- type table_column_colour
    --->    column_do_not_colour
    ;       column_colour
    ;       column_colour_if_pref.

:- type table_column_class
    --->    table_column_class_allocations
    ;       table_column_class_callseqs
    ;       table_column_class_clique
    ;       table_column_class_field_name
    ;       table_column_class_memory
    ;       table_column_class_module_name
    ;       table_column_class_no_class
    ;       table_column_class_number
    ;       table_column_class_ordinal_rank
    ;       table_column_class_port_counts
    ;       table_column_class_proc
    ;       table_column_class_source_context
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
                num_decimal_places
            )
    ;       td_p(percent)
    ;       td_s(string)
    ;       td_as(attr_string)
    ;       td_t(time).

:- type attr_string
    --->    attr_str(
                list(str_attr),
                string
            ).

:- type str_attr
    --->    attr_bold
    ;       attr_italic
    ;       attr_underline.

%---------------------------------------------------------------------------%
%
% List specific structures.
%

:- type list_class
    --->    list_class_vertical_no_bullets
    ;       list_class_vertical_bullets
    ;       list_class_horizontal
    ;       list_class_horizontal_except_title.

%---------------------------------------------------------------------------%
%
% Link specific structures.
%

:- type deep_link
    --->    deep_link(
                % The link command.
                cmd,

                % The preferences for the link command.
                maybe(preferences),

                % A label for the link.
                attr_string,

                % Class of the link; may control how it is displayed.
                link_class
            ).

:- type pseudo_link
    --->    pseudo_link(
                % A label for the pseudo link.
                string,

                % Class of the link; may control how it is displayed.
                link_class
            ).

:- type link_class
    --->    link_class_link
    ;       link_class_control.

%---------------------------------------------------------------------------%
%
% Predicates for working with display structures.
%

:- func make_single_table_header_group(table_data,
    table_column_class, table_column_colour) = table_header_group.

:- func make_multi_table_header_group(string, list(table_data),
    table_column_class, table_column_colour) = table_header_group.

    % header_groups_to_header(HeaderGroups, NumColumns, Header)
    %
    % Convert the list of header groups into a header, and return
    % the number of columns they represent.
    %
:- pred header_groups_to_header(list(table_header_group)::in,
    int::out, table_header::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.

%---------------------------------------------------------------------------%

make_single_table_header_group(ColumnTitle, ColumnClass, Colour) =
    table_header_group(table_header_group_single(ColumnTitle),
        ColumnClass, Colour).

make_multi_table_header_group(MainTitle, SubTitles, ColumnClass, Colour) =
    table_header_group(table_header_group_multi(MainTitle, SubTitles),
        ColumnClass, Colour).

%---------------------------------------------------------------------------%

    % Given a header for a column group, this predicate adds the number
    % of columns it covers to the accumulator.
    %
:- pred table_accumulate_columns(table_header_group::in, int::in, int::out)
    is det.

table_accumulate_columns(HeaderGroup, !NumColumns) :-
    HeaderGroup = table_header_group(ColumnTitles, _, _),
    (
        ColumnTitles = table_header_group_single(_),
        GroupColumns = 1
    ;
        ColumnTitles = table_header_group_multi(_, SubTitles),
        list.length(SubTitles, GroupColumns)
    ),
    !:NumColumns = !.NumColumns + GroupColumns.

header_groups_to_header(HeaderGroups, NumColumns, Header) :-
    list.foldl(table_accumulate_columns, HeaderGroups, 0, NumColumns),
    Header = table_header(HeaderGroups).

%---------------------------------------------------------------------------%
:- end_module display.
%---------------------------------------------------------------------------%
