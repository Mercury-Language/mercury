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

:- import_module list.
:- import_module maybe.
:- import_module string.

:- import_module measurement_units.
:- import_module query.

%-----------------------------------------------------------------------------%

:- type display
    --->    display(
                title       :: maybe(string),
                content     :: list(display_item)
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

                % The number of columns in the table.  If the number of cells
                % in any data row is not equal to this number, the table is not
                % well formed. The number of cells in the header may be smaller
                % than this value when there are header cells that span
                % multiple sub-header cells.
                table_num_cols  :: int,

                % Header row of table.
                table_header    :: maybe(table_header),

                % The data in table.
                table_rows      :: list(table_row)
            ).

:- type table_header
    --->    table_header(
                th_cells        :: list(table_header_cell)
            ).

:- type table_header_cell
    --->    table_header_cell(
                % The table contents.
                thc_contents    :: table_data,

                % The class may be used by a layout to make decisions
                % about how to paint this column.
                thc_class       :: table_col_class
            )
    ;       table_header_group(
                thg_title       :: string,
                thg_subtitles   :: list(table_data),

                % The class may be used by a layout to make decisions
                % about how to paint this column.
                thg_class       :: table_col_class
            ).

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
    --->    table_class_menu
    ;       table_class_top_procs.

:- type table_col_class
    --->    table_col_class_allocations
    ;       table_col_class_callseqs
    ;       table_col_class_memory
    ;       table_col_class_no_class
    ;       table_col_class_number
    ;       table_col_class_ordinal_rank
    ;       table_col_class_port_counts
    ;       table_col_class_proc
    ;       table_col_class_ticks_and_times.

    % Table data can be specified by type to allow formatting, for example
    % to align decimal points.
    %
:- type table_data
    --->    f(float)
    ;       i(int)
    ;       l(deep_link)
    ;       m(
                % The amount of memory.
                memory,

                % The units to display memory in.
                memory_units,

                % The number of decimal places to show.
                int
            )
    ;       p(percent)
    ;       s(string)
    ;       t(time).

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

    % If given a header this predicate adds it to the head of the list and adds
    % the correct number of columns to the column count.
    %
:- pred table_maybe_add_header_col(maybe(table_header_cell)::in,
    list(table_header_cell)::in, list(table_header_cell)::out,
    int::in, int::out) is det.

    % Given a header this predicate adds it to the head of the list and adds
    % the correct number of columns to the column count.
    %
:- pred table_add_header_col(table_header_cell::in,
    list(table_header_cell)::in, list(table_header_cell)::out,
    int::in, int::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.


table_maybe_add_header_col(no, !Cols, !NumCols).

table_maybe_add_header_col(yes(HeaderCol), !Cols, !NumCols) :-
    table_add_header_col(HeaderCol, !Cols, !NumCols).

table_add_header_col(Cell, !Cols, !NumCols) :-
    (
        Cell = table_header_cell(_, _),
        ColsAddend = 1
    ;
        Cell = table_header_group(_, SubHeaders, _),
        length(SubHeaders, ColsAddend)
    ),
    !:NumCols = !.NumCols + ColsAddend,
    list.cons(Cell, !Cols).

%-----------------------------------------------------------------------------%
:- end_module display.
%-----------------------------------------------------------------------------%
