%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2002, 2004-2008 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: html_format.m.
% Author: zs, pbone.
%
% This module contains code that sets the format of the HTML tables
% we generate for individual queries.

% This module appends many strings. Since string.append takes time that is
% linear over the length of both input strings, building a long string
% from many short strings would take quadratic time. This is why we represent
% HTML as a cord of strings instead. This cord is then converted to a list of
% strings and then a single list just before being given to the browser.
%
%-----------------------------------------------------------------------------%

:- module html_format.
:- interface.

:- import_module measurements.
:- import_module profile.
:- import_module query.
:- import_module top_procs.
:- import_module display.

:- import_module bool.
:- import_module cord.
:- import_module list.
:- import_module unit.

%-----------------------------------------------------------------------------%

:- type html == cord(string).

:- func html_to_string(html) = string.

%-----------------------------------------------------------------------------%

    % Construct a complete HTML page from the given display structure.
    %
    % The first parameter is used to gather extra information from the deep
    % profile, for example the name of the Deep.data file to build the URLs
    % from.
    %
:- func htmlize_display(deep, preferences, display) = html.

%-----------------------------------------------------------------------------%

:- func table_start(preferences) = string.
:- func table_end(preferences) = string.

:- func page_banner(cmd, preferences) = string.
:- func page_footer(cmd, preferences, deep) = string.

:- func toggle_cost_criteria_in_top_procs_cmd(preferences, deep, display_limit,
    cost_kind, include_descendants, measurement_scope) = string.

:- func criteria_to_description(order_criteria) = string.
:- func cost_criteria_to_description(cost_kind, include_descendants,
    measurement_scope) = string.

:- type id_fields
    --->    source_proc
    ;       rank_module
    ;       rank_proc
    ;       proc.

:- type totals_disposition
    --->    totals_meaningful
    ;       totals_not_meaningful.

:- type header_wrap_func == (func(string, order_criteria) = string).

:- func fields_header(preferences, id_fields, totals_disposition,
    header_wrap_func) = string.

:- func header_row(string, preferences, id_fields, totals_disposition)
    = string.
:- func separator_row(preferences, id_fields, totals_disposition)
    = string.

:- type sub_lines(T)
    --->    sub_lines(
                sub_line_type   :: T,
                sub_line_list   :: list(line_group(T, unit))
             ).

:- type one_id ---> one_id.
:- type two_id ---> two_id.

:- type one_id_sub_lines == sub_lines(one_id).
:- type two_id_sub_lines == sub_lines(two_id).

:- type one_id_line == line_group(one_id, unit).
:- type two_id_line == line_group(two_id, unit).
:- type one_id_line_group == line_group(one_id, one_id_sub_lines).
:- type two_id_line_group == line_group(two_id, two_id_sub_lines).
:- type one_two_id_line_group == line_group(one_id, two_id_sub_lines).

    % This function takes a context description (which may be empty) and a
    % HTML string describing all fields in a row but the first, and returns
    % the HTML for the full row.
    %
:- func add_context(string, line_group(one_id, LL)) = line_group(two_id, LL).

    % This adds the context from the line group to the HTML as the first field
    % of the resulting line group.
    %
:- func add_self_context(line_group(one_id, LL)) = line_group(two_id, LL).

:- func add_ranks(list(line_group(one_id, LL))) = list(line_group(two_id, LL)).

:- func line_to_one_id_subline_group(line_group(FL, unit))
    = line_group(FL, one_id_sub_lines).

:- func line_to_two_id_subline_group(line_group(FL, unit))
    = line_group(FL, two_id_sub_lines).

:- func one_id_line_to_html(preferences, deep, totals_disposition,
    one_id_line) = string.
:- func one_id_line_group_to_html(preferences, deep, totals_disposition,
    one_id_line_group) = string.
:- func two_id_line_to_html(preferences, deep, totals_disposition,
    two_id_line) = string.
:- func two_id_line_group_to_html(preferences, deep, totals_disposition,
    two_id_line_group) = string.

:- func own_and_desc_to_html(own_prof_info, inherit_prof_info,
    preferences, deep, totals_disposition) = string.

:- pred lookup_ticks_per_sec(profile_stats::in, int::out, bool::out) is det.

:- func proc_dynamic_name(deep, proc_dynamic_ptr) = string.
:- func proc_static_name(deep, proc_static_ptr) = string.

:- pred proc_dynamic_context(deep::in, proc_dynamic_ptr::in,
    string::out, int::out) is det.
:- pred proc_static_context(deep::in, proc_static_ptr::in,
    string::out, int::out) is det.
:- pred call_site_context(deep::in, call_site_static_ptr::in,
    string::out, int::out) is det.

:- pred proc_static_to_line_group_info(preferences::in, deep::in,
    proc_static_ptr::in, string::out, int::out, string::out, string::out)
    is det.
:- func proc_static_to_html_ref(preferences, deep, proc_static_ptr) = string.
:- func module_name_to_html_ref(preferences, deep, string) = string.
:- func clique_ptr_to_html_ref(preferences, deep, string, clique_ptr) = string.
:- func deep_cmd_pref_to_url(preferences, deep, cmd) = string.

:- func plural(int) = string.

    % Convert any special characters in a string into appropriate HTML
    % escapes.
    %
:- func escape_html_string(string) = string.

    % Like escape_html_string, but additionally inserts zero-width space
    % characters to suggest where long strings can be broken over multiple
    % lines.
    %
:- func escape_break_html_string(string) = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module bool.
:- import_module char.
:- import_module exception.
:- import_module float.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module svmap.

:- import_module measurement_units.

%-----------------------------------------------------------------------------%

html_to_string(HTML) = Str :-
    string.append_list(cord.list(HTML), Str).

%-----------------------------------------------------------------------------%

htmlize_display(Deep, Prefs, Display) = HTML :-
    Display = display(MaybeTitle, Items),
    MainTitle = str_to_html("Mercury Deep Profile for ") ++
        str_to_html(Deep ^ data_file_name),
    (
        MaybeTitle = no,
        HeadTitle = MainTitle,
        HeadingHTML = empty_html
    ;
        MaybeTitle = yes(Title),
        TitleHTML = str_to_html(Title),
        HeadTitle = MainTitle ++ str_to_html(" - ") ++ TitleHTML,
        HeadingHTML = wrap_tags("<h3>", "</h3>\n", TitleHTML)
    ),
    HeadTitleHTML = wrap_tags("<title>", "</title>\n", HeadTitle),

    deep_to_http_context(Deep, HTTPContext),
    StyleControlMap0 = default_style_control_map,
    map_join_html(item_to_html("<div>\n", "</div>\n", HTTPContext),
        StyleControlMap0, StyleControlMap1, Items, ItemsHTML),

    ColourScheme = Prefs ^ pref_colour,
    (
        ColourScheme = colour_column_groups,
        StyleControlMap = StyleControlMap1
    ;
        ColourScheme = colour_none,
        % Ignore the updates in StyleControlMap1. This works as long as
        % all such updates implement the colouring of column groups.
        StyleControlMap = default_style_control_map
    ),

    StyleHTML = css_style_html(StyleControlMap),

    HTML = doc_type_html ++
        wrap_tags("<html>\n", "</html>\n",
            wrap_tags("<head>\n", "</head>\n", HeadTitleHTML ++ StyleHTML) ++
            wrap_tags("<body>\n", "</body>\n", HeadingHTML ++ ItemsHTML)
        ).

:- func doc_type_html = html.

doc_type_html =
    str_to_html(
        "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"\n" ++
        "\"http://www.w3.org/TR/html4/strict.dtd\">\n").

%-----------------------------------------------------------------------------%

:- func css_style_html(style_control_map) = html.

css_style_html(StyleControlMap) = HTML :-
    % XXX This ignores colour_column_groups. We should respect it,
    % and process it either here, or when converting table columns to HTML.

    map.to_assoc_list(StyleControlMap, StyleControls),
    ControlHTMLs = list.map(style_control_to_html, StyleControls),
    ControlsHTML = append_htmls(ControlHTMLs),
    HTML = wrap_tags("<style type=\"text/css\">\n", "</style>\n",
        ControlsHTML).

:- func style_control_to_html(pair(style_control, style_element_map)) = html.

style_control_to_html(Control - StyleElementMap) = HTML :-
    Control = style_control(ControlName),
    StyleElements = map.to_assoc_list(StyleElementMap),
    ElementHTMLs = list.map(style_element_to_html, StyleElements),
    ElementsHTML = append_htmls(ElementHTMLs),
    StartFragment = string.format("\t%s\n\t{\n", [s(ControlName)]),
    EndFragment = "\t}\n",
    HTML = wrap_tags(StartFragment, EndFragment, ElementsHTML).

:- func style_element_to_html(pair(style_element, string)) = html.

style_element_to_html(style_element(ElementName) - Value) =
    str_to_html(string.format("\t\t%s: %s;\n", [s(ElementName), s(Value)])).

%-----------------------------------------------------------------------------%

    % Convert a display item into a HTML snippet.
    %
:- pred item_to_html(string::in, string::in, http_context::in,
    style_control_map::in, style_control_map::out,
    display_item::in, html::out) is det.

item_to_html(StartTag, EndTag, HTTPContext, !StyleControlMap, Item, HTML) :-
    (
        Item = display_message(Message),
        HTML = wrap_tags(StartTag, EndTag,
            wrap_tags("<h3>", "</h3>\n", str_to_html(Message)))
    ;
        Item = display_command_link(DeepLink),
        HTML = wrap_tags(StartTag, EndTag,
            link_to_html(HTTPContext, DeepLink))
    ;
        Item = display_table(Table),
        table_to_html(HTTPContext, !StyleControlMap, Table, TableHTML),
        HTML = wrap_tags(StartTag, EndTag, TableHTML)
    ;
        Item = display_list(Class, MaybeTitle, Items),
        (
            MaybeTitle = yes(Title),
            TitleStartTag = "<span id=\"list_title\">",
            TitleEndTag = "</span>",
            TitleHTML = wrap_tags(TitleStartTag, TitleEndTag,
                str_to_html(Title)),
            (
                Class = list_class_vertical_bullets,
                PostTitleHTML = empty_html
            ;
                ( Class = list_class_horizontal
                ; Class = list_class_vertical_no_bullets
                ),
                PostTitleHTML = str_to_html("<br>")
            )
        ;
            MaybeTitle = no,
            TitleHTML = empty_html,
            PostTitleHTML = empty_html
        ),
        (
            Class = list_class_vertical_bullets,
            OutsideStartTag = "<ul>",
            OutsideEndTag = "</ul>\n",
            InnerStartTag = "<li>",
            InnerEndTag = "</li>\n",
            Separator = empty_html
        ;
            Class = list_class_vertical_no_bullets,
            OutsideStartTag = "",
            OutsideEndTag = "\n",
            InnerStartTag = "",
            InnerEndTag = "\n",
            Separator = str_to_html("<br>\n")
        ;
            Class = list_class_horizontal,
            OutsideStartTag = "",
            OutsideEndTag = "\n",
            InnerStartTag = "",
            InnerEndTag = "\n",
            Separator = str_to_html(" ")
        ),
        sep_map_join_html(Separator,
            item_to_html(InnerStartTag, InnerEndTag, HTTPContext),
            !StyleControlMap, Items, InnerItemsHTML),
        ItemsHTML = wrap_tags(OutsideStartTag, OutsideEndTag, InnerItemsHTML),
        HTML = wrap_tags(StartTag, EndTag,
            TitleHTML ++ PostTitleHTML ++ ItemsHTML)
    ).

%-----------------------------------------------------------------------------%
%
% Table htmlization.
%

    % The number of rows to be used for a table header.
    %
:- type table_header_rows
    --->    one_header_row
    ;       two_header_rows.

    % A mapping of column numbers to classes.
    %
:- type column_class_map == map(int, string).

%-----------------------------------------------------------------------------%

    % Create a HTML table entity from the given table description.
    %
:- pred table_to_html(http_context::in,
    style_control_map::in, style_control_map::out,
    table::in, html::out) is det.

table_to_html(HTTPContext, !StyleControlMap, Table, HTML) :-
    Table = table(Class, NumColumns, MaybeHeader, BodyRows),

    ClassStr = table_class_to_string(Class),
    TableStartTag = "<table class=\"" ++ ClassStr ++ "\">\n",
    TableEndTag = "</table>\n",

    % Build a header row.
    (
        MaybeHeader = yes(table_header(THCells)),
        list.foldl5(table_header_num_rows_and_classmap, THCells,
            one_header_row, THNumRows, 0, _, map.init, ClassMap,
            0, _, !StyleControlMap),
        MaybeClassMap = yes(ClassMap),
        map_join_html(table_header_group_to_html_row_1(HTTPContext, THNumRows),
            !StyleControlMap, THCells, InnerHeaderRowOneHTML),
        HeaderRowOneHTML =
            wrap_tags("<tr>", "</tr>\n", InnerHeaderRowOneHTML),
        (
            THNumRows = one_header_row,
            HeaderRowTwoHTML = empty_html
        ;
            THNumRows = two_header_rows,
            map_join_html(table_header_group_to_html_row_2(HTTPContext),
                !StyleControlMap, THCells, InnerHeaderRowTwoHTML),
            HeaderRowTwoHTML =
                wrap_tags("<tr>", "</tr>\n", InnerHeaderRowTwoHTML)
        ),
        InnerHeaderRowThree =
            string.format("<td colspan=\"%d\"/>", [i(NumColumns)]),
        HeaderRowThreeHTML =
            wrap_tags("<tr>", "</tr>\n",  str_to_html(InnerHeaderRowThree)),
        HeaderHTML = HeaderRowOneHTML ++ HeaderRowTwoHTML ++ HeaderRowThreeHTML
    ;
        MaybeHeader = no,
        MaybeClassMap = no,
        HeaderHTML = empty_html
    ),

    % Build the table rows.
    map_join_html(table_row_to_html(HTTPContext, MaybeClassMap, NumColumns),
        !StyleControlMap, BodyRows, BodyRowsHTML),

    % Construct the table.
    HTML = wrap_tags(TableStartTag, TableEndTag, HeaderHTML ++ BodyRowsHTML).

%-----------------------------------------------------------------------------%

    % Return the HTML entity for a table header cell.
    %
:- pred table_header_group_to_html_row_1(http_context::in,
    table_header_rows::in, style_control_map::in, style_control_map::out,
    table_header_group::in, html::out) is det.

table_header_group_to_html_row_1(HTTPContext, HeaderNumRows, !StyleControlMap,
        HeaderGroup, HTML) :-
    HeaderGroup = table_header_group(Titles, ColumnClass, _SetStyle),
    (
        Titles = table_header_group_single(Title),
        (
            HeaderNumRows = one_header_row,
            RowSpan = "1"
        ;
            HeaderNumRows = two_header_rows,
            RowSpan = "2"
        ),
        ColumnSpan = "1",
        ContentsHTML = table_data_to_html(HTTPContext, Title)
    ;
        Titles = table_header_group_multi(MainTitle, SubTitleCells),
        RowSpan = "1",
        list.length(SubTitleCells, NumSubTitleCells),
        ColumnSpan = string.int_to_string(NumSubTitleCells),
        ContentsHTML = str_to_html(MainTitle)
    ),

    ColumnClassStr = table_column_class_to_string(ColumnClass),
    StartTag = string.format(
        "<th rowspan=\"%s\" colspan=\"%s\" class=\"%s\">",
        [s(RowSpan), s(ColumnSpan), s(ColumnClassStr)]),
    EndTag = "</th>\n",
    HTML = wrap_tags(StartTag, EndTag, ContentsHTML).

%-----------------------------------------------------------------------------%

:- pred table_header_group_to_html_row_2(http_context::in,
    style_control_map::in, style_control_map::out,
    table_header_group::in, html::out) is det.

table_header_group_to_html_row_2(HTTPContext, !StyleControlMap,
        HeaderGroup, HTML) :-
    HeaderGroup = table_header_group(Titles, ColumnClass, _SetStyle),
    (
        Titles = table_header_group_single(_),
        HTML = empty_html
    ;
        Titles = table_header_group_multi(_, SubTitleCells),
        map_join_html(table_data_to_th_html(HTTPContext, ColumnClass),
            !StyleControlMap, SubTitleCells, HTML)
    ).

%-----------------------------------------------------------------------------%

:- pred table_data_to_th_html(http_context::in, table_column_class::in,
    style_control_map::in, style_control_map::out,
    table_data::in, html::out) is det.

table_data_to_th_html(HTTPContext, ColumnClass, !StyleControlMap,
        TableData, HTML) :-
    ColumnClassStr = table_column_class_to_string(ColumnClass),
    TableDataHTML = table_data_to_html(HTTPContext, TableData),
    StartTag = string.format("<th class=\"%s\">", [s(ColumnClassStr)]),
    EndTag = "</th>\n",
    HTML = wrap_tags(StartTag, EndTag, TableDataHTML).

%-----------------------------------------------------------------------------%

    % Determine how many rows the table header requires, and set up a map
    % from column numbers to classes. Update the style control map for
    % table header groups that specify table_set_style.
    %
    % This should be used with list.foldl5.
    %
:- pred table_header_num_rows_and_classmap(table_header_group::in,
    table_header_rows::in, table_header_rows::out,
    int::in, int::out, column_class_map::in, column_class_map::out,
    int::in, int::out, style_control_map::in, style_control_map::out) is det.

table_header_num_rows_and_classmap(HeaderGroup, !NumRows, !ColumnNumber,
        !ClassMap, !HeaderGroupNumber, !StyleControlMap) :-
    HeaderGroup = table_header_group(ColumnTitles, ColumnClass, SetStyle),
    ColumnClassStr = table_column_class_to_string(ColumnClass),
    (
        ColumnTitles = table_header_group_single(_),
        NumSubCols = 1,
        svmap.det_insert(!.ColumnNumber, ColumnClassStr, !ClassMap)
    ;
        ColumnTitles = table_header_group_multi(_, SubTitles),
        list.length(SubTitles, NumSubCols),
        !:NumRows = two_header_rows,
        % fold_up is inclusive of the higher number.
        int.fold_up(insert_column_into_classmap(ColumnClassStr),
            !.ColumnNumber, !.ColumnNumber + NumSubCols - 1, !ClassMap)
    ),
    (
        SetStyle = table_do_not_set_style
    ;
        SetStyle = table_set_style,
        update_style_control_map(ColumnClass, !.HeaderGroupNumber,
            !StyleControlMap),
        !:HeaderGroupNumber = !.HeaderGroupNumber + 1
    ),
    !:ColumnNumber = !.ColumnNumber + NumSubCols.

:- pred insert_column_into_classmap(string::in, int::in,
    column_class_map::in, column_class_map::out) is det.

insert_column_into_classmap(Value, Key, !Map) :-
    svmap.det_insert(Key, Value, !Map).

:- pred update_style_control_map(table_column_class::in, int::in,
    style_control_map::in, style_control_map::out) is det.

update_style_control_map(ColumnClass, HeaderGroupNumber, !StyleControlMap) :-
    ColumnClassStr = table_column_class_to_string(ColumnClass),
    StyleControl = style_control("td." ++ ColumnClassStr),
    StyleElement = style_element("background"),
    ( HeaderGroupNumber /\ 1 = 0 ->
        Colour = "LightGrey"
    ;
        Colour = "White"
    ),
    ( map.search(!.StyleControlMap, StyleControl, StyleElementMap0) ->
        map.set(StyleElementMap0, StyleElement, Colour, StyleElementMap),
        svmap.det_update(StyleControl, StyleElementMap, !StyleControlMap)
    ;
        map.det_insert(map.init, StyleElement, Colour, StyleElementMap),
        svmap.det_insert(StyleControl, StyleElementMap, !StyleControlMap)
    ).

%-----------------------------------------------------------------------------%

    % Build a row of a HTML table from the table_row type.
    %
:- pred table_row_to_html(http_context::in, maybe(column_class_map)::in,
    int::in, style_control_map::in, style_control_map::out,
    table_row::in, html::out) is det.

table_row_to_html(HTTPContext, MaybeColClassMap, NumColumns, !StyleControlMap,
        TableRow, HTML) :-
    (
        TableRow = table_section_header(Contents),
        ContentsHTML = table_data_to_html(HTTPContext, Contents),
        StartTag = string.format("<tr><td colspan=\"%d\">", [i(NumColumns)]),
        EndTag = "</td></tr>\n",
        HTML = wrap_tags(StartTag, EndTag, ContentsHTML)
    ;
        TableRow = table_row(Cells),
        map_join_html_count(table_cell_to_html(HTTPContext, MaybeColClassMap),
            !StyleControlMap, 0, Cells, InnerHTML),
        HTML = wrap_tags("<tr>", "</tr>\n", InnerHTML)
    ).

%-----------------------------------------------------------------------------%

:- pred table_cell_to_html(http_context::in, maybe(column_class_map)::in,
    style_control_map::in, style_control_map::out,
    int::in, table_cell::in, html::out) is det.

table_cell_to_html(HTTPContext, MaybeClassMap, !StyleControlMap, ColumnNum,
        Cell, HTML) :-
    (
        Cell = table_empty_cell,
        HTML = str_to_html("<td/>")
    ;
        Cell = table_cell(CellData),
        (
            MaybeClassMap = yes(ClassMap),
            ( map.search(ClassMap, ColumnNum, ColumnClassStrPrime) ->
                ColumnClassStr = ColumnClassStrPrime
            ;
                Msg = string.format(
                    "Class map had no class for col %d, check table structure",
                    [i(ColumnNum)]),
                error(Msg)
            )
        ;
            MaybeClassMap = no,
            ( table_data_class(CellData, ColumnClassPrime) ->
                ColumnClass = ColumnClassPrime
            ;
                ColumnClass = default_table_column_class
            ),
            ColumnClassStr = table_column_class_to_string(ColumnClass)
        ),
        CellHTML = table_data_to_html(HTTPContext, CellData),
        StartTag = string.format("<td class=\"%s\">", [s(ColumnClassStr)]),
        EndTag = "</td>\n",
        HTML = wrap_tags(StartTag, EndTag, CellHTML)
    ).

%-----------------------------------------------------------------------------%

:- func table_data_to_html(http_context, table_data) = html.

table_data_to_html(_, td_f(Float)) =
    str_to_html(two_decimal_fraction(Float)).
table_data_to_html(_, td_i(Int)) =
    str_to_html(commas(Int)).
table_data_to_html(HTTPContext, td_l(Link)) =
    link_to_html(HTTPContext, Link).
table_data_to_html(_, td_m(Mem, Units, Decimals)) =
    str_to_html(format_memory(Mem, Units, Decimals)).
table_data_to_html(_, td_p(Percent)) =
    str_to_html(format_percent(Percent)).
table_data_to_html(_, td_s(String)) =
    str_to_html(escape_break_html_string(String)).
table_data_to_html(_, td_t(Time)) =
    str_to_html(format_time(Time)).

    % This predicate is used when a table class map couldn't be built from the
    % header of the table (perhaps there was no header). It it provides a
    % class for some data that class is used, otherwise the default class is
    % assumed.
    %
:- pred table_data_class(table_data::in, table_column_class::out) is semidet.

table_data_class(td_f(_), table_column_class_number).
table_data_class(td_i(_), table_column_class_number).
table_data_class(td_m(_, _, _), table_column_class_number).
table_data_class(td_p(_), table_column_class_number).
table_data_class(td_t(_), table_column_class_number).

:- func default_table_column_class = table_column_class.

default_table_column_class = table_column_class_no_class.

:- func table_column_class_to_string(table_column_class) = string.

table_column_class_to_string(table_column_class_no_class) = "default".
table_column_class_to_string(table_column_class_allocations) = "allocations".
table_column_class_to_string(table_column_class_callseqs) = "callseqs".
table_column_class_to_string(table_column_class_memory) = "memory".
table_column_class_to_string(table_column_class_number) = "number".
table_column_class_to_string(table_column_class_ordinal_rank) = "ordinal_rank".
table_column_class_to_string(table_column_class_port_counts) = "port_counts".
table_column_class_to_string(table_column_class_proc) = "proc".
table_column_class_to_string(table_column_class_ticks_and_times) =
    "ticks_and_times".

:- func table_class_to_string(table_class) = string.

table_class_to_string(table_class_plain) = "plain".
table_class_to_string(table_class_boxed) = "boxed".

%-----------------------------------------------------------------------------%

    % A style element is a variable you can set for a given control.
    % Examples include "text-align" and "background".
    %
:- type style_element
    --->    style_element(string).

    % Maps a style element to its value.
    %
:- type style_element_map == map(style_element, string).

    % A style control is a category whose properties can be set independently.
    % Examples include "td.allocations and "td.callseqs".
    %
:- type style_control
    --->    style_control(string).

    % Maps a style control to the style elements we should use for it.
    %
:- type style_control_map == map(style_control, style_element_map).

    % Return the default style control map.
    %
:- func default_style_control_map = style_control_map.

default_style_control_map =
    map.from_assoc_list([
        ( style_control("td.allocations") -
            map.from_assoc_list([
                style_element("text-align")     - "right"
            ])
        ),
        ( style_control("td.callseqs") -
            map.from_assoc_list([
                style_element("text-align")     - "right"
            ])
        ),
        ( style_control("td.memory") -
            map.from_assoc_list([
                style_element("text-align")     - "right"
            ])
        ),
        ( style_control("td.number") -
            map.from_assoc_list([
                style_element("text-align")     - "right"
            ])
        ),
        ( style_control("td.ordinal_rank") -
            map.from_assoc_list([
                style_element("text-align")     - "right"
            ])
        ),
        ( style_control("td.port_counts") -
            map.from_assoc_list([
                style_element("text-align")     - "right"
            ])
        ),
        ( style_control("td.proc") -
            map.from_assoc_list([
                style_element("text-align")     - "left"
            ])
        ),
        ( style_control("td.ticks_and_times") -
            map.from_assoc_list([
                style_element("text-align")     - "right"
            ])
        ),
        ( style_control("a.control") -
            map.from_assoc_list([
                style_element("margin")         - "5px",
                style_element("text-decoration") - "none"
            ])
        ),
        ( style_control("table.plain") -
            map.from_assoc_list([
                style_element("border-style")   - "none"
            ])
        ),
        ( style_control("table.boxed") -
            map.from_assoc_list([
                style_element("border-width")   - "1px 1px 1px 1px",
                style_element("border-spacing") - "2px",
                style_element("border-style")   - "outset outset outset outset"
            ])
        ),
        ( style_control("table.boxed th") -
            map.from_assoc_list([
                style_element("border-width")   - "1px 1px 1px 1px",
                style_element("padding")        - "3px 3px 3px 3px",
                style_element("border-style")   - "inset inset inset inset"
            ])
        ),
        ( style_control("table.boxed td") -
            map.from_assoc_list([
                style_element("border-width")   - "1px 1px 1px 1px",
                style_element("padding")        - "3px 3px 3px 3px",
                style_element("border-style")   - "inset inset inset inset"
            ])
        )
    ]).

%-----------------------------------------------------------------------------%

    % Information about the HTTP session. This is used to create HTTP links
    % below.
    %
:- type http_context
    --->    http_context(
                server_name_port    :: string,
                script_name         :: string,
                deep_file           :: string
            ).

%-----------------------------------------------------------------------------%

    % Transform a deep link into HTML.
    %
:- func link_to_html(http_context, deep_link) = html.

link_to_html(HTTPContext, Link) = HTML :-
    Link = deep_link(Cmd, MaybePrefs, Label, Class),
    deep_cmd_to_url(HTTPContext, Cmd, MaybePrefs, URL),
    (
        Class = link_class_control,
        FormatString = "<a class=\"control\" href=\"%s\">[%s]</a>"
    ;
        Class = link_class_link,
        FormatString = "<a class=\"link\" href=\"%s\">%s</a>"
    ),
    string.format(FormatString,
        [s(URL), s(escape_break_html_string(Label))], HTMLStr),
    HTML = str_to_html(HTMLStr).

%-----------------------------------------------------------------------------%

:- pred deep_to_http_context(deep::in, http_context::out) is det.

deep_to_http_context(Deep, HTTPContext) :-
    HTTPContext ^ server_name_port = Deep ^ server_name_port,
    HTTPContext ^ script_name = Deep ^ script_name,
    HTTPContext ^ deep_file = Deep ^ data_file_name.

%-----------------------------------------------------------------------------%

    % Return a URL for the deep structure and command.
    %
:- pred deep_cmd_to_url(http_context::in, cmd::in, maybe(preferences)::in,
    string::out) is det.

deep_cmd_to_url(HTTPContext, Cmd, MaybePrefs, URL) :-
    HostAndPort = HTTPContext ^ server_name_port,
    Script = HTTPContext ^ script_name,
    DataFile = HTTPContext ^ deep_file,
    CmdStr = cmd_to_string(Cmd),
    (
        MaybePrefs = no,
        string.format("http://%s%s?%s&%s",
            [s(HostAndPort), s(Script), s(CmdStr), s(DataFile)], URL)
    ;
        MaybePrefs = yes(Prefs),
        PrefStr = preferences_to_string(Prefs),
        string.format("http://%s%s?%s&%s&%s",
            [s(HostAndPort), s(Script), s(CmdStr), s(PrefStr), s(DataFile)],
            URL)
    ).

%-----------------------------------------------------------------------------%
%
% Generic HTML helper predicates.
%

:- func append_htmls(list(html)) = html.

append_htmls(HTMLs) = cord_list_to_cord(HTMLs).

:- func wrap_tags(string, string, html) = html.

wrap_tags(StartTag, EndTag, InnerHTML) =
    str_to_html(StartTag) ++ InnerHTML ++ str_to_html(EndTag).

:- func empty_html = html.

empty_html = cord.empty.

:- func nl_html = html.

nl_html = str_to_html("\n").

:- func str_to_html(string) = html.

str_to_html(Str) = cord.singleton(Str).

%-----------------------------------------------------------------------------%

    % For each A, MapPred(!StyleControlMap, A, S), and concatenate all Ss.
    %
:- pred map_join_html(
    pred(style_control_map, style_control_map, A, html)::
        in(pred(in, out, in, out) is det),
    style_control_map::in, style_control_map::out,
    list(A)::in, html::out) is det.

map_join_html(MapPred, !StyleControlMap, List, HTML) :-
    sep_map_join_html(empty_html, MapPred, !StyleControlMap, List, HTML).

    % For each A, MapPred(!StyleControlMap, A, S), and concatenate all Ss
    % after putting Separator between them.
    %
:- pred sep_map_join_html(html::in,
    pred(style_control_map, style_control_map, A, html)::
        in(pred(in, out, in, out) is det),
    style_control_map::in, style_control_map::out,
    list(A)::in, html::out) is det.

sep_map_join_html(_, _, !StyleControlMap, [], empty_html).
sep_map_join_html(Separator, MapPred, !StyleControlMap, [Head | Tail], HTML) :-
    MapPred(!StyleControlMap, Head, HeadHTML),
    sep_map_join_html_acc(Separator, MapPred, !StyleControlMap, Tail,
        HeadHTML, HTML).

:- pred sep_map_join_html_acc(html::in,
    pred(style_control_map, style_control_map, A, html)::
        in(pred(in, out, in, out) is det),
    style_control_map::in, style_control_map::out,
     list(A)::in, html::in, html::out) is det.

sep_map_join_html_acc(_, _, !StyleControlMap, [], !HTML).
sep_map_join_html_acc(Separator, MapPred, !StyleControlMap, [Head | Tail],
        !HTML) :-
    MapPred(!StyleControlMap, Head, HeadHTML),
    !:HTML = !.HTML ++ Separator ++ HeadHTML,
    sep_map_join_html_acc(Separator, MapPred, !StyleControlMap, Tail, !HTML).

    % For each A, MapPred(!StyleControlMap, N, A, S), and concatenate all Ss.
    % N is the ordinal number of the element in the list.
    %
:- pred map_join_html_count(
    pred(style_control_map, style_control_map, int, A, html)::
        in(pred(in, out, in, in, out) is det),
    style_control_map::in, style_control_map::out,
    int::in, list(A)::in, html::out) is det.

map_join_html_count(MapPred, !StyleControlMap, N, List, HTML) :-
    sep_map_join_html_count(empty_html, MapPred, !StyleControlMap, N, List,
        HTML).

    % For each A, MapPred(!StyleControlMap, N, A, S), and concatenate all Ss
    % after putting Separator between them.
    % N is the ordinal number of the element in the list.
    %
:- pred sep_map_join_html_count(html::in,
    pred(style_control_map, style_control_map, int, A, html)::
        in(pred(in, out, in, in, out) is det),
    style_control_map::in, style_control_map::out,
    int::in, list(A)::in, html::out) is det.

sep_map_join_html_count(_, _, !StyleControlMap, _, [], empty_html).
sep_map_join_html_count(Separator, MapPred, !StyleControlMap, N,
        [Head | Tail], HTML) :-
    MapPred(!StyleControlMap, N, Head, HeadHTML),
    sep_map_join_html_count_acc(Separator, MapPred, !StyleControlMap, N + 1,
        Tail, HeadHTML, HTML).

:- pred sep_map_join_html_count_acc(html::in,
    pred(style_control_map, style_control_map, int, A, html)::
        in(pred(in, out, in, in, out) is det),
    style_control_map::in, style_control_map::out,
    int::in, list(A)::in, html::in, html::out) is det.

sep_map_join_html_count_acc(_, _, !StyleControlMap, _, [], !HTML).
sep_map_join_html_count_acc(Separator, MapPred, !StyleControlMap, N,
        [Head | Tail], !HTML) :-
    MapPred(!StyleControlMap, N, Head, HeadHTML),
    !:HTML = !.HTML ++ Separator ++ HeadHTML,
    sep_map_join_html_count_acc(Separator, MapPred, !StyleControlMap, N + 1,
        Tail, !HTML).

%-----------------------------------------------------------------------------%
%
% Deprecated html_format code.
%

page_banner(_Cmd, Pref) =
    "<!DOCTYPE HTML PUBLIC ""-//W3C//DTD HTML 4.01//EN""\n" ++
    """http://www.w3.org/TR/html4/strict.dtd"">\n" ++
    "<HTML>\n" ++
    "<HEAD>\n" ++
    "<TITLE>Page created by the Mercury Deep Profiler.</TITLE>\n" ++
    banner_style(Pref) ++
    "</HEAD>\n" ++
    "<BODY>\n".

:- func banner_style(preferences) = string.

banner_style(Pref) = HTML :-
    Fields = Pref ^ pref_fields,

    some [!GroupNum] (
        !:GroupNum = 0,
        IdStyle = string.format("  TD.id       { %s }\n",
            [s(select_colgroup_background(Pref, !.GroupNum))]),
        !:GroupNum = !.GroupNum + 1,
        Fields = fields(PortFields, TimeFields, CallSeqsFields,
            AllocFields, MemoryFields),
        (
            PortFields = no_port,
            PortStyle = ""
        ;
            PortFields = port,
            PortStyle = string.format("  TD.port     { %s }\n",
                [s(select_colgroup_background(Pref, !.GroupNum))]),
            !:GroupNum = !.GroupNum + 1
        ),
        (
            TimeFields = no_time,
            TimeStyle = ""
        ;
            ( TimeFields = ticks
            ; TimeFields = time
            ; TimeFields = ticks_and_time
            ; TimeFields = time_and_percall
            ; TimeFields = ticks_and_time_and_percall
            ),
            TimeStyle = string.format("  TD.time     { %s }\n",
                [s(select_colgroup_background(Pref, !.GroupNum))]),
            !:GroupNum = !.GroupNum + 1
        ),
        (
            CallSeqsFields = no_callseqs,
            CallSeqsStyle = ""
        ;
            ( CallSeqsFields = callseqs
            ; CallSeqsFields = callseqs_and_percall
            ),
            CallSeqsStyle = string.format("  TD.callseqs { %s }\n",
                [s(select_colgroup_background(Pref, !.GroupNum))]),
            !:GroupNum = !.GroupNum + 1
        ),
        (
            AllocFields = no_alloc,
            AllocStyle = ""
        ;
            ( AllocFields = alloc
            ; AllocFields = alloc_and_percall
            ),
            AllocStyle = string.format("  TD.alloc    { %s }\n",
                [s(select_colgroup_background(Pref, !.GroupNum))]),
            !:GroupNum = !.GroupNum + 1
        ),
        (
            MemoryFields = no_memory,
            MemoryStyle = ""
        ;
            ( MemoryFields = memory(_)
            ; MemoryFields = memory_and_percall(_)
            ),
            MemoryStyle = string.format("  TD.memory   { %s }\n",
                [s(select_colgroup_background(Pref, !.GroupNum))])
        )
    ),
    ButtonStyle = "  A.button { margin: 5px; text-decoration: none; }\n",
    HTML =
        "<STYLE TYPE=""text/css"">\n" ++
        IdStyle ++
        PortStyle ++
        TimeStyle ++
        CallSeqsStyle ++
        AllocStyle ++
        MemoryStyle ++
        ButtonStyle ++
        "</STYLE>\n".

:- func select_colgroup_background(preferences, int) = string.

select_colgroup_background(Pref, N) = HTML :-
    (
        Pref ^ pref_colour = colour_column_groups,
        ( N /\ 1 = 0 ->
            Background = even_background
        ;
            Background = odd_background
        ),
        string.format("background: %s", [s(Background)], HTML)
    ;
        Pref ^ pref_colour = colour_none,
        HTML = ""
    ).

:- func even_background = string.

even_background = "rgb(255, 255, 240)".

:- func odd_background = string.

odd_background =  "rgb(240, 240, 255)".

%-----------------------------------------------------------------------------%

table_start(Pref) = HTML :-
    (
        Pref ^ pref_box = nobox,
        HTML = "\n<TABLE width=100%>\n"
    ;
        Pref ^ pref_box = box,
        HTML = "\n<TABLE width=100% border=1>\n"
    ).

table_end(_) = "</TABLE>\n".

%-----------------------------------------------------------------------------%

page_footer(Cmd, Pref, Deep) =
    "<p>\n" ++
    footer_pref_toggles(Cmd, Pref, Deep) ++
    "<br>\n" ++
    string.format("<A CLASS=""button"" HREF=""%s"">[Menu]</A>\n",
        [s(deep_cmd_pref_to_url(Pref, Deep, deep_cmd_menu))]) ++
    string.format("<A CLASS=""button"" HREF=""%s"">[Restart]</A>\n",
        [s(deep_cmd_pref_to_url(Pref, Deep, deep_cmd_restart))]) ++
    string.format("<A CLASS=""button"" HREF=""%s"">[Quit]</A>\n",
        [s(deep_cmd_pref_to_url(Pref, Deep, deep_cmd_quit))]) ++
    "</BODY>\n" ++
    "</HTML>\n".

:- func footer_pref_toggles(cmd, preferences, deep) = string.

footer_pref_toggles(Cmd, Pref, Deep) = AllToggles :-
    RelevantToggles = command_relevant_toggles(Cmd),
    ( list.member(toggle_fields, RelevantToggles) ->
        FieldToggle = footer_field_toggle(Cmd, Pref, Deep)
    ;
        FieldToggle = ""
    ),
    ( list.member(toggle_ancestor_limit, RelevantToggles) ->
        AncestorToggle =
            footer_ancestor_toggle(Cmd, Pref, Deep) ++
            "<br>\n"
    ;
        AncestorToggle = ""
    ),
    ( list.member(toggle_order_criteria, RelevantToggles) ->
        OrderToggle =
            footer_order_criteria_toggle(Cmd, Pref, Deep) ++
            "<br>\n"
    ;
        OrderToggle = ""
    ),
    ( list.member(toggle_summarize, RelevantToggles) ->
        SummarizeToggle = footer_summarize_toggle(Cmd, Pref, Deep)
    ;
        SummarizeToggle = ""
    ),
    ( list.member(toggle_contour, RelevantToggles) ->
        ContourToggle = footer_contour_toggle(Cmd, Pref, Deep)
    ;
        ContourToggle = ""
    ),
    ( list.member(toggle_time_format, RelevantToggles) ->
        TimeFormatToggle = footer_time_format_toggle(Cmd, Pref, Deep)
    ;
        TimeFormatToggle = ""
    ),
    ( list.member(toggle_colour, RelevantToggles) ->
        ColourToggle = footer_colour_toggle(Cmd, Pref, Deep)
    ;
        ColourToggle = ""
    ),
    ( list.member(toggle_box, RelevantToggles) ->
        BoxToggle = footer_box_toggle(Cmd, Pref, Deep)
    ;
        BoxToggle = ""
    ),
    ( list.member(toggle_inactive_modules, RelevantToggles) ->
        InactiveModuleToggle = footer_inactive_modules_toggle(Cmd, Pref, Deep)
    ;
        InactiveModuleToggle = ""
    ),
    ( list.member(toggle_inactive_procs, RelevantToggles) ->
        InactiveProcsToggle = footer_inactive_procs_toggle(Cmd, Pref, Deep)
    ;
        InactiveProcsToggle = ""
    ),
    AllToggles =
        FieldToggle ++
        AncestorToggle ++
        OrderToggle ++
        SummarizeToggle ++
        ContourToggle ++
        TimeFormatToggle ++
        ColourToggle ++
        BoxToggle ++
        InactiveModuleToggle ++
        InactiveProcsToggle.

%-----------------------------------------------------------------------------%

:- type toggle_kind
    --->    toggle_fields
    ;       toggle_box
    ;       toggle_colour
    ;       toggle_ancestor_limit
    ;       toggle_summarize
    ;       toggle_order_criteria
    ;       toggle_contour
    ;       toggle_time_format
    ;       toggle_inactive_modules
    ;       toggle_inactive_procs.

:- func command_relevant_toggles(cmd) = list(toggle_kind).

command_relevant_toggles(deep_cmd_quit) = [].
command_relevant_toggles(deep_cmd_restart) = [].
command_relevant_toggles(deep_cmd_timeout(_)) = [].
command_relevant_toggles(deep_cmd_menu) = [].
command_relevant_toggles(deep_cmd_root(_)) =
    % The clique num doesn't matter.
    command_relevant_toggles(deep_cmd_clique(1)).
command_relevant_toggles(deep_cmd_clique(_)) =
    [toggle_fields, toggle_box, toggle_colour, toggle_ancestor_limit,
    toggle_summarize, toggle_order_criteria, toggle_time_format].
command_relevant_toggles(deep_cmd_proc(_)) =
    [toggle_fields, toggle_box, toggle_colour, toggle_summarize,
    toggle_order_criteria, toggle_time_format].
command_relevant_toggles(deep_cmd_proc_callers(_, _, _)) =
    [toggle_fields, toggle_box, toggle_colour, toggle_order_criteria,
    toggle_contour, toggle_time_format].
command_relevant_toggles(deep_cmd_modules) =
    [toggle_fields, toggle_box, toggle_colour, toggle_order_criteria,
    toggle_time_format, toggle_inactive_modules].
command_relevant_toggles(deep_cmd_module(_)) =
    [toggle_fields, toggle_box, toggle_colour, toggle_order_criteria,
    toggle_time_format, toggle_inactive_procs].
command_relevant_toggles(deep_cmd_top_procs(_, _, _, _)) =
    [toggle_fields, toggle_box, toggle_colour, toggle_time_format].
command_relevant_toggles(deep_cmd_proc_static(_)) = [].
command_relevant_toggles(deep_cmd_proc_dynamic(_)) = [].
command_relevant_toggles(deep_cmd_call_site_static(_)) = [].
command_relevant_toggles(deep_cmd_call_site_dynamic(_)) = [].
command_relevant_toggles(deep_cmd_raw_clique(_)) = [].

:- func footer_field_toggle(cmd, preferences, deep) = string.

footer_field_toggle(Cmd, Pref, Deep) = HTML :-
    Fields = Pref ^ pref_fields,
    ( Fields ^ port_fields = no_port ->
        Port1Toggle = ""
    ;
        Port1Fields = Fields ^ port_fields := no_port,
        Port1Pref = Pref ^ pref_fields := Port1Fields,
        Port1Msg = "[No port counts]",
        Port1Toggle = string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
            [s(deep_cmd_pref_to_url(Port1Pref, Deep, Cmd)), s(Port1Msg)])
    ),
    ( Fields ^ port_fields = port ->
        Port2Toggle = ""
    ;
        Port2Fields = Fields ^ port_fields := port,
        Port2Pref = Pref ^ pref_fields := Port2Fields,
        Port2Msg = "[Port counts]",
        Port2Toggle = string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
            [s(deep_cmd_pref_to_url(Port2Pref, Deep, Cmd)), s(Port2Msg)])
    ),
    ( Fields ^ time_fields = no_time ->
        Time1Toggle = ""
    ;
        Time1Fields = Fields ^ time_fields := no_time,
        Time1Pref = Pref ^ pref_fields := Time1Fields,
        Time1Msg = "[No time info]",
        Time1Toggle = string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
            [s(deep_cmd_pref_to_url(Time1Pref, Deep, Cmd)), s(Time1Msg)])
    ),
    ( Fields ^ time_fields = ticks ->
        Time2Toggle = ""
    ;
        Time2Fields = Fields ^ time_fields := ticks,
        Time2Pref = Pref ^ pref_fields := Time2Fields,
        Time2Msg = "[Ticks]",
        Time2Toggle = string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
            [s(deep_cmd_pref_to_url(Time2Pref, Deep, Cmd)), s(Time2Msg)])
    ),
    ( Fields ^ time_fields = time ->
        Time3Toggle = ""
    ;
        Time3Fields = Fields ^ time_fields := time,
        Time3Pref = Pref ^ pref_fields := Time3Fields,
        Time3Msg = "[Times]",
        Time3Toggle = string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
            [s(deep_cmd_pref_to_url(Time3Pref, Deep, Cmd)), s(Time3Msg)])
    ),
    ( Fields ^ time_fields = ticks_and_time->
        Time4Toggle = ""
    ;
        Time4Fields = Fields ^ time_fields := ticks_and_time,
        Time4Pref = Pref ^ pref_fields := Time4Fields,
        Time4Msg = "[Ticks and times]",
        Time4Toggle = string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
            [s(deep_cmd_pref_to_url(Time4Pref, Deep, Cmd)), s(Time4Msg)])
    ),
    ( Fields ^ time_fields = time_and_percall ->
        Time5Toggle = ""
    ;
        Time5Fields = Fields ^ time_fields := time_and_percall,
        Time5Pref = Pref ^ pref_fields := Time5Fields,
        Time5Msg = "[Times and per-call times]",
        Time5Toggle = string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
            [s(deep_cmd_pref_to_url(Time5Pref, Deep, Cmd)), s(Time5Msg)])
    ),
    ( Fields ^ time_fields = ticks_and_time_and_percall ->
        Time6Toggle = ""
    ;
        Time6Fields = Fields ^ time_fields := ticks_and_time_and_percall,
        Time6Pref = Pref ^ pref_fields := Time6Fields,
        Time6Msg = "[Ticks and times and per-call times]",
        Time6Toggle = string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
            [s(deep_cmd_pref_to_url(Time6Pref, Deep, Cmd)), s(Time6Msg)])
    ),
    ( Fields ^ callseqs_fields = no_callseqs ->
        CallSeqs1Toggle = ""
    ;
        CallSeqs1Fields = Fields ^ callseqs_fields := no_callseqs,
        CallSeqs1Pref = Pref ^ pref_fields := CallSeqs1Fields,
        CallSeqs1Msg = "[No call sequence number info]",
        CallSeqs1Toggle =
            string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
                [s(deep_cmd_pref_to_url(CallSeqs1Pref, Deep, Cmd)),
                s(CallSeqs1Msg)])
    ),
    ( Fields ^ callseqs_fields = callseqs ->
        CallSeqs2Toggle = ""
    ;
        CallSeqs2Fields = Fields ^ callseqs_fields := callseqs,
        CallSeqs2Pref = Pref ^ pref_fields := CallSeqs2Fields,
        CallSeqs2Msg = "[Call sequence numbers]",
        CallSeqs2Toggle =
            string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
                [s(deep_cmd_pref_to_url(CallSeqs2Pref, Deep, Cmd)),
                s(CallSeqs2Msg)])
    ),
    ( Fields ^ callseqs_fields = callseqs_and_percall ->
        CallSeqs3Toggle = ""
    ;
        CallSeqs3Fields = Fields ^ callseqs_fields := callseqs_and_percall,
        CallSeqs3Pref = Pref ^ pref_fields := CallSeqs3Fields,
        CallSeqs3Msg = "[Call sequence numbers including per-call]",
        CallSeqs3Toggle =
            string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
                [s(deep_cmd_pref_to_url(CallSeqs3Pref, Deep, Cmd)),
                s(CallSeqs3Msg)])
    ),
    ( Fields ^ alloc_fields = no_alloc ->
        Alloc1Toggle = ""
    ;
        Alloc1Fields = Fields ^ alloc_fields := no_alloc,
        Alloc1Pref = Pref ^ pref_fields := Alloc1Fields,
        Alloc1Msg = "[No allocations]",
        Alloc1Toggle = string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
            [s(deep_cmd_pref_to_url(Alloc1Pref, Deep, Cmd)), s(Alloc1Msg)])
    ),
    ( Fields ^ alloc_fields = alloc ->
        Alloc2Toggle = ""
    ;
        Alloc2Fields = Fields ^ alloc_fields := alloc,
        Alloc2Pref = Pref ^ pref_fields := Alloc2Fields,
        Alloc2Msg = "[Allocations]",
        Alloc2Toggle = string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
            [s(deep_cmd_pref_to_url(Alloc2Pref, Deep, Cmd)), s(Alloc2Msg)])
    ),
    ( Fields ^ alloc_fields = alloc_and_percall ->
        Alloc3Toggle = ""
    ;
        Alloc3Fields = Fields ^ alloc_fields := alloc_and_percall,
        Alloc3Pref = Pref ^ pref_fields := Alloc3Fields,
        Alloc3Msg = "[Allocations and per-call allocations]",
        Alloc3Toggle = string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
            [s(deep_cmd_pref_to_url(Alloc3Pref, Deep, Cmd)), s(Alloc3Msg)])
    ),
    ( Fields ^ memory_fields = no_memory ->
        Memory1Toggle = ""
    ;
        Memory1Fields = Fields ^ memory_fields := no_memory,
        Memory1Pref = Pref ^ pref_fields := Memory1Fields,
        Memory1Msg = "[No memory info]",
        Memory1Toggle =
            string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
                [s(deep_cmd_pref_to_url(Memory1Pref, Deep, Cmd)),
                s(Memory1Msg)])
    ),
    ( Fields ^ memory_fields = memory(units_words) ->
        Memory2Toggle = ""
    ;
        Memory2Fields = Fields ^ memory_fields := memory(units_words),
        Memory2Pref = Pref ^ pref_fields := Memory2Fields,
        Memory2Msg = "[Words]",
        Memory2Toggle =
            string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
                [s(deep_cmd_pref_to_url(Memory2Pref, Deep, Cmd)),
                s(Memory2Msg)])
    ),
    ( Fields ^ memory_fields = memory(units_bytes) ->
        Memory3Toggle = ""
    ;
        Memory3Fields = Fields ^ memory_fields := memory(units_bytes),
        Memory3Pref = Pref ^ pref_fields := Memory3Fields,
        Memory3Msg = "[Bytes]",
        Memory3Toggle =
            string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
                [s(deep_cmd_pref_to_url(Memory3Pref, Deep, Cmd)),
                s(Memory3Msg)])
    ),
    ( Fields ^ memory_fields = memory_and_percall(units_words) ->
        Memory4Toggle = ""
    ;
        Memory4Fields = Fields ^ memory_fields :=
            memory_and_percall(units_words),
        Memory4Pref = Pref ^ pref_fields := Memory4Fields,
        Memory4Msg = "[Words and per-call words]",
        Memory4Toggle =
            string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
                [s(deep_cmd_pref_to_url(Memory4Pref, Deep, Cmd)),
                s(Memory4Msg)])
    ),
    ( Fields ^ memory_fields = memory_and_percall(units_bytes) ->
        Memory5Toggle = ""
    ;
        Memory5Fields = Fields ^ memory_fields :=
            memory_and_percall(units_bytes),
        Memory5Pref = Pref ^ pref_fields := Memory5Fields,
        Memory5Msg = "[Bytes and per-call bytes]",
        Memory5Toggle =
            string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
                [s(deep_cmd_pref_to_url(Memory5Pref, Deep, Cmd)),
                s(Memory5Msg)])
    ),
    ( Fields = default_fields(Deep) ->
        DefaultToggle = ""
    ;
        DefaultMsg  = "[Restore defaults]",
        DefaultPref = Pref ^ pref_fields := default_fields(Deep),
        DefaultToggle =
            string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
                [s(deep_cmd_pref_to_url(DefaultPref, Deep, Cmd)),
                s(DefaultMsg)])
    ),
    HTML =
        "<strong>Toggle fields:</strong>\n" ++
        DefaultToggle ++
        "<br>\n" ++
        Port1Toggle ++ Port2Toggle ++
        "<br>\n" ++
        Time1Toggle ++ Time2Toggle ++ Time3Toggle ++
        Time4Toggle ++ Time5Toggle ++ Time6Toggle ++
        "<br>\n" ++
        CallSeqs1Toggle ++ CallSeqs2Toggle ++ CallSeqs3Toggle ++
        "<br>\n" ++
        Alloc1Toggle ++ Alloc2Toggle ++ Alloc3Toggle ++
        "<br>\n" ++
        Memory1Toggle ++ Memory2Toggle ++ Memory3Toggle ++
        Memory4Toggle ++ Memory5Toggle ++
        "<br>\n".

:- func footer_ancestor_toggle(cmd, preferences, deep) = string.

footer_ancestor_toggle(Cmd, Pref, Deep) = HTML :-
    (
        Pref ^ pref_anc = no,
        Display1 = yes,
        Display2 = yes,
        Msg1 = "[One ancestor]",
        Pref1 = Pref ^ pref_anc := yes(1),
        Msg2 = "[Two ancestors]",
        Pref2 = Pref ^ pref_anc := yes(2),
        Msg3 = "[Three ancestors]",
        Pref3 = Pref ^ pref_anc := yes(3),
        Msg4 = "[Five ancestors]",
        Pref4 = Pref ^ pref_anc := yes(5),
        Msg5 = "[Ten ancestors]",
        Pref5 = Pref ^ pref_anc := yes(10)
    ;
        Pref ^ pref_anc = yes(OldAncestorLimit),
        ( OldAncestorLimit > 2 ->
            Display1 = yes
        ;
            Display1 = no
        ),
        ( OldAncestorLimit > 1 ->
            Display2 = yes
        ;
            Display2 = no
        ),
        Msg1 = "[Halve ancestors]",
        Pref1 = Pref ^ pref_anc := yes(OldAncestorLimit // 2),
        Msg2 = "[Remove an ancestor]",
        Pref2 = Pref ^ pref_anc := yes(OldAncestorLimit - 1),
        Msg3 = "[Add an ancestor]",
        Pref3 = Pref ^ pref_anc := yes(OldAncestorLimit + 1),
        Msg4 = "[Double ancestors]",
        Pref4 = Pref ^ pref_anc := yes(OldAncestorLimit * 2),
        Msg5 = "[Unlimited ancestors]",
        Pref5 = Pref ^ pref_anc := no
    ),
    Toggle1 = string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
        [s(deep_cmd_pref_to_url(Pref1, Deep, Cmd)), s(Msg1)]),
    Toggle2 = string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
        [s(deep_cmd_pref_to_url(Pref2, Deep, Cmd)), s(Msg2)]),
    Toggle3 = string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
        [s(deep_cmd_pref_to_url(Pref3, Deep, Cmd)), s(Msg3)]),
    Toggle4 = string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
        [s(deep_cmd_pref_to_url(Pref4, Deep, Cmd)), s(Msg4)]),
    Toggle5 = string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
        [s(deep_cmd_pref_to_url(Pref5, Deep, Cmd)), s(Msg5)]),
    (
        Display1 = yes,
        MaybeToggle1 = Toggle1
    ;
        Display1 = no,
        MaybeToggle1 = ""
    ),
    (
        Display2 = yes,
        MaybeToggle2 = Toggle2
    ;
        Display2 = no,
        MaybeToggle2 = ""
    ),
    HTML =
        "<strong>Toggle ancestors:</strong><br />\n" ++
        MaybeToggle1 ++ MaybeToggle2 ++ Toggle3 ++ Toggle4 ++ Toggle5.

:- func footer_box_toggle(cmd, preferences, deep) = string.

footer_box_toggle(Cmd, Pref, Deep) = HTML :-
    (
        Pref ^ pref_box = nobox,
        Pref1 = Pref ^ pref_box := box,
        Msg1 = "[Box]"
    ;
        Pref ^ pref_box = box,
        Pref1 = Pref ^ pref_box := nobox,
        Msg1 = "[Unbox]"
    ),
    HTML = string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
        [s(deep_cmd_pref_to_url(Pref1, Deep, Cmd)), s(Msg1)]).

:- func footer_colour_toggle(cmd, preferences, deep) = string.

footer_colour_toggle(Cmd, Pref, Deep) = HTML :-
    (
        Pref ^ pref_colour = colour_none,
        Pref1 = Pref ^ pref_colour := colour_column_groups,
        Msg1 = "[Colour column groups]"
    ;
        Pref ^ pref_colour = colour_column_groups,
        Pref1 = Pref ^ pref_colour := colour_none,
        Msg1 = "[Fade column groups]"
    ),
    HTML = string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
        [s(deep_cmd_pref_to_url(Pref1, Deep, Cmd)), s(Msg1)]).

:- func footer_summarize_toggle(cmd, preferences, deep) = string.

footer_summarize_toggle(Cmd, Pref, Deep) = HTML :-
    (
        Pref ^ pref_summarize = summarize,
        Pref1 = Pref ^ pref_summarize := dont_summarize,
        Msg1 = "[Expand higher order calls]"
    ;
        Pref ^ pref_summarize = dont_summarize,
        Pref1 = Pref ^ pref_summarize := summarize,
        Msg1 = "[Summarize higher order calls]"
    ),
    HTML = string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
        [s(deep_cmd_pref_to_url(Pref1, Deep, Cmd)), s(Msg1)]).

:- func footer_contour_toggle(cmd, preferences, deep) = string.

footer_contour_toggle(Cmd, Pref, Deep) = HTML :-
    (
        Pref ^ pref_contour = no_contour,
        Pref1 = Pref ^ pref_contour := apply_contour,
        Msg1 = "[Apply contour exclusion]"
    ;
        Pref ^ pref_contour = apply_contour,
        Pref1 = Pref ^ pref_contour := no_contour,
        Msg1 = "[Don't apply contour exclusion]"
    ),
    HTML = string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
        [s(deep_cmd_pref_to_url(Pref1, Deep, Cmd)), s(Msg1)]).

:- func footer_time_format_toggle(cmd, preferences, deep) = string.

footer_time_format_toggle(Cmd, Pref, Deep) = HTML :-
    TimeFields = Pref ^ pref_fields ^ time_fields,
    (
        ( TimeFields = no_time
        ; TimeFields = ticks
        ),
        ToggleTimeFormat = no
    ;
        ( TimeFields = time
        ; TimeFields = ticks_and_time
        ; TimeFields = time_and_percall
        ; TimeFields = ticks_and_time_and_percall
        ),
        ToggleTimeFormat = yes
    ),
    (
        ToggleTimeFormat = no,
        HTML = ""
    ;
        ToggleTimeFormat = yes,
        (
            Pref ^ pref_time = no_scale,
            Pref1 = Pref ^ pref_time := scale_by_millions,
            Msg1  = "[Time in s, us]",
            Pref2 = Pref ^ pref_time := scale_by_thousands,
            Msg2  = "[Time in s, ms, us, ns]"
        ;
            Pref ^ pref_time = scale_by_millions,
            Pref1 = Pref ^ pref_time := no_scale,
            Msg1  = "[Time in s]",
            Pref2 = Pref ^ pref_time := scale_by_thousands,
            Msg2  = "[Time in s, ms, us, ns]"
        ;
            Pref ^ pref_time = scale_by_thousands,
            Pref1 = Pref ^ pref_time := no_scale,
            Msg1  = "[Time in s]",
            Pref2 = Pref ^ pref_time := scale_by_millions,
            Msg2  = "[Time in s, us]"
        ),
        HTML =
            string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
                [s(deep_cmd_pref_to_url(Pref1, Deep, Cmd)), s(Msg1)]) ++
            string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
                [s(deep_cmd_pref_to_url(Pref2, Deep, Cmd)), s(Msg2)])
    ).

%-----------------------------------------------------------------------------%

:- func footer_order_criteria_toggle(cmd, preferences, deep) = string.

footer_order_criteria_toggle(Cmd, Pref, Deep) =
    toggle_criteria(Pref ^ pref_criteria,
        update_criteria_in_prefs(Pref, Deep, Cmd),
        update_cost_criteria_in_prefs(Pref, Deep, Cmd)).

toggle_cost_criteria_in_top_procs_cmd(Pref, Deep, Limit,
        CostKind, InclDesc, Scope) =
    toggle_cost_criteria(CostKind, InclDesc, Scope,
        update_cost_criteria_in_top_procs_cmd(Pref, Deep, Limit)).

%-----------------------------------------------------------------------------%

:- type update_criteria_func == (func(order_criteria) = string).

:- type update_cost_criteria_func ==
    (func(cost_kind, include_descendants, measurement_scope) = string).

:- func toggle_criteria(order_criteria,
    update_criteria_func, update_cost_criteria_func) = string.

toggle_criteria(Criteria, UpdateCriteria, UpdateCostCriteria) = HTML :-
    (
        Criteria = by_context,
        Criteria1 = by_name,
        Msg1 = "[Sort by name]",
        Criteria2 =
            by_cost(default_cost_kind, default_incl_desc, default_scope),
        Msg2 = "[Sort by cost]"
    ;
        Criteria = by_name,
        Criteria1 = by_context,
        Msg1 = "[Sort by context]",
        Criteria2 =
            by_cost(default_cost_kind, default_incl_desc, default_scope),
        Msg2 = "[Sort by cost]"
    ;
        Criteria = by_cost(_, _, _),
        Criteria1 = by_context,
        Msg1 = "[Sort by context]",
        Criteria2 = by_name,
        Msg2 = "[Sort by name]"
    ),
    Toggle1 = string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
        [s(UpdateCriteria(Criteria1)), s(Msg1)]),
    Toggle2 = string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
        [s(UpdateCriteria(Criteria2)), s(Msg2)]),
    (
        Criteria = by_cost(CostKind, InclDesc, Scope),
        ToggleRest = toggle_cost_criteria(CostKind, InclDesc, Scope,
            UpdateCostCriteria)
    ;
        ( Criteria = by_context
        ; Criteria = by_name
        ),
        ToggleRest = ""
    ),
    HTML = "<strong>Toggle ordering criteria:</strong><br />\n" ++
        Toggle1 ++ Toggle2 ++ ToggleRest.

:- func toggle_cost_criteria(cost_kind, include_descendants, measurement_scope,
    update_cost_criteria_func) = string.

toggle_cost_criteria(CostKind, InclDesc, Scope, UpdateCriteria) = Toggles :-
    (
        ( CostKind = cost_redos
        ; CostKind = cost_time
        ; CostKind = cost_callseqs
        ; CostKind = cost_allocs
        ; CostKind = cost_words
        ),
        MsgCalls = "[Sort by calls]",
        ToggleCalls = string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
            [s(UpdateCriteria(cost_calls, InclDesc, Scope)), s(MsgCalls)])
    ;
        CostKind = cost_calls,
        ToggleCalls = ""
    ),
    (
        ( CostKind = cost_calls
        ; CostKind = cost_time
        ; CostKind = cost_callseqs
        ; CostKind = cost_allocs
        ; CostKind = cost_words
        ),
        MsgRedos = "[Sort by redos]",
        ToggleRedos = string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
            [s(UpdateCriteria(cost_redos, InclDesc, Scope)), s(MsgRedos)])
    ;
        CostKind = cost_redos,
        ToggleRedos = ""
    ),
    (
        ( CostKind = cost_calls
        ; CostKind = cost_redos
        ; CostKind = cost_callseqs
        ; CostKind = cost_allocs
        ; CostKind = cost_words
        ),
        MsgTime = "[Sort by time]",
        ToggleTime = string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
            [s(UpdateCriteria(cost_time, InclDesc, Scope)), s(MsgTime)])
    ;
        CostKind = cost_time,
        ToggleTime = ""
    ),
    (
        ( CostKind = cost_calls
        ; CostKind = cost_redos
        ; CostKind = cost_time
        ; CostKind = cost_allocs
        ; CostKind = cost_words
        ),
        MsgCallSeqs = "[Sort by call sequence numbers]",
        ToggleCallSeqs =
            string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
                [s(UpdateCriteria(cost_callseqs, InclDesc, Scope)),
                s(MsgCallSeqs)])
    ;
        CostKind = cost_callseqs,
        ToggleCallSeqs = ""
    ),
    (
        ( CostKind = cost_calls
        ; CostKind = cost_redos
        ; CostKind = cost_time
        ; CostKind = cost_callseqs
        ; CostKind = cost_words
        ),
        MsgAllocs = "[Sort by allocations]",
        ToggleAllocs =
            string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
                [s(UpdateCriteria(cost_allocs, InclDesc, Scope)),
                s(MsgAllocs)])
    ;
        CostKind = cost_allocs,
        ToggleAllocs = ""
    ),
    (
        ( CostKind = cost_calls
        ; CostKind = cost_redos
        ; CostKind = cost_time
        ; CostKind = cost_callseqs
        ; CostKind = cost_allocs
        ),
        MsgWords = "[Sort by words]",
        ToggleWords = string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
            [s(UpdateCriteria(cost_words, InclDesc, Scope)), s(MsgWords)])
    ;
        CostKind = cost_words,
        ToggleWords = ""
    ),
    (
        InclDesc = self,
        MsgDesc = "[Include descendants]",
        ToggleDesc = string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
            [s(UpdateCriteria(CostKind, self_and_desc, Scope)), s(MsgDesc)])
    ;
        InclDesc = self_and_desc,
        MsgDesc = "[Exclude descendants]",
        ToggleDesc = string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
            [s(UpdateCriteria(CostKind, self, Scope)), s(MsgDesc)])
    ),
    (
        Scope = per_call,
        MsgScope = "[Count overall cost]",
        ToggleScope = string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
            [s(UpdateCriteria(CostKind, InclDesc, overall)), s(MsgScope)])
    ;
        Scope = overall,
        MsgScope = "[Count per-call cost]",
        ToggleScope = string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
            [s(UpdateCriteria(CostKind, InclDesc, per_call)), s(MsgScope)])
    ),
    Toggles = ToggleCalls ++ ToggleRedos ++ ToggleTime ++ ToggleCallSeqs ++
        ToggleAllocs ++ ToggleWords ++
        "\n<br>\n" ++ ToggleDesc ++ ToggleScope.

%-----------------------------------------------------------------------------%
%
% Toggles to control showing/hiding inactive modules/procedures
%

:- func footer_inactive_modules_toggle(cmd, preferences, deep) = string.

footer_inactive_modules_toggle(Cmd, Pref0, Deep) = HTML :-
    Pref0 ^ pref_inactive = inactive_items(Procs, Modules),
    (
        Modules = inactive_show,
        Msg  = "[Hide inactive modules]",
        Pref = Pref0 ^ pref_inactive := inactive_items(Procs, inactive_hide)
    ;
        Modules = inactive_hide,
        Msg  = "[Show inactive modules]",
        Pref = Pref0 ^ pref_inactive := inactive_items(Procs, inactive_show)
    ),
    HTML = string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
        [s(deep_cmd_pref_to_url(Pref, Deep, Cmd)), s(Msg)]).

:- func footer_inactive_procs_toggle(cmd, preferences, deep) = string.

footer_inactive_procs_toggle(Cmd, Pref0, Deep) = HTML :-
    Pref0 ^ pref_inactive = inactive_items(Procs, Modules),
    (
        Procs = inactive_show,
        Msg = "[Hide inactive procedures]",
        Pref = Pref0 ^ pref_inactive := inactive_items(inactive_hide, Modules)
    ;
        Procs = inactive_hide,
        Msg = "[Show inactive procedures]",
        Pref = Pref0 ^ pref_inactive := inactive_items(inactive_show, Modules)
    ),
    HTML = string.format("<A CLASS=""button"" HREF=""%s"">%s</A>\n",
        [s(deep_cmd_pref_to_url(Pref, Deep, Cmd)), s(Msg)]).

%-----------------------------------------------------------------------------%

:- func update_criteria_in_prefs(preferences, deep, cmd, order_criteria)
    = string.

update_criteria_in_prefs(Pref0, Deep, Cmd, Criteria) = HTML :-
    Pref = Pref0 ^ pref_criteria := Criteria,
    HTML = deep_cmd_pref_to_url(Pref, Deep, Cmd).

:- func update_cost_criteria_in_prefs(preferences, deep, cmd,
    cost_kind, include_descendants, measurement_scope) = string.

update_cost_criteria_in_prefs(Pref0, Deep, Cmd, CostKind, InclDesc, Scope)
        = HTML :-
    Pref = Pref0 ^ pref_criteria := by_cost(CostKind, InclDesc, Scope),
    HTML = deep_cmd_pref_to_url(Pref, Deep, Cmd).

:- func update_cost_criteria_in_top_procs_cmd(preferences, deep, display_limit,
    cost_kind, include_descendants, measurement_scope) = string.

update_cost_criteria_in_top_procs_cmd(Pref, Deep, Limit,
        CostKind, InclDesc, Scope) = HTML :-
    Cmd = deep_cmd_top_procs(Limit, CostKind, InclDesc, Scope),
    HTML = deep_cmd_pref_to_url(Pref, Deep, Cmd).

%-----------------------------------------------------------------------------%

criteria_to_description(by_context) = "ordered by context".
criteria_to_description(by_name) = "ordered by name".
criteria_to_description(by_cost(CostKind, InclDesc, Scope)) =
    cost_criteria_to_description(CostKind, InclDesc, Scope).

cost_criteria_to_description(CostKind, InclDesc, Scope) = Desc :-
    Desc =
        "ordered by " ++
        incl_desc_to_description(InclDesc) ++ " " ++
        cost_kind_to_description(CostKind) ++ " " ++
        scope_to_description(Scope).

:- func cost_kind_to_description(cost_kind) = string.

cost_kind_to_description(cost_calls)    = "number of calls".
cost_kind_to_description(cost_redos)    = "number of redos".
cost_kind_to_description(cost_time)     = "time".
cost_kind_to_description(cost_callseqs) = "call sequence numbers".
cost_kind_to_description(cost_allocs)   = "memory allocations".
cost_kind_to_description(cost_words)    = "words allocated".

:- func incl_desc_to_description(include_descendants) = string.

incl_desc_to_description(self) = "self".
incl_desc_to_description(self_and_desc) = "total".

:- func scope_to_description(measurement_scope) = string.

scope_to_description(per_call) = "per call".
scope_to_description(overall) = "overall".

%-----------------------------------------------------------------------------%

% The predicates banner_style, fields_header, table_width and
% own_and_desc_to_html all make decisions about what columns each row
% in the table will have.  They therefore have similar control structures,
% and a change in one may require changes in the others as well.

fields_header(Pref, IdFields, TotalsDisp, WrapFunc) = HTML :-
    Fields = Pref ^ pref_fields,
    ProcName = WrapFunc("Procedure", by_name),
    ModuleName = WrapFunc("Module", by_name),

    some [!FirstRow, !SecondRow] (
        (
            IdFields = source_proc,
            Source = WrapFunc("Source", by_context),
            !:FirstRow =
                "<TR>\n" ++
                string.format("<TH ALIGN=LEFT ROWSPAN=2>%s\n", [s(Source)]) ++
                string.format("<TH ALIGN=LEFT ROWSPAN=2>%s\n", [s(ProcName)])
        ;
            IdFields = rank_proc,
            !:FirstRow =
                "<TR>\n" ++
                "<TH ALIGN=LEFT ROWSPAN=2>Rank\n" ++
                string.format("<TH ALIGN=LEFT ROWSPAN=2>%s\n", [s(ProcName)])
        ;
            IdFields = rank_module,
            !:FirstRow =
                "<TR>\n" ++
                "<TH ALIGN=LEFT ROWSPAN=2>Rank\n" ++
                string.format("<TH ALIGN=LEFT ROWSPAN=2>%s\n", [s(ModuleName)])
        ;
            IdFields = proc,
            !:FirstRow =
                "<TR>\n" ++
                string.format("<TH ALIGN=LEFT ROWSPAN=2>%s\n", [s(ProcName)])
        ),
        !:SecondRow = "<TR>\n",

        ShowPortCounts = show_port_counts(Fields),
        (
            ShowPortCounts = yes,
            Calls = WrapFunc("Calls", by_cost(cost_calls, self, overall)),
            Redos = WrapFunc("Redos", by_cost(cost_redos, self, overall)),
            !:FirstRow = !.FirstRow ++
                "<TH COLSPAN=5>Port counts\n",
            !:SecondRow = !.SecondRow ++
                string.format("<TH ALIGN=RIGHT>%s\n", [s(Calls)]) ++
                "<TH ALIGN=RIGHT>Exits\n" ++
                "<TH ALIGN=RIGHT>Fails\n" ++
                string.format("<TH ALIGN=RIGHT>%s\n", [s(Redos)]) ++
                "<TH ALIGN=RIGHT>Excps\n"
        ;
            ShowPortCounts = no
        ),

        ShowQuanta = show_quanta(Fields),
        (
            ShowQuanta = yes,
            TicksSelfOverall = WrapFunc("Self",
                by_cost(cost_time, self, overall)),
            TicksSelfHeading =
                string.format("<TH ALIGN=RIGHT>%s\n", [s(TicksSelfOverall)]),
            TicksSelfFields = 1
        ;
            ShowQuanta = no,
            TicksSelfHeading = "",
            TicksSelfFields = 0
        ),
        ShowTimes = show_times(Fields),
        (
            ShowTimes = yes,
            ( show_quanta(Fields) = yes ->
                TimeSelfOverall = WrapFunc("Time",
                    by_cost(cost_time, self, overall))
            ;
                TimeSelfOverall = WrapFunc("Self",
                    by_cost(cost_time, self, overall))
            ),
            TimeSelfHeading =
                string.format("<TH ALIGN=RIGHT>%s\n", [s(TimeSelfOverall)]),
            TimeSelfFields = 1
        ;
            ShowTimes = no,
            TimeSelfHeading = "",
            TimeSelfFields = 0
        ),
        ( ( ShowQuanta = yes ; ShowTimes = yes ) ->
            TimeSelfPercentHeading = "<TH ALIGN=RIGHT>%\n",
            TimeSelfPercentFields = 1
        ;
            TimeSelfPercentHeading = "",
            TimeSelfPercentFields = 0
        ),
        ShowTimesPerCall = show_times_per_call(Fields),
        (
            ShowTimesPerCall = yes,
            TimeSelfPerCall = WrapFunc("/call",
                by_cost(cost_time, self, per_call)),
            TimeSelfPerCallHeading =
                string.format("<TH ALIGN=RIGHT>%s\n", [s(TimeSelfPerCall)]),
            TimeSelfPerCallFields = 1
        ;
            ShowTimesPerCall = no,
            TimeSelfPerCallHeading = "",
            TimeSelfPerCallFields = 0
        ),
        ( TotalsDisp = totals_meaningful, ShowQuanta = yes ->
            TicksTotalOverall = WrapFunc("Total",
                by_cost(cost_time, self_and_desc, overall)),
            TicksTotalHeading =
                string.format("<TH ALIGN=RIGHT>%s\n", [s(TicksTotalOverall)]),
            TicksTotalFields = 1
        ;
            TicksTotalHeading = "",
            TicksTotalFields = 0
        ),
        ( TotalsDisp = totals_meaningful, ShowTimes = yes ->
            ( show_quanta(Fields) = yes ->
                TimeTotalOverall = WrapFunc("Time",
                    by_cost(cost_time, self_and_desc, overall))
            ;
                TimeTotalOverall = WrapFunc("Total",
                    by_cost(cost_time, self_and_desc, overall))
            ),
            TimeTotalHeading =
                string.format("<TH ALIGN=RIGHT>%s\n", [s(TimeTotalOverall)]),
            TimeTotalFields = 1
        ;
            TimeTotalHeading = "",
            TimeTotalFields = 0
        ),
        (
            TotalsDisp = totals_meaningful,
            ( ShowQuanta = yes ; ShowTimes = yes )
        ->
            TimeTotalPercentHeading = "<TH ALIGN=RIGHT>%\n",
            TimeTotalPercentFields = 1
        ;
            TimeTotalPercentHeading = "",
            TimeTotalPercentFields = 0
        ),
        ( TotalsDisp = totals_meaningful, ShowTimesPerCall = yes ->
            TimeTotalPerCall = WrapFunc("/call",
                by_cost(cost_time, self_and_desc, per_call)),
            TimeTotalPerCallHeading =
                string.format("<TH ALIGN=RIGHT>%s\n", [s(TimeTotalPerCall)]),
            TimeTotalPerCallFields = 1
        ;
            TimeTotalPerCallHeading = "",
            TimeTotalPerCallFields = 0
        ),
        TimeFields =
            TicksSelfFields + TimeSelfFields +
            TimeSelfPercentFields + TimeSelfPerCallFields +
            TicksTotalFields + TimeTotalFields +
            TimeTotalPercentFields + TimeTotalPerCallFields,
        !:SecondRow = !.SecondRow ++
            TicksSelfHeading ++ TimeSelfHeading ++
            TimeSelfPercentHeading ++ TimeSelfPerCallHeading ++
            TicksTotalHeading ++ TimeTotalHeading ++
            TimeTotalPercentHeading ++ TimeTotalPerCallHeading,
        (
            ShowQuanta = yes,
            ShowTimes = yes,
            !:FirstRow = !.FirstRow ++
                string.format("<TH COLSPAN=%d>Clock ticks and times\n",
                    [i(TimeFields)])
        ;
            ShowQuanta = yes,
            ShowTimes = no,
            !:FirstRow = !.FirstRow ++
                string.format("<TH COLSPAN=%d>Clock ticks\n", [i(TimeFields)])
        ;
            ShowQuanta = no,
            ShowTimes = yes,
            !:FirstRow = !.FirstRow ++
                string.format("<TH COLSPAN=%d>Time\n", [i(TimeFields)])
        ;
            ShowQuanta = no,
            ShowTimes = no
        ),

        ShowCallSeqs = show_callseqs(Fields),
        (
            ShowCallSeqs = yes,
            CallSeqsSelfOverall = WrapFunc("Self",
                by_cost(cost_callseqs, self, overall)),
            CallSeqsSelfHeading =
                string.format("<TH ALIGN=RIGHT>%s\n",
                    [s(CallSeqsSelfOverall)]) ++
                "<TH ALIGN=RIGHT>%\n",
            CallSeqsSelfFields = 2
        ;
            ShowCallSeqs = no,
            CallSeqsSelfHeading = "",
            CallSeqsSelfFields = 0
        ),
        ShowCallSeqsPerCall = show_callseqs_per_call(Fields),
        (
            ShowCallSeqsPerCall = yes,
            CallSeqsSelfPerCall = WrapFunc("/call",
                by_cost(cost_callseqs, self, per_call)),
            CallSeqsSelfPerCallHeading =
                string.format("<TH ALIGN=RIGHT>%s\n", [s(CallSeqsSelfPerCall)]),
            CallSeqsSelfPerCallFields = 1
        ;
            ShowCallSeqsPerCall = no,
            CallSeqsSelfPerCallHeading = "",
            CallSeqsSelfPerCallFields = 0
        ),
        ( TotalsDisp = totals_meaningful, ShowCallSeqs = yes ->
            CallSeqsTotalOverall = WrapFunc("Total",
                by_cost(cost_callseqs, self_and_desc, overall)),
            CallSeqsTotalHeading =
                string.format("<TH ALIGN=RIGHT>%s\n",
                    [s(CallSeqsTotalOverall)]) ++
                "<TH ALIGN=RIGHT>%\n",
            CallSeqsTotalFields = 2
        ;
            CallSeqsTotalHeading = "",
            CallSeqsTotalFields = 0
        ),
        ( TotalsDisp = totals_meaningful, ShowCallSeqsPerCall = yes ->
            CallSeqsTotalPerCall = WrapFunc("/call",
                by_cost(cost_callseqs, self_and_desc, per_call)),
            CallSeqsTotalPerCallHeading =
                string.format("<TH ALIGN=RIGHT>%s\n",
                    [s(CallSeqsTotalPerCall)]),
            CallSeqsTotalPerCallFields = 1
        ;
            CallSeqsTotalPerCallHeading = "",
            CallSeqsTotalPerCallFields = 0
        ),
        CallSeqsFields =
            CallSeqsSelfFields + CallSeqsSelfPerCallFields +
            CallSeqsTotalFields + CallSeqsTotalPerCallFields,
        !:SecondRow = !.SecondRow ++
            CallSeqsSelfHeading ++ CallSeqsSelfPerCallHeading ++
            CallSeqsTotalHeading ++ CallSeqsTotalPerCallHeading,
        (
            ShowCallSeqs = yes,
            !:FirstRow = !.FirstRow ++
                string.format("<TH COLSPAN=%d>Call sequence numbers\n",
                    [i(CallSeqsFields)])
        ;
            ShowCallSeqs = no
        ),

        ShowAlloc = show_alloc(Fields),
        (
            ShowAlloc = yes,
            AllocsSelfOverall = WrapFunc("Self",
                by_cost(cost_allocs, self, overall)),
            AllocsSelfHeading =
                string.format("<TH ALIGN=RIGHT>%s\n",
                    [s(AllocsSelfOverall)]) ++
                "<TH ALIGN=RIGHT>%\n",
            AllocsSelfFields = 2
        ;
            ShowAlloc = no,
            AllocsSelfHeading = "",
            AllocsSelfFields = 0
        ),
        ShowAllocPerCall = show_alloc_per_call(Fields),
        (
            ShowAllocPerCall = yes,
            AllocsSelfPerCall = WrapFunc("/call",
                by_cost(cost_allocs, self, per_call)),
            AllocsSelfPerCallHeading =
                string.format("<TH ALIGN=RIGHT>%s\n", [s(AllocsSelfPerCall)]),
            AllocsSelfPerCallFields = 1
        ;
            ShowAllocPerCall = no,
            AllocsSelfPerCallHeading = "",
            AllocsSelfPerCallFields = 0
        ),
        ( TotalsDisp = totals_meaningful, ShowAlloc = yes ->
            AllocsTotalOverall = WrapFunc("Total",
                by_cost(cost_allocs, self_and_desc, overall)),
            AllocsTotalHeading =
                string.format("<TH ALIGN=RIGHT>%s\n",
                    [s(AllocsTotalOverall)]) ++
                "<TH ALIGN=RIGHT>%\n",
            AllocsTotalFields = 2
        ;
            AllocsTotalHeading = "",
            AllocsTotalFields = 0
        ),
        ( TotalsDisp = totals_meaningful, ShowAllocPerCall = yes ->
            AllocsTotalPerCall = WrapFunc("/call",
                by_cost(cost_allocs, self_and_desc, per_call)),
            AllocsTotalPerCallHeading =
                string.format("<TH ALIGN=RIGHT>%s\n", [s(AllocsTotalPerCall)]),
            AllocsTotalPerCallFields = 1
        ;
            AllocsTotalPerCallHeading = "",
            AllocsTotalPerCallFields = 0
        ),
        AllocsFields =
            AllocsSelfFields + AllocsSelfPerCallFields +
            AllocsTotalFields + AllocsTotalPerCallFields,
        !:SecondRow = !.SecondRow ++
            AllocsSelfHeading ++ AllocsSelfPerCallHeading ++
            AllocsTotalHeading ++ AllocsTotalPerCallHeading,
        (
            ShowAlloc = yes,
            !:FirstRow = !.FirstRow ++
                string.format("<TH COLSPAN=%d>Memory allocations\n",
                    [i(AllocsFields)])
        ;
            ShowAlloc = no
        ),

        ShowMemory = show_memory(Fields),
        (
            ShowMemory = yes(_),
            MemorySelfOverall = WrapFunc("Self",
                by_cost(cost_words, self, overall)),
            MemorySelfHeading =
                string.format("<TH ALIGN=RIGHT>%s\n",
                    [s(MemorySelfOverall)]) ++
                "<TH ALIGN=RIGHT>%\n",
            MemorySelfFields = 2
        ;
            ShowMemory = no,
            MemorySelfHeading = "",
            MemorySelfFields = 0
        ),
        ShowMemoryPerCall = show_memory_per_call(Fields),
        (
            ShowMemoryPerCall = yes(_),
            MemorySelfPerCall = WrapFunc("/call",
                by_cost(cost_words, self, per_call)),
            MemorySelfPerCallHeading =
                string.format("<TH ALIGN=RIGHT>%s\n", [s(MemorySelfPerCall)]),
            MemorySelfPerCallFields = 1
        ;
            ShowMemoryPerCall = no,
            MemorySelfPerCallHeading = "",
            MemorySelfPerCallFields = 0
        ),
        ( TotalsDisp = totals_meaningful, ShowMemory = yes(_) ->
            MemoryTotalOverall = WrapFunc("Total",
                by_cost(cost_words, self_and_desc, overall)),
            MemoryTotalHeading =
                string.format("<TH ALIGN=RIGHT>%s\n",
                    [s(MemoryTotalOverall)]) ++
                "<TH ALIGN=RIGHT>%\n",
            MemoryTotalFields = 2
        ;
            MemoryTotalHeading = "",
            MemoryTotalFields = 0
        ),
        ( TotalsDisp = totals_meaningful, ShowMemoryPerCall = yes(_) ->
            MemoryTotalPerCall = WrapFunc("/call",
                by_cost(cost_words, self_and_desc, per_call)),
            MemoryTotalPerCallHeading =
                string.format("<TH ALIGN=RIGHT>%s\n", [s(MemoryTotalPerCall)]),
            MemoryTotalPerCallFields = 1
        ;
            MemoryTotalPerCallHeading = "",
            MemoryTotalPerCallFields = 0
        ),
        MemoryFields =
            MemorySelfFields + MemorySelfPerCallFields +
            MemoryTotalFields + MemoryTotalPerCallFields,
        !:SecondRow = !.SecondRow ++
            MemorySelfHeading ++ MemorySelfPerCallHeading ++
            MemoryTotalHeading ++ MemoryTotalPerCallHeading,
        (
            ShowMemory = yes(Units),
            (
                Units = units_words,
                !:FirstRow = !.FirstRow ++
                    string.format("<TH COLSPAN=%d>Memory words\n",
                        [i(MemoryFields)])
            ;
                Units = units_bytes,
                !:FirstRow = !.FirstRow ++
                    string.format("<TH COLSPAN=%d>Memory bytes\n",
                        [i(MemoryFields)])
            )
        ;
            ShowMemory = no
        ),
        HTML =
            "<THEAD>\n" ++
            !.FirstRow ++
            !.SecondRow ++
            "<TBODY>\n" ++
            separator_row(Pref, IdFields, TotalsDisp)
    ).

%-----------------------------------------------------------------------------%

header_row(Heading, Pref, IdFields, TotalsDisp) = Separator :-
    Separator = string.format("<TR><TD COLSPAN=%d>%s</TD></TR>\n",
        [i(table_width(Pref, IdFields, TotalsDisp)), s(Heading)]).

separator_row(Pref, IdFields, TotalsDisp) = Separator :-
    Separator = string.format("<TR><TD COLSPAN=%d></TD></TR>\n",
        [i(table_width(Pref, IdFields, TotalsDisp))]).

:- func table_width(preferences, id_fields, totals_disposition) = int.

table_width(Pref, IdFields, TotalsDisp) = Width :-
    Fields = Pref ^ pref_fields,
    (
        IdFields = source_proc,
        Id = 2
    ;
        IdFields = rank_module,
        Id = 2
    ;
        IdFields = rank_proc,
        Id = 2
    ;
        IdFields = proc,
        Id = 1
    ),
    (
        Fields ^ port_fields = no_port,
        Port = 0
    ;
        Fields ^ port_fields = port,
        Port = 5
    ),
    (
        Fields ^ time_fields = no_time,
        Time = 0
    ;
        Fields ^ time_fields = ticks,
        Time = 2
    ;
        Fields ^ time_fields = time,
        Time = 2
    ;
        Fields ^ time_fields = ticks_and_time,
        Time = 3
    ;
        Fields ^ time_fields = time_and_percall,
        Time = 3
    ;
        Fields ^ time_fields = ticks_and_time_and_percall,
        Time = 4
    ),
    (
        Fields ^ callseqs_fields = no_callseqs,
        CallSeqs = 0
    ;
        Fields ^ callseqs_fields = callseqs,
        CallSeqs = 2
    ;
        Fields ^ callseqs_fields = callseqs_and_percall,
        CallSeqs = 3
    ),
    (
        Fields ^ alloc_fields = no_alloc,
        Alloc = 0
    ;
        Fields ^ alloc_fields = alloc,
        Alloc = 2
    ;
        Fields ^ alloc_fields = alloc_and_percall,
        Alloc = 3
    ),
    (
        Fields ^ memory_fields = no_memory,
        Memory = 0
    ;
        Fields ^ memory_fields = memory(_),
        Memory = 2
    ;
        Fields ^ memory_fields = memory_and_percall(_),
        Memory = 3
    ),
    (
        TotalsDisp = totals_meaningful,
        Width = Id + Port + Time * 2 + CallSeqs * 2 + Alloc * 2 + Memory * 2
    ;
        TotalsDisp = totals_not_meaningful,
        Width = Id + Port + Time + CallSeqs + Alloc + Memory
    ).

%-----------------------------------------------------------------------------%

add_context(Context, LineGroup0) = LineGroup :-
    LineGroup0 =
        line_group(FileName, LineNumber, Name, Own, Desc, HTML0, LaterLines),
    HTML = string.format("<TD CLASS=id>%s</TD>%s",
        [s(Context), s(HTML0)]),
    LineGroup =
        line_group(FileName, LineNumber, Name, Own, Desc, HTML, LaterLines).

add_self_context(LineGroup0) = LineGroup :-
    LineGroup0 =
        line_group(FileName, LineNumber, Name, Own, Desc, HTML0, LaterLines),
    HTML = string.format("<TD CLASS=id>%s:%d</TD>%s",
        [s(FileName), i(LineNumber), s(HTML0)]),
    LineGroup =
        line_group(FileName, LineNumber, Name, Own, Desc, HTML, LaterLines).

add_ranks(LineGroups0) = add_ranks_2(1, LineGroups0).

:- func add_ranks_2(int, list(line_group(one_id, LL)))
    = list(line_group(two_id, LL)).

add_ranks_2(_Rank, []) = [].
add_ranks_2(Rank, [LineGroup0 | LineGroups0]) = [LineGroup | LineGroups] :-
    LineGroup0 =
        line_group(FileName, LineNumber, Name, Own, Desc, HTML0, LaterLines),
    HTML = string.format("<TD CLASS=id>%d</TD>%s", [i(Rank), s(HTML0)]),
    LineGroup =
        line_group(FileName, LineNumber, Name, Own, Desc, HTML, LaterLines),
    LineGroups = add_ranks_2(Rank + 1, LineGroups0).

line_to_one_id_subline_group(LineGroup0) = LineGroup :-
    LineGroup0 =
        line_group(FileName, LineNumber, Name, Own, Desc, HTML, unit),
    LineGroup =
        line_group(FileName, LineNumber, Name, Own, Desc, HTML,
            sub_lines(one_id, [])).

line_to_two_id_subline_group(LineGroup0) = LineGroup :-
    LineGroup0 = line_group(FileName, LineNumber, Name, Own, Desc,
        HTML, unit),
    LineGroup = line_group(FileName, LineNumber, Name, Own, Desc,
        HTML, sub_lines(two_id, [])).

%-----------------------------------------------------------------------------%

one_id_line_to_html(Pref, Deep, TotalsDisp, LineGroup) =
    "<TR>\n" ++
    LineGroup ^ group_first_line_id ++
    own_and_desc_to_html(LineGroup ^ group_own, LineGroup ^ group_desc,
        Pref, Deep, TotalsDisp) ++
    "</TR>\n".

one_id_line_group_to_html(Pref, Deep, TotalsDisp, LineGroup) =
    "<TR>\n" ++
    LineGroup ^ group_first_line_id ++
    own_and_desc_to_html(LineGroup ^ group_own, LineGroup ^ group_desc,
        Pref, Deep, TotalsDisp) ++
    "</TR>\n" ++
    string.append_list(
        list.map(one_id_line_to_html(Pref, Deep, TotalsDisp),
            LineGroup ^ group_later_lines ^ sub_line_list)).

two_id_line_to_html(Pref, Deep, TotalsDisp, LineGroup) =
    "<TR>\n" ++
    LineGroup ^ group_first_line_id ++
    own_and_desc_to_html(LineGroup ^ group_own, LineGroup ^ group_desc,
        Pref, Deep, TotalsDisp) ++
    "</TR>\n".

two_id_line_group_to_html(Pref, Deep, TotalsDisp, LineGroup) =
    "<TR>\n" ++
    LineGroup ^ group_first_line_id ++
    own_and_desc_to_html(LineGroup ^ group_own, LineGroup ^ group_desc,
        Pref, Deep, TotalsDisp) ++
    "</TR>\n" ++
    string.append_list(
        list.map(two_id_line_to_html(Pref, Deep, TotalsDisp),
            LineGroup ^ group_later_lines ^ sub_line_list)).

%-----------------------------------------------------------------------------%

own_and_desc_to_html(Own, Desc, Pref, Deep, TotalsDisp) = HTML :-
    add_own_to_inherit(Own, Desc) = OwnPlusDesc,
    Root = root_total_info(Deep),
    Calls = calls(Own),
    Exits = exits(Own),
    Fails = fails(Own),
    Redos = redos(Own),
    Excps = excps(Own),

    OwnQuanta = quanta(Own),
    TotalQuanta = inherit_quanta(OwnPlusDesc),
    RootQuanta = inherit_quanta(Root),
    OwnQuantaProp = percentage(OwnQuanta, RootQuanta),
    TotalQuantaProp = percentage(TotalQuanta, RootQuanta),

    OwnCallSeqs = callseqs(Own),
    TotalCallSeqs = inherit_callseqs(OwnPlusDesc),
    RootCallSeqs = inherit_callseqs(Root),
    OwnCallSeqsProp = percentage(OwnCallSeqs, RootCallSeqs),
    TotalCallSeqsProp = percentage(TotalCallSeqs, RootCallSeqs),

    OwnAllocs = allocs(Own),
    TotalAllocs = inherit_allocs(OwnPlusDesc),
    RootAllocs = inherit_allocs(Root),
    OwnAllocProp = percentage(OwnAllocs, RootAllocs),
    TotalAllocProp = percentage(TotalAllocs, RootAllocs),

    OwnWords = words(Own),
    TotalWords = inherit_words(OwnPlusDesc),
    RootWords = inherit_words(Root),
    OwnMemoryProp = percentage(OwnWords, RootWords),
    TotalMemoryProp = percentage(TotalWords, RootWords),

    Fields = Pref ^ pref_fields,

    ShowPortCounts = show_port_counts(Fields),
    (
        ShowPortCounts = yes,
        PortHTML =
            string.format("<TD CLASS=port ALIGN=RIGHT>%s</TD>\n",
                [s(commas(Calls))]) ++
            string.format("<TD CLASS=port ALIGN=RIGHT>%s</TD>\n",
                [s(commas(Exits))]) ++
            string.format("<TD CLASS=port ALIGN=RIGHT>%s</TD>\n",
                [s(commas(Fails))]) ++
            string.format("<TD CLASS=port ALIGN=RIGHT>%s</TD>\n",
                [s(commas(Redos))]) ++
            string.format("<TD CLASS=port ALIGN=RIGHT>%s</TD>\n",
                [s(commas(Excps))])
    ;
        ShowPortCounts = no,
        PortHTML = ""
    ),

    ShowQuanta = show_quanta(Fields),
    (
        ShowQuanta = yes,
        QuantaSelfHTML =
            string.format("<TD CLASS=time ALIGN=RIGHT>%s</TD>\n",
                [s(commas(OwnQuanta))]),
        QuantaTotalHTML =
            string.format("<TD CLASS=time ALIGN=RIGHT>%s</TD>\n",
                [s(commas(TotalQuanta))])
    ;
        ShowQuanta = no,
        QuantaSelfHTML = "",
        QuantaTotalHTML = ""
    ),
    ShowTimes = show_times(Fields),
    (
        ShowTimes = yes,
        TimeSelfHTML =
            string.format("<TD CLASS=time ALIGN=RIGHT>%s</TD>\n",
                [s(overall_time(Pref, Deep, OwnQuanta))]),
        TimeTotalHTML =
            string.format("<TD CLASS=time ALIGN=RIGHT>%s</TD>\n",
                [s(overall_time(Pref, Deep, TotalQuanta))])
    ;
        ShowTimes = no,
        TimeSelfHTML = "",
        TimeTotalHTML = ""
    ),
    ShowTimeFraction = bool.or(ShowQuanta, ShowTimes),
    (
        ShowTimeFraction = yes,
        QuantaPropSelfHTML =
            string.format("<TD CLASS=time ALIGN=RIGHT>%s</TD>\n",
                [s(OwnQuantaProp)]),
        QuantaPropTotalHTML =
            string.format("<TD CLASS=time ALIGN=RIGHT>%s</TD>\n",
                [s(TotalQuantaProp)])
    ;
        ShowTimeFraction = no,
        QuantaPropSelfHTML = "",
        QuantaPropTotalHTML = ""
    ),
    ShowTimesPerCall = show_times_per_call(Fields),
    (
        ShowTimesPerCall = yes,
        TimePerCallSelfHTML =
            string.format("<TD CLASS=time ALIGN=RIGHT>%s</TD>\n",
                [s(per_call_time(Pref, Deep, OwnQuanta, Calls))]),
        TimePerCallTotalHTML =
            string.format("<TD CLASS=time ALIGN=RIGHT>%s</TD>\n",
                [s(per_call_time(Pref, Deep, TotalQuanta, Calls))])
    ;
        ShowTimesPerCall = no,
        TimePerCallSelfHTML = "",
        TimePerCallTotalHTML = ""
    ),

    ShowCallSeqs = show_callseqs(Fields),
    (
        ShowCallSeqs = yes,
        CallSeqsSelfHTML =
            string.format("<TD CLASS=callseqs ALIGN=RIGHT>%s</TD>\n",
                [s(commas(OwnCallSeqs))]),
        CallSeqsTotalHTML =
            string.format("<TD CLASS=callseqs ALIGN=RIGHT>%s</TD>\n",
                [s(commas(TotalCallSeqs))]),
        CallSeqsPropSelfHTML =
            string.format("<TD CLASS=callseqs ALIGN=RIGHT>%s</TD>\n",
                [s(OwnCallSeqsProp)]),
        CallSeqsPropTotalHTML =
            string.format("<TD CLASS=callseqs ALIGN=RIGHT>%s</TD>\n",
                [s(TotalCallSeqsProp)])
    ;
        ShowCallSeqs = no,
        CallSeqsSelfHTML = "",
        CallSeqsTotalHTML = "",
        CallSeqsPropSelfHTML = "",
        CallSeqsPropTotalHTML = ""
    ),
    ShowCallSeqsPerCall = show_callseqs_per_call(Fields),
    (
        ShowCallSeqsPerCall = yes,
        ( Calls = 0 ->
            OwnCallSeqsPerCall = "N/A",
            TotalCallSeqsPerCall = "N/A"
        ;
            OwnCallSeqsPerCall =
                one_decimal_fraction(float(OwnCallSeqs) / float(Calls)),
            TotalCallSeqsPerCall =
                one_decimal_fraction(float(TotalCallSeqs) / float(Calls))
        ),
        CallSeqsPerCallSelfHTML =
            string.format("<TD CLASS=callseqs ALIGN=RIGHT>%s</TD>\n",
                [s(OwnCallSeqsPerCall)]),
        CallSeqsPerCallTotalHTML =
            string.format("<TD CLASS=callseqs ALIGN=RIGHT>%s</TD>\n",
                [s(TotalCallSeqsPerCall)])
    ;
        ShowCallSeqsPerCall = no,
        CallSeqsPerCallSelfHTML = "",
        CallSeqsPerCallTotalHTML = ""
    ),

    ShowAlloc = show_alloc(Fields),
    (
        ShowAlloc = yes,
        AllocSelfHTML =
            string.format("<TD CLASS=alloc ALIGN=RIGHT>%s</TD>\n",
                [s(commas(OwnAllocs))]) ++
            string.format("<TD CLASS=alloc ALIGN=RIGHT>%s</TD>\n",
                [s(OwnAllocProp)]),
        AllocTotalHTML =
            string.format("<TD CLASS=alloc ALIGN=RIGHT>%s</TD>\n",
                [s(commas(TotalAllocs))]) ++
            string.format("<TD CLASS=alloc ALIGN=RIGHT>%s</TD>\n",
                [s(TotalAllocProp)])
    ;
        ShowAlloc = no,
        AllocSelfHTML = "",
        AllocTotalHTML = ""
    ),
    ShowAllocPerCall = show_alloc_per_call(Fields),
    (
        ShowAllocPerCall = yes,
        AllocPerCallSelfHTML =
            string.format("<TD CLASS=alloc ALIGN=RIGHT>%s</TD>\n",
                [s(count_per_call(OwnAllocs, Calls))]),
        AllocPerCallTotalHTML =
            string.format("<TD CLASS=alloc ALIGN=RIGHT>%s</TD>\n",
                [s(count_per_call(TotalAllocs, Calls))])
    ;
        ShowAllocPerCall = no,
        AllocPerCallSelfHTML = "",
        AllocPerCallTotalHTML = ""
    ),

    ShowMemory = show_memory(Fields),
    (
        ShowMemory = yes(Unit),
        (
            Unit = units_words,
            OwnMemory = OwnWords,
            TotalMemory = TotalWords
        ;
            Unit = units_bytes,
            WordSize = Deep ^ profile_stats ^ word_size,
            OwnMemory = OwnWords * WordSize,
            TotalMemory = TotalWords * WordSize
        )
    ;
        ShowMemory = no,
        % These values won't be used.
        OwnMemory = 0,
        TotalMemory = 0
    ),
    (
        ShowMemory = yes(_),
        MemorySelfHTML =
            string.format("<TD CLASS=memory ALIGN=RIGHT>%s</TD>\n",
                [s(commas(OwnMemory))]) ++
            string.format("<TD CLASS=memory ALIGN=RIGHT>%s</TD>\n",
                [s(OwnMemoryProp)]),
        MemoryTotalHTML =
            string.format("<TD CLASS=memory ALIGN=RIGHT>%s</TD>\n",
                [s(commas(TotalMemory))]) ++
            string.format("<TD CLASS=memory ALIGN=RIGHT>%s</TD>\n",
                [s(TotalMemoryProp)])
    ;
        ShowMemory = no,
        MemorySelfHTML = "",
        MemoryTotalHTML = ""
    ),
    ShowMemoryPerCall = show_memory_per_call(Fields),
    (
        ShowMemoryPerCall = yes(_),
        MemoryPerCallSelfHTML =
            string.format("<TD CLASS=memory ALIGN=RIGHT>%s</TD>\n",
                [s(count_per_call(OwnMemory, Calls))]),
        MemoryPerCallTotalHTML =
            string.format("<TD CLASS=memory ALIGN=RIGHT>%s</TD>\n",
                [s(count_per_call(TotalMemory, Calls))])
    ;
        ShowMemoryPerCall = no,
        MemoryPerCallSelfHTML = "",
        MemoryPerCallTotalHTML = ""
    ),

    (
        TotalsDisp = totals_meaningful,
        HTML =
            PortHTML ++

            QuantaSelfHTML ++
            TimeSelfHTML ++
            QuantaPropSelfHTML ++
            TimePerCallSelfHTML ++
            QuantaTotalHTML ++
            TimeTotalHTML ++
            QuantaPropTotalHTML ++
            TimePerCallTotalHTML ++

            CallSeqsSelfHTML ++
            CallSeqsPropSelfHTML ++
            CallSeqsPerCallSelfHTML ++
            CallSeqsTotalHTML ++
            CallSeqsPropTotalHTML ++
            CallSeqsPerCallTotalHTML ++

            AllocSelfHTML ++
            AllocPerCallSelfHTML ++
            AllocTotalHTML ++
            AllocPerCallTotalHTML ++

            MemorySelfHTML ++
            MemoryPerCallSelfHTML ++
            MemoryTotalHTML ++
            MemoryPerCallTotalHTML
    ;
        TotalsDisp = totals_not_meaningful,
        HTML =
            PortHTML ++

            QuantaSelfHTML ++
            TimeSelfHTML ++
            QuantaPropSelfHTML ++
            TimePerCallSelfHTML ++

            CallSeqsSelfHTML ++
            CallSeqsPropSelfHTML ++
            CallSeqsPerCallSelfHTML ++

            AllocSelfHTML ++
            AllocPerCallSelfHTML ++

            MemorySelfHTML ++
            MemoryPerCallSelfHTML
    ).

%-----------------------------------------------------------------------------%

:- func overall_time(preferences, deep, int) = string.

overall_time(Pref, Deep, Quanta) = TimeStr :-
    lookup_ticks_per_sec(Deep ^ profile_stats, TicksPerSec, _Assumed),
    % We display Time as seconds, with two digits after the decimal point.
    % This is the most we can do, given clock granularity.
    Time = float(Quanta) / float(TicksPerSec),
    TimeStr = format_time(Pref, Time).

:- func per_call_time(preferences, deep, int, int) = string.

per_call_time(Pref, Deep, Quanta, Calls) = TimeStr :-
    lookup_ticks_per_sec(Deep ^ profile_stats, TicksPerSec, _Assumed),
    % We display Time as seconds, with two digits after the decimal point.
    % This is the most we can do, given clock granularity.
    Time = float(Quanta) / float(TicksPerSec),
    ( Calls \= 0 ->
        TimePerCall = Time / float(Calls)
    ;
        TimePerCall = 0.0
    ),
    TimeStr = format_time(Pref, TimePerCall).

:- func format_time(preferences, float) = string.

format_time(Pref, Time) = TimeStr :-
    (
        Pref ^ pref_time = no_scale,
        TimeStr0 = four_decimal_fraction(Time),
        Unit = "s"
    ;
        Pref ^ pref_time = scale_by_millions,
        ( Time >= 0.001 ->
            ScaledTime = Time,
            Unit = "s"
        ;
            ScaledTime = 1000000.0 * Time,
            Unit = "us"
        ),
        TimeStr0 = four_decimal_fraction(ScaledTime)
    ;
        Pref ^ pref_time = scale_by_thousands,
        ( Time >= 1.0 ->
            ScaledTime = Time,
            Unit = "s"
        ; Time >= 0.001 ->
            ScaledTime = 1000.0 * Time,
            Unit = "ms"
        ; Time >= 0.000001 ->
            ScaledTime = 1000000.0 * Time,
            Unit = "us"
        ;
            ScaledTime = 1000000000.0 * Time,
            Unit = "ns"
        ),
        TimeStr0 = two_decimal_fraction(ScaledTime)
    ),
    TimeStr = TimeStr0 ++ Unit.

:- func one_decimal_fraction(float) = string.

one_decimal_fraction(Measure) = decimal_fraction("%.1f", Measure).

:- func two_decimal_fraction(float) = string.

two_decimal_fraction(Measure) = decimal_fraction("%.2f", Measure).

:- func four_decimal_fraction(float) = string.

four_decimal_fraction(Measure) = decimal_fraction("%.4f", Measure).

:- func percentage(int, int) = string.

percentage(Fraction, Whole) = PercentageStr :-
    ( Whole = 0 ->
        PercentageStr = "N/A"
    ;
        Percentage = 100.0 * float(Fraction) / float(Whole),
        PercentageStr = string.format("%5.2f", [f(Percentage)])
    ).

lookup_ticks_per_sec(Stats, TicksPerSec, Assumed) :-
    TicksPerSec0 = Stats ^ ticks_per_sec,
    ( TicksPerSec0 = 0 ->
        TicksPerSec = default_ticks_per_sec,
        Assumed = yes
    ;
        TicksPerSec = TicksPerSec0,
        Assumed = no
    ).

    % The number of ticks per sec to assume if the profiling data file does
    % not record the actual tick rate.
    %
:- func default_ticks_per_sec = int.

default_ticks_per_sec = 100.

%-----------------------------------------------------------------------------%

:- func count_per_call(int, int) = string.

count_per_call(Count, Calls) =
    ( Calls = 0 ->
        two_decimal_fraction(0.0)
    ;
        two_decimal_fraction(float(Count) / float(Calls))
    ).

%-----------------------------------------------------------------------------%

:- func show_port_counts(fields) = bool.

show_port_counts(Fields) = ShowPorts :-
    PortFields = Fields ^ port_fields,
    ( PortFields = no_port, ShowPorts = no
    ; PortFields = port, ShowPorts = yes
    ).

:- func show_quanta(fields) = bool.

show_quanta(Fields) = ShowQuanta :-
    TimeFields = Fields ^ time_fields,
    ( TimeFields = no_time, ShowQuanta = no
    ; TimeFields = ticks, ShowQuanta = yes
    ; TimeFields = time, ShowQuanta = no
    ; TimeFields = ticks_and_time, ShowQuanta = yes
    ; TimeFields = time_and_percall, ShowQuanta = no
    ; TimeFields = ticks_and_time_and_percall, ShowQuanta = yes
    ).

:- func show_times(fields) = bool.

show_times(Fields) = ShowTimes :-
    TimeFields = Fields ^ time_fields,
    ( TimeFields = no_time, ShowTimes = no
    ; TimeFields = ticks, ShowTimes = no
    ; TimeFields = time, ShowTimes = yes
    ; TimeFields = ticks_and_time, ShowTimes = yes
    ; TimeFields = time_and_percall, ShowTimes = yes
    ; TimeFields = ticks_and_time_and_percall, ShowTimes = yes
    ).

:- func show_times_per_call(fields) = bool.

show_times_per_call(Fields) = ShowTimesPerCall :-
    TimeFields = Fields ^ time_fields,
    ( TimeFields = no_time, ShowTimesPerCall = no
    ; TimeFields = ticks, ShowTimesPerCall = no
    ; TimeFields = time, ShowTimesPerCall = no
    ; TimeFields = ticks_and_time, ShowTimesPerCall = no
    ; TimeFields = time_and_percall, ShowTimesPerCall = yes
    ; TimeFields = ticks_and_time_and_percall, ShowTimesPerCall = yes
    ).

:- func show_callseqs(fields) = bool.

show_callseqs(Fields) = ShowCallSeqs :-
    CallSeqsField = Fields ^ callseqs_fields,
    ( CallSeqsField = no_callseqs, ShowCallSeqs = no
    ; CallSeqsField = callseqs, ShowCallSeqs = yes
    ; CallSeqsField = callseqs_and_percall, ShowCallSeqs = yes
    ).

:- func show_callseqs_per_call(fields) = bool.

show_callseqs_per_call(Fields) = ShowCallSeqsPerCall :-
    CallSeqsField = Fields ^ callseqs_fields,
    ( CallSeqsField = no_callseqs, ShowCallSeqsPerCall = no
    ; CallSeqsField = callseqs, ShowCallSeqsPerCall = no
    ; CallSeqsField = callseqs_and_percall, ShowCallSeqsPerCall = yes
    ).

:- func show_alloc(fields) = bool.

show_alloc(Fields) = ShowAlloc :-
    AllocFields = Fields ^ alloc_fields,
    ( AllocFields = no_alloc, ShowAlloc = no
    ; AllocFields = alloc, ShowAlloc = yes
    ; AllocFields = alloc_and_percall, ShowAlloc = yes
    ).

:- func show_alloc_per_call(fields) = bool.

show_alloc_per_call(Fields) = ShowPerAlloc :-
    AllocFields = Fields ^ alloc_fields,
    ( AllocFields = no_alloc, ShowPerAlloc = no
    ; AllocFields = alloc, ShowPerAlloc = no
    ; AllocFields = alloc_and_percall, ShowPerAlloc = yes
    ).

:- func show_memory(fields) = maybe(memory_units).

show_memory(Fields) = ShowMemory :-
    MemoryFields = Fields ^ memory_fields,
    ( MemoryFields = no_memory, ShowMemory = no
    ; MemoryFields = memory(Unit), ShowMemory = yes(Unit)
    ; MemoryFields = memory_and_percall(Unit), ShowMemory = yes(Unit)
    ).

:- func show_memory_per_call(fields) = maybe(memory_units).

show_memory_per_call(Fields) = ShowPerMemory :-
    MemoryFields = Fields ^ memory_fields,
    ( MemoryFields = no_memory, ShowPerMemory = no
    ; MemoryFields = memory(_Unit), ShowPerMemory = no
    ; MemoryFields = memory_and_percall(Unit), ShowPerMemory = yes(Unit)
    ).

%-----------------------------------------------------------------------------%

proc_dynamic_name(Deep, PDPtr) = Name :-
    deep_lookup_proc_dynamics(Deep, PDPtr, PD),
    PSPtr = PD ^ pd_proc_static,
    deep_lookup_proc_statics(Deep, PSPtr, PS),
    Name = PS ^ ps_refined_id.

proc_static_name(Deep, PSPtr) = Name :-
    deep_lookup_proc_statics(Deep, PSPtr, PS),
    Name = PS ^ ps_refined_id.

%-----------------------------------------------------------------------------%

proc_dynamic_context(Deep, PDPtr, FileName, LineNumber) :-
    deep_lookup_proc_dynamics(Deep, PDPtr, PD),
    PSPtr = PD ^ pd_proc_static,
    deep_lookup_proc_statics(Deep, PSPtr, PS),
    FileName = PS ^ ps_file_name,
    LineNumber = PS ^ ps_line_number.

proc_static_context(Deep, PSPtr, FileName, LineNumber) :-
    deep_lookup_proc_statics(Deep, PSPtr, PS),
    FileName = PS ^ ps_file_name,
    LineNumber = PS ^ ps_line_number.

call_site_context(Deep, CSSPtr, FileName, LineNumber) :-
    deep_lookup_call_site_statics(Deep, CSSPtr, CSS),
    CSS = call_site_static(PSPtr, _SlotNum, _Kind, LineNumber, _GoalPath),
    deep_lookup_proc_statics(Deep, PSPtr, PS),
    FileName = PS ^ ps_file_name.

%-----------------------------------------------------------------------------%

proc_static_to_line_group_info(Pref, Deep, PSPtr, FileName, LineNumber,
        Name, HTML) :-
    ( valid_proc_static_ptr(Deep, PSPtr) ->
        deep_lookup_proc_statics(Deep, PSPtr, PS),
        FileName = PS ^ ps_file_name,
        LineNumber = PS ^ ps_line_number,
        Name = PS ^ ps_refined_id,
        HTML = proc_static_to_html_ref(Pref, Deep, PSPtr)
    ;
        FileName = "",
        LineNumber = 0,
        Name = "mercury_runtime",
        HTML = Name
    ).

proc_static_to_html_ref(Pref, Deep, PSPtr) = HTML :-
    PSPtr = proc_static_ptr(PSI),
    URL = deep_cmd_pref_to_url(Pref, Deep, deep_cmd_proc(PSI)),
    deep_lookup_proc_statics(Deep, PSPtr, PS),
    ProcName = PS ^ ps_refined_id,
    HTML = string.format("<A HREF=""%s"">%s</A>",
        [s(URL), s(escape_break_html_string(ProcName))]).

module_name_to_html_ref(Pref, Deep, ModuleName) = HTML :-
    URL = deep_cmd_pref_to_url(Pref, Deep, deep_cmd_module(ModuleName)),
    HTML = string.format("<A HREF=""%s"">%s</A>",
        [s(URL), s(escape_break_html_string(ModuleName))]).

clique_ptr_to_html_ref(Pref, Deep, ProcName, CliquePtr) = HTML :-
    CliquePtr = clique_ptr(CliqueNum),
    URL = deep_cmd_pref_to_url(Pref, Deep, deep_cmd_clique(CliqueNum)),
    HTML = string.format("<A HREF=""%s"">%s</A>",
        [s(URL), s(escape_break_html_string(ProcName))]).

deep_cmd_pref_to_url(Pref, Deep, Cmd) =
    machine_datafile_cmd_pref_to_url(Deep ^ server_name_port,
        Deep ^ script_name, Deep ^ data_file_name, Cmd, Pref).

%-----------------------------------------------------------------------------%

plural(N) = Plural :-
    ( N = 1 ->
        Plural = ""
    ;
        Plural = "s"
    ).

%-----------------------------------------------------------------------------%

escape_html_string(String) =
    replace_special_chars(special_html_char, String).

escape_break_html_string(String) =
    replace_special_chars(special_html_char_or_break, String).

:- func replace_special_chars(pred(char, string)::in(pred(in, out) is semidet),
    string::in) = (string::out) is det.

replace_special_chars(SpecialCharTable, String0) = String :-
    string.foldr(replace_special_char_2(SpecialCharTable), String0, [], Chars),
    string.from_char_list(Chars, String).

:- pred replace_special_char_2(pred(char, string)::in(pred(in, out) is semidet),
    char::in, list(char)::in, list(char)::out) is det.

replace_special_char_2(SpecialCharTable, Char, !Acc) :-
    ( SpecialCharTable(Char, String) ->
        string.to_char_list(String, Chars),
        list.append(Chars, !Acc)
    ;
        list.cons(Char, !Acc)
    ).

:- pred special_html_char(char::in, string::out) is semidet.

special_html_char('&', "&amp;").
special_html_char('<', "&lt;").
special_html_char('>', "&gt;").
special_html_char('''', "&apos;").
special_html_char('"', "&quot;").

    % In addition to escaping special HTML characters, insert zero-width space
    % characters to suggest where the browser may split up long sequences of
    % characters over multiple lines.
    %
:- pred special_html_char_or_break(char::in, string::out) is semidet.

special_html_char_or_break('&', "&amp;").
special_html_char_or_break('<', "&lt;").
special_html_char_or_break('>', "&gt;").
special_html_char_or_break('''', "&apos;").
special_html_char_or_break('"', "&quot;").
special_html_char_or_break('.', "." ++ zero_width_space).
special_html_char_or_break('_', "_" ++ zero_width_space).
special_html_char_or_break('/', "/" ++ zero_width_space).
special_html_char_or_break(':', ":" ++ zero_width_space).

    % U+8203 is the Unicode 'ZERO WIDTH SPACE' character.  It is supported
    % by modern browsers, but tends to break search as the invisible character
    % is not ignored when searching.
    %
    % The <WBR> tag is non-standard but doesn't break search in Firefox (at
    % least).
    %
:- func zero_width_space = string.

% zero_width_space = "&#8203;".
zero_width_space = "<wbr />".

%-----------------------------------------------------------------------------%

:- func machine_datafile_cmd_pref_to_url(string, string, string, cmd,
    preferences) = string.

machine_datafile_cmd_pref_to_url(Machine, ScriptName, DataFileName, Cmd,
        Preferences) =
    "http://" ++
    Machine ++
    ScriptName ++ "?" ++
    cmd_to_string(Cmd) ++
    string.char_to_string(query_separator_char) ++
    preferences_to_string(Preferences) ++
    string.char_to_string(query_separator_char) ++
    DataFileName.

%-----------------------------------------------------------------------------%
:- end_module html_format.
%-----------------------------------------------------------------------------%
