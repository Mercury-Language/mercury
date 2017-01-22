%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2002, 2004-2011 The University of Melbourne.
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
% strings and then a single string just before being given to the browser.
%
%-----------------------------------------------------------------------------%

:- module html_format.
:- interface.

:- import_module profile.
:- import_module query.
:- import_module display.

:- import_module cord.

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

    % Convert any special characters in a string into appropriate HTML
    % escapes.
    %
:- func escape_html_string(string) = string.
:- func escape_html_attr_string(attr_string) = string.

    % Like escape_html_string, but additionally inserts zero-width space
    % characters to suggest where long strings can be broken over multiple
    % lines.
    %
:- func escape_break_html_string(string) = string.
:- func escape_break_html_attr_string(attr_string) = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module list.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.

:- import_module measurement_units.

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

    FormatInfo = init_format_info(Deep, Prefs),
    StyleControlMap0 = default_style_control_map,
    map_join_html(item_to_html("<div>\n", "</div>\n", FormatInfo),
        StyleControlMap0, StyleControlMap, Items, ItemsHTML),
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
    StartFragmentHTML =
        str_to_html(string.format("\t%s\n\t{\n", [s(ControlName)])),
    EndFragmentHTML = str_to_html("\t}\n"),
    HTML = StartFragmentHTML ++ ElementsHTML ++ EndFragmentHTML.

:- func style_element_to_html(pair(style_element, string)) = html.

style_element_to_html(style_element(ElementName) - Value) =
    str_to_html(string.format("\t\t%s: %s;\n", [s(ElementName), s(Value)])).

%-----------------------------------------------------------------------------%

    % Convert a display item into a HTML snippet.
    %
:- pred item_to_html(string::in, string::in, format_info::in,
    style_control_map::in, style_control_map::out,
    display_item::in, html::out) is det.

item_to_html(StartTag, EndTag, FormatInfo, !StyleControlMap, Item, HTML) :-
    (
        Item = display_heading(Message),
        HTML = wrap_tags(StartTag, EndTag,
            wrap_tags("<h3>", "</h3>\n", str_to_html(Message)))
    ;
        Item = display_text(Message),
        HTML = wrap_tags(StartTag, EndTag, str_to_html(Message))
    ;
        Item = display_paragraph_break,
        HTML = str_to_html("<p>\n")
    ;
        Item = display_link(DeepLink),
        HTML = wrap_tags(StartTag, EndTag,
            link_to_html(FormatInfo, DeepLink))
    ;
        Item = display_pseudo_link(PseudoLink),
        HTML = wrap_tags(StartTag, EndTag,
            pseudo_link_to_html(FormatInfo, PseudoLink))
    ;
        Item = display_table(Table),
        table_to_html(FormatInfo, !StyleControlMap, Table, TableHTML),
        HTML = wrap_tags(StartTag, EndTag, TableHTML)
    ;
        Item = display_list(Class, MaybeTitle, Items),
        DeveloperMode = FormatInfo ^ fi_pref_developer,
        ( if
            % If developer only items are invisible and all the items in the
            % list are developer items, then don't display the list at all.
            DeveloperMode = developer_options_invisible,
            not (
                some [ListItem] (
                    member(ListItem, Items),
                    not ListItem = display_developer(_)
                )
            )
        then
            HTML = empty
        else
            list_to_html(FormatInfo, !StyleControlMap, Class, MaybeTitle,
                Items, TableHTML),
            HTML = wrap_tags(StartTag, EndTag, TableHTML)
        )
    ;
        Item = display_verbatim(Text),
        HTML = wrap_tags(StartTag, EndTag,
            wrap_tags("<pre>", "</pre>", str_to_html(Text)))
    ;
        Item = display_developer(SubItem),
        DeveloperMode = FormatInfo ^ fi_pref_developer,
        (
            DeveloperMode = developer_options_visible,
            item_to_html(StartTag, EndTag, FormatInfo, !StyleControlMap,
                SubItem, HTML)
        ;
            DeveloperMode = developer_options_invisible,
            HTML = empty
        )
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
:- pred table_to_html(format_info::in,
    style_control_map::in, style_control_map::out,
    table::in, html::out) is det.

table_to_html(FormatInfo, !StyleControlMap, Table, HTML) :-
    Table = table(Class, NumColumns, MaybeHeader, BodyRows),

    ClassStr = table_class_to_string(FormatInfo, Class),
    TableStartTag = "<table class=\"" ++ ClassStr ++ "\">\n",
    TableEndTag = "</table>\n",

    % Build a header row.
    (
        MaybeHeader = yes(table_header(THCells)),
        list.foldl6(table_header_num_rows_and_classmap(FormatInfo), THCells,
            one_header_row, THNumRows, 0, _, map.init, ClassMap,
            0, _, set.init, _, !StyleControlMap),
        MaybeClassMap = yes(ClassMap),
        map_join_html(table_header_group_to_html_row_1(FormatInfo, THNumRows),
            !StyleControlMap, THCells, InnerHeaderRowOneHTML),
        HeaderRowOneHTML =
            wrap_tags("<tr>\n", "</tr>\n", InnerHeaderRowOneHTML),
        (
            THNumRows = one_header_row,
            HeaderRowTwoHTML = empty_html
        ;
            THNumRows = two_header_rows,
            map_join_html(table_header_group_to_html_row_2(FormatInfo),
                !StyleControlMap, THCells, InnerHeaderRowTwoHTML),
            HeaderRowTwoHTML =
                wrap_tags("<tr>\n", "</tr>\n", InnerHeaderRowTwoHTML)
        ),
        InnerHeaderRowThree =
            string.format("<td colspan=\"%d\"/>", [i(NumColumns)]),
        HeaderRowThreeHTML =
            wrap_tags("<tr>", "</tr>",  str_to_html(InnerHeaderRowThree)),
        HeaderHTML = HeaderRowOneHTML ++ HeaderRowTwoHTML ++ HeaderRowThreeHTML
    ;
        MaybeHeader = no,
        MaybeClassMap = no,
        HeaderHTML = empty_html
    ),

    % Build the table rows.
    map_join_html(table_row_to_html(FormatInfo, MaybeClassMap, NumColumns),
        !StyleControlMap, BodyRows, BodyRowsHTML),

    % Construct the table.
    WrappedHeaderHTML = wrap_tags("<thead>\n", "</thead>\n", HeaderHTML),
    WrappedBodyHTML = wrap_tags("<tbody>\n", "</tbody>\n", BodyRowsHTML),
    HTML = wrap_tags(TableStartTag, TableEndTag,
        WrappedHeaderHTML ++ WrappedBodyHTML).

%-----------------------------------------------------------------------------%

    % Return the HTML entity for a table header cell.
    %
:- pred table_header_group_to_html_row_1(format_info::in,
    table_header_rows::in, style_control_map::in, style_control_map::out,
    table_header_group::in, html::out) is det.

table_header_group_to_html_row_1(FormatInfo, HeaderNumRows, !StyleControlMap,
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
        ContentsHTML = table_data_to_html(FormatInfo, Title)
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

:- pred table_header_group_to_html_row_2(format_info::in,
    style_control_map::in, style_control_map::out,
    table_header_group::in, html::out) is det.

table_header_group_to_html_row_2(FormatInfo, !StyleControlMap,
        HeaderGroup, HTML) :-
    HeaderGroup = table_header_group(Titles, ColumnClass, _SetStyle),
    (
        Titles = table_header_group_single(_),
        HTML = empty_html
    ;
        Titles = table_header_group_multi(_, SubTitleCells),
        map_join_html(table_data_to_th_html(FormatInfo, ColumnClass),
            !StyleControlMap, SubTitleCells, HTML)
    ).

%-----------------------------------------------------------------------------%

:- pred table_data_to_th_html(format_info::in, table_column_class::in,
    style_control_map::in, style_control_map::out,
    table_data::in, html::out) is det.

table_data_to_th_html(FormatInfo, ColumnClass, !StyleControlMap,
        TableData, HTML) :-
    ColumnClassStr = table_column_class_to_string(ColumnClass),
    TableDataHTML = table_data_to_html(FormatInfo, TableData),
    StartTag = string.format("<th class=\"%s\">", [s(ColumnClassStr)]),
    EndTag = "</th>\n",
    HTML = wrap_tags(StartTag, EndTag, TableDataHTML).

%-----------------------------------------------------------------------------%

    % Determine how many rows the table header requires, and set up a map
    % from column numbers to classes. Update the style control map for
    % table header groups that specify table_set_style.
    %
    % This should be used with list.foldl6.
    %
:- pred table_header_num_rows_and_classmap(format_info::in,
    table_header_group::in, table_header_rows::in, table_header_rows::out,
    int::in, int::out, column_class_map::in, column_class_map::out,
    int::in, int::out, set(string)::in, set(string)::out,
    style_control_map::in, style_control_map::out) is det.

table_header_num_rows_and_classmap(FormatInfo, HeaderGroup, !NumRows,
        !ColumnNumber, !ClassMap, !HeaderGroupNumber, !ColouredClassStrs,
        !StyleControlMap) :-
    HeaderGroup = table_header_group(ColumnTitles, ColumnClass, MaybeColour),
    ColumnClassStr = table_column_class_to_string(ColumnClass),
    (
        ColumnTitles = table_header_group_single(_),
        NumSubCols = 1,
        map.det_insert(!.ColumnNumber, ColumnClassStr, !ClassMap)
    ;
        ColumnTitles = table_header_group_multi(_, SubTitles),
        list.length(SubTitles, NumSubCols),
        !:NumRows = two_header_rows,
        % fold_up is inclusive of the higher number.
        int.fold_up(insert_column_into_classmap(ColumnClassStr),
            !.ColumnNumber, !.ColumnNumber + NumSubCols - 1, !ClassMap)
    ),
    !:ColumnNumber = !.ColumnNumber + NumSubCols,
    (
        MaybeColour = column_do_not_colour,
        Colour = do_not_colour_column_groups
    ;
        MaybeColour = column_colour,
        Colour = colour_column_groups
    ;
        MaybeColour = column_colour_if_pref,
        Colour = FormatInfo ^ fi_pref_colour_scheme
    ),
    (
        Colour = do_not_colour_column_groups
    ;
        Colour = colour_column_groups,
        update_style_control_map(ColumnClassStr, !HeaderGroupNumber,
            !ColouredClassStrs, !StyleControlMap)
    ).

:- pred insert_column_into_classmap(string::in, int::in,
    column_class_map::in, column_class_map::out) is det.

insert_column_into_classmap(Value, Key, !Map) :-
    map.det_insert(Key, Value, !Map).

:- pred update_style_control_map(string::in, int::in, int::out,
    set(string)::in, set(string)::out,
    style_control_map::in, style_control_map::out) is det.

update_style_control_map(ColumnClassStr, !HeaderGroupNumber,
        !ColouredClassStrs, !StyleControlMap) :-
    StyleControl = style_control("td." ++ ColumnClassStr),
    StyleElement = style_element("background"),
    ( if !.HeaderGroupNumber /\ 1 = 0 then
        Colour = "LightGrey"
    else
        Colour = "White"
    ),
    ( if set.member(ColumnClassStr, !.ColouredClassStrs) then
        unexpected($module, $pred, "repeated table_column_class")
    else
        set.insert(ColumnClassStr, !ColouredClassStrs)
    ),
    ( if map.search(!.StyleControlMap, StyleControl, StyleElementMap0) then
        map.set(StyleElement, Colour, StyleElementMap0, StyleElementMap),
        map.det_update(StyleControl, StyleElementMap, !StyleControlMap)
    else
        StyleElementMap = map.singleton(StyleElement, Colour),
        map.det_insert(StyleControl, StyleElementMap, !StyleControlMap)
    ),
    !:HeaderGroupNumber = !.HeaderGroupNumber + 1.

%-----------------------------------------------------------------------------%

    % Build a row of a HTML table from the table_row type.
    %
:- pred table_row_to_html(format_info::in, maybe(column_class_map)::in,
    int::in, style_control_map::in, style_control_map::out,
    table_row::in, html::out) is det.

table_row_to_html(FormatInfo, MaybeColClassMap, NumColumns, !StyleControlMap,
        TableRow, HTML) :-
    (
        TableRow = table_section_header(Contents),
        ContentsHTML = table_data_to_html(FormatInfo, Contents),
        StartTag = string.format("<tr><td colspan=\"%d\">", [i(NumColumns)]),
        EndTag = "</td></tr>\n",
        HTML = wrap_tags(StartTag, EndTag, ContentsHTML)
    ;
        TableRow = table_separator_row,
        Str = string.format("<tr><td colspan=\"%d\"></td></tr>\n",
            [i(NumColumns)]),
        HTML = str_to_html(Str)
    ;
        TableRow = table_row(Cells),
        map_join_html_count(table_cell_to_html(FormatInfo, MaybeColClassMap),
            !StyleControlMap, 0, Cells, InnerHTML),
        HTML = wrap_tags("<tr>\n", "</tr>\n", InnerHTML)
    ;
        TableRow = table_developer_row(RealTableRow),
        Developer = FormatInfo ^ fi_pref_developer,
        (
            Developer = developer_options_visible,
            table_row_to_html(FormatInfo, MaybeColClassMap, NumColumns,
                !StyleControlMap, RealTableRow, HTML)
        ;
            Developer = developer_options_invisible,
            HTML = cord.empty
        )
    ).

%-----------------------------------------------------------------------------%

:- pred table_cell_to_html(format_info::in, maybe(column_class_map)::in,
    style_control_map::in, style_control_map::out,
    int::in, int::out, table_cell::in, html::out) is det.

table_cell_to_html(FormatInfo, MaybeClassMap, !StyleControlMap, !ColumnNum,
        Cell, HTML) :-
    (
        Cell = table_empty_cell,
        !:ColumnNum = !.ColumnNum + 1,
        HTML = str_to_html("<td/>")
    ;
        (
            Cell = table_cell(CellData),
            Span = 1
        ;
            Cell = table_multi_cell(CellData, Span)
        ),
        (
            MaybeClassMap = yes(ClassMap),
            ( if map.search(ClassMap, !.ColumnNum, ColumnClassStrPrime) then
                ColumnClassStr = ColumnClassStrPrime
            else
                Msg = string.format(
                    "Class map had no class for col %d, check table structure",
                    [i(!.ColumnNum)]),
                unexpected($module, $pred, Msg)
            )
        ;
            MaybeClassMap = no,
            ( if table_data_class(CellData, ColumnClassPrime) then
                ColumnClass = ColumnClassPrime
            else
                ColumnClass = default_table_column_class
            ),
            ColumnClassStr = table_column_class_to_string(ColumnClass)
        ),
        CellHTML = table_data_to_html(FormatInfo, CellData),
        ( if Span = 1 then
            SpanStr = ""
        else
            SpanStr = string.format("colspan=%d ", [i(Span)])
        ),
        !:ColumnNum = !.ColumnNum + Span,
        StartTag = string.format("<td %sclass=\"%s\">",
            [s(SpanStr), s(ColumnClassStr)]),
        EndTag = "</td>\n",
        HTML = wrap_tags(StartTag, EndTag, CellHTML)
    ).

%-----------------------------------------------------------------------------%

:- func table_data_to_html(format_info, table_data) = html.

table_data_to_html(_, td_f(Float)) =
    str_to_html(two_decimal_fraction(Float)).
table_data_to_html(_, td_i(Int)) =
    str_to_html(commas(Int)).
table_data_to_html(FormatInfo, td_l(Link)) =
    link_to_html(FormatInfo, Link).
table_data_to_html(_, td_m(Mem, Units, Decimals)) =
    str_to_html(format_memory(Mem, Units, Decimals)).
table_data_to_html(_, td_p(Percent)) =
    str_to_html(format_percent(Percent)).
table_data_to_html(_, td_s(String)) =
    str_to_html(escape_break_html_string(String)).
table_data_to_html(_, td_as(AttrString)) =
    str_to_html(escape_break_html_attr_string(AttrString)).
table_data_to_html(_, td_t(Time)) =
    str_to_html(format_time(Time)).

    % This predicate is used when a table class map couldn't be built from the
    % header of the table (perhaps there was no header). It provides a class
    % for some data that class is used, otherwise the default class is
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
table_column_class_to_string(table_column_class_clique) = "clique".
table_column_class_to_string(table_column_class_callseqs) = "callseqs".
table_column_class_to_string(table_column_class_field_name) = "field_name".
table_column_class_to_string(table_column_class_memory) = "memory".
table_column_class_to_string(table_column_class_module_name) = "module_name".
table_column_class_to_string(table_column_class_number) = "number".
table_column_class_to_string(table_column_class_ordinal_rank) = "ordinal_rank".
table_column_class_to_string(table_column_class_port_counts) = "port_counts".
table_column_class_to_string(table_column_class_proc) = "proc".
table_column_class_to_string(table_column_class_source_context) =
    "source_context".
table_column_class_to_string(table_column_class_ticks_and_times) =
    "ticks_and_times".

:- func table_class_to_string(format_info, table_class) = string.

table_class_to_string(FormatInfo, Class) = ClassStr :-
    (
        Class = table_class_do_not_box,
        ClassStr = "plain"
    ;
        Class = table_class_box,
        ClassStr = "boxed"
    ;
        Class = table_class_box_if_pref,
        BoxPrefs = FormatInfo ^ fi_pref_box,
        (
            BoxPrefs = do_not_box_tables,
            ClassStr = "plain"
        ;
            BoxPrefs = box_tables,
            ClassStr = "boxed"
        )
    ).

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
        ( style_control("td.clique") -
            map.from_assoc_list([
                style_element("text-align")     - "right"
            ])
        ),
        ( style_control("td.field_name") -
            map.from_assoc_list([
                style_element("text-align")     - "left"
            ])
        ),
        ( style_control("td.memory") -
            map.from_assoc_list([
                style_element("text-align")     - "right"
            ])
        ),
        ( style_control("td.module_name") -
            map.from_assoc_list([
                style_element("text-align")     - "left"
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
        ( style_control("td.source_context") -
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

    % Transform a list of items into HTML.
    %
:- pred list_to_html(format_info::in,
    style_control_map::in, style_control_map::out,
    list_class::in, maybe(string)::in, list(display_item)::in, html::out)
    is det.

list_to_html(FormatInfo, !StyleControlMap, Class, MaybeTitle, Items, HTML) :-
    (
        MaybeTitle = yes(Title),
        TitleStartTag = "<span id=\"list_title\">",
        TitleEndTag = "</span>\n",
        TitleHTML = wrap_tags(TitleStartTag, TitleEndTag,
            str_to_html(Title)),
        (
            Class = list_class_horizontal,
            PostTitleHTML = empty_html
        ;
            ( Class = list_class_horizontal_except_title
            ; Class = list_class_vertical_bullets
            ; Class = list_class_vertical_no_bullets
            ),
            PostTitleHTML = str_to_html("<br>\n")
        )
    ;
        MaybeTitle = no,
        TitleHTML = empty_html,
        PostTitleHTML = empty_html
    ),
    (
        ( Class = list_class_horizontal
        ; Class = list_class_horizontal_except_title
        ),
        OutsideStartTag = "",
        OutsideEndTag = "\n",
        InnerStartTag = "",
        InnerEndTag = "\n",
        Separator = str_to_html("")
    ;
        Class = list_class_vertical_no_bullets,
        OutsideStartTag = "",
        OutsideEndTag = "\n",
        InnerStartTag = "",
        InnerEndTag = "\n",
        Separator = str_to_html("<br>\n")
    ;
        Class = list_class_vertical_bullets,
        OutsideStartTag = "<ul>\n",
        OutsideEndTag = "</ul>\n",
        InnerStartTag = "<li>\n",
        InnerEndTag = "</li>\n",
        Separator = empty_html
    ),
    sep_map_join_html(Separator,
        item_to_html(InnerStartTag, InnerEndTag, FormatInfo),
        !StyleControlMap, Items, InnerItemsHTML),
    ItemsHTML = wrap_tags(OutsideStartTag, OutsideEndTag,
        InnerItemsHTML),
    HTML = TitleHTML ++ PostTitleHTML ++ ItemsHTML.

%-----------------------------------------------------------------------------%

    % Transform a deep link into HTML.
    %
:- func link_to_html(format_info, deep_link) = html.

link_to_html(FormatInfo, Link) = HTML :-
    Link = deep_link(Cmd, MaybePrefs, Label, Class),
    deep_cmd_to_url(FormatInfo, Cmd, MaybePrefs, URL),
    (
        Class = link_class_control,
        FormatString = "<a class=\"control\" href=\"%s\">[%s]</a>",
        string.format(FormatString,
            [s(URL), s(escape_break_html_attr_string(Label))], HTMLStr)
    ;
        Class = link_class_link,
        FormatString = "<a class=\"link\" href=\"%s\">%s</a>",
        string.format(FormatString,
            [s(URL), s(escape_break_html_attr_string(Label))], HTMLStr)
    ),
    HTML = str_to_html(HTMLStr).

    % Transform a pseudo link into HTML.
    % We should follow the same logic for decoration as link_to_html.
    %
:- func pseudo_link_to_html(format_info, pseudo_link) = html.

pseudo_link_to_html(_FormatInfo, PseudoLink) = HTML :-
    PseudoLink = pseudo_link(Label, Class),
    (
        Class = link_class_control,
        HTMLStr = "[" ++ escape_break_html_string(Label) ++ "]"
    ;
        Class = link_class_link,
        HTMLStr = escape_break_html_string(Label)
    ),
    HTML = str_to_html(HTMLStr).

%-----------------------------------------------------------------------------%

    % The information we need to create various parts of the HTML.
    %
:- type format_info
    --->    format_info(
                % Information about preferences.
                fi_pref_colour_scheme   :: colour_column_groups,
                fi_pref_box             :: box_tables,
                fi_pref_developer       :: developer_mode,

                % Information about the current HTTP session, which we use
                % to create links.
                fi_server_name_port     :: string,
                fi_script_name          :: string,
                fi_deep_file            :: string
            ).

:- func init_format_info(deep, preferences) = format_info.

init_format_info(Deep, Prefs) = FormatInfo :-
    FormatInfo = format_info(Prefs ^ pref_colour, Prefs ^ pref_box,
        Prefs ^ pref_developer_mode,
        Deep ^ server_name_port, Deep ^ script_name, Deep ^ data_file_name).

%-----------------------------------------------------------------------------%

    % Return a URL for the deep structure and command.
    %
:- pred deep_cmd_to_url(format_info::in, cmd::in, maybe(preferences)::in,
    string::out) is det.

deep_cmd_to_url(FormatInfo, Cmd, MaybePrefs, URL) :-
    HostAndPort = FormatInfo ^ fi_server_name_port,
    Script = FormatInfo ^ fi_script_name,
    DeepFileName = FormatInfo ^ fi_deep_file,
    DeepQuery = deep_query(yes(Cmd), DeepFileName, MaybePrefs),
    string.format("http://%s%s?%s",
        [s(HostAndPort), s(Script), s(query_to_string(DeepQuery))], URL).

%-----------------------------------------------------------------------------%
%
% Generic HTML helper predicates.
%

html_to_string(HTML) = Str :-
    string.append_list(cord.list(HTML), Str).

:- func append_htmls(list(html)) = html.

append_htmls(HTMLs) = cord_list_to_cord(HTMLs).

:- func wrap_tags(string, string, html) = html.

wrap_tags(StartTag, EndTag, InnerHTML) =
    str_to_html(StartTag) ++ InnerHTML ++ str_to_html(EndTag).

:- func empty_html = html.

empty_html = cord.empty.

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
    pred(style_control_map, style_control_map, int, int, A, html)::
        in(pred(in, out, in, out, in, out) is det),
    style_control_map::in, style_control_map::out,
    int::in, list(A)::in, html::out) is det.

map_join_html_count(MapPred, !StyleControlMap, !.ColumnNum, List, HTML) :-
    sep_map_join_html_count(empty_html, MapPred, !StyleControlMap, !.ColumnNum,
        List, HTML).

    % For each A, MapPred(!StyleControlMap, N, A, S), and concatenate all Ss
    % after putting Separator between them.
    % N is the ordinal number of the element in the list.
    %
:- pred sep_map_join_html_count(html::in,
    pred(style_control_map, style_control_map, int, int, A, html)::
        in(pred(in, out, in, out, in, out) is det),
    style_control_map::in, style_control_map::out,
    int::in, list(A)::in, html::out) is det.

sep_map_join_html_count(_, _, !StyleControlMap, _, [], empty_html).
sep_map_join_html_count(Separator, MapPred, !StyleControlMap, !.ColumnNum,
        [Head | Tail], HTML) :-
    MapPred(!StyleControlMap, !ColumnNum, Head, HeadHTML),
    sep_map_join_html_count_acc(Separator, MapPred, !StyleControlMap,
        !.ColumnNum, Tail, HeadHTML, HTML).

:- pred sep_map_join_html_count_acc(html::in,
    pred(style_control_map, style_control_map, int, int, A, html)::
        in(pred(in, out, in, out, in, out) is det),
    style_control_map::in, style_control_map::out,
    int::in, list(A)::in, html::in, html::out) is det.

sep_map_join_html_count_acc(_, _, !StyleControlMap, _, [], !HTML).
sep_map_join_html_count_acc(Separator, MapPred, !StyleControlMap, !.ColumnNum,
        [Head | Tail], !HTML) :-
    MapPred(!StyleControlMap, !ColumnNum, Head, HeadHTML),
    !:HTML = !.HTML ++ Separator ++ HeadHTML,
    sep_map_join_html_count_acc(Separator, MapPred, !StyleControlMap,
        !.ColumnNum, Tail, !HTML).

%-----------------------------------------------------------------------------%

escape_html_string(String) =
    replace_special_chars(special_html_char, String).

escape_html_attr_string(attr_str(Attrs, String)) =
    handle_html_attrs(Attrs, replace_special_chars(special_html_char, String)).

escape_break_html_string(String) =
    replace_special_chars(special_html_char_or_break, String).

escape_break_html_attr_string(attr_str(Attrs, String)) =
    handle_html_attrs(Attrs,
        replace_special_chars(special_html_char_or_break, String)).

:- func handle_html_attrs(list(str_attr), string) = string.

handle_html_attrs([], Str) = Str.
handle_html_attrs([Attr | Attrs], InsideStr) = Str :-
    InnerStr = handle_html_attrs(Attrs, InsideStr),
    (
        Attr = attr_bold,
        Str = "<b>" ++ InnerStr ++ "</b>"
    ;
        Attr = attr_italic,
        Str = "<i>" ++ InnerStr ++ "</i>"
    ;
        Attr = attr_underline,
        Str = "<u>" ++ InnerStr ++ "</u>"
    ).

:- func replace_special_chars(pred(char, string)::in(pred(in, out) is semidet),
    string::in) = (string::out) is det.

replace_special_chars(SpecialCharTable, String0) = String :-
    string.foldr(replace_special_char_2(SpecialCharTable), String0, [], Chars),
    string.from_char_list(Chars, String).

:- pred replace_special_char_2(
    pred(char, string)::in(pred(in, out) is semidet),
    char::in, list(char)::in, list(char)::out) is det.

replace_special_char_2(SpecialCharTable, Char, !Acc) :-
    ( if SpecialCharTable(Char, String) then
        string.to_char_list(String, Chars),
        list.append(Chars, !Acc)
    else
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
:- end_module html_format.
%-----------------------------------------------------------------------------%
