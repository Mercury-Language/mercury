%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997, 2006 The University of Melbourne.
% Copyright (C) 2014, 2018, 2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% File: html.m.
% Author: fjh.
%
% This module provides a strongly-typed, quite declarative method
% for representing and outputting arbitrary HTML text.
% It is intended for use in CGI scripts.
%
% Basically all the predicates here are wrappers around io.write_string.
% However, the types defined here let you indicate the structure of your
% HTML text in the structure of the Mercury terms used to represent it.
%
%-----------------------------------------------------------------------------%

:- module html.
:- interface.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module pair.
:- import_module string.

%-----------------------------------------------------------------------------%

:- type html
    --->    html(
                header,
                body
            ).

:- type header == list(header_item).

:- type header_item
    --->    title(markup)
    ;       header_item(string).   % String can contain any HTML markup.
                                   % This is a general "catch-all" for
                                   % anything not covered by the above
                                   % cases.
:- type body == markup.

    % XXX add anchors
:- type markup
    --->    heading(int, markup)
    ;       style(style, markup)    % a.k.a. logical style
    ;       font(font, markup)      % a.k.a. physical style
    ;       text(string)
    ;       definition_list(list(pair(markup)))
    ;       list(list_type, list(markup))
    ;       form(string, markup)            % actionURL, form contents
    ;       field(string, field)            % name, field type
    ;       address(markup)
    ;       np      % new paragraph
    ;       br      % line break
    ;       hr      % horizontal_rule
    ;       markup(string)          % String can contain any HTML markup.
                                    % This is a general "catch-all" for
                                    % anything not covered by the above
                                    % cases.
    ;       ','(markup, markup).

:- type list_type
    --->    ordered
    ;       unordered
    ;       menu
    ;       directory.

:- type style
    --->    emph
    ;       strong
    ;       samp
    ;       code
    ;       keyboard
    ;       cite
    ;       var.

:- type font
    --->    italics
    ;       bold
    ;       underline
    ;       typewriter.     % typewriter fixed-width font

    % XXX add maps
:- type field
    --->    text(
                int,            % size (display width in characters)
                int,            % maxlength
                string          % initial (default) value
            )

    ;       password(
                int,            % size
                int,            % maxlength
                string          % initial (default) value
            )

    ;       textarea(
                    int, int,       % rows, columns
                    string          % initial (default) value
            )

    ;       checkbox(
                    bool,           % initial (default) value
                    string          % value sent, if checkbox set
            )

    ;       radio(
                    bool,           % initial (default) value
                    string          % value sent, if button set
            )

    ;       select(
                    int,            % size,
                    bool,           % allow multiple selections?
                    list(pair(
                            string, % selection text
                            bool    % selected?
                    ))
            )

    ;       submit(
                    string          % text on the pushbutton
            )

    ;       reset(

                    string          % text on the pushbutton
            )

    ;       hidden(
                    string          % value
            ).

:- pred output_content_type_html(io::di, io::uo) is det.

:- pred output_html(html::in, io::di, io::uo) is det.

:- pred output_header(header::in, io::di, io::uo) is det.

:- pred output_header_item(header_item::in, io::di, io::uo) is det.

:- pred output_body(body::in, io::di, io::uo) is det.

:- pred output_markup(markup::in, io::di, io::uo) is det.

:- pred output_field(string::in, field::in, io::di, io::uo) is det.

:- pred output_form_start(string::in, io::di, io::uo) is det.

:- pred output_form_end(io::di, io::uo) is det.

    % Convert any special characters in a HTML markup string into
    % appropriate HTML escapes.
    %
:- func escape_html_string(string) = string.
:- pred escape_html_string(string::in, string::out) is det.

    % Convert any special characters in a HTML attribute value string into
    % appropriate HTML escapes.
    %
:- func escape_attr_string(string) = string.
:- pred escape_attr_string(string::in, string::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.

%-----------------------------------------------------------------------------%

:- func list_type_name(list_type) = string.

list_type_name(ordered) = "ol".
list_type_name(unordered) = "ul".
list_type_name(menu) = "menu".
list_type_name(directory) = "dir".

:- func style_name(style) = string.

style_name(emph) = "em".
style_name(strong) = "strong".
style_name(samp) = "samp".
style_name(code) = "code".
style_name(keyboard) = "kbd".
style_name(cite) = "cite".
style_name(var) = "var".

:- func font_name(font) = string.

font_name(italics) = "it".
font_name(bold) = "b".
font_name(underline) = "u".
font_name(typewriter) = "tt".

%-----------------------------------------------------------------------------%

output_content_type_html(!IO) :-
    io.write_string("Content-type: text/html\n\n", !IO).

output_html(html(Head, Body), !IO) :-
    output_header(Head, !IO),
    io.nl(!IO),
    output_body(Body, !IO).

output_header(HeaderItems, !IO) :-
    output_markup_scope("head",
        output_list(output_header_item, HeaderItems), !IO).

output_header_item(title(Title), !IO) :-
    output_markup_scope("title", output_markup(Title), !IO).
output_header_item(header_item(Markup), !IO):-
    io.write_string(Markup, !IO).

output_body(Body, !IO) :-
    output_markup_scope("body", output_markup(Body), !IO).

output_markup((Markup1, Markup2), !IO) :-
    output_markup(Markup1, !IO),
    output_markup(Markup2, !IO).
output_markup(address(Address), !IO) :-
    output_markup_scope("address", output_markup(Address), !IO).
output_markup(heading(Level, Heading), !IO) :-
    io.format("<h%d>", [i(Level)], !IO),
    output_markup(Heading, !IO),
    io.format("</h%d>\n", [i(Level)], !IO).
output_markup(definition_list(Definitions), !IO) :-
    output_markup_scope("dl",
        output_list(output_definition, Definitions), !IO).
output_markup(list(ListType, Items), !IO) :-
    output_markup_scope(list_type_name(ListType),
        output_list(output_list_item, Items), !IO).
output_markup(style(Style, Markup), !IO) :-
    output_markup_scope(style_name(Style),
        output_markup(Markup), !IO).
output_markup(font(Font, Markup), !IO) :-
    output_markup_scope(font_name(Font),
        output_markup(Markup), !IO).
output_markup(text(Text), !IO) :-
    io.write_string(escape_html_string(Text), !IO).
output_markup(form(ActionURL, Markup), !IO) :-
    output_form_start(ActionURL, !IO),
    output_markup(Markup, !IO),
    output_form_end(!IO).
output_markup(field(Name, Field), !IO):-
    output_field(Name, Field, !IO).
output_markup(markup(String), !IO):-
    io.write_string(String, !IO).
output_markup(np, !IO) :-
    io.write_string("<p>\n", !IO).
output_markup(br, !IO) :-
    io.write_string("<br>\n", !IO).
output_markup(hr, !IO) :-
    io.write_string("<hr>\n", !IO).

:- pred output_definition(pair(markup)::in, io::di, io::uo) is det.

output_definition(Item - Description, !IO) :-
    io.write_string("<dt> ", !IO),
    output_markup(Item, !IO),
    io.nl(!IO),
    io.write_string("<dd> ", !IO),
    output_markup(Description, !IO),
    io.nl(!IO).

:- pred output_list_item(markup::in, io::di, io::uo) is det.

output_list_item(Item, !IO) :-
    io.write_string("<li> ", !IO),
    output_markup(Item, !IO),
    io.nl(!IO).

output_form_start(ActionURL, !IO) :-
    io.format("<form action=""%s"" method=""POST"">\n",
        [s(escape_attr_string(ActionURL))], !IO).

output_form_end(!IO) :-
    io.write_string("</form>\n", !IO).

output_field(Name, text(Size, MaxLength, Value), !IO) :-
    io.format(
        "<input name=""%s"" type=""text"" size=""%d"" maxlength=""%d"" value=""%s"">",
        [s(Name), i(Size), i(MaxLength), s(escape_attr_string(Value))], !IO).
output_field(Name, password(Size, MaxLength, Value), !IO) :-
    io.format(
        "<input name=""%s"" type=""password"" size=""%d"" maxlength=""%d"" value=""%s"">",
        [s(Name), i(Size), i(MaxLength), s(escape_attr_string(Value))], !IO).
output_field(Name, textarea(Rows, Columns, Value), !IO) :-
    io.format("<textarea name=""%s"" rows=""%d"" cols=""%d"">",
        [s(Name), i(Rows), i(Columns)], !IO),
    io.write_string(escape_html_string(Value), !IO),
    io.write_string("</textarea>", !IO).
output_field(Name, checkbox(Checked, Value), !IO) :-
    io.format("<input name=""%s"" type=""checkbox"" value=""%s""",
        [s(Name), s(escape_attr_string(Value))], !IO),
    (
        Checked = yes,
        io.write_string(" checked", !IO)
    ;
        Checked = no
    ),
    io.write_string(">", !IO).
output_field(Name, radio(Checked, Value), !IO) :-
    io.format("<input name=""%s"" type=""radio"" value=""%s""",
        [s(Name), s(escape_attr_string(Value))], !IO),
    (
        Checked = yes,
        io.write_string(" checked", !IO)
    ;
        Checked = no
    ),
    io.write_string(">", !IO).
output_field(Name, select(Size, Multiple, Options), !IO) :-
    io.format("<select name=""%s"" size=""%d"" ", [s(Name), i(Size)], !IO),
    (
        Multiple = yes,
        io.write_string("multiple", !IO)
    ;
        Multiple = no
    ),
    io.write_string(">\n", !IO),
    output_list(output_selection_option, Options, !IO),
    io.write_string("</select>", !IO).
output_field(Name, submit(Value), !IO) :-
    io.format("<input name=""%s"" type=""submit"" value=""%s"">",
        [s(Name), s(escape_attr_string(Value))], !IO).
output_field(Name, reset(Value), !IO) :-
    io.format("<input name=""%s"" type=""reset"" value=""%s"">",
        [s(Name), s(escape_attr_string(Value))], !IO).
output_field(Name, hidden(Value), !IO) :-
    io.format("<input name=""=%s"" type=""hidden"" value=""%s"">",
        [s(Name), s(escape_attr_string(Value))], !IO).

:- pred output_selection_option(pair(string, bool)::in, io::di, io::uo)
    is det.

output_selection_option(Text - Selected, !IO) :-
    (
        Selected = yes,
        io.write_string("<option selected>", !IO)
    ;
        Selected = no,
        io.write_string("<option>", !IO)
    ),
    io.write_string(escape_html_string(Text), !IO),
    io.nl(!IO).

%-----------------------------------------------------------------------------%

:- pred output_markup_scope(string::in, pred(io, io)::in(pred(di, uo) is det),
    io::di, io::uo) is det.

output_markup_scope(Name, OutputBody, !IO) :-
    io.format("<%s>\n", [s(Name)], !IO),
    OutputBody(!IO),
    io.format("</%s>\n", [s(Name)], !IO).

:- pred output_list(pred(T, io, io)::in(pred(in, di, uo) is det), list(T)::in,
    io::di, io::uo) is det.

output_list(Pred, List, !IO) :-
    list.foldl(Pred, List, !IO).

%-----------------------------------------------------------------------------%

escape_html_string(S) = ES :-
    escape_html_string(S, ES).

escape_html_string(String, EscapedString) :-
    string.to_char_list(String, Chars),
    escape_html_chars(Chars, EscapedChars, []),
    string.from_char_list(EscapedChars, EscapedString).

:- pred escape_html_chars(list(char)::in, list(char)::out, list(char)::in)
    is det.

escape_html_chars([]) --> [].
escape_html_chars([Char|Chars]) -->
    escape_html_char(Char),
    escape_html_chars(Chars).

:- pred escape_html_char(char::in, list(char)::out, list(char)::in) is det.

escape_html_char(Char) -->
    ( if { special_html_char(Char, String) } then
        { string.to_char_list(String, Chars) },
        insert(Chars)
    else
        [Char]
    ).

escape_attr_string(S) = ES :-
    escape_attr_string(S, ES).

escape_attr_string(String, EscapedString) :-
    string.to_char_list(String, Chars),
    escape_attr_chars(Chars, EscapedChars, []),
    string.from_char_list(EscapedChars, EscapedString).

:- pred escape_attr_chars(list(char)::in, list(char)::out, list(char)::in)
    is det.

escape_attr_chars([]) --> [].
escape_attr_chars([Char | Chars]) -->
    escape_attr_char(Char),
    escape_attr_chars(Chars).

:- pred escape_attr_char(char::in, list(char)::out, list(char)::in) is det.

escape_attr_char(Char) -->
    ( if { special_attr_char(Char, String) } then
        { string.to_char_list(String, Chars) },
        insert(Chars)
    else
        [Char]
    ).

:- pred special_html_char(char::in, string::out) is semidet.

special_html_char('&',"&amp;").
special_html_char('<',"&lt;").
special_html_char('>',"&gt;").

:- pred special_attr_char(char::in, string::out) is semidet.

special_attr_char('&',"&amp;").
special_attr_char('>',"&gt;"). % needed only for broken browsers
special_attr_char('\t',"&#09;").
special_attr_char('\r',"&#10;").
special_attr_char('\n',"&#12;").
special_attr_char(' ',"&#32;").
special_attr_char('"',"&#34;").

:- pred insert(list(T), list(T), list(T)).
:- mode insert(in, out, in) is det.

insert(NewChars, Chars, Chars0) :-
    list.append(NewChars, Chars0, Chars).

%-----------------------------------------------------------------------------%
:- end_module html.
%-----------------------------------------------------------------------------%
