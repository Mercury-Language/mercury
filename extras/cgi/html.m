%-----------------------------------------------------------------------------%
% Copyright (C) 1997, 2006 The University of Melbourne.
% Copyright (C) 2014, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%

% File: html.m.
% Author: fjh.

% This module provides a strongly-typed, quite declarative method
% for representing and outputting arbitrary HTML text. 
% It is intended for use in CGI scripts.
%
% Basically all the predicates here are wrappers around io.write_string.
% However, the types defined here let you indicate the structure of your
% HTML text in the structure of the Mercury terms used to represent it.

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
	--->	html(
			header,
			body
		).

:- type header == list(header_item).

:- type header_item
	--->	title(markup)
	;	header_item(string)	% String can contain any HTML markup.
					% This is a general "catch-all" for
					% anything not covered by the above
					% cases.
	.

:- type body == markup.

	% XXX add anchors
:- type markup
	--->	heading(int, markup)
	;	style(style, markup)	% a.k.a. logical style
	;	font(font, markup)	% a.k.a. physical style
	;	text(string)
	;	definition_list(list(pair(markup)))
	;	list(list_type, list(markup))
	;	form(string, markup)		% actionURL, form contents
	;	field(string, field)		% name, field type
	;	address(markup)
	;	np	% new paragraph
	;	br	% line break
	;	hr	% horizontal_rule
	;	markup(string)		% String can contain any HTML markup.
					% This is a general "catch-all" for
					% anything not covered by the above
					% cases.
	;	','(markup, markup)
	.

:- type list_type
	--->	ordered
	;	unordered
	;	menu
	;	directory
	.

:- type style
	--->	emph
	;	strong
	;	samp
	;	code
	;	keyboard
	;	cite
	;	var
	.

:- type font
	--->	italics
	;	bold
	;	underline
	;	typewriter	% typewriter fixed-width font
	.

	% XXX add maps
:- type field
	--->	text(
			int,		% size (display width in characters)
			int,		% maxlength
			string		% initial (default) value
		)
	;	password(
			int,		% size
			int,		% maxlength
			string		% initial (default) value
		)
	;	textarea(
			int, int,	% rows, columns
			string		% initial (default) value
		)
	;	checkbox(
			bool,		% initial (default) value
			string		% value sent, if checkbox set
		)
	;	radio(
			bool,		% initial (default) value
			string		% value sent, if button set
		)			
	;	select(
			int,		% size,
			bool,		% allow multiple selections?
			list(pair(
				string,	% selection text
				bool	% selected?
			))
		)
	;	submit(
			string		% text on the pushbutton
		)
	;	reset(
			string		% text on the pushbutton
		)
	;	hidden(
			string		% value
		)
	.

:- pred output_content_type_html(state, state).
:- mode output_content_type_html(di, uo) is det.

:- pred output_html(html, state, state).
:- mode output_html(in, di, uo) is det.

:- pred output_header(header, state, state).
:- mode output_header(in, di, uo) is det.

:- pred output_header_item(header_item, state, state).
:- mode output_header_item(in, di, uo) is det.

:- pred output_body(body, state, state).
:- mode output_body(in, di, uo) is det.

:- pred output_markup(markup, state, state).
:- mode output_markup(in, di, uo) is det.

:- pred output_field(string, field, io, io).
:- mode output_field(in, in, di, uo) is det. 

:- pred output_form_start(string::in, io::di, io::uo) is det.
:- pred output_form_end(io::di, io::uo) is det.

% convert any special characters in a HTML markup string into
% appropriate HTML escapes
:- func escape_html_string(string) = string.
:- pred escape_html_string(string::in, string::out) is det.

% convert any special characters in a HTML attribute value string
% into appropriate HTML escapes
:- func escape_attr_string(string) = string.
:- pred escape_attr_string(string::in, string::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module int, char, list.

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

output_content_type_html -->
	write_string("Content-type: text/html\n\n").

output_html(html(Head, Body)) -->
	output_header(Head),
	nl,
	output_body(Body).

output_header(HeaderItems) -->
	output_markup_scope("head",
		output_list(output_header_item, HeaderItems)).

output_header_item(title(Title)) -->
	output_markup_scope("title",
		output_markup(Title)).
output_header_item(header_item(Markup)) -->
	write_string(Markup).

output_body(Body) -->
	output_markup_scope("body",
		output_markup(Body)).

output_markup((Markup1, Markup2)) -->
	output_markup(Markup1),
	output_markup(Markup2).
output_markup(address(Address)) -->
	output_markup_scope("address",
		output_markup(Address)).
output_markup(heading(Level, Heading)) -->
	format("<h%d>", [i(Level)]),
	output_markup(Heading),
	format("</h%d>\n", [i(Level)]).
output_markup(definition_list(Definitions)) -->
	output_markup_scope("dl",
		output_list(output_definition, Definitions)).
output_markup(list(ListType, Items)) -->
	output_markup_scope(list_type_name(ListType),
		output_list(output_list_item, Items)).
output_markup(style(Style, Markup)) -->
	output_markup_scope(style_name(Style),
		output_markup(Markup)).
output_markup(font(Font, Markup)) -->
	output_markup_scope(font_name(Font),
		output_markup(Markup)).
output_markup(text(Text)) -->
	write_string(escape_html_string(Text)).
output_markup(form(ActionURL, Markup)) -->
	output_form_start(ActionURL),
	output_markup(Markup),
	output_form_end.
output_markup(field(Name, Field)) -->
	output_field(Name, Field).
output_markup(markup(String)) -->
	write_string(String).
output_markup(np) -->
	write_string("<p>\n").
output_markup(br) -->
	write_string("<br>\n").
output_markup(hr) -->
	write_string("<hr>\n").

:- pred output_definition(pair(markup), io, io).
:- mode output_definition(in, di, uo) is det.
output_definition(Item - Description) -->
	write_string("<dt> "), output_markup(Item), nl,
	write_string("<dd> "), output_markup(Description), nl.

:- pred output_list_item(markup, io, io).
:- mode output_list_item(in, di, uo) is det.
output_list_item(Item) -->
	write_string("<li> "), output_markup(Item), nl.

output_form_start(ActionURL) -->
	format("<FORM ACTION=""%s"" METHOD=POST>\n",
		[s(escape_attr_string(ActionURL))]).

output_form_end -->
	write_string("</FORM>\n").

output_field(Name, text(Size, MaxLength, Value)) -->
	format("<INPUT NAME=%s TYPE=text SIZE=%d MAXLENGTH=%d VALUE=""%s"">",
		[s(Name), i(Size), i(MaxLength), s(escape_attr_string(Value))]).
output_field(Name, password(Size, MaxLength, Value)) -->
	format(
	"<INPUT NAME=%s TYPE=password SIZE=%d MAXLENGTH=%d VALUE=""%s"">",
		[s(Name), i(Size), i(MaxLength), s(escape_attr_string(Value))]).
output_field(Name, textarea(Rows, Columns, Value)) -->
	format("<TEXTAREA NAME=%s ROWS=%d COLS=%d>",
		[s(Name), i(Rows), i(Columns)]),
	write_string(escape_html_string(Value)),
	write_string("</TEXTAREA>").
output_field(Name, checkbox(Checked, Value)) -->
	format("<INPUT NAME=%s TYPE=checkbox VALUE=""%s""",
		[s(Name), s(escape_attr_string(Value))]),
	( { Checked = yes } ->
		write_string(" CHECKED")
	;
		[]
	),
	write_string(">").
output_field(Name, radio(Checked, Value)) -->
	format("<INPUT NAME=%s TYPE=radio VALUE=""%s""",
		[s(Name), s(escape_attr_string(Value))]),
	( { Checked = yes } ->
		write_string(" CHECKED")
	;
		[]
	),
	write_string(">").
output_field(Name, select(Size, Multiple, Options)) -->
	format("<SELECT NAME=%s SIZE=%d ",
		[s(Name), i(Size)]),
	( { Multiple = yes } ->
		write_string("MULTIPLE")
	;
		[]
	),
	write_string(">\n"),
	output_list(output_selection_option, Options),
	write_string("</SELECT>").
output_field(Name, submit(Value)) -->
	format("<INPUT NAME=%s TYPE=submit VALUE=""%s"">",
		[s(Name), s(escape_attr_string(Value))]).
output_field(Name, reset(Value)) -->
	format("<INPUT NAME=%s TYPE=reset VALUE=""%s"">",
		[s(Name), s(escape_attr_string(Value))]).
output_field(Name, hidden(Value)) -->
	format("<INPUT NAME=%s TYPE=hidden VALUE=""%s"">",
		[s(Name), s(escape_attr_string(Value))]).

:- pred output_selection_option(pair(string, bool), io, state).
:- mode output_selection_option(in, di, uo) is det.

output_selection_option(Text - Selected) -->
	( { Selected = yes } ->
		write_string("<OPTION SELECTED>")
	;
		write_string("<OPTION>")
	),
	write_string(escape_html_string(Text)), nl.

%-----------------------------------------------------------------------------%

:- pred output_markup_scope(string, pred(state, state), state, state).
:- mode output_markup_scope(in, pred(di, uo) is det, di, uo) is det.
output_markup_scope(Name, OutputBody) -->
	format("<%s>\n", [s(Name)]),
	OutputBody,
	format("</%s>\n", [s(Name)]).

:- pred output_list(pred(T, state, state), list(T), io, io).
:- mode output_list(pred(in, di, uo) is det, in, di, uo) is det.
output_list(Pred, List) -->
	foldl(Pred, List).

%-----------------------------------------------------------------------------%

escape_html_string(S) = ES :- escape_html_string(S, ES).

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
	( { special_html_char(Char, String) } ->
		{ string.to_char_list(String, Chars) },
		insert(Chars)
	;
		[Char]
	).

escape_attr_string(S) = ES :- escape_attr_string(S, ES).

escape_attr_string(String, EscapedString) :-
	string.to_char_list(String, Chars),
	escape_attr_chars(Chars, EscapedChars, []),
	string.from_char_list(EscapedChars, EscapedString).

:- pred escape_attr_chars(list(char)::in, list(char)::out, list(char)::in)
	is det.
escape_attr_chars([]) --> [].
escape_attr_chars([Char|Chars]) -->
	escape_attr_char(Char),
	escape_attr_chars(Chars).

:- pred escape_attr_char(char::in, list(char)::out, list(char)::in) is det.
escape_attr_char(Char) -->
	( { special_attr_char(Char, String) } ->
		{ string.to_char_list(String, Chars) },
		insert(Chars)
	;
		[Char]
	).

:- pred special_html_char(char::in, string::out) is semidet.
special_html_char('&',"&amp;").
special_html_char('<',"&lt;").
special_html_char('>',"&gt;").

:- pred special_attr_char(char::in, string::out) is semidet.
special_attr_char('&',"&amp;").
special_attr_char('>',"&gt;").	% needed only for broken browsers
special_attr_char('\t',"&#09;").
special_attr_char('\r',"&#10;").
special_attr_char('\n',"&#12;").
special_attr_char(' ',"&#32;").
special_attr_char('"',"&#34;").

:- pred insert(list(T), list(T), list(T)).
:- mode insert(in, out, in) is det.
insert(NewChars, Chars, Chars0) :-
	list.append(NewChars, Chars0, Chars).

/******
This is junk
	( { char.is_alnum(Char) } ->
		[Char]
	;
		{ char.to_int(Char, Val) },
		{ Hex1 is (Val /\ 0xf0) >> 4 },
		{ Hex2 is Val /\ 0x0f },
		{ char.det_int_to_digit(Hex1, HexChar1) },
		{ char.det_int_to_digit(Hex2, HexChar2) },
		['%', HexChar1, HexChar2]
	).
*******/

%-----------------------------------------------------------------------------%
