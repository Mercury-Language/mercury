%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: cgi.m.
% Author: fjh.

% This module provides a Mercury interface to HTML forms using CGI.
% For documentation on HTML forms and the CGI interface, see
% <http://www.ncsa.uiuc.edu/SDG/Software/Mosaic/Docs/fill-out-forms/
% overview.html>.

% This is intended to conform to versions 1.0 and 1.1 of the
% CGI specification.

%-----------------------------------------------------------------------------%

:- module cgi.
:- interface.
:- import_module io, string, assoc_list, std_util.

% cgi__get_form(MaybeFormEntries):
%	This procedure should be called form within a CGI program
%	that should be invoked with a METHOD of POST.
%	If all goes well, it will return the form entries.
%	If something goes wrong, it will print an appropriate HTML-formatted
%	error message to stdout, call io__set_exit_status(1),
%	and return `no'.
:- pred cgi__get_form(maybe(assoc_list(string, string))::out,
			io__state::di, io__state::uo) is det. 

% cgi__maybe_get_form(MaybeFormEntries):
%	This procedure should be called form within a CGI program
%	that *may* be invoked with a METHOD of POST.
%	If all goes well, it will return the form entries.
%	If not invoked using a METHOD of POST, it will return `no'.
%	If something goes wrong, it will print an appropriate HTML-formatted
%	error message to stdout, call io__set_exit_status(1),
%	and return `no'.
:- pred cgi__maybe_get_form(maybe(assoc_list(string, string))::out,
			io__state::di, io__state::uo) is det. 

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module html, int, char, list.

%-----------------------------------------------------------------------------%

cgi__maybe_get_form(FormEntries) -->
    io__get_environment_var("REQUEST_METHOD", REQUEST_METHOD),
    ( { REQUEST_METHOD \= yes("POST") } ->
	{ FormEntries = no }
    ;
        cgi__get_form_contents(FormEntries)
    ).

cgi__get_form(FormEntries) -->
    io__get_environment_var("REQUEST_METHOD", REQUEST_METHOD),
    ( { REQUEST_METHOD \= yes("POST") } ->
    	cgi__error([
	    "This script should be referenced with a ",
	        "<code>REQUEST_METHOD</code> of <code>POST</code>.\n\n",
	    "If you don't understand this, see this ",
	        "<A HREF=""http://www.ncsa.uiuc.edu/SDG/Software/Mosaic/Docs",
	        "/fill-out-forms/overview.html"">forms overview</A>.\n"
	]),
	{ FormEntries = no }
    ;
        cgi__get_form_contents(FormEntries)
    ).

:- pred cgi__get_form_contents(maybe(assoc_list(string, string))::out,
			io__state::di, io__state::uo) is det. 

cgi__get_form_contents(FormEntries) -->
    io__get_environment_var("CONTENT_TYPE", CONTENT_TYPE),
    io__get_environment_var("CONTENT_LENGTH", CONTENT_LENGTH),
    ( { CONTENT_TYPE \= yes("application/x-www-form-urlencoded") } ->
    	cgi__error([
	    "This script can only be used to decode form results.\n",
	    "It should be referenced with a <code>CONTENT_TYPE</code> of ",
	        "<code>application/x-www-form-urlencoded</code>.\n\n",
	    "If you don't understand this, see this ",
	        "<A HREF=""http://www.ncsa.uiuc.edu/SDG/Software/Mosaic/Docs",
	        "/fill-out-forms/overview.html"">forms overview</A>.\n"
        ]),
	{ FormEntries = no }
    ;
    	{ CONTENT_LENGTH = yes(ContentLengthString) },
    	{ string__to_int(ContentLengthString, ContentLength) },
    	{ ContentLength >= 0 }
    ->
	cgi__get_form_contents(ContentLength, FormEntries)
    ;
    	cgi__error([
	    "Invalid <code>CONTENT_LENGTH</code>.\n",
	    "This may be due to a bug in your WWW browser?\n"
        ]),
	{ FormEntries = no }
    ).

:- pred cgi__get_form_contents(int::in, maybe(assoc_list(string, string))::out,
			io__state::di, io__state::uo) is det. 
cgi__get_form_contents(ContentLength, MaybeFormEntries) -->
    cgi__read_n_characters(ContentLength, Chars, Result),
    ( { Result = eof },
    	cgi__error([
	    "Unexpected end-of-file, or invalid ",
	        "<code>CONTENT_LENGTH</code>.\n\n",
	    "This may be due to a bug in your WWW browser?\n"
        ]),
        { MaybeFormEntries = no }
    ; { Result = error(Error) },
        { io__error_message(Error, ErrorMsg) },
    	cgi__error([
	    "I/O error reading standard input: ", ErrorMsg, "\n\n"
        ]),
        { MaybeFormEntries = no }
    ; { Result = ok },
        ( { cgi__parse_form_entries(FormEntries, Chars, []) } ->
            { MaybeFormEntries = yes(FormEntries) }
        ;
            { MaybeFormEntries = no }
        )
    ).

%-----------------------------------------------------------------------------%

:- pred cgi__read_n_characters(int, list(char), io__result,
				io__state, io__state).
:- mode cgi__read_n_characters(in, out, out, di, uo) is det.

cgi__read_n_characters(NumChars, Chars, Result) -->
    cgi__read_n_characters_rev(NumChars, [], RevChars, Result),
    { list__reverse(RevChars, Chars) }.

:- pred cgi__read_n_characters_rev(int, list(char), list(char), io__result,
				io__state, io__state).
:- mode cgi__read_n_characters_rev(in, in, out, out, di, uo) is det.

cgi__read_n_characters_rev(NumChars, Chars0, Chars, Result) -->
    ( { NumChars = 0 } ->
	{ Result = ok },
	{ Chars = Chars0 }
    ;
	io__read_char(CharResult),
	( { CharResult = eof },
	    { Result = eof },
	    { Chars = Chars0 }
        ; { CharResult = error(Error) },
            { Result = error(Error) },
            { Chars = Chars0 }
        ; { CharResult = ok(Char) },
            { NumChars1 is NumChars - 1 },
            read_n_characters_rev(NumChars1, [Char | Chars0], Chars, Result)
        )
    ).

%-----------------------------------------------------------------------------%

:- pred cgi__parse_form_entries(assoc_list(string, string),
				list(char), list(char)).
:- mode cgi__parse_form_entries(out, in, out) is semidet.

cgi__parse_form_entries(FormEntries) -->
	cgi__parse_form_entry(Name, Value),
	( ['&'] ->
		cgi__parse_form_entries(Rest),
		{ FormEntries = [Name - Value | Rest] }
	;
		{ FormEntries = [Name - Value] }
	).

:- pred cgi__parse_form_entry(string, string, list(char), list(char)).
:- mode cgi__parse_form_entry(out, out, in, out) is semidet.

cgi__parse_form_entry(Name, Value) -->
	cgi__parse_word(Name),
	['='],
	cgi__parse_word(Value).

:- pred cgi__parse_word(string, list(char), list(char)).
:- mode cgi__parse_word(out, in, out) is semidet.

cgi__parse_word(Word) -->
	cgi__parse_word_chars([], WordChars),
	{ string__from_rev_char_list(WordChars, Word) }.

:- pred cgi__parse_word_chars(list(char), list(char), list(char), list(char)).
:- mode cgi__parse_word_chars(in, out, in, out) is semidet.

cgi__parse_word_chars(RevChars0, RevChars) -->
	( [Char], { Char \= ('&'), Char \= ('=') } ->
		( { Char = ('%') } ->
			[Hex1, Hex2],
			{ hex_pair_to_char(Hex1, Hex2, RealChar) }
		; { Char = ('+') } ->
			{ RealChar = ' ' }
		;
			{ RealChar = Char }
		),
		cgi__parse_word_chars([RealChar | RevChars0], RevChars)
	;
		{ RevChars = RevChars0 }
	).

:- pred hex_pair_to_char(char::in, char::in, char::out) is semidet.
hex_pair_to_char(Hex1, Hex2, Char) :-
	char__is_hex_digit(Hex1),
	char__is_hex_digit(Hex2),
	char__digit_to_int(Hex1, Int1),
	char__digit_to_int(Hex2, Int2),
	Val is Int1 * 16 + Int2,
	char__to_int(Char, Val).

%-----------------------------------------------------------------------------%

:- pred cgi__error(list(string)::in, io__state::di, io__state::uo) is det.
cgi__error(MessageList) -->
	{ string__append_list(MessageList, Message) },
	output_content_type_html,
	html__output_html(html([title(text("CGI Error Message"))],
		(heading(1, text("CGI Error")),
		 markup(Message)))),
	io__set_exit_status(1).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
