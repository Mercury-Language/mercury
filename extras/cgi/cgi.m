%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2001, 2006 The University of Melbourne.
% Copyright (C) 2014, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% File: cgi.m.
% Author: fjh.
%
% This module provides a Mercury interface to HTML forms using CGI.
% For documentation on HTML forms and the CGI interface, see
% <http://www.ncsa.uiuc.edu/SDG/Software/Mosaic/Docs/fill-out-forms/
% overview.html>.
%
% This is intended to conform to versions 1.0 and 1.1 of the
% CGI specification.
%
%-----------------------------------------------------------------------------%

:- module cgi.
:- interface.

:- import_module assoc_list.
:- import_module io.
:- import_module maybe.
:- import_module string.

% cgi.get_form(MaybeFormEntries):
%       This procedure should be called form within a CGI program
%       that should be invoked with a METHOD of POST.
%       If all goes well, it will return the form entries.
%       If something goes wrong, it will print an appropriate HTML-formatted
%       error message to stdout, call io.set_exit_status(1),
%       and return `no'.
:- pred cgi.get_form(maybe(assoc_list(string, string))::out,
                        io::di, io::uo) is det.

% cgi.maybe_get_form(MaybeFormEntries):
%       This procedure should be called form within a CGI program
%       that *may* be invoked with a METHOD of POST.
%       If all goes well, it will return the form entries.
%       If not invoked using a METHOD of POST, it will return `no'.
%       If something goes wrong, it will print an appropriate HTML-formatted
%       error message to stdout, call io.set_exit_status(1),
%       and return `no'.
:- pred cgi.maybe_get_form(maybe(assoc_list(string, string))::out,
                        io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module html.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module pair.

%-----------------------------------------------------------------------------%

cgi.maybe_get_form(FormEntries) -->
    io.get_environment_var("REQUEST_METHOD", REQUEST_METHOD),
    ( { REQUEST_METHOD \= yes("POST") } ->
        { FormEntries = no }
    ;
        cgi.get_form_contents(FormEntries)
    ).

cgi.get_form(FormEntries) -->
    io.get_environment_var("REQUEST_METHOD", REQUEST_METHOD),
    ( { REQUEST_METHOD \= yes("POST") } ->
       cgi.error([
            "This script should be referenced with a ",
                "<code>REQUEST_METHOD</code> of <code>POST</code>.\n\n",
            "If you don't understand this, see this ",
                "<A HREF=""http://www.ncsa.uiuc.edu/SDG/Software/Mosaic/Docs",
                "/fill-out-forms/overview.html"">forms overview</A>.\n"
        ]),
        { FormEntries = no }
    ;
        cgi.get_form_contents(FormEntries)
    ).

:- pred cgi.get_form_contents(maybe(assoc_list(string, string))::out,
                        io::di, io::uo) is det.

cgi.get_form_contents(FormEntries) -->
    io.get_environment_var("CONTENT_TYPE", CONTENT_TYPE),
    io.get_environment_var("CONTENT_LENGTH", CONTENT_LENGTH),
    ( { CONTENT_TYPE \= yes("application/x-www-form-urlencoded") } ->
        cgi.error([
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
        { string.to_int(ContentLengthString, ContentLength) },
        { ContentLength >= 0 }
    ->
        cgi.get_form_contents(ContentLength, FormEntries)
    ;
        cgi.error([
            "Invalid <code>CONTENT_LENGTH</code>.\n",
            "This may be due to a bug in your WWW browser?\n"
        ]),
        { FormEntries = no }
    ).

:- pred cgi.get_form_contents(int::in, maybe(assoc_list(string, string))::out,
                        io::di, io::uo) is det.
cgi.get_form_contents(ContentLength, MaybeFormEntries) -->
    cgi.read_n_characters(ContentLength, Chars, Result),
    ( { Result = eof },
        cgi.error([
            "Unexpected end-of-file, or invalid ",
                "<code>CONTENT_LENGTH</code>.\n\n",
            "This may be due to a bug in your WWW browser?\n"
        ]),
        { MaybeFormEntries = no }
    ; { Result = error(Error) },
        { io.error_message(Error, ErrorMsg) },
        cgi.error([
            "I/O error reading standard input: ", ErrorMsg, "\n\n"
        ]),
        { MaybeFormEntries = no }
    ; { Result = ok },
        ( { cgi.parse_form_entries(FormEntries, Chars, []) } ->
            { MaybeFormEntries = yes(FormEntries) }
        ;
            { MaybeFormEntries = no }
        )
    ).

%-----------------------------------------------------------------------------%

:- pred cgi.read_n_characters(int, list(char), io.result,
                                io, io).
:- mode cgi.read_n_characters(in, out, out, di, uo) is det.

cgi.read_n_characters(NumChars, Chars, Result) -->
    cgi.read_n_characters_rev(NumChars, [], RevChars, Result),
    { list.reverse(RevChars, Chars) }.

:- pred cgi.read_n_characters_rev(int, list(char), list(char), io.result,
                                io, io).
:- mode cgi.read_n_characters_rev(in, in, out, out, di, uo) is det.

cgi.read_n_characters_rev(NumChars, Chars0, Chars, Result) -->
    ( { NumChars = 0 } ->
        { Result = ok },
        { Chars = Chars0 }
    ;
        io.read_char(CharResult),
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

:- pred cgi.parse_form_entries(assoc_list(string, string),
                                list(char), list(char)).
:- mode cgi.parse_form_entries(out, in, out) is semidet.

cgi.parse_form_entries(FormEntries) -->
        cgi.parse_form_entry(Name, Value),
        ( ['&'] ->
                cgi.parse_form_entries(Rest),
                { FormEntries = [Name - Value | Rest] }
        ;
                { FormEntries = [Name - Value] }
        ).

:- pred cgi.parse_form_entry(string, string, list(char), list(char)).
:- mode cgi.parse_form_entry(out, out, in, out) is semidet.

cgi.parse_form_entry(Name, Value) -->
        cgi.parse_word(Name),
        ['='],
        cgi.parse_word(Value).

:- pred cgi.parse_word(string, list(char), list(char)).
:- mode cgi.parse_word(out, in, out) is semidet.

cgi.parse_word(Word) -->
        cgi.parse_word_chars([], WordChars),
        { string.from_rev_char_list(WordChars, Word) }.

:- pred cgi.parse_word_chars(list(char), list(char), list(char), list(char)).
:- mode cgi.parse_word_chars(in, out, in, out) is semidet.

cgi.parse_word_chars(RevChars0, RevChars) -->
        ( [Char], { Char \= ('&'), Char \= ('=') } ->
                ( { Char = ('%') } ->
                        [Hex1, Hex2],
                        { hex_pair_to_char(Hex1, Hex2, RealChar) }
                ; { Char = ('+') } ->
                        { RealChar = ' ' }
                ;
                        { RealChar = Char }
                ),
                cgi.parse_word_chars([RealChar | RevChars0], RevChars)
        ;
                { RevChars = RevChars0 }
        ).

:- pred hex_pair_to_char(char::in, char::in, char::out) is semidet.

hex_pair_to_char(Hex1, Hex2, Char) :-
        char.is_hex_digit(Hex1),
        char.is_hex_digit(Hex2),
        char.hex_digit_to_int(Hex1, Int1),
        char.hex_digit_to_int(Hex2, Int2),
        Val = Int1 * 16 + Int2,
        char.to_int(Char, Val).

%-----------------------------------------------------------------------------%

:- pred cgi.error(list(string)::in, io::di, io::uo) is det.

cgi.error(MessageList) -->
        { string.append_list(MessageList, Message) },
        output_content_type_html,
        html.output_html(html([title(text("CGI Error Message"))],
                (heading(1, text("CGI Error")),
                 markup(Message)))),
        io.set_exit_status(1).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
