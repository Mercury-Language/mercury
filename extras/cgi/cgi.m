%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2001, 2006 The University of Melbourne.
% Copyright (C) 2014, 2018, 2022 The Mercury team.
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

%-----------------------------------------------------------------------------%

    % cgi.get_form(MaybeFormEntries):
    % This procedure should be called form within a CGI program
    % that should be invoked with a METHOD of POST.
    % If all goes well, it will return the form entries.
    % If something goes wrong, it will print an appropriate HTML-formatted
    % error message to stdout, call io.set_exit_status(1), and return `no'.
    %
:- pred get_form(maybe(assoc_list(string, string))::out, io::di, io::uo)
    is det.

    % cgi.maybe_get_form(MaybeFormEntries):
    % This procedure should be called form within a CGI program
    % that *may* be invoked with a METHOD of POST.
    % If all goes well, it will return the form entries.
    % If not invoked using a METHOD of POST, it will return `no'.
    % If something goes wrong, it will print an appropriate HTML-formatted
    % error message to stdout, call io.set_exit_status(1), and return `no'.
    %
:- pred maybe_get_form(maybe(assoc_list(string, string))::out, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module html.

:- import_module char.
:- import_module int.
:- import_module io.environment.
:- import_module list.
:- import_module pair.

%-----------------------------------------------------------------------------%

maybe_get_form(FormEntries, !IO) :-
    io.environment.get_environment_var("REQUEST_METHOD", REQUEST_METHOD, !IO),
    ( if REQUEST_METHOD \= yes("POST") then
        FormEntries = no
    else
        get_form_contents(FormEntries, !IO)
    ).

get_form(FormEntries, !IO) :-
    io.environment.get_environment_var("REQUEST_METHOD", REQUEST_METHOD, !IO),
    ( if REQUEST_METHOD \= yes("POST") then
       cgi.error([
            "This script should be referenced with a ",
                "<code>REQUEST_METHOD</code> of <code>POST</code>.\n\n",
            "If you don't understand this, see this ",
                "<A HREF=""http://www.ncsa.uiuc.edu/SDG/Software/Mosaic/Docs",
                "/fill-out-forms/overview.html"">forms overview</A>.\n"
        ], !IO),
        FormEntries = no
    else
        get_form_contents(FormEntries, !IO)
    ).

:- pred get_form_contents(maybe(assoc_list(string, string))::out,
    io::di, io::uo) is det.

get_form_contents(FormEntries, !IO) :-
    io.environment.get_environment_var("CONTENT_TYPE", CONTENT_TYPE, !IO),
    io.environment.get_environment_var("CONTENT_LENGTH", CONTENT_LENGTH, !IO),
    ( if CONTENT_TYPE \= yes("application/x-www-form-urlencoded") then
        cgi.error([
            "This script can only be used to decode form results.\n",
            "It should be referenced with a <code>CONTENT_TYPE</code> of ",
                "<code>application/x-www-form-urlencoded</code>.\n\n",
            "If you don't understand this, see this ",
                "<A HREF=""http://www.ncsa.uiuc.edu/SDG/Software/Mosaic/Docs",
                "/fill-out-forms/overview.html"">forms overview</A>.\n"
        ], !IO),
        FormEntries = no
    else if
        CONTENT_LENGTH = yes(ContentLengthString),
        string.to_int(ContentLengthString, ContentLength),
        ContentLength >= 0
    then
        get_form_contents(ContentLength, FormEntries, !IO)
    else
        cgi.error([
            "Invalid <code>CONTENT_LENGTH</code>.\n",
            "This may be due to a bug in your WWW browser?\n"
        ], !IO),
        FormEntries = no
    ).

:- pred get_form_contents(int::in, maybe(assoc_list(string, string))::out,
    io::di, io::uo) is det.

get_form_contents(ContentLength, MaybeFormEntries, !IO) :-
    read_n_characters(ContentLength, Chars, Result, !IO),
    (
        Result = eof,
        cgi.error([
            "Unexpected end-of-file, or invalid ",
                "<code>CONTENT_LENGTH</code>.\n\n",
            "This may be due to a bug in your WWW browser?\n"
        ], !IO),
        MaybeFormEntries = no
    ;
        Result = error(Error),
        io.error_message(Error, ErrorMsg),
        cgi.error([
            "I/O error reading standard input: ", ErrorMsg, "\n\n"
        ], !IO),
        MaybeFormEntries = no
    ;
        Result = ok,
        ( if parse_form_entries(FormEntries, Chars, []) then
            MaybeFormEntries = yes(FormEntries)
        else
            MaybeFormEntries = no
        )
    ).

%-----------------------------------------------------------------------------%

:- pred read_n_characters(int::in, list(char)::out, io.result::out,
    io::di, io::uo) is det.

read_n_characters(NumChars, Chars, Result, !IO) :-
    read_n_characters_rev(NumChars, [], RevChars, Result, !IO),
    list.reverse(RevChars, Chars).

:- pred read_n_characters_rev(int::in, list(char)::in, list(char)::out,
    io.result::out, io::di, io::uo) is det.

read_n_characters_rev(NumChars, !Chars, Result, !IO) :-
    ( if NumChars = 0 then
        Result = ok
    else
        io.read_char(CharResult, !IO),
        (
            CharResult = eof,
            Result = eof
        ;
            CharResult = error(Error),
            Result = error(Error)
        ;
            CharResult = ok(Char),
            NumChars1 = NumChars - 1,
            read_n_characters_rev(NumChars1, [Char | !.Chars], !:Chars,
                Result, !IO)
        )
    ).

%-----------------------------------------------------------------------------%

:- pred parse_form_entries(assoc_list(string, string)::out,
    list(char)::in, list(char)::out) is semidet.

parse_form_entries(FormEntries) -->
    parse_form_entry(Name, Value),
    ( if ['&'] then
        parse_form_entries(Rest),
        { FormEntries = [Name - Value | Rest] }
    else
        { FormEntries = [Name - Value] }
    ).

:- pred parse_form_entry(string::out, string::out,
    list(char)::in, list(char)::out) is semidet.

parse_form_entry(Name, Value) -->
    parse_word(Name),
    ['='],
    parse_word(Value).

:- pred parse_word(string::out, list(char)::in, list(char)::out) is semidet.

parse_word(Word) -->
    parse_word_chars([], WordChars),
    { string.from_rev_char_list(WordChars, Word) }.

:- pred parse_word_chars(list(char)::in, list(char)::out,
    list(char)::in, list(char)::out) is semidet.

parse_word_chars(RevChars0, RevChars) -->
    ( if [Char], { Char \= ('&'), Char \= ('=') } then
        ( if { Char = ('%') } then
            [Hex1, Hex2],
            { hex_pair_to_char(Hex1, Hex2, RealChar) }
        else if { Char = ('+') } then
            { RealChar = ' ' }
        else
           { RealChar = Char }
        ),
        parse_word_chars([RealChar | RevChars0], RevChars)
    else
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

error(MessageList, !IO) :-
    string.append_list(MessageList, Message),
    output_content_type_html(!IO),
    html.output_html(html([title(text("CGI Error Message"))],
        (heading(1, text("CGI Error")),
        markup(Message))), !IO),
    io.set_exit_status(1, !IO).

%-----------------------------------------------------------------------------%
:- end_module cgi.
%-----------------------------------------------------------------------------%
