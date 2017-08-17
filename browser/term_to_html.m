%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2017 The Mercury team.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: term_to_html.m.
% Main author: wangp.
%
% This module produces an HTML document for browsing a Mercury term.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module mdb.term_to_html.
:- interface.

:- import_module io.
:- import_module maybe.

:- import_module mdb.browser_term.

:- pred write_html_doc(io.output_stream::in, browser_term::in, string::in,
    maybe_error::out, io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module construct.
:- import_module deconstruct.
:- import_module dir.
:- import_module exception.
:- import_module int.
:- import_module list.
:- import_module string.
:- import_module type_desc.
:- import_module univ.

:- import_module mdb.browse.
:- import_module mdb.percent_encoding.

%---------------------------------------------------------------------------%

write_html_doc(Stream, BrowserTerm, MdbDir, Result, !IO) :-
    try_io(write_html_doc_2(Stream, BrowserTerm, MdbDir), TryResult, !IO),
    (
        TryResult = succeeded({}),
        Result = ok
    ;
        TryResult = exception(Univ),
        ( if univ_to_type(Univ, Error : io.error) then
            Result = error(io.error_message(Error))
        else
            Result = error(string(univ_value(Univ)))
        )
    ).

:- pred write_html_doc_2(io.output_stream::in, browser_term::in, string::in,
    {}::out, io::di, io::uo) is cc_multi.

write_html_doc_2(Stream, BrowserTerm, MdbDir, {}, !IO) :-
    make_file_url_prefix(MdbDir, FilePrefix),
    list.foldl(write_string(Stream), header(FilePrefix), !IO),
    write_browser_term_in_script(Stream, BrowserTerm, !IO),
    io.write_string(Stream, footer, !IO).

:- pred make_file_url_prefix(string::in, string::out) is det.

make_file_url_prefix(Path0, FilePrefix) :-
    % Replace backslashes with forward slashes in Windows paths.
    ( if dir.directory_separator('\\') then
        string.replace_all(Path0, "\\", "/", Path)
    else
        Path = Path0
    ),
    Segments = string.split_at_char('/', Path),
    EncodedSegments = list.map(percent_encode_path_segment, Segments),
    EncodedPath = string.join_list("/", EncodedSegments),
    ( if string.prefix(EncodedPath, "/") then
        FilePrefix = "file://" ++ EncodedPath
    else
        FilePrefix = "file:///" ++ EncodedPath
    ).

:- func header(string) = list(string).

header(FilePrefix) = [
    "<!doctype html>\n",

    "<link rel='stylesheet' href='", FilePrefix, "/jstree.style.min.css' />\n",
    "<script src='", FilePrefix, "/jquery.slim.min.js'></script>\n",
    "<script src='", FilePrefix, "/jstree.min.js'></script>\n",

    "<link rel='stylesheet' href='", FilePrefix, "/mdb_term_browser.css' />\n",
    "<script src='", FilePrefix, "/mdb_term_browser.js'></script>\n",

    "<div class='container'>\n",
    " <div class='search-container'>Search\n",
    "  <input type='text' id='searchbox' size='50' />\n",
    " </div>\n",
    " <div id='treeview'></div>\n",
    "</div>\n",

    "<script type='text/javascript'>\n",
    "var term =\n"
].

:- func footer = string.

footer = ";\n</script>\n".

%---------------------------------------------------------------------------%

:- inst plain_term for browser_term/0
    --->    plain_term(ground).

    % Write a JavaScript representation of a Mercury term inside a <script>
    % element. Object keys are left unquoted and we depend on being able to
    % write trailing commas, so the output is not JSON.
    %
:- pred write_browser_term_in_script(io.output_stream, browser_term, io, io).
:- mode write_browser_term_in_script(in, in(plain_term), di, uo) is cc_multi.
:- mode write_browser_term_in_script(in, in, di, uo) is cc_multi.

write_browser_term_in_script(Stream, BrowserTerm, !IO) :-
    (
        BrowserTerm = plain_term(TermUniv),
        Term = univ_value(TermUniv),
        TypeDesc = type_of(Term),
        TypeName = type_name(TypeDesc),
        functor(Term, include_details_cc, Functor0, Arity),
        ( if
            Functor0 = "[|]",
            Arity = 2,
            flatten_list(Term, ElementUnivs0)
        then
            length(ElementUnivs0, Length),
            ( if Length = 1 then
                Functor = "list of 1 element"
            else
                Functor = "list of " ++ from_int(Length) ++ " elements"
            ),
            FlattenedList = yes(ElementUnivs0)
        else
            Functor = Functor0,
            FlattenedList = no
        )
    ;
        BrowserTerm = synthetic_term(Functor, Args, MaybeResult),
        Arity = length(Args),
        (
            MaybeResult = no,
            TypeName = "<<predicate>>"
        ;
            MaybeResult = yes(_),
            TypeName = "<<function>>"
        ),
        FlattenedList = no
    ),

    js_begin_object(Stream, !IO),

    js_object_key(Stream, "type", !IO),
    js_string(Stream, TypeName, !IO),
    js_comma(Stream, !IO),

    js_object_key(Stream, "functor", !IO),
    js_string(Stream, Functor, !IO),
    js_comma(Stream, !IO),

    ( if Arity = 0 then
        true
    else
        browser_term_to_html_flat_string(BrowserTerm, OneLine, Elided, !IO),
        js_object_key(Stream, "oneline", !IO),
        js_string(Stream, OneLine, !IO),
        js_comma(Stream, !IO),
        (
            Elided = yes,
            js_object_key(Stream, "oneline_elided", !IO),
            js_bool(Stream, Elided, !IO),
            js_comma(Stream, !IO)
        ;
            Elided = no
        ),

        js_object_key(Stream, "args", !IO),
        js_begin_array(Stream, !IO),
        (
            FlattenedList = yes(ElementUnivs),
            foldl2(write_numbered_element_in_script(Stream),
                ElementUnivs, 1, _ElementNumber, !IO)
        ;
            FlattenedList = no,
            write_browser_term_args_in_script(Stream, BrowserTerm, !IO)
        ),
        js_end_array(Stream, !IO)
    ),

    js_end_object(Stream, !IO).

:- pred write_browser_term_args_in_script(io.output_stream, browser_term,
    io, io).
:- mode write_browser_term_args_in_script(in, in(plain_term), di, uo)
    is cc_multi.
:- mode write_browser_term_args_in_script(in, in, di, uo)
    is cc_multi.

write_browser_term_args_in_script(Stream, BrowserTerm, !IO) :-
    (
        BrowserTerm = plain_term(TermUniv),
        Term = univ_value(TermUniv),
        ( if
            deconstruct_du(Term, include_details_cc, FunctorNumber, _Arity1,
                Args)
        then
            TypeDesc = type_of(Term),
            ( if
                get_functor_with_names(TypeDesc, FunctorNumber, _Functor,
                    _Arity, _ArgTypes, FieldNames)
            then
                list.foldl2_corresponding(write_du_field_in_script(Stream),
                    Args, FieldNames, 1, _ArgNum, !IO)
            else
                list.foldl2(write_numbered_arg_in_script(Stream),
                    Args, 1, _ArgNum, !IO)
            )
        else
            deconstruct(Term, include_details_cc, _Functor, _Arity, Args),
            list.foldl2(write_numbered_arg_in_script(Stream),
                Args, 1, _ArgNum, !IO)
        )
    ;
        BrowserTerm = synthetic_term(_Function, Args, MaybeResult),
        list.foldl2(write_numbered_arg_in_script(Stream), Args, 1, ArgNum, !IO),
        (
            MaybeResult = no
        ;
            MaybeResult = yes(ResultUniv),
            write_arg_in_script(Stream, ResultUniv, yes("result"), ArgNum, !IO)
        )
    ).

:- pred write_du_field_in_script(io.output_stream::in, univ::in,
    maybe(string)::in, int::in, int::out, io::di, io::uo) is cc_multi.

write_du_field_in_script(Stream, ArgUniv, MaybeFieldName, ArgNum, ArgNum + 1,
        !IO) :-
    write_arg_in_script(Stream, ArgUniv, MaybeFieldName, ArgNum, !IO).

:- pred write_numbered_arg_in_script(io.output_stream::in, univ::in,
    int::in, int::out, io::di, io::uo) is cc_multi.

write_numbered_arg_in_script(Stream, ArgUniv, ArgNum, ArgNum + 1, !IO) :-
    write_arg_in_script(Stream, ArgUniv, no, ArgNum, !IO).

:- pred write_numbered_element_in_script(io.output_stream::in, univ::in,
    int::in, int::out, io::di, io::uo) is cc_multi.

write_numbered_element_in_script(Stream, ArgUniv, Num, Num + 1, !IO) :-
    write_arg_in_script(Stream, ArgUniv, yes("#" ++ from_int(Num)), Num, !IO).

:- pred write_arg_in_script(io.output_stream::in, univ::in,
    maybe(string)::in, int::in, io::di, io::uo) is cc_multi.

write_arg_in_script(Stream, ArgUniv, MaybeFieldName, ArgNum, !IO) :-
    js_begin_object(Stream, !IO),
    js_object_key(Stream, "name", !IO),
    (
        MaybeFieldName = yes(FieldName),
        js_string(Stream, FieldName, !IO)
    ;
        MaybeFieldName = no,
        js_int(Stream, ArgNum, !IO)
    ),
    js_comma(Stream, !IO),
    js_object_key(Stream, "term", !IO),
    write_browser_term_in_script(Stream, plain_term(ArgUniv), !IO),
    js_end_object(Stream, !IO),
    js_comma(Stream, !IO).

%---------------------------------------------------------------------------%

:- pred flatten_list(T::in, list(univ)::out) is semidet.

flatten_list(Term, ElementUnivs) :-
    limited_deconstruct(Term, canonicalize, 2, Functor, Arity, Args),
    (
        Functor = "[]",
        Arity = 0,
        Args = [],
        ElementUnivs = []
    ;
        Functor = "[|]",
        Arity = 2,
        Args = [Head, Tail],
        flatten_list(univ_value(Tail), ElementUnivs0),
        ElementUnivs = [Head | ElementUnivs0]
    ).

%---------------------------------------------------------------------------%

% Helpers for writing out JavaScript values within an HTML <script> element.
% We do not generate indented output because we may need to write large,
% deeply nested terms quickly, and we want the web browser to parse the file
% as quickly as possible.

:- pred js_begin_object(io.output_stream::in, io::di, io::uo) is det.

js_begin_object(Stream, !IO) :-
    io.write_string(Stream, "{\n", !IO).

:- pred js_end_object(io.output_stream::in, io::di, io::uo) is det.

js_end_object(Stream, !IO) :-
    io.write_char(Stream, '}', !IO).

:- pred js_object_key(io.output_stream::in, string::in, io::di, io::uo)
    is det.

js_object_key(Stream, Key, !IO) :-
    % Assume that the key does not require escaping.
    io.write_string(Stream, Key, !IO),
    io.write_char(Stream, ':', !IO).

:- pred js_begin_array(io.output_stream::in, io::di, io::uo) is det.

js_begin_array(Stream, !IO) :-
    io.write_string(Stream, "[\n", !IO).

:- pred js_end_array(io.output_stream::in, io::di, io::uo) is det.

js_end_array(Stream, !IO) :-
    io.write_char(Stream, ']', !IO).

:- pred js_comma(io.output_stream::in, io::di, io::uo) is det.

js_comma(Stream, !IO) :-
    io.write_string(Stream, ",\n", !IO).

:- pred js_bool(io.output_stream::in, bool::in, io::di, io::uo) is det.

js_bool(Stream, B, !IO) :-
    (
        B = yes,
        S = "true"
    ;
        B = no,
        S = "false"
    ),
    io.write_string(Stream, S, !IO).

:- pred js_int(io.output_stream::in, int::in, io::di, io::uo) is det.

js_int(Stream, Int, !IO) :-
    io.write_int(Stream, Int, !IO).

:- pred js_string(io.output_stream::in, string::in, io::di, io::uo) is det.

js_string(Stream, String, !IO) :-
    io.write_char(Stream, '"', !IO),
    string.foldl(escape_and_put_char(Stream), String, !IO),
    io.write_char(Stream, '"', !IO).

:- pred escape_and_put_char(io.output_stream::in, char::in, io::di, io::uo) is det.

escape_and_put_char(Stream, Char, !IO) :-
    ( if escape_char(Char, EscapedCharStr) then
        io.write_string(Stream, EscapedCharStr, !IO)
    else
        io.write_char(Stream, Char, !IO)
    ).

:- pred escape_char(char::in, string::out) is semidet.

escape_char('"', "\\""").
escape_char('\\', "\\\\").
escape_char('/', "\\/").    % prevent HTML parser seeing "</script" in string
escape_char('\b', "\\b").
escape_char('\f', "\\f").
escape_char('\n', "\\n").
escape_char('\r', "\\r").
escape_char('\t', "\\t").

%---------------------------------------------------------------------------%
:- end_module mdb.term_to_html.
%---------------------------------------------------------------------------%
