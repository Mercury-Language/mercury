%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000, 2001, 2004-2006 The University of Melbourne.
% Copyright (C) 2014, 2018, 2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% Main author: conway@cs.mu.oz.au.
%
%---------------------------------------------------------------------------%

:- module tryit.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parsing.
:- import_module xml.
:- import_module xml.cat.
:- import_module xml.doc.
:- import_module xml.dtd.
:- import_module xml.encoding.
:- import_module xml.parse.
:- import_module xml.ns.

:- import_module char.
:- import_module io.environment.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module pretty_printer.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    process_cmd_line_args(Args, !IO).

:- pred process_cmd_line_args(list(string)::in, io::di, io::uo) is det.

process_cmd_line_args([], !IO).
process_cmd_line_args([File | Files], !IO) :-
    io.read_named_file_as_string(File, ReadResult, !IO),
    (
        ReadResult = ok(Text),
        process_arg_text(Text, ArgResult, !IO),
        (
            ArgResult = ok({DTD, Doc}),
            io.stdout_stream(StdOut, !IO),
            ns_translate(Doc, NsDoc),
            pretty_printer.write_doc(StdOut, format(DTD), !IO),
            io.nl(StdOut, !IO),
            io.nl(StdOut, !IO),
            pretty_printer.write_doc(StdOut, format(NsDoc), !IO),
            io.nl(StdOut, !IO)
        ;
            ArgResult = error(Err),
            io.stderr_stream(StdErr, !IO),
            io.format(StdErr, "%s: %s\n", [s(File), s(Err)], !IO),
            io.write_line(StdErr, ArgResult, !IO)
        )
    ;
        ReadResult = error(ReadErr),
        io.stderr_stream(StdErr0, !IO),
        io.format(StdErr0, "error reading file `%s': %s\n",
            [s(File), s(io.error_message(ReadErr))], !IO)
    ),
    process_cmd_line_args(Files, !IO).

:- pred process_arg_text(string::in,
    parse({xml.dtd.dtd, xml.doc.document})::out, io::di, io::uo) is det.

process_arg_text(Text, Result, !IO) :-
    some [!PState] (
        pstate(make_entity(Text), make_encoding(utf8), init, !.IO, !:PState),
        io((pred(Dirs0::out, !.IO::di, !:IO::uo) is det :-
            io.environment.get_environment_var("XML_DIRS", MStr, !IO),
            (
                MStr = no,
                Str = "."
            ;
                MStr = yes(Str)
            ),
            split((':'), Str, Dirs0)
        ), Dirs, !PState),
        set_global(gDirs, dirs(Dirs), !PState),
        io((pred(Cat0::out, !.IO::di, !:IO::uo) is det :-
            load_catalog("catalog", Dirs, Res1, !IO),
            (
                Res1 = catalog_ok(Cat0)
            ;
                Res1 = catalog_error(Err0),
                io.stderr_stream(StdErr0, !IO),
                io.format(StdErr0, "error reading catalog: %s\n",
                    [s(Err0)], !IO),
                init(Catalog0),
                Cat0 = catalog(Catalog0)
            )
        ), Cat, !PState),
        set_global(gCatalog, Cat, !PState),
        map.from_assoc_list([
            "ASCII"   - make_encoding(ascii7),
            "ascii"   - make_encoding(ascii7),
            "Latin-1" - make_encoding(latin1),
            "Latin1"  - make_encoding(latin1),
            "UTF-8"   - make_encoding(utf8),
            "utf-8"   - make_encoding(utf8)
        ], Encodings),
        set_global(gEncodings, encodings(Encodings), !PState),
        parse_document(!PState),
        finish(Result, !.PState, !:IO)
    ).

:- pred split(char::in, string::in, list(string)::out) is det.

split(C, Str0, Strs) :-
    string.to_char_list(Str0, Chars),
    split1(C, [], Strs0, Chars, _),
    list.reverse(Strs0, Strs).

:- pred split1(char::in, list(string)::in, list(string)::out,
    list(char)::in, list(char)::out) is det.

split1(_C, Strs, Strs, [], []).
split1(C, Strs0, Strs) -->
    =([_ | _]),
    split2(C, [], Cs0),
    { reverse(Cs0, Cs) },
    ( if { Cs \= [] } then
        { string.from_char_list(Cs, Str) },
        { Strs1 = [Str | Strs0] }
    else
        { Strs1 = Strs0 }
    ),
    split1(C, Strs1, Strs).

:- pred split2(char::in, list(char)::in, list(char)::out,
    list(char)::in, list(char)::out) is det.

split2(_C, Cs, Cs, [], []).
split2(C, Cs0, Cs) -->
    [C0],
    ( if { C = C0 } then
        { Cs = Cs0 }
    else
        split2(C, [C0 | Cs0], Cs)
    ).
