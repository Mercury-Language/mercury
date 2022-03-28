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
:- import_module xml.encoding.
:- import_module xml.parse.
:- import_module xml.ns.

:- import_module char.
:- import_module io.environment.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module prolog.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    main(Args, !IO).

:- pred main(list(string)::in, io::di, io::uo) is det.

main([], !IO).
main([File | Files], !IO) :-
    prolog.see(File, Res0, !IO),
    ( if Res0 = ok then
        io.read_file_as_string(TextResult, !IO),
        (
            TextResult = error(_, TextErr),
            io.stderr_stream(StdErr0, !IO),
            io.format(StdErr0, "error reading file `%s': %s\n",
                [s(File), s(io.error_message(TextErr))], !IO)
        ;
            TextResult = ok(Text),
            pstate(mkEntity(Text), mkEncoding(utf8), init, !IO),
            io((pred(Dirs0::out, !.IO::di, !:IO::uo) is det :-
                io.environment.get_environment_var("XML_DIRS", MStr, !IO),
                (
                    MStr = no,
                    Str = "."
                ;
                    MStr = yes(Str)
                ),
                split((':'), Str, Dirs0)
            ), Dirs, !IO),
            set(gDirs, dirs(Dirs), !IO),
            io((pred(Cat0::out, !.IO::di, !:IO::uo) is det :-
                load("catalog", Dirs, Res1, !IO),
                (
                    Res1 = ok(Cat0)
                ;
                    Res1 = error(Err0),
                    io.stderr_stream(StdErr0, !IO),
                    io.format(StdErr0, "error reading catalog: %s\n", [s(Err0)],
                        !IO),
                    init(Catalog0),
                    Cat0 = catalog(Catalog0)
                )
            ), Cat, !IO),
            set(gCatalog, Cat, !IO),
            map.from_assoc_list([
                "ASCII"   - mkEncoding(ascii7),
                "ascii"   - mkEncoding(ascii7),
                "Latin-1" - mkEncoding(latin1),
                "Latin1"  - mkEncoding(latin1),
                "UTF-8"   - mkEncoding(utf8),
                "utf-8"   - mkEncoding(utf8)
            ], Encodings),
            set(gEncodings, encodings(Encodings), !IO),
            document(!IO),
            finish(Res, !IO),
            (
                Res = ok((DTD, Doc)),
                nsTranslate(Doc, NsDoc),
                New = cat.ok((DTD, NsDoc)),
                io.write_line(New, !IO)
                % If you do not want to turn the doc to namespace aware, change
                % the above three lines to write_line(Res).
            ;
                Res = error(Err),
                io.stderr_stream(StdErr, !IO),
                io.format(StdErr, "%s: %s\n", [s(File), s(Err)], !IO),
                io.write_line(Res, !IO)
            )
        )
    else
        true
    ),
    main(Files, !IO).

:- pred split(char, string, list(string)).
:- mode split(in, in, out) is det.

split(C, Str0, Strs) :-
    string.to_char_list(Str0, Chars),
    split1(C, [], Strs0, Chars, _),
    reverse(Strs0, Strs).

:- pred split1(char, list(string), list(string), list(char), list(char)).
:- mode split1(in, in, out, in, out) is det.

split1(_C, Strs, Strs, [], []).
split1(C, Strs0, Strs) -->
    =([_|_]),
    split2(C, [], Cs0),
    { reverse(Cs0, Cs) },
    ( { Cs \= [] } ->
        { string.from_char_list(Cs, Str) },
        { Strs1 = [Str|Strs0] }
    ;
        { Strs1 = Strs0 }
    ),
    split1(C, Strs1, Strs).

:- pred split2(char, list(char), list(char), list(char), list(char)).
:- mode split2(in, in, out, in, out) is det.

split2(_C, Cs, Cs, [], []).
split2(C, Cs0, Cs) -->
    [C0],
    ( { C = C0 } ->
        { Cs = Cs0 }
    ;
        split2(C, [C0|Cs0], Cs)
    ).

