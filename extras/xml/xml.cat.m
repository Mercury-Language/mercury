%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000, 2011 The University of Melbourne.
% Copyright (C) 2014, 2018, 2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% Main author: conway@cs.mu.oz.au.
%
%---------------------------------------------------------------------------%

:- module xml.cat.
:- interface.


:- import_module io.
:- import_module list.
:- import_module map.

:- type catalog
    --->    catalog(map(public_id, system_id)).

:- type dirs == list(path).

:- type public_id == string.
:- type system_id == string.
:- type path == string.

:- type catalog_result(T)
    --->    catalog_ok(T)
    ;       catalog_error(string).

:- pred load_catalog(string::in, dirs::in, catalog_result(catalog)::out,
    io::di, io::uo) is det.

:- pred find_catalog(string::in, dirs::in, catalog_result(string)::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module prolog.
:- import_module string.

%---------------------------------------------------------------------------%

:- type entry
    --->    dtd(public_id, system_id)
    ;       none.

load_catalog(Name, Dirs, Res, !IO) :-
    find_catalog(Name, Dirs, Res0, !IO),
    (
        Res0 = catalog_ok(Path),
        read_file(Res1, !IO),
        (
            Res1 = ok(CatChars),
            gather_catalog_lines(1, CatLines0, CatChars, _),
            decomment(CatLines0, CatLines),
            parse(CatLines, Entries, Errors),
            Cat0 = catalog(map.init),
            list.foldl(add_entry, Entries, Cat0, Cat),
            Res = catalog_ok(Cat),
            list.foldl(
                ( pred(Msg::in, !.IO::di, !:IO::uo) is det :-
                    io.stderr_stream(StdErr, !IO),
                    io.format(StdErr, "%s: %s\n", [s(Path), s(Msg)], !IO)
                ), Errors, !IO)
        ;
            Res1 = error(_, Err),
            io.error_message(Err, Msg),
            Res = catalog_error(Msg)
        )
    ;
        Res0 = catalog_error(Msg),
        Res = catalog_error(Msg)
    ).

find_catalog(Name, [], catalog_error(Err), !IO) :-
    string.format("`%s' not found", [s(Name)], Err).
find_catalog(Name, [Dir | Dirs], Res, !IO) :-
    append_list([Dir, "/", Name], Path),
    prolog.see(Path, Res0, !IO),
    ( if Res0 = ok then
        Res = catalog_ok(Path)
    else
        find_catalog(Name, Dirs, Res, !IO)
    ).

:- type catalog_line
    --->    catalog_line(
                line_number :: int,
                line_chars  :: list(char)
            ).

:- pred gather_catalog_lines(int::in, list(catalog_line)::out,
    list(char)::in, list(char)::out) is det.

gather_catalog_lines(_N, [], [], []).
gather_catalog_lines(N, [Line | Lines]) -->
    =([_ | _]),
    gather_catalog_line(N, Line),
    gather_catalog_lines(N + 1, Lines).

:- pred gather_catalog_line(int::in, catalog_line::out,
    list(char)::in, list(char)::out) is det.

gather_catalog_line(LineNumber, catalog_line(LineNumber, Cs)) -->
    collect_until('\n', Cs).

:- pred decomment(list(catalog_line)::in, list(catalog_line)::out) is det.

decomment(Lines0, Lines) :-
    list.map(
        ( pred(Line0::in, Line::out) is det :-
            Line0 = catalog_line(N, Cs0),
            Line = catalog_line(N, Cs),
            collect_until('#', Cs, Cs0, _)
        ), Lines0, Lines).

:- pred parse(list(catalog_line)::in,
    list(entry)::out, list(string)::out) is det.

parse([], [], []).
parse([Line | Lines], Entries, Errors) :-
    Line = catalog_line(N, Cs),
    ( if parse_entry(Entry, Cs, _) then
        parse(Lines, Entries0, Errors),
        Entries = [Entry | Entries0]
    else
        string.format("%d: syntax error", [i(N)], Msg),
        parse(Lines, Entries, Errors0),
        Errors = [Msg | Errors0]
    ).

:- pred parse_entry(entry::out, list(char)::in, list(char)::out) is semidet.

parse_entry(Entry) -->
    ws,
    ( if
        ['P','U','B','L','I','C'], ws, string(PublicId), ws, string(SystemId)
    then
        { Entry = dtd(PublicId, SystemId) }
    else if =([]) then
        { Entry = none }
    else
        { fail }
    ).

:- pred add_entry(entry::in, catalog::in, catalog::out) is det.

add_entry(none, Cat, Cat).
add_entry(dtd(PublicId, SystemId), catalog(Cat0), catalog(Cat)) :-
    map.det_insert(PublicId, SystemId, Cat0, Cat).

:- pred ws(list(char)::in, list(char)::out) is det.

ws -->
    ( if [C], { char.is_whitespace(C) } then
        ws
    else
        []
    ).

:- pred string(string::out, list(char)::in, list(char)::out) is semidet.

string(Str) -->
    ['"'],
    collect_until('"', Cs),
    { string.from_char_list(Cs, Str) }.

:- pred collect_until(char::in, list(char)::out,
    list(char)::in, list(char)::out) is det.

collect_until(_C, [], [], []).
collect_until(C, Cs) -->
    =([_ | _]),
    [C0],
    ( if { C = C0 } then
        { Cs = [] }
    else
        collect_until(C, Cs0),
        { Cs = [C0 | Cs0] }
    ).
