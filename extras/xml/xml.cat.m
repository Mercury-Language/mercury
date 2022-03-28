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

:- import_module xml.dtd.
:- import_module io.

:- type catalog
    --->    catalog(publicId -> systemId).

:- type dirs == [path].

:- type publicId == string.

:- type systemId == string.

:- type path == string.

:- type catRes(T)
    --->    ok(T)
    ;       error(string).

:- pred load(string::in, dirs::in, catRes(catalog)::out, io::di, io::uo)
    is det.

:- pred find(string::in, dirs::in, catRes(string)::out, io::di, io::uo)
    is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module prolog.
:- import_module string.

%---------------------------------------------------------------------------%

:- type entry
    --->    dtd(publicId, systemId)
    ;       none.

load(Name, Dirs, Res, !IO) :-
    find(Name, Dirs, Res0, !IO),
    (
        Res0 = ok(Path),
        read_file(Res1, !IO),
        (
            Res1 = ok(CatChars),
            lines(1, CatLines0, CatChars, _),
            decomment(CatLines0, CatLines),
            parse(Entries, Errors, CatLines),
            init(Cat0),
            foldl(addEntry, Entries, catalog(Cat0), Cat),
            Res = ok(Cat),
            list.foldl((pred(Msg::in, !.IO::di, !:IO::uo) is det :-
                io.stderr_stream(StdErr, !IO),
                io.format(StdErr, "%s: %s\n", [s(Path), s(Msg)], !IO)
            ), Errors, !IO)
        ;
            Res1 = error(_, Err),
            io.error_message(Err, Msg),
            Res = error(Msg)
        )
    ;
        Res0 = error(Msg),
        Res = error(Msg)
    ).

find(Name, [], error(Err), !IO) :-
    string.format("`%s' not found", [s(Name)], Err).
find(Name, [Dir | Dirs], Res, !IO) :-
    append_list([Dir, "/", Name], Path),
    prolog.see(Path, Res0, !IO),
    ( if Res0 = ok then
        Res = ok(Path)
    else
        find(Name, Dirs, Res, !IO)
    ).

:- type (A, B) ---> (A, B).

:- pred lines(int::in, [(int, [char])]::out, [char]::in, [char]::out) is det.

lines(_N, [], [], []).
lines(N, [Line | Lines]) -->
    =([_ | _]),
    line(N, Line),
    lines(N + 1, Lines).

:- pred line(int::in, (int, [char])::out, [char]::in, [char]::out) is det.

line(N, (N, Cs)) -->
    untilDiscard('\n', Cs).

:- pred decomment([(int, [char])]::in, [(int, [char])]::out) is det.

decomment(Lines0, Lines) :-
    map((pred(Line0::in, Line::out) is det :-
        Line0 = (N, Cs0),
        Line = (N, Cs),
        untilDiscard('#', Cs, Cs0, _)
    ), Lines0, Lines).

:- pred parse([entry]::out, [string]::out, [(int, [char])]::in) is det.

parse([], [], []).
parse(Entries, Errors, [Line | Lines]) :-
    Line = (N, Cs),
    ( if parseEntry(Entry, Cs, _) then
        Entries = [Entry | Entries0],
        parse(Entries0, Errors, Lines)
    else
        string.format("%d: syntax error", [i(N)], Msg),
        Errors = [Msg | Errors0],
        parse(Entries, Errors0, Lines)
    ).

:- pred addEntry(entry::in, catalog::in, catalog::out) is det.

addEntry(none, Cat, Cat).
addEntry(dtd(PublicId, SystemId), catalog(Cat0), catalog(Cat)) :-
    map.det_insert(PublicId, SystemId, Cat0, Cat).

:- pred parseEntry(entry::out, [char]::in, [char]::out) is semidet.

parseEntry(Entry) -->
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

:- pred ws([char]::in, [char]::out) is det.

ws -->
    ( if [C], { char.is_whitespace(C) } then
        ws
    else
        []
    ).

:- pred string(string::out, [char]::in, [char]::out) is semidet.

string(Str) -->
    ['"'], untilDiscard('"', Cs),
    { string.from_char_list(Cs, Str) }.

:- pred untilDiscard(char::in, [char]::out, [char]::in, [char]::out) is det.

untilDiscard(_C, [], [], []).
untilDiscard(C, Cs) -->
    =([_ | _]),
    [C0],
    ( if { C = C0 } then
        { Cs = [] }
    else
        { Cs = [C0|Cs0] },
        untilDiscard(C, Cs0)
    ).
