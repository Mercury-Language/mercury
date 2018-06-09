%---------------------------------------------------------------------------%
% Copyright (C) 2000, 2011 The University of Melbourne.
% Copyright (C) 2014, 2018 The Mercury team.
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

:- type catalog	 ---> catalog(publicId -> systemId).

:- type dirs	 == [path].

:- type publicId == string.

:- type systemId == string.

:- type path	 == string.

:- type catRes(T)
	--->	ok(T)
	;	error(string)
	.

:- pred load(string, dirs, catRes(catalog), io, io).
:- mode load(in, in, out, di, uo) is det.

:- pred find(string, dirs, catRes(string), io, io).
:- mode find(in, in, out, di, uo) is det.

:- implementation.

:- import_module char, int, list, map, string.

:- type entry
	--->	dtd(publicId, systemId)
	;	none
	.

load(Name, Dirs, Res) -->
    find(Name, Dirs, Res0),
    (
    	{ Res0 = ok(Path) },
	read_file(Res1),
	(
	    { Res1 = ok(CatChars) },
	    { lines(1, CatLines0, CatChars, _) },
	    { decomment(CatLines0, CatLines) },
	    { parse(Entries, Errors, CatLines) },
	    { init(Cat0) },
	    { foldl(addEntry, Entries, catalog(Cat0), Cat) },
	    { Res = ok(Cat) },
	    foldl((pred(Msg::in, di, uo) is det -->
	        stderr_stream(StdErr),
		format(StdErr, "%s: %s\n", [s(Path), s(Msg)])
	    ), Errors)
	;
	    { Res1 = error(_, Err) },
	    { io.error_message(Err, Msg) },
	    { Res = error(Msg) }
	)
    ;
        { Res0 = error(Msg) },
	{ Res = error(Msg) }
    ).

find(Name, [], error(Err)) -->
    { format("`%s' not found", [s(Name)], Err) }.
find(Name, [Dir|Dirs], Res) -->
    { append_list([Dir, "/", Name], Path) },
    see(Path, Res0),
    ( { Res0 = ok } ->
	{ Res = ok(Path) }
    ;
	find(Name, Dirs, Res)
    ).

:- type (A, B) ---> (A, B).

:- pred lines(int, [(int, [char])], [char], [char]).
:- mode lines(in, out, in, out) is det.

lines(_N, [], [], []).
lines(N, [Line|Lines]) -->
    =([_|_]),
    line(N, Line),
    lines(N + 1, Lines).

:- pred line(int, (int, [char]), [char], [char]).
:- mode line(in, out, in, out) is det.

line(N, (N, Cs)) -->
    untilDiscard('\n', Cs).

:- pred decomment([(int, [char])], [(int, [char])]).
:- mode decomment(in, out) is det.

decomment(Lines0, Lines) :-
    map((pred(Line0::in, Line::out) is det :-
    	Line0 = (N, Cs0),
	Line = (N, Cs),
	untilDiscard('#', Cs, Cs0, _)
    ), Lines0, Lines).

:- pred parse([entry], [string], [(int, [char])]).
:- mode parse(out, out, in) is det.

parse([], [], []).
parse(Entries, Errors, [Line|Lines]) :-
    Line = (N, Cs),
    ( parseEntry(Entry, Cs, _) ->
    	Entries = [Entry|Entries0],
	parse(Entries0, Errors, Lines)
    ;
        format("%d: syntax error", [i(N)], Msg),
	Errors = [Msg|Errors0],
	parse(Entries, Errors0, Lines)
    ).

:- pred addEntry(entry, catalog, catalog).
:- mode addEntry(in, in, out) is det.

addEntry(none, Cat, Cat).
addEntry(dtd(PublicId, SystemId), catalog(Cat0), catalog(Cat)) :-
    map.det_insert(PublicId, SystemId, Cat0, Cat).

:- pred parseEntry(entry, [char], [char]).
:- mode parseEntry(out, in, out) is semidet.

parseEntry(Entry) -->
    ws,
    ( ['P','U','B','L','I','C'], ws, string(PublicId), ws, string(SystemId) ->
    	{ Entry = dtd(PublicId, SystemId) }
    ; =([]) ->
    	{ Entry = none }
    ;
    	{ fail }
    ).

:- pred ws([char], [char]).
:- mode ws(in, out) is det.

ws -->
    ( [C], { char.is_whitespace(C) } ->
    	ws
    ;
    	[]
    ).

:- pred string(string, [char], [char]).
:- mode string(out, in, out) is semidet.

string(Str) -->
    ['"'], untilDiscard('"', Cs),
    { string.from_char_list(Cs, Str) }.

:- pred untilDiscard(char, [char], [char], [char]).
:- mode untilDiscard(in, out, in, out) is det.

untilDiscard(_C, [], [], []).
untilDiscard(C, Cs) -->
    =([_|_]),
    [C0],
    ( { C = C0 } ->
    	{ Cs = [] }
    ;
   	{ Cs = [C0|Cs0] },
	untilDiscard(C, Cs0)
    ).

