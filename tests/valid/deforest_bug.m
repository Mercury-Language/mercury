%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module deforest_bug.

:- interface.

:- import_module char.
:- import_module list.
:- import_module map.

:- type catalog
    --->    catalog(publicId -> systemId).

:- type [] ---> [].
:- type [T1 | T2] == list(T1).
:- type (A -> B) == map(A, B).

:- type dirs     == [path].
:- type publicId == string.
:- type systemId == string.
:- type path     == string.

:- type catRes(T)
    --->    ok(T)
    ;       error(string).

:- type (A, B)
    --->    (A, B).

:- pred deforest_load([(int, [char])], [string], catalog).
:- mode deforest_load(in, out, out) is det.

:- implementation.

:- import_module int.
:- import_module std_util.
:- import_module string.

:- type entry
    --->    dtd(publicId, systemId)
    ;       none.

:- pragma no_inline(deforest_load/3).

deforest_load(CatLines, Errors, Cat) :-
    parse(Entries, Errors, CatLines),
    init(Cat0),
    my_foldl(addEntry, Entries, catalog(Cat0), Cat).

:- pred parse([entry], [string], [(int, [char])]).
:- mode parse(out, out, in) is det.

parse([], [], []).
parse(Entries, Errors, [Line | Lines]) :-
    Line = (N, Cs),
    ( parseEntry(Entry, Cs, _) ->
        Entries = [Entry | Entries0],
        parse(Entries0, Errors, Lines)
    ;
        format("%d: syntax error", [i(N)], Msg),
        Errors = [Msg | Errors0],
        parse(Entries, Errors0, Lines)
    ).

:- pred addEntry(entry, catalog, catalog).
:- mode addEntry(in, in, out) is det.

addEntry(none, Cat, Cat).
addEntry(dtd(PublicId, SystemId), catalog(Cat0), catalog(Cat)) :-
    det_insert(PublicId, SystemId, Cat0, Cat).

:- pred parseEntry(entry, [char], [char]).
:- mode parseEntry(out, in, out) is semidet.

parseEntry(Entry) -->
    ( ['P'], string(PublicId), string(SystemId) ->
        { Entry = dtd(PublicId, SystemId) }
    ; =([]) ->
        { Entry = none }
    ;
        { fail }
    ).

:- pred string(string, [char], [char]).
:- mode string(out, in, out) is semidet.
:- pragma no_inline(string/3).

string("") -->
    { semidet_succeed }.

:- pred my_foldl(pred(T, U, U), list(T), U, U).
:- mode my_foldl(pred(in, in, out) is det, in, in, out) is det.

my_foldl(_, [], A, A).
my_foldl(P, [Head | Tail], A0, A) :-
    P(Head, A0, A1),
    my_foldl(P, Tail, A1, A).
