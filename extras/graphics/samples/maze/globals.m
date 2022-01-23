%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%------------------------------------------------------------------------------
%
% file: globals.m
% author: conway, June 1997
%
% This source file is hereby placed in the public domain. -conway (the author).
%
%------------------------------------------------------------------------------

:- module globals.

:- interface.

:- import_module io.
:- import_module string.

%-----------------------------------------------------------------------------%

:- pred globals.init(io::di, io::uo) is det.

:- pred globals.get(string::in, T::out, io::di, io::uo) is det.

:- pred globals.set(string::in, T::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module map.
:- import_module require.
:- import_module univ.

%-----------------------------------------------------------------------------%

globals.init(!IO) :-
    map.init(Map : map(string, univ)),
    io.set_globals(univ(Map), !IO).

globals.get(Name, Value, !IO) :-
    io.get_globals(UMap0, !IO),
    ( if univ_to_type(UMap0, Map0) then
        ( if UValue = Map0 ^ elem(Name) then
            ( if univ_to_type(UValue, Value0) then
                Value = Value0
            else
                error("globals.get/4: value has bad type.")
            )
        else
            error("globals.get/4: name not found.")
        )
    else
        error("globals.get/4: global store corrupt.")
    ).

globals.set(Name, Value, !IO) :-
    io.get_globals(UMap0, !IO),
    ( if univ_to_type(UMap0, Map0) then
        type_to_univ(Value, UValue),
        map.set(Name, UValue, Map0, Map),
        type_to_univ(Map, UMap),
        io.set_globals(UMap, !IO)
    else
        error("globals.set/4: global store corrupt.")
    ).

%-----------------------------------------------------------------------------%
:- end_module globals.
%-----------------------------------------------------------------------------%
