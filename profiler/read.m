%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-1998,2000,2004-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: read.m.
% Main author: petdr.
%
% Input predicates for use with mercury_profile.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module read.
:- interface.

:- import_module globals.

:- import_module io.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- pred maybe_read_label_addr(maybe(int)::out, io::di, io::uo) is det.

:- pred maybe_read_label_name(maybe(string)::out, io::di, io::uo) is det.

:- pred read_label_addr(int::out, io::di, io::uo) is det.

:- pred read_label_name(string::out, io::di, io::uo) is det.

:- pred read_string(string::out, io::di, io::uo) is det.

:- pred read_int(int::out, io::di, io::uo) is det.

:- pred read_float(float::out, io::di, io::uo) is det.

:- pred read_what_to_profile(what_to_profile::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module demangle.
:- import_module options.

:- import_module bool.
:- import_module list.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

maybe_read_label_addr(MaybeLabelAddr, !IO) :-
    io.read_word(WordResult, !IO),
    (
        WordResult = ok(CharList),
        string.from_char_list(CharList, LabelAddrStr),
        ( if string.base_string_to_int(10, LabelAddrStr, LabelAddr) then
            MaybeLabelAddr = yes(LabelAddr)
        else if string.base_string_to_int(16, LabelAddrStr, LabelAddrHex) then
            MaybeLabelAddr = yes(LabelAddrHex)
        else
            unexpected($pred, "Label address not hexadecimal or integer")
        )
    ;
        WordResult = eof,
        MaybeLabelAddr = no
    ;
        WordResult = error(Error),
        unexpected($pred, io.error_message(Error))
    ).

%---------------------------------------------------------------------------%

maybe_read_label_name(MaybeLabelName, !IO) :-
    globals.io_lookup_bool_option(demangle, Demangle, !IO),
    io.read_word(WordResult, !IO),
    (
        WordResult = ok(CharList0),
        string.from_char_list(CharList0, MangledLabelName),
        (
            Demangle = yes,
            demangle(MangledLabelName, LabelName),
            MaybeLabelName = yes(LabelName)
        ;
            Demangle = no,
            MaybeLabelName = yes(MangledLabelName)
        )
    ;
        WordResult = eof,
        MaybeLabelName = no
    ;
        WordResult = error(Error),
        unexpected($pred, io.error_message(Error))
    ).

%---------------------------------------------------------------------------%

read_label_addr(LabelAddr, !IO) :-
    io.read_word(WordResult, !IO),
    (
        WordResult = ok(CharList),
        string.from_char_list(CharList, LabelAddrStr),
        ( if string.base_string_to_int(10, LabelAddrStr, LabelAddr0) then
            LabelAddr = LabelAddr0
        else if string.base_string_to_int(16,LabelAddrStr, LabelAddrHex) then
            LabelAddr = LabelAddrHex
        else
            unexpected($pred, "Label address not hexadecimal or integer")
        )
    ;
        WordResult = eof,
        unexpected($pred, "EOF reached")
    ;
        WordResult = error(Error),
        unexpected($pred, io.error_message(Error))
    ).

%---------------------------------------------------------------------------%

read_label_name(LabelName, !IO) :-
    globals.io_lookup_bool_option(demangle, Demangle, !IO),
    io.read_word(WordResult, !IO),
    (
        WordResult = ok(CharList0),
        string.from_char_list(CharList0, MangledLabelName),
        (
            Demangle = yes,
            demangle(MangledLabelName, LabelName)
        ;
            Demangle = no,
            LabelName = MangledLabelName
        )
    ;
        WordResult = eof,
        unexpected($pred, "EOF reached")
    ;
        WordResult = error(Error),
        unexpected($pred, io.error_message(Error))
    ).

%---------------------------------------------------------------------------%

read_string(String, !IO) :-
    io.read_word(WordResult, !IO),
    (
        WordResult = ok(CharList),
        string.from_char_list(CharList, String)
    ;
        WordResult = eof,
        unexpected($pred, "EOF reached")
    ;
        WordResult = error(Error),
        unexpected($pred, io.error_message(Error))
    ).

%---------------------------------------------------------------------------%

read_int(Int, !IO) :-
    read_string(IntStr, !IO),
    ( if string.to_int(IntStr, Int0) then
        Int = Int0
    else
        unexpected($pred,
            string.format("Invalid input: not an integer %s", [s(IntStr)]))
    ).

%---------------------------------------------------------------------------%

read_float(Float, !IO) :-
    read_string(FloatStr, !IO),
    ( if string.to_float(FloatStr, Float0) then
        Float = Float0
    else
        unexpected($pred,
            string.format("Invalid input: not a float %s", [s(FloatStr)]))
    ).

%---------------------------------------------------------------------------%

read_what_to_profile(WhatToProfile, !IO) :-
    read_string(Str, !IO),
    ( if what_to_profile(Str, WhatToProfile0) then
        WhatToProfile = WhatToProfile0
    else
        unexpected($pred,
            string.format("Invalid input: WhatToProfile = %s", [s(Str)]))
    ).

%---------------------------------------------------------------------------%
:- end_module read.
%---------------------------------------------------------------------------%
