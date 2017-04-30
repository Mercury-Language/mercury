%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2001, 2005-2006, 2008 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: util.m.
% Authors: conway, zs.
%
% This module defines utility predicates for the CGI program.
%
%---------------------------------------------------------------------------%

:- module util.
:- interface.

:- import_module char.
:- import_module list.

%---------------------------------------------------------------------------%

    % split(Str, Char, Pieces)
    %
    % Split Str into pieces at every occurrence of Char, and return the pieces
    % in order. No piece will contain Char.  If two Chars occur in a row,
    % split will return the empty string as the piece between them.
    %
:- pred split(string::in, char::in, list(string)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module string.

%---------------------------------------------------------------------------%

split(Str0, SplitChar, Strs) :-
    string.to_char_list(Str0, Chars0),
    split_2(Chars0, SplitChar, Strs).

:- pred split_2(list(char)::in, char::in, list(string)::out) is det.

split_2(Chars, SplitChar, PieceStrs) :-
    ( if find_split_char(Chars, SplitChar, Before, After) then
        string.from_char_list(Before, BeforeStr),
        split_2(After, SplitChar, TailStrs),
        PieceStrs = [BeforeStr | TailStrs]
    else
        string.from_char_list(Chars, PieceStr),
        PieceStrs = [PieceStr]
    ).

    % find_split_char(Chars, SplitChar, Before, After):
    %
    % If SplitChar occurs in Chars, it return all the characters in Chars
    % before the first occurrence of SplitChar in Chars in Before, and all the
    % characters after the first occurrence of SplitChar in Chars in After.
    % The first occurrence of SplitChar itself is not returned.
    %
:- pred find_split_char(list(char)::in, char::in,
    list(char)::out, list(char)::out) is semidet.

find_split_char(Chars, SplitChar, Before, After) :-
    find_split_char_2(Chars, SplitChar, [], BeforeRev, After),
    list.reverse(BeforeRev, Before).

:- pred find_split_char_2(list(char)::in, char::in, list(char)::in,
    list(char)::out, list(char)::out) is semidet.

find_split_char_2([Char | Chars], SplitChar, !BeforeRev, After) :-
    ( if Char = SplitChar then
        After = Chars
    else
        !:BeforeRev = [Char | !.BeforeRev],
        find_split_char_2(Chars, SplitChar, !BeforeRev, After)
    ).

%---------------------------------------------------------------------------%
:- end_module util.
%---------------------------------------------------------------------------%
